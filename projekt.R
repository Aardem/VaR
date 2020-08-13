library("readr")
library("ggplot2")
library("lubridate")
library("tseries")
library("nortest")
library("timeDate")
library("PerformanceAnalytics")
library("GAS")


waluty<- read_delim("E:/Studia/Sem. 6/IAR/projekt1/waluty_2012_2018.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
waluty$data<-ymd(waluty$data)
View(waluty)

kursy<-waluty$`1EUR`
stopy_dzienne_proste<- (kursy[-1]-kursy[-length(kursy)])/kursy[-length(kursy)]
stopy_df_EUR<-data.frame("data"=waluty$data[-1],"kurs"=kursy[-1],"stopy_dzienne_proste"=stopy_dzienne_proste)

kursy<-waluty$`1NOK`
stopy_dzienne_proste<- (kursy[-1]-kursy[-length(kursy)])/kursy[-length(kursy)]
stopy_df_NOK<-data.frame("data"=waluty$data[-1],"kurs"=kursy[-1],"stopy_dzienne_proste"=stopy_dzienne_proste)

kursy<-waluty$`1THB`
stopy_dzienne_proste<- (kursy[-1]-kursy[-length(kursy)])/kursy[-length(kursy)]
stopy_df_THB<-data.frame("data"=waluty$data[-1],"kurs"=kursy[-1],"stopy_dzienne_proste"=stopy_dzienne_proste)


var_es_hist<-function(stopy_df){

  v_var<-NULL
  v_es<-NULL

  for (s in 1:(nrow(stopy_df)-499)) {
    okno<-stopy_df[s:(s+499),]
    
    var<-quantile(okno$stopy_dzienne_proste,0.01,type=7)
    v_var<-c(v_var,var)
    
    ES<-mean(okno[okno$stopy_dzienne_proste<var,3])
    v_es<-c(v_es,ES)
  }
  
  result_df<-stopy_df[1:length(v_var),]
  result_df$VAR<-v_var
  result_df$ES<-v_es
  colnames(result_df)[1]<-"data_pocz"
  result_df$data_kon<-stopy_df$data[-c(1:499)]
  result_df<-result_df[,c("data_pocz","data_kon","kurs","stopy_dzienne_proste","VAR","ES")]
}

var_es_hist_wag<-function(stopy_df, q=0.995){
  
  v_var<-NULL
  v_es<-NULL

  for (s in 1:(nrow(stopy_df)-499)) {
    okno<-stopy_df[s:(s+499),]
    i<-1:nrow(okno)
    n<-nrow(okno)
    wagi<-(q^(n-i))*(1-q)/(1-(q^n))

    kolejnosc<-rank(okno$stopy_dzienne_proste,ties.method="first")
    wagi[kolejnosc]<-wagi
    #var<-sort(okno$stopy_dzienne_proste)[(abs(cumsum(wagi)-0.01)==min(abs(cumsum(wagi)-0.01)))]
    var<-max(sort(okno$stopy_dzienne_proste)[cumsum(wagi)<=0.01])

    v_var<-c(v_var,var)
    
    ES<-mean(okno[okno$stopy_dzienne_proste<var,3])
    v_es<-c(v_es,ES)
  }
  
  result_df<-stopy_df[1:length(v_var),]
  result_df$VAR<-v_var
  result_df$ES<-v_es
  colnames(result_df)[1]<-"data_pocz"
  result_df$data_kon<-stopy_df$data[-c(1:499)]
  result_df<-result_df[,c("data_pocz","data_kon","kurs","stopy_dzienne_proste","VAR","ES")]
  
  return(result_df)
}

var_es_boot<-function(stopy_df,n=500,N=100){
  
  v_var<-NULL
  v_es<-NULL

  for (s in 1:(nrow(stopy_df)-499)) {
    okno<-stopy_df[s:(s+499),]
    v_var_los<-NULL
    v_es_los<-NULL
    
    for (i in 1:N) {  #zamien na sapply
    stopy_los<-sample(x = okno$stopy_dzienne_proste, size = n, replace=T)
    
    var_los<-quantile(stopy_los,0.01)
    v_var_los<-c(v_var_los,var_los)
    
    ES_los<-mean(stopy_los[stopy_los<var_los])
    v_es_los<-c(v_es,ES_los)
    }
    
    var<-mean(v_var_los)
    ES<-mean(v_es_los)
    v_var<-c(v_var,var)
    v_es<-c(v_es,ES)
    
  }
  
  result_df<-stopy_df[1:length(v_var),]
  result_df$VAR<-v_var
  result_df$ES<-v_es
  colnames(result_df)[1]<-"data_pocz"
  result_df$data_kon<-stopy_df$data[-c(1:499)]
  result_df<-result_df[,c("data_pocz","data_kon","kurs","stopy_dzienne_proste","VAR","ES")]
  
  return(result_df)
}

var_es_ewma<-function(stopy_df, lambda=0.94){
  
  v_var<-NULL
  v_es<-NULL
  
  for (s in 1:(nrow(stopy_df)-499)) {
    okno<-stopy_df[s:(s+499),]
    
    rets<-okno$stopy_dzienne_proste
    n <- length(rets)+1
    sig.s <- rep(sd(rets), n)
    for (i in 2:n) {
      sig.s[i] <- sqrt(((sig.s[i-1]^2)*lambda) + ((rets[i-1]^2)*(1 - lambda)))
    }
    wartosci<-(rets*sig.s[n])/sig.s[1:(n-1)]

    var<-quantile(wartosci,0.01)
    v_var<-c(v_var,var)
    
    ES<-mean(okno[okno$stopy_dzienne_proste<var,3])
    v_es<-c(v_es,ES)
  }
  
  result_df<-stopy_df[1:length(v_var),]
  result_df$VAR<-v_var
  result_df$ES<-v_es
  colnames(result_df)[1]<-"data_pocz"
  result_df$data_kon<-stopy_df$data[-c(1:499)]
  result_df<-result_df[,c("data_pocz","data_kon","kurs","stopy_dzienne_proste","VAR","ES")]
  
  return(result_df)
}

var_es_monte_carlo<-function(stopy_df){
  
  v_var<-NULL
  v_es<-NULL
  
  for (s in 1:(nrow(stopy_df)-499)) {
    okno<-stopy_df[s:(s+499),]
    
    m<-mean(okno$stopy_dzienne_proste)
    sd<-sd(okno$stopy_dzienne_proste)
    var<-quantile(rnorm(1000,mean=m,sd=sd),0.01)

    v_var<-c(v_var,var)

    ES<-mean(okno[okno$stopy_dzienne_proste<var,3])
    v_es<-c(v_es,ES)
  }
  
  result_df<-stopy_df[1:length(v_var),]
  result_df$VAR<-v_var
  result_df$ES<-v_es
  colnames(result_df)[1]<-"data_pocz"
  result_df$data_kon<-stopy_df$data[-c(1:499)]
  result_df<-result_df[,c("data_pocz","data_kon","kurs","stopy_dzienne_proste","VAR","ES")]
  
  return(result_df)
}

EUR_df<-var_es_hist(stopy_df_EUR)
NOK_df<-var_es_hist(stopy_df_NOK)
THB_df<-var_es_hist(stopy_df_THB)

n<-nrow(EUR_df)
waluta_v<-c(rep("EUR",n),rep("NOK",n),rep("THB",n))

var_es_df<-data.frame("VAR"=c(EUR_df$VAR,NOK_df$VAR,THB_df$VAR),"ES"=c(EUR_df$ES,NOK_df$ES,THB_df$ES),"waluta"=waluta_v,"data_poczatku_okna"=EUR_df$data_pocz)

ggplot(var_es_df,aes(y=VAR,x=data_poczatku_okna,col=waluta))+
  geom_line()
ggplot(var_es_df,aes(y=ES,x=data_poczatku_okna,col=waluta))+
  geom_line()

EUR_df_wag<-var_es_hist_wag(stopy_df_EUR)
NOK_df_wag<-var_es_hist_wag(stopy_df_NOK)
THB_df_wag<-var_es_hist_wag(stopy_df_THB)

EUR_df_boot<-var_es_boot(stopy_df_EUR)
NOK_df_boot<-var_es_boot(stopy_df_NOK)
THB_df_boot<-var_es_boot(stopy_df_THB)

EUR_df_ewma<-var_es_ewma(stopy_df_EUR)
NOK_df_ewma<-var_es_ewma(stopy_df_NOK)
THB_df_ewma<-var_es_ewma(stopy_df_THB)

EUR_df_monte_carlo<-var_es_monte_carlo(stopy_df_EUR)
NOK_df_monte_carlo<-var_es_monte_carlo(stopy_df_NOK)
THB_df_monte_carlo<-var_es_monte_carlo(stopy_df_THB)

DF<-data.frame("VAR"=c(EUR_df$VAR,EUR_df_wag$VAR,EUR_df_boot$VAR,EUR_df_ewma$VAR),"metoda"=c(rep("HIST",nrow(EUR_df)),rep("HIST WAG",nrow(EUR_df_wag)),rep("BOOT",nrow(EUR_df_boot)),rep("EWMA",nrow(EUR_df_boot))),"data_poczatku_okna"=rep(EUR_df_wag$data_pocz,4))
ggplot(DF,aes(y=VAR,x=data_poczatku_okna,col=metoda))+
  geom_line()

#Testy VARu
KupiecTest<-function(stopy_v,VAR,alpha=0.01){
  N <- length(stopy_v)
  przekroczenia_v <- numeric(N)
  przekroczenia_v[which(stopy_v <= VAR)] <- 1
  
  N2 <- sum(przekroczenia_v)
  stos <- N2/N
  test <- 2 * log(((1 - stos)^(N - N2) * stos^N2)/((1 - alpha)^(N - N2) * alpha^N2))

  pvalue <- 1 - pchisq(test, df = 1)

  vect_res <- c(test,pvalue)
}

ChristoffersenTest<-function(stopy_v,VAR,alpha=0.01){
  N <- length(stopy_v)
  przekroczenia_v <- numeric(N)
  przekroczenia_v[which(stopy_v <= VAR)] <- 1
  
  n00 = n01 = n10 = n11 = 0
  for (i in 2:N) {
    if (przekroczenia_v[i] == 0 & przekroczenia_v[i - 1] == 0) 
      n00 <- n00 + 1
    if (przekroczenia_v[i] == 0 & przekroczenia_v[i - 1] == 1) 
      n01 <- n01 + 1
    if (przekroczenia_v[i] == 1 & przekroczenia_v[i - 1] == 0) 
      n10 <- n10 + 1
    if (przekroczenia_v[i] == 1 & przekroczenia_v[i - 1] == 1) 
      n11 <- n11 + 1
  }
  pi0 <- n01/(n00 + n01)
  pi1 <- n11/(n10 + n11)
  pi <- (n01 + n11)/(n00 + n01 + n10 + n11)
  test1 <- -2 * log(((1 - pi)^(n00 + n10) * pi^(n01 + n11))/((1 - pi0)^n00 * pi0^n01 * (1 - pi1)^n10 * pi1^n11))
 
  test2 <- KupiecTest(stopy_v,VAR, alpha)[1]
  test3 <- test2 + test1
  pvalue <- 1 - pchisq(test3, df = 2)
  vect_res <- c(test3, pvalue)
}



ZasiegTest<-function(stopy_v,VAR,alpha=0.01){
  N <- length(stopy_v)
  l_przekroczen<-sum(stopy_v <= VAR)
  prawd<-l_przekroczen/N
  
  #test_v<-NULL
  #for (k in l_przekroczen:(N-1)) {
  #  test<-cumprod((N-k):N)[k+1]*(prawd^k)*((1-prawd)^(N-k))/(cumprod(1:k)[k])
  #  test_v<-c(test_v,test)
  #}
  
  k<-l_przekroczen
  test<-cumprod((N-k):N)[k+1]*(prawd^k)*((1-prawd)^(N-k))/(cumprod(1:k)[k])
  
  #test<-sum(test_v)
  vect_res <- test
}

testowanie_wsteczne<-function(stopy,VAR_v,alpha=0.01){
  test_res_v1<-NULL
  test_res_v2<-NULL
  test_res_v3<-NULL

  for (i in 1:(length(stopy)-499)) {
    okno<-stopy[i:(i+499)]
    
    test_res1<-KupiecTest(okno,VAR_v[i],alpha)[2]>alpha
    test_res_v1[i]<-test_res1

    test_res2<-ChristoffersenTest(okno,VAR_v[i],alpha)[2]>alpha
    test_res_v2[i]<-test_res2
    
    test_res3<-ZasiegTest(okno,VAR_v[i],alpha)
    test_res_v3[i]<-test_res3

  }
  res1<-mean(test_res_v1)
  res2<-mean(test_res_v2)
  res3<-mean(test_res_v3)
  
  res<-c(res1,res2,res3)
}


testy_df<-data.frame("metoda estymacji"=c("historyczna","historyczna z wagami", "bootstrap"),
                     "statystyka testu Kupca"=c(KupiecTest(EUR_df$stopy_dzienne_proste,EUR_df$VAR)[1],KupiecTest(EUR_df_wag$stopy_dzienne_proste,EUR_df_wag$VAR)[1],KupiecTest(EUR_df_boot$stopy_dzienne_proste,EUR_df_boot$VAR)[1]),
                     "p-value testu Kupca"=c(KupiecTest(EUR_df$stopy_dzienne_proste,EUR_df$VAR)[2],KupiecTest(EUR_df_wag$stopy_dzienne_proste,EUR_df_wag$VAR)[2],KupiecTest(EUR_df_boot$stopy_dzienne_proste,EUR_df_boot$VAR)[2]),
                     "statystyka testu Christoffersena"=c(ChristoffersenTest(EUR_df$stopy_dzienne_proste,EUR_df$VAR)[1],ChristoffersenTest(EUR_df_wag$stopy_dzienne_proste,EUR_df_wag$VAR)[1],ChristoffersenTest(EUR_df_boot$stopy_dzienne_proste,EUR_df_boot$VAR)[1]),
                     "p-value testu Christoffersena"==c(ChristoffersenTest(EUR_df$stopy_dzienne_proste,EUR_df$VAR)[2],ChristoffersenTest(EUR_df_wag$stopy_dzienne_proste,EUR_df_wag$VAR)[2],ChristoffersenTest(EUR_df_boot$stopy_dzienne_proste,EUR_df_boot$VAR)[2])
                     )

#testowanko luzne
GAS:::Kupiec
BacktestVaR(EUR_df$stopy_dzienne_proste,EUR_df$VAR,alpha=0.01)[[2]]

print(ZasiegTest(EUR_df_wag$stopy_dzienne_proste[1:500],EUR_df_wag$VAR[1]))

print(testowanie_wsteczne(EUR_df$stopy_dzienne_proste,EUR_df$VAR,alpha=0.01))

print(ZasiegTest(EUR_df_wag$stopy_dzienne_proste[1:500],EUR_df_wag$VAR[1]))

i<-1:nrow(stopy_df_EUR)
n<-nrow(stopy_df_EUR)
q=0.995
wagi<-(q^(n-i))*(1-q)/(1-(q^n))

stopy_df_EUR$stopy_dzienne_proste[sort(stopy_df_EUR$stopy_dzienne_proste*wagi)[round(0.01*n)]==stopy_df_EUR$stopy_dzienne_proste*wagi]
quantile((stopy_df_EUR$stopy_dzienne_proste*wagi),0.01)
sum((stopy_df_EUR$stopy_dzienne_proste*wagi)==quantile((stopy_df_EUR$stopy_dzienne_proste*wagi),0.01,type=1))




ewma <- function(rets, lambda=0.94) {
  n <- length(rets)+1
  sig.s <- rep(sd(rets), n)
  for (i in 2:n) {
    sig.s[i] <- sqrt(((sig.s[i-1]^2)*lambda) + ((rets[i-1]^2)*(1 - lambda)))
  }
  wartosci<-(rets*sig.s[n])/sig.s[1:(n-1)]
  return(wartosci)
}
ewma(stopy_df_EUR$stopy_dzienne_proste)


plot(stopy_df_EUR$stopy_dzienne_proste,type="l")
plot(ewma(stopy_df_EUR$stopy_dzienne_proste),col="red",type="l")

N <- 600
l_przekroczen<-1
prawd<-0.01

test_v<-NULL
for (k in 0:(l_przekroczen-1)) {
  test<-cumprod((N-k+1):N)[k]*(prawd^k)*((1-prawd)^(N-k))/(factorial(k))
  if (k==0) {
    test<-(prawd^k)*((1-prawd)^(N-k))/(factorial(k))
  }
  test_v<-c(test_v,test)
}

#k<-l_przekroczen
#test<-cumprod((N-k):N)[k+1]*(prawd^k)*((1-prawd)^(N-k))/(cumprod(1:k)[k])

test<-sum(test_v)
1-test

wiele_wyj<-1-pbinom(l_przekroczen-1,N,prawd,TRUE)
malo_wyj<-pbinom(l_przekroczen,N,prawd,TRUE)

percent(data.frame(c(3,5,2),c(3,5,2)))

