library("readr")
library("ggplot2")
library("lubridate")
library("tseries")
library("nortest")
library("timeDate")
library("PerformanceAnalytics")

waluty<- read_delim("E:/Studia/Sem. 6/IAR/projekt1/waluty_2012_2018.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
View(waluty)

kursy<-waluty$`1EUR`
stopy_dzienne_proste<- -(kursy[-length(kursy)]-kursy[-1])/kursy[-1]
stopy_df_EUR<-data.frame("data"=ymd(waluty$data)[-c(length(kursy))],"kurs"=kursy[-c(length(kursy))],"stopy_dzienne_proste"=stopy_dzienne_proste)

kursy<-waluty$`1NOK`
stopy_dzienne_proste<- -(kursy[-length(kursy)]-kursy[-1])/kursy[-1]
stopy_df_NOK<-data.frame("data"=ymd(waluty$data)[-c(length(kursy))],"kurs"=kursy[-c(length(kursy))],"stopy_dzienne_proste"=stopy_dzienne_proste)

kursy<-waluty$`1THB`
stopy_dzienne_proste<- -(kursy[-length(kursy)]-kursy[-1])/kursy[-1]
stopy_df_THB<-data.frame("data"=ymd(waluty$data)[-c(length(kursy))],"kurs"=kursy[-c(length(kursy))],"stopy_dzienne_proste"=stopy_dzienne_proste)


var_es<-function(stopy_df){

  v_var<-NULL
  v_es<-NULL

  for (s in 1:(nrow(stopy_df)-251)) {
    okno<-stopy_df[s:(s+251),]
    #var<-VaR(okno$stopy_dzienne_proste,method="historical",p =.99)
    #v_var<-c(v_var,var)
    
    var<-quantile(okno$stopy_dzienne_proste,0.01)
    v_var<-c(v_var,var)
    
    ES<-mean(okno[okno$stopy_dzienne_proste<var,3])
    v_es<-c(v_es,ES)
  }
  
  result_df<-stopy_df[1:length(v_var),]
  result_df$VAR<-v_var
  result_df$ES<-v_es
  colnames(result_df)[1]<-"data_pocz"
  result_df$data_kon<-stopy_df$data[-c(1:251)]
  result_df<-result_df[,c("data_pocz","data_kon","kurs","stopy_dzienne_proste","VAR","ES")]
  
  return(result_df)
}

EUR_df<-var_es(stopy_df_EUR)
NOK_df<-var_es(stopy_df_NOK)
THB_df<-var_es(stopy_df_THB)

n<-nrow(EUR_df)
waluta_v<-c(rep("EUR",n),rep("NOK",n),rep("THB",n))

var_es_df<-data.frame("VAR"=c(EUR_df$VAR,NOK_df$VAR,THB_df$VAR),"ES"=c(EUR_df$ES,NOK_df$ES,THB_df$ES),"waluta"=waluta_v,"data_poczatku_okna"=EUR_df$data_pocz)

ggplot(var_es_df,aes(y=VAR,x=data_poczatku_okna,col=waluta))+
  geom_line()
ggplot(var_es_df,aes(y=ES,x=data_poczatku_okna,col=waluta))+
  geom_line()

