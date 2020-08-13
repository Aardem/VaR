library(readr)
library(ggplot2)
library(lubridate)
library(nortest)
library(statmod)
library(stabledist)
library(moments)



waluty<- read.csv("E:/Studia/Sem. 6/IAR/projekt1/waluty_2012_2018.csv", 
                               sep=";",dec=".",header=TRUE)
View(waluty)
waluty$data<-ymd(waluty$data)


##STOPY DZIENNE
stopy_dzienne_proste<-(waluty$`X1GBP`[-length(waluty$`X1GBP`)]-waluty$`X1GBP`[-1])/waluty$`X1GBP`[-1]
stopy_dzienne_log<-log(waluty$`X1GBP`[-length(waluty$`X1GBP`)]/waluty$`X1GBP`[-1])

waluty_df<-data.frame("data"=waluty$data[-c(length(waluty$`X1GBP`))],"kurs"=waluty$`X1GBP`[-c(length(waluty$`X1GBP`))],"stopy_dzienne_proste"=stopy_dzienne_proste,"stopy_dzienne_log"=stopy_dzienne_log,"roznica"=stopy_dzienne_proste-stopy_dzienne_log)

ggplot(waluty_df,aes(x=data,y=stopy_dzienne_proste))+
  geom_point(aes(y=stopy_dzienne_proste),col="red",size=1)+
  geom_point(aes(y=stopy_dzienne_log),col="green",size=1)

ggplot(waluty_df,aes(x=data,y=roznica))+
  geom_line(aes(y=roznica),col="blue")

hist(waluty_df$roznica)

shapiro.test(waluty_df$stopy_dzienne_proste)
shapiro.test(waluty_df$stopy_dzienne_log)

lillie.test(waluty_df$stopy_dzienne_proste)
lillie.test(waluty_df$stopy_dzienne_log)

ad.test(waluty_df$stopy_dzienne_proste)
ad.test(waluty_df$stopy_dzienne_log)


##STOPY TYGODNIOWE I MIESIECZNE

#tyg
waluty_tyg_df<-waluty_df[wday(waluty_df$data)==2,]

stopy_tyg_proste<-(waluty_tyg_df$kurs[-length(waluty_tyg_df$kurs)]-waluty_tyg_df$kurs[-1])/waluty_tyg_df$kurs[-1]
stopy_tyg_log<-log(waluty_tyg_df$kurs[-length(waluty_tyg_df$kurs)]/waluty_tyg_df$kurs[-1])

waluty_tyg_df<-cbind(waluty_tyg_df[-length(waluty_tyg_df$kurs),-c(3:5)],"stopy_tyg_proste"=stopy_tyg_proste,"stopy_tyg_log"=stopy_tyg_log,"roznica"=stopy_tyg_proste-stopy_tyg_log)

#mies
v_pierw_dm<-c(TRUE,(month(waluty_df$data)[-1]!=month(waluty_df$data)[-length(waluty_df$data)]))
waluty_mies_df<-waluty_df[v_pierw_dm,]

stopy_mies_proste<-(waluty_mies_df$kurs[-length(waluty_mies_df$kurs)]-waluty_mies_df$kurs[-1])/waluty_mies_df$kurs[-1]
stopy_mies_log<-log(waluty_mies_df$kurs[-length(waluty_mies_df$kurs)]/waluty_mies_df$kurs[-1])

waluty_mies_df<-cbind(waluty_mies_df[-length(waluty_mies_df$kurs),-c(3:5)],"stopy_mies_proste"=stopy_mies_proste,"stopy_mies_log"=stopy_mies_log,"roznica"=stopy_mies_proste-stopy_mies_log)


ggplot(waluty_mies_df,aes(x=data,y=stopy_mies_proste))+
  geom_point(aes(y=stopy_mies_proste),col="red",size=1)+
  geom_point(aes(y=stopy_mies_log),col="green",size=1)

ggplot(waluty_mies_df,aes(x=data,y=roznica))+
  geom_line(aes(y=roznica),col="blue")

hist(waluty_mies_df$roznica)

#testy
shapiro.test(waluty_tyg_df$stopy_tyg_proste)
shapiro.test(waluty_tyg_df$stopy_tyg_log)
lillie.test(waluty_tyg_df$stopy_tyg_proste)
lillie.test(waluty_tyg_df$stopy_tyg_log)
ad.test(waluty_tyg_df$stopy_tyg_proste)
ad.test(waluty_tyg_df$stopy_tyg_log)

shapiro.test(waluty_mies_df$stopy_mies_proste)
shapiro.test(waluty_mies_df$stopy_mies_log)
lillie.test(waluty_mies_df$stopy_mies_proste)
lillie.test(waluty_mies_df$stopy_mies_log)
ad.test(waluty_mies_df$stopy_mies_proste)
ad.test(waluty_mies_df$stopy_mies_log)

##TABELA CZESTOSCI
zbior<-waluty_df$stopy_dzienne_proste
m_p<-mean(zbior)
sd_p<-sd(zbior)

z_teo<-rnorm(length(zbior),mean=m_p,sd=sd_p)
z_emp<-zbior

#budowa tabelki
tab_czest<-data.frame("czest_teo"=rep(0,6),"czest_emp"=rep(0,6))
rownames(tab_czest)<-c("< 1 sd", "1-2 sd", "2-3 sd", "3-4 sd", "4-5 sd", "> 5 sd")

buf_t<-0
buf_e<-0
for (rec in 1:nrow(tab_czest)) {
  tab_czest[rec,1]<-sum((z_teo<(m_p+(rec*sd_p)))&(z_teo>(m_p-(rec*sd_p))))-buf_t
  buf_t<-sum((z_teo<(m_p+(rec*sd_p)))&(z_teo>(m_p-(rec*sd_p))))
  
  tab_czest[rec,2]<-sum((z_emp<(m_p+(rec*sd_p)))&(z_emp>(m_p-(rec*sd_p))))-buf_e
  buf_e<-sum((z_emp<(m_p+(rec*sd_p)))&(z_emp>(m_p-(rec*sd_p))))
  
  if (rec==nrow(tab_czest)) {
    tab_czest[rec,1]<-length(z_teo)-buf_t
    tab_czest[rec,2]<-length(z_emp)-buf_e
  }
}

round((tab_czest/sum(tab_czest$czest_teo)),5)

#DOPASOWANIE ROZKLADU TEORETYCZNEGO DO EMPIRYCZNEGO

z_teo<-rt(length(z_emp),df=0.5,ncp=0)
z_teo<-rinvgauss(length(z_emp), 0.0025,dispersion=12500)
z_teo<-rlogis(length(z_emp),location=0,scale=0.003) #OK
z_teo<-rhyper(length(z_emp),23,12,2)
z_teo<-rstable(length(z_emp),alpha = 1.75, beta= skewness(z_emp), gamma=0.0035, delta=0) #OK


ks.test(z_teo,z_emp,exact=NULL)
plot(ecdf(x = z_emp), main = "ECDF of x and y")
lines(ecdf(x = z_teo), col = 2)

plot(density(z_teo))
lines(density(z_emp), col = 2)
