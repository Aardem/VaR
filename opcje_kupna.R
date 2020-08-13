library(ggplot2)

S0<-100
K<-100
r<-0.02
t<-0.25
sigma<-0.2

d1<-(log(S0/K)+(r+0.5*(sigma^2))*t)/(sigma*(t^(1/2)))
d2<-d1-(sigma*sqrt(t))

c <- S0*pnorm(d1)-((K*(exp(-r*t)))*pnorm(d2))

#zmieniana sigma
j1<-seq(0.16,0.24,by=0.001)
c_v<-NULL

for (i1 in j1) {
  sigma=i1
  d1<-(log(S0/K)+(r+0.5*(sigma^2))*t)/(sigma*(t^(1/2)))
  d2<-d1-(sigma*sqrt(t))
  c <- S0*pnorm(d1)-((K*(exp(-r*t)))*pnorm(d2))
  c_v<-c(c_v,c)
}

sigma_df<-data.frame("sigma"=j1,"c"=c_v)
ggplot(sigma_df,aes(x=sigma,y=c))+
  geom_point()+
  geom_abline(intercept = -2.9,slope=21.55, col="red")

#zmieniana r
j1<-seq(0.016,0.024,by=0.001)
c_v<-NULL

for (i1 in j1) {
  r=i1
  d1<-(log(S0/K)+(r+0.5*(sigma^2))*t)/(sigma*(t^(1/2)))
  d2<-d1-(sigma*sqrt(t))
  c <- S0*pnorm(d1)-((K*(exp(-r*t)))*pnorm(d2))
  c_v<-c(c_v,c)
}

r_df<-data.frame("r"=j1,"c"=c_v)
ggplot(r_df,aes(x=r,y=c))+
  geom_point()

#zmieniane K
j1<-seq(60,140,by=1)
c_v<-NULL

for (i1 in j1) {
  K=i1
  d1<-(log(S0/K)+(r+0.5*(sigma^2))*t)/(sigma*(t^(1/2)))
  d2<-d1-(sigma*sqrt(t))
  c <- S0*pnorm(d1)-((K*(exp(-r*t)))*pnorm(d2))
  c_v<-c(c_v,c)
}

k_df<-data.frame("K"=j1,"c"=c_v)
ggplot(k_df,aes(x=K,y=c))+
  geom_point()

#zmieniane T
j1<-seq(1/12,1,by=1/12)
c_v<-NULL

for (i1 in j1) {
  t=i1
  d1<-(log(S0/K)+(r+0.5*(sigma^2))*t)/(sigma*(t^(1/2)))
  d2<-d1-(sigma*sqrt(t))
  c <- S0*pnorm(d1)-((K*(exp(-r*t)))*pnorm(d2))
  c_v<-c(c_v,c)
}

t_df<-data.frame("t"=j1,"c"=c_v)
ggplot(t_df,aes(x=t,y=c))+
  geom_point()

###wszystkie

S0<-100
K<-seq(70,130,by=10)
r<-round(seq(0.016,0.024,by=0.001),3)
t<-round(c(1/12,3/12,6/12,1),3)
sigma<-round(seq(0.14,0.26,by=0.01),2)

opcje_df<-expand.grid("cena_real"=K,"stopa_wol_ryz"=r,"czas_wyg_opcji"=t,"zmiennosc"=sigma)

for (i in 1:nrow(opcje_df)) {
  K<-opcje_df[i,1]
  r<-opcje_df[i,2]
  t<-opcje_df[i,3]
  sigma<-opcje_df[i,4]
  
  d1<-(log(S0/K)+(r+0.5*(sigma^2))*t)/(sigma*(t^(1/2)))
  d2<-d1-(sigma*sqrt(t))
  c <- S0*pnorm(d1)-((K*(exp(-r*t)))*pnorm(d2))
  
  opcje_df$opcja_kupna[i]<-c
  print(i/nrow(opcje_df))
}

ggplot(opcje_df[opcje_df$stopa_wol_ryz==0.02,],aes(x=zmiennosc,y=opcja_kupna,col=as.factor(cena_real)))+
  geom_line(size=1)+
  facet_wrap(~czas_wyg_opcji,ncol = 2)

ggplot(opcje_df[opcje_df$czas_wyg_opcji==0.25&opcje_df$zmiennosc==0.2,],aes(x=stopa_wol_ryz,y=opcja_kupna,col=as.factor(cena_real)))+
  geom_line(size=1)


#opcja sprzedazy
p <- -S0*pnorm(-d1)+((K*(exp(-r*t)))*pnorm(-d2))

S0<-100
K<-seq(70,130,by=10)
r<-round(seq(0.016,0.024,by=0.001),3)
t<-round(c(1/12,3/12,6/12,1),3)
sigma<-round(seq(0.14,0.26,by=0.01),2)

opcje_df<-expand.grid("cena_real"=K,"stopa_wol_ryz"=r,"czas_wyg_opcji"=t,"zmiennosc"=sigma)

for (i in 1:nrow(opcje_df)) {
  K<-opcje_df[i,1]
  r<-opcje_df[i,2]
  t<-opcje_df[i,3]
  sigma<-opcje_df[i,4]
  
  d1<-(log(S0/K)+(r+0.5*(sigma^2))*t)/(sigma*(t^(1/2)))
  d2<-d1-(sigma*sqrt(t))
  p <- -S0*pnorm(-d1)+((K*(exp(-r*t)))*pnorm(-d2))
  
  opcje_df$opcja_kupna[i]<-p
  print(i/nrow(opcje_df))
}

ggplot(opcje_df[opcje_df$stopa_wol_ryz==0.02,],aes(x=zmiennosc,y=opcja_kupna,col=as.factor(cena_real)))+
  geom_line(size=1)+
  facet_wrap(~czas_wyg_opcji,ncol = 2)

ggplot(opcje_df[opcje_df$czas_wyg_opcji==0.25&opcje_df$zmiennosc==0.2,],aes(x=stopa_wol_ryz,y=opcja_kupna,col=as.factor(cena_real)))+
  geom_line(size=1)
