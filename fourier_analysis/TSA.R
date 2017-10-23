#fft of eirgrid and weather data
rm(list = ls())
library(lubridate)
library(plotly)
library(ggplot2)
library(tidyverse)
library(mice)
library(tidyr)
library(reshape2)
library(cowplot)

wave_gen=function(amp=1,f=1,t=seq(0,10,length.out = 100),noise_amp=0){
  noise=noise_amp*runif(n=length(t),min=-1,max=1)
  y=(amp*sin(t*2*pi*f))+noise
  return(y)
}
FFT_test=function(amps,fqs,t_vec,noise_amp=0,plot_out=F){
  library(ggplot2)
  waves=matrix(ncol = length(amps),nrow  = length(t_vec))
  sampling=diff(t_vec)[1]
  
  for(n in seq(length(amps))){
    waves[,n]=wave_gen(amp=amps[n],f = fqs[n],t=t_vec,noise_amp)
  }
  
  waves=as.data.frame(waves)
  w_names=""
  
  for(n in seq(length(fqs))){
    w_names[n]=paste0("wave_",n)
  }
  names(waves)=w_names
  
  ggwaves=cbind(waves,t_vec)
  ggwaves$wave_all=rowSums(waves)
  
  p1=ggplot(data=ggwaves,aes(x=t_vec,y=wave_all)) + geom_line()
  
  melt_waves = melt(ggwaves, id = "t_vec")
  p2=ggplot(melt_waves %>% filter(variable!="wave_all") , aes(x = t_vec, y = value, group = variable, colour = variable)) +  
    geom_line()
  
  
  
  N=length(ggwaves$wave_all)
  fft_y=fft(ggwaves$wave_all)
  fft_y=abs(fft_y[1:floor(N/2)])
  fft_y=fft_y/length(t_vec)*2 #amplitude correction
  
  
  nyquist=0.5
  freq=1/sampling*(1:(N/2))/(N/2)*nyquist #sampling adjustment applied
  
  FFT_DF=data.frame(freq,fft_y)
  
  f1=ggplot(FFT_DF,aes(freq,fft_y))+geom_line()
  f2=ggplot(FFT_DF,aes((1/freq),fft_y))+geom_line()+scale_x_log10()
  
  if(plot_out){
    print(plot_grid(p2,p1,f1,f2,nrow = 2))
  }
  
  
  return(FFT_DF)
}

FFT_filter=function(amps,fqs,t_vec,noise_amp=0,thresh=1){
  library(ggplot2)
  waves=matrix(ncol = length(amps),nrow  = length(t_vec))
  sampling=diff(t_vec)[1]
  
  for(n in seq(length(amps))){
    waves[,n]=wave_gen(amp=amps[n],f = fqs[n],t=t_vec,noise_amp)
  }
  
  waves=as.data.frame(waves)
  w_names=""
  
  for(n in seq(length(fqs))){
    w_names[n]=paste0("wave_",n)
  }
  names(waves)=w_names
  
  ggwaves=cbind(waves,t_vec)
  ggwaves$wave_all=rowSums(waves)
  
  p1=ggplot(data=ggwaves,aes(x=t_vec,y=wave_all)) + geom_line()
  
  melt_waves = melt(ggwaves, id = "t_vec")
  p2=ggplot(melt_waves %>% filter(variable!="wave_all") , aes(x = t_vec, y = value, group = variable, colour = variable)) +  
    geom_line()
  
  print(plot_grid(p2,p1,nrow = 2))
  
  N=length(ggwaves$wave_all)
  fft_o=fft(ggwaves$wave_all)
  fft_y=fft(ggwaves$wave_all)
  
  
  
  fft_y=fft(ggwaves$wave_all)
  fft_y=abs(fft_y[1:floor(N/2)])
  fft_y=fft_y/length(t_vec)*2 #amplitude correction
  
  
  nyquist=0.5
  freq=1/sampling*(1:(N/2))/(N/2)*nyquist #sampling adjustment applied
  
  FFT_DF=data.frame(freq,fft_y)
  
  f1=ggplot(FFT_DF,aes(freq,fft_y))+geom_line()
  f2=ggplot(FFT_DF,aes((1/freq),fft_y))+geom_line()+scale_x_log10()
  
  
  peaks_max=max(abs(fft_o))
  peak_locs=which(abs(fft_o)>peaks_max*thresh)
  #peak_locs=c(peak_locs,peak_locs+1,peak_locs-1)
  ff_filt=rep(0,N)
  ff_filt[peak_locs]=1
  a_filt=fft_o*ff_filt
  a_inv=fft(a_filt,inverse = TRUE)/N
  
  par(mfrow=c(2,2))
  plot(ggwaves$wave_all,type = "l",main = "original")
  plot(abs(fft_o),type = "l",main = "FFT")
  plot(abs(a_filt),type = "l",main = "Filtered FFT")
  plot(Re(a_inv),type = "l",main = "Reconstructed")
  par(mfrow=c(1,1))
  
  
  
  #print(plot_grid(p2,p1,f1,f2,nrow = 2))
  
  
  return(FFT_DF)
}
FFT_auto=function(Data_in,t_vec_in,amps,fqs,t_vec,noise_amp=0){
  library(ggplot2)
  # waves=matrix(ncol = length(amps),nrow  = length(t_vec))
  # 
  # for(n in seq(length(amps))){
  #   waves[,n]=wave_gen(amp=amps[n],f = fqs[n],t=t_vec,noise_amp)
  # }
  # 
  # waves=as.data.frame(waves)
  # w_names=""
  # 
  # for(n in seq(length(fqs))){
  #   w_names[n]=paste0("wave_",n)
  # }
  # names(waves)=w_names
  # 
  # ggwaves=cbind(waves,t_vec)
  # ggwaves$wave_all=rowSums(waves)
  # 
  # p1=ggplot(data=ggwaves,aes(x=t_vec,y=wave_all)) + geom_line()
  # 
  # melt_waves = melt(ggwaves, id = "t_vec")
  # p2=ggplot(melt_waves %>% filter(variable!="wave_all") , aes(x = t_vec, y = value, group = variable, colour = variable)) +
  #   geom_line()
  # 
  # ggwaves$wave_all=Data_in
  
  sampling=as.numeric(mean(diff(t_vec_in),na.rm = T))
  N=length(Data_in)
  
  fft_y=fft(Data_in)
  fft_y=abs(fft_y[1:floor(N/2)])
  fft_y=fft_y/length(t_vec_in)*2 #amplitude correction
  
  
  nyquist=0.5
  freq=1/sampling*(1:(N/2))/(N/2)*nyquist #sampling adjustment applied
  
  FFT_DF=data.frame(freq,fft_y)
  
  f1=ggplot(FFT_DF,aes(freq,fft_y))+geom_line()
  f2=ggplot(FFT_DF,aes((1/freq),fft_y))+geom_line()+scale_x_log10()
  
  print(plot_grid(f1,f2,nrow = 2))
  
  
  return(FFT_DF)
}



t_vec=seq(0,2, by =  .001)
amps=c(10,10,8,6,4,3)
fqs=c(20,10,30,40,50,90)
#fqs=seq(2,100,by=2)
#amps=rep(10,length.out = length(fqs))
noise=0
waves=matrix(ncol = length(amps),nrow  = length(t_vec))

for(n in seq(length(amps))){
  waves[,n]=wave_gen(amp=amps[n],f = fqs[n],t=t_vec,noise)
}

# plot(t_vec,waves[,1],type = "l",xlim = c(0,2))
# plot(t_vec,waves[,2],type = "l",xlim = c(0,2))
# plot(t_vec,waves[,3],type = "l",xlim = c(0,2))
# plot(t_vec,rowSums(waves),type = "l",xlim = c(0,2))


fftdf=FFT_test(amps,fqs,t_vec,noise)
ggplotly(ggplot(fftdf,aes(freq,fft_y))+geom_line())

thresh=(5*sd(fftdf$fft_y))/max(fftdf$fft_y)

FFT_filter(amps,fqs,t_vec,noise,thresh)









elec=read.csv("c:/Patrick/DataScience/TimeSeries/system_combined.csv")
elec$Time=as.POSIXct(strptime(elec$Time, "%Y-%m-%d %H:%M:%S"))

summary(elec)
dim(elec)[1]

p = ggplot(sample_n(elec,5000,replace = F), aes(x = Time, y=System.Demand.MW))+ geom_point()
p
ggplotly(p)

elec_2014 = elec %>% filter(Time >= as.Date("2014-01-01") & Time <= as.Date("2014-12-31") )
p = ggplot(sample_n(elec_2014,5000), aes(x = Time, y=System.Demand.MW))+ geom_point()
ggplotly(p)

elec_2014_jan = elec %>% filter(Time >= as.Date("2014-01-01") & Time <= as.Date("2014-01-31") )
p = ggplot(elec_2014_jan, aes(x = Time, y=System.Demand.MW))+  geom_line()
ggplotly(p)


#imputation and fix missing data
#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/



########app start
#fft_data=fft(elec$System.Demand.MW)
#saveRDS(fft_data,"C:/Patrick/DataScience/TimeSeries/FFT_elec.RDS")

plot(elec$Time[1:200000],(elec$System.Demand.MW[1:200000]),type = "l")
elec$System.Demand.MW=elec$System.Demand.MW-mean(elec$System.Demand.MW)
fft_EirGrid=FFT_auto(Data_in = elec$System.Demand.MW[1:200000], t_vec_in = elec$Time[1:200000])
ggplotly(ggplot(fft_EirGrid,aes(freq,fft_y))+geom_line()+labs(x="cycles per min",y="power/MW"))
ggplotly(ggplot(fft_EirGrid,aes(1/freq,fft_y))+geom_line()+labs(x="min per cycle",y="power/MW"))
###########
fft_EirGrid_peri=fft_EirGrid
fft_EirGrid_peri$freq=1/fft_EirGrid_peri$freq/60

ggplotly(ggplot(fft_EirGrid_peri,aes(freq,fft_y))+geom_line()+
           labs(x="hour per cycle",y="power/MW"))

ggplotly(ggplot(fft_EirGrid_peri[250:100000,],aes(10^freq,fft_y))+geom_line()+
           labs(x="hour per cycle",y="power/MW")+
           scale_x_log10())
########
fft_EirGrid_peri=fft_EirGrid
fft_EirGrid_peri$freq=1/fft_EirGrid_peri$freq/(60*24*7)

ggplotly(ggplot(fft_EirGrid_peri[2:100000,],aes(freq,fft_y))+geom_line()+
           labs(x="weeks per cycle",y="power/MW")+
           scale_x_log10())

p <- plot_ly(fft_EirGrid_peri, x = ~freq, y = ~fft_y,type = "scatter", mode="lines") %>%
  layout(title= "peridogram",xaxis = list(title = "weeks per cycle",type="log"))
p

fft_data=readRDS("C:/Patrick/DataScience/TimeSeries/FFT_elec.RDS")
fft_data=fft_data[-1]

N=length(fft_data)
fft_data.processed=abs(fft_data[1:floor(N/2)])
nyquist=0.5
freq=(1:(N/2))/(N/2)*nyquist

FFT_DF=data.frame(freq,fft_data.processed)

plot(freq,fft_data.processed,xlab = 'freq/cycles per 15min',type="l")
plot(1/freq,fft_data.processed,ylab='power',xlab = 'freq[15mins per cycle]',log = 'x',main="system demand spectrogram",type='o')

f1=ggplot(FFT_DF,aes(freq*4*365,fft_data.processed))+
  geom_line()+
  xlab('freq[cycles per year]')
ggplotly(f1)

f2=ggplot(FFT_DF,aes((1/freq)/4,fft_data.processed))+
  geom_line()+
  ylab("power")+
  xlab("freq[hours per cycle]")+
  scale_x_log10()+
  ggtitle("system demand spectrogram")

ggplotly(f2)

h=qplot(App_timeVec,App_datavec,type='l',main='OpsDaily App_start',ylab='application start')
h+geom_line()










#########air passangers###########
flights2=AirPassengers
flights2.df=data.frame(passangers=as.numeric(flights2))

flights2.df$time=seq(ISOdate(1949,1,1),by="month",length.out = 144)
p1=ggplot(flights2.df,aes(time,passangers))+
  geom_line()+
  ylab("passangers")+
  xlab("time[years]")+
  #scale_x_log10()+
  ggtitle("system demand spectrogram")

ggplotly(p1)

##fft
fft_data=fft(flights2.df$passangers)
fft_data=fft_data[-1]
N=length(fft_data)

fft_data.processed=abs(fft_data[1:floor(N/2)])
nyquist=0.5
freq=(1:(N/2))/(N/2)*nyquist

FFT_DF=data.frame(freq,fft_data.processed)

plot(freq,fft_data.processed,xlab = 'freq[cycles per month]')
plot(1/freq,fft_data.processed,ylab='power',xlab = 'period[months per cycle]',log = 'x',main="system demand spectrogram",type='o')

f1=ggplot(FFT_DF,aes(freq,fft_data.processed))+
  geom_line()+
  xlab('freq[cycles per month]')
ggplotly(f1)

f2=ggplot(FFT_DF,aes((1/freq),fft_data.processed))+
  geom_line()+
  ylab("power")+
  xlab("period[moths per cycle]")+
  #scale_x_log10()+
  ggtitle("system demand spectrogram")

ggplotly(f2)


############## nottem data
#######Average Monthly Temperatures at Nottingham, 1920-1939##########
temps=nottem

temps.df=data.frame(temps_data=as.numeric(temps))
temps.df$time=seq(ISOdate(1920,1,1),by="month",length.out = 240)

p1=ggplot(temps.df,aes(time,temps_data))+
  geom_line()+
  ylab("temperature")+
  xlab("time[years]")+
  #scale_x_log10()+
  ggtitle("system demand spectrogram")

ggplotly(p1)

##fft
fft_data=fft(temps.df$temps_data)
fft_data=fft_data[-1]
N=length(fft_data)

fft_data.processed=abs(fft_data[1:floor(N/2)])
nyquist=0.5
freq=(1:(N/2))/(N/2)*nyquist


FFT_DF=data.frame(freq,fft_data.processed)

plot(freq,fft_data.processed,xlab = 'freq/cycles per month')
plot(1/freq,fft_data.processed,ylab='power',xlab = 'period[months per cycle]',log = 'x',main="system demand spectrogram",type='o')

f1=ggplot(FFT_DF,aes(freq,fft_data.processed))+
  geom_line()+
  xlab('freq[cycles per month]')
ggplotly(f1)

f2=ggplot(FFT_DF,aes((1/freq),fft_data.processed))+
  geom_line()+
  ylab("power")+
  xlab("period[months per cycle]")+
  #scale_x_log10()+
  ggtitle("system demand spectrogram")

ggplotly(f2)


#











#FFT script
#convert relvent colum to date
#obtain data.opsdaily from running Testconnectandextract.r
rm(list=ls())
library(plyr)

activity=read.csv('C:/PPC/DataSnapShot/activity.csv')
activity.at2=activity[activity$Activity_Type_ID==2,]
activity_type=read.csv('C:/PPC/DataSnapShot/activity_type.csv')

data.OpsDaily=read.csv("C:/PPC/DataSnapShot/OpsDailyReport.csv")


data.OpsDaily$App_Start
data.OpsDaily$Addl_Info_Complete
data.OpsDaily$Basic_Info_Complete

data.OpsDaily$App_Start=as.Date(data.OpsDaily$App_Start)
data.OpsDaily$Addl_Info_Complete=as.Date(data.OpsDaily$Addl_Info_Complete)
data.OpsDaily$Basic_Info_Complete=as.Date(data.OpsDaily$Basic_Info_Complete)



App_datavec=(count(data.OpsDaily$App_Start))
bas_datavec=(count(data.OpsDaily$Basic_Info_Complete))
comp_datavec=(count(data.OpsDaily$Addl_Info_Complete))

App_datavec=na.omit(App_datavec)
bas_datavec=na.omit(bas_datavec)
comp_datavec=na.omit(comp_datavec)

#make sequnce of dates, time vector and make a date frame
time_vec=seq((App_datavec$x[1]),App_datavec$x[nrow(App_datavec)],'day')
time_vec=data.frame(time_vec)
names(time_vec)[1]='x'

time_vec_comp=seq((comp_datavec$x[1]),comp_datavec$x[nrow(comp_datavec)],'day')
time_vec_comp=data.frame(time_vec_comp)
names(time_vec_comp)[1]='x'

time_vec_bas=seq((bas_datavec$x[1]),bas_datavec$x[nrow(bas_datavec)],'day')
time_vec_bas=data.frame(time_vec_bas)
names(time_vec_bas)[1]='x'

#join with App_datavec and make na's 0
App_datavec=(merge(time_vec,App_datavec,by ='x',all.x  =TRUE))
App_datavec[is.na(App_datavec)]=0
App_timeVec=App_datavec$x
App_datavec=App_datavec$freq

comp_datavec=(merge(time_vec_comp,comp_datavec,by ='x',all.x  =TRUE))
comp_datavec[is.na(comp_datavec)]=0
comp_timeVec=comp_datavec$x
comp_datavec=comp_datavec$freq

bas_datavec=(merge(time_vec_bas,bas_datavec,by ='x',all.x  =TRUE))
bas_datavec[is.na(bas_datavec)]=0
bas_timeVec=bas_datavec$x
bas_datavec=bas_datavec$freq


########app start
fft_data=fft(App_datavec)
fft_data=fft_data[-1]

N=length(fft_data)
fft_data.processed=abs(fft_data[1:floor(N/2)])
nyquist=0.5
freq=(1:(N/2))/(N/2)*nyquist

plot(freq,fft_data.processed,xlab = 'freq/cycles per day')
plot(1/freq,fft_data.processed,ylab='power',xlab = 'freq[days per cycle]',log = 'x',main="OpsDaily - App_Start",type='o')

h=qplot(App_timeVec,App_datavec,type='l',main='OpsDaily App_start',ylab='application start')
h+geom_line()

#########app complete
fft_data=fft(comp_datavec)
fft_data=fft_data[-1]

N=length(fft_data)
fft_data.processed=abs(fft_data[1:floor(N/2)])
nyquist=0.5
freq=(1:(N/2))/(N/2)*nyquist


plot(freq,fft_data.processed,xlab = 'freq/cycles per day')
plot(1/freq[100:1418],fft_data.processed[100:1418],ylab='power',xlab = 'freq[days per cycle]',log = 'x',main="OpsDaily - addl info complete",type='o')


h=qplot(App_timeVec,App_datavec,type='l',main='OpsDaily App_start',ylab='application start')
h+geom_line()


#########app basic
fft_data=fft(bas_datavec)
fft_data=fft_data[-1]

N=length(fft_data)
fft_data.processed=abs(fft_data[1:floor(N/2)])
nyquist=0.5
freq=(1:(N/2))/(N/2)*nyquist


plot(freq,fft_data.processed,xlab = 'freq/cycles per day')
plot(1/freq[50:1418],fft_data.processed[50:1418],ylab='power',xlab = 'freq[days per cycle]',log = 'x',main="OpsDaily - addl info complete",type='o')


h=qplot(App_timeVec,App_datavec,type='l',main='OpsDaily App_start',ylab='application start')
h+geom_line()


