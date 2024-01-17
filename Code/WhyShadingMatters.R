##############################################################################################
# This script was used to analyze the effect of shading on temperature logger measurements.
#
#Created by Wally Rich
#Created on 06 Dec 2023
#############################################################################################

## clear workspace
rm(list=ls())


###Load libraries
library(ggplot2)
library(dplyr)
library(timetk)
library(lubridate)
library(xts)

###Load data
sd<-sensor_diff


#Tidy data

sd<-sd[,-6]

sd$DateTime<-ymd_hms(sd$DateTime)#convert the date-time to date-time format

sd<-sd%>%na.omit()#remove rows with NA

sd.daytime<-xts(sd[,-1],sd$DateTime)#convert data frame into an xts object for easy time filtering

sd.daytime=sd.daytime['2023-02-04/2023-02-07'] #filter for first 4 days of deployment

sd.day=sd.daytime['T06:00/T19:00'] #subset for daytime values only


sd.day=as.data.frame(sd.day)#convert from an xts object back to a data frame

#convert data frame to long format
sd.long=pivot_longer(data=sd.day,
                              cols=c(5:ncol(sd.day)),
                              names_to='logger',
                              values_to='temp')

#convert temp and irradiance to numeric
sd.long$temp=as.numeric(sd.long$temp)
sd.long$irradiance_avg=as.numeric(sd.long$irradiance_avg)

sd.long$logger=gsub("_.*","",sd.long$logger) #combine all loggers to same treatment for plotting boxplots/jitters
sd.long$logger=factor(sd.long$logger,levels=c('Odyssey','Nat','Tidbit','Minidot','Pro','PendantLT','PendantMX','Pendant'))  

sd.long<-sd.long%>%na.omit()

#for consistent plot themes
newtheme <- theme_classic() + theme(text = element_text(size=14,family="Arial"))+
  theme(axis.text.x = element_text(size=14,colour="black"), axis.text.y = element_text(size=14,colour="black"))+
  theme(plot.margin = unit(c(5.5,5.5,5.5,20), "pt"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_blank())+theme()

#Figure 2b
ggplot(sd.long[sd.long$Shade=='Unshaded'&sd.long$Depth=='Shallow',])+
  geom_jitter(aes(x=logger,y=temp,color=irradiance_avg))+
  scale_color_viridis(option='mako')+
  scale_y_continuous(limits = c(-1,2.3),breaks=seq(-1,2,1))+
  scale_x_discrete(labels=c('Odyssey','Star-Oddi','Tidbit','Minidot','Pro','Pendant LT','Pendant MX','Pendant'))+
  ylab('Mean offset from SBE-56 (°C)')+
  geom_hline(yintercept=0,linetype='dashed',color='black')+
  newtheme+
  theme(legend.position = c(0.03, 0.95),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(2, 2, 2, 2),
        axis.text.x=element_text(angle=45,hjust=1),
        axis.title.x = element_blank())+
  labs(color=expression(paste("PAR (µmol photons ", m^-2, s^-1,")")))



###Plot (2a) time series with largest deviation as shaded ribbon above seabird time series line
#Load data
sdata<-sensor_data

#Tidy data
sdata$DateTime<-ymd_hms(sdata$DateTime)#convert the date-time to date-time format

sdata<-sdata%>%na.omit()#remove rows with NA

sdata.daytime<-xts(sdata[,-1],sdata$DateTime)#convert data frame into an xts object for easy time filtering

sdata.daytime=sdata.daytime['2023-02-04/2023-02-07'] #filter for first 4 days of deployment

sdata.daytime<-as.data.frame(sdata.daytime) #convert xts object back into a data frame
sdata.daytime$DateTime<-ymd_hms(sdata.daytime$DateTime) #convert date and time back into date-time format

#convert the temp measured by the seabird and pendant 3 into numeric
sdata.daytime$Seabird_avg<-as.numeric(sdata.daytime$Seabird_avg) 
sdata.daytime$Pendant_3<-as.numeric(sdata.daytime$Pendant_3)

#Figure 2a
sdata.daytime%>%
  filter(Shade=="Unshaded")%>%
  ggplot(aes(DateTime,Seabird_avg))+
  geom_ribbon(aes(ymin=Seabird_avg, ymax=Pendant_3),fill="#43BBADFF")+
  geom_path(color="#0B0405FF")+
  #facet_wrap(~date(dt),nrow=1,scales="free_x")+
  newtheme+
  xlab("")+
  ylab("Temperature (°C)")+
  theme(text=element_text(size=8),
        panel.grid = element_blank(),
        strip.background = element_blank())


show_col(viridis_pal(option="mako")(12))
