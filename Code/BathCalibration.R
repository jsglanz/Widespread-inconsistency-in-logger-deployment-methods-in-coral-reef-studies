##############################################################################################
# This script was used to analyze the logger offset from the true calibration bath temperature.
#
#Created by Jess Glanz
#Created on 06 Dec 2023
#############################################################################################

## clear workspace
rm(list=ls())

###Load libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(timetk)


#Load data
obc<-OriginalBathCal
ebc<-ElecBlueCalBath

#remove empty rows at bottom
obc<-obc[c(1:78),]


#Convert both datasets from wide to long format 
obcl<-obc%>%select(1:5,8:34)%>%gather(key=Logger,value=temp,6:32)
ebcl<-ebc%>%select(1:3,6:12)%>%gather(key=Rep,value=temp,4:10)%>%mutate(Logger="Electric Blue")

#Calculate the difference between the logger reading and the true bath temp
obcl$diff<-obcl$temp-obcl$Instantaneous
ebcl$diff<-ebcl$temp-ebcl$Instantaneous

#Separate "Logger" name column into "logger" and "rep" columns
obcl<-separate(data = obcl,
              col = Logger,
              into = c("Logger", "Rep"),
              sep = "\\_") #Separate Logger column into three columns based on underscore symbol

#Convert date-time into date-time format in both datasets
obcl$dt<-mdy_hm(obcl$DateTime)
ebcl$dt<-mdy_hm(ebcl$date)

#Subset data and summarize the mean and range of offset from true temp
#without the Minidot
obcl_nomd<-obcl%>% filter(Logger!="Minidot" & T %in% c("Start","5","Mid","End"))%>%group_by(T,Logger)%>%summarise(mean=mean(diff),min=min(diff),max=max(diff))

#with only the Minidot, truncated to its operational maximum temp
obcl_ymd<-obcl%>% filter(Logger=="Minidot" & tf<=35 & T %in% c("Start","5","Mid","End"))%>%group_by(T,Logger)%>%summarise(mean=mean(diff),min=min(diff),max=max(diff))

#for the electric blue loggers
ebcl_s<-ebcl%>% filter(T %in% c("Start","5","Mid","End"))%>%group_by(T,Logger)%>%summarise(mean=mean(diff),min=min(diff),max=max(diff))



#Bind each summary dataset from above
bcl_c<-bind_rows(obcl_nomd,obcl_ymd,ebcl_s)

#Rename "5" in the timepoint colunn
bcl_c$T[bcl_c$T=="5"]<-"R5"

#Calculate range
bcl_c$range<-bcl_c$max-bcl_c$min

#Subset original dataset like above
bcl_nom<-obcl%>%filter(Logger!="Minidot") #everything but the minidot

bcl_md<-obcl%>%filter(Logger=="Minidot"&tf<=35) #only the minidot with temps up to 35

#merge all original datasets together
all_bc<-bind_rows(bcl_nom,bcl_md,ebcl)

#Calculate the RMSE, mean, and sd for each logger at each timepoint during the bath calibration for ranking in plot and supplement
bc_rmsemsd<-all_bc%>%
  filter(T %in% c("Start","5","Mid","End"))%>%
  group_by(Logger,T)%>%
  summarise(RMSE=Metrics::rmse(temp,Instantaneous),Mean=mean(diff),SD=sd(diff))



##Plot the data
bcp<-bcl_c%>%
  ggplot(aes(y=Logger))+
  geom_vline(xintercept=0,linetype="dashed",size=0.25)+
  geom_linerange(aes(y = Logger,
                     xmin = min,
                     xmax = max,
                     color=T),size=0.75,position = position_dodge2(width = 0.75))+
  geom_point(aes(x=mean,color=T),position=ggstance::position_dodgev(height=0.75),size=0.85)+
  scale_color_manual(name = "Timepoint",values = c("End"="#312142FF","Mid" = "#414388FF", "R5" = "#5ACCADFF","Start"="#A4E0BBFF"),labels=c("30min","15min","5min","Start"))+
  theme_classic()+
  xlab("Deviation from Calibration Temperature (°C)")+
  ylab("")+
  xlim(-1.665,1.66)+
  scale_y_discrete(limits = c("Odyssey","Electric Blue","Pendant","Tidbit","Minidot","PendantLT","PendantMXLT","Pro","Nat", "Seabird"),labels=c("Odyssey","Electric Blue", "Pendant","Tidbit","MiniDot","Pendant LT","Pendant MX LT","Pro","Star-Oddi","Sea-Bird"), guide = guide_axis(angle = 45))+
  # scale_y_discrete(limits = c("Odyssey","PendantMXLT","Tidbit","Minidot","Pro","PendantLT","Pendant","Nat", "Seabird"),labels=c("Odyssey","PendantLT_MX","Tidbit","Minidot","Pro","Pendant_LT","Pendant","Star Oddi","Seabird"), guide = guide_axis(angle = 45))+
  theme(
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "left",
    legend.margin = margin(2, 2, 2, 2),
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 8, face = "bold"),
    legend.key.size = unit(0.2, 'cm'),
    text=element_text(size=8))+
  guides(color = guide_legend(reverse=TRUE))   



##Save the plot
ggsave(
  "bathcalplot.pdf",
  plot = bcp,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 3,
  height = 3,
  units = "in",
  dpi = 600) 

