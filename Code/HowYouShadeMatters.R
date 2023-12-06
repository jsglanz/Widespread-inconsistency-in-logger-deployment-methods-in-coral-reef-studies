##############################################################################################
# This script was used to analyze different shading methods.
#
#Created by Jess Glanz
#Created on 06 Dec 2023
#############################################################################################

## clear workspace
rm(list=ls())


###Load libraries
library(ggplot2)
library(dplyr)
library(timetk)


###Load data
shade_data<-Shading_Exp_Master

#tidy date
shade_data$t<-mdy_hm(shade_data$time)

#rename "control" as "unshaded" for clarity
shade_data$treat[shade_data$treat=="control"]<-"Unshaded"

##Subset data by depth
shallow<-shade_data%>%filter(depth_type=="shallow")
mid<-shade_data%>%filter(depth_type=="middle")

#Subset for daytime on day 1
d1m<-mid%>%filter(t %>% between_time("2023-06-07 06:00:00", "2023-06-07 19:00:00"))


d1s<-shallow%>%filter(t %>% between_time("2023-06-07 06:00:00", "2023-06-07 19:00:00"))


#order treatment for plotting
d1s$treat <- ordered(d1s$treat, levels = c("SBE56", "pvc", "reflective","white","black","Unshaded"))


##Plot data
f4a<-ggplot(d1s,aes(x =temp,  fill=treat))+
  geom_density( size = 0.25, color="white",alpha = 0.85)+
  theme_classic()+
  theme(text=element_text(size=8),
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "left",
        legend.margin = margin(2, 2, 2, 2),
        legend.text      = element_text(size = 8),
        legend.title     = element_text(size = 8, face = "bold"),
        legend.key.size = unit(0.2, 'cm'))+
  scale_fill_manual(name="",values = c("SBE56" = "#0B0405FF", "pvc"="#414388FF", "reflective" = "#38639DFF", "white"="#35A1ABFF", "black"="#46BEADFF", "Unshaded"="#6CD3ADFF"),breaks=c('SBE56','pvc','reflective','white','black','Unshaded'),labels=c("SBE-56","PVC","Reflective","White","Black","Unshaded"))+
  labs(y= "Proportion of measurements", x = "Temperature (°C)")


##Save plot 4a
ggsave(
  "fig4a.pdf",
  plot = f4a,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 3,
  height = 3,
  units = "in",
  dpi = 600) 


###Compare shading methods to each other
#Remove seabird data
d1nsb<-d1s%>%filter(treat!="SBE56")

#Calculate differences between measured temp and actual temp
d1nsb$Diff<-d1nsb$temp-d1nsb$actual_temp

#Convert any negative values in the corrected PAR to 0
d1nsb$recorrected.lt[d1nsb$recorrected.lt<0]<-0

#summarize mean, standard error, max, and min offsets from actual temp by treatment and PAR
dat_sum<-d1nsb %>% group_by(treat,recorrected.lt) %>% 
  summarize(mean_diff=mean(Diff,na.rm=T),se_diff=(sd(Diff,na.rm=T)/sqrt(na.omit(length((Diff))))),max=max(Diff),min=min(Diff)) %>% ungroup()

#summarize mean offsets by treatment and 100 bins of PAR
dsb<-dat_sum %>%
  group_by(treat,cut(recorrected.lt,breaks=100,labels=FALSE)) %>%
  summarise(mean=mean(mean_diff))

#rename second column "bin"
colnames(dsb)[2]<-'bin'

#create vector with ranges of each PAR bin
labs <- levels(cut(dat_sum$recorrected.lt, 100))

#keep only minimum of each bin to plot along x-axis
low<-as.data.frame(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs))))

#add column that matches the "bin" column from the dataset called dsb and convert to numeric
low <- tibble::rownames_to_column(low, "bin")
low$bin<-as.numeric(low$bin)

#join dsb and low by the "bin" column
dsb<-left_join(dsb,low,by="bin")

#Plot the shallow data
f4b<-ggplot(dsb,aes(lower,mean,fill=treat,col=treat)) + 
  geom_point(alpha=0.5,pch=21,col='black')+
  geom_smooth(method="lm",se=TRUE)+
  geom_hline(yintercept = 0,lty=2)+
  ylim(0,2.5)+
  scale_color_manual(name="",values = c( "pvc"="#414388FF", "reflective" = "#38639DFF", "white"="#35A1ABFF", "black"="#46BEADFF", "Unshaded"="#6CD3ADFF"),breaks=c('pvc','reflective','white','black','Unshaded'),labels=c("PVC","Reflective","White","Black","Unshaded"))+
  scale_fill_manual(name="",values = c( "pvc"="#414388FF", "reflective" = "#38639DFF", "white"="#35A1ABFF", "black"="#46BEADFF", "Unshaded"="#6CD3ADFF"),breaks=c('pvc','reflective','white','black','Unshaded'),labels=c("PVC","Reflective","White","Black","Unshaded"))+
  ylab("Mean Offset from SBE-56 (°C)")+
  xlab(expression(paste("PAR µmol ", m^{-2},s^-1)))+theme_classic()+
  theme(text=element_text(size=8),
        legend.position = c(0.05, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(2, 2, 2, 2),
        legend.text      = element_text(size = 8),
        legend.title     = element_text(size = 8, face = "bold"),
        legend.key.size = unit(0.2, 'cm'))

ggsave(
  "shallowshading.pdf",
  plot = f4b,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 3,
  height = 3,
  units = "in",
  dpi = 600) 


####Repeat for deeper deployment
#Remove seabird data
d1nsb<-d1m%>%filter(treat!="SBE56")

#Calculate differences between measured temp and actual temp
d1nsb$Diff<-d1nsb$temp-d1nsb$actual_temp

#Convert any negative values in the corrected PAR to 0
d1nsb$recorrected.lt[d1nsb$recorrected.lt<0]<-0

#summarize mean, standard error, max, and min offsets from actual temp by treatment and PAR
dat_sum<-d1nsb %>% group_by(treat,recorrected.lt) %>% 
  summarize(mean_diff=mean(Diff,na.rm=T),se_diff=(sd(Diff,na.rm=T)/sqrt(na.omit(length((Diff))))),max=max(Diff),min=min(Diff)) %>% ungroup()

#summarize mean offsets by treatment and 100 bins of PAR
dsb<-dat_sum %>%
  group_by(treat,cut(recorrected.lt,breaks=100,labels=FALSE)) %>%
  summarise(mean=mean(mean_diff))

#rename second column "bin"
colnames(dsb)[2]<-'bin'

#create vector with ranges of each PAR bin
labs <- levels(cut(dat_sum$recorrected.lt, 100))

#keep only minimum of each bin to plot along x-axis
low<-as.data.frame(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs))))

#add column that matches the "bin" column from the dataset called dsb and convert to numeric
low <- tibble::rownames_to_column(low, "bin")
low$bin<-as.numeric(low$bin)

#join dsb and low by the "bin" column
dsb<-left_join(dsb,low,by="bin")

#Plot the shallow data
s4<-ggplot(dsb,aes(lower,mean,fill=treat,col=treat)) + 
  geom_point(alpha=0.5,pch=21,col='black')+
  geom_smooth(method="lm",se=TRUE)+
  geom_hline(yintercept = 0,lty=2)+
  ylim(0,2.5)+
  scale_color_manual(name="",values = c( "pvc"="#414388FF", "reflective" = "#38639DFF", "white"="#35A1ABFF", "black"="#46BEADFF", "Unshaded"="#6CD3ADFF"),breaks=c('pvc','reflective','white','black','Unshaded'),labels=c("PVC","Reflective","White","Black","Unshaded"))+
  scale_fill_manual(name="",values = c( "pvc"="#414388FF", "reflective" = "#38639DFF", "white"="#35A1ABFF", "black"="#46BEADFF", "Unshaded"="#6CD3ADFF"),breaks=c('pvc','reflective','white','black','Unshaded'),labels=c("PVC","Reflective","White","Black","Unshaded"))+
  ylab("Mean Offset from SBE-56 (°C)")+
  xlab(expression(paste("PAR µmol ", m^{-2},s^-1)))+theme_classic()+
  theme(text=element_text(size=8),
        legend.position = c(0.05, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(2, 2, 2, 2),
        legend.text      = element_text(size = 8),
        legend.title     = element_text(size = 8, face = "bold"),
        legend.key.size = unit(0.2, 'cm'))

ggsave(
  "deepshading.pdf",
  plot = s4,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 3,
  height = 3,
  units = "in",
  dpi = 600) 



