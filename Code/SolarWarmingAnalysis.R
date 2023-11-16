##############################################################################################
# This script was used to analyze the warming effect of solar radiation in the February deployment.
#
#Created by Mike Fox and Jess Glanz
#Created on 16 Nov 2023
#############################################################################################

## clear workspace
rm(list=ls())


#load libraries
library(viridis)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(purrr)
library(broom)
library(chron)


#load data
dat<-sensor_diff

#rename columns with more than one underscore and convert data frame to long format
dat2<-dat %>%
  select(2:33)%>%
  dplyr::rename(MXLT_1="PendantMX_LT_1",
                MXLT_2="PendantMX_LT_2",
                MXLT_3="PendantMX_LT_3") %>%
  pivot_longer(Nat_1:Tidbit_3,names_to="Logger",values_to="Diff") %>% 
  separate(Logger,sep="_",into=c("Model","Rep"))


#summarize mean difference from the seabird across reps by each logger type/brand for only the shallow deployment
dat_sum<-dat2 %>% filter(Depth=="Shallow") %>% group_by(Model,DateTime,irradiance_avg,Shade) %>% 
  summarize(mean_diff=mean(Diff,na.rm=T)) %>% ungroup()


#create date time 
dat_sum<-dat_sum %>% mutate(Date=as_datetime(dat_sum$DateTime))

#and truncate based on daylight hours @ deployment time (7am to 6pm)
trimmed<-dat_sum %>% filter(hour(Date) >=06 & hour(Date) <= 19) # trim to 6am to 7pm - 1hr before and after sunset

### AND trim to first four days of deployment to avoid potential of biofouling on irradiance sensor 
trimmed<-trimmed %>% filter(Date <mdy("02-08-2023"))

#### now do linear regressions for avg offsed for all shade treatments 
solar.warming<- trimmed %>% group_by(Model,Shade) %>% 
  nest() %>% 
  mutate(
    fit = purrr::map(data, ~ lm(mean_diff ~ irradiance_avg, data = .x)),
    tidied = purrr::map(fit, tidy),
    rsq =purrr::map(fit,glance) #pull model performance metrics but only want rsq
  ) %>% 
  unnest(tidied,rsq)

#drop all the other metrics we don't want ---
#########
#########          This is what is in supplemental table
supp.table<-solar.warming %>% dplyr::select(-c(data,fit,adj.r.squared:nobs))
#########
#########

#extract intercepts (heating per unit irradiance) for only unshaded treatments
intercept.warming<-solar.warming %>% filter(Shade=="Unshaded" &term!="irradiance_avg") %>% 
  select(estimate,std.error)

#extract slopes (heating per unit irradiance) for only unshaded treatments
slope.warming<-solar.warming %>% filter(Shade=="Unshaded" &term=="irradiance_avg") %>% 
  select(-data,-fit)

#merge slope and intercept dataframes
slopes.ordered<-merge(slope.warming,intercept.warming,by=c("Model","Shade"))

#order by slope
slopes.ordered<-slopes.ordered %>% arrange(estimate.x)

#convert model into a factor variable
slopes.ordered$Model<-as.factor(slopes.ordered$Model)

#format slope
slopes.ordered$estimate<-format(slopes.ordered$estimate, scientific = FALSE)

#set order of logger models
slopes.ordered$Model<-ordered(slopes.ordered$Model,levels=c("Odyssey","Nat", "Minidot","Tidbit","Pro","PendantLT",
                               "Pendant","MXLT"),
        labels= c("Odyssey","Star-Oddi", "PME-MiniDot","Onset-Tidbit",
                  "Onset-Pro","Onset-PendantLT","Onset-Pendant","Onset-PendantMX"))

#calculate PAR at which diff=0.5 -- all y-intercepts at 0 so the equation is
# 0.5-intercept (estimate.y)/slope (estimate.x)
slopes.ordered<-slopes.ordered %>% mutate(Threshold=(0.5-estimate.y)/as.numeric(estimate.x))
slopes.ordered<-slopes.ordered %>% mutate(Threshold.plot=ifelse(Threshold<1000,Threshold,NA))

#order the models based on slopes from least to greatest
trimmed$Model<-ordered(trimmed$Model,levels=c("Odyssey","Nat", "Minidot","Tidbit","Pro","PendantLT",
                                              "Pendant","MXLT"),
                       labels= c("Odyssey","Star-Oddi", "PME-MiniDot","Onset-Tidbit",
                                 "Onset-Pro","Onset-PendantLT","Onset-Pendant","Onset-PendantMX"))


#Plot and add cut off line for irradiance value at which avg offset >0.5C for shallow loggers
f3<-ggplot(trimmed,aes(irradiance_avg,mean_diff,group=Shade,fill=Shade,col=Shade))+
  geom_point(alpha=0.5,pch=21,col='black')+
  geom_smooth(method="lm",se=TRUE)+
  facet_wrap(~Model,nrow=2)+geom_hline(yintercept = 0,lty=2)+
 scale_fill_manual(name="",values=c("#366A9FFF","#A0DFB9FF"))+
  scale_color_manual(name="",values=c("#366A9FFF","#A0DFB9FF"))+
 geom_vline(data=slopes.ordered,mapping=aes(xintercept=Threshold.plot,group=Model),lty=2)+
  geom_text(data=slopes.ordered,mapping=aes(x=Threshold.plot+50,y=-.5,
                      label=paste("0.5°C bias =",round(Threshold.plot,0), "PAR"),hjust=0),
                      parse=FALSE,size=1.75,col='black')+
  ylab("Mean Offset from SBE-56 (°C)")+
  xlab(expression(paste("PAR µmol ", m^{-2},s^-1)))+theme_classic()+
  theme(text=element_text(size=8),
        panel.grid = element_blank(),
        legend.position="top")
        #strip.background = element_blank())

#save plot
ggsave(
  "fig3.pdf",
  plot = f3,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 600) 

###FIGURE CAPTION

#Fig XX. The effect of solar heating on measurement bias varies
#       across commonly used temperature loggers and is linearly related to irradiance.
#       Each panel represents the 
#       average deviation of three replicate loggers from the mean temperature 
#       recorded by three SBE-56s during a four-day deployment in Feb 2022. In all
#       cases shading reduced the effect of solar heating and reduced measurement variability.
#       Irradiance threshold were estimated to identify irradiance values above which
#       temperature bias of 0.5°C can be expected (vertical dashed lines). Horizontal dashed
#       lines represent 0 or no difference from SBE-56 temperature. 


## Determine % reduction in light between depths ----------

#subset shallow and deep data
shall<-sensor_diff %>%filter(Depth=="Shallow")
deep<-sensor_diff %>%filter(Depth=="Deep")

#truncate deep data to first few hours to avoid when biofouling occurred
deep1<- deep %>%select(2,5)%>%filter(DateTime %>% between_time("2023-02-10 12:00:00", "2023-02-10 15:00:00"))

#truncate shallow data to comparable time period
shall1<-shall %>% select(2,5) %>% filter(DateTime %>% between_time("2023-02-09 12:00:00", "2023-02-09 15:00:00"))

#rename light column to reflect depth
colnames(shall1)[2]<-'Shallow'
colnames(deep1)[2]<-'Mid'

#separate date and time into two separate columns
shall2<-separate(shall1, DateTime, c("date", "time"), sep = " ")
deep2<-separate(deep1, DateTime, c("date", "time"), sep = " ")

#convert time to time format
shall2$time<-chron(times=shall2$time)
deep2$time<-chron(times=deep2$time)

#join shallow and deep data by time
sdl1<-left_join(shall2,deep2,by="time")

#compute reduction percentage between light at deep vs light at shallow
sdl1$red<-sdl1$Mid/sdl1$Shallow

#average percentage reduction
mean(sdl1$red)
