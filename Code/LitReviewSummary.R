##############################################################################################
# This script was used to summarize key findings from the literature review:
# Here I break down the proportion of studies using the most common logger brands, Onset models, 
# the range of sampling frequencies, and mentioning shading and/or calibrating their loggers.
#
#Created by Jess S. Glanz
#Created on 16 Nov 2023
#############################################################################################

## clear workspace
rm(list=ls())


#load libraries
library(dplyr)
library(ggplot2)


#load data
lrs<-LitRevSummary

#add dummy variable
lrs$dummy<-1 

## Break down proportion of studies using most common logger brands --------------

#add column with less frequently used brands named other
lrs<- lrs %>% mutate(brand =
                     case_when(Brand == "Onset Computer Corporation" ~ "aOnset",
                               Brand == "Sea-Bird Scientific" ~ "bSea-Bird",
                               Brand == "Not mentioned" ~ "fNot Mentioned",
                               Brand == "RBR" ~ "cRBR",
                               Brand == "ReefNet" ~ "dReefNet",
                               Brand == "Aanderaa" ~ "eOther",
                               Brand == "Alec Electronics Co" ~ "eOther",
                               Brand == "Aquatec" ~ "eOther",
                               Brand == "Dataflow Systems Ltd" ~ "eOther",
                               Brand == "DTS" ~ "eOther", 
                               Brand == "Eureka Water Probes" ~ "eOther",
                               Brand == "Gemini Data Loggers Ltd" ~ "eOther",
                               Brand == "General Oceanics" ~ "eOther",
                               Brand == "Hugrun (now Star-Oddi)" ~ "eOther",
                               Brand == "Innovasea VR2Tx" ~ "eOther",
                               Brand == "Lowell Instruments" ~ "eOther",
                               Brand == "Marine Geophysics Lab" ~ "eOther",
                               Brand == "Maxim Integrated Products" ~ "eOther",
                               Brand == "Nortek" ~ "eOther",
                               Brand == "Opuhala" ~ "eOther",
                               Brand == "Ryan Industries" ~ "eOther",
                               Brand == "Precision Measurement Engineering Inc" ~ "eOther", 
                               Brand == "Royal Netherlands Institute for Sea Research" ~ "eOther",
                               Brand == "Sensornet" ~ "eOther",
                               Brand == "Teledyne Marine" ~ "eOther",
                               Brand == "U Beratherm and Grant Squirrel" ~ "eOther",
                               Brand == "Vemco" ~ "eOther",
                               Brand == "Yellow Springs Instruments" ~ "eOther"),
                   .after=Brand)

#sum number of studies using each brand 
lrs_b <- lrs %>%
  group_by(brand) %>%
  summarise(count=sum(dummy))

#convert sum into a percentage for labels on brand donut plot
lrs_b$p<-lrs_b$count/(sum(lrs_b$count))*100

#compute ymax and ymin for plot
lrs_b$fraction = lrs_b$count / sum(lrs_b$count)
lrs_b$ymax = cumsum(lrs_b$fraction)-0.01
lrs_b$ymin = c(0, head(lrs_b$ymax, n=-1))+0.01


#plot brand donut
lrs_b%>%
  ggplot(aes(fill=brand,ymax=ymax, ymin=ymin,  xmax=4, xmin=3))+
  geom_rect()+
  scale_fill_manual(name="Logger Brands",labels=c("aOnset"="Onset","bSea-Bird"="SeaBird","cRBR"="RBR","dReefNet"="ReefNet","eOther"="Other","fNot Mentioned"="Not mentioned"),values=c("#3482A4FF","#35A0ABFF", "#46BEADFF","#7ED7AFFF","#414388FF","#3A2C59FF"))+
  scale_color_manual(values=c("#3482A4FF","#35A0ABFF", "#46BEADFF","#7ED7AFFF","#414388FF","#3A2C59FF"))+
  ylim(0,1)+
  xlim(c(1, 4)) +
  coord_polar(theta="y", direction = -1) +
  theme_void() +
  geom_text( x=3.5, y=0.687695214,aes(label=paste0(61.5,"%")), color="white",size=3) +
  geom_text( x=3.5, y=0.282153652,aes(label=paste0(19.6,"%")), color="white",size=3) +
  theme(
    legend.position = c(.5, .5),
    legend.justification = c("center", "center"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2),
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 8, face = "bold"),
    legend.key.size = unit(0.4, 'cm'),
    text=element_text(size=8)
  )

## Break down proportion of studies using most common Onset models --------------

#subset data for only studies using Onset loggers
on<-lrs %>% filter(brand=="aOnset")

#make sure spelling/formatting of "Not mentioned" is consistent in the model column
on$model[grepl("Not",on$model)]<-"Not mentioned"

#add column with less frequently used Onset models labeled as "Other" and more commonly used ones with shorter, standardized names
on <- on %>% mutate(mod =
                      case_when(
                        model=="Not mentioned"~"aOther",
                        model=="HOBO DO Data Logger"~"aOther",
                        model=="Stowaway XTI"~"aOther",
                        model=="HOBO Aquapro"~"aOther",
                        model=="Water level data logger"~"aOther",
                        model=="Optic Stowaway"~"aOther",
                        model=="HOBO Stowaway Tidbit"~"aOther",
                        model=="HOBO TidbiT"~"bTidbit",
                        model=="HOBO Pro"~"ePro",
                        model=="HOBO Pendant"~"dPend",
                        model=="HOBO MXTemp"~"aOther",
                        model=="HOBO MXTemp/Light"~"aOther",
                        model=="HOBO Pendant Temp"~"dPend",
                        model=="HOBO Pendant Temp/Light"~"cPend-LT"))

#sum number of studies using each model
on_m <- on %>%
  group_by(mod) %>%
  summarise(count=sum(dummy))

#convert sum into a percentage for labels on Onset donut plot
on_m$p<-on_m$count/(sum(on_m$count))*100

#standardize percentage to the proportion of all studies using Onset
on_m$cp<-(on_m$p/100)*61.5

#compute ymax, ymin, and label position for plot
on_m$ymax = cumsum(on_m$cp)-0.01
on_m$ymin = c(0, head(on_m$ymax, n=-1))+0.01
on_m$labelPosition <- (on_m$ymax + on_m$ymin) /2

#create row for all non-Onset loggers
mod<-"nO"
count<-127
p<-38.5
cp<-38.5
ymax<-100
ymin<-61.5
labelPosition<-NA

nO<-data.frame(mod,count,p,cp,ymax,ymin,labelPosition)

#bind row with summary of Onset model data for Onset donut
on_m<-bind_rows(on_m,nO)

#compute a good label 
on_m$p<-round(on_m$p, digits = 2)#round percentage to 2 digits

on_m$label <- paste0(on_m$mod,"\n",on_m$p,"%") #"\n" put space between the plastic type and its count

#plot onset donut
on_m%>%
  ggplot(aes(fill=mod,alpha=mod,ymax=ymax, ymin=ymin,  xmax=4, xmin=3))+
  geom_rect()+
  geom_text( x=3.5, aes(y=labelPosition, label=label),color="white",size=2.6) +
  scale_fill_manual(name="Onset Models",values = c("nO"="white","aOther"="#A4E0BBFF", "ePro"="#0B0405FF","cPend-LT"= "#3482A4FF","dPend"= "#3F3770FF", "bTidbit"= "#3AAEADFF"), breaks = c("aOther","bTidbit","cPend-LT","dPend","ePro"))+
  scale_alpha_manual(values = c("nO"=0,"aOther"=1, "ePro"=1,"cPend-LT"= 1,"dPend"= 1, "bTidbit"= 1), breaks = c("aOther","bTidbit","cPend-LT","dPend","ePro"))+
  xlim(c(1, 4)) +
  coord_polar(theta="y", direction = 1) +
  theme_void() +
  guides(fill = "none",color="none",alpha="none")


## Histogram of sampling frequencies --------------

#subset data to only count studies once no matter how many different loggers they used
lrs_u <-lrs %>% 
  distinct(DOI, .keep_all = TRUE)

#subset data for only studies that reported the sampling frequency used
lrs_f<-lrs_u%>%filter(Freq1!="Not mentioned")

#convert frequency to numeric
lrs_f$Freq1<-as.numeric(lrs_f$Freq1)

#add a column for sampling frequency bins
lrs_f<- lrs_f %>% mutate(f1 =
                       case_when(
                         Freq1<=5~"<5",
                         (Freq1>5 & Freq1 <= 10)~"5-10",
                         (Freq1>10 & Freq1 <= 15)~"10-15",
                         (Freq1>15 & Freq1 <= 30)~"15-30",
                         (Freq1>30 & Freq1 <= 60)~"30-60",
                         Freq1> 60 ~"60+"
                       ))

#sum number of studies per sampling frequency bin
lrs_f2 <- lrs_f %>%
  group_by(f1) %>%
  summarise(count=sum(dummy))

#create bin labels with x and y coordinates for plot
annotation <- data.frame(
  x = c("<5","5-10","10-15","15-30","30-60","60+"),
  label = c("<5min", "5-10min","10-15min","15-30min","30-60min","60+min")
)

#plot number of studies by sampling frequency bin
lrs_f2%>%
  mutate(f1 = factor(f1, levels=c("60+","30-60","15-30","10-15","5-10","<5"))) %>%
  ggplot(aes(x=f1, y=count)) + 
  geom_bar(stat = "identity",fill="lightgrey") +
  coord_flip()+
  theme_classic()+
  xlab("Sampling Frequency")+
  ylab("Number of Studies")+
  theme( axis.text.y = element_blank(),axis.ticks.y = element_blank(),text=element_text(size=8))+
  geom_text(data=annotation, aes( x=x, y=9.55, label=label),                 
            color="black", 
            size=5 , fontface="bold" )
