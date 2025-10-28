##########GLM RESULTS, TEMPERATURE AND PERFORATOR TOOL RELATIONSHIPS###########

## RELEASE 1.0 NOTES##

##This code provides the opportunity to determine the relationship between environmental temperature (minimum temperature of the coldest month) based on the WorldClim database and the presence/absence of needles and awls within ethnographies associated with the eHRAF World Cultures database. 

#READ IN THE FOLLOWING PACKAGES
installing all the packages below##
packages <-c('ggplot2','tidyr','dplyr') #These lines of code imports all of my packages
for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))
theme_set(theme_bw())
#install.packages("spaMM")
library("spaMM")
library(dplyr)
library(stringr)

#Read in main data table
mtable<-read.csv("C:\\Users\\mlitynsk\\OneDrive - University of Wyoming\\R for Ethno\\table1.csv")

# Fit the initial model - comparing all perforator tool ethnographic observations to MTCM data
model<- fitme(Count ~ Temperature + Matern(1|LAT+LONG),family = "binomial", data = mtable)
summary(model) #summarize the model results


##Eliminate the "Toolkit" category here
ttable <- mtable[mtable$Usage != "Toolkit" | is.na(mtable$Usage), ]
ttable$Therm_vs.Other<-NA #Add category with thermoregulation vs.alternative

###This for loop generalizes individual activity types into categories - "Thermoregulation" and "Alternative"
for (i in 1:nrow(ttable)) {
  if (is.na(ttable$Usage[i]) == T) {
    next
  }else if(ttable$Usage[i] == "Clothing" |
           ttable$Usage[i] == "Shoes" |
           ttable$Usage[i] == "Shelter" |
           ttable$Usage[i] == "Snowshoes" |
           ttable$Usage[i] == "Bedding" |
           ttable$Usage[i] == "Mats" |
           ttable$Usage[i] == "Sled" |
           ttable$Usage[i] == "Blankets "|
           ttable$Usage[i] == "Blankets") {
    ttable$Therm_vs.Other[i] <- "Thermoregulation"
  } else {
    ttable$Therm_vs.Other[i] <- "Alternative"
  }
}

###The model below examines the relationship between ethnographic needles and awls used for "alternative" reasons and MTCM data
Al_NA<-ttable[is.na(ttable$Therm_vs.Other),]
Al_Sub<-subset(ttable,Therm_vs.Other == "Alternative")
Al_Sub<-rbind(Al_Sub,Al_NA)
model<- fitme(Count ~ Temperature + Matern(1|LAT+LONG),
              family = "binomial",  # or "binomial" if Count is 0/1
              data = Al_Sub)
summary(model)


##The model below examines the relationship between MTCM and ethnographically observed needles
Needles_NA<-ttable[is.na(ttable$Tool_Type),]
Needles_Sub<-subset(ttable,Tool_Type == "needle")
Needles_Sub<-rbind(Needles_NA,Needles_Sub)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Needles_Sub,family="binomial")
summary(model)

##The model below examines the relationship between MTCM and ethnographically observed awls 
Awls_NA<-ttable[is.na(mtable$Tool_Type),]
Awls_Sub<-subset(ttable,Tool_Type == "awl")
Awls_Sub<-rbind(Awls_NA,Awls_Sub)
model<-fitme(Count~Temperature + Matern(1|LAT+LONG),data=Awls_Sub,family="binomial")
summary(model) 

##The model below examines the relationship between MTCM and only those tools associated with "thermoregulation" activities
Thermo_NA<-ttable[is.na(ttable$Therm_vs.Other),]
Thermo_Sub<-subset(ttable, Therm_vs.Other == "Thermoregulation")
Thermo_Sub<-rbind(Thermo_Sub, Thermo_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Thermo_Sub,family="binomial")
#therm_glm<-glm(Count~Temperature,data=Thermo_Sub,family="binomial")
summary(model)

###########The remaining models evaluate individual activity types#####################

###FOR ONLY TATTOOING##
Tatt_NA<-mtable[is.na(mtable$Usage),]
Tatt_Sub<-subset(mtable,Usage == "Tattooing")
Tatt_Sub<-rbind(Tatt_Sub,Tatt_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Tatt_Sub,family="binomial")
#tatt_glm<-glm(Count~Temperature,data=Tatt_Sub,family="binomial")
summary(model) 


##BASKETRY##
Bask_NA<-mtable[is.na(mtable$Usage),]
Bask_Sub<-subset(mtable,Usage == "Baskets")
Bask_Sub<-rbind(Bask_Sub,Bask_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Bask_Sub,family="binomial")
#bask_glm<-glm(Count~Temperature,data=Bask_Sub,family="binomial")
summary(model) 

##Clothing##
Cloth_NA<-mtable[is.na(mtable$Usage),]
Cloth_Sub<-subset(mtable,Usage == "Clothing")
Cloth_Sub<-rbind(Cloth_Sub,Cloth_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Cloth_Sub,family="binomial")
#cloth_glm<-glm(Count~Temperature,data=Cloth_Sub,family="binomial")
summary(model) 


##Ceremony or Ritual##
Cer_NA<-mtable[is.na(mtable$Usage),]
Cer_Sub<-subset(mtable,Usage == "Ceremony or Ritual")
Cer_Sub<-rbind(Cer_Sub,Cer_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Cer_Sub,family="binomial")
#cer_glm<-glm(Count~Temperature,data=Cer_Sub,family="binomial")
summary(model) 

##Piercing##
Pier_NA<-mtable[is.na(mtable$Usage),]
Pier_Sub<-subset(mtable,Usage == "Piercing")
Pier_Sub<-rbind(Pier_Sub,Pier_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Pier_Sub,family="binomial")
#pier_glm<-glm(Count~Temperature,data=Pier_Sub,family="binomial")
summary(model) 


##Shoes##
Shoe_NA<-mtable[is.na(mtable$Usage),]
Shoe_Sub<-subset(mtable,Usage == "Shoes")
Shoe_Sub<-rbind(Shoe_Sub,Shoe_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Shoe_Sub,family="binomial")
#shoe_glm<-glm(Count~Temperature,data=Shoe_Sub,family="binomial")
summary(model)  


##Mythology##
Myth_NA<-mtable[is.na(mtable$Usage),]
Myth_Sub<-subset(mtable,Usage == "Mythology")
Myth_Sub<-rbind(Myth_Sub,Myth_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Myth_Sub,family="binomial")
#myth_glm<-glm(Count~Temperature,data=Myth_Sub,family="binomial")
summary(model) 

##Mats##
Mat_NA<-mtable[is.na(mtable$Usage),]
Mat_Sub<-subset(mtable,Usage == "Mats")
Mat_Sub<-rbind(Mat_Sub,Mat_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Mat_Sub,family="binomial")
#mat_glm<-glm(Count~Temperature,data=Mat_Sub,family="binomial")
summary(model)

###Shelter
Shel_NA<-mtable[is.na(mtable$Usage),]
Shel_Sub<-subset(mtable,Usage == "Shelter")
Shel_Sub<-rbind(Shel_Sub,Shel_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Shel_Sub,family="binomial")
#shel_glm<-glm(Count~Temperature,data=Shel_Sub,family="binomial")
summary(model) 

###Medical
Med_NA<-mtable[is.na(mtable$Usage),]
Med_Sub<-subset(mtable,Usage == "Medical")
Med_Sub<-rbind(Med_Sub,Med_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Med_Sub,family="binomial")
#med_glm<-glm(Count~Temperature,data=Med_Sub,family="binomial")
summary(model)

###Snowshoes
Snow_NA<-mtable[is.na(mtable$Usage),]
Snow_Sub<-subset(mtable,Usage == "Snowshoes")
Snow_Sub<-rbind(Snow_Sub,Snow_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Snow_Sub,family="binomial")
#snow_glm<-glm(Count~Temperature,data=Snow_Sub,family="binomial")
summary(model)


###Blankets
Blank_NA<-mtable[is.na(mtable$Usage),]
Blank_Sub<-subset(mtable,Usage == "Blankets ")
Blank_Sub<-rbind(Blank_Sub,Blank_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Blank_Sub,family="binomial")
#blank_glm<-glm(Count~Temperature,data=Blank_Sub,family="binomial")
summary(model)


##Fishing
Fish_NA<-mtable[is.na(mtable$Usage),]
Fish_Sub<-subset(mtable,Usage == "Fishing")
Fish_Sub<-rbind(Fish_Sub,Fish_NA)
model<-fitme(Count~Temperature+Matern(1|LAT+LONG),data=Fish_Sub,family="binomial")
#fish_glm<-glm(Count~Temperature,data=Fish_Sub,family="binomial")
summary(model)
