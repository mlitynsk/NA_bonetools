##########GLM RESULTS, TEMPERATURE AND PERFORATOR TOOL RELATIONSHIPS###########

## RELEASE 1.0 NOTES##

##This code provides the opportunity to determine the relationship between environmental temperature (minimum temperature of the coldest month) based on the WorldClim database and the presence/absence of needles and awls within ethnographies associated with the eHRAF World Cultures database. 

#READ IN THE FOLLOWING PACKAGES
installing all the packages below##
packages <-c('ggplot2','tidyr','dplyr') #These lines of code imports all of my packages
for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))
theme_set(theme_bw())

#READ IN THE FOLLOWING CSV FILES
a_final_table<-read.csv("C:\\Users\\kenna\\OneDrive\\Documents\\Dissertation Research\\CHAPTER 1 INFO\\American Antiquity Submission Materials\\a_final_table.csv")
another_final_table<-read.csv("C:\\Users\\kenna\\OneDrive\\Documents\\Dissertation Research\\CHAPTER 1 INFO\\American Antiquity Submission Materials\\another_final_table.csv")
Al_table<-read.csv("Dissertation Research/CHAPTER 1 INFO/American Antiquity Submission Materials/Al_table.csv")
Therm_table<-read.csv("C:\\Users\\kenna\\OneDrive\\Documents\\Dissertation Research\\CHAPTER 1 INFO\\American Antiquity Submission Materials\\Therm_table.csv")

#THE GLM BELOW EXAMINES THE RELATIONSHIP BETWEEN ENVIRONMENTAL TEMPERATURE AND THE PRESENCE/ABSENCE OF ALL THOSE PERFORATOR TOOLS REFLECTED ETHNOGRAPHICALLY AMONG EHRAF.
all_tools<-glm(Count~Temperature, data=another_final_table,family="binomial")
summary(all_tools)

##THESE GLMS EXAMINE THE RELATIONSHIP BETWEEN ENVIRONMENTAL TEMPERATURE AND THE PRESENCE OF NEEDLES AND AWLS (CONSIDERED SEPARATELY)

Needles_NA<-a_final_table[is.na(a_final_table$Tool_Type),]
Needles_Sub<-subset(a_final_table,Tool_Type == "needle")
Needles_Sub<-rbind(Needles_NA,Needles_Sub)
need_glm<-glm(Count~Temperature,data=Needles_Sub,family="binomial")
summary(need_glm) 

Awls_NA<-a_final_table[is.na(a_final_table$Tool_Type),]
Awls_Sub<-subset(a_final_table,Tool_Type == "awl")
Awls_Sub<-rbind(Awls_NA,Awls_Sub)
awl_glm<-glm(Count~Temperature,data=Awls_Sub,family="binomial")
summary(awl_glm) 

#THE FOLLOWING CODE SUBSETS THE DATA ACCORDING TO THE TOOLS USED ETHNOGRAPHICALLY FOR ACTIVITIES OUTSIDE THE REALM OF THERMOREGULATION. THE GLM EXAMINES THE RELATIONSHIP BETWEEN TEMPERATURE AND THOSE PERFORATOR TOOLS USED FOR SUCH ALTERNATIVE ACTIVITIES.
Al_NA<-Al_table[is.na(Al_table$Therm_vs.Other),]
Al_Sub<-subset(Al_table,Therm_vs.Other == "Alternative")
Al_Bind<-rbind(Al_Sub,Al_NA)
al_glm<-glm(Count~Temperature,data=Al_Bind,family="binomial")
summary(al_glm) 

##THIS GLM EXAMINES THE RELATIONSHIP BETWEEN TEMPERATURE AND THOSE PERFORATOR TOOLS USED FOR THERMOREGULATION 
Thermo_NA<-Therm_table[is.na(Therm_table$Therm_vs.Other),]
Thermo_Sub<-subset(Therm_table, Therm_vs.Other == "Thermoregulation")
Thermo_Bind<-rbind(Thermo_Sub, Thermo_NA)
therm_glm<-glm(Count~Temperature,data=Thermo_Bind,family="binomial")
summary(therm_glm) 

###GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND TATTOOING##
Tatt_NA<-a_final_table[is.na(a_final_table$Usage),]
Tatt_Sub<-subset(a_final_table,Usage == "Tattooing")
Tatt_Sub<-rbind(Tatt_Sub,Tatt_NA)
tatt_glm<-glm(Count~Temperature,data=Tatt_Sub,family="binomial")
summary(tatt_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND BASKETRY##
Bask_NA<-a_final_table[is.na(a_final_table$Usage),]
Bask_Sub<-subset(a_final_table,Usage == "Baskets")
Bask_Sub<-rbind(Bask_Sub,Bask_NA)
bask_glm<-glm(Count~Temperature,data=Bask_Sub,family="binomial")
summary(bask_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND CLOTHING##
Cloth_NA<-a_final_table[is.na(a_final_table$Usage),]
Cloth_Sub<-subset(a_final_table,Usage == "Clothing")
Cloth_Sub<-rbind(Cloth_Sub,Cloth_NA)
cloth_glm<-glm(Count~Temperature,data=Cloth_Sub,family="binomial")
summary(cloth_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND CEREMONY/RITUAL CATEGORY##
Cer_NA<-a_final_table[is.na(a_final_table$Usage),]
Cer_Sub<-subset(a_final_table,Usage == "Ceremony or Ritual")
Cer_Sub<-rbind(Cer_Sub,Cer_NA)
cer_glm<-glm(Count~Temperature,data=Cer_Sub,family="binomial")
summary(cer_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND PIERCING##
Pier_NA<-a_final_table[is.na(a_final_table$Usage),]
Pier_Sub<-subset(a_final_table,Usage == "Piercing")
Pier_Sub<-rbind(Pier_Sub,Pier_NA)
pier_glm<-glm(Count~Temperature,data=Pier_Sub,family="binomial")
summary(pier_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND SHOES##
Shoe_NA<-a_final_table[is.na(a_final_table$Usage),]
Shoe_Sub<-subset(a_final_table,Usage == "Shoes")
Shoe_Sub<-rbind(Shoe_Sub,Shoe_NA)
shoe_glm<-glm(Count~Temperature,data=Shoe_Sub,family="binomial")
summary(shoe_glm)  

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND MYTHOLOGY##
Myth_NA<-a_final_table[is.na(a_final_table$Usage),]
Myth_Sub<-subset(a_final_table,Usage == "Mythology")
Myth_Sub<-rbind(Myth_Sub,Myth_NA)
myth_glm<-glm(Count~Temperature,data=Myth_Sub,family="binomial")
summary(myth_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND MATS##
Mat_NA<-a_final_table[is.na(a_final_table$Usage),]
Mat_Sub<-subset(a_final_table,Usage == "Mats")
Mat_Sub<-rbind(Mat_Sub,Mat_NA)
mat_glm<-glm(Count~Temperature,data=Mat_Sub,family="binomial")
summary(mat_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND SHELTER##
Shel_NA<-a_final_table[is.na(a_final_table$Usage),]
Shel_Sub<-subset(a_final_table,Usage == "Shelter")
Shel_Sub<-rbind(Shel_Sub,Shel_NA)
shel_glm<-glm(Count~Temperature,data=Shel_Sub,family="binomial")
summary(shel_glm) 

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND MEDICAL USE##
Med_NA<-a_final_table[is.na(a_final_table$Usage),]
Med_Sub<-subset(a_final_table,Usage == "Medical")
Med_Sub<-rbind(Med_Sub,Med_NA)
med_glm<-glm(Count~Temperature,data=Med_Sub,family="binomial")
summary(med_glm)

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND SNOWSHOES##
Snow_NA<-a_final_table[is.na(a_final_table$Usage),]
Snow_Sub<-subset(a_final_table,Usage == "Snowshoes")
Snow_Sub<-rbind(Snow_Sub,Snow_NA)
snow_glm<-glm(Count~Temperature,data=Snow_Sub,family="binomial")
summary(snow_glm)

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND BLANKETS##
Blank_NA<-a_final_table[is.na(a_final_table$Usage),]
Blank_Sub<-subset(a_final_table,Usage == "Blankets ")
Blank_Sub<-rbind(Blank_Sub,Blank_NA)
blank_glm<-glm(Count~Temperature,data=Blank_Sub,family="binomial")
summary(blank_glm)

##GLM FOR EXAMINING THE RELATIONSHIP BETWEEN TEMPERATURE AND FISHING##
Fish_NA<-a_final_table[is.na(a_final_table$Usage),]
Fish_Sub<-subset(a_final_table,Usage == "Fishing")
Fish_Sub<-rbind(Fish_Sub,Fish_NA)
fish_glm<-glm(Count~Temperature,data=Fish_Sub,family="binomial")
summary(fish_glm)


