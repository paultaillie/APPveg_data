# format veg data for community analysis

# Paul J. Taillie
# North Carolina State University
# Nov 28, 2017

# load packages
library(reshape2)
library(MASS)
library(tidyverse)

# #clear environment
remove(list=ls())


# read in soil analysis reports --------------------------------------------
soil_2004<-read.csv(file="raw_data/soils_report_2004.csv")
soil_2017a<-read.csv(file="raw_data/soils_report_2017a.csv")
soil_2017b<-read.csv(file="raw_data/soils_report_2017b.csv")

# combine 2017
soil_2017a<-soil_2017a%>%
  select(Sample.Description..1,Na...mg.kg.,Ca...mg.kg.)%>%
  mutate(plots=as.character(Sample.Description..1))%>%
  select(-Sample.Description..1)
soil_2017b<-soil_2017b%>%
  select(Sample.Description..1,Na...mg.kg.,Ca...mg.kg.)%>%
  mutate(plots=as.character(Sample.Description..1))%>%
  select(-Sample.Description..1)

soil_2017<-bind_rows(soil_2017a,soil_2017b)%>%
  arrange(plots)

# create PlotID for 2004
soil_2004$PlotID<-rep(NA,nrow(soil_2004))
plotnum=as.character(soil_2004$SampleDescription1)
numchar<-nchar(plotnum)
for (i in 1:length(numchar)){
  if (numchar[i]==1){soil_2004$PlotID[i]<-paste(soil_2004$Comm[i],"0",plotnum[i],sep="")}
  else {soil_2004$PlotID[i]<-paste(soil_2004$Comm[i],plotnum[i],sep="")}
}
soil_2004<-arrange(soil_2004,PlotID)
data.frame(soil_2004$PlotID,soil_2017$plots)

soil_all<-data.frame(
  plots=c(soil_2004$PlotID,soil_2017$PlotID),
  Na=c(soil_2004$Na_ppm,soil_2017$Na...mg.kg.),
  Ca=c(soil_2004$Ca_ppm,soil_2017$Ca...mg.kg.),
  site=as.factor(c(substr(soil_2004$PlotID,1,2),substr(soil_2017$Sample.Description..1,1,2))),
  comm=as.factor(c(substr(soil_2004$PlotID,3,5),substr(soil_2017$Sample.Description..1,3,5))),
  year=as.factor(c(rep("2004",nrow(soil_2004)),rep("2017",nrow(soil_2017)))))
soil_all$plots<-as.character(soil_all$plots)

#reorder factors
soil_all$site<-factor(soil_all$site,levels(soil_all$site)[c(4,3,2,1,5)])
soil_all$comm<-factor(soil_all$comm,levels(soil_all$comm)[c(1,3,2)])

#write.csv(soil_all,file="processed_data/soils_formatted.csv")
















