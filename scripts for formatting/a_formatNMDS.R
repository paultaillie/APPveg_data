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
 

# read in 2004 groundcover data --------------------------------------------
cover.2004<-read.csv("raw_data/cover_2004.csv")
cover.2004.U<-cover.2004[cover.2004$Cover.Strata=="understory",]

#change to character
cover.2004.U$Cover.Site<-as.character(cover.2004.U$Cover.Site)
cover.2004.U$Cover.Community<-as.character(cover.2004.U$Cover.Community)
cover.2004.U$Cover.Plot.ID<-as.character(cover.2004.U$Cover.Plot.ID)
cover.2004.U$Cover.Sub.Plot.Number<-as.character(cover.2004.U$Cover.Sub.Plot.Number)

i=1
temp.site="na"
temp.comm="na"
temp.plot="na"
temp.subplot="na"
#reformat plot ID
plot<-rep(NA,nrow(cover.2004.U))
for(i in 1:nrow(cover.2004.U)){
  if(cover.2004.U$Cover.Site[i]=="Gull.Rock"){temp.site<-"GR"}
  if(cover.2004.U$Cover.Site[i]=="Long.Shoal"){temp.site<-"LS"}
  if(cover.2004.U$Cover.Site[i]=="Manns.Harbor"){temp.site<-"MA"}
  if(cover.2004.U$Cover.Site[i]=="Palmetto.Peartree"){temp.site<-"PP"}
  if(cover.2004.U$Cover.Site[i]=="Swanquarter"){temp.site<-"SW"}
  if(cover.2004.U$Cover.Community[i]=="Forest"){temp.comm<-"FOR"}
  if(cover.2004.U$Cover.Community[i]=="Transition"){temp.comm<-"TRA"}
  if(cover.2004.U$Cover.Community[i]=="Marsh"){temp.comm<-"MAR"}
  if(nchar(as.character(cover.2004.U$Cover.Plot.ID[i]))<2){temp.plot<-paste("0",cover.2004.U$Cover.Plot.ID[i],sep="")
  }else{temp.plot<-cover.2004.U$Cover.Plot.ID[i]}
  # temp.subplot<-as.character(cover.2004.U$Cover.Sub.Plot.Number[i])
  plot[i]<-paste(temp.site,temp.comm,temp.plot,sep="")
}

# clean data
dat04<-data.frame(plot=plot,species=cover.2004.U$Cover.Species,percent=cover.2004.U$Cover.Percent)
dat04$species<-as.character(dat04$species)
dat04$species[which(dat04$species=="SCAM")]<-"SCIRPUS"
dat04$species[which(dat04$species=="SCRO")]<-"SCIRPUS"
dat04$species[which(dat04$species=="UNKSCIRP")]<-"SCIRPUS"
dat04$species[which(dat04$species=="HUSP")]<-"HYDR"
dat04$species[which(dat04$species=="HYUM")]<-"HYDR"
dat04$species[which(dat04$species=="HIMI")]<-"KOVI"
dat04$species[which(dat04$species=="SMGL")]<-"SMILAX"
dat04$species[which(dat04$species=="SMHI")]<-"SMILAX"
dat04$species[which(dat04$species=="SMLA")]<-"SMILAX"
dat04$species[which(dat04$species=="SMRO")]<-"SMILAX"
dat04$species[which(dat04$species=="DICA2")]<-"DICA"
dat04$species[which(dat04$species=="ELEOCHsp")]<-"ELEO"
dat04$species[which(dat04$species=="DWD")]<-"DEAD"
dat04$species[which(dat04$species=="ANVI")]<-"ANDR"
dat04$species[which(dat04$species=="EUCA")]<-"EUPA"
dat04$species[which(dat04$species=="HYSP")]<-"HYDR"
dat04$species[which(dat04$species=="MYHE")]<-"MYCE"
dat04$species[which(dat04$species=="PISE")]<-"PINE"
dat04$species[which(dat04$species=="PITA")]<-"PINE"
dat04$species[which(dat04$species=="RHUS")]<-"RHCO"
dat04$species[which(dat04$species=="water")]<-"BARE"
dat04$species[which(dat04$species=="wrack")]<-"WRACK"

# get plot averages
dat.04.sums<-dcast(dat04,
                   formula=plot+species~.,
                   fun.aggregate=sum,
                   value.var="percent")
colnames(dat.04.sums)[3]<-"percent"

#divide percents by 5 subplots
dat.04.means<-data.frame(plot=dat.04.sums[1],species=dat.04.sums[2],percent=dat.04.sums[3]/5)
dat.04.means$percent<-as.numeric(dat.04.means$percent)
dat.04.wide<-recast(dat.04.means,plot~species)
dat.04.wide.matrix<-as.matrix(dat.04.wide[,2:ncol(dat.04.wide)])
rownames(dat.04.wide.matrix)<-dat.04.wide[,1]

dat.04.wide.matrix[which(is.na(dat.04.wide.matrix)==TRUE)]<-0

head(dat.04.wide.matrix)

# Remove rare and unknown (CAGL, citoniz, LUAL, PAQU, SAPA, UNK1, UNK2, UNKGRASS, UNKSEDGE)
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="CALU")]

dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="CAGL")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="citoniz")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="CYPE")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="DICA")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="FISP")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="ILVO")]

dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="LUAL")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="PAQU")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="JUVI")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="PYLA")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="QUNI")]

dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="SAPA")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="SEMA")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="RHYN")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="SEGE")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="SPCY")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="SYTI")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="VIBU")]

dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="UNK1")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="UNK2")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="UNKGRASS")]
dat.04.wide.matrix<-dat.04.wide.matrix[,-which(colnames(dat.04.wide.matrix)=="UNKSEDGE")]

dat.04.reduced<-dat.04.wide.matrix
write.csv(dat.04.reduced,file="processed_data/SpecDat04.csv")



#          #        #           #
#--------     2016  ------------------------------------------------------------------
#         #         #           #
# #clear environment
remove(list=ls())

cover.2016.raw<-read.csv("raw_data/cover_2016.csv")
#pull out understory only
cover.2016<-cover.2016.raw[cover.2016.raw$Cover.Strata=="U",]
#Get rid of 2016 LS data
cover.2016.temp<-cover.2016[which(cover.2016$Cover.Site!="LS"),]

# Read in new LS data
cover.2017<-read.csv("raw_data/cover_2017_LS.csv")
# combine
cover.2016.U<-rbind(cover.2016.temp[,1:7],cover.2017)



#change to character
cover.2016.U$Cover.Site<-as.character(cover.2016.U$Cover.Site)
cover.2016.U$Cover.Community<-as.character(cover.2016.U$Cover.Community)
cover.2016.U$Cover.Plot.ID<-as.character(cover.2016.U$Cover.Plot.ID)
cover.2016.U$Cover.Sub.Plot.Number<-as.character(cover.2016.U$Cover.Sub.Plot.Number)

# format plot
plot<-rep(NA,nrow(cover.2016.U))

for(i in 1:nrow(cover.2016.U)){
  temp.site<-cover.2016.U$Cover.Site[i]
  temp.comm<-toupper(substr(cover.2016.U$Cover.Community[i],1,3))
  if(nchar(cover.2016.U$Cover.Plot.ID[i])<2){temp.plot<-paste("0",cover.2016.U$Cover.Plot.ID[i],sep="")
  }else{temp.plot<-cover.2016.U$Cover.Plot.ID[i]}
  plot[i]<-paste(temp.site,temp.comm,temp.plot,sep="")
}

#combine clean data
dat16<-data.frame(plot=plot,species=cover.2016.U$Cover.Species,percent=cover.2016.U$Cover.Percent)
dat16$species<-as.character(dat16$species)
# clean shit up
dat16$species[which(dat16$species=="ACRE")]<-"ACRU"
dat16$species[which(dat16$species=="ANDR 1")]<-"ANDR"
dat16$species[which(dat16$species=="ANGO")]<-"ANDR"
dat16$species[which(dat16$species=="ANGL")]<-"ANDR"
dat16$species[which(dat16$species=="ANSP")]<-"ANDR"
dat16$species[which(dat16$species=="ANVI")]<-"ANDR"
dat16$species[which(dat16$species=="ASSY")]<-"ASLA"
dat16$species[which(dat16$species=="CAHA")]<-"BAHA"
dat16$species[which(dat16$species=="EAHA")]<-"BAHA"
dat16$species[which(dat16$species=="CLADIUM")]<-"CLJA"
dat16$species[which(dat16$species=="ELEOSP")]<-"ELEO"
dat16$species[which(dat16$species=="ELSE")]<-"ELEO"
dat16$species[which(dat16$species=="ELSP")]<-"ELEO"
dat16$species[which(dat16$species=="GA__")]<-"GATI"
dat16$species[which(dat16$species=="Gylussacia")]<-"GADU"
dat16$species[which(dat16$species=="IGSA")]<-"IPSA"
dat16$species[which(dat16$species=="ILGA")]<-"ILGL"
dat16$species[which(dat16$species=="IVFK")]<-"IVFR"
dat16$species[which(dat16$species=="JURU")]<-"JURO"
dat16$species[which(dat16$species=="JUSP")]<-"JUEF"
dat16$species[which(dat16$species=="LYLI?")]<-"LYLI"
dat16$species[which(dat16$species=="MICRO")]<-"MICR"
dat16$species[which(dat16$species=="Morning Glory")]<-"IPSA"
dat16$species[which(dat16$species=="MOSS")]<-"SPHA"
dat16$species[which(dat16$species=="NARE")]<-"BARE"
dat16$species[which(dat16$species=="OSR")]<-"OSRE"
dat16$species[which(dat16$species=="PAAU")]<-"PHAU"
dat16$species[which(dat16$species=="Pickeral Weed")]<-"POSP"
dat16$species[which(dat16$species=="PITA")]<-"PINE"
dat16$species[which(dat16$species=="PISE")]<-"PINE"
dat16$species[which(dat16$species=="PLJA")]<-"CLJA"
dat16$species[which(dat16$species=="ROSA")]<-"ROPA"
dat16$species[which(dat16$species=="RUAR")]<-"RUBUS"
dat16$species[which(dat16$species=="SMIL")]<-"SMILAX"
dat16$species[which(dat16$species=="SPAL")]<-"SCIRPUS"
dat16$species[which(dat16$species=="SPCY")]<-"SCIRPUS"
dat16$species[which(dat16$species=="SCAM")]<-"SCIRPUS"
dat16$species[which(dat16$species=="TLRA")]<-"TORA"
dat16$species[which(dat16$species=="Typha")]<-"TYPHA"
dat16$species[which(dat16$species=="TYPH")]<-"TYPHA"
dat16$species[which(dat16$species=="WOVA")]<-"WOVI"
dat16$species[which(dat16$species=="TYPHA")]<-"TYLA"
dat16$species[which(dat16$species=="WATER")]<-"BARE"

# get plot averages
dat.16.sums<-dcast(dat16,
                   formula=plot+species~.,
                   fun.aggregate=sum,
                   value.var="percent")
colnames(dat.16.sums)[3]<-"percent"




#divide percents by 5 subplots
dat.16.means<-data.frame(plot=dat.16.sums[1],species=dat.16.sums[2],percent=dat.16.sums[3]/5)
dat.16.means$percent<-as.numeric(dat.16.means$percent)
dat.16.wide<-recast(dat.16.means,plot~species)
dat.16.wide.matrix<-as.matrix(dat.16.wide[,2:ncol(dat.16.wide)])
rownames(dat.16.wide.matrix)<-dat.16.wide[,1]

dat.16.wide.matrix[which(is.na(dat.16.wide.matrix)==TRUE)]<-0

#  dat.16.wide.matrix[,43]

# delete rare or unknowns
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="A____")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="CENT")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="FERN")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="GRASS")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="IRIS")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="OESP")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="POCO")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="S....")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="SEVI")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNK 1 ")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="ASLA")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="CACA")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="CARA")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="GADU")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="IPSA")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="LOJA")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="MAVI")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="MICR")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="PAQU")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="PESP")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="POSP")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="RUBUS")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="VIRO")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="NYBI")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="NA")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNK HERB")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNKN")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNKN-1")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNKN 1")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNKN 2")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNKN 2 ")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNKN 3")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="UNKN GRASS")]
dat.16.wide.matrix<-dat.16.wide.matrix[,-which(colnames(dat.16.wide.matrix)=="RUSH")]

dat.16.reduced<-dat.16.wide.matrix

write.csv(dat.16.reduced,file="processed_data/SpecDat16.csv")




































































