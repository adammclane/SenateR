library(ggplot2)
library(lubridate)
require(doBy)
require(scales)
require(dplyr)
require(jsonlite)
require(RCurl)
library(grid)

url2<-paste("https://www.govtrack.us/api/v2/role?current=true&limit=1000")

data2<-as.data.frame(fromJSON(getURL(url2)),stringsAsFactors=F)

data2$count = 1

#birthday of week setup using lubridate
data2$birthwday<-wday(data2$objects.person$birthday)
#summarize data using doBy
wbday<-summaryBy(count~objects.party+birthwday+objects.role_type_label,data=data2,FUN=sum)
#setup weekday plot with ggplot
ggplot(wbday,aes(x=birthwday,y=count.sum,group=objects.party,color=objects.party))+geom_bar(aes(fill = objects.party),stat="identity",position="dodge")+facet_grid(.~objects.role_type_label)

#birth year setup w/ lubridate
data2$birthyear<-year(data2$objects.person$birthday)
#summarize with doby
birthyear<-summaryBy(count~objects.party+birthyear+objects.role_type_label,data=data2,FUN=sum)
#using birthyear, calculate current age
birthyear$age<-year(now())-birthyear$birthyear
#plot senate and rep age

ggplot(birthyear[birthyear$objects.role_type_label!='President'&birthyear$objects.role_type_label!='Vice President',],aes(x=age,color=objects.party))+geom_density()+facet_grid(.~objects.role_type_label)+ theme(
  panel.border = element_rect(fill = NA, colour = "orange", size = 2)
)




