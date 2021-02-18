# po文時間、做子圖、看有沒有規律(他們PO文的日期時間和按讚數有無相關)
# (
#   護理系吃貨 liyu_foodmap 24.2K
#   老爺說半夜不要 foodiemylife 23K
#   CHIEN簡 nissa523food_  27k
# )
#護理系
library(rvest)
library(lubridate)
library(ggplot2)
liurl<-c("https://www.instastalker.net/user/liyu_foodmap/3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/2055202195711886313_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/2026164751415107157_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/2003730248532864822_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1984798953173969164_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1970396082320100691_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1958727966896756543_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1943486321712624994_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1929022369242999303_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1914534173444613350_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1900726899472469403_3214542666")
AllPostUrl<-c()
Alltime<-c()
Allday<-c()
Alllike<-c()
for (i in (1:length(liurl))) {
  Lpost<-read_html(liurl[i])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
  AllPostUrl<-c(AllPostUrl,Lpost)
  Ltime<-read_html(liurl[i])%>%html_nodes(".posts-author__footer-info span:nth-child(1)")%>%html_text()
  Llike<-read_html(liurl[i])%>%html_nodes("span~ span+ span")%>%html_text()
  timeAPM<-purrr::map_chr(strsplit(Ltime,"\n| "),31)
  timeALL<-as.character(parse_date_time(timeAPM, '%I:%M %p'))
  Alltime<-c(Alltime,purrr::map_chr(strsplit(timeALL," "),2))
  Allday<-c(Allday,purrr::map_chr(strsplit(Ltime,"\n| "),32))
  Alllike<-c(Alllike,as.numeric(purrr::map_chr(strsplit(Llike,"\n| "),30)))
}
liyuDF<-data.frame(PostUrl=AllPostUrl,Day=Allday,Time=Alltime,Like=Alllike,stringsAsFactors = F)
liyuDF$Hour<-as.numeric(purrr::map_chr(strsplit(Alltime,":"),1))+8
liyuDF$Day<-mdy(liyuDF$Day)#把日期字串轉日期
saveRDS(liyuDF,"liyuDF.rds")

#老爺說半夜不要看
library(rvest)
library(lubridate)
library(ggplot2)
foourl<-c("https://www.instastalker.net/user/foodiemylife/6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/2054489836119441026_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/2039263654924085092_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/2024048385369044474_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/2010273723627566634_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/1995041122981689010_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/1979815527997879767_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/1963879722918039041_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/1945755588396701076_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/1930545045268536404_6624736568",
         "https://www.instastalker.net/user/foodiemylife/6624736568/1916760481085755230_6624736568")
AllPostUrl<-c()
Alltime<-c()
Allday<-c()
Alllike<-c()
for (i in (1:length(foourl))) {
  Lpost<-read_html(foourl[i])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
  AllPostUrl<-c(AllPostUrl,Lpost)
  Ltime<-read_html(foourl[i])%>%html_nodes(".posts-author__footer-info span:nth-child(1)")%>%html_text()
  Llike<-read_html(foourl[i])%>%html_nodes("span~ span+ span")%>%html_text()
  timeAPM<-purrr::map_chr(strsplit(Ltime,"\n| "),31)
  timeALL<-as.character(parse_date_time(timeAPM, '%I:%M %p'))
  Alltime<-c(Alltime,purrr::map_chr(strsplit(timeALL," "),2))
  Allday<-c(Allday,purrr::map_chr(strsplit(Ltime,"\n| "),32))
  Alllike<-c(Alllike,as.numeric(purrr::map_chr(strsplit(Llike,"\n| "),30)))
}
fooDF<-data.frame(PostUrl=AllPostUrl,Day=Allday,Time=Alltime,Like=Alllike,stringsAsFactors = F)
fooDF$Hour<-as.numeric(purrr::map_chr(strsplit(Alltime,":"),1))+8
fooDF$Day<-mdy(fooDF$Day)
saveRDS(fooDF,"fooDF.rds")

#CHIEN簡 nissa523food_  27k
library(rvest)
library(lubridate)
library(ggplot2)
nisurl<-c("https://www.instastalker.net/user/nissa523food_/1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1996534811088189519_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1936385881344360683_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1907408883284964372_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1882728023176550878_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1859554934287396580_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1842180216270003321_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1824002355096759282_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1807382088643927023_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1786374972533732242_1414556578",
          "https://www.instastalker.net/user/nissa523food_/1414556578/1758094927474253574_1414556578")
AllPostUrl<-c()
Alltime<-c()
Allday<-c()
Alllike<-c()
for (i in (1:length(nisurl))) {
  Lpost<-read_html(nisurl[i])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
  AllPostUrl<-c(AllPostUrl,Lpost)
  Ltime<-read_html(nisurl[i])%>%html_nodes(".posts-author__footer-info span:nth-child(1)")%>%html_text()
  Llike<-read_html(nisurl[i])%>%html_nodes("span~ span+ span")%>%html_text()
  timeAPM<-purrr::map_chr(strsplit(Ltime,"\n| "),31)
  timeALL<-as.character(parse_date_time(timeAPM, '%I:%M %p'))
  Alltime<-c(Alltime,purrr::map_chr(strsplit(timeALL," "),2))
  Allday<-c(Allday,purrr::map_chr(strsplit(Ltime,"\n| "),32))
  Alllike<-c(Alllike,as.numeric(purrr::map_chr(strsplit(Llike,"\n| "),30)))
}
nisDF<-data.frame(PostUrl=AllPostUrl,Day=Allday,Time=Alltime,Like=Alllike,stringsAsFactors = F)
nisDF$Hour<-as.numeric(purrr::map_chr(strsplit(Alltime,":"),1))+8
nisDF$Day<-mdy(nisDF$Day)
nisDF$Hour[193]<-1
saveRDS(nisDF,"nisDF.rds")


library(dplyr)
library(ggplot2)
liyutimelike<-select(liyuDF,Hour,Like)%>%group_by(Hour)%>%summarise(TotalLike=sum(Like),HourCount=n())
liyutimelike$User<-"liyu_foodmap"
footimelikde<-select(fooDF,Hour,Like)%>%group_by(Hour)%>%summarise(TotalLike=sum(Like),HourCount=n())
footimelikde$User<-"foodiemylife"
nistimelikde<-select(nisDF,Hour,Like)%>%group_by(Hour)%>%summarise(TotalLike=sum(Like),HourCount=n())
nistimelikde$User<-"nissa523food_"
Alltimelike<-rbind(liyutimelike,footimelikde,nistimelikde)
#時間po文總數子圖
ggplot(Alltimelike,aes(x=Hour,y=HourCount,fill=User))+
  geom_bar(stat="identity")+
  facet_grid(User~.)+
  scale_fill_manual(values=c("#6fc2d0", "#373a6d","#0B6374"))+
  theme_bw(base_size = 18)+
  geom_text(stat="identity",aes(label=HourCount),vjust=-0.03,size=4.5)+
  labs(x="幾點(小時)",y="發文數量(篇)",title="統計每小時發文篇數")
 
#po文時間跟每篇文平均讚數(po文總讚數/篇數)bar圖
Alltimelike$Mean<-round(Alltimelike$TotalLike/Alltimelike$HourCount)
#liyu圖
ggplot(Alltimelike, aes(x=Hour, y=Mean, fill=User))+
  geom_bar(stat="identity", position="dodge")+
  theme_bw(base_size = 18)+
  scale_fill_manual(values=c("#beeef7","#ff8246","#6fc2d0"))+
  annotate("text",x=c(13.2,13.2),y=c(-30,1950),label=c("13","1904"),size=8)+
  labs(x="幾點(小時)",y="平均讚數(總讚數/總篇數)",title="liyu_foodmap平均每小時每篇文按讚數")
#foo圖
ggplot(Alltimelike, aes(x=Hour, y=Mean, fill=User))+
  geom_bar(stat="identity", position="dodge")+
  theme_bw(base_size = 18)+
  scale_fill_manual(values=c("#ff8246","#6fc2d0","#beeef7"))+
  annotate("text",x=c(18.7,18.7,19.7,19.7),y=c(-10,1500,-10,1500),label=c("19","1460","20","1464"))+
  labs(x="幾點(小時)",y="平均讚數(總讚數/總篇數)",title="foodiemylife平均每小時每篇文按讚數")
#nis圖
ggplot(Alltimelike, aes(x=Hour, y=Mean, fill=User))+
  geom_bar(stat="identity", position="dodge")+
  theme_bw(base_size = 18)+
  scale_fill_manual(values=c("#beeef7","#6fc2d0","#ff8246"))+
  annotate("text",x=c(18.2,18.2),y=c(-30,1350),label=c("18","1306"),size=8)+
  labs(x="幾點(小時)",y="平均讚數(總讚數/總篇數)",title="nissa523food_平均每小時每篇文按讚數")
#發文時間跟總讚數 有沒有相關
Hourlike<-select(Alltimelike,Hour,TotalLike)%>%group_by(Hour)%>%summarise(TotalLike=sum(TotalLike))
ggplot(Hourlike,aes(x=Hour,y=TotalLike))+
  geom_bar(stat="identity",fill = "#6fc2d0")+
  theme_bw(base_size = 18)+
  geom_text(stat="identity",aes(label=TotalLike),vjust=-0.5,size=4.5)+
  annotate("text",x=c(2.5,8),y=c(70000,70000),
           label=c("相關係數=",as.character(cor(Hourlike$Hour,Hourlike$TotalLike))),
           size=8)+
  labs(x="幾點(小時)",y="總讚數",title="每小時總按讚數")


# Hourlike1<-select(Alltimelike,Hour,TotalLike,HourCount)%>%group_by(Hour)%>%summarise(TotalLike=sum(TotalLike),TotalCount=sum(HourCount))
# Hourlike1$Mean<-round(Hourlike1$TotalLike/Hourlike1$TotalCount)
# cor(Hourlike1$Mean,Hourlike1$Hour)
# #畫總按讚數圖
# ggplot(Hourlike1,aes(x=Hour,y=Mean))+
#   geom_point()+
#   geom_line(color ="#0B6374",size=1.3)+
#   theme_bw()+
#   geom_text(stat="identity",aes(label=Mean),vjust=-0.8)+
#   annotate("text",x=c(2.5,7.5),y=c(1500,1500),
#            label=c("相關係數=",as.character(cor(Hourlike1$Mean,Hourlike1$Hour))),
#            size=8)+
#   labs(x="幾點(小時)",y="平均每篇文按讚數(個)",title="                                         每小時總按讚數")
# 
#   
# #請問Hour與TotalLike是否有關? 0.4004984 中度相關
# cor(Hourlike$Hour,Hourlike$TotalLike)
