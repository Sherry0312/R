library(dplyr) 
#算每個月有幾篇文
MonthTotal<-group_by(AllDATA,Group)%>%summarise(MonthTotal=n())

#算每個景點的熱門比例
#AllAVE跟MONAVE因為數字太小看不出差別 所以乘100
#AllAVE=那一個景點有幾篇文章/總資料數
AllAVEDATA<-select(AllDATA,Time,Group,AttractionName)%>%
  filter(is.na(AttractionName)==F)%>%
  group_by(AttractionName)%>%
  summarise(AttractionNameTotal=n())%>%
  mutate(AllAVE=AttractionNameTotal/length(AllDATA$Source)*100)
#MonAVE=那一個月內的那一個景點有幾篇文章/那一個月內有幾篇貼文
MonAVEDATA<-filter(AllDATA,Group=="202005")%>%
  filter(is.na(AttractionName)==F)%>%
  group_by(AttractionName)%>%
  summarise(AttractionNameTotal=n())%>%
  mutate(MonAVE=AttractionNameTotal/as.numeric(MonthTotal[grepl(202005,MonthTotal$Group),][2])*100)
#把MonAVEDATA的AttractionName存成向量 判斷他有沒有在一個月的熱門內
MonAVEDATAAttractionName<-c(MonAVEDATA$AttractionName)


for(i in 1:length(AllDATA$Source)){
  for(j in 1:length(AllAVEDATA$AttractionName)){
    if(AllDATA$AttractionName[i]==AllAVEDATA$AttractionName[j]){
      AllDATA$AllAVE[i]<-AllAVEDATA$AllAVE[j]
    }else{
      next
    }
  }
  if(AllDATA$AttractionName[i]%in%MonAVEDATAAttractionName){
    for(k in 1:length(MonAVEDATA$AttractionName)){
      if(AllDATA$AttractionName[i]==MonAVEDATA$AttractionName[k]){
        AllDATA$MonAVE[i]<-MonAVEDATA$MonAVE[k]
      }else{
        next
      }
    }
  }else{
    AllDATA$MonAVE[i]<-0
  }
}

#計算完熱門度的資料
saveRDS(AllDATA,"AllDATA(hot).rds")

#prefer
AllDATA$Allprefer73<-AllDATA$GoogleRate/5*70+AllDATA$AllAVE*30
AllDATA$Allprefer37<-AllDATA$GoogleRate/5*30+AllDATA$AllAVE*70
AllDATA$Allprefer55<-AllDATA$GoogleRate/5*50+AllDATA$AllAVE*50
AllDATA$Monprefer73<-AllDATA$GoogleRate/5*70+AllDATA$MonAVE*30
AllDATA$Monprefer37<-AllDATA$GoogleRate/5*30+AllDATA$MonAVE*70
AllDATA$Monprefer55<-AllDATA$GoogleRate/5*50+AllDATA$MonAVE*50

#計算完熱門度的資料
saveRDS(AllDATA,"AllDATA(prefer).rds")

#確定裡面的景點都不重複
library(dplyr)
UniqueDATA<-data.frame()
attractionName<-c()
for(i in 1:length(AllDATA$Source)){
  if(AllDATA$AttractionName[i]%in%attractionName){
    next
  }else{
    UniqueDATA<-rbind(UniqueDATA,AllDATA[i,])
    attractionName<-c(attractionName,AllDATA$AttractionName[i])
  }
}

#存一次沒有重複景點的資料
saveRDS(UniqueDATA,"UniqueDATA.rds")
#匯出CSV檔 丟到python上傳firebase
write.csv(UniqueDATA,file="DATA.csv",row.names = F)

#取城市
for(i in 1:length(UniqueDATA$AttractionName)){
  if(is.na(UniqueDATA$Address)==T){
    UniqueDATA$Country[i]<-NA
  }else{
    UniqueDATA$Country[i]<-substr(strsplit(UniqueDATA$Address[i],"台灣")[[1]][2],start=1,stop=3)
  }
  
}

UniqueDATA$Country<-gsub("台","臺",UniqueDATA$Country)
UniqueDATA<-subset(UniqueDATA,UniqueDATA$Country!="澎湖縣")

DATA<-select(UniqueDATA,AttractionName,Address,Country)
#把country怪怪的改掉
for(i in 1:length(UniqueDATA$Source)){
  if(UniqueDATA$AttractionName[i]=="星月天空景觀餐廳"){
    UniqueDATA$Country[i]<-"南投縣"
  }else{
    next
  }
}
#刪掉綠島
UniqueDATA<-subset(UniqueDATA,UniqueDATA$PlaceID!="753514381697642")
saveRDS(UniqueDATA,"FinalDATA.rds")

#匯出CSV檔 丟到python上傳firebase
write.csv(FinalDATA,file="FinalDATADATA.csv",row.names = F)