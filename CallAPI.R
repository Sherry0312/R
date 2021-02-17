#整理資料 
#subset做子集：清除沒有打卡地點的資料
DATA<-subset(DATA,is.na(DATA$PlaceID)==F)
#subset做子集：清除沒有照片的資料
DATA<-subset(DATA,is.na(DATA$PhotoUrl)==F)

saveRDS(DATA,"美食DATA.rds")

#呼叫API得到景點詳細資訊
for(i in (1:length(DATA$PostUrl))){
  if(is.na(DATA$PlaceID[i])==T){
    #每一篇文的地址
    DATA$Address[i]<-NA
    #每一篇文的電話
    DATA$TEL[i]<-NA
    #每一篇文的地點的google評分  
    DATA$GoogleRate[i]<-NA
    #每一篇文的地點的回應數量 
    DATA$GoogleReviewCount[i]<-NA
    #每一篇文的地點的經度
    DATA$latitude[i]<-NA
    #每一篇文的地點的緯度
    DATA$longitude[i]<-NA
    #禮拜一status
    DATA$StatusMon[i]<-NA
    #禮拜一StartTime
    DATA$OpenMonS[i]<-NA
    #禮拜一closeTmie
    DATA$OpenMonC[i]<-NA
    #禮拜二status
    DATA$StatusTUE[i]<-NA
    #禮拜二StartTime
    DATA$OpenTueS[i]<-NA
    #禮拜二closeTmie
    DATA$OpenTueC[i]<-NA
    #禮拜三status
    DATA$StatusWed[i]<-NA
    #禮拜三StartTime
    DATA$OpenWedS[i]<-NA
    #禮拜三closeTmie
    DATA$OpenWedC[i]<-NA
    #禮拜四status
    DATA$StatusThu[i]<-NA
    #禮拜四StartTime
    DATA$OpenThuS[i]<-NA
    #禮拜四closeTmie
    DATA$OpenThuC[i]<-NA
    #禮拜五status
    DATA$StatusFri[i]<-NA
    #禮拜五StartTime
    DATA$OpenFriS[i]<-NA
    #禮拜五closeTmie
    DATA$OpenFriC[i]<-NA
    #禮拜六status
    DATA$StatusSat[i]<-NA
    #禮拜六StartTime
    DATA$OpenSatS[i]<-NA
    #禮拜六closeTmie
    DATA$OpenSatC[i]<-NA
    #禮拜日status
    DATA$StatusSun[i]<-NA
    #禮拜日StartTime
    DATA$OpenSunS[i]<-NA
    #禮拜日closeTmie
    DATA$OpenSunC[i]<-NA
  }else{
    key<-"____________" 
    AttractionName<-DATA$AttractionName[i]
    store<-gsub(" ","%20",AttractionName)
    Store_id<-paste0("https://maps.googleapis.com/",store,"&inputtype=textquery&key=",key)
    placeText<-fromJSON(Store_id)
    placeID<-placeText$candidates$place_id
    StoreUrl<-paste0("https://maps.googleapis.com/",placeID,"&fields=name,rating,formatted_phone_number,opening_hours/weekday_text,user_ratings_total,formatted_address&language=zh-TW&key=",key) 
    StoreInform<-fromJSON(StoreUrl[1])
    #每一篇文的地址
    Address<-StoreInform$result$formatted_address
    if(length(Address)==0){
      DATA$Address[i]<-NA
    }else{
      DATA$Address[i]<-Address
    }
    #每一篇文的電話
    TEL<-StoreInform$result$formatted_phone_number
    if(length(TEL)==0){
      DATA$TEL[i]<-NA
    }else{
      DATA$TEL[i]<-TEL
    }
    #每一篇文的地點的google評分  
    Rate<-StoreInform$result$rating
    if(length(Rate)==0){
      DATA$GoogleRate[i]<-0
    }else{
      DATA$GoogleRate[i]<-Rate
    }
    #每一篇文的地點的回應數量  
    ReviewCount<-StoreInform$result$user_ratings_total
    if(length(ReviewCount)==0){
      DATA$GoogleReviewCount[i]<-NA
    }else{
      DATA$GoogleReviewCount[i]<-ReviewCount
    }
    #經緯度
    Addr<-gsub(" ","%20",DATA$Address[i])
    TransAddUrl<-paste0("https://maps.googleapis.com/",Addr,"&language=zh-TW&key=",key)
    TransAdd<-fromJSON(TransAddUrl)
    #每一篇文的地點的緯度
    
    latitude<-TransAdd$results$geometry$location$lat
    if(length(latitude)==0){
      DATA$latitude[i]<-NA
    }else{
      DATA$latitude[i]<-latitude
    }
    #每一篇文的地點的經度
    longitude<-TransAdd$results$geometry$location$lng
    if(length(longitude)==0){
      DATA$longitude[i]<-NA
    }else{
      DATA$longitude[i]<-longitude
    }
    #營業時間
    OpenTime<-StoreInform$result$opening_hours$weekday_text
    BusinessHour<-NULL
    if(!is.null(OpenTime)){
      OpenTime<-strsplit(OpenTime,split=" ")
      Sys.setlocale("LC_TIME", "C")
      
      for(n in 1:length(OpenTime)){
        OpenTime[[n]][1]<-gsub(":","",OpenTime[[n]][1])
        if(length(OpenTime[[n]])==4){
          temp<-c(OpenTime[[n]][c(1,2,4)],T)
        }else if (length(OpenTime[[n]])==3){
          temp<-c(OpenTime[[n]][c(1,2,3)],T)
        }else{
          if(OpenTime[[n]][2]=="休息"){
            temp<-c(OpenTime[[n]][c(1)],NA,NA,F)
          }
        }
        BusinessHour<-data.frame(rbind(BusinessHour,temp),stringsAsFactors = F)
        for(k in 1:nrow(BusinessHour)){
          rownames(BusinessHour)[k]<-k
        }
      }
      colnames(BusinessHour)<-c("Week","Open Time","Close Time","Status")
    }
    if(length(BusinessHour$Status[1])==0){
      #禮拜一status
      DATA$StatusMon[i]<-NA
      #禮拜一StartTime
      DATA$OpenMonS[i]<-NA
      #禮拜一closeTmie
      DATA$OpenMonC[i]<-NA
      #禮拜二status
      DATA$StatusTUE[i]<-NA
      #禮拜二StartTime
      DATA$OpenTueS[i]<-NA
      #禮拜二closeTmie
      DATA$OpenTueC[i]<-NA
      #禮拜三status
      DATA$StatusWed[i]<-NA
      #禮拜三StartTime
      DATA$OpenWedS[i]<-NA
      #禮拜三closeTmie
      DATA$OpenWedC[i]<-NA
      #禮拜四status
      DATA$StatusThu[i]<-NA
      #禮拜四StartTime
      DATA$OpenThuS[i]<-NA
      #禮拜四closeTmie
      DATA$OpenThuC[i]<-NA
      #禮拜五status
      DATA$StatusFri[i]<-NA
      #禮拜五StartTime
      DATA$OpenFriS[i]<-NA
      #禮拜五closeTmie
      DATA$OpenFriC[i]<-NA
      #禮拜六status
      DATA$StatusSat[i]<-NA
      #禮拜六StartTime
      DATA$OpenSatS[i]<-NA
      #禮拜六closeTmie
      DATA$OpenSatC[i]<-NA
      #禮拜日status
      DATA$StatusSun[i]<-NA
      #禮拜日StartTime
      DATA$OpenSunS[i]<-NA
      #禮拜日closeTmie
      DATA$OpenSunC[i]<-NA
    }else{
      #禮拜一status
      DATA$StatusMon[i]<-BusinessHour$Status[1]
      #禮拜一StartTime
      DATA$OpenMonS[i]<-BusinessHour$`Open Time`[1]
      #禮拜一closeTmie
      DATA$OpenMonC[i]<-BusinessHour$`Close Time`[1]
      #禮拜二status
      DATA$StatusTUE[i]<-BusinessHour$Status[2]
      #禮拜二StartTime
      DATA$OpenTueS[i]<-BusinessHour$`Open Time`[2]
      #禮拜二closeTmie
      DATA$OpenTueC[i]<-BusinessHour$`Close Time`[2]
      #禮拜三status
      DATA$StatusWed[i]<-BusinessHour$Status[3]
      #禮拜三StartTime
      DATA$OpenWedS[i]<-BusinessHour$`Open Time`[3]
      #禮拜三closeTmie
      DATA$OpenWedC[i]<-BusinessHour$`Close Time`[3]
      #禮拜四status
      DATA$StatusThu[i]<-BusinessHour$Status[4]
      #禮拜四StartTime
      DATA$OpenThuS[i]<-BusinessHour$`Open Time`[4]
      #禮拜四closeTmie
      DATA$OpenThuC[i]<-BusinessHour$`Close Time`[4]
      #禮拜五status
      DATA$StatusFri[i]<-BusinessHour$Status[5]
      #禮拜五StartTime
      DATA$OpenFriS[i]<-BusinessHour$`Open Time`[5]
      #禮拜五closeTmie
      DATA$OpenFriC[i]<-BusinessHour$`Close Time`[5]
      #禮拜六status
      DATA$StatusSat[i]<-BusinessHour$Status[6]
      #禮拜六StartTime
      DATA$OpenSatS[i]<-BusinessHour$`Open Time`[6]
      #禮拜六closeTmie
      DATA$OpenSatC[i]<-BusinessHour$`Close Time`[6]
      #禮拜日status
      DATA$StatusSun[i]<-BusinessHour$Status[7]
      #禮拜日StartTime
      DATA$OpenSunS[i]<-BusinessHour$`Open Time`[7]
      #禮拜日closeTmie
      DATA$OpenSunC[i]<-BusinessHour$`Close Time`[7]
    }
  }
}


#subset做子集：清除沒有營業時間的資料
CleanDATA<-subset(DATA,is.na(DATA$StatusMon)==F)

#把24小時的開始時間改成0:00
CleanDATA$OpenMonS<-gsub("24","0:00",CleanDATA$OpenMonS)
CleanDATA$OpenTueS<-gsub("24","0:00",CleanDATA$OpenTueS)
CleanDATA$OpenWedS<-gsub("24","0:00",CleanDATA$OpenWedS)
CleanDATA$OpenThuS<-gsub("24","0:00",CleanDATA$OpenThuS)
CleanDATA$OpenFriS<-gsub("24","0:00",CleanDATA$OpenFriS)
CleanDATA$OpenSatS<-gsub("24","0:00",CleanDATA$OpenSatS)
CleanDATA$OpenSunS<-gsub("24","0:00",CleanDATA$OpenSunS)
#把24小時的結束時間改成23:59
CleanDATA$OpenMonC<-gsub("小時營業","23:59",CleanDATA$OpenMonC)
CleanDATA$OpenTueC<-gsub("小時營業","23:59",CleanDATA$OpenTueC)
CleanDATA$OpenWedC<-gsub("小時營業","23:59",CleanDATA$OpenWedC)
CleanDATA$OpenThuC<-gsub("小時營業","23:59",CleanDATA$OpenThuC)
CleanDATA$OpenFriC<-gsub("小時營業","23:59",CleanDATA$OpenFriC)
CleanDATA$OpenSatC<-gsub("小時營業","23:59",CleanDATA$OpenSatC)
CleanDATA$OpenSunC<-gsub("小時營業","23:59",CleanDATA$OpenSunC)

#把單一hashtag清理好的資料儲存成rds
#-之後的要改
saveRDS(CleanDATA,"美食CleanDATA.rds")
#合併所有hashtag的dataframe
#AllDATA<-data.frame()
AllDATA<-readRDS("C:/Users/Jacky/Documents/大四/下/網頁程式設計/期末程式/AllDATA.rds")
AllDATA<-rbind(AllDATA,CleanDATA)
saveRDS(AllDATA,"AllDATA.rds")