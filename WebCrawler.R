# 在搜尋框裡面輸入hashtag後點選搜尋按鈕
Hashtags = c("台北旅行","新北旅行","桃園旅行","臺中旅行","臺南旅行","高雄旅行","基隆旅行","新竹旅行",
             "嘉義旅行","苗栗旅行","彰化旅行","南投旅行","雲林旅行","屏東旅行","宜蘭旅行","花蓮旅行",
             "台東旅行","澎湖旅行","金門旅行")
Hashtags = c("台北旅遊","新北旅遊","桃園旅遊","臺中旅遊","臺南旅遊","高雄旅遊","基隆旅遊","新竹旅遊",
             "嘉義旅遊","苗栗旅遊","彰化旅遊","南投旅遊","雲林旅遊","屏東旅遊","宜蘭旅遊","花蓮旅遊",
             "台東旅遊","澎湖旅遊","金門旅遊")
Hashtags = c("台北美食","新北美食","桃園美食","臺中美食","臺南美食","高雄美食","基隆美食","新竹美食",
             "嘉義美食","苗栗美食","彰化美食","南投美食","雲林美食","屏東美食","宜蘭美食","花蓮美食",
             "台東美食","澎湖美食","金門美食") # <hashtag here>

#匯入AllDATA
AllDATA<-readRDS("C:/Users/Jacky/Documents/大四/下/網頁程式設計/期末程式/AllDATA.rds")

AllSource<-c()
Alltag<-c()
AllPostUrl<-c()
for(i in (1:length(Hashtags))){
  remDr$navigate(paste0("https://www.instagram.com/explore/tags/",Hashtags[i],"/?hl=zh-tw"))
  Sys.sleep(6)
  #載入第一頁(未滾動滾輪往下載入的狀況)
  page1_source<-remDr$getPageSource()
  page1Content<-read_html(page1_source[[1]],encoding = "UTF-8") 
  href<- page1Content %>% html_nodes("a") %>% html_attr('href')
  #有33篇
  posthref<-href[grepl('/p/',href)] 
  url<-paste0("https://www.instagram.com",posthref)
  
  for(j in (1:length(url))){
    #判斷那一篇文章的url有沒有已經存在在資料內
    if(url[j]%in%AllDATA$PostUrl|url[j]%in%AllPostUrl){
      next
    }else{
      AllSource<-c(AllSource,"IG")
      Alltag<-c(Alltag,Hashtags[i])
      AllPostUrl<-c(AllPostUrl,url[j])
    }
  }
  #Sys.sleep(2)
}

DATA<-data.frame(Source=AllSource,Hashtag=Alltag,PostUrl=AllPostUrl,stringsAsFactors = F)

for(i in (1:length(DATA$PostUrl))){
  remDr$navigate(DATA$PostUrl[i])
  Sys.sleep(1)
  post_source<-remDr$getPageSource()
  postContent<-read_html(post_source[[1]],encoding = "UTF-8") 
  
  #發文者
  poster<- postContent%>% html_nodes(".e1e1d .ZIAjV") %>% html_text()
  if(length(poster)==0){
    DATA$Poster[i]<- NA
    DATA$Time[i]<-NA
    DATA$IGLike[i]<-NA
    DATA$PlaceID[i]<-NA
    DATA$AttractionName[i]<-NA
    DATA$PhotoUrl[i]<-NA
  }else{
    #發文者
    DATA$Poster[i]<-poster
    #發文時間
    Time<-postContent %>% html_nodes("._1o9PC")%>%html_attr("title")
    DATA$Time[i]<-as.character(as.POSIXct(Time, format="%Y年%m月%d日"))
    #發文時間分組
    DATA$Group[i]<-paste0(strsplit(DATA$Time[i],"-")[[1]][1],strsplit(DATA$Time[i],"-")[[1]][2])
    #按讚數
    like<-postContent %>% html_nodes("._8A5w5 span")%>%html_text()
    if(length(like)==0){
      DATA$IGLike[i]<-NA
    }else{
      DATA$IGLike[i]<-as.numeric(gsub(",","",like))
    }
    #打卡地點
    place<-postContent %>% html_nodes(".O4GlU")
    if(length(place)==0){
      #每一篇文的打卡地點(ID)
      DATA$PlaceID[i]<-NA
      DATA$AttractionName[i]<-NA
    }else{
      placeURL<-place%>%html_attr('href')
      #每一篇文的打卡地點(ID)
      DATA$PlaceID[i]<-strsplit(placeURL,"/")[[1]][4]
      #每一篇文的打卡地點(店名)
      DATA$AttractionName[i]<-html_text(place)
    }
    #照片網址
    PhotoURL<-postContent %>% html_nodes(".KL4Bh img")%>%html_attr("src")
    if(length(PhotoURL)==0){
      DATA$PhotoUrl[i]<-NA
    }else{
      DATA$PhotoUrl[i]<-PhotoURL[1]
    }
  }
}  

saveRDS(DATA,"美食未清洗DATA.rds")