# 載入package
library(RSelenium)
library(rvest)
library(dplyr)
library(jsonlite)

# 建立連線後開啟instagram登入網址
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444)
remDr$open()
remDr$navigate("https://www.instagram.com/accounts/login/")

# 輸入帳號
username = "_____________"  # <username here>
Keyname <- remDr$findElement(using = 'xpath', value =
                               "//div[@class='-MzZI'][1]/div[@class='_9GP1n   ']/label[@class='f0n8F ']/input[@class='_2hvTZ pexuQ zyHYP']")
Keyname$sendKeysToElement(list(username))
#輸入密碼
password = "_____________"  # <password here>
Keypassword <- remDr$findElement(using = 'xpath', value =
                                   "//div[@class='-MzZI'][2]/div[@class='_9GP1n   ']/label[@class='f0n8F ']/input[@class='_2hvTZ pexuQ zyHYP']")
Keypassword$sendKeysToElement(list(password))
#點選登入按鈕
Signin <- remDr$findElement(using = 'xpath', value = "//button[@class='sqdOP  L3NKy   y3zKF     ']")
Signin$clickElement()

#要先在網頁板點掉通知