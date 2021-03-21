#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require(htmltools)) {
  install.packages("htmltools")
}
library(htmltools)
if (!require(shiny)) {
  install.packages("shiny")
}
library(shiny)
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)
if (!require(data.table)) {
  install.packages("data.table")
}
library(data.table)
if (!require(leaflet)) {
  install.packages("leaflet")
}
library(leaflet)
if (!require(lubridate)) {
  install.packages("lubridate")
}
library(lubridate)
if (!require(readxl)) {
  install.packages("readxl")
}
library(readxl)
if (!require(rsconnect)) {
  install.packages("rsconnect")
}
library(rsconnect)

if (!require(mapview)) {
    install.packages("mapview")
}
library(mapview)
if (!require(magick)) {
    install.packages("magick")
}
library(magick)

if (!require(stringr)) {
  install.packages("stringr")
}
library(stringr)
if (!require(webshot)) {
    install.packages("webshot")

}
webshot :: install_phantomjs()




shinyServer(function(input, output, session) {
    #コロナのデータ読み込み
  data<-
    fread("kanagawa.csv", encoding="UTF-8") %>%
    mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
    select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y)%>%
    filter(居住都道府県=="神奈川県")%>%
    filter(居住市区町村=="川崎市"|居住市区町村=="")%>%
    mutate(Residential_City=paste0(居住市区町村,備考))%>%
    select(-居住市区町村,-備考,-X,-Y)%>%
    rename("Fixed_Date"="確定日",
           "Hospital_Pref"="受診都道府県",
           "Residential_Pref"="居住都道府県")%>%
    filter(Residential_City!="")
  data2<-fread("kanagawa.csv", encoding="UTF-8") %>%
    mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
    select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y)%>%
    filter(居住都道府県=="神奈川県")%>%
    filter(居住市区町村!="川崎市",居住市区町村!="")%>%
    rename("Residential_City"="居住市区町村")%>%
    select(-備考)%>%
    rename("Fixed_Date"="確定日",
           "Hospital_Pref"="受診都道府県",
           "Residential_Pref"="居住都道府県")
  
  patient<-
    read.csv("https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv") %>%
    filter(!str_detect(居住地,"管内")) %>%
    filter(発表日>="2020-12-01") %>%
    rename("Fixed_Date"="発表日","Residential_City"="居住地") %>%
    select(-年代,-性別)%>%
    mutate(Residential_City = str_replace(Residential_City,"神奈川県",""))%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))
  
  kanagawa<-read.csv("kanagawa2.csv") %>%
    select(-X,-note)%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))
  
  
  kanagawa2<-rbind(kanagawa,patient) %>%
    mutate(Hospital_Pref ="神奈川県",
           Residential_Pref="神奈川県"
    )
  
  xy<-read.csv("xy.csv") %>%
    select(-X.1)%>%
    rename("Residential_City"="居住市区町村")
  chigasaki<-
    read.csv("chigasaki.csv")%>%
    mutate(Hospital_Pref ="神奈川県",
           Residential_Pref="神奈川県"
    )%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))%>%
    left_join(xy,by="Residential_City")%>%
    filter(!is.na(X))
  
  list1<-read.csv("list.csv")
  data3<-
    data%>%
    left_join(list1,by=c("Residential_City"="list"))%>%
    select(-Residential_City)%>%
    rename("Residential_City"="管内")%>%
    filter(!is.na(X))
  
  kanagawa2<-
    left_join(kanagawa2,xy,by="Residential_City") %>%
    mutate(Fixed_Date=as.Date(Fixed_Date))%>%
    filter(!is.na(X))
  
  
  kawasaki<-
    read.csv("kawasaki.csv") %>%
    select(-X)%>%
    mutate(Fixed_Date=as.Date(Fixed_Date))%>%
    left_join(list1)%>%
    select(-note,-管内,-Residential_City)%>%
    rename("Residential_City"="list")
  
  data7<-bind_rows(data2,data3,kanagawa2,kawasaki,chigasaki)

    date<-
      kawasaki%>%
      data.frame()%>%
      arrange(desc(Fixed_Date))%>%
      distinct(Fixed_Date)
    
    output$date<-
      renderUI({
        dateInput("x",
                  label = h5("累積日数の最後の日付入力"),
                  max = date[1,1],
                  value = date[1,1])
      })
    

    jinko<-read.csv("jinko.csv")
    jinko<-data.frame(jinko)

    output$covid_map <- renderLeaflet({
        #ここ書き換える
        switch (input$button,
                leaflet1 = { date<-lubridate::ymd(input$x)-input$y
                #集計
                data7.1<-data7%>%
                    filter(Fixed_Date>=date,Fixed_Date<=lubridate::ymd(input$x))%>%
                    group_by(Residential_City,X,Y)%>%
                    summarise(count=n())%>%
                    filter(X>0,Y>0)

                #leafletの可視化
                leaflet(data7.1) %>% addTiles() %>%
                    addProviderTiles(providers$CartoDB.Positron) %>%
                    #setView(lng=139.4725,lat=35.4478,zoom=10)%>%
                    fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                    addCircleMarkers(~X, ~Y, stroke=FALSE,
                                     radius =sqrt(data7.1$count)*input$en,
                                     label = ~htmlEscape(Residential_City),
                                     labelOptions = labelOptions(direction = 'auto',noHide = T, textOnly = TRUE,textsize = "10px"))%>%
                    addCircleMarkers(~X, ~Y, stroke=FALSE,
                                     radius =sqrt(data7.1$count)*input$en,
                                     label = ~htmlEscape(count),
                                     labelOptions = labelOptions(direction = 'bottom',noHide = T, textOnly = TRUE,textsize = "10px"),
                    )%>%addControl(tags$div(HTML(paste(date,lubridate::ymd(input$x),sep = "~")))  , position = "topright") },
                leaflet2={
                    date<-lubridate::ymd(input$x)-input$y
                    #集計
                    data7.1<-data7%>%
                        filter(Fixed_Date>=date,Fixed_Date<=lubridate::ymd(input$x))%>%
                        group_by(Residential_City,X,Y)%>%
                        summarise(count=n())%>%
                        filter(X>0,Y>0)
                    jinko2<-left_join(data7.1,jinko,by=c("Residential_City"="City"))
                    jinko3<-jinko2%>%
                        mutate(count_j=count/jinko*100000)
                    leaflet(jinko3) %>% addTiles() %>%
                        addProviderTiles(providers$CartoDB.Positron) %>%
                        #setView(lng=139.4825,lat=35.4478,zoom=10)%>%
                        fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                        addCircleMarkers(~X, ~Y, stroke=FALSE,
                                         radius =sqrt(jinko3$count_j)*input$en,
                                         label = ~htmlEscape(Residential_City),
                                         labelOptions = labelOptions(direction = 'auto',noHide = T, textOnly = TRUE,textsize = "10px"))%>%
                        addCircleMarkers(~X, ~Y, stroke=FALSE,
                                         radius =sqrt(jinko3$count_j)*input$en,
                                         label = ~htmlEscape(round(count_j,digits = 4)),
                                         labelOptions = labelOptions(direction = 'bottom',noHide = T, textOnly = TRUE,textsize = "10px")
                        )%>%addControl(tags$div(HTML(paste(date,lubridate::ymd(input$x),sep = "~")))  , position = "topright")
                    }
                )
    }
    )
    }
    )
      