# ---模糊地理位置的具体位置信息转换查询：----
# ---制作人：张彪----------------------------
# ---时间：2018-8-2--------------------------

# 功能介绍：
# 使用该查询工具，可以依据模糊的地理位置，得到相对详细的位置信息
# 这些信息主要包括：经纬度，所处的省市县等
# 还可以标注在地图上，在地图上查看所查找位置周边信息




# ---------------1.所需工具包列表：-----------------------
library(shiny)
library(REmap)
library(rjson)
library(RCurl)
library(leaflet)
library(shinydashboard)
library(shinythemes)


# ---------------2.前端设计：-----------------------------
ui = shinyUI(
     fluidPage(
          titlePanel(div('模糊地理位置信息查询工具',style = "font-family:'STZhongsong';color:orange")),
          br(),
          fluidRow(
               column(
                    width = 3,
                    textInput('lbs','请输入模糊地址：','山东浪潮科技园'),
                    box(
                         style = 'font-size:110%;',
                         title = h5(strong('经纬度信息展示:')),
                         width = 12,
                         height = 100,
                         solidHeader = F,
                         background = 'yellow',
                         status = 'warning',
                         collapsible = F,
                         tableOutput('map_lbs_ag')
                    ),
                    br(),
                    br(),
                    box(
                         style = 'font-size:110%;',
                         width = 12,
                         title = h5(strong('省市县信息展示:')),
                         height = 400,
                         solidHeader = F,
                         background = 'yellow',
                         status = 'warning',
                         collapsible = F,
                         tableOutput('text_lbs')
                    )
               ),
               column(
                    width = 9,
                    strong('地图信息展示:'),
                    br(),
                    box(
                         style = 'font-size:110%;',
                         width = 12,
                         height = 400,
                         solidHeader = F,
                         background = 'yellow',
                         status = 'warning',
                         collapsible = F,
                         leafletOutput('map_lbs')
                    )
                    
               )
          )
     )
)


# --------------------------------------------------------------------

# 百度地图接口调用：

# 获取模糊地理位置的经纬度信息：
get_goord = function(address){
     ak = 'IhMA1O1bfchXE1Q6xgDUoE9cepoi4EYx'
     url = paste('http://api.map.baidu.com/geocoder/v2/?ak=',
                 ak,'&callback=renderOption&output=json&address=',
                 address,sep = '')
     url_string = URLencode(url)
     json = readLines(url_string,warn = F,encoding = 'UTF-8')
     geocoord = fromJSON(substr(json,regexpr('\\(',json)+1,nchar(json)-1))
     lng = geocoord$result$location$lng
     lat = geocoord$result$location$lat
     result = data.frame(lat = lat,lng = lng)
     return(result)
}

# 根据经纬度信息获取省市县位置信息：
get_position = function(position){
     position = paste(position$lat,position$lng,sep = ',')
     ak = 'IhMA1O1bfchXE1Q6xgDUoE9cepoi4EYx'
     url = paste('http://api.map.baidu.com/geocoder/v2/?ak=',
                 ak,'&callback=renderReverse&location=',
                 position,'&output=json',sep = '')
     url_string = URLencode(url)
     jason = readLines(url_string, warn = F, encoding = 'UTF-8')
     geo = fromJSON(substr(jason,regexpr('\\(',jason) + 1,nchar(jason) - 1))
     prov = geo$result$addressComponent$province
     city = geo$result$addressComponent$city
     dist = geo$result$addressComponent$district
     lbs_data = data.frame(
          label = c('所在地省份','所在地城市','所在地区县'),
          value = c(prov,city,dist)
     )
     return(lbs_data)
}

# ------------3.服务端-----------------------

server = function(input,output){
     output$text_lbs = renderTable({
          lbs_data = get_goord(input$lbs)
          lbs_data_new = get_position(lbs_data)
          lbs_data_new
     })
     
     output$map_lbs_ag = renderTable({
          lbs_data = get_goord(input$lbs)
          lbs_data
     })
     
     output$map_lbs = renderLeaflet({
          lbs_data = get_goord(input$lbs)
          leaflet() %>%
               addTiles() %>%
               setView(lat = lbs_data$lat,lng = lbs_data$lng,zoom = 12) %>%
               addMarkers(lat = lbs_data$lat,lng = lbs_data$lng,popup = input$lbs)
     })
}


shinyApp(ui = ui,server = server)
