library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(readxl)
library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(dashboardthemes)
datas <-read.csv("C:/Users/HP/Downloads/combined dataset.csv")
data_2015<-read_excel("C:/Users/HP/Downloads/2015_new.xlsx",col_names = TRUE)
corr_2015<-cor(as.matrix(data_2015[,-c(1,2,3)]))
data_2016<-read_excel("C:/Users/HP/Downloads/2016_new.xlsx",col_names = TRUE)
corr_2016<-cor(as.matrix(data_2016[,-c(1,2,3)]))
data_2017<-read_excel("C:/Users/HP/Downloads/2017_new.xlsx",col_names = TRUE)
corr_2017<-cor(as.matrix(data_2017[,-c(1,2,3)]))
support_15<-read_excel("C:/Users/HP/Downloads/2015_new.xlsx",range = "support_15!A3:B13")
support_16<-read_excel("C:/Users/HP/Downloads/2016_new.xlsx",range = "supportreg_16!A3:B13")
support_17<-read_excel("C:/Users/HP/Downloads/2017_new.xlsx",range = "supportreg_17!A3:B13")
gdp_reg_15<-read_excel("C:/Users/HP/Downloads/2015_new.xlsx",range = "gdpreg_15!A3:B13")
gdp_reg_16<-read_excel("C:/Users/HP/Downloads/2016_new.xlsx",range = "gdpreg_16!A3:B13")
gdp_reg_17<-read_excel("C:/Users/HP/Downloads/2017_new.xlsx",range = "gdpreg_17!A3:B13")
health_15<-read_excel("C:/Users/HP/Downloads/2015_new.xlsx",range = "lifeexpectancy_15!A3:B13")
health_16<-read_excel("C:/Users/HP/Downloads/2016_new.xlsx",range = "lifeexpectancy_16!A3:B13")
health_17<-read_excel("C:/Users/HP/Downloads/2017_new.xlsx",range = "lifeexpectancy_17!A3:B13")
pivreg_15<-read_excel("C:/Users/HP/Downloads/2015_new.xlsx",range = "happinessreg_15!A3:B13")
pivreg_16<-read_excel("C:/Users/HP/Downloads/2016_new.xlsx",range = "Happinessreg_16!A3:B13")
pivreg_17<-read_excel("C:/Users/HP/Downloads/2017_new.xlsx",range = "happinessreg_17!A3:B13")
gdpyears<- read_excel("C:/Users/HP/Downloads/gdp over years.xlsx",col_names = TRUE)
lifeyears <- read_excel("C:/Users/HP/Downloads/life expectancy over years.xlsx",col_names = TRUE)
socyears<-read_excel("C:/Users/HP/Downloads/social support over years.xlsx",col_names = TRUE)
last10_2015<-data_2015 %>% select(Country,Region,`Happiness.Rank`,`Happiness.Score`) %>% tail(n=10)
last10_2016<-data_2016 %>% select(Country,Region,`Happiness Rank`,`Happiness Score`) %>% tail(n=10)
last10_2017<-data_2017 %>% select(Country,Region,`Happiness Rank`,`Happiness Score`) %>% tail(n=10)
top10_2015<-data_2015 %>% select(Country,Region,`Happiness.Rank`,`Happiness.Score`) %>% head(n=10)
top10_2016<-data_2016 %>% select(Country,Region,`Happiness Rank`,`Happiness Score`) %>% head(n=10)
top10_2017<-data_2017 %>% select(Country,Region,`Happiness Rank`,`Happiness Score`) %>% head(n=10)

ui<-dashboardPage(
  skin = 'red',
  dashboardHeader( title = "World_Happines_Report"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("DATASET", tabName = "Dataset", icon = icon("th")),
      menuItem("CORRELATION", icon = icon("th"), tabName = "CoRr"),
      menuItem("HAPPINESS BY REGION", icon = icon("th"), tabName = "Happyness"),
      menuSubItem("REGION", tabName = "HappinesScore"),
      menuItem("OVER YEARS", icon = icon("th"), tabName = "hap"),
      menuItem("HAPPINESS OVER YEARS", icon = icon("th"), tabName = "xyz"),
      menuItem("LAST 10 HAPPIEST COUNTRIES", icon = icon("th"), tabName = "top"),
      menuItem("TOP 10 HAPPIEST COUNTRIES", icon = icon("th"), tabName = "bot")
    ),
    width = 300
  ),
  
  
  dashboardBody(
    shinyDashboardThemes(
      theme="grey_dark"),
    tabItems(
      tabItem(tabName = "Dataset", 
              fluidRow(
                
                fluidPage(
                  tags$style(".fa-database {color:#E87722}"),
                  fluidRow(column(DT::dataTableOutput("RawData"),
                                  width = 12)))
                
              )
      ),
      tabItem(tabName = "CoRr", 
              fluidRow(
                box(
                  selectInput("place",
                              "Input Year:",
                              c("2015","2016","2017"))),
                fluidRow(
                  box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("plothist",height=600),width=12))
                
              )
      ),
      tabItem(tabName = "Happyness", 
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("happyness",height=600),width=12))
              
              
      ),
      tabItem(tabName = "HappinesScore", 
              fluidRow(
                box(
                  selectInput("Factors",
                              "Input Factors:",
                              c("GDP","Social Support","Life Expectancy"))),
                fluidRow(
                  box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("HappinesScore",height=600),width=12))
                
              )
      ),
      tabItem(tabName = "xyz", 
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("Hoy",height=600),width=12))
              
              
      ),
      tabItem(tabName = "top", 
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("top1",height=600),width=12)),
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("top2",height=600),width=12)),
             fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("top3",height=600),width=12))
                
              ),
      tabItem(tabName = "bot", 
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("bot1",height=600),width=12)),
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("bot2",height=600),width=12)),
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("bot3",height=600),width=12))
              
      ),
      tabItem(tabName = "hap", 
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("hap1",height=600),width=12)),
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("hap2",height=600),width=12)),
              fluidRow(
                box(title="Distribution Of Variable:",background = "light-blue",solidHeader = TRUE,plotOutput("hap3",height=600),width=12))
              
      )
      
      
      
      
      
      
      
      
      
    
    
)
    
  )
  
)



# Define server logic required to draw a histogram
server <- function(input, output)
{
  output$RawData <-
    DT::renderDataTable(
      DT::datatable({
        datas
      },
      options = list(lengthMenu=list(c(5,15,20,100,468),c('5','15','20','100','468')),pageLength=10,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'F05A0A', 'color': ''});",
                       "}"),
                     columnDefs=list(list(className='dt-center',targets="_all"))
      ),
      filter = "top",
      selection = 'multiple',
      style = 'bootstrap',
      class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("Country","Region","Happiness.Rank","Happiness.Score","GDP.Per.Capita","Social.Support","Life.Expectancy","Freedom","Government.Corruption","Generosity","Dystopia.Residual","Years")
      ))
  output$plothist <- renderPlot({
    if (input$place == "2015") {
      corrplot(corr_2015,type="upper",order="original",method = "circle", tl.col = "black",tl.srt = 45)
    }else if (input$place == "2016") {
      corrplot(corr_2016,type="upper",
               
               order="original",method = "square",
               
               tl.col = "black",
               
               tl.srt = 45)
    }else if(input$place == "2017") {
      corrplot(corr_2017,type = "upper",method = "color",order="original")
    }
  })
  
  output$happyness <- renderPlot({  
    plot_15<-ggplot(data = pivreg_15,mapping =aes(x = `Region`,y = `Sum of Happiness.Score` ))
    plot_15<- plot_15 + 
      # apply basic black and white theme - this theme removes the background colour by default
      theme_bw() + 
      # remove gridlines. panel.grid.major is for vertical lines, panel.grid.minor is for horizontal lines
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            # remove borders
            panel.border = element_blank(),
            # removing borders also removes x and y axes. Add them back
            axis.line = element_line())
    
    
    plot_16<-ggplot(data = pivreg_16,mapping =aes(x = `Region`,y = `Happiness.Score.` ))
    plot_16<- plot_16 + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line())
    
    plot_17<-ggplot(data = pivreg_17,mapping =aes(x = `Region`,y = `Happiness.Score` ))
    plot_17<- plot_17 + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line())
    
    
    plot_15 + geom_bar(stat="identity",aes(fill=Region))  
    
    plot_16 + geom_bar(stat="identity",aes(fill=Region)) 
    
    plot_17 + geom_bar(stat="identity",aes(fill=Region))
  })
  output$HappinesScore <- renderPlot({
    
    if (input$Factors == "GDP") {
      plot_15<-ggplot(data = gdp_reg_15,mapping =aes(x = `Region`,y = `GDP.Per.Capita.` ))
      plot_15<- plot_15 + 
        
        theme_bw() + 
        
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              
              panel.border = element_blank(),
              axis.line = element_line())
      
      
      plot_16<-ggplot(data = gdp_reg_16,mapping =aes(x = `Region`,y = `GDP.per.Capita.` ))
      plot_16<- plot_16 + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line())
      
      plot_17<-ggplot(data = gdp_reg_17,mapping =aes(x = `Region`,y = `GDP.Per.Capita` ))
      plot_17<- plot_17 + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line())
      
      
      plot_15 + geom_bar(stat="identity",aes(fill=Region))  
      
      plot_16 + geom_bar(stat="identity",aes(fill=Region)) 
      
      plot_17 + geom_bar(stat="identity",aes(fill=Region)) 
    }
    
    else if (input$Factors == "Social Support") {
      
      plothlth_15<-ggplot(data = support_15,mapping =aes(x = `Region`,y = `Social.Support.` ))
      plothlth_15<- plothlth_15 + 
        
        theme_bw() + 
        
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              
              panel.border = element_blank(),
              axis.line = element_line())
      
      
      plothlth_16<-ggplot(data = support_16,mapping =aes(x = `Region`,y = `Social.support.` ))
      plothlth_16<- plothlth_16 + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line())
      
      
      
      
      plothlth_15 + geom_bar(stat="identity",aes(fill=Region))  
      
      plothlth_16 + geom_bar(stat="identity",aes(fill=Region)) 
      
      
      
      
      
      
    }
    else if (input$Factors == "Life Expectancy") {
      plothlth_15<-ggplot(data = health_15,mapping =aes(x = `Region`,y = `Life.Expectancy.` ))
      plothlth_15<- plothlth_15 + 
        
        theme_bw() + 
        
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              
              panel.border = element_blank(),
              axis.line = element_line())
      
      
      plothlth_16<-ggplot(data = health_16,mapping =aes(x = `Region`,y = `Life.Expectancy.` ))
      plothlth_16<- plothlth_16 + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line())
      
      plothlth_17<-ggplot(data = health_17,mapping =aes(x = `Region`,y = `Life.expectancy` ))
      plothlth_17<- plothlth_17 + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line())
      
      
      plothlth_15 + geom_bar(stat="identity",aes(fill=Region))  
      
      plothlth_16 + geom_bar(stat="identity",aes(fill=Region)) 
      
      plothlth_17 + geom_bar(stat="identity",aes(fill=Region)) 
      
      
    }
    
  }) 
  output$Hoy <- renderPlot({
    df1<-read_excel("C:/Users/HP/Downloads/years happiness score.xlsx")
    df1
    plot(df1$Years,df1$Happiness.Score,
         main = "HAPPINESS SCORE OVER YEARS",
         type = "b",
         xlab = "YEARS",
         ylab = "HAPPINESS SCORES",col = "red",lwd = 2,cex = 5)
  })
  output$top1 <- renderPlot({
      coul<-brewer.pal(11,"Set3")
      windows.options(width=10,height=30)
      barplot(last10_2015$`Happiness.Score`,
              main = "LAST 10 happiest countries 2015",
              names.arg = last10_2015$Country,
              xlab = "COUNTRIES",
              ylab = "RANKS",
              col = coul,
              #par(margin(15,3,3,1)),
              las=2)
  })
  output$top2 <- renderPlot({
      coul<-brewer.pal(11,"Set3")
      windows.options(width=10,height=30)
      barplot(last10_2016$`Happiness Score`,
              main = "LAST 10 happiest countries 2016",
              names.arg = last10_2016$Country,
              xlab = "COUNTRIES",
              ylab = "RANKS",
              col = coul,
              #par(margin(15,3,3,1)),
              las=2)
  })
  output$top3 <- renderPlot({
      coul<-brewer.pal(11,"Set3")
      windows.options(width=10,height=30)
      barplot(last10_2017$`Happiness Score`,
              main = "last 10 happiest countries 2017",
              names.arg = last10_2017$Country,
              xlab = "COUNTRIES",
              ylab = "RANKS",
              col = coul,
              #par(margin(15,3,3,1)),
              las=2)
    
  })
  
  output$bot1 <- renderPlot({
    coul<-brewer.pal(11,"Set3")
    #windows.options(width=10,height=30)
    barplot(top10_2015$`Happiness.Score`,
            main = "Top 10 happiest countries 2015",
            names.arg = top10_2015$Country,
            xlab = "COUNTRIES",
            ylab = "RANKS",ylim = c(0,8),
            col = coul,
            #par(margin(15,3,3,1)),
            las=2)
  
  })
  
  output$bot2 <- renderPlot({
    coul<-brewer.pal(11,"Set3")
    #windows.options(width=10,height=30)
    barplot(top10_2016$`Happiness Score`,
            main = "Top 10 happiest countries 2016",
            names.arg = top10_2016$Country,
            xlab = "COUNTRIES",
            ylab = "RANKS",
            col = coul,
            #par(margin(15,3,3,1)),
            las=2)
    
  })
  
  
  output$bot3 <- renderPlot({
    coul<-brewer.pal(11,"Set3")
    #windows.options(width=10,height=30)
    barplot(top10_2017$`Happiness Score`,
            main = "Top 10 happiest countries 2017",
            names.arg = top10_2017$Country,
            xlab = "COUNTRIES",
            ylab = "RANKS",
            col = coul,
            #par(margin(15,3,3,1)),
            las=2)
    
  })
  output$hap1 <- renderPlot({
    plot(socyears$YEARS,socyears$`SOCIAL.SUPPORT`,
         type = "b",col = "dodger blue",xlab= "YEARS",
         ylab = "SOCIAL SUPPORT") 
  })
  output$hap2 <- renderPlot({
    plot(gdpyears$YEARS,gdpyears$`GDP PER CAPITA`,
         type = "b",col = "purple",xlab= "YEARS",
         ylab = "GDP PER CAPITA")
    
    
  })
  
  output$hap3 <- renderPlot({
    
    plot(lifeyears$YEARS,lifeyears$`LIFE EXPECTANCY`,
         type = "b",col = "red",xlab= "YEARS",
         ylab = "LIFE EXPECTANCY")
    
  })
  
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
