library(shiny)
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggrepel)
library(shinyWidgets)
library(shinyjs)
library(plotly)


ui <- fluidPage(
                useShinyjs(),
                list(tags$head(HTML('<link rel="icon", href="nestimg.jpg", 
                                 type="image/png" />'))),
                div(style="padding: 1px 0px; width: '100%'",
                    titlePanel(windowTitle="NEST thermostat data plot", title = "")),
    
                titlePanel(title= div(img(src="nestimg.jpg", height = 50, width = 50),  "NEST thermostat data plot")),
    
                hr(),
                p(id="help", tags$a(href="https://takeout.google.com/settings/takeout" , target="_blank", "Follow this link"), "to", strong("download"),"CSV file form your", strong("Google take out"),"account
                  and then upload it bellow using Browse option ", "at Google Takeout first", strong("deselect all, scroll down to Nest , check box Nest, 
                  click types and leave only thermostat option checked"), "so it doesn take long to create download, it should only take a minute and you should get an email
                  that data is ready to download, extract the zip file and in it , there is a sensors,csv with dates for example", strong("2020-02-sensors.csv"), ", upload it."),
                
                #p(checkboxInput("dumdat", "Or just try our test data")),
                hr(),
                 
                fluidRow(column(3, fileInput("file1", "Choose CSV file"), multiple = FALSE, accept = (".csv")),
                          column(3,  uiOutput("day"), selectInput("day","Select day:", choices = NULL)),
                          column(3,  uiOutput("week"), selectInput("week","Select week:", choices = NULL)),
                          column(3,  uiOutput("month"), selectInput("month","Select month:", choices = NULL))),
                hr(),
                 
                fluidRow(plotlyOutput("chart2")),
                hr(),
                fluidRow(column(6,plotOutput("chart3")),
                         column(6,plotlyOutput("chart4"))),
                 
                hr(),
                fluidRow((column(9, actionButton("tog","Toggle data"), offset = 5))),
                fluidRow(hidden(tableOutput('table1')))
)

server <- function(input, output, session) {
    
    #load data
    nst <- reactive({ 
        inFile <- input$file1
        
        validate(need(tools::file_ext(input$file1) =="csv", "Wrong" ))
        
        #print(tools::file_ext(input$file1))
        
        
        read.csv(inFile$datapath, stringsAsFactors=FALSE)
    })
    #peocess dataframe
    nest <- reactive({
            nst() %>% mutate(dates= as.POSIXct(paste(nst()$Date,Time)), days= nst()$Date) %>% select(dates,avg.temp., days) %>%
            rename(temp=avg.temp.)
    })
    
    #add column with weeknumbers
    aggweeks <- reactive({ nest() %>% mutate(weeknum = as.numeric(format(nest()$dates, format = "%U"))+1)
                
    })
    
    #aggweeks()$weeknum <- aggweeks()$weeknum +1
    
    #add column with month number
    montc <- reactive({ nest() %>% mutate(montnum = as.numeric(format(nest()$dates, format="%m")))
        
    })
    
    #update choices when file is uploaded
    observeEvent( nest(), { 
        day_choices <- nest()$days
        updateSelectInput(session, "day", choices= day_choices)
    })
    
    observeEvent( aggweeks(), {
        week_choices <- aggweeks()$weeknum 
        updateSelectInput(session, "week", choices= week_choices)
    })
    
    observeEvent( montc(), {
        month_choices <- c(0,montc()$montnum)
        updateSelectInput(session, "month", choices= month_choices)
    })
    
    #Create plot if input changes
    
    #Daily
    observeEvent( input$day, {
        
        day_selected <- input$day
        
        nestd <- nest() %>% filter(days==day_selected)
        
        daymm <- nestd %>% filter(temp==max(temp) | temp==min(temp)) %>%  filter(row_number()==1 | row_number()==n())
        
        
        
        output$chart2 <- renderPlotly(ggplot(nestd, aes(x=dates, y=temp))+ geom_line() + ylab("Temperature")+
                                     xlab("Day")+ggtitle("NEST Thermostat")+ theme_minimal())
        
        
        output$chart3 <- renderPlot(ggplot(daymm, aes(dates,temp, fill=dates))+geom_bar(stat = "identity")+
                                        geom_text(aes(label=temp), vjust=1.6, color="white", size=5)+
                                        theme_minimal())
        
        df <- nst()  %>%  filter(Date==input$day)
        output$table1 <- renderTable(df)
        
        
    })
    
    #Weekly
    
    observeEvent( input$week, {
        
        week_selected <- input$week
        
        nestw <- aggweeks() %>% filter(weeknum==week_selected)
        
        #wekmm <- nestw %>% filter(temp==max(temp) | temp==min(temp)) %>%  filter(row_number()==1 | row_number()==n())
        
        wekmm <- aggweeks() %>% group_by(weeknum) %>% summarize(maxtemp = max(temp)) %>%
            ggplot(., aes(weeknum , maxtemp , fill=maxtemp))+  geom_bar(stat = "identity")+
            geom_text(aes(label=maxtemp), vjust=1.6, color="white", size=5)+
            theme_hc()
        
        
        output$chart2 <- renderPlotly( ggplot(nestw, aes(x=dates, y=temp))+ geom_line() + ylab("Temperature")+
                                         xlab("Day")+ggtitle("NEST Thermostat")+ theme_minimal())
        
        output$chart3 <- renderPlot(wekmm)
        
        df1 <- nst() %>% filter(nst()$Date %in% subset(aggweeks(),weeknum==input$week)$days)
        output$table1 <- renderTable(df1)
        
    })
    

    

    
    
    #Monthly
    
    observeEvent( input$month, {
        
        month_selected <- input$month 
        
        if (month_selected==0) {
            ggplot()
        }
        
        else{
            nest1 <- nest()
            output$chart2 <- renderPlotly(ggplot(nest1, aes(x=dates, y=temp)) +
                                               geom_line() + ylab("Den")+xlab("Teplota")+ggtitle("NEST Thermostat")+
                                               theme_minimal())
        }           
    })
    
    #Toggle table data
    
    observeEvent(input$tog, {
        
       toggle("table1")
        
    })
    
    #Toggle download help
    
    observeEvent(input$file1,{
        
        toggle("help")
    })
    
}

shinyApp(ui, server)

