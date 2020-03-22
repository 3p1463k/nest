## app.R ##
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggrepel)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(dashboardthemes)




ui <- dashboardPagePlus(
    
    useShinyjs(),
    
    header = dashboardHeaderPlus(title = tagList(
        span(class = "logo-lg", "Nest thermostat plot"), 
        img(src = "https://image.flaticon.com/icons/svg/204/204074.svg"))),
    
    sidebar = dashboardSidebar(
        
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Help", tabName = "help", icon = icon("question"),badgeLabel = "info", badgeColor = "green"),
            menuItem("Summary", tabName = "summary" , icon = icon("bar-chart-o")),
            menuItem( actionButton("dataset", "Load test dataset",  class="btn btn-secondary btn-xs") , icon = icon("bar-chart-o")),
            menuItem("Upload",  fileInput("file1", "Choose CSV file"), multiple = FALSE, accept = (".csv"), icon = icon("upload"),startExpanded = TRUE),
            menuItem(selectInput("day","Select day:", choices = NULL), icon = icon("calendar")),
            menuItem(selectInput("week","Select week:", choices = NULL), icon = icon("calendar")),
            menuItem(selectInput("month","Select month:", choices = NULL), icon = icon("calendar"))
            
        )
    ),
    
    body = dashboardBody(
        
        ### changing theme
        shinyDashboardThemes(
            theme = "grey_dark"
        ),
        
        # Boxes need to be put in a row (or column)
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    
                    fluidRow(
                        box(plotlyOutput("chart2", height = 650) %>% withSpinner(type = "5"), width = 9),
                        
                        box(plotOutput("chart3", height = 300), width = 3),
                        
                        box(plotOutput("chart4", height = 300), width = 3)
                        
                    ),
                    
                    hr(),
                    
                    fluidRow((column(12, actionButton("tog","Toggle data table", class = "btn btn-primary btn-lg btn-block")))),
                    
                    hr(),
                    
                    fluidRow(tableOutput('table1'))
                    
            ),
            
            # Second tab content
            tabItem(tabName = "help",
                    h1("HOW to download your Nest thermostat data"),
                    hr(),
                    
                    h3(id="help",p(tags$a(href="https://takeout.google.com/settings/takeout" , target="_blank", "Follow this link"), 
                                   "to", strong("download"),"CSV file from your", strong("Google take out"),"account and then upload it using Browse... option in the side bar panel. ")) ,
                    hr(),
                    h4(p("At Google Takeout first: ", strong("deselect all"))),
                    hr(),
                    
                    fluidRow(
                        box(div(img(src = "gt1.png")))
                    ),
                    h4(p("- scroll down to Nest , check box Nest,")),
                    hr(),
                    
                    fluidRow(
                        box(div(img(src = "gt5.png")))
                    ),
                    h4(p("- click types and leave only thermostat option checked", "so it doesn take long to create the download")),
                    hr(),
                    
                    fluidRow(
                        box(div(img(src = "gt2.png")))
                    ),
                    
                    h4(p("- scroll down the page, click Next step and in the next step click Create Export")),
                    hr(),
                    
                    fluidRow(
                        box(div(img(src = "gt3.png")))
                    ),
                    
                    h3(p("You will get an email with the link to download a zip, there will be a csv file for each month, for example", strong("2020-01-sensors.csv"), ", upload it and analyze your data.")),
                    hr()
                    
                    
            ),
            
            # Third tab content
            
            tabItem(tabName = "summary",
                    
                    fluidRow(box(plotOutput("chart5", height = 300), width =4),
                             box(plotOutput("chart6", height = 300), width = 4),
                             box(plotOutput("chart7", height = 300), width = 4)
                             
                    ),
                    
                    fluidRow(box(plotOutput("chart9", height = 300), width = 4),
                            box(plotOutput("chart10", height = 300), width = 4),
                            box(plotOutput("chart11", height = 300), width = 4)
                    ),
                    
                    fluidRow(box(plotOutput("chart13", height = 300), width = 4),
                             box(plotOutput("chart14", height = 300), width = 4),
                             box(plotOutput("chart15", height = 300), width = 4)
                    )
                    
            )
        )
    ),
    
    title = "Nest thermostat plot"
)

server <- function(input, output, session) {
    
    
    #load data
    nst <- reactive({
        
        inFile <- input$file1
        
        if (is.null(input$file1)) {
            
            inFile <- read.csv("2020-01-sensors.csv",  stringsAsFactors=FALSE)
            
        }
        
        #validate(need(tools::file_ext(input$file1) =="csv", "Wrong file" ))
        
        else{
            read.csv(inFile$datapath, stringsAsFactors=FALSE)
        }
        
    })
    
    #process dataframe
    nest <- reactive({
        nst() %>% mutate(dates= as.POSIXct(paste(nst()$Date,Time)), days= nst()$Date) %>% select(dates,avg.temp., days, avg.humidity.) %>%
            rename(temp=avg.temp., humidity=avg.humidity.)
    })
    
    #add column with weeknumbers
    aggweeks <- reactive({ nest() %>% mutate(weeknum = as.numeric(format(nest()$dates, format = "%U"))+1)
        
    })
    
    #add column with month number
    montc <- reactive({ nest() %>% mutate(montnum = as.numeric(format(nest()$dates, format="%m")))
    })
    
    
    #update choices when file is uploaded
    observeEvent(montc(), {
        month_choices <- c(montc()$montnum, 0)
        updateSelectInput(session, "month", choices= month_choices)
    })
    
    observeEvent(aggweeks(), {
        week_choices <- aggweeks()$weeknum 
        updateSelectInput(session, "week", choices= week_choices)
    })
    
    
    
    observeEvent(nest(), { 
        day_choices <- nest()$days
        updateSelectInput(session, "day", choices= day_choices)
    })
    
    
    
    #Create plot if input changes
    #Daily
    
    observeEvent(input$day, {
        
        day_selected <- input$day
        
        nestd <- nest() %>% filter(days==day_selected)
        
        daymm <- nestd %>% filter(temp==max(temp) | temp==min(temp)) %>%  filter(row_number()==1 | row_number()==n())
        
        daymin <- daymm %>% filter(temp==min(daymm$temp)) %>% slice(0:1)
        
        daymax <- daymm %>% filter(temp==max(daymm$temp)) %>% slice(0:1)
        
        dhum <- nest() %>% group_by(days) %>%  filter(days==day_selected) %>% filter(humidity== max(humidity)| humidity== min(humidity))%>%
            filter(row_number()==1 | row_number()==n())
        
        output$chart2 <- renderPlotly({ggplot(nestd, aes(x=dates, y=temp))+ geom_line(colour="blue") + ylab("Temperature")+ xlab("Day")+ggtitle("NEST Thermostat")+
                                          geom_point(data=daymin, color="green", size=3)+ geom_point(data=daymax, color="red", size=3)+
                                          theme_minimal()})
        
        
        output$chart3 <- renderPlot(ggplot(daymm, aes(dates,temp, fill=dates))+geom_bar(stat = "identity")+
                                        geom_text(aes(label=temp), vjust=1.6, color="white", size=10)+ggtitle("Daily")+
                                        ylab("Temperature")+ xlab("MINIMUM AND MAXIMUM TEMPERATURE")+
                                        theme_minimal())
        
        output$chart4 <- renderPlot(ggplot(dhum, aes(dates,humidity, fill=dates))+geom_bar(stat = "identity")+
                                        geom_text(aes(label=humidity), vjust=1.6, color="white", size=10)+ggtitle("Daily MIN / MAX humidity")+
                                        ylab("Humidity")+ xlab("MINIMUM AND MAXIMUM HUMIDITY")+
                                        theme_minimal())
        
        
        df <- nst()  %>%  filter(Date==input$day)
        output$table1 <- renderTable(df)
        
        
    })
    
    #Weekly
    
    observeEvent(input$week, {
        
        week_selected <- input$week
        
        nestw <- aggweeks() %>% filter(weeknum==week_selected)
        
        weekmm <- nestw %>% filter(temp==max(temp) | temp==min(temp)) %>%  filter(row_number()==1 | row_number()==n())
        
        whum <- aggweeks() %>% group_by(weeknum) %>% filter(weeknum==week_selected) %>% filter(humidity== max(humidity) | humidity== min(humidity)) %>%
            filter(row_number()==1 | row_number()==n())
        
        weekmin <- nestw %>% filter(temp==min(weekmm$temp)) %>% slice(0:1)
        
        weekmax <- nestw %>% filter(temp==max(weekmm$temp)) %>% slice(0:1)
        
        
        output$chart2 <- renderPlotly( ggplot(nestw, aes(x=dates, y=temp))+ geom_line(color="blue") + ylab("Temperature")+xlab("Day")+ggtitle("NEST Thermostat")+
                                           geom_point(data=weekmin, color="green", size=3)+ geom_point(data=weekmax, color="red", size=3)+
                                           theme_minimal())
        
        output$chart3 <- renderPlot(ggplot(weekmm, aes(dates,temp, fill=dates))+geom_bar(stat = "identity")+ggtitle("Weekly")+
                                        geom_text(aes(label=temp), vjust=1.6, color="white", size=10)+ggtitle("Weekly MIN / MAX temperature")+
                                        ylab("Temperature")+ xlab("MINIMUM AND MAXIMUM TEMPERATURE")+
                                        theme_minimal())
        
        output$chart4 <- renderPlot(ggplot(whum, aes(dates,humidity, fill=dates))+geom_bar(stat = "identity")+
                                        geom_text(aes(label=humidity), vjust=1.6, color="white", size=10)+ggtitle("Daily MIN / MAX humidity")+
                                        ylab("Humidity")+ xlab("MINIMUM AND MAXIMUM HUMIDITY")+
                                        theme_minimal())
        
        
        df1 <- nst() %>% filter(nst()$Date %in% subset(aggweeks(),weeknum==input$week)$days)
        output$table1 <- renderTable(df1)
        
    })
    
    #Monthly
    
    observeEvent(input$month, {
        
        month_selected <- input$month 
        
        if (month_selected==0) {
            ggplot()
        }
        
        else{
            nest1 <- nest()
            
            mnmm <- montc() %>% filter(montnum==month_selected) %>% filter(temp==max(temp) | temp==min(temp)) %>%  filter(row_number()==1 | row_number()==n())
            mnhum <- montc() %>% filter(montnum==month_selected) %>% filter(humidity==max(humidity) | humidity==min(humidity)) %>%  filter(row_number()==1 | row_number()==n())
            
            monmin <- mnmm %>% filter(temp==min(mnmm$temp)) %>% slice(0:1)
            
            monmax <- mnmm %>% filter(temp==max(mnmm$temp)) %>% slice(0:1)
            
            
            output$chart2 <- renderPlotly(ggplot(nest1, aes(x=dates, y=temp)) +
                                              geom_line(color="black") + ylab("Temperature")+xlab("Day")+ggtitle("NEST Thermostat")+
                                              geom_point(data=monmin, color="green", size=3)+ geom_point(data=monmax, color="red", size=3)+
                                              theme_minimal())
            
            
            output$chart3 <- renderPlot(ggplot(mnmm, aes(dates,temp, fill=dates))+geom_bar(stat = "identity")+
                                            geom_text(aes(label=temp), vjust=1.6, color="white", size=10)+ggtitle("Monthly MIN / MAX temperature")+
                                            ylab("Temperature")+ xlab("MINIMUM AND MAXIMUM TEMPERATURE")+
                                            theme_minimal())
            output$chart4 <- renderPlot(ggplot(mnhum, aes(dates,humidity, fill=dates))+geom_bar(stat = "identity")+
                                            geom_text(aes(label=humidity), vjust=1.6, color="white", size=10)+ggtitle("Monthly MIN / MAX humidity")+
                                            ylab("Humidity")+ xlab("MINIMUM AND MAXIMUM HUMIDITY")+
                                            theme_minimal())
        }           
    })
    
    
    #Toggle table data
    
    observeEvent(input$tog, {
        
        toggle("table1")
        
    })
    
   
}

shinyApp(ui, server)
