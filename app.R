
library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(shinythemes)



sp500 <- read.csv("constituents_csv.csv")  # dataset for SP500 Companies




ui <- navbarPage("S&P500 Stonks App", theme = shinytheme("flatly"),
                 
                 tabPanel("Welcome",
                          
                          titlePanel("README"),
                          
                          mainPanel(
                              p("The S&P500 Stonks App returns a time series graph of the closing price, daily summary statistics, 
               a brief company overview, and recent news related to your choice of a S&P500 company."),
                              
                              
                              p("Please note that this app will only return information about the", 
                                strong("most recent complete market day."), 
                                "Thus, if, for example, today is Sunday March 21, the app should return market information for
                  Friday March 19 (assuming that the market is open on Friday March 19). "),
                              
                              tags$br(),
                              
                              strong("Limitations"),
                              
                              h5("Due to the free nature of the APIs used, there are restrictions on the usage of this app:"),
                              
                              tags$ul(
                                  tags$li("No more than 2 requests per minute"), 
                                  tags$li("No more than 100 requests per day ")
                              ),
                              
                              p("If you do not follow these limitations, the proper results may not be shown and the app will return
                 the statement 'No [feature of interest] is available.'")
                              
                          )
                          
                          
                 ),
                 
                 # Application title
                 tabPanel("App",
                          
                          titlePanel("S&P500 Stonks App"),
                          
                          h5("Please do not make more than 2 requests per minute"),
                          
                          # User chooses sector
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput(inputId = "sector" , label = "S&P500 Sector" , 
                                              choices = c("--", unique(sp500$Sector))), 
                                  
                                  # reactive, changes depending on sector
                                  selectInput(inputId = "stock" , label = "Company Stock Pick" , 
                                              choices = NULL), 
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                  plotOutput(outputId = "stock_price"), # line graph
                                  
                                  htmlOutput("stats_label"),
                                  
                                  tableOutput("company_stats"),
                                  
                                  tags$br(),
                                  
                                  htmlOutput("descrip_label"),
                                  
                                  textOutput("description"),   # company description
                                  
                                  tags$br(), 
                                  
                                  htmlOutput("news_label"),
                                  
                                  dataTableOutput("news")
                                  
                              )
                          )
                 )
)


server <- function(input, output, session) {
    
    
    ###### CONDITIONAL LABELS #######
    
    output$stats_label<- renderUI({
        req(input$stock != "--")
        HTML(paste("<b>", "Daily Summary Statistics", "</b>"))
        
    })
    
    output$descrip_label<- renderUI({
        req(input$stock != "--")
        HTML(paste("<b>", "Company Overview", "</b>"))
        
        
    })
    
    output$news_label<- renderUI({
        req(input$stock != "--")
        HTML(paste("<b>", "Recent News", "</b>"))
        
    })
    
    ##################################
    
    sector_df <- reactive({
        sp500 %>% filter(Sector == input$sector) 
    })
    
    # getting stock symbol 
    stock_ticker <- reactive({
        sector_df() %>%
            filter(Name == input$stock) %>% 
            select(Symbol)
    })
    
    
    observeEvent(sector_df(), {
        ### get stock picks 
        stock_choices <- sort(sector_df()$Name)
        
        ### update stock picks 
        updateSelectInput(input = "stock", choices = c("--", stock_choices))
        
    })
    
    
    # return line graph of stock that user chose
    output$stock_price <- renderPlot({
        
        # user must choose a company or no graph 
        req(input$stock != "--")
        
        
        ######## Alpha Vantage API (Stock Price Graph) ##############
        r <- GET("https://www.alphavantage.co/query",
                 query = list(
                     apikey = Sys.getenv("VANTAGE_KEY"),
                     `function` = "TIME_SERIES_INTRADAY", 
                     symbol = stock_ticker(),   # user choice of company (ticker)
                     interval = "5min", 
                     outputsize = "full"
                 )
        )
        stop_for_status(r)
        json <- content(r, as = "text", encoding = "UTF-8")
        
        a <- fromJSON(json, flatten = TRUE)$`Time Series (5min)`
        
        shiny::validate(
            need(is.null(a) == FALSE, "No chart available, please select another Company")
        )
        ##################################################################
        
        df <- data.frame(t(sapply(a, c))) %>% rownames_to_column(var = "Time") # convert nested list into df 
        
        c_date <- as_date(df[1,1])  # get date of the last closing value
        
        df$`X4..close` <- as.numeric(sapply(df$`X4..close`, unlist)) #unlisting column
        
        df$Time <- as_datetime(df$Time)  # convert Time column into a date
        
        df1 <- df %>% select(Time, `X4..close`) %>%  filter(as.Date(Time, tz = "UTC") == c_date)
        
        # check for chart not available
        
        
        # plotting the graph
        ggplot(df1, aes(x = Time, y = `X4..close`, group = 1)) +
            geom_line(color = "navy") +
            labs(title = paste0(input$stock, " Stock Price for ", c_date), y = "Closing Price ($)", x = "Time")+
            scale_x_datetime(date_breaks = "1 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +
            scale_y_continuous(breaks = pretty(df1$`X4..close`, n = 10))
        
    })
    
    ########### Alpha Vantage API for Company Overview #########
    
    company_info <- reactive({
        r <- GET(
            "https://www.alphavantage.co/query",
            query = list(
                apikey = Sys.getenv("VANTAGE_KEY2"),
                `function` = "OVERVIEW",
                symbol = stock_ticker()
            )
        )
        stop_for_status(r)
        json <- content(r, as = "text", encoding = "UTF-8")
        print(json)
        a<-fromJSON(json, flatten = TRUE)
        stats <- data.frame(t(sapply(a, c)))
        
    })
    
    
    
    ##########################################################
    
    
    output$company_stats <- renderTable({
        
        req(input$stock != "--")
        
        shiny::validate(
            need(try(length(company_info()$Name)) != 0, "No summary statistics available")
        )
        
        x <- company_info() %>% 
            select(Name, Symbol, MarketCapitalization, PERatio, DividendYield, EPS, X52WeekHigh, X52WeekLow) %>% 
            rename("Mkt Cap" = MarketCapitalization, 
                   "P/E Ratio" = PERatio, 
                   "Div/Yield" = DividendYield, 
                   "52 Wk High" = X52WeekHigh,
                   "52 Wk Low" = X52WeekLow)
        
        
        x
    })
    
    output$description <- renderText({
        req(input$stock != "--")
        
        shiny::validate(need(try(length(company_info()$Description)) != 0, "No description available"))
        
        y <- company_info() %>% select(Description) %>% as.character()
        
        y
    })
    
    
    
    output$news <- renderDataTable(escape = FALSE, {
        
        #require a stock to be chosen
        req(input$stock != "--")
        
        ######### GNEWS API ########
        
        r <- GET("https://gnews.io/api/v4/search",
                 query = list(
                     token = Sys.getenv("GNEWS_TOKEN2"),
                     q = stock_ticker(),   #user chosen stock
                     from = Sys.Date()-5,  # change to last 5 days
                     lang = "en",
                     sortby = "relevance"
                     
                 )
        )
        
        # stop_for_status(r)
        json <- content(r, as = "text", encoding = "UTF-8")
        x <- fromJSON(json, flatten = TRUE)$articles
        
        shiny::validate(
            need(length(x) != 0, "No recent news available, please select another Company")
        )
        
        
        c <- data.frame(lapply(x, unlist)) %>% select(title, url)
        
        
        ###############################
        
        if(is.null(c$title) == FALSE){
            
            c$url <- sapply(c$url, function(url){
                toString(tags$a(href = url, url))
            })
            
            return(c %>% rename("Article Title" = title, "Link" = url) %>% unique())
            
        } 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
