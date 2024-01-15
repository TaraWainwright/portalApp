# Data portal queries

# Import data and load packages
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(wordcloud)
library(paletteer)

df <- read_csv("final_data.csv")
qs <- read_csv("search_data.csv")
str(df)

# min/max date
mind <- min(df$floored_date)
maxd <- max(df$floored_date)


# Define UI for application
ui <- fluidPage(

  # Change style - slider 
  tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #188301;
                                                  border-top: 1px solid #000039 ;
                                                  border-bottom: 1px solid #000039 ;}
                           .irs-from, .irs-to, .irs-single { background: #188301 !important }')),
  
    # Application title
    titlePanel("NHM Data Portal Queries"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("resource", 
                        label = "Select Resource",
                        choices = unique(df$resource_name), 
                        selected = "Specimens"),
            br(),
            br(),
            selectInput("var",
                        label = "Select Variable",
                        choice = c("Number of DOIs" = "doi_count",
                                   "Number of unique users" = "user_count",
                                   "Number of downloads" = "download",
                                   "Common queries" = "searches"),
                        selected = "doi_count"),
            br(),
            br(),
            sliderInput("range", 
                        label = "Select Dates",
                        min = mind,
                        max = maxd,
                        value = c(min(df$floored_date), max(df$floored_date)),
                        timeFormat = "%b %Y")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("resources_plot")
        )
    )
)


# Define server logic
server <- function(input, output){

  data_source <- reactive({
    if(input$var == "searches"){
      input$var
      input$resource
      qs %>% 
        select(floored_date, resource_name, n, input$var) %>% 
        filter(resource_name == input$resource,
               floored_date >= input$range[1],
               floored_date <= input$range[2]) %>% 
        group_by(searches) %>% 
        summarise(total = sum(n))
    }
    
    else{
    input$var
    input$resource
    df %>% 
      select(floored_date, resource_name, input$var) %>% 
      filter(resource_name == input$resource)
    }
  })

  
  output$resources_plot <- renderPlot({
    
    if(input$var == "searches"){
      
      wordcloud(words = data_source()$searches, freq = data_source()$total, 
                min.freq = 1, max.words = 50, 
                random.order = FALSE, 
                scale=c(4, .75), rot.per = 0.25,
                random.color = FALSE,
                colors = c("maroon", "dark green", "dark blue", "dark orange"))
      }
    
    else{
    
    y_label_df <- tibble(
      variables = c('doi_count', 'user_count', 'download'),
      labels = c('Number of DOIs', 'Number of unique users', 'Number of downloads')
    )
    
    y_lab_name <- y_label_df %>% 
      filter(variables == input$var) %>% 
      select(labels) %>% 
      pull() 
    
    ggplot(data_source(), aes(x = floored_date, y = .data[[input$var]]))+
      geom_point() +
      geom_line(color = "#188301", size = 1, alpha=0.75)+
      xlim(input$range[1],input$range[2])+
      xlab("Date")+
      ylab(y_lab_name)+
      theme_light()
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
