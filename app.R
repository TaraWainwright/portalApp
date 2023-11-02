# Data portal queries

# Import data and load packages
library(ggplot2)
library(shiny)
library(tidyverse)
library(zoo)

df <- read_csv("final_data.csv")
str(df)

# min/max date
mind <- min(df$floored_date)
maxd <- max(df$floored_date)


# Define UI for application
ui <- fluidPage(

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
                                   "Number of downloads" = "download"),
                                   #"Common queries" = "searches"),
                        selected = "doi_count"),
            br(),
            br(),
            sliderInput("range", 
                        label = "Select Dates",
                        min = mind,
                        max = max(df$floored_date),
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
server <- function(input, output) {
  
  finalInput <- reactive({
    input$var
    input$resource
    df %>% 
      select(floored_date, resource_name, input$var) %>% 
      filter(resource_name == input$resource)
  })
  
  output$resources_plot <- renderPlot({
    
    y_label_df <- tibble(
      variables = c('doi_count', 'user_count', 'download'),
      labels = c('Number of DOIs', 'Number of unique users', 'Number of downloads')
    )
    
    y_lab_name <- y_label_df %>% 
      filter(variables == input$var) %>% 
      select(labels) %>% 
      pull() 
    
    ggplot(finalInput(), aes(x = floored_date, y = .data[[input$var]]))+
      geom_point() +
      geom_line()+
      xlim(input$range[1],input$range[2])+
      xlab("Date")+
      ylab(y_lab_name)+
      theme_light()
  })
}
# add wordcloud into this? or has to be separate?

# Run the application 
shinyApp(ui = ui, server = server)
