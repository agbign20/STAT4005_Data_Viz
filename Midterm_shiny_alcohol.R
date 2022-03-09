library(tidyverse)
library(shiny)
library(ggrepel)
library(here)
alcohol_df <- read_csv(here("data/alcohol.csv"))

onecountry_df <- alcohol_df %>% filter(country == "Australia")

ggplot(alcohol_df, aes(x = beer_servings, y = wine_servings)) +
  geom_point() +
  geom_label_repel(data = onecountry_df, aes(label = country)) +
  geom_point(data = onecountry_df, size = 3, shape = 1)


var_choices <- names(alcohol_df)[c(2:4)]
ui <- fluidPage(
  sidebarLayout(sidebarPanel(
    selectInput("countrychoice",
                label = "Choose a Country", 
                choices = levels(factor(alcohol_df$country))),
    radioButtons("varchoice", label = "Choose a Statistic",
                 choices = var_choices),
    sliderInput("binnumber", label = "Choose a Number of Bins", 
                min = 1, max = 50, value = 15, step = 1)),
    mainPanel(plotOutput("scatgraph"),
              plotOutput("histgraph"))
  )
)




server <- function(input, output, session) {
  df_sub <- reactive({
    onecountry_df <- alcohol_df %>% filter(country == input$countrychoice)
  })
  
  scat_plot <- reactive({
  
    base_plot <- ggplot(alcohol_df, 
                        aes(x = beer_servings, 
                            y= wine_servings))
    base_plot +
      geom_point() +
      geom_label_repel(data = onecountry_df, aes(label = input$countrychoice)) +
      geom_point(data = onecountry_df, size = 3, shape = 1)
  })
  
  hist_plot <- reactive({
      # ggplot(df_sub(), aes_string(x = input$varchoice)) +
      # geom_histogram(colour = "black", fill = "white", bins = 15)
      base_plot <- ggplot(df_sub(), 
                          aes(x = input$countrychoice, 
                              y= .data[[input$varchoice]]))
      base_plot + geom_col(colour = "black", fill = "white",
                           bins = input$binnumber) +
        theme_minimal(base_size = 22)
      
  })
  
  output$scatgraph <- renderPlot({
    scat_plot()
  })
  
  
  output$histgraph <- renderPlot({
    hist_plot()
  })
  
  
}

shinyApp(ui, server)
