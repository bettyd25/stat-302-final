# Final Project

library(shiny)
library(bslib)
library(tidyverse)
library(sf)

# loading data---------------
load("data/shiny_kpop_hits_data.rds")
load("data/top_kpop_groups.rds")

artist_names <- unique(top_kpop_groups$artist.s)

artist_hex_codes <- tibble(
  artist_name = c(
    "TWICE", "BTS", "BIGBANG", "BoA", 
    "Girls' Generation", "Red Velvet", "EXO", "SHINee", 
    "BLACKPINK", "SHINHWA", "Wonder Girls", "NCT DREAM", 
    "PSY", "Park Hyo Shin", "SEVENTEEN", "SUPER JUNIOR", 
    "Sung Si Kyung", "TVXQ!", "Wanna One"
  ),
  hex_code = c(
    "#f9c596", "#b37eb5", "#FF503C", "yellow", 
    "#a300ff", "#e79a8e", "#fff8e7", "#88d8c0", 
    "#000000", "#ff8080", "#2e2f9a", "#eff9bb", 
    "#051032", "green", "#F7CAC9", "#0098f7", 
    "#4B4B4B", "#eb282f", "#37aed6"
  )
)

# app code =============

# Define UI for application that draws a histogram
ui <- page_sidebar(

    # Application title
    title = "Top Hits in Kpop by Artist",
    
    # left side bar
    sidebar = sidebar(
      position = "left",
      
      # Input: selecting an artist
      selectInput(
        "artist_choice",
        label = strong("Select Kpop Artist of Interest"),
        choices = artist_names
      )
    ),
    card(
      plotOutput("polarPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$polarPlot <- renderPlot({
    artist <- input$artist_choice
    artist_color <- artist_hex_codes |> filter(
      artist_name == input$artist_choice
    ) |> 
      pull(hex_code)
    
    shiny_kpop_hits_data |> 
      as.data.frame() |> 
      filter(artist.s == artist, variable != "avg_year") |>
      ggplot(mapping = aes(x = variable, 
                           y = value)) +
      geom_col(width = 1, alpha = 0.5,
               fill = artist_color) +
      geom_hline(yintercept = seq(1, 10, 1), linetype = "dotted") +
      geom_segment(
        x = 0.5:6.5,  # match numb of variables
        y = 0, 
        xend = 0.5:6.5, 
        yend = 10
      ) +
      labs(
        title =  labs(title = paste("Average Hits for", artist, "in Deciles"))
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 15, face = "bold",
                             hjust = 0.5)
      ) +
      annotate(
        geom = "text",
        x = 1:7,
        y = 10,
        label =  c("Danceability", 
                   "Energy",
                   "Loudness",
                   "Speechiness", 
                   "Valence",
                   "Tempo",
                   "Duration"),
        
        size = 5
      ) +
      coord_polar()
  })
  
  
  
}

# Run the application ------------
shinyApp(ui = ui, server = server)
