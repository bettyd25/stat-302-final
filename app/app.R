# Final Project

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(gt)

# loading data---------------
load("data/merged_kpop_hits_all_years.rds") 

load("data/shiny_kpop_hits_data.rds")
load("data/top_kpop_groups.rds")

kpop_idols <- read_csv("data/kpopidolsv3.csv")

top_kpop_idols <- kpop_idols |> filter(
  tolower(Group) %in% tolower(top_kpop_groups$artist.s) | 
    tolower(`Former Group`) %in% tolower(top_kpop_groups$artist.s) | 
    tolower(`Other Group`) %in% tolower(top_kpop_groups$artist.s)
) |> 
  mutate(Group = tolower(Group),
         `Former Group` = tolower(`Former Group`),
         `Other Group` = tolower(`Other Group`))

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
      ),
      br(),
      tableOutput("songTable"),
      
      br(),
      
      tableOutput("memberTable"),
      card_body("Note that some groups may not have members listed due to their current status."),
      br(),
      plotOutput("agePlot")
    ),
    

      card(
        imageOutput("groupImage"),
        card_body("The color is the color or fan color associated with the kpop artist!")
        ),
    
    layout_column_wrap(
      width = 0.5,
      
      card(
        plotOutput("polarPlot")
        
      ),
      
      card(
        selectInput(
          "song_aspect",
          label = "Select aspect of songs",
          
          choices = list(
            "Danceability",
            "Loudness",
            "Energy",
            "Acousticness",
            "Mode",
            "Speechiness",
            "Tempo",
            "Duration (ms)"
          )
           
        ),
        
        plotOutput("songPlot")
        
        
          )
        )
      )

# Define server logic required to draw a histogram
server <- function(input, output) {

  # polar plot :))
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
      geom_col(width = 1,
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
        title = element_text(face = "bold",
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
        
        size = 4
      ) +
      coord_polar()
  })
  
 # make member table 
  output$memberTable <- render_gt({
    artist <- input$artist_choice
    selected_group <- tolower(input$artist_choice)
    
    member <- top_kpop_idols |> 
      filter(
        selected_group == Group |
          selected_group == `Former Group` |
          selected_group == `Other Group`
      ) |> 
      select(
        `Stage Name`, `Full Name`
      )
    
    # Create a table using gt
    member |> 
      gt() |> 
      tab_header(
        title = paste("Members of", artist),
        subtitle = "Stage Name and Original Name"
      ) |> 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = everything())
      ) |> 
      tab_style(
        style = cell_text(align = "center", weight = "bold"),
        locations = cells_column_labels(columns = everything())
      )
  })
  
  # make image
  output$groupImage <- renderImage({
    artist <- input$artist_choice
    
    file_path <- paste0("www/", artist, ".jpg")
  
    if (!file.exists(file_path)) {
      file_path <- "www/default.jpg"  # Optional: Fallback image if the specific artist image is missing
    }
    
    list(
      src = file_path,
      alt = paste("Image of", artist),
      width = "100%"  # Adjust as needed
    )
  
  }, deleteFile = FALSE)
  
  # make plots of member ages
  output$agePlot <- renderPlot({
    selected_group <- tolower(input$artist_choice)
    
    artist_color <- artist_hex_codes |> filter(
      artist_name == input$artist_choice
    ) |> 
      pull(hex_code)
    
    
    member <- top_kpop_idols |> 
      filter(
        selected_group == Group |
          selected_group == `Former Group` |
          selected_group == `Other Group`
      ) |> 
      mutate(age = as.numeric(difftime(today(), dmy(`Date of Birth`), units = "days")) / 365.25) |> 
      mutate(age = round(age, 0))
    
    if (nrow(member) == 0) {
      return(
        ggplot() +
          geom_blank() +
          theme_void() +
          labs(title = "No age boxplot available")
      )
    }
    
    ggplot(member, aes(x = age)) +
      geom_boxplot(fill = artist_color) +
      scale_x_continuous(
        name = NULL,
        breaks = seq(0, ceiling(max(member$age)), by = 1),
        labels = function(x) paste0(x, " years")  
      ) +
      labs(
        title = "Boxplot of Member Ages"
      ) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.y = element_text(angle = 0, hjust = 0.5), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank()
      )
  })
  
  # make table of top songs
  output$songTable <- render_gt({
    artist <- input$artist_choice
    
    artist_color <- artist_hex_codes |> filter(
      artist_name == input$artist_choice
    ) |> 
      pull(hex_code)
    
    merged_kpop_hits_all_years |> 
      filter(artist.s == artist) |> 
      select(title, year) |> 
      gt() |> 
      tab_header(
        title = paste("Top Songs of", artist),
        subtitle = "Top Songs in Years"
      ) |> 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = everything())
      ) |> 
      tab_style(
        style = cell_text(align = "center", weight = "bold"),
        locations = cells_column_labels(columns = everything())
      ) |> 
      tab_options(
        table.background.color = artist_color
      )
  }) 
  
  # make toggle-able year boxplots
  
  output$songPlot <- renderPlot({
    artist <- input$artist_choice
    
    artist_color <- artist_hex_codes |> filter(
      artist_name == input$artist_choice
    ) |> 
      pull(hex_code)
    
    top_songs <- merged_kpop_hits_all_years |> 
      filter(artist.s == artist)
    
    song_asp <- switch(
      input$song_aspect,
      "Danceability" = top_songs$danceability,
      "Loudness" = top_songs$loudness,
      "Energy" = top_songs$energy,
      "Acousticness" = top_songs$acousticness,
      "Mode" = top_songs$mode,
      "Speechiness" = top_songs$speechiness,
      "Tempo" = top_songs$tempo,
      "Duration (ms)" = top_songs$duration_ms
    )
    
    top_songs |> 
      ggplot(aes(year, song_asp)) +
      geom_point() +
      geom_smooth(se = FALSE, color = artist_color) + 
      scale_x_continuous(labels = function(x) paste0(x)) +  
      labs(y = NULL,
           x = NULL,
           title = paste0(input$song_aspect, " through the years")) + 
      theme_minimal()
    
  })
  
  
  
}

# Run the application ------------
shinyApp(ui = ui, server = server)
