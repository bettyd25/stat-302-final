
# loading packages
library(tidyverse)

load("data/merged_kpop_hits_all_years.rds")

# editing data for shiny app (part 1 graph) -------------------

# this section contains the relevant code to create the polar coordinate 
# graph of each kpop group

decile_rank <- function(x = 0:99) {
  # Set decile
  dec_breaks <- c(
    -Inf,
    quantile(x,
             probs = c(.1, .2, .3, .4, .5,
                       .6, .7, .8, .9),
             na.rm = TRUE
    ),
    Inf
  )
  cut(x = x, breaks = quart_breaks, labels = FALSE)
}

shiny_kpop_hits_data <- merged_kpop_hits_all_years |> 
  select(-X, title) |> 
  group_by(artist.s) |> 
  summarize(
    avg_danceability = mean(danceability),
    avg_energy = mean(energy),
    avg_key = mean(key),
    avg_loudness = mean(key),
    avg_mode = median(mode),
    avg_speechiness = mean(speechiness),
    avg_valence = mean(valence),
    avg_tempo = mean(tempo),
    avg_duration = mean(duration_ms)/6000,
    avg_year = median(year)
  ) |>
  mutate(
    dec_avg_danceability = decile_rank(avg_danceability),
    dec_avg_energy = decile_rank(avg_energy),
    dec_avg_loudness = decile_rank(avg_loudness),
    dec_avg_speechiness = decile_rank(avg_speechiness),
    dec_avg_valence = decile_rank(avg_valence),
    dec_avg_tempo = decile_rank(avg_tempo),
    dec_avg_duration = decile_rank(avg_duration)
  ) |> 
  select(artist.s, contains("dec_avg_")) |> 
  pivot_longer(
    cols = -artist.s,
    names_to = "variable",
    values_to = "value"
  )

save(shiny_kpop_hits_data, file = "app/data/shiny_kpop_hits_data.rds")


## obtaining a selection of idols to work with
top_kpop_groups <- merged_kpop_hits_all_years |> 
  filter(artist.s != "코케",
         artist.s != "Various Artists", 
         artist.s != "Groove Edition") |> 
  group_by(artist.s) |> 
  summarize(Count = n()) |> 
  arrange(desc(Count)) |> 
  slice_max(order_by = Count, n = 15)

save(top_kpop_groups, file = "app/data/top_kpop_groups.rds")


## artist colors
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

# editing data for shiny app (part 2) -------------------------------------
kpop_idols <- read_csv("data/raw/kpopidolsv3.csv")

top_kpop_idols <- kpop_idols |> filter(
  Group %in% top_kpop_groups$artist.s | `Former Group` %in% top_kpop_groups$artist.s | `Other Group` %in% top_kpop_groups$artist.s
)


# code testing ------------------------------------------------------------

artist <- "2PM"
artist_color <- "skyblue"

shiny_kpop_hits_data |> 
  as.data.frame() |> 
  filter(artist.s == artist,
         variable != "avg_year") |>
  ggplot(mapping = aes(x = variable, 
                       y = value)) +
  geom_col(width = 1, alpha = 0.5,
           fill = artist_color) +
  geom_hline(yintercept = seq(1, 10, 1), linetype = "dotted") +
  geom_segment(
    x = 0.5:6.5,  # Adjust to match the number of variables (7)
    y = 0, 
    xend = 0.5:6.5, 
    yend = 10
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
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






  
