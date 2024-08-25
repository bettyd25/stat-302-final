

# loading packages --------------------------------------------------------

library(tidyverse)

# Merging Song Data -------------------------------------------------------
### song data successfully merged in "data/merged_kpop_hits_all_years.rds"


### obtain files that merges all kpop_hits
files <- list.files("data/raw/", pattern = "\\.csv$", full.names = TRUE)
exclude_patterns <- c("data/raw/kpopidolsv3.csv")
files <- files[!grepl(paste(exclude_patterns, collapse = "|"), files)]


merged_data <- list()
# Combine data
for (file in files) {
  year <- as.numeric(gsub("\\D", "", file))
  ## tryCatch to identify errors
  tryCatch({
    data <- read.csv(file)
    data$year <- year
    merged_data[[length(merged_data) + 1]] <- data
  })
}
if (length(merged_data) > 0) {
  final_merged_data <- do.call(rbind, merged_data)
  saveRDS(final_merged_data, "data/merged_kpop_hits_all_years.rds")
} else {
  cat("No data to merge.\n")
}

## looking at the merged rds
view(readRDS("data/merged_kpop_hits_all_years.rds"))


# Codebook Creation -------------------------------------------------------

## creating a tibble codebook of top songs
top_songs_codebook <- data.frame(
  variable = c(
    "X",
    "title",
    "artist.s",
    "danceability",
    "energy",
    "key",
    "loudness",
    "mode",
    "speechiness",
    "acousticness",
    "valence",
    "tempo",
    "duration_ms",
    "time_signature",
    "year"
  ),
  description = c(
    "Rank of the popular song; popularity counting up",
    "Title of the song",
    "The artist who sang or covered or made the instrumental of the song",
    "How suitable a track is for dancing based on a combination of musical elements",
    "Perceptual measure of intensity and activity based on its dynamic range, perceived loudness, timbre, onset",
    "Key the track is in. Integers map to pitches using standard Pitch Class notation.",
    "Overall average loudness measured in decibels",
    "Type of scale. Major is represented by 1 and minor is 0",
    "Detects the presence of spoken words in a track. Higher percentage inficates more exclusively speech-like qualities",
    "Confidence measure of whether the track is acoustic. Values range from 0.0 to 1.0. 1.0 represents high.",
    "Musical positiveness conveyed by a track.",
    "Overall estimated tempo of a track in beats per minute.",
    "Duration of the track in milliseconds.",
    "Estimated overall time signature of a track. The time signature is a notational convention to specify how many beats",
    "Year that the song became popular"
  )
)

write_csv(top_songs_codebook, "data/merged_top_songs_codebook.csv")


kpop_idol_codebook <- data.frame(
  variable = c(
    "`Stage Name`",
    "`Full Name`",
    "`Korean Name`",
    "`K Stage Name`",
    "`Date of Birth`",
    "Group",
    "Debut",
    "Company",
    "Country",
    "`Second Country`",
    "Height",
    "Weight",
    "Birthplace",
    "`Other Group`",
    "`Former Group`",
    "Gender",
    "age",
    "debut_age"
  ),
  description = c(
    "Stage Name of the individual",
    "Full Name of the individual",
    "Korean Name of the individual",
    "K Stage Name of the individual",
    "Date of Birth of the individual",
    "Group the individual belongs to (null for soloists)",
    "Debut date of the individual",
    "Company that the individual is managed by (if applicable)",
    "Country of origin",
    "Second Country (if applicable)",
    "Height of the individual (in cm)",
    "Weight of the individual (in kg)",
    "Place of birth",
    "Other Group (if applicable)",
    "Former Group (if applicable)",
    "Gender of the individual",
    "Current Age of the Idol as of 2024",
    "Debut Age of the Kpop Idol"
  )
)

write_csv(kpop_idol_codebook, "data/idol_data_codebook.csv")



# Cleaning Idol Data ------------------------------------------------------

kpop_idols <- read_csv("data/raw/kpopidolsv3.csv")

idol_data <- kpop_idols |> 
  filter(!is.na(Debut)) |> 
  mutate(`Date of Birth` = ifelse(`Date of Birth` == "0/01/1900", NA, `Date of Birth`),
         Debut = ifelse(Debut == "0/01/1900", NA, Debut)) |> 
  mutate(`Date of Birth` = dmy(`Date of Birth`),
         Debut = dmy(Debut),
         age = as.numeric(interval(`Date of Birth`, today()) %/% years(1)),
         debut_age = as.numeric(interval(`Date of Birth`, Debut) %/% years(1)))

save(idol_data, file = "data/idol_data.rds")

