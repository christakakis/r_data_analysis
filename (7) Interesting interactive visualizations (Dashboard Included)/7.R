# Import libraries
library(splitstackshape)
library(tidyverse)
library(tidygraph)
library(cshapes)
library(ggrepel)
library(stringr)
library(leaflet)
library(ggplot2)
library(stringr)
library(ggraph)
library(readr)
library(tmap)
library(sf)
library(networkD3)

# Define path to the .csv file
path <- "C:/Users/panos/Desktop/album.csv"
albums <- read_csv(path)

#[Pre-processing]
# Checking for NA values. Fortunately we have none.
sum(is.na(albums))
colSums(is.na(albums))

# Check for NAs anyways.
albums <- albums %>% drop_na()

# Split the album column into year and title. Also convert year from string to numeric.
albums <- tidyr::separate(albums, album, into = c("year", "title"), sep = "[)]")
albums$year <- str_replace(albums$year, "[(]", "")
albums$year <- as.numeric(albums$year)

# Keep an un-edited albums df copy for later use
albums_raw <- albums

# Check the amount of unique artists.
length(unique(albums$artist))

# Split album genres into two new columns with only the first two.
# If an album has only one genre then, copy that to the second column. 
albums <- tidyr::separate(albums, genre, 
                           into = c("primary_genre", "secondary_genre"), ",")
albums$secondary_genre <- ifelse(is.na(albums$secondary_genre), 
                                 albums$primary_genre, albums$secondary_genre)


# [1]
# (1a)
# New df with albums counter for the first genre.
primary_genre_album <- count(albums, primary_genre)
colnames(primary_genre_album) <- c('Genre','Albums')

# Plot
ggplot(primary_genre_album, aes(Albums, reorder(Genre, Albums))) + 
  geom_bar(stat = "identity", fill = "#1cbaeb", width = 0.5) +
  geom_text(aes(label = Albums), color = "black", 
                                  position = position_dodge(width = 2), hjust = -0.05) +
  labs(x = "Total Albums", y = "Genre") +
  ggtitle("BarPlot For Total Albums per Primary Genre",
          subtitle = "Assumption: Each album belongs only to the primary genre.") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))  + 
  theme_minimal() 


# (1b)
# New df with albums counter for every genre.
genres_album <- count(albums_raw, genre)
colnames(genres_album) <- c('Genres','Albums')
# For simpler plotting reasons, we keep only a subgroup of the df.
genres_album_plot <- filter(genres_album, Albums > 25)

# Plot
ggplot(genres_album_plot, aes(Albums, reorder(Genres, Albums))) + 
  geom_bar(stat = "identity", fill = "#1cbaeb", width = 0.5) +
  geom_text(aes(label = Albums), color = "black", 
            position = position_dodge(width = 2), hjust = -0.05) +
  labs(x = "Total Albums", y = "Genres") +
  ggtitle("BarPlot For Total Albums per Genres",
          subtitle = "Assumption: Each album belongs to all its genres.\nShowing only albums that their genre counter is over 25 for simpler plot.") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))  + 
  theme_minimal() 


# (1c)
# New dfs with artists counter for the first genre.
primary_genre_artist <- count(albums, primary_genre, artist)
primary_genre_artist_next <- count(primary_genre_artist, primary_genre)
colnames(primary_genre_artist_next) <- c('Genre', 'Artists')

# Plot
ggplot(primary_genre_artist_next, aes(Artists, reorder(Genre, Artists))) + 
  geom_bar(stat = "identity", fill = "#1cbaeb", width = 0.5) +
  geom_text(aes(label = Artists), color = "black", 
            position = position_dodge(width = 2), hjust = -0.05) +
  labs(x = "Total Artists", y = "Genre") +
  ggtitle("BarPlot For Total Artists per Primary Genre",
          subtitle = "Assumption: Each artist belongs only to the primary genre.") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))  + 
  theme_minimal() 


# (1d)
# New dfs with artists counter for every genre.
genres_artist <- count(albums_raw, genre, artist)
genres_artist_next <- count(genres_artist, genre)
colnames(genres_artist_next) <- c('Genres','Artists')
# For simpler plotting reasons, we keep only a subgroup of the df.
genres_artist_plot <- filter(genres_artist_next, Artists > 5)

# Plot
ggplot(genres_artist_plot, aes(Artists, reorder(Genres, Artists))) + 
  geom_bar(stat = "identity", fill = "#1cbaeb", width = 0.5) +
  geom_text(aes(label = Artists), color = "black", 
            position = position_dodge(width = 2), hjust = -0.05) +
  labs(x = "Total Artists", y = "Genres") +
  ggtitle("BarPlot For Total Artists per Genres",
          subtitle = "Assumption: Each artist belongs to all its genres.\nShowing only artists that their genre counter is over 5 for simpler plot.") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))  + 
  theme_minimal() 


# [2]
# New df with albums per year.
albums_per_year <- count(albums, year)
colnames(albums_per_year) <- c('year','albums')

# Plot
ggplot(albums_per_year, aes(year, albums)) + 
  geom_bar(stat = "identity", fill = "#1cbaeb", width = 1.2) +
  #geom_text(aes(label = albums), color = "black", 
            #position = position_jitter(width = 2), hjust = 0.6) +
  labs(x = "Year", y = "Albumbs") +
  ggtitle("Distribution For Albums per Year",
          subtitle = "From 1938 to 2020.") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68")) +
  scale_x_continuous(breaks=seq(1938,2022,10)) + 
  theme_minimal() 


# [3]
# Arrange in descending order the df with albums for every genre.
# Print the top five genres and then filter them into a new df.
primary_genre_album <- arrange(primary_genre_album, desc(Albums))
primary_genre_album[1:5,]

# Filtering step
top_5_genres <- filter(albums, primary_genre == "Pop-Rock" | primary_genre == "Blues" |
                         primary_genre == "Jazz" | primary_genre == "R&B" |
                         primary_genre == "International")

# Plot
ggplot(top_5_genres, aes(x = year)) +
  geom_histogram(aes(fill = interaction(primary_genre)), bins = 40) +
  ggtitle("Genre Distribution Of All Albums",
          subtitle = "For Top-5 genres.\nFrom 1938 to 2020.")+
  labs(y = "Albums", x = "Years") +
  guides(fill=guide_legend(title="Genre")) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68")) +
  scale_x_continuous(breaks=seq(1938,2022,10))


# [4]
# Creating separate artist df
# Split genres by comma and keep only the first two.
artists <- tidyr::separate(albums_raw, genre, 
                              into = c("primary_genre", "secondary_genre"), ",")

# If an artist only has one genre, add it to the second column
# Using ifelse
artists$secondary_genre <- ifelse(is.na(artists$secondary_genre), 
                                  artists$primary_genre, artists$secondary_genre)
# Using function coalesce
#artists$secondary_genre <- artists %>% 
#                                  mutate(artists$secondary_genre <- coalesce(
#                                    artists$secondary_genre,artists$primary_genre))

# Keep only unique artist and genres combinations. A total of: 3692
artists <- dplyr::select(artists, -year, -title)
artists <- distinct(artists, artists$artist, .keep_all = TRUE)
artists <- dplyr::select(artists, primary_genre, secondary_genre, artist)

# Create a table of the columns
two_way_table <- table(artists$primary_genre, artists$secondary_genre)

# Plot
mosaicplot(two_way_table, main = "Genres Mosaic Plot",
           sub = "Relationships of all genre pairs",
           xlab = "Primary Genre",
           ylab = "Secondary Genre",
           las = 4,
           color = "skyblue2",
           border = "chocolate"
          )


# [5]
# New df for alterations
albums_for_map <- albums

# Obtain world data
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  data(world)
  # or
  world <- st_read(system.file("shapes/world.gpkg", package="spData"))
}

# Extract country names from album titles.
albums_for_map$contains = str_extract(
  albums_for_map$title, 
  pattern = regex(paste(world$name_long, collapse = "|"), ignore_case = FALSE)
)

# Drop NAs and rename columns.
albums_for_map <- albums_for_map %>% drop_na()
colnames(albums_for_map) <- 
  c('primary_genre','secondary_genre','artist','year','title','name_long')

# Merge the two dfs by country name and count unique reference for each country.
final_map <- merge(world,albums_for_map,by="name_long")
final_map <- count(final_map, name_long, geometry)
colnames(final_map) <- c('country','references','geometry')

# Make tmap interactive
tmap_mode("view")

# Plot
tm_shape(final_map) + 
  tm_polygons(col = "references", palette = "seq", border.col = "#9c350c") + 
  tm_text("references", col = "#7568ed")


# [6]
# (6a)
# Find the links between the top 5 primary genres.
graph_top_5 <- count(albums, primary_genre, secondary_genre)
graph_top_5 <- filter(graph_top_5, primary_genre == "Pop-Rock" | primary_genre == "Blues" |
                         primary_genre == "Jazz" | primary_genre == "R&B" |
                         primary_genre == "International")

# Plot the graph.
simpleNetwork(graph_top_5, height="300px", width="300px",        
                   Source = 1,        
                   Target = 2,       
                   linkDistance = 100,
                   charge = -800,
                   fontSize = 14,
                   fontFamily = "serif",
                   linkColour = "#666",
                   nodeColour = "#69b3a2",
                   opacity = 0.7,
                   zoom = T
)


# (6b)
# Count the number of primary genre albums for every artist.
artist_genre_sankeyNetwork <- count(albums, artist, primary_genre)
colnames(artist_genre_sankeyNetwork) <- c('artist','primary_genre','count_album_genre','IDsource','IDtarget')
# Only keep a subset of the df for simpler plotting.
artist_genre_sankeyNetwork <- filter(artist_genre_sankeyNetwork, count_album_genre > 30)

# Convert artists name and primary genre as nodes for networkD3 and then
# give them a unique ID. 
nodes <- data.frame(
  name=c(as.character(artist_genre_sankeyNetwork$artist), 
         as.character(artist_genre_sankeyNetwork$primary_genre)) %>% unique()
)
artist_genre_sankeyNetwork$IDsource <- match(artist_genre_sankeyNetwork$artist, nodes$name)-1 
artist_genre_sankeyNetwork$IDtarget <- match(artist_genre_sankeyNetwork$primary_genre, nodes$name)-1

# Plot graph
sankeyNetwork(Links = artist_genre_sankeyNetwork, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "count_album_genre", NodeID = "name", 
                       sinksRight=FALSE)


# (6c)
# Number of albums distribution per year for a selected artist.
artist_albums_per_year <- count(albums, artist, year)
colnames(artist_albums_per_year) <- c('artist','year','album_counter')

# Filter what artist you want to plot.
artist_albums_per_year <- filter(artist_albums_per_year, artist == "Coldplay")

# Plot
ggplot(artist_albums_per_year, aes(year, album_counter)) + 
  geom_bar(stat = "identity", fill = "#1cbaeb", width = 0.7) +
  labs(x = "Year", y = "Albums") +
  ggtitle("Number of albums distribution per year.",
          subtitle = "For selected artist.") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68")) +
  #scale_x_continuous(breaks=seq(1938,2022,10)) + 
  theme_minimal() +
  geom_text(aes(label = album_counter), color = "black", 
            position = position_dodge(width = 1), vjust = 1.1)
