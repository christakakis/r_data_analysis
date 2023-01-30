# Import libraries
library(data.table)
library(tidyverse)
library(tidygraph)
library(lubridate)
library(networkD3)
library(stringr)
library(ggplot2)
library(ggraph)
library(readr)
library(tmap)
library(sf)

# Make tmap interactive
tmap_mode("view")

# [Pre-processing]
# Define paths to the datasets.  
path_1 <- "Population_by_Borough_NYC.csv"
path_2 <- "NYPD_Shooting_Incident_Data__Historic_.csv"

# Load the dataframes.
population <- read_csv(path_1)
shootings <- read_csv(path_2)

# Load shapefile and covert to upper in order to be able to merge dfs together.
shapename <- read_sf("nybb_22b/nybb.shp")
shapename$BoroName <- toupper(shapename$BoroName)

# For some reason R can't process the .csv file correctly, 
# so we split the columns with the help of delimiter (;).
shootings <- tidyr::separate(shootings, 1, 
               into = c("INCIDENT_KEY", "OCCUR_DATE",
                                      "OCCUR_TIME", "BORO",
                                      "PRECINCT", "JURISDICTION_CODE",
                                      "LOCATION_DESC", "STATISTICAL_MURDER_FLAG",
                                      "PERP_AGE_GROUP", "PERP_SEX",
                                      "PERP_RACE", "VIC_AGE_GROUP",
                                      "VIC_SEX", "VIC_RACE",
                                      "X_COORD_CD", "Y_COORD_CD",
                                      "Latitude", "Longitude",
                                      "Lon_Lat"), sep = "[;]")

# Also transform logical values to capital letters for obvious reasons.
shootings$STATISTICAL_MURDER_FLAG <- toupper(shootings$STATISTICAL_MURDER_FLAG)

# Checking for NA values for the whole dataset and for separate columns.
sum(is.na(shootings))
colSums(is.na(shootings))


# [1]
# Committed murders per borough.
# Create the desired sub-dataframe from counting what we want.
murders_per_borough <- count(shootings, BORO, STATISTICAL_MURDER_FLAG)
# Filter only the TRUE observations and keep the two cols we want
murders_per_borough <- filter(murders_per_borough, STATISTICAL_MURDER_FLAG == TRUE)
murders_per_borough <- select(murders_per_borough, -STATISTICAL_MURDER_FLAG)
# Rename columns in order to merge the dataframes we want.
colnames(murders_per_borough) <- c('BoroName','Murders')
murders_per_borough <- merge(murders_per_borough, shapename, by="BoroName")
# Convert to sf object
murders_per_borough <- st_as_sf(murders_per_borough)

# Plot map
tm_shape(murders_per_borough) + 
  tm_polygons(col = "Murders", palette = "seq", border.col = "#9c350c") + 
  tm_text("Murders", col = "#7568ed")


# [2]
# BoxPlot of Attempted Murders Throughout The Years.
# Obtain only year of the shootings.
shootings$OCCUR_DATE <- format(as.Date(shootings$OCCUR_DATE, format="%m/%d/%Y"),"%Y")
# Count the attempted true and false murders.
murders_per_year <- count(shootings, OCCUR_DATE, STATISTICAL_MURDER_FLAG)
# Transform them to numeric for plotting.
murders_per_year$OCCUR_DATE <- as.numeric(murders_per_year$OCCUR_DATE) 

# Plot
ggplot(data=murders_per_year, aes(x=OCCUR_DATE, y=n, colour=STATISTICAL_MURDER_FLAG)) +
  geom_bar(stat='identity') +
  theme_minimal() + 
  scale_color_brewer(palette="Set2") +
  labs(x = "Year", y = "Number of Attempted Murders") +
  ggtitle("BoxPlot of Attempted Murders Throughout The Years.", 
          subtitle = "For observing how many were successful or not.") +
  geom_text(aes(label = n), color = "white", 
            position = position_dodge(width = 1), vjust = 1.1)


# [3]
# Race Mosaic Plot relationships between race of perpetrators and victims. 
# Count all the possible links between the races.
racial <- count(shootings, PERP_RACE, VIC_RACE)
# Replace empty values with Unknown.
racial[racial == ''] <- "UNKNOWN"
# Remove all the unknown registries from the dataframe.
racial <- filter(racial, PERP_RACE != "UNKNOWN")
racial <- filter(racial, VIC_RACE != "UNKNOWN")
# Create a table of the columns
two_way_table <- table(racial$PERP_RACE, racial$VIC_RACE)

# Plot
mosaicplot(two_way_table, main = "Race Mosaic Plot",
           sub = "Relationships of all race attempted murder pairs",
           xlab = "Perpator Race",
           ylab = "Victim Race",
           las = 4,
           color = "skyblue2",
           border = "chocolate"
)


# [4]
# Sankey Network Graph with links between the year and number of shootings for Perpator sex. 
# For a subset of total for simpler plotting. M is Male, U is Unknown.
# Count the occurrences for every year.
sex <- count(shootings, OCCUR_DATE, PERP_SEX)
# Replace empty values with letter U from Unknown.
sex[sex == ''] <- "U"

# Only keep a subset of the df for simpler plotting.
sex <- filter(sex, n > 400)
# Convert artists name and primary genre as nodes for networkD3 and then
# give them a unique ID. 
nodes <- data.frame(
  name=c(as.character(sex$OCCUR_DATE), 
         as.character(sex$PERP_SEX)) %>% unique()
)
sex$IDsource <- match(sex$OCCUR_DATE, nodes$name)-1 
sex$IDtarget <- match(sex$PERP_SEX, nodes$name)-1

# Plot graph
sankeyNetwork(Links = sex, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "name", 
              sinksRight=FALSE)


# [5]
# The most common hour of shootings per borough and the total count for that time.
# New dataframe for safety reasons.
shootings_hours <- shootings
# Extract hour of shooting time in 24-hr format.
shootings_hours$OCCUR_TIME <- factor(shootings_hours$OCCUR_TIME)
shootings_hours$OCCUR_TIME <- hms(as.character(shootings_hours$OCCUR_TIME))
# Keep only hours.
shootings_hours$OCCUR_TIME <- lubridate::hour(shootings_hours$OCCUR_TIME)
# Count how many shootings there were 
# for each hour of the 24, for each borough.
times <- count(shootings_hours, OCCUR_TIME, BORO)
times <- as.data.table(times)
# Keep the maximum value of counter for each hour and borough combination.
times <- times[, .SD[which.max(n)], by=BORO]
# Rename column names.
colnames(times) <- c('BoroName','OCCUR_TIME', 'Shootings')
# Merge the dataframe by borough name.
times <- merge(times, shapename, by="BoroName")
# Convert to sf object in order to link
# with geospatial information.
times <- st_as_sf(times)

# Plot
tm_shape(times) + 
  tm_polygons(col = "Shootings", palette = "seq", border.col = "#9c350c") + 
  tm_text("OCCUR_TIME", col = "#7568ed")