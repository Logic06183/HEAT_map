# Install required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("maps")) install.packages("maps", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("tidyr")) install.packages("tidyr", repos = "https://cloud.r-project.org")
if (!require("countrycode")) install.packages("countrycode", repos = "https://cloud.r-project.org")
if (!require("readr")) install.packages("readr", repos = "https://cloud.r-project.org")
if (!require("ggrepel")) install.packages("ggrepel", repos = "https://cloud.r-project.org")

# Load libraries
library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(countrycode)
library(readr)
library(ggrepel)

# Read the CSV file with proper handling for tab-delimited data
data <- read_delim('partners_data.csv', delim = "\t", col_names = TRUE)
cat("Data loaded with", nrow(data), "rows\n")

# Print column names to verify structure
cat("Column names:", paste(head(colnames(data), 10), collapse=", "), "\n")

# Check if we have a Country column and HEAT column
if ("Country" %in% colnames(data) && "HEAT" %in% colnames(data)) {
  cat("Found 'Country' and 'HEAT' columns\n")
  
  # Filter for HEAT project partners only
  heat_data <- data %>% 
    filter(HEAT == 1)
  
  cat("Filtered to", nrow(heat_data), "HEAT project partners\n")
  
  # Print first few countries to verify
  cat("First few HEAT countries:", paste(head(heat_data$Country, 10), collapse=", "), "\n")
} else {
  stop("Could not find required columns. Check the column names.")
}

# Clean up the country names
heat_data$Country <- trimws(heat_data$Country)

# Print unique countries to debug
unique_countries <- unique(heat_data$Country)
cat("Found", length(unique_countries), "unique countries with HEAT partners\n")
cat("Sample countries:", paste(head(unique_countries, 10), collapse=", "), "\n")

# Count institutions by country
country_counts <- heat_data %>%
  group_by(Country) %>%
  summarise(count = n()) %>%
  filter(!is.na(Country) & Country != "")
cat("Processed data for", nrow(country_counts), "countries with HEAT partners\n")

# Create a bar chart of top countries
bar_file <- "heat_partners_barchart.png"
png(bar_file, width = 1200, height = 800, res = 100)

top_countries <- country_counts %>%
  arrange(desc(count)) %>%
  head(15)

ggplot(top_countries, aes(x = reorder(Country, count), y = count)) +
  geom_bar(stat = "identity", fill = "#A50F15") +
  coord_flip() +
  labs(title = "Top 15 Countries with HEAT Project Partners",
       x = "",
       y = "Number of Partner Institutions") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

dev.off()
cat("Bar chart saved to", bar_file, "\n")

# Create a world map using ggplot2
output_file <- "heat_partner_heatmap.png"
png(output_file, width = 1600, height = 900, res = 120)

# Get world map data
world <- map_data("world")

# Create a comprehensive mapping dictionary for country names
country_mapping <- c(
  "United States" = "USA",
  "USA" = "USA",
  "United Kingdom" = "UK",
  "UK" = "UK",
  "South Africa" = "South Africa",
  "Côte d'Ivoire" = "Ivory Coast",
  "Burkina Faso" = "Burkina Faso",
  "Tanzania" = "Tanzania",
  "Kenya" = "Kenya",
  "Zimbabwe" = "Zimbabwe",
  "Malawi" = "Malawi",
  "Chad" = "Chad",
  "Ethiopia" = "Ethiopia",
  "Ghana" = "Ghana",
  "Senegal" = "Senegal",
  "Togo" = "Togo",
  "Uganda" = "Uganda",
  "Rwanda" = "Rwanda",
  "Nigeria" = "Nigeria",
  "Mali" = "Mali",
  "Mozambique" = "Mozambique",
  "Namibia" = "Namibia",
  "Botswana" = "Botswana",
  "Zambia" = "Zambia",
  "France" = "France",
  "Germany" = "Germany",
  "Italy" = "Italy",
  "Spain" = "Spain",
  "Sweden" = "Sweden",
  "Norway" = "Norway",
  "Finland" = "Finland",
  "Belgium" = "Belgium",
  "Netherlands" = "Netherlands",
  "Switzerland" = "Switzerland",
  "Austria" = "Austria",
  "Denmark" = "Denmark",
  "Estonia" = "Estonia",
  "Greece" = "Greece",
  "Ireland" = "Ireland",
  "South Korea" = "South Korea",
  "Burundi" = "Burundi",
  "Cameroon" = "Cameroon",
  "Mauritania" = "Mauritania"
)

# Clean country names and apply mapping
country_counts <- country_counts %>%
  mutate(
    # Apply mapping
    region = case_when(
      Country %in% names(country_mapping) ~ country_mapping[Country],
      TRUE ~ Country
    )
  )

# Join data with map
choropleth <- left_join(world, country_counts, by = "region")

# Plot
ggplot(data = choropleth, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = count), color = "white", linewidth = 0.1) +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15", na.value = "grey90", name = "Number of Partners") +
  theme_minimal() +
  labs(title = "Global Distribution of HEAT Project Partners",
       subtitle = paste("Total of", nrow(heat_data), "HEAT partner institutions across", 
                       nrow(country_counts), "countries"),
       caption = "Source: HEAT Partnership Data") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_fixed(1.3)

dev.off()
cat("Map plot saved to", output_file, "\n")

# Create a second version with different styling
output_file2 <- "heat_partner_heatmap_alt.png"
png(output_file2, width = 1600, height = 900, res = 120)

# Plot with different styling
ggplot(data = choropleth, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = count), color = "#333333", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#FEE5D9", 
    mid = "#FB6A4A",
    high = "#67000D",
    midpoint = median(country_counts$count, na.rm = TRUE),
    na.value = "#EEEEEE", 
    name = "Number of HEAT Partners"
  ) +
  theme_minimal() +
  labs(title = "Global Distribution of HEAT Project Partners",
       subtitle = paste("Total of", nrow(heat_data), "HEAT partner institutions across", 
                       nrow(country_counts), "countries"),
       caption = "Source: HEAT Partnership Data") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#CCECF4"),
    plot.background = element_rect(fill = "white")
  ) +
  coord_fixed(1.3)

dev.off()
cat("Alternative map plot saved to", output_file2, "\n")

# Add a new section after the main map plot to create a labeled version

# Alternative labeled map version
output_file3 <- "heat_partner_labeled_map.png"
png(output_file3, width = 1800, height = 1000, res = 120)

# Prepare the label data - exclude data providers, Gueladio_Cisse partners, and US Department of Commerce
label_data <- heat_data %>%
  # Only select columns we know exist
  select(Institution, Country, lon, lat, Data_Providers, Gueladio_Cisse) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  # Filter out data providers (where Data_Providers == 1)
  filter(Data_Providers == 0 | is.na(Data_Providers)) %>%
  # Filter out Gueladio_Cisse partners (where Gueladio_Cisse == 1)
  filter(Gueladio_Cisse == 0 | is.na(Gueladio_Cisse)) %>%
  # Filter out US Department of Commerce specifically
  filter(Institution != "US Department of Commerce")

# Print some debug info
cat("Created label data with", nrow(label_data), "rows (after applying all exclusion filters)\n")

# Plot the map with labels
ggplot() +
  # Base map
  geom_polygon(data = choropleth, aes(x = long, y = lat, group = group, fill = count), 
               color = "white", linewidth = 0.1) +
  # Add points for all institutions (including data providers)
  geom_point(data = heat_data %>% filter(!is.na(lon) & !is.na(lat)), 
             aes(x = lon, y = lat), 
             color = "black", size = 2, alpha = 0.7) +
  # Add text labels only for non-data providers
  geom_text_repel(data = label_data, aes(x = lon, y = lat, label = Institution),
                 size = 2.5, box.padding = 0.5, point.padding = 0.3,
                 segment.color = "grey50", min.segment.length = 0,
                 max.overlaps = 15) +
  # Styling
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15", na.value = "grey90", 
                     name = "Number of Partners") +
  theme_minimal() +
  labs(title = "HEAT Project Partners with Institution Labels",
       subtitle = paste("Total of", nrow(heat_data), "HEAT partner institutions across", 
                       nrow(country_counts), "countries (data providers not labeled)"),
       caption = "Source: HEAT Partnership Data") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_fixed(1.3)

dev.off()
cat("Labeled map saved to", output_file3, "\n")
