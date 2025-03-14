# Install required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("maps")) install.packages("maps", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("ggrepel")) install.packages("ggrepel", repos = "https://cloud.r-project.org")
if (!require("sf")) install.packages("sf", repos = "https://cloud.r-project.org")
if (!require("rnaturalearth")) install.packages("rnaturalearth", repos = "https://cloud.r-project.org")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata", repos = "https://cloud.r-project.org")

# Load libraries
library(ggplot2)
library(maps)
library(dplyr)
library(ggrepel)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Set output file
output_file <- "RP1_conceptual_diagram.png"
png(output_file, width = 2000, height = 1200, res = 150)

# Get world map data with focus on Africa
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world[world$continent == "Africa", ]

# Create sample cohort data points only in Africa
# These are example locations - replace with actual cohort locations if available
cohorts <- data.frame(
  name = c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4", "Cohort 5", 
           "Cohort 6", "Cohort 7", "Cohort 8", "Cohort 9", "Cohort 10",
           "Cohort 11", "Cohort 12", "Cohort 13", "Cohort 14", "Cohort 15"),
  lon = c(18.4, 28.0, 36.8, 15.0, -4.0, 
          -17.4, 32.5, 30.9, 3.7, 11.5,
          39.2, 32.8, 15.4, 1.2, 27.9),
  lat = c(-33.9, -26.1, -1.3, 12.1, 5.3, 
          14.6, -25.9, -29.8, 3.8, 3.8,
          -6.8, -2.5, -15.3, 6.1, -19.4),
  size = c(15, 18, 12, 10, 14, 
           11, 13, 16, 9, 10,
           12, 14, 11, 10, 13)
)

# Create data processing center location (example)
data_center <- data.frame(
  name = "Data Processing Center",
  lon = 18.4,  # Moved to South Africa
  lat = -33.9,
  size = 30
)

# Create a base map focusing on Africa
p <- ggplot() +
  geom_sf(data = world, fill = "#E8F8F5", color = "#CCCCCC") +
  geom_sf(data = africa, fill = "#D6EAF8", color = "#85C1E9") +
  
  # Add cohort points
  geom_point(data = cohorts, aes(x = lon, y = lat, size = size),
             color = "#F39C12", fill = "#F1C40F", shape = 21, alpha = 0.8) +
  
  # Add data center
  geom_point(data = data_center, aes(x = lon, y = lat, size = size),
             color = "#2471A3", fill = "#3498DB", shape = 21, alpha = 0.8) +
  
  # Add arrows from cohorts to data center
  geom_segment(data = cohorts, 
               aes(x = lon, y = lat, xend = data_center$lon, yend = data_center$lat),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "#E74C3C", alpha = 0.6, linewidth = 0.8) +
  
  # Add IPD label at data center
  annotate("text", x = data_center$lon, y = data_center$lat - 10,
           label = "Individual Participant Data (IPD)\nAnalysis", 
           size = 6, fontface = "bold", color = "#2471A3") +
  
  # Styling
  labs(title = "Research Project 1: Individual Participant Data",
       subtitle = "Gathering cohort studies across sub-Saharan Africa for IPD analysis",
       caption = "Source: HEAT Partnership") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_sf(xlim = c(-25, 55), ylim = c(-40, 40))

# Print the plot
print(p)

# Save the plot
dev.off()
cat("Conceptual diagram saved to", output_file, "\n")
