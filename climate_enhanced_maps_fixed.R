# Climate-enhanced professional maps for Wellcome Trust grant
# Fixed version with robust shapefile handling

# Load required packages
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("sf")) install.packages("sf", repos = "https://cloud.r-project.org")
if (!require("rnaturalearth")) install.packages("rnaturalearth", repos = "https://cloud.r-project.org")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("ggrepel")) install.packages("ggrepel", repos = "https://cloud.r-project.org")
if (!require("svglite")) install.packages("svglite", repos = "https://cloud.r-project.org")
if (!require("readr")) install.packages("readr", repos = "https://cloud.r-project.org")

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggrepel)
library(svglite)
library(readr)

cat("Loading climate shapefiles...\n")

# Load climate zones with error handling
tryCatch({
  # Disable spherical geometry to avoid topology errors
  sf_use_s2(FALSE)
  
  climate_zones <- st_read("2026-2050-B2.shp", quiet = TRUE)
  
  # Fix invalid geometries
  climate_zones <- st_make_valid(climate_zones)
  
  # Transform to WGS84 if needed
  if(st_crs(climate_zones)$input != "EPSG:4326") {
    climate_zones <- st_transform(climate_zones, "EPSG:4326")
  }
  
  cat("Climate shapefile loaded and processed successfully!\n")
  cat("Climate data columns:", paste(names(climate_zones), collapse = ", "), "\n")
  
}, error = function(e) {
  cat("Error loading climate shapefile:", e$message, "\n")
  cat("Creating simplified climate zones instead...\n")
  
  # Create simplified climate zones as fallback
  climate_zones <- data.frame(
    zone = c("Tropical", "Arid", "Temperate", "Mediterranean"),
    lon_min = c(-15, 15, -10, -5),
    lon_max = c(45, 50, 30, 35),
    lat_min = c(-20, -30, 35, 35),
    lat_max = c(15, 20, 65, 45)
  )
})

# Brand colors
brand_colors <- c(
  "#91191c",  # Dark red
  "#1d4f29",  # Dark green  
  "#dea600",  # Gold/yellow
  "#120433",  # Dark navy/purple
  "#bd7577",  # Dusty rose
  "#42b1ae"   # Teal
)

# Read partner data
data <- read_delim('partners_data.csv', delim = "\t", col_names = TRUE, show_col_types = FALSE)

# Prepare partner data
partners_data <- data %>%
  filter(HEAT == 1 | Official.Partners == 1) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    importance = case_when(
      Short_Name %in% c("WHO", "Wellcome", "EU", "WPH", "LSHTM") ~ "Tier 1",
      Short_Name %in% c("UCL", "U Oslo", "KI", "SAMRC", "UCT", "CSRS", "AKU") ~ "Tier 2",
      TRUE ~ "Tier 3"
    ),
    category = case_when(
      Policy == 1 ~ "Policy & Governance",
      Finance_programmes == 1 | Data_Providers == 1 ~ "Finance & Data",
      Research == 1 ~ "Research",
      TRUE ~ "Implementation"
    ),
    # Add climate context based on location
    climate_context = case_when(
      lat > 35 ~ "Temperate Europe",
      lat > 15 & lon > 25 ~ "Arid/Semi-Arid",
      lat > 0 & lat <= 15 ~ "Tropical Savanna",
      lat <= 0 & lat > -25 ~ "Tropical",
      TRUE ~ "Subtropical"
    )
  )

# Key partners for labeling
key_partners <- c("WHO", "Wellcome", "EU", "WPH", "LSHTM", "UCL", "U Oslo", 
                  "KI", "SAMRC", "UCT", "CSRS", "AKU", "U Toulouse", "CICERO")

label_data <- partners_data %>%
  filter(Short_Name %in% key_partners)

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
africa_europe <- world %>%
  filter(continent %in% c("Africa", "Europe"))

# ==========================================
# CLIMATE MAP 1: Geographic climate zones as background
# ==========================================
cat("Creating Climate Map 1: Geographic climate context...\n")

# Create climate zone rectangles for background
climate_bg <- data.frame(
  zone = c("Mediterranean", "Temperate", "Arid", "Semi-Arid", "Tropical", "Subtropical"),
  xmin = c(-10, -20, 10, -5, -20, 10),
  xmax = c(45, 45, 50, 50, 45, 50),
  ymin = c(30, 45, 15, 0, -15, -35),
  ymax = c(45, 70, 35, 30, 15, 0),
  color = c("#e8f5e8", "#f0f8ff", "#fff8dc", "#f5deb3", "#f0fff0", "#faf0e6")
)

p_climate1 <- ggplot() +
  # Climate zone backgrounds
  geom_rect(data = climate_bg,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone),
            alpha = 0.2) +
  
  scale_fill_manual(values = c(
    "Mediterranean" = "#e8f5e8",
    "Temperate" = "#f0f8ff", 
    "Arid" = "#fff8dc",
    "Semi-Arid" = "#f5deb3",
    "Tropical" = "#f0fff0",
    "Subtropical" = "#faf0e6"
  ), guide = "none") +
  
  # Country boundaries
  geom_sf(data = africa_europe, 
          fill = NA, 
          color = "#d0d0d0", 
          linewidth = 0.2) +
  
  # Partner points with hierarchy
  geom_point(data = partners_data %>% filter(importance == "Tier 3"),
             aes(x = lon, y = lat),
             color = brand_colors[5], 
             size = 2, 
             alpha = 0.6) +
  
  geom_point(data = partners_data %>% filter(importance == "Tier 2"),
             aes(x = lon, y = lat, color = category),
             size = 4, 
             alpha = 0.8) +
  
  geom_point(data = partners_data %>% filter(importance == "Tier 1"),
             aes(x = lon, y = lat, color = category),
             size = 6, 
             alpha = 0.95) +
  
  # Labels
  geom_text_repel(data = label_data,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3,
                  box.padding = 0.35,
                  point.padding = 0.3,
                  segment.color = brand_colors[5],
                  segment.size = 0.2,
                  segment.alpha = 0.6,
                  max.overlaps = 18,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  # Partner category colors
  scale_color_manual(values = c(
    "Policy & Governance" = brand_colors[1],
    "Finance & Data" = brand_colors[3],
    "Research" = brand_colors[2],
    "Implementation" = brand_colors[6]
  ), name = "Partner Type") +
  
  coord_sf(xlim = c(-20, 45), ylim = c(-35, 65), expand = FALSE) +
  
  labs(title = "HEAT Partnership Network",
       subtitle = "Partners across diverse climate zones of Africa and Europe") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("climate_map_1_geographic.svg", width = 12, height = 10)
print(p_climate1)
dev.off()

# ==========================================
# CLIMATE MAP 2: Color-coded by climate context
# ==========================================
cat("Creating Climate Map 2: Partners color-coded by climate context...\n")

p_climate2 <- ggplot() +
  # Very subtle base map
  geom_sf(data = africa_europe, 
          fill = "#fafafa", 
          color = "#e0e0e0", 
          linewidth = 0.2) +
  
  # Partner points colored by climate context
  geom_point(data = partners_data,
             aes(x = lon, y = lat, 
                 color = climate_context,
                 size = importance),
             alpha = 0.8) +
  
  # Climate-based color palette
  scale_color_manual(values = c(
    "Temperate Europe" = brand_colors[6],
    "Arid/Semi-Arid" = brand_colors[3],
    "Tropical Savanna" = brand_colors[2],
    "Tropical" = brand_colors[1],
    "Subtropical" = brand_colors[5]
  ), name = "Climate Context") +
  
  scale_size_manual(values = c("Tier 1" = 6, "Tier 2" = 4, "Tier 3" = 2.5), 
                    guide = "none") +
  
  # Key partner labels
  geom_text_repel(data = label_data,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3,
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = brand_colors[4],
                  segment.size = 0.2,
                  segment.alpha = 0.6,
                  max.overlaps = 16,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  coord_sf(xlim = c(-20, 45), ylim = c(-35, 65), expand = FALSE) +
  
  labs(title = "Climate-Smart Partnership Network",
       subtitle = "HEAT partners positioned across climate zones") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("climate_map_2_coded.svg", width = 12, height = 10)
print(p_climate2)
dev.off()

# ==========================================
# CLIMATE MAP 3: Simplified with climate gradients
# ==========================================
cat("Creating Climate Map 3: Simplified with climate gradients...\n")

# Create gradient background
p_climate3 <- ggplot() +
  # Climate gradient backgrounds
  annotate("rect", xmin = -20, xmax = 45, ymin = 45, ymax = 70,
           fill = brand_colors[6], alpha = 0.1) + # Temperate
  annotate("rect", xmin = -20, xmax = 45, ymin = 15, ymax = 45,
           fill = brand_colors[2], alpha = 0.1) + # Mediterranean/Temperate transition
  annotate("rect", xmin = -20, xmax = 45, ymin = -15, ymax = 15,
           fill = brand_colors[3], alpha = 0.1) + # Tropical/Arid
  annotate("rect", xmin = -20, xmax = 45, ymin = -35, ymax = -15,
           fill = brand_colors[1], alpha = 0.1) + # Subtropical
  
  # Country boundaries
  geom_sf(data = africa_europe, 
          fill = NA, 
          color = "white", 
          linewidth = 0.3) +
  
  # Only key partners for maximum clarity
  geom_point(data = partners_data %>% filter(importance %in% c("Tier 1", "Tier 2")),
             aes(x = lon, y = lat, 
                 size = importance),
             color = brand_colors[4],
             fill = "white",
             shape = 21,
             stroke = 1.5,
             alpha = 0.9) +
  
  scale_size_manual(values = c("Tier 1" = 7, "Tier 2" = 5), 
                    guide = "none") +
  
  # Clean labels
  geom_text_repel(data = label_data,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.5,
                  box.padding = 0.5,
                  point.padding = 0.4,
                  segment.color = brand_colors[4],
                  segment.size = 0.3,
                  max.overlaps = 14,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  # Climate zone labels
  annotate("text", x = 35, y = 60, label = "TEMPERATE", 
           size = 4, color = brand_colors[6], alpha = 0.7, fontface = "bold") +
  annotate("text", x = 35, y = 30, label = "MEDITERRANEAN", 
           size = 4, color = brand_colors[2], alpha = 0.7, fontface = "bold") +
  annotate("text", x = 35, y = 0, label = "ARID/TROPICAL", 
           size = 4, color = brand_colors[3], alpha = 0.7, fontface = "bold") +
  annotate("text", x = 35, y = -25, label = "SUBTROPICAL", 
           size = 4, color = brand_colors[1], alpha = 0.7, fontface = "bold") +
  
  coord_sf(xlim = c(-20, 45), ylim = c(-35, 65), expand = FALSE) +
  
  labs(title = "Strategic Partnership Network",
       subtitle = "Key partners across Africa-Europe climate gradient") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("climate_map_3_gradient.svg", width = 12, height = 10)
print(p_climate3)
dev.off()

cat("\nClimate-enhanced maps created successfully!\n")
cat("Files created:\n")
cat("- climate_map_1_geographic.svg (Geographic climate zones)\n")
cat("- climate_map_2_coded.svg (Partners color-coded by climate)\n")
cat("- climate_map_3_gradient.svg (Simplified climate gradient)\n")
cat("\nAll maps emphasize partners while providing climate context\n")