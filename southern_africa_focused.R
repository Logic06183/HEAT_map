# Southern Africa focused map for main partners
# Clean design with light blue background per feedback

# Load packages
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

# Define Southern Africa main partners based on feedback
main_partners <- c("WPH", "UCT", "S27", "CSAG", "SPHFM", "SAMRC", "SU", "UP")

# Filter for Southern Africa countries and main partners
southern_africa_data <- data %>%
  filter(Country %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", "Malawi", 
                       "Zambia", "Namibia")) %>%
  filter(Short_Name %in% main_partners | 
         Institution %in% c("Wits Planetary Health Research", "Section 27", 
                           "The Health Foundation", "Climate Systems Analysis Group",
                           "School of Public Health and Family Medicine")) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    category = case_when(
      Policy == 1 ~ "Policy & Governance",
      Finance_programmes == 1 | Data_Providers == 1 ~ "Finance & Data", 
      Research == 1 ~ "Research",
      Engagement..Advocacy..and.Capacity.Building == 1 ~ "Engagement",
      TRUE ~ "Implementation"
    ),
    # Create cleaner labels
    clean_label = case_when(
      Short_Name == "WPH" ~ "Wits PHR",
      Short_Name == "UCT" ~ "UCT",
      Short_Name == "S27" ~ "Section 27", 
      Short_Name == "CSAG" ~ "CSAG",
      Short_Name == "SPHFM" ~ "UCT Public Health",
      Short_Name == "SAMRC" ~ "SAMRC",
      Short_Name == "SU" ~ "Stellenbosch Univ",
      Short_Name == "UP" ~ "Univ of Pretoria",
      TRUE ~ Short_Name
    )
  )

cat("Found", nrow(southern_africa_data), "main partners in Southern Africa:\n")
cat(paste(southern_africa_data$clean_label, collapse = ", "), "\n")

# Get Southern Africa countries
world <- ne_countries(scale = "medium", returnclass = "sf")
southern_africa_map <- world %>%
  filter(name %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", 
                     "Malawi", "Zambia", "Namibia", "Eswatini", "Lesotho"))

# Category colors
category_colors <- c(
  "Policy & Governance" = brand_colors[1],    # Dark red
  "Finance & Data" = brand_colors[3],         # Gold
  "Research" = brand_colors[2],               # Dark green
  "Engagement" = brand_colors[4],             # Navy
  "Implementation" = brand_colors[6]          # Teal
)

# ==========================================
# SOUTHERN AFRICA FOCUSED MAP
# ==========================================
cat("Creating Southern Africa focused map...\n")

p_sa <- ggplot() +
  # Light blue background as requested
  geom_sf(data = southern_africa_map, 
          fill = "#e6f3ff",        # Light blue background
          color = "#b3d9ff",       # Slightly darker blue for borders
          linewidth = 0.5) +
  
  # Main partner points - larger and prominent
  geom_point(data = southern_africa_data,
             aes(x = lon, y = lat, color = category),
             size = 6, 
             alpha = 0.9) +
  
  # White outline for better contrast
  geom_point(data = southern_africa_data,
             aes(x = lon, y = lat),
             color = "white",
             size = 7,
             alpha = 0.5) +
  
  # Clean labels with partner names
  geom_text_repel(data = southern_africa_data,
                  aes(x = lon, y = lat, label = clean_label),
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.4,
                  segment.color = brand_colors[4],
                  segment.size = 0.4,
                  segment.alpha = 0.8,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  # Color scale
  scale_color_manual(values = category_colors, name = "Partner Type") +
  
  # Focus on Southern Africa region (Malawi down to SA)
  coord_sf(xlim = c(16, 35), ylim = c(-35, -8), expand = FALSE) +
  
  # Clean styling
  labs(title = "Southern Africa Partnership Network",
       subtitle = "Main HEAT partners and collaborators") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save as SVG
svglite("southern_africa_main_partners.svg", width = 10, height = 12)
print(p_sa)
dev.off()

# ==========================================
# ALTERNATIVE: Even cleaner version
# ==========================================
cat("Creating cleaner alternative version...\n")

p_sa_clean <- ggplot() +
  # Very minimal background - just country outlines
  geom_sf(data = southern_africa_map, 
          fill = "#f8fbff",        # Very light blue-white
          color = brand_colors[6], # Teal borders
          linewidth = 0.3) +
  
  # Partner points - bold and clear
  geom_point(data = southern_africa_data,
             aes(x = lon, y = lat),
             color = brand_colors[4],
             fill = brand_colors[3],
             shape = 21,
             size = 8, 
             stroke = 2,
             alpha = 0.9) +
  
  # Institution names as direct labels
  geom_text(data = southern_africa_data,
            aes(x = lon, y = lat, label = clean_label),
            size = 4,
            color = "white",
            fontface = "bold") +
  
  # Subtle connection lines between SA partners
  geom_segment(data = southern_africa_data %>% 
                filter(Country == "South Africa") %>%
                expand.grid(from = .$clean_label, to = .$clean_label) %>%
                filter(from != to) %>%
                left_join(southern_africa_data %>% select(clean_label, from_lon = lon, from_lat = lat), by = c("from" = "clean_label")) %>%
                left_join(southern_africa_data %>% select(clean_label, to_lon = lon, to_lat = lat), by = c("to" = "clean_label")) %>%
                slice_head(n = 5),  # Just a few connections
               aes(x = from_lon, y = from_lat, xend = to_lon, yend = to_lat),
               alpha = 0.1, color = brand_colors[6], linewidth = 0.5) +
  
  coord_sf(xlim = c(16, 35), ylim = c(-35, -8), expand = FALSE) +
  
  labs(title = "Southern Africa Core Partners",
       subtitle = "Key collaborators from Malawi to South Africa") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("southern_africa_clean.svg", width = 10, height = 12)
print(p_sa_clean)
dev.off()

cat("\nSouthern Africa focused maps created!\n")
cat("Files created:\n")
cat("- southern_africa_main_partners.svg (with legend)\n") 
cat("- southern_africa_clean.svg (minimal clean version)\n")
cat("\nPartners included:", paste(southern_africa_data$clean_label, collapse = ", "), "\n")
cat("Let me know if you need to add/remove any specific partners!\n")