# Southern Africa map for Wellcome Trust Climate Center application
# Fixed version without ocean download dependency

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

# Define all Southern Africa partners identified from the PDF
climate_center_partners <- c(
  "WPH",           # Wits PHR
  "CSAG",          # Climate Systems Analysis Group  
  "SPHFM",         # School of Public Health and Family Medicine
  "UCT",           # The Health Foundation (UCT-affiliated)
  "WCPH",          # Western Cape Provincial Health
  "SAMRC",         # South African Medical Research Council
  "SU",            # Stellenbosch University
  "S27",           # Section 27
  "UP",            # University of Pretoria
  "UKZN Law",     # Univ. of KwaZulu-Natal Law Centre
  "U Malawi",      # University of Malawi
  "CeSHHAR",       # CeSHHAR (Zimbabwe)
  "U Botswana",    # University of Botswana
  "MOSASWA"        # MOSASWA (Mozambique)
)

# Filter for Southern Africa Climate Center partners
sa_climate_partners <- data %>%
  filter(Country %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", "Malawi")) %>%
  filter(Short_Name %in% climate_center_partners) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    category = case_when(
      Policy == 1 ~ "Policy & Governance",
      Finance_programmes == 1 | Data_Providers == 1 ~ "Finance & Data", 
      Research == 1 ~ "Research",
      Engagement..Advocacy..and.Capacity.Building == 1 ~ "Engagement",
      TRUE ~ "Partner"
    ),
    # Create clean labels based on the PDF
    clean_label = case_when(
      Short_Name == "WPH" ~ "Wits PHR",
      Short_Name == "CSAG" ~ "Climate Systems Analysis Group",
      Short_Name == "SPHFM" ~ "School of Public Health & Family Medicine",
      Short_Name == "UCT" ~ "The Health Foundation",
      Short_Name == "WCPH" ~ "Western Cape Provincial Health",
      Short_Name == "SAMRC" ~ "SAMRC",
      Short_Name == "SU" ~ "Stellenbosch University",
      Short_Name == "S27" ~ "Section 27",
      Short_Name == "UP" ~ "University of Pretoria", 
      Short_Name == "UKZN Law" ~ "Univ. of KwaZulu-Natal Law Centre",
      Short_Name == "U Malawi" ~ "Univ. of Malawi",
      Short_Name == "CeSHHAR" ~ "CeSHHAR",
      Short_Name == "U Botswana" ~ "University of Botswana",
      Short_Name == "MOSASWA" ~ "MOSASWA",
      TRUE ~ Institution
    )
  )

cat("Found", nrow(sa_climate_partners), "Climate Center partners in Southern Africa:\n")
for(country in unique(sa_climate_partners$Country)) {
  partners_in_country <- sa_climate_partners %>% filter(Country == country)
  cat(country, ":", paste(partners_in_country$clean_label, collapse = ", "), "\n")
}

# Get Southern Africa countries
world <- ne_countries(scale = "medium", returnclass = "sf")
southern_africa_map <- world %>%
  filter(name %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", 
                     "Malawi", "Zambia", "Namibia", "Eswatini", "Lesotho"))

# Category colors using brand palette
category_colors <- c(
  "Policy & Governance" = brand_colors[1],    # Dark red
  "Finance & Data" = brand_colors[3],         # Gold
  "Research" = brand_colors[2],               # Dark green
  "Engagement" = brand_colors[4],             # Navy
  "Partner" = brand_colors[6]                 # Teal
)

# ==========================================
# WELLCOME TRUST CLIMATE CENTER - SOUTHERN AFRICA MAP
# ==========================================
cat("Creating Wellcome Trust Climate Center Southern Africa map...\n")

p_climate_center <- ggplot() +
  # Light blue background for the entire plot area (representing sea/ocean)
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Country boundaries in light gray  
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        # Very light gray for land
          color = "#d0d0d0",       # Light gray for boundaries
          linewidth = 0.4) +
  
  # Climate Center partner points
  geom_point(data = sa_climate_partners,
             aes(x = lon, y = lat, color = category),
             size = 6, 
             alpha = 0.9) +
  
  # White outline for better visibility against blue background
  geom_point(data = sa_climate_partners,
             aes(x = lon, y = lat),
             color = "white",
             size = 7,
             alpha = 0.6) +
  
  # Partner labels
  geom_text_repel(data = sa_climate_partners,
                  aes(x = lon, y = lat, label = clean_label),
                  size = 3,
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = brand_colors[4],
                  segment.size = 0.3,
                  segment.alpha = 0.8,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  # Color scale
  scale_color_manual(values = category_colors, name = "Partner Type") +
  
  # Focus on Southern Africa region
  coord_sf(xlim = c(15, 36), ylim = c(-36, -8), expand = FALSE) +
  
  # Professional styling for grant application
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Partnership Network") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#e6f3ff", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save as SVG
svglite("wellcome_climate_center_southern_africa.svg", width = 11, height = 13)
print(p_climate_center)
dev.off()

# ==========================================
# CLEAN VERSION: Minimal design
# ==========================================
cat("Creating clean minimal version...\n")

p_clean <- ggplot() +
  # Light blue background for sea
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Country boundaries
  geom_sf(data = southern_africa_map, 
          fill = "#f8f8f8",        
          color = "#d0d0d0",       
          linewidth = 0.3) +
  
  # Partner points - uniform styling
  geom_point(data = sa_climate_partners,
             aes(x = lon, y = lat),
             color = brand_colors[4],
             fill = brand_colors[3],
             shape = 21,
             size = 6, 
             stroke = 1.5,
             alpha = 0.95) +
  
  # Clean labels
  geom_text_repel(data = sa_climate_partners,
                  aes(x = lon, y = lat, label = clean_label),
                  size = 2.8,
                  box.padding = 0.3,
                  point.padding = 0.2,
                  segment.color = brand_colors[5],
                  segment.size = 0.2,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  coord_sf(xlim = c(15, 36), ylim = c(-36, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Partners") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#e6f3ff", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("wellcome_climate_center_clean.svg", width = 11, height = 13)
print(p_clean)
dev.off()

cat("\nWellcome Trust Climate Center Southern Africa maps created!\n")
cat("Files:\n")
cat("- wellcome_climate_center_southern_africa.svg (with legend)\n") 
cat("- wellcome_climate_center_clean.svg (minimal version)\n")
cat("\nTotal partners:", nrow(sa_climate_partners), "across", length(unique(sa_climate_partners$Country)), "countries\n")