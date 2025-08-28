# Southern Africa map for Wellcome Trust Climate Center application
# Includes all identified partners from the stakeholder map PDF

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

# Also include by institution name for broader matching
climate_center_institutions <- c(
  "Wits Planetary Health Research",
  "Climate Systems Analysis Group", 
  "School of Public Health and Family Medicine",
  "The Health Foundation",
  "Western Cape Provincial Health",
  "South African Medical Research Council",
  "Stellenbosch University",
  "Section 27",
  "University of Pretoria",
  "University of KwaZulu-Natal Law Centre",
  "University of Malawi",
  "CeSHHAR",
  "University of Botswana",
  "MOSASWA"
)

# Filter for Southern Africa Climate Center partners
sa_climate_partners <- data %>%
  filter(Country %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", "Malawi")) %>%
  filter(Short_Name %in% climate_center_partners | 
         Institution %in% climate_center_institutions |
         Readable_Name %in% climate_center_institutions) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    category = case_when(
      Policy == 1 ~ "Policy & Governance",
      Finance_programmes == 1 | Data_Providers == 1 ~ "Finance & Data", 
      Research == 1 ~ "Research",
      Engagement..Advocacy..and.Capacity.Building == 1 ~ "Engagement",
      TRUE ~ "Partner"
    ),
    # Create clean labels
    clean_label = case_when(
      Short_Name == "WPH" ~ "Wits PHR",
      Short_Name == "CSAG" ~ "CSAG",
      Short_Name == "SPHFM" ~ "UCT Public Health",
      Short_Name == "UCT" ~ "UCT",
      Short_Name == "WCPH" ~ "Western Cape Health",
      Short_Name == "SAMRC" ~ "SAMRC",
      Short_Name == "SU" ~ "Stellenbosch Univ",
      Short_Name == "S27" ~ "Section 27",
      Short_Name == "UP" ~ "Univ of Pretoria", 
      Short_Name == "UKZN Law" ~ "UKZN Law",
      Short_Name == "U Malawi" ~ "Univ of Malawi",
      Short_Name == "CeSHHAR" ~ "CeSHHAR",
      Short_Name == "U Botswana" ~ "Univ of Botswana",
      Short_Name == "MOSASWA" ~ "MOSASWA",
      TRUE ~ Institution
    )
  )

cat("Found", nrow(sa_climate_partners), "Climate Center partners in Southern Africa:\n")
cat("Countries represented:", paste(unique(sa_climate_partners$Country), collapse = ", "), "\n")
cat("Partners:", paste(sa_climate_partners$clean_label, collapse = ", "), "\n")

# Get Southern Africa countries for the map
world <- ne_countries(scale = "medium", returnclass = "sf")
southern_africa_map <- world %>%
  filter(name %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", 
                     "Malawi", "Zambia", "Namibia", "Eswatini", "Lesotho"))

# Get ocean/sea areas for light blue background
ocean <- ne_download(scale = "medium", type = "ocean", returnclass = "sf")

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
  # Ocean/sea background in light blue
  geom_sf(data = ocean, 
          fill = "#e6f3ff", 
          color = NA) +
  
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
  
  # White outline for better visibility
  geom_point(data = sa_climate_partners,
             aes(x = lon, y = lat),
             color = "white",
             size = 7,
             alpha = 0.6) +
  
  # Partner labels
  geom_text_repel(data = sa_climate_partners,
                  aes(x = lon, y = lat, label = clean_label),
                  size = 3.5,
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = brand_colors[4],
                  segment.size = 0.3,
                  segment.alpha = 0.7,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  # Color scale
  scale_color_manual(values = category_colors, name = "Partner Type") +
  
  # Focus on Southern Africa region
  coord_sf(xlim = c(16, 35), ylim = c(-35, -8), expand = FALSE) +
  
  # Professional styling for grant application
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Partnership Network") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save as SVG
svglite("wellcome_climate_center_southern_africa.svg", width = 10, height = 12)
print(p_climate_center)
dev.off()

# ==========================================
# ALTERNATIVE: Simplified version without legend
# ==========================================
cat("Creating simplified version...\n")

p_simple <- ggplot() +
  # Light blue sea background
  geom_sf(data = ocean, 
          fill = "#e6f3ff", 
          color = NA) +
  
  # Light gray countries
  geom_sf(data = southern_africa_map, 
          fill = "#f8f8f8",        
          color = "#d0d0d0",       
          linewidth = 0.3) +
  
  # Partner points - single color for simplicity
  geom_point(data = sa_climate_partners,
             aes(x = lon, y = lat),
             color = brand_colors[4],
             fill = brand_colors[3],
             shape = 21,
             size = 7, 
             stroke = 2,
             alpha = 0.9) +
  
  # Direct labels on points
  geom_text(data = sa_climate_partners,
            aes(x = lon, y = lat, label = clean_label),
            size = 3,
            color = "white",
            fontface = "bold") +
  
  coord_sf(xlim = c(16, 35), ylim = c(-35, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Partners") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("wellcome_climate_center_simple.svg", width = 10, height = 12)
print(p_simple)
dev.off()

cat("\nWellcome Trust Climate Center Southern Africa maps created!\n")
cat("Files created:\n")
cat("- wellcome_climate_center_southern_africa.svg (with legend)\n") 
cat("- wellcome_climate_center_simple.svg (simplified version)\n")
cat("\nPartners included across", length(unique(sa_climate_partners$Country)), "countries:\n")
for(country in unique(sa_climate_partners$Country)) {
  partners_in_country <- sa_climate_partners %>% filter(Country == country)
  cat(country, ":", paste(partners_in_country$clean_label, collapse = ", "), "\n")
}