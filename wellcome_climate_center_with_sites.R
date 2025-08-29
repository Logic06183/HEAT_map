# Final Wellcome Trust Climate Center map with field sites
# Polished versions with field sites included

# Load packages
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("sf")) install.packages("sf", repos = "https://cloud.r-project.org")
if (!require("rnaturalearth")) install.packages("rnaturalearth", repos = "https://cloud.r-project.org")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("ggrepel")) install.packages("ggrepel", repos = "https://cloud.r-project.org")
if (!require("svglite")) install.packages("svglite", repos = "https://cloud.r-project.org")

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggrepel)
library(svglite)

# Brand colors
brand_colors <- c(
  "#91191c",  # Dark red
  "#1d4f29",  # Dark green  (Co-applicants)
  "#dea600",  # Gold/yellow
  "#120433",  # Dark navy/purple (text)
  "#bd7577",  # Dusty rose
  "#42b1ae"   # Teal (Collaborators)
)

# Partner list
polished_partners <- data.frame(
  Institution = c(
    # Co-applicants
    "Wits Planetary Health Research",
    "Climate Systems Analysis Group", 
    "University of Cape Town",
    "Western Cape Provincial Health",
    "SA Medical Research Council",
    "Stellenbosch University",
    "Section 27",
    "University of Malawi",
    "Kamuzu University of Health Sciences",
    "Centre for Sexual Health and HIV AIDS Research",
    # Collaborators  
    "LEAD Southern Africa",
    "World Health Organization",
    "SA Weather Service",
    "Statistics South Africa"
  ),
  Short_Name = c("Wits PHR", "Climate Systems", "Univ Cape Town", "WC Health", 
                 "SA Medical Research", "Stellenbosch Univ", "Section 27",
                 "Univ Malawi", "KUHeS", "CeSHHAR", 
                 "LEAD", "WHO", "Weather Service", "Stats SA"),
  Country = c("South Africa", "South Africa", "South Africa", "South Africa",
              "South Africa", "South Africa", "South Africa", "Malawi", 
              "Malawi", "Zimbabwe", "Malawi", "Zimbabwe", "South Africa", "South Africa"),
  lon = c(28.0305, 18.4607, 18.4597, 18.4241, 28.0473, 18.8654, 28.0307,
          35.331234, 35.2, 31.060158, 35.0, 31.0, 28.2041, 28.2118),
  lat = c(-26.1913, -33.9301, -33.9575, -33.9249, -26.1825, -33.9328, -26.1921,
          -15.386391, -15.2, -17.856704, -15.0, -17.8, -25.8069, -25.7461),
  type = c("Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", 
           "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant",
           "Collaborator", "Collaborator", "Collaborator", "Collaborator"),
  stringsAsFactors = FALSE
)

# Field sites
field_sites <- data.frame(
  Site = c("Agincourt HDSS", "Soweto", "Thohoyandou", "Blantyre", 
           "Harare Central", "Cape Flats"),
  Country = c("South Africa", "South Africa", "South Africa", "Malawi", 
              "Zimbabwe", "South Africa"),
  lon = c(31.2, 27.9, 30.5, 35.0, 31.05, 18.5),
  lat = c(-24.8, -26.26, -22.9, -15.8, -17.83, -33.95),
  stringsAsFactors = FALSE
)

# Get Southern Africa countries
world <- ne_countries(scale = "medium", returnclass = "sf")
southern_africa_map <- world %>%
  filter(name %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", 
                     "Malawi", "Zambia", "Namibia", "Eswatini", "Lesotho",
                     "Angola", "Tanzania"))

# Type colors
type_colors <- c(
  "Co-applicant" = "#1d4f29",   # Dark green
  "Collaborator" = "#42b1ae"    # Teal
)

# Group partners by regions for better labeling
cape_town_partners <- polished_partners %>% 
  filter(lat < -33 & lon < 20)

johannesburg_partners <- polished_partners %>% 
  filter(lat > -27 & lat < -25 & lon > 27)

malawi_partners <- polished_partners %>% 
  filter(Country == "Malawi")

zimbabwe_partners <- polished_partners %>% 
  filter(Country == "Zimbabwe")

# ==========================================
# VERSION 1: POLISHED WITH FIELD SITES
# ==========================================
cat("Creating polished map with field sites...\n")

p_with_sites <- ggplot() +
  # Ocean background
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Land areas
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#b0b0b0",       
          linewidth = 0.7) +
  
  # Field sites (triangular markers, subtle)
  geom_point(data = field_sites,
             aes(x = lon, y = lat),
             shape = 17,  # Triangle
             size = 3.5,
             color = "#8B7355",
             alpha = 0.7) +
  
  # Field site labels (smaller, italic)
  geom_text_repel(data = field_sites,
                  aes(x = lon, y = lat, label = Site),
                  size = 2.8,
                  family = "Arial",
                  color = "#8B7355",
                  fontface = "italic",
                  box.padding = 0.2,
                  point.padding = 0.1,
                  segment.color = "#8B7355",
                  segment.size = 0.2,
                  segment.alpha = 0.4,
                  max.overlaps = 15,
                  force = 0.5) +
  
  # Partner points (on top of field sites)
  geom_point(data = polished_partners,
             aes(x = lon, y = lat, color = type),
             size = 7,
             alpha = 0.9) +
  
  # Partner labels (strategic placement)
  geom_text_repel(data = polished_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.5,
                  family = "Arial",
                  fontface = "bold",
                  color = "#120433",
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = "#120433",
                  segment.size = 0.3,
                  segment.alpha = 0.5,
                  force = 1.5,
                  max.overlaps = 30,
                  min.segment.length = 0,
                  direction = "both",
                  seed = 123) +
  
  scale_color_manual(values = type_colors, name = "") +
  
  coord_sf(xlim = c(14, 37), ylim = c(-36, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Co-applicants, Collaborators & Field Sites") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#e6f3ff", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 20, face = "bold", color = "#120433", 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = "#120433", 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.text = element_text(size = 11, face = "bold", color = "#120433"),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("wellcome_climate_center_final_with_sites.svg", width = 11, height = 13)
print(p_with_sites)
dev.off()

# ==========================================
# VERSION 2: LAYERED WITH FIELD SITES (FIGMA READY)
# ==========================================
cat("Creating layered version with field sites for Figma...\n")

p_layered_sites <- ggplot() +
  # Ocean background
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Land masses
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#b0b0b0",       
          linewidth = 0.7) +
  
  # Field sites
  geom_point(data = field_sites,
             aes(x = lon, y = lat),
             shape = 17,  # Triangle
             size = 3.5,
             color = "#8B7355",
             alpha = 0.7) +
  
  # Field site labels (positioned to avoid partner labels)
  geom_text_repel(data = field_sites,
                  aes(x = lon, y = lat, label = Site),
                  size = 2.8,
                  family = "Arial",
                  color = "#8B7355",
                  fontface = "italic",
                  box.padding = 0.2,
                  point.padding = 0.1,
                  segment.color = "#8B7355",
                  segment.size = 0.2,
                  segment.alpha = 0.4,
                  max.overlaps = 15,
                  force = 0.3) +
  
  # Partner points
  geom_point(data = polished_partners,
             aes(x = lon, y = lat, color = type),
             size = 7,
             alpha = 0.9) +
  
  # Cape Town area labels (left side)
  geom_text_repel(data = cape_town_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.5,
                  family = "Arial",
                  fontface = "bold",
                  color = "#120433",
                  xlim = c(NA, 17),  # Force labels to left
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "#120433",
                  segment.size = 0.3,
                  segment.alpha = 0.5,
                  max.overlaps = 10) +
  
  # Johannesburg area labels (right side)
  geom_text_repel(data = johannesburg_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.5,
                  family = "Arial",
                  fontface = "bold",
                  color = "#120433",
                  xlim = c(29, NA),  # Force labels to right
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "#120433",
                  segment.size = 0.3,
                  segment.alpha = 0.5,
                  max.overlaps = 10) +
  
  # Malawi labels (top)
  geom_text_repel(data = malawi_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.5,
                  family = "Arial",
                  fontface = "bold",
                  color = "#120433",
                  ylim = c(NA, -14),  # Force labels up
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "#120433",
                  segment.size = 0.3,
                  segment.alpha = 0.5,
                  max.overlaps = 10) +
  
  # Zimbabwe labels (center)
  geom_text_repel(data = zimbabwe_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.5,
                  family = "Arial",
                  fontface = "bold",
                  color = "#120433",
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "#120433",
                  segment.size = 0.3,
                  segment.alpha = 0.5,
                  max.overlaps = 10) +
  
  scale_color_manual(values = type_colors, name = "") +
  
  coord_sf(xlim = c(14, 37), ylim = c(-36, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Network & Field Sites") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#e6f3ff", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 20, face = "bold", color = "#120433", 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = "#120433", 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.text = element_text(size = 11, face = "bold", color = "#120433"),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("wellcome_climate_center_layered_with_sites.svg", width = 12, height = 13)
print(p_layered_sites)
dev.off()

cat("\n🎯 FINAL MAPS WITH FIELD SITES:\n")
cat("✅ wellcome_climate_center_final_with_sites.svg - Polished version with field sites\n") 
cat("✅ wellcome_climate_center_layered_with_sites.svg - Figma-ready with field sites\n")
cat("\n📍 Field sites included:\n")
for(i in 1:nrow(field_sites)) {
  cat("   •", field_sites$Site[i], "(", field_sites$Country[i], ")\n")
}
cat("\n✅ Both versions ready for final editing!\n")