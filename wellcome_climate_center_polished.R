# Polished Wellcome Trust Climate Center map 
# Refined coastal areas and strategic label placement

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

# Partner list with strategic positioning for better label placement
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
  # Strategic label directions to avoid overlaps
  label_x_nudge = c(1.5, 0, 0, -1.5, 1.5, 1.5, -1.5, 0, -1.5, 1.5, -1.5, -1.5, 0, 1.5),
  label_y_nudge = c(0, 0.5, -0.5, 0, 0.5, 0, -0.5, 0.5, 0, 0, -0.5, 0.5, -0.5, 0),
  type = c("Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", 
           "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant",
           "Collaborator", "Collaborator", "Collaborator", "Collaborator"),
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

# ==========================================
# POLISHED MAP WITH PROPER COASTAL AREAS
# ==========================================
cat("Creating polished map with refined ocean areas...\n")

p_polished <- ggplot() +
  # Start with light background (will be ocean)
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Land areas on top
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#b0b0b0",       
          linewidth = 0.7) +
  
  # Partner points
  geom_point(data = polished_partners,
             aes(x = lon, y = lat, color = type),
             size = 7,
             alpha = 0.9) +
  
  # Strategic label placement with nudging
  geom_text_repel(data = polished_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.5,
                  family = "Arial",
                  fontface = "bold",
                  color = "#120433",
                  nudge_x = polished_partners$label_x_nudge,
                  nudge_y = polished_partners$label_y_nudge,
                  box.padding = 0.3,
                  point.padding = 0.2,
                  segment.color = "#120433",
                  segment.size = 0.3,
                  segment.alpha = 0.5,
                  force = 1,
                  max.overlaps = 30,
                  min.segment.length = 0,
                  direction = "both",
                  seed = 123) +
  
  scale_color_manual(values = type_colors, name = "") +
  
  coord_sf(xlim = c(14, 37), ylim = c(-36, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Co-applicants & Collaborators") +
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

svglite("wellcome_climate_center_polished.svg", width = 11, height = 13)
print(p_polished)
dev.off()

# ==========================================
# ALTERNATIVE: LAYERED VERSION FOR FIGMA
# ==========================================
cat("Creating layered version optimized for Figma editing...\n")

# Group partners by geographic regions for better spacing
cape_town_partners <- polished_partners %>% 
  filter(lat < -33 & lon < 20)

johannesburg_partners <- polished_partners %>% 
  filter(lat > -27 & lat < -25 & lon > 27)

malawi_partners <- polished_partners %>% 
  filter(Country == "Malawi")

zimbabwe_partners <- polished_partners %>% 
  filter(Country == "Zimbabwe")

p_layered <- ggplot() +
  # Ocean background (full panel)
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Land masses
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#b0b0b0",       
          linewidth = 0.7) +
  
  # Partner points by region for cleaner grouping
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
  
  # Malawi labels (top right)
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
  
  # Wider view to show proper coastline
  coord_sf(xlim = c(14, 37), ylim = c(-36, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Co-applicants & Collaborators") +
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

svglite("wellcome_climate_center_layered.svg", width = 12, height = 13)
print(p_layered)
dev.off()

cat("\n🎯 POLISHED MAPS CREATED:\n")
cat("✅ wellcome_climate_center_polished.svg - Refined ocean/coastal areas\n") 
cat("✅ wellcome_climate_center_layered.svg - Optimized for Figma editing\n")
cat("\n📋 Improvements:\n")
cat("✅ Ocean properly surrounds African coast\n")
cat("✅ Strategic label placement by region\n") 
cat("✅ Labels forced to optimal directions\n")
cat("✅ Cleaner spacing using geographic grouping\n")
cat("✅ Wider view to show proper coastline\n")
cat("\nBoth versions are ready for final editing in Figma!\n")