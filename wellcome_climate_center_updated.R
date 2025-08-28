# Updated Wellcome Trust Climate Center Southern Africa map
# Based on feedback: remove/add partners, simplify UCT, simplified colors

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

# UPDATED partner list based on feedback:
# REMOVE: MOSASWA, U Botswana, UKZN Law, UP
# KEEP existing: WPH, CSAG, SPHFM, UCT, WCPH, SAMRC, SU, S27, U Malawi, CeSHHAR
# ADD: Lead (Malawi), WHO (Zimbabwe), SAWS, StatsSA

# Create the updated partner dataset
updated_partners <- data.frame(
  Institution = c(
    "Wits Planetary Health Research",
    "Climate Systems Analysis Group", 
    "University of Cape Town",  # Simplified UCT
    "Western Cape Provincial Health",
    "South African Medical Research Council",
    "Stellenbosch University", 
    "Section 27",
    "University of Malawi",
    "CeSHHAR",
    "LEAD Southern and Eastern Africa",  # New collaborator
    "World Health Organization",         # New collaborator
    "South African Weather Service",     # New collaborator  
    "Statistics South Africa"            # New collaborator
  ),
  Short_Name = c("Wits PHR", "CSAG", "UCT", "WCPH", "SAMRC", "SU", "Section 27",
                 "U Malawi", "CeSHHAR", "LEAD", "WHO", "SAWS", "StatsSA"),
  Country = c("South Africa", "South Africa", "South Africa", "South Africa",
              "South Africa", "South Africa", "South Africa", "Malawi", 
              "Zimbabwe", "Malawi", "Zimbabwe", "South Africa", "South Africa"),
  lon = c(28.0305, 18.4607, 18.4597, 18.4241, 28.0473, 18.8654, 28.0307,
          35.331234, 31.060158, 35.0, 31.0, 28.2041, 28.2118),
  lat = c(-26.1913, -33.9301, -33.9575, -33.9249, -26.1825, -33.9328, -26.1921,
          -15.386391, -17.856704, -15.0, -17.8, -25.8069, -25.7461),
  category = c("Research", "Research", "Research", "Policy", "Research", 
               "Research", "Policy", "Research", "Research", "Research",
               "Policy", "Research", "Policy"),
  type = c("Partner", "Partner", "Partner", "Partner", "Partner", 
           "Partner", "Partner", "Partner", "Partner", "Collaborator",
           "Collaborator", "Collaborator", "Collaborator"),
  stringsAsFactors = FALSE
)

cat("Updated partner list:\n")
cat("Partners:", nrow(updated_partners %>% filter(type == "Partner")), "\n")
cat("Collaborators:", nrow(updated_partners %>% filter(type == "Collaborator")), "\n")
cat("Countries:", paste(unique(updated_partners$Country), collapse = ", "), "\n")

# Get Southern Africa countries
world <- ne_countries(scale = "medium", returnclass = "sf")
southern_africa_map <- world %>%
  filter(name %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", 
                     "Malawi", "Zambia", "Namibia", "Eswatini", "Lesotho"))

# ==========================================
# VERSION 1: All same color
# ==========================================
cat("Creating Version 1: All same color...\n")

p_uniform <- ggplot() +
  # Light blue background for sea
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Country boundaries in light gray
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#d0d0d0",       
          linewidth = 0.4) +
  
  # All partners - uniform color
  geom_point(data = updated_partners,
             aes(x = lon, y = lat),
             color = brand_colors[4],  # Navy blue
             fill = brand_colors[3],   # Gold
             shape = 21,
             size = 6, 
             stroke = 1.5,
             alpha = 0.9) +
  
  # Clean labels
  geom_text_repel(data = updated_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.2,
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = brand_colors[4],
                  segment.size = 0.3,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  coord_sf(xlim = c(15, 36), ylim = c(-36, -8), expand = FALSE) +
  
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
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("wellcome_climate_center_uniform.svg", width = 11, height = 13)
print(p_uniform)
dev.off()

# ==========================================
# VERSION 2: Research vs Policy split
# ==========================================
cat("Creating Version 2: Research/Policy split...\n")

# Simple color scheme: Research vs Policy
simple_colors <- c(
  "Research" = brand_colors[2],  # Dark green
  "Policy" = brand_colors[1]     # Dark red
)

p_split <- ggplot() +
  # Light blue background for sea
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Country boundaries in light gray
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#d0d0d0",       
          linewidth = 0.4) +
  
  # Partners colored by Research/Policy
  geom_point(data = updated_partners,
             aes(x = lon, y = lat, color = category),
             size = 6, 
             alpha = 0.9) +
  
  # White outline for better visibility
  geom_point(data = updated_partners,
             aes(x = lon, y = lat),
             color = "white",
             size = 7,
             alpha = 0.6) +
  
  # Labels
  geom_text_repel(data = updated_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.2,
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = brand_colors[4],
                  segment.size = 0.3,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  # Simple color scale
  scale_color_manual(values = simple_colors, name = "") +
  
  coord_sf(xlim = c(15, 36), ylim = c(-36, -8), expand = FALSE) +
  
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
    legend.text = element_text(size = 11, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("wellcome_climate_center_split.svg", width = 11, height = 13)
print(p_split)
dev.off()

# ==========================================
# VERSION 3: Partners vs Collaborators
# ==========================================
cat("Creating Version 3: Partners/Collaborators distinction...\n")

# Color by type
type_colors <- c(
  "Partner" = brand_colors[2],      # Dark green
  "Collaborator" = brand_colors[6]  # Teal
)

p_type <- ggplot() +
  # Light blue background for sea
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Country boundaries in light gray
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#d0d0d0",       
          linewidth = 0.4) +
  
  # Points colored by Partner/Collaborator type
  geom_point(data = updated_partners,
             aes(x = lon, y = lat, color = type),
             size = 6, 
             alpha = 0.9) +
  
  # White outline
  geom_point(data = updated_partners,
             aes(x = lon, y = lat),
             color = "white",
             size = 7,
             alpha = 0.6) +
  
  # Labels
  geom_text_repel(data = updated_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.2,
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = brand_colors[4],
                  segment.size = 0.3,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  scale_color_manual(values = type_colors, name = "") +
  
  coord_sf(xlim = c(15, 36), ylim = c(-36, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Partners & Collaborators") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#e6f3ff", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.text = element_text(size = 11, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("wellcome_climate_center_types.svg", width = 11, height = 13)
print(p_type)
dev.off()

cat("\nUpdated Wellcome Trust Climate Center maps created!\n")
cat("Changes made:\n")
cat("✅ REMOVED: MOSASWA, U Botswana, UKZN Law, UP\n")
cat("✅ ADDED: Lead (Malawi), WHO (Zimbabwe), SAWS, StatsSA\n") 
cat("✅ SIMPLIFIED: UCT consolidated\n")
cat("✅ THREE COLOR OPTIONS:\n")
cat("   - wellcome_climate_center_uniform.svg (all same color)\n")
cat("   - wellcome_climate_center_split.svg (research/policy split)\n")
cat("   - wellcome_climate_center_types.svg (partners/collaborators)\n")
cat("\nFinal count:", nrow(updated_partners), "organizations across", length(unique(updated_partners$Country)), "countries\n")
cat("Ready for team feedback!\n")