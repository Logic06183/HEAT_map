# Final Wellcome Trust Climate Center map with team feedback applied
# Based on provided SVG styling and team corrections

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

# Brand colors matching your SVG
brand_colors <- c(
  "#91191c",  # Dark red
  "#1d4f29",  # Dark green  (Co-applicants from SVG)
  "#dea600",  # Gold/yellow
  "#120433",  # Dark navy/purple (text color from SVG)
  "#bd7577",  # Dusty rose
  "#42b1ae"   # Teal (Collaborators from SVG)
)

# FINAL partner list with ALL team feedback applied:
final_partners <- data.frame(
  Institution = c(
    "Wits Planetary Health Research",
    "Climate Systems Analysis Group", 
    "University of Cape Town",
    "Western Cape Provincial Health",
    "South African Medical Research Council",
    "Stellenbosch University", 
    "Section 27",
    "University of Malawi",
    "KUHeS",                             # CORRECTED capitalization
    "CeSHHAR",                          # CORRECTED capitalization  
    "LEAD Southern and Eastern Africa",  # Collaborator (not co-applicant)
    "World Health Organization",         # Collaborator
    "South African Weather Service",     # Collaborator
    "Statistics South Africa"            # Collaborator
  ),
  Short_Name = c("Wits PHR", "CSAG", "UCT", "WCPH", "SAMRC", "SU", "Section 27",
                 "U Malawi", "KUHeS", "CeSHHAR", "LEAD", "WHO", "SAWS", "StatsSA"),
  Country = c("South Africa", "South Africa", "South Africa", "South Africa",
              "South Africa", "South Africa", "South Africa", "Malawi", 
              "Malawi", "Zimbabwe", "Malawi", "Zimbabwe", "South Africa", "South Africa"),
  lon = c(28.0305, 18.4607, 18.4597, 18.4241, 28.0473, 18.8654, 28.0307,
          35.331234, 35.2, 31.060158, 35.0, 31.0, 28.2041, 28.2118),
  lat = c(-26.1913, -33.9301, -33.9575, -33.9249, -26.1825, -33.9328, -26.1921,
          -15.386391, -15.2, -17.856704, -15.0, -17.8, -25.8069, -25.7461),
  # UPDATED terminology: Co-applicant vs Collaborator (not Partner vs Collaborator)
  type = c("Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", 
           "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant", "Co-applicant",
           "Collaborator", "Collaborator", "Collaborator", "Collaborator"),
  stringsAsFactors = FALSE
)

cat("Final partner list with team feedback:\n")
cat("Co-applicants:", nrow(final_partners %>% filter(type == "Co-applicant")), "\n")
cat("Collaborators:", nrow(final_partners %>% filter(type == "Collaborator")), "\n")
cat("✅ LEAD marked as Collaborator (not Co-applicant)\n")
cat("✅ Capitalizations fixed: KUHeS, CeSHHAR\n")
cat("✅ Terminology updated: Co-applicant vs Collaborator\n")

# Get Southern Africa countries
world <- ne_countries(scale = "medium", returnclass = "sf")
southern_africa_map <- world %>%
  filter(name %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", 
                     "Malawi", "Zambia", "Namibia", "Eswatini", "Lesotho"))

# Colors matching your SVG exactly
type_colors <- c(
  "Co-applicant" = "#1d4f29",   # Dark green from SVG circles
  "Collaborator" = "#42b1ae"    # Teal from SVG circles
)

# ==========================================
# FINAL MAP - EXACT SVG STYLING
# ==========================================
cat("Creating final map with exact SVG styling...\n")

p_final <- ggplot() +
  # Light blue background matching SVG
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  # Country boundaries in light gray (matching SVG)
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5",        
          color = "#d0d0d0",       
          linewidth = 0.4) +
  
  # Points exactly matching SVG styling
  geom_point(data = final_partners,
             aes(x = lon, y = lat, color = type),
             size = 6.76 * 2,  # Matching SVG radius * 2 for diameter
             alpha = 0.9) +
  
  # White outline circles (matching SVG white outline effect)
  geom_point(data = final_partners,
             aes(x = lon, y = lat),
             color = "white",
             size = 6.76 * 2.2,
             alpha = 0.6) +
  
  # Labels with exact SVG text styling
  geom_text_repel(data = final_partners,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.2,                    # Matching SVG 9.10px
                  family = "Arial",              # Matching SVG font
                  fontface = "bold",             # Matching SVG font-weight
                  color = "#120433",             # Exact SVG text color
                  box.padding = 0.4,
                  point.padding = 0.3,
                  segment.color = "#120433",
                  segment.size = 0.3,
                  max.overlaps = 20) +
  
  # Exact color matching from SVG
  scale_color_manual(values = type_colors, name = "") +
  
  coord_sf(xlim = c(15, 36), ylim = c(-36, -8), expand = FALSE) +
  
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

# Save with exact dimensions
svglite("wellcome_climate_center_final.svg", width = 11, height = 13)
print(p_final)
dev.off()

# ==========================================
# ALTERNATIVE: Hub/Portfolio split option
# ==========================================
cat("Creating hub/portfolio alternative...\n")

# Add hub/portfolio categorization
final_partners_hub <- final_partners %>%
  mutate(
    hub_portfolio = case_when(
      # Research hub
      Short_Name %in% c("Wits PHR", "CSAG", "UCT", "SAMRC", "U Malawi", "KUHeS", "CeSHHAR") ~ "Research Hub",
      # Policy/Implementation portfolio  
      Short_Name %in% c("WCPH", "Section 27", "SAWS", "StatsSA") ~ "Policy Portfolio",
      # External collaborators
      Short_Name %in% c("LEAD", "WHO", "SU") ~ "External Partners",
      TRUE ~ "Other"
    )
  )

# Hub/Portfolio colors
hub_colors <- c(
  "Research Hub" = "#1d4f29",      # Dark green
  "Policy Portfolio" = "#91191c",   # Dark red  
  "External Partners" = "#42b1ae"   # Teal
)

p_hub <- ggplot() +
  theme(panel.background = element_rect(fill = "#e6f3ff", color = NA)) +
  
  geom_sf(data = southern_africa_map, 
          fill = "#f5f5f5", color = "#d0d0d0", linewidth = 0.4) +
  
  geom_point(data = final_partners_hub,
             aes(x = lon, y = lat, color = hub_portfolio),
             size = 6.76 * 2, alpha = 0.9) +
  
  geom_point(data = final_partners_hub,
             aes(x = lon, y = lat),
             color = "white", size = 6.76 * 2.2, alpha = 0.6) +
  
  geom_text_repel(data = final_partners_hub,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3.2, family = "Arial", fontface = "bold",
                  color = "#120433", box.padding = 0.4, point.padding = 0.3,
                  segment.color = "#120433", segment.size = 0.3, max.overlaps = 20) +
  
  scale_color_manual(values = hub_colors, name = "") +
  
  coord_sf(xlim = c(15, 36), ylim = c(-36, -8), expand = FALSE) +
  
  labs(title = "Wellcome Trust Climate Center",
       subtitle = "Southern Africa Hubs & Portfolios") +
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

svglite("wellcome_climate_center_hubs.svg", width = 11, height = 13)
print(p_hub)
dev.off()

cat("\n🎯 FINAL MAPS CREATED WITH ALL TEAM FEEDBACK:\n")
cat("✅ wellcome_climate_center_final.svg - Co-applicants/Collaborators (recommended)\n") 
cat("✅ wellcome_climate_center_hubs.svg - Hub/Portfolio alternative\n")
cat("\n📋 Applied feedback:\n")
cat("✅ Partner → Co-applicant terminology\n")
cat("✅ LEAD as Collaborator (teal color)\n") 
cat("✅ Fixed: KUHeS, CeSHHAR capitalizations\n")
cat("✅ Exact SVG styling match\n")
cat("✅ Ready for field sites when identified\n")