# Professional local stakeholder map for Wellcome Trust grant
# Johannesburg/Pretoria region with best practices applied

# Load packages
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("ggrepel")) install.packages("ggrepel", repos = "https://cloud.r-project.org")
if (!require("svglite")) install.packages("svglite", repos = "https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(ggrepel)
library(svglite)

# Brand colors
brand_colors <- c(
  "#91191c",  # Dark red
  "#1d4f29",  # Dark green
  "#dea600",  # Gold/yellow
  "#120433",  # Dark navy/purple
  "#bd7577",  # Dusty rose
  "#42b1ae"   # Teal
)

# Local partner data with tier system
local_data <- data.frame(
  name = c("Wits PHR", "University of Pretoria", "Stellenbosch University",
           "Department of Health", "Department of Science & Innovation",
           "South African Weather Service", "IBM Research Africa",
           "Division of Human Genetics", "Quantium Health", "Sydney Brenner Institute",
           "Right to Care", "Clinical HIV Research Unit", "The Aurum Institute",
           "Wits RHI", "Chris Hani Baragwanath Hospital", "Helen Joseph Hospital",
           "Section 27", "Global Change Institute", "CSAG", "Re-Action Consulting"),
  short = c("Wits PHR", "UP", "SU", "DoH", "DSI", "SAWS", "IBM", "DHG", 
            "QH", "SBI", "RTC", "CHRU", "Aurum", "RHI", "CHBH", "HJH", 
            "Section 27", "GCI", "CSAG", "RAC"),
  lon = c(28.0305, 28.2293, 18.8654, 28.1871, 28.2118, 28.2041, 28.0687,
          28.0382, 28.0566, 28.0382, 28.0194, 28.019, 28.0405, 28.0437,
          27.9407, 28.0196, 28.0307, 28.0298, 18.4607, 28.0473),
  lat = c(-26.1913, -25.7545, -33.9328, -25.7461, -25.7461, -25.8069, -26.0455,
          -26.1814, -26.0999, -26.1814, -26.1739, -26.1745, -26.1864, -26.1874,
          -26.2601, -26.1742, -26.1921, -26.1905, -33.9301, -26.2041),
  tier = c("Tier 1", "Tier 1", "Tier 2", "Tier 1", "Tier 1", "Tier 2", "Tier 2",
           "Tier 2", "Tier 3", "Tier 2", "Tier 3", "Tier 2", "Tier 2", "Tier 2",
           "Tier 3", "Tier 3", "Tier 2", "Tier 3", "Tier 2", "Tier 3"),
  category = c("Research", "Research", "Research", "Policy", "Policy", "Data",
               "Research", "Research", "Partner", "Research", "Partner", "Research",
               "Research", "Research", "Partner", "Partner", "Engagement", "Research",
               "Research", "Partner"),
  area = c("Johannesburg", "Pretoria", "Cape Town", "Pretoria", "Pretoria",
           "Pretoria", "Johannesburg", "Johannesburg", "Johannesburg", "Johannesburg",
           "Johannesburg", "Johannesburg", "Johannesburg", "Johannesburg",
           "Johannesburg", "Johannesburg", "Johannesburg", "Johannesburg",
           "Cape Town", "Johannesburg"),
  stringsAsFactors = FALSE
)

# Filter for main areas (remove Cape Town for focus)
local_main <- local_data %>%
  filter(area %in% c("Johannesburg", "Pretoria"))

# Category colors
category_colors <- c(
  "Policy" = brand_colors[1],
  "Research" = brand_colors[2],
  "Data" = brand_colors[6],
  "Engagement" = brand_colors[4],
  "Partner" = brand_colors[5]
)

# Tier sizes
tier_sizes <- c("Tier 1" = 6, "Tier 2" = 4, "Tier 3" = 2.5)

# ==========================================
# PROFESSIONAL LOCAL MAP 1: Clean hierarchy
# ==========================================
cat("Creating Professional Local Map: Clean hierarchy...\n")

p_local <- ggplot() +
  # Area backgrounds (very subtle)
  annotate("rect", xmin = 27.9, xmax = 28.35, ymin = -26, ymax = -25.65,
           fill = brand_colors[6], alpha = 0.05) +
  annotate("rect", xmin = 27.85, xmax = 28.35, ymin = -26.35, ymax = -26,
           fill = brand_colors[2], alpha = 0.05) +
  
  # Area labels (subtle)
  annotate("text", x = 28.22, y = -25.75, label = "PRETORIA", 
           size = 8, color = brand_colors[6], alpha = 0.15, fontface = "bold") +
  annotate("text", x = 28.05, y = -26.25, label = "JOHANNESBURG", 
           size = 8, color = brand_colors[2], alpha = 0.15, fontface = "bold") +
  
  # Connection lines from Wits PHR to government partners
  geom_segment(data = local_main %>% filter(category == "Policy"),
               aes(x = 28.0305, y = -26.1913, xend = lon, yend = lat),
               alpha = 0.15, color = brand_colors[1], linewidth = 1) +
  
  # All partners with hierarchy
  geom_point(data = local_main,
             aes(x = lon, y = lat, 
                 color = category, 
                 size = tier),
             alpha = 0.85) +
  
  # Smart labels with hierarchy
  geom_text_repel(data = local_main,
                  aes(x = lon, y = lat, 
                      label = short,
                      fontface = case_when(tier == "Tier 1" ~ "bold",
                                         tier == "Tier 2" ~ "plain",
                                         TRUE ~ "plain")),
                  size = case_when(local_main$tier == "Tier 1" ~ 3.5,
                                  local_main$tier == "Tier 2" ~ 3,
                                  TRUE ~ 2.5),
                  box.padding = 0.35,
                  point.padding = 0.25,
                  segment.color = brand_colors[5],
                  segment.size = 0.3,
                  segment.alpha = 0.6,
                  max.overlaps = 30,
                  color = brand_colors[4]) +
  
  # Highlight Wits PHR as central hub
  geom_point(data = local_main %>% filter(short == "Wits PHR"),
             aes(x = lon, y = lat),
             color = brand_colors[3], 
             fill = "white",
             shape = 21, 
             size = 8, 
             stroke = 2.5) +
  
  # Scale and colors
  scale_size_manual(values = tier_sizes, guide = "none") +
  scale_color_manual(values = category_colors, name = "") +
  
  # Coordinate limits
  coord_fixed(xlim = c(27.85, 28.35), ylim = c(-26.35, -25.65)) +
  
  # Scale bar
  annotate("segment", x = 28.2, xend = 28.25, y = -26.32, yend = -26.32,
           linewidth = 1.5, color = brand_colors[4]) +
  annotate("text", x = 28.225, y = -26.31, label = "10 km", 
           size = 3.5, color = brand_colors[4], fontface = "bold") +
  
  # North arrow
  annotate("text", x = 28.32, y = -25.72, label = "N", 
           size = 6, fontface = "bold", color = brand_colors[4]) +
  annotate("segment", x = 28.32, xend = 28.32, y = -25.74, yend = -25.78,
           arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
           linewidth = 1.2, color = brand_colors[4]) +
  
  # Professional styling
  labs(title = "Local Partnership Network",
       subtitle = "Johannesburg-Pretoria regional stakeholders") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    legend.margin = margin(t = 20),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("professional_local_clean.svg", width = 11, height = 9)
print(p_local)
dev.off()

# ==========================================
# PROFESSIONAL LOCAL MAP 2: Focused on key partners only
# ==========================================
cat("Creating Professional Local Map: Key partners focus...\n")

# Select only the most important local partners
key_local <- local_main %>%
  filter(tier %in% c("Tier 1", "Tier 2")) %>%
  filter(!short %in% c("SU", "CSAG"))  # Remove Cape Town partners for focus

p_local2 <- ggplot() +
  # Subtle background
  geom_rect(aes(xmin = 27.9, xmax = 28.3, ymin = -26.3, ymax = -25.7),
            fill = "#fafafa", alpha = 0.5) +
  
  # Hub connections from Wits PHR
  geom_segment(data = key_local %>% filter(short != "Wits PHR"),
               aes(x = 28.0305, y = -26.1913, xend = lon, yend = lat),
               alpha = 0.2, color = brand_colors[6], linewidth = 0.8) +
  
  # Partner points with clear hierarchy
  geom_point(data = key_local,
             aes(x = lon, y = lat, color = category),
             size = 5,
             alpha = 0.9) +
  
  # Wits PHR as central hub
  geom_point(data = key_local %>% filter(short == "Wits PHR"),
             aes(x = lon, y = lat),
             color = brand_colors[3], 
             fill = "white",
             shape = 21, 
             size = 9, 
             stroke = 3) +
  
  # Clear, readable labels
  geom_text_repel(data = key_local,
                  aes(x = lon, y = lat, label = short),
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = brand_colors[5],
                  segment.size = 0.4,
                  segment.alpha = 0.7,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  scale_color_manual(values = category_colors, name = "Partner Type") +
  
  coord_fixed(xlim = c(27.9, 28.3), ylim = c(-26.3, -25.7)) +
  
  labs(title = "Core Local Partnership Network",
       subtitle = "Key strategic partners in Johannesburg-Pretoria region") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    legend.margin = margin(t = 20),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("professional_local_focused.svg", width = 11, height = 9)
print(p_local2)
dev.off()

cat("\nProfessional local maps created successfully!\n")
cat("Files created:\n")
cat("- professional_local_clean.svg (Complete local network)\n")
cat("- professional_local_focused.svg (Key partners only)\n")
cat("\nBoth maps are optimized for Wellcome Trust grant applications\n")