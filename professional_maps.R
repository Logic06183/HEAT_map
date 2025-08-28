# Professional stakeholder maps for Wellcome Trust grant application
# Following best practices for dense point visualization

# Load required packages
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("sf")) install.packages("sf", repos = "https://cloud.r-project.org")
if (!require("rnaturalearth")) install.packages("rnaturalearth", repos = "https://cloud.r-project.org")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("ggrepel")) install.packages("ggrepel", repos = "https://cloud.r-project.org")
if (!require("svglite")) install.packages("svglite", repos = "https://cloud.r-project.org")
if (!require("readr")) install.packages("readr", repos = "https://cloud.r-project.org")
if (!require("cowplot")) install.packages("cowplot", repos = "https://cloud.r-project.org")

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggrepel)
library(svglite)
library(readr)
library(cowplot)

# Brand colors from the palette
brand_colors <- c(
  "#91191c",  # Dark red
  "#1d4f29",  # Dark green  
  "#dea600",  # Gold/yellow
  "#120433",  # Dark navy/purple
  "#bd7577",  # Dusty rose
  "#42b1ae"   # Teal
)

# Read data
data <- read_delim('partners_data.csv', delim = "\t", col_names = TRUE, show_col_types = FALSE)

# Prepare partner data with importance levels
partners_data <- data %>%
  filter(HEAT == 1 | Official.Partners == 1) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    # Create importance tiers for visual hierarchy
    importance = case_when(
      Short_Name %in% c("WHO", "Wellcome", "EU", "WPH", "LSHTM") ~ "Tier 1",
      Short_Name %in% c("UCL", "U Oslo", "KI", "SAMRC", "UCT", "CSRS", "AKU") ~ "Tier 2",
      TRUE ~ "Tier 3"
    ),
    # Simplified categories
    category = case_when(
      Policy == 1 ~ "Policy & Governance",
      Finance_programmes == 1 | Data_Providers == 1 ~ "Finance & Data",
      Research == 1 ~ "Research",
      TRUE ~ "Implementation"
    )
  )

# Define Tier 1 and Tier 2 partners to label
key_partners <- c("WHO", "Wellcome", "EU", "WPH", "LSHTM", "UCL", "U Oslo", 
                  "KI", "SAMRC", "UCT", "CSRS", "AKU", "U Toulouse", "CICERO",
                  "U Leeds", "AKHS", "CeSHHAR", "U Michigan", "UW", "HEAL")

label_data <- partners_data %>%
  filter(Short_Name %in% key_partners)

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
africa_europe <- world %>%
  filter(continent %in% c("Africa", "Europe"))

# ==========================================
# PROFESSIONAL MAP 1: Clean with visual hierarchy
# ==========================================
cat("Creating Professional Map 1: Visual hierarchy...\n")

p1 <- ggplot() +
  # Base map - very subtle
  geom_sf(data = africa_europe, 
          fill = "#fafafa", 
          color = "#e8e8e8", 
          linewidth = 0.2) +
  
  # Tier 3 partners (smallest, most transparent)
  geom_point(data = partners_data %>% filter(importance == "Tier 3"),
             aes(x = lon, y = lat),
             color = brand_colors[5], 
             size = 1.5, 
             alpha = 0.4) +
  
  # Tier 2 partners (medium)
  geom_point(data = partners_data %>% filter(importance == "Tier 2"),
             aes(x = lon, y = lat, color = category),
             size = 3.5, 
             alpha = 0.7) +
  
  # Tier 1 partners (largest, most prominent)
  geom_point(data = partners_data %>% filter(importance == "Tier 1"),
             aes(x = lon, y = lat, color = category),
             size = 5.5, 
             alpha = 0.9) +
  
  # Labels only for key partners
  geom_text_repel(data = label_data,
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3,
                  box.padding = 0.35,
                  point.padding = 0.3,
                  segment.color = brand_colors[5],
                  segment.size = 0.2,
                  segment.alpha = 0.5,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  # Color scale
  scale_color_manual(values = c(
    "Policy & Governance" = brand_colors[1],
    "Finance & Data" = brand_colors[3],
    "Research" = brand_colors[2],
    "Implementation" = brand_colors[6]
  ), name = "") +
  
  # Clean coordinate system
  coord_sf(xlim = c(-20, 45), ylim = c(-35, 65), expand = FALSE) +
  
  # Minimal theme
  labs(title = "HEAT Partnership Network",
       subtitle = "Key stakeholders across Africa and Europe") +
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

svglite("professional_map_1_hierarchy.svg", width = 12, height = 10)
print(p1)
dev.off()

# ==========================================
# PROFESSIONAL MAP 2: Clustered regions
# ==========================================
cat("Creating Professional Map 2: Regional clusters...\n")

# Define regional clusters
partners_data <- partners_data %>%
  mutate(
    region = case_when(
      Country %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique", "Namibia") ~ "Southern Africa",
      Country %in% c("Kenya", "Uganda", "Tanzania", "Rwanda", "Burundi", "Ethiopia") ~ "East Africa",
      Country %in% c("Côte d'Ivoire", "Burkina Faso", "Ghana", "Senegal", "Mali", "Togo", "Nigeria") ~ "West Africa",
      Country %in% c("Chad", "Cameroon", "Mauritania") ~ "Central Africa",
      Country %in% c("United Kingdom", "Ireland") ~ "UK & Ireland",
      Country %in% c("France", "Belgium", "Netherlands", "Germany", "Switzerland", "Austria") ~ "Western Europe",
      Country %in% c("Sweden", "Norway", "Denmark", "Finland", "Estonia") ~ "Northern Europe",
      Country %in% c("Italy", "Spain", "Greece") ~ "Southern Europe",
      TRUE ~ "Other"
    )
  )

# Calculate cluster centers
cluster_centers <- partners_data %>%
  group_by(region) %>%
  summarise(
    cluster_lon = mean(lon, na.rm = TRUE),
    cluster_lat = mean(lat, na.rm = TRUE),
    n_partners = n(),
    .groups = 'drop'
  ) %>%
  filter(region != "Other")

p2 <- ggplot() +
  # Base map
  geom_sf(data = africa_europe, 
          fill = "#f8f8f8", 
          color = "#d0d0d0", 
          linewidth = 0.2) +
  
  # Cluster bubbles
  geom_point(data = cluster_centers,
             aes(x = cluster_lon, y = cluster_lat, size = n_partners),
             color = brand_colors[6], 
             fill = brand_colors[6],
             alpha = 0.2) +
  
  # Individual partners
  geom_point(data = partners_data,
             aes(x = lon, y = lat, color = region),
             size = 2, 
             alpha = 0.8) +
  
  # Cluster labels
  geom_text(data = cluster_centers,
            aes(x = cluster_lon, y = cluster_lat, label = paste0(region, "\n(", n_partners, " partners)")),
            size = 3.5,
            color = brand_colors[4],
            fontface = "bold",
            nudge_y = 2) +
  
  # Size scale
  scale_size_continuous(range = c(10, 30), guide = "none") +
  
  # Regional colors
  scale_color_manual(values = c(
    "Southern Africa" = brand_colors[1],
    "East Africa" = brand_colors[2],
    "West Africa" = brand_colors[3],
    "Central Africa" = brand_colors[4],
    "UK & Ireland" = brand_colors[5],
    "Western Europe" = brand_colors[6],
    "Northern Europe" = brand_colors[1],
    "Southern Europe" = brand_colors[2],
    "Other" = "#999999"
  ), guide = "none") +
  
  coord_sf(xlim = c(-20, 45), ylim = c(-35, 65), expand = FALSE) +
  
  labs(title = "Regional Partnership Clusters",
       subtitle = "Geographic distribution of HEAT network") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = brand_colors[4], 
                              hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = brand_colors[4], 
                                  hjust = 0.5, margin = margin(b = 20)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

svglite("professional_map_2_clusters.svg", width = 12, height = 10)
print(p2)
dev.off()

# ==========================================
# PROFESSIONAL MAP 3: With inset for dense areas
# ==========================================
cat("Creating Professional Map 3: With inset map...\n")

# Main map
main_map <- ggplot() +
  geom_sf(data = africa_europe, 
          fill = "#fafafa", 
          color = "#e0e0e0", 
          linewidth = 0.2) +
  
  # Add rectangle showing inset area
  annotate("rect", xmin = 16, xmax = 32, ymin = -35, ymax = -22,
           fill = NA, color = brand_colors[1], linewidth = 1, linetype = "dashed") +
  
  # All partners
  geom_point(data = partners_data,
             aes(x = lon, y = lat, color = category),
             size = 2.5, 
             alpha = 0.7) +
  
  # Selected labels
  geom_text_repel(data = label_data %>% filter(!Country %in% c("South Africa")),
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 2.5,
                  box.padding = 0.2,
                  point.padding = 0.1,
                  segment.color = brand_colors[5],
                  segment.size = 0.2,
                  segment.alpha = 0.5,
                  max.overlaps = 15,
                  color = brand_colors[4]) +
  
  scale_color_manual(values = c(
    "Policy & Governance" = brand_colors[1],
    "Finance & Data" = brand_colors[3],
    "Research" = brand_colors[2],
    "Implementation" = brand_colors[6]
  ), name = "") +
  
  coord_sf(xlim = c(-20, 45), ylim = c(-35, 65), expand = FALSE) +
  
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = NA)
  )

# Inset map for Southern Africa
sa_partners <- partners_data %>%
  filter(Country %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique"))

inset_map <- ggplot() +
  geom_sf(data = world %>% filter(name %in% c("South Africa", "Zimbabwe", "Botswana", "Mozambique")),
          fill = "#f0f0f0", 
          color = "#c0c0c0", 
          linewidth = 0.3) +
  
  geom_point(data = sa_partners,
             aes(x = lon, y = lat, color = category),
             size = 4, 
             alpha = 0.9) +
  
  geom_text_repel(data = sa_partners %>% filter(Short_Name %in% key_partners),
                  aes(x = lon, y = lat, label = Short_Name),
                  size = 3,
                  box.padding = 0.3,
                  point.padding = 0.2,
                  segment.color = brand_colors[5],
                  segment.size = 0.2,
                  max.overlaps = 20,
                  color = brand_colors[4],
                  fontface = "bold") +
  
  scale_color_manual(values = c(
    "Policy & Governance" = brand_colors[1],
    "Finance & Data" = brand_colors[3],
    "Research" = brand_colors[2],
    "Implementation" = brand_colors[6]
  ), guide = "none") +
  
  coord_sf(xlim = c(16, 32), ylim = c(-35, -22), expand = FALSE) +
  
  labs(title = "Southern Africa Detail") +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold", color = brand_colors[4], hjust = 0.5),
    plot.background = element_rect(fill = "white", color = brand_colors[1], linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine with inset
combined <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.02, y = 0.02, width = 0.35, height = 0.35) +
  draw_label("HEAT Partnership Network", 
             x = 0.5, y = 0.95, 
             size = 20, 
             fontface = "bold", 
             color = brand_colors[4]) +
  draw_label("Strategic partners across Africa and Europe", 
             x = 0.5, y = 0.91, 
             size = 14, 
             color = brand_colors[4])

svglite("professional_map_3_inset.svg", width = 12, height = 10)
print(combined)
dev.off()

# ==========================================
# PROFESSIONAL MAP 4: Simplified for maximum clarity
# ==========================================
cat("Creating Professional Map 4: Maximum clarity...\n")

# Select only the most critical partners for cleanest visualization
critical_partners <- c("WHO", "Wellcome", "EU", "WPH", "LSHTM", "UCL", 
                       "SAMRC", "UCT", "CSRS", "AKU", "U Oslo", "KI")

critical_data <- partners_data %>%
  filter(Short_Name %in% critical_partners)

p4 <- ggplot() +
  # Very subtle base map
  geom_sf(data = africa_europe, 
          fill = "white", 
          color = "#f0f0f0", 
          linewidth = 0.1) +
  
  # Connection lines between partners
  geom_segment(data = expand.grid(
    from = critical_partners[1:3],
    to = critical_partners[4:12]
  ) %>%
    left_join(critical_data %>% select(Short_Name, from_lon = lon, from_lat = lat), 
              by = c("from" = "Short_Name")) %>%
    left_join(critical_data %>% select(Short_Name, to_lon = lon, to_lat = lat), 
              by = c("to" = "Short_Name")) %>%
    filter(!is.na(to_lon)),
    aes(x = from_lon, y = from_lat, xend = to_lon, yend = to_lat),
    alpha = 0.05, 
    color = brand_colors[6], 
    linewidth = 0.5) +
  
  # Partner points
  geom_point(data = critical_data,
             aes(x = lon, y = lat, color = category),
             size = 8, 
             alpha = 0.9) +
  
  # Clean labels
  geom_text(data = critical_data,
            aes(x = lon, y = lat, label = Short_Name),
            size = 4,
            color = "white",
            fontface = "bold") +
  
  scale_color_manual(values = c(
    "Policy & Governance" = brand_colors[1],
    "Finance & Data" = brand_colors[3],
    "Research" = brand_colors[2],
    "Implementation" = brand_colors[6]
  ), name = "Partner Type") +
  
  coord_sf(xlim = c(-20, 45), ylim = c(-35, 65), expand = FALSE) +
  
  labs(title = "Core HEAT Partnership Network",
       subtitle = "Key strategic partners") +
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

svglite("professional_map_4_simplified.svg", width = 12, height = 10)
print(p4)
dev.off()

cat("\nAll professional maps created successfully!\n")
cat("Files created:\n")
cat("- professional_map_1_hierarchy.svg (Visual hierarchy)\n")
cat("- professional_map_2_clusters.svg (Regional clusters)\n")
cat("- professional_map_3_inset.svg (With Southern Africa inset)\n")
cat("- professional_map_4_simplified.svg (Core partners only)\n")
cat("\nAll maps follow Wellcome Trust grant standards with:\n")
cat("- No latitude/longitude grid lines\n")
cat("- Brand color palette\n")
cat("- Clean, professional appearance\n")
cat("- Visual hierarchy for partner importance\n")