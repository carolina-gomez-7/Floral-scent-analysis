# Install and load required packages
install.packages("paletteer")
install.packages("showtext")
install.packages("sysfonts")


library(tidyverse)
library(pheatmap)
library(viridis)
library(showtext)
library(sysfonts)

# Load dataset
data <- read.csv("filtered_data_feb19_2026.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

plant_species_order <- c(
  "Ranunculus acris","Tilia sp2","Epilobium angustifolium","Veronica spicata",
  "Rhinanthus minor","Nepeta racemosa","Lavandula angustifolia","Stachys palustris",
  "Ballota nigra","Buddleja davidii","Ligustrum vulgare","Symphoricarpos albus",
  "Hydrangea paniculata","Heracleum sphondylium","Tripleurospermum inodorum",
  "Jacobaea vulgaris","Centaurea nigra","Cirsium vulgare","Cirsium arvense",
  "Arctium minus","Rubus fruticosus agg.","Rubus caesius","Trifolium repens",
  "Lotus corniculatus","Hypericum perforatum","Geranium pratense","Impatiens glandulifera",
  "Cornus sanguinea","Calystegia silvatica"
)

# Force factor levels (THIS is what controls the plotting order)
data$Plant_species <- factor(data$Plant_species, levels = plant_species_order)
data$Compound <- as.factor(data$Compound)
data$Class <- as.factor(data$Class)

# Font (Mac-specific path; keep if it works for you)
font_add("Helvetica", regular = "/System/Library/Fonts/Supplemental/Helvetica.ttc")
showtext_auto()

plot_heatmap <- function(class_filter, title, filename) {
  
  heatmap_data <- data %>%
    filter(Class %in% class_filter) %>%
    group_by(Plant_species, Compound) %>%
    summarize(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Compound, values_from = mean_abundance, values_fill = 0) %>%
    arrange(Plant_species)   # ✅ enforce row order
  
  # Convert to matrix
  heatmap_matrix <- as.matrix(heatmap_data[,-1])
  rownames(heatmap_matrix) <- as.character(heatmap_data$Plant_species)
  
  # ✅ OPTIONAL: drop any NA rows (species not in your order list)
  heatmap_matrix <- heatmap_matrix[!is.na(rownames(heatmap_matrix)), , drop = FALSE]
  
  # Breaks
  non_zero_values <- heatmap_matrix[heatmap_matrix > 0 & is.finite(heatmap_matrix)]
  if (length(non_zero_values) > 0) {
    breaks <- c(
      0,
      seq(from = min(non_zero_values, na.rm = TRUE),
          to = max(heatmap_matrix, na.rm = TRUE),
          length.out = 99)
    )
  } else {
    warning("Heatmap matrix has no non-zero values; using default breaks.")
    breaks <- c(0, seq(0.01, 1, length.out = 99))
  }
  
  # Colors
  zero_color <- "#fffafe"
  custom_colors_final <- c(zero_color, plasma(99))
  
  # Save PDF
  pdf(filename, width = 11.69, height = 8.27, family = "Helvetica")
  showtext_begin()
  
  pheatmap(
    heatmap_matrix,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    main = title,
    fontsize_row = 9,
    fontsize_col = 9,
    angle_col = 90,
    color = custom_colors_final,
    breaks = breaks,
    labels_row = parse(text = paste0("italic('", rownames(heatmap_matrix), "')"))
  )
  
  showtext_end()
  dev.off()
}

# Generate heatmaps
plot_heatmap("Monoterpenoid", "Monoterpenoid Compounds per Plant Species", "Monoterpenoid_Heatmap_feb19_2026_2.pdf")
plot_heatmap("Sesquiterpenoid", "Sesquiterpenoid Compounds per Plant Species", "Sesquiterpenoid_Heatmap_feb19_2026.pdf")
plot_heatmap(c("Benzenoid", "Aldehyde", "Alcohol", "Ester", "Ketone", "Other"),
             "Other Volatile Compounds per Plant Species", "Other_Compounds_Heatmap_feb19_2026.pdf")

