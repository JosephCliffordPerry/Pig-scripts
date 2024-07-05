
pig.data <- pig_data %>%
  tidyr::separate_wider_delim(Centre_of_mass, " - ", names = c("CoMX", "CoMY")) %>%
  dplyr::mutate(
    Type = stringr::str_extract(Folder, "(sub)?fertile"),
    Type = ifelse(Type == "fertile", "Normal", "Abnormal"),
    CoMX = as.numeric(CoMX), CoMY = as.numeric(CoMY),
    TypeInt = as.factor(ifelse(Type == "Normal", 1, 0)))

pig_standard_measuremnts<-as.data.frame(cbind(Folder =pig.data$Folder,
                                              cellID = pig.data$CellID,
                                              perimeter_pixels = pig.data$Perimeter_pixels,
                                              Regularity = pig.data$Regularity,
                                              Bounding_width_pixels = pig.data$Bounding_width_pixels,
                                              Bounding_height_pixels = pig.data$Bounding_height_pixels,
                                              Area_square_pixels = pig.data$Area_square_pixels,
                                              Elongation = pig.data$Elongation,
                                              Ellipticity = pig.data$Ellipticity,
                                              Circularity = pig.data$Circularity))


pig_standard_measuremnts$Sampleid<-basename(pig_standard_measuremnts$Folder)

pig_standard_measuremnts <- pig_standard_measuremnts %>%
  mutate(
    Breed = case_when(
      grepl(" sample 6 ", Sampleid, ignore.case = TRUE) ~ "Hampshire",
      grepl(" sample 2 ", Sampleid, ignore.case = TRUE) ~ "Landrace",
      grepl(" sample 5 ", Sampleid, ignore.case = TRUE) ~ "Large White",
      grepl(" sample 3 ", Sampleid, ignore.case = TRUE) ~ "Pietrain",
      grepl(" sample 9 ", Sampleid, ignore.case = TRUE) ~ "White Duroc",
      TRUE ~ "Other"))
# Filter out rows where Breed is "Other"
filtered_data <- pig_standard_measuremnts %>%
  filter(Breed != "Other")


ggplot(filtered_data, aes(x = Breed, y = as.numeric(perimeter_pixels))) +
  geom_violin() +
  theme_bw() +
  labs(
    x = "Breed",
    y = "Value"
  )



# Reshape the data into long format
long_df <- filtered_data %>%
  pivot_longer(
    cols = c(perimeter_pixels, Regularity, Bounding_width_pixels, Bounding_height_pixels, Area_square_pixels, Elongation, Ellipticity, Circularity),
    names_to = "Measurement",
    values_to = "Value"
  )

long_df$Value<-as.numeric(long_df$Value)

# Create the violin plot faceted by Breed
ggplot(long_df, aes(x = Breed, y = Value, fill = Breed)) +
  geom_violin() +
  facet_wrap(~ Measurement, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Violin Plots of Various Measurements by Breed",
    x = element_blank(),
    y = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")



# Assuming you have a conversion factor from pixels to microns
# Replace the conversion_factor with your actual value
conversion_factor <- 1/15.33 # Example conversion factor (pixels to microns)

# Convert pixel measurements to microns
pig_standard_measuremnts <- as.data.frame(cbind(
  Folder = pig.data$Folder,
  cellID = pig.data$CellID,
  perimeter_microns = pig.data$Perimeter_pixels * conversion_factor,
  Regularity = pig.data$Regularity,
  Bounding_width_microns = pig.data$Bounding_width_pixels * conversion_factor,
  Bounding_height_microns = pig.data$Bounding_height_pixels * conversion_factor,
  Area_square_microns = pig.data$Area_square_pixels * (conversion_factor ^ 2),
  Elongation = pig.data$Elongation,
  Ellipticity = pig.data$Ellipticity,
  Circularity = pig.data$Circularity
))

# Add Sampleid and Breed columns
pig_standard_measuremnts$Sampleid <- basename(pig_standard_measuremnts$Folder)

# Assign breed based on Sampleid
pig_standard_measuremnts <- pig_standard_measuremnts %>%
  mutate(
    Breed = case_when(
      grepl(" sample 6 ", Sampleid, ignore.case = TRUE) ~ "Hampshire",
      grepl(" sample 2 ", Sampleid, ignore.case = TRUE) ~ "Landrace",
      grepl(" sample 5 ", Sampleid, ignore.case = TRUE) ~ "Large White",
      grepl(" sample 3 ", Sampleid, ignore.case = TRUE) ~ "Pietrain",
      grepl(" sample 9 ", Sampleid, ignore.case = TRUE) ~ "White Duroc",
      TRUE ~ "Other"
    )
  )

# Filter out rows where Breed is "Other"
filtered_data <- pig_standard_measuremnts %>%
  filter(Breed != "Other")

# Reshape the data into long format for ggplot
long_df <- filtered_data %>%
  pivot_longer(
    cols = c(perimeter_microns, Regularity, Bounding_width_microns, Bounding_height_microns, Area_square_microns, Elongation, Ellipticity, Circularity),
    names_to = "Measurement",
    values_to = "Value"
  )



long_df$Value<-as.numeric(long_df$Value)

long_df <- long_df %>%
  group_by(Measurement) %>%
  mutate(Median_Value = median(Value, na.rm = TRUE))

custom_labels <- c(
  "Area_square_microns" = "Area (µm^2)",
  "Bounding_height_microns" = "Bounding height (µm)",
  "Bounding_width_microns" = "Bounding width (µm)",
  "Circularity" = "Circularity",
  "Ellipticity" = "Ellipticity",
  "Elongation" = "Elongation",
  "perimeter_microns" = "Perimeter (µm)",
  "Regularity" = "Regularity"
  # Replace "Measurement1", "Measurement2", etc. with actual values in your data
)
# Create the violin plot with boxplot overlays and global median line
violin_plot <- ggplot(long_df, aes(x = Breed, y = Value)) +
  geom_hline(aes(yintercept = Median_Value), linetype = "solid", color = "red", linewidth = 0.5) +
  geom_violin(fill = "grey80", color = "black", width = 0.8) +
  geom_boxplot(width = 0.2, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  facet_wrap(~ Measurement, scales = "free_y", labeller = as_labeller(custom_labels)) +  # Clean up facet labels
  theme_bw() +
  labs(
    title = element_blank(),
    x = element_blank(),
    y = element_blank()
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    strip.text = element_text(size = 12, face = "bold")  # Adjust facet label size and style
  )

# Print the plot
print(violin_plot)



ggsave(filename = "figures/Breed_stats_suppliment.png", width = 170, height = 170, units = "mm", dpi = 300)
