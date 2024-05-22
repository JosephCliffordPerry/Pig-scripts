
# Define the regular expression patterns
subfertile_pattern <- "subfertile"
fertile_pattern <- "fertile"


# Add a new column based on the counts
pig_data$Reported_fertility <- ifelse(grepl(subfertile_pattern, pig_data$Folder, ignore.case = TRUE), "subfertile",
                                        ifelse(grepl(fertile_pattern, pig_data$Folder, ignore.case = TRUE), "fertile", NA))

#Filter breed dataset


Breed_data <- pig_data %>%
  mutate(Breed = case_when(
    Reported_fertility == "fertile" & grepl(" sample 6 ", Folder, ignore.case = TRUE) ~ "Hampshire",
    Reported_fertility == "fertile" & grepl(" sample 2 ", Folder, ignore.case = TRUE) ~ "Landrace",
    Reported_fertility == "fertile" & grepl(" sample 5 ", Folder, ignore.case = TRUE) ~ "Large White",
    Reported_fertility == "fertile" & grepl(" sample 3 ", Folder, ignore.case = TRUE) ~ "Pietrain",
    Reported_fertility == "fertile" & grepl(" sample 9 ", Folder, ignore.case = TRUE) ~ "White Duroc",
    TRUE ~"Other"
  ))

# make diameter umap

diameter_data <- pig_data %>% dplyr::select(starts_with("Diameter_profile_"))
set.seed(08000)
diameterumap <- umap(diameter_data, preserve.seed = TRUE)
diameterumap_clusters<- as.data.frame(cbind(diameterumap[["layout"]],as.factor(Breed_data$Breed)))
diameterumap_clusters$V3 <- factor(diameterumap_clusters$V3)
ggplot(data = diameterumap_clusters, aes(V1, V2, color = V3)) +
  geom_point(size = 0.8)+
  labs(title = "UMAP on diameter profile against Breed", x = "UMAP1", y = "UMAP2", color = "Breeds")+
  scale_color_discrete(labels = c("Hampshire", "Landrace ", "Large White ","Other","Pietrain","White Duroc"))


ggsave(filename = "figures/Breed_diameter_Umap.png",width = 180,height = 180,units = "mm",dpi = 300)


################
#split dataset by breed
Hampshire<-subset(Breed_data, Breed == "Hampshire")
Landrace<-subset(Breed_data, Breed == "Landrace")
Large_white<-subset(Breed_data, Breed == "Large White")
Other<-subset(Breed_data, Breed == "Other")
Pietrain<-subset(Breed_data, Breed == "Pietrain")
white_duroc<-subset(Breed_data, Breed == "White Duroc")

# Split each dataset into diameter, radius, and angle profiles
split_profiles <- function(data) {
  diameter_data <- data %>% select(starts_with("Diameter_profile_"))
  radius_data <- data %>% select(starts_with("Radius_profile_"))
  angle_data <- data %>% select(starts_with("Angle_profile_"))

  return(list(diameter = diameter_data, radius = radius_data, angle = angle_data))
}


# Apply split_profiles function to each dataset
Hampshire_profiles <- split_profiles(Hampshire)
Landrace_profiles <- split_profiles(Landrace)
Large_white_profiles <- split_profiles(Large_white)
Other_profiles <- split_profiles(Other)
Pietrain_profiles <- split_profiles(Pietrain)
White_Duroc_profiles <- split_profiles(white_duroc)

# List of profile types
profile_types <- list(
  Hampshire = Hampshire_profiles,
  Landrace = Landrace_profiles,
  Large_white = Large_white_profiles,
  Other = Other_profiles,
  Pietrain = Pietrain_profiles,
  White_Duroc = White_Duroc_profiles
)
# Initialize empty matrices to store p-values for each profile type
p_value_matrix_diameter <- matrix(nrow = length(profile_types), ncol = length(profile_types))
p_value_matrix_radius <- matrix(nrow = length(profile_types), ncol = length(profile_types))
p_value_matrix_angle <- matrix(nrow = length(profile_types), ncol = length(profile_types))

# Perform Wilcoxon rank sum tests for every combination of profile types
for (i in seq_along(profile_types)) {
  for (j in seq_along(profile_types)) {
    # Extract profile data for each pair
    profile1_diameter <- colMeans(profile_types[[i]]$diameter)
    profile2_diameter <- colMeans(profile_types[[j]]$diameter)

    profile1_radius <- colMeans(profile_types[[i]]$radius)
    profile2_radius <- colMeans(profile_types[[j]]$radius)

    profile1_angle <- colMeans(profile_types[[i]]$angle)
    profile2_angle <- colMeans(profile_types[[j]]$angle)

    # Perform Wilcoxon rank sum tests and store p-values in the matrices
   diameter_test_result <- wilcox.test(profile1_diameter, profile2_diameter)
   radius_test_result <- wilcox.test(profile1_radius, profile2_radius)
   angle_test_result <- wilcox.test(profile1_angle, profile2_angle)

    p_value_matrix_diameter[i, j] <-diameter_test_result$p.value
    p_value_matrix_radius[i, j] <-radius_test_result$p.value
    p_value_matrix_angle[i, j] <-angle_test_result$p.value
  }
}

# Apply multiple testing correction
p_adjusted_matrix_diameter <- p.adjust(p_value_matrix_diameter, method = "holm")
p_adjusted_matrix_radius <- p.adjust(p_value_matrix_radius, method = "holm")
p_adjusted_matrix_angle <- p.adjust(p_value_matrix_angle, method = "holm")

#The matrices
p_adjusted_matrix_diameter
p_adjusted_matrix_radius
p_adjusted_matrix_angle


###############

