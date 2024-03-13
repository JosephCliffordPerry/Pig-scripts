#orientator

#cluster
library(dplyr)
library(factoextra)
library(stringr)
diameter_data <- pig_data %>% dplyr::select(starts_with("Diameter_profile_"))
#
# clusters <- hkmeans(diameter_data, k = 2)
# save(clusters,file = "Pig_diameter_clusters.rda")
load("Pig_diameter_clusters.rda")
# save cluster to check it correctly split

  cellid <- pig_data$CellID
  clust <- clusters$cluster
  export <- data.frame(CellID = cellid, Cluster = clust)

write.table(export, file = "Pig_diam.txt", sep = "\t", quote = FALSE, row.names = FALSE)
##############################
#making a consensus for later comparison
MakeConsensusdf <- function(outlinedata) {
  Outliney <- t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y")))
  Outlinex <- t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_X")))
  meanOutlineY <- rowMeans(Outliney)
  meanOutlineX <- rowMeans(Outlinex)
  polygon_df <- data.frame(Column1 = meanOutlineY, Column2 = meanOutlineX)
  return(polygon_df)
}

make_cluster_consensus <- function(cluster, outlinedata) {
  outline_clusters <- cbind(outlinedata, cluster$cluster)


title <- paste(names(cluster[1:(length(cluster) - 1)]), collapse = " ")

words <- str_extract_all(title, "\\b\\w+\\b")[[1]]
# Extract words and numbers
extracted2 <- gsub("_\\d+", "", words[1])
words2 <- unique(gsub("_", " ", extracted2))

numbers <- gsub("[^0-9]+", " ", title)

# Split the numbers by space
number_parts <- as.numeric(unlist(strsplit(numbers, " ")))

# Determine the range of numbers
number_range <- paste(min(number_parts, na.rm = TRUE), max(number_parts, na.rm = TRUE), sep = ":")
if (any(grepl("\\d+:\\d+", number_range))) {
  # Create new title
  title <- paste0(words2, sep = " ", number_range, recycle0 = TRUE)

    path_data_xlist <- list()
    path_data_ylist <- list()
    for (i in 2:length(number_parts)) {
      # Construct the column names to select
      selected_Xcolumns <- paste0("Outline_OrientedCoordinates_X_", 0:99)
      selected_ycolumns <- paste0("Outline_OrientedCoordinates_Y_", 0:99)

      # Filter the columns based on the constructed names
      path_datax <- outlinedata[selected_Xcolumns]
      path_datay <- outlinedata[selected_ycolumns]

      path_data_xlist[[i]] <- path_datax
      path_data_ylist[[i]] <- path_datay
    }
    path_dataxdf <- as.data.frame(path_data_xlist[2:length(number_parts)])
    path_dataydf <- as.data.frame(path_data_ylist[2:length(number_parts)])
    path_datadf <- cbind(path_dataxdf, path_dataydf)
    path_clusters <- cbind(path_datadf, cluster$cluster)
    a <- list()
    a2 <- list()
    # Calculate the number of rows in each facet
    facet_counts <- as.data.frame(table(cluster$cluster))
    facet_count_vector <- facet_counts[[2]]
    for (j in 1:max(outline_clusters$`cluster$cluster`)) {
      A1 <- outline_clusters %>% filter(`cluster$cluster` == j)
      A2 <- path_clusters %>% filter(`cluster$cluster` == j)
      consensus_df <- MakeConsensusdf(A1)
      path_consensus_df <- MakeConsensusdf(A2)
      # Add a facet column to the consensus_df and pathdf
      consensus_df$facet <- j
      path_consensus_df$facet <- j
      # Add facet count column
      consensus_df$facet_count <- facet_count_vector[j]

      a[[j]] <- consensus_df
      a2[[j]] <- path_consensus_df
    }



    # Combine the individual consensus dataframes into one faceted dataframe
    faceted_df <- dplyr::bind_rows(a)
    faceted_path_df <- dplyr::bind_rows(a2)

    # Determine how many times you need to repeat faceted_path_df
    repeat_factor <- ceiling(nrow(faceted_df) / nrow(faceted_path_df))

    # Repeat faceted_path_df using the repeat function from base R
    faceted_path_df <- faceted_path_df[rep(seq_len(nrow(faceted_path_df)), each = repeat_factor), ]
    # make sure repeats don't overshoot
    faceted_path_df <- faceted_path_df[1:nrow(faceted_df), ]

    colourfactor <- factor(faceted_df$facet)
    consensus <- ggplot(faceted_df, aes(Column2, Column1, fill = colourfactor)) +
      geom_polygon() +
      geom_text(aes(x = Inf, y = Inf, label = facet_count), vjust = 1.5, hjust = 1.5) +
      #geom_path(data = faceted_path_df, aes(x = faceted_path_df$Column2, y = faceted_path_df$Column1), linewidth = 1.5) +
      facet_wrap(faceted_df$facet) +
      theme_minimal()# +
      #coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
consensus
} #else {
#     title <- paste0(words2, sep = " ", recycle0 = TRUE)
#
#     a <- list()
#
#     # Calculate the number of rows in each facet
#     facet_counts <- as.data.frame(table(cluster$cluster))
#     facet_count_vector <- facet_counts[[2]]
#     for (j in 1:max(outline_clusters$`cluster$cluster`)) {
#       A1 <- outline_clusters %>% filter(`cluster$cluster` == j)
#
#       consensus_df <- MakeConsensusdf(A1)
#
#       # Add a facet column to the consensus_df
#       consensus_df$facet <- j
#
#       # Add facet count column
#       consensus_df$facet_count <- facet_count_vector[j]
#
#       a[[j]] <- consensus_df
#     }
#
#
#
#     # Combine the individual consensus dataframes into one faceted dataframe
#     faceted_df <- dplyr::bind_rows(a)
#
#     consensus <- ggplot(faceted_df, aes(Column2, Column1, fill = colourfactor)) +
#       geom_polygon() +
#       geom_text(aes(x = Inf, y = Inf, label = facet_count), vjust = 1.5, hjust = 1.5) +
#       facet_wrap(faceted_df$facet) +
#       theme_minimal() +
#       coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
#   }
#
#   return(consensus)
# }

########
#get outlines
outlinedata <- pig_data %>% dplyr::select(starts_with("Outline_Oriented"))
#use NMA companion consensus function
graph1 <- make_cluster_consensus(cluster = clusters, outlinedata = outlinedata)


############
#reorientating via inversion of the X of group2

Pigclus<-cbind(pig_data,cluster$cluster)
#group 2 selected
Long_on_right<- Pigclus[Pigclus$`cluster$cluster` == 2, ]

Long_on_right_inverted_outlines <- Long_on_right %>%
  mutate(across(starts_with("Outline_OrientedCoordinates_X"), ~ . - 2 * .))
#reordering outlines
new_order <- seq(99, 0, by = -1)


# Generate the new column names for Angle, Diameter, and Radius profiles
new_column_names_angle <- paste0("Angle_profile_", new_order)
new_column_names_diameter <- paste0("Diameter_profile_", new_order)
new_column_names_radius <- paste0("Radius_profile_", new_order)
# Generate the new column names for X-coordinates
new_column_names_x <- paste0("Outline_OrientedCoordinates_X_", new_order)

# Generate the new column names for Y-coordinates
#new_column_names_y <- paste0("Outline_OrientedCoordinates_y_", new_order)
# Rename Angle, Diameter, and Radius profile columns
Long_on_right_inverted_outlines_swapped <- Long_on_right_inverted_outlines %>%
 # mutate(across(starts_with("Outline_OrientedCoordinates_X"), ~ . - 2 * .)) %>%
  rename_with(~ new_column_names_x, starts_with("Outline_OrientedCoordinates_X")) %>%
  #rename_with(~ new_column_names_y, starts_with("Outline_OrientedCoordinates_y")) %>%
  rename_with(~ new_column_names_angle, starts_with("Angle_profile_")) %>%
  rename_with(~ new_column_names_diameter, starts_with("Diameter_profile_")) %>%
  rename_with(~ new_column_names_radius, starts_with("Radius_profile_"))


Long_on_left<- Pigclus[Pigclus$`cluster$cluster` == 1, ]
reoriented_outline_pig_data<-rbind(Long_on_right_inverted_outlines_swapped,Long_on_left)
reoriented_outlinedata <-reoriented_outline_pig_data%>% dplyr::select(starts_with("Outline_Oriented"))
graph2 <- make_cluster_consensus(cluster = clusters, outlinedata = reoriented_outlinedata)
graph2
