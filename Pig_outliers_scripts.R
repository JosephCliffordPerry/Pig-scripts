# detection of pig outliers
# Use as a new distinct set of cluster identities
#####################################################
get_outlier_features <- function(profile_data) {
  boolean_matrix <- matrix(FALSE, nrow = nrow(profile_data), ncol = ncol(profile_data))
  for (columnumber in 1:ncol(profile_data)) {
    vec <- as.vector(unlist(profile_data[, columnumber]))
    Q1 <- quantile(vec, 0.25)
    Q3 <- quantile(vec, 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- vec < lower_bound | vec > upper_bound
    boolean_matrix[, columnumber] <- outliers
  }
  return(boolean_matrix)
}
#############################################################
make_outlier_cluster <- function(profile_data, profile_type) {
  boolean_matrix <- get_outlier_features(profile_data)
  #true boolean double
  tboolean <- which(boolean_matrix, arr.ind = TRUE)
 # make table of true values
  booleancount <- table(tboolean[, 1])
  #cell ids
  outliercluster <- cbind(pig_data$CellID, 1)
  #try and take a reasonable portion
  outlier_rows <- names( which((booleancount > mean(booleancount))))
  #format dataframe
  outliercluster[as.numeric(outlier_rows), 2] <- 2
  outliercluster <- as.data.frame(outliercluster)
  names(outliercluster)[names(outliercluster) == "V1"] <- paste0(profile_type, " ", "outliers")
  names(outliercluster)[names(outliercluster) == "V2"] <- "Clustering_file"
  return(list(outliercluster))
}
angle_data <- pig_data %>% dplyr::select(starts_with("Angle_profile_"))
diameter_data <- pig_data %>% dplyr::select(starts_with("Diameter_profile_"))
radius_data <- pig_data %>% dplyr::select(starts_with("Radius_profile_"))
angle_outliers <- make_outlier_cluster(angle_data, "angle")
diameter_outliers <- make_outlier_cluster(diameter_data, "diameter")
radius_outliers <- make_outlier_cluster(radius_data, "radius")
outlier_ids<-paste0(angle_outliers[[1]][["Clustering_file"]],diameter_outliers[[1]][["Clustering_file"]],radius_outliers[[1]][["Clustering_file"]])
#makes clustering files
Outliers2<-cbind(pig_data$CellID,as.integer(outlier_ids))
#adds proper cluster levels
unique_values <- unique(Outliers2[,2])
new_values <- match(Outliers2[,2], unique_values)

 # Replace the values in the column with the new values
   Outliers2[,2] <- new_values
write.table(Outliers2, file = "data/Pig_outliers4.txt", sep = "\t", quote = FALSE, row.names = FALSE)

