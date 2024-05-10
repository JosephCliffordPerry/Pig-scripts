#Simple Filtratrion based on derived morphometric values From previous
#clusterings to give intuitive and clear groupings of the morphology
#Via the usage of NMA to manually determine and apply morphometric filter and apply

#Manual feature based filtering function
# Function to filter out rows with specified column value
filter_and_extract <- function(data, column_name, value) {
  # Filter rows where specified column equals the specified value
  smaller_rows <- data[data[[column_name]] < value, ]

  # Extract rows where specified column equals the specified value
  bigger_rows <- data[data[[column_name]] >= value, ]

  # Return filtered dataset and extracted rows
  return(list(bigger_rows = bigger_rows, smaller_rows = smaller_rows))
}
#param1
#Angles at index 0 < 160
#This has a cluster boundary distance of 20
# Assuming 'data' is your dataframe
param1filtered_and_extracted <- filter_and_extract(pig_data, "Angle_profile_0", 160)

# Extracted rows
param1extracted_rows <- param1filtered_and_extracted$smaller_rows
# Filtered dataset
param1filtered_dataset <- param1filtered_and_extracted$bigger_rows
#param2
#angles at index 8 > 144.5
#This has a cluster boundary distance of 0.5
param2filtered_and_extracted <- filter_and_extract(param1filtered_dataset, "Angle_profile_8", 144.5)

# Extracted rows
param2extracted_rows <- param2filtered_and_extracted$bigger_rows
# Filtered dataset
param2filtered_dataset <- param2filtered_and_extracted$smaller_rows


#param3
#radii at index 44 < 57
#This has a cluster boundary distance of 1
param3filtered_and_extracted <- filter_and_extract(param2filtered_dataset, "Radius_profile_44", 57)

# Extracted rows
param3extracted_rows <- param3filtered_and_extracted$smaller_rows
# Filtered dataset
param3filtered_dataset <- param3filtered_and_extracted$bigger_rows

#make easy test file
param3<-cbind(param3extracted_rows$CellID,4)
param1<-cbind(param1extracted_rows$CellID,3)
param2<-cbind(param2extracted_rows$CellID,2)
remaning<-cbind(param3filtered_dataset$CellID,1)
Manual_clusters<-rbind(param1,param2,param3,remaning)


write.table(Manual_clusters, file = "filter2.txt", sep = "\t", quote = FALSE, row.names = FALSE)

##########################
# Manual filter for cells with posterior bulge

param1filtered_and_extracted<- filter_and_extract(pig_data, "Angle_profile_6", 140)

# Extracted rows
param1extracted_rows <- param1filtered_and_extracted$smaller_rows
# Filtered dataset
param1filtered_dataset <- param1filtered_and_extracted$bigger_rows
#
param2filtered_and_extracted <- filter_and_extract(param1filtered_dataset, "Angle_profile_5", 140)

# Extracted rows
param2extracted_rows <- param2filtered_and_extracted$bigger_rows
# Filtered dataset
param2filtered_dataset <- param2filtered_and_extracted$smaller_rows

param3filtered_and_extracted <- filter_and_extract(param1filtered_dataset, "Angle_profile_4", 140)

# Extracted rows
param3extracted_rows <- param2filtered_and_extracted$bigger_rows
# Filtered dataset
param3filtered_dataset <- param2filtered_and_extracted$smaller_rows


param3<-cbind(param3extracted_rows$CellID,4)
param1<-cbind(param1extracted_rows$CellID,3)
param2<-cbind(param2extracted_rows$CellID,2)
remaning<-cbind(param3filtered_dataset$CellID,1)
Manual_clusters<-rbind(param1,param2,param3,remaning)


write.table(Manual_clusters, file = "posterior_bulge.txt", sep = "\t", quote = FALSE, row.names = FALSE)
