#size by date and size by breed
Breed_data <- pig_data %>%
  mutate(Breed = case_when(
     grepl(" sample 6 ", Folder, ignore.case = TRUE) ~ "Hampshire",
     grepl(" sample 2 ", Folder, ignore.case = TRUE) ~ "Landrace",
    grepl(" sample 5 ", Folder, ignore.case = TRUE) ~ "Large White",
   grepl(" sample 3 ", Folder, ignore.case = TRUE) ~ "Pietrain",
     grepl(" sample 9 ", Folder, ignore.case = TRUE) ~ "White Duroc",
    TRUE ~"Other"
  ))
data <- separate(Breed_data,Folder, into = c("date", "sample_number"), sep = " sample ")# Remove all words after the sample number
#don't separate fertile and subfertile
data$sample_number<- gsub("\\s+.*$", "", data$sample_number)
data$date<-basename(data$date)
Size_date_dataset<-as.data.frame(cbind(data$Area_square_pixels,data$date,data$sample_number,data$CellID))

mean_size_per_date <- aggregate(as.numeric(V1) ~ V2, data = Size_date_dataset, FUN = mean)
mean_size_per_sample <- aggregate(as.numeric(V1) ~ V3, data = Size_date_dataset, FUN = mean)
wilcox.test(mean_size_per_date$`as.numeric(V1)`,mean_size_per_sample$`as.numeric(V1)`)



#realised I can just do this with NMA
#samples
cellid <- Size_date_dataset$V4
clust <- Size_date_dataset$V3
export <- data.frame(CellID = cellid, Cluster = clust)

write.table(export, file = "Pig_sample.txt", sep = "\t", quote = FALSE, row.names = FALSE)
#dates
cellid <- Size_date_dataset$V4
clust <- as.integer(as.factor(Size_date_dataset$V2))
export <- data.frame(CellID = cellid, Cluster = clust)

write.table(export, file = "Pig_date.txt", sep = "\t", quote = FALSE, row.names = FALSE)
