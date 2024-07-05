
pig.data <- pig_data %>%
  tidyr::separate_wider_delim(Centre_of_mass, " - ", names = c("CoMX", "CoMY")) %>%
  dplyr::mutate(
    Type = stringr::str_extract(Folder, "(sub)?fertile"),
    Type = ifelse(Type == "fertile", "Normal", "Abnormal"),
    CoMX = as.numeric(CoMX), CoMY = as.numeric(CoMY),
    TypeInt = as.factor(ifelse(Type == "Normal", 1, 0)))
folders <- unique(pig.data$Folder)
pig.data$Sample <- paste0(pig.data$Type, "_", sapply(pig.data$Folder, function(f) which(folders == f)))

renamedham<-as.data.frame(Outliers2) %>% rename(
  CellID = V1,
  Morphology_cluster = V2)

Pig_with_morphology_clusters<-merge(pig.data,renamedham,by = "CellID")
Pig_with_morphology_clusters$Sampleid<-basename(Pig_with_morphology_clusters$Folder)

Pig_with_morphology_Breed_data <- Pig_with_morphology_clusters %>%
  mutate(
    Breed = case_when(
      grepl(" sample 6 ", Sampleid, ignore.case = TRUE) ~ "Hampshire",
      grepl(" sample 2 ", Sampleid, ignore.case = TRUE) ~ "Landrace",
      grepl(" sample 5 ", Sampleid, ignore.case = TRUE) ~ "Large White",
      grepl(" sample 3 ", Sampleid, ignore.case = TRUE) ~ "Pietrain",
      grepl(" sample 9 ", Sampleid, ignore.case = TRUE) ~ "White Duroc",
      TRUE ~ "Other"
    ),
    Morphology_cluster = case_when(
      Morphology_cluster %in% c(1) ~ 1,
      Morphology_cluster %in% c(5, 6) ~ 2,
      Morphology_cluster %in% c(4, 7, 8) ~ 3,
      TRUE ~ as.numeric(Morphology_cluster)
    )
  )
data <- separate(Pig_with_morphology_Breed_data,Sampleid, into = c("date", "sample_number"), sep = " sample ")# Remove all words after the sample number
write.table(data,"data/data_with_date_and_clusters.txt",sep = "\t", row.names = FALSE)

# Subset where Type is "Abnormal"
abnormal <- subset(data, Type == "Abnormal")
table(abnormal$Morphology_cluster)
# Subset where TypeInt is "Normal"
normal <- subset(data, Type == "Normal")
table(normal$Morphology_cluster)
