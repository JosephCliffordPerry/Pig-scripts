
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


data <- separate(Pig_with_morphology_clusters,Sampleid, into = c("date", "sample_number"), sep = " sample ")# Remove all words after the sample number
write.table(data,"data/data_with_date_and_clusters.txt",sep = "\t")

