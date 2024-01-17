library(NMAcompanion)
pig_data<-read.table("D:/Full_pig_project/real_Merge_of_all_pig_datasets_stats.txt",sep = "\t",header = TRUE)

#gives The number of each detected group
count_subfertile <- sum(grepl("subfertile",pig_data$Folder, ignore.case = TRUE))
count_fertile <- sum(grepl(" fertile", pig_data$Folder, ignore.case = TRUE))
