library(NMAcompanion)
library(tidyverse)
library(umap)
library(ggplot2)
set.seed(08000)
pig_data<-read.table("D:/Full_pig_project/real_Merge_of_all_pig_datasets_stats.txt",sep = "\t",header = TRUE)
load("Pig_outliers")
#gives The number of each detected group
count_subfertile <- sum(grepl("subfertile",pig_data$Folder, ignore.case = TRUE))
count_fertile <- sum(grepl(" fertile", pig_data$Folder, ignore.case = TRUE))
cluster_pop<-table(hamming_data_from_outlier$Clustering_file)
#1661 total outgroups 8%
#1196 removed a group that is visibly very similar 6%
hamming_data_from_outlier[1:2]
write.table(hamming_data_from_outlier[1:2],file = "Pig_outlier_clusters.txt",sep = "\t",row.names = FALSE)
renamedham<-hamming_data_from_outlier %>% rename(
  CellID = UUID
)

# Define the regular expression patterns
subfertile_pattern <- "subfertile"
fertile_pattern <- "fertile"


# Add a new column based on the counts
pig_data$Reported_fertility <- ifelse(grepl(subfertile_pattern, pig_data$Folder, ignore.case = TRUE), "subfertile",
                                      ifelse(grepl(fertile_pattern, pig_data$Folder, ignore.case = TRUE), "fertile", NA))



Pigham<-merge(pig_data,renamedham,by = "CellID")


grouped_Pigham<-Pigham %>% group_by(Folder)
Sample_list_with_clusters<-list()
Sample_list_with_clusters<-group_split(grouped_Pigham)
#make umaps to display cluster distribution
graph_list<-list()
for( i in 1:length(Sample_list_with_clusters)){
pig<- data.frame(rbind(Sample_list_with_clusters[[i]]))
umap<-umap(pig[,245:344])
umapdf<-as.data.frame(umap$layout)
umapdf$clusters_factor <- as.factor(pig$Clustering_file)
gromph3 <- ggplot(data = umapdf, aes(V1, V2, color = clusters_factor)) +
  geom_point() +
  labs(title = basename(pig[1,6]), x = NULL, y = NULL, colour = "clusters") +
   theme(legend.position = "none") +
   scale_color_discrete() +
  theme_minimal() +
   theme(legend.position = "none")
graph_list[[i]]<-gromph3
}
# Set the directory where you want to save the graphs
output_directory <- "D:/Full_pig_project/Cluster_umaps/"

# Save each plot as an image
for (i in seq_along(graph_list)) {
  # Extract the title of the plot
  plot_title <- graph_list[[i]][["labels"]][["title"]]

  # Replace spaces with underscores in the title for the filename
  plot_filename <- paste0(output_directory, gsub(" ", "_", plot_title), ".png")

  # Save the plot as a PNG file
  ggsave(plot_filename, graph_list[[i]], device = "png")

  cat("Saved plot:", plot_filename, "\n")
}
