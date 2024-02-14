library(NMAcompanion)
library(tidyverse)
library(umap)
library(ggplot2)
library(patchwork)
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
#renames UUID to CellID
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
Pigham$Folder <- paste( Pigham$Folder, Pigham$Reported_fertility,sep = "_")
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
  plot_filename <- paste0(output_directory,gsub("[[:punct:][:space:]]", "", plot_title),"_", i,".png")

  # Save the plot as a PNG file
  ggsave(plot_filename, graph_list[[i]], device = "png")

  cat("Saved plot:", plot_filename, "\n")
}

# graphs show uneaven distribution of abnormal sperm testing
#individual samples to see if they fit the pattern find >6% and >8%
# Initialize an empty data frame
result_df <- data.frame(
  Sampleid = character(),
  normalCount = numeric(),
  normalPercent = numeric(),
  Intermediary1Count = numeric(),
  Intermediary1Percent = numeric(),
  Intermediary2Count = numeric(),
  Intermediary2Percent = numeric(),
  Intermediary3Count = numeric(),
  Intermediary3Percent = numeric(),
  Intermediary4Count = numeric(),
  Intermediary4Percent = numeric(),
  Extreme1Count = numeric(),
  Extreme1Percent = numeric(),
  Extreme2Count = numeric(),
  Extreme2Percent = numeric(),
  Extreme3Count = numeric(),
  Extreme3Percent = numeric(),
  stringsAsFactors = FALSE
)
for (i in 1:length(Sample_list_with_clusters)) {
pig<- data.frame(rbind(Sample_list_with_clusters[[i]]))
normal<-sum(pig$Clustering_file == 1)
normalpercent<-(normal/nrow(pig))*100
sample<-basename(pig[1,6])

Intermediaryphenotype1<-sum(pig$Clustering_file == 2)
Intermediaryphenotype1percent<-(Intermediaryphenotype1/nrow(pig))*100

Intermediaryphenotype2<-sum(pig$Clustering_file == 3)
Intermediaryphenotype2percent<-(Intermediaryphenotype2/nrow(pig))*100

Intermediaryphenotype3<-sum(pig$Clustering_file ==4)
Intermediaryphenotype3percent<-(Intermediaryphenotype3/nrow(pig))*100

Intermediaryphenotype4<-sum(pig$Clustering_file == 5)
Intermediaryphenotype4percent<-(Intermediaryphenotype4/nrow(pig))*100

Extremephenotype1<-sum(pig$Clustering_file == 6)
Extremephenotype1percent<-(Extremephenotype1/nrow(pig))*100

Extremephenotype2<-sum(pig$Clustering_file == 7)
Extremephenotype2percent<-(Extremephenotype2/nrow(pig))*100

Extremephenotype3<-sum(pig$Clustering_file == 8)
Extremephenotype3percent<-(Extremephenotype3/nrow(pig))*100

# Create a row with the results
# Get the sample name
sample_name <- basename(pig[1, 6])

# Add the results to the data frame
result_df <- rbind( result_df,
  data.frame(
    Sampleid = sample_name,
    normalCount = normal,
    normalPercent = normalpercent,
    Intermediary1Count = Intermediaryphenotype1,
    Intermediary1Percent = Intermediaryphenotype1percent,
    Intermediary2Count = Intermediaryphenotype2,
    Intermediary2Percent = Intermediaryphenotype2percent,
    Intermediary3Count = Intermediaryphenotype3,
    Intermediary3Percent = Intermediaryphenotype3percent,
    Intermediary4Count = Intermediaryphenotype4,
    Intermediary4Percent = Intermediaryphenotype4percent,
    Extreme1Count = Extremephenotype1,
    Extreme1Percent = Extremephenotype1percent,
    Extreme2Count = Extremephenotype2,
    Extreme2Percent = Extremephenotype2percent,
    Extreme3Count = Extremephenotype3,
    Extreme3Percent = Extremephenotype3percent
  )
)

}

#make stacked bar


# Select only the percentage columns
percentage_cols <- grep("Percent", names(result_df), value = TRUE)
percentage_df <- result_df[, c("Sampleid", percentage_cols)]

# Create a new column 'Group' based on groups of ten samples
percentage_df <- percentage_df %>%
  mutate(Group = factor((row_number() - 1) %/% 10 + 1))

Percentagelist<-list()
# Split the data into a list by unique values in 'column_to_split'
Percentagelist <- split(percentage_df, percentage_df[["Group"]])

bargraphlist<-list()
for( i in 1:length(Percentagelist)){

# Create the stacked bar chart
percentage_df<-Percentagelist[[i]]
melted_df <- reshape2::melt(percentage_df, id.vars = "Sampleid")
# Remove rows where the value in 'variable_column' is "Group"
filtered_df <- subset(melted_df, !grepl("Group", get("variable")))
filtered_df_nonzero <- filtered_df[filtered_df$value != 0, ]
filtered_df_nonzero$value <- as.numeric(filtered_df_nonzero$value)
bargraphlist[[i]]<-ggplot(filtered_df_nonzero, aes(x = factor(Sampleid), y = value, fill = variable,group = variable)) +
  geom_bar(position = "fill",stat = "identity") +
  labs(title = paste0("Stacked Bar Chart of Phenotype Percentages",i),
       x = "Sample", y = "Percentage") +
  scale_fill_manual(values = c(
    "normalPercent" = "blue",
    "Intermediary1Percent" = "green",
    "Intermediary2Percent" = "yellow",
    "Intermediary3Percent" = "orange",
    "Intermediary4Percent" = "red",
    "Extreme1Percent" = "purple",
    "Extreme2Percent" = "brown",
    "Extreme3Percent" = "pink"
  )) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Set the directory where you want to save the graphs
output_directory <- "D:/Full_pig_project/Cluster_bargraphs/"

# Save each plot as an image
for (i in seq_along(bargraphlist)) {
  # Extract the title of the plot
  plot_title <- bargraphlist[[i]][["labels"]][["title"]]

  # Replace spaces with underscores in the title for the filename
  plot_filename <- paste0(output_directory,gsub("[[:punct:][:space:]]", "", plot_title),"_", i,".png")

  # Save the plot as a PNG file
  ggsave(plot_filename, bargraphlist[[i]], device = "png")

  cat("Saved plot:", plot_filename, "\n")
}
library(dplyr)

filtered_df %>%
  group_by(Sample) %>%
  summarise(total_value = sum(value))
