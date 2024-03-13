pig_data<-read.table("D:/Full_pig_project/real_Merge_of_all_pig_datasets_stats.txt",sep = "\t",header = TRUE)
load("Pig_outliers")
#rename_ids
renamedham<-hamming_data_from_outlier %>% rename(
  CellID = UUID
)

Pigham<-merge(pig_data,renamedham,by = "CellID")
Pigham$Folder <- paste( Pigham$Folder, Pigham$Reported_fertility,sep = "_")
grouped_Pigham<-Pigham %>% group_by(Folder)
Sample_list_with_clusters<-list()
Sample_list_with_clusters<-group_split(grouped_Pigham)
result_df <- data.frame(
  Sampleid = character(),
  normalCount = numeric(),
  normalPercent = numeric(),
  Intermediary1Count = numeric(),
  Intermediary1Percent = numeric(),
  Extreme1Count = numeric(),
  Extreme1Percent = numeric(),

  stringsAsFactors = FALSE
)
for (i in 1:length(Sample_list_with_clusters)) {
  pig<- data.frame(rbind(Sample_list_with_clusters[[i]]))
  normal<-sum(pig$Clustering_file == 1)
  normalpercent<-(normal/nrow(pig))*100
  sample<-basename(pig[1,6])

  Intermediaryphenotype1<-sum(pig$Clustering_file == 4,5,8)
  Intermediaryphenotype1percent<-(Intermediaryphenotype1/nrow(pig))*100

  Extremephenotype1<-sum(pig$Clustering_file == 2,3,6,7)
  Extremephenotype1percent<-(Extremephenotype1/nrow(pig))*100



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
                        Extreme1Count = Extremephenotype1,
                        Extreme1Percent = Extremephenotype1percent
                      )
  )

}

#make stacked bar


# Select only the percentage columns
percentage_cols <- grep("Percent", names(result_df), value = TRUE)
percentage_df <- result_df[, c("Sampleid", percentage_cols)]

# Sort the data frame
sorted_percentage_df <- percentage_df[order(-percentage_df$normalPercent),]

# Melt the sorted data frame
melted_df <- reshape2::melt(sorted_percentage_df, id.vars = "Sampleid")

# Remove rows where the value in 'variable_column' is "Group"
filtered_df <- subset(melted_df, !grepl("Group", get("variable")))

# Convert 'value' column to numeric
filtered_df$value <- as.numeric(filtered_df$value)

# Sort the melted data frame based on 'normalPercent' column

filtered_df <- filtered_df %>%
  mutate(Sampleid = factor(Sampleid, levels = unique(Sampleid)[order(-value)]))



# Plot the sorted data
ggplot(filtered_df, aes(x = factor(Sampleid), y = value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Stacked Bar Chart of Phenotype Percentages",
       x = "Sample", y = "Percentage") +
  scale_fill_manual(values = c(
    "normalPercent" = "white",
    "Intermediary1Percent" = "grey",
    "Extreme1Percent" = "black"

  )) +
  theme_minimal() +
  theme(axis.text.x = NULL)
