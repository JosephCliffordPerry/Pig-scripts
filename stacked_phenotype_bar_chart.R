pig_data<-read.table("D:/Full_pig_project/real_Merge_of_all_pig_datasets_stats.txt",sep = "\t",header = TRUE)
#load("Pig_outliers")
Outliers4<-read.table("D:/Full_pig_project/Pig_scripts/Pig_outliers4.txt",sep = "\t",header = TRUE)

#rename_ids
renamedham<-hamming_data_from_outlier %>% rename(
  CellID = UUID
)
#alternative clusterings
# renamedham<-Outliers4 %>% rename(
#       CellID = V1,
# Clustering_file = V2)
#alternative clustering
# renamedham<-as.data.frame(Manual_clusters) %>% rename(
#        CellID = V1,
# Clustering_file = V2
#    )

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
result_df <- data.frame(
  Sampleid = character(),
  normalCount = numeric(),
  normalPercent = numeric(),
  Intermediary1Count = numeric(),
  IntermediaryPercent = numeric(),
  Extreme1Count = numeric(),
  ExtremePercent = numeric(),

  stringsAsFactors = FALSE
)
for (i in 1:length(Sample_list_with_clusters)) {
  pig<- data.frame(rbind(Sample_list_with_clusters[[i]]))
  normal<-sum(pig$Clustering_file == 1)
  normalpercent<-(normal/nrow(pig))*100
  sample<-basename(pig[1,6])
  #Outliers 4
  Intermediaryphenotype1<-sum(pig$Clustering_file == 2,pig$Clustering_file ==5,pig$Clustering_file ==6)
  Intermediaryphenotype1percent<-(Intermediaryphenotype1/nrow(pig))*100
  Extremephenotype1<-sum(pig$Clustering_file == 4, pig$Clustering_file ==3, pig$Clustering_file ==7,pig$Clustering_file ==8)
  Extremephenotype1percent<-(Extremephenotype1/nrow(pig))*100
  #first outlier cluster identity
  # Extremephenotype1<-sum(pig$Clustering_file == 2, pig$Clustering_file ==3, pig$Clustering_file ==7,pig$Clustering_file ==6)
  # Extremephenotype1percent<-(Extremephenotype1/nrow(pig))*100
  # Intermediaryphenotype1<-sum(pig$Clustering_file == 4,pig$Clustering_file ==5,pig$Clustering_file ==8)
  # Intermediaryphenotype1percent<-(Intermediaryphenotype1/nrow(pig))*100
  #Manual filter
  #  Intermediaryphenotype1<-sum(pig$Clustering_file == 4,pig$Clustering_file ==3 )
  # Intermediaryphenotype1percent<-(Intermediaryphenotype1/nrow(pig))*100
  # Extremephenotype1<-sum(pig$Clustering_file == 2)
  # Extremephenotype1percent<-(Extremephenotype1/nrow(pig))*100
  #


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
                        IntermediaryPercent = Intermediaryphenotype1percent,
                        Extreme1Count = Extremephenotype1,
                        ExtremePercent = Extremephenotype1percent
                      )
  )

}

#make stacked bar


# Select only the percentage columns
percentage_cols <- grep("Percent", names(result_df), value = TRUE)
percentage_df <- result_df[, c("Sampleid", percentage_cols)]
sorted_percentage_df <- percentage_df[order(-percentage_df$normalPercent), ]

# Melt the sorted data frame
melted_df <- reshape2::melt(sorted_percentage_df, id.vars = "Sampleid")



# Convert 'Sampleid' to factor with correct order
melted_df$Sampleid <- factor(melted_df$Sampleid, levels = unique(melted_df$Sampleid))


# Normalize the values in melted_df
melted_df_normalized <- melted_df %>%
  group_by(Sampleid) %>%
  mutate(normalized_value = value / sum(value)) %>%
  ungroup()

# Plot the sorted data for both fertile and subfertile
ggplot(melted_df_normalized, aes(x = factor(Sampleid), y = value, fill = variable)) +
  geom_col(position = "fill",width = 1) +
  labs(title =  element_blank(),
       x = "Sample", y = "Percentage") +
  scale_fill_manual(values = c(
    "normalPercent" = "white",
    "IntermediaryPercent" = "grey",
    "ExtremePercent" = "black"

  )) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),legend.title = element_blank())

###############################################
# add facets
# Define the regular expression patterns
subfertile_pattern <- "subfertile"
fertile_pattern <- "fertile"


# Add a new column based on the counts
melted_df$Reported_fertility <- ifelse(grepl(subfertile_pattern, sorted_percentage_df$Sampleid, ignore.case = TRUE), "subfertile",
                                      ifelse(grepl(fertile_pattern, sorted_percentage_df$Sampleid, ignore.case = TRUE), "fertile", NA))

  plot<-ggplot(melted_df, aes(x = factor(Sampleid), y = value, fill = variable)) +
     geom_col(position = "fill",width = 1) +
     labs(title =  element_blank(),
          x = "Sample", y = "Proportion") +
     scale_fill_manual(values = c(
        "normalPercent" = "white",
         "IntermediaryPercent" = "grey",
         "ExtremePercent" = "black"  )) +
     theme_minimal() +
     theme(axis.text.x = element_blank(),legend.title = element_blank(),
           legend.position = c(0.85, 0.8),  # Specify legend position (x, y)
           legend.box.background = element_rect(color = "black", size = 1),
           legend.key = element_rect(color = "black", size = 1),  # Set legend key outline color
           legend.key.size = unit(1, "lines")) +  # Add box around legend)+
     facet_wrap(~ Reported_fertility, nrow = 1,scale = "free")

  ggsave(path =  plot, width = 170, height = 170, units = "mm", dpi = 300)
#############################################
#stacked bar chart grouped by date
# Split the "sampleid" column into two columns: "date" and "sample_number"
data <- separate(result_df,Sampleid, into = c("date", "sample_number"), sep = " sample ")# Remove all words after the sample number

result_df$date <-data$date

  # Select only the percentage columns
  percentage_cols <- grep("Percent", names(result_df), value = TRUE)
  percentage_df <- result_df[, c("Sampleid","date", percentage_cols)]
  sorted_percentage_df <- percentage_df[order(-percentage_df$normalPercent), ]


  # Melt the sorted data frame
  melted_df <- reshape2::melt(sorted_percentage_df, id.vars = c("Sampleid","date"))



  # Convert 'Sampleid' to factor with correct order
  melted_df$Sampleid <- factor(melted_df$Sampleid, levels = unique(melted_df$Sampleid))

  # Define the regular expression patterns
  subfertile_pattern <- "subfertile"
  fertile_pattern <- "fertile"


  # Add a new column based on the counts
  melted_df$Reported_fertility <- ifelse(grepl(subfertile_pattern, sorted_percentage_df$Sampleid, ignore.case = TRUE), "subfertile",
                                         ifelse(grepl(fertile_pattern, sorted_percentage_df$Sampleid, ignore.case = TRUE), "fertile", NA))

  ggplot(melted_df, aes(x = factor(Sampleid), y = value, fill = variable,color = date)) +
    geom_col(width = 1) +
    labs(title =  element_blank(),
         x = "Sample", y = "Percentage") +
    scale_fill_manual(values = c(
      "normalPercent" = "white",
      "IntermediaryPercent" = "grey",
      "ExtremePercent" = "black"  )) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),legend.title = element_blank())+
    facet_wrap(~ Reported_fertility, nrow = 1,scale = "free")
