library(dunn.test)

#use result df to make a do stats tests with dates and samples

# Split the "sampleid" column into two columns: "date" and "sample_number"
data <- separate(result_df,Sampleid, into = c("date", "sample_number"), sep = " sample ")# Remove all words after the sample number
data$sample_number<- gsub("\\s+.*$", "", data$sample_number)

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(data)
kruskal_test
Datedunn<-dunn.test(data$normalPercent, g = data$date, method = "bonferroni")
sampledunn<-dunn.test(data$normalPercent, g = data$sample_number, method = "bonferroni")

# Create an empty list to store the results
dunn_results <- list()
date_results<-list()
sample_results<-list()
# Iterate over each column (except 'date' and 'sample') for comparison
for (col in colnames(data)[-c(which(colnames(data) == "date"), which(colnames(data) == "sample"))]) {
  # Check if the column contains numeric data
  if(is.numeric(data[[col]])) {
    # Remove missing values
    col_data <- na.omit(data[[col]])

    # Perform Dunn's post hoc test for 'date' and 'sample'
    dunn_result_date <- dunn.test(col_data, g = data$date, method = "bonferroni")
    dunn_result_sample <- dunn.test(col_data, g = data$sample, method = "bonferroni")
    significance_threshold <- 0.5
    # Count the number of significant comparisons
    dunn_result_date$significantcount <- sum(dunn_result_date$P.adjusted < significance_threshold)
    dunn_result_sample$significantcount <- sum(dunn_result_sample$P.adjusted < significance_threshold)

    # Store the results in the list
    dunn_results[[paste0(col, "_vs_date")]] <- dunn_result_date
    dunn_results[[paste0(col, "_vs_sample")]] <- dunn_result_sample
    # Store the results in the separate lists
    date_results[[paste0(col, "_vs_date")]] <- dunn_result_date
    sample_results[[paste0(col, "_vs_sample")]] <- dunn_result_sample
  } else {
    cat(paste("Skipping column", col, "as it does not contain numeric data.\n"))
  }
}
# Convert the lists of results into a dataframe
dunn_results_df <- as.data.frame(do.call(rbind, dunn_results))
date_results_df <- as.data.frame(do.call(rbind, date_results))
sample_results_df <- as.data.frame(do.call(rbind, sample_results))
significance_threshold <- 0.5

# Count the number of significant comparisons
dunn_results_df$significantcount <- sum(dunn_results_df$P.adjusted < significance_threshold)

date_significant<-sum(as.numeric(date_results_df$significantcount))
sample_significant<-sum(as.numeric(sample_results_df$significantcount))

# Create a dataframe to store the results
bardf <- data.frame(Group = c("Sample", "Date"),
                         Significant_Comparative_Groups = c(0, 7))

# Create a bar plot

ggplot(bardf, aes(x = Group, y = Significant_Comparative_Groups, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Significant Comparative Groups at P<0.05",
       x = "Group", y = "Number of Significant Comparative Groups") +
  scale_fill_manual(values = c("Sample" = "skyblue", "Date" = "lightgreen")) +
  theme_minimal()
