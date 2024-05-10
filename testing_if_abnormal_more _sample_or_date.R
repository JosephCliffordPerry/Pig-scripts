
#use result df to make a do stats tests with dates and samples

# Split the "sampleid" column into two columns: "date" and "sample_number"
data <- separate(result_df,Sampleid, into = c("date", "sample_number"), sep = " sample ")# Remove all words after the sample number
#don't separate fertile and subfertile
data$sample_number<- gsub("\\s+.*$", "", data$sample_number)




# Perform Kruskal-Wallis test on data$normalPercent stratified by data$date and data$sample_number
kruskal_test_result <- kruskal.test(normalPercent ~ date , data = data)
kruskal_test_result2 <- kruskal.test(normalPercent ~  sample_number, data = data)
# #separate fertile and subfertile
# data$sample_number <- gsub("\\s.*_fertile", "_fertile", data$sample_number)
# data$sample_number <- gsub("\\s.*_subfertile", "_subfertile", data$sample_number)


# # Perform Kruskal-Wallis test
# kruskal_test <- kruskal.test(data)
# kruskal_test
# Datedunn<-dunn.test(data$normalPercent, g = data$date, method = "bonferroni")
# sampledunn<-dunn.test(data$normalPercent, g = data$sample_number, method = "bonferroni")
#
# # Create an empty list to store the results
# dunn_results <- list()
# date_results<-list()
# sample_results<-list()
# # Iterate over each column (except 'date' and 'sample') for comparison
# for (col in colnames(data)[-c(which(colnames(data) == "date"), which(colnames(data) == "sample"))]) {
#   # Check if the column contains numeric data
#   if(is.numeric(data[[col]])) {
#     # Remove missing values
#     col_data <- na.omit(data[[col]])
#
#     # Perform Dunn's post hoc test for 'date' and 'sample'
#     dunn_result_date <- dunn.test(col_data, g = data$date, method = "bonferroni")
#     dunn_result_sample <- dunn.test(col_data, g = data$sample, method = "bonferroni")
#     significance_threshold <- 0.5
#     # Count the number of significant comparisons
#     dunn_result_date$significantcount <- sum(dunn_result_date$P.adjusted < significance_threshold)
#     dunn_result_sample$significantcount <- sum(dunn_result_sample$P.adjusted < significance_threshold)
#
#     # Store the results in the list
#     dunn_results[[paste0(col, "_vs_date")]] <- dunn_result_date
#     dunn_results[[paste0(col, "_vs_sample")]] <- dunn_result_sample
#     # Store the results in the separate lists
#     date_results[[paste0(col, "_vs_date")]] <- dunn_result_date
#     sample_results[[paste0(col, "_vs_sample")]] <- dunn_result_sample
#   } else {
#     cat(paste("Skipping column", col, "as it does not contain numeric data.\n"))
#   }
# }
# # Convert the lists of results into a dataframe
# dunn_results_df <- as.data.frame(do.call(rbind, dunn_results))
# date_results_df <- as.data.frame(do.call(rbind, date_results))
# sample_results_df <- as.data.frame(do.call(rbind, sample_results))
# significance_threshold <- 0.05
#
# # Count the number of significant comparisons
# dunn_results_df$significantcount <- sum(dunn_results_df$P.adjusted < significance_threshold)
#
# date_significant<-sum(as.numeric(date_results_df$significantcount))
# sample_significant<-sum(as.numeric(sample_results_df$significantcount))
#
# # Create a dataframe to store the results
# bardf <- data.frame(Group = c("Sample", "Date"),
#                          Significant_Comparative_Groups = c(20, 37))
#
# # Create a bar plot
#
# ggplot(bardf, aes(x = Group, y = Significant_Comparative_Groups, fill = Group)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Number of Significant Comparative Groups at P<0.05",
#        x = "Group", y = "Number of Significant Comparative Groups") +
#   scale_fill_manual(values = c("Sample" = "skyblue", "Date" = "lightgreen")) +
#   theme_minimal()
#

#########
#test for IQR between dates and samples. separate the data in dates and samples

#calculate IQR by sample
IQR_per_date_normal <- aggregate(normalPercent ~ date, data = data, FUN = IQR)
IQR_per_sample_normal <- aggregate(normalPercent ~ sample_number, data = data, FUN = IQR)

IQR_per_date_Intermediary <- aggregate(IntermediaryPercent ~ date, data = data, FUN = IQR)
IQR_per_sample_Intermediary <- aggregate(IntermediaryPercent ~ sample_number, data = data, FUN = IQR)

IQR_per_date_Extreme <- aggregate(ExtremePercent ~ date, data = data, FUN = IQR)
IQR_per_sample_Extreme <- aggregate(ExtremePercent ~ sample_number, data = data, FUN = IQR)

mean(IQR_per_date_normal $normalPercent)
mean(IQR_per_sample_normal$normalPercent)

mean(IQR_per_date_Intermediary$IntermediaryPercent)
mean(IQR_per_sample_Intermediary$IntermediaryPercent)

mean(IQR_per_date_Extreme$ExtremePercent)
mean(IQR_per_sample_Extreme$ExtremePercent)
#both  6,2,5,3,9 are known breeds
#normal
mean(c(7.213260,7.223302,4.062165,2.830837,4.587580))
#intermediary
mean(c(6.865009,5.957962,6.300833,1.363050,5.231615))
#extreme
mean(c(3.0658996,2.7584681,1.6379332,2.0547334,1.3750400))
#test for mean abnormality between dates and samples. separate the data in dates and samples
#calculate mean abnormality by sample and data
mean_per_date_normal <- aggregate(normalPercent ~ date, data = data, FUN = mean)
correlation_per_date_normal <- aggregate(data$normalPercent ~ data$date, data = data, FUN = cor)




mean_per_sample_normal <- aggregate(normalPercent ~ sample_number, data = data, FUN = mean)

mean_per_date_normal
mean_per_sample_normal

wilcox.test(mean_per_date_normal$normalPercent,mean_per_sample_normal$normalPercent)

sample_date<-cbind(data$date,data$sample_number)




####### Sample date timeline

PlotX <- as.Date(data$date,format = "%d.%m.%y")
ploty <- data$sample_number
plot_data<-as.data.frame(cbind(Plotx = as.character(PlotX),ploty =ploty))

ggplot(plot_data, aes(x =Plotx, y = ploty)) +
  geom_point() +
  geom_line(aes(group = ploty), color = "red") +
  labs(title = "Timeline of Samples",
       x = "Date",
       y = "Sample Label") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

