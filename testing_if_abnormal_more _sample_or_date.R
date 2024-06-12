
#use result df to make a do stats tests with dates and samples

# Split the "sampleid" column into two columns: "date" and "sample_number"
data <- separate(result_df,Sampleid, into = c("date", "sample_number"), sep = " sample ")# Remove all words after the sample number
#don't separate fertile and subfertile
data$sample_number<- gsub("\\s+.*$", "", data$sample_number)




# Perform Kruskal-Wallis test on data$normalPercent stratified by data$date and data$sample_number
kruskal_test_result <- kruskal.test(normalPercent ~ date , data = data)
kruskal_test_result2 <- kruskal.test(normalPercent ~  sample_number, data = data)

#########
#test for IQR between dates and samples. separate the data in dates and samples

#calculate IQR by sample
IQR_per_date_normal <- aggregate(normalPercent ~ date, data = data, FUN = IQR)
IQR_per_sample_normal <- aggregate(normalPercent ~ sample_number, data = data, FUN = IQR)

IQR_per_date_Intermediary <- aggregate(IntermediaryPercent ~ date, data = data, FUN = IQR)
IQR_per_sample_Intermediary <- aggregate(IntermediaryPercent ~ sample_number, data = data, FUN = IQR)

IQR_per_date_Extreme <- aggregate(ExtremePercent ~ date, data = data, FUN = IQR)
IQR_per_sample_Extreme <- aggregate(ExtremePercent ~ sample_number, data = data, FUN = IQR)

mean(IQR_per_date_normal$normalPercent)
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

####### Sample date timeline####

PlotX <- as.Date(data$date,format = "%d.%m.%y")
ploty <- data$sample_number
sum_vec<-data$normalCount+data$Intermediary1Count+data$Extreme1Count
# Step 2: Scale the resulting vector to be between 1 and 10
plot_size <- ((sum_vec - min(sum_vec)) / (max(sum_vec) - min(sum_vec))) * 4 + 1
plot_data<-as.data.frame(cbind(Plotx = as.character(PlotX),ploty =ploty,plot_size = plot_size))

ggplot(plot_data, aes(x =Plotx, y = ploty)) +
  geom_point(size = plot_size) +
  geom_line(aes(group = ploty), color = "red") +
  labs(title = "Timeline of Samples",
       x = "Date",
       y = "Sample Label") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
ggsave(filename = "figures/Sample_date_timeline.png", width = 170, height = 90, units = "mm", dpi = 300)

