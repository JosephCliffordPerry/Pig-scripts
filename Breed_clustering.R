Breed_Clusters<-read.table("D:/Full_pig_project/Pig_Breed_Cluster_Table.txt",sep = "\t",header = TRUE)
Total_known_samples<-sum(Breed_Clusters$Freq)
Hampshire<- Breed_Clusters[1:16,]
Hampshire$Dataset_distribution <-(sum(Hampshire$Freq)/Total_known_samples)*100
Hampshire$Cluster_difference_from_distribution <- (Hampshire$Dataset_distribution - Hampshire$Percentage_Of_Cluster)

Landrace<-Breed_Clusters[17:32,]
Landrace$Dataset_distribution <-(sum(Landrace$Freq)/Total_known_samples)*100
Landrace$Cluster_difference_from_distribution <- (Landrace$Dataset_distribution - Landrace$Percentage_Of_Cluster)

Large_white<-Breed_Clusters[33:48,]
Large_white$Dataset_distribution <-(sum(Large_white$Freq)/Total_known_samples)*100
Large_white$Cluster_difference_from_distribution <- (Large_white$Dataset_distribution - Large_white$Percentage_Of_Cluster)

Pietrain<-Breed_Clusters[49:64,]
Pietrain$Dataset_distribution <-(sum(Pietrain$Freq)/Total_known_samples)*100
Pietrain$Cluster_difference_from_distribution <- (Pietrain$Dataset_distribution - Pietrain$Percentage_Of_Cluster)

White_Duroc<-Breed_Clusters[65:80,]
White_Duroc$Dataset_distribution <-(sum(White_Duroc$Freq)/Total_known_samples)*100
White_Duroc$Cluster_difference_from_distribution <- (White_Duroc$Dataset_distribution - White_Duroc$Percentage_Of_Cluster)


bargraphlist<-list()
for( i in 1:max(Breed_Clusters$Cluster)){

  cluster<- subset(Breed_Clusters, Cluster == i)


  bargraphlist[[i]]<-ggplot(cluster, aes(x = i, y = Percentage_Of_Cluster, fill = Dataset,group = Dataset)) +
    geom_bar(position = "fill",stat = "identity") +
    labs(title = paste0("Stacked Bar Chart of Cluster",i),
         x = paste0("Cluster ",cluster[1,1]), y = "Percentage")  +
    theme_minimal()+
    theme(axis.title.x = element_blank(),  # Remove x-axis title
          axis.text.x = element_blank())
}
#make exportable graphs
cluster_Breed_Graph4<-bargraphlist[[13]]+bargraphlist[[14]]+bargraphlist[[15]]+bargraphlist[[16]]


#make graph of breed percentages
ggplot(Breed_Clusters, aes(x = 1, y = Freq, fill = Dataset,group = Dataset)) +
  geom_bar(position = "fill",stat = "identity")
