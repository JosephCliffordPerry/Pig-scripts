#library list
library(dplyr)
library(factoextra)
library(stringr)
library(tidyverse)
library(umap)
library(ggplot2)
library(patchwork)
library(scales)


library(cluster)
#library(dendextend)
#library(data.table)

#library(modEvA) # plotting GLM outputs
#library(boot) #
library(caret) # cross - validation methods
#library(randomForest)
#library(fdm2id)

library(caTools)
source("Ben's_common_functions.R")

COLOURS <- RColorBrewer::brewer.pal(8, "Dark2")

if (!dir.exists("data")) {
  dir.create("data")
}

if (!dir.exists("figures")) {
  dir.create("figures")
}
pig_data<-read.table("D:/Full_pig_project/real_Merge_of_all_pig_datasets_stats.txt",sep = "\t",header = TRUE)



