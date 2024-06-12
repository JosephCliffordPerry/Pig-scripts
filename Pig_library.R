#library list
library(dplyr)
library(factoextra)
library(stringr)
library(tidyverse)
library(umap)
library(ggplot2)
library(patchwork)
library(scales)
library(caret) # cross - validation methods
library(data.table)
library(modEvA)

library(caTools)
source("Ben's_common_functions.R")

COLOURS <- RColorBrewer::brewer.pal(8, "Dark2")

if (!dir.exists("data")) {
  dir.create("data")
}

if (!dir.exists("figures")) {
  dir.create("figures")
}

# Move the file
file.rename("Pig_Datasets.csv.bz", "data/Pig_Datasets.csv.bz")
pig_data<- read.csv(bzfile("data/Pig_Datasets.csv.bz"))
write.table(pig_data,"data/real_Merge_of_all_pig_datasets_stats.txt",sep = "/t")

