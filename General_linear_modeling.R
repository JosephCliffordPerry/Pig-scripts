# Streamlined analysis with Joe's manually placed landmarks
# The landmarks were lifted over to my own nmd file for clean stats export

######################## Imports #################
library(tidyverse)
library(umap)
library(cluster)
library(dendextend)
library(data.table)
library(patchwork)
library(modEvA) # plotting GLM outputs
library(boot) #
library(caret) # cross - validation methods
library(randomForest)
library(fdm2id)

library(caTools)

source("Ben's_common_functions.R")

COLOURS <- RColorBrewer::brewer.pal(8, "Dark2")

if (!dir.exists("data")) {
  dir.create("data")
}

if (!dir.exists("figures")) {
  dir.create("figures")
}

######################## Functions##############################################
# Remove cells with angle profile values far outside the expected range. Most
# of these are edge detection errors
remove.poor.edge.detection <- function(data) {
  angle.profiles <- dplyr::select(data, matches("Angle_profile_\\d+$"))
  has.error <- apply(angle.profiles, 1, function(x) any(x > 220 | x < 40))
  data[!has.error, ]
}

remove.metadata.colummns <- function(data) {
  data %>% dplyr::select(
    -Type,
    -Difference_from_median,
    -starts_with("Franken"),
    -contains("sample"),
    -contains("shape.cluster"),
    -contains("Predicted"),
    -Dataset, -File, -CellID, -Component,
    -Folder, -Image, -CoMX, -CoMY
  )
}

remove.nonclustering.columns <- function(data) {
  remove.metadata.colummns(data) %>% dplyr::select(
    -starts_with("Seg"),
    -starts_with("Outline"),
    -contains("seg"),
    -contains("pixel")
  )
}

# Remove all columns with tail-orientation info (all profiles)
remove.tail.related.columns <- function(data) {
  remove.nonclustering.columns(data) %>% dplyr::select(
    -contains("profile")
  )
}

# run glm on a training and test dataset
run.glm <- function(train, test) {
  # Decent numbers in split?
  print(dim(train))
  print(dim(test))

  # Equal numbers of fertile and subfertile in train?
  print(table(train$Type))
  print(table(test$Type))

  result <- list()

  all.glm <- glm(TypeInt ~ .,
                 data = remove.nonclustering.columns(train),
                 family = binomial(link = "logit")
  )
  plotGLM(model = all.glm)

  # predictions on the validation set
  test$PredictedType <- predict(all.glm, newdata = test, type = "response")
  train$PredictedType <- predict(all.glm, newdata = train, type = "response")

  # assigning the probability cutoff as 0.5
  test$PredictedClass <- as.factor(ifelse(test$PredictedType >= 0.5, 1, 0))
  train$PredictedClass <- as.factor(ifelse(train$PredictedType >= 0.5, 1, 0))

  sample.numbers <- test %>%
    dplyr::group_by(Folder, Type) %>%
    dplyr::summarise(Cells = n())

  by.sample <- test %>%
    dplyr::group_by(Folder, Type) %>%
    dplyr::summarise(
      MeanPredict = mean(PredictedType),
      MedianPredict = median(PredictedType)
    )

  by.sample.plot <- ggplot(test, aes(x = Folder, y = PredictedType, col = Type)) +
    geom_boxplot() +
    geom_text(data = sample.numbers, aes(x = Folder, y = -0.05, label = Cells)) +
    labs(y = "Predicted type of sample", x = "Sample") +
    theme_bw()


  pred.sample.plot <- ggplot(by.sample, aes(x = Type, y = MeanPredict)) +
    geom_hline(yintercept = 0.5) +
    geom_point() +
    labs(y = "Mean of sample", x = "Type") +
    theme_bw()

  return(list(
    "test" = test, "train" = train, "by.sample" = by.sample.plot,
    "predicted.samples" = pred.sample.plot, "model" = all.glm,
    "confusionMatrix" = caret::confusionMatrix(test$PredictedClass, test$TypeInt, positive = "1")
  ))
}


# run glm on a training and test dataset
run.profile.glm <- function(train, test) {
  # Decent numbers in split?
  print(dim(train))
  print(dim(test))

  # Equal numbers of fertile and subfertile in train?
  print(table(train$Type))
  print(table(test$Type))

  result <- list()

  all.glm <- glm(TypeInt ~ .,
                 data = train %>% select(contains("profile"),TypeInt),
                 family = binomial(link = "logit")
  )
  plotGLM(model = all.glm)

  # predictions on the validation set
  test$PredictedType <- predict(all.glm, newdata = test, type = "response")
  train$PredictedType <- predict(all.glm, newdata = train, type = "response")

  # assigning the probability cutoff as 0.5
  test$PredictedClass <- as.factor(ifelse(test$PredictedType >= 0.5, 1, 0))
  train$PredictedClass <- as.factor(ifelse(train$PredictedType >= 0.5, 1, 0))

  sample.numbers <- test %>%
    dplyr::group_by(Sample, Type) %>%
    dplyr::summarise(Cells = n())

  by.sample <- test %>%
    dplyr::group_by(Sample, Type) %>%
    dplyr::summarise(
      MeanPredict = mean(PredictedType),
      MedianPredict = median(PredictedType)
    )

  by.sample.plot <- ggplot(test, aes(x = Sample, y = PredictedType, col = Type)) +
    geom_boxplot() +
    geom_text(data = sample.numbers, aes(x = Sample, y = -0.05, label = Cells)) +
    labs(y = "Predicted type of sample", x = "Sample") +
    theme_bw()


  pred.sample.plot <- ggplot(by.sample, aes(x = Type, y = MeanPredict)) +
    geom_hline(yintercept = 0.5) +
    geom_point() +
    labs(y = "Mean of sample", x = "Type") +
    theme_bw()

  return(list(
    "test" = test, "train" = train, "by.sample" = by.sample.plot,
    "predicted.samples" = pred.sample.plot, "model" = all.glm,
    "aggregted_prediction" = by.sample,
    "confusionMatrix" = caret::confusionMatrix(test$PredictedClass, test$TypeInt, positive = "1")
  ))
}

save.double <- function(plot, name, height = 170) {
  ggsave(paste0("figures/", name), plot, dpi = 300, units = "mm", width = 170, height = height)
}

# detection of pig outliers

#############################################################
make_outlier_cluster <- function(profile_data, profile_type) {
  boolean_matrix <- get_outlier_features(profile_data)
  #true boolean double
  tboolean <- which(boolean_matrix, arr.ind = TRUE)
  # #set a 5% threshold of the dataset
  # outlier_threshold <- (nrow(profile_data) / 20)
  # make table of true values
  booleancount <- table(tboolean[, 1])
  #cell ids
  outliercluster <- cbind(pig_data$CellID, 1)
  ## take the top 5% strangest values
  ## outlier_rows <- names(head(sort(booleancount, decreasing = TRUE), outlier_threshold))

  #try and take a reasonable portion
  outlier_rows <- names( which((booleancount > mean(booleancount))))
  #format dataframe
  outliercluster[as.numeric(outlier_rows), 2] <- 2
  outliercluster <- as.data.frame(outliercluster)
  names(outliercluster)[names(outliercluster) == "V1"] <- paste0(profile_type, " ", "outliers")
  names(outliercluster)[names(outliercluster) == "V2"] <- "Clustering_file"
  return(list(outliercluster))
}
######################## Read data #############################################

# Data from reanalysis of raw images - 21759 nuclei
# Exported from 2.2.0 so diameter profiles are absolute
#pig.data <- read.nma.stats("Normal_abnormal_merged_stats.txt")
pig.data <- read.nma.stats("data/real_Merge_of_all_pig_datasets_stats.txt") #- my dataset has 20795 sperm?
pig.data <- remove.poor.edge.detection(pig.data) # removes a few hundred

######################## Targeted GLMs on angle radius and diameter outliers using the outlier matrix####################
set.seed(123)
get_outlier_features <- function(profile.data) {
  profile.data <- dplyr::select(profile.data, contains("profile"))
  quartiles <- apply(profile.data, 2, quantile, probs=c(0.25, 0.75))
  IQRs <- quartiles[2,] - quartiles[1,]
  lower.bounds <- quartiles[1,] - 1.5 * IQRs
  upper.bounds <- quartiles[2,] + 1.5 * IQRs

  result <- sapply(1:ncol(profile.data), \(i) profile.data[,i]<lower.bounds[i]|profile.data[,i]>upper.bounds[i])
  mode(result) <- "integer"
  colnames(result) <- paste0(colnames(profile.data), "_outlier")
  result
}


#make an outlier dataframe by adding on the new numeric data
pig.outliers<- get_outlier_features(pig.data)

pig.outliers<-cbind(pig.data,pig.outliers)
#Run linear model on data
# dividing the samples into groups
train.folders <- sample.split(unique(pig.outliers$Sample), SplitRatio = 0.7)

# select all cells from sample groups assigned as train or test
train <- pig.outliers[pig.outliers$Sample %in% unique(pig.outliers$Sample)[train.folders == T], ]
test <- pig.outliers[pig.outliers$Sample %in% unique(pig.outliers$Sample)[train.folders == F], ]

per.sample.output <- run.profile.glm(train, test)

complete.glm.plots <- patchwork::wrap_plots(per.sample.output$by.sample, per.sample.output$predicted.samples,
                                            nrow = 1
) + plot_annotation(tag_levels = c("A"))



#save.double(complete.glm.plots, "complete.glm.png", height = 85)

# Do the abnormal predictions come down mainly to the size? Yes.
sample.prediction.plot <- ggplot(per.sample.output$test, aes(x = PredictedType, y = Area_square_pixels)) +
  geom_point(data = per.sample.output$test %>% dplyr::select(-Sample, -Type), col = "grey", size = 0.1) +
  geom_point(col = "blue", size = 0.1) +
  labs(x = "Confidence nucleus is normal", y = "Area of nucleus") +
  facet_wrap(Type ~ Sample) +
  theme_bw()

save.double(sample.prediction.plot, "sample.prediction.plot.png")


# Calculate the coefficient of variation
cv <- function(x) sd(x) / mean(x)

cv(per.sample.output$test$PredictedType)

# Are the abnormals more variable?
cv.data <- per.sample.output$test %>%
  dplyr::group_by(Sample, Type) %>%
  dplyr::summarise(CV = cv(PredictedType))

# A bit, but not really in a useful way
ggplot(cv.data, aes(x = Type, y = CV)) +
  geom_boxplot(size = 0.1) +
  geom_jitter(width = 0.1) +
  labs(x = "Sample", y = "Coefficient of variation") +
  theme_bw()


Agregated_prediction_outliers<-per.sample.output$aggregted_prediction %>% dplyr::arrange(MeanPredict)


# Performs much better - we can pretty much be sure of fails

################
# standard measuremnts used in two papers
# (Banaszewska and Andraszek, 2021)
#(Saravia et al., 2007)
# Head length (μm)	L
# Head width (μm)	W
# Head perimeter (μm)	P
# Head area (μm2)	A
# Head ellipticity	L/W
# Head elongation	(L−W)/(L+W)
# Head roughness	4π(A/P2)
# Head regularity	π(L×W/4×A)


# run glm on a training and test dataset
run.other.glm <- function(train, test) {
  # Decent numbers in split?
  print(dim(train))
  print(dim(test))

  # Equal numbers of fertile and subfertile in train?
  print(table(train$Type))
  print(table(test$Type))

  result <- list()

  all.glm <- glm(TypeInt ~ .,
                 data = train %>% select(starts_with("V"), TypeInt),
                 family = binomial(link = "logit")
  )
  plotGLM(model = all.glm)

  # predictions on the validation set
  test$PredictedType <- predict(all.glm, newdata = test, type = "response")
  train$PredictedType <- predict(all.glm, newdata = train, type = "response")

  # assigning the probability cutoff as 0.5
  test$PredictedClass <- as.factor(ifelse(test$PredictedType >= 0.5, 1, 0))
  train$PredictedClass <- as.factor(ifelse(train$PredictedType >= 0.5, 1, 0))

  sample.numbers <- test %>%
    dplyr::group_by(Sample, Type) %>%
    dplyr::summarise(Cells = n())

  by.sample <- test %>%
    dplyr::group_by(Sample, Type) %>%
    dplyr::summarise(
      MeanPredict = mean(PredictedType),
      MedianPredict = median(PredictedType)
    )

  by.sample.plot <- ggplot(test, aes(x = Sample, y = PredictedType, col = Type)) +
    geom_boxplot() +
    geom_text(data = sample.numbers, aes(x = Sample, y = -0.05, label = Cells)) +
    labs(y = "Predicted type of sample", x = "Sample") +
    theme_bw()


  pred.sample.plot <- ggplot(by.sample, aes(x = Type, y = MeanPredict)) +
    geom_hline(yintercept = 0.5) +
    geom_point() +
    labs(y = "Mean of sample", x = "Type") +
    theme_bw()

  return(list(
    "test" = test, "train" = train, "by.sample" = by.sample.plot,
    "predicted.samples" = pred.sample.plot, "model" = all.glm,
    "aggregted_prediction" = by.sample,
    "confusionMatrix" = caret::confusionMatrix(test$PredictedClass, test$TypeInt, positive = "1")
  ))
}


pig_standard_measuremnts<-as.data.frame(cbind(Sample = pig.data$Sample,pig.data$Perimeter_pixels,pig.data$Regularity,pig.data$Bounding_width_pixels,pig.data$Bounding_height_pixels,pig.data$Area_square_pixels,pig.data$Elongation,pig.data$Ellipticity,pig.data$Circularity))

# Convert all columns except the first to numeric
pig_standard_measuremnts[, -1] <- lapply(pig_standard_measuremnts[, -1], as.numeric)

pig_standard_measuremnts$Type<-pig.data$Type
pig_standard_measuremnts$TypeInt<-pig.data$TypeInt

train.folders <- sample.split(unique(pig_standard_measuremnts$Sample), SplitRatio = 0.7)


# select all cells from sample groups assigned as train or test
train <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == T], ]
test <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == F], ]

per.sample.output <- run.other.glm(train, test)

complete.glm.plots <- patchwork::wrap_plots(per.sample.output$by.sample, per.sample.output$predicted.samples,
                                            nrow = 1
) + plot_annotation(tag_levels = c("A"))



#save.double(complete.glm.plots, "complete.glm.png", height = 85)

# Do the abnormal predictions come down mainly to the size? Yes.
sample.prediction.plot <- ggplot(per.sample.output$test, aes(x = PredictedType, y = V6)) +
  geom_point(data = per.sample.output$test %>% dplyr::select(-Sample, -Type), col = "grey", size = 0.1) +
  geom_point(col = "blue", size = 0.1) +
  labs(x = "Confidence nucleus is normal", y = "Area of nucleus") +
  facet_wrap(Type ~ Sample) +
  theme_bw()

save.double(sample.prediction.plot, "sample.prediction.plot.png")


# Calculate the coefficient of variation
cv <- function(x) sd(x) / mean(x)

cv(per.sample.output$test$PredictedType)

# Are the abnormals more variable?
cv.data <- per.sample.output$test %>%
  dplyr::group_by(Sample, Type) %>%
  dplyr::summarise(CV = cv(PredictedType))

# A bit, but not really in a useful way
ggplot(cv.data, aes(x = Type, y = CV)) +
  geom_boxplot(size = 0.1) +
  geom_jitter(width = 0.1) +
  labs(x = "Sample", y = "Coefficient of variation") +
  theme_bw()


Agregated_prediction_outliers<-per.sample.output$aggregted_prediction %>% dplyr::arrange(MeanPredict)


########################################################################################################################################
Outliers4<-read.table("D:/Full_pig_project/Pig_scripts/Pig_outliers4.txt",sep = "\t",header = TRUE)

#adding our categories that are based on our detected abnormality instead of the breeder reported on that allows for a different divide based only on morphology

renamedham<-Outliers4 %>% rename(
  CellID = V1,
  Morphology_cluster = V2)

Pig_with_morphology_clusters<-merge(pig.outliers,renamedham,by = "CellID")

fertility_data <- data.frame(Sample = Pig_with_morphology_clusters$Sample, Pig_with_morphology_clusters$Morphology_cluster)

#create a new fertile/subfertile grouping from our morphology data
result <- fertility_data %>%
  group_by(Sample) %>%
  summarise(
    non_one_percentage = mean(Pig_with_morphology_clusters.Morphology_cluster != 1) * 100
  ) %>%
  mutate(
    fertility_status = ifelse(non_one_percentage > 20, paste0(Sample, "_subfertile"), paste0(Sample, "_fertile"))
  ) %>%
  select(Sample, fertility_status)

# Join the result back to the original data
Pig_with_in_house_fertility<- Pig_with_morphology_clusters %>%
  left_join(result, by = "Sample")

# # Update TypeINT based on to split datafertility_status
# Pig_with_in_house_fertility <- Pig_with_in_house_fertility %>%
#   mutate(
#     TypeInt = case_when(
#       grepl("Normal.*fertile", fertility_status) ~ 1,
#       grepl("Normal.*subfertile", fertility_status) ~ 2,
#       grepl("Abnormal.*fertile", fertility_status) ~ 3,
#       grepl("Abnormal.*subfertile", fertility_status) ~ 4,
#       TRUE ~ NA_integer_ # Add this to handle any unexpected cases
#     )
#   )
# Update TypeINT based on to split datafertility_status
Pig_with_in_house_fertility <- Pig_with_in_house_fertility %>%
  mutate(
    TypeInt = case_when(
      grepl("_fertile", fertility_status) ~ 0L,
      grepl("_subfertile", fertility_status) ~ 1L,

      TRUE ~ NA_integer_ # Add this to handle any unexpected cases
    ),
    TypeInt = factor(TypeInt, levels = c(0, 1), labels = c("0", "1"))
  )

#Run linear model on data
# dividing the samples into groups
train.folders <- sample.split(unique(Pig_with_in_house_fertility$Sample), SplitRatio = 0.7)

# select all cells from sample groups assigned as train or test
train <- Pig_with_in_house_fertility[Pig_with_in_house_fertility$Sample %in% unique(Pig_with_in_house_fertility$Sample)[train.folders == T], ]
test <- Pig_with_in_house_fertility[Pig_with_in_house_fertility$Sample %in% unique(Pig_with_in_house_fertility$Sample)[train.folders == F], ]

per.sample.output <- run.profile.glm(train, test)

complete.glm.plots <- patchwork::wrap_plots(per.sample.output$by.sample, per.sample.output$predicted.samples,
                                            nrow = 1
) + plot_annotation(tag_levels = c("A"))



#save.double(complete.glm.plots, "complete.glm.png", height = 85)

# Do the abnormal predictions come down mainly to the size? Yes.
sample.prediction.plot <- ggplot(per.sample.output$test, aes(x = PredictedType, y = Area_square_pixels)) +
  geom_point(data = per.sample.output$test %>% dplyr::select(-Sample, -Type), col = "grey", size = 0.1) +
  geom_point(col = "blue", size = 0.1) +
  labs(x = "Confidence nucleus is normal", y = "Area of nucleus") +
  facet_wrap(Type ~ Sample) +
  theme_bw()

save.double(sample.prediction.plot, "sample.prediction.plot.png")


# Calculate the coefficient of variation
cv <- function(x) sd(x) / mean(x)

cv(per.sample.output$test$PredictedType)

# Are the abnormals more variable?
cv.data <- per.sample.output$test %>%
  dplyr::group_by(Sample, Type) %>%
  dplyr::summarise(CV = cv(PredictedType))

# A bit, but not really in a useful way
ggplot(cv.data, aes(x = Type, y = CV)) +
  geom_boxplot(size = 0.1) +
  geom_jitter(width = 0.1) +
  labs(x = "Sample", y = "Coefficient of variation") +
  theme_bw()


Agregated_prediction_outliers<-per.sample.output$aggregted_prediction %>% dplyr::arrange(MeanPredict)

##############################################################################################################
# standard data Our split


renamedham<-Outliers4 %>% rename(
  CellID = V1,
  Morphology_cluster = V2)

Pig_with_morphology_clusters<-merge(pig.outliers,renamedham,by = "CellID")

fertility_data <- data.frame(Sample = Pig_with_morphology_clusters$Sample, Pig_with_morphology_clusters$Morphology_cluster)

#create a new fertile/subfertile grouping from our morphology data
result <- fertility_data %>%
  group_by(Sample) %>%
  summarise(
    non_one_percentage = mean(Pig_with_morphology_clusters.Morphology_cluster != 1) * 100
  ) %>%
  mutate(
    fertility_status = ifelse(non_one_percentage > 20, paste0(Sample, "_subfertile"), paste0(Sample, "_fertile"))
  ) %>%
  select(Sample, fertility_status)

# Join the result back to the original data
Pig_with_in_house_fertility<- Pig_with_morphology_clusters %>%
  left_join(result, by = "Sample")

# # Update TypeINT based on to split datafertility_status
# Pig_with_in_house_fertility <- Pig_with_in_house_fertility %>%
#   mutate(
#     TypeInt = case_when(
#       grepl("Normal.*fertile", fertility_status) ~ 1,
#       grepl("Normal.*subfertile", fertility_status) ~ 2,
#       grepl("Abnormal.*fertile", fertility_status) ~ 3,
#       grepl("Abnormal.*subfertile", fertility_status) ~ 4,
#       TRUE ~ NA_integer_ # Add this to handle any unexpected cases
#     )
#   )
# Update TypeINT based on to split datafertility_status
Pig_with_in_house_fertility <- Pig_with_in_house_fertility %>%
  mutate(
    TypeInt = case_when(
      grepl("_fertile", fertility_status) ~ 0L,
      grepl("_subfertile", fertility_status) ~ 1L,

      TRUE ~ NA_integer_ # Add this to handle any unexpected cases
    ),
    TypeInt = factor(TypeInt, levels = c(0, 1), labels = c("0", "1"))
  )



pig_standard_measuremnts<-as.data.frame(cbind(Sample = Pig_with_in_house_fertility$Sample,Pig_with_in_house_fertility$Perimeter_pixels,Pig_with_in_house_fertility$Regularity,Pig_with_in_house_fertility$Bounding_width_pixels,Pig_with_in_house_fertility$Bounding_height_pixels,Pig_with_in_house_fertility$Area_square_pixels,Pig_with_in_house_fertility$Elongation,Pig_with_in_house_fertility$Ellipticity,Pig_with_in_house_fertility$Circularity))

# Convert all columns except the first to numeric
pig_standard_measuremnts[, -1] <- lapply(pig_standard_measuremnts[, -1], as.numeric)

pig_standard_measuremnts$Type<-Pig_with_in_house_fertility$Type
pig_standard_measuremnts$TypeInt<-Pig_with_in_house_fertility$TypeInt


train.folders <- sample.split(unique(pig_standard_measuremnts$Sample), SplitRatio = 0.7)


# select all cells from sample groups assigned as train or test
train <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == T], ]
test <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == F], ]

per.sample.output <- run.other.glm(train, test)

complete.glm.plots <- patchwork::wrap_plots(per.sample.output$by.sample, per.sample.output$predicted.samples,
                                            nrow = 1
) + plot_annotation(tag_levels = c("A"))



#save.double(complete.glm.plots, "complete.glm.png", height = 85)

# Do the abnormal predictions come down mainly to the size? Yes.
sample.prediction.plot <- ggplot(per.sample.output$test, aes(x = PredictedType, y = V6)) +
  geom_point(data = per.sample.output$test %>% dplyr::select(-Sample, -Type), col = "grey", size = 0.1) +
  geom_point(col = "blue", size = 0.1) +
  labs(x = "Confidence nucleus is normal", y = "Area of nucleus") +
  facet_wrap(Type ~ Sample) +
  theme_bw()

save.double(sample.prediction.plot, "sample.prediction.plot.png")


# Calculate the coefficient of variation
cv <- function(x) sd(x) / mean(x)

cv(per.sample.output$test$PredictedType)

# Are the abnormals more variable?
cv.data <- per.sample.output$test %>%
  dplyr::group_by(Sample, Type) %>%
  dplyr::summarise(CV = cv(PredictedType))

# A bit, but not really in a useful way
ggplot(cv.data, aes(x = Type, y = CV)) +
  geom_boxplot(size = 0.1) +
  geom_jitter(width = 0.1) +
  labs(x = "Sample", y = "Coefficient of variation") +
  theme_bw()


Agregated_prediction_outliers<-per.sample.output$aggregted_prediction %>% dplyr::arrange(MeanPredict)
