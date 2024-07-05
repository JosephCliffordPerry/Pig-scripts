
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
    geom_point() +
    labs(y = "Mean of sample predicted fertile", x = "Type") +
    theme_bw()

  return(list(
    "test" = test, "train" = train, "by.sample" = by.sample.plot,
    "predicted.samples" = pred.sample.plot, "model" = all.glm,
    "aggregted_prediction" = by.sample,
    "confusionMatrix" = caret::confusionMatrix(test$PredictedClass, test$TypeInt, positive = "1")
  ))
}



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
    geom_point() +
    labs(y = "Mean of sample predicted fertile", x = "Type") +
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
set.seed(08000)
# detection of pig outliers

######################## Read data #############################################

# Data from reanalysis of raw images - 21759 nuclei
# Exported from 2.2.0 so diameter profiles are absolute

pig.data <- pig_data %>%
  tidyr::separate_wider_delim(Centre_of_mass, " - ", names = c("CoMX", "CoMY")) %>%
    dplyr::mutate(
    Type = stringr::str_extract(Folder, "(sub)?fertile"),
    Type = ifelse(Type == "fertile", "Normal", "Abnormal"),
    CoMX = as.numeric(CoMX), CoMY = as.numeric(CoMY),
    TypeInt = as.factor(ifelse(Type == "Normal", 1, 0)))
folders <- unique(pig.data$Folder)
pig.data$Sample <- paste0(pig.data$Type, "_", sapply(pig.data$Folder, function(f) which(folders == f)))

pig.data <- remove.poor.edge.detection(pig.data) # removes a few hundred

#### Targeted GLMs on angle radius and diameter outliers using the outlier matrix####
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

Aper.sample.output <- run.profile.glm(train, test)


####standard measurement model####
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



pig_standard_measuremnts<-as.data.frame(cbind(sample = pig.data$Sample,perimeter_pixels = pig.data$Perimeter_pixels,Regularity = pig.data$Regularity,Bounding_width_pixels = pig.data$Bounding_width_pixels,Bounding_height_pixels = pig.data$Bounding_height_pixels,Area_square_pixels = pig.data$Area_square_pixels,Elongation = pig.data$Elongation,Ellipticity = pig.data$Ellipticity,Circularity = pig.data$Circularity))

# Convert all columns except the first to numeric
pig_standard_measuremnts[, -1] <- lapply(pig_standard_measuremnts[, -1], as.numeric)

pig_standard_measuremnts$Type<-pig.data$Type
pig_standard_measuremnts$TypeInt<-pig.data$TypeInt

train.folders <- sample.split(unique(pig_standard_measuremnts$Sample), SplitRatio = 0.7)


# select all cells from sample groups assigned as train or test
train <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == T], ]
test <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == F], ]

Bper.sample.output <- run.other.glm(train, test)

########################################################################################################################################
#Outliers4<-read.table("D:/Full_pig_project/Pig_scripts/Pig_outliers4.txt",sep = "\t",header = TRUE)

#adding our categories that are based on our detected abnormality instead of the breeder reported on that allows for a different divide based only on morphology

renamedham<-as.data.frame(Outliers2) %>% rename(
  CellID = V1,
  Morphology_cluster = V2)

Pig_with_morphology_clusters<-merge(pig.data,renamedham,by = "CellID")

fertility_data <- data.frame(Sample = Pig_with_morphology_clusters$Sample, Pig_with_morphology_clusters$Morphology_cluster)

#create a new fertile/subfertile grouping from our morphology data
result <- fertility_data %>%
  group_by(Sample) %>%
  summarise(
    non_one_percentage = mean(Pig_with_morphology_clusters.Morphology_cluster != 1) * 100
  ) %>%
  mutate(
    fertility_status = ifelse(non_one_percentage > 20, paste0(Sample, "_Subfertile"), paste0(Sample, "_Fertile"))
  ) %>%
  select(Sample, fertility_status)

# Join the result back to the original data
Pig_with_in_house_fertility<- Pig_with_morphology_clusters %>%
  left_join(result, by = "Sample")


# Update TypeINT based on to split datafertility_status
Pig_with_in_house_fertility <- Pig_with_in_house_fertility %>%
  mutate(
    TypeInt = case_when(
      grepl("_Fertile", fertility_status) ~ 1L,
      grepl("_Subfertile", fertility_status) ~ 0L,

      TRUE ~ NA_integer_ # Add this to handle any unexpected cases
    ),
      Type  = case_when(
        grepl("_Fertile", fertility_status) ~ "Fertile",
        grepl("_Subfertile", fertility_status) ~ "Subfertile",

        TRUE ~ NA_character_ # Add this to handle any unexpected cases
    ),
    TypeInt = factor(TypeInt, levels = c(0, 1), labels = c("1", "0"))
  )
# Remove numbers from the string
Fertility_change <- gsub("[0-9]", "", unique(Pig_with_in_house_fertility$fertility_status) )
table(Fertility_change)
# #Run linear model on data
# # dividing the samples into groups
# train.folders <- sample.split(unique(Pig_with_in_house_fertility$Sample), SplitRatio = 0.7)
#
# # select all cells from sample groups assigned as train or test
# train <- Pig_with_in_house_fertility[Pig_with_in_house_fertility$Sample %in% unique(Pig_with_in_house_fertility$Sample)[train.folders == T], ]
# test <- Pig_with_in_house_fertility[Pig_with_in_house_fertility$Sample %in% unique(Pig_with_in_house_fertility$Sample)[train.folders == F], ]
#
# Cper.sample.output <- run.profile.glm(train, test)
#
# ##############################################################################################################
# # standard data Our split
#
# #Run pig with in house fertility firstPig_with_in_house_fertility
#
# pig_standard_measuremnts<-as.data.frame(cbind(Sample = Pig_with_in_house_fertility$Sample,Pig_with_in_house_fertility$Perimeter_pixels,Pig_with_in_house_fertility$Regularity,Pig_with_in_house_fertility$Bounding_width_pixels,Pig_with_in_house_fertility$Bounding_height_pixels,Pig_with_in_house_fertility$Area_square_pixels,Pig_with_in_house_fertility$Elongation,Pig_with_in_house_fertility$Ellipticity,Pig_with_in_house_fertility$Circularity))
#
# # Convert all columns except the first to numeric
# pig_standard_measuremnts[, -1] <- lapply(pig_standard_measuremnts[, -1], as.numeric)
#
# pig_standard_measuremnts$Type<-Pig_with_in_house_fertility$Type
# pig_standard_measuremnts$TypeInt<-Pig_with_in_house_fertility$TypeInt
#
#
# train.folders <- sample.split(unique(pig_standard_measuremnts$Sample), SplitRatio = 0.7)
#
#
# # select all cells from sample groups assigned as train or test
# train <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == T], ]
# test <- pig_standard_measuremnts[pig_standard_measuremnts$Sample %in% unique(pig_standard_measuremnts$Sample)[train.folders == F], ]
#
# Dper.sample.output <- run.other.glm(train, test)
#
# #####################

#Format stitch of the 4 graphs

A<-Aper.sample.output[["predicted.samples"]]
B<-Bper.sample.output[["predicted.samples"]]
C<-Cper.sample.output[["predicted.samples"]]
D<-Dper.sample.output[["predicted.samples"]]

# Example for changing X-axis labels
A2 <-A+
  labs(
    x = "Breeder Reported Fertility",   # Change X-axis label
    title = "A"  # Change plot title
  ) +
  scale_x_discrete(labels = c("Subfertile", "Fertile"))+  # Change X-axis labels
scale_y_continuous(limits = c(0, 1))

B2 <-B+
  labs(
    x = "Breeder Reported Fertility",   # Change X-axis label
    title = "B"  # Change plot title
  ) +
  scale_x_discrete(labels = c("Subfertile", "Fertile"))+  # Change X-axis labels
scale_y_continuous(limits = c(0, 1))

#
# C2 <-C+
#   labs(
#     x = "20% Abnormality Threshold",   # Change X-axis label
#     title = "C"  # Change plot title
#   )+
# scale_y_continuous(limits = c(0, 1))
#
# # Convert the character variable to a factor with reversed levels
# C2$data$Type <- factor(C2$data$Type, levels = rev(unique(C2$data$Type)))
# # Update the plot with the new factor variable and reversed x-axis
# C2 <- C2 + scale_x_discrete(limits = rev(levels(C2$data$Type)))
# # D2 <-D+
# #   labs(
# #     x = "20% Abnormality Fertility Threshold",   # Change X-axis label
# #     title = "D"  # Change plot title
# #   )+
# # scale_y_continuous(limits = c(0, 1))


Patch<- A2 + B2
Patch
ggsave(filename = "figures/GLM_graph.png",width = 180,height = 180,units = "mm",dpi = 300)

