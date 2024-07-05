# Test breed/date associations with sperm phenotype
library(tidyverse)
library(data.table)
library(patchwork)
library(MASS)
library(brant)
library(broom)
library(forcats)

# Save the given plot to the figures dir
save.ggplot <- function(plot, name, width=170, height=170) {
  ggsave(paste0("figures/", name), plot, dpi = 300, units = "mm", width = width, height = height)
}

# Read the pig data and assign cluster names & data types
pig.data <- data.table::fread("data/data_with_date_and_clusters.txt", sep = "\t") %>%
  dplyr::mutate(
    # The cluster is an discrete ordinal response, since there is a clear severity hierarchy
    # Set the factor to be ordered for downstream regression
    Cluster = ordered(factor(case_when(Morphology_cluster == 1 ~ "Normal",
                                       Morphology_cluster == 2 ~ "Intermediate",
                                       Morphology_cluster == 3 ~ "Extreme"),
                             levels = c("Normal", "Intermediate", "Extreme"))),

    # lubridate to get clean date formatting, then convert to factor so this is
    # not interpreted as numerical in models
    SampleCollectionDate = as.factor(lubridate::dmy(date)),

    # Since we have only one sample for each named breed, we can use
    # sample number as a proxy for the breed also
    Sample = paste0(Breed, " (s", str_extract(sample_number, "^\\d+"), ")"))

# Split to those with and without breed info
pig.known.breed   <- pig.data %>% dplyr::filter(Breed != "Other")
pig.unknown.breed <- pig.data %>% dplyr::filter(Breed == "Other")

# Basic visual exploration of the data - what are we expecting?
pig.data.summary <- pig.data %>%
  dplyr::group_by(Breed, SampleCollectionDate, Cluster) %>%
  dplyr::summarise(Count = n())

ggplot(pig.data.summary, aes(x = Breed, y=Count, fill = Cluster, col=SampleCollectionDate)) +
  geom_col()+
  ggplot(pig.data.summary, aes(x = SampleCollectionDate,  y=Count, fill = Cluster, col=Breed)) +
  geom_col() +
  patchwork::plot_annotation(tag_levels = c("A")) +
  patchwork::plot_layout(guides = "collect")


# Half the cells have a known breed
# Fairly even balance of cells across the known breeds
# Some dates do not have breeds - be cautious with interaction terms

# Quick test with Scheirer-Ray-Hare (non-parametric two factor anova equivalent)
srh <- rcompanion::scheirerRayHare(Cluster ~ Sample*SampleCollectionDate,
                                   data = pig.data)
# Shows that both sample, date and the interaction are significant
# Can we delve more into this with a more focussed dataset?

# Run proportional odds logistic regression. Suitable when dealing with ordinal
# discrete response variables. See
# https://bookdown.org/chua/ber642_advanced_regression/ordinal-logistic-regression.html
# for an overview of OLR.
# Returns a list containing the model, a tidied version for writing to file, and
# some validation tests
run.mass.polr <- function(data, formula){
  # Run regression. Using negative log-log link because lower ranked responses are
  # more common. Hessian matrix included for summary info.
  model <- MASS::polr(formula, data = data, Hess=TRUE, method = "loglog")

  # Is the proportional odds assumption valid? Are we entitled to use this model
  # on our data? Check for deviations from proportional odds for each regressor
  # using Brant-Wald test. All values should give p>0.05 i.e. H0 (proportional
  # odds) holds. Omnibus tests the global model, other rows test each regressor.
  brant.data <- brant::brant(model)

  # Run intercept-only regression - no independent variables included
  intercept.only.model <- MASS::polr(Cluster ~ 1, data = data, method = "loglog")

  # Compare the models. Can we reject the null hypothesis that the model without
  # predictors is as good as the model with the predictors?
  model.comparison <- anova(intercept.only.model,model)

  # Calculate confidence intervals
  # confidence.intervals <- confint(model)

  # Calculate p values since they are not automatically included from polr
  tidy.model <- cbind(broom::tidy(model),
                      p.value = pnorm(abs(broom::tidy(model)$statistic),
                                      lower.tail = FALSE) * 2,
                      # Interpret coefficients - what effect does a unit change in this
                      # parameter make to the response variable relative to the reference?
                      exp.estimate = exp(broom::tidy(model)$estimate))

  # Add a significance *** column for convenience
  tidy.model$significance <- case_when(tidy.model$p.value < 0.001 ~ "***",
                                       tidy.model$p.value < 0.01 ~ "**",
                                       tidy.model$p.value < 0.05 ~ "*",
                                       .default = "")

  list( full.model = model,
        tidy.model = tidy.model,
        brant.data = brant.data,
        intercept.only.model = intercept.only.model,
        # confidence.intervals = confidence.intervals,
        model.comparison = model.comparison)
}

# Is there an interaction between sample and date?
# We will need all tested combinations of sample and date to have values present
# Subset data to the dates which have values for all of the samples
dates.have.samples <- apply(
  table(pig.known.breed$Sample, pig.known.breed$SampleCollectionDate), # pairwise counts
  2, all  # are all values in each column non-zero?
)

# Now get the matching dates
dates.with.all.samples <- names(dates.have.samples[dates.have.samples])

pig.filt <- pig.known.breed %>%
  dplyr::filter( SampleCollectionDate %in% dates.with.all.samples) %>%
  # drop missing dates from the factor otherwise the tests will complain about missing data
  dplyr::mutate(SampleCollectionDate = forcats::fct_drop(SampleCollectionDate))

# Test interaction between sample and date for only dates present in all samples
sample.model <- run.mass.polr(pig.filt, "Cluster ~ Sample * SampleCollectionDate")
write_tsv(sample.model$tidy.model, file = "figures/olr_results.tsv")


# Inspect the tables within sample.model for interpretation. Example:

# For cells of White Duroc, the odds that the cell is extreme versus
# intermediate or normal increases by 2.1 times (when compared to cells not from
# White Duroc, and holding sample collection date constant)

# For cells of White Duroc, the odds that the cell is extreme or
# intermediate versus normal increases by 2.1 times (when compared to cells not from
# White Duroc, and holding sample collection date constant)


# Predominant date effect from 2016-07-06. What is happening?
ggplot(pig.data, aes(x = SampleCollectionDate, fill = Cluster, alpha = Cluster))+
  geom_bar(position = "fill")+
  scale_fill_manual( values = c("#7eb17e", "#2e8f3d", "#19563e"))+
  scale_alpha_manual(values = c("Normal" = 0, "Intermediate" = 1, "Extreme"=1))+
  coord_cartesian(ylim = c(0, 0.5))+
  labs(y = "Proportion of sample")+
  facet_wrap(~Breed)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.key = element_rect(colour = "black"),
        legend.position = "top")

save.ggplot(last_plot(), "Figure_Sxxxx_Breed_clusters.png")

ggplot(pig.data, aes(x = Sample, fill = Cluster, alpha = Cluster))+
  geom_bar(position = "fill")+
  scale_fill_manual( values = c("#7eb17e", "#2e8f3d", "#19563e"))+
  scale_alpha_manual(values = c("Normal" = 0, "Intermediate" = 1, "Extreme"=1))+
  coord_cartesian(ylim = c(0, 0.5))+
  labs(y = "Proportion of sample")+
  facet_wrap(~SampleCollectionDate)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.key = element_rect(colour = "black"),
        legend.position = "top")

save.ggplot(last_plot(), "Figure_Sxxxx_Sample_clusters.png")

