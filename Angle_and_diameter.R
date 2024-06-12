#line graphs
diameter_data <- pig_data %>% dplyr::select(starts_with("Diameter_profile_"))
#diameters
D1 <- diameter_data/15.33
d1 <- apply(D1[1:100], 2, median)
d2 <- apply(D1[1:100], 2, quantile, probs = 0.25)
d3 <- apply(D1[1:100], 2, quantile, probs = 0.75)

# Combine the matrices for each cluster into a list
d <- list(median = d1, Q25 = d2, Q75 = d3)
# Convert the list into a data frame
d_df <- as.data.frame(d)
# Add an index column
d_df$index <- 1:100
# Create the ggplot plot
Diameter_graph<-ggplot(d_df, aes(x = index, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.5) +
  labs(title, x = "Index", y = "Diameter (µm)")+
  theme_bw()

#angles
angle_data <- pig_data %>% dplyr::select(starts_with("Angle_profile_"))
A1 <- angle_data
a1 <- apply(A1[1:100], 2, median)
a2 <- apply(A1[1:100], 2, quantile, probs = 0.25)
a3 <- apply(A1[1:100], 2, quantile, probs = 0.75)

# Combine the matrices for each cluster into a list
a <- list(median = a1, Q25 = a2, Q75 = a3)
# Convert the list into a data frame
a_df <- as.data.frame(a)
# Add an index column
a_df$index <- 1:100
# Create the ggplot plot
Angle_graph<-ggplot(a_df, aes(x = index, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.5) +
  labs(title, x = "Index", y = "Angle (°)")+
  theme_bw()
Figure1_graph<-Angle_graph/Diameter_graph
Figure1_graph
#ggsave 125mm by 112mm
ggsave("figures/Figure1_angle_graph.png",plot = Figure1_graph,width = 125,height = 112,units = "mm")

