# common functions


# Calculate the median profile for the given profile type
# data - a data frame with profile columns
# profile.type - a string. One of (Angle, Diameter, Radius)
# group_col - a string with the name of a grouping column
calculate.median.profile <- function(data, profile.type, group_col) {
  group_col <- ensym(group_col)
  data %>%
    dplyr::select(group_col, matches(paste0(profile.type, "_profile_\\d+$"))) %>%
    tidyr::pivot_longer(-{{ group_col }}, names_to = "Profile_position", values_to = "Value") %>%
    dplyr::group_by(Profile_position, {{ group_col }}) %>%
    dplyr::mutate(
      Position = as.numeric(gsub(paste0(profile.type, "_profile_"), "", Profile_position)),
      Median = median(Value),
      Q25 = quantile(Value, 0.25),
      Q75 = quantile(Value, 0.75)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Value, -Profile_position) %>%
    dplyr::distinct()
}

# data - a data frame with profile columns
# profile.type - a string. One of (Angle, Diameter, Radius)
run.umap.on.profile <- function(data, profile.type) {
  set.seed(42)
  input <- data %>% dplyr::select(contains(paste0(profile.type, "_profile_")))
  rownames(input) <- data$CellID

  umap.out <- umap::umap(as.matrix(input))
  as.data.frame(umap.out$layout)
}

read.nma.stats <- function(file) {
  data <- fread(file, header = T) %>%
    tidyr::separate_wider_delim(Centre_of_mass, " - ", names = c("CoMX", "CoMY")) %>%
    dplyr::mutate(
      Type = stringr::str_extract(Folder, "(sub)?fertile"),
      Type = ifelse(Type == "fertile", "Normal", "Abnormal"),
      CoMX = as.numeric(CoMX), CoMY = as.numeric(CoMY),
      TypeInt = as.factor(ifelse(Type == "Normal", 1, 0))
    )

  folders <- unique(data$Folder)

  # Convert folder to a sample id
  data$Sample <- paste0(data$Type, "_", sapply(data$Folder, function(f) which(folders == f)))
  data
}

# umap.data - the data frame with V1 and V2 coordinates
# full.data - the data from which the umap was constructed
# group_col - a string with the name of a grouping column
plot.umap <- function(umap.data, full.data, group_col) {
  ggplot(umap.data, aes(x = V1, y = V2, col = full.data[[group_col]])) +
    geom_point(size = 0.1) +
    labs(col = group_col) +
    theme_bw() +
    theme(legend.position = "top")
}

# profile.data - the data frame with profile coordinates
# group_col - a string with the name of a grouping column
plot.profile <- function(profile.data, group_col) {
  ggplot(profile.data, aes(x = Position, y = Median, fill = .data[[group_col]])) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.5) +
    geom_line(aes(col = .data[[group_col]])) +
    labs(x = "Position") +
    theme_classic() +
    theme(legend.position = "top")
}


# Given a data frame with one or more nuclei, create a consensus
# nucleus from coordinate columns
make.consensus <- function(data) {
  xcoords <- data %>% dplyr::select(starts_with("Outline_OrientedCoordinates_X"))
  ycoords <- data %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y"))

  data.frame(
    "xmeans" = apply(xcoords, 2, mean),
    "ymeans" = apply(ycoords, 2, mean)
  )
}

# Plot a consensus nucleus for the given data
# consensus - the data frame with xy coordinates for the consensus
# colour - the colour for the consensus
# alpha - the transparency
# showPoints - T/F display point are each XY coordinate
# showLines - T/F display lines connecting XY coordinates
# showFill - T/F fill the consensus with the colour
# sideBySide - T/F if true, plot each nucleus in a separate plot, else overlay
# ... - other arguments to pass to geoms e.g linewidth, size
plot.consensus <- function(consensus, colours, alpha = 1, showPoints = F,
                           showLines = F, showFill = T, sideBySide = F,
                           xmax = 0, xmin = 0, ymax = 0, ymin = 0, ...) {

  # if separate plots requested and we have multiple consensus nuclei
  if (sideBySide && !is.data.frame(consensus) && length(consensus) > 1) { # plot the nuclei in separate plots

    # Determine the max plot dimensions we need
    xmax <- ifelse(xmax == 0, max(sapply(consensus, function(d) max(d$xmeans))), xmax)
    xmin <- ifelse(xmin == 0, min(sapply(consensus, function(d) min(d$xmeans))), xmin)
    ymax <- ifelse(ymax == 0, max(sapply(consensus, function(d) max(d$ymeans))), ymax)
    ymin <- ifelse(ymin == 0, min(sapply(consensus, function(d) min(d$ymeans))), ymin)


    p <- plot.consensus(consensus[[1]], colours[1], alpha, showPoints, showLines, showFill,
                        sideBySide = F, xmax, xmin, ymax, ymin, ...
    )
    for (i in 2:length(consensus)) {
      p <- p + plot.consensus(consensus[[i]], colours[i], alpha, showPoints, showLines, showFill,
                              sideBySide = F, xmax, xmin, ymax, ymin, ...
      )
    }
    return(p)
  }

  p <- ggplot() # initialise blank plot

  add.consensus <- function(data, colour) {
    if (showFill) p <- p + geom_polygon(data = data, fill = colour, alpha = alpha, aes(x = xmeans, y = ymeans), ...)
    if (showLines) p <- p + geom_polygon(data = data, col = colour, fill = NA, aes(x = xmeans, y = ymeans), ...)
    if (showPoints) p <- p + geom_point(data = data, col = colour, aes(x = xmeans, y = ymeans), ...)
    p
  }

  if (is.data.frame(consensus)) { #  single consensus
    p <- add.consensus(consensus, colours)
  } else { # list of consensus data frames

    # Determine the max plot dimensions we need
    xmax <- ifelse(xmax == 0, max(sapply(consensus, function(d) max(d$xmeans))), xmax)
    xmin <- ifelse(xmin == 0, min(sapply(consensus, function(d) min(d$xmeans))), xmin)
    ymax <- ifelse(ymax == 0, max(sapply(consensus, function(d) max(d$ymeans))), ymax)
    ymin <- ifelse(ymin == 0, min(sapply(consensus, function(d) min(d$ymeans))), ymin)

    for (i in 1:length(consensus)) { #  add each consensus in turn
      p <- add.consensus(consensus[[i]], colours[i])
    }
  }

  p +
    coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = TRUE) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
}
