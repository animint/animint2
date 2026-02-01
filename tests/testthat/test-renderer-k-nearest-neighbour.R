acontext("Proximity kNN Animation")

library(dplyr)
library(animint2)
library(XML)

# Helper Function: kNN Animint
knn_animint <- function(train, test, class.vec, k = 30,
                        interval = 1000,
                        out.dir = "knn_animint",
                        github_repo = NULL) {
  # 1. Prepare Data
  train.df <- as.data.frame(train)
  colnames(train.df) <- c("x", "y")
  train.df$class <- as.character(class.vec)
  train.df$train.id <- seq_len(nrow(train.df))

  test.df <- as.data.frame(test)
  colnames(test.df) <- c("x", "y")
  n.test <- nrow(test.df)

  # k sequence
  if (k <= 5) {
    k.sequence <- 1:k
  } else {
    k.sequence <- seq(1, min(10, k), by = 1)
    if (k >= 15) {
      k.sequence <- c(k.sequence, seq(15, k, by = 5))
    }
    if (!k %in% k.sequence) k.sequence <- c(k.sequence, k)
    k.sequence <- sort(unique(k.sequence))
  }

  # 2. Build Animation Frames
  all.data.list <- list()
  frame.id <- 1

  for (test.idx in 1:n.test) {
    test.point <- test.df[test.idx, ]

    # Calculate distances
    distances <- sqrt((train.df$x - test.point$x)^2 + (train.df$y - test.point$y)^2)
    train.sorted <- train.df
    train.sorted$distance <- distances
    train.sorted <- train.sorted[order(distances), ]
    train.sorted$rank <- seq_len(nrow(train.sorted))

    for (k.val in k.sequence) {
      train.sorted$is.knn <- train.sorted$rank <= k.val

      # Votes
      knn.subset <- train.sorted[train.sorted$is.knn, ]
      votes <- knn.subset %>%
        group_by(class) %>%
        dplyr::summarise(votes = dplyr::n(), .groups = "drop")

      predicted <- votes %>%
        arrange(desc(votes)) %>%
        slice(1) %>%
        pull(class)

      # Training points with aesthetic variables
      train.frame <- train.sorted %>%
        mutate(
          frame.id = frame.id,
          k = k.val,
          test.idx = test.idx,
          is.neighbor = is.knn,
          # Visual properties based on neighbor status
          point.alpha = ifelse(is.knn, 1.0, 0.3),
          point.stroke = ifelse(is.knn, 1.2, 0.5)
        )

      # Lines to k-nearest neighbors
      lines.frame <- train.sorted %>%
        filter(is.knn) %>%
        mutate(
          x.start = test.point$x,
          y.start = test.point$y,
          x.end = x,
          y.end = y,
          frame.id = frame.id,
          line.group = paste0("test", test.idx, "_train", train.id)
        )

      # Convex hull polygon
      polygon.frame <- data.frame()
      if (sum(train.sorted$is.knn) >= 3) {
        knn.points <- train.sorted[train.sorted$is.knn, ]
        hull.idx <- chull(knn.points$x, knn.points$y)
        polygon.frame <- knn.points[hull.idx, c("x", "y")]
        polygon.frame$frame.id <- frame.id
        polygon.frame$poly.group <- paste0("test", test.idx, "_k", k.val)
      }

      # Circle at k-th neighbor
      radius <- train.sorted$distance[k.val]
      angles <- seq(0, 2 * pi, length.out = 100)
      circle.frame <- data.frame(
        x = test.point$x + radius * cos(angles),
        y = test.point$y + radius * sin(angles),
        frame.id = frame.id,
        circle.group = paste0("test", test.idx, "_k", k.val)
      )

      # Current test point
      test.current <- data.frame(
        x = test.point$x,
        y = test.point$y,
        frame.id = frame.id
      )

      # Previously classified
      prev.classified <- data.frame()
      if (test.idx > 1) {
        for (prev.idx in 1:(test.idx - 1)) {
          prev.point <- test.df[prev.idx, ]
          prev.dist <- sqrt((train.df$x - prev.point$x)^2 + (train.df$y - prev.point$y)^2)
          prev.sorted <- train.df[order(prev.dist), ]
          prev.knn <- prev.sorted[1:k, ]
          prev.vote <- prev.knn %>%
            group_by(class) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
            arrange(desc(n)) %>%
            slice(1)

          prev.classified <- rbind(prev.classified, data.frame(
            x = prev.point$x,
            y = prev.point$y,
            class = prev.vote$class,
            frame.id = frame.id
          ))
        }
      }

      # Future unclassified
      future.points <- data.frame()
      if (test.idx < n.test) {
        future.points <- test.df[(test.idx + 1):n.test, ] %>%
          mutate(frame.id = frame.id)
      }

      # Votes
      votes.frame <- votes %>%
        mutate(frame.id = frame.id, k = k.val)

      # Title
      title.text <- sprintf(
        "Test Point %d/%d | k = %d | Predicted: %s",
        test.idx, n.test, k.val, predicted
      )

      all.data.list[[frame.id]] <- list(
        training = train.frame,
        lines = lines.frame,
        polygon = polygon.frame,
        circle = circle.frame,
        test_current = test.current,
        test_prev = prev.classified,
        test_future = future.points,
        votes = votes.frame,
        title = data.frame(
          x = mean(range(train.df$x)),
          y = max(train.df$y) + 0.8,
          label = title.text,
          frame.id = frame.id
        )
      )

      frame.id <- frame.id + 1
    }
  }

  # 3. Combine Data

  extract_element <- function(element.name) {
    bind_rows(lapply(all.data.list, function(x) x[[element.name]]))
  }

  training.all <- extract_element("training")
  lines.all <- extract_element("lines")
  polygon.all <- extract_element("polygon")
  circle.all <- extract_element("circle")
  test.current.all <- extract_element("test_current")
  test.prev.all <- extract_element("test_prev")
  test.future.all <- extract_element("test_future")
  votes.all <- extract_element("votes")
  title.all <- extract_element("title")

  # 4. Color Scheme
  unique.classes <- unique(class.vec)
  n.classes <- length(unique.classes)

  # Constants mapping
  class.colors <- c("#4472C4", "#70AD47", "#FFC000", "#C00000")[1:n.classes]
  names(class.colors) <- unique.classes

  class.shapes <- c(1, 2, 0, 5)[1:n.classes]
  names(class.shapes) <- unique.classes

  # Calculate proper axis ranges
  x.range <- range(c(train.df$x, test.df$x))
  y.range <- range(c(train.df$y, test.df$y))
  x.padding <- diff(x.range) * 0.1
  y.padding <- diff(y.range) * 0.1

  # 5. Create Plots

  # Main Feature Space Plot
  p.feature <- ggplot() +
    geom_polygon(
      data = polygon.all,
      aes(x = x, y = y, group = poly.group),
      fill = "#FFFFCC",
      alpha = 0.4,
      showSelected = "frame.id"
    ) +
    geom_path(
      data = circle.all,
      aes(x = x, y = y, group = circle.group),
      color = "gray40",
      linetype = "dashed",
      size = 1,
      showSelected = "frame.id"
    ) +
    geom_segment(
      data = lines.all,
      aes(x = x.start, y = y.start, xend = x.end, yend = y.end, group = line.group),
      color = "gray60",
      linetype = "dotted",
      size = 0.8,
      alpha = 0.6,
      showSelected = "frame.id"
    ) +
    # Combined Training Points (Neighbors and Non-neighbors)
    # Using aesthetic mapping for alpha and stroke to differentiate
    geom_point(
      data = training.all,
      aes(
        x = x, y = y, fill = class,
        alpha = point.alpha, stroke = I(point.stroke)
      ),
      shape = 21,
      color = "gray50",
      size = 2.5,
      showSelected = "frame.id"
    ) +
    scale_alpha_identity() + # Use literal values from data for alpha
    geom_point(
      data = test.prev.all,
      aes(x = x, y = y, fill = class),
      shape = 21,
      color = "black",
      size = 2.5,
      alpha = 1.0,
      stroke = 1.5,
      showSelected = "frame.id"
    ) +
    geom_text(
      data = test.future.all,
      aes(x = x, y = y),
      label = "?",
      color = "#8B4513",
      size = 8,
      fontface = "bold",
      vjust = 0,
      showSelected = "frame.id"
    ) +
    geom_point(
      data = test.current.all,
      aes(x = x, y = y),
      shape = 21,
      fill = "#FF0000",
      color = "black",
      size = 3,
      alpha = 1.0,
      showSelected = "frame.id"
    ) +
    geom_point(
      data = test.current.all,
      aes(x = x, y = y),
      shape = 21,
      fill = "transparent",
      color = "#FF0000",
      size = 4,
      stroke = 2,
      showSelected = "frame.id"
    ) +
    geom_text(
      data = title.all,
      aes(x = x, y = y, label = label),
      size = 5,
      fontface = "bold",
      vjust = 0,
      showSelected = "frame.id"
    ) +
    scale_fill_manual(values = class.colors, name = "Class") +
    theme_animint(width = 900, height = 700) +
    scale_x_continuous(
      breaks = pretty(x.range, n = 10),
      limits = c(x.range[1] - x.padding, x.range[2] + x.padding)
    ) +
    scale_y_continuous(
      breaks = pretty(y.range, n = 10),
      limits = c(y.range[1] - y.padding, y.range[2] + y.padding)
    ) +
    coord_fixed() +
    labs(x = "x1", y = "x2") +
    theme_bw() +
    theme(
      legend.position = "top",
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_line(color = "gray92"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )

  # Vote Chart
  p.votes <- ggplot() +
    geom_bar(
      data = votes.all,
      aes(x = class, y = votes, fill = class),
      stat = "identity",
      position = "identity",
      width = 0.6,
      showSelected = "frame.id"
    ) +
    scale_fill_manual(values = class.colors) +
    scale_y_continuous(
      breaks = seq(0, k, by = 5),
      limits = c(0, k)
    ) +
    labs(
      title = "Classification Votes",
      x = "Class",
      y = "Votes"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11)
    ) +
    theme_animint(width = 400, height = 500)

  # 6. Create animint
  animint(
    featureSpace = p.feature,
    voteChart = p.votes,
    title = "k-Nearest Neighbors Classification",
    time = list(
      variable = "frame.id",
      ms = interval
    ),
    first = list(frame.id = 1)
  )
}

test_that("kNN animation renders correctly", {
  set.seed(42)

  train.data <- matrix(c(rnorm(40, mean = -1), rnorm(40, mean = 1)),
    ncol = 2, byrow = TRUE
  )
  test.data <- matrix(rnorm(4, mean = 0, sd = 1.2), ncol = 2)
  classes <- rep(c("Class A", "Class B"), each = 20)

  viz <- knn_animint(
    train = train.data,
    test = test.data,
    class.vec = classes,
    k = 10,
    interval = 500
  )

  info <- animint2HTML(viz)

  # 1. Check title exists and is correct
  expect_true(nzchar(saveXML(info$html)) > 0)

  # Check <title> tag for the main title
  page.title <- xmlValue(getNodeSet(info$html, "//title")[[1]])
  expect_equal(page.title, "k-Nearest Neighbors Classification")

  # 2. Check for plots presence using SVGs with plot_* IDs
  feature.node <- getNodeSet(info$html, '//svg[@id="plot_featureSpace"]')
  vote.node <- getNodeSet(info$html, '//svg[@id="plot_voteChart"]')

  expect_equal(length(feature.node), 1)
  expect_equal(length(vote.node), 1)

  # 3. Check for specific text appearing in the SVG
  svg.text <- getNodeSet(info$html, "//text")
  all.text <- sapply(svg.text, xmlValue)
  expect_true(any(grepl("Classification Votes", all.text)))

  # 4. Check visual elements
  circles <- getNodeSet(info$html, "//circle")
  expect_true(length(circles) > 0)

  paths <- getNodeSet(info$html, "//path")
  expect_true(length(paths) > 0)

  rects <- getNodeSet(info$html, "//rect")
  expect_true(length(rects) > 0)
})
