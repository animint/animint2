# Consider matrix of combinations of alpha and alpha_off in aes parameter
# vs geom parameter.
# Each can be one of geom, aes, or none.
# The test matrix of tuples, (alpha, alpha_off), is:
# (geom, geom), (geom, aes), (geom, none),
# (aes,  geom), (aes,  aes), (aes,  none),
# (none, geom), (none, aes), (none, none)

# The (alpha, none) column is the original behavior, where the alpha of
# unselected values is the original alpha - 0.5.
# (none, none) is the base case, where the selection uses alpha = 1, and the
# unselected use the original - 0.5 formula.
# (geom, geom) behaves similar to (none, none), but the alpha is set to any
# value the user selects.
# (geom, aes) gives the selection a defined alpha, and unselected points use
# the aes value.
# (aes, aes) gives both the selection and unselected items alpha from their aes.
# (none, geom) gives selected points the default opacity of 1, and unselected
# points the provided opacity.
# (none, aes) gives selected points the default opacity of 1, and unselected
# points use the aes value.

acontext("User defined opacity")


alpha_seq <- seq(0.1, 1, by = 0.1)
alpha_rev_seq <- seq(1, 0.1, by = -0.1)

plot.dt <- data.frame(
  x = 1:10,
  y = 1:10,
  alpha_seq = alpha_seq,
  alpha_rev_seq = alpha_rev_seq
)

alpha_on <- 0.8
alpha_off <- 0.2

scatter.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    size = 5,
    aes(x, y, alpha = alpha_seq)
  ) +
  ggtitle("Scatter plot with non-interactive alpha")

geom.geom.plot <- ggplot() +
geom_point(
  data = plot.dt,
  alpha = alpha_on,
  alpha_off = alpha_off,
  size = 5,
  clickSelects = "y",
  aes(x, y, id=paste0("y", y))
) +
ggtitle("Scatter plot with (geom, geom)")

geom.aes.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    alpha = alpha_on,
    size = 5,
    clickSelects = "y",
    aes(x, y, alpha_off = alpha_seq)
  ) +
  ggtitle("Scatter plot with (geom, aes)")

geom.none.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    alpha = alpha_on,
    size = 5,
    clickSelects = "y",
    aes(x, y)
  ) +
  ggtitle("Scatter plot with (geom, none)")

# TODO: fix this, right now behaving like (aes, none)
aes.geom.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    alpha_off = alpha_off,
    size = 5,
    clickSelects = "y",
    aes(x, y, alpha = alpha_seq)
  ) +
  ggtitle("Scatter plot with (aes, geom)")

aes.aes.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    size = 5,
    clickSelects = "y",
    aes(x, y, alpha = alpha_seq, alpha_off = alpha_rev_seq)
  ) +
  ggtitle("Scatter plot with (aes, aes)")

aes.none.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    size = 5,
    clickSelects = "y",
    aes(x, y, alpha = alpha_seq)
  ) +
  ggtitle("Scatter plot with (aes, none)")

none.geom.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    size = 5,
    alpha_off = alpha_off,
    clickSelects = "y",
    aes(x, y)
  ) +
  ggtitle("Scatter plot with (none, geom)")

none.aes.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    size = 5,
    clickSelects = "y",
    aes(x, y, alpha_off = alpha_seq)
  ) +
  ggtitle("Scatter plot with (none, aes)")

none.none.plot <- ggplot() +
  geom_point(
    data = plot.dt,
    size = 5,
    clickSelects = "y",
    aes(x, y)
  ) +
  ggtitle("Scatter plot with (none, none)")

scatter.viz <- list()
scatter.viz$noninteractive <- scatter.plot
scatter.viz$geomGeom <- geom.geom.plot
scatter.viz$geomAes <- geom.aes.plot
scatter.viz$geomNone <- geom.none.plot
scatter.viz$aesGeom <- aes.geom.plot
scatter.viz$aesAes <- aes.aes.plot
scatter.viz$aesNone <- aes.none.plot
scatter.viz$noneGeom <- none.geom.plot
scatter.viz$noneAes <- none.aes.plot
scatter.viz$noneNone <- none.none.plot

animint2HTML(scatter.viz)


get_points_geom <- function(geom, full.node.set) {
  getNodeSet(full.node.set, paste0("//svg[@id='plot_", geom, "']//circle"))
}

opacity_extract_pattern <- "(?<=opacity: )(\\-?\\d*\\.?\\d*)"

get_opacity <- function (node) {
    style <- xmlAttrs(node)[["style"]]
    as.numeric(
      regmatches(style, regexpr(opacity_extract_pattern, style, perl = TRUE)))
      }

# It can't hurt to make sure we explicitly set the initial state,
# just in case some browser or Selenium update changes things
before.update.nodes <- clickHTML(id=paste0("y", 1))
after.update.nodes <- clickHTML(id=paste0("y", 2))


test_that("(geom, geom) opacity updates", {
  before.nodes <- get_points_geom("geomGeom", before.update.nodes)
  after.nodes <- get_points_geom("geomGeom", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], alpha_on)
  expect_equal(before.opacities[2], alpha_off)
  expect_equal(after.opacities[1], alpha_off)
  expect_equal(after.opacities[2], alpha_on)
})

test_that("(geom, aes) opacity updates", {
  before.nodes <- get_points_geom("geomAes", before.update.nodes)
  after.nodes <- get_points_geom("geomAes", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], alpha_on)
  expect_equal(before.opacities[2], alpha_seq[2])
  expect_equal(after.opacities[1], alpha_seq[1])
  expect_equal(after.opacities[2], alpha_on)
})

test_that("(geom, none) opacity updates", {
  before.nodes <- get_points_geom("geomNone", before.update.nodes)
  after.nodes <- get_points_geom("geomNone", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], alpha_on)
  expect_equal(before.opacities[2], alpha_on - 0.5)
  expect_equal(after.opacities[1], alpha_on - 0.5)
  expect_equal(after.opacities[2], alpha_on)
})

test_that("(aes, geom) opacity update", {
  before.nodes <- get_points_geom("aesGeom", before.update.nodes)
  after.nodes <- get_points_geom("aesGeom", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], alpha_seq[1])
  expect_equal(before.opacities[2], alpha_off)
  expect_equal(after.opacities[1], alpha_off)
  expect_equal(after.opacities[2], alpha_seq[2])
})

test_that("(aes, aes) opacity updates", {
  before.nodes <- get_points_geom("aesAes", before.update.nodes)
  after.nodes <- get_points_geom("aesAes", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], alpha_seq[1])
  expect_equal(before.opacities[2], alpha_rev_seq[2])
  expect_equal(after.opacities[1], alpha_rev_seq[1])
  expect_equal(after.opacities[2], alpha_seq[2])
})

test_that("(aes, none) opacity updates", {
  before.nodes <- get_points_geom("aesNone", before.update.nodes)
  after.nodes <- get_points_geom("aesNone", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], alpha_seq[1])
  expect_equal(before.opacities[2], alpha_seq[2] - 0.5)
  expect_equal(after.opacities[1], alpha_seq[1] - 0.5)
  expect_equal(after.opacities[2], alpha_seq[2])
})

test_that("(none, geom) opacity updates", {
  before.nodes <- get_points_geom("noneGeom", before.update.nodes)
  after.nodes <- get_points_geom("noneGeom", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], 1)
  expect_equal(before.opacities[2], alpha_off)
  expect_equal(after.opacities[1], alpha_off)
  expect_equal(after.opacities[2], 1)
})

test_that("(none, aes) opacity updates", {
  before.nodes <- get_points_geom("noneAes", before.update.nodes)
  after.nodes <- get_points_geom("noneAes", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], 1)
  expect_equal(before.opacities[2], alpha_seq[2])
  expect_equal(after.opacities[1], alpha_seq[1])
  expect_equal(after.opacities[2], 1)
})

test_that("(none, none) opacity updates", {
  before.nodes <- get_points_geom("noneNone", before.update.nodes)
  after.nodes <- get_points_geom("noneNone", after.update.nodes)
  before.opacities <- sapply(before.nodes, get_opacity)
  after.opacities <- sapply(after.nodes, get_opacity)
  expect_equal(before.opacities[1], 1)
  expect_equal(before.opacities[2], 1 - 0.5)
  expect_equal(after.opacities[1], 1 - 0.5)
  expect_equal(after.opacities[2], 1)
})
