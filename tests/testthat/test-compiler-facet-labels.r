context("Facet Labels")

get_labels_matrix <- function(plot, ...) {
  data <- a_plot_build(plot)
  a_facet <- data$plot$a_facet
  panel <- data$panel

  a_labels <- get_labels_info(a_facet, panel, ...)
  labeller <- match.fun(a_facet$labeller)

  # Create matrix of labels
  matrix <- lapply(labeller(a_labels), cbind)
  matrix <- do.call("cbind", matrix)
  matrix
}

get_labels_info <- function(a_facet, panel, ...) {
  UseMethod("get_labels_info")
}

get_labels_info.grid <- function(a_facet, panel, type) {
  if (type == "rows") {
    a_labels <- unique(panel$layout[names(a_facet$rows)])
    attr(a_labels, "type") <- "rows"
    attr(a_labels, "a_facet") <- "grid"
  } else {
    a_labels <- unique(panel$layout[names(a_facet$cols)])
    attr(a_labels, "type") <- "cols"
    attr(a_labels, "a_facet") <- "grid"
  }
  a_labels
}

get_labels_info.wrap <- function(a_facet, panel) {
  a_labels <- panel$layout[names(a_facet$facets)]
  attr(a_labels, "a_facet") <- "wrap"
  if (!is.null(a_facet$switch) && a_facet$switch == "x") {
    attr(a_labels, "type") <- "rows"
  } else {
    attr(a_labels, "type") <- "cols"
  }
  a_labels
}


test_that("labellers handle facet labels properly", {
  a_labels <- list(var1 = letters[1:2], var2 = letters[3:4])

  expect_identical(a_label_value(a_labels), a_labels)
  expect_identical(a_label_value(a_labels, FALSE), list(c("a, c", "b, d")))

  expect_identical(a_label_both(a_labels), list(c("var1: a", "var1: b"), c("var2: c", "var2: d")))
  expect_identical(a_label_both(a_labels, FALSE), list(c("var1, var2: a, c", "var1, var2: b, d")))
})

test_that("labellers handle plotmath expressions", {
  a_labels <- list(var1 = c("alpha", "beta"), var2 = letters[3:4])

  expected_parsed <- list(
    list(expression(alpha), expression(beta)),
    list(expression(c), expression(d))
  )
  expect_identical(a_label_parsed(a_labels), expected_parsed)

  expected_parsed_multi <- list(list(
    expression(list(alpha, c)),
    expression(list(beta, d))
  ))
  expect_identical(a_label_parsed(a_labels, FALSE), expected_parsed_multi)
})

test_that("a_label_value() handles factors", {
  a_labels_chr <- list(var1 = letters[1:2], var2 = letters[3:4])
  a_labels <- lapply(a_labels_chr, factor)

  expect_identical(a_label_value(a_labels), a_labels_chr)
})

test_that("labeller() dispatches labellers", {
  p <- a_plot(mtcars, a_aes(wt, mpg)) + a_geom_point()
  expected_cyl_both <- cbind(paste("cyl:", c(4, 6, 8)))
  expected_am_both <- cbind(paste("am:", 0:1))

  # Rows and cols dispatch with a_facet_wrap()
  p1 <- p + a_facet_wrap(~cyl, labeller = labeller(.rows = a_label_both))
  p2 <- p + a_facet_wrap(~cyl, labeller = labeller(.cols = a_label_both))
  expect_equal(get_labels_matrix(p1), expected_cyl_both)
  expect_equal(get_labels_matrix(p2), expected_cyl_both)

  # a_facet_wrap() shouldn't get both rows and cols
  p3 <- p + a_facet_wrap(~cyl, labeller = labeller(
    .cols = a_label_both, .rows = a_label_both))
  expect_error(ggplotGrob(p3))

  # a_facet_grid() can get both rows and cols
  p4 <- p + a_facet_grid(am ~ cyl, labeller = labeller(
    .cols = a_label_both, .rows = a_label_both))
  expect_equal(get_labels_matrix(p4, "rows"), expected_am_both)
  expect_equal(get_labels_matrix(p4, "cols"), expected_cyl_both)

  # Cannot have a specific labeller for a variable which already has a
  # margin-wide labeller
  p5 <- p + a_facet_wrap(~cyl, labeller = labeller(
    .rows = a_label_both, cyl = a_label_value))
  expect_error(ggplotGrob(p5))

  # Variables can be attributed labellers
  p6 <- p + a_facet_grid(am + cyl ~ ., labeller = labeller(
     am = a_label_both, cyl = a_label_both))
  expect_equal(
    get_labels_matrix(p6, "rows"),
    cbind(
      paste("am:", rep(0:1, each = 3)),
      paste("cyl:", rep(c(4, 6, 8), 2))
    )
  )

  # Default labeller is used for other variables
  p7 <- p + a_facet_grid(am ~ cyl, labeller = labeller(.default = a_label_both))
  expect_equal(get_labels_matrix(p7, "rows"), expected_am_both)
  expect_equal(get_labels_matrix(p7, "cols"), expected_cyl_both)
})

test_that("as_labeller() deals with non-labellers", {
  p <- a_plot(mtcars, a_aes(wt, mpg)) + a_geom_point()
  lookup <- c(`0` = "zero", `1` = "one")

  # Lookup table
  p1 <- p + a_facet_wrap(~am, labeller = labeller(am = lookup))
  expect_equal(get_labels_matrix(p1), cbind(c("zero", "one")))

  # Non-labeller function taking character vectors
  p2 <- p + a_facet_wrap(~am, labeller = labeller(am = function(x) paste0(x, "-foo")))
  expect_equal(get_labels_matrix(p2), cbind(c("0-foo", "1-foo")))
})

test_that("old school labellers still work", {
  my_labeller <- function(variable, value) {
    paste0("var = ", as.character(value))
  }

  expect_warning(p <-
    a_plot(mtcars, a_aes(disp, drat)) +
    a_geom_point() +
    a_facet_grid(~cyl, labeller = my_labeller))

  expected_labels <- cbind(paste("var =", c(4, 6, 8)))
  expect_identical(get_labels_matrix(p, "cols"), expected_labels)
})
