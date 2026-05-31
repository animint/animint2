acontext("knitting quarto website outputs")

test_that("quarto website output includes animint directory", {
  skip_if_not_installed("quarto")
  skip_if_not(nzchar(quarto::quarto_path()), "Quarto CLI not installed")

  temp.dir <- tempfile()
  dir.create(temp.dir)
  on.exit(unlink(temp.dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "project:",
    "  type: website",
    "  output-dir: _site"
  ), file.path(temp.dir, "_quarto.yml"))

  writeLines(c(
    "---",
    "title: Home",
    "---",
    "",
    "```{r myplot}",
    "library(animint2)",
    "p <- ggplot() + geom_point(aes(x = 1:10, y = 1:10))",
    "animint(plot = p)",
    "```"
  ), file.path(temp.dir, "index.qmd"))

  old_wd <- getwd()
  setwd(temp.dir)
  on.exit(setwd(old_wd), add = TRUE)

  quarto::quarto_render("index.qmd", quiet = TRUE)

  expect_true(file.exists(file.path(temp.dir, "_site", "myplot", "plot.json")))
  expect_true(file.exists(file.path(temp.dir, "_site", "myplot", "animint.js")))
})
