acontext("render animint in pdf")
knitr::knit_meta() #clear knitr 'metadata'
screenshot.Rmd <- system.file(
  "examples", "test_knit_print_screenshot.Rmd",
  package = "animint2",
  mustWork = TRUE)
folder <- file.path("animint-htmltest", "test-pdf-folder")
unlink(folder, recursive = TRUE)
doc.Rmd <- file.path(folder, "doc.Rmd")
dir.create(folder, recursive = TRUE, showWarnings = FALSE)
file.copy(screenshot.Rmd, doc.Rmd, overwrite=TRUE)

options(animint2.chromote_sleep_seconds=5)
rmarkdown::render(doc.Rmd)
Capture.PNG <- file.path(folder, "unnamedchunk1", "Capture.PNG")

test_that("PNG and PDF files exist", {
  expect_gt(file.size(Capture.PNG), 0)
  doc.pdf <- file.path(folder, "doc.pdf")
  expect_gt(file.size(doc.pdf), 0)
})

test_that("tex file contains include", {
  doc.tex <- file.path(folder, "doc.tex")
  doc.lines <- readLines(doc.tex)
  include <- grep("includegraphics[width=\\textwidth]", doc.lines, fixed=TRUE, value=TRUE)
  expect_match(include, Capture.PNG, fixed=TRUE)
})

options(animint2.chromote_sleep_seconds=NULL)
test_that("error for knit without option set", {
  expect_error({
    rmarkdown::render(doc.Rmd)
  }, "LaTeX failed to compile", fixed=TRUE)
})
