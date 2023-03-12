.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()

  tips <- c(
    "Need help? Try filing a reproducible example on https://github.com/tdhock/animint2/issues",
    "Find out what's changed in animint2 at https://github.com/tdhock/animint2/blob/master/NEWS",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Need help getting started? Try reading https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch02-ggplot2.html"
  )

  tip <- sample(tips, 1)
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}
