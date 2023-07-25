acontext("Firefox browser")

# generate a list of viz
p <- qplot(1:10)
n <- 20
set.seed(123)  # for reproducible random results
random_names <- replicate(n, paste(sample(letters, 5), collapse = ""))
viz <- setNames(replicate(n, p, simplify = FALSE), random_names)

info <- animint2HTML(viz)

# Scroll to the bottom of the page
remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);") # remDr is a global variable
Sys.sleep(5)

# Get current scroll position
scroll_position <- remDr$executeScript("return window.pageYOffset;")
Sys.sleep(5)
new_scroll_position <- remDr$executeScript("return window.pageYOffset;")

test_that("The scrollbar does not move", {
  expect_equal(scroll_position, new_scroll_position, info = "The scrollbar moved.")
})