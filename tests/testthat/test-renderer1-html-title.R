acontext("HTML page title")

iris$id <- 1:nrow(iris)
viz <-
  list(petal=a_plot()+
       a_geom_point(a_aes(Petal.Width, Petal.Length, fill=Species),
                  clickSelects="id", data=iris),
       sepal=a_plot()+
       a_geom_point(a_aes(Sepal.Width, Sepal.Length, fill=Species),
                  clickSelects="id", data=iris),
       title="Iris data")

test_that("title option converted to <title>", {
  info <- animint2HTML(viz)
  nodes <- getNodeSet(info$html, "//title")
  generated.title <- xmlValue(nodes[[1]])
  expect_match(generated.title, "Iris data")
})

