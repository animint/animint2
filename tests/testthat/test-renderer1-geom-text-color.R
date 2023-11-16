acontext("geom text color")
library(animint2)
df <- data.frame(x=1,y="foo")
viz <- animint(
  text=ggplot()+
    geom_text(aes(x, 4, label=y, id="ONETEXT"), color="black", clickSelects="y", data=df)+
    geom_text(aes(x, 3, label=y, color=y), data=df)+
    scale_color_manual(values=c(foo="blue"))+
    geom_text(aes(x, 2, label=y), color="red", data=df)+
    geom_text(aes(x, 1, label=y), color="black", color_off="pink", clickSelects="y", data=df))

info <- animint2HTML(viz)
test_that("geom_text color rendered as fill style", {
  fill <- getStyleValue(info$html, '//text[@class="geom"]', "fill")
  expect_color(fill, c("black", "blue","red","black"))
  opacity <- getStyleValue(info$html, '//text[@class="geom"]', "opacity")
  expect_identical(opacity, c("1","1","1","1"))
})

clickID("ONETEXT")
after.html <- getHTML()
test_that("geom_text color rendered as fill style", {
  fill <- getStyleValue(after.html, '//text[@class="geom"]', "fill")
  print(fill)
  expect_color(fill, c("black", "red","pink"))
})

test_that("default text alpha_off correct", {
  opacity <- getStyleValue(after.html, '//text[@class="geom"]', "opacity")
  print(opacity)
  expect_identical(opacity, c("0.5","1","0.5"))
})
