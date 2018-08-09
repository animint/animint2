library(testthat)
acontext("hline")

n.rows <- 100
df <- data.frame(x=rnorm(n.rows), y=rnorm(n.rows))
sc <- a_plot()+
  a_geom_point(a_aes(x, y), data=df)
two <- data.frame(x=c(0, 1))

viz <-
  list(one=sc+
         a_geom_hline(yintercept=0)+
         a_geom_vline(xintercept=0),

       two=sc+
         a_geom_hline(a_aes(yintercept=x), data=two)+
         a_geom_vline(a_aes(xintercept=x), data=two))

info <- animint2HTML(viz)

line.class.vec <-
  c("a_geom2_hline_one"=1, "a_geom3_vline_one"=1,
    "a_geom5_hline_two"=2, "a_geom6_vline_two"=2)

test_that("hlines and vlines appear", {
  for(line.class in names(line.class.vec)){
    xpath <- sprintf('//g[@class="%s"]//line', line.class)
    line.list <- getNodeSet(info$html, xpath)
    expected.length <- line.class.vec[[line.class]]
    expect_equal(length(line.list), expected.length)
    attr.mat <- sapply(line.list, xmlAttrs)
    xy.mat <- attr.mat[c("x1", "x2", "y1", "y2"), ]
    xy.vec <- as.numeric(xy.mat)
    expect_true(all(is.finite(xy.vec)))
  }
})
