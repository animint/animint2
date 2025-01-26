acontext("geom_abline")

viz <- animint(p=qplot(wt, mpg, data = mtcars) + 
  geom_abline(intercept = c(20, 5), slope = c(1,4)) + 
  facet_wrap(~cyl))
info <- animint2HTML(viz)

tsv.file <- file.path("animint-htmltest", "geom2_abline_p_chunk1.tsv")
tsv.data <- read.table(tsv.file, header=TRUE, comment.char = "")

test_that("columns of abline tsv", {
  expected.names <- sort(c("PANEL", "x", "xend", "y", "yend"))
  computed.names <- sort(names(tsv.data))
  expect_identical(computed.names, expected.names)
})

ablines <- getNodeSet(info$html, '//svg//g[@class="geom2_abline_p"]//line')
attr_ablines <- sapply(ablines, xmlAttrs)
start_ends <- attr_ablines[c("x1", "x2", "y1", "y2"), ]

test_that("All six ablines render", {
  expect_equal(length(ablines), 6)
})

test_that("Start and end of ablines are not NA", {
  expect_true(all(start_ends != "NaN"))
})

test_that("lines do not exceed ranges of plot", {
  expect_true(all(as.numeric(start_ends) >= 0))
})

if(FALSE){
  (ab.df <- rbind(
    data.frame(sign="neg", i=1, s=-seq(1,5), i.fac=c('a','b','c','a','b')),
    data.frame(sign="pos", i=c(-0.5,0,0.5),s=1,i.fac=c('a','b','a'))))
  viz <- animint(
    p=ggplot()+
      geom_abline(aes(
        intercept=i,slope=s,color=i.fac),
        showSelected="sign",
        data=ab.df)+
      geom_point(aes(
        x,x),
        clickSelects="sign",
        size=5,
        data=data.frame(x=0:1, sign=c('pos','neg')))
  )    
  viz
}

viz <- animint(
  p=ggplot()+
    geom_abline(aes(
      intercept=i,slope=s),
      data=data.frame(i=10, s=-2))+
    geom_point(aes(
      x,x),
      data=data.frame(x=c(0,10)))+
    geom_point(aes(
      x,y),
      data=data.frame(x=5,y=0))
)
info <- animint2HTML(viz)
get_num <- function(cls, el, at){
  xpath <- sprintf("//g[@class='%s']//%s", cls, el)
  out <- as.numeric(xmlAttrs(getNodeSet(info$html, xpath)[[1]])[at])
  names(out) <- at
  as.list(out)
}
cxy <- get_num("geom3_point_p","circle",c("cx","cy"))
abl <- get_num('geom1_abline_p',"line",c("x1","x2","y1","y2"))
(slope <- with(abl, (y1-y2)/(x1-x2)))
abline.at.5 <- slope*(cxy$cx-abl$x1)+abl$y1
test_that("abline with negative slope intersects point", {
  expect_equal(abline.at.5, cxy$cy)
})
