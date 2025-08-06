acontext("geom_abline")

viz <- animint(p=qplot(wt, mpg, data = mtcars) + 
  geom_abline(intercept = c(20, 5), slope = c(1,4)) + 
  facet_wrap(~cyl))
info <- animint2HTML(viz)

tsv.file <- file.path("animint-htmltest", "geom2_abline_p_chunk1.tsv")
tsv.data <- read.table(tsv.file, header=TRUE, comment.char = "")

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

# Test for testing that geom_ablines are clipped to the plot area even after update_axes is called
data(mtcars)
# Grouping variable
mtcars$cyl <- as.factor(mtcars$cyl)
cyl.levels <- levels(mtcars$cyl)
# Generate 60 ablines (20 per cyl), all visible at once
set.seed(123)
abline_data <- do.call(rbind, lapply(cyl.levels, function(cyl_val) {
  data.frame(
    slope = runif(20, -2, 2),
    intercept = runif(20, 0, 500),
    cyl = cyl_val
  )
}))
viz <- list(
  title = "Many geom_ablines without filtering",
  allablines = ggplot() +
  theme_animint(update_axes = c("x", "y"), height=400, width=400) +
    geom_point(aes(mpg, disp, color = cyl), data = mtcars, showSelected = "cyl") +
    geom_abline(
      aes(slope = slope, intercept = intercept, color = cyl),
      size = 1,
      data = abline_data
    ) +
    ggtitle("All ablines, all cyl groups together"),
    selector.types = list(cyl = "single")
)
info <- animint2HTML(viz)
ablines <- getNodeSet(info$html, '//svg//g[contains(@class, "geom2_abline_allablines")]//line')
abline_coords <- sapply(ablines, xmlAttrs)

# Extract x1, x2, y1, y2 from all lines
x1s <- as.numeric(abline_coords["x1", ])
x2s <- as.numeric(abline_coords["x2", ])
y1s <- as.numeric(abline_coords["y1", ])
y2s <- as.numeric(abline_coords["y2", ])

test_that("ablines are clipped within plot area", {
  expect_true(all(!is.na(c(x1s, x2s, y1s, y2s))))
  expect_true(all(x1s >= 0 & x1s <= 400))
  expect_true(all(x2s >= 0 & x2s <= 400))
  expect_true(all(y1s >= 0 & y1s <= 400))
  expect_true(all(y2s >= 0 & y2s <= 400))
})
