acontext("hit or miss monte carlo")

library(plyr)

HitOrMissMonteCarlo <- function() {
  FUN <- function(x) x-x^2
  data.generator = function() {
    
    from <- 0 
    to <- 1
    max <- 200
    shape.option <- c(20, 4)
    
    x1 <- runif(max, from, to)
    ymin <- optimize(FUN, c(from, to), maximum = FALSE)$objective
    ymax <- optimize(FUN, c(from, to), maximum = TRUE)$objective
    x2 <- runif(max, ymin, ymax) 
    y <- FUN(x1)
    
    serial <- seq(1, max)
    under.The.Curve <- (x2 < y)
    shape <- shape.option[(x2 < y) + 1]
    
    generated.iter <- data.frame(iter = serial)
    generated.data <- data.frame("S.No" = serial, "y" = y, "x1" = x1, "x2" = x2, "Under.The.Curve" = under.The.Curve, "shape" = shape)
    
    invisible(list(data=generated.data, iter=generated.iter))
  }
  
  init <- data.generator()
  data <- init$data
  iteration <- init$iter
  
  data <- ldply(data$S.No, function(i) {
    dfAll <- subset(data, S.No<=i)
    df <- subset(data, S.No==i)
    trueCount <- length(dfAll$Under.The.Curve[dfAll$Under.The.Curve]==TRUE)
    calcPI <- (2*i)/trueCount
    cbind(df, pi=calcPI)
  })
  iteration <- cbind(iteration, pi = data$pi)
  
  data <- ldply(data$S.No, function(i) {
    df <- subset(data, S.No <= i)
    cbind(df, iter = i)
  })
  
  viz.one <- ggplot(data.frame(x=c(0,1)), aes(x=x))+
    stat_function(fun=FUN)+
    ggtitle("Hit or Miss Monte Carlo Integration")+
    geom_point(aes(x1,x2, key=S.No, color=Under.The.Curve, shape=Under.The.Curve),
               showSelected="iter",
               size=4,
               data=data)+
    scale_shape_manual(values = c("TRUE" = 20, "FALSE" = 4))+ # Now working in Animint2 Viz
    scale_color_manual(values = c("TRUE" = "black", "FALSE"="red"))+
    theme_bw()+
    theme_animint(width=600, height=600)
  
  viz.two <- ggplot()+
    ggtitle("Monte Carlo Estimation of \u03C0")+
    geom_tallrect(aes(
      xmin=iter-0.5, xmax=iter+0.5),
      clickSelects="iter",
      alpha=0.5,
      data=iteration)+
    geom_hline(yintercept = pi, size=1)+
    geom_path(aes(S.No, pi, key=iter),
              showSelected="iter",
              size=1,
              data=data)+
    geom_text(aes(iter, pi, key=iter, label=paste("\u03C0", "=", sprintf("%.6f", pi), sep = " ")),
              showSelected=c("iter"),
              size=20,
              data=iteration)+
    labs(x="Iterations", y="Value of \u03C0")+
    scale_y_continuous(breaks = c(1, 2, 3, pi, 4, 5, 6, 7, 8))+
    theme_bw()+
    theme_animint(width=600, height=600)
  
  (viz.publish <- animint(viz.one,
                          viz.two,
                          title="Hit or Miss Monte Carlo Integration",
                          time=list(variable="iter", ms=200),
                          first=list(iter=1)))
  return(viz.publish)
}

viz <- HitOrMissMonteCarlo()

info <- animint2HTML(viz)

test_that("x and y labels are not null", {
  xlabel <- getNodeSet(info$html, "//text[@class='xtitle']")
  ylabel <- getNodeSet(info$html, "//text[@class='ytitle']")
  
  expect_equal(length(xlabel), 2)
  expect_equal(length(ylabel), 2)
})

test_that("Different Points Are Rendered on Integration Curve", {
  x1_nodes <- getNodeSet(info$html, "//g[@class='geom2_point_vizone']/g[@class='PANEL1']/circle/@cx")
  y1_nodes <- getNodeSet(info$html, "//g[@class='geom2_point_vizone']/g[@class='PANEL1']/circle/@cy")
  
  Sys.sleep(1.739)

  info$newHtml <- getHTML()

  x2_nodes <- getNodeSet(info$newHtml, "//g[@class='geom2_point_vizone']/g[@class='PANEL1']/circle/@cx")
  y2_nodes <- getNodeSet(info$newHtml, "//g[@class='geom2_point_vizone']/g[@class='PANEL1']/circle/@cy")

  expect_false(identical(x1_nodes, x2_nodes))
  expect_false(identical(y1_nodes, y2_nodes))
})

test_that("Different points are rendered on Pi estimation plot", {
  coordPanel1 <- getNodeSet(info$html, "//g[@class='geom5_path_viztwo']/g[@class='PANEL1']/path/@d")

  Sys.sleep(2)

  info$newHtml <- getHTML()

  coordPanel2 <- getNodeSet(info$newHtml, "//g[@class='geom5_path_viztwo']/g[@class='PANEL1']/path/@d")
  
  expect_false(identical(coordPanel1, coordPanel2))
})
