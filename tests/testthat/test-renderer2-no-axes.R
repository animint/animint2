acontext("no axes")

viz <- list(
  gg=a_plot()+
    geom_point(aes(Petal.Length, Sepal.Length),
               data=iris)+
    theme_bw()+
    theme(axis.line=a_element_blank(), axis.text=a_element_blank(), 
          axis.ticks=a_element_blank(), axis.title=a_element_blank(),
          panel.background = a_element_blank(),
          panel.border = a_element_blank())
  )

test_that("axes hidden", {
  info <- animint2HTML(viz)
  ec <- function(a_element, class){
    data.frame(a_element, class)
  }
  elem.df <- rbind(
    ec("rect", paste0(c("background","border"), "_rect")),
    ec("g", "axis"),
    ec("path", "domain"),
    ec("text", paste0(c("x", "y"), "title")))
  for(elem.i in seq_along(elem.df$a_element)){
    xpath <- with(elem.df[elem.i, ], {
      sprintf('//%s[@class="%s"]', a_element, class)
    })
    a_element.list <- getNodeSet(info$html, xpath)
    expect_equal(length(a_element.list), 0)
  }
})
