acontext("lilac chaser vi")

## Function to implement the vi.lilac.chaser() function from package 'animation'
vi_lilac_chaser <- function(np = 10,
                            nmax = 1,
                            col = 'magenta',
                            p.size = 20,
                            c.size = 4
)
{
    x <- seq(0, 2 * pi * np/(np + 1), length = np)  # Get co-ordinates to plot

    # Get data in a data-frame to pass to a_plot
    df <- data.frame()
    for (i in 1:np) {
        df <- rbind(df, cbind(sin(x[-i]), cos(x[-i]), ptn = i))}
    colnames(df) <- c("sinv", "cosv", "ptn")


    # Plot to display the points and the '+' mark in the middle
    p1 <- a_plot(data = df) +
        # Display the points
        a_geom_point(data = df,
                   a_aes(x = sinv, y = cosv),
                   showSelected = "ptn",
                   col = col,
                   size = p.size) +
        # Display the '+' mark
        a_geom_segment(a_aes(x=-0.1, y=0, xend=0.1, yend=0), size=c.size) +
        a_geom_segment(a_aes(x=0, y=-0.1, xend=0, yend=0.1), size=c.size) +
        xlim(c(-1.33, 1.33)) +
        ylim(c(-1.33, 1.33)) +
        # Hide the axes, titles and others..
        a_theme_bw() +
        a_theme(axis.line=a_element_blank(),
              axis.text.x=a_element_blank(), axis.text.y=a_element_blank(),
              axis.ticks=a_element_blank(),
              axis.title.x=a_element_blank(), axis.title.y=a_element_blank(),
              legend.a_position="none",
              panel.background=a_element_blank(),panel.border=a_element_blank(),
              panel.grid.major=a_element_blank(),panel.grid.minor=a_element_blank(),
              plot.background=a_element_blank())


    # Automate using animint taking point number 'ptn' as variable
    plots <- list(plot1 = p1)
    plots$time <- list(variable = "ptn", ms = 150)
    plots$duration <- list(ptn=0)
    return(plots)
}

plots <- vi_lilac_chaser()
info <- animint2HTML(plots)

test_that("axes hidden", {
    # info <- animint2HTML(viz)
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

test_that("x and y have no labels", {
    xlabel <- getNodeSet(info$html, "//text[@class='xtitle']")
    ylabel <- getNodeSet(info$html, "//text[@class='ytitle']")
    expect_equal(length(xlabel), 0)
    expect_equal(length(ylabel), 0)
})

test_that("Different points are rendered", {
    x1_nodes <- getNodeSet(info$html, "//circle[@class='a_geom']/@cx")
    y1_nodes <- getNodeSet(info$html, "//circle[@class='a_geom']/@cy")
    x1_pts <- sapply(x1_nodes, xmlNode)
    y1_pts <- sapply(y1_nodes, xmlNode)

    Sys.sleep(1.739)  # Wait an arbitrary amount to get point locations

    info$html <- getHTML()

    x2_nodes <- getNodeSet(info$html, "//circle[@class='a_geom']/@cx")
    y2_nodes <- getNodeSet(info$html, "//circle[@class='a_geom']/@cy")
    x2_pts <- sapply(x2_nodes, xmlNode)
    y2_pts <- sapply(y2_nodes, xmlNode)
    expect_false(identical(x1_pts, x2_pts))
    expect_false(identical(y1_pts, y2_pts))
})
