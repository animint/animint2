context("Data")

test_that("stringsAsFactors doesn't affect results", {

    sAF <- getOption("stringsAsFactors")
    dat.character <- data.frame(x = letters[5:1], y = 1:5, stringsAsFactors = FALSE)
    dat.factor <- data.frame(x = letters[5:1], y = 1:5, stringsAsFactors = TRUE)

    base <- a_plot(mapping = a_aes(x, y)) + a_geom_point()
    xlabels <- function(x) x$panel$ranges[[1]]$x.a_labels

    options(stringsAsFactors = TRUE)
    char_true <- a_plot_build(base %+% dat.character)
    factor_true <- a_plot_build(base %+% dat.factor)

    options(stringsAsFactors = FALSE)
    char_false <- a_plot_build(base %+% dat.character)
    factor_false <- a_plot_build(base %+% dat.factor)

    options(stringsAsFactors = sAF)

    expect_equal(xlabels(char_true), letters[1:5])
    expect_equal(xlabels(char_false), letters[1:5])
    expect_equal(xlabels(factor_true), letters[1:5])
    expect_equal(xlabels(factor_false), letters[1:5])
})
