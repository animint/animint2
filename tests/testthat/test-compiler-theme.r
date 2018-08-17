context("a_themes")

test_that("Modifying a_theme element properties with + operator", {

  # Changing a "leaf node" works
  t <- a_theme_grey() + a_theme(axis.title.x = a_element_text(colour = 'red', margin = margin()))
  expect_identical(t$axis.title.x, a_element_text(colour = 'red', margin = margin()))
  # Make sure the a_theme class didn't change or get dropped
  expect_true(is.a_theme(t))
  # Make sure the element class didn't change or get dropped
  expect_true(inherits(t$axis.title.x, "a_element"))
  expect_true(inherits(t$axis.title.x, "a_element_text"))

  # Modifying an intermediate node works
  t <- a_theme_grey() + a_theme(axis.title = a_element_text(colour = 'red'))
  expect_identical(t$axis.title, a_element_text(colour = 'red'))

  # Modifying a root node changes only the specified properties
  t <- a_theme_grey() + a_theme(text = a_element_text(colour = 'red'))
  expect_identical(t$text$colour, 'red')
  expect_identical(t$text$family, a_theme_grey()$text$family)
  expect_identical(t$text$face,   a_theme_grey()$text$face)
  expect_identical(t$text$size,   a_theme_grey()$text$size)
  # Descendent is unchanged
  expect_identical(t$axis.title.x, a_theme_grey()$axis.title.x)

  # Adding a_element_blank replaces a_element
  t <- a_theme_grey() + a_theme(axis.text.y = a_element_blank())
  expect_identical(t$axis.text.y, a_element_blank())

  # Adding a non-blank element to an a_element_blank() replaces it
  t <- t + a_theme(axis.text.y = a_element_text(colour = 'red'))
  expect_identical(t$axis.text.y, a_element_text(colour = 'red'))

  # Adding empty a_theme() has no effect
  t <- a_theme_grey() + a_theme()
  expect_identical(t, a_theme_grey())

  expect_error(a_theme_grey() + "asdf")
})


test_that("Adding a_theme object to ggplot object with + operator", {

  p <- qplot(1:3, 1:3)
  p <- p + a_theme(axis.title = a_element_text(size = 20))
  expect_true(p$a_theme$axis.title$size == 20)

  # Should update specified properties, but not reset other properties
  p <- p + a_theme(text = a_element_text(colour = 'red'))
  expect_true(p$a_theme$text$colour == 'red')
  tt <- a_theme_grey()$text
  tt$colour <- 'red'
  expect_identical(p$a_theme$text, tt)

})


test_that("Replacing a_theme elements with %+replace% operator", {
  # Changing a "leaf node" works
  t <- a_theme_grey() %+replace% a_theme(axis.title.x = a_element_text(colour = 'red'))
  expect_identical(t$axis.title.x, a_element_text(colour = 'red'))
  # Make sure the class didn't change or get dropped
  expect_true(is.a_theme(t))

  # Changing an intermediate node works
  t <- a_theme_grey() %+replace% a_theme(axis.title = a_element_text(colour = 'red'))
  expect_identical(t$axis.title, a_element_text(colour = 'red'))
  # Descendent is unchanged
  expect_identical(t$axis.title.x, a_theme_grey()$axis.title.x)

  # Adding empty a_theme() has no effect
  t <- a_theme_grey() %+replace% a_theme()
  expect_identical(t, a_theme_grey())

  expect_error(a_theme_grey() + "asdf")
})


test_that("Calculating a_theme element inheritance", {
  t <- a_theme_grey() + a_theme(axis.title = a_element_text(colour = 'red'))

  # Check that properties are passed along from axis.title to axis.title.x
  e <- a_calc_element('axis.title.x', t)
  expect_identical(e$colour, 'red')
  expect_false(is.null(e$family))
  expect_false(is.null(e$face))
  expect_false(is.null(e$size))


  # Check that rel() works for relative sizing, and is applied at each level
  t <- a_theme_grey(base_size = 12) +
    a_theme(axis.title   = a_element_text(size = rel(0.5))) +
    a_theme(axis.title.x = a_element_text(size = rel(0.5)))
  e <- a_calc_element('axis.title', t)
  expect_identical(e$size, 6)
  ex <- a_calc_element('axis.title.x', t)
  expect_identical(ex$size, 3)


  # Check that a a_theme_blank in a parent node gets passed along to children
  t <- a_theme_grey() + a_theme(text = a_element_blank())
  expect_identical(a_calc_element('axis.title.x', t), a_element_blank())
})


test_that("Complete and non-complete a_themes interact correctly with each other", {
  # The 'complete' attribute of t1 + t2 is the OR of their 'complete' attributes.

  # But for _element properties_, the one on the right modifies the one on the left.
  t <- a_theme_bw() + a_theme(text = a_element_text(colour = 'red'))
  expect_true(attr(t, "complete"))
  expect_equal(t$text$colour, 'red')

  # A complete a_theme object (like a_theme_bw) always trumps a non-complete a_theme object
  t <- a_theme(text = a_element_text(colour = 'red')) + a_theme_bw()
  expect_true(attr(t, "complete"))
  expect_equal(t$text$colour, a_theme_bw()$text$colour)

  # Adding two non-complete a_themes: the one on the right modifies the one on the left.
  t <- a_theme(text = a_element_text(colour = 'blue')) +
    a_theme(text = a_element_text(colour = 'red'))
  expect_false(attr(t, "complete"))
  expect_equal(t$text$colour, 'red')
})


test_that("Complete and non-complete a_themes interact correctly with ggplot objects", {
  # Check that adding two a_theme successive a_theme objects to a ggplot object
  # works like adding the two a_theme object to each other
  p <- a_plot_build(qplot(1:3, 1:3) + a_theme_bw() + a_theme(text = a_element_text(colour = 'red')))
  expect_true(attr(p$plot$a_theme, "complete"))

  # Compare the a_theme objects, after sorting the items, because item order can differ
  pt <- p$plot$a_theme
  tt <- a_theme_bw() + a_theme(text = a_element_text(colour = 'red'))
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)


  p <- a_plot_build(qplot(1:3, 1:3) + a_theme(text = a_element_text(colour = 'red')) + a_theme_bw())
  expect_true(attr(p$plot$a_theme, "complete"))
  # Compare the a_theme objects, after sorting the items, because item order can differ
  pt <- p$plot$a_theme
  tt <- a_theme(text = a_element_text(colour = 'red')) + a_theme_bw()
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)


  p <- a_plot_build(qplot(1:3, 1:3) + a_theme(text = a_element_text(colour = 'red', face = 'italic')))
  expect_false(attr(p$plot$a_theme, "complete"))
  expect_equal(p$plot$a_theme$text$colour, "red")
  expect_equal(p$plot$a_theme$text$face, "italic")


  p <- a_plot_build(qplot(1:3, 1:3) +
    a_theme(text = a_element_text(colour = 'red')) +
    a_theme(text = a_element_text(face = 'italic')))
  expect_false(attr(p$plot$a_theme, "complete"))
  expect_equal(p$plot$a_theme$text$colour, "red")
  expect_equal(p$plot$a_theme$text$face, "italic")


  # Only gets red property; because of the way lists are processed in R, the
  # the second item doesn't get used properly. But I think that's OK.
  p <- a_plot_build(qplot(1:3, 1:3) +
    a_theme(text = a_element_text(colour = 'red'), text = a_element_text(face = 'italic')))
  expect_false(attr(p$plot$a_theme, "complete"))
  expect_equal(p$plot$a_theme$text$colour, "red")
  expect_equal(p$plot$a_theme$text$face, "plain")
})

test_that("a_theme(validate=FALSE) means do not validate_element", {
  p <- qplot(1:3, 1:3)
  bw <- p + a_theme_bw()
  red.text <- a_theme(text = a_element_text(colour = "red"))
  bw.before <- bw + a_theme(animint.width = 500, validate = FALSE)
  expect_equal(bw.before$a_theme$animint.width, 500)

  bw.after <- p + a_theme(animint.width = 500, validate = FALSE) + a_theme_bw()
  expect_null(bw.after$a_theme$animint.width)

  red.after <- p + a_theme(animint.width = 500, validate = FALSE) + red.text
  expect_equal(red.after$a_theme$animint.width, 500)

  red.before <- p + red.text + a_theme(animint.width = 500, validate = FALSE)
  expect_equal(red.before$a_theme$animint.width, 500)
})
