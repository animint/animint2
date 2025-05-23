#' Insert an interactive animation into an R markdown document using a customized print method.
#' @param x named list of ggplots and option lists to pass to \code{animint2dir}.
#' @param options knitr options.
#' @param ... placeholder.
#' @importFrom knitr knit_print
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
#' @author Carson Sievert
#' @export
knit_print.animint <- function(x, options, ...) {
  if (!requireNamespace("knitr")) warning("Please install.packages('knitr')")
  # This function should be evaluated in knitr's output directory
  output.dir <- knitr::opts_knit$get()[["output.dir"]]
  ## sink()
  ## print(output.dir)
  old.wd <- setwd(output.dir)
  on.exit(setwd(old.wd))
  # the current knitr chunk 'label' defines a directory to place the animints 
  # hopefully this regular expression is safe enough to workaround bad chunk names
  # http://stackoverflow.com/questions/8959243/r-remove-non-alphanumeric-symbols-from-a-string
  dir <- gsub("[^[:alnum:]]", "", options$label)
  animint2dir(x, out.dir = dir, json.file = 'plot.json', open.browser = FALSE)
  res <- new_animint(list(id = dir), json.file = file.path(dir, 'plot.json'))
  # if this is the first plot, place scripts just before the plot
  # there has to be a better way to do this, but this will do for now -- http://stackoverflow.com/questions/14308240/how-to-add-javascript-in-the-head-of-a-html-knitr-document
  if (length(knitr::knit_meta(class = "animint", clean = FALSE)) == 0) {
    res <- sprintf('
<script type="text/javascript" src="%s/vendor/d3.v3.js"></script>
<script type="text/javascript" src="%s/animint.js"></script>
<script type="text/javascript" src="%s/vendor/jquery-1.11.3.min.js"></script>
<script type="text/javascript" src="%s/vendor/selectize.min.js"></script>
<link rel="stylesheet" type="text/css" href="%s/vendor/selectize.css" />
<script type="text/javascript" src="%s/vendor/driver.js.iife.js"></script>
<link rel="stylesheet" href="%s/vendor/driver.css" />
%s', dir, dir, dir, dir, dir, dir, dir, res)
  }
  knitr::asis_output(res, meta = list(animint = structure("", class = "animint")))
}

# Helper function to create the HTML needed to embed animint plots 
# Note htmltools provides a better of doing this, but trying to avoid yet another dependency
new_animint <- function(attrs, json.file) {
  jsonFile <- paste0('"', json.file, '"')
  nms <- names(attrs)
  attrz <- paste(nms, shQuote(attrs), sep = '=', collapse = ' ')
  idx <- which(nms == 'id')
  classx <- which(nms == 'class')
  if (length(idx)) {
    prefix <- '"#'
    nm <- attrs[[idx]]
  } else if (length(classx)) {
    prefix <- '".'
    nm <- attrs[[idx]]
  }  else warning('Unknown attribute')
  # using chunk labels is problematic for JS variable names is problematic since '-', '.', etc are illegal
  escaped <- gsub("[-.]", "_", nm)
  selectr <- paste0(prefix, escaped)
  paste0('<p></p>\n<div ', attrz,
         '></div>\n<script>var ', escaped,
         ' = new animint(', selectr,
         '", ', jsonFile,
         ');</script>')
}

#' Shiny ui output function
#' @param outputId output variable to read the plot from
#' @seealso http://shiny.rstudio.com/articles/building-outputs.html
#' @export
#' 
animintOutput <- function(outputId) {
  # Note that requireNamespace("shiny") should load digest & htmltools (both used later on)
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  deps <- lapply(animint_dependencies(), shiny::createWebDependency)
  htmltools::attachDependencies(htmltools::tags$div(id = outputId, class = 'shinyAnimint'), deps)
}

#' Create an animint output element
#' 
#' Shiny server output function customized for animint plots 
#' (similar to \code{shiny::plotOutput} and friends).
#' 
#' @param expr An expression that creates a list of ggplot objects.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is expr a quoted expression (with \code{quote()})? 
#' This is useful if you want to save an expression in a variable.
#' @seealso http://shiny.rstudio.com/articles/building-outputs.html
#' @export
#' 
renderAnimint <- function(expr, env = parent.frame(), quoted = FALSE) {
  # Note that requireNamespace("shiny") should load digest & htmltools (both used later on)
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  
  # Convert the expression + environment into a function
  func <- shiny::exprToFunction(expr, env, quoted)
  
  # this will tell knitr how to place animint into an interactive document
  # implementation is similar to htmlwidgets::shinyRenderWidget
  # we can't use that in our case since we must call animint2dir
  # everytime shiny calls renderFunc
  renderFunc <- function(shinysession, name, ...) {
    val <- func()
    tmp <- tempfile()
    dir.create(tmp)
    stuff <- animint2dir(val, out.dir = tmp, open.browser = FALSE)
    shiny::addResourcePath("animintAssets", tmp)
    list(jsonFile = "plot.json")
  }
  shiny::markRenderFunction(animint2::animintOutput, renderFunc)
}

# html dependencies according htmltools protocols
# these are here basically so we can take advantage of shiny::createWebDependency
animint_dependencies <- function() {
  list(html_dependency_d3(),
       html_dependency_animint(),
       html_dependency_shinyAnimint())
}

html_dependency_d3 <- function() {
  htmltools::htmlDependency(name = "d3",
                 version = "3.0.0",
                 src = system.file("htmljs/vendor", package = "animint2"),
                 script = "d3.v3.js")
}

html_dependency_animint <- function() {
  htmltools::htmlDependency(name = "animint",
                 version = packageVersion("animint2"),
                 src = system.file("htmljs", package = "animint2"),
                 script = "animint.js")
}

html_dependency_shinyAnimint <- function() {
  htmltools::htmlDependency(name = "shinyAnimint",
                 version = packageVersion("animint2"),
                 src = system.file("shiny", package = "animint2"),
                 script = "shinyAnimint.js")
}

html_dependency_plotJSON <- function(path, fileName) {
  htmltools::htmlDependency(name = "plotJSON",
                 version = packageVersion("animint2"),
                 src = path,
                 script = fileName)
}

