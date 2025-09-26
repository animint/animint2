#' Insert an interactive animation into an R markdown document using a customized print method.
#' @param x named list of ggplots and option lists to pass to \code{animint2dir}.
#' @param options knitr chunk options.
#' @param ... ignored.
#' @importFrom knitr knit_print
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
#' @author Carson Sievert
#' @export
knit_print.animint <- function(x, options, ...) {
  output.dir <- knitr::opts_knit$get()[["output.dir"]]
  # the current knitr chunk 'label' defines a directory to place the animints
  viz_id <- gsub("[^[:alnum:]]", "", options$label)
  out.dir <- file.path(output.dir, viz_id)
  animint2dir(x, out.dir = out.dir, open.browser = FALSE)
  res <- if(knitr::is_latex_output())sprintf(
    "\\includegraphics[height=\\textwidth]{%s/Capture.PNG}", out.dir
  ) else sprintf(
    ## <div id="Ch01vizKeeling"></div><script>var Ch01vizKeeling = new animint("#Ch01vizKeeling","Ch01vizKeeling/plot.json");</script>
    '<div id="%s"></div>\n<script>var %s = new animint("#%s","%s/plot.json");</script>', viz_id, viz_id, viz_id, viz_id
  )
  # if this is the first plot, place scripts just before the plot
  # there has to be a better way to do this, but this will do for now -- http://stackoverflow.com/questions/14308240/how-to-add-javascript-in-the-head-of-a-html-knitr-document
  if (length(knitr::knit_meta(class = "animint", clean = FALSE)) == 0) {
    res <- sprintf('
<script type="text/javascript" src="%s/vendor/d3.v3.js"></script>
<script type="text/javascript" src="%s/animint.js"></script>
<script type="text/javascript" src="%s/vendor/quadprog.js"></script>
<link rel="stylesheet" type="text/css" href="%s/animint.css" />
<script type="text/javascript" src="%s/vendor/jquery-1.11.3.min.js"></script>
<script type="text/javascript" src="%s/vendor/selectize.min.js"></script>
<link rel="stylesheet" type="text/css" href="%s/vendor/selectize.css" />
<script type="text/javascript" src="%s/vendor/driver.js.iife.js"></script>
<link rel="stylesheet" href="%s/vendor/driver.css" />
%s', viz_id, viz_id, viz_id, viz_id, viz_id, viz_id, viz_id, viz_id, viz_id, res)
  }
  knitr::asis_output(res, meta = list(animint = structure("", class = "animint")))
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

