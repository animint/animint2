translatePattern <-
  paste0("translate[(]",
         "(?<x>.*?)",
         ",",
         "(?<y>.*?)",
         "[)]")

acontext <- function(...){
  print(...)
  context(...)
}

## Parse the first occurance of pattern from each of several strings
## using (named) capturing regular expressions, returning a matrix
## (with column names).
str_match_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- do.call(rbind,lapply(seq_along(string),function(i){
    st <- attr(parsed,"capture.start")[i,]
    if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
    substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
  }))
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}

## Parse several occurances of pattern from each of several strings
## using (named) capturing regular expressions, returning a list of
## matrices (with column names).
str_match_all_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- gregexpr(pattern,string,perl=TRUE)
  lapply(seq_along(parsed),function(i){
    r <- parsed[[i]]
    starts <- attr(r,"capture.start")
    if(r[1]==-1)return(matrix(nrow=0,ncol=1+ncol(starts)))
    names <- attr(r,"capture.names")
    lengths <- attr(r,"capture.length")
    full <- substring(string[i],r,r+attr(r,"match.length")-1)
    subs <- substring(string[i],starts,starts+lengths-1)
    m <- matrix(c(full,subs),ncol=length(names)+1)
    colnames(m) <- c("",names)
    if("name" %in% names){
      rownames(m) <- m[, "name"]
    }
    m
  })
}

getSelectorWidgets <- function(html=getHTML()){
  tr.list <- getNodeSet(html,
                        '//table[@class="table_selector_widgets"]//tr')
  td.list <- sapply(tr.list[-1], function(tr)xmlChildren(tr)[[1]])
  sapply(td.list, xmlValue)
}

clickHTML <- function(...){
  v <- c(...)
  stopifnot(length(v) == 1)
  e <- remDr$findElement(names(v), as.character(v))
  e$clickElement()
  Sys.sleep(1)
  getHTML()
}

clickID <- function(...){
  v <- c(...)
  stopifnot(length(v) == 1)
  remDr$executeScript(sprintf("document.getElementById('%s').dispatchEvent(new CustomEvent('click'))", as.character(v)))
}

rgba.pattern <- paste0(
  "(?<before>rgba?)",
  " *[(] *",
  "(?<red>[0-9]+)",
  " *, *",
  "(?<green>[0-9]+)",
  " *, *",
  "(?<blue>[0-9]+)",
  "(?:",
  " *, *",
  "(?<alpha>[^)]+)",
  ")?",
  " *[)]")
ensure_rgba <- function(color.vec){
  match.mat <- str_match_perl(color.vec, rgba.pattern)
  is.not.rgb <- is.na(match.mat[,1])
  hex.vec <- toRGB(color.vec[is.not.rgb])
  not.rgb.mat <- col2rgb(hex.vec, alpha=TRUE)
  rgb.cols <- c("red", "green", "blue")
  match.mat[is.not.rgb, rgb.cols] <- t(not.rgb.mat[rgb.cols,])
  match.mat[is.not.rgb, "alpha"] <- not.rgb.mat["alpha",]/255
  is.rgb <- match.mat[, "before"] == "rgb"
  match.mat[is.rgb, "alpha"] <- 1
  is.transparent <- match.mat[, "alpha"] == 0
  match.mat[, rgb.cols] <- 0
  opacity <- as.numeric(match.mat[, "alpha"])
  if(any(is.na(opacity))){
    print(match.mat)
    stop("missing alpha opacity value")
  }
  match.mat[, "alpha"] <- paste(opacity)
  rgba.cols <- c(rgb.cols, "alpha")
  rgba.mat <- matrix(match.mat[, rgba.cols], nrow(match.mat), length(rgba.cols))
  no.paren <- apply(rgba.mat, 1, function(x)paste(x, collapse=", "))
  paste0("rgba(", no.paren, ")")
}
stopifnot(ensure_rgba("transparent") == ensure_rgba("rgba(0, 0, 0, 0.0)"))
stopifnot(ensure_rgba("rgba(0, 0, 0, 0.0)") == ensure_rgba("rgba(0, 0, 0, 0)"))
stopifnot(ensure_rgba("rgba(0, 0, 0, 0.1)") != ensure_rgba("rgba(0, 0, 0, 0)"))
stopifnot(ensure_rgba("rgb(0, 0, 0)") == ensure_rgba("rgba(0, 0, 0, 1)"))

expect_color <- function(computed.vec, expected.vec) {
  computed.rgb <- ensure_rgba(computed.vec)
  expected.rgb <- ensure_rgba(expected.vec)
  expect_identical(computed.rgb, expected.rgb)
}

expect_transform <- function(actual, expected, context = "translate", tolerance = 5) {
  # supports multiple contexts
  nocontext <- gsub(paste(context, collapse = "||"), "", actual)
  # reduce to some 'vector' of numbers: (a, b, c, ...)
  vec <- gsub("\\)\\(", ",", nocontext)
  clean <- gsub("\\)", "", gsub("\\(", "", vec))
  nums <- as.numeric(strsplit(clean, split = "\\,")[[1]])
  expect_equal(nums, expected, tolerance, scale = 1)
}

expect_links <- function(html, urls){
  expect_attrs(html, "a", "href", urls)
}

expect_attrs <- function(html, element.name, attr.name, urls){
  stopifnot(is.character(urls))
  xpath <- paste0("//", element.name)
  pattern <- paste0(attr.name, "$")
  node.set <- getNodeSet(html, xpath)
  rendered.urls <- rep(NA, length(node.set))
  for(node.i in seq_along(node.set)){
    node <- node.set[[node.i]]
    node.attrs <- xmlAttrs(node)
    href.i <- grep(pattern, names(node.attrs))
    if(length(href.i)==1){
      rendered.urls[[node.i]] <- node.attrs[[href.i]]
    }
  }
  for(u in urls){
    expect_true(u %in% rendered.urls)
  }
}

expect_styles <- function(html, styles.expected){
  stopifnot(is.list(styles.expected))
  stopifnot(!is.null(names(styles.expected)))
  geom <- getNodeSet(html, '//*[@class="geom"]')
  style.strs <- as.character(sapply(geom, function(x) xmlAttrs(x)["style"]))
  pattern <-
    paste0("(?<name>\\S+?)",
           ": *",
           "(?<value>.+?)",
           ";")
  style.matrices <- str_match_all_perl(style.strs, pattern)
  for(style.name in names(styles.expected)){
    style.values <- sapply(style.matrices, function(m)m[style.name, "value"])
    for(expected.regexp in styles.expected[[style.name]]){
      ## Test that one of the observed styles matches the expected
      ## regexp.
      expect_match(style.values, expected.regexp, all=FALSE)
    }
  }
}

getTextValue <- function(tick)xmlValue(getNodeSet(tick, "text")[[1]])

getStyleValue <- function(html, xpath, style.name) {
  node.list <- getNodeSet(html, xpath)
  style.vec <- sapply(node.list, function(node){
    attr.vec <- xmlAttrs(node)
    if("style" %in% names(attr.vec)){
      attr.vec[["style"]]
    }else{
      NA
    }
  })
  pattern <-paste0(
    "(?<name>\\S+?)",
    ": *",
    "(?<value>.+?)",
    ";")
  style.matrices <- str_match_all_perl(style.vec, pattern)
  sapply(style.matrices, function(m){
    ## style.name can be a vector of style names to extract!
    val.vec <- rep(NA, length(style.name))
    if(1 < length(style.name))names(val.vec) <- style.name
    found <- style.name %in% rownames(m)
    if(any(found)){
      style.found <- style.name[found]
      val.vec[found] <- m[style.found, "value"]
    }
    val.vec
  })
}

## testthat there is no warning generated by a piece of code.
expect_no_warning <- function(object, regexp, ...){
  expect_warning(object, NA)
}

getTransform <- function(tick)xmlAttrs(tick)[["transform"]]

## get difference between axis ticks in both pixels and on original data scale
## @param doc rendered HTML document
## @param ticks which ticks? (can use text label of the tick)
## @param axis which axis?
getTickDiff <- function(doc, ticks = 1:2, axis="x"){
  g.ticks <- getNodeSet(doc, "g[@class='tick major']")
  tick.labs <- sapply(g.ticks, getTextValue)
  names(g.ticks) <- tick.labs
  tick.transform <- sapply(g.ticks[ticks], getTransform)
  trans.mat <- str_match_perl(tick.transform, translatePattern)
  num <- as.numeric(trans.mat[, axis])
  val <- abs(diff(num))
  attr(val, "label-diff") <- diff(as.numeric(names(tick.transform)))
  val
}
both.equal <- function(x, tolerance = 0.1){
  if(is.null(x) || !is.vector(x) || length(x) != 2){
    return(FALSE)
  }
  isTRUE(all.equal(x[[1]], x[[2]], tolerance))
}

# normalizes tick differences obtained by getTickDiff
normDiffs <- function(xdiff, ydiff, ratio = 1) {
  xlab <- attr(xdiff, "label-diff")
  ylab <- attr(ydiff, "label-diff")
  if (is.null(xlab) || is.null(ylab)) warning("label-diff attribute is missing")
  c(ratio * xdiff / xlab, ydiff / ylab)
}


# Return the range of the geom in pixels as rendered in the browser
# Works for geom_point
get_pixel_ranges <- function(html=NULL, geom_class=NULL){
  if(is.null(html) || is.null(geom_class)){
    stop("please specify html and geom_class")
  }
  nodes <- getNodeSet(html,
                       paste0('//g[@class="', geom_class, '"]//circle'))
  attrs <- sapply(nodes, xmlAttrs)[c("cx", "cy"), ]
  if(is.matrix(attrs)){
    xranges <- range(as.numeric(attrs[1, ]), na.rm = T)
    yranges <- range(as.numeric(attrs[2, ]), na.rm = T)
  }else if(is.vector(attrs) && length(attrs) == 2){
    xranges <- range(as.numeric(attrs[["cx"]]), na.rm = T)
    yranges <- range(as.numeric(attrs[["cy"]]), na.rm = T)
  }else{
    return(NULL)
  }
  return(list(x=xranges, y=yranges))
}

# returns TRUE if two objects are unequal using all.equal
unequal <- function(object, expected, ...){
  !isTRUE(all.equal(object, expected, ...))
}


#' Run animint tests
#'
#' Convenience function for running animint tests.
#'
#' @param dir character string with the path to animint's source code. Defaults to current directory
#' @param filter If not NULL, only tests with file names matching
#' this regular expression will be executed. Matching will take on the
#' file name after it has been stripped of "test-" and ".r".
#' @export
#' @examples
#'
#' \dontrun{
#' # run tests in test-rotate.R with Firefox
#' tests_init("firefox")
#' tests_run(filter = "rotate")
#' # clean-up
#' tests_exit()
#' }
#'

tests_run <- function(dir = ".", filter = NULL) {
  if (!"package:RSelenium" %in% search())
    stop("Please load RSelenium: library(RSelenium)")
  if (!"package:testthat" %in% search())
    stop("Please load testthat: library(testthat)")
  testDir <- find_test_path(dir)
  # testthat::test_check assumes we are in path/to/animint/tests
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(dirname(testDir))
  # avoid weird errors if this function is called via testhat::check()
  # https://github.com/hadley/testthat/issues/144
  Sys.setenv("R_TESTS" = "")
  testthat::test_check("animint2", filter = filter)
}

#' Kill child process(es) that may have been initiated in animint testing
#'
#' Read process IDs from a file and kill those process(es)
#'
#' @seealso \link{tests_run}
#' @export
tests_exit <- function() {
  res <- stop_binary()
  Sys.unsetenv("ANIMINT_BROWSER")
  f <- file.path(find_test_path(), "pids.txt")
  if (file.exists(f)) {
    e <- try(readLines(con <- file(f), warn = FALSE), silent = TRUE)
    if (!inherits(e, "try-error")) {
      pids <- as.integer(e)
      res <- c(res, tools::pskill(pids))
    }
    close(con)
    unlink(f)
  }
  invisible(all(res))
}

#' Spawn a child R session that runs a 'blocking' command
#'
#' Run a blocking command in a child R session (for example a file server or shiny app)
#'
#' @param directory path that the  server should map to.
#' @param port port number to _attempt_ to run server on.
#' @param code R code to execute in a child session
#' @return port number of the successful attempt
run_servr <- function(directory = ".", port = 4848,
                      code = "servr::httd(dir='%s', port=%d)") {
  dir <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  cmd <- sprintf(
    paste("write.table(Sys.getpid(), file='%s', append=T, row.name=F, col.names=F);", code),
    file.path(find_test_path(), "pids.txt"), dir, port
  )
  system2("Rscript", c("-e", shQuote(cmd)), wait = FALSE)
}

# --------------------------
# Functions that are used in multiple places
# --------------------------

stop_binary <- function() {
  if (exists("pJS")) pJS$stop()
  # these methods are really queries to the server
  # thus, if it is already shut down, we get some arcane error message
  e <- try({
    remDr$closeWindow()
    remDr$closeServer()
  }, silent = TRUE)
  TRUE
}

# find the path to animint's testthat directory
find_test_path <- function(dir = ".") {
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  if (!grepl("animint", dir, fixed = TRUE))
    stop("animint must appear somewhere in 'dir'")
  base_dir <- basename(dir)
  if (!base_dir %in% c("animint2", "tests", "testthat"))
    stop("Basename of dir must be one of: 'animint2', 'tests', 'testhat'")
  ext_dir <- switch(base_dir,
                    animint2 = "tests/testthat",
                    tests = "testthat",
                    testthat = "")
  file.path(dir, ext_dir)
}

