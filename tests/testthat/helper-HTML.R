#' Apply `animint2dir` to a list ggplots and extract the (rendered) page source via RSelenium
#'
#' @param plotList A named list of ggplot2 objects
animint2HTML <- function(plotList) {
  res <- animint2dir(plotList, out.dir = "animint-htmltest",
                     open.browser = FALSE)
  remDr$refresh()
  Sys.sleep(1)
  res$html <- getHTML()
  ## [ERROR - 2019-06-05T18:30:55.358Z] Session [e7c4e500-871e-11e9-a9b5-8dab1f486f7e] - page.onError - msg: TypeError: 'undefined' is not an object (evaluating 's_info.type')
  ## [ERROR - 2019-06-05T18:30:55.360Z] Session [e7c4e500-871e-11e9-a9b5-8dab1f486f7e] - page.onError - stack:
  ##   (anonymous function) (http://localhost:4848/animint-htmltest/animint.js:2535)
  ## print(sapply(res$selectors, "[[", "type"))
  res
}
getHTML <- function(){
  XML::htmlParse(remDr$getPageSource(), asText = TRUE)
}
#' Initiate external processes necessary for running tests.
#'
#' Initiates a local file server and remote driver.
#'
#' @param browserName Name of the browser to use for testing.
#' See ?RSelenium::remoteDriver for details.
#' @param dir character string with the path to animint's source code. Defaults to current directory
#' @param port port number used for local file server
#' @param ... list of additional options passed onto RSelenium::remoteDriver
#' @return invisible(TRUE)
#' @export
#' @seealso \link{tests_run}
#'
tests_init <- function(browserName = "phantomjs", dir = ".", port = 4848, ...) {
  # try to exit out of previously initated processes
  ex <- tests_exit()
  # start a non-blocking local file server under path/to/animint/tests/testhat
  testPath <- find_test_path(dir)
  run_servr(port = port, directory = testPath)
  # animint tests are performed in path/to/testthat/animint-htmltest/
  # note this path has to match the out.dir argument in animint2THML...
  testDir <- file.path(testPath, "animint-htmltest")
  # if the htmltest directory exists, wipe clean, then create an empty folder
  unlink(testDir, recursive = TRUE)
  dir.create(testDir)
  # start-up remote driver
  if (browserName == "phantomjs") {
    message("Starting phantomjs binary. To shut it down, run: \n pJS$stop()")
    pJS <<- RSelenium::phantom()
  } else {
    message("Starting selenium binary. To shut it down, run: \n",
            "remDr$closeWindow() \n",
            "remDr$closeServer()")
    RSelenium::checkForServer(dir = system.file("bin", package = "RSelenium"))
    selenium <- RSelenium::startServer()
  }
  # give an binaries a moment to start up
  Sys.sleep(8)
  remDr <<- RSelenium::remoteDriver(browserName = browserName, ...)
  # give the backend a moment to start-up
  Sys.sleep(6)
  remDr$open(silent = TRUE)
  Sys.sleep(2)
  # some tests don't run reliably with phantomjs (see tests-widerect.R)
  Sys.setenv("ANIMINT_BROWSER" = browserName)
  # wait a maximum of 30 seconds when searching for elements.
  remDr$setTimeout(type = "implicit", milliseconds = 30000)
  # wait a maximum of 30 seconds for a particular type of operation to execute
  remDr$setTimeout(type = "page load", milliseconds = 30000)
  # if we navigate to localhost:%s/htmltest directly, some browsers will
  # redirect to www.htmltest.com. A 'safer' approach is to navigate, then click.
  remDr$navigate(sprintf("http://localhost:%s/animint-htmltest/", port))
  ## Why not just navigate to the right URL to begin with?
  ## e <- remDr$findElement("xpath", "//a[@href='animint-htmltest/']")
  ## e$clickElement()
  invisible(TRUE)
}
## get both horizontal and vertical grid lines
get_grid_lines <- function(html, p_name, grid_class){
  path.i <-
    '//svg[@id="plot_%s"]//g[@class="grid_%s"]//g[@class="%s"]//line'
  path.hor <- sprintf(path.i, p_name, grid_class, "y")
  path.vert <- sprintf(path.i, p_name, grid_class, "x")
  nodes_h <- getNodeSet(html, path.hor)
  nodes_v <- getNodeSet(html, path.vert)
  # take x1, x2, y1, y2 values only
  attr_h <- sapply(nodes_h, xmlAttrs)[1:4, ]
  attr_v <- sapply(nodes_v, xmlAttrs)[1:4, ]
  attr_h <- apply(attr_h, 2, as.numeric)
  attr_v <- apply(attr_v, 2, as.numeric)
  return(list(hor=attr_h, vert=attr_v))
}

