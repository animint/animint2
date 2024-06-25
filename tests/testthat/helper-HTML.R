#' Apply `animint2dir` to a list ggplots and extract the (rendered) page source via RSelenium
#'
#' @param plotList A named list of ggplot2 objects
animint2HTML <- function(plotList) {
  unlink("animint-htmltest", recursive=TRUE)
  res <- animint2dir(plotList, out.dir = "animint-htmltest",
                     open.browser = FALSE)
  remDr$refresh()
  Sys.sleep(2)
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
#' @param port port portnumber used for local file server
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
  # start-up remote driver
  remotePort <- 4444L
  OS <- Sys.info()[['sysname']]
  if(OS == "Linux") {
    animint_server <- "localhost"   
  }
  if(OS == "Windows" || OS == "Darwin") {
    animint_server <- "host.docker.internal"
  }
  if(browserName == "chromote"){
    chrome.session <- chromote::ChromoteSession$new()
    
    chrome.session$view()
    chrome.session$refresh <- function(){
      ## from https://github.com/rstudio/chromote?tab=readme-ov-file#loading-a-page-reliably
      prom <- chrome.session$Page$loadEventFired(wait_ = FALSE)  # Get the promise for the loadEventFired
      chrome.session$Page$reload()
      # Block until p resolves
      chrome.session$wait_for(prom)
    }
    chrome.session$navigate <- function(u){
      chrome.session$Page$navigate(u)
    }
    chrome.session$getPageSource <- function(){
      doc <- chrome.session$DOM$getDocument()
      chrome.session$DOM$getOuterHTML(doc$root$nodeId)$outerHTML
    }
    remDr <<- chrome.session
    remDr$browserName <-"chromote"
    remDr$navigate(sprintf("http://localhost:4848/animint-htmltest/"))
  }else{
    if (browserName == "phantomjs") {
      message("Starting phantomjs binary. To shut it down, run: \n pJS$stop()")
      pJS <<- wdman::phantomjs(
        port = remotePort,
        phantomver = "latest"
      )
      ## Give time for phantomjs binary to start
      animint_server <- "localhost"
      Sys.sleep(8)  
    } else if(browserName=="firefox"){
      ## If using firefox, you'll need to run selenium-firefox docker image in order to make it work correctly.
      ## We're using docker to avoid version incompatibility issues.
      message("You need to run selenium docker image(selenium/standalone-firefox:2.53.0) as specified in docs(https://github.com/tdhock/animint2/wiki/Testing). \nNote: Ignore if already running.")
    }else stop("unrecognized browser name")
    remDr <<- RSelenium::remoteDriver(
      port = remotePort,
      browser = browserName,
      )
    ## wait for the remote driver to start-up
    Sys.sleep(6)
    remDr$open(silent = TRUE)
    ## some tests don't run reliably with phantomjs (see tests-widerect.R)
    Sys.setenv("ANIMINT_BROWSER" = browserName)
    ## wait a maximum of 30 seconds when searchinsg for elements.
    remDr$setTimeout(type = "implicit", milliseconds = 30000)
    ## wait a maximum of 30 seconds for a particular type of operation to execute
    remDr$setTimeout(type = "page load", milliseconds = 30000)
    ## if we navigate to localhost:%s/htmltest directly, some browsers will
    ## redirect to www.htmltest.com. A 'safer' approach is to navigate, then click.
    remDr$browserName <-""
    
    remDr$navigate(sprintf("http://%s:%s/animint-htmltest/", animint_server, port))
  }
  
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
