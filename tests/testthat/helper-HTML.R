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
tests_init <- function(dir = ".", ...) {
  # try to exit out of previously initated processes
  ex <- tests_exit()
  # start a non-blocking local file server under path/to/animint/tests/testhat
  testPath <- find_test_path(dir)
  run_servr(port = 4848, directory = testPath)
  # animint tests are performed in path/to/testthat/animint-htmltest/
  # note this path has to match the out.dir argument in animint2THML...
  testDir <- file.path(testPath, "animint-htmltest")
  # if the htmltest directory exists, wipe clean, then create an empty folder
  unlink(testDir, recursive = TRUE)
  options(chromote.timeout = 120)
  chrome.session <- chromote::ChromoteSession$new()
  # Enable required DevTools domains for coverage
  chrome.session$Runtime$enable()
  chrome.session$Profiler$enable()

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
  remDr$navigate("http://localhost:4848/animint-htmltest/")
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
### The hex codes come from
### https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
key2hex_code <- c(
  Backspace="08",
  Enter="0D",
  ArrowDown="28")
### https://chromedevtools.github.io/devtools-protocol/tot/Input/ says
### that dispatchKeyEvent() requires DOM key codes (in decimal) for
### the windowsVirtualKeyCode and nativeVirtualKeyCode arguments.
key2dec_code <- structure(
  strtoi(key2hex_code,base=16),
  names=names(key2hex_code))
# Function to send a key event
sendKey <- function(key) {
  stopifnot(is.character(key))
  for (type in c("keyDown", "keyUp")) {
    remDr$Input$dispatchKeyEvent(type = type, key = key, code = key, windowsVirtualKeyCode = key2dec_code[[key]], nativeVirtualKeyCode = key2dec_code[[key]])
  }
}

getClassBound <- function(geom.class, position){
  script.txt <- sprintf(
    'document.getElementsByClassName("%s")[0].getBoundingClientRect().%s', 
    geom.class, position)
  Sys.sleep(2)
  runtime_evaluate(script=script.txt)
}

# JS Coverage collection functions
start_js_coverage <- function() {
  tryCatch({
    remDr$Profiler$enable()
    remDr$Profiler$startPreciseCoverage(
      callCount = TRUE,
      detailed = TRUE
    )
    TRUE
  }, error = function(e) {
    warning("Failed to start JS coverage: ", e$message)
    FALSE
  })
}

stop_js_coverage <- function() {
  tryCatch({
    cov <- remDr$Profiler$takePreciseCoverage()
    outfile <- "js-coverage.json"
    # Ensure the format matches what v8-to-istanbul expects
    coverage_data <- list(
      result = cov$result,
      url = "http://localhost:4848/animint-htmltest/animint.js"
    )
    jsonlite::write_json(coverage_data, outfile, auto_unbox = TRUE)
    message("JS coverage saved to ", normalizePath(outfile))
    TRUE
  }, error = function(e) {
    warning("Failed to save JS coverage: ", e$message)
    FALSE
  })
}