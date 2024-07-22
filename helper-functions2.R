sendKey <- function(key, code, keyCode) {
  remDr$Input$dispatchKeyEvent(type = "keyDown", key = key, code = code, windowsVirtualKeyCode = keyCode, nativeVirtualKeyCode = keyCode)
  remDr$Input$dispatchKeyEvent(type = "keyUp", key = key, code = code, windowsVirtualKeyCode = keyCode, nativeVirtualKeyCode = keyCode)
}

clickID <- function(...){
  v <- c(...)
  stopifnot(length(v) == 1)
  runtime_evaluate(script=sprintf("document.getElementById('%s').dispatchEvent(new CustomEvent('click'))", as.character(v)))
}

runtime_evaluate <- function(script=NULL,return.value=FALSE){
  eval.result<- remDr$Runtime$evaluate(script,returnByValue = TRUE)
  if (return.value){
    eval.result$result$value
  }else{
    eval.result
  }
}