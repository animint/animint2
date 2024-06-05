acontext("global variables")

viz <- list(scatter=ggplot()+
              geom_point(aes(y=Petal.Length, x=Sepal.Length,
                             color=Species),
                         showSelected="Species",
                         data=iris))

myScript <- 'myArray = [];
for(var b in window) { 
  if(window.hasOwnProperty(b)) {myArray.push(b);} 
}
return myArray;'

myScript2 <- 'myArray = [];
for(var b in window) { 
  if(window.hasOwnProperty(b)) {myArray.push(b);} 
}'

getVariables <- function(){
  if (remDr$browserName=="chromote"){
    
    vars <- remDr$Runtime$evaluate(myScript2,returnByValue = FALSE)
    print(vars)
  } else {
    vars <- remDr$executeScript(myScript)
  }  
  # ignore the "plot" variable -- 
  # https://github.com/tdhock/animint/pull/62#issuecomment-100008532
  # also ignore jQuery1238915281937 variable:
  grep("plot|jQuery", vars, value=TRUE, invert=TRUE)
}

    
   
    
  
  


test_that("animint.js only defines 1 object, called animint", {
  info <- animint2HTML(viz)
  animint.vars <- getVariables()
  #print(animint.vars)
  index.file <- file.path("animint-htmltest", "index.html")
  #print(index.file)
  html.lines <- readLines(index.file)
  #print(html.lines)
  html.without <- html.lines[!grepl("animint.js", html.lines)]
  #print(html.without)
  cat(html.without, file=index.file, sep="\n")
  # Note: It's OK for the webdriver to spit out 
  # ReferenceError: Can't find variable: animint
  # since we've removed the animint.js script
  remDr$refresh()
  without.vars <- getVariables()
  #print(without.vars)
  diff.vars <- animint.vars[!animint.vars %in% without.vars]
  print(diff.vars)
  expect_identical(diff.vars, "animint")
})
