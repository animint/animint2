acontext("chunk vars")

test_that("produce as many chunk files as specified", {
  viz <- list(
    iris=ggplot()+
      geom_point(aes(
        Petal.Width, Sepal.Length),
        showSelected="Species",
        data=iris, chunk_vars="Species",
        validate_params = FALSE))
  tdir <- tempfile()
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 0)
  animint2dir(viz, tdir, open.browser=FALSE)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 3)
  viz <- list(
    iris=ggplot()+
      geom_point(aes(
        Petal.Width, Sepal.Length),
        showSelected="Species",
        data=iris,
        chunk_vars=character(), validate_params = FALSE))
  tdir <- tempfile()
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 0)
  animint2dir(viz, tdir, open.browser=FALSE)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 1)
})

test_that("produce informative errors for bad chunk_vars", {
  viz <- list(iris=ggplot()+
                geom_point(aes(Petal.Width, Sepal.Length),
                           showSelected="Species",
               data=iris, chunk_vars="species", validate_params = FALSE))
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "invalid chunk_vars species; possible showSelected variables: Species")
  viz <- list(iris=ggplot()+
                geom_point(aes(Petal.Width, Sepal.Length),
                           showSelected="Species",
               data=iris, chunk_vars=NA, validate_params = FALSE))
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, paste("chunk_vars must be a character vector;",
           "use chunk_vars=character() to specify 1 chunk"), fixed=TRUE)
})

data(breakpoints, package = "animint2")
only.error <- subset(breakpoints$error,type=="E")
only.segments <- subset(only.error,bases.per.probe==bases.per.probe[1])
signal.colors <- c(estimate="#0adb0a",
                   latent="#0098ef")
breakpointError <-
  list(signal=ggplot()+
         geom_point(aes(position, signal),
                    showSelected="bases.per.probe",
                    data=breakpoints$signals)+
         geom_line(aes(position, signal), colour=signal.colors[["latent"]],
                   data=breakpoints$imprecision)+
         geom_segment(aes(first.base, mean, xend=last.base, yend=mean),
                      showSelected=c("segments", "bases.per.probe"),
                      colour=signal.colors[["estimate"]],
                      data=breakpoints$segments)+
         geom_vline(aes(xintercept=base),
                    showSelected=c("segments", "bases.per.probe"),
                    colour=signal.colors[["estimate"]],
                    linetype="dashed",
                    data=breakpoints$breaks),
       error=ggplot()+
         geom_vline(aes(xintercept=segments),
                    clickSelects="segments",
                    data=only.segments, lwd=17, alpha=1/2)+
         geom_line(aes(segments, error, group=bases.per.probe),
                   clickSelects="bases.per.probe",
                   data=only.error, lwd=4))

bytes.used <- function(file.vec, apparent.size = FALSE){
  ## Note: the apparent.size flag gives sizes that are consistent
  ## with file.info, but those sizes actually under-estimate the
  ## actual amount of space used on disk.
  file.str <- paste(file.vec, collapse=" ")
  if(apparent.size){
    cmd <- paste("ls -l", file.str, "| awk '{print $5}'")
  } else{
    cmd <- paste("du -k", file.str, "| awk '{print $1 * 1024}'")
  }
  tryCatch({
    du.lines <- system(cmd, intern=TRUE)
    as.integer(sub("\t.*", "", du.lines))
  }, error=function(e){
    rep(NA_integer_, length(file.vec))
  })
}

test.paths <-
  c(tempfile=tempfile(),
    HOME=file.path(Sys.getenv("HOME"), "ANIMINT_TEST_FOO"),
    getwd=file.path(getwd(),"ANIMINT_TEST_FOO"))
for(f in test.paths){
  unlink(f)
  cat("foo", file=f)
}
du.bytes <- bytes.used(test.paths)
apparent.bytes <- bytes.used(test.paths, apparent.size = TRUE)
byte.df <- data.frame(
  du.bytes, apparent.bytes,
  file.size=file.size(test.paths),
  test.paths)
test_that("default chunks are at least 4KB", {
  tdir <- tempfile()
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 0)
  expect_no_warning({
    animint2dir(breakpointError, tdir, open.browser=FALSE)
  })
  tsv.files <- Sys.glob(file.path(tdir, ".+chunk[0-9]+.tsv"))  # exclude common tsv
  geom <- sub("_.*", "", basename(tsv.files))
  files.by.geom <- split(tsv.files, geom)
  for(files in files.by.geom){
    if(length(files) > 1){
      info <- file.info(files)
      expect_true(all(4096 < info$size))
    }
  }
})

## new common chunk tests.
panel_group <- function(x, y, panel, g)data.table(x, y, panel, g)

test_that("common chunk has x and colour not PANEL", {
  line_dt <- rbind(
    panel_group(0:1, rnorm(2), "A", "left"),
    panel_group(0:1, rnorm(2), "B", "left"),
    panel_group(0:1, rnorm(2), "C", "left"),
    panel_group(2:3, rnorm(2), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_line(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel")+
      facet_grid(. ~ panel))
  info <- animint2dir(viz, open.browser = FALSE)
  common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","PANEL","y"))
})

test_that("common chunk has x and colour", {
  line_dt <- rbind(
    panel_group(0:1, rnorm(2), "A", "left"),
    panel_group(0:1, rnorm(2), "B", "left"),
    panel_group(0:1, rnorm(2), "C", "left"),
    panel_group(2:3, rnorm(2), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_line(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2dir(viz, open.browser = FALSE)
  common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","y"))
})

test_that("common chunk has colour", {
  line_dt <- rbind(
    panel_group(0:1, rnorm(2), "A", "left"),
    panel_group(1:2, rnorm(2), "B", "left"),
    panel_group(0:1, rnorm(2), "C", "left"),
    panel_group(2:3, rnorm(2), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_line(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2dir(viz, open.browser = FALSE)
  common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","x","y"))
})

test_that("should not make common chunk", {
  line_dt <- rbind(
    panel_group(c(0,1,NA), rnorm(3), "A", "left"),
    panel_group(c(NA,1,2), rnorm(3), "B", "left"),
    panel_group(c(0,NA,2), rnorm(3), "C", "left"),
    panel_group(2, rnorm(1), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2dir(viz, open.browser = FALSE)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  expect_false(file.exists(common.tsv))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","x","y"))
})

test_that("x included in common chunk for path", {
  line_dt <- rbind(
    panel_group(c(0,1,NA), rnorm(3), "A", "left"),
    panel_group(c(0,NA,2), rnorm(3), "B", "left"),
    panel_group(c(NA,1,2), rnorm(3), "C", "left"),
    panel_group(c(3,NA), rnorm(2), "A", "mid"),
    panel_group(c(3,4),  rnorm(2), "B", "mid"),
    panel_group(c(5,6),  rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2dir(viz, open.browser = FALSE)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","y"))
})

## TODO fix this test.
## test_that("x included in common chunk for line", {
##   line_dt <- rbind(
##     panel_group(c(0,1,NA), rnorm(3), "A", "left"),
##     panel_group(c(0,NA,2), rnorm(3), "B", "left"),
##     panel_group(c(NA,1,2), rnorm(3), "C", "left"),
##     panel_group(c(3,NA), rnorm(2), "A", "mid"),
##     panel_group(c(3,4),  rnorm(2), "B", "mid"),
##     panel_group(c(5,6),  rnorm(2), "A", "right"))
##   viz <- animint(
##     lines=ggplot()+
##       geom_line(aes(
##         x, y, group=g, color=g),
##         data=line_dt,
##         chunk_vars="panel",
##         showSelected="panel"))
##   info <- animint2dir(viz, open.browser = FALSE)
##   common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
##   common.dt <- fread(common.tsv)
##   expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
##   chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
##   chunk1.dt <- fread(chunk1.tsv)
##   expect_identical(sort(names(chunk1.dt)), c("group","y"))
## })

test_that("x not included in common chunk (mid has one group with 2 rows and another with 3)", {
  line_dt <- rbind(
    panel_group(c(0,1,NA), rnorm(3), "A", "left"),
    panel_group(c(0,NA,2), rnorm(3), "B", "left"),
    panel_group(c(NA,1,2), rnorm(3), "C", "left"),
    panel_group(c(3,4,5), rnorm(3), "A", "mid"),
    panel_group(c(3,4), rnorm(2), "B", "mid"),
    panel_group(c(5,6), rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2dir(viz, open.browser = FALSE)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","x","y"))
})

test_that("x not included in common chunk (missing value in right)", {
  line_dt <- rbind(
    panel_group(c(0,1,NA), rnorm(3), "A", "left"),
    panel_group(c(0,NA,2), rnorm(3), "B", "left"),
    panel_group(c(NA,1,2), rnorm(3), "C", "left"),
    panel_group(c(3,4), rnorm(2), "A", "mid"),
    panel_group(c(3,4), rnorm(2), "B", "mid"),
    panel_group(c(NA,6), rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2dir(viz, open.browser = FALSE)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","x","y"))
})

