library(data.table)
if(FALSE){
  atime::atime_versions_remove("animint2")
}
(bisect.dt <- rowwiseDT(
  old=,     new=, label=, expected=, stage=,
  "Slow", "Fast", "", "?", "",
  "Fast", "CIfixed", "", "", "",
  "FixLegend", "CIfixed", "", "", ""))
bisect.i <- 3
for(bisect.i in 1:nrow(bisect.dt)){
  print(bisect.row <- bisect.dt[bisect.i])
  bisect.row[, atime::bisect(normalizePath("../.."), "getCommonChunk improved in #238", old, new)]
}
