library(data.table)
(bisect.dt <- rowwiseDT(
  old=,     new=, label=, expected=, stage=,
  "Slow", "Fast", "", "?", "",
  "Fast", "CIfixed", "", "", ""))
for(bisect.i in 1:nrow(bisect.dt)){
  print(bisect.row <- bisect.dt[bisect.i])
  bisect.row[, atime::bisect("../..", "getCommonChunk improved in #238", old, new)]
}
