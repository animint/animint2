a_aesthetics <- function(x) {
  req_aes <- x$required_aes
  def_aes <- names(x$default_aes)
  def_aes <- setdiff(def_aes, req_aes)
  if (length(req_aes) == 0) {
    # Suppress warnings which occur when sorting NULL
    return(suppressWarnings(sort(names(x$default_aes))))
  }
  if (length(def_aes) == 0) {
    return(paste("\\strong{", sort(x$required_aes), "}",sep = ""))
  }
  return(c(paste("\\strong{", sort(x$required_aes), "}", sep = ""), sort(def_aes)))
}
a_geom_aesthetics <- function(x) {
  a_aesthetics(find_subclass("a_Geom", x))
}
a_stat_aesthetics <- function(x) {
  a_aesthetics(find_subclass("a_Stat", x))
}


rd_aesthetics <- function(type, name) {
  obj <- switch(type,
    a_geom = find_subclass("a_Geom", name),
    a_stat = find_subclass("a_Stat", name)
  )
  a_aes <- a_aesthetics(obj)

  paste("\\code{", type, "_", name, "} ",
    "understands the following aesthetics (required aesthetics are in bold):\n\n",
    "\\itemize{\n",
    paste("  \\item \\code{", a_aes, "}", collapse = "\n", sep = ""),
    "\n}\n", sep = "")
}
