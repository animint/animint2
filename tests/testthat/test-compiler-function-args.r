context("function-args")

filter_args <- function(x) {
  all_names <- names(x)
  all_names <- setdiff(all_names, c("self", "data", "scales", "coordinates", "..."))
  x[all_names]
}

test_that("geom_xxx and GeomXxx$draw arg defaults match", {
  animint2_ns <- asNamespace("animint2")
  objs <- ls(animint2_ns)
  geom_fun_names <- objs[grepl("^(geom|annotation)_", objs)]
  # These aren't actually geoms, or need special parameters and can't be tested this way.
  geom_fun_names <- setdiff(
    geom_fun_names,
    c("geom_aesthetics", "geom_map", "annotation_custom", "annotation_map",
      "annotation_raster", "annotation_id")
  )

  # For each geom_xxx function and the corresponding GeomXxx$draw and
  # GeomXxx$draw_groups functions, make sure that if they have same args, that
  # the args have the same default values.
  lapply(geom_fun_names, function(geom_fun_name) {
    geom_fun    <- animint2_ns[[geom_fun_name]]
    draw        <- geom_fun()$geom$draw_layer
    draw_groups <- geom_fun()$geom$draw_group

    fun_args <- formals(geom_fun)
    draw_args <- c(gganimintproto_formals(draw), gganimintproto_formals(draw_groups))
    draw_args <- filter_args(draw_args)

    common_names <- intersect(names(fun_args), names(draw_args))

    expect_identical(fun_args[common_names], draw_args[common_names],
      info = paste0("Mismatch between arg defaults for ", geom_fun_name,
        " and ", class(geom_fun()$geom)[1], "'s $draw and/or $draw_group functions.")
    )
  })
})
