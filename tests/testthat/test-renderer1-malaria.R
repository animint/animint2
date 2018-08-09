library(testthat)
acontext("malaria data viz")

data(malaria, package = "animint2")

fp.fn.colors <- c(FP="skyblue",
                  fp="skyblue",
                  fn="#E41A1C",
                  FN="#E41A1C",
                  tn="white",
                  tp="grey",
                  errors="black")

chrom2int <- function(chrom){
  only.num <- sub("PyYM_([0-9]{2})_v1", "\\1", chrom)
  factor(as.integer(only.num), 1:14)
}

for(df.name in names(malaria)){
  df <- malaria[[df.name]]
  if("chrom" %in% names(df)){
    malaria[[df.name]]$chrom.fac <- chrom2int(df$chrom)
  }
}

rownames(malaria$amplicons) <- malaria$amplicons$LOCUS_ID
normalize <- function(LOCUS_ID, position){
  LID <- paste(LOCUS_ID)
  firstVariant <- malaria$amplicons[LID, ]$firstVariant
  lastVariant <- malaria$amplicons[LID, ]$lastVariant
  mid <- (firstVariant+lastVariant)/2
  left <- firstVariant
  left <- mid-200
  right <- mid+200
  bases <- right - left
  (position-left)/bases
}

malaria$error.variants$POS.norm <- with(malaria$error.variants, {
  normalize(LOCUS_ID, POS)
})
malaria$regions$regionStart.norm <- with(malaria$regions, {
  normalize(LOCUS_ID, regionStart)
})
malaria$regions$regionEnd.norm <- with(malaria$regions, {
  normalize(LOCUS_ID, regionEnd)
})
malaria$amplicons$firstVariant.norm <- with(malaria$amplicons, {
  normalize(LOCUS_ID, firstVariant)
})
malaria$amplicons$lastVariant.norm <- with(malaria$amplicons, {
  normalize(LOCUS_ID, lastVariant)
})

viz <-
  list(errorCurves=a_plot()+
         a_theme_bw()+
         ggtitle(paste("error curves, select MQ threshold"))+
         xlab("MQ threshold")+
         ylab("incorrectly called variants")+
         make_tallrect(malaria$error.curves, "filterVar.thresh")+
         a_geom_line(a_aes(filterVar.thresh, metric.value,
                       group=metric.name,
                       color=metric.name),
                   data=malaria$error.curves)+
         a_scale_color_manual(values=fp.fn.colors)+
         a_geom_text(a_aes(filterVar.thresh, metric.value+offset,
                       color=metric.name,
                       a_label=paste(metric.value, metric.name, " ")),
                   showSelected="filterVar.thresh",
                   hjust=1,
                   data=malaria$error.curves),

       chroms=a_plot()+
         a_theme_bw()+
         ggtitle("Sanger sequenced amplicons")+
         a_theme_animint(width=600)+
         a_geom_text(a_aes(chrom.fac, position/1e3,
                       a_label=sprintf("MQ threshold = %.1f",
                         filterVar.thresh)),
                   showSelected="filterVar.thresh",
                   data=malaria$filterVar.labels)+
         a_geom_text(a_aes(chrom.fac, position/1e3,
                       a_label=paste(fp, "fp_")),
                   showSelected=c("filterVar.thresh", "highly.divergent.regions",
                                  "annotation"),
                   clickSelects="LOCUS_ID",
                   hjust=1,
                   color=fp.fn.colors[["fp"]],
                   data=subset(malaria$error.amplicons, fp != 0))+
         a_geom_text(a_aes(chrom.fac, position/1e3,
                       a_label=paste0("_" , fn, " fn")),
                   clickSelects="LOCUS_ID",
                   showSelected=c("filterVar.thresh", "highly.divergent.regions",
                                "annotation"),
                   color=fp.fn.colors[["fn"]],
                   hjust=0,
                   data=subset(malaria$error.amplicons, fn != 0))+
         a_geom_segment(a_aes(chrom.fac, 0, 
                          yend=bases/1e3, xend=chrom.fac),
                      data=malaria$chroms)+
         a_geom_point(a_aes(chrom.fac, position/1e3,
                        color=highly.divergent.regions,
                        fill=annotation),
                    clickSelects="LOCUS_ID",
                    size=5,
                    pch=21,
                    data=malaria$amplicons)+
         a_scale_color_manual(values=c(none="white", some="black"))+
         a_scale_x_discrete("Malaria parasite yoelii yoelii chromosome",
                          drop=FALSE)+
         ylab("position on chromosome (kilo bases = kb)"),

       variants=a_plot()+
         a_theme_bw()+
         ggtitle("Variants in each sanger sequenced amplicon")+
         a_theme_animint(width=1000, height=600)+
         a_scale_fill_manual(values=fp.fn.colors)+
         a_scale_y_discrete("amplicon LOCUS_ID", drop=FALSE)+
         a_scale_x_continuous("relative position on amplicon",
                            limits=c(-0.05, 1.05),
                            breaks=c())+
         a_geom_text(a_aes(firstVariant.norm, LOCUS_ID,
                       a_label=paste0(firstVariant, "_")),
                   showSelected=c("highly.divergent.regions", "annotation"),
                   hjust=1,
                   data=malaria$amplicons)+
         a_geom_text(a_aes(lastVariant.norm, LOCUS_ID,
                       a_label=paste0("_", lastVariant, " --- ",
                                    lastVariant-firstVariant, " bases")),
                   showSelected=c("highly.divergent.regions", "annotation"),
                   hjust=0,
                   data=malaria$amplicons)+
         a_geom_segment(a_aes(firstVariant.norm, LOCUS_ID,
                          xend=lastVariant.norm, yend=LOCUS_ID),
                      showSelected=c("highly.divergent.regions", "annotation"),
                      clickSelects="LOCUS_ID",
                      size=12,
                      alpha=0.6,
                      data=malaria$amplicons)+
         a_geom_segment(a_aes(regionStart.norm, LOCUS_ID,
                          xend=regionEnd.norm, yend=LOCUS_ID,
                          color=region.type),
                      showSelected=c("highly.divergent.regions", "annotation"),
                      size=8,
                      data=malaria$regions)+
         a_scale_color_manual(values=c("#E41A1C", #red
                              "#377EB8", #blue
                              "#4DAF4A", #green
                              "#984EA3", #purple
                              "#FF7F00", #orange
                              LCR="#FFFF33", #yellow
                              "#A65628",
                              "#F781BF",
                                     HDR="black"))+
         a_geom_point(a_aes(POS.norm, LOCUS_ID,
                        tooltip=paste(Coding, Variant_type),
                        fill=error.type),
                    showSelected=c("highly.divergent.regions",
                                   "annotation", "filterVar.thresh"),
                    color="black",
                    pch=21,
                    size=4,
                    data=malaria$error.variants),

       first=list(filterVar.thresh=malaria$filterVar$best.thresh),

       title="Malaria parasite NextGenSeq variant calling errors")

expect_no_warning({
  info <- animint2HTML(viz)
})

expected.dot.df <- 
  subset(malaria$error.variants,
         filterVar.thresh==malaria$filterVar$best.thresh)
test_that("expected number of <circle> rendered", {
  circle.list <-
    getNodeSet(info$html, '//g[@class="a_geom13_point_variants"]//circle')
  expect_equal(length(circle.list), nrow(expected.dot.df))
})

region.lines <-
  getNodeSet(info$html, '//g[@class="a_geom12_segment_variants"]//line')
test_that("one line is rendered for each region", {
  expect_equal(length(region.lines), nrow(malaria$regions))
})

getFillStroke <- function(value){
  xpath <- sprintf(
    '//tr[@id="plot_chroms_highly_divergent_regions_variable_%s"]//circle',
    value)
  getStyleValue(info$html, xpath, c("stroke", "fill"))
}
some <- getFillStroke("some")
none <- getFillStroke("none")
test_that("a_geom_point(a_aes(color)) legend shows as circle stroke", {
  expect_true(some["stroke",] != none["stroke",])
  expect_true(some["fill",] == none["fill",])
})

get.opacity <- function(class.name){
  xpath <- sprintf(
    '//tr[@class="%s"]//td[@class="legend_entry_a_label"]',
    class.name)
  getStyleValue(getHTML(), xpath, "opacity")
}
before.vec <- get.opacity("annotation_variable")
test_that("INTERGENIC legend entry opacity 1 before clicking", {
  expect_identical(before.vec, c("1", "1", "1", "1"))
})

clickID("plot_chroms_annotation_variable_INTERGENIC")
after.vec <- get.opacity("annotation_variable")
test_that("INTERGENIC legend entry opacity 0.5 after clicking", {
  expect_identical(after.vec, c("1", "0.5", "1", "1"))
})

before.vec <- get.opacity("highly_divergent_regions_variable")
test_that("none legend entry opacity 1 before clicking", {
  expect_identical(before.vec, c("1", "1"))
})

clickID("plot_chroms_highly_divergent_regions_variable_none")
after.vec <- get.opacity("highly_divergent_regions_variable")
test_that("none legend entry opacity 0.5 after clicking", {
  expect_identical(after.vec, c("0.5", "1"))
})

## TODO: test number of geoms rendered in chroms and variants plots,
## before and after clicking.



