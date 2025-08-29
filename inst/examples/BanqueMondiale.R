library(animint2)
data(BanqueMondiale, package="animint2fr")
not.na <- subset(BanqueMondiale, !(is.na(espérance.de.vie) | is.na(taux.de.fertilité)))
subset(not.na, is.na(not.na$population))
subset(not.na, pays == "Kuwait" & 1991 <= année & année <= 1995)
not.na[not.na$pays=="Kuwait", "population"] <- 1700000
BOTH <- function(df, top, side)data.frame(
  df,
  top=factor(top, c("Taux de fertilité", "Année")),
  side=factor(side, c("Année", "Espérance de vie")))
TS <- function(df)BOTH(df, "Année", "Espérance de vie")
SCATTER <- function(df)BOTH(df, "Taux de fertilité", "Espérance de vie")
TS_FERT <- function(df)BOTH(df, "Taux de fertilité", "Année")
MAP <- function(df)BOTH(df, "Année", "Année")
années <- unique(not.na[, "année", drop=FALSE])
by.pays <- split(not.na, not.na$pays)
prem.année <- min(not.na$année)
dern.année <- max(not.na$année)
min.années <- do.call(rbind, lapply(by.pays, subset, année == min(année)))
min.années$année <- 1959.5
année.breaks <- seq(1960,2010,by=10)
map_df <- animint2::map_data("world")
pays2région <- with(unique(not.na[, c("région","pays")]), structure(région, names=pays))
unique(subset(map_disp, is.na(région))$pays)
map2wb <- c(
  Antigua="Antigua and Barbuda",
  Brunei="Brunei Darussalam",
  Bahamas="Bahamas, The", 
  "Democratic Republic of the Congo"="Congo, Dem. Rep.",
  "Republic of Congo"="Congo, Rep.",
  "Ivory Coast"="Cote d'Ivoire",
  Egypt="Egypt, Arab Rep.", 
  Micronesia="Micronesia, Fed. Sts.",
  UK="United Kingdom",
  Gambia="Gambia, The",
  ##"Hong Kong SAR, China", 
  Iran="Iran, Islamic Rep.",
  ##"Channel Islands",
  Kyrgyzstan="Kyrgyz Republic",
  "Saint Kitts"="St. Kitts and Nevis", 
  "North Korea"="Korea, Dem. Rep.",
  "South Korea"="Korea, Rep.",
  Laos="Lao PDR",
  "Saint Lucia"="St. Lucia",
  "North Macedonia"="Macedonia, FYR", 
  ##"Macao SAR, China",
  Palestine="West Bank and Gaza",
  Russia="Russian Federation", 
  Slovakia="Slovak Republic",
  "Saint Martin"="Sint Maarten (Dutch part)",
  Syria="Syrian Arab Republic", 
  Trinidad="Trinidad and Tobago",
  Tobago="Trinidad and Tobago",
  USA="United States",
  "Saint Vincent"="St. Vincent and the Grenadines", 
  Venezuela="Venezuela, RB",
  "Virgin Islands"="Virgin Islands (U.S.)",
  Yemen="Yemen, Rep.")
map_df$pays <- with(map_df, ifelse(
  region %in% names(map2wb), map2wb[region], region))
map_df$région <- pays2région[map_df$pays]
map_names <- c(x="long", y="lat")
for(new.var in names(map_names)){
  old.var <- map_names[[new.var]]
  old.val <- map_df[[old.var]]
  m <- min(old.val)
  old.01 <- (old.val-m)/(max(old.val)-m)
  map_df[[new.var]] <- old.01*(dern.année-prem.année)+prem.année
}
map_disp <- subset(map_df, !is.na(région))

unot <- function(DFu, DFref)unique(subset(DFu, !pays %in% DFref$pays)$pays)
unot(map_disp, not.na)
dput(unot(not.na, map_disp))
wb.facets <- animint(
  ts=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1100, height=600)+
    facet_grid(side ~ top, scales="free")+
    ## TS_FERT
    make_widerect(
      not.na, "année", data.fun=TS_FERT,
      title="Rectangle gris pour l'année")+
    geom_label_aligned(aes(
      taux.de.fertilité, année,
      key=pays,
      colour=région, label=pays),
      showSelected="pays",
      clickSelects="pays",
      help="Noms des pays sélectionnés.",
      alignment="horizontal",
      data=TS_FERT(min.années),
      vjust=1)+
    geom_path(aes(
      taux.de.fertilité, année,
      key=pays, group=pays,
      colour=région),
      clickSelects="pays",
      help="Série temporelle pour taux de fertilité, une courbe pour chaque pays.",
      data=TS_FERT(not.na),
      size=4,
      alpha=1,
      alpha_off=0.1)+
    ## MAP
    geom_polygon(aes(
      x, y,
      key=group,
      group=group,
      fill=région),
      title="Carte du monde",
      clickSelects="pays",
      color="black",
      color_off="transparent",
      alpha=1,
      alpha_off=0.3,
      data=MAP(map_disp))+
    ## SCATTER
    geom_point(aes(
      taux.de.fertilité, espérance.de.vie, colour=région, size=population,
      key=pays), # key aesthetic for smooth transitions!
      clickSelects="pays",
      showSelected="année",
      help="Nuage de points pour l'année sélectionnée, un point pour chaque pays.",
      alpha=1,
      alpha_off=0.3,
      chunk_vars=character(),
      data=SCATTER(not.na))+
    geom_text(aes(
      taux.de.fertilité, espérance.de.vie, label=pays,
      key=pays), #also use key here!
      showSelected=c("pays", "année", "région"),
      clickSelects="pays",
      alpha=0.7,
      help="Noms des pays sélectionnés.",
      chunk_vars=character(),
      data=SCATTER(not.na))+
    geom_text(aes(
      5, 85, label=paste0("année = ", année),
      key=1),
      showSelected="année",
      title="L'année sélectionnée",
      data=SCATTER(années))+
    ## TS
    make_tallrect(
      not.na, "année", data.fun=TS,
      title="Rectangle gris pour l'année")+
    geom_line(aes(
      année, espérance.de.vie,
      key=pays, group=pays,
      colour=région),
      clickSelects="pays",
      help="Série temporelle pour l'espérance de vie, une courbe pour chaque pays.",
      data=TS(not.na),
      size=4,
      alpha=1,
      alpha_off=0.1)+
    geom_label_aligned(aes(
      année, espérance.de.vie,
      key=pays,
      colour=région, label=pays),
      showSelected="pays",
      clickSelects="pays",
      help="Noms des pays sélectionnés.",
      data=TS(min.années),
      hjust=1)+
    ## SCALES
    scale_size_animint(breaks=10^(9:5))+
    scale_x_continuous(
      "",
      breaks=c(année.breaks, 1:9))+
    scale_y_continuous(
      "",
      breaks=c(année.breaks, seq(25,85,by=10))
    ),
  time=list(variable="année", ms=2000),
  duration=list(année=1000, pays=1000, région=1000),
  first=list(année=1975, pays=c("United States", "Canada", "France", "Japan")),
  selector.types=list(pays="multiple"),
  source="https://github.com/animint/animint2/blob/master/inst/examples/BanqueMondiale.R",
  out.dir="BanqueMondiale-facets-map",
  video="https://youtu.be/O57kdpZZ03k",
  title="Données de la Banque Mondiale (séléction multiple, facet_grid, carte)")
if(Sys.which("firefox")!="")options(browser="firefox")
wb.facets
if(FALSE){
  animint2pages(wb.facets, "2025-08-BanqueMondiale-facets-map")
}
