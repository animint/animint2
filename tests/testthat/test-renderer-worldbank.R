# test-renderer-worldbank.R

library(animint2)
library(data.table)
library(XML)

# Setup: Load and prepare data
data(WorldBank)

WorldBank$Region <- sub(" [(].*", "", WorldBank$region)

not.na <- data.table(WorldBank)[
  !(is.na(life.expectancy) | is.na(fertility.rate))
]

not.na[is.na(population), population := 1700000]

# Facet Helper Functions
FACETS <- function(df, top, side) {
  data.frame(
    df,
    top  = factor(top, c("Fertility rate", "Years")),
    side = factor(side, c("Years", "Life expectancy"))
  )
}

TS.LIFE <- function(df) FACETS(df, "Years", "Life expectancy")
SCATTER <- function(df) FACETS(df, "Fertility rate", "Life expectancy")
TS.FERT <- function(df) FACETS(df, "Fertility rate", "Years")
MAP.FAC <- function(df) FACETS(df, "Years", "Years")

years <- as.data.frame(unique(not.na[, .(year)]))

first.year <- min(not.na$year)
last.year <- max(not.na$year)

by.country <- split(not.na, not.na$country)
min.years <- rbindlist(lapply(by.country, function(d) d[year == min(year)]))
min.years[, year := first.year - 0.5]

# World map preparation
world_map <- animint2::map_data("world")
map_df <- world_map
map_names <- c(x = "long", y = "lat")

country2Region <- with(
  unique(not.na[, .(Region, country)]),
  structure(Region, names = country)
)

map2wb <- c(
  Antigua = "Antigua and Barbuda",
  Brunei = "Brunei Darussalam",
  Bahamas = "Bahamas, The",
  "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
  "Republic of Congo" = "Congo, Rep.",
  "Ivory Coast" = "Cote d'Ivoire",
  Egypt = "Egypt, Arab Rep.",
  Micronesia = "Micronesia, Fed. Sts.",
  UK = "United Kingdom",
  Gambia = "Gambia, The",
  Iran = "Iran, Islamic Rep.",
  Kyrgyzstan = "Kyrgyz Republic",
  "Saint Kitts" = "St. Kitts and Nevis",
  "North Korea" = "Korea, Dem. Rep.",
  "South Korea" = "Korea, Rep.",
  Laos = "Lao PDR",
  "Saint Lucia" = "St. Lucia",
  "North Macedonia" = "Macedonia, FYR",
  Palestine = "West Bank and Gaza",
  Russia = "Russian Federation",
  Slovakia = "Slovak Republic",
  "Saint Martin" = "Sint Maarten (Dutch part)",
  Syria = "Syrian Arab Republic",
  Trinidad = "Trinidad and Tobago",
  Tobago = "Trinidad and Tobago",
  USA = "United States",
  "Saint Vincent" = "St. Vincent and the Grenadines",
  Venezuela = "Venezuela, RB",
  "Virgin Islands" = "Virgin Islands (U.S.)",
  Yemen = "Yemen, Rep."
)

map_disp <- with(
  map_df,
  data.frame(
    group,
    country = ifelse(region %in% names(map2wb), map2wb[region], region)
  )
)

map_disp$Region <- country2Region[map_disp$country]
map_disp <- as.data.frame(map_disp)

for (new.var in names(map_names)) {
  old.var <- map_names[[new.var]]
  old.val <- map_df[[old.var]]
  m <- min(old.val, na.rm = TRUE)
  old.01 <- (old.val - m) / (max(old.val, na.rm = TRUE) - m)
  map_disp[[new.var]] <- old.01 * (last.year - first.year) + first.year
}

# Plot parameters
line_alpha <- 3 / 5
line_size <- 4

# Build plots
ts.right <- ggplot() +
  geom_tallrect(
    aes(xmin = year - 0.5, xmax = year + 0.5),
    clickSelects = "year",
    data = TS.LIFE(years),
    alpha = 1 / 2
  ) +
  geom_line(
    aes(year, life.expectancy, group = country, color = Region),
    clickSelects = "country",
    data = TS.LIFE(not.na),
    size = line_size,
    alpha = line_alpha
  )

ts.facet <- ts.right +
  theme_bw() +
  theme(panel.margin = grid::unit(0, "lines")) +
  facet_grid(side ~ top, scales = "free") +
  xlab("") +
  ylab("")

ts.scatter <- ts.facet +
  geom_point(
    aes(
      fertility.rate, life.expectancy,
      color = Region, size = population,
      key = country
    ),
    clickSelects = "country",
    showSelected = "year",
    data = SCATTER(not.na),
    alpha = 1,
    alpha_off = 0.3
  ) +
  geom_text(
    aes(
      fertility.rate, life.expectancy,
      label = country,
      key = country
    ),
    data = SCATTER(not.na),
    showSelected = c("country", "year"),
    vjust = 0,
    alpha = 0.7
  ) +
  scale_size_animint(pixel.range = c(2, 20), breaks = 10^(9:5))

scatter.both <- ts.scatter +
  geom_widerect(
    aes(ymin = year - 0.5, ymax = year + 0.5),
    clickSelects = "year",
    data = TS.FERT(years),
    alpha = 1 / 2
  ) +
  geom_path(
    aes(fertility.rate, year, group = country, color = Region),
    clickSelects = "country",
    data = TS.FERT(not.na),
    size = line_size,
    alpha = 1,
    alpha_off = 0.1
  ) +
  geom_label_aligned(
    aes(
      fertility.rate, year,
      key = country,
      colour = Region,
      label = country
    ),
    data = TS.FERT(min.years),
    showSelected = "country",
    clickSelects = "country",
    alignment = "horizontal",
    vjust = 1
  )

scatter.map <- scatter.both +
  geom_polygon(
    aes(
      x = x, y = y,
      key = group,
      group = group,
      fill = Region
    ),
    data = MAP.FAC(map_disp),
    title = "World map",
    clickSelects = "country",
    color = "black",
    color_off = "transparent",
    alpha = 1,
    alpha_off = 0.3
  )

# Execute: Create animint
viz <- animint(
  title = "World Bank data (multiple selection, facets, map)",
  scatter = scatter.map +
    theme_animint(width = 1000, height = 600),
  time = list(variable = "year", ms = 3000),
  duration = list(year = 1000),
  first = list(
    year    = 1975,
    country = c("United States", "Canada", "France", "Japan", "India")
  ),
  selector.types = list(country = "multiple"),
  options = list(
    width  = 1000,
    height = 600
  )
)

# Render to HTML
info <- animint2HTML(viz)

# ========================================
# TESTS
# ========================================

test_that("Output HTML is generated and valid", {
  expect_true(nzchar(saveXML(info$html)) > 0)
})

test_that("Page title is correct", {
  page.title <- xmlValue(getNodeSet(info$html, "//title")[[1]])
  expect_equal(page.title, "World Bank data (multiple selection, facets, map)")
})

test_that("Scatter plot SVG exists", {
  scatter.node <- getNodeSet(info$html, '//svg[@id="plot_scatter"]')
  expect_equal(length(scatter.node), 1,
    label = "Scatter plot SVG should exist"
  )
})

test_that("Visual elements (circles) are present", {
  circles <- getNodeSet(info$html, "//circle")
  expect_true(length(circles) > 0,
    label = "Should have circle elements for scatter points"
  )
})

test_that("Visual elements (paths) are present", {
  paths <- getNodeSet(info$html, "//path")
  expect_true(length(paths) > 0,
    label = "Should have path elements for lines"
  )
})

test_that("Visual elements (map polygons) are present", {
  polygons <- getNodeSet(info$html, "//polygon")
  path_elements <- getNodeSet(info$html, "//path[@d]")
  map_elements <- length(polygons) + length(path_elements)
  expect_true(map_elements > 0,
    label = "Should have polygon or path elements for world map"
  )
})

test_that("Interactive rectangles (selections) are present", {
  rects <- getNodeSet(info$html, "//rect")
  expect_true(length(rects) > 0,
    label = "Should have rect elements for year selectors"
  )
})

test_that("Text labels are present", {
  svg.text <- getNodeSet(info$html, "//text")
  all.text <- sapply(svg.text, xmlValue)
  expect_true(length(all.text) > 0,
    label = "Should have text elements"
  )
})

test_that("Initial countries are correctly selected and labeled", {
  svg.text <- getNodeSet(info$html, "//text")
  all.text <- sapply(svg.text, xmlValue)
  initial.countries <- c("United States", "Canada", "France", "Japan", "India")
  country.matches <- sapply(initial.countries, function(country) {
    any(grepl(country, all.text, fixed = TRUE))
  })
  expect_true(sum(country.matches) > 0,
    label = "At least one initial country should appear in text labels"
  )
})

