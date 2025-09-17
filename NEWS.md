# Changes in version 2025.9.16

- `animint2pages()` gains arguments `chromote_width` and `chromote_height`, both dimensions in pixels of browser window used to create `Capture.PNG`, new defaults 3000x2000 should be large enough to handle most typical data viz (previous default width was not on Ubuntu).
- `update_gallery()` uses `method="curl"` when downloading `Capture.PNG` (this fixes previous default method which resulted in PNG with errors).

# Changes in version 2025.9.12 (PR#226)

- Tooltip is hidden after clicking to de-select.

# Changes in version 2025.9.11 (PR#224)

- Bugfix when `meke_tallrect()` is last geom, now selection menu renders. Thanks @suhaani-agarwal for the PR.

# Changes in version 2025.9.10 (PR#223)

- `geom_label_aligned()` and `aes(tooltip)` now work in Rmd docs rendered to HTML.

# Changes in version 2025.9.9 (PR#153)

- Multiple ggplots may now be arranged on the page via `theme_animint()` options `colspan=2`, `rowspan=2`, `last_in_row=TRUE` (meaning next ggplot occurs in next row). Backwards-compatible since old arrangement code is used when none of these options is present. Thanks to @biplab-sutradhar for contributions to the PR.

# Changes in version 2025.9.6 (PR#220)

- `aes(tooltip)`: Fixed an issue where tooltips containing newline characters (e.g. `"two\nlines"`) caused rendering failures when combined with other aesthetics such as `aes(color=...)`. The problem was due to TSV export with `quote=FALSE`, which broke parsing when fields contained newlines. The TSV writer now uses `quote="auto"`, ensuring fields with `\n` are preserved correctly. On the rendering side, newline characters in tooltips are converted to `<br/>`, so multi-line tooltips are now properly supported. Thanks @suhaani-agarwal for the PR.

# Changes in version 2025.8.16 (PR#214)

- `geom_abline()`: Fixed an issue where lines extended beyond plot boundaries when `update_axes()` was called. The clipping logic has now been moved from `pre_process()` on the R side to the renderer side, ensuring that `<line>` elements are correctly clipped to the plot boundaries during `update_axes()`. Thanks @suhaani-agarwal for the PR.

# Changes in version 2025.7.21 (PR#203)

- Added `geom_label_aligned()`, a new geom that plots text labels with non-overlapping positioning along a specified alignment axis ("horizontal" or "vertical"). It uses quadratic programming to optimize label placement and includes options for spacing (`min_distance`), alignment, rounded background rectangles (`label_r`), disabling the background rectangle (`background_rect = FALSE`), etc. Thanks @suhaani-agarwal for the PR.

# Changes in version 2025.7.18 (PR#201)

- `animint2pages()` default branch is gh-pages instead of main.
- `update_gallery()` creates repos/* directories and removes errors.csv file if necessary.

# Changes in version 2025.7.10 (PR#208)

- Added Codecov integration for both R and JavaScript tests; [coverage reports now available](https://app.codecov.io/github/animint/animint2). Thanks @suhaani-agarwal for the PR.

# Changes in version 2025.6.28 (PR#204) 

- `geom_raster()` has been removed. Thanks to @biplab-sutradhar for the PR.

# Changes in version 2025.6.4 (PR#197)

- `aes(tooltip = "...")` now renders fast, lightweight tooltips using D3. Thanks @suhaani-agarwal for the PR.

# Changes in version 2025.1.28

## PR#185

- Tour highlights the geom in the correct plot (instead of always the first viz on the page).

## PR#186

- `geom_abline()`: fix and vectorize `pre_process()` method.

## PR#184

- Add simple Hello world example to ?animint.
- Increase text size of "a" in legend SVG.

# Changes in version 2025.1.25 (PR#182)

- Tour text includes selector names for geoms with named clickSelects/showSelected.
- `animint2pages(chromote_sleep_seconds=NULL)` is the new default (no screenshot).
- knit_print.animint supports Start Tour button.

# Changes in version 2025.1.24 (PR#164)

- New Start Tour widget at the bottom of each data viz, which highlights what interactions are possible with each geom. Use `geom_*(title="title for geom in tour", help="details about what this geom is supposed to represent)` to change what is displayed for each geom during the tour. Powered by https://driverjs.com/

# Changes in version 2025.1.21 (PR#181)

- `animint(video="https://some.url")` renders the link at the bottom
  of the data viz.
- `update_gallery()` uses `download.file()` instead of git clone (faster).

# Changes in version 2024.11.27 (PR#170)

- When scale_log10 changes -Inf to NA, Inf handling logic now works
  (previous was error, user should still fix their code to change -Inf
  to 0).

# Changes in version 2024.11.11 (PR#166)

- if(require(maps)) in test-compiler-animation.R.

# Changes in version 2024.11.2 (PR#163)

- Added [gtable] to \link{gtable} to fix new CRAN issue (\link{} targets missing package anchors).

# Changes in version 2024.10.10 (PR#154)

- Updated `animist.js` to use classes for source links, resolving issue #145.

# Changes in version 2024.9.18 (PR#131)
  
- Added functionality to capture screenshot in animint2pages

# Changes in version 2024.9.17

- @export for internal S3 methods, to quiet roxygen2::document() messages.
 
# Changes in version 2024.8.29 (PR#147)
 
- animint2pages initial commit README.md uses viz title.
- animint2pages gains owner argument, with default taken from `gh::gh_whoami` (same as before, always returns user, even when token has permissions on an org); this allows user to specify an org where the user/token has write permissions. This is used in a new test-compiler-ghpages.R, which now assumes `PAT_GITHUB` has Administration and Contents permissions for all repos in `animint-test` org, so our test code can delete the `animint2pages_test_repo`, create a new one, and then use animint2pages twice, to test the results of creation/update.

# Changes in version 2024.8.27 (PR#144)
 
- Remove selenium, combine renderer tests into single CI job.

# Changes in version 2024.6.6 (PR#126)
 
- Add chromote as headless browser for testing and remove phantomjs/firefox support

# Changes in version 2024.3.12 (PR#119)
 
- Add validation checks for duplicate args passed geom and aes
  
# Changes in version 2024.2.4 (PR#116)
 
- Add validation checks for duplicate and missing args passed to animint.

# Changes in version 2024.1.24 (PR#112)

- move servr from Suggests to Imports, for more user-friendly installation / getting started for newbies (no need for separate install command for servr).
- bugfix in compiler height_proportion computation, which occured in ggplots with space=free and both vertical/horizontal panels. Before the vertical panels were always the same size, now they can be different sizes.

# Changes in version 2023.11.21

- setDTthreads(1) in CRAN testthat.R (created from build.sh).
- use servr by default when open.browser=TRUE.

# Changes in version 2023.11.16 (PR#88)

- Before creating a new out.dir, do not remove old out.dir, and
  instead stop with an error, if out.dir already exists, but
  animint.js does not.

# Changes in 2023.11.15 (PR#101)

- New function `animint2pages(viz,"new_github_repo")` for
  publishing/sharing animints, replacement for animint2gist, which
  stopped working recently.
- New option `animint(source="http://path.to/source.R")` which should
  be the URL of data viz source code, used to display a link below the
  rendered viz.
- New function `update_gallery("path/to/gallery_repo")` for updating
  galleries such as https://animint.github.io/gallery/
- Bugfix: geom_text renders color as svg fill style (was rendering as
  stroke style, a regression introduced by the initial implementation
  of `fill_off`).
- re-organization of animint.js in order to reduce duplication /
  emphasize similarities and differences between geoms.
- geom rect and tile now default to color="black" instead of
  transparent, for consistency with other geoms (and for the case of
  using clickSelects, which defaults to black color for selected, and
  transparent for not). To get the old behavior, specify
  color="transparent" (for non-clickSelects).
  
# Changes in 2023.10.6

- User-configurable selection style - fill_off.

# Changes in 2023.6.11

- Remove maptools dependency.
- split.x -> split_recursive to silence new CRAN check.
- setDTthreads(1) in examples for CRAN.

# Changes in 2023.3.14

- Changes to satisfy CRAN (import is, consistent function args, graphical-units.Rd, rm Fox book URL in stat_ellipse.Rd).
- facet and plot title text size configurable via theme(strip.text, axis.text).

# Changes in 2022.9.14

- Include Yufan Fei as contributor in DESCRIPTION.
- Allow configurable legend/axis text size via theme.

# Changes in 2022.8.31

- User-configurable selection styles, alpha_off and colour_off.

# Changes in 2022.8.25

- import data.table, for faster compilation.

# Changes in 2022.5.25

- Add ability to rotate geom_text labels, following ggplot2's semantics of rotation direction. 

# Changes in 2022.5.24

- Fixed small test regression and set up Github Actions. Neither of these is user facing.

# Changes in 2022.2.2

- Default x/y axis text size increased to 16px from 11px in animint.js.
- Suggest RColorBrewer to avoid CRAN NOTE "Undeclared package RColorBrewer in Rd xrefs".

# Changes in 2022.1.25

- Remove geom/stat_boxplot/quantile due to errors from CRAN check.

# Changes in 2020.9.18

- update moved mcmaster URL in docs.

# Changes in 2020.8.19

- Move geom-specific code from saveLayer/if-else to Geom$export_animint and pre_process methods in Geom sub-classes.

# Changes in 2020.7.9

- Using RSelenium 1.7.4.
- Removed Defunct and Redundant code/examples.
- User can test using docker to avoid incompatibility issues.
- imports knitr as knit_print imported in namespace.
- Use new grid::unitType() to determine unit type of margin in pt/points to line conversion.

# Changes in 2020.3.2

- data.frame(stringsAsFactors=TRUE) in tests.

# Changes in 2019.7.12

- Added three authors which are mentioned in Rd/R files.

# Changes in 2019.7.3

- ggsave(file.path(tempdir(), f)) in examples.

# Changes in 2019.7.2

- animint2dir writes to temp dir rather than user dir during examples/tests.

# Changes in 2019.6.4

- bugfix for facet_grid(scales/space="free")
- bugfix for update_axes with geom_abline
- bugfix for graph height computation, which only showed up when there were lots of rows in the facet spec.

# Changes in 2018.12.14

- animint fun, export print method.
- infinite values converted to range min/max before saving tsv files.

# Changes in 2018.10.4

- DEP: no longer depend on any branch of ggplot2.
- train_layout from ggplot2 - R/panel.r function is now g_train_layout. train_layout from animint2 - R/z_facet.R function is the same. Both are internal functions.

# Changes in 2017.08.24

- DSL: clickSelects/showSelected are now specified as parameters rather than aesthetics.
