# Changes in version 2024.7.5 (PR#131)
  
  - Added functionality to capture screenshot in animint2page

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
