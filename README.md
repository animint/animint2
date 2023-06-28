animint2 provides an animated, interactive grammar of graphics

[![](https://github.com/tdhock/animint2/actions/workflows/tests.yaml/badge.svg)](https://github.com/tdhock/animint2/actions/workflows/tests.yaml)

## Animint2: animated, interactive grammar of graphics

Animint2 is an advanced data visualization system that makes it easy for
people who work with big data to create multi-layer, multi-plot,
multi-panel (facetted), interactive, and possibly animated graphics, in
order to understand their big data sets. Animint2 is fork of
[ggplot2](https://github.com/hadley/ggplot2) that adds the clickSelects
and showSelected interactive keywords to the grammar of graphics, and
renders to the web using [D3](http://d3js.org/). For example, this
multi-layer facetted interactive animation of WorldBank data was created
using just [*10 geoms and \~60 lines of R code*]{.spurious-link
target="inst/examples/WorldBank-facets.R"}.

[![](https://raw.githubusercontent.com/tdhock/animint/master/screencast-WorldBank.gif)](https://rcdata.nau.edu/genomic-ml/WorldBank-facets/)

More examples of advanced data visualizations that are possible/easy to
create with animint can be found in the
[animint-gallery](https://rcdata.nau.edu/genomic-ml/animint-gallery/).

[![](https://user-images.githubusercontent.com/932850/228421946-2c68bbc8-b11c-4312-a6d0-85a960acd0c1.png)](https://rcdata.nau.edu/genomic-ml/animint-gallery/)

## Installation

``` {.r org-language="R"}
install.packages("animint2")#from CRAN (stable).
##OR from GitHub (development).
if(!require("remotes"))install.packages("remotes")
remotes:install_github("tdhock/animint2")
```

## FAQ

If you have problems rendering animints (such as a blank web page) then
please see our [FAQ wiki
page](https://github.com/tdhock/animint2/wiki/FAQ#web-browser-on-local-indexhtml-file-is-blank).

## Usage

The best reference for learning is the [Animint2 Designer
Manual](https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch02-ggplot2.html),
which provides several examples and exercises to get you started using
animint2.

## Differences with other packages

[Our JCGS
paper](https://amstat.tandfonline.com/doi/abs/10.1080/10618600.2018.1513367?journalCode=ucgs20)
provides a detailed comparison (an executive summary is below).

### R packages for animated graphics

R packages [gganim](https://github.com/tdhock/gganim),
[animation](https://cloud.r-project.org/web/packages/animation/) and
[gganimate](https://github.com/thomasp85/gganimate) can generate
animated graphics, in which the only interaction is going forward or
backward in time. In contrast animint2 provides animation and also
interaction with other variables (not only time).

### shiny+plotly web applications

Typical shiny web applications provide interactive graphics with
indirect manipulation (via menus/buttons/etc) but the emphasis in
animint2 is direct manipulation (via mouse clicks on lines/points/etc).
Direct manipulation in shiny apps can also be achieved using
[plotly](https://plotly-r.com/client-side-linking.html) or
[dash](https://dash.plotly.com/r/interactive-graphing), in which
interactivity can be specified using events; in contrast animint2 uses
the clickSelects/showSelected keywords to specify interactivity, and
makes it easy to design multi-layer graphics (which are more difficult
in plotly/dash, for which the emphasis is on pre-defined chart types).

### loon exploratory graphics

[loon](http://great-northern-diver.github.io/loon/) is an excellent R
package for interactive exploratory graphics; the focus of animint2 is
interactive presentation graphics.

### Old animint

animint2 is a redesign of [animint](https://github.com/tdhock/animint)
with:

-   Cleaner syntax. In the old animint we had showSelected/clickSelects
    as aesthetics, and in animint2 they are now geom parameters.
-   Easier installation. The old animint depended on ggplot2 but
    animint2 does not (it has copied the necessary functions from
    ggplot2).

For a concrete example of how the syntax changed, consider the following
example
[dataviz](https://rcdata.nau.edu/genomic-ml/animint-gallery/2015-06-10-Data-viz-with-206-selectors/),
adapted from
[file:tests/testthat/test-renderer1-variable-value.R](tests/testthat/test-renderer1-variable-value.R)
and explained in [Chapter 14 of the Animint2
Manual](https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch14-PeakSegJoint.html).
The data set used to draw the blue line segments in the bottom plot
looks like this:

``` {.r org-language="R"}
> with(peak.problems, data.frame(selector.name=paste0(problem.name, "peaks"), problem.name, peaks, bases.per.problem))
             selector.name       problem.name peaks bases.per.problem
1  size.100.problem.1peaks size.100.problem.1     1               100
2  size.100.problem.2peaks size.100.problem.2     1               100
3   size.50.problem.1peaks  size.50.problem.1     1                50
4   size.50.problem.2peaks  size.50.problem.2     1                50
5   size.50.problem.3peaks  size.50.problem.3     1                50
6   size.50.problem.4peaks  size.50.problem.4     1                50
7  size.100.problem.1peaks size.100.problem.1     2               100
8  size.100.problem.2peaks size.100.problem.2     2               100
9   size.50.problem.1peaks  size.50.problem.1     2                50
10  size.50.problem.2peaks  size.50.problem.2     2                50
11  size.50.problem.3peaks  size.50.problem.3     2                50
12  size.50.problem.4peaks  size.50.problem.4     2                50
> 
```

and the old animint code looks like this:

``` {.r org-language="R"}
geom_segment(aes(
  showSelected.variable=selector.name,
  showSelected.value=peaks,
  clickSelects=problem.name,
  showSelected2=bases.per.problem),
  data=peaks.dt)
```

In both animint and animint2, there are \"selectors\" which are
variables in the interactive graphic that can change based on what you
click on. In the old animint, selectors were specified using aesthetics:

-   The `aes(clickSelects)`{.verbatim} means that whenever you click on
    one of these segments, the `problem.name`{.verbatim} selector will
    change. For example clicking the segment that is plotted for the
    first row of data will change `problem.name`{.verbatim} to
    `size.100.problem.1`{.verbatim}.
-   The `aes(showSelected2)`{.verbatim} means that the only segments
    that will be shown are the ones which correspond to the current
    value of the `bases.per.problem`{.verbatim} selector. For example
    the segment for the first row of data will only be shown if
    `100`{.verbatim} is selected for the `bases.per.problem`{.verbatim}
    selector.
-   The `showSelected.variable`{.verbatim} and
    `showSelected.value`{.verbatim} mean to show the segment only if the
    value of `showSelected.value`{.verbatim} is the current selection of
    the `showSelected.variable`{.verbatim} selector. For example the
    segment for the first row of data will only be shown if
    `1`{.verbatim} is selected for the
    `size.100.problem.1peaks`{.verbatim} selector.

The new animint2 syntax uses parameters instead of aesthetics, so is
much more concise:

``` {.r org-language="R"}
geom_segment(
 showSelected=c(selector.name="peaks", "bases.per.problem"),
 clickSelects="problem.name")
```

Both `showSelected`{.verbatim} and `clickSelects`{.verbatim} should be
character vectors. Named elements of the character vector are
interpreted as the old variable/value aes, and un-named elements are
interpreted as the old clickSelects/showSelected aes.
