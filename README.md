# Animint2


## About

Animint2 is an R package for generating and sharing interactive data visualizations, sometimes referred to as animints. It is a fork of, and uses similar syntax to, [ggplot2](https://ggplot2.tidyverse.org/). Animint2 is especially useful for large datasets, but smaller datasets can be made interactive, too. It is also capable of generating static data visualizations.

Play around with this interactive data visualization of [data from the World Bank](https://rcdata.nau.edu/genomic-ml/WorldBank-facets/). For more examples, go to the [animint gallery](https://rcdata.nau.edu/genomic-ml/animint-gallery/).

To learn how to generate your own interactive data visualizations, go to the official [Animint2 Manual](https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch00-preface.html). If you encounter problems, please see the [animint2 wiki](https://github.com/animint/animint2/wiki) or [report them](https://github.com/animint/animint2/issues).

![An interactive data visualization. It uses fertility data from the World Bank. The user uses the selection menu and clicks on the legend, which causes changes in a couple of line graphs.](world_bank_screencast.gif)


## Installation

``` r
# Install the official package from CRAN.
# This is the option most people should choose:
install.packages("animint2")

# If you want to install the development version:
devtools::install_github("animint/animint2")
```


## Use

Animint2 uses the same implementation of `ggplot2`’s grammar of graphics—with a few additions. If you’re familiar with `ggplot2`, using `animint2` will be easy. If you’re not, no worries. To get started, see the brief [Animint2 Quick Start Guide](vignettes/starter-kit.html) or read the first few chapters of the [Animint2 Manual](https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch00-preface.html).

`animint2` renders and animates data visualizations. It can neither manipulate the datasets you give it nor generate its own data.


## Similar Packages

`animint2` isn’t the only R package that can create animated or interactive data visualizations.

[animation](https://cran.r-project.org/package=animation) and [gganimate](https://cloud.r-project.org/web/packages/gganimate/index.html) can animate changes between variables over time. The [loon](https://cran.r-project.org/package=loon) package specializes in exploratory data analysis. [plotly](https://cran.r-project.org/package=plotly) is probably most similar to animint2 in terms of functionality.

For comparisons between the aforementioned packages and `animint2`, see [the differences wiki page](https://github.com/animint/animint2/wiki/Differences-with-other-packages).


## Problems?

The `animint2` package is a work in progress. If you spot any bugs or unexpected behaviors, please let us know by [reporting it as an issue on GitHub](https://github.com/animint/animint2/issues). Thanks! Have a great day.
