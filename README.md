
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edgar

<!-- badges: start -->
<!-- badges: end -->

This site is the home, a personal package, and a vehicle for graphs made
with R. It is the home because the site summarizes all kind of
visualizations that I made for my work. However, it is also an R package
and it installs the source code of many illustrations and the package
provides functions to make graphs with `ggplot2` (e.g., slope plots)

## Installation

You can install Graphs from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgar-treischl/edgar")
```

## Illustrations

The package installs the source code for all illustrations from this
website. The `plotgraph()` function just picks the installed source code
and returns the graphs. For example, the `datasaurus` plot.

``` r
## basic example code
library(edgar)
plotgraph("datasaurus.R")
```

<img src="vignettes/web_only/illustrations/saurus.png" alt="Data saurus by Edgar Treischl" width="100%" />

Inspect the website to for a specific graph. Without input, the
`plotgraph()` function returns all available graphs.

``` r
plotgraph()
#> Error in plotgraph(): Please run `plotgraph()` with a valid argument.
#> Valid examples are:
#> anscombe_quartet.R
#> boxplot_illustration.R
#> boxplot_pitfalls.R
#> data_joins.R
#> datasaurus.R
#> gapminder.R
#> long_wide.R
#> pacman.R
#> simpson.R
#> ucb_admission.R
```

## Graphs

And the package gives access to shortcut functions to make `ggplot2`
graphs. For example, `ggslope()` returns a slope chart.

``` r
library(tidyr)
#> Warning: package 'tidyr' was built under R version 4.1.2
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.1.2
df <- tribble(
 ~times, ~country,  ~gdp, ~inc,
 "1990",   "A",  22.3, TRUE,
 "2000",   "A",  44.6, TRUE,
 "1990",   "B",  12.3, FALSE,
 "2000",   "B",  4.6, FALSE
 )

ggslope(df,
         times = times,
         outcome = gdp,
         group = country)+
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
