
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visged <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/visged/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/visged/actions)
[![](https://codecov.io/gh/jl5000/visged/branch/master/graph/badge.svg)](https://codecov.io/gh/jl5000/visged)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/visged/badge)](https://www.codefactor.io/repository/github/jl5000/visged)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Produce a variety of visualisations for family tree GEDCOM files.

The package is part of the `gedcompendium` ecosystem of packages. This
ecosystem enables the handling of `tidyged` objects (tibble
representations of GEDCOM files), and the main package of this ecosystem
is [`tidyged`](https://jl5000.github.io/tidyged/).

<img src="man/figures/allhex.png" width="65%" style="display: block; margin: auto;" />

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jl5000/visged")
```

## Example

``` r
library(tidyged)
library(visged)
#> Loading required package: tidyged.io
#> When importing existing GEDCOM files, you should ensure that they are error free.
#> This package assumes imported GEDCOM files are valid and very few validation checks are carried out.
#> Several GEDCOM validators are available, including an online validator at http://ged-inline.elasticbeanstalk.com/

sw <- gedcom() %>%
  add_indi(sex = "M", indi_notes = "The central character in the Star Wars Skywalker Saga") %>%
  add_indi_names(given = "Anakin", surname = "Skywalker", type = "birth") %>%
  add_indi_names(prefix = "Darth", given = "Vader", type = "given") %>%
  add_indi(sex = "F", indi_notes = "Queen of Naboo") %>%
  add_indi_names(given = "Padme", surname = "Amidala", type = "birth") %>% 
  add_indi(sex = "F") %>% 
  add_indi_names(given = "Leia", surname = "Skywalker", type = "birth") %>%
  add_indi_names(prefix = "Princess", given = "Leia", surname = "Organa", type = "adoptive") %>% 
  add_indi(sex = "M") %>%
  add_indi_names(given = "Luke", surname = "Skywalker", type = "birth") %>% 
  add_indi(sex = "M") %>% 
  add_indi_names(given = "Obi-Wan", nickname = "Ben", surname = "Kenobi", type = "birth") %>% 
  add_famg(husband = "Anakin", wife = "Padme", children = c("Luke", "Leia")) %>%
  activate_indi("Anakin") %>% 
  add_indi_event_death(age_at_event = "45y", event_cause = "Killed by son Luke",
                       place_name = "Second Death Star", place_notes = "Orbiting Endor System")
```

``` r
pedigree_chart(sw, "Luke")
```

<img src="man/figures/luke_pedigree.png" width="50%" style="display: block; margin: auto;" />

``` r
descendancy_chart(sw, "Anakin")
```

<img src="man/figures/anakin_descendancy.png" width="50%" style="display: block; margin: auto;" />
