
<!-- README.md is generated from README.Rmd. Please edit that file -->

# srilankamotif

<!-- badges: start -->

<!-- badges: end -->

This package is developed to generate Sri Lankan-inspired artistic
motifs using R through mathematical and geometric constructions. Beyond
its visual outputs, the package serves an educational purpose by
demonstrating how mathematics can be translated into art through coding,
helping users strengthen their programming skills, particularly in data
visualization and functional programming in R, while engaging in
creative exploration.

## Installation

You can install the development version of srilankamotif from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("thiyangt/srilankamotif")
```

``` r
library(srilankamotif)
library(ggplot2)
```

## Example

## Inspired by Lotus

``` r
library(srilankamotif)
## basic example code
generate_flower()
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

## Inspired by Binara (බිනර) Mala

``` r
draw_dual_2flowers()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

``` r
generate_binara_3flower()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" alt="" width="100%" />

# Inspired by Arimbuwa

``` r
generate_arimbuwa()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="" width="100%" />

## Inspired by Gal Binduwa (ගල් බින්දු)

``` r
generate_galbinduwa()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="" width="100%" />

## References

[Traditional Sinhala
Degins](https://upload.wikimedia.org/wikipedia/commons/e/e9/%E0%B6%B4%E0%B7%90%E0%B6%BB%E0%B6%AB%E0%B7%92_%E0%B7%83%E0%B7%92%E0%B6%82%E0%B7%84%E0%B6%BD_%E0%B6%B8%E0%B7%9D%E0%B7%83%E0%B7%8A%E0%B6%AD%E0%B6%BB_old_sinhala_designs.jpg)
