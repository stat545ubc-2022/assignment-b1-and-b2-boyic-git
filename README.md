
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groupInto

<!-- badges: start -->
<!-- badges: end -->

The goal of `groupInto` is to automatically create a group of
categorical values by dividing the numerical values into equal parts.
User can easily create such column by giving source column and desired
group names.

## Installation

You can install the development version of groupInto from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2022/assignment-b1-and-b2-boyic-git", ref="1.0.0")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(groupInto)
## basic example code
sample <- data.frame("value"=1:10)
(grouped_sample <- group_into(sample, "value", "group", c("Small", "Medium", "Large")))
#>    value  group
#> 1      1  Small
#> 2      2  Small
#> 3      3  Small
#> 4      4  Small
#> 5      5 Medium
#> 6      6 Medium
#> 7      7 Medium
#> 8      8  Large
#> 9      9  Large
#> 10    10  Large
```
