Assignment B2
================

## Setup

``` r
library(datateachr) 
library(tidyverse)
```

## Exercise 1 & 2

In this assignment, I want to create a function that create a new
categorical variable with `n` categories using a numerical variable by
dividing the range of that variable into `n` equal parts. The original
code is as follows:

``` r
sample_area_range <- max(cancer_sample$area_mean) - min(cancer_sample$area_mean)
(new_cancer_sample <- cancer_sample %>% 
   mutate(size = case_when(area_mean < sample_area_range / 3 ~ "Small",
                           area_mean < sample_area_range*2/3 ~ "Mid",
                           area_mean >= sample_area_range*2/3 ~ "Large"),
          .after = diagnosis))
```

    ## # A tibble: 569 × 33
    ##         ID diagn…¹ size  radiu…² textu…³ perim…⁴ area_…⁵ smoot…⁶ compa…⁷ conca…⁸
    ##      <dbl> <chr>   <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1  8.42e5 M       Mid      18.0    10.4   123.    1001   0.118   0.278   0.300 
    ##  2  8.43e5 M       Mid      20.6    17.8   133.    1326   0.0847  0.0786  0.0869
    ##  3  8.43e7 M       Mid      19.7    21.2   130     1203   0.110   0.160   0.197 
    ##  4  8.43e7 M       Small    11.4    20.4    77.6    386.  0.142   0.284   0.241 
    ##  5  8.44e7 M       Mid      20.3    14.3   135.    1297   0.100   0.133   0.198 
    ##  6  8.44e5 M       Small    12.4    15.7    82.6    477.  0.128   0.17    0.158 
    ##  7  8.44e5 M       Mid      18.2    20.0   120.    1040   0.0946  0.109   0.113 
    ##  8  8.45e7 M       Small    13.7    20.8    90.2    578.  0.119   0.164   0.0937
    ##  9  8.45e5 M       Small    13      21.8    87.5    520.  0.127   0.193   0.186 
    ## 10  8.45e7 M       Small    12.5    24.0    84.0    476.  0.119   0.240   0.227 
    ## # … with 559 more rows, 23 more variables: concave_points_mean <dbl>,
    ## #   symmetry_mean <dbl>, fractal_dimension_mean <dbl>, radius_se <dbl>,
    ## #   texture_se <dbl>, perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>,
    ## #   smoothness_worst <dbl>, compactness_worst <dbl>, concavity_worst <dbl>, …

The function should take a table, a variable name of the column that is
to be grouped, a variable name to be used for the new column and a
vector of category names. It should output a new table with a new column
with a name is requested in the input. I call this function
`group_into`.

``` r
#` Adding a new categorical column into the table 
#`
#` This function will create a new table that includes the original table and a 
#` new column which is a categorical variable with values depending on the values 
#` requested by the user.
#`
#` @param table original table need to be modified (it makes sense to name the 
#` variable simple like a single word "table")
#` @param source_name name of the column to be categorized (the name is intuitive)
#` @param new_name name of the new column which has categorized values (the name 
#` is intuitive)
#` @param categories a vector of categories, it needs to be in ascending order
#`(because the element in the vector is a category so I take the plural form)
#` 
#` @return new_table the table with a new categorical column
group_into <- function(table, source_name, new_name, categories) {
  # check if source_name in the table
  stopifnot(source_name %in% colnames(table))
  
  # check length of categories is greater than 0
  stopifnot(length(categories) > 0)
  
  # group the samples depending on the value of source_name 
  new_table <- table
  new_table[, new_name] <- cut(table[[source_name]], breaks = length(categories), labels = categories)
  
  return(new_table)
}
```

## Exercise 3 - Examples

Working examples of group using the `cancer_sample` dataset

``` r
# working example using the `cancer_sample` dataset
(aream_cancer_sample_using_function <- group_into(cancer_sample, "area_mean", 
                                               "area", 
                                               c("Small", "Medium", "Large")))
```

    ## # A tibble: 569 × 33
    ##          ID diagnosis radius_m…¹ textu…² perim…³ area_…⁴ smoot…⁵ compa…⁶ conca…⁷
    ##       <dbl> <chr>          <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1   842302 M               18.0    10.4   123.    1001   0.118   0.278   0.300 
    ##  2   842517 M               20.6    17.8   133.    1326   0.0847  0.0786  0.0869
    ##  3 84300903 M               19.7    21.2   130     1203   0.110   0.160   0.197 
    ##  4 84348301 M               11.4    20.4    77.6    386.  0.142   0.284   0.241 
    ##  5 84358402 M               20.3    14.3   135.    1297   0.100   0.133   0.198 
    ##  6   843786 M               12.4    15.7    82.6    477.  0.128   0.17    0.158 
    ##  7   844359 M               18.2    20.0   120.    1040   0.0946  0.109   0.113 
    ##  8 84458202 M               13.7    20.8    90.2    578.  0.119   0.164   0.0937
    ##  9   844981 M               13      21.8    87.5    520.  0.127   0.193   0.186 
    ## 10 84501001 M               12.5    24.0    84.0    476.  0.119   0.240   0.227 
    ## # … with 559 more rows, 24 more variables: concave_points_mean <dbl>,
    ## #   symmetry_mean <dbl>, fractal_dimension_mean <dbl>, radius_se <dbl>,
    ## #   texture_se <dbl>, perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>,
    ## #   smoothness_worst <dbl>, compactness_worst <dbl>, concavity_worst <dbl>, …

``` r
(compactness_cancer_sample_using_function <- group_into(cancer_sample, "compactness_mean", 
                                               "compactness", 
                                               c("non-compact", "compact")))
```

    ## # A tibble: 569 × 33
    ##          ID diagnosis radius_m…¹ textu…² perim…³ area_…⁴ smoot…⁵ compa…⁶ conca…⁷
    ##       <dbl> <chr>          <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1   842302 M               18.0    10.4   123.    1001   0.118   0.278   0.300 
    ##  2   842517 M               20.6    17.8   133.    1326   0.0847  0.0786  0.0869
    ##  3 84300903 M               19.7    21.2   130     1203   0.110   0.160   0.197 
    ##  4 84348301 M               11.4    20.4    77.6    386.  0.142   0.284   0.241 
    ##  5 84358402 M               20.3    14.3   135.    1297   0.100   0.133   0.198 
    ##  6   843786 M               12.4    15.7    82.6    477.  0.128   0.17    0.158 
    ##  7   844359 M               18.2    20.0   120.    1040   0.0946  0.109   0.113 
    ##  8 84458202 M               13.7    20.8    90.2    578.  0.119   0.164   0.0937
    ##  9   844981 M               13      21.8    87.5    520.  0.127   0.193   0.186 
    ## 10 84501001 M               12.5    24.0    84.0    476.  0.119   0.240   0.227 
    ## # … with 559 more rows, 24 more variables: concave_points_mean <dbl>,
    ## #   symmetry_mean <dbl>, fractal_dimension_mean <dbl>, radius_se <dbl>,
    ## #   texture_se <dbl>, perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>,
    ## #   smoothness_worst <dbl>, compactness_worst <dbl>, concavity_worst <dbl>, …

A failed example because I do not provide correct source column name

``` r
group_into(cancer_sample, "area_mean_not_existing", "area", c("Small", "Medium", "Large"))
```

    ## Error in group_into(cancer_sample, "area_mean_not_existing", "area", c("Small", : source_name %in% colnames(table) is not TRUE

A failed example because I provide a empty catogory

``` r
group_into(cancer_sample, "area_mean", "area", c())
```

    ## Error in group_into(cancer_sample, "area_mean", "area", c()): length(categories) > 0 is not TRUE

## Exercise 4

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
test1 <- group_into(cancer_sample, "area_mean", "area", c("Small", "Medium", "Large"))


test_that("Exercise 4 test", {
          expect_equal(as.character(test1$area[1]), "Medium")
          expect_error(group_into(cancer_sample, "area_mean_not_existing", "area", c("Small", "Medium", "Large")))
          expect_error(group_into(cancer_sample, "area_mean", "area", c())) # empty vector
          })
```

    ## Test passed 😸
