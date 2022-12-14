---
author:
- Rob J Hyndman
title: Exploratory time series analysis using R
venue: WOMBAT 2022
---

## Date

1pm -- 4pm, 6 December 2022

## Course description

Many organisations collect huge amounts of data over time, and we need
time series analysis tools capable of handling the scale, frequency and
structure of the data collected. In this workshop, we will look at some
R packages and methods that have been developed to handle the analysis
of large collections of time series. We will look at the tsibble data
structure for flexibly managing collections of related time series, and
consider how to do data wrangling, data visualisation, and exploratory
data analysis to analyse time series data in high dimensions.

-   **Session 1**: How to wrangle time series data with familiar tidy
    tools.
-   **Session 2**: How to visualize the trend and seasonal patterns in
    individual time series.
-   **Session 3**: How to compute time series features and visualize
    large collections of time series.

## Prework

Attendees are expected to be familiar with R, and with the tidyverse
collection of packages including dplyr and ggplot2. They will need to
have R and RStudio installed on their own device, and have installed the
fpp3 package.

People who don't use R regularly, or don't know the tidyverse packages,
are recommended to do the tutorials at
[learnr.numbat.space](http://learnr.numbat.space) beforehand.

Please ensure your computer has a recent version of R and RStudio
installed. The following code will install the main packages needed for
the workshop.

``` r
install.packages(c("tidyverse","fpp3","GGally"))
```

## Slides

-   [Session
    1](https://github.com/robjhyndman/time_series_workshop/raw/main/session1.pdf)
-   [Session
    2](https://github.com/robjhyndman/time_series_workshop/raw/main/session2.pdf)
-   [Session
    3](https://github.com/robjhyndman/time_series_workshop/raw/main/session3.pdf)

## [Labs](https://github.com/robjhyndman/time_series_workshop/blob/main/Labs.md)
