Exploratory time series analysis using R
================

### Wombat workshop: 2022

by [Rob J Hyndman](https://robjhyndman.com)

:spiral_calendar: 6 December 2022<br>
:alarm_clock:     13:00 - 16:00<br>

-----

## Lab Sessions

### Lab Session 1

 1. Download [`tourism.xlsx`](http://robjhyndman.com/data/tourism.xlsx) from [`http://robjhyndman.com/data/tourism.xlsx`](http://robjhyndman.com/data/tourism.xlsx), and read it into R using `read_excel()` from the `readxl` package.
 2. Create a tsibble which is identical to the `tourism` tsibble from the `tsibble` package.
 3. Find what combination of `Region` and `Purpose` had the maximum number of overnight trips on average.
 4. Create a new tsibble which combines the Purposes and Regions, and just has total trips by State.

### Lab Session 2

Look at the quarterly tourism data for the Snowy Mountains

```r
snowy <- tourism |> filter(Region == "Snowy Mountains")
```

- Use `autoplot()`, `gg_season()` and `gg_subseries()` to explore the data.
- What do you learn?

### Lab Session 3


1. Produce an STL decomposition of the Snowy Mountains data.
2. Experiment with different values of the two `window` arguments.
3. Plot the seasonally adjusted series.

### Lab Session 4


* Find the most seasonal time series in the tourism data.
* Which state has the strongest trends?
* Use a feature-based approach to look for outlying series in `tourism`.
* What is unusual about the series you identify as outliers?
