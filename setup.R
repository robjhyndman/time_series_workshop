# Set up chunk for all slides
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = TRUE,
  dev.args = list(pointsize = 11)
)
options(
  digits = 3,
  width = 85,
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)
library(fpp3)
library(patchwork)
library(glue)
library(stringr)

global_economy <- global_economy |>
  select(Year, Country, GDP, Imports, Exports, Population)
tourism <- tourism |>
  mutate(
    State = recode(State,
                   "Australian Capital Territory" = "ACT",
                   "New South Wales" = "NSW",
                   "Northern Territory" = "NT",
                   "Queensland" = "QLD",
                   "South Australia" = "SA",
                   "Tasmania" = "TAS",
                   "Victoria" = "VIC",
                   "Western Australia" = "WA"
    ),
    Region = str_replace(Region, "Australia's ", "")
  )
