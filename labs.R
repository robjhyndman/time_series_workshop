library(fpp3)

# Lab Session 1

download.file("http://robjhyndman.com/data/tourism.xlsx", tourism_file <- tempfile())
my_tourism <- readxl::read_excel(tourism_file) |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(
    index = Quarter,
    key = c(Region, State, Purpose)
  )

my_tourism |>
  as_tibble() |>
  group_by(Region, Purpose) |>
  summarise(Trips = mean(Trips)) |>
  ungroup() |>
  filter(Trips == max(Trips))

state_tourism <- my_tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips)) |>
  ungroup()

# Lab Session 2

snowy <- tourism |>
  filter(Region == "Snowy Mountains") |>
  select(-State, -Region)
snowy |> autoplot(Trips)
snowy |> gg_season(Trips)
snowy |> gg_subseries(Trips)

# Lab Session 3

snowy |>
  model(STL(Trips ~ season(window = 7) + trend(window = 11))) |>
  components() |>
  autoplot()

## Changing the size of the windows changes the trend and seasonal components
## A smaller window gives a more flexible (fast changing) component
## A longer window gives a smoother (slow changing) component

snowy |>
  model(STL(Trips ~ season(window = 7) + trend(window = 11))) |>
  components() |>
  autoplot(season_adjust)

# Lab Session 4

library(broom)

## Compute features
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts")) |>
  na.omit()

## Compute principal components
tourism_prcomp <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)

## Plot the first two components
tourism_prcomp |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col=Purpose)) +
  geom_point()

## Pull out most unusual series from first principal component
outliers <- tourism_prcomp |>
  filter(.fittedPC1 > 10) |>
  arrange(desc(.fittedPC2))
outliers

## Visualise the unusual series
tourism |>
  semi_join(outliers, by = c("State", "Region", "Purpose")) |>
  autoplot(Trips) +
  facet_grid(vars(State, Region, Purpose)) +
  labs(title = "Outlying time series in PC space")
