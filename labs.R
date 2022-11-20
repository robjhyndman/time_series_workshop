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
PBS_feat <- PBS |>
  features(Cost, feature_set(pkgs = "feasts")) |>
  na.omit()

## Compute principal components
PBS_prcomp <- PBS_feat |>
  select(-Concession, -Type, -ATC1, -ATC2) |>
  prcomp(scale = TRUE) |>
  augment(PBS_feat)

## Plot the first two components
PBS_prcomp |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point()

## Pull out most unusual series from first principal component
outliers <- PBS_prcomp |>
  filter(.fittedPC1 > 7)
outliers

## Visualise the unusual series
PBS |>
  semi_join(outliers, by = c("Concession", "Type", "ATC1", "ATC2")) |>
  autoplot(Cost) +
  facet_grid(vars(Concession, Type, ATC1, ATC2)) +
  labs(title = "Outlying time series in PC space")
