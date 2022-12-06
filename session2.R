library(fpp3)

# Maximum temperatures

maxtemp <- vic_elec |>
  index_by(Day = date(Time)) |>
  summarise(Temperature = max(Temperature))
maxtemp |>
  autoplot(Temperature) +
  labs(x = "Week", y = "Max temperature")

# Ansett

ansett |>
  autoplot(Passengers)
ansett |>
  filter(Class == "Economy") |>
  autoplot(Passengers)
ansett |>
  filter(Airports == "MEL-SYD") |>
  autoplot(Passengers)

## Beer production

beer <- aus_production |>
  select(Quarter, Beer) |>
  filter(year(Quarter) >= 1992)
beer |> autoplot(Beer)
beer |> gg_season(Beer, labels = "right")
beer |> gg_subseries(Beer)

## Victorian electricity demand

vic_elec |> gg_season(Demand)
vic_elec |> gg_season(Demand, period = "week")
vic_elec |> gg_season(Demand, period = "day")

## US Retail employment

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
us_retail_employment |>
  autoplot(Employed) +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")
dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
components(dcmp) |> autoplot()
us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "#D55E00") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")
us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), season_adjust, color = "#0072B2") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")
