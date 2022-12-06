library(fpp3)

## Pharmaceutical Benefits Scheme

PBS |>
  features(Scripts, feat_stl)

PBS |> features(Scripts, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col=ATC1)) +
  geom_point() + facet_grid(Concession ~ Type)

most_seasonal <- PBS |>
  features(Scripts, feat_stl) |>
  arrange(desc(seasonal_strength_year)) |>
  head(1)

PBS |>
  right_join(most_seasonal, by = c("ATC1", "ATC2", "Concession", "Type")) |>
  ggplot(aes(x = Month, y = Scripts)) +
  geom_line() + facet_grid(vars(ATC2, Concession, Type))

most_trended <- PBS |>
  features(Scripts, feat_stl) |>
  arrange(desc(trend_strength)) |>
  head(1)

PBS |>
  right_join(most_trended, by = c("ATC1", "ATC2", "Concession", "Type")) |>
  ggplot(aes(x = Month, y = Scripts)) +
  geom_line() + facet_grid(vars(ATC2, Concession, Type))

# Compute features
PBS_features <- PBS |>
  features(Scripts, feature_set(pkgs = "feasts")) |>
  select(-`...26`) |>
  na.omit()

# Compute PCs
pcs <- PBS_features |>
  select(-ATC1, -ATC2, -Type, -Concession) |>
  prcomp(scale = TRUE) |>
  broom::augment(PBS_features)

pcs |> ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point() +
  theme(aspect.ratio = 1)
pcs |> ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Concession)) +
  geom_point() +
  theme(aspect.ratio = 1)
pcs |> ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Type)) +
  geom_point() +
  theme(aspect.ratio = 1)
pcs |> ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = ATC1)) +
  geom_point() +
  theme(aspect.ratio = 1)

outliers <- pcs |>
  filter(.fittedPC2 < -15)

pcs |> ggplot(aes(x = .fittedPC1, y = .fittedPC2, col=Type)) +
  geom_point() +
  theme(aspect.ratio = 1) +
  geom_point(data = outliers, aes(x = .fittedPC1, y = .fittedPC2), col = "black", shape = 1, size = 3)

semi_join(PBS, outliers, by = c("ATC1", "ATC2", "Concession", "Type")) |>
  mutate(Series = glue::glue("{ATC2}", "{Concession}", "{Type}", .sep = "\n\n")) |>
  ggplot(aes(x = Month, y = Scripts)) +
  geom_line() + facet_grid(Series ~ .) +
  labs(title = "Outlying time series in PC space")
