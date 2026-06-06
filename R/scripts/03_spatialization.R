
# load data
stations <- sf::st_read("data/stations.gpkg")
roads <- sf::st_read("data/roads.gpkg")
pop_dens <- sf::st_read("data/pop_dens.gpkg")
krs_tub <- sf::st_read("data/krs_tueb.gpkg")

# reduce to tub and apply same crs
chargers_tueb <- sf::st_intersection(stations,krs_tub)
stations_tub_ll <- sf::st_transform(chargers_tueb, 4326)
krs_tueb <- sf::st_transform(krs_tub, 4326)
pop_tub <- sf::st_intersection(
  pop_dens,
  sf::st_transform(krs_tueb, 25832)
)
pop_tub <- sf::st_transform(pop_tub, 4326)
# color palette for pop circles
pal <- leaflet::colorNumeric(
  "YlOrRd",
  pop_tub$Einwohner
)
# visualize
leaflet::leaflet() |>
  leaflet::addProviderTiles("CartoDB.Positron") |>
  leaflet::addPolygons(
    data = krs_tueb,
    color = "black",
    weight = 2,
    fillOpacity = 0
  ) |>

  leaflet::addCircleMarkers(
    data = pop_tub,
    radius = ~pmax(3, sqrt(Einwohner) / 5), # sizing circles
    fillColor = ~pal(Einwohner),
    fillOpacity = 0.5,
    stroke = FALSE,
    group = "Population"
  ) |>

  leaflet::addCircleMarkers(
    data = stations_tub_ll,
    radius = 3,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.6
  )



