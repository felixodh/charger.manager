## code to prepare `package_data` dataset goes here



# api ---------------------------------------------------------------------

# iterate through data packages 1 - 80k
station_url <- "https://api.mobidata-bw.de/ocpdb/api/ocpi/3.0/locations?offset=1&limit=40000"

# Create request
station_request <- httr2::request(station_url) |>
  httr2::req_user_agent("Mozilla/5.0") |>
  httr2::req_headers(Accept = "application/json")

# Perform request and get response
station_response <- httr2::req_perform(station_request)
station_json <- httr2::resp_body_json(station_response)


# contemplate data --------------------------------------------------------

# data union

stations <- list(stations = c(part1,part2,part3,part4))

saveRDS(stations,"stations.rds")

### meta dats stations location ###

# create tibble from meta data locations
stations <- readRDS("data-raw/stations.rds")

# reduce list levels
stations_flt <- purrr::list_flatten(stations)

# apply trans_fun_dt to each list element to get a df of metas
result <- data.table::rbindlist(
  lapply(stations_flt, trans_fun_dt),
  fill = TRUE
)

# create tibble and define variable types where necessary
results_tbl <- dplyr::tibble(result) |>
  dplyr::mutate(postal_code = as.integer(postal_code),
                last_updated = lubridate::ydm_hms(last_updated,tz = "Europe/Berlin"),
                latitude = as.double(latitude),
                longitude = as.double(longitude),
                id = as.integer(id),
                original_id = as.integer(original_id)
  )

stations_meta_loc <- results_tbl
usethis::use_data(stations_meta_loc)
### charger specs ###

# apply trans_fun_chargpool to each list element to get a df of metas
chargpool <- data.table::rbindlist(
  lapply(stations_flt, trans_fun_chargpool),
  fill = TRUE
)

chargepool <- dplyr::tibble(chargpool) |>
  dplyr::mutate(
    station_id = as.integer(station_id),
    original_station_id = as.integer(original_station_id),
    id = as.integer(id),
    original_uid = as.integer(original_uid),
    evse_uid = as.integer(evse_uid),
    connector_max_volt = as.integer(connector_max_volt),
    connector_max_amp = as.integer(connector_max_amp),
    connector_electric_power = as.integer(connector_electric_power),
    connector_id = as.integer(connector_id),
    connector_original_id = as.integer(connector_original_id)
  )

usethis::use_data(chargepool)


# spatialize data ---------------------------------------------------------

# convert stationsmeta to simple feature
stations_sf <- sf::st_as_sf(
  stations_meta_loc,
  coords = c("longitude","latitude"),
  crs = 4326
)
# convert from wgs 84 to UTM zone 32N
stations_sf <- sf::st_transform(
  stations_sf,crs = 25832
)

sf::st_write(
  stations_sf,
  "data/stations.gpkg"
)






usethis::use_data(package_data, overwrite = TRUE)
