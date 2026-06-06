
# charger stations --------------------------------------------------------
# load meta data from api package wise
# iterate through data packages 1 - 80k: offsets have to be updated
station_url <- "https://api.mobidata-bw.de/ocpdb/api/ocpi/3.0/locations?offset=1&limit=8000"
# Create request
station_request <- httr2::request(station_url) |>
  httr2::req_user_agent("Mozilla/5.0") |>
  httr2::req_headers(Accept = "application/json")

# Perform request and get response
station_response <- httr2::req_perform(station_request)
station_json <- httr2::resp_body_json(station_response)

# save data parts (for all parts)
saveRDS(station_json, "data-raw/charger_1_8000.rds")

# unite data parts
part1 <- readRDS("data-raw/charger_1_8000.rds")
part2 <- readRDS("data-raw/charger_80001_16000.rds")
part3 <- readRDS("data-raw/charger_16001_24000.rds")
part4 <- readRDS("data-raw/charger_24001_32000.rds")
stations <- list(stations = c(part1,part2,part3,part4))

saveRDS(stations,"data-raw/stations.rds")

# unite data lists as downloaded
stations <- list(stations = c(part1,part2,part3,part4))
saveRDS(stations,"data-raw/stations.rds")

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
# save as package data
stations_meta_loc <- results_tbl
usethis::use_data(stations_meta_loc)

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

# write geo data
sf::st_write(
  stations_sf,
  "data/stations.gpkg"
)


# charger specifications --------------------------------------------------
# charger specs are voltage, power, operator, socket type etc.
# apply trans_fun_chargpool to each list element to get a df of metas
chargpool <- data.table::rbindlist(
  lapply(stations_flt, trans_fun_chargpool),
  fill = TRUE
)
# create tibble + define vartypes
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

# municipal borders -------------------------------------------------------
# subset city: downloaded shp file from https://gdz.bkg.bund.de
districts_krs <- sf::st_read("data/external/vg2500_12-31.utm32s.shape/vg2500/VG2500_KRS.shp")
krs_tueb <- districts_krs[districts_krs$GEN == "Tübingen",]
krs_tueb <- sf::st_transform(krs_tueb, 25832) # utm

sf::st_write(krs_tueb,"data/krs_tueb.gpkg")

# population --------------------------------------------------------------
# download population density from https://experience.arcgis.com/experience/2ce4e1cccb244421a281fa813c7523fc
# pop density is per inhabitants/km2
pop <- readr::read_csv2(
  "data/external/Georeferenzierte_BevDaten_2021/Georeferenzierte_BevDaten_2021.csv",
  locale = readr::locale(decimal_mark = ",")
)

# get coordinates
pop <- pop |>
  dplyr::mutate(
    x = as.numeric(stringr::str_extract(Gitter_ID_1km, "(?<=E)\\d+")),
    y = as.numeric(stringr::str_extract(Gitter_ID_1km, "(?<=N)\\d+"))
  )

# transform to sf
pop_sf <- sf::st_as_sf(
  pop,
  coords = c("x", "y"),
  crs = 3035
)
pop_sf <- sf::st_transform(pop_sf, 25832) # utm

sf::st_write(pop_sf,"data/pop_dens.gpkg")


# public infrastrusture ----------------------------------------------------------
# roads
roads <- osmdata::opq("Tübingen, Germany") |>
  osmdata::add_osm_feature(key = "highway") |>
  osmdata::osmdata_sf()

roads_utm <- sf::st_transform(
  roads$osm_lines,
  25832
)

sf::st_write(roads_utm,"data/roads.gpkg")
