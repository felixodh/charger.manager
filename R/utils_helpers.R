
#' Transform charging station API object into a flat list
#'
#' Converts a nested API response for an EV charging station into a
#' simplified named list with consistent data types. This helps to
#' standardize heterogeneous API outputs for further analysis in R.
#'
#' @param x A single charging station object returned from the API.
#' It is expected to contain fields such as `name`, `address`,
#' `coordinates`, and `operator`.
#'
#' @return A named list with standardized character and logical fields,
#' including station metadata, location information, and operator details.
#'
#' @details
#' The function flattens nested fields (e.g. `coordinates`, `operator`)
#' and coerces all outputs into consistent types:
#' \itemize{
#'   \item Character fields: names, addresses, IDs, region codes
#'   \item Logical fields: availability flags such as `publish`
#'   \item Extracted nested fields: latitude, longitude, operator name
#' }
#'
#' Missing or unavailable fields are replaced with `NA_character_`
#' where appropriate.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' station <- list(
#'   name = "Example Station",
#'   address = "Main Street 1",
#'   postal_code = "72070",
#'   city = "Tübingen",
#'   country = "DE",
#'   time_zone = "Europe/Berlin",
#'   last_updated = "2024-01-01",
#'   has_in_person_support = FALSE,
#'   facility_description = "Fast charging available",
#'   id = "123",
#'   publish = TRUE,
#'   original_id = "abc",
#'   source = "api",
#'   official_region_code = "DE-BW",
#'   coordinates = list(latitude = 48.52, longitude = 9.06),
#'   operator = list(name = "Example Operator")
#' )
#'
#' trans_fun_dt(station)
#' }
trans_fun_dt <- function(x) {
  list(
    name = as.character(x$name),
    address = as.character(x$address),
    postal_code = as.character(x$postal_code),
    city = as.character(x$city),
    country = as.character(x$country),
    time_zone = as.character(x$time_zone),
    last_updated = as.character(x$last_updated),
    has_in_person_support = as.logical(x$has_in_person_support),
    facilities = NA_character_,
    facility_description = as.character(x$facility_description),
    id = as.character(x$id),
    publish = as.logical(x$publish),
    original_id = as.character(x$original_id),
    source = as.character(x$source),
    official_region_code = as.character(x$official_region_code),
    latitude = as.character(x$coordinates$latitude),
    longitude = as.character(x$coordinates$longitude),
    operator = as.character(x$operator$name)
  )
}



#' Transform charging pool API object into a flat structure
#'
#' Extracts nested information from a charging station API response,
#' including charging pool, EVSE, and connector-level attributes.
#' The function flattens deeply nested lists into a single named list
#' suitable for tabular conversion and spatial analysis workflows.
#'
#' @param x A single charging station object returned from the API.
#' It must contain a `charging_pool` field with nested EVSE and connector
#' structures.
#'
#' @return A named list containing:
#' \itemize{
#'   \item Station-level identifiers (`station_id`, `original_station_id`)
#'   \item Charging pool identifiers (`id`, `original_uid`)
#'   \item EVSE-level identifiers (`evse_uid`, `evse_id`, `evse_original_uid`)
#'   \item Connector attributes (standard, format, power type, voltage, amperage, power)
#'   \item Connector identifiers (`connector_id`, `connector_original_id`)
#' }
#'
#' @details
#' The function assumes that only the first elements of nested lists are used:
#' \code{charging_pool[[1]]}, \code{evses[[1]]}, and \code{connectors[[1]]}.
#' This simplifies hierarchical API structures but may discard additional
#' charging units if multiple exist.
#'
#' All returned values are coerced to character type for consistency in
#' downstream data frames.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- list(
#'   id = "station_1",
#'   original_id = "orig_1",
#'   charging_pool = list(
#'     list(
#'       id = "pool_1",
#'       original_uid = "p1",
#'       evses = list(
#'         list(
#'           uid = "evse_1",
#'           evse_id = "e1",
#'           original_uid = "eu1",
#'           connectors = list(
#'             list(
#'               standard = "CCS",
#'               format = "socket",
#'               power_type = "DC",
#'               max_voltage = 400,
#'               max_amperage = 125,
#'               max_electric_power = 50000,
#'               id = "c1",
#'               original_id = "co1"
#'             )
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' trans_fun_chargpool(x)
#' }
trans_fun_chargpool <- function(x){
  list(
    station_id = as.character(x$id),
    original_station_id = as.character(x$original_id),
    id = as.character(x$charging_pool[[1]]$id),
    original_uid = as.character(x$charging_pool[[1]]$original_uid),
    evse_uid = as.character(x$charging_pool[[1]]$evses[[1]]$uid),
    evse_id = as.character(x$charging_pool[[1]]$evses[[1]]$evse_id),
    evse_original_uid = as.character(x$charging_pool[[1]]$evses[[1]]$original_uid),
    connector_standard =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$standard),
    connector_format =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$format),
    connector_pwr_type =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$power_type),
    connector_max_volt =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$max_voltage),
    connector_max_amp =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$max_amperage),
    connector_electric_power =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$max_electric_power),
    connector_id =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$id),
    connector_original_id =
      as.character(x$charging_pool[[1]]$evses[[1]]$connectors[[1]]$original_id)
  )
}



