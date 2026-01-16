#' @title Scan the position of all vehicles
#' @name gtfs_scan
#'
#' @description Determines the geographical position of vehicles according to a timetable, based on GTFS data.
#'
#' @param dir GTFS directory (in .zip format)
#' @param day Day ("YYYY-MM-DD")
#' @param time Time ("HH:MM:SS")
#'
#' @return Generates a leaflet map representing the position of vehicles and the 'position_vehicles' object in 'sf' format.
#'
#' @importFrom tidytransit read_gtfs filter_feed_by_date
#' @importFrom hms as_hms
#' @importFrom dplyr group_by reframe mutate filter slice ungroup left_join lag arrange summarise select bind_rows %>% rowwise
#' @importFrom lubridate setdiff
#' @importFrom sf st_as_sf st_transform st_cast st_union st_line_sample st_geometry st_distance st_sfc st_geometry<-
#' @importFrom leaflet addTiles %>% addCircleMarkers leaflet
#' @export
#'
#' @examples
#' \dontrun{
#' gtfs_scan(
#'   dir = "C/.../GTFS.zip",
#'   day = "2025-11-05",
#'   time = "08:05:00")
#' }



gtfs_scan <- function(dir,
                      day,
                      time) {



  ########################################################################
  ############# CHARGEMENT DU GTFS ET PREPARATION DES OBJETS #############
  ########################################################################

  gtfs <- read_gtfs(dir)


  # Choix du jour et de l'horaire
  jour <- day
  horaire <- as_hms(time)

  # Filtre du GTFS par la date
  gtfs <- filter_feed_by_date(gtfs, jour)

  # Export en data.frame des elements du GTFS
  stop_times <- gtfs$stop_times
  stop_id <- gtfs$stops
  trips <- gtfs$trips
  routes <- gtfs$routes

  # Tri des trip_id via les stop_times qui sont impliques dans l'horaire choisi
  tripId <- stop_times %>%
    group_by(trip_id) %>%
    reframe(
      start_time = arrival_time[stop_sequence == 1],
      end_time = arrival_time[stop_sequence == max(stop_sequence)]
    ) %>%
    mutate(
      horaire = ifelse(horaire >= start_time & horaire <= end_time, "Oui", "Non")
    ) %>%
    filter(horaire == "Oui")

  # Tri des stop_times qui sont impliques dans l'horaire choisi (par trip_id)
  stop_times_filtered <- stop_times %>%
    filter(trip_id %in% tripId$trip_id)



  ##########################################################################
  ############# IDENTIFIER LES VEHICULES EXACTEMENT AUX ARRETS #############
  ##########################################################################

  result_exact <- stop_times_filtered %>%
    filter(arrival_time == horaire) %>%
    group_by(trip_id) %>%
    slice(1) %>%  # au cas ou il y aurait des doublons
    ungroup()

  result_exact <- left_join(result_exact, stop_id, by = "stop_id")

  result_exact <- st_as_sf(result_exact,
                           coords = c("stop_lon", "stop_lat"),
                           crs = 4326)

  result_exact <- result_exact[, c("trip_id", "geometry")]

  result_exact <- result_exact %>%
    mutate(interpole = "NON")

  ########################################################################
  ############# IDENTIFIER LES VEHICULES POSITION INTERPOLEE #############
  ########################################################################

  # Identifier les trip_id qui doivent interpoler la position du vehicule
  trip_ids_remaining <- setdiff(tripId$trip_id, result_exact$trip_id)

  # Trier les stop_times ou la position du vehicule doit etre interpolee
  result_NOT_exact <- stop_times_filtered %>%
    filter(trip_id %in% trip_ids_remaining)

  # Ajout des localisations lat long des arrets des vehicules dans leur course
  result_NOT_exact <- left_join(result_NOT_exact, stop_id, by = "stop_id")

  # Cacul de la distance entre deux arrets et de la vitesse locale
  result_NOT_exact_2 <- result_NOT_exact %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    st_transform(2154) %>%
    arrange(trip_id, stop_sequence) %>%
    group_by(trip_id) %>%
    mutate(
      # Distance (metres)
      geom_prev = lag(geometry),
      dist_m = as.numeric(st_distance(geometry, geom_prev, by_element = TRUE)),
      dist_m = ifelse(is.na(dist_m), 0, dist_m),

      # Temps (secondes)
      time_prev = lag(arrival_time),
      delta_t_s = as.numeric(difftime(arrival_time, time_prev, units = "secs")),
      delta_t_s = ifelse(is.na(delta_t_s) | delta_t_s <= 0, NA, delta_t_s),

      # Vitesse (m/s et km/h)
      speed_ms = dist_m / delta_t_s,
      speed_kmh = speed_ms * 3.6,

      # Distance cumulee
      dist_cum_m = cumsum(dist_m)
    ) %>%
    select(-geom_prev, -time_prev) %>%
    ungroup()

  ### Preparation de l'interpolation
  pos_vehicule <- result_NOT_exact_2 %>%
    arrange(trip_id, stop_sequence) %>%
    group_by(trip_id) %>%
    filter(
      any(arrival_time <= horaire) & any(arrival_time >= horaire)
    ) %>%
    summarise(
      # arret avant et apres l’heure donnee
      stop_prev = geometry[max(which(arrival_time <= horaire))],
      stop_next = geometry[min(which(arrival_time >= horaire))],
      t_prev = max(arrival_time[arrival_time <= horaire]),
      t_next = min(arrival_time[arrival_time >= horaire])
    ) %>%
    ungroup() %>%
    mutate(
      # fraction du temps ecoule entre les deux arrets
      ratio_t = as.numeric(horaire - t_prev) / as.numeric(t_next - t_prev),
      ratio_t = pmax(0, pmin(1, ratio_t))  # bornage [0,1]
    )

  # Interpolation spatiale (position sur le segment, On utilise st_line_sample() sur la ligne reliant les deux points)
  pos_vehicule <- pos_vehicule %>%
    rowwise() %>%
    mutate(
      ligne = st_sfc(st_cast(st_union(stop_prev, stop_next), "LINESTRING")),
      geom_interp = st_line_sample(ligne, sample = ratio_t) |> st_cast("POINT")
    ) %>%
    ungroup() %>%
    st_as_sf() %>%
    select(trip_id, t_prev, t_next, ratio_t, geom_interp)

  st_geometry(pos_vehicule) <- "geom_interp"

  pos_vehicule <- pos_vehicule[, c("trip_id", "geom_interp")]

  pos_vehicule <- st_transform(pos_vehicule, 4326)

  names(pos_vehicule)[names(pos_vehicule) == "geom_interp"] <- "geometry"
  st_geometry(pos_vehicule) <- "geometry"

  pos_vehicule <- pos_vehicule %>%
    mutate(interpole = "OUI")


  #####################################################################################
  ############# UNION DES DATA.FRAME DES POSITIONS EXACTES ET INTERPOLEES #############
  #####################################################################################

  result_all <- bind_rows(result_exact, pos_vehicule)

  result_all <- result_all %>%
    left_join(trips %>% select(trip_id, route_id), by = "trip_id")

  result_all <- result_all %>%
    left_join(routes %>% select(route_id, route_long_name, route_type), by = "route_id")


  route_labels <- c(
    "0" = "Tram, Streetcar, Light rail",
    "1" = "Subway, Metro",
    "2" = "Rail",
    "3" = "Bus",
    "4" = "Ferry",
    "5" = "Cable tram",
    "6" = "Aerial lift, suspended cable car",
    "7" = "Funicular",
    "11" = "Trolleybus",
    "12" = "Monorail"
  )

  # Remplacer les codes par les labels
  result_all$route_type <- route_labels[as.character(result_all$route_type)]

  position_vehicles <<- result_all


  leaflet(result_all) %>%
    addTiles() %>%
    addCircleMarkers(
      popup = ~paste0(
        "<b>Trip ID :</b> ", trip_id, "<br>",
        "<b>Interpolation :</b> ", interpole, "<br>",
        "<b>Ligne :</b> ", route_long_name, "<br>",
        "<b>Vehicule :</b> ", route_type, "<br>",
        "<b>Jour :</b> ", jour, "<br>",
        "<b>Horaire :</b> ", horaire
      ),
      radius = 5,
      color = "blue",
      fillOpacity = 0.8
    )

}


