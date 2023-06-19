telecharger_stations <- function(code_departement, limites_zone, suivi_regie) {
  hubeau::get_hydrobio_stations_hydrobio(
    code_departement = paste(code_departement, collapse = ",")
    ) %>%
    dplyr::select(
      code_station_hydrobio, libelle_station_hydrobio,
      uri_station_hydrobio, coordonnee_x, coordonnee_y,
      code_cours_eau, libelle_cours_eau, code_masse_eau,
      libelle_masse_eau, code_departement
      ) %>%
    sf::st_as_sf(
      coords = c("coordonnee_x", "coordonnee_y"),
      crs = 2154,
      remove=FALSE
      ) %>%
    rmapshaper::ms_clip(limites_zone) %>%
    dplyr::mutate(
      regie = code_station_hydrobio %in% suivi_regie$code_station
    )
}

telecharger_indices <- function(code_departement, code_indice, limites_zone, suivi_regie) {
  hubeau::get_hydrobio_indices(
    list(
      code_departement = paste(code_departement, collapse = ","),
      code_indice = paste(code_indice, collapse = ",")
      )
    ) %>%
    sf::st_as_sf(
      coords = c("coordonnee_x", "coordonnee_y"),
      crs = 2154,
      remove = FALSE
      ) %>%
    rmapshaper::ms_clip(limites_zone) %>%
  dplyr::distinct(
    code_station_hydrobio, code_support, libelle_support,
    code_prelevement, date_prelevement, code_indice, libelle_indice,
    resultat_indice, code_qualification, libelle_qualification
    ) %>%
    dplyr::mutate(
      date_prelevement = lubridate::as_date(date_prelevement)
      ) %>%
    dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
    dplyr::filter(!is.na(resultat_indice)) %>%
    dplyr::mutate(
      regie = code_station_hydrobio %in% suivi_regie$code_station
    )
}

check_updates <- function(codes_departements, codes_departements_extra, code_indice, limites_zone, suivi_regie, dest_folder) {
  if (!file.exists(file.path(dest_folder, "indices_hydrobio.rds"))) {
    to_update <- TRUE
  } else {
    indices_hydrobio <- readRDS(file.path(dest_folder, "indices_hydrobio.rds"))

    old_data <- indices_hydrobio %>%
      dplyr::distinct(code_station_hydrobio, code_support, date_prelevement) %>%
      dplyr::group_by(code_station_hydrobio, code_support) %>%
      dplyr::summarise(old = max(date_prelevement), .groups = "drop")

    new_data <- hubeau::get_hydrobio_indices(
      list(
        code_departement = paste(c(codes_departements, codes_departements_extra), collapse = ","),
        code_indice = paste(code_indice, collapse = ",")
      )
    ) %>%
      sf::st_as_sf(
        coords = c("coordonnee_x", "coordonnee_y"),
        crs = 2154, remove = FALSE
      ) %>%
      rmapshaper::ms_clip(limites_zone) %>%
      dplyr::filter(
        code_departement %in% codes_departements |
          code_station_hydrobio %in% suivi_regie$code_station
        ) %>%
      dplyr::distinct(
        code_departement, code_station_hydrobio, code_support, libelle_support, code_prelevement,
        date_prelevement, code_indice, libelle_indice, resultat_indice,
        code_qualification, libelle_qualification
      ) %>%
      dplyr::mutate(
        date_prelevement = lubridate::as_date(date_prelevement)
      ) %>%
      dplyr::mutate(
        annee = lubridate::year(date_prelevement)
      ) %>%
      dplyr::filter(!is.na(resultat_indice)) %>%
      dplyr::distinct(code_station_hydrobio, code_support, date_prelevement) %>%
      dplyr::group_by(code_station_hydrobio, code_support) %>%
      dplyr::summarise(new = max(date_prelevement), .groups = "drop")

    to_update <- dplyr::full_join(
      old_data, new_data,
      by = c("code_station_hydrobio", "code_support")
    ) %>%
      dplyr::mutate(to_update = new > old | (!is.na(new) & is.na(old))) %>%
      dplyr::pull(to_update) %>%
      any()
  }

  to_update
}


