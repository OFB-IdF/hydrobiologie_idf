hubeau::get_hydrobio_stations_hydrobio(
  code_departement = "75,77,78,91,92,93,94,95,10,52"
) %>%
  dplyr::select(
    code_station_hydrobio, libelle_station_hydrobio, uri_station_hydrobio, coordonnee_x, coordonnee_y, code_cours_eau, libelle_cours_eau, code_masse_eau, libelle_masse_eau, code_departement
  ) %>%
  sf::st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154, remove=FALSE) %>%
  rmapshaper::ms_clip(limites_sn) %>%
  saveRDS(file = "data/stations_hydrobio.rds")

readRDS("data/stations_hydrobio.rds") %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_bbox() %>%
  saveRDS(file = "data/bbox_stations.rds")

hubeau::get_hydrobio_indices(
  list(
    code_departement = "75,77,78,91,92,93,94,95,10,52",
    code_indice = "2928,5856,5910,7613,6951,7036" #IBMR, IBD, IBG-équivalent, I2M2, Indice invertébrés grand cours d'eau, IPR
  )
) %>%
  sf::st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154, remove=FALSE) %>%
  rmapshaper::ms_clip(limites_sn) %>%
  dplyr::distinct(
    code_station_hydrobio, code_support, libelle_support, code_prelevement, date_prelevement, code_indice, libelle_indice, resultat_indice, code_qualification, libelle_qualification
  ) %>%
  dplyr::mutate(
    date_prelevement = lubridate::as_date(date_prelevement)
  ) %>%
  dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
  dplyr::filter(!is.na(resultat_indice)) %>%
  saveRDS(file = "data/indices_hydrobio.rds")

openxlsx2::read_xlsx("data/labo/Historique prog labo.xlsx") %>%
  janitor::clean_names() %>%
  (function(df_xl) {
    indices <- df_xl[1,-seq(3)] %>%
      t() %>%
      as.vector() %>%
      unique()

    purrr::map_dfr(
      indices,
      function(i) {
        df_xl[-1,c(seq(3), which(as.vector(t(df_xl[1,])) == i))] %>%
          tidyr::pivot_longer(
            cols = -seq(3),
            names_to = "annee",
            values_to = "realisation"
          ) %>%
          dplyr::mutate(
            annee = annee %>%
              stringr::str_extract_all(
                pattern = "\\d{4}"
              ) %>%
              as.numeric(),
            indice = i,
            code_station = paste0("0", code_station)
          ) %>%
          dplyr::filter(realisation == "X") %>%
          dplyr::select(
            cours_deau, commune, code_station, indice, annee, realisation
          )
      }
    ) %>%
      dplyr::mutate(
        code_indice = dplyr::case_when(
          indice == "IBD" ~ "5856",
          indice == "MPCE" ~ "5910",
          indice == "IBMR" ~ "2928"
        )
      ) %>%
      (function(df) {
        dplyr::bind_rows(
          df,
          df %>%
            dplyr::filter(indice == "MPCE") %>%
            dplyr::mutate(code_indice = "7613")
        )
      })

  }) %>%
  saveRDS(file = "data/operations_labo.rds")
