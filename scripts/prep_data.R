stations_hydrobio <- readRDS("data/stations_hydrobio.rds")
indices_hydrobio <- readRDS("data/indices_hydrobio.rds")
operations_labo <- readRDS("data/operations_labo.rds")

indices_moy_an <- indices_hydrobio %>%
  dplyr::group_by(
    code_station_hydrobio, annee,
    code_support, libelle_support,
    code_indice, libelle_indice
    ) %>%
  dplyr::summarise(
    resultat_moy = mean(resultat_indice),
    .groups = "drop"
    ) %>%
  dplyr::mutate(
    libelle_support = dplyr::case_when(
      code_support == 4 ~ "Poissons",
      code_support == 10 ~ "Diatomées",
      code_support == 13 ~ "Macroinvertébrés",
      code_support == 27 ~ "Macrophytes"
    )
  )

stations_sf <- stations_hydrobio %>%
  dplyr::filter(
    code_station_hydrobio %in% indices_hydrobio$code_station_hydrobio
    ) %>%
  dplyr::select(
    code_station_hydrobio, libelle_station_hydrobio, uri_station_hydrobio,
    coordonnee_x, coordonnee_y,
    code_cours_eau, libelle_cours_eau, code_masse_eau, libelle_masse_eau,
    code_departement
    )

fun_summ_eqb <- function(eqb, stations, indices) {
  stations %>%
    dplyr::inner_join(
      indices %>%
        dplyr::filter(libelle_support %in% eqb) %>%
        dplyr::group_by(code_station_hydrobio) %>%
        dplyr::summarise(
          nb_taxons = dplyr::n_distinct(code_support),
          nb_annee = dplyr::n_distinct(annee),
          EQB = paste(eqb, collapse = ", ")
        ),
      by = "code_station_hydrobio"
    )
}

list(
  "Poissons", "Macroinvertébrés", "Diatomées", "Macrophytes",
  c("Poissons", "Macroinvertébrés", "Diatomées", "Macrophytes"),
  c("Macroinvertébrés", "Diatomées", "Macrophytes")
) %>%
  purrr::map_df(
    .f = fun_summ_eqb,
    stations = stations_sf,
    indices = indices_moy_an
  ) %>%
  dplyr::mutate(
    taille_symbole = dplyr::case_when(
      nb_annee == 1 ~ 4,
      nb_annee <= 5 ~ 6,
      nb_annee <= 10 ~ 8,
      nb_annee > 10 ~ 12
    ),
    labo = code_station_hydrobio %in% operations_labo$code_station,
    colour_eqb = dplyr::case_when(
      nb_taxons == 1 ~ "#440154FF",
      nb_taxons == 2 ~ "#31688EFF",
      nb_taxons == 3 ~ "#35B779FF",
      nb_taxons == 4 ~ "#FDE725FF"
    ),
    colour = ifelse(
      code_station_hydrobio %in% operations_labo$code_station,
      "blue",
      "darkgrey"
    ),
    size = ifelse(
      code_station_hydrobio %in% operations_labo$code_station,
      4,
      2
    )
  ) %>%
  # sf::st_as_sf(
  #   coords = c("coordonnee_x", "coordonnee_y"), remove = FALSE,
  #   crs = 2154
  #   ) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(
    code_departement %in% c(75, 77, 78, 91, 92, 93, 94, 95) |  labo
  ) %>%
  dplyr::mutate(
    EQB = factor(
      EQB,
      levels = c(
        "Poissons, Macroinvertébrés, Diatomées, Macrophytes",
        "Macroinvertébrés, Diatomées, Macrophytes",
        "Macroinvertébrés", "Diatomées", "Macrophytes", "Poissons"
      )
    ),
    libelle_station = paste0(libelle_station_hydrobio, " (", nb_annee, " années de suivi)")
  ) %>%
  dplyr::arrange(EQB) %>%
  saveRDS(file = "data/stations_sf.rds")

data_graphs <- indices_moy_an %>%
  tidyr::complete(
    code_station_hydrobio, code_indice
  ) %>%
  dplyr::left_join(
    stations_hydrobio %>%
      dplyr::select(code_station_hydrobio, libelle_station_hydrobio) %>%
      dplyr::mutate(
        libelle_station = paste0(libelle_station_hydrobio, " (", code_station_hydrobio, ")")
      ),
    by = "code_station_hydrobio"
  ) %>%
  dplyr::left_join(
    operations_labo %>%
      dplyr::select(-commune) %>%
      dplyr::mutate(labo = TRUE),
    by = c(
      "code_station_hydrobio" = "code_station",
      "code_indice" = "code_indice",
      "annee" = "annee"
    )
  ) %>%
  dplyr::mutate(
    indice = dplyr::case_when(
      code_indice == 5856 ~ "IBD (diatomées)",
      code_indice == 2928 ~ "IBMR (macrophytes)",
      code_indice == 5910 ~ "IBG équivalent (macroinvertébrés)",
      code_indice == 7613 ~ "I2M2 (macroinvertébrés)",
      code_indice == 6951 ~ "Invertébrés grands cours d'eau",
      code_indice == 7036 ~ "IPR (poissons)"
    ) %>%
      factor(levels = c("IBG équivalent (macroinvertébrés)", "I2M2 (macroinvertébrés)", "Invertébrés grands cours d'eau", "IBD (diatomées)", "IBMR (macrophytes)",  "IPR (poissons)")),
    labo = ifelse(is.na(labo), FALSE, labo)
  )

saveRDS(data_graphs, file = "data/data_graphs.rds")
