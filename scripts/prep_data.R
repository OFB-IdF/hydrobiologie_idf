calc_indice_moy_an <- function(indices) {
  indices %>%
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
}

prep_data_carte <- function(stations, indices, liste_eqb) {
  fun_summ_eqb <- function(eqb, stations, indices) {
    stations %>%
      dplyr::inner_join(
        indices %>%
          dplyr::group_by(code_station_hydrobio) %>%
          dplyr::mutate(nb_taxons = dplyr::n_distinct(code_support)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(code_station_hydrobio, code_support) %>%
          dplyr::mutate(nb_annee = dplyr::n_distinct(annee)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(libelle_support %in% eqb) %>%
          dplyr::group_by(code_station_hydrobio, nb_taxons) %>%
          dplyr::summarise(
            EQB = paste(eqb, collapse = ", ") %>%
              factor(levels = c(
                "Poissons, Macroinvertébrés, Diatomées, Macrophytes",
                "Macroinvertébrés, Diatomées, Macrophytes",
                "Macroinvertébrés", "Diatomées", "Macrophytes", "Poissons"
              )),
            nb_annee = max(nb_annee)
          ),
        by = "code_station_hydrobio"
      ) %>%
      dplyr::mutate(
        taille_symbole = dplyr::case_when(
          nb_annee == 1 ~ 4,
          nb_annee <= 5 ~ 6,
          nb_annee <= 10 ~ 8,
          nb_annee > 10 ~ 12
        ),
        colour_eqb = dplyr::case_when(
          nb_taxons == 1 ~ "#440154FF",
          nb_taxons == 2 ~ "#31688EFF",
          nb_taxons == 3 ~ "#35B779FF",
          nb_taxons == 4 ~ "#FDE725FF"
        ),
        colour = ifelse(regie, "blue", "darkgrey"),
        size = ifelse(regie, 4, 2)
      )
  }

  indices_moy_an <- calc_indice_moy_an(indices)

  stations_sf <- stations %>%
    dplyr::select(
      code_station_hydrobio, libelle_station_hydrobio,
      uri_station_hydrobio,
      coordonnee_x, coordonnee_y,
      code_cours_eau, libelle_cours_eau, code_masse_eau,
      libelle_masse_eau,
      code_departement, regie
      )

  liste_eqb %>%
    purrr::map_df(
      .f = fun_summ_eqb,
      stations = stations_sf,
      indices = indices_moy_an
    ) %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(
      libelle_station = paste0(
        libelle_station_hydrobio, " (", nb_annee, " années de suivi)"
        )
      ) %>%
    dplyr::arrange(EQB)

}


prep_data_graph <- function(stations, indices, suivi_regie) {
  calc_indice_moy_an(indices) %>%
  tidyr::complete(
    code_station_hydrobio, code_indice
  ) %>%
  dplyr::left_join(
    stations %>%
      dplyr::select(code_station_hydrobio, libelle_station_hydrobio) %>%
      dplyr::mutate(
        libelle_station = paste0(
          libelle_station_hydrobio,
          " (", code_station_hydrobio, ")"
          )
      ),
    by = "code_station_hydrobio"
  ) %>%
  dplyr::left_join(
    suivi_regie %>%
      dplyr::select(-commune) %>%
      dplyr::mutate(regie = TRUE),
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
    regie = ifelse(is.na(regie), FALSE, regie)
  )

}
