limites_idf <-  sf::st_read("C:/QGIS-CUSTOM/DATA/VECTEUR/administration/ADMIN EXPRESS/ADE_3-1_SHP_LAMB93_FXX/REGION.shp") %>%
  dplyr::filter(INSEE_REG == "11") %>%
  dplyr::summarise() %>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

bbox_idf <- sf::st_bbox(limites_idf)

limites_sn <- sf::st_read(
  dsn = "C:/QGIS-CUSTOM/DATA/VECTEUR/hydrographie/bdtopage_idf.gpkg",
  layer = "BassinHydrographique"
) %>%
  dplyr::filter(LbBH == "Seine-Normandie") %>%
  rmapshaper::ms_simplify()

bbox_sn <- sf::st_bbox(limites_sn)

masque_metropole <- sf::st_read("C:/QGIS-CUSTOM/DATA/VECTEUR/administration/ADMIN EXPRESS/ADE_3-1_SHP_LAMB93_FXX/REGION.shp") %>%
  sf::st_difference(limites_sn) %>%
  # dplyr::filter(INSEE_REG != "11") %>%
  dplyr::summarise() %>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

reseau_hydro <- sf::st_read("C:/QGIS-CUSTOM/DATA/VECTEUR/hydrographie/bdtopage_idf.gpkg", layer = "cours_eau_2022") %>%
  # rmapshaper::ms_clip(
  #   limites_idf %>%
  #     sf::st_transform(crs = 2154) %>%
  #     sf::st_buffer(1000)
  # ) %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::select(TopoOH, CdOH) %>%
  rmapshaper::ms_simplify()

edl_2022 <- sf::st_read("C:/QGIS-CUSTOM/DATA/VECTEUR/surveillance/edl_sn.gpkg") %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  dplyr::mutate(
    dplyr::across(
      c("ETAT.BIOLOGIQUE", "ETAT.ECOLOGIQUE", "ETAT.PHYSICO.CHIMIQUE"),
      function(x) {factor(x, levels = c("très bon", "bon", "moyen", "médiocre", "mauvais", "indéterminé"))}
    )
  ) %>%
  rmapshaper::ms_simplify()

save(limites_idf, limites_sn, masque_metropole, reseau_hydro, edl_2022, file = "data/referentiels.rda")
