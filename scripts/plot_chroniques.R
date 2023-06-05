#' Title
#'
#' @param data_stations
#' @param data_graphs
#' @param interactive
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr as_tibble summarise n_distinct left_join select mutate case_when group_by
#' @importFrom ggplot2 ggplot geom_col aes position_dodge labs theme_minimal theme element_blank element_text
#' @importFrom plotly ggplotly
#' @importFrom sf st_drop_geometry
plot_chroniques <- function(data_stations, data_graphs, interactive = FALSE) {
  if ("sf" %in% class(data_stations))
    data_stations <- data_stations %>%
      sf::st_drop_geometry() %>%
      dplyr::as_tibble()

  PlotChroniques <- data_graphs %>%
    dplyr::left_join(
      dplyr::select(data_stations, code_station_hydrobio, code_departement),
      by = "code_station_hydrobio"
    ) %>%
    dplyr::mutate(
      idf = code_departement %in% c(75, 77, 78, 91, 92, 93, 94, 95),
      EQB = dplyr::case_when(
        code_indice %in% c(7036) ~ "Poissons",
        code_indice %in% c(2928) ~ "Macrophytes",
        code_indice %in% c(5856) ~ "Diatomées",
        code_indice %in% c(5910, 7613, 6951) ~ "Macroinvertébrés"
      ) %>%
        factor(levels = c("Poissons", "Macroinvertébrés", "Macrophytes", "Diatomées"))
      ) %>%
    dplyr::group_by(idf, code_station_hydrobio, EQB) %>%
    dplyr::summarise(
      nb_annee = dplyr::n_distinct(annee),
      .groups = "drop"
      ) %>%
    dplyr::mutate(
      Chronique = cut(
        nb_annee,
        breaks = c(0, 5, 10, 15, 20, 50),
        labels = c("< 5", "5-10", "10-15", "15-20", "> 20 ans"),
        include.lowest = TRUE
        )
    ) %>%
    dplyr::group_by(EQB, Chronique) %>%
    dplyr::summarise(n = dplyr::n_distinct(code_station_hydrobio), .groups = "drop") %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
      mapping = ggplot2::aes(x = Chronique, y = n, fill = EQB),
      position = ggplot2::position_dodge(preserve = "single")
    ) +
    ggplot2::labs(
      x = "Durée de la chronique",
      y = "Nombre de station",
      fill = "\nElément de qualité\nbiologique"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(hjust = .95),
      legend.position = "bottom"
    )

  if (interactive) {
    plotly::ggplotly(PlotChroniques) %>%
      plotly::layout(
        legend = list(
          orientation = 'h',
          title = list(
            side = 'top'
          )
          ),
        xaxis = list(title = FALSE),
        yaxis = list(title = FALSE),
        margin = list(
          t = 50, b = 100, r = 50, l = 100
        )
        ) %>%
      plotly::add_annotations(
        xref = "paper", yref = "paper",
        x = 1, y = -.15,
        text = "Durée de la chronique",
        showarrow = FALSE
      ) %>%
      plotly::add_annotations(
        xref = "paper", yref = "paper",
        x = -.15, y = 1,
        text = "Nombre de stations",
        showarrow = FALSE,
        textangle = -90
      )
  } else {
    PlotChroniques
  }
}
