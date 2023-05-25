library(sf)

data_graphs <- readRDS("data/data_graphs.rds")

#https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    if (length(unique(na.omit(x))) == 1) {
      breaks <- unique(na.omit(x))
    } else {
      breaks <- floor(pretty(x, n, ...))
    }

    names(breaks) <- attr(breaks, "labels")
    unique(breaks)
  }
  return(fxn)
}

popups_indices <- data_graphs %>%
  dplyr::group_by(code_station_hydrobio) %>%
  dplyr::group_split() %>%
  purrr::map(
    function(data_station) {
      x_lims <- range(na.omit(data_station$annee))
      x_breaks <- integer_breaks(n = 3)(data_station$annee)

      popup <-
      (
        data_station %>%
          dplyr::group_by(indice) %>%
          dplyr::group_split() %>%
          purrr::map(
            function(data_indice) {
              p <- data_indice %>%
                ggplot2::ggplot(
                  mapping = ggplot2::aes(
                    x = annee,
                    y = resultat_moy
                  )
                ) +
                ggplot2::geom_smooth(
                  se = FALSE,
                  method = "gam",
                  formula = y ~ s(x, k = 2),
                  linetype = "dashed",
                  size = .5
                ) +
                ggplot2::geom_point(
                  mapping = ggplot2::aes(
                    colour = labo,
                    # size = labo
                    )
                ) +
                ggplot2::scale_colour_manual(
                  guide = "none",
                  values = c(
                    `TRUE` = "black",
                    `FALSE` = "darkgrey"
                  )
                ) +
                # ggplot2::scale_size_manual(
                #   guide = "none",
                #   values = c(
                #     `TRUE` = 3,
                #     `FALSE` = 2
                #   )
                # ) +
                ggplot2::theme_light() +
                ggplot2::theme(
                  panel.grid.major.x = ggplot2::element_blank(),
                  panel.grid.minor.x = ggplot2::element_blank(),
                  panel.grid.minor.y = ggplot2::element_blank()
                ) +
                ggplot2::labs(x = NULL, y = NULL) +
                ggplot2::facet_wrap(ggplot2::vars(indice)) +
                ggplot2::scale_x_continuous(
                  limits = x_lims,
                  breaks = x_breaks
                )

              if (unique(data_indice$code_indice) %in% c(5856, 2928, 5910, 6951))
                p <- p +
                  ggplot2::scale_y_continuous(
                    limits = c(0, 20),
                    breaks = c(0, 5, 10, 15, 20)
                  )

              if (unique(data_indice$code_indice) %in% c(7613))
                p <- p +
                  ggplot2::scale_y_continuous(
                    limits = c(0, 1),
                    breaks = c(0, .25, .5, .75, 1)
                  )

              if (unique(data_indice$code_indice) %in% c(7036))
                p <- p +
                  ggplot2::scale_y_reverse(
                    limits = c(60, 0),
                    breaks = c(60, 45, 30, 15, 0)
                  )

              p
              # plotly::ggplotly(p)
            }
          ) %>%
          patchwork::wrap_plots() +
          patchwork::plot_annotation(
            title = unique(data_station$libelle_station)
          )
        # plotly::subplot(nrows = 2) %>%
        #   plotly::layout(title = unique(data_station$libelle_station))
      )

      ggplot2::ggsave(
        plot = popup,
        filename = paste0("www/popups/", unique(data_station$code_station_hydrobio), ".png"),
        width = 18,
        height = 12,
        units = "cm",
        dpi = 150
      )

      popup
    }
  ) %>%
  purrr::set_names(
    data_graphs %>%
      dplyr::group_by(code_station_hydrobio) %>%
      dplyr::group_keys() %>%
      dplyr::pull(1)
  )

# saveRDS(popups_indices, "data/popups_indices.rds")

# %>%
#   '['(stations_sf$code_station_hydrobio) %>%
#   leafpop::popupGraph(width = 600, height = 400)
