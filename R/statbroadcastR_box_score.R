#' obtain box score data from statbroadcast
#'
#' This function is designed to obtain statbroadcast data for a game of a given
#' ID and return box score details in a format as closely approximating the
#' games/teams endpoint of the collegefootballdata.com API.
#'
#' @param id the game id in statbroadcast
#' @returns a tibble of box score data
get_box_score <- function(id) {
  xml <-
    xml2::read_xml(glue::glue('http://archive.statbroadcast.com/{id}.xml'))
  game_id <- id
  visitor_name <- xml2::xml_child(xml, 2) |>
    rvest::html_attr('name')
  home_name <- xml2::xml_child(xml, 3) |>
    rvest::html_attr('name')
  visitor_points <- xml2::xml_child(xml, 2) |>
    rvest::html_elements('linescore') |>
    rvest::html_attr('score') |>
    as.numeric()
  home_points <- xml2::xml_child(xml, 3) |>
    rvest::html_elements('linescore') |>
    rvest::html_attr('score') |>
    as.numeric()
  visitor_yards <- xml2::xml_child(xml, 2) |>
    rvest::html_elements('totals') |>
    rvest::html_attr('totoff_yards') |>
    as.numeric()
  home_yards <- xml2::xml_child(xml, 3) |>
    rvest::html_elements('totals') |>
    rvest::html_attr('totoff_yards') |>
    as.numeric()
  visitor_passing <- xml2::xml_child(xml, 2) |>
    rvest::html_elements('totals') |>
    rvest::html_elements('pass')  |>
    rvest::html_attrs() |>
    dplyr::first() |>
    t() |>
    tibble::as_tibble() |>
    dplyr::select(
      net_passing_yards = yds,
      completions = comp,
      attempts = att,
      passing_tds = td,
      passes_intercepted = int
    )
  home_passing <- xml2::xml_child(xml, 3) |>
    rvest::html_elements('totals') |>
    rvest::html_elements('pass')  |>
    rvest::html_attrs() |>
    dplyr::first() |>
    t() |>
    tibble::as_tibble() |>
    dplyr::select(
      net_passing_yards = yds,
      completions = comp,
      attempts = att,
      passing_tds = td,
      passes_intercepted = int
    )


}
