#' obtain pbp data from statbroadcast
#'
#' This function is designed to obtain statbroadcast data for a game of a given
#' ID and return pbp details in a format as closely approximating the
#' games/teams endpoint of the collegefootballdata.com API.
#'
#' @param id the game id in statbroadcast
#' @returns a tibble of pbp data
get_pbp <- function(id) {
  xml <-
    xml2::read_xml(glue::glue('http://archive.statbroadcast.com/{id}.xml'))
  pbp <- purrr::map_dfr(xml2::xml_child(xml, 7) |>
                          rvest::html_elements('qtr'), \(x) {
                            pbp <- x |>
                              rvest::html_elements('play') |>
                              rvest::html_attrs() |>
                              lapply(t) |>
                              lapply(data.frame) |>
                              data.table::rbindlist(fill = TRUE) |>
                              tibble::as_tibble()
                            pbp$period <- x |>
                              rvest::html_attr('number') |>
                              as.numeric()
                            pbp
                          })
  away_name <- xml2::xml_child(xml, 2) |>
    rvest::html_attr('name')
  away_id <- xml2::xml_child(xml, 2) |>
    rvest::html_attr('id')
  home_name <- xml2::xml_child(xml, 3) |>
    rvest::html_attr('name')
  home_id <- xml2::xml_child(xml, 3) |>
    rvest::html_attr('id')
  pbp <- pbp |>
    dplyr::mutate(
      offense_play = ifelse(hasball == home_id, home_name, away_name),
      offense_conference = NA_character_,
      defense_play = ifelse(hasball == home_id, away_name, home_name),
      home = home_name,
      away = away_name,
      hscore = replace(hscore, dplyr::row_number() == 1, 0),
      vscore = replace(vscore, dplyr::row_number() == 1, 0),
      clock = dplyr::case_when(stringr::str_detect(tokens, 'CMT:') ~ '15:00',
                               stringr::str_detect(tokens, 'T:15:00') ~ '15:00',
                               T~clock)
    ) |>
    tidyr::fill(hscore) |>
    tidyr::fill(vscore) |>
    tidyr::fill(clock) |>
    dplyr::mutate(
      offense_score = ifelse(hasball == home_id, hscore, vscore),
      defense_score = ifelse(hasball == away_id, vscore, hscore),
      game_id = id,
      new_drive_flg = stringr::str_detect(tokens, '\\{DRIVE\\}'),
      drive_number = cumsum(new_drive_flg),
      drive_id = paste0(game_id, sprintf('%02d', drive_number)),
      clock.minutes = as.numeric(gsub(':', '', stringr::str_extract(clock, '(.*):'))),
      clock.seconds = as.numeric(gsub(':', '', stringr::str_extract(clock, ':(.*)'))),
      offense_timeouts = NA_real_,
      defense_timeouts = NA_real_,
    ) |>
    dplyr::group_by(drive_id) |>
    dplyr::mutate(play_number = dplyr::row_number(),
                  id_play = paste0(drive_id, sprintf('%02d', play_number))) |>
    dplyr::ungroup()


}
