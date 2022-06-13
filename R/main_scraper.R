#' Update team and tournament coverage
#'
#' Uses RSelenium to extract tournament IDs and team names to access statbroadcast
#' data
#'

myswitch <- function (remDr, windowId)
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL,
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

rD <- RSelenium::rsDriver(browser = "firefox")
remDr <- rD$client
remDr$navigate("https://www.statbroadcast.com/events/all.php#")
add_tourneys_button <-
  remDr$findElement(using = 'css selector', "#tournbtn")
add_tourneys_button$clickElement()
pages <- xml2::read_html(remDr$getPageSource()[[1]]) |>
  rvest::html_nodes('li.paginate_button:nth-child(11) > a:nth-child(1)') |>
  rvest::html_text() |>
  as.numeric()
links <- c()
events <- c()
for (n in c(1:pages)) {
  next_page_button <-
    remDr$findElement(using = 'css selector', '#events_next > a:nth-child(1)')
  links <- c(
    links,
    xml2::read_html(remDr$getPageSource()[[1]]) |>
      rvest::html_nodes('#events > tbody:nth-child(2)') |>
      rvest::html_nodes('a') |>
      rvest::html_attr('href')
  )
  events <- c(
    events,
    xml2::read_html(remDr$getPageSource()[[1]]) |>
      rvest::html_nodes('#events > tbody:nth-child(2)') |>
      rvest::html_table() |>
      dplyr::first() |>
      dplyr::pull(X1)
  )
  next_page_button$clickElement()
}
events_tmp <- data.frame(events, links) |>
  dplyr::mutate(
    type = dplyr::case_when(
      stringr::str_detect(links, 'tournament') ~ 'tournament',
      stringr::str_detect(links, 'events') ~ 'event',
      stringr::str_detect(links, 'statmonitr') ~ 'team',
      T ~ NA_character_
    )
  )
games <- events_tmp |>
  dplyr::filter(type == 'event') |>
  dplyr::mutate(Date = events,
                Result = NA_character_,
                Sport = NA_character_) |>
  dplyr::select(Date, Result, Sport, Link = links)
events <- events_tmp |>
  dplyr::filter(type != 'event')
for(row in c(1:nrow(events))){
  name <- events$events[row]
  link <- events$links[row]
  type <- events$type[row]
  if(type == 'tournament'){
    id <- stringr::str_extract(link, 'tid=(.*)$')
  } else {
    id <- stringr::str_extract(link, 'gid=(.*)$')
  }
  remDr$navigate(glue::glue("http://www.statbroadcast.com/events/archive.php?{id}"))
  Sys.sleep(1)
  pages <- xml2::read_html(remDr$getPageSource()[[1]]) |>
    rvest::html_nodes('li.paginate_button') |>
    rvest::html_text() |>
    as.numeric() |>
    na.omit() |>
    max()
  for(page in c(1:pages)){
    next_page_button <-
      remDr$findElement(using = 'css selector', '#archiveTable_next > a:nth-child(1)')
    events_table <- xml2::read_html(remDr$getPageSource()[[1]]) |>
      rvest::html_nodes('#archiveTable') |>
      rvest::html_table() |>
      dplyr::first() |>
      dplyr::slice(-c(1)) |>
      dplyr::select(-c(Link))
    urls <- c()
    if(events_table$Date[1] != 'No data available in table'){
      for(game in c(1:nrow(events_table))){
        game_link <- remDr$findElement(using = 'css selector', glue::glue("tr.{ifelse(game %% 2 == 1,'odd','even')}:nth-child({game}) > td:nth-child(4) > button:nth-child(1)"))
        game_link$clickElement()
        Sys.sleep(0.2)
        primary_window_id <- remDr$getWindowHandles()[[1]]
        game_window_id <- remDr$getWindowHandles()[[2]]
        remDr$switchToWindow(game_window_id)
        game_url <- remDr$getCurrentUrl()[[1]]
        urls <- c(urls, game_url)
        remDr$closeWindow()
        remDr$switchToWindow(primary_window_id)
        Sys.sleep(0.2)
      }
      events_table$Link <- urls
      games <- dplyr::bind_rows(games, events_table)
    }
    next_page_button$clickElement()
    Sys.sleep(0.2)
  }
}
remDr$close()
system("taskkill /im java.exe /f",
       intern = FALSE,
       ignore.stdout = FALSE)
rm(rD)
gc()
