get_bref_season_boxscores <- function(season = NULL) {
  # operationalize season as starting on 1 september, or get actual start dates from tables...
  # loop through all dates between start and end, and output a data.frame to be ingested by a db
  # see how big your db is on hostgator
  # loop through dates in the season; if no games, move on
  daily_games_url <- "https://www.basketball-reference.com/boxscores/?month=03&day=1&year=2019"
  daily_gamelinks <- xml2::read_html(daily_games_url) %>%
    rvest::html_nodes(".gamelink a") %>%
    rvest::html_attr("href") %>%
    paste0("https://www.basketball-reference.com", .)
  # loop through gamelinks and extract boxscore information
  for (gamelink in daily_gamelinks) {
    message("getting boxscore for ", gamelink)
    boxscore <- breftools::get_bref_boxscore(gamelink)
  }
  
}
