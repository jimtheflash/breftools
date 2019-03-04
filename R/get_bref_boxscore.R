#' Get bref boxscore table
#' @importFrom magrittr %>%
#' @param gamelink url to boxscore page
#' @return data.frame of boxscore
#' @export
get_bref_boxscore <- function(gamelink = NULL) {
  # parse the gamelink for the gamedate
  gamedate <- stringi::stri_split(gamelink, regex = "/")
  gamedate <- unlist(lapply(gamedate, function(x) x[[length(x)]]))
  ## the date string is the first 8 characters
  gamedate <- substr(gamedate, 1, 8)
  gamedate <- as.Date(paste0(substr(gamedate, 1, 4), "-", 
                             substr(gamedate, 5, 6), "-", 
                             substr(gamedate, 7, 8)))
  # read in the game html
  game_html <- xml2::read_html(gamelink)
  # get team abbreviations to construct tag names
  team_abbrevs <- game_html %>%
    rvest::html_nodes("#content span strong") %>%
    rvest::html_text()
  # use the team_abbrevs to construct tags for boxscore pulls
  boxscores <- list()
  for (team in team_abbrevs) {
    message("getting boxscore for ", team)
    # tables are named using the #box_{{team abbrev}}_{{boxscore type}}
    # only concerned with basics for now
    boxscore_basic_tag <- paste0("#box_", tolower(team), "_basic")
    boxscore_player_links <- paste0(boxscore_basic_tag, " a")
    # grab the boxscore
    boxscore <- game_html %>%
      rvest::html_nodes(boxscore_basic_tag) %>%
      rvest::html_table() %>%
      as.data.frame()
    ## the names of the columns are in the first row but need tidying
    new_names <- dplyr::filter(boxscore, dplyr::row_number() == 1) %>% 
      as.character()
    new_names[grepl("/+", new_names)] <- "PlusMinus"
    names(boxscore) <- new_names
    # filter out bad rows and derived columns
    boxscore <- boxscore %>%
      dplyr::filter(dplyr::row_number() != 1 &
                      MP != "MP" &
                      !grepl("^Team.", Starters)) %>%
      dplyr::select(-dplyr::matches("%"))
    # append team abbrevs
    boxscore$team <- team
    boxscore$opponent <- as.character(team_abbrevs[team_abbrevs != team])
    # append gamedate
    boxscore$gamedate <- gamedate
    # append links to player pages
    playerlinks <- game_html %>%
      rvest::html_nodes(boxscore_player_links) %>%
      rvest::html_attr("href")
    boxscore$playerlink <- playerlinks
    # append the playerids
    playerids <- stringi::stri_split(playerlinks, regex = "/")
    playerids <- unlist(lapply(playerids, function(x) x[[length(x)]]))
    playerids <- gsub(".html", "", playerids)
    boxscore$playerid <- playerids
    # add boxscore to the boxscores object
    boxscores[[team]] <- boxscore
  }
  # stack together and return
  output <- dplyr::bind_rows(boxscores)
  return(output)
}