#' Generate Rankings
#' 
#' @param .ratings Raw ratings for all skiers
#' @param .season season
#' @param .date date
#' @param .prov provisional number of races
#' @export
#' @import dplyr
ath_ranking <- function(.ratings,.season = max(.ratings$season),
                        .date = as.character(Sys.Date()),.prov = 6){
  if (.season > max(.ratings$season)) .season <- max(.ratings$season)
  rnk <- .ratings %>%
    filter(season == .season & date <= .date) %>%
    group_by(gender,fisid,name) %>%
    mutate(season_races = n()) %>%
    filter(date == max(date) & race_count >= .prov) %>%
    group_by(gender) %>%
    mutate(ranking = min_rank(-new_rating)) %>%
    arrange(gender,ranking) %>%
    select(fisid,name,nation,new_rating,ranking) %>%
    as.data.frame()
  setNames(split(rnk,rnk$gender),c('Men','Women'))
}