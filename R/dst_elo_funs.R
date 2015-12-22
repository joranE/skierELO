#' Calculate Distace ELO Ratings
#' 
#' Calculates entire history of ELO ratings for distance races.
#' 
#' @param races list of data.frames with results, in chronological order
#' @param current_rating data.frame with pre-initialized starting rating
#' @param K List of K factors for each race type
#' @param P inflation factor for provisional ratings
#' @param provisional_n number of races a skier is considered provisional
#' @param season_shrink amount to shrink ratings between seasons
#' @param default_rating
#' @export
calc_elo_dst <- function(races,current_rating,
                          K = c('Interval' = 32,'Mass' = 25,'Pursuit' = 25,'Handicap' = 20,'Pursuit Break' = 25),
                          P = 1.25,provisional_n = 6L,season_shrink = 2/3,default_rating = 1300){
  all_ratings <- vector("list",length(races))
  prev_season <- '1991-1992'
  for (i in seq_along(races)){
    cur_race <- races[[i]]
    cur_season <- cur_race$season[1]
    if (cur_season != prev_season){
      current_rating$cur_rating <- with(current_rating,
                                        default_rating + ((season_shrink) * (cur_rating - default_rating)))
      prev_season <- cur_season
    }
    
    race_type <- cur_race$start[1]
    race_cat <- cur_race$cat1[1]
    
    if (all(is.na(cur_race$time))){
      next
    }
    if (any(is.na(cur_race$time)) && !all(is.na(cur_race$time))){
      cur_race$time[is.na(cur_race$time)] <- max(cur_race$time,na.rm = TRUE) + 10
    }
    
    cur_names <- cur_race[,c('gender','season','date','raceid','fisid','name','nation')]
    cur_names$race_count <- 0L
    cur_names$cur_rating <- 0
    ind <- match(cur_names$fisid,current_rating$fisid)
    cur_names$race_count <- current_rating$race_count[ind]
    cur_names$cur_rating <- current_rating$cur_rating[ind]
    
    K1 <- unname(K[race_type])
    if (!(race_cat %in% c('WC','WSC','TDS','OWG','WJC','U23'))){
      K1 <- 2/3 * K1
    }
    
    cur_names$new_rating <- eloDstC(rating = cur_names$cur_rating,
                                    raceCount = cur_names$race_count,
                                    time = cur_race$time,
                                    K = K1,
                                    P = P,
                                    provisionalN = provisional_n)
    
    #Save new ratings
    all_ratings[[i]] <- cur_names
    
    #Update current rankings
    current_rating$cur_rating[ind] <- cur_names$new_rating
    current_rating$race_count[ind] <- current_rating$race_count[ind] + 1
    
  }
  all_ratings <- dplyr::rbind_all(all_ratings)
  all_ratings
}

