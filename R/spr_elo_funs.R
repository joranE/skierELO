#' Calculate Sprint ELO Ratings
#' 
#' Calculates entire history of ELO ratings for sprint races.
#' 
#' @param races list of data.frame's with race results, in chronological order
#' @param current_rating data.frame with pre-initialized starting rating
#' @param K K factor
#' @param P inflation factor for provisional ratings
#' @param provisional_n number of races a skier is considered provisional
#' @param season_shrink amount to shrink ratings between seasons
#' @param default_rating default rating
#' @export
calc_elo_spr <- function(races,current_rating,K = 32,P = 1.25,
                         provisional_n = 6L,season_shrink = 2/3,default_rating = 1300){
  all_ratings <- vector("list",length(races))
  prev_season <- '1991-1992'
  current_rating_matrix <- as.matrix(current_rating[,c('race_count','cur_rating')])
  
  for (i in seq_along(races)){
    cur_race <- races[[i]]
    cur_season <- cur_race$season[1]
    if (cur_season != prev_season){
      current_rating_matrix[,2L] <- default_rating + ((season_shrink) * (current_rating_matrix[,2] - default_rating))
      prev_season <- cur_season
    }
    
    race_cat <- cur_race$cat1[1]
    
    cur_names <- cur_race[,c('gender','season','date','raceid','fisid','name','nation')]
    ind <- fastmatch::fmatch(cur_names$fisid,current_rating$fisid)
    cur_names$race_count <- current_rating_matrix[ind,1L]
    cur_names$cur_rating <- current_rating_matrix[ind,2L]
    
    K1 <- K
    if (!(race_cat %in% c('WC','WSC','TDS','OWG','WJC','U23'))){
      K1 <- 2/3 * K1
    }
    
    cur_names$new_rating <- eloSprC(rating = cur_names$cur_rating,
                                    raceCount = cur_names$race_count,
                                    K = K1,
                                    P = P,
                                    provisionalN = provisional_n)
    
    #Save new ratings
    all_ratings[[i]] <- cur_names
    
    #Update current rankings
    current_rating_matrix[ind,2L] <- cur_names$new_rating
    current_rating_matrix[ind,1L] <- current_rating_matrix[ind,1L] + 1
    
  }
  all_ratings <- dplyr::bind_rows(all_ratings)
  all_ratings
}