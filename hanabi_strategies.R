rm(list = ls())

library(dplyr)

# Outputs column number, from color letter
color_column <- function(col){
  if (is.na(col)){
    return(NA)
  } else if (col == "W"){
    return(1)
  } else if (col == "R" ){
    return(2)
  } else if (col == "G" ){
    return(3)
  } else if (col == "B" ){
    return(4)
  } else if (col == "Y" ){
    return(5)
  } else {
    println("INVALID COLOR!")
    return(6)
  }
}

# Returns 0 if discardable, 1 if playable, 2 if not playable, 3 if unknown
card_playable <- function(card_info, table){
  if(is.na(card_info[2])){
    return(3)
  }
  number = as.integer(card_info[2])
  color_num = color_column(card_info[1])
  if(number <= min(table[1,] ) ){
    return(0)
  } else if(!is.na(color_num) &&
    table[1, color_num] >= number ){
    return(0)
  } else if(min(table[1,]) == max(table[1,]) && number == max(table[1,]) + 1 ){
    return(1)
  } else if(!is.na(color_num) && number == table[1,color_num] + 1){
    return(1)
  } else if(number > max(table[1,]) + 1){
    return(2)
  } else if(!is.na(color_num) && number > table[1,color_num] + 1){
    return(2)
  } else {
    return(3)
  }
}

# Outputs a five element string, each indicating the playability of each card
hand_playable <- function(hand_info, table) {
  result = c()
  for(i in 1:nrow(hand_info)){
    result <- append(result, card_playable(hand_info[i,], table) )
  }
  return(result)
}

strategy1 <- function(active_player, clock, fuse, table, discard, hands, info, 
                      last_round, info_hist, action_hist) {
  print(action_hist)
  own_hand_info <- info %>% filter(player == active_player) %>% select(color, number)
  others_unknown_playable <- hand_playable(hands[,1:2],table)
  others_info <- info %>% filter(player != active_player)
  others_known_playable <- hand_playable(others_info[,1:2],table)
  if (1 %in% hand_playable(own_hand_info, table) ){
    #print('A')
    return( c("P", which(hand_playable(own_hand_info, table) == 1)[1]) )
  } else if (0 %in% hand_playable(own_hand_info, table) && clock < 8) {
    #print('B')
    return( c("D", which(hand_playable(own_hand_info, table) == 0)[1]) )
  } else if (clock > 0 & TRUE %in% (others_known_playable == 3 & others_unknown_playable == 1)) {
    #print('C')
    reveal_index <- which((others_known_playable == 3 & 
                                        others_unknown_playable == 1) == TRUE)[1]
    reveal_player <- hands$player[reveal_index]
    if(is.na(others_info$number[reveal_index])){
      reveal_categ <- "N"
      reveal_info <- hands$number[reveal_index]
    } else { 
      reveal_categ <- "C"
      reveal_info <- hands$color[reveal_index]
    }
    reveal_player_index <- sum( hands$player[1:reveal_index] == reveal_player)
    return( c("R", reveal_player, reveal_categ, reveal_info) )
    
  } else {
    return( c("D", 1))
  }
}

strategy2 <- function(active_player, clock, fuse, table, discard, hands, info, 
                      last_round, info_hist, action_hist) {
  own_hand_info <- info %>% filter(player == active_player) %>% select(color, number)
  others_unknown_playable <- hand_playable(hands[,1:2],table)
  others_info <- info %>% filter(player != active_player)
  others_known_playable <- hand_playable(others_info[,1:2],table)
  if (1 %in% hand_playable(own_hand_info, table) ){
    #print('A')
    return( c("P", which(hand_playable(own_hand_info, table) == 1)[1]) )
  } else if (0 %in% hand_playable(own_hand_info, table) && clock < 8) {
    #print('B')
    return( c("D", which(hand_playable(own_hand_info, table) == 0)[1]) )
  } else if (clock > 0 & TRUE %in% (others_known_playable == 3 & others_unknown_playable == 1)) {
    #print('C')
    reveal_index <- which((others_known_playable == 3 & 
                             others_unknown_playable == 1) == TRUE)[1]
    reveal_player <- hands$player[reveal_index]
    if(is.na(others_info$number[reveal_index])){
      reveal_categ <- "N"
      reveal_info <- hands$number[reveal_index]
    } else { 
      reveal_categ <- "C"
      reveal_info <- hands$color[reveal_index]
    }
    reveal_player_index <- sum( hands$player[1:reveal_index] == reveal_player)
    return( c("R", reveal_player, reveal_categ, reveal_info) )
    
  } else {
    return( c("D", 5))
  }
}


strategy7 <- function(active_player, clock, fuse, table, discard, hands, info, 
                      last_round, info_hist, action_hist) {
  own_hand_info <- info %>% filter(player == active_player) %>% select(color, number)
  others_unknown_playable <- hand_playable(hands[,1:2],table)
  others_info <- info %>% filter(player != active_player)
  others_known_playable <- hand_playable(others_info[,1:2],table)
  if (1 %in% hand_playable(own_hand_info, table) ){
    #print('A')
    return( c("P", which(hand_playable(own_hand_info, table) == 1)[1]) )
  } else if (0 %in% hand_playable(own_hand_info, table) && clock < 8) {
    #print('B')
    return( c("D", which(hand_playable(own_hand_info, table) == 0)[1]) )
  } else if ( sum( hand_playable(own_hand_info, table) == 3 & 
                   !is.na(own_hand_info[,2])) > 0 & fuse > 1  ){
    return( c("P", which(hand_playable(own_hand_info, table) == 3 & 
                !is.na(own_hand_info[,2]))[1]) )
  } else if (clock > 0 & TRUE %in% (others_known_playable == 3 & others_unknown_playable == 1)) {
    #print('C')
    reveal_index <- which((others_known_playable == 3 & 
                             others_unknown_playable == 1) == TRUE)[1]
    reveal_player <- hands$player[reveal_index]
    if(is.na(others_info$number[reveal_index])){
      reveal_categ <- "N"
      reveal_info <- hands$number[reveal_index]
    } else { 
      reveal_categ <- "C"
      reveal_info <- hands$color[reveal_index]
    }
    reveal_player_index <- sum( hands$player[1:reveal_index] == reveal_player)
    return( c("R", reveal_player, reveal_categ, reveal_info) )
    
  } else {
    return( c("D", 5))
  }
}

# Idea behind strategy 8_
#If another player has a playable card such that you can reveal info
#about only that card, then do so.
#If info is revealed about only one of your cards, play that card.
strategy8 <- function(active_player, clock, fuse, table, discard, hands, info, 
                      last_round, info_hist, action_hist) {
  own_hand_info <- info %>% filter(player == active_player) %>% select(color, number)
  others_unknown_playable <- hand_playable(hands[,1:2],table)
  others_info <- info %>% filter(player != active_player)
  others_known_playable <- hand_playable(others_info[,1:2],table)
  # unique = 1 if that color or number is unique in that players' hand.
  # = 0 if not
  unique <- hands %>% mutate( col_pl = paste(color, player) )%>% 
    mutate(num_pl = paste(number, player) ) %>%
    mutate(color = as.integer( !col_pl %in% col_pl[duplicated(col_pl)] )) %>%
    mutate(number = as.integer( !num_pl %in% num_pl[duplicated(num_pl)] )) %>%
    select(color, number, player)
  print(unique)
  if (1 %in% hand_playable(own_hand_info, table) ){
    #print('A')
    return( c("P", which(hand_playable(own_hand_info, table) == 1)[1]) )
  } else if (0 %in% hand_playable(own_hand_info, table) && clock < 8) {
    #print('B')
    return( c("D", which(hand_playable(own_hand_info, table) == 0)[1]) )
  } else if (clock > 0 & TRUE %in% (others_known_playable == 3 & others_unknown_playable == 1)) {
    #print('C')
    reveal_index <- which((others_known_playable == 3 & 
                             others_unknown_playable == 1) == TRUE)[1]
    reveal_player <- hands$player[reveal_index]
    if(is.na(others_info$number[reveal_index])){
      reveal_categ <- "N"
      reveal_info <- hands$number[reveal_index]
    } else { 
      reveal_categ <- "C"
      reveal_info <- hands$color[reveal_index]
    }
    reveal_player_index <- sum( hands$player[1:reveal_index] == reveal_player)
    return( c("R", reveal_player, reveal_categ, reveal_info) )
    
  } else {
    return( c("D", 5))
  }
}


# EXAMPLE

table <- data.frame( W = 3, R = 1, Y = 1, G = 0, B = 2)

hands <- data.frame(color =
  c("Y", "W", "R", "W", "B",
    "B", "B", "G", "Y", "W"),
  number =
  c(1, 3, 5, 1, 2,
    4, 2, 2, 1, 3),
  player = rep(c(1,2), each = 5) )

info <- data.frame(color =
  c(NA, NA, NA, "W", "B",
    NA, NA, NA, NA, "W",
    NA, "W", "W", "B", NA),
  number =
  c(1, 3, NA, 1, NA,
    4, 2, NA, 1, NA,
    1, 5, 5, NA, NA),
  player = rep(c(1,2,3), each = 5) )

#strategy8(3, 5,4,table,0,hands,info,0)

