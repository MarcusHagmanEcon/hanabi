rm(list = ls(all.names = TRUE))
library(dplyr)
source('C:/Users/MA.4780/Documents/hanabi/hanabi_strategies.R')


n_players <- 3
n_hand <- 5
moves_remaining <- n_players+1
# Create deck:
color <- rep(c('W','R','G','B','Y'),each=10)
number <- rep(c(1,1,1,2,2,3,3,4,4,5),times=5)
deck <- data.frame(color,number)

playgame <- function(seed,strategy){
  set.seed(seed)
  lastround <- FALSE
  # Initialize game:
  player <- 1
  clock <- 8
  fuse <- 4
  drawpile <- deck[sample(1:nrow(deck)), ]
  rownames(drawpile) <- 1:nrow(drawpile)
  
  # Hand out cards:
  hands <- drawpile[1:(n_players*n_hand),]
  hands$player <- rep(1:n_players, each = n_hand)
  drawpile <- drawpile[(n_players*n_hand+1):nrow(drawpile),]
  
  # Initialize info set, discard pile, table
  info <- setNames(data.frame(matrix(ncol = 3, nrow = (n_players*n_hand))),c('color','number','player'))
  info$player <- rep(1:n_players, each = n_hand)
  table <- setNames(data.frame(matrix(0,ncol = 5, nrow = 1)),c('W','R','G','B','Y'))
  discard <- setNames(data.frame(matrix(ncol = 3, nrow =0)),c('color','number','player'))
  counter <- 0 
  playing = TRUE
  while (playing == TRUE) {
    action <- strategy(player, clock, fuse, table, discard, hands[hands$player != player,], info, lastround)
    if(action[1] == 'R'){         # Reveal info
      if(clock == 0){
        print('Illegal move: clock tokens are used up.')
        break
      }
      clock = clock-1
      if(action[3]=='C'){
        info$color[info$player == action[2] & hands$color == action[4]] = action[4]
      } else {
        info$number[info$player == action[2] & hands$number == action[4]] = action[4]
      }
    } else if(action[1] == 'D'){  # Discard card
      ind_disc <- strtoi(rownames(hands[hands$player==player,])[strtoi(action[2])])
      discard <- rbind(discard, hands[ind_disc,])
      hands <- hands[-ind_disc,]
      info <- info[-ind_disc,]
      if(lastround==FALSE){
        hands <- rbind(hands,c(drawpile[1,],'player' = player))
        drawpile <- drawpile[-1,]
      } else{
          hands <- rbind(hands,c(NA,NA,player))
      }
      info <- rbind(info,c(NA,NA,player))
      rownames(hands) <- 1:nrow(hands)
      rownames(info) <- 1:nrow(info)
      clock <- clock +1
    } else {                      # Play card
      ind_play <- strtoi(rownames(hands[hands$player==player,])[strtoi(action[2])])
      card <- hands[ind_play,]
      hands <- hands[-ind_play,]
      info <- info[-ind_play,]
      if(lastround==FALSE){
        hands <- rbind(hands,c(drawpile[1,],'player' = player))
        drawpile <- drawpile[-1,]
      } else{
        hands <- rbind(hands,c(NA,NA,player))
      }
      info <- rbind(info,c(NA,NA,player))
      rownames(hands) <- 1:nrow(hands)
      rownames(info) <- 1:nrow(info)
      if(table[,card$color]==card$number-1){
        table[,card$color] = table[,card$color] +1
      } else {
        fuse <- fuse-1
        discard <- rbind(discard, card)
      }
    }
    player <- player + 1
    if(player>3){
      player <- player - 3
    }
    if(nrow(drawpile)==0){
      moves_remaining <- moves_remaining-1
      lastround = TRUE
    }
    if(moves_remaining==0){
      playing = FALSE
    }
    if(nrow(drawpile)>0){
      rownames(drawpile) <- 1:nrow(drawpile)
    }
    counter <- counter+1
    #print(counter)
  }
  # points = append(points,sum(table))
  # print(points)
  return(sum(table))
}

points = data.frame("1" = c(0))
for(i in 1:10){
  points[i,1] = playgame(i,strategy8)
  #points[i,2] = playgame(i,strategy7)
  print(i)
}
