library(dplyr)
library(readr)

# This function creates the states for markov simulation based on the number of outs in an inning, and the number of runners on base. The function results in a dataframe with two columns of states before and after the play
states_full_df <- function(bevent_data){ # The states_full_df function takes an input of bevent data file. States are generated before and after the play specific to the observation
  before_play_state <- with(bevent_data, paste(outs,' ', # Prior to the play, what is the state in which we are in
                                               ifelse(is.na(first.runner),0,1), # If there is a runner on first, denote 1. Otherwise 0
                                               ifelse(is.na(second.runner),0,1), # If ther is a runner on second, denote 1. Otherwise 0
                                               ifelse(is.na(third.runner),0,1), sep='')) # If ther is a runner on third, denote 1. Otherwise 0
  
  after_play_state <- with(bevent_data, # after_play_state creates a list of states following the result of the play
                           paste(outs+outs.on.play,' ', # Totals the number of outs 
                                 as.numeric(runner.first.dest ==1 | batter.dest==1), # Inputs the resulting player or nonexistant player on FIRST base after the play
                                 as.numeric(runner.first.dest==2 | runner.second.dest==2 | batter.dest==2), # Inputs the resulting player or nonexistant player on SECOND base after the play
                                 as.numeric(runner.first.dest==3 | runner.second.dest==3 |runner.third.dest==3 | batter.dest==3), sep='')) # Inputs the resulting player or nonexistant player on THIRD base after the play
  
  after_play_state <- ifelse(substr(after_play_state,1,1)=='3', '3 000', after_play_state) # If the play results in a third out. The inning is over and the '3 000' state is created
  return(cbind(before_play_state,after_play_state))
}

# This function takes an input of state and outputs an empty matrix to fill
empty_matrix <- function(state){ # The function takes a state input
  m <- as.data.frame(matrix(as.numeric(0), nrow=25, ncol=25)) # We create a matrix that is 25 x 25
  names(m) <- state # For purposes accessing states in results, we rename the columns of m to match state
  row.names(m) <- state # For purposes accessing states in results, we rename the columns of m to match state
  return(m) # Return resulting df
}


# The prepare_bevent function takes an input of a file name specific to a team, generates states for the team with regard to performance, and returns these states.
prepare_bevent <- function(bevent_file){
  path <- paste(dir_path, bevent_file, sep = '')
  bevent <- read_csv(path, col_names = FALSE) # Based on file name, import file into R and define as bevent
  names(bevent) <- bevent_headers$headers # Define headers in terms of the bevent_headers file
  bevent$home.team <- substr(bevent$game.id,1,3) # The home team in the observations are defined by the first three characters in the game.id
  df <- states_full_df(bevent) # Run the states_full_df function to get df of states for before and after each play
  bevent <- cbind(bevent, df) # Add the resulting states generated to the bevent df
  return(bevent) # return bevent
}

# The team bevent file takes an input of a prepared bevent from the prepare_bevent. The function subsets the variables to only those of teams
team_bevent<-function(bevent, team){ # The team_bevent function takes an input of a bevent file and team in string form
  print(bevent$after_play_state[1:10])
  print(team)
  if(team!='ALL' & !team %in% bevent$home.team){ # If team is incorrectly called, mark error
    stop('Error') # Mark error
  }
  else if(team!='ALL'){ # If a specific team is called, get subset of their games
    bevent = bevent[(bevent$home.team ==team & bevent$batting.team==1)| (bevent$away.team==team & bevent$batting.team==0),] # Subset data to hold to only called team
  }
  return(bevent) # return bevent
}

# Based on a unique id that identifies each player, this function creates a list of dataframes of players inputed
player_bevent<-function(bevent, players){ # The function takes an input of a bevent file and a list of players
  player_bevents <- list() # Create a list titled player bevents
  for(i in players){ # For loop to loop over players in list
    player_bevents[[length(player_bevents)+1]] <- subset(bevent, res.batter==i) # define list elements for when the inputed players are on offense at the plate
  }
  names(player_bevents) <- players # Name the items after the inputed players
  return(player_bevents) # return new list
}


#Returns the transition matricies for the specified team based on events
team_transition_matrix <- function(bevent, states){ # This function takes an input of a bevent file and the states of a game
  t <- empty_matrix(states) # Define t using the empty matrix function
  t_non <- empty_matrix(states) # Define t_non using the empty matrix function
  if(nrow(bevent)>0){ # If the bevent is larger than 0
    for(i in 1:(nrow(bevent))){ # For loop 
      if ( i %% 1000 == 0){
        print(i)
      }
      if(bevent[i,'after_play_state']=="3 000" & bevent[i,'inning']==9 & bevent[i,'batting.team']==1){
        
        t[as.character(bevent$before_play_state[i]), as.character(bevent$after_play_state[i])] = t[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])]# If the the game ends, do nothing
        
      } # If the the game ends, do nothing
      
      else if(bevent$batter.event.flag[i]==FALSE){ # If there is an out as a result of a play other than an offensive at bat
        
        t_non[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])] = t_non[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])] +1 # Normal matrix if a runner advances
      }else{ 
        t[as.character(bevent$before_play_state[i]), as.character(bevent$after_play_state[i])] = t[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])] + 1 # 
      }
    }
  }
  temp_t <- t+t_non # Create temp_t by adding t and t_non together
  t <- t/rowSums(temp_t) #Calculate batter event probabilities
  t_non <- t_non/rowSums(temp_t) #Calculate batter event probabilities
  t_non[is.na(t_non)]<- 0 # Covert na to 0
  t[is.na(t)] <- 0 # Convert na to 0
  return(list(t,t_non))
}


# This function takes an input of player bevents, player_ids and states. The function produces transition matricies for the list of players
player_transition_matrix <- function(player_bevents, players, states){ # The inputs of this function are player bevents, player ids and states
  player_t_list = list() # Create a new lest
  for(id in players){ # For loop to loop over players in list
    t <- empty_matrix(states) # Define t using the empty matrix function
    t_non <- empty_matrix(states) # Define t_non using the empty matrix function
    bevent <- player_bevents[[id]] # define bevent to df in player_bevent list
    if(nrow(bevent)>0){ # if there are more than zero observations in bevent
      for(i in 1:(nrow(bevent))){
        if(bevent[i,'after_play_state']=="3 000" & bevent[i,'inning']== 9 & bevent[i,'batting.team']==1){ 
          t[as.character(bevent$before_play_state[i]), as.character(bevent$after_play_state[i])] = t[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])]# If the the game ends, do nothing
        }#If there is a non-batter event (aka SB, wild pitch, pick off) we to the nonBAT transition matrix
        else if(bevent$batter.event.flag[i]==FALSE){ # If there is an out as a result of a play other than an offensive at bat
          t_non[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])] = t_non[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])] +1 # Normal matrix if a runner advances
        }
        else{
          t[as.character(bevent$before_play_state[i]), as.character(bevent$after_play_state[i])] = t[as.character(bevent$before_play_state[i]),as.character(bevent$after_play_state[i])] + 1 #  
        }
      }
    }
    temp_t= t + t_non # Define temp_t as a sum of the t and t_non matrices
    t = t/rowSums(temp_t) # Calculate probabilities
    t[is.na(t)]=0 # Convert nas to 0
    t_non = t_non/rowSums(temp_t) # Calculate probabilities
    t_non[is.na(t_non)]=0 # convert nas to 0
    player_t_list[[length(player_t_list)+1]]=list(t, t_non) # Append t, t_non to list
  }
  names(player_t_list)=players # Define names of items in list as players
  return(player_t_list) # Return list
}


#Returns transition matricies for the MLB, a specific team, or a vector of players
state_transition_matrix <- function(year, team, players, bevent){
  if(team!='ALL' & length(players)!=0){ # If mistake is made in the inputs
    stop('Error') # report error
  }
  
  
  
  states <- c('0 000','0 100', '0 010', '0 001', '0 110', '0 101', '0 011', '0 111',
              '1 000','1 100', '1 010', '1 001', '1 110', '1 101', '1 011', '1 111',
              '2 000','2 100', '2 010', '2 001', '2 110', '2 101', '2 011', '2 111', '3 000') # define states in a 9 inning baseball game
  
  if(length(players)==0){ # If players are not inputed, use the current team
    bevents <- team_bevent(bevent, team) # Use the team bevent function to create transition matricies 
    t_matrix <- team_transition_matrix(bevents, states) # Define t matrix with team_transition_matrix function
    return(t_matrix) # Return matrix
  }else{ # If players are inputed
    bevents <- player_bevent(bevent, players) # Define player bevents using the player_bevent function
    player_t_list <- player_transition_matrix(bevents, players, states) # Aggregate list of t matricies based on players 
    return(player_t_list) # Return list
  } 
}

# Events in the t_non matrix are rare. This function normalizes probablities based on all teams
normalize_non <- function(season, non_season, trans_matrix){ # The inputs to this function are the seasons t and non t average matricies and a trans_matrix 
  scalars <- ifelse(rowSums(trans_matrix)==0,0,rowSums(season)/rowSums(trans_matrix)) # Define scalers based on probabilities
  matrix_norm <- trans_matrix*scalars # Define normalized matrix
  return(list(matrix_norm,non_season)) # Return list containing normalized matricies
}


# The lineup function takes inputs of players, seasons, and the year to normailze and produced a lineup with the needed matricies for markov simulation
lineup<-function(players, seasons, normalized_year, n_matricies, bevent){ # The inputs of this function are a list of players, seasons, and the year in which to normalize t_non
  states <- c('0 000','0 100', '0 010', '0 001', '0 110', '0 101', '0 011', '0 111',
              '1 000','1 100', '1 010', '1 001', '1 110', '1 101', '1 011', '1 111',
              '2 000','2 100', '2 010', '2 001', '2 110', '2 101', '2 011', '2 111', '3 000') # Define states
  matrix_list <- list() # define new list
  years <- unique(seasons) # define years
  batting_order <- vector() # create a batting order vector 
  for(year in years){ # For loop to get data on players by defined year
    players_ = players[which(seasons==year)] # define players by season
    current_year <- state_transition_matrix(year, "ALL", players, bevent) # Run state_transition_matrix function for current year data 
    batting_order <- c(batting_order, names(current_year))
    for(i in 1:length(current_year)){ # For loop over the current year players
      miss_states <- states[which(rowSums(current_year[[i]][[1]])[-25]==0)]  
      if(length(miss_states)>0){ # Define MLB avg if states are missing
        #warning(paste(names(current_year)[i], 'does not have PA in states:', paste(miss_states, collapse=' & '), '. If you do not replace this player,', 'league average will be used for these state transition probabilities'))
        current_year[[i]][[1]][miss_states,] = n_matricies[[1]][miss_states,] # Missing states are now defined
      }
      matrix_list[[length(matrix_list)+1]] = normalize_non(n_matricies[[1]], n_matricies[[2]], current_year[[i]][[1]])
    }
  }
  names(matrix_list)<- batting_order # Define the batting order of the list
  return(matrix_list[players]) #return in the order the names are listed
}


simulate_game <- function(lineup_matricies, StateRuns){
  states = c('0 000','0 100', '0 010', '0 001', '0 110', '0 101', '0 011', '0 111',
             '1 000','1 100', '1 010', '1 001', '1 110', '1 101', '1 011', '1 111',
             '2 000','2 100', '2 010', '2 001', '2 110', '2 101', '2 011', '2 111', '3 000')
  
  
  
  #innitialize inning, runs, batter number
  inning = 1
  runs = 0
  end_game = FALSE
  batter = 1
  while(!end_game){
    state = '0 000'
    current_half =TRUE
    while(current_half){
      bat_event = lineup_matricies[[batter]][[1]]
      non_bat_event = lineup_matricies[[batter]][[2]]
      Unif = runif(1)
      #if the random number is greater than the sum of bat event prob, then it is a non bat event
      if(Unif>sum(bat_event[state,])){
        #transition probabilities given current state
        transitions = as.matrix(non_bat_event[state,])
        Unif = Unif - sum(bat_event[state,])
        for(t in 1:25){
          if(cumsum(transitions)[t] >= Unif){
            new_state= states[t]
            break
          }
        }
        BAT = FALSE
        #otherwise it is a bat event
      }else{
        #transition probabilities given current state
        transitions = as.matrix(bat_event[state,])
        for(t in 1:25){
          if(cumsum(transitions)[t] >= Unif){
            new_state= states[t]
            break
          }
        }
        BAT = TRUE
      }
      #if 3 outs we end the inning
      if(new_state == '3 000'){
        current_half=FALSE
      }else{
        #if bat event, then add the runs scored starting in current state and ending in new_state
        #if non bat event, then add runs scored starting in current state and ending in new_state minus 1 (batter can't score)
        if(BAT) runs = runs + StateRuns[state, new_state] else runs = runs + max(StateRuns[state,new_state]-1,0)
        #new_state is now the current state
        state = new_state
        #if bat event, we go to the next batter
        if(BAT){
          if(batter==9) batter=1 else batter=batter+1
        }
      }
    }
    if(inning==9){
      end_game=TRUE 
      #If inning is 8, we advance to 9th inning 65% of the time (Average of 8.65 innings per game in the Wild Card era)
    }else if(inning==8){
      U = runif(1)
      if(U<.65) inning = inning + 1 else end_game=TRUE
    }else{inning = inning+1}
  }
  return(runs)
}

simulate_season <- function(data, year, n_matricies){
  file <- paste('all', year,'.csv', sep='') # define file being searched for
  bevent <- prepare_bevent(file) # Ulilize the prepare bevents function on file
  
  states = c('0 000','0 100', '0 010', '0 001', '0 110', '0 101', '0 011', '0 111',
             '1 000','1 100', '1 010', '1 001', '1 110', '1 101', '1 011', '1 111',
             '2 000','2 100', '2 010', '2 001', '2 110', '2 101', '2 011', '2 111', '3 000')
  
  StateRuns <- read.csv("~/da-401/StateRuns.csv", header = F)
  
  
  names(StateRuns)=states[-25]
  row.names(StateRuns)=states[-25]
  
  for(i in 1:nrow(data)){
    print("game")
    print(i)
    home.lineup <- lineup(players=c(data$home.batter.1[i],
                                    data$home.batter.2[i],
                                    data$home.batter.3[i],
                                    data$home.batter.4[i],
                                    data$home.batter.5[i],
                                    data$home.batter.6[i],
                                    data$home.batter.7[i],
                                    data$home.batter.8[i],
                                    data$home.batter.9[i]),
                          seasons=rep(year,9),
                          normalized_year=year,
                          n_matricies = n_matricies,
                          bevent = bevent)
    away.lineup <- lineup(players=c(data$visitor.batter.1[i],
                                    data$visitor.batter.2[i],
                                    data$visitor.batter.3[i],
                                    data$visitor.batter.4[i],
                                    data$visitor.batter.5[i],
                                    data$visitor.batter.6[i],
                                    data$visitor.batter.7[i],
                                    data$visitor.batter.8[i],
                                    data$visitor.batter.9[i]),
                          seasons=rep(year,9),
                          normalized_year=year,
                          n_matricies = n_matricies,
                          bevent = bevent)
    
    for(j in 1:100){
      sim.away.score <- simulate_game(away.lineup, StateRuns)
      
      sim.home.score <- simulate_game(home.lineup, StateRuns)
      
      data$sim.home.win[i] <- ifelse(sim.home.score > sim.away.score, data$sim.home.win[i]+1, data$sim.home.win[i])
    }
    
    data$sim.home.win[i] <- ifelse(data$sim.home.win[i] >= 50, 1, 0)
    data$TP[i] <- ifelse(data$sim.home.win[i] == data$act.home.win[i] & data$act.home.win[i]==1, 1, 0)
    data$TN[i] <- ifelse(data$sim.home.win[i] == data$act.home.win[i] & data$act.home.win[i]==0, 1, 0)
    data$FP[i] <- ifelse(data$sim.home.win[i] != data$act.home.win[i] & data$act.home.win[i]==0 , 1, 0)
    data$FN[i] <- ifelse(data$sim.home.win[i] != data$act.home.win[i] & data$act.home.win[i]==1, 1, 0)
  }
  
  
  return(data)
}
############
bevent_headers <- read_csv("~/da-401/bevent-fields.csv")
dir_path <- "da-401/bevent-output/"
n_matricies <- state_transition_matrix(2015, 'ALL', vector()) # Calculate the normalized matricies based on the defined year 


bgame <- read_csv('~/da-401/bgame-output/all2015.csv')
bgame$sim.home.score <- 0
bgame$sim.away.score <- 0
bgame$TP <- NA
bgame$TN <- NA
bgame$FP <- NA
bgame$FN <- NA
bgame$sim.home.win <- 0
bgame$act.home.win <- ifelse(bgame$home.final.score > bgame$visitor.final.score, 1, 0)
bgame_2015_sub <- bgame[99:100,]

start_time <- Sys.time()
bgame_2015<- simulate_season(bgame, 2015, n_matricies)
end_time <- Sys.time()
end_time - start_time
write.csv(bgame_2015_sub, file = "~/da-401/predict2015.csv")


ari2015 <- read_csv("~/da-401/bevent-output/2015ARI_bevent.txt")
names(ari2015) <- bevent_headers$headers 
c <- ari2015 %>%
  filter(res.batter == "hudsd001")

start_time <- Sys.time()
test <- read_csv("~/da-401/bevent-output/all2015.csv", col_names = F)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
test2 <- read.csv("~/da-401/bevent-output/all2015.csv", header = F)
end_time <- Sys.time()
end_time - start_time

names(test) <- bevent_headers$headers
names(test2) <- bevent_headers$headers
summary(test)
summary(test2)

if (is.na(test$second.runner[5])){
  print('yes')
}

sum(bgame_2015$TP)
sum(bgame_2015$TN)
sum(bgame_2015$FP)
sum(bgame_2015$FN)

(sum(bgame_2015$TP) + sum(bgame_2015$TN)) / 162
sum(bgame_2015_sub$TP) / (sum(bgame_2015_sub$TP)+sum(bgame_2015_sub$FN))