#********************
#
# empty the workspace
#
#********************
rm(list=ls())


#*******************
#
# set directory path
#
#*******************
dir.path="C:/Users/jchoi02/Desktop/R/Blackjack/"
# dir.path="C:/Users/JinCheol Choi/Desktop/R/Blackjack/"

#*******
#
# import
#
#*******
library(data.table)
source(paste0(dir.path, "Blackjack_Functions.R"))
Strategies=fread(paste0(dir.path, "Strategies.csv"),
                 header=T)
Strategies_Profits=copy(Strategies)
Strategies_Profits[, 2:11]=as.numeric(0)
Cols=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A")
Strategies_Profits[, (Cols):=lapply(.SD, as.numeric), .SDcols=Cols]
Strategies_Counts=copy(Strategies_Profits)
Strategies_Volumes=copy(Strategies_Profits)
Strategies_Wins=copy(Strategies_Profits)
Strategies_Draws=copy(Strategies_Profits)
Strategies_Loses=copy(Strategies_Profits)
Strategies_Busts=copy(Strategies_Profits)
Strategies_H_Profits=copy(Strategies_Profits)
Strategies_S_Profits=copy(Strategies_Profits)
Strategies_D_Profits=copy(Strategies_Profits)
Strategies_SP_Profits=copy(Strategies_Profits)
Strategies_Sur_Profits=copy(Strategies_Profits)

# Surrender profit is expected to be always -0.5*Betting
# Strategies_Sur_Profits[, 2:11]=as.numeric(-0.5*Betting)
Strategies_Sur_Profits[, 2:11]=as.numeric(-Inf) # if Sur is not allowed

#***********
#
# parameters
#
#***********
Bankroll=100
Betting=1
N_Decks=8
N_Players=4
Min_Value=15 # Min_Value<=21
Deck_Pile=rep(
  # a deck of 52 cards
  rep(c("A", 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10), # the last three are J, Q, and, K
      times=4),
  times=N_Decks
)

#
Simulation=FALSE

# distribute cards
Deck_Pile=Deck_Pile[sample(1:length(Deck_Pile))]

# insurance
Insurance_Profits=c()

# run monte carlo simulation
source(paste0(dir.path, "Monte_Carlo.R"))
sum(Strategies_Counts[, 2:11])
sum(Strategies_Volumes[, 2:11])

# put the results together
source(paste0(dir.path, "Aggregator.R"))
Best_Strategies
Expected_Profits
Winning_Rates

# fwrite(Best_Strategies,
#        paste0(dir.path, "Best_Strategies.csv"))
sum(Weighted_Profits[!Which_NaN_1, -1])/sum(Strategies_Volumes[, 2:11])

# save.image(paste0(dir.path, "Rdata/", Min_Value, ".Rdata"))
# load(paste0(dir.path, "Rdata/", Min_Value, ".Rdata"))