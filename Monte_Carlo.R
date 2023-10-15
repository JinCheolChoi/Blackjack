run_ind=0
while(TRUE){
  if(Simulation==TRUE){
    if(Update_Best_Strategies==TRUE){
      run_ind=run_ind+1
      if(run_ind%%500==0){
        # # refresh all aggregate quantities
        # Strategies_Counts=copy(Strategies_Temp)
        # Strategies_Volumes=copy(Strategies_Temp)
        # Strategies_Wins=copy(Strategies_Temp)
        # Strategies_Draws=copy(Strategies_Temp)
        # Strategies_Loses=copy(Strategies_Temp)
        # Strategies_Busts=copy(Strategies_Temp)
        # Strategies_H_Profits=copy(Strategies_Temp)
        # Strategies_S_Profits=copy(Strategies_Temp)
        # Strategies_D_Profits=copy(Strategies_Temp)
        # Strategies_SP_Profits=copy(Strategies_Temp)
        source(paste0(dir.path, "Aggregator.R"))
      }
    }
  }
  
  # reshuffle the deck if there is less than 30% cards remaining
  if(length(Deck_Pile)<N_Decks*52*0.2){
    Deck_Pile=rep(
      # a deck of 52 cards
      rep(c("A", 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10), # the last three are J, Q, and, K
          times=4),
      times=N_Decks
    )
    # distribute cards
    Deck_Pile=Deck_Pile[sample(1:length(Deck_Pile))]
  }
  
  # cards on hand
  My_Hand=Deck_Pile[c(1, N_Players+2)]
  Dealer_Hand=Deck_Pile[c(N_Players+1, 2*c(N_Players+1))]
  Revealed_Dealer_Hand=Dealer_Hand[1]
  
  # remaining cards on the pile
  Deck_Pile=Deck_Pile[-c(1:(2*c(N_Players+1)))]
  
  #*********************
  # implement strategies
  #*********************
  # H : Hit
  # S : Stand
  # D : Double-Down
  # SP : Split
  # Sur : Surrender (optional)
  
  # insurance
  if(Revealed_Dealer_Hand=="A"){
    if(is.blackjack(Dealer_Hand)){
      Insurance_Profits=c(Insurance_Profits,
                          Betting) # Betting/2*2
    }else{
      Insurance_Profits=c(Insurance_Profits,
                          -Betting/2)
    }
  }
  
  switch(as.character(is.blackjack(My_Hand)),
         "TRUE"={
           Rows=c("A,10")
           # scenario #1
           # if both my hand and dealer's hand are blackjack
           if(is.blackjack(Dealer_Hand)){
             Profit_Temp=0
             Win_Temp=0
             Draw_Temp=1
           }else if(!is.blackjack(Dealer_Hand)){
             # scenario #2
             # if only my hand is blackjack
             Profit_Temp=Betting*1.5
             Win_Temp=1
             Draw_Temp=0
           }
           Strategies_Counts[Hand%in%Rows,
                             (Revealed_Dealer_Hand):=lapply(.SD,
                                                            function(y){
                                                              sum(as.numeric(y), 1, na.rm=TRUE)
                                                            }),
                             .SDcols=c(Revealed_Dealer_Hand)]
           Strategies_Volumes[Hand%in%Rows,
                              (Revealed_Dealer_Hand):=lapply(.SD,
                                                             function(y){
                                                               sum(as.numeric(y), 1, na.rm=TRUE)
                                                             }),
                              .SDcols=c(Revealed_Dealer_Hand)]
           
           Strategies_Wins[Hand%in%Rows,
                           (Revealed_Dealer_Hand):=lapply(.SD,
                                                          function(y){
                                                            sum(as.numeric(y), Win_Temp, na.rm=TRUE)
                                                          }),
                           .SDcols=c(Revealed_Dealer_Hand)]
           Strategies_Draws[Hand%in%Rows,
                            (Revealed_Dealer_Hand):=lapply(.SD,
                                                           function(y){
                                                             sum(as.numeric(y), Draw_Temp, na.rm=TRUE)
                                                           }),
                            .SDcols=c(Revealed_Dealer_Hand)]
           # No Strategies_Loses
           # No Strategies_Busts
           
           Strategies_S_Profits[Hand%in%Rows,
                                (Revealed_Dealer_Hand):=lapply(.SD,
                                                               function(y){
                                                                 sum(as.numeric(y), Profit_Temp, na.rm=TRUE)
                                                               }),
                                .SDcols=c(Revealed_Dealer_Hand)]
           
           # Simulation
           if(Simulation){
             if(Win_Temp==1 & Draw_Temp==0){
               Result_Temp="Win"
             }else if(Win_Temp==0 & Draw_Temp==1){
               Result_Temp="Draw"
             }else{
               Result_Temp=""
             }
             Results=data.table(
               Branch="O",
               Depth="0",
               Strategy="Origin",
               Pre_Hand=paste(sort(My_Hand, decreasing=TRUE), collapse=","),
               Post_Hand=paste(sort(My_Hand, decreasing=TRUE), collapse=","),
               Deck_Pile=list(Deck_Pile),
               Pre_Dealer_Hand=paste(sort(Dealer_Hand, decreasing=TRUE), collapse=","),
               Dealer_Hand=paste(sort(Dealer_Hand, decreasing=TRUE), collapse=","),
               Result=Result_Temp,
               Profit=Profit_Temp,
               Revealed_Dealer_Hand=Revealed_Dealer_Hand,
               Row="A,10"
             )
             
             Betting_Results=rbind(Betting_Results,
                                   Results)
           }
           next
         })
  
  All_Bust="No"
  depth=0
  Possible_Moves=c()
  # My_Hand=c("3", "3")
  while(All_Bust=="No"){
    if(depth==0){
      Possible_Moves=rbind(
        Possible_Moves,
        
        # Origin
        data.table(
          Branch="O",
          Depth=depth,
          Strategy="Origin",
          Pre_Hand=list(My_Hand),
          Post_Hand=list(My_Hand),
          Deck_Pile=list(Deck_Pile)
        )
      )
    }
    if(depth>=1){
      for(ind in 1:length(Which_Expandables)){
        Branch_Temp=Possible_Moves_Temp[Which_Expandables[ind], Branch]
        My_Hand_Temp=unlist(Possible_Moves_Temp[Which_Expandables[ind], Post_Hand])
        Current_Full_Deck_Pile=Possible_Moves_Temp[Which_Expandables[ind], Deck_Pile][[1]]
        Possible_Moves=rbind(
          Possible_Moves,
          
          # Stand
          data.table(
            Branch=paste(Branch_Temp, "S", sep="-"),
            Depth=depth,
            Strategy="Stand",
            Pre_Hand=list(My_Hand_Temp),
            Post_Hand=list(Stand(My_Hand_Temp,
                                 Deck_Pile=Current_Full_Deck_Pile)),
            Deck_Pile=list(Deck_Pile_Temp)
          ),
          
          # Hit
          data.table(
            Branch=paste(Branch_Temp, "H", sep="-"),
            Depth=depth,
            Strategy="Hit",
            Pre_Hand=list(My_Hand_Temp),
            Post_Hand=list(Hit(My_Hand_Temp,
                               Deck_Pile=Current_Full_Deck_Pile)),
            Deck_Pile=list(Deck_Pile_Temp)
          ),
          fill=TRUE
        )
        
        # Double_Down
        # Double_Down cannot be done after hitting!
        if(Possible_Moves_Temp[ind, Strategy]%in%c("Split", "Origin")){
          Possible_Moves=rbind(
            Possible_Moves,
            data.table(
              Branch=paste(Branch_Temp, "D", sep="-"),
              Depth=depth,
              Strategy="Double_Down",
              Pre_Hand=list(My_Hand_Temp),
              Post_Hand=list(Double_Down(My_Hand_Temp,
                                         Deck_Pile=Current_Full_Deck_Pile)),
              Deck_Pile=list(Deck_Pile_Temp)
            ),
            fill=TRUE
          )
        }
        
        # Split
        if(is.pair(My_Hand_Temp)){
          if("A"%in%My_Hand_Temp){
            # A pair of As is allowed to get only one more card
            # so, let's distinguish it in Strategy
            Strategy_Name="Split_A"
          }else{
            Strategy_Name="Split"
          }
          Possible_Moves=rbind(
            Possible_Moves,
            data.table(
              Branch=paste(Branch_Temp, paste0("SP", 1:2), sep="-"),
              Depth=depth,
              Strategy=Strategy_Name,
              Pre_Hand=list(My_Hand_Temp),
              Post_Hand=Split(My_Hand_Temp,
                              Deck_Pile=Current_Full_Deck_Pile),
              Deck_Pile=list(Deck_Pile_Temp)
            ),
            fill=TRUE
          )
        }
      }
    }
    
    # Simulation
    # decide the next move
    if(Simulation){
      if(depth==0){
        Possible_Moves_To_Remove_Branches=NULL
      }else if(depth>=1){
        Branches_To_Keep=Possible_Moves[Depth==depth-1, ]
        for(Branches_ind in 1:nrow(Branches_To_Keep)){
          Branch_Temp=Branches_To_Keep[Branches_ind, Branch]
          Next_Move_Temp=Branches_To_Keep[Branches_ind, Next_Move]
          Possible_Moves_To_Remove_Branches=Possible_Moves[grepl(paste0(Branch_Temp, "-"),
                                                                 substr(Branch, 0, nchar(Branch_Temp)+1),
                                                                 fixed=TRUE) &
                                                             Strategy!=Next_Move_Temp, Branch]
          
          Possible_Moves=Possible_Moves[!Branch%in%Possible_Moves_To_Remove_Branches, ]
        }
        
      }
    }
    
    # impose constraints for the next move
    # (1) A card can be dealt only once after Double_Down, Stand, and Split_A
    Possible_Moves_Temp=Possible_Moves[Depth==depth &
                                         Strategy!="Double_Down" &
                                         Strategy!="Stand" &
                                         Strategy!="Split_A", ]
    
    
    # (2) only proceed with further strategies if hand is equal to or smaller than a certain value
    Which_Expandables=which(sapply(Possible_Moves_Temp[, Post_Hand],
                                   Value_Calculator)<=Min_Value)
    
    if(length(Which_Expandables)==0){
      All_Bust="Yes"
      next
    }else{
      # Simulation
      if(Simulation){
        Current_Hand=Possible_Moves_Temp[Depth==depth, Post_Hand]
        for(Hand_ind in 1:length(Current_Hand)){
          Branch_Temp=Possible_Moves_Temp[Hand_ind, Branch]
          Current_Hand_Temp=Current_Hand[[Hand_ind]]
          if(length(Current_Hand_Temp)==2 &
             is.pair(Current_Hand_Temp)){
            Row=paste0(sort(Current_Hand_Temp, decreasing=TRUE), collapse=",")
          }else if(length(Current_Hand_Temp)==2 &
                   is.A(Current_Hand_Temp)){
            Row=paste0(sort(Current_Hand_Temp, decreasing=TRUE), collapse=",")
          }else if(length(Current_Hand_Temp)==2 &
                   !is.pair(Current_Hand_Temp) &
                   !is.A(Current_Hand_Temp)){
            Row=Value_Calculator(Current_Hand_Temp)
          }else if(length(Current_Hand_Temp)>2){
            Row=Value_Calculator(Current_Hand_Temp)
          }
          Next_Move_Temp=as.character(Best_Strategies[Hand==Row,
                                                      .SD,
                                                      .SDcols=which(colnames(Best_Strategies)==Revealed_Dealer_Hand)])
          
          if(Row==c("A,A") & Next_Move_Temp=="SP"){
            Next_Move_Temp="Split_A"
          }else if(Next_Move_Temp=="SP"){
            Next_Move_Temp="Split"
          }else if(Next_Move_Temp=="S"){
            Next_Move_Temp="Stand"
          }else if(Next_Move_Temp=="H"){
            Next_Move_Temp="Hit"
          }else if(Next_Move_Temp=="D"){
            Next_Move_Temp="Double_Down"
          }
          
          Possible_Moves[
            Branch==Branch_Temp,
            Next_Move:=Next_Move_Temp
          ]
        }
        
      }
    }
    
    depth=depth+1
    
  }
  
  # Simulation
  if("Next_Move"%in%colnames(Possible_Moves)){
    Possible_Moves[, Next_Move:=NULL]
  }
  
  Possible_Moves[Strategy=="Split_A",
                 Strategy:="Split"]
  Possible_Moves[, Pre_Dealer_Hand:=rep(list(Dealer_Hand),
                                        nrow(Possible_Moves))]
  
  Results_Original=cbind(
    Possible_Moves,
    do.call(
      rbind,
      apply(Possible_Moves,
            1,
            function(x){
              Result_Calculator(Hand=x$Post_Hand,
                                Dealer_Hand=x$Pre_Dealer_Hand,
                                Deck_Pile=x$Deck_Pile,
                                Strategy=x$Strategy)
            })
    ),
    Revealed_Dealer_Hand
  )
  
  Results_Original[,
                   Row:=sapply(Results_Original[, Pre_Hand],
                               function(x){
                                 if(length(x)==2 &
                                    is.pair(x)){
                                   Row=paste0(sort(x, decreasing=TRUE), collapse=",")
                                 }else if(length(x)==2 &
                                          is.A(x)){
                                   Row=paste0(sort(x, decreasing=TRUE), collapse=",")
                                 }else if(length(x)==2 &
                                          !is.pair(x) &
                                          !is.A(x)){
                                   Row=Value_Calculator(x)
                                 }else if(length(x)>2){
                                   Row=Value_Calculator(x)
                                 }
                                 Row
                               })
  ]
  
  Results=copy(Results_Original)
  
  # Simulation
  # record simulation result
  if(Simulation){
    # find the last nodes if not Split
    Which_Last_Nodes=c()
    for(Result_ind in 1:nrow(Results)){
      Branch_Temp=Results[Result_ind, Branch]
      Results_Temp=Results[grepl(paste0(Branch_Temp, "-"),
                                 substr(Branch, 0, nchar(Branch_Temp)+1),
                                 fixed=TRUE), ]
      if(nrow(Results_Temp)==0){
        Which_Last_Nodes=c(Which_Last_Nodes, Result_ind)
      }
    }
    Results=Results[Which_Last_Nodes, ]
    
    
    Results[Strategy%in%c("Origin",
                          "Stand",
                          "Hit",
                          "Split") &
              Result=="Win", Profit:=Betting]
    Results[Strategy%in%c("Origin",
                          "Stand",
                          "Hit",
                          "Split") &
              Result=="Draw", Profit:=0]
    Results[Strategy%in%c("Origin",
                          "Stand",
                          "Hit",
                          "Split") &
              Result=="Lose", Profit:=-Betting]
    Results[Strategy%in%c("Origin",
                          "Stand",
                          "Hit",
                          "Split") &
              Result=="Bust", Profit:=-Betting]
    
    Results[Strategy%in%c("Double_Down") &
              Result=="Win", Profit:=2*Betting]
    Results[Strategy%in%c("Double_Down") &
              Result=="Draw", Profit:=0]
    Results[Strategy%in%c("Double_Down") &
              Result=="Lose", Profit:=-2*Betting]
    Results[Strategy%in%c("Double_Down") &
              Result=="Bust", Profit:=-2*Betting]
    
    Results[Strategy%in%c("Surrender"), Profit:=-0.5*Betting]
    
    Betting_Results=rbind(Betting_Results,
                          Results)
    
    # if(nrow(Betting_Results[is.na(Profit),])>0){
    #   stop()
    # }
    
    # Results_Original will be used to record aggregate information
    if("Split"%in%Results_Original$Strategy){
      Results=copy(Results_Original)
    }
  }
  
  # remove the first row if no strategy is implemented to the initial hand
  # basically, we just stand on the initial hand
  if(nrow(Results)==1){
    Results[Depth==0,
            `:=`(Branch="O-S",
                 Depth=1,
                 Strategy="Stand")]
  }else{
    Results=Results[Depth!=0]
  }
  Results[, is.A:=lapply(.SD, function(x){lapply(x, is.A)}), .SDcols="Pre_Hand"]
  Results[, is.pair:=lapply(.SD, function(x){lapply(x, is.pair)}), .SDcols="Pre_Hand"]
  
  # if(sum(unlist(lapply(Results$Row, is.null)))>0){
  #   print(Results[unlist(lapply(Results$Row, is.null)), ])
  # }
  
  #*****************
  # Calculate_Profit
  # Stand
  Results[Strategy=="Stand" &
            Result=="Win", `:=`(Profit=Betting,
                                Volume=1,
                                Win=1,
                                Draw=0,
                                Lose=0,
                                Bust=0)]
  Results[Strategy=="Stand" &
            Result=="Draw", `:=`(Profit=0,
                                 Volume=1,
                                 Win=0,
                                 Draw=1,
                                 Lose=0,
                                 Bust=0)]
  Results[Strategy=="Stand" &
            Result=="Lose", `:=`(Profit=-Betting,
                                 Volume=1,
                                 Win=0,
                                 Draw=0,
                                 Lose=1,
                                 Bust=0)]
  Results[Strategy=="Stand" &
            Result=="Bust", `:=`(Profit=-Betting,
                                 Volume=1,
                                 Win=0,
                                 Draw=0,
                                 Lose=0,
                                 Bust=1)]
  
  # Double_Down
  Results[Strategy=="Double_Down" &
            Result=="Win", `:=`(Profit=2*Betting,
                                Volume=1,
                                Win=1,
                                Draw=0,
                                Lose=0,
                                Bust=0)]
  Results[Strategy=="Double_Down" &
            Result=="Draw", `:=`(Profit=0,
                                 Volume=1,
                                 Win=0,
                                 Draw=1,
                                 Lose=0,
                                 Bust=0)]
  Results[Strategy=="Double_Down" &
            Result=="Lose", `:=`(Profit=-2*Betting,
                                 Volume=1,
                                 Win=0,
                                 Draw=0,
                                 Lose=1,
                                 Bust=0)]
  Results[Strategy=="Double_Down" &
            Result=="Bust", `:=`(Profit=-2*Betting,
                                 Volume=1,
                                 Win=0,
                                 Draw=0,
                                 Lose=0,
                                 Bust=1)]
  
  # Hit & Split
  Results[Strategy=="Hit" &
            Result=="Bust",
          `:=`(Profit=-Betting,
               Volume=1,
               Win=0,
               Draw=0,
               Lose=0,
               Bust=1)]
  
  Results[Strategy=="Split" &
            is.pair==TRUE &
            is.A==TRUE &
            Result=="Win", `:=`(Profit=Betting,
                                Volume=1,
                                Win=1,
                                Draw=0,
                                Lose=0,
                                Bust=0)]
  Results[Strategy=="Split" &
            is.pair==TRUE &
            is.A==TRUE &
            Result=="Draw", `:=`(Profit=0,
                                 Volume=1,
                                 Win=0,
                                 Draw=1,
                                 Lose=0,
                                 Bust=0)]
  Results[Strategy=="Split" &
            is.pair==TRUE &
            is.A==TRUE &
            Result=="Lose", `:=`(Profit=-Betting,
                                 Volume=1,
                                 Win=0,
                                 Draw=0,
                                 Lose=1,
                                 Bust=0)]
  Results[Strategy=="Split" &
            is.pair==TRUE &
            is.A==TRUE &
            Result=="Bust", `:=`(Profit=-Betting,
                                 Volume=1,
                                 Win=0,
                                 Draw=0,
                                 Lose=0,
                                 Bust=1)]
  
  # Branches
  Branches=Results[Strategy%in%c("Hit", "Split"),
                   Branch]
  
  # Aggregate informative quantities (i.e., Profit, Volume, Win, Draw, Lose, and Bust) belonging to branches
  if(length(Branches)>=1){
    for(ind in length(Branches):1){
      Branch_Temp=Branches[ind]
      Results_Temp=Results[grepl(Branch_Temp,
                                 substr(Branch, 0, nchar(Branch_Temp)),
                                 fixed=TRUE)]
      if(nrow(Results_Temp)>1){
        Target_Dept=Results_Temp[Branch==Branch_Temp, Depth]+1
        Results_Temp=Results_Temp[Branch!=Branch_Temp &
                                    Depth==Target_Dept, ]
        Profit_Temp=Results_Temp[grepl(Branch_Temp,
                                       substr(Branch, 0, nchar(Branch_Temp)),
                                       fixed=TRUE)][, sum(Profit, na.rm=TRUE)]
        Volume_Temp=Results_Temp[grepl(Branch_Temp,
                                       substr(Branch, 0, nchar(Branch_Temp)),
                                       fixed=TRUE)][, sum(Volume, na.rm=TRUE)]
        Win_Temp=Results_Temp[grepl(Branch_Temp,
                                    substr(Branch, 0, nchar(Branch_Temp)),
                                    fixed=TRUE)][, sum(Win, na.rm=TRUE)]
        Draw_Temp=Results_Temp[grepl(Branch_Temp,
                                     substr(Branch, 0, nchar(Branch_Temp)),
                                     fixed=TRUE)][, sum(Draw, na.rm=TRUE)]
        Lose_Temp=Results_Temp[grepl(Branch_Temp,
                                     substr(Branch, 0, nchar(Branch_Temp)),
                                     fixed=TRUE)][, sum(Lose, na.rm=TRUE)]
        Bust_Temp=Results_Temp[grepl(Branch_Temp,
                                     substr(Branch, 0, nchar(Branch_Temp)),
                                     fixed=TRUE)][, sum(Bust, na.rm=TRUE)]
        Results[Branch==Branch_Temp, `:=`(Profit=Profit_Temp,
                                          Volume=Volume_Temp,
                                          Win=Win_Temp,
                                          Draw=Draw_Temp,
                                          Lose=Lose_Temp,
                                          Bust=Bust_Temp)]
      }
      if(nrow(Results_Temp)==1){
        switch(Results_Temp[, Result][[1]],
               "Win"={Profit_Temp=Betting;Win_Temp=1;Draw_Temp=0;Lose_Temp=0;Bust_Temp=0},
               "Draw"={Profit_Temp=0;Win_Temp=0;Draw_Temp=1;Lose_Temp=0;Bust_Temp=0},
               "Lose"={Profit_Temp=-Betting;Win_Temp=0;Draw_Temp=0;Lose_Temp=1;Bust_Temp=0},
               "Bust"={Profit_Temp=-Betting;Win_Temp=0;Draw_Temp=0;Lose_Temp=0;Bust_Temp=1})
        Volume_Temp=1
        Results[Branch==Branch_Temp, `:=`(Profit=Profit_Temp,
                                          Volume=Volume_Temp,
                                          Win=Win_Temp,
                                          Draw=Draw_Temp,
                                          Lose=Lose_Temp,
                                          Bust=Bust_Temp)]
      }
    }
  }
  
  invisible(
    apply(
      Results,
      1,
      function(x){
        Strategies_Counts[Hand%in%x$Row,
                          (Revealed_Dealer_Hand):=lapply(.SD,
                                                         function(y){
                                                           sum(as.numeric(y), 1, na.rm=TRUE)
                                                         }),
                          .SDcols=c(Revealed_Dealer_Hand)]
        Strategies_Volumes[Hand%in%x$Row,
                           (Revealed_Dealer_Hand):=lapply(.SD,
                                                          function(y){
                                                            sum(as.numeric(y), x$Volume, na.rm=TRUE)
                                                          }),
                           .SDcols=c(Revealed_Dealer_Hand)]
        Strategies_Wins[Hand%in%x$Row,
                        (Revealed_Dealer_Hand):=lapply(.SD,
                                                       function(y){
                                                         sum(as.numeric(y), x$Win, na.rm=TRUE)
                                                       }),
                        .SDcols=c(Revealed_Dealer_Hand)]
        Strategies_Draws[Hand%in%x$Row,
                         (Revealed_Dealer_Hand):=lapply(.SD,
                                                        function(y){
                                                          sum(as.numeric(y), x$Draw, na.rm=TRUE)
                                                        }),
                         .SDcols=c(Revealed_Dealer_Hand)]
        Strategies_Loses[Hand%in%x$Row,
                         (Revealed_Dealer_Hand):=lapply(.SD,
                                                        function(y){
                                                          sum(as.numeric(y), x$Lose, na.rm=TRUE)
                                                        }),
                         .SDcols=c(Revealed_Dealer_Hand)]
        Strategies_Busts[Hand%in%x$Row,
                         (Revealed_Dealer_Hand):=lapply(.SD,
                                                        function(y){
                                                          sum(as.numeric(y), x$Bust, na.rm=TRUE)
                                                        }),
                         .SDcols=c(Revealed_Dealer_Hand)]
        switch(x$Strategy,
               "Hit"={Strategies_H_Profits[Hand%in%x$Row,
                                           (Revealed_Dealer_Hand):=lapply(.SD,
                                                                          function(y){
                                                                            sum(as.numeric(y), x$Profit, na.rm=TRUE)
                                                                          }),
                                           .SDcols=c(Revealed_Dealer_Hand)]},
               "Stand"={Strategies_S_Profits[Hand%in%x$Row,
                                             (Revealed_Dealer_Hand):=lapply(.SD,
                                                                            function(y){
                                                                              sum(as.numeric(y), x$Profit, na.rm=TRUE)
                                                                            }),
                                             .SDcols=c(Revealed_Dealer_Hand)]},
               "Double_Down"={Strategies_D_Profits[Hand%in%x$Row,
                                                   (Revealed_Dealer_Hand):=lapply(.SD,
                                                                                  function(y){
                                                                                    sum(as.numeric(y), x$Profit, na.rm=TRUE)
                                                                                  }),
                                                   .SDcols=c(Revealed_Dealer_Hand)]},
               "Split"={Strategies_SP_Profits[Hand%in%x$Row,
                                              (Revealed_Dealer_Hand):=lapply(.SD,
                                                                             function(y){
                                                                               sum(as.numeric(y), x$Profit, na.rm=TRUE)
                                                                             }),
                                              .SDcols=c(Revealed_Dealer_Hand)]})
        
      }
    )
  )
}

# Strategies_Counts
# Strategies_Volumes
# Strategies_Wins
# Strategies_Draws
# Strategies_Loses
# Strategies_Busts
# Strategies_H_Profits
# Strategies_S_Profits
# Strategies_D_Profits
# Strategies_SP_Profits
# Strategies_Sur_Profits
# 
# Strategies_Counts[, 2:11]
# Strategies_Volumes[, 2:11]
# Strategies_Wins[, 2:11]
# Strategies_Draws[, 2:11]
# Strategies_Loses[, 2:11]
# Strategies_Busts[, 2:11]
# Strategies_H_Profits[, 2:11]
# Strategies_S_Profits[, 2:11]
# Strategies_D_Profits[, 2:11]
# Strategies_SP_Profits[, 2:11]
# Strategies_Sur_Profits[, 2:11]
