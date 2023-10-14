# update Strategies_Sur_Profits
# assign NaN to non-applicable Hands
Which_NaN_1=which(apply(Strategies_Volumes[, -1],
                        1,
                        sum)==0)
Which_NaN_2=sapply(Strategies_Counts$Hand,
                   function(x){
                     Value_Calculator(unlist(strsplit(x, ",")))
                   })>Min_Value
Which_NaN_3=Which_NaN_2|sapply(Strategies_Counts$Hand,
                               function(x){!is.pair(unlist(strsplit(x, ",")))})


Strategies_H_Profits[Which_NaN_2, c(2:11)]=NaN
Strategies_S_Profits[Which_NaN_1, c(2:11)]=NaN
Strategies_D_Profits[Which_NaN_2, c(2:11)]=NaN
Strategies_SP_Profits[Which_NaN_3, c(2:11)]=NaN
Strategies_Sur_Profits[Which_NaN_1, c(2:11)]=NaN

# Best_Strategies
Best_Strategies=as.data.table(
  cbind(
    Hand=Strategies_Counts$Hand,
    do.call(
      rbind,
      lapply(
        Strategies_Counts$Hand,
        function(x){
          mapply(
            function(a, b, c, d){
              Which_Strategy=which.max(c(a, b, c, d))
              if(length(Which_Strategy)==0){
                "None"
              }else{
                switch(Which_Strategy,
                       "1"={"H"},
                       "2"={"S"},
                       "3"={"D"},
                       "4"={"SP"}
                       # "5"={"Sur"}
                )
              }
            },
            # decide by expected profits 
            Strategies_H_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11],
            Strategies_S_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11],
            Strategies_D_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11],
            Strategies_SP_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11]
            # Strategies_Sur_Profits[Hand==x, 2:11]
            )
        }
      )
    )
  )
)

# Expected_Profits
Expected_Profits=as.data.table(
  cbind(
    Hand=Strategies_Counts$Hand,
    do.call(
      rbind,
      lapply(
        Strategies_Counts$Hand,
        function(x){
          mapply(
            function(a, b, c, d){
              Which_Strategy=which.max(c(a, b, c, d))
              if(length(Which_Strategy)==0){
                "None"
              }else{
                c(a, b, c, d)[Which_Strategy]
              }
            },
            Strategies_H_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11],
            Strategies_S_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11],
            Strategies_D_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11],
            Strategies_SP_Profits[Hand==x, 2:11]/Strategies_Volumes[Hand==x, 2:11]
            # Strategies_Sur_Profits[Hand==x, 2:11]
            )
        }
      )
    )
  )
)
Expected_Profits[,
                 (Cols):=lapply(.SD, as.numeric),
                 .SDcols=Cols]

# Weighted_Profits
Weighted_Profits=as.data.table(
  cbind(
    Hand=Strategies_Counts$Hand,
    do.call(
      rbind,
      lapply(
        Strategies_Counts$Hand,
        function(x){
          mapply(
            function(a, b, c, d){
              Which_Strategy=which.max(c(a, b, c, d))
              if(length(Which_Strategy)==0){
                "None"
              }else{
                c(a, b, c, d)[Which_Strategy]
              }
            },
            Strategies_H_Profits[Hand==x, 2:11]*Strategies_Volumes[Hand==x, 2:11]/sum(Strategies_Volumes[, 2:11]),
            Strategies_S_Profits[Hand==x, 2:11]*Strategies_Volumes[Hand==x, 2:11]/sum(Strategies_Volumes[, 2:11]),
            Strategies_D_Profits[Hand==x, 2:11]*Strategies_Volumes[Hand==x, 2:11]/sum(Strategies_Volumes[, 2:11]),
            Strategies_SP_Profits[Hand==x, 2:11]*Strategies_Volumes[Hand==x, 2:11]/sum(Strategies_Volumes[, 2:11])
            # Strategies_Sur_Profits[Hand==x, 2:11]
            )
        }
      )
    )
  )
)
Weighted_Profits[,
                 (Cols):=lapply(.SD, as.numeric),
                 .SDcols=Cols]


# Winning_Rates
Winning_Rates=cbind(
  Hand=Strategies_Counts$Hand,
  Strategies_Wins[, 2:11]/Strategies_Volumes[, 2:11]
)



