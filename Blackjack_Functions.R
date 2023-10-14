is.blackjack=function(Hand){
  if(length(Hand)==2){
    return("A"%in%Hand & "10"%in%Hand)
  }else{
    FALSE
  }
}

is.pair=function(Hand){
  if(length(Hand)==2){
    return(Hand[1]==Hand[2])
  }else{
    FALSE
  }
}

is.A=function(Hand){
  return("A"%in%Hand)
}

Value_Calculator=function(Hand){
  if(!is.A(Hand)){
    Value=sum(as.numeric(Hand))
  }else if(is.A(Hand)){
    Values=sum(
      as.numeric(Hand[Hand!="A"]))+
      apply(
        do.call(
          expand.grid,
          rep(list(c(1, 11)),
              times=sum(Hand=="A"))
        ),
        1,
        sum
      )
    
    if(sum(Values<=21)==0){
      Value=min(Values[Values>21])  # too many (return the minimum value if larger than 21)
    }else{
      Value=max(Values[Values<=21]) # A is always considered 11
    }
    
  }
  
  return(Value)
}

Result_Determinator=function(My_Value,
                             Dealer_Value){
  
  if(My_Value>21){
    Result="Bust"
  }else{
    if(Dealer_Value>21){
      Result="Win"
    }else{
      if(My_Value>Dealer_Value){
        Result="Win"
      }else if(My_Value==Dealer_Value){
        Result="Draw"
      }else if(My_Value<Dealer_Value){
        Result="Lose"
      }
    }
  }
  
  return(Result)
}

Dealer_Draw=function(Hand,
                     Deck_Pile){
  
  while(Value_Calculator(Hand)<17){
    Hand=c(Hand,
           Deck_Pile[1])
    Deck_Pile=Deck_Pile[-1]
  }
  return(Hand)
}

Hit=function(Hand,
             Deck_Pile){
  Deck_Pile_Temp<<-Deck_Pile[-1]
  
  return(
    c(Hand,
      Deck_Pile[1])
  )
}

Stand=function(Hand,
               Deck_Pile){
  Deck_Pile_Temp<<-Deck_Pile
  
  return(
    Hand
  )
}

Double_Down=function(Hand,
                     Deck_Pile){
  Deck_Pile_Temp<<-Deck_Pile[-1]
  
  return(
    c(Hand,
      Deck_Pile[1])
  )
}

Split=function(Hand,
               Deck_Pile){
  Deck_Pile_Temp<<-Deck_Pile[-c(1, 2)]
  if(is.pair(Hand)){
    return(
      list(c(Hand[1], Deck_Pile[1]),
           c(Hand[2], Deck_Pile[2]))
    ) 
  }
}

Result_Calculator=function(Hand,
                           Dealer_Hand,
                           Deck_Pile,
                           Strategy){
  
  Dealer_Hand=Dealer_Draw(Dealer_Hand,
                          Deck_Pile)
  
  My_Value=Value_Calculator(Hand)
  Dealer_Value=Value_Calculator(Dealer_Hand)
  
  Result=Result_Determinator(My_Value=My_Value,
                             Dealer_Value=Dealer_Value)
  
  return(
    list(
      Dealer_Hand=Dealer_Hand,
      Result=Result
    )
  )
}

