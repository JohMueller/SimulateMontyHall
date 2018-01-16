# The Monty-Hall-Problem:
# There are three different doors and behind of them is the price. Behind the other doors is nothing.
# The gameshow participant selects on of the doors. 
# Then the gameshow host opens of the doors behind which there is nothing. Which leaves two doors unopened.
# The gameshow host then asks the participant if they want to change their guess and switch to the other door.
# The statsical right decision would be to switch as this strategy dramatically increases the chances of winning (from 1/3 to 2/3).
# As this stratgey is highly counter-intuitiv a simulation study can be used to demonstrate that it is in fact the better one.

# This is a function to simulate the Monty-Hall-Gameshow:
gameshow <- function(switch = FALSE){
  
  #Step 1: Guess
  
  gates <- c(1:3)
  answer <- sample(gates, 1, replace = TRUE)
  guess <- sample(gates, 1, replace = TRUE)
  
  #Step 2: Open First Gate
  if(guess != answer){
    removed_gates <- c(guess, answer)
  }
  if(guess == answer){
    removed_gates <- c(answer, sample(gates[-answer], 1))
  }
  
  # Step 3: If strategy is "switch" --> new guess
  if(switch == TRUE){
    guess <- removed_gates[which(removed_gates != guess)]
  }
  # Step 3: Check if correct
  return(guess == answer)
  
}


##### Simulate the Gameshow with switch or stay strategy for N iterations
simulate_gameshow<- function(switch=FALSE, N = 10000){

  gameshow_results <- 0

  for(i in 1:N){
    if(gameshow(switch) == TRUE){
    gameshow_results <- gameshow_results + 1
    }
  }

  print(paste("your win percentage was ",gameshow_results/N*100, "%" ))
}



