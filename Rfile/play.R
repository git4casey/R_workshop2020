# Get symbol function
get_symbols <- function(){
  wheel <- c("DD","7","BBB","BB","B","0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06 ,0.1, 0.25, 0.53))
}
symbols <- get_symbols()


# Get prize function
score <- function(symbols){
  # identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B","BB","BBB")
  # get prize
  if(same){
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
  } else if(all(bars)){
    prize <- 5
  } else {
    prize <- 0
  }
  # return prize
  #prize <- paste0("You win $", prize)
  return(prize)
  
}

prize <- NULL
for(i in 1:100){
  symbols <- get_symbols()
  prize[i] <- score(symbols)
}
