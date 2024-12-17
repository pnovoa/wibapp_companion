blest_rank_correlation <- function(rank1, rank2){
  
  N <- length(rank1)
  
  match_matrix <- matrix(c(rank1, rank2), ncol = 2)
  
  result <- apply(match_matrix, MARGIN = 1, function(mrow){
    Rxi <- mrow[1]
    Ryi <- mrow[2]
    Ryi*(N + 1 - Rxi)^2
  }
  )
  
  return(
    1 - (12*sum(result) - N*(N+1)^2*(N+2))/(N*(N+1)^2*(N-1))
  )
}


weighted_rank_measure_of_correlation <- function(rank1, rank2){
  N <- length(rank1)
  
  match_matrix <- matrix(c(rank1, rank2), ncol = 2)
  
  result <- apply(match_matrix, MARGIN = 1, function(mrow){
    Rxi <- mrow[1]
    Ryi <- mrow[2]
    
    (Rxi - Ryi)^2 * ((N - Rxi + 1) + (N - Ryi + 1))
  }
  )
  
  return(
    1 - (6 * sum(result))/(N^4 + N^3 - N^2 - N)
  )
}


ws_coefficient <- function(rank1, rank2){
  
  N <- length(rank1)
  
  match_matrix <- matrix(c(rank1, rank2), ncol = 2)
  
  result <- apply(match_matrix, MARGIN = 1, function(mrow){
    Rxi <- mrow[1]
    Ryi <- mrow[2]
    2^-(Rxi) * abs(Rxi - Ryi)/max(abs(1-Rxi), abs(N-Rxi))
  }
  )
  
  return(
    1 - sum(result)
  )
}