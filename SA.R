setwd("C:\\Users\\Republic of Gamers\\Desktop\\Fax\\3L\\AAHPS\\DN5")

fill <- function(solution) {
  visit <- integer(N)
  
  for (id in solution)
    for (i in S[[id]])
      visit[i] <- 1
  
  idx <- c()
  for (k in 1 : K)
    if (!is.element(k, solution))
        idx <- c(idx, k)
  
  idx <- sample(idx)
  for (k in idx) {
    contribute <- 0
    
    for (u in S[[k]])
      if (visit[u] == 0)
        contribute <- 1
      
    if (contribute == 1) {
      solution <- c(solution, k)
      
      for (u in S[[k]])
        visit[u] <- 1
    }
  }

  return (solution)
}

generate <- function(solution) {
  visit <- integer(K)
  
  for (k in solution)
    visit[k] <- 1
  
  it <- 1
  ret <- list()
  for (i in 1 : K) {
    new_sol <- solution
    
    if (visit[i] == 1)
      new_sol <- setdiff(new_sol, i)
    
    if (visit[i] == 0)
      new_sol <- c(new_sol, i)
    
    new_sol <- fill(new_sol)
    
    ret[[it]] <- new_sol
    it <- it + 1
  }
  
  return (ret)
}

quality <- function(solution) {
  count <- integer(N)

  for (id in solution)
    for (u in S[[id]])
        count[u] <- count[u] + 1
  
  ret <- 0
  for (i in 1 : N)
    if (count[i] > 0)
        ret <- ret + count[i] * prices[i] * 1.05 ** (count[i] - 1)
  
  return (ret)
}

REGULAR <- F
FILE_NAME <- "Problem10.txt"

line <- read.csv(FILE_NAME, nrows = 1, header = F)
N <<- length(line)
prices <<- integer(N)
total_price <- 0

for (i in 1 : N) {
  prices[i] <- line[[i]]
  total_price <- total_price + prices[i]
}

K <<- 0
S <<- list()
con <- file(FILE_NAME, open = "r")
line <- readLines(con, n = 1, warn = F)

while (length(line <- readLines(con, n=  1, warn = F)) > 0) {
  line <- strsplit(line, ", ")[[1]]
  
  K <- K + 1
  n_el <- length(line)
  S[[K]] <- integer(n_el)
  
  for (j in 1 : n_el)
    S[[K]][j] <- as.numeric(line[j])
}

close(con)

cur_solution <<- sample(1 : K, sample(1 : K, 1), rep = F)
cur_solution <<- fill(cur_solution)

T <- 100; dt = 0.999
best_solution <- cur_solution
q_best <- quality(best_solution)
while (T > 1) {
  neighbours <- generate(cur_solution)
  next_solution <- sample(neighbours, 1)[[1]]
  rand_samp <- next_solution
  
  if (!REGULAR) {
    best_next_sol <- c()
    q_best_next <- 1e15
    
    for (sol in neighbours) {
      q_sol <- quality(sol)
      
      if (q_sol < q_best_next) {
        q_best_next <- q_sol
        best_next_sol <- sol
      }
    }
    
    next_solution <- best_next_sol
  }

  q_cur <- quality(cur_solution)
  q_next <- quality(next_solution)
  
  if (q_next < q_best) {
    q_best <- q_next
    best_solution <- next_solution
  }
  
  if (q_next < q_cur) {
    cur_solution <- next_solution
  } else {
    q_rand <- quality(rand_samp)
    dif <- (q_rand - q_cur) / total_price
    prob <- round(100 * exp(-dif / T))
    
    if (sample(1 : 100, 1) < prob)
      cur_solution <- rand_samp
    
    T <- T * dt
  }
}

print(q_best)
print(sort(best_solution))
