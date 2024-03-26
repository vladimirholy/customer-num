
# Constraints of the Optimization Problem

funWeights <- function(cur, x, r) {
  
  constr <- sum(cur) - 1
  
  der <- rep(1, length(cur))
  
  return(list(constraints = constr, jacobian = der))
  
}

# Objective Function of the Maximum Likelihood (ML) Estimation

funObjectiveLikelihood <- function(cur, x, r) {
  
  q <- c(cur, 1 - sum(cur))
  p <- x / sum(x)
  
  obj <- suppressWarnings(x %*% log(r %*% q))
  obj <- -obj
  
  der <- t(x / as.vector(r %*% q)) %*% r
  der <- -der
  der <- der[-length(der)] - der[length(der)]
  
  return(list(objective = obj, gradient = der))
  
}

# Objective Function of the Least Squares (LS) Estimation

funObjectiveSquares <- function(cur, x, r) {
  
  q <- c(cur, 1 - sum(cur))
  p <- x / sum(x)
  
  obj <- sum((p - r %*% q)^2)
  
  der <- -2 * t(p - r %*% q) %*% r
  der <- der[-length(der)] - der[length(der)]
  
  return(list(objective = obj, gradient = der))
  
}

# Estimation of the Size and Composition of Customer Base

customerDist <- function(x, r, f, init, method = 'ML', opti) {
  
  n <- nrow(r)
  m <- ncol(r)
  
  a <- sum(x)
  p <- x / a
  
  if (method == 'ML') {
    
    q_init <- init[1:(m - 1)] / sum(init)
    q_low <- rep(0, m - 1)
    q_high <- rep(1, m - 1)
    
    res <- suppressWarnings(nloptr(x0 = q_init, eval_f = funObjectiveLikelihood, lb = q_low, ub = q_high, eval_g_ineq = funWeights, opts = opti, x = x, r = r))
    sol <- res$solution
    
    q <- c(sol, 1 - sum(sol))
    
  } else if (method == 'LS') {
    
    q_init <- init[1:(m - 1)] / sum(init)
    q_low <- rep(0, m - 1)
    q_high <- rep(1, m - 1)
    
    res <- suppressWarnings(nloptr(x0 = q_init, eval_f = funObjectiveSquares, lb = q_low, ub = q_high, eval_g_ineq = funWeights, opts = opti, x = x, r = r))
    sol <- res$solution
    
    q <- c(sol, 1 - sum(sol))

  }
  
  v <- q * a / f
  
  d <- sum(v)
  u <- v / d
  
  return(list(q = q, u = u, d = d))
  
}
