
mnorm <- function (X, mu, Co) {
  L <- nrow(X)
  N <- ncol(X)
  
  Mu <- matrix(rep(mu, N), L, N)
  
  coef <- 1 / sqrt((2*pi)^L * det(Co))
  expo <- exp(-0.5 * colSums((X - Mu) * (solve(Co) %*% (X - Mu))))
    
  return (matrix(coef * expo, N, 1))
}

h <- function (x, deg = 1) {
  ret_h <- matrix(1, 0, length(x))
  
  for (i in 0:deg) {
    ret_h <- rbind(ret_h, x^i)
  }
  
  return (ret_h)
}

H <- function (h, X, deg = 1) {
  return (matrix(apply(X, 2, h, deg), deg+1, ncol(X)))
}

b_dist_bef <- function (b, Sigma_b) {
  mu_b <- matrix(0, nrow(Sigma_b), 1)
    
  return (mnorm(b, mu_b, Sigma_b))
}

b_dist_aft <- function (b, Sigma_b, sigma, f, H) {
  Sigma_b_inv <- solve(Sigma_b)
  
  Co <- solve(Sigma_b_inv + (H %*% t(H)) / sigma^2)
  mu <- (Co %*% H %*% f) / sigma^2
  
  return (mnorm(b, mu, Co))
}

predict_blm <- function (x_pred, X, Y, deg, sigma, Sigma_b) {
  N_pred <- ncol(x_pred)
  Sigma_b_inv <- solve(Sigma_b)
  H_samp <- H(h, X, deg)
  H_pred <- H(h, x_pred, deg)
  
  A_inv <- solve(Sigma_b_inv + (H_samp %*% t(H_samp)) / sigma^2)
  
  m_var <- sigma^2 + colSums(H_pred * (A_inv %*% H_pred))
  # m_var <- ifelse(abs(m_var) < .Machine$double.eps, 0, m_var)
  
  m_est <- as.vector((t(H_pred) %*% A_inv %*% H_samp %*% Y) / sigma^2)
  
  return (list(est = m_est, var = m_var))
}
