test_that("hvd works", {
  # Example
  om <- 0.2 + 0.12 * cos(0.4 * (0:1023))
  x <- cos(cumsum(om))
  result <- hvd(x, 3, 0.02)
  
  Y <- result$Y
  A <- result$A
  F_r <- result$om_r
  dev <- result$dev
  
  par(mfrow = c(2, 1))
  plot(x, type = 'l', xlim = c(400, 600), ylim = c(-1.1, 1.1), 
       ylab = 'Initial signal')
  plot(Y, type = 'l', xlim = c(400, 600), ylim = c(-1.1, 1.1), 
       xlab = 'Points', ylab = 'Signal Components')
  
  library(stats)
  plot(spec.pgram(x),log="dB")
  
})
