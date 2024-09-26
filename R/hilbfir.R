#' @importFrom stats filter
hilbfir <- function(x) {
  x <- as.vector(x)
  # A length N=231 Remez Hilbert transformer with filtering procedure
  # for long length data > (N-1)*3+1
  # fp – Filter cutoff frequency (0.01>fp>0.5)
  N <- 231 - 1
  fp <- 0.01
  h <- gsignal::remez(N, c(fp, 0.5 - fp)*2, c(1, 1), ftype="hilbert")
  
  #xH0 <- filter(h, x)
  
  # 90 degree – phase forward and reverse digital filtering initial signal and Hilbert projection
  n <- ceiling((N + 1) / 2)
  l <- length(x)
  xH <- filter(h, x)
  xH <- xH[n:(l - n - 1)]
  xb <- x[l:(l - 3 * N)]
  xHb <- filter(h, xb)
  xHb <- xHb[(3 * N):(n)]
  xH <- c(xH, xHb[(3 * N + 2 - 3 * n):(3 * N - n + 1)])
  
  return(xH)
}

# # example
# N <- 231 - 1
# fp <- 0.01
# h <- gsignal::remez(N, c(fp, 0.5 - fp)*2, c(0.9998, 0.9999), ftype="hilbert")
# 
# # Filter spectral characteristics
# hf <- signal::freqz(h)
# H <- hf$h
# F <- hf$f
# 
# par(mfrow = c(1, 2))
# plot(F / pi, abs(H), type = 'b', pch = 20, main = "Frequency Response", xlab = "Frequency", ylab = "|FRF|", ylim = c(0.995, 1.005), xlim = c(0, 0.1))
# plot(F / pi, abs(H), type = 'b', pch = 20, main = "Frequency Response", xlab = "Frequency", ylab = "|FRF|", ylim = c(0.995, 1.005), xlim = c(0.9, 1))
# 
# # Compare signal before and after filtering
# x <- sin(0.1 * (1:1024))
# xH <- hilbfir(x)
# par(mfrow=c(1, 1))
# plot(x, type = 'l', col = 'blue', lty = 2, main = "Signal Comparison", ylab = "Amplitude", xlab = "Sample Index")
# lines(xH, col = 'black')
# legend("topright", legend = c("before", "after filtering"), col = c("blue", "black"), lty = c(2, 1))
