hilbfir <- function(x) {
  x <- as.vector(x)
  # A length N=231 Remez Hilbert transformer with filtering procedure
  # for long length data > (N-1)*3+1
  # fp – Filter cutoff frequency (0.01>fp>0.5)
  N <- 231 - 1
  fp <- 0.01
  h <- firpm(N, c(fp, 0.5 - fp) * 2, c(1, 1), type = "Hilbert")
  
  xH0 <- filter(h, 1, x)
  
  # 90 degree – phase forward and reverse digital filtering initial signal and Hilbert projection
  n <- ceiling((N + 1) / 2)
  l <- length(x)
  a <- 1
  b <- h
  xH <- filter(b, a, x)
  xH <- xH[n:(l - n - 1)]
  xb <- x[l:1][(l - 3 * N + 1):l]
  xHb <- filter(b, a, xb)
  xHb <- xHb[(3 * N):(n)]
  xH <- c(xH, xHb[(3 * N + 2 - 3 * n):(3 * N - n + 1)])
  
  return(xH)
}

N <- 231 - 1
fp <- 0.01
h <- firpm(N, c(fp, 0.5 - fp) * 2, c(0.9998, 0.9999), type = "Hilbert")

# Filter spectral characteristics
library(signal)
H <- freqz(h)
F <- seq(0, 1, length.out = length(H))

par(mfrow = c(1, 2))
plot(F / pi, abs(H), type = 'p', pch = 20, main = "Frequency Response", xlab = "Frequency", ylab = "|FRF|", ylim = c(0.995, 1.005), xlim = c(0, 0.1))
plot(F / pi, abs(H), type = 'p', pch = 20, main = "Frequency Response", xlab = "Frequency", ylab = "|FRF|", ylim = c(0.995, 1.005), xlim = c(0.9, 1))

# Compare signal before and after filtering
x <- sin(0.1 * (1:1024))
xH <- hilbfir(x)
plot(x, type = 'l', col = 'blue', lty = 2, main = "Signal Comparison", ylab = "Amplitude", xlab = "Sample Index")
lines(xH, col = 'black')
legend("topright", legend = c("before", "after filtering"), col = c("blue", "black"), lty = c(2, 1))
