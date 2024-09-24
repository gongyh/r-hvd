library(signal)

lpf <- function(x, fp) {
  # A length N=231 Remez lowpass filter with filtering procedure
  # for long length data > (N-1)*3+1
  # fp – Filter cutoff frequency (0.01>fp>0.5)
  N <- 231 - 1
  h <- firpm(N, c(0, 0.001, fp, 0.5) * 2, c(1, 1, 0, 0), "l") # filter frequency response
  xf <- filtfilt(h, 1, x)
  
  return(xf)
}

set.seed(123) # for reproducibility
x <- 3 + rnorm(700)
xf <- lpf(x, 0.02)
N <- 231 - 1
fp <- 0.02
h <- firpm(N, c(0, 0.001, fp, 0.5) * 2, c(1, 1, 0, 0), "l")

# Filter spectral characteristics
H <- freqz(h)
F <- H$freq

par(mfrow=c(1, 2))
# Initial and filtered signal
plot(F, abs(H$h), type='p', pch=20, main="Frequency Response", xlab='Frequency', ylab='|FRF|')
axis(1, at=c(0, 0.05), labels=c(0, 0.05))
axis(2, at=c(0.99, 1.01), labels=c(0.99, 1.01))

plot(F, abs(H$h), type='p', pch=20, main="Frequency Response", xlab='Frequency', ylab='|FRF|')
axis(1, at=c(0.8, 1), labels=c(0.8, 1))
axis(2, at=c(0, 0.001), labels=c(0, 0.001))

# Compare signal before and after filtering
plot(x, type='o', col='black', main="Signal Comparison", xlab='Sample', ylab='Amplitude', pch=20)
grid()
lines(xf, col='red')
legend("topright", legend=c("before", "after filtering"), col=c("black", "red"), lty=1)

