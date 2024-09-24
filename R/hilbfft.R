hilbfft <- function(x) {
  # A Hilbert transform (based on FFT) with filtering procedure
  x <- as.vector(x)
  xH <- Im(hht::HilbertTransform(x))
  return(xH)
}

# Compare the original data and the Hilbert transformed data on a line plot:
x <- sin(0.06 * (1:400))
xH <- hilbfft(x)

plot(x, type = 'l', lty = 3, col = 'blue', ylim = range(c(x, xH)), ylab = "Value", xlab = "Index")
grid()
lines(xH, col = 'black')
legend("topright", legend = c("Original", "Hilbert transformed"), col = c("blue", "black"), lty = c(3, 1))
