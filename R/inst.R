
inst <- function(x, Fs) {
  x <- as.vector(x)
  xH <- hilbfir(x)  # Hilbert transform via FIR filter
  A2 <- (xH^2 + x^2)
  A <- sqrt(A2)  # Envelope
  Xc <- x + 1i * xH  # Analytic signal
  Xd <- Xc[2:length(x)] * Conj(Xc[1:(length(x) - 1)])
  omega1 <- Arg(Xd)
  om1 <- 0.5 * (c(0, omega1) + c(omega1, 0))
  F <- Fs * om1 / (2 * pi)  # Instantaneous frequency
  ph1 <- atan2(xH, x)
  phi <- signal::unwrap(ph1)  # Instantaneous phase
  
  return(list(A = A, F = F, phi = phi))
}

# Fs <- 1
# x <- cos(0.24 * (1:1024)) + 0.9 * sin(0.22 * (1:1024))
# result <- inst(x, Fs)
# A <- result$A
# F1 <- result$F
# phi <- result$phi
# om1 <- 2 * pi * F1
# 
# # IF via analytical differentiation of the arctangent of the fraction
# xd <- diff(x) * Fs
# xdH <- hilbfir(xd)
# var2 <- (x * xdH - xd * xH)
# xH <- hilbfir(x)
# A2 <- (xH^2 + x^2)
# om2 <- var2 / A2
# 
# # IF via differentiation of the phase angle
# om3 <- diff(phi) * Fs
# 
# par(mfrow = c(3, 1))
# plot(x, main = "Signal and Envelope", type = "l")
# lines(A, col="red")
# 
# plot(phi, main = "Phase", type = "l")
# 
# 
# plot(om1, ylim = c(0.2, 0.5), main = "Three IF estimations", type = "l")
# lines(om2, col=2)
# lines(om3, col=3)
# legend("topright", legend = c("om1", "om2", "om3"), col = 1:3, lty = 1)
# 
