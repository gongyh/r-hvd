#' @importFrom hht HilbertTransform
#' @importFrom pracma cumtrapz
synchdem <- function(x, omega_i, fp) {
  # Synchronous demodulation of the component "xi" with the frequency 
  # vector "omega_i" from the initial composition x.
  # fp - Lowpass filter cutoff frequency (0.005>fp>0.1)
  
  x <- as.vector(x)
  om <- as.vector(omega_i)
  xH <- hilbfir(x)  # Hilbert transform via FIR filter
  A2 <- (xH^2 + x^2)
  A <- sqrt(A2)
  #A <- abs(hht::HilbertTransform(x))
  Am <- mean(A[200:(length(x) - 200)])  # A, amplitude
  cs <- pracma::cumtrapz(om)
  xc <- Am * cos(cs)
  xs <- Am * sin(cs)
  x1 <- x * xc
  x2 <- x * xs
  x3 <- xH * xc
  x4 <- xH * xs
  Acos <- (x1 + x4) / Am
  Asin <- (x3 - x2) / Am
  
  AcosM <- lpf(Acos, fp)
  AsinM <- lpf(Asin, fp)
  Ai <- sqrt(abs((AcosM)^2 + (AsinM)^2))
  phi <- atan2(AsinM, AcosM)  # phase shift correction  
  xi <- Ai * cos(pracma::cumtrapz(om) + phi)
  
  return(list(xi = xi, Ai = Ai, phi = phi))
}
