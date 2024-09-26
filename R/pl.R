#' Plot Decomposed Components and Hilbert Spectrum
#'
#' This function plots the decomposed components, their instantaneous frequencies,
#' envelopes, and the sum of components using the Hilbert spectrum.
#'
#' @param Y A matrix of decomposed components obtained from HVD.
#' @param A A matrix of component envelopes.
#' @param F_r A matrix of component relative angular frequencies.
#' @param Fs The sampling frequency.
#' @return This function does not return a value but produces plots.
#' @export
#' @examples
#' om <- 0.2 + 0.12 * cos(0.4 * (0:1023))
#' x <- cos(cumsum(om))
#' result <- hvd(x, 3, 0.02)
#' pl(result$Y, result$A, result$om_r, 2 * pi)
#'
#' @references
#' Feldman, M. (2011). HILBERT TRANSFORM APPLICATION IN MECHANICAL VIBRATION. John Wiley & Sons.
pl <- function(Y, A, F_r, Fs) {
  graphics::par(mfrow=c(1,1))
  N <- 0 # Number of the excluded points from the start/end
  s <- dim(Y)
  pp <- (N + 1):(nrow(Y) - N)
  t <- pp / Fs # Time
  dec <- round(s[1] / 150)
  
  c <- c('red', 'blue', 'green', 'purple', 'brown', 'cyan', 'gray')
  if (s[2] > 7) s[2] <- 7
  
  F <- F_r * Fs / (2 * pi) # Frequency, [Hz]
  
  for (k in 1:s[2]) {
    graphics::plot(t, Y[pp, k], type='l', col=c[k], xlab='Time, s', ylab=paste('^Y', k), main='', ylim=c(min(Y[pp, k]), max(Y[pp, k])))
    graphics::grid()
    
    graphics::plot(t, F[pp, k], type='l', col=c[k], lwd=2, xlab='Time, s', ylab='Frequency, Hz', main='', ylim=c(0, 1.2 * max(F[pp, ])))
    graphics::grid()
    
    graphics::plot(t, A[pp, k], type='l', col=c[k], lwd=2, xlab='Time, s', ylab='Amplitude', main='', ylim=c(0, 1.2 * max(A[pp, ])))
    graphics::grid()
    
    graphics::plot(t, Y[pp, k], type='l', col=c[k], xlab='Time, s', ylab='Y', main='', ylim=c(min(Y[pp, ]), max(Y[pp, ])))
    graphics::grid()
  }
  
  graphics::title(main='Components', outer=TRUE)
  graphics::title(main='Component instantaneous frequency', outer=TRUE)
  graphics::title(main='Component envelope', outer=TRUE)
  
  graphics::title(main='Components', outer=TRUE)
  graphics::plot(t, rowSums(Y[pp, , drop=FALSE]), type='l', xlab='Time, s', ylab='Y', main='Sum of components')
  graphics::grid()
  
  # rgl::plot3d(t[seq(1, length(pp), by=dec)], F[pp[seq(1, length(pp), by=dec)], ], A[pp[seq(1, length(pp), by=dec)], ], col=c[k], type='h', xlab='Time, s', ylab='Frequency, Hz', zlab='Amplitude')
  # graphics::title(main='Hilbert spectrum')
  # rgl::view3d(-50, 70)
}
