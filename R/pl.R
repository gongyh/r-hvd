
pl <- function(Y, A, F_r, Fs) {
  # Decomposed components and Hilbert spectrum presentation
  
  # Y – Array of decomposed components (through HVD),
  # A – Array of component envelopes,
  # F_r – Array of component relative angular frequencies
  # Fs -sampling frequency,
  # F=Fs*F_r/2/pi – Plotted absolute frequency [Hz]
  
  # Example: pl(Y,A,F_r,1)
  
  # © 2011 Michael Feldman
  # For use with the book “HILBERT TRANSFORM APPLICATION
  # IN MECHANICAL VIBRATION”, John Wiley & Sons, 2011
  
  graphics::par(mfrow=c(1,1))
  N <- 0 # Number of the excluded points from the start/end
  s <- dim(Y)
  pp <- (N + 1):(length(Y) - N)
  t <- pp / Fs # Time
  dec <- round(s[1] / 150)
  
  c <- c('k-', 'b-', 'r-', 'm-', 'g-', 'c-', 'y-')
  if (s[2] > 7) s[2] <- 7
  
  F <- F_r * Fs / (2 * pi) # Frequency, [Hz]
  
  for (k in 1:s[2]) {
    graphics::plot(t, Y[pp, k], type='l', col=c[k], xlab='Time, s', ylab=paste('^Y', k), main='', ylim=c(min(Y[pp, k]), max(Y[pp, k])))
    grid()
    
    graphics::plot(t, F[pp, k], type='l', col=c[k], lwd=2, xlab='Time, s', ylab='Frequency, Hz', main='', ylim=c(0, 1.2 * max(F[pp, ])))
    grid()
    
    graphics::plot(t, A[pp, k], type='l', col=c[k], lwd=2, xlab='Time, s', ylab='Amplitude', main='', ylim=c(0, 1.2 * max(A[pp, ])))
    grid()
    
    graphics::plot(t, Y[pp, k], type='l', col=c[k], xlab='Time, s', ylab='Y', main='', ylim=c(min(Y[pp, ]), max(Y[pp, ])))
    grid()
  }
  
  title(main='Components', outer=TRUE)
  title(main='Component instantaneous frequency', outer=TRUE)
  title(main='Component envelope', outer=TRUE)
  
  title(main='Components', outer=TRUE)
  graphics::plot(t, rowSums(Y[pp, , drop=FALSE]), type='l', xlab='Time, s', ylab='Y', main='Sum of components')
  grid()
  
  graphics::plot3d(t[seq(1, length(pp), by=dec)], F[pp[seq(1, length(pp), by=dec)], ], A[pp[seq(1, length(pp), by=dec)], ], col=c[k], type='h', xlab='Time, s', ylab='Frequency, Hz', zlab='Amplitude')
  title(main='Hilbert spectrum')
  rgl::view3d(-50, 70)
}

# # Example
# om <- 0.2 + 0.12 * cos(0.4 * (0:1023))
# x <- cos(cumsum(om))
# result <- hvd(x, 3, 0.05)
# pl(result$Y, result$A, result$F_r, 2 * pi)

