#' Hilbert Vibration Decomposition
#'
#' This function performs a Hilbert Vibration Decomposition (HVD) on a signal.
#' It decomposes the input signal into `n` components and returns their amplitudes,
#' frequencies, and phase shifts.
#'
#' @param x A numeric vector representing the input signal.
#' @param n The number of components to decompose the signal into.
#' @param fp The cutoff frequency for the lowpass filter.
#' @return A list containing the following components:
#' \item{Y}{A matrix of the decomposed signal components.}
#' \item{A}{A matrix of the amplitudes of the decomposed signal components.}
#' \item{om_r}{A matrix of the angular frequencies of the decomposed signal components.}
#' \item{dev}{A vector of the relative standard deviation of the components.}
#' @export
#' @examples
#' om <- 0.2 + 0.12 * cos(0.4 * (0:1023))
#' x <- cos(cumsum(om))
#' result <- hvd(x, 3, 0.02)
#' 
#' Y <- result$Y
#' A <- result$A
#' F_r <- result$om_r
#' dev <- result$dev
#' 
#' par(mfrow = c(2, 1))
#' plot(x, type = 'l', xlim = c(400, 600), ylim = c(-1.1, 1.1), ylab = 'Initial signal')
#' plot(Y[,1], type = 'l', col=1, xlim = c(400, 600), ylim = c(-1.1, 1.1), xlab = 'Points', ylab = 'Signal Components')
#' lines(Y[,2], col = 2)
#' lines(Y[,3], col = 3)
#' 
#' library(stats)
#' plot(spec.pgram(x),log="dB")
hvd <- function(x, n, fp) {
  if (n > 7) {
    print("Max number of components not greater than 7")
  }
  if (n <= 0) {
    print("Number of components less than 1")
    return(list(Y = NULL, A = NULL, om_r = NULL, dev = NULL))
  }
  
  x <- as.vector(x)
  s <- numeric(n + 1)
  s[1] <- sd(x)
  
  if (s[1] == 0) {
    print("Zero signal")
    return(list(Y = NULL, A = NULL, om_r = NULL, dev = NULL))
  }
  
  Y <- matrix(0, nrow = length(x), ncol = n)
  A <- matrix(0, nrow = length(x), ncol = n)
  om_r <- matrix(0, nrow = length(x), ncol = n)
  
  for (k in 1:n) {
    inst_result <- inst(x, 1)
    At <- inst_result[[1]]
    Ft <- inst_result[[2]]
    phit <- inst_result[[3]]
    
    omf <- 2 * pi * lpf(Ft, fp)  # Angular Frequency lowpass filtering (Smoothing)
    synchdem_result <- synchdem(x, omf, fp)
    yi <- synchdem_result[[1]]
    Ai <- synchdem_result[[2]]
    phi <- synchdem_result[[3]]
    
    Y[, k] <- yi
    A[, k] <- Ai
    om_r[, k] <- omf
    
    x <- x - yi
    s[k] <- sd(x) / s[1]
    
    if (k == 7) {
      dev <- c(1, diff(s))
      return(list(Y = Y, A = A, om_r = om_r, dev = dev))
    }
  }
  
  dev <- s  # Relative standard deviation of the components
  return(list(Y = Y, A = A, om_r = om_r, dev = dev))
}
