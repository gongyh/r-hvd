
lpf <- function(x, fp) {
  # A length N=231 Remez lowpass filter with filtering procedure
  # for long length data > (N-1)*3+1
  # fp – Filter cutoff frequency (0.01>fp>0.5)
  N <- 231 - 1
  h <- signal::remez(N, c(0,0.001,fp,0.5)*2, c(1,1,0,0), ftype="bandpass") # filter frequency response
  xf <- signal::filtfilt(h, x)
  
  return(xf)
}

# set.seed(123) # for reproducibility
# x <- 3 + rnorm(700)
# xf <- lpf(x, 0.02)
# N <- 231 - 1
# fp <- 0.02
# h <- remez(N, c(0,0.001,fp,0.5)*2, c(1,1,0,0), ftype="bandpass")
# 
# # Filter spectral characteristics
# H <- freqz(h)
# Freq <- H$f
# 
# par(mfrow=c(1, 2))
# # Initial and filtered signal
# plot(Freq, abs(H$h), type='b', pch=20, xlim=c(0, 0.05), ylim=c(0.99, 1.01),
#      main="Frequency Response", xlab='Frequency', ylab='|FRF|')
# 
# plot(Freq, abs(H$h), type='b', pch=20, xlim=c(0.8, 1), ylim=c(0, 0.001),
#      main="Frequency Response", xlab='Frequency', ylab='|FRF|')
# 
# # Compare signal before and after filtering
# par(mfrow=c(1, 1))
# plot(x, type='o', col='black', main="Signal Comparison", xlab='Sample', ylab='Amplitude', pch=20)
# grid()
# lines(xf, col='red')
# legend("topright", legend=c("before", "after filtering"), col=c("black", "red"), lty=1)
# 
