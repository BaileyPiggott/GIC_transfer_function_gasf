# function to generate spectral estimate of data

spec_est <- function(data, nw, k){
  # generally, nw = 4, k = 7
  
  N <- length(data)
  
  # get spectral estimate -------
  # this is for zero-padding - you can change the 3 to whatever you want, but 2 or 3 is usually fine
  nFFT <- 2^(floor(log2(N)) + 3) 
  
  # generate the slepians (the $v will just return the matrix)
  slep <- dpss(n=N, k=k, nw=nw, returnEigenvalues=FALSE)$v
  
  # dummy container to hold the tapered data + extra zeros
  y_k.tmp <- matrix(0, nrow=nFFT, ncol=k)
  
  # multiplies each column of your slepian matrix by the data
  y_k.tmp[1:N, ] <- apply(slep, 2, "*", data)
  
  # This will be a matrix with k columns and nFFT rows - each column is one eigencoefficient
  y_k <- mvfft(y_k.tmp)
  
  return(y_k)
  
  
}