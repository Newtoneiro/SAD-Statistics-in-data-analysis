liczba_probek_lista <- c(3, 100, 1000)

analiza_estymatory <- function(liczba_probek_lista) {
  for (liczba_probek in liczba_probek_lista) {
    liczba_prob <- 100
    
    lambda <- 2
    
    X <- replicate(liczba_prob, rpois(liczba_probek, lambda))
    
    estymatory <- colMeans(X)
    
    
    Y <- sqrt(liczba_probek*1/lambda) * (estymatory - lambda)
    
    tytul_wykresu <- paste("Histogram estymatorów dla liczby probek =", liczba_probek, sep=" ")
    hist(Y, breaks=15, probability=TRUE, col="blue",
         main=tytul_wykresu, xlab="x",
         ylab="y", xlim=c(-5,5))
    curve(dnorm(x, mean=0, sd=1), col="red", lwd=2, add=TRUE)
  }
}

analiza_estymatory(liczba_probek_lista)
