require(ggplot2)

oblicz_moc_testu <- function(liczba_probek, delta, sigma_x, sigma_y, prog = 0.05, liczba_symulacji = 10000) {
  
  liczba_istotnych_wynikow <- 0
  
  for (i in 1:liczba_symulacji) {
    
    x <- rnorm(liczba_probek, mean = 0, sd = sigma_x)
    y <- rnorm(liczba_probek, mean = delta, sd = sigma_y)
    
    wynik_testu <- t.test(x, y, var.equal = TRUE)
    
    if (wynik_testu$p.value < prog) {
      
      liczba_istotnych_wynikow <- liczba_istotnych_wynikow + 1
    
      }
    
  }
  
  moc <- liczba_istotnych_wynikow / liczba_symulacji
  return(moc)
}

oblicz_moc_testu_wilcoxona <- function(liczba_probek, delta, sigma_x, sigma_y, prog = 0.05, liczba_symulacji = 10000) {
  
  liczba_istotnych_wynikow <- 0
  
  for (i in 1:liczba_symulacji) {
    
    x <- rnorm(liczba_probek, mean = 0, sd = sigma_x)
    y <- rnorm(liczba_probek, mean = delta, sd = sigma_y)
    
    wynik_testu <- wilcox.test(x, y)
    
    if (wynik_testu$p.value < prog) {
      
      liczba_istotnych_wynikow <- liczba_istotnych_wynikow + 1
      
    }
    
  }
  
  moc <- liczba_istotnych_wynikow / liczba_symulacji
  return(moc)
}

liczba_probek_lista <- c(30,50,70)
sigma_x_lista <- c(0.5, 0.75, 1, 1.25, 1.5)
sigma_y_lista <- c(0.5, 0.75, 1, 1.25, 1.5)
wartosci_delty <- seq(-1, 1, by = 0.05)

analiza_zmiany_liczby_probek <- function(liczba_probek_lista, sigma_x, sigma_y) {
  for (liczba_probek in liczba_probek_lista){
    wartosci_testow_wilcoxona <- sapply(wartosci_delty, function(delta) oblicz_moc_testu_wilcoxona(liczba_probek, delta, sigma_x, sigma_y))
    wartosci_testow <- sapply(wartosci_delty, function(delta) oblicz_moc_testu(liczba_probek, delta, sigma_x, sigma_y))
    
    wyniki <- data.frame(
      delta = wartosci_delty,
      moce_test = wartosci_testow,
      moce_wilcoxon = wartosci_testow_wilcoxona
    )
    
    min_delta <- wartosci_delty[which.min(abs(wyniki$moce_test - 0.8))]
    min_delta_wilcox <- wartosci_delty[which.min(abs(wyniki$moce_wilcoxon - 0.8))]
    
    print(paste("Min delta test liczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma_x, "parametr sigma dla próby Y σ =", sigma_y, min_delta, sep=" "))
    print(paste("Min delta wilcoxon liczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma_x, "parametr sigma dla próby Y σ =", sigma_y, min_delta_wilcox, sep=" "))
    
    tytul_wykresu <- paste("Porównanie mocy t-testu i testu Wilcoxona\nliczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma_x, "parametr sigma dla próby Y σ =", sigma_y, sep=" ") 
    
    wykres <- ggplot(wyniki, aes(x = delta)) +
      geom_line(aes(y = moce_test, color = "t-test")) +
      geom_line(aes(y = moce_wilcoxon, color = "Wilcoxon")) +
      labs(title = tytul_wykresu,
           x = "delta",
           y = "Moc testu") +
      scale_color_manual("", values = c("t-test" = "red", "Wilcoxon" = "blue")) +
      theme_minimal()
    
    print(wykres)
  }
}
analiza_zmiany_sigma_x_i_sigma_y_lacznie <- function(liczba_probek, sigma_lista) {
  for (sigma in sigma_lista){
    wartosci_testow_wilcoxona <- sapply(wartosci_delty, function(delta) oblicz_moc_testu_wilcoxona(liczba_probek, delta, sigma, sigma))
    wartosci_testow <- sapply(wartosci_delty, function(delta) oblicz_moc_testu(liczba_probek, delta, sigma, sigma))
    
    wyniki <- data.frame(
      delta = wartosci_delty,
      moce_test = wartosci_testow,
      moce_wilcoxon = wartosci_testow_wilcoxona
    )
    
    min_delta <- wartosci_delty[which.min(abs(wyniki$moce_test - 0.8))]
    min_delta_wilcox <- wartosci_delty[which.min(abs(wyniki$moce_wilcoxon - 0.8))]
    
    print(paste("Min delta test liczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma, "parametr sigma dla próby Y σ =", sigma, min_delta, sep=" "))
    print(paste("Min delta wilcoxon liczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma, "parametr sigma dla próby Y σ =", sigma, min_delta_wilcox, sep=" "))
    
    tytul_wykresu <- paste("Porównanie mocy t-testu i testu Wilcoxona\nliczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma, "parametr sigma dla próby Y σ =", sigma, sep=" ") 
    
    wykres <- ggplot(wyniki, aes(x = delta)) +
      geom_line(aes(y = moce_test, color = "t-test")) +
      geom_line(aes(y = moce_wilcoxon, color = "Wilcoxon")) +
      labs(title = tytul_wykresu,
           x = "delta",
           y = "Moc testu") +
      scale_color_manual("", values = c("t-test" = "red", "Wilcoxon" = "blue")) +
      theme_minimal()
    
    print(wykres)
  }
}

analiza_zmiany_sigma_x_i_sigma_y_rozlacznie <- function(liczba_probek, sigma_x_lista, sigma_y_lista) {
  przetworzone_kombinacje <- list()
  
  for (sigma_x in sigma_x_lista) {
    for (sigma_y in sigma_y_lista) {
      if (sigma_x == sigma_y) {
        next
      }
      
      kombinacja <- paste(sigma_x, sigma_y, sep = "-")
      odwrotna_kombinacja <- paste(sigma_y, sigma_x, sep = "-")
      if (kombinacja %in% przetworzone_kombinacje || odwrotna_kombinacja %in% przetworzone_kombinacje) {
        next
      }
      
      przetworzone_kombinacje <- c(przetworzone_kombinacje, kombinacja)
      
      wartosci_testow_wilcoxona <- sapply(wartosci_delty, function(delta) oblicz_moc_testu_wilcoxona(liczba_probek, delta, sigma_x, sigma_y))
      wartosci_testow <- sapply(wartosci_delty, function(delta) oblicz_moc_testu(liczba_probek, delta, sigma_x, sigma_y))
      
      wyniki <- data.frame(
        delta = wartosci_delty,
        moce_test = wartosci_testow,
        moce_wilcoxon = wartosci_testow_wilcoxona
      )
      
      min_delta <- wartosci_delty[which.min(abs(wyniki$moce_test - 0.8))]
      min_delta_wilcox <- wartosci_delty[which.min(abs(wyniki$moce_wilcoxon - 0.8))]
      
      print(paste("Min delta test liczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma_x, "parametr sigma dla próby Y σ =", sigma_y, min_delta, sep=" "))
      print(paste("Min delta wilcoxon liczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma_x, "parametr sigma dla próby Y σ =", sigma_y, min_delta_wilcox, sep=" "))
      
      tytul_wykresu <- paste("Porównanie mocy t-testu i testu Wilcoxona\nliczba próbek =", liczba_probek, "parametr sigma dla próby X σ =", sigma_x, "parametr sigma dla próby Y σ =", sigma_y, sep=" ") 
      
      wykres <- ggplot(wyniki, aes(x = delta)) +
        geom_line(aes(y = moce_test, color = "t-test")) +
        geom_line(aes(y = moce_wilcoxon, color = "Wilcoxon")) +
        labs(title = tytul_wykresu,
             x = "delta",
             y = "Moc testu") +
        scale_color_manual("", values = c("t-test" = "red", "Wilcoxon" = "blue")) +
        theme_minimal()
      
      print(wykres)
    }
  }
}



analiza_zmiany_liczby_probek(liczba_probek_lista, sigma_x_lista[3], sigma_y_lista[3])
analiza_zmiany_sigma_x_i_sigma_y_lacznie(liczba_probek[1], sigma_x_lista)
analiza_zmiany_sigma_x_i_sigma_y_rozlacznie(liczba_probek[1], sigma_x_lista, sigma_y_lista)




