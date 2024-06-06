# Krok 1: Wczytanie danych

X <- scan("./projekt_2/dane/Wlasna_Inflacja_table.txt", sep='\n') # Dane inflacji własnej studenckiej od 2020 do 2023 roku
Y <- scan("./projekt_2/dane/Oficjalna_Inflacja_table.txt", sep='\n') # Dane inflacji oficjalnej od 2020 do 2023 roku

# Przedstawmy dane w formie wykresów liniowych na wspólnym wykresie
miesiace <- seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = length(X))
plot(miesiace, X, type = "l", col = "blue", lwd = 2, ylim = range(c(X, Y)),
     xlab = "Czas", ylab = "Inflacja (%)", main = "Porównanie Inflacji Własnej Studenckiej i Oficjalnej")
lines(miesiace, Y, col = "red", lwd = 2)
legend("topright", legend = c("Inflacja własna studencka", "Inflacja oficjalna"),
       col = c("blue", "red"), lty = 1, lwd = 2)

# Krok 2: Wykresy pudełkowe obu inflacji
boxplot(X, Y, names = c("Inflacja własna studencka", "Inflacja oficjalna"), 
        main = "Porównanie inflacji własnej studenckiej i oficjalnej", ylab = "Procent")

# Krok 3: Wykres pudełkowy różnic Xi - Yi
differences <- X - Y
boxplot(differences, main="Różnice Inflacja własna - Inflacja oficjalna", ylab="Procent")

# Krok 4: Testowanie hipotezy
# Hipoteza zerowa H0: Mediana różnic = 0
# Hipoteza alternatywna HA: Mediana różnic != 0

# Przeprowadzenie testu Wilcoxona (test par Wilcoxona)
test_result <- wilcox.test(X, Y, paired=TRUE)
test_result

# Interpretacja wyniku testu
if (test_result$p.value < 0.05) {
  cat("Odrzucamy hipotezę zerową na poziomie istotności 0.05. Istnieje statystycznie istotna różnica między inflacjami.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej na poziomie istotności 0.05. Brak statystycznie istotnej różnicy między inflacjami.\n")
}