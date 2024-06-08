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
# Hipoteza zerowa H0: Średnia inflacji studenckiej (własnej) jest mniejsza lub równa średniej inflacji oficjalnej
# Hipoteza alternatywna HA: Średnia inflacji studenckiej (własnej) jest większa od średniej inflacji oficjalnej

# Sprawdzenie normalności rozkładów
shapiro_x <- shapiro.test(X)
shapiro_y <- shapiro.test(Y)
print(shapiro_x)
print(shapiro_y)

# # Przeprowadzenie testu Wilcoxona (test par Wilcoxona)
wilcox_test <- wilcox.test(X, Y, paired = TRUE, alternative = "greater")

# Wyniki testu
print(wilcox_test)