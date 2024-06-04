require(ggplot2)

perform_t_test <- function(n, delta, sigma, alpha = 0.05) {
  set.seed(123)
  x <- rnorm(n, mean = 0, sd = sigma)
  y <- rnorm(n, mean = delta, sd = sigma)
  
  test_result <- t.test(x, y)
  p_value <- test_result$p.value
  
  power <- power.t.test(n = n, delta = delta, sd = sigma, sig.level = alpha, type = "two.sample")$power
  
  return(c(p_value = p_value, power = power))
}

perform_wilcoxon_test <- function(n, delta, sigma, alpha = 0.05) {
  set.seed(123)
  x <- rnorm(n, mean = 0, sd = sigma)
  y <- rnorm(n, mean = delta, sd = sigma)
  
  test_result <- wilcox.test(x, y)
  p_value <- test_result$p.value
  
  return(p_value)
}

calculate_wilcoxon_power <- function(n, delta, sigma, alpha = 0.05, n_simulations = 1000) {
  set.seed(123)
  significant_results <- 0
  
  for (i in 1:n_simulations) {
    x <- rnorm(n, mean = 0, sd = sigma)
    y <- rnorm(n, mean = delta, sd = sigma)
    
    test_result <- wilcox.test(x, y)
    if (test_result$p.value < alpha) {
      significant_results <- significant_results + 1
    }
  }
  
  power <- significant_results / n_simulations
  return(power)
}

n <- 30 
sigma <- 1
alpha <- 0.05

delta_values <- seq(0, 1, by = 0.1)
results <- t(sapply(delta_values, function(delta) perform_t_test(n, delta, sigma)))
wilcoxon_p_values <- sapply(delta_values, function(delta) perform_wilcoxon_test(n, delta, sigma))
wilcoxon_power_values <- sapply(delta_values, function(delta) calculate_wilcoxon_power(n, delta, sigma))

results_df <- data.frame(
  delta = delta_values,
  p_value = results[, "p_value"],
  power = results[, "power"],
  wilcoxon_p_value = wilcoxon_p_values,
  wilcoxon_power = wilcoxon_power_values
)

print(results_df)

ggplot(results_df, aes(x = delta)) +
  geom_line(aes(y = p_value, color = "t-test p-value")) +
  geom_line(aes(y = wilcoxon_p_value, color = "Wilcoxon p-value")) +
  geom_line(aes(y = power, color = "t-test power")) +
  geom_line(aes(y = wilcoxon_power, color = "Wilcoxon power")) +
  labs(title = "Porównanie wyników t-testu i testu Wilcoxona oraz ich mocy dla różnych wartości delta",
       x = "delta",
       y = "Wartość") +
  scale_color_manual("", values = c("t-test p-value" = "blue", "Wilcoxon p-value" = "green", "t-test power" = "red", "Wilcoxon power" = "purple")) +
  theme_minimal()

ggplot(results_df, aes(x = delta)) +
  geom_line(aes(y = power, color = "t-test power")) +
  geom_line(aes(y = wilcoxon_power, color = "Wilcoxon power")) +
  labs(title = "Porównanie mocy t-testu i testu Wilcoxona dla różnych wartości delta",
       x = "delta",
       y = "Moc testu") +
  scale_color_manual("", values = c("t-test power" = "red", "Wilcoxon power" = "purple")) +
  theme_minimal()

ggplot(results_df, aes(x = delta)) +
  geom_line(aes(y = p_value, color = "t-test p-value")) +
  geom_line(aes(y = wilcoxon_p_value, color = "Wilcoxon p-value")) +
  labs(title = "Porównanie wartości p t-testu i testu Wilcoxona dla różnych wartości delta",
       x = "delta",
       y = "p-value") +
  scale_color_manual("", values = c("t-test p-value" = "blue", "Wilcoxon p-value" = "green")) +
  theme_minimal()
