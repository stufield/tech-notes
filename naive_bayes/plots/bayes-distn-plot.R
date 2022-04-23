# Naive Bayes plot
value <- 14
scale <- 0.9
file  <- "plots/bayes-distn.pdf"
pdf(file = file, height = 10 * scale, width = 12 * scale,
    useDingbats = FALSE, title = "naive-bayes-distn")
control_par <- list(mean = 10, sd = 1.5)
disease_par <- list(mean = 17, sd = 2)
control_vec <- seq(5, 16, 0.1)
disease_vec <- seq(9, 25, 0.1)
plot(control_vec, dnorm(control_vec, control_par$mean, control_par$sd),
     type = "l", main="Bayes Odds Ratio Calculation",
     ylab = "Density", xlab = "Measurement Values", xlim = c(5, 25),
     col = "blue", lwd = 1.5)
lines(disease_vec, dnorm(disease_vec, disease_par$mean, disease_par$sd),
      col = "red", lwd = 1.5)
abline(v = control_par$mean, col = "gray70", lty = 5, lwd = 1)
abline(v = disease_par$mean, col = "gray70", lty = 5, lwd = 1)
abline(v = value, col=3, lty = 2, lwd=2)
legend("topright", legend = c("Control", "Disease"), col = c("blue", "red"), lty = 1)
text(control_par$mean, 0.1, sprintf("mu = %i", control_par$mean), cex = 1.5)
text(control_par$mean, 0.09, sprintf("sd = %0.1f", control_par$sd), cex = 1.5)
text(disease_par$mean, 0.1, sprintf("mu = %i", disease_par$mean), cex = 1.5)
text(disease_par$mean, 0.09, sprintf("sd = %0.1f", disease_par$sd), cex = 1.5)
log_odds <- log2(dnorm(value, control_par$mean, control_par$sd) /
                 dnorm(value, disease_par$mean, disease_par$sd))
text(23, 0.2, sprintf("log2(odds) = %0.4f", log_odds), cex = 1.5)
cat("Creating:", file, "\n")
