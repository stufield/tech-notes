# mixed effects for samples at 2 time points plot
set.seed(1001)
file  <- "mixed-samples.pdf"
scale <- 1
pdf(file = file, height = 10 * scale, width = 12 * scale,
    useDingbats = FALSE, title = "mixed-effects-samples")
shift <- 4
n     <- 15
t0 <- rnorm(n, 10, 2)
plot(1:2, c(t0[1L], t0[1L] + 3), type = "n",
     ylim = c(range(t0) + c(-1, shift)), ylab = "Response",
     xlab = "TimePoint/Predictor", cex.lab = 2, axes = FALSE)
box()
for ( i in seq_len(n) ) {
  lines(1:2, c(t0[i], t0[i] + shift), lwd = 1.5, type = "b",
        pch = 21, col = 1, bg = 2)
}
lines(1:2, c(mean(t0), mean(t0 + shift)), type = "b",
      lwd = 2, col = 2, lty = 2, pch = 21, bg = "blue")
axis(1, at = 1:2, c("x0", "x1"), cex.axis = 2)
