# mixed effects for samples at
# 2 time points plot

set.seed(1001)
file  <- "mixed-samples.pdf"
scale <- 1
par_def  <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))

pdf(file = file, height = 7 * scale, width = 12 * scale,
    useDingbats = FALSE, title = "mixed-effects-samples")
shift <- 4
n     <- 15
par(par_def)
t0 <- rnorm(n, 10, 2)
plot(1:2, c(t0[1L], t0[1L] + 3), type = "n",
     ylim = c(range(t0) + c(-1, shift)), ylab = "Response",
     main = "Individuals at Baseline (x0) and Time 1 (x1)",
     xlab = "TimePoint", cex.lab = 1.5, axes = FALSE)
box()
for ( i in seq_len(n) ) {
  lines(1:2, c(t0[i], t0[i] + shift), lwd = 3, type = "b",
        pch = 21, col = 1, bg = 4, cex = 2)
}
lines(1:2, c(mean(t0), mean(t0 + shift)), type = "b", cex = 2,
      lwd = 3, col = 2, lty = 2, pch = 21, bg = "green")
axis(1, at = 1:2, labels = c("x0", "x1"), cex.axis = 1.5)
