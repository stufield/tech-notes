library(power)
nboot <- 100
nsim  <- 250
out   <- list()
out$delta <- parallel::mclapply(seq(3, 25, 1), function(i)
  simulatePowerData(seq(0.5, 4, 0.1), n = i, nsim = nsim, nboot = nboot))
out$n <- parallel::mclapply(seq(0.5, 4, 0.1), function(i)
  simulatePowerData(seq(3, 25, 1), delta = i, nsim = nsim, nboot = nboot))
power:::figure("PowerSimulations_delta.pdf", width = 15, height = 10)
invisible(sapply(out$delta, plot))
dev.off()
power:::figure("PowerSimulations_n.pdf", width = 15, height = 10)
invisible(sapply(out$n, plot))
dev.off()
