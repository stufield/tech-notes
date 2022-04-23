
create_fake_long_data <- function() {
  t.beta1 <- seq(1, 200, 0.5)
  control <- list(nsubj = 20, beta0 = 1000, max.obs = 15, r.seed = 1,
                  sd.pars = list(sigma = 250), auto.cor = .25)
  treated <- list(nsubj = 20, beta0 = 1000, max.obs = 15, r.seed = 1,
                  sd.pars = list(sigma = 450), auto.cor = .25)
  sim_list <- lapply(seq(length(t.beta1)), function(i) {
               on.exit(rm(.Random.seed, envir = .GlobalEnv))
               control$beta1 <- 0
               treated$beta1 <- t.beta1[i]
               SomaMixedEffects::createLongData(control, treated)
          })
  sim_list <- purrr::map2(sim_list, seq(length(sim_list)), function(x, y) {
               names(x)[ which(names(x) == "yij") ] = sprintf("SOMAmer_%02i", y)
               return(x)
               })
  sim_list <- Reduce(function(...) cbind(...), sim_list)
  sim_list <- sim_list[, sort(names(sim_list))]
  sim_list <- sim_list[, c("pid", "time", "Group",
                           grep("^SOMAmer",names(sim_list), value = TRUE))]
  sim_list
}


fit_mixed_models <- function(adat) {
  apts <- grep("^SOMAmer", names(adat), value=TRUE)
  names(apts) <- apts
  models <- lapply(apts, function(apt) {
    levels <- length(table(adat$SampleGroup)) # how many Group levels?
    fixed <- ifelse(levels==1, "time", "time*SampleGroup")
    f <- as.formula(sprintf("%s ~ %s", apt, fixed))
    fit.lme.try(formula=f, random=~1|pid, data=adat)
    })
  invisible(models)
}

create.mixed.models.table <- function(X) {
  # X = the list of models created above
  summary <- lapply(X, function(apt) {
              aov.table <- as.matrix(anova(apt))[,-c(1,2)]
              matnames <- outer(rownames(aov.table),colnames(aov.table),paste,sep="_")
              out <- c(t(aov.table))
              names(out) <- c(t(matnames))
              out <- data.frame(as.list(out))
              # recast to 1 row data frame
              out$converged <- apt$converged
              # has the model converged?
              out[,-grep("Intercept",names(out))] # often don't care about intercept
               })
  ret <- do.call(rbind, summary) # combine single rows into 1 table
  test.col <- which(names(ret)=="converged") - 1 # p-value is next to converged column
  ret$fdr <- p.adjust(ret[[test.col]], method = "fdr") # Benjamini-Hochberg FDR correction
  ret$p.bonferroni <- p.adjust(ret[[test.col]], method = "bon") # Bonferroni correction
  ret <- ret[ order(ret$fdr, -ret$time.SampleGroup_F.value), ] # reorder by q-value & p-value
  ret$rank <- seq(nrow(ret)) # assign ranks
  ret
}

run <- function() {
  on.exit(rm(.Random.seed, envir = .GlobalEnv))
  fake.long.adat <<- create_fake_long_data()
  fake.models <<- SomaMixedEffects(fake.long.adat)
  fake.mm.table <<- SomaMixedEffects::createMixedModelsTable(fake.models)
  fake.mm.plots <<- lapply(rownames(fake.mm.table), function(.x)
                   mixedEffectsPlots(fake.long.adat, response = .x,
                                     time = "time", ID = "pid", group = "SampleGroup"))
  plot.grob.list(fake.mm.plots, c(1,2), "Sim_MixedModel_LongPlots2.pdf",
                 width=20, height=10)
}
