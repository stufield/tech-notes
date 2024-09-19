# Decision boundaries: KKNN vs Naïve Bayes
Stu Field
19 September 2024

------------------------------------------------------------------------

# Overview

It is sometimes nice to visualize the decision boundaries of various
models. Here we compare 2 commonly used models: naïve Bayes and
k-nearest neighbors.

## KNN vs naïve Bayes

Below are decision boundaries for 2 simulated data sets using K-Nearest
Neighbors and naïve Bayes models. For the first data set (top row) the
true boundary is simulated such that disease (red) protein 1 $> 44$ and
protein 2 $> 74$, these the data are simulated with an unrealistic harsh
cutoff to form the classes (unrealistic). The lower 2 panels are more
realistic data simulated from bivariate normal distributions and show
the difference in the boundary between the two methods.

``` r
par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))
par(par_def)
par(mfrow = c(2L, 2L))
plot_decision_boundary(fake_data(), res = 50, model.type = "knn")
plot_decision_boundary(fake_data(), res = 50, model.type = "bayes")
plot_decision_boundary(fake_data2(), res = 50, model.type = "knn")
plot_decision_boundary(fake_data2(), res = 50, model.type = "bayes")
```

![](figures/kknn-bayes-knn-vs-bayes-1.png)

## Choosing *k* in KNN

``` r
par(mfrow = c(3L, 3L))
for ( i in 2:10 ) {
  plot_decision_boundary(fake_data(), res = 50, model.type = "knn", k = i)
}
```

![](figures/kknn-bayes-knn-k-1.png)

------------------------------------------------------------------------

### Code Reference

``` r
fake_data <- function(a = 3, b = 2.14, sd = 250, pts = 200) {
  withr::local_seed(1001)
  # deterministic model
  # y.det <- a*x^b   
  # y <- rnorm(length(y.det), mean = y.det, sd = sd) # stochastic 'y'
  df <- data.frame(x1 = rnorm(pts, 45, 2), x2 = rnorm(pts, 75, 2))
  Response <- rep("disease", pts)
  Response[df$x1 < 44 | df$x2 <= 74] = "control"
  list(x = df, y = factor(Response))
}

fake_data2 <- function(pts = 100) {
  withr::local_seed(999)
  df1 <- data.frame(x1 = rnorm(pts, 46, 1.5), x2 = rnorm(pts, 75, 1.5)) # control
  df2 <- data.frame(x1 = rnorm(pts, 44, 1), x2 = rnorm(pts, 78, 1))     # disease
  df  <- rbind(df1, df2)
  list(x = df, y = factor(rep(c("control", "disease"), each = pts)))
}


# Calculate KNN bivariate results
#'
#' @param X data matrix
#' @param newdata new predictors to predict on
#' @param y response vector
predict_bivariate_knn <- function(X, newdata, y, k, ...) {
  if ( k < 2L ) {
    stop("Neighborhood (k) must be >= 1: ", k, call. = FALSE)
  }
  if ( missing(newdata) ) {
    newdata <- X
  }
  X <- data.matrix(X)
  ntr <- nrow(X)
  if ( length(y) != ntr ) {
    stop(
       sprintf("Length of class vector [y=%i] unequal to num. training samples (n=%i)",
               length(y), ntr),
       call. = FALSE)
  }
  if ( ntr < k ) {
    warning(
      sprintf("Neighborhood (k=%i) exceeds training data (n=%i) ... resetting k=%i",
              k, ntr, ntr),
      call. = FALSE)
      k <- ntr
  }
  nte <- nrow(newdata)
  idx <- seq(ntr)
  class_names <- names(table(y))
  neighbor_list <- lapply(seq(nte), function(.i) {
                          new_vals <- newdata[.i, ]
                          if ( length(new_vals) != 2L ) {
                            stop("Problem with new values ... length =",
                                 length(new_vals), call. = FALSE)
                          }
                          distances <- dist(rbind(new_vals, X), ...)[idx]
                          names(distances) <- idx
                          head(sort(distances), k)   # get the neighborhood
  })
  ret <- lapply(neighbor_list, function(.x) {
     prob <- table(y[as.numeric(names(.x))])
     prob <- unname(prob / sum(prob))[2L] # proportion disease in neighborhood
     if ( prob == 0.5 ) {
       class <- sample(class_names, 1, prob = prop.table(table(y))) # random tie-break
     } else {
       class <- ifelse(prob > 0.5, class_names[2L], class_names[1L])
     }
     data.frame(class = class, prob = prob)
  })
  do.call(rbind, ret)
}

#' plot a bivariate decision boundary
#'
#' @param res Resulution for the plot.
plot_decision_boundary <- function(data, res = 50, model.type = c("knn", "bayes"),
                                   k = 15, line.col = "darkviolet", lwd = 2,
                                   lty = 2, contours = 0.5, fast = FALSE) {
  model.type <- match.arg(model.type)
  z  <- data$y
  train <- data$x
  x1 <- train[, 1L]
  x2 <- train[, 2L]
  x1grid <- seq(min(x1), max(x1), length = res)
  x2grid <- seq(min(x2), max(x2), length = res)
  grid <- data.frame(expand.grid(x1 = x1grid, x2 = x2grid))   # grid of probs

  if ( model.type == "bayes" ) {
    rm(k)
    model <- robustNaiveBayes(train, data$y)
    prob  <- predict(model, grid, type = "raw")[, "disease"]
    title <- "Naive Bayes | disease (Pr>0.5) space: "
  } else if ( model.type == "knn" ) {
    if ( fast ) {
      model <- knn(train, grid, z, k = k, prob = TRUE) # compiled code
      prob  <- attr(model, "prob")
      prob  <- ifelse(model == "disease", prob, 1 - prob)
    } else {
      model <- predict_bivariate_knn(train, grid, z, k = k, method = "mink")
      prob  <- model$prob
    }
     title <- sprintf("KNN (k=%i) | disease (Pr>0.5) space: ",k)
  } else {
    stop("Inproper [model.type=] argument passed; can be [bayes, knn]", call. = FALSE)
  }

  prob_grid <- matrix(prob, nrow = res)
  pos_space <- round(sum(prob_grid >= 0.5) / res^2, 3L)

  contour(x = x1grid, y = x2grid, z = prob_grid, levels = contours,
          lwd = lwd, lty = lty, labcex = 1, vfont = c("sans serif", "bold"),
          col = line.col, xlab = "Protein 1", ylab = "Protein 2",
          main = paste0(title, pos_space), axes = TRUE)

  points(grid, pch = ".", cex = 0.5, col = ifelse(prob >= 0.50, "red", "dodgerblue"))
  points(train, pch = 21, col = 1, bg = ifelse(z == "control", "dodgerblue", "red"))
  invisible(data)
}
```
