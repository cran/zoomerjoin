## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(zoomerjoin)

## -----------------------------------------------------------------------------
n <- 10^5 # number of data points
d <- 10^2 # dimension

# Create a matrix of 10^6 observations in R^100
X <- matrix(runif(n * d), n, d)
# Second Dataset is a copy of the first with points shifted an infinitesimal
# amount
X_2 <- as.data.frame(X + matrix(rnorm(n * d, 0, .0001), n, d))
X <- as.data.frame(X)

## -----------------------------------------------------------------------------
euclidean_probability(.01, n_bands = 5, band_width = 8, r = .25)
euclidean_probability(.1, n_bands = 5, band_width = 8, r = .25)

euclidean_probability(.01, n_bands = 10, band_width = 4, r = .15)
euclidean_probability(.1, n_bands = 10, band_width = 4, r = .15)

euclidean_probability(.01, n_bands = 40, band_width = 8, r = .15)
euclidean_probability(.1, n_bands = 40, band_width = 8, r = .15)

## -----------------------------------------------------------------------------
set.seed(1)
start <- Sys.time()
joined_out <- euclidean_inner_join(
  X,
  X_2,
  threshold = .01,
  n_bands = 40,
  band_width = 8,
  r = .15
)
n_matches <- nrow(joined_out)
time_taken <- Sys.time() - start
print(paste("found", n_matches, "matches in", round(time_taken), "seconds"))

