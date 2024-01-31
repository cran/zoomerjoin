## -----------------------------------------------------------------------------
library(tidyverse)
library(microbenchmark)
library(fuzzyjoin)
library(zoomerjoin)

corpus_1 <- dime_data %>% # dime data is packaged with zoomerjoin
    head(500)
names(corpus_1) <- c("a", "field")
corpus_1

## -----------------------------------------------------------------------------
corpus_2 <- dime_data %>% # dime data is packaged with zoomerjoin
    tail(500)
names(corpus_2) <- c("b", "field")
corpus_2

## -----------------------------------------------------------------------------
set.seed(1)
start_time <- Sys.time()
join_out <- jaccard_inner_join(corpus_1, corpus_2,
                           by = "field", n_gram_width=6,
                           n_bands=20, band_width=6, threshold = .8)
print(Sys.time() - start_time)
print(join_out)

## -----------------------------------------------------------------------------
jaccard_curve(20,6)

## -----------------------------------------------------------------------------
organization_names <- c(
                        "American Civil Liberties Union",
                        "American Civil Liberties Union (ACLU)",
                        "NRA National Rifle Association",
                        "National Rifle Association NRA",
                        "National Rifle Association",
                        "Planned Parenthood",
                        "Blue Cross"
)
standardized_organization_names <- jaccard_string_group(organization_names, threshold=.5, band_width = 3)
print(standardized_organization_names)

