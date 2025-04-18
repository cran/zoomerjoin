#' Fuzzy joins for Jaccard distance using MinHash
#'
#' @param a,b The two dataframes to join.
#'
#' @param by A named vector indicating which columns to join on. Format should
#'   be the same as dplyr: `by = c("column_name_in_df_a" =
#'   "column_name_in_df_b")`, but two columns must be specified in each dataset
#'   (x column and y column). Specification made with `dplyr::join_by()` are
#'   also accepted.
#'
#' @param block_by A named vector indicating which column to block on, such that
#'   rows that disagree on this field cannot be considered a match. Format
#'   should be the same as dplyr: `by = c("column_name_in_df_a" =
#'   "column_name_in_df_b")`
#'
#' @param n_gram_width The length of the n_grams used in calculating the Jaccard
#'   similarity. For best performance, I set this large enough that the chance
#'   any string has a specific n_gram is low (i.e. `n_gram_width` = 2 or 3 when
#'   matching on first names, 5 or 6 when matching on entire sentences).
#'
#' @param n_bands The number of bands used in the minihash algorithm (default is
#'   40). Use this in conjunction with the `band_width` to determine the
#'   performance of the hashing. The default settings are for a
#'   (.2, .8, .001, .999)-sensitive hash i.e. that pairs with a similarity of less
#'   than .2 have a >.1% chance of being compared, while pairs with a similarity
#'   of greater than .8 have a >99.9% chance of being compared.
#'
#' @param band_width The length of each band used in the minihashing algorithm
#'   (default is 8) Use this in conjunction with the `n_bands` to determine the
#'   performance of the hashing. The default settings are for a
#'   (.2, .8, .001, .999)-sensitive hash i.e. that pairs with a similarity of less
#'   than .2 have a >.1% chance of being compared, while pairs with a similarity
#'   of greater than .8 have a >99.9% chance of being compared.
#'
#' @param threshold The Jaccard similarity threshold above which two strings
#'   should be considered a match (default is .95). The similarity is equal to
#'   1 - the Jaccard distance between the two strings, so 1 implies the strings
#'   are identical, while a similarity of zero implies the strings are completely
#'   dissimilar.
#'
#' @param clean Should the strings that you fuzzy join on be cleaned (coerced to
#'   lower-case, stripped of punctuation and spaces)? Default is `FALSE`.
#'
#' @param progress Set to `TRUE` to print progress.
#'
#' @param similarity_column An optional character vector. If provided, the data
#'   frame will contain a column with this name giving the Jaccard similarity
#'   between the two fields. Extra column will not be present if anti-joining.
#'
#' @return A tibble fuzzily-joined on the basis of the variables in `by.` Tries
#'   to adhere to the same standards as the dplyr-joins, and uses the same
#'   logical joining patterns (i.e. inner-join joins and keeps only observations
#'   in both datasets).
#'
#' @rdname jaccard-joins
#' @export
#'
#' @examples
#' # load baby names data
#' # install.packages("babynames")
#' library(babynames)
#'
#' baby_names <- data.frame(name = tolower(unique(babynames$name))[1:500])
#' baby_names_sans_vowels <- data.frame(
#'   name_wo_vowels = gsub("[aeiouy]", "", baby_names$name)
#' )
#' # Check the probability two pairs of strings with similarity .8 will be
#' # matched with a band width of 8 and 30 bands using the `jaccard_probability()`
#' # function:
#' jaccard_probability(.8, 30, 8)
#'
#' # Run the join and only keep rows that have a match:
#' jaccard_inner_join(
#'   baby_names,
#'   baby_names_sans_vowels,
#'   by = c("name" = "name_wo_vowels"),
#'   threshold = .8,
#'   n_bands = 20,
#'   band_width = 6,
#'   n_gram_width = 1,
#'   clean = FALSE # default
#' )
#'
#' # Run the join and keep all rows from the first dataset, regardless of whether
#' # they have a match:
#' jaccard_left_join(
#'   baby_names,
#'   baby_names_sans_vowels,
#'   by = c("name" = "name_wo_vowels"),
#'   threshold = .8,
#'   n_bands = 20,
#'   band_width = 6,
#'   n_gram_width = 1
#' )
jaccard_inner_join <- function(a, b,
                               by = NULL,
                               block_by = NULL,
                               n_gram_width = 2,
                               n_bands = 50,
                               band_width = 8,
                               threshold = .7,
                               progress = FALSE,
                               clean = FALSE,
                               similarity_column = NULL) {

  fuzzy_join_core(a = a, b = b, by = by,
            join_func = jaccard_join,
            mode = "inner",
            block_by = block_by,
            n_gram_width = n_gram_width,
            n_bands = n_bands,
            band_width = band_width,
            threshold = threshold,
            progress = progress,
            similarity_column = similarity_column,
            clean = clean
  )
}

#' @rdname jaccard-joins
#' @export
jaccard_anti_join <- function(a, b,
                              by = NULL,
                              block_by = NULL,
                              n_gram_width = 2,
                              n_bands = 50,
                              band_width = 8,
                              threshold = .7,
                              progress = FALSE,
                              clean = FALSE, similarity_column = NULL) {
  fuzzy_join_core(a = a, b = b, by = by,
            join_func = jaccard_join,
            mode = "anti",
            block_by = block_by,
            n_gram_width = n_gram_width,
            n_bands = n_bands,
            band_width = band_width,
            threshold = threshold,
            progress = progress,
            similarity_column = similarity_column,
            clean = clean
  )
}

#' @rdname jaccard-joins
#' @export
jaccard_left_join <- function(a, b,
                              by = NULL,
                              block_by = NULL,
                              n_gram_width = 2,
                              n_bands = 50,
                              band_width = 8,
                              threshold = .7,
                              progress = FALSE,
                              clean = FALSE,
                              similarity_column = NULL) {

  fuzzy_join_core(a = a, b = b, by = by,
            join_func = jaccard_join,
            mode = "left",
            block_by = block_by,
            n_gram_width = n_gram_width,
            n_bands = n_bands,
            band_width = band_width,
            threshold = threshold,
            progress = progress,
            similarity_column = similarity_column,
            clean = clean
  )
}

#' @rdname jaccard-joins
#' @export
jaccard_right_join <- function(a, b,
                               by = NULL,
                               block_by = NULL,
                               n_gram_width = 2,
                               n_bands = 50,
                               band_width = 8,
                               threshold = .7,
                               progress = FALSE,
                               clean = FALSE,
                               similarity_column = NULL) {
  fuzzy_join_core(a = a, b = b, by = by,
            join_func = jaccard_join,
            mode = "right",
            block_by = block_by,
            n_gram_width = n_gram_width,
            n_bands = n_bands,
            band_width = band_width,
            threshold = threshold,
            progress = progress,
            similarity_column = similarity_column,
            clean = clean
  )
}

#' @rdname jaccard-joins
#' @export
jaccard_full_join <- function(a, b,
                              by = NULL,
                              block_by = NULL,
                              n_gram_width = 2,
                              n_bands = 50,
                              band_width = 8,
                              threshold = .7,
                              progress = FALSE,
                              clean = FALSE,
                              similarity_column = NULL) {
  fuzzy_join_core(a = a, b = b, by = by,
            join_func = jaccard_join,
            mode = "full",
            block_by = block_by,
            n_gram_width = n_gram_width,
            n_bands = n_bands,
            band_width = band_width,
            threshold = threshold,
            progress = progress,
            similarity_column = similarity_column,
            clean = clean
  )
}
