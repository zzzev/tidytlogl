#' Compute log-likelihood values for text mining.
#'
#' @param df A data frame (or tibble, etc.) in tidytext format
#' @param group_key Column name of \code{df} to use for grouping of
#' log-likelihood
#' @param token_key Column name of \code{df} that contains tokens or n-grams
#' @param threshold Minimum log-likelihood value (number) to include in results
#' @return A data frame with columns for token, group, and ll (log-likelihood
#' values above \code{threshold})
#' @importFrom magrittr %>%
#' @export
tidy_logl <- function(df, group_key, token_key, threshold = 3) {
  total_words_spoken <- nrow(df)
  q_group_key <- dplyr::enquo(group_key)
  per_group_totals <- df %>%
    dplyr::count(!!q_group_key) %>%
    dplyr::rename(per_group = n)

  q_token_key <- dplyr::enquo(token_key)
  per_token_totals <- df %>%
    dplyr::count(!!q_token_key) %>%
    dplyr::rename(per_token = n)

  df %>%
    dplyr::count(!!q_group_key, !!q_token_key) %>%
    dplyr::left_join(per_group_totals) %>%
    dplyr::left_join(per_token_totals) %>%
    dplyr::mutate(
      in_group_non_tokens = per_group - n,
      out_group_tokens = per_token - n,
      out_group_non_tokens = total_words_spoken - per_group - per_token
    ) %>%
    dplyr::rename( # see above linked PDF (p.2) for variable naming reference
      a = n,
      b = out_group_tokens,
      c = in_group_non_tokens,
      d = out_group_non_tokens
    ) %>%
    dplyr::select(-c(per_group, per_token)) %>%
    dplyr::mutate(
      e1 = (a + c) * ((a + b) / total_words_spoken),
      e2 = (b + d) * ((a + b) / total_words_spoken),
      ll = 2 * ((a * log(a / e1)) + (b * log(b / e2)))
    ) %>%
    dplyr::filter(!is.na(ll), ll > threshold) %>%
    dplyr::mutate(ll = dplyr::if_else(a < e1, ll * -1, ll)) %>%
    dplyr::select(-c(a, b, c, d)) %>%
    dplyr::arrange(desc(ll))
}