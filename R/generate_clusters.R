#' @title generate_clusters
#'
#' @description Brief description of what the function does.
#'
#' @param J Integer. Number of clusters (default: 100).
#' @param njrange Integer vector of length 2. Range (min, max) for cluster sizes (default: \code{c(50, 100)}).
#' @param seed Integer. Random seed for reproducibility (default: 123456).
#'
#' @return Description of the output value or object the function returns.
#'
#' @details Additional details about the function, such as implementation notes or edge cases.
#'
#' @importFrom rlang .data
#'
generate_clusters <- function(J = 100, njrange = c(50, 100), seed =1234) {
    # library(tidyverse)

    # set seed
    set.seed(seed)

    # Generate random cluster sizes within the specified range
    nj_sizes <- stats::runif(J, njrange[1], njrange[2]) |> round()

    # Total number of individuals
    N <- sum(nj_sizes)

    # Create the initial data frame with individual IDs and cluster IDs
    data <- data.frame(id = 1:N,
                       school = unlist(purrr::map(1:J, ~ rep(.x, each = nj_sizes[.x])))) |>
      dplyr::group_by(.data$school) |>
      # Create a standardized cluster size variable 'W_nj'
      dplyr::mutate(
        W_nj = dplyr::if_else(
          njrange[1] == njrange[2],
          0,
          (dplyr::n() - njrange[1]) / (njrange[2] - njrange[1])
        )
      ) |> # NOTE: maybe reconsider
      dplyr::ungroup()

    return(list(
        data = data,
        nj_sizes = nj_sizes,
        njrange = njrange,
        J = J,
        N = N,
        seed = seed
    ))
}

