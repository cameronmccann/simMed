#' @title baseR_generate_clusters
#'
#' @description Generate J cluster sizes in a range and return the individual-level data and metadata.
#'
#' @param J integer. Number of clusters. Default 100.
#' @param njrange numeric length 2. Min and max cluster size (inclusive-ish). Default c(50, 100).
#' @param seed optional integer. If provided, used to seed RNG.
#'
#' @return A list with elements: data (data.frame), nj_sizes, njrange, J, N, seed.

baseR_generate_clusters <- function(J = 100, njrange = c(50, 100), seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # guardrails
  stopifnot(length(njrange) == 2, is.numeric(njrange), J >= 0)
  if (J == 0) {
    return(list(
      data = data.frame(id = integer(0), school = integer(0), W_nj = numeric(0)),
      nj_sizes = integer(0), njrange = njrange, J = 0, N = 0, seed = seed
    ))
  }

  nj_sizes <- round(stats::runif(J, njrange[1], njrange[2]))
  N <- sum(nj_sizes)

  # vector of cluster ids of length N, e.g. 1,1,...(nj_sizes[1] times), 2,2,..., etc.
  school <- rep.int(seq_len(J), times = nj_sizes)

  # W_nj is standardized cluster size attached to each row by its school
  W_nj <- (nj_sizes[school] - njrange[1]) / (njrange[2] - njrange[1])

  data <- data.frame(
    id = seq_len(N),
    school = school,
    W_nj = W_nj
  )

  list(
    data = data,
    nj_sizes = nj_sizes,
    njrange = njrange,
    J = J,
    N = N,
    seed = seed
  )
}
