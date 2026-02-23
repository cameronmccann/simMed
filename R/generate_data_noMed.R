#' @title generate_data_noMed
#'
#' @description
#' Generates a clustered dataset with an individual-level treatment (\code{A}),
#' an outcome (\code{Y}), individual-level covariates (\code{X}), and a
#' cluster-level variable (\code{Z}). A simplified version of \code{generate_data()}
#' with no mediator. Supports both binary and continuous outcomes and computes
#' the true total treatment effect (TTE) if requested.
#'
#' @param J Integer. Number of clusters (default: 100).
#' @param njrange Integer vector of length 2. Range (min, max) for cluster sizes (default: \code{c(50, 100)}).
#' @param Yfamily Character. Family for the outcome (\code{"gaussian"} or \code{"binomial"}). Defaults to \code{"binomial"}.
#' @param if.null Logical. If \code{TRUE}, generate data under the null hypothesis (i.e., no effects). Defaults to \code{FALSE}.
#' @param seed Integer. Random seed for reproducibility (default: 123456).
#' @param num_x Integer. Number of individual-level covariates (\code{X}). Defaults to 3.
#' @param a_on_x Numeric. Effect of \code{X} on treatment \code{A}. Defaults to \code{sqrt(0.05625 / 3)}.
#' @param a_on_z Numeric. Effect of \code{Z} on treatment \code{A}. Defaults to \code{sqrt(0.15 / 1)}.
#' @param x_z Numeric. Correlation between \code{X} and the cluster-level variable \code{Z} (default: 0).
#' @param yintercept Numeric. Intercept in the outcome \code{Y} model (default: 1).
#' @param y_on_a Numeric. Main effect of \code{A} on \code{Y} (default: 0.5).
#' @param y_on_az Numeric. Interaction effect of \code{A} and \code{Z} on \code{Y} (default: 0.2).
#' @param y_on_anj Numeric. Interaction effect of \code{A} and cluster size (\code{nj}) on \code{Y} (default: 0.2).
#' @param y_on_x Numeric. Effect of each individual-level covariate \code{X} on \code{Y}.
#'   Defaults to \code{sqrt(0.15 / num_x)}.
#' @param y_on_z Numeric. Effect of \code{Z} on \code{Y}. Defaults to \code{sqrt(0.4)}.
#' @param quadratic.A Logical. If \code{TRUE}, include quadratic term for \code{A} in the mediator/outcome models (default: FALSE).
#' @param quadratic.Y Logical. If \code{TRUE}, include quadratic term for \code{Y} itself (used in some advanced simulations) (default: FALSE).
#' @param iccx Numeric. Intra-class correlation for the \code{X} variables (default: 0.2).
#' @param icca Numeric. Intra-class correlation for treatment \code{A} (default: 0.2).
#' @param iccy Numeric. Intra-class correlation for outcome \code{Y} (default: 0.2).
#' @param include_truevals Logical. If \code{TRUE}, call \code{trueVals2.0d()} to compute the true potential outcomes and
#'   mediation effects (default: TRUE).
#' @param include_overlapMsg Logical. If \code{TRUE}, ...  (default: TRUE).
#' @param plot_PSdiagnostics Logical. If \code{TRUE}, ... (default: FALSE).
#' @param randomize Logical. If \code{TRUE}, randomizes treatment \code{A} (default: FALSE).
#' @param ensure_cluster_positivity Logical. If \code{TRUE}, will only generate data that meets the positivity assumption (default: TRUE).
#'
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{data}}{A data frame containing the simulated data: \code{A} (treatment), \code{Y} (outcome),
#'                      \code{Z} (cluster-level var), \code{X1},...,\code{Xp} (individual-level covariates),
#'                      \code{school} (cluster ID), \code{W_nj} (standardized cluster size),
#'                      \code{ps_true} (true propensity score), \code{iptw_true} (IPTW weights).}
#'   \item{\code{truevals}}{A list from \code{trueVals_noMed()} containing the true total treatment
#'   effect at the individual and cluster levels. \code{NULL} if \code{include_truevals = FALSE}.}
#'   \item{\code{overlap}}{A list containing diagnostic plots and summary for propensity scores (\code{ps_true})
#'                         and stabilized IPTW (\code{iptw_true}). Useful for checking overlap/outliers.}
#'   \item{\code{parameters}}{A list of the input parameters and additional generated quantities
#'   (e.g., \code{nj_sizes}, \code{y_given}, \code{clust_trt_prop}).}
#' }
#'
#'
#' @details
#' This function is a wrapper that calls:
#' \itemize{
#'   \item \code{generate_clusters()}: Builds the cluster IDs and sets cluster sizes in \code{njrange}.
#'   \item \code{generate_confounders()}: Creates individual-level confounders \code{X} (optionally correlated with \code{Z}).
#'   \item \code{generate_treatment()}: Simulates the binary (or continuous) treatment \code{A} with optional ICC.
#'   \item \code{generate_outcome()}: Constructs outcome \code{Y} based on \code{A}, \code{M}, \code{Z},
#'                                    interactions, ICC, etc., again depending on the family chosen.
#'   \item \code{trueVals_noMed()}: if \code{include_truevals = TRUE}.
#' }
#' Note: No mediator is generated; all mediator-related coefficients are fixed to 0 internally when calling
#' \code{generate_outcome()}.
#'
#'
#' @examples
#' result <- generate_data_noMed(
#'   J = 50,
#'   njrange = c(10, 20),
#'   Yfamily = "gaussian"
#' )
#' str(result$data)
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom stats var quantile qlogis pnorm rnorm
#'
#' @export
generate_data_noMed <- function(J = 100,                        # Number of clusters
                          njrange = c(50, 100),            # Range for cluster sizes
                          # Mfamily = "binomial",            # Family for mediator ('gaussian' or 'binomial')
                          Yfamily = "binomial",            # Family for outcome ('gaussian' or 'binomial')
                          if.null = FALSE,
                          seed = 123456,                   # Seed for reproducibility
                          num_x = 3,                       # Number of individual-level confounders
                          a_on_x = sqrt(0.05625 / 3), # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
                          a_on_z = sqrt(0.15 / 1), # a_z = sqrt(0.4 / 1),
                          x_z = 0,                         # Correlation between 'X' and 'Z'
                          # m_on_a = 0.2,                    # Effect of 'A' on 'M'
                          # m_on_az = 0.2,                   # Interaction: 'A' x 'Z' on 'M'
                          # m_on_anj = 0.2,                  # Interaction: 'A' x cluster size on 'M'
                          # m_on_x = sqrt(0.15 / num_x),     # Effect of 'X' on 'M'
                          # m_on_z = sqrt(0.4),              # Effect of 'Z' on 'M'
                          # int.XZ = TRUE,                   # Include X:Z interaction in mediator/outcome model #' @param int.XZ Logical. If \code{TRUE}, include an interaction term between \code{X} and \code{Z} in mediator/outcome models. Defaults to \code{TRUE}.
                          yintercept = 1,                  # Intercept for outcome model
                          y_on_a = 0.5,                    # Effect of 'A' on 'Y'
                          # y_on_m = 1,                      # Effect of 'M' on 'Y'
                          # y_on_am = 0,                     # Interaction: 'A' x 'M' on 'Y'
                          y_on_az = 0.2,                   # Interaction: 'A' x 'Z' on 'Y'
                          # y_on_mz = 0.2,                   # Interaction: 'M' x 'Z' on 'Y'
                          y_on_anj = 0.2,                  # Interaction: 'A' x cluster size on 'Y'
                          y_on_x = sqrt(0.15 / num_x),     # Effect of 'X' on 'Y'
                          y_on_z = sqrt(0.4),              # Effect of 'Z' on 'Y'
                          quadratic.A = FALSE,             # Include quadratic term for 'A'
                          # quadratic.M = FALSE,             # Include quadratic term for 'M'
                          quadratic.Y = FALSE,             # Include quadratic term for 'Y'
                          iccx = 0.2,                      # Intra-class correlation for 'X'
                          icca = 0.2,                      # Intra-class correlation for 'A'
                          # iccm = 0.2,                      # Intra-class correlation for 'M'
                          iccy = 0.2,                      # Intra-class correlation for 'Y'
                          include_truevals = TRUE,         # Whether or not to compute true values
                          include_overlapMsg = TRUE,       # Whether or not to display messages about PS overlap in console
                          plot_PSdiagnostics = FALSE,
                          randomize = FALSE,
                          ensure_cluster_positivity = TRUE
) {

  # Input validation for Yfamily
  if (!Yfamily %in% c("binomial", "gaussian")) {
    stop("Yfamily must be either 'binomial' or 'gaussian'")
  }

  # 1. Cluster generation  --------------------------------------------------
  set.seed(seed)

  # Generate initial cluster structure
  # (cluster IDs, cluster sizes) based on J and njrange
  data_list <- generate_clusters(J = J, njrange = njrange, seed = seed)

  # 2. Generate confounders (X & Z) -----------------------------------------
  data_list <- generate_confounders(
    data_list = data_list,
    nj_sizes = data_list$nj_sizes,
    num_x = num_x,
    iccx = iccx,
    x_z = x_z
  )

  # 3. Generate treatment (A) -----------------------------------------------
  data_list <- generate_treatment(
    data_list = data_list,
    nj_sizes = data_list$nj_sizes,
    icca = icca,
    quadratic.A = quadratic.A,
    num_x = num_x,
    randomize = randomize,
    a_on_x = a_on_x, # a_x = data_list$a_x,
    a_on_z = a_on_z # a_z = a_z,
  )

  # 3b. Check for Positivity Assumption
  if (ensure_cluster_positivity) {
    var_by_cluster <- tapply(data_list$data$A, data_list$data$school, var)
    bad <- which(var_by_cluster == 0 | is.na(var_by_cluster))
    # if (any(var_by_cluster == 0, na.rm = TRUE)) {
    #     return(NULL)
    # }
    if (length(bad) > 0) {
      bad_ids <- names(bad)
      msg <- paste0(
        "Positivity check failed in ", length(bad),
        " cluster(s): ", paste(bad_ids, collapse = ", "), ". \n",
        "Data was not generated. "
        # "Within-cluster variance of A is zero (or NA). "#,
        # "Try increasing J/njrange, enabling randomization, or relaxing constraints."
      )
      message(msg)
      return(NULL)
    }
  }


  # 4. Diagnostic plots of the propensity scores ----------------------------
  overlap_plot <- NULL
  overlap_plot_logit <- NULL
  if (plot_PSdiagnostics == TRUE) {
    # Overlap plot (density of ps_true by treatment group)
    overlap_plot <- ggplot2::ggplot(data_list$data, ggplot2::aes(x = ps_true, color = factor(A), fill = factor(A))) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::labs(
        title = "Density Plot of ps_true by Treatment Group (A)",
        x = "True Propensity Score (ps_true)",
        y = "Density",
        fill = "Treatment (A)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "top",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )

    # Overlap plot on the logit scale
    overlap_plot_logit <- ggplot2::ggplot(data_list$data, ggplot2::aes(x = stats::qlogis(ps_true), fill = factor(A))) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::labs(
        title = "Density Plot of Logit(ps_true) by Treatment Group (A)",
        x = "Logit of the True Propensity Score",
        y = "Density",
        fill = "Treatment (A)"
      ) +
      ggplot2::theme_minimal()
  }

  # Summaries of extreme PS values (below 0.01 or above 0.99)
  n_ps_below_001 <- sum(data_list$data$ps_true < 0.01, na.rm = TRUE)
  n_ps_above_099 <- sum(data_list$data$ps_true > 0.99, na.rm = TRUE)
  pct_ps_below_001 <- 100 * n_ps_below_001 / nrow(data_list$data)
  pct_ps_above_099 <- 100 * n_ps_above_099 / nrow(data_list$data)

  # Combine into a message string
  ps_msg <- paste0(
    "Number of PSs < 0.01: ", n_ps_below_001, " (",
    round(pct_ps_below_001, 2), "%); ",
    "Number of PSs > 0.99: ", n_ps_above_099, " (",
    round(pct_ps_above_099, 2), "%)"
  )
  if (include_overlapMsg == TRUE) {
    message(ps_msg) # Print info about overlap to console
  }

  # Create an IPTW variable for each observation
  data_list$data <- data_list$data |>
    dplyr::mutate(
      iptw_true = ifelse(A == 1, 1 / ps_true, 1 / (1 - ps_true))
    )

  # Summaries of extreme IPTW values (below 1st pct or above 99th pct)
  first_percentile <- stats::quantile(data_list$data$iptw_true, probs = 0.01, na.rm = TRUE)
  ninety_ninth_percentile <- stats::quantile(data_list$data$iptw_true, probs = 0.99, na.rm = TRUE)

  n_iptw_below_1p <- sum(data_list$data$iptw_true < first_percentile, na.rm = TRUE)
  n_iptw_above_99p <- sum(data_list$data$iptw_true > ninety_ninth_percentile, na.rm = TRUE)
  pct_iptw_below_1p <- 100 * n_iptw_below_1p / nrow(data_list$data)
  pct_iptw_above_99p <- 100 * n_iptw_above_99p / nrow(data_list$data)

  # Combine into a message string
  iptw_msg <- paste0(
    "Number of cases < 1st percentile of IPTW (",
    round(first_percentile, 4), "): ", n_iptw_below_1p, " (",
    round(pct_iptw_below_1p, 2), "%); ",
    "Number of cases > 99th percentile of IPTW (",
    round(ninety_ninth_percentile, 4), "): ", n_iptw_above_99p, " (",
    round(pct_iptw_above_99p, 2), "%)"
  )
  if (include_overlapMsg == TRUE) {
    message(iptw_msg)  # Print info about IPTW extremes to console
  }

  # 5. Generate mediator (M) ------------------------------------------------

  # Add a placeholder M = 0 so generate_outcome() doesn't break, but zero out all M-related coefficients
  data_list$data$M <- 0

  # 6. Generate outcome (Y) -------------------------------------------------
  data_list <- generate_outcome(
    data_list = data_list,
    iccy = iccy,
    yintercept = yintercept,
    y_on_a = y_on_a,
    y_on_m = 0,
    y_on_am = 0,
    y_on_az = y_on_az,
    y_on_mz = 0,
    y_on_anj = y_on_anj,
    num_x = num_x,
    y_on_x = y_on_x, #
    y_on_z = y_on_z, #
    quadratic.Y = quadratic.Y,
    # int.XZ = int.XZ,
    Yfamily = Yfamily,
    if.null = if.null
  )

  # 7. (Optional) Compute true potential outcomes  --------------------------
  # # If include_truevals is TRUE, compute Y(a0, gm(a1)) values
  # # and obtain mediation effects (PNDE, PNIE, etc.).
  # if (include_truevals == TRUE) {
  #   true_vals <- trueVals2.0f(data_list = data_list) #trueVals2.0d(data_list = data_list) #
  # } else {
  #   true_vals <- NULL
  # }

  true_tte <- NULL
  if (include_truevals) {
    true_tte <- trueVals_noMed(data_list = data_list, Yfamily = Yfamily)
  }

  # # 8. Calculate mediation effects (if true values available) ---------------
  #
  # # Extract the relevant potential outcomes from true_vals
  # if (!is.null(true_tte)) {
  #   # Individual-level potential outcomes
  #   y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
  #   y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
  #   y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
  #   y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`
  #
  #   # Compute individual-level mediation effects
  #   pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
  #   pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
  #   tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
  #   tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect
  #
  #   # Cluster-level potential outcomes
  #   y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
  #   y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
  #   y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
  #   y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
  #
  #   # Compute cluster-level mediation effects
  #   pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0
  #   pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0
  #   tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1
  #   tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0
  # } else {
  #   # If we didn't compute trueVals2.0d, set effects to NULL
  #   pnde_ind <- pnie_ind <- tnde_ind <- tnie_ind <- NULL
  #   pnde_cluster <- pnie_cluster <- tnde_cluster <- tnie_cluster <- NULL
  # }

  # 9. Post-processing: separate out X columns if needed --------------------
  datobs <- data_list$data
  if (is.matrix(datobs$X)) {
    # If X was stored as a matrix, split it into X1, X2, ..., X_{num_x}
    for (i in seq_len(num_x)) {
      datobs[[paste0("X", i)]] <- datobs$X[, i]
    }
    datobs$X <- NULL
  }
  data_list$data <- datobs
  rm(datobs)

  # 10. Prepare final output  -----------------------------------------------
  result_data <- list(
    data = data_list$data,
    truevals = true_tte,
    overlap = list(
      overlap_plot = overlap_plot,
      overlap_plot_logit = overlap_plot_logit,
      ps_summary = ps_msg,
      iptw_summary = iptw_msg
    ),
    parameters = list(
      J = J,
      # N = N,
      njrange = njrange,
      nj_sizes = data_list$nj_sizes,
      y_given = data_list$y_given,
      # m_given = data_list$m_given,
      seed = seed,
      num_x = num_x,
      iccx = iccx,
      x_z = x_z,
      icca = icca,
      quadratic.A = quadratic.A,
      # iccm = iccm,
      # m_on_a = m_on_a,
      # m_on_az = m_on_az,
      # m_on_anj = m_on_anj,
      # m_on_x = m_on_x, #
      # m_on_z = m_on_z, #
      # quadratic.M = quadratic.M,
      # int.XZ = int.XZ,
      iccy = iccy,
      yintercept = yintercept,
      y_on_a = y_on_a,
      # y_on_m = y_on_m,
      # y_on_am = y_on_am,
      y_on_az = y_on_az,
      # y_on_mz = y_on_mz,
      y_on_anj = y_on_anj,
      y_on_x = y_on_x, #
      y_on_z = y_on_z, #
      quadratic.Y = quadratic.Y,
      Yfamily = Yfamily,
      # Mfamily = Mfamily,
      if.null = if.null,
      clust_trt_prop = data_list$clust_trt_prop
    )
  )

  return(result_data)
}

