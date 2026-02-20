#' @title trueVals_noMed
#'
#' @description
#' Computes the true total treatment effect (TTE) of \code{A} on \code{Y}, at
#' both the individual and cluster levels. Called internally by
#' \code{generate_data_noMed()} when \code{include_truevals = TRUE}.
#'
#' @param data_list A list produced by \code{generate_data_noMed()} (before final output
#'   assembly), containing \code{data}, \code{y_given}, and all generation parameters
#'   as top-level elements (e.g., \code{y_on_a}, \code{y_on_az}, \code{iccy}, \code{Yfamily}).
#' @param Yfamily Character. Family for the outcome (\code{"gaussian"} or \code{"binomial"}). Defaults to \code{"binomial"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{E_Y1_individual}}{True expected outcome under \code{A = 1}, averaged over individuals.}
#'   \item{\code{E_Y0_individual}}{True expected outcome under \code{A = 0}, averaged over individuals.}
#'   \item{\code{TTE_individual}}{True individual-level TTE: \code{E[Y(1)] - E[Y(0)]}.}
#'   \item{\code{TTE_cluster}}{True TTE averaged at the cluster level (mean of within-cluster TTEs).}
#'   \item{\code{cluster_level_tte}}{A data frame with per-cluster TTE values.}
#' }
#'
#'
#' @details
#' This function calculates the expected outcomes under the two potential-outcome scenarios:
#' \eqn{Y(1)} and \eqn{Y(0)}. The TTE at the individual level is \code{mean(E[Y(1)] - E[Y(0)])} across all
#' individuals; the cluster-level TTE averages the individual-level TTEs within each
#' cluster, then averages across clusters.
#'
#' @examples
#' \dontrun{
#' # Typically called internally in generate_data_noMed()
#' result <- generate_data_noMed(J = 50, Yfamily = "binomial", include_truevals = TRUE)
#' result$truevals$TTE_individual
#' result$truevals$TTE_cluster
#' }
#'
#' @import dplyr
#' @importFrom stats pnorm
#'
#' @seealso \code{\link{generate_data_noMed}}
trueVals_noMed <- function(data_list,
                           Yfamily
                         # from_GenData_2.0 = FALSE # added argument to easily change GenData_2.0() data to this func
) {



  # 1. Extract quantities from data_list & generate gen_y -------------------
  data <- data_list$data
  # nj_sizes <- data_list$nj_sizes
  Yfamily <- data_list$Yfamily
  iccy <- data_list$iccy
  seed <- data_list$seed
  iccx <- data_list$iccx
  x_z <- data_list$x_z
  num_x <- data_list$num_x

  # Extract outcome parameters
  gen_y <- list(
    iccy = data_list$iccy,
    yintercept = data_list$yintercept,
    y_on_a = data_list$y_on_a,
    y_on_m = 0,
    y_on_am = 0,
    y_on_az = data_list$y_on_az,
    y_on_mz = 0,
    y_on_anj = data_list$y_on_anj,
    y_on_x = data_list$y_on_x,
    y_on_z = data_list$y_on_z
  )

  y_given <- data_list$y_given

  # # Attach cluster sizes to the data
  # data <- data |> dplyr::mutate(nj = nj_sizes[school])
  # Use scaled version of cluster size (W_nj) instead of raw
  W_nj <- data_list$data$W_nj

  # 2. Helper function(s) ---------------------------------------------------

  # Calculate latent outcome Y with M = 0 (no mediator)
  calc_y_latent <- function(a, z, W_nj, given, Yfamily) {
    latent_y <- gen_y$y_on_a * a +
      gen_y$y_on_az * a * z +
      gen_y$y_on_anj * a * W_nj +
      given

    if (Yfamily == "binomial") {
      stats::pnorm(latent_y, mean = 0, sd = sqrt(1 - iccy))
    } else {
      latent_y
    }
  }


  # 3.  ---------------------------------------------------------------------

  E_y1 <- calc_y_latent(a = 1, z = data$Z, W_nj = W_nj, given = y_given, Yfamily = Yfamily)
  E_y0 <- calc_y_latent(a = 0, z = data$Z, W_nj = W_nj, given = y_given, Yfamily = Yfamily)

  # Individual-level average
  tte_individual <- mean(E_y1 - E_y0, na.rm = TRUE)

  # Cluster-level average
  cluster_tte <- data |>
    dplyr::mutate(tte_i = E_y1 - E_y0) |>
    dplyr::group_by(school) |>
    dplyr::summarize(cluster_tte = mean(tte_i, na.rm = TRUE))


  # 4. Return true values ---------------------------------------------------

  return(
    list(
      E_Y1_individual = mean(E_y1),
      E_Y0_individual = mean(E_y0),
      TTE_individual = tte_individual,
      TTE_cluster = mean(cluster_tte$cluster_tte),
      cluster_level_tte = cluster_tte
    )
  )


#
#
#
#   # 1. Extract components from data_list ------------------------------------
#   if (from_GenData_2.0 == TRUE) {
#     data <- data_list$datobs
#     # Change X headers
#     colnames(data) <- gsub(pattern = "^X\\.", replacement = "X", colnames(data))
#
#     nj_sizes <- data_list$nj_sizes
#     Mfamily <- "binomial"    # <- ADJUST LATER
#     Yfamily <- data_list$Yfamily
#     iccm <- data_list$iccm
#     iccy <- data_list$iccy
#     seed <- data_list$seedone
#     iccx <- data_list$iccx
#     x_z <- data_list$x_z
#     num_x <- data_list$num_x
#
#     # Extract mediator parameters
#     gen_m <- list(
#       iccm = iccm,
#       m_on_a = data_list$gen_m$m1_on_a,
#       m_on_x = data_list$gen_m$m1_on_x,
#       m_on_z = data_list$gen_m$m1_on_z,
#       m_on_az = data_list$gen_m$m1_on_az,
#       m_on_anj = data_list$gen_m$m1_on_anj
#     )
#
#     # Extract outcome parameters
#     gen_y <- list(
#       iccy = iccy,
#       yintercept = data_list$gen_y$yintercept,
#       y_on_a = data_list$gen_y$y_on_a,
#       y_on_m = data_list$gen_y$y_on_m1,
#       y_on_am = data_list$gen_y$y_on_am1,
#       y_on_az = data_list$gen_y$y_on_az,
#       y_on_mz = data_list$gen_y$y_on_m1z,
#       y_on_anj = data_list$gen_y$y_on_anj,
#       y_on_x = data_list$gen_y$y_on_x,
#       y_on_z = data_list$gen_y$y_on_z
#     )
#
#     m_given <- data_list$m1_given
#     y_given <- data_list$y_given
#
#     # # Attach cluster sizes to the data
#     # data <- data |> dplyr::mutate(nj = nj_sizes[school])
#     # Use scaled version of cluster size (W_nj) instead of raw
#     W_nj <- data_list$data$W_nj
#
#   }
#   if (from_GenData_2.0 == FALSE) {
#
#     data <- data_list$data
#     nj_sizes <- data_list$nj_sizes
#     Mfamily <- data_list$Mfamily
#     Yfamily <- data_list$Yfamily
#     iccm <- data_list$iccm
#     iccy <- data_list$iccy
#     seed <- data_list$seed
#     iccx <- data_list$iccx
#     x_z <- data_list$x_z
#     num_x <- data_list$num_x
#
#     # Extract mediator parameters
#     gen_m <- list(
#       iccm = data_list$iccm,
#       m_on_a = data_list$m_on_a,
#       m_on_x = data_list$m_on_x,
#       m_on_z = data_list$m_on_z,
#       m_on_az = data_list$m_on_az,
#       m_on_anj = data_list$m_on_anj
#     )
#
#     # Extract outcome parameters
#     gen_y <- list(
#       iccy = data_list$iccy,
#       yintercept = data_list$yintercept,
#       y_on_a = data_list$y_on_a,
#       y_on_m = data_list$y_on_m,
#       y_on_am = data_list$y_on_am,
#       y_on_az = data_list$y_on_az,
#       y_on_mz = data_list$y_on_mz,
#       y_on_anj = data_list$y_on_anj,
#       y_on_x = data_list$y_on_x,
#       y_on_z = data_list$y_on_z
#     )
#
#     m_given <- data_list$m_given
#     y_given <- data_list$y_given
#
#     # # Attach cluster sizes to the data
#     # data <- data |> dplyr::mutate(nj = nj_sizes[school])
#     # Use scaled version of cluster size (W_nj) instead of raw
#     W_nj <- data_list$data$W_nj
#   }
#
#
#   # 2. Helper functions -----------------------------------------------------
#
#   # Calculate latent mean for M
#   calc_m_latent <- function(a, z, W_nj, given) {
#     latent_m <- gen_m$m_on_a * a +
#       gen_m$m_on_az * a * z +
#       gen_m$m_on_anj * a * W_nj +
#       given
#     return(latent_m)
#   }
#
#   # Calculate latent outcome Y
#   calc_y_latent <- function(m, a, z, W_nj, given) {
#     latent_y <- gen_y$y_on_m * m +
#       gen_y$y_on_a * a +
#       gen_y$y_on_am * a * m +
#       gen_y$y_on_az * a * z +
#       gen_y$y_on_mz * m * z +
#       gen_y$y_on_anj * a * W_nj +
#       given
#     return(latent_y)
#   }
#
#
#   # 3. Function to compute E[Y(a0, gm(a1))] for each observation ------------
#   compute_expected_y <- function(a0_val, a1_val, data) {
#     z <- data$Z
#     W_nj <- data$W_nj
#     given_m <- m_given
#     given_y <- y_given
#
#     # Compute mediator parameters
#     m_latent <- calc_m_latent(a = a1_val, z = z, W_nj = W_nj, given = given_m)
#     # add error term to m_latent for mediator
#
#     if (Mfamily == "binomial") {
#       # ══════════════════════════════
#       # Binary mediator:
#       # p(M=1) = pnorm(m_latent)
#       # p(M=0) = 1 - p(M=1)
#       # E[Y] = p(M=0)*E[Y|M=0] + p(M=1)*E[Y|M=1]
#       # ══════════════════════════════
#       p_m1 <- stats::pnorm(m_latent, mean = 0, sd = sqrt(1 - iccm))
#       p_m0 <- 1 - p_m1
#
#       # For each M = 0/1, compute latent Y
#       y_latent_m0 <- calc_y_latent(m = 0, a = a0_val, z = z, W_nj = W_nj, given = given_y)
#       y_latent_m1 <- calc_y_latent(m = 1, a = a0_val, z = z, W_nj = W_nj, given = given_y)
#
#       # Then get E[Y|M=0], E[Y|M=1] depending on outcome family
#       if (Yfamily == "binomial") {
#         E_y_m0 <- stats::pnorm(y_latent_m0, 0, sqrt(1 - iccy))
#         E_y_m1 <- stats::pnorm(y_latent_m1, 0, sqrt(1 - iccy))
#       } else {
#         # If Y is gaussian
#         E_y_m0 <- y_latent_m0
#         E_y_m1 <- y_latent_m1
#       }
#
#       E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
#
#     } else {
#       # ══════════════════════════════
#       # Continuous mediator:
#       # M ~ Normal(m_latent, var=1 - iccm)
#       # We must integrate or do a linear approximation
#       # ══════════════════════════════
#       # # var_m <- 1 - iccm # delete if var_m is not used anywhere
#       # Ma <- m_latent + rnorm(length(m_latent), sd = sqrt(1 - iccm)) # added error term
#       #
#       # if (Yfamily == "gaussian") {
#       #     # Continuous M, continuous Y => linear
#       #     # E[Y] = E[E[Y|M]] = E[y_latent(M)]
#       #     y_latent_mean_m <- calc_y_latent(m = Ma, a = a0_val, z = z, W_nj = W_nj, given = given_y)
#       #     E_y <- y_latent_mean_m #+ rnorm(length(m_latent), sd = sqrt(1 - iccy))
#       # } else {
#       #     # Continous M, binary Y => handled later (next step)
#       #     E_y <- NULL
#       # }
#
#
#       # Instead of adding one random draw per individual, I will draw multiple times from the M distribution & average over the draws
#       if (Yfamily == "gaussian") {
#         # m_latent
#         # number of draws done multiple times from the distribution of M
#         num_draws <- 1000
#         N <- nrow(data) #test_list$data)
#         # Create matrix of repeated draws from M distribution for each individual i
#         # set.seed(datseeds[iseed])
#         M_draws <- sapply(1:num_draws, function(k) {
#           stats::rnorm(N, mean = m_latent, sd = sqrt(1 - gen_m$iccm)) #test_list$parameters$iccm))
#         })
#         # For each individual i & draw k, we compute the latent Y under A=0 (stored in y_latent_mat)
#         y_latent_mat <- matrix(NA, nrow = N, ncol = num_draws)
#         for (k in seq_len(num_draws)) {
#           y_latent_mat[, k] <- calc_y_latent(
#             m = M_draws[, k],
#             a = a0_val,
#             z = z, # test_list$data$Z,
#             W_nj = W_nj, # test_list$data$W_nj,
#             given = given_y # test_list$parameters$y_given
#           )
#         }
#         # 4) Because Y is continuous normal with intraclass correlation iccy,
#         #    the final outcome is Y_latent + N(0, 1 - iccy).
#         #    But the *expected value* of Y is just Y_latent, so we do not
#         #    add a random draw for Y if we want E[Y].
#         #    The expected outcome for each individual i is the average across draws k.
#         E_y <- rowMeans(y_latent_mat) # E_Y_each_subj <- rowMeans(y_latent_mat)
#         # overall E[Y(a0=0, gm(a1=1))]
#         # mean(E_Y_each_subj)
#
#       } else if(Yfamily == "binomial") {
#         # Continous M, binary Y => handled later (next step)
#         # E_y <- NULL
#
#         # Trying same approach as gaussian outcome
#         # m_latent
#         # number of draws done multiple times from the distribution of M
#         num_draws <- 1000
#         N <- nrow(data) #test_list$data)
#         # Create matrix of repeated draws from M distribution for each individual i
#         # set.seed(datseeds[iseed])
#         M_draws <- sapply(1:num_draws, function(k) {
#           stats::rnorm(N, mean = m_latent, sd = sqrt(1 - gen_m$iccm)) #test_list$parameters$iccm))
#         })
#         # For each individual i & draw k, we compute the latent Y under A=0 (stored in y_latent_mat)
#         y_latent_mat <- matrix(NA, nrow = N, ncol = num_draws)
#         for (k in seq_len(num_draws)) {
#           y_latent_mat[, k] <- calc_y_latent(
#             m = M_draws[, k],
#             a = a0_val,
#             z = z, # test_list$data$Z,
#             W_nj = W_nj, # test_list$data$W_nj,
#             given = given_y # test_list$parameters$y_given
#           )
#           # y_latent_mat[, k] <- pnorm(y_latent_mat[, k], mean = 0, sd = sqrt(1 - iccy))
#         }
#         # 4) Because Y is continuous normal with intraclass correlation iccy,
#         #    the final outcome is Y_latent + N(0, 1 - iccy).
#         #    But the *expected value* of Y is just Y_latent, so we do not
#         #    add a random draw for Y if we want E[Y].
#         #    The expected outcome for each individual i is the average across draws k.
#         # E_y <- rowMeans(y_latent_mat) # E_Y_each_subj <- rowMeans(y_latent_mat)
#         # overall E[Y(a0=0, gm(a1=1))]
#         # mean(E_Y_each_subj)
#
#         # E_y <- rowMeans(y_latent_mat)
#         # E_y <- pnorm(rowMeans(y_latent_mat), mean = 0, sd = sqrt(1 - iccy))
#
#         E_y <- rowMeans(
#           stats::pnorm(y_latent_mat, mean = 0, sd = sqrt(1 - iccy))
#         )
#
#       } else {
#         stop("Yfamily must be gaussian or binomial")
#       }
#
#
#     }
#
#     return(E_y)
#   }
#
#   # CHeck for null values or missing
#   # if (!is.null(data_list$Mfamily) && !is.null(data_list$Yfamily)) {#
#   #     print("step 1 all good")
#   # } else {
#   #         print(paste0("step 1: Mfamily is NULL: ", is.null(data_list$Mfamily), " & Yfamily is NULL: ", is.null(data_list$Yfamily)))
#   # }
#
#   # -------------------------------------------------------------------------
#   # 4. LOOP OVER (a0, a1) = (0,0), (0,1), (1,0), (1,1) & COMPUTE TRUE EXPECTATIONS
#   # -------------------------------------------------------------------------
#   a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
#   truevals_individual <- list()
#   truevals_cluster <- list()
#
#   for (j in 1:nrow(a_vals)) {
#     a0_val <- a_vals$a0[j]
#     a1_val <- a_vals$a1[j]
#     label <- glue::glue("Y(a0={a0_val}, gm(a1={a1_val}))")
#
#     # ══════════════════════════════
#     # Continuous M, Binary Y => Monte Carlo approximation
#     # done by generating a large population with generate_data2.0c(...).
#     # ══════════════════════════════
#     # if (!is.null(data_list$Mfamily) && !is.null(data_list$Yfamily)) {#
#     # if (Mfamily == "gaussian" & Yfamily == "binomial") {
#     #
#     #     # Generate a large "population" (J=10,000) to approximate integral
#     #     large_J <- 10000
#     #     big_njrange <- c(data_list$njrange[1], data_list$njrange[2])
#     #
#     #     pop_result <- generate_data2.0c(
#     #         include_truevals = FALSE, #
#     #         include_overlapMsg = FALSE,
#     #         J = large_J,
#     #         njrange = big_njrange,
#     #         Mfamily = "gaussian",
#     #         Yfamily = "binomial",
#     #         if.null = FALSE, # CHANGE
#     #         seed = seed,
#     #         num_x = num_x,
#     #         # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
#     #         # a_z = sqrt(0.4 / 1),
#     #         x_z = x_z,
#     #         m_on_a = data_list$m_on_a,
#     #         m_on_az = data_list$m_on_az,
#     #         m_on_anj = data_list$m_on_anj,
#     #         # m_on_x = sqrt(0.15 / num_x),
#     #         # m_on_z = sqrt(0.4),
#     #         # int.XZ = TRUE,       # DELETE
#     #         yintercept = data_list$yintercept,
#     #         y_on_a = data_list$y_on_a,
#     #         y_on_m = data_list$y_on_m,
#     #         y_on_am = data_list$y_on_am,
#     #         y_on_az = data_list$y_on_az,
#     #         y_on_mz = data_list$y_on_mz,
#     #         y_on_anj = data_list$y_on_anj,
#     #         # y_on_x = sqrt(0.15 / num_x),
#     #         # y_on_z = sqrt(0.4),
#     #         quadratic.A = data_list$quadratic.A,
#     #         quadratic.M = data_list$quadratic.M,
#     #         quadratic.Y = data_list$quadratic.Y,
#     #         iccx = data_list$iccx,
#     #         icca = data_list$icca,
#     #         iccm = data_list$iccm,
#     #         iccy = data_list$iccy
#     #     )
#     #
#     #     # # Add the cluster sizes to the newly generated dataset
#     #     # pop_result$data <- pop_result$data |>
#     #     #     dplyr::mutate(nj = nj_sizes[school]) # I believe this is unnecessary since we are usin W_nj
#     #
#     #     # Compute the mediator latent mean for M under (A=a1_val)
#     #     m_mu <- calc_m_latent(
#     #         a = a1_val,
#     #         z = pop_result$data$Z,
#     #         W_nj = pop_result$data$W_nj,
#     #         given = pop_result$parameters$m_given
#     #     )
#     #
#     #     # Compute the latent Y for (A=a0_val, M=m_mu)
#     #     y_latent <- calc_y_latent(
#     #         m = m_mu,
#     #         a = a0_val,
#     #         z = pop_result$data$Z,
#     #         W_nj = pop_result$data$W_nj,
#     #         given = pop_result$parameters$y_given
#     #     )
#     #
#     #     # Convert latent Y
#     #     E_y_each <- pnorm(y_latent, mean = 0, sd = sqrt(1 - iccy))
#     #
#     #     # Individual-level expected outcome:
#     #     truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
#     #
#     #     # Cluster-level expected outcome:
#     #     cluster_means <- pop_result$data  |>
#     #         dplyr::mutate(E_y = E_y_each) |>
#     #         dplyr::group_by(school) |>
#     #         dplyr::summarize(cluster_avg = mean(E_y, na.rm = TRUE))
#     #     truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
#     #
#     # } else {
#     # ══════════════════════════════
#     # If Continuous M, Binary/Continuous Y or
#     # Continuous M, Continuous Y
#     # ══════════════════════════════
#     E_y_each <- compute_expected_y(a0_val, a1_val, data)
#
#     # Individual-level average
#     truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
#
#     # Cluster-level average
#     cluster_means <- data  |>
#       dplyr::mutate(E_y = E_y_each) |>
#       dplyr::group_by(school) |>
#       dplyr::summarize(cluster_avg = mean(E_y, na.rm = TRUE))
#     truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
#   }
#   # }else {
#   #     stop("Mfamily or Yfamily is missing or NULL")
#   # }
#   # }
#
#   # -------------------------------------------------------------------------
#   # 5. RETURN THE TRUE VALUES
#   # -------------------------------------------------------------------------
#   # return(
#   #     list(
#   #         truevals_individual = truevals_individual,
#   #         truevals_cluster    = truevals_cluster
#   #     )
#   # )
#   return(
#     list(
#       truevals_individual = truevals_individual,
#       truevals_cluster    = truevals_cluster,
#       pop_data = if (exists("pop_result") && !is.null(pop_result$data)) pop_result$data else NULL
#     )
#   )
}

