#' @title generate_mediator
#'
#' @description
#' Generates an individual-level mediator variable (\code{M}) in a clustered setting
#' that is either binomial (`Mfamily` = "binomial") or gaussian (`Mfamily` = "gaussian"),
#' influenced by treatment, covariates, cluster-level confounder (\code{Z}), and
#' interactions between treatment and cluster size as well as between treatment
#' and cluster-level confounder.
#'
#' @param data_list A list containing a data frame with variables 'school', 'A', 'Z', 'X', and 'W_nj'.
#' @param nj_sizes Vector of cluster sizes (length equals the number of unique schools).
#' @param iccm Numeric. Intra-class correlation for 'M'.
#' @param num_x Numeric. Number of X covariates.
#' @param m_on_a Numeric. Effect of 'A' on 'M'.
#' @param m_on_az Numeric. Interaction effect of 'A' and 'Z' on 'M'.
#' @param m_on_anj Numeric. Interaction effect of 'A' and cluster size on 'M'.
#' @param m_on_x description
#' @param m_on_z description
#' @param quadratic.M Logical. If TRUE, includes quadratic terms for 'X' in the model for 'M'.
#' @param Mfamily Character. Either "binomial" or "gaussian". Determines the distribution of the mediator 'M'.
#'
#' @return The modified `data_list` with the generated mediator 'M' and associated parameters.
#'
#' @details Additional details about the function, such as implementation notes or edge cases.
#'
#' @importFrom utils modifyList
#' @importFrom stats rbinom
#'
#' @seealso [pm1()]

generate_mediator <- function(data_list, nj_sizes, iccm = 0.2, num_x = 3, m_on_a = 0.2,
                              m_on_az = 0.2, m_on_anj = 0.2,
                              m_on_x = sqrt(0.15 / num_x), m_on_z = sqrt(0.4),
                              quadratic.M = FALSE,
                              # int.XZ = TRUE, ##' @param int.XZ Logical. If FALSE, sets the 'm_on_anj' effect to 0.
                              Mfamily = "binomial") {

    # Input validation for Mfamily
    if (!Mfamily %in% c("binomial", "gaussian")) {
        stop("Mfamily must be either 'binomial' or 'gaussian'")
    }

    gen_m <- list(
        iccm = iccm,         # Intra-class correlation for 'M'
        m_on_a = m_on_a,        # Effect of 'A' on 'M'
        m_on_x = sqrt(0.15 / num_x), # m_on_x, # Effect of 'X' on 'M'
        m_on_z = sqrt(0.4), # m_on_z,  # Effect of 'Z' on 'M'
        m_on_az = m_on_az,       # Interaction effect of 'A' and 'Z' on 'M'
        m_on_anj = m_on_anj      # Interaction effect of 'A' and cluster size on 'M'
    )

    J <- length(unique(data_list$data$school))
    N <- sum(nj_sizes) #N <- nrow(data_list$data)

    # # skipping this part
    # # # If interaction between 'A' and cluster size is not included
    # if (int.XZ == FALSE) {
    #     gen_m[c("m_on_anj")] <- 0 # NOTE: delete
    # }

    # Generate cluster-level random effects for mediators
    mb <- unlist(purrr::map(1:J, ~rep(stats::rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm"]])), each = nj_sizes[.x])))

    # Generate mediator 'M'
    if (quadratic.M == TRUE) {
        # Include quadratic terms if specified
        Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)

        # Compute the linear predictor for 'M1'
        m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xquad) +
            gen_m[["m_on_z"]] * data_list$data$Z
    } else {
        # Linear terms only
        Xlinear <- data_list$data$X

        # Compute the linear predictor for 'M1'
        m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xlinear) +
            gen_m[["m_on_z"]] * data_list$data$Z # m_latent relabel
    }

    # Generate mediator values
    if (Mfamily == "binomial") {
        # # try plogis
        # latent <- gen_m[["m_on_a"]] * data_list$data$A +
        #     gen_m[["m_on_az"]] * data_list$data$A * data_list$data$Z +
        #     gen_m[["m_on_anj"]] * data_list$data$A * data_list$data$W_nj +
        #     m_given
        # prob1 <- plogis(latent)
        # data_list$data$M <- rbinom(N, size = 1, prob = prob1 + rnorm(N, 0, sd = sqrt(1 - gen_m[["iccm"]])))

        #
        probm <- pm1(1, data_list$data$A, data_list$data$Z, data_list$data$W_nj, m_given, gen_m)
        data_list$data$M <- stats::rbinom(N, size = 1, prob = probm)

        # latent <- gen_m[["m_on_a"]] * data_list$data$A + gen_m[["m_on_az"]] * data_list$data$A*data_list$data$Z + gen_m[["m_on_anj"]] * data_list$data$A*data_list$data$W_nj + m_given
        # prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
        # # m * prob1 + (1 - m) * (1 - prob1) # m = 1 so can drop
        # data_list$data$M <- rbinom(N, 1, prob = prob1)
    } else if (Mfamily == "gaussian") {
        # mean_m <- m_given # m_latent
        mean_m <- m_given +
            gen_m[["m_on_a"]] * data_list$data$A +
            gen_m[["m_on_az"]] * data_list$data$A * data_list$data$Z +
            gen_m[["m_on_anj"]] * data_list$data$A * data_list$data$W_nj

        data_list$data$M <- stats::rnorm(N, mean = mean_m, sd = sqrt(1 - gen_m$iccm))
    }

    modifyList(data_list, list(
        iccm = iccm,
        m_on_a = gen_m[["m_on_a"]],
        m_on_x = gen_m[["m_on_x"]],
        m_on_z = gen_m[["m_on_z"]],
        m_on_az = gen_m[["m_on_az"]],
        m_on_anj = gen_m[["m_on_anj"]],
        quadratic.M = quadratic.M,
        Mfamily = Mfamily,
        m_given = m_given
    ))
}


##################################### END ######################################
