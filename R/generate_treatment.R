# CURRENTLY UPDATING: 2025-07-10
# # Might need to update:
#   # documentation
#   # output of a_x & a_z to a_on_x & a_on_z
# #

# Generate Treatment Assignments
#' @title generate_treatment
#'
#' @description
#' Generates a binary, individual-level treatment (\code{A}) in a clustered setting
#' either by using a propensity score (default; influenced by \code{Z} and \code{X})
#' or by completely random assignment (when randomize = \code{TRUE}; i.e., randomized experiment).
#'
#' @param data_list A list containing at least the element \code{data}, which is
#'   a \code{data.frame} with variables \code{X}, \code{Z}, and \code{school}.
#' @param nj_sizes A numeric vector of length \code{J} (number of schools)
#'   indicating the number of individuals in each school.
#' @param icca A numeric value for the intra-class correlation for \code{A}.
#' @param quadratic.A Logical. If \code{TRUE}, quadratic terms for \code{X}
#'   (transformed as (\code{X}^2 - 1)/sqrt(4)) are used in the treatment model.
#'   Default is \code{FALSE}.
#' @param a_on_x Numeric; coefficient on \code{rowSums(X)} in the treatment model.
#' @param a_on_z Numeric; coefficient on \code{Z} in the treatment model.
#' @param num_x Number of columns in \code{X}. Default is 3.
#' @param randomize Logical. If \code{TRUE}, assignments are completely
#'   randomized with probability 0.5 for treatment vs control, overriding the
#'   model-based propensity score. Default is \code{FALSE}.
#'
#' @return Returns \code{data_list} with updated elements:
#' \itemize{
#'    \item \code{data$A}: treatment assignments
#'    \item \code{data$ps_true}: true propensity scores
#'    \item \code{a_on_x}, \code{a_on_z}: the coefficients used to generate
#'    \item \code{icca}, \code{quadratic.A}: the intra-correlation coefficient for treatment and whether pretreatment covariates are quadraticly related to A
#'    \item \code{clust_trt_prop}: cluster-level treatment proportions
#' }
#'
#' @importFrom utils modifyList
#' @importFrom stats pnorm rbinom

generate_treatment <- function(data_list,
                               nj_sizes,
                               icca,
                               quadratic.A = FALSE,
                               a_on_x = sqrt(0.05625 / 3), #a_x = sqrt(0.05625 / 3),
                               a_on_z = sqrt(0.15 / 1), #a_z = sqrt(0.15 / 1),
                               num_x = 3,
                               randomize = FALSE) {

    # If randomize is TRUE, generate random A (like a randomized experiment)
    if (isTRUE(randomize)) {

        N <- nrow(data_list$data)
        # Assign each individual to treatment with probability 0.5
        data_list$data$A <- stats::rbinom(N, 1, 0.5)

        # Set the true propensity score to 0.5 for everyone in the randomized scenario
        data_list$data$ps_true <- rep(0.5, N)

        # For consistency, return placeholders (or NA) for a_x, a_z
        # or keep them the same as the default.
        # Here we just store NA since they're not used.
        return(
            modifyList(data_list, list(
                a_on_x = NA_real_, #a_x = NA_real_,
                a_on_z = NA_real_, #a_z = NA_real_,
                icca = icca,
                quadratic.A = quadratic.A
            ))
        )

    } else {

        # --- Original logic: Generate A based on a model with cluster effects ---

        # Store generation parameters
        gen_a <- list(
            icca = icca,

            a_on_x = a_on_x, #a_x = a_x,
            a_on_z = a_on_z #a_z = a_z
            # a_x = sqrt(0.05625 * 1 / num_x), #sqrt(0.075 * 1 / num_x), #sqrt(0.15 * 1 / num_x),
            # a_z = sqrt(0.15 / 1) #sqrt(0.2 / 1) #sqrt(0.4 / 1)
        )

        J <- length(unique(data_list$data$school))
        N <- nrow(data_list$data)

        # Generate cluster-level random effects for 'A'
        ab <- unlist(
            purrr::map(seq_len(J), ~rep(stats::rnorm(1, mean = 0, sd = sqrt(gen_a[["icca"]])),
                                        each = nj_sizes[.x]))
        )

        # Compute the linear predictor for 'A'
        if (!quadratic.A) {
            Xlinear <- as.matrix(data_list$data$X) #Xlinear <- data_list$data$X
            a_given <- ab + gen_a[["a_on_x"]] * rowSums(Xlinear) + gen_a[["a_on_z"]] * data_list$data$Z
        } else {
            # Include quadratic terms if specified
            Xquad <- (as.matrix(data_list$data$X)^2 - 1) / sqrt(4) #Xquad <- (data_list$data$X^2 - 1) / sqrt(4)
            a_given <- ab + gen_a[["a_on_x"]] * rowSums(Xquad) + gen_a[["a_on_z"]] * data_list$data$Z
        }

        # Compute the true propensity score
        ps_true <- stats::pnorm(a_given, mean = 0, sd = sqrt(1 - gen_a$icca))

        # Generate binary treatment 'A' using the true propensity score
        data_list$data$A <- stats::rbinom(N, 1, ps_true)

        # Add true propensity score to data
        data_list$data$ps_true <- ps_true

        # Return updated data_list with parameters used
        return(
            modifyList(data_list, list(
                a_on_x = gen_a[["a_on_x"]],
                a_on_z = gen_a[["a_on_z"]],
                icca = icca,
                quadratic.A = quadratic.A,
                clust_trt_prop = tapply(data_list$data$A, data_list$data$school, mean, na.rm = TRUE)
            ))
        )
    }
}
