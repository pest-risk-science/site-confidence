#' Predict number of pests from trap catch data
#'
#' @description
#' Wrapper around \code{predict(m_gamma_v3_full, ...)} that estimates the number
#' of active pests on a 10 ha site from observed trap catch and surveillance
#' configuration parameters.
#'
#' Rows where \code{ftw1 == 0} cannot be used with the log offset and are
#' returned as \code{NA}. A zero catch does not reliably indicate zero pests.
#' Analysis of the training data shows that approximately 62\% of zero-catch
#' observations correspond to genuinely pest-free sites, but the remaining 38\%
#' are missed detections where pests were present (median \code{num_pests} = 15,
#' 90th percentile = 70). The model cannot distinguish between these cases.
#' Results for \code{ftw1 = 0} rows should be reported as inconclusive rather
#' than as evidence of pest absence.
#'
#' Prediction intervals are derived from the fitted Gamma distribution using the
#' model dispersion parameter. They capture within-model uncertainty only. When
#' the spatial distribution of pests is unknown (i.e. random vs. clustered),
#' an additional ~18% median relative error should be expected beyond the
#' interval bounds — see \code{model_accuracy.md} for details.
#'
#' @param newdata A data frame containing the following columns:
#'   \describe{
#'     \item{ftw1}{Pests per trap per week observed in week 1 (numeric, >= 0).}
#'     \item{n_traps}{Number of traps deployed (integer: 1, 3, 5, or 10).}
#'     \item{step_size}{Mean daily pest step size in metres (numeric).
#'       Model was trained on values 5, 20, 43, 50, 62.6. Predictions outside
#'       this range are extrapolations and carry higher uncertainty.}
#'     \item{lure_attract}{Lure attractiveness radius in metres (numeric: 7, 14,
#'       25, 36, or 50).}
#'   }
#' @param model A fitted Gamma GLM object. Defaults to \code{m_gamma_v3_full}.
#' @param interval Logical. If \code{TRUE} (default), return prediction interval
#'   columns alongside the point estimate.
#' @param level Numeric. Confidence level for the prediction interval.
#'   Defaults to 0.95.
#'
#' @return A data frame with \code{nrow(newdata)} rows and columns:
#'   \describe{
#'     \item{est}{Point estimate of \code{num_pests}.}
#'     \item{lower}{Lower bound of the prediction interval (\code{NA} if
#'       \code{interval = FALSE} or \code{ftw1 == 0}).}
#'     \item{upper}{Upper bound of the prediction interval (\code{NA} if
#'       \code{interval = FALSE} or \code{ftw1 == 0}).}
#'   }
#'   Rows where \code{ftw1 == 0} have \code{NA} for all columns.
#'
#' @examples
#' newdata <- data.frame(
#'   ftw1         = c(0, 5.2, 18.0, 42.5),
#'   n_traps      = c(3,  3,   5,    10),
#'   step_size    = c(43, 43,  43,   43),
#'   lure_attract = c(25, 25,  25,   25)
#' )
#' predict_num_pests(newdata)
#'
#' @export
predict_num_pests <- function(newdata,
                               model    = m_gamma_v3_full,
                               interval = TRUE,
                               level    = 0.95) {

  # --- Input validation ---
  required_cols <- c("ftw1", "n_traps", "step_size", "lure_attract")
  missing_cols  <- setdiff(required_cols, names(newdata))
  if (length(missing_cols) > 0) {
    stop(
      "newdata is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!inherits(model, "glm")) {
    stop("model must be a fitted glm object.")
  }

  if (level <= 0 || level >= 1) {
    stop("level must be between 0 and 1 (exclusive).")
  }

  # --- Identify unobservable rows ---
  zero_idx <- newdata$ftw1 == 0
  n_zero   <- sum(zero_idx)

  if (n_zero > 0) {
    warning(
      n_zero, " row(s) have ftw1 = 0 and cannot be predicted (NA returned). ",
      "In the training data, ~62% of zero-catch cases had zero pests, but ",
      "~38% were missed detections (median num_pests = 15, 90th pctile = 70). ",
      "Treat these rows as inconclusive, not as evidence of pest absence."
    )
  }

  # --- Initialise output ---
  out <- data.frame(
    est   = NA_real_,
    lower = NA_real_,
    upper = NA_real_
  )[rep(1L, nrow(newdata)), ]
  rownames(out) <- NULL

  # --- Predict on valid rows ---
  valid_idx <- !zero_idx

  if (any(valid_idx)) {
    nd_valid  <- newdata[valid_idx, , drop = FALSE]
    est_valid <- predict(model, newdata = nd_valid, type = "response")
    out$est[valid_idx] <- est_valid

    if (interval) {
      disp  <- summary(model)$dispersion
      shape <- 1 / disp
      alpha <- 1 - level

      out$lower[valid_idx] <- qgamma(alpha / 2,       shape = shape,
                                      rate  = shape / est_valid)
      out$upper[valid_idx] <- qgamma(1 - alpha / 2,   shape = shape,
                                      rate  = shape / est_valid)
    }
  }

  out
}
