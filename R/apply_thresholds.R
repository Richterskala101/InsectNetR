#' Apply species-specific threshold models to ATF detections
#'
#' This function filters detections based on species-specific threshold rules,
#' either from the shallow Singer-style models (\code{fit_thresholds_shallow})
#' or from big-tree models (\code{fit_thresholds_big}).
#'
#' @param atf data.table or data.frame containing aggregated time-series
#'   features, including columns \code{detID}, \code{german}, \code{conf}, etc.
#'   as produced by \code{calculate_atf()}.
#' @param thresholds Either:
#'   - a \code{data.table} of threshold rules from \code{fit_thresholds_shallow()},
#'   - or a named list of \code{partykit::ctree} objects from \code{fit_thresholds_big()}.
#' @param type character, one of \code{"shallow"} or \code{"big"}, indicating
#'   the format of \code{thresholds}.
#'
#' @return data.table of detections with an additional column \code{keep}
#'   indicating whether the detection passed the thresholding.
#' @export
apply_thresholds <- function(atf, thresholds, type = c("shallow","big")) {
  requireNamespace("data.table")
  requireNamespace("partykit")

  dt <- data.table::as.data.table(atf)
  type <- match.arg(type)

  if (type == "shallow") {
    if (!all(c("german","rules") %in% names(thresholds))) {
      stop("Thresholds must come from fit_thresholds_shallow()")
    }

    dt[, keep := FALSE]

    for (sp in unique(thresholds$german)) {
      rules <- thresholds[german == sp, rules][1]  # first rule
      if (is.na(rules) || rules == "") next

      # Build logical expression
      expr <- parse(text = rules)

      # Apply within-species
      idx <- which(dt$german == sp)
      dt$keep[idx] <- with(dt[idx,], eval(expr))
    }
  }

  if (type == "big") {
    if (!is.list(thresholds) || !all(sapply(thresholds, inherits, "BinaryTree"))) {
      stop("Thresholds must be a list of ctree models from fit_thresholds_big()")
    }

    dt[, keep := FALSE]

    for (sp in names(thresholds)) {
      mod <- thresholds[[sp]]
      if (is.null(mod)) next
      sub <- dt[german == sp]
      if (nrow(sub) == 0) next

      preds <- predict(mod, newdata = sub, type = "response")
      dt[german == sp, keep := (preds == 1)]
    }
  }

  data.table::setorder(dt, german, Plot_ID, timestamp)
  dt
}
