#' Fit one large conditional inference tree per species
#'
#' This function fits a single conditional inference tree for each species,
#' using all available ATF predictors simultaneously. Compared to the
#' Singer-style shallow approach, this produces more complex trees but
#' greatly reduces code and model-selection overhead.
#'
#' @param atf data.table or data.frame containing aggregated time-series
#'   features. Must include columns: \code{german}, \code{validation} (0/1),
#'   and all ATF predictors (as produced by \code{calculate_atf}).
#' @param maxdepth integer, maximum depth of the tree (default 5).
#'
#' @return a named list of \code{partykit::ctree} models, one per species.
#' @export
fit_thresholds_big <- function(atf, maxdepth = 5) {
  requireNamespace("partykit")
  requireNamespace("data.table")

  dt <- data.table::as.data.table(atf)
  if (!"validation" %in% names(dt)) {
    stop("Need human validation column: validation (0=FP,1=TP)")
  }

  species <- unique(dt$german)
  preds <- setdiff(names(dt), c("detID","Plot_ID","timestamp","german","validation"))

  models <- list()
  for (sp in species) {
    sub <- dt[german == sp]
    f <- as.formula(paste("validation ~", paste(preds, collapse = "+")))
    mod <- partykit::ctree(
      f, data = sub,
      controls = partykit::ctree_control(maxdepth = maxdepth,
                                         teststat = "max", testtype = "Bonferroni",
                                         alpha = 0.05)
    )
    models[[sp]] <- mod
  }
  models
}
