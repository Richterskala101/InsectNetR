#' Fit shallow conditional inference tree thresholds (Singer et al. 2024)
#'
#' This function reimplements the "shallow tree" thresholding approach from
#' Singer et al. (2024). It fits species-specific conditional inference trees
#' with max depth 1 (type 1 models) and max depth 2 (type 2 models) using
#' all aggregated time-series features. Candidate models are then evaluated
#' by precision, recall, and a weighted performance metric.
#'
#' @param atf data.table or data.frame containing aggregated time-series
#'   features. Must include columns: \code{german}, \code{validation} (0/1),
#'   and all ATF predictors (as produced by \code{calculate_atf}).
#' @param w numeric, weighting factor for model performance. Default 0.75
#'   (precision weighted 3x higher than recall).
#' @param parallel logical, whether to use parallel processing (default TRUE).
#' @param max_cores integer, maximum number of cores for parallel processing
#'   (default: \code{parallel::detectCores()}).
#'
#' @return data.table of candidate threshold models per species with columns:
#'   \code{german}, \code{model}, \code{rules}, \code{prec}, \code{recall},
#'   \code{model_performance}.
#'
#' @references
#' Singer, D., Hagge, J., Kamp, J., Hondong, H. & Schuldt, A. (2024) Aggregated time-series features boost species-specific differentiation of true and false positives in passive acoustic monitoring of bird assemblages, Remote Sensing in Ecology and Conservation, https://doi.org/10.1002/rse2.385
#'
#' @export
fit_thresholds_shallow <- function(atf, w = 0.75,
                                   parallel = TRUE,
                                   max_cores = parallel::detectCores()) {
  requireNamespace("partykit")
  requireNamespace("data.table")
  requireNamespace("dplyr")

  dt <- data.table::as.data.table(atf)
  if (!"validation" %in% names(dt)) {
    stop("Need human validation column: validation (0=FP,1=TP)")
  }

  species <- unique(dt$german)
  preds <- setdiff(names(dt), c("detID","Plot_ID","timestamp","german","validation"))

  # parallel setup
  if (parallel) {
    cl <- parallel::makeCluster(max_cores)
    doParallel::registerDoParallel(cl)
  }

  # type 1 models (maxdepth = 1)
  ctrl1 <- partykit::ctree_control(teststat = "max", testtype = "Bonferroni",
                                   maxdepth = 1, alpha = 0.05)

  results1 <- foreach::foreach(sp = species, .combine = rbind) %:%
    foreach::foreach(p = preds, .combine = rbind) %dopar% {
      sub <- dt[german == sp]
      f <- as.formula(paste("validation ~", p))
      mod <- partykit::ctree(f, data = sub, controls = ctrl1)
      rules <- as.data.frame(partykit:::.list.rules.party(mod))
      if (nrow(rules) == 0) return(NULL)
      # compute precision/recall
      out <- data.frame(
        german = sp,
        model = p,
        prec = mean(mod@predict_response == sub$validation, na.rm = TRUE),
        recall = sum(mod@predict_response == 1 & sub$validation == 1) /
          sum(sub$validation == 1),
        rules = paste(rules$rules, collapse = "; ")
      )
      out
    }

  # type 2 models (maxdepth = 2)
  ctrl2 <- partykit::ctree_control(teststat = "max", testtype = "Bonferroni",
                                   maxdepth = 2, alpha = 0.05)

  combos <- utils::combn(preds, 2, simplify = FALSE)
  results2 <- foreach::foreach(sp = species, .combine = rbind) %:%
    foreach::foreach(pair = combos, .combine = rbind) %dopar% {
      sub <- dt[german == sp]
      f <- as.formula(paste("validation ~", paste(pair, collapse = "+")))
      mod <- partykit::ctree(f, data = sub, controls = ctrl2)
      rules <- as.data.frame(partykit:::.list.rules.party(mod))
      if (nrow(rules) == 0) return(NULL)
      out <- data.frame(
        german = sp,
        model = paste(pair, collapse = "+"),
        prec = mean(mod@predict_response == sub$validation, na.rm = TRUE),
        recall = sum(mod@predict_response == 1 & sub$validation == 1) /
          sum(sub$validation == 1),
        rules = paste(rules$rules, collapse = "; ")
      )
      out
    }

  if (parallel) parallel::stopCluster(cl)

  models <- dplyr::bind_rows(results1, results2) %>%
    dplyr::mutate(model_performance = prec * w + recall * (1 - w)) %>%
    dplyr::group_by(german) %>%
    dplyr::slice_max(order_by = model_performance, n = 1)

  data.table::as.data.table(models)
}
