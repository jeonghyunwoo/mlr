
mase.fun = function(task, model, pred, feats, extra.args) {
  Tn = nrow(getTaskData(task))
  top_error   = sum(abs(getPredictionTruth(pred) - getPredictionResponse(pred)))
  bottom_diff = (Tn / (Tn - 1)) * sum(abs(diff(getTaskTargets(task))))
  top_error / bottom_diff
}

#' @export mase
#' @rdname measures
#' @usage  none
#' @format none
mase = makeMeasure(
  id = "mase",
  minimize = TRUE,
  name = "Mean Absolute Scaled Error",
  properties = c("regr", "req.pred", "req.truth", "req.task"),
  best = 0,
  worst = Inf,
  fun = mase.fun
)
