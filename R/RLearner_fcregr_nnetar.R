#'@export
makeRLearner.fcregr.nnetar = function() {
  makeRLearnerRegr(
    cl = "fcregr.nnetar",
    package = "forecast",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "p", lower = 0L, default = 1L),
      makeIntegerLearnerParam(id = "P", lower = 0L, default = 1L),
      makeIntegerLearnerParam(id = "size", lower = 0L, default = 1L),
      makeIntegerLearnerParam(id = "repeats", lower = 1L, default = 20L),
      makeUntypedLearnerParam(id = "xreg", default = NULL, tunable = FALSE),
      makeNumericLearnerParam(id = "lambda", default = 1),
      makeUntypedLearnerParam(id = "model", default = NULL),
      makeLogicalLearnerParam(id = "scale.inputs", default = TRUE),
      makeIntegerLearnerParam(id = "h", lower = 0L, default = 1L, when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, tunable = FALSE, when = "predict"),
      makeUntypedLearnerParam(id = "level", default = c(80,95), when = "predict"),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict")
      ),
    properties = c("numerics","ts"),
    name = "Neural Network Time Series Forecasts",
    short.name = "nnetar",
    note = ""
  )
}
#'@export
trainLearner.fcregr.nnetar = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::nnetar(y = data$target, ...)
}

#'@export
predictLearner.fcregr.nnetar = function(.learner, .model, .newdata, ...){
  se.fit = .learner$predict.type == "se"
  if (!se.fit){
    p = as.numeric(forecast::forecast(.model$learner.model, ...)$mean)
  } else {
    pse = forecast::forecast(.model$learner.model, ...)
    pMean  = as.matrix(pse$mean)
    pLower = pse$lower
    pUpper = pse$upper
    colnames(pMean)  = "point_forecast"
    colnames(pLower) = paste0("lower_",colnames(pLower))
    colnames(pUpper) = paste0("upper_",colnames(pUpper))
    p = cbind(pMean,pLower,pUpper)
  }
  return(p)
}

