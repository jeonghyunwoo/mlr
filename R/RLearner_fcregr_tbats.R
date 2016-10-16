#'@export
#' @importFrom xts try.xts reclass
makeRLearner.fcregr.tbats = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.tbats",
    package = "forecast",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "use.box.cox", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "use.trend", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "use.damped.trend", default = FALSE, tunable = TRUE),
      makeUntypedLearnerParam(id = "seasonal.periods", default = NULL),
      makeLogicalLearnerParam(id = "use.arma.errors", default = TRUE, tunable = TRUE),
      makeLogicalLearnerParam(id = "use.parallel", default = FALSE, tunable = TRUE),
      makeNumericLearnerParam(id = "bc.lower", lower = -Inf, upper = Inf, default = 0),
      makeNumericLearnerParam(id = "bc.upper", lower = -Inf, upper = Inf, default = 1),
      makeUntypedLearnerParam(id = "model", default = NULL),
      makeNumericLearnerParam(id = "max.p", lower = 0, upper = Inf, default = 5),
      makeNumericLearnerParam(id = "max.q", lower = 0, upper = Inf, default = 5),
      makeNumericLearnerParam(id = "max.P", lower = 0, upper = Inf, default = 2),
      makeNumericLearnerParam(id = "max.Q", lower = 0, upper = Inf, default = 2),
      makeNumericLearnerParam(id = "max.order", lower = 0, upper = Inf, default = 5),
      makeLogicalLearnerParam(id = "stationary", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "stepwise", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "test", values = c("kpss","adf","pp"), default = "kpss"),
      makeLogicalLearnerParam(id = "allowdrift", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "allowmean", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, tunable = TRUE),
      makeIntegerLearnerParam(id = "h", lower = 0, upper = Inf, default = 1, tunable = FALSE,
                              when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, tunable = FALSE, when = "predict"),
      makeUntypedLearnerParam(id = "level", default = c(80,95), when = "predict"),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict")
    ),
    properties = c("numerics","ts", "quantile"),
    name = "Exponential smoothing state space model with Box-Cox transformation,
    ARMA errors, Trend and Seasonal Fourier components",
    short.name = "tbats",
    note = ""
  )
}

#'@export
trainLearner.fcregr.tbats = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::tbats(y = data$target, ...)
}

#'@export
predictLearner.fcregr.tbats = function(.learner, .model, .newdata, ...){
  se.fit = .learner$predict.type == "quantile"
  if (!se.fit){
    p = as.numeric(forecast::forecast(.model$learner.model, ...)$mean)
  } else {
    pse = forecast::forecast(.model$learner.model, ...)
    pMean  = as.matrix(pse$mean)
    pLower = pse$lower
    pUpper = pse$upper
    colnames(pMean)  = "point_forecast"
    colnames(pLower) = paste0("lower_",pse$level)
    colnames(pUpper) = paste0("upper_",pse$level)
    p = cbind(pMean,pLower,pUpper)
  }
  return(p)
}
