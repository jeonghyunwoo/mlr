#'@export
#' @importFrom xts try.xts reclass
makeRLearner.fcregr.arfima = function() {
  makeRLearnerRegr(
    cl = "fcregr.arfima",
    package = "forecast",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id = "drange", len = 2L, lower = 0, default = c(0,0.5)),
      makeDiscreteLearnerParam(id = "estim", values = c("mle","ls"), default = "mle"),
      makeLogicalLearnerParam(id = "parallel", default = FALSE, tunable = FALSE),
      makeNumericLearnerParam(id = "d", lower = 0, upper = Inf),
      makeNumericLearnerParam(id = "D", lower = 0, upper = Inf),
      makeNumericLearnerParam(id = "lambda", lower = 0),
      makeNumericLearnerParam(id = "max.p", lower = 0, upper = Inf, default = 5),
      makeNumericLearnerParam(id = "max.q", lower = 0, upper = Inf, default = 5),
      makeNumericLearnerParam(id = "max.P", lower = 0, upper = Inf, default = 2),
      makeNumericLearnerParam(id = "max.Q", lower = 0, upper = Inf, default = 2),
      makeNumericLearnerParam(id = "max.order", lower = 0, upper = Inf, default = 5),
      makeNumericLearnerParam(id = "max.d", lower = 0, upper = Inf, default = 2),
      makeNumericLearnerParam(id = "max.D", lower = 0, upper = Inf, default = 1),
      makeNumericLearnerParam(id = "start.p", lower = 0, upper = Inf, default = 2),
      makeNumericLearnerParam(id = "start.q", lower = 0, upper = Inf, default = 2),
      makeNumericLearnerParam(id = "start.P", lower = 0, upper = Inf, default = 1),
      makeNumericLearnerParam(id = "start.Q", lower = 0, upper = Inf, default = 1),
      makeLogicalLearnerParam(id = "stationary", default = FALSE, tunable = TRUE),
      makeLogicalLearnerParam(id = "stepwise", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "seasonal", default = TRUE, tunable = TRUE),
      makeDiscreteLearnerParam(id = "ic", values = c("aicc","aic","bic"), default = "aicc"),
      makeDiscreteLearnerParam(id = "test", values = c("kpss","adf","pp"), default = "kpss"),
      makeLogicalLearnerParam(id = "allowdrift", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "allowmean", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, tunable = TRUE),
      makeNumericLearnerParam(id = "h", lower = 0, upper = Inf, default = 1, tunable = FALSE,
                              when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, tunable = FALSE, when = "predict"),
      makeUntypedLearnerParam(id = "level", default = c(80,95), when = "predict"),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict")
    ),
    properties = c("numerics","ts","se"),
    name = "AutoRegressive Fractionally Integrated Moving Average",
    short.name = "arfima",
    note = ""
  )
}

#'@export
trainLearner.fcregr.arfima = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::arfima(y = data$target, ...)
}

#'@export
predictLearner.fcregr.arfima = function(.learner, .model, .newdata, ...){
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

