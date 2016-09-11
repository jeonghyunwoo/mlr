#'@export
makeRLearner.fcregr.Arima = function() {
  makeRLearnerRegr(
    cl = "fcregr.Arima",
    package = "forecast",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "order", len = 3L,
                                    lower = 0L, upper = Inf,
                                    default = c(0L,0L,0L),
                                    tunable = TRUE),
      makeIntegerVectorLearnerParam(id = "seasonal", len = 3L,
                                    lower = 0L, upper = Inf,
                                    default = c(0L,0L,0L),
                                    tunable = TRUE),
      makeLogicalLearnerParam(id = "include.mean", default = TRUE, tunable = TRUE),
      makeLogicalLearnerParam(id = "include.drift", default = FALSE, tunable = TRUE),
      makeNumericLearnerParam(id = "lambda", default = 1, tunable = TRUE, special.vals = list(NULL),
                              when = "both"),
      makeDiscreteLearnerParam(id = "method", values = c("CSS-ML", "ML", "CSS"),
                               default = "CSS-ML", tunable = FALSE),
      # Make prediction parameters
      makeNumericLearnerParam(id = "n.ahead", lower = 1, upper = Inf,
                       default = 1, when = "predict"),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, tunable = TRUE, when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, tunable = FALSE, when = "predict"),
      makeUntypedLearnerParam(id = "level", default = c(80,95), when = "predict"),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict"),
      makeUntypedLearnerParam(id = "xreg", default = NULL)
    ),
    properties = c("numerics","ts","se"),
    name = "AutoRegressive Integrated Moving Average",
    short.name = "Arima",
    note = ""
    )
}

#'@export
trainLearner.fcregr.Arima = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task,.subset, target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  forecast::Arima(y = data$target, ...)
}

#'@export
predictLearner.fcregr.Arima = function(.learner, .model, .newdata, ...){
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

updateLearner.fcregr.Arima = function(.learner, .model, .newdata, .task, ...){
  data = ts(.newdata, start = 1, frequency = .task$task.desc$frequency)
  forecast::Arima(y = data, model = .model$learner.model,...)
}


