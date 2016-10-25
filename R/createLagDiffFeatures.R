#' @title Generate lags and differences for feature variables
#'
#' @description
#' Replace all variables with their generated lagged and differenced variables.
#' Uses the \code{xts} framework for developing lags and is only available for \code{TimeTasks}.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param lag [\code{integer}]\cr
#'   An integer vector of lag lengths.
#' @param difference [\code{integer}]\cr
#'   An integer of the order of differencing
#' @param cols [\code{character}]\cr
#'   A character vector of columns to create dummy features for.
#'    Default is to use all columns.
#' @param seasonal.lag [\code{integer}]\cr
#'   An integer vector of seasonal lag lengths, made as \code{seasonal.lag * frequency}
#' @param seasonal.difference [\code{integer}]\cr
#'   An integer of the seasonal order of difference, made as \code{seasonal.difference * frequency}
#' @param frequency [\code{integer}]\cr
#'   An integer representing the periodicity in the time series. If frequency is declared in the task,
#'   the task frequency will be used.
#' @param na.pad [\code{logical}]\cr
#'   A logical to denote whether the data should be padded to the original size with NAs
#' @param difference.lag [\code{integer}]\cr
#'   An integer denoting the period to difference over
#' @param seasonal.difference.lag [\code{integer}]\cr
#'   An integer denoting the period to seasonaly difference over
#' @param return.nonlag [\code{logical}]\cr
#'   A logical to denote whether the original unlagged features should be returned
#' @export
#' @family eda_and_preprocess
createLagDiffFeatures = function(obj, lag = 0L, difference = 0L, difference.lag = 1L,
                                 cols = NULL, target = character(0L),
                                 seasonal.lag = 0L, seasonal.difference = 0L,
                                 seasonal.difference.lag = 1L, frequency = 1L,
                                 na.pad = TRUE, return.nonlag = TRUE) {
  ## FIXME: differences only accepts one value, should allow for more
  assertInteger(lag,lower = 0L, upper = 1000L)
  assertInteger(difference,lower = 0L, upper = 1000L, len = 1L)
  assertInteger(difference.lag,lower = 0L, upper = 1000L, len = 1L)
  assertInteger(seasonal.lag,lower = 0L, upper = 1000L)
  assertInteger(seasonal.difference,lower = 0L, upper = 1000L, len = 1L)
  assertInteger(seasonal.difference.lag,lower = 0L, upper = 1000L, len = 1L)
  assertLogical(na.pad)
  assert(checkClass(obj, "xts"), checkClass(obj, "TimeTask"))
  assertCharacter(target, any.missing = FALSE)
  if (!is.null(cols))
    assertCharacter(cols, any.missing = FALSE)
  UseMethod("createLagDiffFeatures")
}

#' @export
createLagDiffFeatures.xts = function(obj, lag = 0L, difference = 0L, difference.lag = 1L,
                                     cols = NULL, target = character(0L),
                                     seasonal.lag = 0L, seasonal.difference = 0L,
                                     seasonal.difference.lag = 1L, frequency = 1L,
                                     na.pad = TRUE, return.nonlag = TRUE) {
  work.cols = colnames(obj)
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
    x = obj[,cols]
  }
  seasonal.lag        = seasonal.lag * frequency
  seasonal.difference = seasonal.difference * frequency

  #FIXME: Should we pad these or not?
  if (any(lag > 0L) && any(difference > 0L)){
    xLagDiff = lag.xts(obj, k = lag)
    xLagDiff = diff.xts(xLagDiff, lag = difference.lag, differences = difference)
  } else if (any(lag > 0L)){
    xLagDiff = lag.xts(obj, k = lag)
  } else if (any(difference > 0L)){
    xLagDiff = diff.xts(obj,lag = difference.lag, differences = difference)
  } else {
    xLagDiff = NULL
  }


  if (frequency > 1L){
    if (any(seasonal.lag > 0L) && any(seasonal.difference > 0L)){
      xSeasonLagDiff = lag.xts(obj, k = seasonal.lag)
      xSeasonLagDiff = diff.xts(xSeasonLagDiff, lag = seasonal.difference.lag,
                                differences = seasonal.difference)
    } else if (any(seasonal.lag > 0L)){
      xSeasonLagDiff = lag.xts(obj, k = seasonal.lag)
    } else if (any(seasonal.difference > 0L)){
      xSeasonLagDiff = diff.xts(obj,lag = seasonal.difference.lag,
                                differences = seasonal.difference)
    } else {
      xSeasonLagDiff = NULL
    }
  } else {
    xSeasonLagDiff = NULL
  }
  if (!is.null(xLagDiff))
    colnames(xLagDiff) <- paste0(work.cols,"_lag",lag,"_diff",difference)
  if (!is.null(xSeasonLagDiff))
    colnames(xSeasonLagDiff) <- paste0(work.cols,"_lag",seasonal.lag,"_diff",seasonal.difference)

  if (!is.null(xLagDiff) && !is.null(xSeasonLagDiff)){
    repeatedCols <- !(colnames(xLagDiff) %in% colnames(xSeasonLagDiff))
    x = cbind(xLagDiff[,repeatedCols],xSeasonLagDiff)
  } else if(!is.null(xLagDiff)){
    x = xLagDiff
  } else if (!is.null(xSeasonLagDiff)){
    x = xSeasonLagDiff
  } else {
    stop("No lags or differences were chosen")
  }

  if (return.nonlag){
    obj = obj[,c(setdiff(work.cols, cols)), drop = FALSE]
    obj = cbind(obj, x)
  } else {
    obj = cbind(obj[,target],x)
  }

  if (na.pad == FALSE){
    removeNaPad = 1:(max(lag, seasonal.lag) +
                       (max(difference * difference.lag,
                            seasonal.difference * seasonal.difference.lag)))
    obj = obj[-removeNaPad,]
  }
  return(obj)
}

#' @export
createLagDiffFeatures.TimeTask = function(obj, lag = 0L, difference = 0L, difference.lag = 1L,
                                          cols = NULL, target = character(0L),
                                          seasonal.lag = 0L, seasonal.difference = 0L,
                                          seasonal.difference.lag = 1L, frequency = 1L,
                                          na.pad = TRUE, return.nonlag = TRUE) {
  target = getTaskTargetNames(obj)
  data = getTaskData(obj)
  frequency = obj$task.desc$frequency
  # FIXME: There must be a better way to do this
  work.cols = setdiff(colnames(data),"dates")
  data = xts::xts(data[,work.cols], order.by = data[,"dates"])
  colnames(data) = work.cols
  data = createLagDiffFeatures( obj = data, lag = lag, difference = difference,
                                difference.lag = difference.lag,
                                cols = cols, target = target,
                                seasonal.lag = seasonal.lag,
                                seasonal.difference = seasonal.difference,
                                seasonal.difference.lag = seasonal.difference.lag,
                                frequency = frequency, na.pad = na.pad,
                                return.nonlag = return.nonlag)
  data <- cbind(dates = index(data),as.data.frame(data))
  changeData(obj,data = data)

}
