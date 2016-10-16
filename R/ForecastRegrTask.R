#' @export
#' @rdname Task
#' @importFrom zoo index coredata
makeForecastRegrTask = function(id = deparse(substitute(data)), data, target,
                                weights = NULL, blocking = NULL, fixup.data = "warn",
                                check.data = TRUE, frequency = 1L) {
  assertString(id)
  assertClass(data,"xts")
  assertString(target)
  assertInteger(frequency, lower = 0L, max.len = 1L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  data <- data.frame(dates = index(data), coredata(data))

  if (fixup.data != "no") {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  }

  task = makeSupervisedTask("fcregr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  }

  task$task.desc = makeTaskDesc.ForecastRegrTask(task, id, target, frequency)
  addClasses(task, c("ForecastRegrTask","TimeTask"))
}

makeTaskDesc.ForecastRegrTask = function(task, id, target, frequency) {
  td = makeTaskDescInternal(task, "fcregr", id, target, time = TRUE)
  td$dates = c(task$env$data$dates[1],task$env$data$dates[nrow(task$env$data)])
  if (missing(frequency))
    frequency = task$task.desc$frequency
  td$frequency = frequency
  addClasses(td, c("TaskDescForecastRegr", "TaskDescSupervised"))
}
