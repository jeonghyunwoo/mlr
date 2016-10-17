#' @export
updateData = function(task, newdata, weights = NULL){
  assertClass(task, "Task")

  if (xts::is.xts(newdata))
    newdata = as.data.frame(dates = index(newdata), newdata[,1])
  assertDataFrame(newdata)
  data = getTaskData(task)
  data = rbind(data,newdata)
  changeData(task, data, weights)
}
