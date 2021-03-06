#' @title Query properties of learners.
#'
#' @description
#' Properties can be accessed with \code{getLearnerProperties(learner)}, which returns a
#' character vector.
#'
#' The learner properties are defined as follows:
#' \describe{
#'   \item{numerics, factors, ordered}{Can numeric, factor or ordered factor features be handled?}
#'   \item{missings}{Can missing values in features be handled?}
#'   \item{weights}{Can observations be weighted during fitting?}
#'   \item{oneclas, twoclass, multiclass}{Only for classif: Can one-class, two-class or multi-class classification problems be handled?}
#'   \item{class.weights}{Only for classif: Can class weights be handled?}
#'   \item{rcens, lcens, icens}{Only for surv: Can right, left, or interval censored data be handled?}
#'   \item{prob}{For classif, cluster, multilabel, surv: Can probabilites be predicted?}
#'   \item{se}{Only for regr: Can standard errors be predicted?}
#'   \item{featimp}{For classif, regr, surv: Does the model support extracting information on feature importance?}
#' }
#'
#' @template arg_learner
#' @param props [\code{character}]\cr
#'   Vector of properties to query.
#' @return \code{getLearnerProperties} returns a character vector with learner properties.
#'  \code{hasLearnerProperties} returns a logical vector of the same length as \code{props}.
#' @name LearnerProperties
#' @rdname LearnerProperties
#' @aliases getLearnerProperties hasLearnerProperties
#' @family learner
NULL

#' @rdname LearnerProperties
#' @export
getLearnerProperties = function(learner) {
  UseMethod("getLearnerProperties")
}

#' @export
getLearnerProperties.Learner = function(learner) {
  learner$properties
}

#' @export
getLearnerProperties.character = function(learner) {
  getLearnerProperties(checkLearner(learner))
}

#' @rdname LearnerProperties
#' @export
hasLearnerProperties = function(learner, props) {
  learner = checkLearner(learner)
  assertSubset(props, getSupportedLearnerProperties())
  props %in% getLearnerProperties(learner)
}

#' Deprecated, use \code{hasLearnerProperties} instead.
#' @param learner Deprecated.
#' @param props Deprecated.
#' @export
hasProperties = function(learner, props) {
  .Deprecated("hasLearnerProperties")
  hasLearnerProperties(learner, props)
}

getSupportedLearnerProperties = function(type = NA_character_) {
  p = list(
    classif    = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass", "class.weights", "featimp"),
    multilabel = c("numerics", "factors", "ordered", "missings", "weights", "prob", "oneclass", "twoclass", "multiclass"),
    regr       = c("numerics", "factors", "ordered", "missings", "weights", "se", "featimp", "ts"),
    cluster    = c("numerics", "factors", "ordered", "missings", "weights", "prob"),
    surv       = c("numerics", "factors", "ordered", "missings", "weights", "prob", "lcens", "rcens", "icens", "featimp"),
    costsens   = c("numerics", "factors", "ordered", "missings", "weights", "prob", "twoclass", "multiclass"),
    fcregr     = c("numerics", "factors", "ordered", "missings", "weights", "quantile", "featimp", "ts")
  )
  if (is.na(type))
    unique(unlist(p))
  else
    p[[type]]
}

