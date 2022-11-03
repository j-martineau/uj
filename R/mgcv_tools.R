#' @name mgcv_tools
#' @title Aliases and wrappers for functions in package \code{mgcv}.
#' @description Alias for \code{\link[mgcv]{bam}} (big-data generalized additive
#'   models).
#' @export
mgbam <- mgcv::bam

#' @describeIn mgcv_tools Alias for \code{\link[mgcv]{ocat}} (ordered category
#'   logistic regression family).
#' @export
mgocat <- mgcv::ocat

#' @describeIn mgcv_tools Alias for \code{\link[mgcv]{predict.bam}} (fitted
#'   value prediction for new data).
#' @export
mgpred <- mgcv::predict.bam
