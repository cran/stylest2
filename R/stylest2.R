#' stylest2: A package for estimating authorship of texts.
#'
#' stylest2 provides a set of functions for fitting a model of speaker
#' distinctiveness, including tools for selecting the optimal vocabulary for the
#' model and predicting the most likely speaker (author) of a new text.
#' 
#' @importFrom Matrix rowSums colMeans
#' @importFrom stats smooth quantile
#' 
#' @name stylest
NULL