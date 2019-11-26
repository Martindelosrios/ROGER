#' A function to get the classification of each galaxy in a catalog.
#'
#' This function allows you to estimate the orbital classification of each galaxy
#'  inside a catalog using phase-space information.
#' @param cat: Data frame with the galaxy catalog. It should have as columns the
#'             distance to the cluster center normalized to the virial radius of each
#'             cluster ('r') and the line-of-sight velocity relative to the cluster
#'             normalized to the line-of-sight velocity of the cluster ('v').
#' @param model: Machine Learning model to be used for the classification.
#' @export
#' @examples
#' get_class(cat, all_vs_all_KNN)

get_class <- function(cat, model){
  model_predictions <- predict(model, newdata = cat, type = 'prob')
  return(model_predictions)
}

