#' A function to get the classification of each galaxy in a catalog.
#'
#' This function allows you to estimate the orbital classification of each galaxy
#'  inside a catalog using phase-space information.
#' @param cat: Data frame with the galaxy catalog. It should have as columns the
#'             distance to the cluster center normalized to the virial radius of each
#'             cluster ('r') and the line-of-sight velocity relative to the cluster
#'             normalized to the line-of-sight velocity of the cluster ('v').
#' @param model: Machine Learning model to be used for the classification.
#' @param type: Output of the machine learning algorithm. Either 'prob' for probabilities or 'raw' for the most probable class. Default 'prob'.
#' @param threshold: If type = 'prob' this is the probability threshold to determine the predicted class of a certain galaxy. If threshold = 0, the function returns the full probility vector. Default = 0.
#' @export
#' @examples
#' get_class(cat, all_vs_all_KNN)

get_class <- function(cat, model, type = 'prob', threshold = 0){

  model_predictions <- predict(model, newdata = cat, type = type)

  if((type == 'prob') & (threshold > 0)){
    class   <- 1:length(model_predictions$X1)
    class[] <- -99 
    for(i in 1:length(model_predictions$X1)){
      mpc <- which.max(model_predictions[i,])
      if(model_predictions[i, mpc] > threshold){
        class[i] <- paste0('X', toString(mpc))
      } else{
        class[i] <- 'NA'
      }
    }
    model_predictions <- class
  }

  return(model_predictions)
}

