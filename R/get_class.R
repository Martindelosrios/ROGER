#' A function to get the classification of each galaxy in a catalog.
#'
#' This function allows you to estimate the orbital classification of each galaxy
#'  inside a catalog using phase-space information.
#' @param cat: Data frame with the galaxy catalog. It should have as columns the
#'             distance to the cluster center normalized to the virial radius of each
#'             cluster ('r') and the line-of-sight velocity relative to the cluster
#'             normalized to the line-of-sight velocity of the cluster ('v').
#' @param model: Machine Learning model to be used for the classification.
#' @param type: Output of the machine learning algorithm. Either 'prob' for probabilities or 'class' for the most probable class. Default 'prob'.
#' @param threshold: If type = 'prob' this is the probability threshold to determine the predicted class of a certain galaxy. If threshold = 0, the function returns the full probility vector. Default = 0.
#' @details model: There are 3 available models. knn for a K-nearest neighbour, svm for a Support Vector Machine and rf for a Random Forest. See XXXX.XXXX for more ddetails on the models.
#' @export
#' @examples
#' get_class(cat, knn, type = 'prob', threshold = 0.5)

get_class <- function(cat, model, type = 'prob', threshold = 0){

  if(type == 'prob'){
    model_predictions <- as.data.frame(predict(model, newdata = cat, type = type))
    if(threshold > 0){
      class   <- 1:length(model_predictions$X1)
      class[] <- -99 
      for(i in 1:length(model_predictions$X1)){
        mpc <- which.max(model_predictions[i,])
        if(model_predictions[i, mpc] > threshold){
          if(mpc == 1){class[i] <- 'CL'}
          if(mpc == 2){class[i] <- 'BS'}
          if(mpc == 3){class[i] <- 'IN'}
          if(mpc == 4){class[i] <- 'RIN'}
          if(mpc == 5){class[i] <- 'ITL'}
        } else{
          class[i] <- 'NA'
        }
      }
      model_predictions <- as.data.frame(class)
      colnames(model_predictions) <- c('pred_class')
    } else if (threshold == 0){
      colnames(model_predictions) <- c('CL', 'BS', 'IN', 'RIN', 'ITL')
    }
  } else if (type == 'class'){
    model_predictions <- as.data.frame(predict(model, newdata = cat))
    colnames(model_predictions) <- c('pred_class')
    levels(model_predictions$pred_class) <- c('CL', 'BS', 'IN', 'RIN', 'ITL')
  }

  return(model_predictions)
}

