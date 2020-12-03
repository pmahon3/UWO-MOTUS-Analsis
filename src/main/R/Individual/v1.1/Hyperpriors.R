hyperpriors <- function(constants,
                        median_xiDelta,
                        df_xiDelta = 4,
                        median_xi_y,
                        df_xi_y = 4,
                        median_xi,
                        df_xi = 4){

  ## Compute parameters for hyperpriors based on prior information.

  constants$tau_xiDelta <- (qt(.75,df_xiDelta) / median_xiDelta)^2
  constants$df_xiDelta <- df_xiDelta
  
  constants$tau_xi_y <- (qt(.75,df_xi_y) / median_xi_y)^2
  constants$df_xi_y <- df_xi_y
  
  constants$tau_xi <- (qt(.75,df_xi) / median_xi)^2
  constants$df_xi <- df_xi

  return(constants)
}
  

