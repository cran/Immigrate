#' imIM4E
#'
#' This function performs imIM4E(imbalance Iterative Margin-Maximization under Max-Min entropy) algorithm.
#' @param train_xx model matrix of explanatory variables
#' @param train_yy label vector
#' @param epsilon criterion for stopping iteration
#' @param sig sigma used in algorithm, default to be 1
#' @param rho rho used in algorithm, default to be 1
#' @param max_iter maximum number of iteration 
#' @param lambda lambda used in algorithm, default to be 1
#' @param removesmall whether remove features with small weights, default to be FALSE
#' @keywords imIM4E
#' @return \item{ w}{new weight after one loop}
#' @return \item{ iter_num}{ number of iteration for convergence}
#' @return \item{ final_c}{ final cost}
#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' re<-imIM4E(xx,yy)
#' print(re)
imIM4E<-function(train_xx,train_yy,epsilon=0.01,
                 sig=1,rho=1, lambda=1,max_iter=10,removesmall=FALSE){
  suppressWarnings(
  return(imIM4ECpp(oneimIM4E=one.imIM4E,train_xx,train_yy,epsilon=epsilon,
                   sig=sig,rho=rho, lambda=lambda,max_iter=max_iter,removesmall=removesmall)))
}