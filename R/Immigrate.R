

#' Immigrate
#'
#' This function performs Immigrate(Iterative Max-Min Entropy Margin-Maximization with Interaction Terms ) algorithm.
#' @param train_xx model matrix of explanatory variables
#' @param train_yy label vector
#' @param epsilon criterion for stopping iteration
#' @param sig sigma used in algorithm, default to be 1
#' @param lambda lambda used in algorithm, default to be 1
#' @param max_iter maximum number of iteration 
#' @param removesmall whether remove features with small weights, default to be FALSE
#' @keywords Immigrate
#' @return \item{w}{new weight after one loop}
#' @return \item{iter_num}{ number of iteration for convergence}
#' @return \item{final_c}{final cost}
#' @import Rcpp

#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' re<-Immigrate(xx,yy)
#' print(re)
Immigrate<-function(train_xx,train_yy,epsilon=0.01,
                    sig=1, lambda=1,max_iter=10,removesmall=FALSE){
  suppressWarnings(
  return(ImmigrateCpp(oneImmigrate=one.Immigrate,train_xx,train_yy,epsilon=epsilon,
                      sig=sig, lambda=lambda,max_iter=max_iter,removesmall=removesmall)))
}
