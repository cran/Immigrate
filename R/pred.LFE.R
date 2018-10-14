#' pred.LFE
#'
#' This function performs predion for LFE(Local Feature Extraction) algorithm.
#' @param w weight 
#' @param xx model matrix of explanatory variables
#' @param yy label vector 
#' @param newx model matrix to be preded 
#' @keywords LFE
#' @return preded labels
#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' w<-LFE(xx,yy)
#' pred<-pred.LFE(w,xx,yy,xx)
#' print(pred)

pred.LFE<-function(w,xx,yy,newx){
  yy<-as.numeric(yy)
  if (ncol(xx) != ncol(newx)){
    stop("xx and newx have different lengths of explanatory variables")
  }
  pred<-sapply(c(1:nrow(newx)), function(i){
    yy[as.numeric(which.min(rowSums(crossprod(abs(t(xx)-as.numeric(newx[i,])),w)*t(abs(t(xx)-as.numeric(newx[i,])))))[1])]
  })
  return(pred)
}
