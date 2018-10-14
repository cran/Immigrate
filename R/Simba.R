#' Simba
#'
#' This function performs Simba(Iterative Search Margin Based Algorithm).
#' @param train_xx model matrix of explanatory variables
#' @param train_yy label vector
#' @param T number of instance used to update weights, default to be 5
#' @keywords Simba
#' @return \item{w}{new weight after Simba algorithm}
#' 
#' @export
#' @examples
#' data(park)
#' xx<-park$xx
#' yy<-park$yy
#' re<-Simba(xx,yy)
#' print(re)
#' @references Gilad-Bachrach R, Navot A, Tishby N. Margin based feature selection-theory and algorithms[C]//Proceedings of the twenty-first international conference on Machine learning. ACM, 2004: 43.

Simba<-function(train_xx,train_yy,T=5){
  N<-nrow(train_xx)
  p<-ncol(train_xx)
  if ( (T<1)|(T>N)){
    stop("use wrong T")
  }
  w<-rep(1,p)
  ins_update<-sample(c(1:N),T)
  weight<-sapply(c(1:T),function(i){
    k<-ins_update[i]
    tmp_yyy<-abs(train_yy-train_yy[k])
    tmp_yy<-rep(0,length(tmp_yyy))
    tmp_yy[which(tmp_yyy==0)]<-1
    tmp_train_xx<-t(train_xx)*w
    dis<-colSums((tmp_train_xx-tmp_train_xx[,k])^2)
    dis_h<-tmp_yy*dis
    dis_m<-(1-tmp_yy)*dis
    nh<-which(dis_h==min(dis_h[dis_h>0]))[1]
    nm<-which(dis_m==min(dis_m[dis_m>0]))[1]
    nm_xx<-tmp_train_xx[,k]-tmp_train_xx[,nm]
    nh_xx<-tmp_train_xx[,k]-tmp_train_xx[,nh]
    nm_w<-sqrt( (nm_xx^2)%*%(w^2) )[1,1]
    nh_w<-sqrt( (nh_xx^2)%*%(w^2) )[1,1]
    delta_w<-(as.vector(nm_xx)^2/nm_w-as.vector(nh_xx)^2/nh_w)*w/2
    w<<-w+delta_w
  })
  w<-pmax(w,0)
  return(w^2/max(w^2))
}