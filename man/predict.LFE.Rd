% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.LFE.R
\name{predict.LFE}
\alias{predict.LFE}
\title{predict.LFE}
\usage{
\method{predict}{LFE}(object, xx, yy, newx, ...)
}
\arguments{
\item{object}{weight}

\item{xx}{model matrix of explanatory variables}

\item{yy}{label vector}

\item{newx}{model matrix to be preded}

\item{...}{further arguments passed to or from other methods}
}
\value{
predicted labels
}
\description{
This function performs predion for LFE(Local Feature Extraction) algorithm.
}
\examples{
data(park)
xx<-park$xx
yy<-park$yy
w<-LFE(xx,yy)
pred<-predict(w,xx,yy,xx)
print(pred)
}
\keyword{LFE}
\keyword{predict}