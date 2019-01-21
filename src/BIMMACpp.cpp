#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List BIMMACpp(Function Immigrate,NumericMatrix train_xx,NumericVector train_yy,int nIter = 10, int max_iter = 10,bool removesmall = false,bool randomw0 = true, double sigstart=0.02,double sigend=4){
  List matrix_list = List::create();
  double siggap = exp(log(sigend/sigstart)/10);
  NumericVector sigseq(nIter);
  double signow = sigstart/siggap;
  for (int i = 0; i < nIter; i++){
      signow = signow*siggap;
      sigseq[i] = signow;
  }
  List Immigrate_result;
  
  for(int i=0;i<nIter;i++)
  {
    Immigrate_result = Immigrate(train_xx,train_yy,Named("sig") = sigseq[i], Named("removesmall") = removesmall,Named("max_iter") = max_iter,Named("randomw0") = randomw0);
    std::ostringstream ss;
    ss << i;
    std::string list_name =  ss.str();

    matrix_list[list_name] = Immigrate_result["w"];
  }
  List BIMMA_list;
  BIMMA_list["matrix"] = matrix_list;
  BIMMA_list["sig"] = sigseq;
  return BIMMA_list;
}
