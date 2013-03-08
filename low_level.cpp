#include <Rcpp.h>
using namespace Rcpp;


int lifelse(bool test,int out1,int out2){
  if(test==true){return(out1);}
  if(test==false){return(out2);}
}

int getGclocation(int timesweeper,NumericMatrix gcurve, NumericVector ML){
  //this function computes the glocation
  int gclocation;
  int lengthML;
  double diff;
  lengthML= ML.length()-1;
  if(gcurve(timesweeper,2)>=min(ML)){
    for(int i=0; i<lengthML;i++){
       diff=pow(( ML(i)-gcurve(timesweeper,3)),2.0);
       gclocation=lifelse(diff<=.0001,i,0);  
    }
  }else{gclocation=0;}
  Rcout<<"outputted gclocation==::"<<gclocation<<"\n" ;
  return gclocation ;
}

// [[Rcpp::export]]

List espcomputeC(NumericMatrix gcurve,NumericMatrix p, int modday, NumericVector ML)
{//                                       #compute ESP
  //initalize the variables     
  NumericMatrix peaks2;//make temporary peaks matrix
  
  int pos;//vector of positive peak's
  int neg;//vector of negative peak's
  double ESPout;// total ESP
  double peaks2val;   
  int timesweep;//length of times to sweep over 
  int tsweep;
  int timesweeper;
  int gclocation;
  int wmaxML;
  int wminML;

  wmaxML=which_max(ML);
  wminML=which_min(ML);
  peaks2 = p; //#need a structure to turn to zero to prevent counting a peak more than once.
  timesweep= gcurve.nrow();
  Rcout<<"timesweep   ::"<<timesweep <<"   "<<gcurve.ncol()<<"\n";
  NumericVector esp(timesweep,0.0);//vector of partial esp's
  Rcout<<"length esp   ::"<<esp.length()<<"\n";
  for(int timesweeper=0; timesweeper<timesweep; timesweeper++){
    gclocation=getGclocation(timesweeper,gcurve,ML);
    Rcout<<"gclocation main    ::"<<gclocation<<"  "<<getGclocation(timesweeper,gcurve,ML)<<"\n";
    if(gclocation>0){
      Rcout<<"hi you know you are in a good place\n";
      //      exit(2);
      tsweep = gcurve(timesweeper,1)+1;
      peaks2val = peaks2(gclocation,tsweep);
      if(peaks2val>0){
	esp(timesweeper) = peaks2val;
	peaks2(gclocation,tsweep) = 0;
	for(int i; i<=200; i++){
	  pos = lifelse((gclocation+i< wmaxML),gclocation+i,wmaxML);
	  neg = lifelse((gclocation-i> wminML),gclocation-i,wminML);
	  if(peaks2(pos,tsweep)>=0) {peaks2(pos,tsweep) = 0;}
	  if(peaks2(neg,tsweep)>=0) {peaks2(neg,tsweep) = 0;}
	  if(peaks2(neg,tsweep)<0 && peaks2(pos,tsweep)<0) {break;}
	}
      }else{
	Rcout<<"Bloody  time sweep::"<<timesweeper<<" peaks2val ::"<<peaks2val<<"gclocation  ::"<<gclocation<<"\n";
    esp(timesweeper) = peaks2val;  }
      
    }else{
      	Rcout<<"Mary  time sweep::"<<timesweeper<<" peaks2val ::"<<peaks2val<<"gclocation  ::"<<gclocation<<"\n";
    esp(timesweeper)= 0;}
  }
  ESPout= sum(esp);

  return(Rcpp::List::create(Rcpp::Named("esp")=ESPout));
}
