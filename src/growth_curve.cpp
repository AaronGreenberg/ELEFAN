
#include <Rcpp.h>
using namespace Rcpp;// This set of cpp routines speeds up computing growth curves.
// The main routines are almost directly ported from the R routines
// however it is important to compute growth curves very quickly.

// [[Rcpp::export]]
double cgrowth_rootf(double x,double K,double Linf,double locperiodC,double TW,double age){
//makes computing tstart and time when length is .95%Linf easy.
  const double pi=3.141592653589793116;// need to define pi.
  double period;
  double out;
  double w;
  w = 1.0/365.0;
  period = (locperiodC*K)/(2.0*pi*w)*(sin(2.0*pi*w*(age-TW))-sin(2.0*pi*w*(x-TW)));
  out = Linf*(1.0-exp(-K*(age-x)+period));
  return out;
}

  
double cbisect(double a,double b,double equal,double K,double Linf,double locperiodC,double TW,double age){
  if(pow((cgrowth_rootf(a,K,Linf,locperiodC,TW,age)-equal),2.0)<.0000000001){return a ;}
  if(pow((cgrowth_rootf(b,K,Linf,locperiodC,TW,age)-equal),2.0)<.0000000001){return b ;}//make sure zero isn't endpoints.
  //This function uses bisection to compute the required values of tstart and...
  if((cgrowth_rootf(a,K,Linf,locperiodC,TW,age)-equal)*(cgrowth_rootf(b,K,Linf,locperiodC,TW,age)-equal)>0){return 1 ;}
  int termtest=1;//set counter to protect against errors. 
  while(termtest<= 10000) {// limit iterations to prevent infinite loop
    double d;
    d = (a + b)/2.0; //new midpoint
    //Rcout << "Bisection trace"<<a<<":"<<b<<":"<<d<<":"<<termtest<<"\n";
      if(((cgrowth_rootf(d,K,Linf,locperiodC,TW,age)-equal)==0||(b-a)/2.0<= .0000000001)) { //solution found
      return d ;
      break;
  }
    termtest = termtest++; //increment step counter
    if((cgrowth_rootf(d,K,Linf,locperiodC,TW,age)-equal)*(cgrowth_rootf(a,K,Linf,locperiodC,TW,age)-equal)>0){ a = d;}
  else{b = d ;}// new interval
  }
 }
   
double cgrowth_rootf2 (double x,double K,double Linf,double locperiodC,double TW,double ts,double age){
//makes computing to and time when length is .95%Linf easy. 
  double period;
  double out;
  double w;
   const double pi=3.141592653589793116;// need to define pi.
  w = 1.0/365.0;
  period = (locperiodC*K)/(2.0*pi*w)*(sin(2.0*pi*w*(x-(TW-age)))-sin(2.0*pi*w*(ts-TW)));
  out = Linf*(1.0-exp(-K*(x-(ts-age))+period));
  return out ;
}

 
double cbisect2(double a,double b,double equal,double K,double Linf,double locperiodC,double TW, double ts,int  age){
  if(pow((cgrowth_rootf2(a,K,Linf,locperiodC,TW,ts,age)-equal),2.0)<.000000000001){return a ;}
  if(pow((cgrowth_rootf2(b,K,Linf,locperiodC,TW,ts,age)-equal),2.0)<.000000000001){return b ;}//make sure zero isn't endpoints.
  //This function uses bisection to compute the required values of tstart and...
  if((cgrowth_rootf2(a,K,Linf,locperiodC,TW,ts,age)-equal)*(cgrowth_rootf2(b,K,Linf,locperiodC,TW,ts,age)-equal)>0){return 1 ;}
  int termtest=1;//set counter to protect against errors. 
  while(termtest<= 10000) {// limit iterations to prevent infinite loop
    double d;
    d = (a + b)/2.0; //new midpoint
    if(((cgrowth_rootf2(d,K,Linf,locperiodC,TW,ts,age)-equal)==0||(b-a)/2<= .000000000001)) { //solution found
      return d ;
      break;
  }
    termtest = termtest++; //increment step counter
    if((cgrowth_rootf2(d,K,Linf,locperiodC,TW,ts,age)-equal)*(cgrowth_rootf2(a,K,Linf,locperiodC,TW,ts,age)-equal)>0){ a = d;}
  else{b = d ;}// new interval
  }
 }
   



// [[Rcpp::export]]


List curves_cpp(double Linf,double locperiodC,double tw,double K,NumericVector ML,double modday,double sdate,double sML,double birthdaycurve){
  K = K/365.0;// get everything in the correct 
  double w;
  w = 1.0/365.0;
  double TW;
  TW = tw*365.0;
  const double pi=3.141592653589793116;// need to define pi.


 double age;
 
 age= sdate-birthdaycurve; //need to compute age.
  //first  compute time_start
 double timestart;
 timestart =  cbisect(-200.0*365.0,200*365.0,sML,K,Linf,locperiodC,TW,age);
  //second compute time of  95%*Linf
  double nintyfivetime;
  nintyfivetime =  cbisect2(-200*365,200.0*365.0,.95*Linf,K,Linf,locperiodC,TW,timestart,age);
  //compute tzero
  //really only important when locperiodC!=0 because it should be about -timestart  but ...
  double zerotime;
  zerotime =cbisect2(-200*365,200.0*365.0,0.0,K,Linf,locperiodC,TW,timestart,age);
  //get vector of times!
  double upwind;
  double downwind;

  upwind = (ceil(nintyfivetime));
  downwind = (floor(zerotime));
  //Rcout<< "upwind::"<< upwind<<"95time::"<<nintyfivetime<<"  downwind::"<< downwind <<"zerotime::"<<zerotime<<"timestart::"<<timestart<<"::\n";
  IntegerVector time((-1.0*downwind+upwind)); //create a vector of times from downwind to upwind
  NumericMatrix cur((upwind-downwind),4);        //initalize growth curve data structure
  for (int i = 0; i < ceil(-1.0*downwind+upwind); i++) {
    time(i) = int(downwind+i);
    //Rcout<<"timesweep"<< time(i)<<":::"<<i<<"\n";
  double period;
  int intsdate;
  intsdate=sdate;
  period = (locperiodC*K)/(2.0*pi*w)*(sin(2.0*pi*w*(time(i)-(TW-age)))-sin(2.0*pi*w*(timestart-TW)));
  cur(i,0) =(time(i)+intsdate);//keep real time
  //Rcout<<"cur"<<cur(i,0)<<"::"<<i<<"###"<<-1.0*downwind+upwind<<"\n";
  if(((time(i)+intsdate)%int(modday))>0)
    {
     cur(i,1)= (time(i)+intsdate)%int(modday);
    }
  else{
   cur(i,1)= (time(i)+intsdate+30*int(modday))%int(modday);
    } //wrap time so mapping the time to the plot is easy
  cur(i,2) =Linf*(1-exp(-K*((time(i)-(-age+timestart)))+period));//put in the growth curve
  cur(i,3) = ML(which_min(pow(ML-cur(i,2),2)));            //get a version of the growth curve that makes computing esp anda
  
 }

 return Rcpp::List::create(Rcpp::Named("c") =cur,
			    Rcpp::Named("tzero") =downwind);  

}
