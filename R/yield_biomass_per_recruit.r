#' Minimal doc
#' @import compiler PBSmodelling animation
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export


YR_kef <- function(E,Littlec,M,K,Linf,Pi=YieldProbs,pas=NULL)
  {
Lc=Littlec*Linf #convert from little c to Lc 
M=M*K #convert from M/K to M
#Non vectorized YR_knife edge
small_c <- Lc/Linf
U <- 1-small_c
m <- (1-E)/(M/K)

YR_ke <- (E*(U^m))*(1-((3*U)/(1+m))+((3*(U^2))/(1+(2*m)))-((U^3)/(1+(3*m))))
print("testing YR_kef")
print(c(E,Linf,M/K,K,Littlec,YR_ke))
return(YR_ke)
}



YR_nkef <- function(E,Littlec,M,K,Linf,Pi=YieldProbs,pas=NULL)
  {
    
    Lc=Littlec*Linf #convert from little c to Lc 
    M=M*K #convert from M/K to M
    small_c <- Lc/Linf
    U <- 1-small_c
    m <- (1-E)/(M/K)
      #Non vectorized YR_ not knife edge
    if(!is.na(sum(Pi))){
      l1 <- datain$ML-(datain$ML[2]-datain$ML[1])/2
      l2 <- datain$ML+(datain$ML[2]-datain$ML[1])/2
      U1 <- 1-(l1/Linf)#lower class
      U2 <- 1-(l2/Linf)#upper class 
#reduction factor
ri <- (U2^((M/K)*(E/(1-E))*Pi))/(U1^((M/K)*(E/(1-E))*Pi))
Gi <- c()
Gi[1]=ri[1]
for (j in 2:length(ri)) {Gi[j] <- Gi[j-1]*ri[j]}#doing product that computs Gi
yr1 <- (E*U1^(M/K))*(1-((3*U1)/(1+m))+(3*(U1^2)/(1+(2*m)))-((U1^3)/(1+(3*m))))#upper limit of class
yr2 <- (E*U2^(M/K))*(1-((3*U2)/(1+m))+(3*(U2^2)/(1+(2*m)))-((U2^3)/(1+(3*m))))#lower limit
t1 <- c()
t2 <- c()
t1[1] <- NA
t2[1] <- NA
for (k in 2:length(U1))
{

  t1[k] <- Gi[k-1]*yr1[k]
  t2[k] <- Gi[k]*yr2[k]
}
Diff <- t1-t2
yr <- Diff*Pi
cum_yr <- c()
cum_yr[1] <- 0
for (l in 2:length(U1))
{
  
  cum_yr[l] <- cum_yr[l-1]+yr[l]

}
YR_nke <- cum_yr[length(cum_yr)]
## if(YR_nke<0){
##   print("Negative Yeild_per Recruit")
##   print(c(E,Linf,Littlec,M,K))
##   print("diff")
##   print(Diff)                           #
#}

    
return(YR_nke)
}
}



BR_kef <- function(E,Littlec,M,K,Linf,Pi=YieldProbs,pas=NULL)
  {
Lc=Littlec*Linf #convert from little c to Lc 
M=M*K #convert from M/K to M
                                        # B/R Knife-edge
small_c <- Lc/Linf
U <- 1-small_c
m <- (1-E)/(M/K)
m_prim <- 1/(M/K)
D <- 1-(3*U/(1+m_prim))+(3*U^2/(1+(2*m_prim)))-(U^3/(1+(3*m_prim)))
N <- (1-E)*(1-((3*U)/(1+m))+((3*U^2)/(1+(2*m)))-(U^3/(1+(3*m))))
BR_ke <- N/D
return(BR_ke)
    
}


BR_nkef <- function(E,Littlec,M,K,Linf,Pi=YieldProbs,pas=NULL)
  {

l1 <- datain$ML-(datain$ML[2]-datain$ML[1])/2
l2 <- datain$ML+(datain$ML[2]-datain$ML[1])/2
 U1 <- 1-(l1/Linf)#lower class
  U2 <- 1-(l2/Linf)#upper class 
  
Lc=Littlec*Linf #convert from little c to Lc 
M=M*K #convert from M/K to M
m <- (1-E)/(M/K)
small_c <- Lc/Linf
U <- 1-small_c
if(!is.na(sum(Pi))){
# B/R Not Knife-edge
m_prim2 <- m/(1-E)
BR_nke <- c()

ri <- (U2^((M/K)*(E/(1-E))*Pi))/(U1^((M/K)*(E/(1-E))*Pi))
Gi <- c()
Gi[1]=ri[1]
for (j in 2:length(ri)) {Gi[j] <- Gi[j-1]*ri[j]}
N1 <- (1-E)*(1-(3*U1/(1+m))+((3*U1^2)/(1+(2*m)))-((U1^3)/(1+(3*m))))
N2 <- (1-E)*(1-(3*U2/(1+m))+((3*U2^2)/(1+(2*m)))-((U2^3)/(1+(3*m))))
D1 <- 1-((3*U1)/(1+m_prim2))+((3*U1^2)/(1+(2*m_prim2)))-((U1^3)/(1+(3*m_prim2)))
D2 <- 1-((3*U2)/(1+m_prim2))+((3*U2^2)/(1+(2*m_prim2)))-((U2^3)/(1+(3*m_prim2)))

br1 <- (1-E)*(N1/D1)
br2 <- (1-E)*(N2/D2)
t1 <- c()
t2 <- c()
t1[1] <- NA
t2[1] <- NA
for (k in 2:length(U1))
{
t1[k] <- Gi[k-1]*br1[k]
t2[k] <- Gi[k]*br2[k]
}
Diff <- t1-t2
br <- Diff*Pi
cum_br <- c()
cum_br[1] <- 0
for (l in 2:length(U1))
{
cum_br[l] <- cum_br[l-1]+br[l]
}
Si <- (M/K)*(E/(1-E))*Pi
rj <- (U2^Si)/(U1^Si)

gi <- c()
gi[1]=rj[1]
for (j in 2:length(rj)) {gi[j] <- gi[j-1]*rj[j]}

cum_bri <- (1-E)*(N1/D1)

BR_nke <- cum_bri[length(cum_bri)]
}
return(BR_nke)
}



yield_biomass_per_recruit<- function(M,K,Littlec,Linf,Pi=YieldProbs,pas=NULL)
{
if (is.null(pas)) pas <- 0.015
E=seq(.01,1,by=pas) 
if(is.na(sum(Pi)))
  { #make things = na
    YR_nke <- BR_nke <- NA
  }
else{
tab_names <- c('YR_ke','YR_nke','BR_ke','BR_nke')
tab_final <- array(dim=c(length(E),4),dimnames=(list(E,tab_names)))
tab_final[,1] <- sapply(E,YR_kef,Linf=Linf,M=M,K=K,Littlec=Littlec,Pi=YieldProbs,pas=NULL)

#print(tab_final)
print("YieldProbs")
#print(YieldProbs)
tab_final[,2] <- sapply(E,YR_nkef,Littlec,M,K,Linf,Pi=YieldProbs,pas=NULL)
#print(tab_final)
tab_final[,3] <- sapply(E,BR_kef,Littlec,M,K,Linf,Pi=YieldProbs,pas=NULL)
#print(tab_final)
tab_final[,4] <-sapply(E,BR_nkef,Littlec,M,K,Linf,Pi=YieldProbs,pas=NULL)
#print(tab_final)
}

return(tab_final)
}
 
