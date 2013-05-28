#' Minimal doc
#' @import compiler PBSmodelling animation
#' @description minimal documentation for roxygen purposes and could be added later 
#' @export

yield_biomass_per_recruit <- function (M,K,Littlec,Linf,Pi=YieldProbs,pas=NULL)
{

  
Lc=Littlec*Linf #convert from little c to Lc 
M=M*K #convert from M/K to M
#print("HI I am in ypr to party!")
if (is.null(pas)) pas <- 0.05
#if (is.null(pas2)) pas2 <- 1

# Y/R Knife-edge

E <- seq(from=0, to=1, by=pas)
small_c <- Lc/Linf
U <- 1-small_c
m <- (1-E)/(M/K)

YR_ke <- (E*(U^m))*(1-((3*U)/(1+m))+((3*(U^2))/(1+(2*m)))-((U^3)/(1+(3*m))))
print(sum(Pi))
if(!is.na(sum(Pi))){
  
# Y/R Not Knife-edge
#l1 <- seq(from=0, to=(Linf-pas2), by=pas2)
#l2 <- seq(from=pas2, to=Linf, by=pas2)
l1 <- datain$ML-(datain$ML[2]-datain$ML[1])/2
l2 <- datain$ML+(datain$ML[2]-datain$ML[1])/2
U1 <- 1-(l1/Linf)
U2 <- 1-(l2/Linf)
#print("l1!")
#print(l1)
#print("l2!")
#print(l2)
#print("Pi")
#print(Pi)
#print("U1")
#print(U1)
#print("U2")
#print(U2)

YR_nke <- c()
for (i in 1:length(E))
{
ri <- (U2^((M/K)*(E[i]/(1-E[i]))*Pi))/(U1^((M/K)*(E[i]/(1-E[i]))*Pi))
Gi <- c()
Gi[1]=ri[1]
for (j in 2:length(ri)) {Gi[j] <- Gi[j-1]*ri[j]}

yr1 <- (E[i]*U1^m[i])*((1-(3*U1)/(1+m[i]))+(3*(U1^2)/(1+(2*m[i])))-((U1^3)/(1+(3*m[i]))))
yr2 <- (E[i]*U2^m[i])*((1-(3*U2)/(1+m[i]))+(3*(U2^2)/(1+(2*m[i])))-((U2^3)/(1+(3*m[i]))))

t1 <- c()
t2 <- c()
t1[1] <- NA
t2[1] <- NA
#print("HI I am about to loop!")

for (k in 2:length(U1))
{
#print("t1[k]")
#print(t1[k])
#print("Gi[k-1]*yr1[k]")
#print(Gi[k-1]*yr1[k])
#print(k)

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

YR_nke[i] <- cum_yr[length(cum_yr)]
}
}

# B/R Knife-edge

m_prim <- 1/(M/K)
D <- 1-(3*U/(1+m_prim))+(3*U^2/(1+(2*m_prim)))-(U^3/(1+(3*m_prim)))
N <- (1-E)*(1-((3*U)/(1+m))+((3*U^2)/(1+(2*m)))-(U^3/(1+(3*m))))
BR_ke <- N/D

if(!is.na(sum(Pi))){
# B/R Not Knife-edge
m_prim2 <- m/(1-E)
BR_nke <- c()
for (i in 1:length(E))
{
ri <- (U2^((M/K)*(E[i]/(1-E[i]))*Pi))/(U1^((M/K)*(E[i]/(1-E[i]))*Pi))
Gi <- c()
Gi[1]=ri[1]
for (j in 2:length(ri)) {Gi[j] <- Gi[j-1]*ri[j]}

N1 <- (1-E[i])*(1-(3*U1/(1+m[i]))+((3*U1^2)/(1+(2*m[i])))-((U1^3)/(1+(3*m[i]))))
N2 <- (1-E[i])*(1-(3*U2/(1+m[i]))+((3*U2^2)/(1+(2*m[i])))-((U2^3)/(1+(3*m[i]))))
D1 <- 1-((3*U1)/(1+m_prim2[i]))+((3*U1^2)/(1+(2*m_prim2[i])))-((U1^3)/(1+(3*m_prim2[i])))
D2 <- 1-((3*U2)/(1+m_prim2[i]))+((3*U2^2)/(1+(2*m_prim2[i])))-((U2^3)/(1+(3*m_prim2[i])))

br1 <- (1-E[i])*(N1/D1)
br2 <- (1-E[i])*(N2/D2)

t1 <- c()
t2 <- c()
t1[1] <- NA
t2[1] <- NA
for (k in 2:length(U1))
{
  
#print("t1[k]")
#print(t1[k])
#print("Gi[k-1]*yr1[k]")
#print(Gi[k-1]*yr1[k])
#print(k)

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
Si <- (M/K)*(E[i]/(1-E[i]))*Pi
rj <- (U2^Si)/(U1^Si)

gi <- c()
gi[1]=rj[1]
for (j in 2:length(rj)) {gi[j] <- gi[j-1]*rj[j]}

cum_bri <- (1-E[i])*(N1/D1)

BR_nke[i] <- cum_bri[length(cum_bri)]
}
}
if(is.na(sum(Pi)))
  { #make things = na
    YR_nke <- BR_nke <- NA
  }
tab_names <- c('YR_ke','YR_nke','BR_ke','BR_nke')
tab_final <- array(dim=c(length(E),4),dimnames=(list(E,tab_names)))
tab_final[,1] <- YR_ke
tab_final[,2] <- YR_nke
tab_final[,3] <- BR_ke
tab_final[,4] <- BR_nke
return(tab_final)
}
