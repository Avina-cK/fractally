'
n     = rule number
N     = length of grid
T     = number of time steps
Rand  = initial condition is random or one single live cell in the middle  
'

ElementaryCA<-function(n, N,T, Rand){
  library(binaryLogic)
  n<-as.binary(n, n=8)
  R0<-rep(0, N)
  GOL<-matrix(rep(0,N), ncol = N)
  if(Rand==TRUE){GOL[1,]<-sample(c(1,0),N, replace=TRUE)}
  if(Rand==FALSE){GOL[1,floor(N/2)]<-1}
  t<-1
  rot <- function(x) t(apply(x, 2, rev))
  while(t<(T+1)){
    GOL<-rbind(GOL, c(R0))
    if(t<T){
      for(i in 2:(N-1)){
        #111
        if((GOL[t, i] == 1) && (GOL[t,i-1] == 1) && (GOL[t,i+1] == 1)){GOL[t+1,i]<-n[1]}
        
        #110
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){GOL[t+1,i]<-n[2]}
        
        #101
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){GOL[t+1,i]<-n[3]}
        
        #100
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 0)){GOL[t+1,i]<-n[4]}
        
        #011
        if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 1)){GOL[t+1,i]<-n[5]}
        
        #010
        if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){GOL[t+1,i]<-n[6]}
        
        #001
        if((GOL[t, i-1] == 0) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){GOL[t+1,i]<-n[7]}
        
        #000
        if((GOL[t, i] == 0) && (GOL[t,i-1] == 0) && (GOL[t,i+1] == 0)){GOL[t+1,i]<-n[8]}
      }  
    }
    
    t<-t+1
  }  
  
  return(
    image(rot(GOL), axes=FALSE, col = gray.colors(2,0.95,0))
  )
}
