N=516
R0<-rep(0, N)
GOL<-matrix(rep(0,N), ncol = N)
GOL[1,floor(N/2)+3]<-1
T=256
t<-1
rotate <- function(x) t(apply(x, 2, rev))
while(t<(T+1)){
  GOL<-rbind(GOL, c(R0))
  if(t<T){
    for(i in 2:(N-1)){
      #111->0
      if((GOL[t, i] == 1) && (GOL[t,i-1] == 1) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-1
      }
      
      #110->1
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-0
      }
      
      #101->0
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-0
      }
      
      #100->1
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-1
      }
      
      #011->1
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-1
      }
      
      #010->0
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-0
      }
      
      #001->1
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-1
      }
      
      #000->0
      if((GOL[t, i] == 0) && (GOL[t,i-1] == 0) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-0
      }
    }  
  }
  
  t<-t+1
}
Image(rotate(GOL), axes=FALSE)
