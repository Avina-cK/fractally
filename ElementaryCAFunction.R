# Uncomment the 4 lines below to download and install library: 'binaryLogic' 
# url <- "https://cran.r-project.org/src/contrib/Archive/binaryLogic/binaryLogic_0.3.9.tar.gz"
# pkgFile <- "binaryLogic_0.3.9.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(pkgs=pkgFile, type="source", repos=NULL)

'
n     = rule number [0 to 256]
N     = length of grid
T     = number of time steps
Rand  = initial condition is random or one single live cell in the middle  
initial_state = a specific initial condition or NULL
'

ElementaryCA<-function(n, N, T, Rand, initial_state){
  library(binaryLogic)
  n<-as.binary(n, n=8)
  R0<-rep(0, N)
  GOL<-matrix(rep(0,N), ncol = N)
  if(Rand==TRUE){GOL[1,]<-sample(c(1,0),N, replace=TRUE)}
  if(Rand==FALSE){
    if (is.null(initial_state)){
      GOL[1,floor(N/2)]<-1
    } else{GOL[1,]<-initial_state}
  }
  t<-1
  rot <- function(x) t(apply(x, 2, rev))
  while(t<(T+1)){
    GOL<-rbind(GOL, c(R0))
    if(t<T){
      i=1;
        #-11
        if((GOL[t, i] == 1) && (GOL[t,i+1] == 1)){
          GOL[t+1,i]<-n[4]}
        #-10
        if((GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
          GOL[t+1,i]<-n[6]}
        
        #-01
        if((GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
          GOL[t+1,i]<-n[7]}
        
        #-00
        if((GOL[t, i] == 0) && (GOL[t,i+1] == 0)){
          GOL[t+1,i]<-n[8]}
      
      i=N;
        #11-
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 1)){
          GOL[t+1,i]<-n[2]}
        #10-
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 0)){
          GOL[t+1,i]<-n[4]}
        #01-
        if((GOL[t, i-1] == 0) && (GOL[t,i] == 1)){
          GOL[t+1,i]<-n[6]}
        #000
        if((GOL[t,i-1] == 0) && (GOL[t, i] == 0)){
          GOL[t+1,i]<-n[8]}
      
      
      for(i in 2:(N-1)){
        #111
        if((GOL[t,i-1] == 1) && (GOL[t, i] == 1) && (GOL[t,i+1] == 1)){
            GOL[t+1,i]<-n[1]}
        
        #110
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
            GOL[t+1,i]<-n[2]}
        
        #101
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
            GOL[t+1,i]<-n[3]}
        
        #100
        if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 0)){
            GOL[t+1,i]<-n[4]}
        
        #011
        if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 1)){
            GOL[t+1,i]<-n[5]}
        
        #010
        if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
            GOL[t+1,i]<-n[6]}
        
        #001
        if((GOL[t, i-1] == 0) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
            GOL[t+1,i]<-n[7]}
        
        #000
        if((GOL[t,i-1] == 0) && (GOL[t, i] == 0) && (GOL[t,i+1] == 0)){
            GOL[t+1,i]<-n[8]}
      }  
    }
    
    t<-t+1
  } 
  
  GOL<-GOL[1:T, 1:N];
  state_configuration<-0:N; time <-0:T;
  return(image( state_configuration, time
               , rot(rot(rot(GOL)))
               , axes=FALSE
               , col = gray.colors(2,0.95,0)
               , ylim = rev(range(time))
               )
  )
  
}

# Example
N = 6; T = 10;
ElementaryCA(90, N, T, FALSE, c(0,1,1,1,0,0))
axis(1, at = seq(0, N, by = 1), las=2)
axis(2, at = rev(seq(0, T, by = 1)), las=2)
grid(nx = N, ny = T,col = "grey")
