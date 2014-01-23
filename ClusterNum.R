ClusterNum<-function(X,d="euclidean",method="ward",hclust=NULL){
#X is required
  if(class(hclust)=="NULL"){
     X<-data.frame(scale(X));
     d<-dist(X,d);
     hc<-hclust(d,method);
     N<-dim(X)[1];
  }else if(class(hclust)=="hclust"){
     hc<-hclust;
     N<-length(hc$order);
  }else{
     stop("The class of hclust is wrong!");
  }  
  R2<-seq(0,1,length=N);
  PSF<-rep(0,N);PST2<-PSF;
  T<-sum(diag(cov(X)*(N-1)));
  Nm<-rep(0,N-1);Wm<-rep(0,N-1);
  pdf("cluster");plot(1,type="n");
  rect0<-rect.hclust(hc,k=2);
  for(t in 2:(N-1)){
    rect<-rect0;
    if(t<N-1){
       rect0<-rect.hclust(hc,k=t+1);
    }
    b<-colMeans(X[rect[[1]],])-colMeans(X);
    B<-length(rect[[1]])*t(b)%*%b;
    for(j in 1:t){
        if(length(rect[[j]])!=length(rect0[[j]])){
        break;}
      }
    Nm[t]<-length(rect[[j]]);
    Wm[t]<-sum(diag(cov(X[rect[[j]],])*(length(rect[[j]])-1)));
    for(i in t:2){
      b<-colMeans(X[rect[[i]],])-colMeans(X);
      B<-B+length(rect[[i]])*t(b)%*%b;
    }
   R2[t]<-B/T;
   PSF[t]<-(B*(N-t))/((T-B)*(t-1));
  }
  dev.off();
  R1<-R2[-1];SPRSQ<-matrix(R1-R2[1:(N-1)]);
  Bkl<-SPRSQ*T;
  PST2<-(Nm-2)*Bkl/(Wm-Bkl);
  PST2[1]<-(N-2)*Bkl[1]/(T-Bkl[1]);
  PST2[c(N-1,N-2)]<-0;
  data.frame(RSQ=R2[1:(N-1)],SPRSQ,PSF=PSF[1:(N-1)],PST2);
}

#a simple example
data(USArrests);
ClusterNum(USArrests);