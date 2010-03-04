`distchi2` <-
function(mat)
{
n<-dim(mat)[1]
p<-dim(mat)[2]


tableau.disjonctif.vecteur <- function (x) {
  y <- matrix(0, nr=length(x), nc=length(levels(as.factor(x))))
  for (i in 1:length(x)) 
  { 
    y[i, which(levels(as.factor(x))==x[i])] <- 1
  }
  y
}


tableau.disjonctif <- function (x) {
  if( is.vector(x) )
    y <- tableau.disjonctif.vecteur(x)
  else {
    y <- NULL
    y.names <- NULL
    for (i in 1:dim(x)[2]) {
      y <- cbind(y, tableau.disjonctif.vecteur(x[,i]))
      y.names <- c(y.names, paste(dimnames(x)[[2]][i],"_", levels(as.factor(x[,i])), sep='') )
    }
  }
  colnames(y) <- y.names
  y
}

disj<-tableau.disjonctif(mat)
#c<-dim(disj)[2]

tot<-apply(disj,2,sum)

dist<-matrix(0,n,n)

 for(i in 2:n)
 {for(j in 1:(i-1))
  { 
   dist[i,j]=n/p*sum(matrix(abs(disj[i,]-disj[j,]))/tot)
  }
 }
   return(dist)
}

