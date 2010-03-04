\name{clustermap}
\alias{clustermap}
\title{Classification of dataset using kmeans or hclust algorithm and representation of clusters on a map.}
\description{
The function \code{clustermap()} performs a classification of the sites from the variables included in $dataset$ and computes a bar plot of the clusters calculated.
Classification methods comes from \code{hclust()} (hierarchical cluster analysis) and \code{kmeans()} (k-means clustering) and number of class is chosen with \code{clustnum}.
}
\usage{
clustermap(long, lat, dataset, clustnum, method="kmeans", type=NULL,
centers=NULL, scale=FALSE, listvar=NULL, listnomvar=NULL, carte=NULL,
criteria=NULL, xlab="Cluster", ylab="Count", label="", cex.lab=1, pch=16,
col="blue", names.arg="", axes=FALSE, lablong="", lablat="")
}

\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{dataset}{matrix of variables of numeric values}
  \item{clustnum}{Number of clusters}
  \item{method}{two methos : `kmeans' by default or `hclust'}
  \item{type}{If method=`hclust'', type=``complete'' by default  (the possibilities are given in help(hclust)
  as `ward', `single', etc). If method=`kmeans', type="Hartigan-Wong" by default (the possibilities are given in help(kmeans)
  as `Forgy', etc)}
  \item{centers}{If method='kmeans', user can give a matrix with initial cluster centers.}
  \item{scale}{If scale=TRUE, the dataset is reducted.}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{list of character with name of variables from $listvar$}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{label}{vector of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{a vector of symbol which must be equal to the number of group else all sites are printed in pch[1]}
  \item{col}{a vector of colors which must be equal to the number of group else all sites and all bars are printed in col[1]}
  \item{names.arg}{names of Clusters}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
The two windows are interactive : the sites selected by a bar chosen on the bar plot 
are represented on the map in red and the values of sites selected on the map by `points' or `polygon' are 
represented in red on the bar plot. The dendogram is also drawn for 'hclust' method.  
In option, possibility to choose the classification method. 
}
\value{
A vector of boolean of size n (TRUE if the site was in the last selection) and a vector of size n whith the number of cluster for each site
}
\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} \cr \cr

Murtagh, F (1985). \emph{Multidimensional Clustering Algorithms}. \cr \cr

Hartigan, J. A. and Wong, M. A. (1979). A K-means clustering algorithm. Applied Statistics 28, 100-108}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\note{To use the functions hclust.r and kmeans.r, we take many arguments by default.
If the user would like to modify these arguments, he should call these functions first and
then use the function barmap.r to visualize the calculated clusters. }

\seealso{\code{\link{barmap}}, \code{\link{pcamap}}}

\examples{
# données oldcol
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

obs<-clustermap(coords[,1], coords[,2],columbus@data[,7:12],3,listvar=columbus@data,
listnomvar=names(columbus@data),carte=cont,criteria=(columbus@data$CP==1),
col=colors()[20:22],label=as.character(1:49),cex.lab=0.7)

obs<-clustermap(coords[,1], coords[,2],columbus@data[,7:12],3,method="hclust",
listvar=columbus@data,listnomvar=names(columbus@data),carte=cont,
criteria=(columbus@data$CP==1),col=colors()[20:22],label=as.character(1:49),
cex.lab=0.7)

# Extract for GeoXp manual
# data avifaune 
#library(ade4)
#data(mafragh)
#d<-distchi2(mafragh$flo)

#obs<-clustermap(x<-mafragh$xy[,1],y<-mafragh$xy[,2],d,
#4,method="hclust",type="ward",
#listvar=mafragh$partition,listnomvar="class")

#clas<-obs$vectclass
#e<-by(d,clas,mean)
#g<-rbind(e[1]$'1',e[2]$'2',e[3]$'3',e[4]$'4')


#clustermap(x<-mafragh$xy[,1],y<-mafragh$xy[,2],
#d,4,centers=g,listvar=mafragh$partition,listnomvar="class")
}

\keyword{spatial}
\keyword{multivariate}
