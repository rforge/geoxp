\name{mvariocloudmap}
\alias{mvariocloudmap}
\title{Interactive multivariate variocloud and map}
\description{
The function \code{mvariocloudmap()} draws a scatterplot of pairwise Mahalanobis
distances and spatial distances with a map. It is a multivariate version of the
variocloud. The number of couples of sites plotted can be reduced by considering
couples above a quantile regression curve.
}
\usage{
mvariocloudmap(long, lat, object, listvar=NULL, listnomvar=NULL,quantiles=NULL, criteria=NULL,
carte = NULL, label = "",cex.lab=1,pch = 16,col="blue", xlab = "Pairwise spatial distances",
ylab="Pairwise Mahalanobis distances", lablong = "", lablat = "", axes=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{object}{a spatial weight matrix of class nb}
  \item{listvar}{matrix of variables which permit to add graphics such as histogram, etc. and plot bubbles on map using the tlclk window}
  \item{listnomvar}{names of variables from \code{listvar}}
  \item{quantiles}{the value of alpha for representing alpha-quantile regression}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"blue" by default, color of points on the two graphics}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
  \item{axes}{TRUE for drawing axes on the map}
}
\details{
The pairwise Mahalanobis distances are calculated using the Minimum Covariance Determinant (MCD)
estimator associated with \eqn{75\%}{75\%} of observations (function \code{covMcd} in the robustbase package).
Users have the possibility to select some couples of sites on the scatterplot that are also highlightened
on the map. Selection of observations on the map is also possible and leads to the selection of all the
couples which contain the selected observations on the scatterplot.
}

\value{
A matrix of boolean of size \eqn{n \times n}{n x n}
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} }

\author{Fizmoser P., Thomas-Agnan C., Ruiz-Gazen A., Laurent T.}

\examples{
data(meuse)
data(meuse.riv)

dat.meuse<-log(1+meuse[,3:7])

# matrice des plus proches voisins
coords <- coordinates(cbind(meuse$x,meuse$y))
col.knn <- knearneigh(coords, k=7)
nb.meuse <- knn2nb(col.knn)

obs<-mvariocloudmap(meuse$x,meuse$y,nb.meuse,dat.meuse,names(dat.meuse),quantiles=0.95,col='violet',pch=7,
carte=meuse.riv[-c(1:20,73:98,156:176),],label=as.character(1:155),cex.lab=0.7)
}

\keyword{spatial}
\keyword{multivariate}
\seealso{\code{\link{misolationmap}}}
