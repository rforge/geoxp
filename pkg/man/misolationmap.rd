\name{misolationmap}
\alias{misolationmap}
\title{Interactive multivariate isolation plot and map}
\description{
The function \code{misolationmap} draws a scatterplot with the pairwise Mahalanobis distances between the observations and their neighbors on the y-axis and the "degree of isolation" of the observations on the x-axis and a map
}
\usage{
misolationmap(long, lat, object,propneighb=0.4,chisqqu=0.975, listvar=NULL, listnomvar=NULL, criteria=NULL,
carte = NULL, label = "",cex.lab=1,pch = 16,col="blue", xlab = "degree of isolation",
ylab="Pairwise Mahalanobis distances", lablong = "", lablat = "", axes=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{object}{a spatial weight matrix of class nb}
  \item{propneighb}{proportion of neighbors included in ellipsoid}
  \item{chisqqu}{value of alpha for the definition of global outliers}
  \item{listvar}{matrix of variables which permit to add graphics such as histogram, etc. and plot bubbles on map using the tlclk window}
  \item{listnomvar}{names of variables from \code{listvar}}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"blue" by default, color of the points on the two graphics}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
  \item{axes}{TRUE for drawing axes on the map}
}
\details{
The pairwise Mahalanobis distances are calculated using the robust Minimum Covariance Determinant (MCD)
estimator associated with \eqn{75\%}{75\%} of observations (function \code{covMcd} in the robustbase package).
For each observation, the degree of isolation is a chi-square quantile of the conditional distribution of the
pairwise Mahalanobis distances associated with the ellipsoid containing the proportion \code{propneighb}
of neighbors. The parameter \code{propneighb} gives the proportion of neighbors that is expected to be quite
similar to the observation in order to conclude that the observation is not a local outlier.
Under independence and normality conditions, the user can expect a degree of isolation close by the parameter
\code{propneighb} (vertical line on the scatterplot). An observation with a high degree of isolation is suspected
to be a local outlier. Users have also the possibility to plot bubbles on the map which size depends on the
robust Mahalanobis distance of each observation to the center of the distribution (function \code{arw} in the
package mvoutlier).
}

\value{
A matrix of boolean of size \eqn{n \times n}{n x n}
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Fizmoser P., Thomas-Agnan C., Ruiz-Gazen A., Laurent T.,}

\examples{
data(moss)
data(kola.background)
xy <- moss[,c("XCOO","YCOO")]
xy.knn <- knearneigh(as.matrix(xy), k=15)
#xy.knn <- dnearneigh(as.matrix(xy), 0,50000 )

nb.kola <- knn2nb(xy.knn)
dat.kola <- log10(moss[, c("Ag","As","Bi","Cd","Co","Cu","Ni")])

obs<-misolationmap(xy$XCOO,xy$YCOO,nb.kola,propneighb=0.40,chisqqu=0.975,dat.kola,names(dat.kola),col='royalblue',pch=7,
carte= kola.background,cex.lab=0.7)

}

\keyword{spatial}
\keyword{multivariate}
\seealso{\code{\link{mvariocloudmap}}}
