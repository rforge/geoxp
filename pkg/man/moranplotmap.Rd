\name{moranplotmap}
\alias{moranplotmap}
\title{Moran scatterplot and map}
\description{
The function \code{moranplotmap()} draws a moran plot, used to detect spatial
autocorrelation in the variable var. On the x-axis, is represented \eqn{var-\bar{var}}{var-bar(var)}
and on the y-axis \eqn{W(var-\bar{var})}{W(var-bar(var))}, where W is the spatial weight matrix.
It also calcultes Moran's I statistic (see nonnormoran.r) and
give a p-value associated to the autocorrelation test (gaussian version and permutation version). }

\usage{
moranplotmap(long, lat, var, W, flower=FALSE, locmoran=FALSE, listvar=NULL, listnomvar=NULL,
carte=NULL, criteria=NULL, label="", cex.lab=1, col="blue",pch=16, xlab=expression((X-bar(X))),
ylab=expression(W(X-bar(X))), axes=FALSE, lablong="", lablat="",
names.arg=c("H.-H.","L.-H.","L.-L.","H.-L."))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numeric values of size n}
  \item{W}{A spatial weigth matrix of size \eqn{n \times n}{n x n}}
  \item{flower}{if TRUE, link neighbouring sites }
  \item{locmoran}{if TRUE, print local Moran's I statistic on the Moran plot}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables listvar}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{col}{"blue" by default, color of bars on the histogram}
  \item{pch}{16 by default, symbol for selected points}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
  \item{names.arg}{names of quadrants of the Moran plot}
}
\details{
For the permutation test, for each drawing the values of the variable $var$ are
randomly assigned to the sites. We then calculate Moran'I statistic associated to each drawing and we give the frequency of drawings when Moran'I statistic is lower or equal to the
observed Moran'I statistic. Moreover, the function gives the opportunity to link neighbouring sites and gives Local Moran's I statistic.
For a site i:
\deqn{I_i=(var_i-\bar{var})\sum_{j}W_{ij}(var_j-\bar{var})}{I_i=(var_i-bar(var))sum_jW_ij(var_j-bar(var))}
with j not equal to i.}

\note{
In the case of the spatial weigth matrix is not normalized, the Moran'I statistic is not
equal to \eqn{\beta}{beta} used in regression line for model \eqn{W(X-\bar{X})=\beta(X-\bar{X})+u}{W(X-bar(X))=beta(X-bar(X))+u}. That is why
the regression line is only drawn in the case of W normalized.}

\value{
Returns \code{obs}, vector of size n of boolean with selected sites, and Moran'I statistic \code{MORAN}.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} \cr \cr

Jim Lesage, ``Spatial Econometrics Toolbox'', \url{http://www.spatial-econometrics.com/}}

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\examples{
# data baltimore
data(baltimore)
W <- makeneighborsw(baltimore$X,baltimore$Y,method="neighbor",4)
moranplotmap(baltimore$X,baltimore$Y,baltimore$PRICE,W,label=baltimore$STATION,
criteria=(baltimore$PRICE>mean(baltimore$PRICE)),listvar=baltimore,
listnomvar=names(baltimore),flower=TRUE,locmoran=TRUE,cex.lab=0.7,pch=5)

}

\seealso{\code{\link{neighbourmap}},\code{\link{makeneighborsw}},\code{\link{normw}},\code{\link{nonormmoran}} }
