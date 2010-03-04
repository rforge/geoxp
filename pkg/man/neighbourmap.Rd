\name{neighbourmap}
\alias{neighbourmap}
\title{Neighbour plot and map}
\description{
The function \code{neighbourmap()} identifies spatial outliers by comparing a variable value for
a particular site with these of its neighbouring sites. It draws a scatterplot of the values of the
variable at neighbouring sites for a neighbourhood structure given by a binary weight matrix W and
links this scatterplot with a map.
}

\usage{
neighbourmap(long, lat, var, W, id=FALSE, listvar=NULL, listnomvar=NULL, carte=NULL,
criteria=NULL, label="", cex.lab=1, pch=16, col="blue", xlab="",ylab="",
axes=FALSE, lablong="", lablat="") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numeric values of size n}
  \item{W}{A spatial weigth matrix of size \eqn{n \times n}{n x n}}
  \item{id}{If TRUE, drawing the linear curve y=x}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"blue" by default, color of the points on the neighbor plot}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
For a selected site j on the map, are represented on the map its neighbors, and on the graph, on the x-axis the value of \code{var} for this site, and in y-axis, the values of \code{var} for the neighbouring sites of j. For a selected point on the graph, the corresponding pair of sites is represented on the map with a link.}

\note{
When user selects sites on the graph or on the map, he cannot add a selection by using the other graphic.}

\value{
Returns a matrix of size \eqn{n \times n}{n x n} of boolean with TRUE if pair (i,j) was in the last selection.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\examples{
# data oldcol

columbus <- readShapePoly(system.file("etc/shapes/columbus.shp",
package="spdep")[1])

list.OLD <- spdf2list(columbus)
contours.OLD <- list.OLD$poly
x.OLD <- list.OLD$X
y.OLD <- list.OLD$Y

W <- makeneighborsw(x.OLD , y.OLD,method="neighbor",4)
obs<-neighbourmap(x.OLD , y.OLD,columbus@data$CRIME, W,carte=contours.OLD,
criteria=(columbus@data$CRIME>mean(columbus@data$CRIME)),listvar=columbus@data,
listnomvar=names(columbus@data),id=TRUE,col="grey",pch=4)
}

\seealso{\code{\link{moranplotmap}},\code{\link{makeneighborsw}},\code{\link{normw}},\code{\link{nonormmoran}} }

