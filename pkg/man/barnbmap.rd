\name{barnbmap}
\alias{barnbmap}
\title{Bar plot of the number of neighbour and map}
\description{
The function \code{barnbmap} draws the bar plot of the link number distribution for
a neighbourhood structure given by a nb object (spdep package) and links this bar plot with a map.
}

\usage{
barnbmap(object, coords = NULL, listvar=NULL, listnomvar=NULL,
carte=NULL, criteria=NULL, label="", col="blue",pch=16, xlab="",
ylab="", cex.lab=1, axes=FALSE, lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{object}{a object of class nb}
  \item{coords}{a matrix of size \eqn{n \times 2}{n x 2}}
  \item{listvar}{matrix of variables which permit to plot bubbles using the tk window}
  \item{listnomvar}{names of variables listvar}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of
  the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{label}{vector of character of size n with names of sites}
  \item{col}{a vector of colors which may be equal to the number of group else all sites and all bars are printed in col[1]}
  \item{pch}{a vector of symbol which may be equal to the number of group else all sites are printed in pch[1]}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{cex.lab}{character size of label}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
For a selected site j on the map, are represented on the map its neighbours.
For a selected bar on the graph, the corresponding sites are represented on the map with a link which means that two sites
are neighbours.}

\note{
When user select sites on the graph or on the map, he cannot add a selection by using the other graphic.}

\value{
No value returned.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009),
\emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Laurent T.}

\keyword{spatial}

\examples{
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

barnbmap(col.gal.nb, coords,carte=cont,listvar=columbus@data,
listnomvar=names(columbus),criteria=which(coords[,1]>mean(coords[,1])),
col=colors()[98:106], label=as.character(1:49),cex.lab=0.7,pch=1:9)
}

\seealso{\code{\link{moranplotmap}},\code{\link{makeneighborsw}},\code{\link{normw}},\code{\link{nonormmoran}} }

