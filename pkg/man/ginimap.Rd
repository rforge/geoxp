\name{ginimap}
\alias{ginimap}
\title{Lorentz curve and map}
\description{
The function \code{ginimap()} computes a Lorentz curve from \code{var} and calculates the Gini Index associated to \code{var}.
}
\usage{
ginimap(long,lat,var,listvar=NULL, listnomvar=NULL,carte=NULL,criteria=NULL,
label="", cex.lab=1, pch=5, col="green", xlab="", ylab="", axes=FALSE,
lablong="", lablat="")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numeric values of size n}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"blue" by default, color of bars on the histogram}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Users have the possibility to choose a threshold by mouse clicking on the Lorentz curve or by specifying it in the menu. The corresponding
pair (F,G) and the value of `var' are then printed on the graph and the sites
with a value of \code{var} lower or equal to the threshold are then selected on the map.
} 

\note{The Gini Index is given in the tcltk window (see function gini.r for the formula used to calculate it).}
 
\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{gini}}}

\examples{
# data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)
obs<-ginimap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$INCOME,
listvar=eire.df,listnomvar=names(eire.df),carte=eire.contours,col="orange",
label=as.character(1:26),cex.lab=0.7,xlab="Income",ylab="Total Income")
}


\keyword{spatial}
\keyword{univar} 