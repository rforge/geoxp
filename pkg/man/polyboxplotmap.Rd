\name{polyboxplotmap}
\alias{polyboxplotmap}
\title{Interactive polyboxplot and map}
\description{
Parallel Boxplots of a numerical variable by levels of a factor. 
It interacts with a map.
}
\usage{
polyboxplotmap(long, lat, var1, var2, varwidth=FALSE,listvar = NULL, listnomvar = NULL,
carte = NULL, criteria = NULL, label = "", cex.lab=1, names.arg = "", xlab = "",
ylab= "count", pch = 16, col = "grey", axes = FALSE, lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var1}{a vector of factor of size n}
  \item{var2}{a vector of numerical values of size n}
  \item{varwidth}{if \code{varwidth} is TRUE, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{names.arg}{names of factors of var1}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"grey" by default, color of the points of the scatter plot}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Each site is associated to a value of a level of $var1$ and of a value of $var2$. 
There is an interactivity only in one direction : the sites selected by quartile-intervals on one of the boxplots 
are then represented on the map in red (or colors according to the options). 
}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{dbledensitymap}}}


\examples{
# Data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)

polyboxplotmap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$pale,
eire.df$POPCHG, listvar=eire.df,listnomvar=names(eire.df),carte=eire.contours,
xlab="Appartenance to the region of Pale", names.arg=c("Hors Pale","Pale"),
col=c("purple","violet"))
}
\keyword{spatial}
\keyword{multivariate} 