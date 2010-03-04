\name{boxplotmap}
\alias{boxplotmap}
\title{Interactive boxplot and map}
\description{
The function \code{boxplotmap()} draws a boxplot of the given variable $var$
and a map with site of coordinates (\code{long},\code{lat}). }

\usage{
boxplotmap(long,lat,var,listvar=NULL, listnomvar=NULL,carte=NULL,criteria=NULL,
label="", cex.lab=1,pch=16,col="grey",xlab="",ylab="",axes=FALSE, 
lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numerical values of size n}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{list of character of size p with names of variables from \code{listvar}}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{label}{vector of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"grey" by default, color of the boxplot}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
There is an interactivity only in one direction : the sites selected by interquartile on the boxplot 
are represented on the map in red. 
}

\value{A vector of boolean of size n. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# donnees oldcol
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

boxplotmap(coords[,1], coords[,2],columbus@data$CRIME,listvar=columbus@data,
carte=cont, listnomvar=names(columbus@data),criteria=(columbus@data$CP==1),
xlab="Crime",ylab="Percent",label=columbus@data$COLUMBUS.I,cex.lab=0.7) 

# donnees boston
data(boston)
obs<-boxplotmap(boston.utm[,1],boston.utm[,2],boston.c$CRIM,listvar=boston.c,
listnomvar=names(boston.c),criteria=(boston.c$CHAS==1)) 

# données eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)
boxplotmap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$POPCHG,
listvar=eire.df,listnomvar=names(eire.df),carte=eire.contours,
xlab="Taux d'évolution de la population de 1921 à 1962",pch=7)
}

\keyword{spatial}
\keyword{univar} 