\name{densitymap}
\alias{densitymap}

\title{Kernel density estimates and map}
\description{
The function \code{densitymap} draws kernel density estimates of the variable \code{var} with 'bkde.R'
and a map with sites of coordinate (\code{long},\code{lat}). Each site is associated to a value of \code{var} and there is interactivity between the two windows.
}
\usage{
densitymap(long, lat, var, kernel='triweight',listvar=NULL, listnomvar=NULL,
carte=NULL, criteria=NULL,label="",cex.lab=1, pch=16, col="blue", xlab="",
ylab="", axes=FALSE, lablong="", lablat="")
}

\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numeric values of size n}
  \item{kernel}{Smoothing kernel (see help(bkde) for list of options)}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{label}{vector of character of size n with names of sites}
  \item{cex.lab}{character size of label on map}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"blue" by default, color of the density curve}  
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}

\details{
The user can choose an interval on the density curve by mouse clicking on the graph on the extremities of interval or by specifying directly values. The sites selected by an interval are then represented on the map in red. 
The selection of sites on the map by `points' or `polygon' results in the drawing of the kernel densities of the subdistributions corresponding to this subset of sites. 
Finally, the user can modify the bandwith parameter with a cursor in the tcltk 
window (parameter \eqn{\alpha}{alpha}). \eqn{\alpha}{alpha} is the smoothing parameter for the kernel smooth : it represents the mean percentage of sample points involved in the local averaging (example : \eqn{\alpha=20}{alpha=20} means that on average, \eqn{n \times 0.2}{n x 0.2} points are in any interval of length 2h where h is the usual bandwidth).}

\value{A vector of boolean of size n. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} \cr \cr

Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. New York: Springer. \cr \cr

Wand M.P. et Jones M.C. (1995), \emph{Kernel Smoothing}, Chapman \& Hall.}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# data on price indices of real estate in France
data(immob)
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP)$poly
densitymap(immob$longitude,immob$latitude,immob$prix.vente,
carte=cont_midiP,listvar=immob,listnomvar=names(immob),
xlab="Prix de vente moyen par m2",label=immob$Nom,cex.lab=0.7,col='purple',pch=15)

# data olcol
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

densitymap(coords[,1], coords[,2],columbus@data$CRIME,listvar=columbus@data,
carte=cont, listnomvar=names(columbus@data),
criteria=(columbus@data$CRIME>mean(columbus@data$CRIME)))

# data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)
densitymap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$A,
carte=eire.contours,listvar=eire.df,listnomvar=names(eire.df),
xlab="Taux d'individus au groupe sanguin A",ylab="densite")

}

\keyword{spatial}
\keyword{smooth}
\keyword{univar}
\keyword{multivariate} 
