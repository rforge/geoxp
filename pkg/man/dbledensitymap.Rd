\name{dbledensitymap}
\alias{dbledensitymap}
\title{Double Kernel density estimates and map}
\description{
The function \code{dbledensitymap} plots two kernel density estimates from \code{var1} and \code{var2},
computed with `bkde.R', and a map with sites of coordinates (\code{long},\code{lat}). Each site is associated to a value of \code{var1} and \code{var2} and
the two windows are interactive. }
\usage{
dbledensitymap(long, lat, var1, var2, kernel='triweight',listvar=NULL, 
listnomvar=NULL, carte=NULL, criteria=NULL, label="", cex.lab=1, pch=16,
col=c("grey","blue"), xlab=c("",""), ylab=c("density","density"),axes=FALSE,
lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var1}{a vector of numeric values of size n}
  \item{var2}{a vector of numeric values of size n}
  \item{kernel}{Smoothing kernel (see help(bkde) for list of options)}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables $listvar$}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{label}{vector of character of size n with names of sites}
  \item{cex.lab}{character size of label on map}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{c("grey","blue") by default, color of the two density curves}  
  \item{xlab}{a list of title for the two x-axis graphics}
  \item{ylab}{a list of title for the two y-axis graphics}
  \item{axes}{a boolean with True for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
The user can choose an interval on the density curve by mouse clicking on the lower and upper boundaries of the interval or by giving directly these values.
The selected sites are then represented on the map in red.
A selection by `points' or `polygon' on the map results in the drawing of the density of the corresponding sub-distribution on the density plot. Finally, the user can modify the bandwith parameter with a cursor in the tk window (parameter \eqn{\alpha}{alpha}). \eqn{\alpha}{alpha} is the smoothing parameter for the kernel smooth : it represents the mean percentage of sample points involved in the local averaging (example : \eqn{\alpha=20}{alpha=20} means that on average, \eqn{n \times 0.2}{n x 0.2} points are in any interval of length 2h where h is the usual bandwidth).}


\value{A vector of boolean of size n. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} \cr \cr

Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. New York: Springer. \cr \cr

Wand M.P. et Jones M.C. (1995), \emph{Kernel Smoothing}, Chapman \& Hall.
}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# data auckland
data(auckland)
contours.auckland<-polylist2list(auckpolys)
dbledensitymap(auckland$Easting,auckland$Northing,var1=auckland$Deaths.1977.85,
var2=auckland$Under.5.1981,carte=contours.auckland,
listvar=cbind(auckland,as.numeric(auckland$Deaths.1977.85>mean(auckland$Deaths.1977.85))),
 listnomvar=c(names(auckland),"sup to mean"),
xlab=c("Deaths.1977.85","Under.5.1981"),
criteria=(auckland$Deaths.1977.85>mean(auckland$Deaths.1977.85)))

# data columbus
x <- readShapePoly(system.file("shapes/columbus.shp", package="maptools")[1])
colombus.contours<-spdf2list(x)$poly
coords <- coordinates(x)
colombus<-x@data

obs<-dbledensitymap(coords[,1],coords[,2],colombus$HOVAL,
colombus$CRIME,label=colombus$NEIGNO,carte=colombus.contours,
cex.lab=0.7,listvar=colombus,listnomvar=names(colombus),
col=c("purple","pink"),xlab=c("hoval","crime"),pch=14)

# data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)
dbledensitymap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$A,eire.df$towns,
xlab=c("Individuals rate of blood type A",
"Surface urbaine"),carte=eire.contours,kernel="normal",
listvar=eire.df,listnomvar=names(eire.df))

}

\keyword{spatial}
\keyword{smooth} 