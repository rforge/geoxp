\name{dblehistomap}
\alias{dblehistomap}
\title{Double Interactive Histogram and map}
\description{
The function \code{dblehistomap()} draws two histograms of the given variables \code{var1} and \code{var2}
and a map with sites of coordinates (\code{long},\code{lat}). Each site is associated to a value of \code{var1}
and \code{var2} and there is interactivity between the two windows created.}

\usage{
dblehistomap(long, lat, var1, var2,listvar=NULL, listnomvar=NULL, nbcol=c(10,10),
carte = NULL,criteria=NULL, pch = 16, col=c("grey","blue"), xlab = c("",""),
ylab=c("count","count"), label = "",cex.lab=1,axes=FALSE, lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var1}{a vector of numeric values of size n}
  \item{var2}{a vector of numeric values of size n}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{nbcol}{a list of number of cells for histogram 1 and histogram 2 (10 by default)}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{pch}{16 by default, symbol for selected points}
  \item{col}{a list of colors for the bars on the histogram 1 and histogram 2}
  \item{xlab}{a list of title for the two x-axis graphics}
  \item{ylab}{a list of title for the two y-axis graphics}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{The selection of sites by `points' or `polygons' on the map results in the
drawing of the red histograms of the subdistributions corresponding to this subset of sites.}

\value{A vector of boolean of size n. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# data on price indices of real estate in France
data(immob)
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP)$poly
dblehistomap(immob$longitude,immob$latitude,immob$prix.vente,
immob$prix.location,carte= cont_midiP,listvar=immob,
listnomvar=names(immob),label=immob$Nom,cex.lab=0.7,xlab=c("vente","location"),
col=colors()[99:100])

# data auckland
data(auckland)
contours.auckland<-polylist2list(auckpolys)
dblehistomap(auckland$Easting,auckland$Northing,var1=auckland$Deaths.1977.85,
var2=auckland$Under.5.1981,carte=contours.auckland,
listvar=cbind(auckland,as.numeric(auckland$Deaths.1977.85>mean(auckland$Deaths.1977.85))),
listnomvar=c(names(auckland),"sup to mean"),
xlab=c("Deaths.1977.85","Under.5.1981"),
criteria=(auckland$Deaths.1977.85>mean(auckland$Deaths.1977.85)))

# data colombus
x <- readShapePoly(system.file("shapes/columbus.shp", package="maptools")[1])
colombus.contours<-spdf2list(x)$poly
coords <- coordinates(x)
colombus<-x@data

dblehistomap(coords[,1],coords[,2],
colombus$HOVAL,colombus$CRIME,xlab=c("hoval","crime"),
label=colombus$NEIGNO, cex.lab=0.7, carte=colombus.contours,
listvar=colombus,listnomvar=names(colombus))

}
\keyword{spatial}
\keyword{multivariate}
