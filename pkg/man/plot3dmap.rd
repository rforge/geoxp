\name{plot3dmap}
\alias{plot3dmap}

\title{Interactive Plot3d and map}
\description{
The function \code{plot3dmap()} draws a 3d-plot of three given variables $names.var$
and a map with sites of coordinates \code{coordinates(sp.obj)}.
}
\usage{
plot3dmap(sp.obj, names.var, box=TRUE,
names.attr=names(sp.obj), criteria=NULL, carte=NULL, identify=FALSE, cex.lab=0.8,
pch=16, col="lightblue3",xlab="", ylab="", zlab="", axes=FALSE, lablong="", lablat="")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp.obj}{object of class extending Spatial-class}
  \item{names.var}{a vector of three characters; attribute names or column numbers in attribute table}
  \item{box}{a boolean with TRUE for drawing a box on the scatterplot 3d}
  \item{names.attr}{names to use in panel (if different from the names of variable used in sp.obj)}
  \item{criteria}{a vector of boolean of size the number of Spatial units, which permit to represent preselected sites with a cross, using the tcltk window}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{identify}{if not FALSE, identify plotted objects (currently only working for points plots). Labels for identification are the row.names of the attribute table row.names(as.data.frame(sp.obj)).}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"lightblue3" by default, color of bars on the histogram}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{zlab}{a title for the graphic z-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Sites selected on the map by `points' or `polygon' are
represented in red in the 3-d plot.
}
\note{
This function uses the \code{rgl} package and open a \code{rgl} device.
}

\value{
In the case where user click on \code{save results} button,
a vector of integer is created as a global variable in \code{last.select} object.
It corresponds to the number of spatial units selected just before leaving the Tk window.
}

\references{Thibault Laurent, Anne Ruiz-Gazen, Christine Thomas-Agnan (2012), GeoXp: An R Package for Exploratory Spatial Data Analysis. \emph{Journal of Statistical Software}, 47(2), 1-23.}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{scattermap}}}
\examples{
# data on price indices of real estate in France
######
# data on price indices of real estate in France
data(immob)
row.names(immob)<-immob$Nom

# immob is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
immob.sp = SpatialPoints(cbind(immob$longitude,immob$latitude))
# ... and then by integrating other variables to create SpatialPointsDataFrame
immob.spdf = SpatialPointsDataFrame(immob.sp, immob)
# For more details, see vignette('sp', package="sp")

# optional : we add some contours that don't correspond to the spatial unit
# but are nice for mapping
if (require("maptools", quietly=TRUE)) {
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP[-c(22,23),])$poly

# an example of plot3dmap
plot3dmap(immob.spdf, c("prix.vente","prix.location","variation.vente"),
box=FALSE, carte=cont_midiP, identify=TRUE, cex.lab=0.5,xlab="prix.vente",
ylab="prix.location", zlab="variation.vente")

}
######
# data eire
if (require("spData", quietly=TRUE) && require("rgdal", quietly=TRUE)) {
eire <- readOGR(system.file("shapes/eire.shp", package="spData")[1])
row.names(eire) <- as.character(eire$names)
proj4string(eire) <- CRS("+proj=utm +zone=30 +units=km")

# an example of use
plot3dmap(eire, c("A","RETSALE","INCOME"), xlab="A",ylab="RETSALE",zlab="INCOME")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{multivariate}
