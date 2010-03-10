\name{driftmap}
\alias{driftmap}

\title{Interactive driftplot and map}
\description{
The function \code{driftmap()} creates two devices : a device with the map of coord (\code{long},\code{lat}) which permits
to make selection of sites and a device divided into 2 rows and 2 columns which contains : (cell 1) the selected sites
divided into m rows and q columns (m and q are selected with the tk window), (cell 2) a scatter plot with \code{lat}
in x-axis and the mean and median of \code{var} calculated for the m rows in y-axis, (cell 3) a scatter plot with the mean and
median of \code{var} calculated for the q columns in x-axis and \code{long} in y-axis and (cell 4) a legend indicating the
direction of the North, the variable used, etc.}
\usage{
driftmap(long, lat, var, interpol=TRUE, nuage=FALSE,
listvar=NULL, listnomvar=NULL, carte=NULL, xlab="", col=c("blue","red","blue","pink"),
pch=c(16,16,16,16), lty=c(1,2),cex=0.7,label=NULL,
cex.lab=1,axes=FALSE)
}

\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numeric values of size n}
  \item{interpol}{if TRUE, the mean and median calculated are linearly interpoled}
  \item{nuage}{if TRUE, the values taken by var are also represented on right plot and on the left plot}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{xlab}{name of var printed with the legend plot}
  \item{col}{c("blue","red","blue","pink") by default, list of colors for (1) the mean points, (2) the median points, (3) the sites selected on the map and (4) the values of var on the left and on the right plot}
  \item{pch}{c(16,16,16,16) by default, list of symbols for (1) the mean points, (2) the median points, (3) the sites selected on the map and (4) the values of var on the left and on the right plot}
  \item{lty}{the line type for mean and median in the case of interpol=TRUE}
  \item{cex}{the amount by which plotting symbols on the cell 1 and in the case of nuage=TRUE cell 2 and cell 3}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
}

\details{
Possibility to change the number of cells in the grid with the tk window, to interpolate the means and medians calculated (by default), 
to work on a rotated map. At the begining, all sites have been selected to the map : users have to deselect sites that they 
have choosen.}

\value{No values returned, only drawing of a graphic.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}
}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{angleplotmap}},\code{\link{variocloudmap}},\code{\link{rotation}}}

\examples{
# data on price indices of real estate in France
data(immob)
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP)$poly
driftmap(immob$longitude,immob$latitude,immob$prix.vente,
carte= cont_midiP, col=colors()[35:38] ,lty=c(3,4),cex.lab=0.7,
xlab="Price of sell",nuage=TRUE,pch=c(1,3,5,7),listvar=immob,
listnomvar=names(immob))


# Data on conflicts in Africa
data(afcon)
africa <- readShapePoly(system.file("shapes/Africa.shp", package = "GeoXp")[1])
africa.contour<-spdf2list(africa)$poly
driftmap(afcon$x,afcon$y,afcon$totcon,interpol=TRUE,nuage=TRUE,
xlab="Number of conflicts", carte=africa.contour,pch=c(4,5,16,16))

# data meuse
data(meuse)
data(meuse.riv)
driftmap(meuse$x,meuse$y,meuse$copper,
nuage=TRUE,interpol=TRUE,xlab="Concentration in cuivre (in ppm)",
carte=meuse.riv)
}

\keyword{spatial} 
