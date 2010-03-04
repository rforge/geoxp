\name{plot3dmap}
\alias{plot3dmap}

\title{Interactive Plot3d and map}
\description{
The function \code{histomap()} draws a histogram of the given variable $var$
and a map with sites of coordinates (\code{long},\code{lat}). Each site is associated to a value of \code{var} and there is interactivity between the two windows.
}
\usage{
plot3dmap(long,lat,var1,var2,var3,box=TRUE, listvar=NULL, listnomvar=NULL,
criteria=NULL, carte=NULL, label="",cex.lab=1, pch=16, col="blue", xlab="",
ylab="",zlab="", axes=FALSE,lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var1}{a vector of numeric values of size n}
  \item{var2}{a vector of numeric values of size n}
  \item{var3}{a vector of numeric values of size n}
  \item{box}{a boolean with TRUE for drawing a box on the scatterplot 3d}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"blue" by default, color of points on the scatterplot 3d}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{zlab}{a title for the graphic z-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Sites selected by a bar on the histogram are represented on the map in red and the values of sites selected on the map by `points' or `polygon' are 
represented in red as a sub-histogram on the histogram. 
}
\value{
A vector of boolean of size n. TRUE if the site was in the last selection.
}
\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}
\examples{
# data on price indices of real estate in France
data(immob)
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP)$poly
plot3dmap(immob$longitude,immob$latitude,immob$prix.vente,immob$prix.location,
immob$variation.vente,box=FALSE, carte= cont_midiP,listvar=immob, col='purple',
listnomvar=names(immob),label=immob$Nom,cex.lab=0.6,xlab="prix.vente",
ylab="prix.location",zlab="variation.vente")

# data oldcol
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

plot3dmap(coords[,1], coords[,2],columbus@data$CRIME,columbus@data$HOVAL,
columbus@data$INC, xlab='Crime',ylab='Hoval',zlab='Income',listvar=columbus@data,
listnomvar=names(columbus@data),criteria=(columbus@data$CRIME>mean(columbus@data$CRIME)),
carte=cont,col="grey", label=as.character(1:length(columbus@data$X)),cex.lab=0.7)


# data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)

plot3dmap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$A,eire.df$RETSALE,
eire.df$INCOME,carte=eire.contours,listvar=eire.df,
listnomvar=names(eire.df),xlab="A",ylab="RETSALE",zlab="INCOME")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{univar}
