\name{histomap}
\alias{histomap}

\title{Interactive Histogram and map}
\description{
The function \code{histomap()} draws a histogram of the given variable \code{var}
and a map with sites of coordinates (\code{long},\code{lat}). Each site is associated to a value of \code{var} and there is interactivity between the two windows.
}
\usage{
histomap(long,lat,var,nbcol=10, listvar=NULL, listnomvar=NULL, criteria=NULL,
carte=NULL, label="",cex.lab=1, pch=16, col="blue", xlab="",ylab="",axes=FALSE,
lablong="",lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numeric values of size n}
  \item{nbcol}{number of cells for histogram (10 by default)}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
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
histomap(immob$longitude,immob$latitude,immob$prix.vente,
carte= cont_midiP,listvar=immob, col='grey',
listnomvar=names(immob),label=immob$Nom,cex.lab=0.6)

# data oldcol
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

histomap(coords[,1], coords[,2],columbus@data$CRIME,listvar=columbus@data, 
listnomvar=names(columbus@data),criteria=(columbus@data$CRIME>mean(columbus@data$CRIME)),
carte=cont,col="grey")

# data afcon
data(afcon)
africa <- readShapePoly(system.file("shapes/Africa.shp", package = "GeoXp")[1])
africa.contour<-spdf2list(africa)$poly
histomap(afcon$x,afcon$y,afcon$totcon,listvar=afcon,
listnomvar=names(afcon),criteria=(afcon$totcon>mean(afcon$totcon)),
label=afcon$name,cex.lab=0.6,pch=14,carte=africa.contour)

# data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)

histomap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$A,carte=eire.contours,listvar=eire.df,
listnomvar=names(eire.df),xlab="Rate of individuals with groupe sanguin A",pch=5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{univar}
