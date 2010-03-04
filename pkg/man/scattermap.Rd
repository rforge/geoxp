\name{scattermap}
\alias{scattermap}

\title{Interactive scatterplot and map}

\description{
The function \code{scattermap} draws a scatterplot of the given variables (\code{var1},\code{var2}) and a map with sites
of coordinates (\code{long},\code{lat}). Boxplots of each variable \code{var1} and \code{var2} are represented below the x-axis and y-axis.}

\usage{
scattermap(long,lat,var1,var2,listvar=NULL, listnomvar=NULL,reg.line=TRUE,quantiles=NULL,
criteria=NULL, carte=NULL,label="",cex.lab=1, pch=16, xlab="",ylab="", col="grey",
axes=FALSE, lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var1}{a vector of numeric values of size n}
  \item{var2}{a vector of numeric values of size n}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{reg.line}{If TRUE, drawing of the linear predictor for `response' in linear model}
  \item{quantiles}{a list of order of conditionnal quantile regression spline }
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{col}{"grey" by default, color of the points of the scatter plot}
  \item{axes}{a boolean with True for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{Each site is associated to a value of \code{var1} and a value of \code{var2}.
There is an interactivity between the two windows: the sites selected by `point' or `polygon' on the scatterplot are 
represented on the map in red; sites selected on the map are then represented in red on the scatterplot. Users have 
the possibility to draw linear predictor for `response' in linear model (option \code{reg.line}) or conditionnal quantile regression spline
(option \code{quantiles}).}

\value{A vector of boolean of size n. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# data on price indices of real estate in France
data(immob)
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP)$poly
scattermap(immob$longitude,immob$latitude,immob$prix.vente,
immob$prix.location,carte= cont_midiP,listvar=immob,
listnomvar=names(immob),pch=5,xlab="Prix de vente",ylab="Prix de location",
col="purple",quantiles=0.5)

# data baltimore
data(baltimore)
scattermap(baltimore$X,baltimore$Y,baltimore$PRICE,baltimore$SQFT,
listvar=baltimore, listnomvar=names(baltimore),xlab="SQFT",ylab="PRICE",
criteria=(baltimore$CITCOU==1),axes=TRUE,lablong="X",lablat="Y",
reg.line=TRUE)

# Data Eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)

scattermap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$ROADACC,
eire.df$OWNCONS, listvar=eire.df,listnomvar=names(eire.df),carte=eire.contours,
xlab="Routes",ylab="Taux de consomation",reg.line=TRUE,quantiles=0.95)
}

\keyword{regression}
\keyword{spatial}
\keyword{multivariate} 