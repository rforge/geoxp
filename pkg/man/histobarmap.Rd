\name{histobarmap}
\alias{histobarmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Histogram, barplot and map}

\description{
The function \code{histobarmap()} draws a bar plot (vertical bar) of the given variable \code{var1},
a histogram of the given variable \code{var2} and a map with sites of coordinates (\code{long},\code{lat}).}

\usage{
histobarmap(long,lat,var1,var2,criteria=NULL,carte=NULL, label = "",cex.lab=1,
pch = 16, col= "grey", nbcol = 10,names.arg="", xlab=c("",""), ylab=c("",""),
listvar=NULL, listnomvar=NULL, axes=FALSE,lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var1}{a vector of factor of size n}
  \item{var2}{a vector of numerical values of size n}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"grey" by default, color of bars on the histogram}
  \item{nbcol}{number of cells for histogram (10 by default)}
  \item{names.arg}{names of factors of \code{var1}}
  \item{xlab}{a list of title for the two x-axis graphics}
  \item{ylab}{a list of title for the two y-axis graphics}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{a list with names of variables \code{listvar}}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Each site is associated to a factor of \code{var1} and to a value of \code{var2}.
There is interactivity between the three windows created : the sites selected by a bar on the bar plot or on the histogram are represented on the map in red and the value and factor of sites selected on the map are represented in red on the bar plot and on the histogram.   
}

\value{A vector of boolean of size n. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}},  \code{\link{dbledensitymap}}}



\examples{
# Data Colombus
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly
obs<-histobarmap(coords[,1], coords[,2],columbus@data$CP,
columbus@data$HOVAL,label=columbus@data$NEIGNO, carte=cont,
listvar=columbus@data,listnomvar=names(columbus@data),col=colors()[100:101],
xlab=c("CP","Hoval"),ylab=c("Count","Count"),names.arg=c("A","B"),pch=2)

# Data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)
histobarmap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$pale,eire.df$A,carte=eire.contours,
listvar=eire.df,listnomvar=names(eire.df),
xlab=c("Appartenance à la région du Pale","Taux moyen de groupe sanguin A"),
names.arg=c("Hors Pale","Pale"),
col=colors()[101:102],label=as.character(1:26),cex.lab=0.7,pch=c(3,7))
}

\keyword{spatial}
\keyword{multivariate} 