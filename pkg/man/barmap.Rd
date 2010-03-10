\name{barmap}
\alias{barmap}
\title{Interactive Bar plot and map}
\description{
The function \code{barmap()} draws a bar plot (vertical bar) of the given factor variable \code{var} and a map with sites of coordinates (\code{long},\code{lat}).}

\usage{
barmap(long,lat,var,listvar=NULL,listnomvar=NULL,criteria=NULL,carte=NULL,
label="",cex.lab=1,col="blue",pch=16,xlab="",ylab="",names.arg="",
axes=FALSE, lablong="", lablat="")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of factor of size n}
  \item{listvar}{matrix of variables which permit to plot bubbles on map or add a graphic using the tk window}
  \item{listnomvar}{list of character with name of variables from \code{listvar}}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours using the tcltk window : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{col}{a vector of colors which must be equal to the number of group else all sites and all bars are printed in col[1]}
  \item{pch}{a vector of symbol which must be equal to the number of group else all sites are printed in pch[1]}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{names.arg}{names of factors of \code{var}}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{The selection of a bar on the bar plot results in the corresponding 
sites coloured on the map with the corresponding colour observed on the bar.
Reversely, the selection of sites on the map by "points" or "polygon"
results in the drawing of the sub-barplot in red.
}

\value{A vector of boolean of size n. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# data oldcol 
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

barmap(coords[,1], coords[,2], columbus@data$NSA, listvar=columbus@data,
carte=cont, listnomvar=names(columbus@data),criteria=(columbus@data$CP==1),
col=colors()[101:102],pch=5:6,label=as.character(1:49),cex.lab=0.5)

# data eire
data(eire)
eire.contours<-polylist2list(eire.polys.utm)
barmap(eire.coords.utm$V1,eire.coords.utm$V2,eire.df$pale,carte=eire.contours,
listvar=eire.df,listnomvar=names(eire.df),
xlab="Appartenance to the region of Pale",names.arg=c("Hors Pale","Pale"),
col=c("purple","pink"))

}
\keyword{spatial}
\keyword{univar}
