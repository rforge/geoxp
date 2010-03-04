\name{pcamap}
\alias{pcamap}
\title{Generalized Principal Component Analysis and map}
\description{
The function \code{pcamap()} draws the plots summarizing a generalized Principal Component Analysis (PCA), made with `genpca.r'. It draws the scatterplot of the individuals projected on a chosen principal component plane (with their percentage of inertia), together with the scatterplot of the variables projected into the same plane with the quality of representation in order to interpret the principal component axes. The individuals scatterplot interacts with the map.
}
\usage{
pcamap(long,lat,dataset,name.dataset=NULL,direct=c(1,2),weight=rep(1/nrow(dataset),length=nrow(dataset)),
metric=diag(ncol(dataset)), center=NULL, reduce=TRUE,qualproj=FALSE,
listvar=NULL, listnomvar=NULL,criteria=NULL,carte=NULL,label="",cex.lab=1, pch=16, col="grey", 
axes=FALSE, xlab=as.character(direct[1]), ylab=as.character(direct[2]), lablong="", lablat="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{dataset}{ matrix \eqn{n \times p}{n x p} of variables}
  \item{name.dataset}{List of Names of dataset}
  \item{direct}{Two-dimension vector containing the numbers of principal axes to plot}
  \item{weight}{vector of size n of weight (by default : weight=t(1/n,...,1/n))}
  \item{metric}{ matrix \eqn{p \times p}{p x p} (by default : metric=Identity matrix)}
  \item{center}{A vector of size p of the gravity center which by default is equal to \eqn{\bar X}{bar(X)}}
  \item{reduce}{if TRUE, reduced PCA}
  \item{qualproj}{if TRUE, print the quality of representation of individuals }
  \item{listvar}{matrix of variables which permit to add graphics such as histogram, etc. and plot bubbles on map using the tlclk window}
  \item{listnomvar}{names of variables from \code{listvar}}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with name of site. Names are printed on map after a selection}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"grey" by default, color of the two graphics}
  \item{axes}{TRUE for drawing axes on the map}
  \item{xlab}{a title for the two graphics x-axis}
  \item{ylab}{a title for the two graphics y-axis}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Let \deqn{D=diag(\lambda_1,...,\lambda_p)}{D=diag(lambda_1,...,lambda_p)}
\deqn{1_p=(1,...,1)'}{1_p=(1,...,1)'}
Let the coordinates of individuals in the principals components \deqn{CC=(C_1',...,C_n')'}{CC=(C_1',...,C_n')'}
with \eqn{C_i=(C_i^1,...,C_i^p)}{C_i=(C_i^1,...,C_i^p)} \cr
Let the coordinates of variables in the principals components \deqn{CC=(V_1',...,V_p')'}{CC=(V_1',...,V_p')'}
with \eqn{V_i=(V_i^1,...,V_i^p)}{V_i=(V_i^1,...,V_i^p)} \cr
Part of inertia : \deqn{(\frac{\lambda_1}{\sum_i\lambda_i},...,\frac{\lambda_p}{\sum_i\lambda_i})'}{(frac(lambda_1)(sum_ilambda_i),...,frac(lambda_p)(sum_ilambda_i))'}
Quality of representation of individual k projected on plane (i,j):
\deqn{Qu=\sqrt{\frac{(C_k^i)^2+(C_k^j)^2}{\sum_l(C_k^l)^2}}}{Qu=sqrt(frac((C_k^i)^2+(C_k^j)^2)(sum_l(C_k^l)^2)}
Quality of representation of variable k projected on plane (i,j):
\deqn{VQu=\sqrt{\frac{(V_k^i)^2+(V_k^j)^2}{\sum_l(V_k^l)^2}}}{VQu=sqrt(frac((V_k^i)^2+(V_k^j)^2)(sum_l(V_k^l)^2))}
}

\value{
Return `obs', vector of size n of boolean with sites selected, `inertia' vector of size p with percent of inertia of each component, `casecoord' matrix \eqn{n \times p}{n x p} of indivuduals, `varcoord' matrix \eqn{n \times p}{n x p} of principal components.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} \cr \cr

Caussinus H., Fekri M., Hakam S., Ruiz-Gazen A. (2003) , \emph{A monitoring display of Multivariate Outliers}, Computational Statistics and Data Analysis, vol. 44, 1-2, 237-252.}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\examples{
# Data Colombus
example(columbus)
coords <- coordinates(columbus)
cont<-spdf2list(columbus)$poly

obs<-pcamap(coords[,1], coords[,2],columbus@data[,6:12],
label=columbus@data$NEIGNO, carte=cont,listvar=columbus@data,
qualproj=TRUE,listnomvar=names(columbus@data),name.dataset=names(columbus@data[,6:12]),
cex.lab=0.7, pch=6, col="purple")


# data boston
data(boston)
obs<-pcamap(boston.utm[,1],boston.utm[,2],boston.c[,12:15],label=boston.c$TOWN, 
listvar=boston.c,listnomvar=names(boston.c),name.dataset=names(boston.c[,12:15]),
cex.lab=0.3,center=c(0,0,0,0))

}

\keyword{spatial}
\keyword{multivariate}
\seealso{\code{\link{clustermap}},\code{\link{genpca}}}
