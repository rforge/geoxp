\name{variocloudmap}
\alias{variocloudmap}
\title{Interactive variocloud and map}

\description{
The function \code{variocloudmap()} draws a semi-variocloud (directional or omnidirectional) and a map.
It is used to detect spatial autocorrelation. Possibility to draw the empirical semi-variogram
and a robust empirical semi-variogram.}

\usage{
variocloudmap(long, lat, var, bin=NULL, quantiles=NULL, listvar=NULL, listnomvar=NULL,
criteria=NULL, carte = NULL, label = "", cex.lab=1, pch = 16, col="grey", xlab = "",
ylab="", axes=FALSE, lablong = "", lablat = "", xlim=NULL, ylim=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector x of size n}
  \item{lat}{a vector y of size n}
  \item{var}{a vector of numeric values of size n}
  \item{bin}{list of values where empirical variogram is evaluated}
  \item{quantiles}{list of values of quantile orders (the regression quantile is obtained by spline smoothing)}
  \item{listvar}{matrix of variables}
  \item{listnomvar}{names of variables \code{listvar}}
  \item{criteria}{a vector of size n of boolean which permit to represent preselected sites with a cross, using the tcltk window}  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y
  coordinates of the vertices of the polygon}
  \item{label}{a list of character of size n with names of sites}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"grey" by default, colors of points on the angle plot}  
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
  \item{xlim}{the x limits of the plot}
  \item{ylim}{the y limits of the plot}
}
\details{
For some couple of sites \eqn{(s_i,s_j)}{(s_i,s_j)}, the graph represents on the y-axis the semi squared difference
between \eqn{var_i}{var_i} and \eqn{var_j}{var_j} :
\deqn{\gamma_{ij}=\frac{1}{2}(var_i-var_j)^2}{gamma_ij=0.5(var_i-var_j)^2}
and on the x-absis the distance \eqn{h_{ij}}{h_(ij)} between \eqn{s_i}{s_i} and \eqn{s_j}{s_j}.
The semi Empirical variogram has been calculated as :
\deqn{\gamma(h)=\frac{1}{2|N(h)|}\sum_{N(h)}(Z(s_i)-Z(s_j))^2}{gamma(h)=0.5/|N(h)|sum_(N(h))(Z(s_i)-Z(s_j))^2}
where
\deqn{N(h)=\{(s_i,s_j):s_i-s_j=h;i,j=1,...,n\}}{N(h)=\{(s_i,s_j):s_i-s_j=h;i,j=1,...,n\}}
and the robust version :
\deqn{\gamma(h)=\frac{1}{2(0.457+\frac{0.494}{|N(h)|})}(\frac{1}{|N(h)|}\sum_{N(h)}|Z(s_i)-Z(s_j)|^{1/2})^4}{gamma(h)=frac(1)(2(0.457+frac(0.494)(|N(h)|)))(frac(1)(|N(h)|)sum_(N(h))|Z(s_i)-Z(s_j)|^(1/2))^4}
The number N of points to evaluate the empirical variogram and the distance \eqn{\epsilon}{epsilon} between
points are set as follows :
\deqn{N=\frac{1}{max(30/n^2,0.08,d/D)}}{N=frac(1)(max(30/n^2,0.08,d/D))}
and :
\deqn{\epsilon=\frac{D}{N}}{epsilon=frac(D)(N)}
with : \deqn{D=max(h_{ij})-min(h_{ij})}{D=max(h_ij)-min(h_ij)}
and : \deqn{d=max(h_{ij}^{(l)}-h_{ij}^{(l+1)}),}{d=max(h_ij^(l)-h_ij^(l+1)),}
where \eqn{h^{(l)}}{h^(l)} is the vector of sorted distances.
In options, possibility to represent a regression quantile smoothing spline \eqn{g_{\alpha}}{g_alpha} (in that
case the points below this quantile curve are not drawn). }

\value{
A matrix of boolean of size \eqn{n \times n}{n x n}. TRUE if the couple of site was in the last selection of points.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} \cr \cr

Cressie N. and Hawkins D. (1980), \emph{Robust estimation of the variogram}, in Journal of the international association for mathematical geology, 13, 115-125.}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\examples{
# data meuse
data(meuse)
data(meuse.riv)
obs<-variocloudmap(meuse$x,meuse$y,meuse$zinc,
quantiles=0.75,listvar=meuse,listnomvar=names(meuse),
xlim=c(0,2000),ylim=c(0,500000),pch=2,carte=meuse.riv[c(21:65,110:153),])
#bin=c(0,50,100,250,500,750,1000,1250,1500,1750,2000) )
#points(meuse.riv, type = "l", asp = 1)
}


\seealso{\code{\link{angleplotmap}}, \code{\link{driftmap}}}