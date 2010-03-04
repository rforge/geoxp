\name{makeneighborsw}
\alias{makeneighborsw}
\title{Spatial weight matrix}
\description{
The function \code{makeneighborsw()} create a spatial weight matrix based on a given number of nearest neighbors
(option "neighbor" by default), based on a threshold distance (option method="distance") or both these 2 methods.}
\usage{
makeneighborsw(xc,yc,method="neighbor",m=1,d,cum=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{a vector x of size n}
  \item{yc}{a vector y of size n}
  \item{method}{"neighbor" by default, "distance" or "both"}  
  \item{m}{ number of nearest neighbors}
  \item{d}{threshold point}
  \item{cum}{if cum=TRUE, W is the sum of spatial weight matrix based on k nearest neighbours (for \eqn{k \leq m}{k < m};
   if FALSE W is the spatial weight matrix based only on \eqn{m^{th}}{m^th} nearest neighbours}
}
\details{
In the case of method="neighbor", for each site, we order the other sites by their distance from this
site. If cum=TRUE, for i, if j is among the \eqn{m^{th}}{m^th}
nearest sites, then :     
\deqn{W_{ij}=1}{W_ij=1}
else
\deqn{W_{ij}=0}{W_ij=0}
If cum=FALSE, for \deqn{s_i}{s_i}, if \deqn{s_j}{s_j} is the \eqn{m^{th}}{m^th}
nearest site, then :     
\deqn{W_{ij}=1}{W_ij=1}
else
\deqn{W_{ij}=0}{W_ij=0}
In case of ties, the nearest neighbour is randomly chosen. \cr \cr
In the case of method="distance", if site i is seperated from j by a distance lower
or equal to a given threshold : 
\deqn{W_{ij}=1}{W_ij=1}
else
\deqn{W_{ij}=0}{W_ij=0}
\cr 
In the case of method="both" W must verify the two first conditions.}

\value{
A spatial weight matrix of size \eqn{n \times n}{n x n}}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} }

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\seealso{\code{\link{moranplotmap}},\code{\link{normw}}}

\examples{
# data auckland
data(auckland)
x.ext <- auckland$Easting[1:10]
y.ext <- auckland$Northing[1:10] 
#plot(x.ext,y.ext,type='p')
#text(x.ext,y.ext,as.character(1:10))

# matrix based on 5 nearest neighbors
W<-makeneighborsw(x.ext,y.ext,method="neighbor",m=3)

# matrix based on a threshold distance
W1<-makeneighborsw(x.ext,y.ext,method="distance",d=20)

# matrix based on the two methods
W2<-makeneighborsw(x.ext,y.ext,method="both",m=3,d=20)
} 