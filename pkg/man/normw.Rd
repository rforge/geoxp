\name{normw}
\alias{normw}

\title{Normalize a spatial weight matrix}
\description{
The function \code{normw()} normalizes a spatial weight matrix}

\usage{
normw(w)
}

\arguments{
  \item{w}{A spatial weight matrix of size \eqn{n \times n}{n x n}}
}
\details{
 \deqn{W_{ij}=\frac{W_{ij}}{\sum_kW_{ik}}}{W_ij=frac(W_(ij))(sum_kW_(ik))}
}

\value{
A matrix of size \eqn{n \times n}{n x n}
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} }

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Robidou L.}

\keyword{spatial}

\seealso{\code{\link{neighbourmap}}, \code{\link{moranplotmap}},\code{\link{makeneighborsw}}}

\examples{
# data auckland
data(auckland)
x.ext <- auckland$Easting[1:10]
y.ext <- auckland$Northing[1:10] 
#plot(x.ext,y.ext,type='p')
#text(x.ext,y.ext,as.character(1:10))

# matrix based on 5 nearest neighbors
W<-makeneighborsw(x.ext,y.ext,method="neighbor",m=3)
W2<-normw(W)
} 