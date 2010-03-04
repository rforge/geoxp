\name{nonormmoran}
\alias{nonormmoran}
\title{Detection of spatial autocorrelation}
\description{
The function `nonnormmoran' is used to detect spatial autocorrelation in
the residuals $u$ from the least squares model \eqn{Y=\beta \times X+u}{Y=beta X+u}.
It calculates Moran's I statistic of the residuals based on the gaussian asymptotic distribution and give a p-value associated to the test of spatial autocorrelation (gaussian version). }
\usage{
nonormmoran(y, x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{vector of size n of dependent variable}
  \item{x}{matrix \eqn{n \times p}{n x p} containing explanatory variables}
  \item{W}{spatial weight matrix}
}
\details{
W is supposed standartized :
\deqn{I=(n/s)\frac{u'Wu}{u'u}}{I=(n/s)frac(u'Wu)(u'u)}
\deqn{I \sim N(E(I),var(I))}{I sim N(E(I),var(I))}
let \eqn{M=(I-X(X'X)^{-1}X')}{M=(I-X(X'X)^(-1)X')}
\deqn{E(I)=(n/s)\frac{tr(MW)}{n-k}}{E(I)=(n/s)frac(tr(MW))(n-k)}
\deqn{d=\frac{n-p}{n+p+2}}{d=frac(n-p)(n+p+2)}
\deqn{V(I)=(n/s)^2[tr(MWMW')+tr(MW)^2+(tr(MW))^2]/d-E(I)^2}{V(I)=(n/s)^2[tr(MWMW')+tr(MW)^2+(tr(MW))^2]/d-E(I)^2}
\deqn{Z_I=\frac{I-E(I)}{v(I)^{1/2}}}{Z_I=frac(I-E(I))(v(I)^(1/2))}

In the case of W is normed, $s=n$ else $s$ is the number of nonzero links}

\value{
(nobs,nvar,morani,imean,istat,ivar,prob) where `nobs' is the number of observations,
`nvar', the number of explanatory variables, `morani' is the Moran's $I$ statistic estimate, 
`imean' is E(I), ivar is var(I), \code{istat} is the normalized Moran's I statistic
(corresponding to \eqn{Z_I}{Z_i}), and \code{prob} the associated p-value.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Translated into R from Jim Lessage's \emph{Spatial Econometrics Toolbox}, \url{http://www.spatial-econometrics.com/} }

\keyword{spatial}

\examples{
# data baltimore
data(baltimore)
W <- makeneighborsw(baltimore$X,baltimore$Y,method="neighbor",4)
# W is not normed
nonormmoran(baltimore$PRICE,baltimore[,14:15],W)
# W is normed
nonormmoran(baltimore$PRICE,baltimore[,14:15],normw(W))
}

\seealso{\code{\link{moranplotmap}},\code{\link{neighbourmap}},\code{\link{makeneighborsw}},\code{\link{normw}} }
