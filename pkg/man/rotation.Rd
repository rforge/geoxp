\name{rotation}
\alias{rotation}
\title{Transform coordinates of sites using a rotation}

\description{
The function \code{rotation()} is used to modify coordinates of sites by a rotation
with an angle equal to \code{angle}.}

\usage{
rotation(coords, angle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coords}{matrix \eqn{n\times 2}{nx2} of coordinates }
  \item{angle}{value of angle to use in rotation in degree }
}
\details{
Let:
\deqn{x=(cos(\theta),-sin(\theta))}{x=(cos(theta),-sin(theta))}
\deqn{y=(sin(\theta),cos(\theta))}{y=(sin(theta),cos(theta))}
\deqn{nlecoord=coords \times cbind(x,y)}{nlecoord=coords x cbind(x,y)}
}

\value{
matrix \eqn{n\times 2}{nx2} of new coordinates.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\keyword{utilities}
\seealso{\code{\link{angleplotmap}},\code{\link{variocloudmap}}}


