\name{slider2}
\alias{slider2}
\title{Scroll bar}

\description{
Creates a scroll bar for modifying the value of a parameter (same as slider1.r).
}
\usage{
slider2(fenetre, refresh.code, names, minima, maxima,
resolutions, starts, no = 0)
}

\arguments{
  \item{fenetre}{number of windows}
  \item{refresh.code}{name of function called after modifying the value of a parameter}
  \item{names}{title for scroll bar}
  \item{minima}{minimum value of parameter}
  \item{maxima}{maximum value of parameter}
  \item{resolutions}{scale}
  \item{starts}{Initial Value}
  \item{no}{number of scroll bar}
}
\details{
This function is used in the functions which draw a curve to modify the smoothing parameter.
This is not an interactive function.}

\value{Drawing of a tcltk window.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\seealso{\code{\link{densitymap}},\code{\link{dbledensitymap}}} 