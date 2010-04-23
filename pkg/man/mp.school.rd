\name{mp.school}
\alias{mp.school}
\docType{data}
\title{Midi-pyrennees school}
\description{
This data frame contains some information about schools in Midi-Pyrenees region.
Data have been aggregated by pseudo-cantons which is the spatial unit level.
}
\usage{data(immob)}
\format{
  A data frame with 155 observations on the following 8 variables.
  \describe{
    \item{\code{PSE_CANT}}{Code of the pseudo-canton}
    \item{\code{COMMUNE}}{Name of the main city}
    \item{\code{code_dep}}{Code of the 'departement' area of the pseudo-canton}
    \item{\code{longitude}}{x-coordinate of the pseudo-canton}
    \item{\code{latitude}}{y-coordinate of the pseudo-canton}
    \item{\code{Nb_Student_per_class}}{Average number of students per class}
    \item{\code{Cast_per_student}}{Average cast per student}
    \item{\code{ruralite}}{Criteria index of rurality }
  }
}

\source{
Prepared by T. Laurent.
}

\note{
The variables  \code{Nb_Student_per_class} and   \code{Nb_Student_per_class}
have been permuted because of the confidentiality of this data set.
}

\examples{
data(mp.school)
}
\keyword{datasets}
