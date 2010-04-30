\name{mp.school}
\alias{mp.school}
\docType{data}
\title{Midi-pyrennees school}
\description{
This data frame contains some information about schools in Midi-Pyrenees region.
The school is the spatial unit level.
}
\usage{data(immob)}
\format{
  A data frame with 226 observations on the following 8 variables.
  \describe{
    \item{\code{longitude}}{x-coordinate of the pseudo-canton}
    \item{\code{latitude}}{y-coordinate of the pseudo-canton}
    \item{\code{name.city}}{Name of the city}
    \item{\code{index.rurality}}{A factor with levels of rurality}
    \item{\code{Nb.students}}{Number of students}
    \item{\code{Occupancy.rate}}{Rate of occupancy}
    \item{\code{Cost.per.student}}{Average cast per student}
    \item{\code{Nb.students.per.class}}{Average number of students per class}
      }
}

\source{
Prepared by T. Laurent.
}

\note{
The variables \code{Occupancy.rate}, \code{Cost.per.student} and  \code{Nb.students.per.class}
have been permuted because of the confidentiality of this data set.
}

\examples{
data(mp.school)
}
\keyword{datasets}
