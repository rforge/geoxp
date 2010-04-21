\name{spdf2list}
\alias{spdf2list}

\title{Extract from a SpatialPolygonsDataFrame object the middle coordinates of boundary
box and the vertices of the polygons (in the case where polygons are given)}

\description{
The function \code{spdf2list()} calculates
the middle coordinates of boundary box and in the case where polygons are included into a
\code{SpatialPolygonsDataFrame} object, it extracts the vertices of the polygons,
seperating polygons from each other by 3 rows of NaN.
}

\usage{
spdf2list(data)
}

\arguments{
  \item{data}{A \code{SpatialPolygonsDataFrame} object}
}
\details{
The user can then represent the coordinates of sites of a \code{SpatialPolygonsDataFrame} object
as background map, using the option `carte' included in all interactive functions.
}

\note{
The data of a \code{SpatialPolygonsDataFrame} object can be directly extract using @data}


\value{
It returns two vectors of middle coordinates for x-axis and y-axis cooresponding to middle
of each boundary box.  It returns a matrix of numeric values with 2 columns
(x and y coordinates of the vertices of the polygons) where polygons are seperated from
each other by 3 rows of NaN.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2010), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}\cr \cr

Roger S.Bivand, Edzer J.Pebesma, Virgilio Gomez-Rubio (2009),  \emph{Applied Spatial Data Analysis with R}, Springer.
}

\author{Thibault Laurent}

\seealso{\code{\link{polylist2list}}}

\examples{
africa <- readShapePoly(system.file("shapes/Africa.shp", package = "GeoXp")[1])
africa.contour<-spdf2list(africa)$poly
}

\keyword{manip}
