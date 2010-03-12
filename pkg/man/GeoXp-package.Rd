\name{GeoXp-package}
\alias{GeoXp-package}
\alias{GeoXp}
\docType{package}
\title{
Interactive exploratory spatial data analysis
}
\description{
GeoXp is a tool for researchers in spatial statistics, spatial econometrics, 
geography, ecology etc allowing to link dynamically statistical plots with 
elementary maps. This coupling consists in the fact that the selection of a 
zone on the map results in the automatic highlighting of the corresponding points 
on the statistical graph or reversely, the selection of a portion of the graph 
results in the automatic highlighting of the corresponding points on the map. 
GeoXp includes tools from different areas of spatial statistics 
including geostatistics as well as spatial econometrics and point processes. 
Besides elementary plots like boxplots, histograms or simple scatterplos, GeoXp 
also couples with maps Moran scatterplots, variogram cloud, Lorentz Curves,...
In order to make the most of the multidimensionality of the data, GeoXp includes 
some dimension reduction techniques such as PCA.
}
\details{
\tabular{ll}{
Package: \tab GeoXp\cr
Type: \tab Package\cr
Version: \tab 1.4.3\cr
Date: \tab 2010-03-12\cr
License: \tab GPL Vesion 2 or later \cr
}

The coordinates of sites are represented by 
two vectors \code{long} (for x-axis) and \code{lat} (for y-axis) of size n of
numeric values. Attribute value is designed by argument \code{var} of size n and
can be a vector of numeric values (function \code{histomap()}, \code{densitymap()},...) or
a factor (function \code{barmap()},...). Some functions use both a vector of numeric
values and a factor (function \code{histobarmap()}, \code{polyboxplotmap()},...). Few functions as
\code{pcamap()} and \code{clustermap()} use a matrix of numeric variables. \cr
Contrary to \code{spdep} package (Roger Bivand), a site cannot be yet represented by
lattice/area style. However, user can represent a spatial polygonal contour
as background map with option \code{carte}. A spatial polygonal contour is a matrix
of numeric values with 2 columns (x and y coordinates of the vertices of the polygons)
where polygons are seperated from each other 
by 3 rows of NaN. We include in GeoXp 2 functions (\code{polylist2list()} and \code{spdf2list()})
which convert some spatial objects (\code{Polylist} and \code{SpatialPolygonDataFrame})
into matrix as decribed above to draw a background map. \cr
Among options which are common to each function, users have the 
possibility to give a \code{criteria}, vector of boolean of size n with TRUE on
specific sites. These sites are then represented by a croice on the  map 
by clicking on "non intercative selection" button. \cr
Moreover, users have the possibility to make bubbles and add some graphs 
(histogram, barplot or scattermap) giving both a matrix of variables 
\code{listvar} of size \eqn{n \times p}{n x p} and a list of character of size p
for variable names. Users can choose a proportional
symbol mapping : in function plot, we give value
\eqn{var^{0.5}}{sqrt(var)} to argument \code{cex}. User can choose if a legend has to appear on
the map. He could choose then three values represented by bubbles of 
corresponding sizes. \cr
Finally, users can choose to represent the graphical with different colors
using argument \code{color}. In the case of factors (as function \code{barmap}), users could choose
if a legend with corresponding colors will appear on the map. Users can also modify the representation of
selected sites on map with argument \code{pch}.    \cr
GeoXp don't use classes yet excepted \code{bar.nb()} and \code{hist.nb()} functions which call a \code{nb} object
(see package spdep). These two functions make an analysis of a neighborhood matrix. 

 
}

\author{
Christine Thomas-Agnan, Yves Aragon, Anne Ruiz-Gazen, Thibault Laurent, Laurianne Robidou
Maintainer: Thibault Laurent <thibault.laurent@univ-tlse1.fr>
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2009), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}

\keyword{spatial}

