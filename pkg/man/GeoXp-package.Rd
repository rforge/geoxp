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
Version: \tab 1.5.0 \cr
Date: \tab 2010-04-10\cr
License: \tab GPL Vesion 2 or later \cr
}

In the version 1.5.0, GeoXp uses the SpatialClass object proposed by Roger Bivand
in \code{sp} package. The main advantage of using this structure object is on one hand,
a SpatialClass object can contain both spatial coordinates and a data.frame of observed variable
and on an other hand, it offers the possibility to make spatial analysis using both
packages \code{sp} and \code{GeoXp}. \cr
On the map, the coordinates of sites are represented by
using the function \code{coordinates} included in \code{sp} pachage, which calculates
\code{long} (for x-axis) and \code{lat} (for y-axis), applied on a Spatial Class Object.
Attribute value is designed by argument \code{name.var}, a character, which design
the name of interest variable in the data of the Spatial Class object. The basic function
using only one variable: \code{histomap()}, \code{densitymap()},... (numeric values) or
\code{barmap()},...(factor values). Some functions use both a vector of numeric
values and a factor (function \code{histobarmap()}, \code{polyboxplotmap()},...). Few functions as
\code{pcamap()} and \code{clustermap()} use a matrix of numeric variables. \cr
At this moment, a site cannot be yet represented by
lattice/area style (only by centroïd references coordinates). However, user can represent a
spatial polygonal contour as background map with option \code{carte}.
A spatial polygonal contour is a matrix of numeric values with 2 columns (x and y coordinates of the vertices of the polygons)
where polygons are seperated from each other 
by 3 rows of NaN. In the case of a SpatialPolygonDataFrame, the contours are automatically included in the
functions. \cr
The functions (\code{polylist2list()} and \code{spdf2list()})
convert some spatial objects (\code{Polylist} and \code{SpatialPolygonDataFrame})
into matrix as decribed above to draw a background map. \cr
Among options which are common to each function, users have the 
possibility to give a \code{criteria}, vector of boolean of size n with TRUE on
specific sites. These sites are then represented by a green croice on the map
by clicking on "preselected sites" button. \cr
Moreover, users have the possibility to make bubbles and add some graphs 
(histogram, barplot or scattermap). The potential variables are included
in the data.frame of the SpatialObject. Users can choose a proportional
symbol mapping : in function plot, we give value
\eqn{var^{0.5}}{sqrt(var)}. User can choose if a legend has to appear on
the map. He could choose then three values represented by bubbles of 
corresponding sizes. \cr
Finally, users can choose to represent the graphical with different colors
using argument \code{col}. In the case of factors (as function \code{barmap}), users could choose
if a legend with corresponding colors will appear on the map. Users can also modify the representation of
selected sites on map with argument \code{pch}.    \cr
New functions \code{bar.nb()} and \code{hist.nb()} give the opportunity to analyse spatial
weight matrix build using functions included in \code{spdep} package.
}

\author{
Christine Thomas-Agnan, Yves Aragon, Anne Ruiz-Gazen, Thibault Laurent, Laurianne Robidou
Maintainer: Thibault Laurent <thibault.laurent@univ-tlse1.fr>
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2010), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas} \cr \cr

Roger S.Bivand, Edzer J.Pebesma, Virgilio Gomez-Rubio (2009),  \emph{Applied Spatial Data Analysis with R}, Springer.
}

\keyword{spatial}

