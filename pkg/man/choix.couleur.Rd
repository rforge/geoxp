\name{choix.couleur}
\alias{choix.couleur }
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Selection of colors before plotting map }

\description{
The function \code{choix.couleur()} is used in most of the GeoXp functions to initialize some parameters before plotting map in
the case of the use of a factor. 
}

\usage{
choix.couleur(graphChoice,listvar=NULL,listnomvar=NULL,
varChoice1=NULL,legends,col,pch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{graphChoice}{kind of graphic chosen}
   \item{listvar}{list of variables}
   \item{listnomvar}{names of variables}
   \item{varChoice1}{the name of the chosen variable}
   \item{legends}{parameters of plot}
   \item{col}{color of plot}
   \item{pch}{symbols}
}

\details{This function is not an interactive function.}

\value{A list of parameters which permit to use the function carte.}
\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), \emph{Statistique et Econométrie pour données géoréférencées : modèles et études de cas}}
\author{Laurent T.}

\keyword{spatial}
\keyword{utilities} 
