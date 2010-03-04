\name{distchi2}
\alias{distchi2}
\title{It gives an euclidian distance matrix for factors using metric of chi2.}

\description{
The function \code{distchi2} calculates an euclidian distance matrix for factors using metric of \eqn{\chi^2}{chi^2}.
}
\usage{
distchi2(mat)
}

\arguments{
  \item{mat}{A matrix of factors given as numeric values}
}
\details{
The euclidian distance between two individuals $i$ and $k$ using the metric of \eqn{\chi^2}{chi^2} is given by :

\deqn{d_{\chi^2}^2=\frac{n}{p}\sum_{j}p\sum_l^{m_j}\delta_{ik}^{jl}\frac{1}{n_l^j}}{d^2_(chi^2)=n/p sum_j^p sum_l^m_j delta_ik^(jl) 1/n_l^j}

with n, number of individuals, p number of variables, \eqn{m_j}{m_j} is the number of possibilities for variable \eqn{Y^j}{Y^j}, \eqn{n_l^j}{n_l^j} is the number of individuals
where $l$ has been observed for \eqn{Y^j}{Y^j} and \eqn{\delta_{ik}^{jl}=1}{delta_ik^(jl)=1} if there is a discordance between $i$ and $k$ for $l$ and 0 otherwise. It is not an interactive function.
}


\value{
It returns a \code{dist} object (see help(dist)).
}

\references{Besse Philippe, \emph{Data mining 2 : Modélisation
statistique et apprentissage}.}

\author{Thibault LAURENT}

\seealso{\code{\link{clustermap}}}

\keyword{manip}

