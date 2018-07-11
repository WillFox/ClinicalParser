\name{multivariate_forest}
\alias{multivariate_forest}
\alias{multivariate_forest.default}
\alias{multivariate_forest.formula}
\alias{print.multivariate_forest}
\alias{summary.multivariate_forest}
\alias{print.summary.multivariate_forest}
\title{Forest Plot for multivariate Data}
\description{Plot a Forest Plot from multivariate cox regression with population values.}
\usage{
multivariate_forst(x, ...)
\method{multivariate_forest}{default}(x, y, ...)
\method{multivariate_forest}{formula}(formula, data = list(), ...)
\method{print}{multivariate_forest}(x, ...)
\method{summary}{univarariate_forest}(object, ...)
\method{predict}{multivariate_forest}(object, newdata=NULL, ...)
}
\arguments{
\item{x}{ a numeric design matrix for the model. }
\item{y}{ a numeric vector of responses. }
\item{formula}{ a symbolic description of the model to be fit. }
\item{data}{ an optional data frame containing the variables in the model. }
\item{object}{ an object of class \code{"linmod"}, i.e., a fitted model. }
\item{\dots}{ not used. }
}
\value{
An object of class \code{logreg}, basically a list including elements
\item{coefficients}{  a named vector of coefficients }
\item{vcov}{ covariance matrix of coefficients }
\item{fitted.values}{ fitted values }
\item{residuals}{ residuals }
}
\author{William Fox}
\examples{
data(cats, package="MASS")
mod1 <- linmod(Hwt~Bwt*Sex, data=cats)
mod1
summary(mod1)
}
\keyword{survival}
\keyword{forest}
\keyword{multivariate}
\keyword{plot}

