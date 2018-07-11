\name{univariate_forest}
\alias{univariate_forest}
\alias{univariate_forest.default}
\alias{univariate_forest.formula}
\alias{print.univariate_forest}
\alias{summary.univariate_forest}
\alias{print.summary.univariate_forest}
\title{Forest Plot for Univariate Data}
\description{Plot a Forest Plot from univariate cox regression with population values.}
\usage{
univariate_forst(x, ...)
\method{univariate_forest}{default}(x, y, ...)
\method{univariate_forest}{formula}(formula, data = list(), ...)
\method{print}{univariate_forest}(x, ...)
\method{summary}{univarariate_forest}(object, ...)
\method{predict}{univariate_forest}(object, newdata=NULL, ...)
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
\keyword{univariate}
\keyword{plot}

