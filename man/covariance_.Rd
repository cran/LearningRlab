\name{covariance_}

\alias{covariance_}

\title{Coariance Calculus Function}

\description{This function calculates the covariance of two vectors of numbers.}

\usage{
covariance_(x,y)
}

\arguments{
  \item{x}{Should be a vector}
  \item{y}{Should be a vector}
}

\details{To calculate the covariance, the user should give two vectors of numbers. The result is a measure of the joint variability of two vectors of numbers. The covariance formule is the following: \cr{\figure{covariance.jpg}{options: width=3.5in}}}

\value{Numeric, the covariance of two vectors of numbers.}

\author{
Dennis Monheimius, \email{dennis.monhemimius@edu.uah.es}
\cr{Eduardo Benito, \email{eduardo.benito@edu.uah.es}}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }

%\seealso{}

\examples{

    #data creation
    data = c(1,4,3,3,2,5,7,12,1,2,3,12)
    data2 = c(1,2,4,4,6,5,11,2,10,5,6,1)
    covariance_(data, data2)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~covariance }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~covarianza }% __ONLY ONE__ keyword per line
