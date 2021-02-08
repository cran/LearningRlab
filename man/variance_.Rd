\name{variance_}

\alias{variance_}

\title{Variance Calculus Function}

\description{This function calculates the variance of a numbers vector.}

\usage{
variance_(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the variance, the user should give a numbers vector. The result is the expectation of the squared deviation of all numbers vector from its mean. The variance formule is the following:  \cr{\figure{variance.jpg}{options: width=2.5in}}}

\value{Numeric, the variance of the numbers vector.}

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
    
    variance_(data)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~variance }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~varianza }% __ONLY ONE__ keyword per line
