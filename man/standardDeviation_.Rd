\name{standardDeviation_}

\alias{standardDeviation_}

\title{Standard Deviation Calculus Function}

\description{This function calculates the standard deviation of a numbers vector.}

\usage{
standardDeviation_(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the standard deviation, the user should give a numbers vector. The result is the square root of the sum of the differences between each vector element and the mean squared divided by the number of elemets. The standard deviation formule is the following:
}

\value{Numeric, the standard deviation of the numbers vector.}

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
    
    standardDeviation_(data)
  
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~standart }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~deviation }% __ONLY ONE__ keyword per line
\keyword{ ~tipica}
\keyword{ ~desviacion}
