\name{explain.standardDeviation}

\alias{explain.standardDeviation}


\title{Standard Deviation Function Explained} 

\description{Step by step demonstration of the standard deviation calculus.}

\usage{
explain.standardDeviation(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the standard deviation, the user should give a numbers vector. The result is the explained process to calculate the standard deviation, with the data of the dataset provided like argument. We can saw the standard deviation formule in the standardDeviation_ help document.}

\value{Numeric result and the process of this calculus explained.}

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
    data <- c(1,5,3,7,10,4,2)
    
    explain.standardDeviation(data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~standard }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~deviation }% __ONLY ONE__ keyword per line
\keyword{ ~explained}
\keyword{ ~explicada}
\keyword{ ~desviacion}
\keyword{ ~tipica}
