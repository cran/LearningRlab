\name{explain.averageDeviation}

\alias{explain.averageDeviation}

\title{Average Absolute Deviation Function Explained}

\description{Step by step demonstration of the average absolute deviation calculus.}

\usage{ 
explain.averageDeviation(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the average absolute deviation, the user should give a numbers vector. The result is the explained process to calculate the average absolute deviation, with the data of the dataset provided like argument. We can saw the average absolute deviation formule in the averageDeviation_ help document.}

\value{Numeric, the average absolute deviation of the numbers vector.}

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
  data <- c(7,2,5,7,1,4,12)
  
  explain.averageDeviation(data)
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~average}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~absolute}% __ONLY ONE__ keyword per line
\keyword{ ~deviation}
\keyword{ ~explained}
\keyword{ ~explicada}
\keyword{ ~absoluta}
\keyword{ ~desviacion}
