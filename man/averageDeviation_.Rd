\name{averageDeviation_}

\alias{averageDeviation_}

\title{Average Absolute Deviation Function}

\description{This function calculates the average absolute deviation of a numbers vector.}

\usage{
averageDeviation_(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the average deviation, the user should give a numbers vector. The result is the sum of the differences in absolute value between each vector element and the mean, divided by the number of elemets. The average absolute deviation formule is the following:  \cr{\figure{averageDeviation.jpg}{options: width=3.5in}}}

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
data <- c(1:20)
result = averageDeviation_(data)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~average }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~deviation }% __ONLY ONE__ keyword per line
\keyword{ ~absolute}
\keyword{ ~desviacion}
\keyword{ ~absoluta}
