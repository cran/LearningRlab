\name{frecuency_abs}
\alias{frecuency_abs}
\title{Absolute Frecuency Calculus}
\description{This function calculate the number of times that a specific number appears in the data set.}
\usage{
frecuency_abs(v,x)
}
\arguments{
  \item{v}{Should be a vector}
  \item{x}{Should be a number}
}
\details{The absolute frecuency formula is the following: \cr{\figure{absolutefrec.jpg}{options: width=3.5in}}}
\value{An integer that represents the number of times that the value appears in the vector}

\author{
Dennis Monheimius, \email{dennis.monhemimius@edu.uah.es}
\cr{Eduardo Benito, \email{eduardo.benito@edu.uah.es}}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }


%\seealso{}
\examples{
  {
    #data creation
    data = c(1,4,3,3,2,5,7,12,1,2,3,12)
    value = 12
    #function execution
    frecuency_abs(data, value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~frecuency }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~frecuencia }% __ONLY ONE__ keyword per line
\keyword{ ~absoluta}
\keyword{ ~absolute}