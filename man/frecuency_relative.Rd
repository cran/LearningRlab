\name{frecuency_relative}
\alias{frecuency_relative}
\title{Relative Frecuency Calculus}
\description{This function calculate the number of times that a specific number appears in the data set divided by the total length of the vector.}
\usage{
frecuency_relative(v,x)
}
\arguments{
  \item{v}{Should be a vector}
  \item{x}{Should be a number}
}
\details{The relative frecuency formula is the following: \cr{\figure{relativefrec.jpg}}}
\value{A double that represents the number of times that the value appears in the vector regarding the total of elements}

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
    data <- c(1,4,3,3,2,5,7,12,1,2,3,12)
    value = 12
    frecuency_relative(data, value)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~frecuency }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~frecuencia }% __ONLY ONE__ keyword per line
\keyword{ ~relativa}
\keyword{ ~relative}