\name{frecuency_absolute_acum}
\alias{frecuency_absolute_acum}
\title{Accumulated Absolute Frecuency Calculus}
\description{This function calculate the number of times that a specific number appears in the data set. The value depends on the elements that are lower than itself}
\usage{
frecuency_absolute_acum(v,x)
}
\arguments{
  \item{v}{Should be a vector}
  \item{x}{Should be a number}
}
\details{The accumulated absolute frecuency formula is the following: \cr{\if{latex}{\figure{absoluteacumfrec.jpg}{options: width=3.5in}} \if{html}{\figure{absoluteacumfrec.jpg}{options: width=336}}}}
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
    data = c(1,4,3,3,2,5,7,12,1,2,3,12)
    value = 12
    #function execution
    frecuency_absolute_acum(data, value)
    
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~frecuency }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~frecuencia }% __ONLY ONE__ keyword per line
\keyword{ ~absoluta}
\keyword{ ~absolute}
\keyword{ ~acumulada}
\keyword{ ~accumulated}