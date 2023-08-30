\name{explain.relative_frecuency}
\alias{explain.relative_frecuency}
\title{Relative Frecuency Calculus Explained}
\description{Step by step demonstration of the relative frecuency calculus}
\usage{
explain.relative_frecuency(v,x)
}
\arguments{
  \item{v}{Should be a vector}
  \item{x}{Should be a number}
}
\details{To calculate the relative frecuency, the user should give a vector and a number. We can saw the relative frecuency formule in the frecuency_relative help document.}
\value{A demonstration of the calculus process}

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
    data <- c(1,2,2,5,10,4,2)
    value = 2
    #function execution
    explain.relative_frecuency(data, value)
}
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~relative }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~explain }% __ONLY ONE__ keyword per line
\keyword{ ~frecuency }