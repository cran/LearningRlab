\name{explain.quartile}
\alias{explain.quartile}
\title{Quartiles Calculus Explained}
\description{Step by step demonstration of the quartiles calculus}
\usage{
explain.quartile(x)
}
\arguments{
  \item{x}{Should be a vector}
}
\details{To calculate the quartiles, the user should give a vector. We can saw the quartile formule in the quartile_ help document.}
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
    
    explain.quartile(data)
}
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~quartile }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~explain }% __ONLY ONE__ keyword per line
