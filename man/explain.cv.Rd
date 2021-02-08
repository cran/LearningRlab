\name{explain.cv}

\alias{explain.cv}

\title{Coefficient of Variation Function Explained}

\description{Step by step demonstration of the coefficient of variation  calculus.}

\usage{
explain.cv(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the coefficient of variation, the user should give a numbers vector. The result is defined as the ratio of the standard deviation to the mean. We can saw the coefficient of variation formule in the cv_ help document.}

\value{Numeric result and the process of this calculus explained.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }

%\seealso{}

\examples{
  
  #data creation
  data <- c(10,4,5,7,3,4,1)
  
  explain.cv(data)
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~coefficient}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~variation }% __ONLY ONE__ keyword per line
\keyword{ ~coeficiente }
\keyword{ ~variacion }
\keyword{ ~cv }
\keyword{ ~explain }
\keyword{ ~explicada}
