\name{explain.variance}

\alias{explain.variance}

\title{Variance Function Explained}

\description{Step by step demonstration of the variance calculus.}

\usage{
explain.variance(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the variance, the user should give a numbers vector. The result is the explained process to calculate the variance, with the data of the dataset provided like argument. We can saw the variance formule in the variance_ help document.}

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
  data <- c(10,4,5,7,3,4,1)
  
  explain.averageDeviation(data)
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~variance }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~explain }% __ONLY ONE__ keyword per line
\keyword{ ~varianza}
\keyword{ ~explicada}
