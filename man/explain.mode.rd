\name{explain.mode}

\alias{explain.mode}

\title{Mode Function Explained}

\description{Step by step demonstration of the mode calculus.}

\usage{
explain.mode(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the mode, the user should give a numbers vector. The result is the explained process to calculate the mode, with the data of the dataset provided like argument. We can saw the mode formule in the mode_ help document.}

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
  {
    #data creation
    data <- c(1,1,2,5,2,3,1,4,1)
    
    explain.mode(data)
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~mode }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~explain }% __ONLY ONE__ keyword per line
\keyword{ ~moda.explicada}
\keyword{ ~explicada}
