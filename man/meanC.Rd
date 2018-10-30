\name{meanC}

\alias{meanC}

\title{Mean Function Developed in C}

\description{This function calculates the arithmetic mean of a numbers vector.}

\usage{
meanC(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the arithmetic mean of a dataset, the user should give a numbers vector. The result is the addition of all vector elements divided by the number of elements. The arithmetic mean formule is the following: \cr{\figure{amean.jpg}}
}

\value{A numeric, the arithmetic mean of the numbers vector.}

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
  vector = c(1:10)
  meanC(vector)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~meanC }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mediaC }% __ONLY ONE__ keyword per line
\keyword{ ~media.aritmetica}
\keyword{ ~arithmetic.mean}
