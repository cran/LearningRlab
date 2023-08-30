\name{explain.mean}

\alias{explain.mean}

\title{Mean Function Explained}

\description{Step by step demonstration of the arithmetic mean calculus.}

\usage{
explain.mean(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the arithmetic mean of a dataset, the user should give a vector. The result is the explained process to calculate the arithmetic mean, with the data of the dataset provided like argument. We can saw the arithmetic mean formule in the mean_ help document. }

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
    data <- c(1,2,2,5,10,4,2)
    
    explain.mean(data)
}
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~mean }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~explain }% __ONLY ONE__ keyword per line
\keyword{ ~media.explicada}
\keyword{ ~explicada}
