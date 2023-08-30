\name{explain.geometricMean}

\alias{explain.geometricMean}

\title{Geometric Mean Function Explained}

\description{Step by step demonstration of the geometric mean calculus.}

\usage{
explain.geometricMean(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the geometric mean of a dataset, the user should give a vector. The result is the explained process to calculate the geometric mean, with the data of the dataset provided like argument. We can saw the geometric mean formule in the geometricMean_ help document. }

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
    data <- c(5,21,12,7,3,9,1)
    
    explain.geometricMean(data)
}
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~explain }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~geometric }% __ONLY ONE__ keyword per line
\keyword{ ~mean}
\keyword{ ~explicada}
\keyword{ ~geometrica}
\keyword{ ~media}
