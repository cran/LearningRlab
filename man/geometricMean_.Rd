\name{geometricMean_}

\alias{geometricMean_}

\title{Geometric Mean Function}

\description{This function calculates the geometric mean of a numbers vector.}

\usage{
geometricMean_(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the geometric mean of a dataset, the user should give a numbers vector. The result is the product of all vector elements raise to 1 divided by the number of elements. The arithmetic mean formule is the following: \cr{\figure{gmean.jpg}{options: width=3.5in}}}

\value{A numeric, the geometric mean of the numbers vector.}

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
  data = c(1:20)
  
  geometricMean_(data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~geometric }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mean }% __ONLY ONE__ keyword per line
\keyword{ ~geometrica}
\keyword{ ~media}
