\name{drawVector}

\alias{drawVector}

\title{Draw Vector Function}

\description{This function prints all the elements of a vector}

\usage{
drawVector(buffer)
}

\arguments{
  \item{buffer}{A vector of elements}
}

%\details{}

\value{There isn't return value, prints on screen}

%\references{}

\author{
Dennis Monheimius, \email{dennis.monhemimius@edu.uah.es}
\cr{Eduardo Benito, \email{eduardo.benito@edu.uah.es}}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5
      or c(true,false,false) creates a vector with the booleans: true, false, true}

%\seealso{}

\examples{
{
  #data creation
  data <- c(1:12)
  drawVector(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~draw }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~vector }% __ONLY ONE__ keyword per line
\keyword{ ~pintar}
