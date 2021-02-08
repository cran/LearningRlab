\name{explain.covariance}

\alias{explain.covariance}

\title{Covariance Function Explained}

\description{Step by step demonstration of the covariance calculus.}

\usage{
explain.covariance(x,y)
}

\arguments{
  \item{x}{Should be a vector}
  \item{y}{Should be a vector}
}

\details{To calculate the covariance, the user should give two vectors of numbers. The result is the explained process to calculate the covariance, with the data of the datasets provided like argument. We can saw the harmonic mean formule in the covariance_ help document.}

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
  data2 <- c(1,8,3,4,4,5,7)
  
  explain.covariance(data, data2)
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~covaiance }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~covarianza }% __ONLY ONE__ keyword per line
\keyword{ ~explain }
\keyword{ ~explicada}
