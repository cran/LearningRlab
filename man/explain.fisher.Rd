\name{explain.fisher}

\alias{explain.fisher}

\title{F Fisher Distribution Function Explained}

\description{Step by step demonstration of the fisher distribution calculus.}

\usage{
explain.fisher(x,y)
}

\arguments{
  \item{x}{Should be a vector}
  \item{y}{Should be a vector}
}

\details{To calculate the fisher distribution, the user should give two vectors of numbers. The result is a continuous probability distribution that arises frequently as the null distribution of a test statistic. We can saw fisher distribution formule in the fisher_ help document.}

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
  
  explain.fisher(data, data2)
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~fisher }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution }% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }
\keyword{ ~explain }
\keyword{ ~explicada}
