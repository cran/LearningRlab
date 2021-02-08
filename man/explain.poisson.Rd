\name{explain.poisson}

\alias{explain.poisson}

\title{Poisson Distribution Function Explained}

\description{Step by step demonstration of the Poisson distribution calculus.}

\usage{
explain.poisson(k,lam)
}

\arguments{
  \item{k}{Should be a numbers}
  \item{lam}{Should be a numbers}
}

\details{To calculate the Poisson distribution, the user should give two number ( the number of times the phenomenon and the number of occurrences). The result is a discrete probability distribution that expresses, from a mean frequency of occurrence, the probability that a certain number of events will occur during a certain period of time. We can saw the Poisson distribution correlation formule in the poisson_ help document.}

\value{Numeric result and the process of this calculus explained.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{Each variable is a number. Example: lam <- 2 | k <- 3 }

%\seealso{}

\examples{

    #data creation
    lam = 2
    k = 3
    explain.poisson(k,lam)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~poisson }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution }% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }
\keyword{ ~explain }
\keyword{ ~explicada}
