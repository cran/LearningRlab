\name{explain.binomial}

\alias{explain.binomial}

\title{Binomial Distribution Function Explained}

\description{Step by step demonstration of the binomial distribution calculus.}

\usage{
explain.binomial(n,x,p)
}

\arguments{
  \item{x}{Should be a numbers.}
  \item{n}{Should be a numbers.}
  \item{p}{Should be a numbers.}
}

\details{To calculate the binomial distribution, the user should give three number (the number of trials, probability of success and binomial random variable). The result is a discrete probability distribution that counts the number of successes in a sequence of n independent Bernoulli trials with a fixed probability p of occurrence of success between trials. We can saw the binomial distribution formule in the binomial_ help document.}

\value{Numeric result and the process of this calculus explained.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{Each variable is a number. Example: n <- 3 | x <- 2 | p <- 0.7 }

%\seealso{}

\examples{
  
  #data creation
  n = 3
  x = 2
  p = 0.7
    
  explain.binomial(n,x,p)
  
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~binomial }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution}% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }
\keyword{ ~explain }
\keyword{ ~explicada}
