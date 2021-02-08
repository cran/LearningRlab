\name{binomial_}

\alias{binomial_}

\title{Binomial Distribution Calculus Function}

\description{This function calculates the binomial distribution of experiment.}

\usage{
binomial_(n,x,p)
}

\arguments{
  \item{x}{Should be a numbers.}
  \item{n}{Should be a numbers.}
  \item{p}{Should be a numbers.}
}

\details{To calculate the binomial distribution, the user should give three number (the number of trials, probability of success and binomial random variable). The result is a discrete probability distribution that counts the number of successes in a sequence of n independent Bernoulli trials with a fixed probability p of occurrence of success between trials. The binomial distribution formule is the following:  \cr{\figure{binomial.jpg}{options: width=3.5in}}}

\value{Numeric, the binomial distribution of three variables.}

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
    binomial_(n,x,p)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~binomial }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution}% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }

