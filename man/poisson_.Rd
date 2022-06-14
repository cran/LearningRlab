\name{poisson_}

\alias{poisson_}

\title{Poisson Distribution Calculus Function}

\description{This function calculates the Poisson distribution of experiment.}

\usage{
poisson_(k,lam)
}

\arguments{
  \item{k}{Should be a numbers}
  \item{lam}{Should be a numbers}
}

\details{To calculate the Poisson distribution, the user should give two number ( the number of times the phenomenon and the number of occurrences). The result is a discrete probability distribution that expresses, from a mean frequency of occurrence, the probability that a certain number of events will occur during a certain period of time. The Poisson distribution formule is the following:  \cr{\if{latex}{\figure{poisson.jpg}{options: width=3.5in}} \if{html}{\figure{poisson.jpg}{options: width=336}}}}

\value{Numeric, the pearson correlation of two numbers.}

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
    poisson_(k,lam)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~poisson }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution }% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }

