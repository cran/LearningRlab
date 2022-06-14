\name{chisquared_}

\alias{chisquared_}

\title{Chisquared Distribution Calculus Function}

\description{This function calculates the chisquared distribution of two vectors of numbers.}

\usage{
chisquared_(x,y)
}

\arguments{
  \item{x}{Should be a vector.}
  \item{y}{Should be a vector.}
}

\details{To calculate the chisquared distribution, the user should give two vectors of numbers. The result  is a sum of the squares of k independent standard normal random variables. The chisquared distribution formule is the following:  \cr{\if{latex}{\figure{chisquared.jpg}{options: width=3.5in}} \if{html}{\figure{chisquared.jpg}{options: width=336}}}}

\value{Numeric, the chisquared distribution of two vectors of numbers.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }

%\seealso{}

\examples{

    #data creation
    data = c(1,4,3,3,2,5,7,12,1,2,3,12)
    data2 = c(1,2,4,4,6,5,11,2,10,5,6,1)
    chisquared_(data, data2)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~chisquared_ }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~chicuadrado }% __ONLY ONE__ keyword per line
