\name{pearson_}

\alias{pearson_}

\title{Pearson Correlation Calculus Function}

\description{This function calculates the pearson correlation of two vectors of numbers.}

\usage{
pearson_(x,y)
}

\arguments{
  \item{x}{Should be a vector}
  \item{y}{Should be a vector}
}

\details{To calculate the pearson correlation, the user should give two vectors of numbers. The result is the covariance of the two vectors of numbers divided by the product of their standard deviations. The pearson correlation formule is the following:  \cr{\figure{pearson.jpg}{options: width=3.5in}}}

\value{Numeric, the pearson correlation of two vectors of numbers.}

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
    pearson_(data, data2)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~pearson }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~correlaction }% __ONLY ONE__ keyword per line
\keyword{ ~correlaccion }

