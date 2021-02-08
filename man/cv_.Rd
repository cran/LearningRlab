\name{cv_}

\alias{cv_}

\title{Coefficient of Variation Calculus Function}

\description{This function calculates the coefficient of variation of a numbers vector.}

\usage{
cv_(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the coefficient of variation, the user should give a numbers vector. The result is defined as the ratio of the standard deviation to the mean. The coefficient of variation formule is the following: \cr{\figure{cv.jpg}{options: width=2.5in}}}

\value{Numeric, the coefficient of variation of the numbers vector.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }

%\seealso{}

\examples{

    #data creation
    data = c(1,4,3,3,2,5,7,12,1,2,3,12)
    
    cv_(data)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~coefficient}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~variation }% __ONLY ONE__ keyword per line
\keyword{ ~coeficiente }
\keyword{ ~variacion }
\keyword{ ~cv }

