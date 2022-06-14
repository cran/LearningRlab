\name{laplace_}

\alias{laplace_}

\title{Laplace's Rule Calculus Function}

\description{This function calculates the Laplace's rule of experiment.}

\usage{
laplace_(x,y)
}

\arguments{
  \item{x}{Should be a vector}
  \item{y}{Should be a vector}
}

\details{To calculate the Laplace's rule, the user should give two vector (unfavorable cases/favorable cases). The result isas the quotient between the number of favorable cases to A, and that of all possible results of the experiment. The Laplace's rule formule is the following:  \cr{\if{latex}{\figure{laplace.jpg}{options: width=3.5in}} \if{html}{\figure{laplace.jpg}{options: width=336}}}}

\value{Numeric, the pearson correlation.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }

%\seealso{}

\examples{

    #data creation
    data = 3
    data2 = c(1,2,3,4,5,6)
    laplace_(data, data2)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~laplace }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~rule}% __ONLY ONE__ keyword per line
\keyword{ ~regla }

