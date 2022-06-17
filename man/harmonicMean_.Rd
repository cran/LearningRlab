\name{harmonicMean_ }

\alias{harmonicMean_ }

\title{Harmonic Mean Function}

\description{This function calculates the harmonic mean of a numbers vector.}

\usage{
harmonicMean_ (x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the harmonic mean, the user should give a numbers vector. The result is calculated by dividing the number of observations by the reciprocal of each number in the vector. The harmonic mean formule is the following: \cr{\if{latex}{\figure{harmonicMean.jpg}{options: width=3.5in}} \if{html}{\figure{harmonicMean.jpg}{options: width=336}}}}

\value{Numeric, the harmonic mean of the numbers vector.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }

%\seealso{}

\examples{

    #data creation
    data = c(1,4,3,3,2,5,7,12,1,2,3,12)
    
    harmonicMean_(data)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~harmonic }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~armonica }% __ONLY ONE__ keyword per line
\keyword{ ~mean }
\keyword{ ~media }
