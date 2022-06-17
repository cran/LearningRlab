\name{mode_}

\alias{mode_}

\title{Mode Calculus Function}

\description{This function calculates the mode of a numbers vector.}

\usage{
mode_(x)
}

\arguments{
  \item{x}{Should be a numbers vector}
}

\details{To calculate the mode of a dataset, the user should give a numbers vector. The result is the numeric value that appears most often. In other words, it's the value that is most likely to be sampled. The mode formule is the following: \cr{\if{latex}{\figure{mode.jpg}{options: width=2.5in}} \if{html}{\figure{mode.jpg}{options: width=240}}}}

\value{Numeric, the mode of the numbers vector.}

\author{
Dennis Monheimius, \email{dennis.monhemimius@edu.uah.es}
\cr{Eduardo Benito, \email{eduardo.benito@edu.uah.es}}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5 }

%\seealso{}

\examples{
{

    #data creation
    data = c(1,2,2,3,4)
    
    mode_(data)
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~mode }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~moda }% __ONLY ONE__ keyword per line
