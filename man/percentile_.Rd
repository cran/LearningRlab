\name{percentile_}
\alias{percentile_}
\title{Percentile Calculus Function}
\description{This function calculate the percentiles of a vector of numbers}
\usage{
percentile_(x)
}
\arguments{
  \item{x}{Should be a vector}
}
\details{To calculate the percentiles, the user should give a vector. This function divide the dataset in 100 parts as equal as possible. The formula is the following: }

\value{A vector sorted with the elements divided by 100 parts}

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
    data = c(1,4,3,3,2,5,7,12,1,2,3,12)
    
    percentile_(data)
    
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~percentile }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~percentil }% __ONLY ONE__ keyword per line
