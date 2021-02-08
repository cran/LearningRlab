\name{quartile_}
\alias{quartile_}
\title{Quartiles Calculus}
\description{Calculates the 3 Quartiles of a vector of data}
\usage{
quartile_(x)
}
\arguments{
  \item{x}{Should be a vector}
}
\details{To calculate the quartiles, the user should give a vector. This function divide the dataset in 4 parts as equal as possible. The formula is the following:
 \cr{\figure{quartile.jpg}{options: width=3.5in}}}

\value{A vector sorted with the elements divided by 4 parts}

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
    data = c(1:20)
    
    quartile_(data)
   
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~quartile }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~cuartiles }% __ONLY ONE__ keyword per line
\keyword{ ~quantile }
