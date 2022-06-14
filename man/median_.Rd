\name{median_}

\alias{median_}

\title{Median Calculus Function}

\description{This function calculates the median of a numbers vector.}

\usage{
	median_(x)
}

\arguments{
	\item{x}{Should be a numbers vector}
}

\details{To calculate the median, the user should give a numbers vector. The result is the value separating the higher half from the lower half of the dataset, it may be thought of as the middle value. The median formule is the following: \cr{\if{latex}{\figure{median.jpg}{options: width=3.5in} \if{html}{\figure{median.jpg}{options: width=336}}}
}

\value{A numeric, the median of the numbers vector.}

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

result = median_(c(1,3,2,5,12,4,4,2,9))
result
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~median }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mediana }% __ONLY ONE__ keyword per line
