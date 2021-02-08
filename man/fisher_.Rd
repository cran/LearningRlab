\name{fisher_}

\alias{fisher_}

\title{F Fisher Distribution Calculus Function}

\description{This function calculates the fisher distribution of a numbers vector.}

\usage{
fisher_(x,y)
}

\arguments{
  \item{x}{Should be a vector}
  \item{y}{Should be a vector}
}

\details{To calculate the fisher distribution, the user should give two vectors of numbers. The result is a continuous probability distribution that arises frequently as the null distribution of a test statistic. The fisher distributionformule is the following:  \cr{\figure{fisher.jpg}{options: width=3.5in}}}

\value{Numeric, the fisher distribution.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{A vector is created by c(), like c(1,2,3,4,5) creates a vector with the numbers: 1,2,3,4,5}

%\seealso{}

\examples{

    #data creation
    x <- c(70,75,74,72,68,59)
    y <- c(74,77,70,80,72,76)
    
    fisher_(x,y)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~tstudent }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution }% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }

