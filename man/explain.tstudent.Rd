\name{explain.tstudent}

\alias{explain.tstudent}

\title{T-Student Distribution Function Explained}

\description{Step by step demonstration of the T-Student distribution calculus.}

\usage{
explain.tstudent(x,u,s,n)
}

\arguments{
  \item{x}{Should be a number}
  \item{u}{Should be a number}
  \item{s}{Should be a number}
  \item{n}{Should be a number}
}

\details{To calculate the T-Student distribution, the user should give four number (sample mean, population mean, population standard deviation and sample size). The result  is a probability distribution that arises from the problem of estimating the mean of a normally distributed population when the sample size is small. We can saw the T-Student distribution formule in the tstudent_ help document.}

\value{Numeric result and the process of this calculus explained.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{Each variable is a number. Example: x <- 2 | y <- 4}

%\seealso{}

\examples{
  
   #data creation
    x = 52.9
    u = 50
    s = 3
    n = 10
    
    explain.tstudent(x,u,s,n)
    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~tstudent }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution }% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }
\keyword{ ~explain }
\keyword{ ~explicada}
