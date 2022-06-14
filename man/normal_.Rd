\name{normal_}

\alias{normal_}

\title{Normal Distribution Calculus Function}

\description{This function calculates the normal distribution of experiment.}

\usage{
normal_(x)
}

\arguments{
  \item{x}{Should be a numbers.}
}

\details{To calculate the normal distribution, the user should give a number. The result isis a type of continuous probability distribution for a real-valued random variable. The normal distribution formule is the following:  \cr{\if{latex}{\figure{normal.jpg}{options: width=2.5in}} \if{html}{\figure{normal.jpg}{options: width=240}}}}

\value{Numeric, the normal distribution.}

\author{Jose Manuel Gomez Caceres, \email{josemanuel.gomezc@edu.uah.es}
\cr{Juan Jose Cuadrado, \email{jjcg@uah.es}}
\cr{Universidad de Alcala de Henares}
}

\note{The variable is a number. Example: x <- 0.1 }

%\seealso{}

\examples{

    #data creation
    x = 0.1
  
    normal_(x)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~normal }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~distribution}% __ONLY ONE__ keyword per line
\keyword{ ~distribucion }

