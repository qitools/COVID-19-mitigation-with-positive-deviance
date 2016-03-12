\name{brca}
\alias{brca}
\title{brca}
\usage{
brca (genetics = 'no', history = 'no', T1 = 0, AgeMen = 99, Age1st = 99, N_Rels = 99, N_Biop = 99, HypPlas = 99, Race = 1)
}
\arguments{
  \item{genetics}{yes or no}

  \item{history}{string}

  \item{T1}{A number}

  \item{AgeMen}{A number}

  \item{Age1st}{A number}

  \item{N_Rels}{A number}

  \item{N_Biop}{A number}

  \item{HypPlas}{A number}

  \item{Race}{A number}

}
\value{
The probability of breast cancer based on \code{T1} and \code{AgeMen} and \code{Age1st} and \code{N_Rels} and \code{N_Biop}.
}
\description{
This function implements the following equation:
\itemize{
	\item \href{http://pubmed.gov/10491430}{Costantino JP, Gail MH, Pee D, Anderson S, Redmond CK, Benichou J, Wieand S,  Validation studies for models projecting the risk of invasive and total breast cancer incidence. J Natl Cancer Inst 1999; 91:1541-1548}
	}
}
\examples{
Vignette: 50 year old woman with no risk factors
}
\author{
\itemize{
	\item \href{https://github.com/orgs/openRules/teams/brca}{BRCA}
	}
}
\references{
\itemize{
	\item \href{http://pubmed.gov/10491430}{Costantino JP, Gail MH, Pee D, Anderson S, Redmond CK, Benichou J, Wieand S,  Validation studies for models projecting the risk of invasive and total breast cancer incidence. J Natl Cancer Inst 1999; 91:1541-1548}
	}
}
\seealso{
\itemize{
\item \code{\link{pending}}.
	}
}
\keyword{openRules}
\keyword{clinical prediction rules}

