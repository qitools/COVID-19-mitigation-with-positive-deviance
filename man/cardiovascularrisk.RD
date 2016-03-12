\name{cardiovascularrisk}
\alias{cardiovascularrisk}
\title{Cardiovascular risk prediction}
\usage{
pulmnodule (age, diameter, cancer, smoker, spiculation, location, geo_region)
}
\arguments{
  \item{age}{A number}

  \item{diameter}{A number}

  \item{smoker, current or former}{yes or no}

  \item{spiculation}{yes or no}

  \item{location in upper lobe}{yes or no}

  \item{geo_region}{string}
}
\value{
The probability of cardiovascular disease withing 10 years based on \code{age}, \code{gender}, \code{ethnicity}, \code{smoker}, \code{diabetes}, \code{medication for hypertension}, \code{systolic blood pressure}, \code{total cholesterol}, and \code{HDL cholesterol}.
}
\description{
This function implements the following clinical prediction rule:
\itemize{
\item \href{http://pubmed.gov/24222018}{New Pooled Cohort ASCVD Risk Equations} for the 2013 ACC/AHA Guidelines to Reduce Cardiovascular Risk
}
}

\author{
  \href{https://github.com/orgs/openRules/teams/cardiovascular-risk}{Cardiovascular risk}
}
\references{
\href{http://pubmed.gov/24222018}{New Pooled Cohort ASCVD Risk Equations} for the 2013 ACC/AHA Guidelines to Reduce Cardiovascular Risk
}
\seealso{
\itemize{
\item \code{\link{pending}}.
}
}
\keyword{openRules}
\keyword{clinical prediction rules}

