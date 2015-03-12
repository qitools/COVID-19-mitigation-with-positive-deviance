\name{pulmnodule}
\alias{pulmnodule}
\title{pulmnodule}
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
The probability of malignancy based on \code{age} and \code{diameter} and \code{smoker} and \code{spiculation} and \code{location} and text of clinical suggestions partly based on geo_region.
}
\description{
This function implements the following clinical prediction rule:
\itemize{
\item \href{http://pubmed.gov/9129544}{Mayo Clinic model for prediction of malignancy in patients with noncalcified pulmonary nodules. 1997}
}
}
\examples{
Vignette: 3 mm nodule in a 80 yo male with yes to all questions and upper lobe location
}
\author{
  \href{https://github.com/orgs/openRules/teams/pulmonary-nodule}{Pulmonary nodule}
}
\references\{
\href{http://pubmed.gov/23649456}{Diagnosis and Management of Lung Cancer, 3rd ed: American College of Chest Physicians Evidence-Based Clinical Practice Guidelines. 2012}
}
\seealso{
\itemize{
\item \code{\link{pending}}.
}
}
\keyword{openRules}
\keyword{clinical prediction rules}

http://cran.r-project.org/doc/manuals/R-exts.html
http://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
