\name{cardiovascularrisk}
\alias{cardiovascularrisk}
\title{Cardiovascular risk prediction}
\usage{
cardiovascularrisk (age0, gender, ethnicity, smoke0, diabetes0, bprx, sbp, tchol0, hdl0, geo_region)
}
\arguments{
  \item{age}{a number}

  \item{gender}{male or female}

  \item{ethnicity}{African-American or Anglo or Asian or Hispanic}

  \item{smoker, current or former}{yes or no}

  \item{diabetes}{yes or no}

  \item{medication for hypertension}{yes or no}

  \item{systolic blood pressure}{a number}

  \item{total cholesterol}{a number}

  \item{HDL cholesterol}{a number}

  \item{geo_region}{string}
}
\value{
The probability of cardiovascular disease withing 10 years based on \code{age}, \code{gender}, \code{ethnicity}, \code{smoker}, \code{diabetes}, \code{medication for hypertension}, \code{systolic blood pressure}, \code{total cholesterol}, and \code{HDL cholesterol}.
}
\description{
This function implements the following clinical prediction rule:
\itemize{
	\item Goff et al. 2013 ACC/AHA guideline on the assessment of cardiovascular risk: a report of the American College of Cardiology/American Heart Association Task Force on Practice Guidelines. Circulation. 2014 PMID \href{http://pubmed.gov/24222018}{24222018}
	}
The following are validations of this rule:
\itemize{
	\item Pandya. Cost-effectiveness of 10-Year Risk Thresholds for Initiation of Statin Therapy for Primary Prevention of Cardiovascular Disease. JAMA. 2015 PMID \href{http://pubmed.gov/26172894}{26172894}
	\item Murtner et al. Validation of the atherosclerotic cardiovascular disease Pooled Cohort risk equations. JAMA 2014 PMID \href{http://pubmed.gov/24682252}{24682252}
	}
Concerns about this prediction rule is the lack of reliable data in the cohorts to include the following variables: family history of premature risk of Coronary disease, physicial fitness. The following are studies with concern about validity of this rule:
\itemize{
	\item DeFilippis. An analysis of calibration and discrimination among multiple cardiovascular risk scores in a modern multiethnic cohort. Ann Intern Med. 2015 PMID \href{http://pubmed.gov/25686167}{25686167}
	\item Ridker PM, Cook NR. Statins: new American guidelines for prevention of
cardiovascular disease. Lancet. 2013 PMID \href{http://pubmed.gov/24268611}{24268611}
	}
Alternative clinical prediction rules:
\itemize{
	\item Ridker et al. Development and validation of improved algorithms for the assessment of global cardiovascular risk in women: the Reynolds Risk Score. JAMA. 2007 PMID \href{http://pubmed.gov/17299196}{17299196}
	}
}
\author{
\itemize{
	\href{https://github.com/orgs/openRules/teams/cardiovascular-risk}{Cardiovascular risk}
	}
}
\references{
\itemize{
	\item Goff et al. 2013 ACC/AHA guideline on the assessment of cardiovascular risk: a report of the American College of Cardiology/American Heart Association Task Force on Practice Guidelines. Circulation. 2014 PMID \href{http://pubmed.gov/24222018}{24222018}
	}
}
\seealso{
\itemize{
	\item \code{\link{pending}}.
	}
}
\keyword{openRules}
\keyword{clinical prediction rules}

