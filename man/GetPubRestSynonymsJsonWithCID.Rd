% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pubchem.R
\name{GetPubRestSynonymsJsonWithCID}
\alias{GetPubRestSynonymsJsonWithCID}
\title{GetPubRestSynonymsJsonWithCID}
\usage{
GetPubRestSynonymsJsonWithCID(cid, waitTime = 0)
}
\arguments{
\item{cid}{pubchem compound id}

\item{waitTime}{wait time for each round}
}
\value{
result in json
}
\description{
GetPubRestSynonymsJsonWithCID
}
\examples{
GetPubRestSynonymsJsonWithCID(cid = "33613")

}
