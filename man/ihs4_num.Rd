% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ihs4_num.R
\name{ihs4_num}
\alias{ihs4_num}
\title{Calculates IHS4 scores based on abscess, draining fistula, and inflammatory
nodule counts.}
\usage{
ihs4_num(nodules, abscesses, draining_tunnels)
}
\arguments{
\item{nodules}{Integer vector representing the nodule count}

\item{abscesses}{Integer vector representing the abscess count}

\item{draining_tunnels}{Integer vector representing the draining tunnel count}
}
\value{
An integer vector representing the IHS4 scores (0-3: Mild,
4-10: Moderate, 11- : Severe).
}
\description{
Calculates IHS4 scores based on abscess, draining fistula, and inflammatory
nodule counts.
}
\examples{
ihs4_num(
  nodules = c(5, 3, 2),
  abscesses = c(2, 1, 0),
  draining_tunnels = c(1, 2, 3)
)
}
\references{
Zouboulis, C C et al. “Development and validation of the
International Hidradenitis Suppurativa Severity Score System (IHS4),
a novel dynamic scoring system to assess HS severity.” The British
journal of dermatology vol. 177,5 (2017): 1401-1409. doi:10.1111/bjd.15748
}
\seealso{
Other IHS4: 
\code{\link{ihs4_char}()}
}
\concept{IHS4}
