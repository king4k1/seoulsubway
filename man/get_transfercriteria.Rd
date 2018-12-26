\name{get_transfercriteria}
\alias{get_transfercriteria}
\title{get_transfercriteria}
\usage{
get_transfercriteria(depart, arrival, penalty)
}
\description{
Select criteria via longitude/latitude.
So you can get transfer station's list through some criteria in depart->arrival path way.
I gave +- 0.05 default penalty to longitude/latitude. because we can choose bypass route.
}
\examples{
get_transfercriteria(depart="", arrival="", penalty=0.05)
}
