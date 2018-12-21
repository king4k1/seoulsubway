\name{get_transfercriteria}
\alias{get_transfercriteria}
\title{get_transfercriteria}
\usage{
get_transfercriteria(depart, depart_line, arrival, arrival_line, penalty)
}
\description{
Select criteria via longitude/latitude and also use depart_line/arrival_line.
So you can get transfer station's list through some criteria in depart->arrival path way.
I gave +- 0.05 default penalty to longitude/latitude. because we can choose bypass route.
}
\examples{
get_transfercriteria(depart="먹골", depart_line="7", arrival="혜화", arrival_line="4", penalty=0.05)
}
