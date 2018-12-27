\name{get_transferinfo}
\alias{get_transferinfo}
\title{get_transferinfo}
\usage{
get_transferinfo(depart, depart_line, arrival, arrival_line, count)
}
\description{
Prints available transfer station when you move from A to B in subway line
This function is useful.because, use criteria for select transfer station.
criteria is informed in get_transfercriteria
you can choose 1,2 in 'count' argument
}
\examples{
get_transferinfo(depart="", depart_line="", arrival="", arrival_line="", count=1)

get_transferinfo(depart="", depart_line="", arrival="", arrival_line="", count=2)
}
