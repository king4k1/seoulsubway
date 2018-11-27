\name{get_transferinfo}
\alias{get_transferinfo}
\title{get_transferinfo}
\usage{
get_transferinfo(depart, depart_line, arrival, arrival_line, count=count)
}
\description{
Prints available transfer station when you move from A to B in subway line
This function is useful.because, use criteria for select transfer station.
criteria is informed in get_transfercriteria
you can choose 1,2 in 'count' argument
}
\examples{
get_transferinfo(depart="먹골", depart_line="7", arrival="혜화", arrival_line="4", count=1)

get_transferinfo(depart="먹골", depart_line="7", arrival="혜화", arrival_line="4", count=2)
}
