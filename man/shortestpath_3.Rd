\name{shortestpath_3}
\alias{shortestpath_3}
\title{shortestpath_3}
\usage{
shortestpath_3(depart, depart_line, arrival, arrival_line)
}
\description{
Prints shortestpath(Three transfer / time depend) for subway in Seoul.
this function will start in shortestpath when yo can't get path(both one, two transfer count)
when you can't get a path, then print 300count default.
cause this is just a sub-function for shortestpath().
}
\examples{
shortestpath_3(depart="먹골", depart_line="7", arrival="정릉", arrival_line="UI")
}
