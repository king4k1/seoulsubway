\name{shortestpath}
\alias{shortestpath}
\title{shortestpath}
\usage{
shortestpath(departure, arrival, max_route=1)
}
\description{
Prints shortestpath(time depend) for subway in Seoul also you can get a subway list in your path.
when you can't get a path, you should consider another one.
cause these function is just consider zero, one, two, Three transfer count to find shortestpath for subway.

also you can consider another route by choosing max_route
}
\examples{
shortestpath(departure="", arrival="", max_route=1)
}
