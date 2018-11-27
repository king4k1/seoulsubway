\name{pathplot}
\alias{pathplot}
\title{pathplot}
\usage{
pathplot(depart, depart_line, arrival, arrival_line, naver_secret,
                     naver_key, kakao_key, zoom=4)
}
\description{
Prints shortestpath(time depend) plot for subway in Seoul
}
\examples{
pathplot(depart = "신도림", depart_line = "2", arrival = "강동",
            arrival_line = "5", kakao_key=kakao_key,
            naver_key=naver_key, naver_secret=naver_secret, zoom=6)}
