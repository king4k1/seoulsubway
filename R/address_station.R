### 지하철 주소를 얻기 위한 api
### 공공데이터포털 내 외부코드로 지하철 역 정보 조회

address_station <- function(code, apikey) {
  base_url <- "http://openAPI.seoul.go.kr:8088/"
  url_fed_to_get <- paste0(base_url,apikey, "/xml",
                           '/SearchSTNInfoByFRCodeService',
                           '/1/5/', code)
  APItree <- xmlTreeParse(url_fed_to_get, useInternalNodes = TRUE)
  APIroot <- xmlRoot(APItree)
  APIports <- APIroot[[3]]
  result <- data.frame("STATION_NM" =  
                         xmlApply(APIports[2]$STATION_NM, xmlValue)$text,
                       "STATION_NM_ENG" = 
                         xmlApply(APIports[3]$STATION_NM_ENG, xmlValue)$text,
                       "address" = 
                         xmlApply(APIports[13]$ADDRESS, xmlValue)$text,
                       "lat" =
                         xmlApply(APIports[36]$XPOINT_WGS, xmlValue)$text,
                       "long" = xmlApply(APIports[37]$YPOINT_WGS, xmlValue)$text)
  return(result)
}