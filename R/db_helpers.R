queryCityList <- function(
    pool){
  air_city_sql <- 'SELECT DISTINCT name FROM air_city_info'
  query <- sqlInterpolate(pool, air_city_sql)
  air_city_list <- dbGetQuery(pool, query)
  return(air_city_list)
}

queryAirSiteList <- function(
    pool,cityName){
  sql <- paste0("SELECT name ,code value FROM air_site_info where city='",cityName,"'")
  query <- sqlInterpolate(pool, sql)
  res <- dbGetQuery(pool, query)
  return(convert2List(res))
}

queryMetaSiteList <- function(
    pool,code){
  sql <- paste0("SELECT station name,mcode value FROM air_site_info where code='",code,"'")
  query <- sqlInterpolate(pool, sql)
  res <- dbGetQuery(pool, query)
  return(convert2List(res))
}

queryAnalysisData <- function(
    pool,airCode,metaCode,airType,stDate,edDate){
  metaData <- queryMeteData(pool,metaCode,stDate,edDate)
  airData <- queryCityAirData(pool,airCode,airType,stDate,edDate)
  data <- mergedData(airData,metaData)
  print(data)
  return(data)
}


queryMeteData <- function(pool,code,stDate,edDate){
  sql <-
    paste0('SELECT date,station,code,ws,wd,rh,air_temp,atmos_pres,dew_point FROM meta_site_data where code=',"'",code,"' and date>='",stDate,"' and date<='",edDate,"'")
  print(sql)
  query <-
    sqlInterpolate(pool, sql)
  res <- dbGetQuery(pool, query)
  return(res)
}


queryCityAirData <- function(pool,code,polType,stDate,edDate){
  print(polType)
  if(polType==0)
    sql <-
      paste0('SELECT datadate as date,name,"AQI","PM2.5" "PM25","PM10","SO2","NO2","O3",
           "CO" FROM air_city_data where name=',"'",code,"' and datadate>='",
             stDate,"' and datadate<='",edDate,"'")
  else{
    sql <-
      paste0('SELECT datadate as date,name,"AQI","PM2.5" "PM25","PM10","SO2","NO2","O3",
           "CO" FROM air_site_data where name=',"'",code,"' and datadate>='",
             stDate,"' and datadate<='",edDate,"'")
  }
  print(sql)
  query <-
    sqlInterpolate(pool, sql)
  res <- dbGetQuery(pool, query)
  return(res)
}

mergedData <-function(air_data,mete_data){
  data <- left_join(
    air_data,
    mete_data,
    by = "date"
  )
  return(data)
}



convert2List <- function(res){
  rlist <- as.list(res)
  nlist <- list()
  len <- length(rlist$name)
  i <- 1;
  while (i<=len) {
    nlist[rlist$name[i]]=paste0(rlist$name[i],"_",rlist$value[i]);
    i = i+1;
  }
  print(nlist)
  return(nlist)
}
