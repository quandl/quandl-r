reset_config <- function() {
  Quandl.api_key(NULL)
  Quandl:::Quandl.api_version(NULL)
}

mock_data <- function(database_code = "NSE", dataset_code = "OIL") {
  response_start <- "{\"dataset\":{ \"id\":6668,"
  dataset_code <- paste0("\"dataset_code\":\"", dataset_code, "\"", ",")
  database_code <- paste0("\"database_code\":\"", database_code, "\"", ",")
  response_end <- "\"name\":\"Oil India Limited\",
       \"description\":\"Historical\",
       \"refreshed_at\":\"2015-08-07T02:37:20.453Z\",
       \"newest_available_date\":\"2015-08-06\",
       \"oldest_available_date\":\"2009-09-30\",
       \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Last\",\"Close\",\"Total Trade Quantity\",\"Turnover (Lacs)\"],
       \"frequency\":\"daily\",
       \"type\":\"Time Series\",
       \"premium\":false,\"limit\":2,
       \"transform\":null,
       \"column_index\":null,
       \"start_date\":\"2009-09-30\",
       \"end_date\":\"2015-08-06\",
       \"data\":[[\"2015-08-06\",450.9,460.7,447.3,454.8,456.4,339324.0,1542.22],[\"2015-08-05\",440.5,454.0,439.05,450.2,449.4,287698.0,1286.17]],
       \"collapse\":null,
       \"order\":\"desc\",
       \"database_id\":33
    }
  }"
  paste0(response_start, dataset_code, database_code, response_end)
}

mock_annual_data <- function() {
  "{
    \"dataset\":{
      \"id\":6668,
      \"dataset_code\":\"OIL\",
      \"database_code\":\"NSE\",
      \"name\":\"Oil India Limited\",
      \"description\":\"Historical prices for Oil India Limited\",
      \"refreshed_at\":\"2015-08-07T02:37:20.453Z\",
      \"newest_available_date\":\"2015-08-06\",
      \"oldest_available_date\":\"2009-09-30\",
      \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Last\",\"Close\",\"Total Trade Quantity\",\"Turnover (Lacs)\"],
      \"frequency\":\"daily\",
      \"type\":\"Time Series\",
      \"premium\":false,
      \"limit\":3,
      \"transform\":null,
      \"column_index\":null,
      \"start_date\":\"2009-09-30\",
      \"end_date\":\"2015-08-06\",
      \"data\":[[\"2015-12-31\",450.9,460.7,447.3,454.8,456.4,339324.0,1542.22],[\"2014-12-31\",565.0,579.0,565.0,578.9,576.4,212525.0,1220.73],[\"2013-12-31\",484.05,492.0,475.1,488.2,488.35,359499.0,1741.57]],
      \"collapse\":\"annual\",
      \"order\":\"desc\",
      \"database_id\":33
    }
  }"
}

mock_monthly_data <- function() {
 "{
    \"dataset\":{
      \"id\":6668,
      \"dataset_code\":\"OIL\",
      \"database_code\":\"NSE\",
      \"name\":\"Oil India Limited\",
      \"description\":\"Historical prices for Oil India Limited\\u003cbr\\u003e\\u003cbr\\u003eNational Stock Exchange of India\\u003cbr\\u003e\\u003cbr\\u003eTicker: OIL\\u003cbr\\u003e\\u003cbr\\u003eISIN: INE274J01014\",
      \"refreshed_at\":\"2015-08-07T02:37:20.453Z\",
      \"newest_available_date\":\"2015-08-06\",
      \"oldest_available_date\":\"2009-09-30\",
      \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Last\",\"Close\",\"Total Trade Quantity\",\"Turnover (Lacs)\"],
      \"frequency\":\"daily\",
      \"type\":\"Time Series\",
      \"premium\":false,
      \"limit\":3,
      \"transform\":null,
      \"column_index\":null,
      \"start_date\":\"2009-09-30\",
      \"end_date\":\"2015-08-06\",
      \"data\":[[\"2015-08-31\",450.9,460.7,447.3,454.8,456.4,339324.0,1542.22],[\"2015-07-31\",425.0,435.0,423.2,432.95,432.35,330239.0,1416.0],[\"2015-06-30\",448.0,451.7,445.1,447.8,446.8,352514.0,1576.93]],
      \"collapse\":\"monthly\",
      \"order\":\"desc\",
      \"database_id\":33
    }
  }"
}

mock_annual_frequency_data <- function() {
  "{
    \"dataset\":{
      \"id\":9610551,
      \"dataset_code\":\"4\",
      \"database_code\":\"TESTS\",
      \"name\":\"Annual\",
      \"description\":\"Annual Dataset\",
      \"refreshed_at\":\"2013-12-13T16:41:20.000Z\",
      \"newest_available_date\":\"2012-12-31\",
      \"oldest_available_date\":\"1960-12-31\",
      \"column_names\":[\"Date\",\"Value\"],
      \"frequency\":\"annual\",
      \"type\":\"Time Series\",
      \"premium\":false,
      \"limit\":null,
      \"transform\":null,
      \"column_index\":null,
      \"start_date\":\"1995-01-01\",
      \"end_date\":\"2005-01-01\",
      \"data\":[[\"2005-12-31\",235423072.52975],[\"2004-12-31\",235423072.52975],[\"2003-12-31\",232301606.48629],[\"2002-12-31\",229369011.15939],[\"2001-12-31\",226314934.86817],[\"2000-12-31\",223159429.23579],[\"1999-12-31\",219663636.48],[\"1998-12-31\",216140988.036],[\"1997-12-31\",212633197.392],[\"1996-12-31\",209097696.132],[\"1995-12-31\",205699755.0]],
      \"collapse\":null,
      \"order\":\"desc\",
      \"database_id\":3509
    }
  }"
}

mock_quarterly_collapse_data <- function() {
  "{
    \"dataset\":{
      \"id\":9610539,
      \"dataset_code\":\"1\",
      \"database_code\":\"TESTS\",
      \"name\":\"Daily Dataset Test\",
      \"description\":\"Daily Dataset\",
      \"refreshed_at\":\"2014-01-07T23:46:36.000Z\",
      \"newest_available_date\":\"2013-12-12\",
      \"oldest_available_date\":\"2012-12-03\",
      \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Last\",\"Close\",\"Total Trade Quantity\",\"Turnover (Lacs)\"],
      \"frequency\":\"daily\",
      \"type\":\"Time Series\",
      \"premium\":false,
      \"limit\":null,
      \"transform\":null,
      \"column_index\":null,
      \"start_date\":\"2012-12-03\",
      \"end_date\":\"2013-12-12\",
      \"data\":[[\"2013-12-31\",462.0,470.65,458.5,461.0,460.05,622936.0,2903.94],[\"2013-09-30\",449.7,457.8,435.0,435.25,437.4,394564.0,1742.0],[\"2013-06-30\",616.0,621.0,552.15,571.1,574.55,2132226.0,12276.45],[\"2013-03-31\",506.85,519.9,506.85,508.1,510.75,845984.0,4348.81],[\"2012-12-31\",463.1,470.0,462.1,466.0,465.8,135251.0,630.17]],
      \"collapse\":\"quarterly\",
      \"order\":\"desc\",
      \"database_id\":3509
    }
  }"
}

mock_search_empty_response <- function() {
  "{\"datasets\":[],\"meta\":{\"query\":\"sdfsdfdsfsdf\",\"per_page\":10,\"current_page\":1,\"prev_page\":null,\"total_pages\":0,\"total_count\":0}}"
}

mock_search_response <- function() {
  "{\"datasets\":[
    {
      \"id\":6668,
      \"dataset_code\":\"OIL\",
      \"database_code\":\"NSE\",
      \"name\":\"Oil India Limited\",
      \"description\":\"Historical prices for Oil India Limited\\u003cbr\\u003e\\u003cbr\\u003eNational Stock Exchange of India\\u003cbr\\u003e\\u003cbr\\u003eTicker: OIL\\u003cbr\\u003e\\u003cbr\\u003eISIN: INE274J01014\",
      \"refreshed_at\":\"2015-08-08T03:23:52.573Z\",
      \"newest_available_date\":\"2015-08-07\",
      \"oldest_available_date\":\"2009-09-30\",
      \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Last\",\"Close\",\"Total Trade Quantity\",\"Turnover (Lacs)\"],
      \"frequency\":\"daily\",
      \"type\":\"Time Series\",
      \"premium\":false,
      \"database_id\":33
    },
    {
      \"id\":20002963,
      \"dataset_code\":\"OIL\",
      \"database_code\":\"XTSX\",
      \"name\":\"LGX Oil \\u0026 Gas Inc (OIL) Adjusted Stock Prices\",
      \"description\":\" \\u003cb\\u003eTicker\",
      \"refreshed_at\":\"2015-03-05T16:05:18.000Z\",
      \"newest_available_date\":\"2015-03-04\",
      \"oldest_available_date\":\"2015-02-17\",
      \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Close\",\"Volume\",\"Adjustment Factor\",\"Adjustment Type\"],
      \"frequency\":\"daily\",
      \"type\":\"Time Series\",
      \"premium\":true,
      \"database_id\":13187
    },
    {
      \"id\":1994252,
      \"dataset_code\":\"OIL\",
      \"database_code\":\"ECONWEBINS\",
      \"name\":\"Crude Oil Prices 1861-1999\",
      \"description\":\"U.S. Dollars per barrel. Source:BP, 1861-1944:US Average\",
      \"refreshed_at\":\"2014-09-10T23:21:53.002Z\",
      \"newest_available_date\":\"1999-12-31\",
      \"oldest_available_date\":\"1861-12-31\",
      \"column_names\":[\"Year\",\"$ (Money of the Day)\",\"$ (1999)\"],
      \"frequency\":\"annual\",
      \"type\":\"Time Series\",
      \"premium\":false,
      \"database_id\":247
    }],
    \"meta\":{
      \"query\":\"oil\",
      \"per_page\":3,
      \"current_page\":1,
      \"prev_page\":null,
      \"total_pages\":149466,
      \"total_count\":448398,
      \"next_page\":2,
      \"current_first_item\":1,
      \"current_last_item\":3
    }
  }"
}

mock_response <- function(status_code = 200,
 content = mock_data()) {
  httr:::response(
    status_code = status_code,
    content = content
  )
}