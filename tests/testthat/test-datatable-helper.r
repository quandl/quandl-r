mock_datatable_data <- function(cursor_id = 'null') {
  mock_data <- "{\"datatable\":
    {\"data\":
      [[\"AAPL\",10.102,null,\"1979-09-30\"],
      [\"AAPL\",23.585,null,\"1980-09-30\"],
      [\"AAPL\",66.143,null,\"1981-09-30\"],
      [\"AAPL\",102.209,null,\"1982-09-30\"],
      [\"AAPL\",129.639,null,\"1983-09-30\"],
      [\"AAPL\",91.448,null,\"1984-09-30\"],
      [\"AAPL\",139.734,null,\"1985-09-30\"],
      [\"AAPL\",273.531,null,\"1986-09-30\"],
      [\"AAPL\",371.4382,null,\"1987-09-30\"],
      [\"AAPL\",620.3379,null,\"1988-09-30\"],
      [\"AAPL\",634.313,null,\"1989-09-30\"],
      [\"AAPL\",712.0122,null,\"1990-09-30\"],
      [\"AAPL\",671.3921,null,\"1991-09-30\"],
      [\"AAPL\",805.8086,null,\"1992-09-30\"],
      [\"AAPL\",431.1943,null,\"1993-09-30\"],
      [\"AAPL\",395.4189,null,\"1994-09-30\"],
      [\"AAPL\",661.0,null,\"1995-09-30\"],
      [\"AAPL\",-1204.0,null,\"1996-09-30\"],
      [\"AAPL\",-403.0,null,\"1997-09-30\"],
      [\"AAPL\",268.0,null,\"1998-09-30\"],
      [\"AAPL\",386.0,null,\"1999-09-30\"],
      [\"AAPL\",522.0,null,\"2000-09-30\"],
      [\"AAPL\",-344.0,26992,\"2001-09-30\"],
      [\"AAPL\",17.0,28310,\"2002-09-30\"],
      [\"AAPL\",-1.0,29015,\"2003-09-30\"]],
      \"columns\":[{\"name\":\"ticker\",\"type\":\"String\"},
                 {\"name\":\"oper_income\",\"type\":\"BigDecimal(12,4)\"},
                 {\"name\":\"comm_share_holder\",\"type\":\"Integer\"},
                 {\"name\":\"per_end_date\",\"type\":\"Date\"}]},
      \"meta\":{\"next_cursor_id\":\"#cursor_id\"
    }
  }"
  mock_data <- gsub("\"#cursor_id\"", cursor_id, mock_data)
  return(mock_data)
}

mock_empty_datatable_data <- function() {
  mock_data <- "{\"datatable\":
  {\"data\": [],
  \"columns\":[{\"name\":\"ticker\",\"type\":\"String\"},
  {\"name\":\"oper_income\",\"type\":\"BigDecimal(12,4)\"},
  {\"name\":\"comm_share_holder\",\"type\":\"Integer\"},
  {\"name\":\"per_end_date\",\"type\":\"Date\"}]},
  \"meta\":{\"next_cursor_id\": null
  }
  }"
  return(mock_data)
}
