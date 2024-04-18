retrieveR365EndpointAsDF <- function(endpointURL, user, password) {
  endpointURL <- gsub(" ", "%20", endpointURL)
  endpointURL <- gsub("'", "%27", endpointURL)
  x <- ifelse(grepl("[?]", endpointURL), "&$count=true", "?$count=true")
  endpointURL <- paste(endpointURL, x, sep = "")
  # print(endpointURL)
  req <- request(endpointURL) |> req_auth_basic(user, password)
  resp <- req_perform(req)
  respJSON <- resp |> httr2::resp_body_json()
  count <- respJSON$`@odata.count`
  # print(count)
  DF <- respJSON$value %>%
    spread_all()
  pageSize <- nrow(DF)

  i <- 1
  while (nrow(DF) < count) {
    #print(paste("loop", i))
    epurl <- paste(endpointURL, "&$skip=", i * pageSize, sep = "")
    # print(epurl)
    req <- request(epurl) |> req_auth_basic(user, password)
    resp <- req_perform(req)
    respJSON <- resp |> httr2::resp_body_json()
    df <- respJSON$value %>%
      spread_all()
    DF <- bind_rows(DF, df)
    i <- i + 1
  }
  #attr(DF, "class") <- attr(DF, "class")[-1]
  DF <- as.data.frame(DF)
  DF %>% select(-c(`..JSON`))
}
