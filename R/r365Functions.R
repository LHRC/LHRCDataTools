brgId <- "2cd5cddd-8b98-4207-8ade-e9e160d761c1"
bnsId <- "32a6eae8-7a87-40f1-aa06-c71633bb18e1"
okcId <- "f0808e2c-880a-426d-bb03-2db7a2bdc78e"

getDFFromEndpoint <- function(endpointURL, user, password) {
  endpointURL <- gsub(" ", "%20", endpointURL)
  endpointURL <- gsub("'", "%27", endpointURL)
  x <- ifelse(grepl("[?]", endpointURL), "&$count=true", "?$count=true")
  endpointURL <- paste(endpointURL, x, sep = "")
  # print(endpointURL)
  req <- request(endpointURL) |> req_auth_basic(user, pw)
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
    req <- request(epurl) |> req_auth_basic(user, pw)
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

getGLAccountsDF <- function(user, password) {
  GLAccountsEndpoint <- "https://odata.restaurant365.net/api/v2/views/GlAccount"
  DF <- getDFFromEndpoint(GLAccountsEndpoint) %>%
    select(glAccountId, name, glAccountNumber, glType)
  DF
}

getLocationsDF <- function(user, password){

}

getTransactionsBetween <- function(locationID, startDate, endDate, user, password) {
  #"https://odata.restaurant365.net/api/v2/views/Transaction?$filter=date%20ge%202024-02-26T00:00:00Z%20and%20date%20lt%202024-02-29T00:00:00Z"
  txEndpoint <- stringr::str_glue("https://odata.restaurant365.net/api/v2/views/Transaction?$filter=locationId%20eq%20{locationID}%20and%20date%20ge%20{startDate}T00:00:00Z%20and%20date%20lt%20{endDate}T23:59:59Z")
  print(txEndpoint)
  txDF <- getDFFromEndpoint(txEndpoint)
  #txDF <- txDF %>% filter (type != "Budget")
  detailDF <- getTransactionDetailsDF(txDF %>% select(transactionId))

  combinedDF <- left_join(detailDF, txDF, by = "transactionId")
  combinedDF
}
