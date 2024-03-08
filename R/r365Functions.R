brgId <- "2cd5cddd-8b98-4207-8ade-e9e160d761c1"
bnsId <- "32a6eae8-7a87-40f1-aa06-c71633bb18e1"
okcId <- "f0808e2c-880a-426d-bb03-2db7a2bdc78e"

#' @export
getDFFromEndpoint <- function(endpointURL, user, password) {
  endpointURL <- gsub(" ", "%20", endpointURL)
  endpointURL <- gsub("'", "%27", endpointURL)
  x <- ifelse(grepl("[?]", endpointURL), "&$count=true", "?$count=true")
  endpointURL <- paste(endpointURL, x, sep = "")
  # print(endpointURL)
  req <- httr2::request(endpointURL) |> httr2::req_auth_basic(user, password)
  resp <- httr2::req_perform(req)
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

#' @export
getGLAccountsDF <- function(user, password) {
  GLAccountsEndpoint <- "https://odata.restaurant365.net/api/v2/views/GlAccount"
  DF <- getDFFromEndpoint(GLAccountsEndpoint) %>%
    select(glAccountId, name, glAccountNumber, glType)
  DF
}

#' @export
getLocationsDF <- function(user, password){

}

#' @export
getTransactionsForLocationBetweenDF <- function(locationID, startDate, endDate, user, password) {
  #"https://odata.restaurant365.net/api/v2/views/Transaction?$filter=date%20ge%202024-02-26T00:00:00Z%20and%20date%20lt%202024-02-29T00:00:00Z"
  txEndpoint <- stringr::str_glue("https://odata.restaurant365.net/api/v2/views/Transaction?$filter=locationId%20eq%20{locationID}%20and%20date%20ge%20{startDate}T00:00:00Z%20and%20date%20lt%20{endDate}T23:59:59Z")
  print(txEndpoint)
  txDF <- getDFFromEndpoint(txEndpoint)
  #txDF <- txDF %>% filter (type != "Budget")
  detailDF <- getTransactionDetailsDF(txDF %>% select(transactionId))
  combinedDF <- left_join(detailDF, txDF, by = "transactionId")
  combinedDF
}

#' @export
getTransactionDetailsDF <- function(txIDs) {
  for (row in 1:nrow(txIDs)) {
    result <- getTransactionDetails(txIDs[row, "transactionId"])
    if (row == 1) {
      DF <- result
    } else {
      if (! is.null(result) ) {
        DF <- dplyr::bind_rows(DF, result)
      }
    }
  }
  DF
}
#' @export
getTransactionDetailsDF <- function(txID) {
  txDetEndpoint <- stringr::str_glue("https://odata.restaurant365.net/api/v2/views/TransactionDetail?$filter=transactionId%20eq%20({txID})")
  print(txDetEndpoint)
  DF <- getDFFromEndpoint(txDetEndpoint)
  if (nrow(DF) > 0) {
    DF <- DF %>%
      group_by(transactionId, glAccountId) %>%
      summarise(credit = sum(credit), debit = sum(debit), .groups = "drop")
    combinedDF <- left_join(DF, GLAccounts %>% select(glAccountId, name, glAccountNumber, glType), by = "glAccountId")
    combinedDF
  } else {
    NULL
  }
}

getLocationIdDF <- function(user, password){

}

getLocatonIdByName <- function(name, user, password){

}
