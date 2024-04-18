#' @export
getTransactionsForLocationBetweenDF <- function(locationID, startDate, endDate, user, password) {
  #"https://odata.restaurant365.net/api/v2/views/Transaction?$filter=date%20ge%202024-02-26T00:00:00Z%20and%20date%20lt%202024-02-29T00:00:00Z"
  txEndpoint <- stringr::str_glue("https://odata.restaurant365.net/api/v2/views/Transaction?$filter=locationId%20eq%20{locationID}%20and%20date%20ge%20{startDate}T00:00:00Z%20and%20date%20lt%20{endDate}T23:59:59Z")
  print(txEndpoint)
  txDF <- getDFFromEndpoint(txEndpoint, user, password)
  #txDF <- txDF %>% filter (type != "Budget")
  detailDF <- getTransactionDetailsDF(txDF %>% select(transactionId), user, password)

  combinedDF <- left_join(detailDF, txDF, by = "transactionId")
  combinedDF
}
