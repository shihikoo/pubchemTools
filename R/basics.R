
#' GetPugRestUrl
#' Get the url for Pug Rest APIs. For more info about the Pug Rest API: https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest
#'
#' @param searchTerm search term
#' @param searchDomain domain searched. e.g. name, cid, inchikey
#' @param returnType type of the return file. e.g. XML, json
#' @param returnContent content to return. e.g. "", "record", "cid", "description", "synonyms",
#' @param searcBasehUrl basic search url
#' @return searchUrl in string
#' @export
#'
#' @examples GetPugRestUrl(searchTerm = "amoxicillin")
#'
GetPugRestUrl <- function(searchTerm, searchDomain = "name", returnType = "json", returnContent = "description", searcBasehUrl = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/"){
  searchUrl <- paste0(searcBasehUrl, searchDomain,"/",searchTerm, "/", returnContent, "/",returnType)
  return(searchUrl)
}

#' GetContentWithLink
#'
#' @param link a string of characters
#' @param waitTime a number. Waiting of the program
#'
#' @return a string of characters of the returned content
#' @export
#'
#' @examples baselink <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
#' link <- paste0(baselink, "efetch.fcgi?db=pubmed&rettype=xml&id=26502666")
#' content <- GetContentWithLink(link, 0.3)
#'
#' @import httr
#'
GetContentWithLink <- function(link, waitTime = 0) {
  httr::set_config(httr::config(http_version = 0))
  content = NULL
  while (is.null(content)) {
    tryCatch({
      Sys.sleep(waitTime)
      r0 <- httr::POST(as.character(link))
      content <- httr::content(r0, "text", encoding = "UTF-8")
    }, error = function(e) {
      print(e)
    })
  }
  return(content)
}

