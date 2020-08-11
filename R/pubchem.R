#' GetPubRestDescriptionJsonWithName
#'
#' @param chemName chemcial name
#' @param waitTime wait time for each round
#'
#' @return result in json
#' @export
#'
#' @examples GetPubRestDescriptionJsonWithName(chemName = "amoxicillin")
#'
#' @import rjson
#'
GetPubRestDescriptionJsonWithName <- function(chemName, waitTime = 0){
  link <- GetPugRestUrl(chemName, searchDomain = "name", returnType="json", returnContent = "description")
  content <- GetContentWithLink(link, waitTime )
  jsonResult <- rjson::fromJSON(content)
  return(jsonResult)
}

#' GetPubRestSynonymsJsonWithCID
#'
#' @param cid pubchem compound id
#' @param waitTime wait time for each round
#'
#' @return result in json
#' @export
#'
#' @examples GetPubRestSynonymsJsonWithCID(cid = "33613")
#'
#' @import rjson
#'
GetPubRestSynonymsJsonWithCID <- function(cid, waitTime = 0){
  link <- GetPugRestUrl(cid, searchDomain = "cid", returnType="json", returnContent = "synonyms")
  content <- GetContentWithLink(link, waitTime )
  jsonResult <- rjson::fromJSON(content)
  return(jsonResult)
}

#' GetSynonymsWithCID
#' Retrive pubchem compound id with chemical name input
#'
#' @param cid pubchem compound id
#' @param waitTime wait time for each run
#'
#' @return chemName in string with cid as
#' @export
#'
#' @examples GetSynonymsWithCID(cid = "33613")
#'
GetSynonymsWithCID <- function(cid, waitTime = 0){
  jsonResult <- GetPubRestSynonymsJsonWithCID(cid, waitTime = 0)
  synonyms <- jsonResult$InformationList$Information[[1]]$Synonym
  return(synonyms)
}

#' GetCIDWithName
#' Retrive pubchem compound id with chemical name input
#'
#' @param chemName chemical name
#' @param waitTime wait time for each run
#'
#' @return chemName in string with cid as
#' @export
#'
#' @examples GetCIDWithName(chemName = "amoxicillin")
#'
GetCIDWithName <- function(chemName, waitTime = 0){
  jsonResult <- GetPubRestDescriptionJsonWithName(chemName,waitTime = 0)
  cid <- jsonResult$InformationList$Information[[1]]$CID
  chemName <- jsonResult$InformationList$Information[[1]]$Title
  names(cid) <-chemName
  return(cid)
}

