
#' GetPubchemCId
#' Retrive pubchem compound id with chemical name input
#'
#' @param chemName chemical name
#' @param waitTime wait time for each run
#'
#' @return cid in string
#' @export
#'
#' @examples GetPubchemCId(chemName = "amoxicillin")
#'
GetPubchemCId <- function(chemName, waitTime = 0){
  jsonResult <- GetPubRestJson(chemName, waitTime = 0)

  cid <- jsonResult$PC_Compounds[[1]]$id$id$cid

  return(cid)
}


#' GetSynonyms
#' Retrive all synonyms with input compound id
#'
#' @param cid compond id of pubchem database
#' @param apiKey optional parameter. ApiKey of the user.
#' @param email optional parameter. Email of the user.
#'
#' @return list of synonyms in strings
#' @export
#'
#' @examples GetSynonyms(cid = "33613")
#'
#' @import XML
#'
GetSynonyms <- function(cid, apiKey="", email=""){
  doc <- GetDoc( endpoint = "esummary", db = "pccompound", id = cid, apiKey = apiKey, email = email)

  results <- do.call(rbind, XML::xpathApply(doc, "//DocSum", function(x) {
    article <- XML::xmlDoc(x)
    title <- RetriveXmlNodeValuefromDoc(doc, "//Item[@Name='SynonymList']//Item")
    return(title)}))

  return(as.list(results))
}
