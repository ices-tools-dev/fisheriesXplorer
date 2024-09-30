#'This function create an HTML output for the microsoft feedback form
#' 
#' @param null
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' make_contact_and_feedback()
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
make_contact_and_feedback <- function() {
  string_citation <- HTML(
    paste0(
      
      "<b>", "<font size=5>", "Contact & Feedback", "</font>", "</b>", "<br/>",
      "<font size=3>",
      "You can contact us via ", "<a href = ", "'mailto: neil.maginnis@ices.dk'", ">email</a>", "<br/>",
      "You can submit an issue to our GitHub ", "<a href='","https://github.com/ices-tools-dev/fisheriesXplorer/issues", "' target='_blank'>", "repository.","</a><br/>",
      "Please give us your feedback: ","<br/><br/><p align='center'><iframe width='820px' height='580px' src='https://forms.office.com/e/h8b1LTEN96' frameborder='0' marginwidth='0' marginheight='0' style='border: none; max-width:100%; max-height:100vh' allowfullscreen webkitallowfullscreen mozallowfullscreen msallowfullscreen> </iframe>",
      "</font>",  "<br/>"
    )
  )
  
  return(string_citation)
}