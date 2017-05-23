#' Query Data ILOSTAT
#'
#' fix utf-8 encoding bug
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param Str a character vector with UTF-8 encoding errors.
#' @author ILO bescond
#' @keywords ILO, SDMX, R
#' @seealso \code{\link{getCodelist}} \code{\link{getDataStructure}}
#' @export
#' @examples
#' ################################# use to identify correct 'DSD'		 
#' test <- c("emquÃªte", "MÃ©nage")
#' recodeChar(test)	

recodeChar <- function(Str){
		Encoding(Str) <- "UTF-8"
		for (i in 1:nrow(Ariane:::charsets))	{
		Str <- gsub(Ariane:::charsets$Actual[i], Ariane:::charsets$Unicode[i], Str, fixed = TRUE)
		}
		for (i in 1:nrow(Ariane:::charsets))	{
		Str <- gsub(Ariane:::charsets$Unicode[i],Ariane:::charsets$Expected[i], Str, fixed = TRUE)
		}
	Str	
}