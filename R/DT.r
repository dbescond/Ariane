#' datatable custum for ilostat need
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param X dataframe to view.
#' @param apps, option when embed in an apps, default FALSE.
#' @author ILO bescond
#' @import DT
#' @export
 
DT <- function(X, apps = FALSE){

option <- "rownames = TRUE, 
						filter = 'top',		
						selection = 'single',
						extensions = c('ColReorder','ColVis','TableTools'), 
						options = 	list(
										stateSave = TRUE,
										dom = 'TRClfript',
										searchHighlight = TRUE,
										pageLength = 15, 
										autoWidth = TRUE,
										tableTools =list(
														aButtons = 	list('print')
														),
										lengthChange =TRUE
									), 
						class = c('compact'),
						escape = TRUE )"
if(apps %in% TRUE){
option <- "rownames = TRUE, 
						filter = 'top',		
						selection = 'single',
						extensions = c('ColReorder','ColVis','TableTools'), 
						options = 	list(
										stateSave = TRUE,
										dom = 'TRClfript', 
										searchHighlight = TRUE,
										pageLength = 15, 
										autoWidth = TRUE,
										tableTools =list(
														sSwfPath = copySWF('www'),
														aButtons = 	list('copy', 'print', 
																		list(
																			sExtends = 'collection',
																			sButtonText = 'Save',
																			aButtons = c('csv', 'xls')
																		)
																	)
														),
										lengthChange =TRUE
									), 
						class = c('compact', 'cell-border'),
						server = TRUE,
						escape = TRUE"
}						
						
eval(parse(text=paste(" DT::datatable(X,", option)))						
} 
 
 