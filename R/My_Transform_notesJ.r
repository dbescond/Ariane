#' transform code efficiently inside one columns in en, fr or sp language used for csv upload format
#'
#' @param col_note reference columns code contains 0, 1 or multiple code like C2:158 | C3:485, will be transform as #485$NA_#485$NA 
#' @param SEP separator between multiple code, default " | "
#' @author ILO bescond  
#' @keywords ILO
#' @export
#' @examples
#' ################################# use to identify a proper	'DSD'		 
#'
#' X %>%	mutate(	Notes_Source_Code = 	plyr:::mapvalues(Notes_Source_Code,	from = levels(as.factor(Notes_Source_Code)), 
#'																			to = My_Transform_notesJ(levels(as.factor(Notes_Source_Code)),SEP = "_")))
#'


My_Transform_notesJ <- function(col_note,SEP = " | "){				

# col_note= levels(X$Qtable_Notes_String) ; SEP = "#"


	if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
		if(SEP %in% " | "){
			col_note <- gsub(" | ","_",col_note,fixed = TRUE) 
			SEP <- "_"
		}
		TEST <- My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",1,ref=SEP) %>% 
					select(-new) %>% 
					mutate(code = as.character(code))
		REF <- colnames(TEST)[substr(colnames(TEST),1,4)%in%"PASS"]
		for (i in 1:length(REF)){
			TEST[,REF[i]] <- paste0("#",stringr::str_split_fixed(TEST[,REF[i]], ":", 2)[,2],"$NA")
		}
		TEST <- TEST %>% 
				unite_("new", REF,sep ="_", remove = FALSE) %>% 
				select(code,new) %>%
				mutate(	new = gsub("_#$NA","",new, fixed=TRUE),
						new = gsub("#$NA",NA,new, fixed=TRUE),
						new = gsub("_","",new, fixed=TRUE))

		REF <- 	as.data.frame(cbind(code = col_note, fack = col_note), stringsAsFactors=FALSE) %>% 
					as.tbl %>%
					left_join(as.tbl(TEST), by = "code") %>% select(new)
		
		as.character(REF$new)
	}
	else{
		NULL
	}
}
