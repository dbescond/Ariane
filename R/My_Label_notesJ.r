#' map code to label at factor levels efficiently in en, fr or sp language
#'
#' @param col_note reference columns code contains 0, 1 or multiple code like C2:158 | C3:485
#' @param SEP separator between multiple code, default " | "
#' @param Lang language, default = en, fr, sp also available 
#' @author ILO bescond  
#' @keywords ILO

#' @examples
#' ################################# use to identify a proper	'DSD'		 
#'
#' X %>%	mutate(	Notes_Source_Label = 	mapvalues(Notes_Source_Code,	from = levels(as.factor(Notes_Source_Code)), 
#'																			to = My_Label_notesJ(levels(as.factor(Notes_Source_Code)),SEP = "_", Lang = Lang)))
#'
#' @export
My_Label_notesJ <- function(col_note,SEP = " | ", Lang = "en"){	
Lang <- toupper(Lang)

	if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
		if(SEP %in% " | "){
			col_note <- gsub(" | ","_",col_note, fixed = TRUE) ; SEP <- "_"
		}
		TEST <- My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",KEY=1,ref=SEP)
		REF <- colnames(TEST)[substr(colnames(TEST),1,4)%in%"PASS"]

		for (i in 1:length(REF)){
			TEST <- My_unsplit_KEY(TEST,REF[i],c("T1","T2"),ref=":") %>% 	select(-T1) %>%
							left_join(select(Ariane:::CODE_ORA$T_NTE_NOTE, 		T2 = NTE_ID, 	T1 = NTE_TYPE_CODE),	by ="T2") %>%
							left_join(select_(Ariane:::CODE_ORA$T_NTY_NOTE_TYPE, 	.dots = c(T1 = "NTY_CODE", 	T1_NEW = paste0("NTY_TEXT_",Lang))), 	by ="T1") %>% 
							left_join(select_(Ariane:::CODE_ORA$T_NTE_NOTE, .dots = c( T2 = "NTE_ID", 	T2_NEW = paste0("NTE_TEXT_",Lang))),	by ="T2")
		
			TEST <- eval(parse(text=paste0("TEST %>% mutate(",REF[i]," = ifelse((!T2_NEW%in%c('NA',NA) | !T1_NEW%in%c('NA',NA)),paste0(T1_NEW,': ',T2_NEW),",REF[i],"))"))) %>%
						select(-T1,-T2,-T1_NEW,-T2_NEW)

			ifelse(i%in%1,TEST$new <- TEST[,REF[i]], TEST$new <- paste0(TEST$new," | ",TEST[,REF[i]]))			
		}
		as.character(gsub(" | NA","",TEST$new,  fixed = TRUE))
	}
	else{
		NULL
	}
}


#' @export
My_Label_notesJ_NEW <- function(col_note,SEP = " | ", Lang = "en"){	
Lang <- toupper(Lang)

	if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
		if(SEP %in% " | "){
			col_note <- gsub(" | ","_",col_note, fixed = TRUE) ; SEP <- "_"
		}
		TEST <- My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",KEY=1,ref=SEP)
		REF <- colnames(TEST)[substr(colnames(TEST),1,4)%in%"PASS"]

		for (i in 1:length(REF)){
			TEST <- My_unsplit_KEY(TEST,REF[i],c("T1","T2"),ref=":") %>% 	select(-T1) %>%
							left_join(select(mutate_all(Ariane:::CODE_ORA$T_NTE_NOTE, funs(as.character)), 		T2 = NTE_ID, 	T1 = NTE_TYPE_CODE),	by ="T2") %>%
							left_join(select_(mutate_all(Ariane:::CODE_ORA$T_NTY_NOTE_TYPE, funs(as.character)), 	.dots = c(T1 = "NTY_CODE", 	T1_NEW = paste0("NTY_TEXT_",Lang))), 	by ="T1") %>% 
							left_join(select_(mutate_all(Ariane:::CODE_ORA$T_NTE_NOTE, funs(as.character)), .dots = c( T2 = "NTE_ID", 	T2_NEW = paste0("NTE_TEXT_",Lang))),	by ="T2")
		
			TEST <- eval(parse(text=paste0("TEST %>% mutate(",REF[i]," = ifelse((!T2_NEW%in%c('NA',NA) | !T1_NEW%in%c('NA',NA)),paste0(T1_NEW,': ',T2_NEW),",REF[i],"))"))) %>%
						select(-T1,-T2,-T1_NEW,-T2_NEW)

			ifelse(i%in%1,TEST$new <- TEST[,REF[i]], TEST$new <- paste0(TEST$new," | ",TEST[,REF[i]]))			
		}
		as.character(gsub(" | NA","",TEST$new,  fixed = TRUE))
	}
	else{
		NULL
	}
}

#' @export
My_Label_notesJ_OLD <- function(col_note,SEP = " | ", Lang = "en"){	
Lang <- toupper(Lang)

	if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
		if(SEP %in% " | "){
			col_note <- gsub(" | ","_",col_note, fixed = TRUE) ; SEP <- "_"
		}
		TEST <- My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",KEY=1,ref=SEP)
		REF <- colnames(TEST)[substr(colnames(TEST),1,4)%in%"PASS"]

		for (i in 1:length(REF)){
			TEST <- My_unsplit_KEY(TEST,REF[i],c("T1","T2"),ref=":") %>% 	select(-T1) %>%
							left_join(select(mutate_all(CODE_ORA$T_NTE_NOTE, funs(as.character)), 		T2 = NTE_ID, 	T1 = NTE_TYPE_CODE),	by ="T2") %>%
							left_join(select_(mutate_all(CODE_ORA$T_NTY_NOTE_TYPE, funs(as.character)), 	.dots = c(T1 = "NTY_CODE", 	T1_NEW = paste0("NTY_TEXT_",Lang))), 	by ="T1") %>% 
							left_join(select_(mutate_all(CODE_ORA$T_NTE_NOTE, funs(as.character)), .dots = c( T2 = "NTE_ID", 	T2_NEW = paste0("NTE_TEXT_",Lang))),	by ="T2")
		
			TEST <- eval(parse(text=paste0("TEST %>% mutate(",REF[i]," = ifelse((!T2_NEW%in%c('NA',NA) | !T1_NEW%in%c('NA',NA)),paste0(T1_NEW,': ',T2_NEW),",REF[i],"))"))) %>%
						select(-T1,-T2,-T1_NEW,-T2_NEW)

			ifelse(i%in%1,TEST$new <- TEST[,REF[i]], TEST$new <- paste0(TEST$new," | ",TEST[,REF[i]]))			
		}
		as.character(gsub(" | NA","",TEST$new,  fixed = TRUE))
	}
	else{
		NULL
	}
}
