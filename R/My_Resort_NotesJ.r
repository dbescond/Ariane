#' resort code efficiently inside one columns in en, fr or sp language
#'
#' @param col_note reference columns code contains 0, 1 or multiple code like C2:158 | C3:485
#' @param SEP separator between multiple code, default " | "
#' @author ILO bescond  
#' @keywords ILO
#' @export
#' @examples
#' ################################# use to identify a proper	'DSD'		 
#'
#' X %>%	mutate(	Notes_Indicator_Code 	= plyr:::mapvalues(Notes_Indicator_Code,	from = levels(as.factor(Notes_Indicator_Code)), 
#'																				to = My_Resort_NotesJ(levels(as.factor(Notes_Indicator_Code)),SEP = "_"), warn_missing = FALSE))
#'


My_Resort_NotesJ <- function(col_note,SEP = " | "){						
	
									
	if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
		REF_NOTE_TYPE_SORT <- Ariane:::CODE_ORA$T_NTY_NOTE_TYPE %>% 
												select(NTY_CODE,NTY_SORT)
		REF_NOTE_SORT <- Ariane:::CODE_ORA$T_NTE_NOTE %>% 
												select(NTE_ID,NTE_SORT,NTE_TYPE_CODE) %>%
												mutate(	NTE_SORT = ifelse(NTE_SORT%in%".", 9999,NTE_SORT),
														NTE_ID = as.character(NTE_ID))
		if(SEP %in% " | "){
			col_note <- gsub(" | ","_",col_note, fixed = TRUE) 
			SEP <- "_"
			}
		aaa <- t(My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",1,ref=SEP))
		for (i in 1:ncol(aaa)){

				my.note.string <- aaa[rownames(aaa)[substr(rownames(aaa),1,4)%in%"PASS"],i]
				my_decompose <- My_unsplit_KEY(cbind(new = my.note.string, code = my.note.string) ,"code",1,ref=":")
				my_decompose <- as.data.frame(my_decompose[!my_decompose$code%in%NA,])

			if(!plyr:::empty(my_decompose)){

				my_decompose <- my_decompose %>% 
									left_join(select(REF_NOTE_SORT, 	PASS2 = NTE_ID, NTE_TYPE_CODE), by="PASS2") %>%  mutate(NTE_TYPE_CODE = ifelse(PASS1 %in% 'T30', 'T30', NTE_TYPE_CODE))  %>% select(-PASS1) %>% rename_("PASS1" = "NTE_TYPE_CODE") %>% mutate(PASS1 = ifelse(PASS1 %in% NA, 'T30', PASS1)) %>%
									left_join(select(REF_NOTE_TYPE_SORT,PASS1 = NTY_CODE, ID1 = NTY_SORT), by="PASS1")%>% 
									left_join(select(REF_NOTE_SORT,	PASS2 = NTE_ID, ID2 = NTE_SORT), by="PASS2") %>%
									mutate(	ID2 = ifelse(ID2 %in% NA, PASS2, ID2),
											ID1 = as.numeric(ID1),
											ID2 = as.numeric(ID2),
											PASS2 = as.numeric(PASS2)) %>%
									arrange(ID1,ID2,PASS2)
				aaa["new",i] <- paste(my_decompose$new, collapse="_")
			}
		}
		aaa <- as.data.frame(t(aaa))[,c("code","new")] %>% filter(!code%in%NA) %>% as.tbl
		REF <- 	as.tbl(as.data.frame(cbind(code = col_note, fack = col_note))) %>% 
						left_join(aaa, by = "code")

		as.character(REF$new)
	}
	else{
		NULL
	}
}


#' @export

My_Resort_Notes_Type <- function(col_note,SEP = " | ", check = TRUE, addsep = FALSE){						
	
									
	if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
		REF_NOTE_TYPE_SORT <- Ariane:::CODE_ORA$T_NTY_NOTE_TYPE %>% 
												select(NTY_CODE,NTY_SORT, NTY_GROUP_ID)
		REF_NOTE_SORT <- Ariane:::CODE_ORA$T_NTE_NOTE %>% 
												select(NTE_ID,NTE_SORT,NTE_TYPE_CODE) %>%
												mutate(	NTE_SORT = ifelse(NTE_SORT%in%".", 9999,NTE_SORT),
														NTE_ID = as.character(NTE_ID))
		if(SEP %in% " | "){
			col_note <- gsub(" | ","_",col_note, fixed = TRUE) 
			SEP <- "_"
			}
		aaa <- t(My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",1,ref=SEP))
		for (i in 1:ncol(aaa)){

				my.note.string <- aaa[rownames(aaa)[substr(rownames(aaa),1,4)%in%"PASS"],i]
				my_decompose <- My_unsplit_KEY(cbind(new = my.note.string, code = my.note.string) ,"code",1,ref=":")
				my_decompose <- as.data.frame(my_decompose[!my_decompose$code%in%NA,])

			if(!plyr:::empty(my_decompose)){

				my_decompose <- my_decompose %>% 
									left_join(select(REF_NOTE_SORT, 	PASS2 = NTE_ID, NTE_TYPE_CODE), by="PASS2") %>%  mutate(NTE_TYPE_CODE = ifelse(PASS1 %in% 'T30', 'T30', NTE_TYPE_CODE)) %>% select(-PASS1) %>% rename_("PASS1" = "NTE_TYPE_CODE") %>%
									left_join(select(REF_NOTE_TYPE_SORT,PASS1 = NTY_CODE, ID1 = NTY_SORT), by="PASS1")%>% 
									left_join(select(REF_NOTE_TYPE_SORT,PASS1 = NTY_CODE, ID3 = NTY_GROUP_ID), by="PASS1")%>% 
									left_join(select(REF_NOTE_SORT,		PASS2 = NTE_ID, ID2 = NTE_SORT), by="PASS2") %>%
									mutate(	ID2 = ifelse(ID2 %in% NA, PASS2, ID2),
											ID3 = as.numeric(ID3),
											ID1 = as.numeric(ID1),
											ID2 = as.numeric(ID2),
											PASS2 = as.numeric(PASS2)) %>%
									arrange(ID3, ID1, ID2,PASS2)
				my_decompose <- my_decompose %>% mutate(new = ifelse(!is.na(code), paste0(PASS1, ':', PASS2),new ))		
			if(check ){						
				if(length(unique(my_decompose$ID3)) > 1){
						MySourceNote = paste(my_decompose[my_decompose$ID3 %in% '296','new'], collapse="_")
						MyIndicatorNote = paste(my_decompose[my_decompose$ID3 %in% '297','new'], collapse="_")
						aaa["new",i] <- paste0(MySourceNote ,'|',MyIndicatorNote)
						
				}
				if(addsep ){
					if(length(unique(my_decompose$ID3)) == 1){
						if(unique(my_decompose$ID3) %in% '296'){
								aaa["new",i] <- paste0(paste(my_decompose$new, collapse="_"), '|')
						}
						if(unique(my_decompose$ID3) %in% '297'){
								aaa["new",i] <- paste0('|', paste(my_decompose$new, collapse="_"))
						}
					}
				} else {aaa["new",i] <- paste(my_decompose$new, collapse="_")}
				
				
				}
			else{
			MySourceNote = paste(my_decompose[my_decompose$ID3 %in% '296','new'], collapse="_")
						MyIndicatorNote = paste(my_decompose[my_decompose$ID3 %in% '297','new'], collapse="_")
						aaa["new",i] <- paste0(MySourceNote ,'|',MyIndicatorNote)
			}	
			}
		}
		aaa <- as.data.frame(t(aaa))[,c("code","new")] %>% filter(!code%in%NA) %>% as.tbl
		REF <- 	as.tbl(as.data.frame(cbind(code = col_note, fack = col_note))) %>% 
						left_join(aaa, by = "code")

		as.character(REF$new)
	}
	else{
		NULL
	}
}

