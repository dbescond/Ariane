#' ilostat codelist definition  
#'

#' @author ILO bescond  
#' @keywords ILO
#' @export

LoadDataST <- function(ReadMe, wd){

################################################################## DATA
#setwd("./COMMON/A0 Short term indicators") 



HEADER_TS <- t(readxl:::read_excel(paste0(wd, "ILO_Meta/CODE_HEADER.xlsx"), sheet  ="TS"))

colnames(HEADER_TS) <- HEADER_TS[1,]
HEADER_TS <- HEADER_TS[-1,]



Header_CL <- readxl:::read_excel(paste0(wd, "ILO_Meta/CODE_HEADER.xlsx"), sheet  ="CL")
new_col <- Header_CL

Header_CL <- Header_CL[!Header_CL$del%in%1,] 

# HEADER_CL <- as.data.frame(rbind(t(Header_CL[,2]),t(Header_CL[,2])),stringsAsFactors = FALSE)
HEADER_CL <- as.data.frame(t(Header_CL[,2]),stringsAsFactors = FALSE)

colnames(HEADER_CL) <- t(Header_CL[,1])
REF_CL <- Header_CL[Header_CL[,2]%in%"1",][,1]


colnames(HEADER_CL) <- gsub("_",    " ", tolower(colnames(HEADER_CL)), fixed = TRUE)
colnames(HEADER_CL) <- gsub("\\b(\\w)", "\\U\\1", colnames(HEADER_CL), perl=TRUE)
colnames(HEADER_CL) <- gsub(" ", "_", colnames(HEADER_CL), fixed = TRUE)

ReadMe$COUNTRY <- as.character(NA) 
ReadMe$SURVEY <- as.character(NA)





for (i in 1:length(ReadMe$PATH)){
	if(ReadMe$Types[i]%in%"TS" & str_sub(ReadMe$PATH[i],-4,-1)%in%c("xlsm","xlsx",".xls")){
		X <- readxl:::read_excel(ReadMe$PATH[i], sheet="Database", col_names =FALSE) 
		colnames(X) <- X[1,] 
		X <- as.tbl(X[-1,!colnames(X)%in%c("NA", NA)]) %>% 
				filter(!Country_Code %in%NA)
		colnames(X) <- gsub("_",    " ", tolower(colnames(X)), fixed = TRUE)
		colnames(X) <- gsub("\\b(\\w)", "\\U\\1", colnames(X), perl=TRUE)
		colnames(X) <- gsub(" ", "_", colnames(X), fixed = TRUE)
		X <- X[,colnames(X)%in%names(HEADER_TS)]
		X <- X[,names(HEADER_TS)%in%colnames(X)]

		X <- X %>% 	
				mutate_all(funs(mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))) %>% 
				reshapeDataLongFormatST
		X <- X[,colnames(X)%in%colnames(HEADER_CL)]
		colnames(X) <- new_col[match(colnames(X), new_col$Header), "NEW"] %>% t %>% c
		X <- X[,!colnames(X)%in%NA]

		# check duplicate over the various file

		X <- X  %>% 
				unite_("ID",c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code","Time","Freq_Code"), sep="/", remove = FALSE)
		TEST_DUPLICATE <- X %>% group_by(ID) %>% tally %>% ungroup %>% filter(n > 1)
		if(nrow(TEST_DUPLICATE)> 0){
			X <- distinct_(X,"ID")
			print(paste(ReadMe$PATH[i],"_       i         _ Duplicates, plse check result on CHECK_DUPLICATE",sep=" "))
			a <- unlist(str_split(ReadMe$PATH[i], "/"))[length(str_split(ReadMe$PATH[i], "/")[[1]])]		
			a <- substr(a, 1, nchar(a)-5)
			colnames(TEST_DUPLICATE)[1] <- "Country_Code/Source_Code/Indicator_Code/Sex_Code/Classif1_Code/Classif2_Code/Time/Freq_Code"
			data.table:::fwrite(TEST_DUPLICATE, paste0(mywd, 'ILO_Data/check/CHECK_DUPLICATE_INFILE_',a,'.csv'),na = '')
			rm(a)
		}
		X <- X 	%>% 
			select(-ID) %>% 
			mutate_all(funs(mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))) %>% 
			mutate(		Add_Status 	= as.character(ifelse(Add_Status %in% NA,"M",Add_Status)),
						Sex_Code	= ifelse(Sex_Code %in% c("T","F","M"),paste("SEX",Sex_Code,sep="_"),Sex_Code),
						Sex_Code    = as.character(gsub("_X_","_",Sex_Code,fixed = TRUE)),
						Classif1_Code = as.character(ifelse(Classif1_Code %in% NA,"XXX_XXX_XXX",Classif1_Code)),
						Classif2_Code = as.character(ifelse(Classif2_Code %in% NA,"XXX_XXX_XXX",Classif2_Code)),
						Value =as.numeric(Value), 
						Notes_Source_Code = ifelse(Notes_Source_Code %in% NA, 'R1:3902', paste0('R1:3902_', Notes_Source_Code))) %>% as.tbl

		ReadMe$COUNTRY[i] 	<- X 	%>% 
								summarise(test = paste(unique(Country_Code), collapse=";")) %>% 
								as.character
		ReadMe$SURVEY[i] 	<- X 	%>% 
								mutate(ID = paste(Country_Code, Source_Code, sep="/")) %>% 
								group_by(ID) %>% 
								summarise(count = n(), test = paste0(unique(paste(Country_Code, Source_Code, count,i, sep="/")))) %>% 
								summarise(test = paste(unique(test), collapse=";")) %>% 
								as.character
		save(X,file = paste0(wd, "ILO_Data/ON_STI_FILES/File",i,".Rdata"))
	}

	if (str_sub(ReadMe$PATH[i],-6,-1)%in%c(".Rdata") & ReadMe$Types[i] %in% 'CL'){
	

		load(ReadMe$PATH[i])
		colnames(X) <- gsub("_",    " ", tolower(colnames(X)), fixed = TRUE)
		colnames(X) <- gsub("\\b(\\w)", "\\U\\1", colnames(X), perl=TRUE)
		colnames(X) <- gsub(" ", "_", colnames(X), fixed = TRUE)
		colnames(X) <- new_col[match(colnames(X), new_col$Header), "NEW"] %>% t  %>% c
		X <- as.tbl(X[,!colnames(X)%in%NA])

		if(unique(str_sub(X$Time,1,1)%in%"Y")){
			X <- X %>% 
					mutate(	Time = paste0(str_sub(Time,2,5),str_sub(Time,7,9)),
							Time = gsub("Q0","Q",Time, fixed = TRUE))
		}

		X <- X %>% 	
				mutate_all(funs(as.character) ) %>%
				mutate_all(funs(mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))) #clean fack or bad value

		X <- bind_rows(HEADER_CL,as.tbl(X))[-c(1),]
		X <- X[,colnames(X)%in%colnames(HEADER_CL)]

		ReadMe$COUNTRY[i] 	<- X 	%>% 
								summarise(test = paste(unique(Country_Code), collapse=";")) %>% 
								as.character
		ReadMe$SURVEY[i] 	<- X 	%>% 
								mutate(ID = paste(Country_Code, Source_Code, sep="/")) %>% 
								group_by(ID) %>% 
								summarise(count = n(), test = paste0(unique(paste(Country_Code, Source_Code, count,i, sep="/")))) %>% 
								summarise(test = paste(unique(test), collapse=";")) %>% 
								as.character


		# filled the gaps in term of meta
		X <- X %>% 
				mutate(Indicator_Code = ifelse(nchar(Indicator_Code)>11, paste0(str_sub(Indicator_Code, 1,8), str_sub(Indicator_Code, -3,-1)), Indicator_Code),	
					Add_Status 	= as.character(ifelse(Add_Status %in% NA,"M",Add_Status)),
					Sex_Code	= ifelse(Sex_Code %in% c("T","F","M"),paste("SEX",Sex_Code,sep="_"),Sex_Code),
					Sex_Code    = as.character(gsub("_X_","_",Sex_Code,fixed = TRUE)),
					Classif1_Code = as.character(ifelse(Classif1_Code %in% NA,"XXX_XXX_XXX",Classif1_Code)),
					Classif2_Code = as.character(ifelse(Classif2_Code %in% NA,"XXX_XXX_XXX",Classif2_Code)),
					Value =as.numeric(Value))  %>% as.tbl

		save(X,file = paste0(wd, "ILO_Data/ON_STI_FILES/File",i,".Rdata"))
	}


	if (str_sub(ReadMe$PATH[i],-6,-1)%in%c(".Rdata") & str_detect(ReadMe$Types[i],'ilostat')){
	
		ref_add_repo <- str_split(ReadMe$Types[i], '_', simplify = TRUE)[1,1]
		load(ReadMe$PATH[i])

		try(X <- X %>% select(-Is_Validate), silent = TRUE)	
		X <- X %>% mutate(IND_CODE = paste0(str_sub(indicator, 1, 9), str_sub(indicator,-2,-1))) %>% left_join(
						Ariane:::CODE_ORA$T_CIN_COL_IND %>% filter(COL_CODE %in% 'STI') %>%  mutate(IND_CODE = paste0(str_sub(IND_CODE, 1, 9), str_sub(IND_CODE,-2,-1))) %>% distinct(IND_CODE) %>% mutate(check = 1)
						, by = 'IND_CODE') %>% 
					mutate(check = ifelse(indicator %in% c('EES_T9ES_NB','EMP_T9MP_NB','HOW_T9MP_NB','HOW_X9ES_NB'), 1,check )) %>%
					filter(check %in% 1) %>% select(-check)
		ReadMe$COUNTRY[i] 	<- X 	%>% 
								summarise(test = paste(unique(ref_area), collapse=";")) %>% 
								as.character
		ReadMe$SURVEY[i] 	<- X 	%>% 
								mutate(ID = paste(ref_area, source, sep="/")) %>% 
								group_by(ID) %>% 
								summarise(count = n(), test = paste0(unique(paste(ref_area, source, count,i, sep="/")))) %>% 
								summarise(test = paste(unique(test), collapse=";")) %>% 
								as.character


		X <- X %>%  # current STI compatible format
				rename(
					Country_Code = ref_area, 
					Source_Code = source, 
					Indicator_Code = indicator, 
					Sex_Code = sex, 
					Classif1_Code = classif1, 
					Classif2_Code = classif2,
					Time = time,
					Value = obs_value, 
					Value_Status_Code = obs_status, 
					Freq_Code = freq_code, 
					Notes_Classif_Code = note_classif, 
					Notes_Indicator_Code = note_indicator,					
					Notes_Source_Code = note_source	) %>% 
				select(-collection) %>% 
				mutate(	Currency_Code = as.character(NA), 
						Add_Repository = ref_add_repo,
						Add_Status = 'B') %>% 
				select_(.dots = colnames(HEADER_CL))	%>% 
				mutate(Indicator_Code = ifelse(substr(Indicator_Code, 17,17) %in% '2', paste0(str_sub(Indicator_Code, 1, 5), '9', str_sub(Indicator_Code, 7, -1)), Indicator_Code), 
					Indicator_Code = ifelse(nchar(Indicator_Code)>11, paste0(str_sub(Indicator_Code, 1,8), str_sub(Indicator_Code, -3,-1)), Indicator_Code),	
					Add_Status 	= as.character(ifelse(Add_Status %in% NA,"M",Add_Status)),
					Sex_Code	= ifelse(Sex_Code %in% c("T","F","M"),paste("SEX",Sex_Code,sep="_"),Sex_Code),
					Sex_Code    = as.character(gsub("_X_","_",Sex_Code,fixed = TRUE)),
					Classif1_Code = as.character(ifelse(Classif1_Code %in% NA,"XXX_XXX_XXX",Classif1_Code)),
					Classif2_Code = as.character(ifelse(Classif2_Code %in% NA,"XXX_XXX_XXX",Classif2_Code)),
					Value =as.numeric(Value)) %>% as.tbl

		save(X,file = paste0(wd, "ILO_Data/ON_STI_FILES/File",i,".Rdata"))
	}

	ReadMe$ID[i] <- i
 
print(paste0(i,"#",length(ReadMe$PATH),"#",nrow(X),"#", nrow(X %>% filter(as.numeric(str_sub(Time,1,4)) >2009)), '#',ReadMe$PATH[i]))
rm(X)
}



saveRDS(ReadMe %>% select(-SURVEY) %>% as.tbl,file = paste0(wd, "ILO_Data/ON_STI_FILES/ReadMe.rds"))



}