#' Prepare ilo  packages
#'
#' fix utf-8 encoding bug
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param X data frame.
#' @param DELETE in deletion format, default is FALSE.
#' @param LABEL add label to the notes, default is FALSE.
#' @author ILO bescond
#' @keywords ILO,  R
#' @export
#' @rdname prepare_ilo
prepare_segment_ilo <- function(FREQ, DBNAme, query, cou){

e <- new.env()


	for (i in 1:length(cou)){
		X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/',query, '_', cou[i],'_',FREQ, '.rds'))%>%
				mutate_all(funs(as.character))%>%
				mutate(obs_value = as.numeric(obs_value)) %>%
				select(-sex_version, -classif1_version,	-classif2_version)
		
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
	
		if(nrow(X)>0){	
	 
			X <- X  %>% 
				select(source, indicator, sex:obs_status, note_classif, note_indicator, note_source, info) 
					
			if(!FREQ %in% c('M','Q') ){
					X <- X %>% mutate(time = as.numeric(time))
				}
			invisible(gc(reset = TRUE))				
			assign(cou[i],X ,e)			
				
			print(cou[i])
			
		}			
		rm(X)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
			
	}			
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	getwd()
	tools:::makeLazyLoadDB(e, paste0(ilo:::path$sys, "Tmp/data/data/cou/", DBNAme)) 
	rm(e)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	aa <- file.size(paste0(ilo:::path$sys, "Tmp/data/data/cou/", DBNAme, '.rdb'))
	print(aa)
	if(aa == 0){
		file.remove(paste0(ilo:::path$sys, "Tmp/data/data/cou/", DBNAme, '.rdb'))
		file.remove(paste0(ilo:::path$sys, "Tmp/data/data/cou/", DBNAme, '.rdx'))
	} else{
		file.copy(from = paste0(ilo:::path$sys, "Tmp/data/data/cou/", DBNAme, '.rdb'), to = paste0(ilo:::path$tools, 'R/data/data/cou/', DBNAme, '.rdb'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
		file.copy(from = paste0(ilo:::path$sys, "Tmp/data/data/cou/", DBNAme, '.rdx'), to = paste0(ilo:::path$tools, 'R/data/data/cou/', DBNAme, '.rdx'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
	invisible(gc(reset = TRUE))
	}
	
}

#' @export

prepare_segment_ilo2 <- function(FREQ, DBNAme, query, cou){

e <- new.env()

DB <- NULL
	for (i in 1:length(cou)){
		X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/',query, '_', cou[i],'_',FREQ, '.rds'))%>%
				mutate_all(funs(as.character))%>%
				mutate(obs_value = as.numeric(obs_value)) %>%
				select(-sex_version, -classif1_version,	-classif2_version)
		
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
	
		if(nrow(X)>0){	
	 
			X <- X  %>% 
				select(ref_area, source, indicator, sex:obs_status, note_classif, note_indicator, note_source, info)
					
			if(!FREQ %in% c('M','Q') ){
					X <- X %>% mutate(time = as.numeric(time))
				}
			invisible(gc(reset = TRUE))				
			
			#	
				
			DB <- bind_rows(DB, X)
			
		}			
		rm(X)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
			
	}

	test <- DB %>% count(indicator) %>% left_join(Ariane:::CODE_ORA$T_IND_INDICATOR %>% select(indicator = IND_CODE, IND_SORT), by = 'indicator') %>% arrange(IND_SORT) %>% .$indicator
	
	for (i in 1:length(test)){
		X <- DB %>% filter(indicator %in% test[i]) %>% select(-indicator)
		 assign(test[i],X ,e)
		rm(X)
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
		DB <- DB %>% filter(!indicator %in% test[i])
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
		print(test[i])
	}
	rm(DB)



	
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	getwd()
	tools:::makeLazyLoadDB(e, paste0(ilo:::path$sys, "Tmp/data/data/ind/", DBNAme)) 
	rm(e)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	aa <- file.size(paste0(ilo:::path$sys, "Tmp/data/data/ind/", DBNAme, '.rdb'))
	print(aa)
	if(aa == 0){
		file.remove(paste0(ilo:::path$sys, "Tmp/data/data/ind/", DBNAme, '.rdb'))
		file.remove(paste0(ilo:::path$sys, "Tmp/data/data/ind/", DBNAme, '.rdx'))
	} else{
		file.copy(from = paste0(ilo:::path$sys, "Tmp/data/data/ind/", DBNAme, '.rdb'), to = paste0(ilo:::path$tools, 'R/data/data/ind/', DBNAme, '.rdb'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
		file.copy(from = paste0(ilo:::path$sys, "Tmp/data/data/ind/", DBNAme, '.rdx'), to = paste0(ilo:::path$tools, 'R/data/data/ind/', DBNAme, '.rdx'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
	invisible(gc(reset = TRUE))
	}
	
}


#' @export
prepare_codelist_ilo <- function(){
e <- new.env()


CODE <- Ariane:::CODE_ORA

code <- c("T_CLA_CLASSIF",
"T_DTS_DATASET",
"T_CIS_COL_IND_SRC",
"T_CIN_COL_IND",
"T_CIC_COL_IND_CLV",
"T_COL_COLLECTION", 
"T_CLT_CODELIST",
"T_CLV_CLASSIF_VERSION", 
"T_CLY_CLASSIF_TYPE", 
"T_COU_COUNTRY", 
"T_CUR_CURRENCY", 
"T_GRP_GROUP", 
"T_CBG_COUNTRY_BY_GROUP",
"T_IND_INDICATOR", 
"T_NTE_NOTE", 
"T_NTY_NOTE_TYPE", 
"T_SRC_SOURCE", 
"T_SUR_SURVEY", 
"T_TOP_TOPIC", 
"T_USR_USER",
"T_RVA_REPRESENTED_VARIABLE",
"T_SGR_SRC_GROUP", 
"T_SUI_SUBJECT_INDICATOR", 
"T_DCO_DATASET_COLLECTION", 
"T_SUB_SUBJECT", 
"T_NRD_NOTE_REMOVED_DISSEM"

)
CODE <- CODE[names(CODE) %in% code]

COU <- readxl::read_excel(paste0(ilo:::path$sys, 'ILO_Meta/REGION.xlsx'), sheet = 'Consolidation') %>% select(ilo_code, ISO3_CODE)

CODE$T_DCO_DATASET_COLLECTION <- CODE$T_DCO_DATASET_COLLECTION %>% 
										left_join(CODE$T_DTS_DATASET %>% select(DCO_DATASET_ID = DTS_ID, dataset = DTS_CODE)) %>%
										left_join(CODE$T_COL_COLLECTION %>% select(DCO_COLLECTION_ID = COL_ID, collection = COL_CODE)) %>% select(-contains('_ID'))



CL_DTS_DATASET <- CODE$T_DTS_DATASET %>% select(-contains("_ID")) %>%
					rename(	code = DTS_CODE, 
							label_en = DTS_TEXT_EN,
							label_fr = DTS_TEXT_FR,
							label_es = DTS_TEXT_SP,
							description_en = DTS_DESCR_EN, 
							description_fr = DTS_DESCR_FR, 
							description_es = DTS_DESCR_SP,
							sort = DTS_SORT	) %>%
					mutate(sort = as.numeric(sort), 
							code = str_sub(code, 1,2)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())

				
CODE$T_USR_USER <- CODE$T_USR_USER %>% select(USR_ID, USR_LASTNAME) %>%
					rename(	code  = USR_ID,
							label_en = USR_LASTNAME	) %>%
					mutate(sort = 1:n(), 
							label_en = tolower(paste0(str_sub(label_en, 1,1), str_sub(label_en, -2,-1), sep = ''))) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())
					
					
					
					
CODE$T_CLV_CLASSIF_VERSION <- CODE$T_CLV_CLASSIF_VERSION %>% select(-contains("_ID"), -contains("CLV_SHORT"), -CLV_CLASSIF_TYPE_CODE, -contains("CLV_DESCR"), -CLV_IS_EDITABLE, -CLV_IS_DISSEMINATED) %>% 
					mutate_all(funs(as.character) ) %>%
					mutate(CLV_SORT = as.numeric(CLV_SORT)) %>%
					rename(	code = CLV_CODE, 
							label_en = CLV_TEXT_EN,
							label_fr = CLV_TEXT_FR,
							label_es = CLV_TEXT_SP,
							sort = CLV_SORT	) %>%
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort)  %>%
					mutate(sort = 1:n())
				
				
				
CODE$T_CLA_CLASSIF <- CODE$T_CLA_CLASSIF %>% select(-contains("_ID"), -contains("CLA_LEVEL"), -CLA_DETAILS, -CLA_IS_DEFAULT, -CLA_IS_TOTAL ,-CLA_IS_VALIDATE ) %>% 
					mutate_all(funs(as.character) ) %>%
					mutate(CLA_SORT = as.numeric(CLA_SORT)) %>%
					rename(	code = CLA_CODE, 
							label_en_1 = CLA_TEXT_EN,
							label_fr_1 = CLA_TEXT_FR,
							label_es_1 = CLA_TEXT_SP,
							sort = CLA_SORT	) %>% 
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort) %>%
					left_join(select(CODE$T_CLV_CLASSIF_VERSION, 
										CLA_VERSION_CODE = code, 
										label_en_T = label_en, 
										label_fr_T = label_fr, 
										label_es_T = label_es), 
							by = 'CLA_VERSION_CODE') %>%
					unite(label_en,  label_en_T, label_en_1, sep=": ", remove = TRUE) %>%
					unite(label_fr,  label_fr_T,  label_fr_1, sep=": ", remove = TRUE) %>%
					unite(label_es,  label_es_T, label_es_1, sep=": ", remove = TRUE) %>%
					select(-CLA_VERSION_CODE) %>%
					mutate(sort = 1:n())


	
CODE$T_CLY_CLASSIF_TYPE <- CODE$T_CLY_CLASSIF_TYPE %>% select(-contains("_ID")) %>%
					mutate_all(funs(as.character) ) %>%
					mutate(CLY_SORT = as.numeric(CLY_SORT)) %>%
					rename(	code = CLY_CODE, 
							label_en = CLY_TEXT_EN,
							label_fr = CLY_TEXT_FR,
							label_es = CLY_TEXT_SP,
							description_en = CLY_DESCR_EN, 
							description_fr = CLY_DESCR_FR, 
							description_es = CLY_DESCR_SP, 
							sort =  CLY_SORT) %>% 
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())

CODE$T_COU_COUNTRY <- CODE$T_COU_COUNTRY %>% 
					select(-contains("_ID"), -COU_IS_ILOMEMBER, -COU_IS_RECEIVING_QUEST,  -COU_CALLING_CODE)%>% 
					mutate_all(funs(as.character)) %>% 	
					rename(	code = COU_ISO3_CODE,
							label_en = COU_TEXT_EN,
							label_fr = COU_TEXT_FR,
							label_es = COU_TEXT_SP, 
							sort =  COU_SORT_EN) %>% 
					mutate(sort = as.numeric(sort), 
							code_iso2 = COU_ISO2_CODE, 
							code_iso3n = as.numeric(as.character(COU_ISO3N_CODE))) %>%
					arrange(sort) %>% select(-COU_SORT_FR, -COU_SORT_SP, -COU_ONU_CODE, -COU_ISO3N_CODE, -COU_ISO2_CODE) %>%
					mutate(sort = 1:n()) %>% 
					left_join(COU %>% select(code = ISO3_CODE, ilo_code))

					
					
CODE$T_GRP_GROUP <- CODE$T_GRP_GROUP %>% select(-contains("_ID")) %>% 
					mutate_all(funs(as.character) ) %>% 
					rename(	code = GRP_CODE, 
							label_en = GRP_TEXT_EN, 
							label_fr = GRP_TEXT_FR,
							label_es = GRP_TEXT_SP
					) %>% mutate(sort = 1:n(), sort = as.numeric(sort))
				
CODE$T_CBG_COUNTRY_BY_GROUP <- CODE$T_CBG_COUNTRY_BY_GROUP %>% 
					select(-contains("_ID")) %>% 
					mutate_all(funs(as.character)) %>%
					rename(	code = CBG_GROUP_CODE, 
							code_country = CBG_COUNTRY_CODE) 

					
					
					
CL_COL_IND_SRC <- CODE$T_CIS_COL_IND_SRC %>% 
					left_join(CODE$T_CIN_COL_IND %>% 
								select(CIS_CIN_ID  = CIN_ID, CIN_COLLECTION_ID, CIN_INDICATOR_ID), by = "CIS_CIN_ID")  %>% 
					left_join(CODE$T_IND_INDICATOR %>% 
								select(CIN_INDICATOR_ID = IND_ID, IND_CODE), by = c("IND_CODE", "CIN_INDICATOR_ID")) %>% 
					left_join(CODE$T_COL_COLLECTION %>% 
								select(CIN_COLLECTION_ID  = COL_ID, COL_CODE ), by = c("COL_CODE", "CIN_COLLECTION_ID")) %>%
					left_join(CODE$T_SRC_SOURCE %>% 
								select(CIS_SOURCE_ID  = SRC_ID, SRC_CODE ), by = c("CIS_SOURCE_ID", "SRC_CODE")) %>% 
					select(contains('_CODE')) %>% 
					rename(indicator = IND_CODE, collection = COL_CODE, source =  SRC_CODE)

					
CL_COL_IND_CLV <- CODE$T_CIC_COL_IND_CLV %>% 
					select(contains('_CODE'), CIC_IS_DISSEM)  
colnames(CL_COL_IND_CLV) <- c('collection','indicator','classif_vs', 'dissem')				
				
				
CL_SUB_IND <- CODE$T_SUI_SUBJECT_INDICATOR %>%	
					left_join(CODE$T_IND_INDICATOR %>% 
								select(SUI_INDICATOR_ID = IND_ID, IND_CODE), by = c("SUI_INDICATOR_ID", "IND_CODE")) %>% 
					left_join(CODE$T_SUB_SUBJECT %>% 
								select(SUI_SUBJECT_ID  = SUB_ID, SUB_CODE ), by = c("SUI_SUBJECT_ID", "SUB_CODE")) %>% 
					select(contains('CODE'))	%>%
				rename(indicator = IND_CODE, subject = SUB_CODE)
							
					

CL_COL_IND <- CL_COL_IND_CLV %>% distinct(collection, indicator)					
					
					
				
CODE$T_IND_INDICATOR <- CODE$T_IND_INDICATOR %>% 
							select(-contains("_ID"), -contains("_LEVEL"), -IND_UNIT_MULT_CODE)%>% 
							mutate_all(funs(as.character) ) %>%
							mutate(IND_SORT = as.numeric(IND_SORT)) %>%
							select(-IND_IS_LABORSTA, -IND_INSTR_EN, -IND_INSTR_FR, -IND_INSTR_SP, -IND_HOT_INDICATOR_RANKING, -IND_HEADER_EN, -IND_HEADER_FR, -IND_HEADER_SP, -IND_UNIT_MEASURE_TYPE_CODE) %>%
					rename(	code = IND_CODE, 
							label_en = IND_TEXT_EN,
							label_fr = IND_TEXT_FR,
							label_es = IND_TEXT_SP,
							description_en = IND_DESCR_EN, 
							description_fr = IND_DESCR_FR, 
							description_es = IND_DESCR_SP,
							decimal = IND_DECIMAL, 
							code_Measure = IND_UNIT_MEASURE_CODE, 
							sort =  IND_SORT) %>% 
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())

CODE$T_TOP_TOPIC <- CODE$T_TOP_TOPIC %>% select(-contains("_ID"), -contains("_INSTR_")) %>% 
					mutate_all(funs(as.character) )  %>%
					mutate(TOP_SORT = as.numeric(TOP_SORT)) %>%
					rename(	code = TOP_CODE, 
							label_en = TOP_TEXT_EN,
							label_fr = TOP_TEXT_FR,
							label_es = TOP_TEXT_SP,
							description_en = TOP_DESCR_EN, 
							description_fr = TOP_DESCR_FR, 
							description_es = TOP_DESCR_SP, 
							sort =  TOP_SORT) %>% 
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())
					
					
					
CODE$T_RVA_REPRESENTED_VARIABLE <- CODE$T_RVA_REPRESENTED_VARIABLE %>% 
					select(-contains("_ID")) %>% 
					mutate_all(funs(as.character) ) %>%
					mutate(RVA_SORT = as.numeric(RVA_SORT)) %>%
					rename(	code = RVA_CODE, 
							label_en = RVA_TEXT_EN,
							label_fr = RVA_TEXT_FR,
							label_es = RVA_TEXT_SP,
							description_en = RVA_DESCR_EN, 
							description_fr = RVA_DESCR_FR, 
							description_es = RVA_DESCR_SP, 
							sort =  RVA_SORT) %>% 
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort)  %>%
					mutate(sort = 1:n())

					
CL_INDICATOR_UNIT <- 	CODE$T_CLT_CODELIST %>% 
					filter(CLT_COLUMN_NAME %in% "UNIT_MEASURE") %>% 
					select(-CLT_ID, -CLT_COLUMN_NAME) %>%
					rename(	code = CLT_COLUMN_CODE, 
							label_en = CLT_TEXT_EN,
							label_fr = CLT_TEXT_FR,
							label_es = CLT_TEXT_SP,
							sort = CLT_SORT	)%>%
					mutate( sort = as.numeric(sort)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())

					
CL_INDICATOR_TYPE <- 	CODE$T_CLT_CODELIST %>% 
					filter(CLT_COLUMN_NAME %in% "UNIT_MEASURE_TYPE") %>% 
					select(-CLT_ID, -CLT_COLUMN_NAME) %>%
					rename(	code = CLT_COLUMN_CODE, 
							label_en = CLT_TEXT_EN,
							label_fr = CLT_TEXT_FR,
							label_es = CLT_TEXT_SP,
							sort = CLT_SORT	)%>%
					mutate( sort = as.numeric(sort)) %>%
					arrange(sort)  %>%
					mutate(sort = 1:n())
							
							
							
CL_INDICATOR_COLLECTION <- 	CODE$T_COL_COLLECTION %>%
					rename(	code = COL_CODE, 
							label_en = COL_TEXT_EN,
							label_fr = COL_TEXT_FR,
							label_es = COL_TEXT_SP)%>%
					mutate(sort = 1:n(), sort = as.numeric(sort)) %>%
					arrange(sort)  %>% 
					select(code, label_en, label_fr, label_es, sort) %>%
					mutate(sort = 1:n())

CODE$T_SRC_SOURCE <- CODE$T_SRC_SOURCE %>% 
					select(-contains("_ID"), -contains("_DESCR_"), -SRC_GROUP_CODE) %>% 
					mutate_all(funs(as.character) ) %>%
					rename(	code = SRC_CODE, 
							label_en = SRC_TEXT_EN,
							label_fr = SRC_TEXT_FR,
							label_es = SRC_TEXT_SP,
							acronym_en = SRC_ACRONYM_EN, 
							acronym_fr = SRC_ACRONYM_FR, 
							acronym_es = SRC_ACRONYM_SP ) %>% 
					mutate(sort = as.numeric(SRC_SORT)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n()) %>%
					select(-SRC_SORT)

					
CODE$T_SUR_SURVEY <- CODE$T_SUR_SURVEY %>% 
					mutate(	SUR_SURVEY_TITLE_EN = ifelse(SUR_SURVEY_TITLE_EN %in% NA, SUR_ORIGINAL_SURVEY_TITLE, SUR_SURVEY_TITLE_EN ), 
							SUR_SURVEY_TITLE_FR = ifelse(SUR_SURVEY_TITLE_FR %in% NA, SUR_ORIGINAL_SURVEY_TITLE, SUR_SURVEY_TITLE_FR ), 
							SUR_SURVEY_TITLE_SP = ifelse(SUR_SURVEY_TITLE_SP %in% NA, SUR_ORIGINAL_SURVEY_TITLE, SUR_SURVEY_TITLE_SP )) %>% 
					rename( code = SUR_ID, 
							label_en = SUR_SURVEY_TITLE_EN, 
							label_fr = SUR_SURVEY_TITLE_SP, 
							label_es = SUR_SURVEY_TITLE_FR, 
							sort =  SUR_SORT, 
							code_Geo = SUR_COUNTRY_CODE, 
							code_Source = SUR_SOURCE_CODE) %>% 
					mutate(sort = as.numeric(sort)) %>%
					select(-SUR_COUNTRY_ID,  -SUR_SOURCE_ID, -SUR_ACRONYM) %>%
					arrange(code_Geo, sort) %>% 
					select(-SUR_ORIGINAL_SURVEY_TITLE) %>% 
					left_join(select(CODE$T_SRC_SOURCE, code_Source = code,acronym_en, acronym_fr, acronym_es ), by = 'code_Source') %>%
					mutate( label_en = str_c(code_Geo,acronym_en, label_en, sep = ' - '), 
							label_fr = str_c(code_Geo,acronym_fr, label_fr, sep = ' - '), 
							label_es = str_c(code_Geo,acronym_es, label_es, sep = ' - '),
							code = str_c(code_Source, code, sep = ':')
							) %>% 
					select(-acronym_en, -acronym_fr, -acronym_es, -code_Source, -code_Geo) %>%
					mutate(sort = 1:n())
					
					

					
					
CODE$T_SGR_SRC_GROUP <- CODE$T_SGR_SRC_GROUP  %>% 
					mutate_all(funs(as.character)) %>% 
					select(-SGR_ID) %>%
					rename(	code = SGR_CODE, 
							label_en = SGR_TEXT_EN,
							label_fr = SGR_TEXT_FR,
							label_es = SGR_TEXT_SP,
							sort = SGR_SORT	) %>%
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())
	

CODE$T_CUR_CURRENCY <- CODE$T_CUR_CURRENCY %>% 
					mutate(code_Type = "T30") %>%
					unite(code1, CUR_ID, sep= "", remove = TRUE) %>% 
					unite(code, code_Type, code1, sep= ":", remove = TRUE) %>% 
					select(-contains("_ID"), -CUR_END_DATE, -CUR_IS_CURRENT, -CUR_SYMBOL) %>% 
					rename(	label_en = CUR_TEXT_EN,
							label_fr = CUR_TEXT_FR, 
							label_es = CUR_TEXT_SP, 
							code_Geo = CUR_COUNTRY_CODE
					) %>%
					mutate(	label_en = paste0("Local currency: ",code_Geo, ' - ',label_en, '(', CUR_CODE, ')'), 
							label_fr = paste0("Monnaie local: ",code_Geo, ' - ',label_fr, '(', CUR_CODE, ')'), 
							label_es = paste0("Moneda local: ",code_Geo, ' - ',label_es, '(', CUR_CODE, ')')
					) %>% 
					mutate(sort = 1:n(), sort = as.numeric(sort)) 

CODE$T_NTY_NOTE_TYPE <- CODE$T_NTY_NOTE_TYPE %>% rename(type = NTY_GROUP_ID) %>% 
					select(-contains("_ID")) %>% 
					mutate_all(funs(as.character) ) %>%
					rename(	code = NTY_CODE, 
							label_en = NTY_TEXT_EN,
							label_fr = NTY_TEXT_FR,
							label_es = NTY_TEXT_SP,
							sort = NTY_SORT	) %>% 
					mutate(sort = as.numeric(sort) ) %>%
					arrange(sort)
					
					
					
pass <- CODE$T_NTY_NOTE_TYPE %>% slice(1) %>% 
					mutate(	code = "I0", 
							label_en = "Local currency", 
							label_fr = "Monnaie local", 
							label_es = "Moneda local", 
							sort = 0, 
							type = "297")
							
							
CODE$T_NTY_NOTE_TYPE <- bind_rows(pass, CODE$T_NTY_NOTE_TYPE) %>% arrange(sort) %>%
					mutate(sort = 1:n())%>% 
					mutate(type = type %>% plyr:::mapvalues(from= c("296","297","298"), to = c('src','ind','cla'))) 
						

						
CL_FLAG <- 	CODE$T_CLT_CODELIST %>% filter(CLT_COLUMN_NAME %in% "VALUE_STATUS") %>% 
					select(-CLT_ID, -CLT_COLUMN_NAME) %>%
					rename(	code = CLT_COLUMN_CODE, 
							label_en = CLT_TEXT_EN,
							label_fr = CLT_TEXT_FR,
							label_es = CLT_TEXT_SP,
							sort = CLT_SORT	)%>%
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort)	%>%
					mutate(sort = 1:n())			
									
									
									
CODE$T_NTE_NOTE <- CODE$T_NTE_NOTE %>% 
					rename(NTE_CODE = NTE_ID) %>% 
					select(-contains("_ID"), -NTE_IS_GENERAL, -NTE_IS_VALIDATE, -contains("NTE_DESCR"))%>% 
					mutate_all(funs(as.character) ) %>%
					rename(	sort = NTE_SORT	) %>%
					mutate( sort = as.numeric(sort)) %>%
					left_join(select(CODE$T_NTY_NOTE_TYPE, 
										NTE_TYPE_CODE = code, 
										label_en_T = label_en, 
										label_fr_T = label_fr, 
										label_es_T = label_es, 
										sort_T = sort), 
							by = 'NTE_TYPE_CODE') %>%
					arrange(sort_T, sort) %>% 
					unite(code,  NTE_TYPE_CODE, NTE_CODE, sep=":", remove = TRUE) %>%
					unite(label_en,  label_en_T, NTE_TEXT_EN, sep=": ", remove = TRUE) %>%
					unite(label_fr,  label_fr_T,  NTE_TEXT_FR, sep=": ", remove = TRUE) %>%
					unite(label_es,  label_es_T, NTE_TEXT_SP, sep=": ", remove = TRUE) %>% 
					select(-sort_T) %>% bind_rows(select(CODE$T_CUR_CURRENCY, -code_Geo),CL_FLAG ) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())

CL_FREQ <- 	CODE$T_CLT_CODELIST %>% 
					filter(CLT_COLUMN_NAME %in% "FREQ", CLT_COLUMN_CODE %in% c('A','Q','M')) %>% 
					select(-CLT_ID, -CLT_COLUMN_NAME) %>%
					rename(	code = CLT_COLUMN_CODE, 
							label_en = CLT_TEXT_EN,
							label_fr = CLT_TEXT_FR,
							label_es = CLT_TEXT_SP,
							sort = CLT_SORT	)%>%
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort) %>%
					mutate(sort = 1:n())


CL_LANG <- 	CODE$T_CLT_CODELIST %>% 
					filter(CLT_COLUMN_NAME %in% "FREQ" ) %>% 
					select(-CLT_ID, -CLT_COLUMN_NAME) %>%
					rename(	code = CLT_COLUMN_CODE, 
							label_en = CLT_TEXT_EN,
							label_fr = CLT_TEXT_FR,
							label_es = CLT_TEXT_SP,
							sort = CLT_SORT	)%>%
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort)  %>% 
					mutate( code = tolower(code),
							code = ifelse(code %in% "sp", "es", code)) 


CL_ZZZ <- 	CODE$T_CLT_CODELIST %>% 
					filter(CLT_COLUMN_NAME %in% c('DATA_CHANNEL', 'WORKFLOW_STATUS', 'LOAD_MODE') ) %>% 
					select(-CLT_COLUMN_NAME, -CLT_COLUMN_CODE) %>%
					rename(	code = CLT_ID, 
							label_en = CLT_TEXT_EN,
							label_fr = CLT_TEXT_FR,
							label_es = CLT_TEXT_SP,
							sort = CLT_SORT	)%>% 
					mutate(sort = as.numeric(sort)) %>%
					arrange(sort)  %>% 
					mutate( code = tolower(code),
							code = ifelse(code %in% "sp", "es", code)) %>% 
					bind_rows(CODE$T_USR_USER) 	
							
CODE$T_SUB_SUBJECT <- 	CODE$T_SUB_SUBJECT %>% 
							select(-contains('_ID'))	%>% 
							rename(code = SUB_CODE,label_en = SUB_TEXT_EN,
							label_fr = SUB_TEXT_FR,
							label_es = SUB_TEXT_SP )


CODE$T_NRD_NOTE_REMOVED_DISSEM <- CODE$T_NRD_NOTE_REMOVED_DISSEM %>% 
						left_join(select(CODE$T_NTY_NOTE_TYPE, NTE_TYPE_CODE = code, type), by = "NTE_TYPE_CODE") %>%
						mutate(note = paste0(NTE_TYPE_CODE, ':', NRD_NOTE_ID)) %>% 
						rename(indicator = IND_CODE) %>% 
						select(-contains('_ID'), -NTE_TYPE_CODE) 
							
					
assign("cl_classif",CODE$T_CLA_CLASSIF,e)
assign("cl_dataset",CL_DTS_DATASET,e)
assign("cl_classif_type",CODE$T_CLY_CLASSIF_TYPE,e)
assign("cl_classif_version",CODE$T_CLV_CLASSIF_VERSION,e)
assign("cl_country",CODE$T_COU_COUNTRY,e)
assign("cl_country_group",CODE$T_GRP_GROUP,e)
assign("cl_country_by_group",CODE$T_CBG_COUNTRY_BY_GROUP,e)
assign("cl_indicator",CODE$T_IND_INDICATOR,e)
assign("cl_indicator_topic",CODE$T_TOP_TOPIC,e)
assign("cl_indicator_variable",CODE$T_RVA_REPRESENTED_VARIABLE,e)
assign("cl_indicator_unit",CL_INDICATOR_UNIT,e)
assign("cl_indicator_type",CL_INDICATOR_TYPE,e)
assign("cl_indicator_collection",CL_INDICATOR_COLLECTION,e)

assign("cl_survey",CODE$T_SUR_SURVEY,e)
assign("cl_source",CODE$T_SRC_SOURCE,e)
assign("cl_source_group",CODE$T_SGR_SRC_GROUP,e)
assign("cl_dts_col",CODE$T_DCO_DATASET_COLLECTION,e)
assign("cl_col_ind",CL_COL_IND,e)
assign("cl_col_ind_clv",CL_COL_IND_CLV,e)
assign("cl_col_ind_scr",CL_COL_IND_SRC,e)
assign("cl_sub_ind",CL_SUB_IND,e)
assign("cl_subject",CODE$T_SUB_SUBJECT,e)

assign("cl_note_currency",CODE$T_CUR_CURRENCY,e)
assign("cl_note",CODE$T_NTE_NOTE,e)
assign("cl_note_type",CODE$T_NTY_NOTE_TYPE,e)
assign("cl_note_delete",CODE$T_NRD_NOTE_REMOVED_DISSEM,e)

assign("cl_freq",CL_FREQ,e)
assign("cl_lang",CL_LANG,e)
assign("cl_status",CL_FLAG,e)
assign("cl_z",CL_ZZZ,e)





tools:::makeLazyLoadDB(e, paste0(ilo:::path$sys, 'Tmp/data/settings/cl')) 

	rm(e)
	invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
file.copy(from = paste0(ilo:::path$sys, 'Tmp/data/settings/cl.rdb'), to = paste0(ilo:::path$tools, 'R/data/settings/cl.rdb'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
file.copy(from = paste0(ilo:::path$sys, 'Tmp/data/settings/cl.rdx'), to = paste0(ilo:::path$tools, 'R/data/settings/cl.rdx'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
	
}


#' @export
prepare_setting_ilo <- function(){ # repare settings 
e <- new.env()
LastUpdate <- Sys.time()
assign("lastupdate",LastUpdate,e)

seg <- c(list.files(paste0(ilo:::path$sys, 'Tmp/data/data/cou')) %>% .[str_detect(.,fixed('.rdb'))]) %>% str_sub(., 1, nchar(.)-4)


seg <- cbind(files = seg, size = file.size(paste0(ilo:::path$sys, 'Tmp/data/data/', seg, '.zip'))) %>% as.data.frame %>% as.tbl %>%
			mutate(	collection = str_split_fixed(files, fixed('_'), n = 2)[,1],
					freq = 	str_split_fixed(files, fixed('_'), n = 2)[,2],
					size = as.numeric(as.character(size))/1000) %>% 
					filter(!substr(collection,1,4) %in% "init") %>% 
					arrange(desc(size)) %>%
			mutate_if(is.character, funs(factor))
					


assign("segment",seg,e)

tools:::makeLazyLoadDB(e, paste0(ilo:::path$sys, 'Tmp/data/settings/set')) 
rm(e)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
file.copy(from = paste0(ilo:::path$sys, 'Tmp/data/settings/set.rdb'), to = paste0(ilo:::path$tools, 'R/data/settings/set.rdb'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
file.copy(from = paste0(ilo:::path$sys, 'Tmp/data/settings/set.rdx'), to = paste0(ilo:::path$tools, 'R/data/settings/set.rdx'), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

}
