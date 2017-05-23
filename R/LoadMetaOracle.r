#' ilostat codelist definition  
#'

#' @author ILO bescond  
#' @keywords ILO
#' @export

LoadMetaOracle <- function(initJ, wd, wdAriane){
require(RJDBC)
	

################################################################################ 
################################################################################
################################################################################
################################################################################
################################################################################
# Oracle codelist table
################################################################################
################################################################################
################################################################################
################################################################################
################################################################## METADATA
# copy all meta (ORACLE) from N drive 
 
meta <- c(	"T_CBG_COUNTRY_BY_GROUP", 
			"T_CLA_CLASSIF", 
			"T_CLT_CODELIST", 
			"T_CLV_CLASSIF_VERSION", 
			"T_CLY_CLASSIF_TYPE", 
			"T_COL_COLLECTION", 
			"T_COU_COUNTRY", 
			"T_CUR_CURRENCY", 
			"T_GRP_GROUP", 
			"T_IND_INDICATOR", 
			"T_NTE_NOTE", 
			"T_NTY_NOTE_TYPE", 
			"T_SRC_SOURCE", 
			"T_SUR_SURVEY", 
			"T_TOP_TOPIC", 
			"T_SUB_SUBJECT", 
			"T_DTS_DATASET", 
			"T_RVA_REPRESENTED_VARIABLE", 
			"T_SUI_SUBJECT_INDICATOR", 
			"T_DCO_DATASET_COLLECTION", 
			"T_CIC_COL_IND_CLV", 
			"T_CIS_COL_IND_SRC", 
			"T_CIN_COL_IND", 
			"T_NBI_NOTETYPE_BY_IND", 
			"T_NBT_NOTETYPE_BY_TOP", 
			"T_NBS_NOTETYPE_BY_SRC", 
			"T_NDI_NOTE_DEFAULT_BY_IND", 
			"T_TIM_TIME", 
			#"T_SER_SERIE", 
			"T_NRD_NOTE_REMOVED_DISSEM") 
 
 
CODE_ORA <-list()
   
ch <- dbConnect(RJDBC::JDBC(initJ$driverClass,initJ$classPath)  , initJ$dns, initJ$user, initJ$pwd)
for (i in 1:length(meta)){

		QUERY <- paste0("select * from ilostat.",meta[i])
		res <- dbSendQuery(ch,QUERY)
		PASS <- as.tbl(dbFetch(res))
		dbClearResult(res); rm(res)
		write.csv(PASS,paste0(wd, "help/",meta[i], ".csv"),row.names = FALSE, na = "")

		CODE_ORA[[meta[i]]] <- as.tbl(PASS %>% 	
										mutate_all(funs(as.character) ) %>%
										mutate_all(funs(. %>% plyr:::mapvalues(c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))))										
										
	print(meta[i])
 }
 
dbDisconnect(ch)


CODE_ORA$T_USR_USER 		<- as.tbl(as.data.frame(read.csv(paste0(wd, "help/T_USR_USER.csv"),header = TRUE,sep=",", stringsAsFactors=FALSE))) %>% 
												mutate_all(funs(as.character) ) %>%
												mutate_all(funs(. %>% plyr::mapvalues(c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE)))
										

CODE_ORA$T_FRQ_FREQUENCY 	<- as.tbl(as.data.frame(read.csv(paste0(wd, "help/T_FRQ_FREQUENCY.csv"),header = TRUE,sep=",", stringsAsFactors=FALSE))) %>% 
											mutate_all(funs(as.character))
CODE_ORA$T_EAP_WEIGHT 		<- as.tbl(as.data.frame(read.csv(paste0(wd, "help/T_EAP_WEIGHT.csv"),header = TRUE,sep=",", stringsAsFactors=FALSE))) %>% 
											mutate_all(funs(as.character))

charsets 		<- as.tbl(as.data.frame(read.csv(paste0(wd, "help/charsets.csv"),header = TRUE,sep=",", stringsAsFactors=FALSE))) 
Encoding(charsets$Actual) <- "UTF-8"
Encoding(charsets$Expected) <- "UTF-8"
charsets <- charsets %>% mutate(nchar = nchar(Actual)) %>% 
						arrange(desc(nchar)) %>%
						filter(!Expected %in%c(NA,""), 
								!Actual %in%c(NA,"")) %>%
						select(Actual, Unicode,Expected ) %>%
						mutate(Unicode = paste0("<",Unicode,">"))

CODE_ORA$T_COU_COUNTRY <- CODE_ORA$T_COU_COUNTRY	%>% 
								left_join(readxl:::read_excel(paste0(wd, "REGION.xlsx"), sheet = 'Consolidation') %>% 
											select(COU_ISO3_CODE = ISO3_CODE,  COU_ISO3N_CODE  = iso3_NUM), 
								by = 'COU_ISO3_CODE')										


CODE_ORA$T_CBG_COUNTRY_BY_GROUP <- CODE_ORA$T_CBG_COUNTRY_BY_GROUP %>%
						left_join(select(CODE_ORA$T_GRP_GROUP, CBG_GROUP_ID = GRP_ID , CBG_GROUP_CODE = GRP_CODE ), by = "CBG_GROUP_ID") %>%
						left_join(select(CODE_ORA$T_COU_COUNTRY, CBG_COUNTRY_ID = COU_ID , CBG_COUNTRY_CODE = COU_ISO3_CODE ), by = "CBG_COUNTRY_ID")

CODE_ORA$T_CLA_CLASSIF <- CODE_ORA$T_CLA_CLASSIF %>%
						left_join(select(CODE_ORA$T_CLV_CLASSIF_VERSION, CLA_VERSION_ID = CLV_ID , CLA_VERSION_CODE = CLV_CODE ), by = "CLA_VERSION_ID")


CODE_ORA$T_CLV_CLASSIF_VERSION <- CODE_ORA$T_CLV_CLASSIF_VERSION %>%
						left_join(select(CODE_ORA$T_CLY_CLASSIF_TYPE, CLV_CLASSIF_TYPE_ID = CLY_ID , CLV_CLASSIF_TYPE_CODE = CLY_CODE ), by = "CLV_CLASSIF_TYPE_ID")


CODE_ORA$T_COL_COLLECTION <- CODE_ORA$T_COL_COLLECTION %>%
						left_join(select(CODE_ORA$T_CLT_CODELIST, COL_FREQ_ID = CLT_ID , COL_FREQ_CODE = CLT_COLUMN_CODE ), by = "COL_FREQ_ID")

											
CODE_ORA$T_CUR_CURRENCY <- CODE_ORA$T_CUR_CURRENCY %>% mutate(CUR_IS_CURRENT = ifelse(CUR_IS_CURRENT %in% 'Y', 0, 1))  %>%
						left_join(select(CODE_ORA$T_COU_COUNTRY, CUR_COUNTRY_ID = COU_ID , CUR_COUNTRY_CODE = COU_ISO3_CODE ), by = "CUR_COUNTRY_ID") %>% arrange(CUR_COUNTRY_CODE, CUR_IS_CURRENT) 

						
CODE_ORA$T_SUR_SURVEY <- CODE_ORA$T_SUR_SURVEY %>%
						left_join(select(CODE_ORA$T_COU_COUNTRY, SUR_COUNTRY_ID = COU_ID , SUR_COUNTRY_CODE = COU_ISO3_CODE ), by = "SUR_COUNTRY_ID") %>%
						left_join(select(CODE_ORA$T_SRC_SOURCE, SUR_SOURCE_ID = SRC_ID , SUR_SOURCE_CODE = SRC_CODE ), by = "SUR_SOURCE_ID") 
						
CODE_ORA$T_NTE_NOTE <- CODE_ORA$T_NTE_NOTE %>%
						left_join(select(CODE_ORA$T_NTY_NOTE_TYPE, NTE_TYPE_ID = NTY_ID , NTE_TYPE_CODE = NTY_CODE ), by = "NTE_TYPE_ID")

						
CODE_ORA$T_CIC_COL_IND_CLV <- CODE_ORA$T_CIC_COL_IND_CLV %>%
						left_join(select(CODE_ORA$T_CIN_COL_IND, CIC_COLLECTION_ID = CIN_COLLECTION_ID, CIC_INDICATOR_ID = CIN_INDICATOR_ID, CIC_CIN_ID  = CIN_ID), by ="CIC_CIN_ID") %>%
						left_join(select(CODE_ORA$T_COL_COLLECTION, CIC_COLLECTION_ID = COL_ID , CIC_COLLECTION_CODE = COL_CODE), by ="CIC_COLLECTION_ID") %>%
						left_join(select(CODE_ORA$T_IND_INDICATOR, CIC_INDICATOR_ID = IND_ID , CIC_INDICATOR_CODE = IND_CODE), by ="CIC_INDICATOR_ID") %>%
						left_join(select(CODE_ORA$T_CLV_CLASSIF_VERSION, CIC_CLASSIF_VERSION_ID = CLV_ID , CIC_CLASSIF_VERSION_CODE = CLV_CODE), by ="CIC_CLASSIF_VERSION_ID") 


CODE_ORA$T_IND_INDICATOR <- CODE_ORA$T_IND_INDICATOR %>%
						left_join(select(CODE_ORA$T_CLT_CODELIST, IND_UNIT_MEASURE_TYPE_ID = CLT_ID , IND_UNIT_MEASURE_TYPE_CODE = CLT_COLUMN_CODE ), by = "IND_UNIT_MEASURE_TYPE_ID") %>%
						left_join(select(CODE_ORA$T_CLT_CODELIST, IND_UNIT_MEASURE_ID = CLT_ID , IND_UNIT_MEASURE_CODE = CLT_COLUMN_CODE ), by = "IND_UNIT_MEASURE_ID") %>% 
						left_join(select(CODE_ORA$T_CLT_CODELIST, IND_UNIT_MULT_ID = CLT_ID , IND_UNIT_MULT_CODE = CLT_COLUMN_CODE ), by = "IND_UNIT_MULT_ID")
					
CODE_ORA$T_SGR_SRC_GROUP <- CODE_ORA$T_CLT_CODELIST %>% filter(CLT_COLUMN_NAME %in% "SOURCE_GROUP") %>% 
							select(-CLT_COLUMN_NAME) %>% rename(SGR_ID = CLT_ID,
																							SGR_CODE = CLT_COLUMN_CODE,
																																		SGR_TEXT_EN = CLT_TEXT_EN,
																																		SGR_TEXT_FR = CLT_TEXT_FR,
																																		SGR_TEXT_SP = CLT_TEXT_SP, 
																																		SGR_SORT = CLT_SORT)

CODE_ORA$T_SRC_SOURCE <- CODE_ORA$T_SRC_SOURCE %>% 
						left_join(select(CODE_ORA$T_SGR_SRC_GROUP, SRC_GROUP_ID = SGR_ID , SRC_GROUP_CODE = SGR_CODE ), by = "SRC_GROUP_ID") 		
																																							
CODE_ORA$T_CIN_COL_IND <- CODE_ORA$T_CIN_COL_IND %>% left_join(select(CODE_ORA$T_COL_COLLECTION, CIN_COLLECTION_ID =COL_ID, COL_CODE), by = 'CIN_COLLECTION_ID') %>% 
					left_join(select(CODE_ORA$T_IND_INDICATOR, CIN_INDICATOR_ID = IND_ID, IND_CODE), by = 'CIN_INDICATOR_ID')

																																		
																																		
CODE_ORA$T_DCO_DATASET_COLLECTION <- CODE_ORA$T_DCO_DATASET_COLLECTION %>% left_join(select(Ariane:::CODE_ORA$T_DTS_DATASET, DCO_DATASET_ID = DTS_ID, DTS_CODE), by = 'DCO_DATASET_ID') %>% 
									left_join(select(CODE_ORA$T_COL_COLLECTION, DCO_COLLECTION_ID =COL_ID, COL_CODE), by = 'DCO_COLLECTION_ID') %>%  
									add_row(DTS_CODE= 'FACK',  COL_CODE = 'CP' ) %>% 
									add_row(DTS_CODE= 'FACK',  COL_CODE = 'CP2' ) %>% 
									add_row(DTS_CODE= 'FACK',  COL_CODE = 'KI' ) %>% 
									add_row(DTS_CODE= 'FACK',  COL_CODE = 'KIST' )
											
											

CODE_ORA$T_CIS_COL_IND_SRC <- CODE_ORA$T_CIS_COL_IND_SRC %>% 
								left_join(select(CODE_ORA$T_CIN_COL_IND, CIS_CIN_ID = CIN_ID, COL_CODE, IND_CODE), by = c('CIS_CIN_ID')) %>%
								left_join(select(CODE_ORA$T_SRC_SOURCE, CIS_SOURCE_ID = SRC_ID, SRC_CODE), by = 'CIS_SOURCE_ID')



CODE_ORA$T_NRD_NOTE_REMOVED_DISSEM <- CODE_ORA$T_NRD_NOTE_REMOVED_DISSEM %>% left_join(select(CODE_ORA$T_NTE_NOTE, NRD_NOTE_ID = NTE_ID, NTE_TYPE_CODE), by = 'NRD_NOTE_ID')

CODE_ORA$T_NRD_NOTE_REMOVED_DISSEM <- CODE_ORA$T_NRD_NOTE_REMOVED_DISSEM %>% left_join(select(CODE_ORA$T_IND_INDICATOR, NRD_INDICATOR_ID = IND_ID, IND_CODE), by = 'NRD_INDICATOR_ID') 
					
CODE_ORA$T_SUI_SUBJECT_INDICATOR <- CODE_ORA$T_SUI_SUBJECT_INDICATOR %>% 
							left_join(select(CODE_ORA$T_IND_INDICATOR, SUI_INDICATOR_ID = IND_ID, IND_CODE), by = 'SUI_INDICATOR_ID') %>% 
							left_join(select(CODE_ORA$T_SUB_SUBJECT, SUI_SUBJECT_ID = SUB_ID, SUB_CODE), by = 'SUI_SUBJECT_ID')

	
save(CODE_ORA,file = paste0(wd, "CODE_ORA.rda"))
rm( meta)
 

################################################################################
# STI codelist COMPUTE

 
COMPUTE <- list()

COMPUTEREF <- readxl::read_excel(paste0(wd, "CODE_COMPUTE.xlsx"), sheet  ="COMPUTE", col_types = rep('text',9))

COMPUTE$CLASS_NB 				<- COMPUTEREF %>% filter(ID%in%"COMPUTE_CLASS_NB",COL_STI%in%1) %>% select(-contains("COL_"))
COMPUTE$INDICATOR_NB 			<- COMPUTEREF %>% filter(ID%in%"COMPUTE_INDICATOR_NB",COL_STI%in%1) %>% select(-contains("COL_"))
COMPUTE$SEX_NB 					<- COMPUTEREF %>% filter(ID%in%"COMPUTE_SEX_NB",COL_STI%in%1) %>% select(-contains("COL_"))
COMPUTE$QUARTER 				<- COMPUTEREF %>% filter(ID%in%"COMPUTE_QUARTER",COL_STI%in%1) %>% select(-contains("COL_"))
COMPUTE$YEAR 					<- COMPUTEREF %>% filter(ID%in%"COMPUTE_YEAR",COL_STI%in%1) %>% select(-contains("COL_"))
COMPUTE$INDICATOR_RT 			<- COMPUTEREF %>% filter(ID%in%"COMPUTE_INDICATOR_RT",COL_STI%in%1) %>% select(-contains("COL_"))
COMPUTE$INDICATOR_CLASS_RT 		<- COMPUTEREF %>% filter(ID%in%"COMPUTE_INDICATOR_CLASS_RT",COL_STI%in%1) %>% select(-contains("COL_"))

save(COMPUTE,file = paste0(wd, "COMPUTE.rda"))
rm( COMPUTEREF)

MAP <- read_csv(paste0(wd, "ref_map_plotly.csv")) %>% mutate_all(as.factor) %>% rename(country.label = COUNTRY, country = CODE)

save(CODE_ORA, COMPUTE, MAP,charsets,file = paste0(wdAriane,  "R/sysdata.rda"))

}