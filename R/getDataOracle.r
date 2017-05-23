#' Query Data ILOSTAT
#'
#' Query data from ILOSTAT Oracle database
#'
#' Helper function to efficiently query data from ILOSTAT Oracle database.
#'
#' @param data_query SQL character string for select in table Data and Note, faster if split in 2: QTABLE and VALUE, check example.
#' @param extra to get extra administrative info, default equal FALSE
#' @param Connect	list of 4 characters string to initialiyse JDBC connection: list(
#' 											drv = JDBCDriver , a DBI driver connection, 
#' 											dns = 'jdbc_oracle_thin_client//my_localhost:my_port/my_service_name', 
#' 											user = 'my_user_name', 
#' 											pwd = 'my_password')
#' @param Codelist	list tables from the internal ilostat codelist
#' @author ILO bescond  
#' @keywords ILO, RJDBC, DBI

#' @examples
#' ################################## 
#'	
#' WHERE <- c(	QTABLE = "where Collection_Code in ('STI') and 
#'								Indicator_Code in ('EMP_TEMP_SEX_ECO_NB','EES_TEES_SEX_ECO_NB','HOW_TEMP_SEX_ECO_NB','HOW_XEES_SEX_ECO_NB') and 
#'								Classif1_Version_Code in ('ECO_SECTOR','ECO_AGGREGATE') and
#'								Year > 2001",
#'				VALUE 	= "			Classif1_Code in ('ECO_SECTOR_NAG','ECO_AGGREGATE_MAN') and 
#'								Sex_Code in ('SEX_T')")
#'	res <- getDataOracle(WHERE, Connect = initJ, Codelist = CODE_ORA) 
#'  head(res)					

#' @export
getDataOracle <- function(data_query, extra = FALSE, Save  = FALSE, mv = FALSE, con){
# data_query <- "where Country_Code in ('ABW')"; extra = TRUE; Save  = TRUE; mv = TRUE
# require(rJava)
#require(RJDBC)

data_query_note <- 	data_query[1]
data_query_data <- 	paste0(data_query, collapse = " and ")

ch 				<- 	dbConnect(drv = Oracle(), username = con[1], password = con[2], dbname = paste0(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", con[3], ")(PORT=", 1521, "))",
  "(CONNECT_DATA=(SERVICE_NAME=",con[4], ")))"))

  
DB_DATA			<- 	"ilostat.v_yi_data_internal_dissem"	 
DB_NOTE			<- 	"ilostat.v_yi_note_internal_dissem"    

DB_INFO   		<- 	"ilostat.T_QTA_QTABLE"

col_data_in 	<- 	paste(c("QTABLE_ID","VALUE_ID","COLLECTION_CODE","COUNTRY_CODE","SOURCE_CODE","SURVEY_ID","INDICATOR_CODE","TIME_FREQ","TIME","SEX_VERSION_CODE","CLASSIF1_VERSION_CODE","CLASSIF2_VERSION_CODE","CLASSIF3_VERSION_CODE","CLASSIF4_VERSION_CODE","CLASSIF5_VERSION_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE","CLASSIF3_CODE","CLASSIF4_CODE","CLASSIF5_CODE","VALUE","VALUE_STATUS_CODE","LAST_STATUS","LOAD_MODE"  ),collapse=", ")



# fetch data (X)
QUERY 		<- 	paste0("select "
					,col_data_in," from ",DB_DATA," " ,
					data_query_data
				)

QUERY 		<- 	gsub("\n"	," ", QUERY,fixed = TRUE)	
QUERY 		<- 	gsub("   "	," ", QUERY,fixed = TRUE)	
QUERY 		<- 	gsub("  "	," ", QUERY,fixed = TRUE)	
QUERY		<- 	toupper(QUERY)				


res 		<- 	dbSendQuery(ch,QUERY)
X 			<- 	as.tbl(fetch(res)) %>% mutate(WEB = 1 )
dbClearResult(res)
rm(res)


X <- X %>% mutate(WEB = ifelse(COLLECTION_CODE %in% 'STI' & substr(TIME,5,5) %in% '', 0, 1))
try(X <- X %>% mutate(WEB = ifelse(!INDICATOR_CODE %in% Ariane:::CODE_ORA$T_SUI_SUBJECT_INDICATOR$IND_CODE,0,WEB)),  silent = TRUE)
try(X <- X %>% mutate(WEB = ifelse(COLLECTION_CODE %in% c('KI','KIST','CP','CP2'),1,WEB)),  silent = TRUE)

try(X <- X %>% mutate(WEB = ifelse(!COLLECTION_CODE %in% Ariane:::CODE_ORA$T_DCO_DATASET_COLLECTION$COL_CODE,0,WEB)),  silent = TRUE)

try(X <- X %>% left_join(Ariane:::CODE_ORA$T_CIN_COL_IND %>% select(COLLECTION_CODE = COL_CODE, INDICATOR_CODE = IND_CODE) %>% mutate(keep = TRUE) , by = c('COLLECTION_CODE', 'INDICATOR_CODE')) %>% mutate(WEB = ifelse(!keep %in% TRUE,0, WEB )) %>% select(-keep),  silent = TRUE)
invisible(gc(reset = TRUE))

# TEST version on STI for quarterly and monthly data
TEST_vs <- Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% 
				filter(	CIC_COLLECTION_CODE %in% unique(Ariane:::CODE_ORA$T_DCO_DATASET_COLLECTION$COL_CODE), 
						CIC_INDICATOR_CODE %in% unique(Ariane:::CODE_ORA$T_SUI_SUBJECT_INDICATOR$IND_CODE), 
						CIC_IS_DISSEM %in% 'Y') 
test_version <- TEST_vs %>% mutate(TEST = paste0(CIC_COLLECTION_CODE, '/', CIC_INDICATOR_CODE, "/", CIC_CLASSIF_VERSION_CODE)) %>% select(TEST) %>% distinct(TEST) %>% t %>% c  
test_version <- c(test_version, paste(TEST_vs$CIC_COLLECTION_CODE, TEST_vs$CIC_INDICATOR_CODE, "NA", sep = "/")) %>% unique(.)
test_version <- c(test_version, paste(TEST_vs$CIC_COLLECTION_CODE, TEST_vs$CIC_INDICATOR_CODE, "NOC", sep = "/")) %>% unique(.)
rm(TEST_vs)



try(X <- 	X  %>% 	
			mutate(WEB = ifelse(!paste0(COLLECTION_CODE, '/', INDICATOR_CODE, "/", SEX_VERSION_CODE) %in% test_version, 0, WEB)) %>% 
			mutate(WEB = ifelse(!paste0(COLLECTION_CODE, '/', INDICATOR_CODE, "/", CLASSIF1_VERSION_CODE) %in% test_version, 0, WEB)) %>% 
			mutate(WEB = ifelse(!paste0(COLLECTION_CODE, '/', INDICATOR_CODE, "/", CLASSIF2_VERSION_CODE) %in% test_version, 0, WEB)) %>% 
			mutate(WEB = ifelse(!paste0(COLLECTION_CODE, '/', INDICATOR_CODE, "/", CLASSIF3_VERSION_CODE) %in% test_version, 0, WEB)) %>% 
			mutate(WEB = ifelse(!paste0(COLLECTION_CODE, '/', INDICATOR_CODE, "/", CLASSIF4_VERSION_CODE) %in% test_version, 0, WEB)) %>% 
			mutate(WEB = ifelse(!paste0(COLLECTION_CODE, '/', INDICATOR_CODE, "/", CLASSIF5_VERSION_CODE) %in% test_version, 0, WEB)),  silent = TRUE)
rm(test_version)				
invisible(gc(reset = TRUE))

TEST_src <- Ariane:::CODE_ORA$T_CIS_COL_IND_SRC %>% select(COL_CODE, IND_CODE, SRC_CODE) %>% 
			left_join(select(Ariane:::CODE_ORA$T_DCO_DATASET_COLLECTION, COL_CODE) %>% distinct(COL_CODE) %>% mutate(keep = TRUE), by = 'COL_CODE') %>% filter(keep %in% 'TRUE') %>% select(-keep) %>% 
			left_join(select(Ariane:::CODE_ORA$T_SUI_SUBJECT_INDICATOR, IND_CODE) %>% distinct(IND_CODE) %>% mutate(keep = TRUE), by = 'IND_CODE') %>% filter(keep %in% 'TRUE') %>% select(-keep) %>% 
			unite(test, COL_CODE, IND_CODE, SRC_CODE, sep = '/', remove = TRUE) %>% t %>% as.character %>% unique(.)


try(X <- 	X  %>% 	mutate(WEB = ifelse(!paste0(COLLECTION_CODE, '/', INDICATOR_CODE, "/", SOURCE_CODE) %in% TEST_src, 0, WEB)) ,  silent = TRUE)
rm(TEST_src)


invisible(gc(reset = TRUE))

# fetch notes at Qtable levels (Y)
QUERY 	<- 	paste0("select  
				qtable_id ,
				currency_id , 
				currency_code ,
				note_id , 
				note_type_code , 
				note_type_sort ,
				note_sort, 
				note_level_id		
				from ",DB_NOTE," " 
				,data_query_note, "
				and note_level_id  = 110 "
			)
QUERY 		<- 	gsub("\n"	," ", QUERY,fixed = TRUE)	
QUERY 		<- 	gsub("   "	," ", QUERY,fixed = TRUE)	
QUERY 		<- 	gsub("  "	," ", QUERY,fixed = TRUE)	
QUERY		<- 	toupper(QUERY)	
		

res 		<- 	dbSendQuery(ch,QUERY)
Y 	<- 	as.tbl(fetch(res))
dbClearResult(res)
rm(res)
invisible(gc(reset = TRUE))


Y <- Y %>% filter(QTABLE_ID %in% unique(X$QTABLE_ID)) 
		
REF 	<- Ariane:::CODE_ORA$T_NTY_NOTE_TYPE

Y <- Y %>%
		left_join(select(REF, NOTE_TYPE_CODE = NTY_CODE, NTY_GROUP_ID = NTY_GROUP_ID),by ="NOTE_TYPE_CODE")
		

# prepare currency 
if(!plyr:::empty(Y[!Y$CURRENCY_ID%in%NA & Y$NOTE_ID %in% NA,])){
Y <- Y %>% mutate(ID = 1:n())
	cur_tab 	<- 	Y %>%	
						filter(!CURRENCY_ID%in%NA & Y$NOTE_ID %in% NA) %>%
						mutate(CURRENCY_CODE = as.character(CURRENCY_ID)) %>% 
						select(QTABLE_ID,CURRENCY_CODE, ID )
					
	X 			<- 	X %>%
						left_join(select(cur_tab, QTABLE_ID = QTABLE_ID,CURRENCY_CODE = CURRENCY_CODE) %>% distinct(QTABLE_ID, .keep_all =  TRUE),by ="QTABLE_ID")
		
	Y 			<- 	Y %>%	
						filter(!ID %in% cur_tab$ID) %>% select(-ID)
	rm(cur_tab)
} else {
	X 			<- 	X %>% 
						mutate(CURRENCY_CODE = as.character(NA))
}
invisible(gc(reset = TRUE))
# prepare notes source
if(!plyr:::empty(Y[Y$NTY_GROUP_ID%in%c("296"),])){ 
	######### Source Notes 297 Indicator Notes, 298 Classification Notes
	src_tab 	<- 	Y %>% 
						filter(Y$NTY_GROUP_ID%in%c("296"))	%>% 
						arrange(QTABLE_ID, NOTE_TYPE_SORT, NOTE_SORT, NOTE_ID) %>% 
						mutate(NOTES_SOURCE_REF = paste0(NOTE_TYPE_CODE, ":",NOTE_ID )) %>%
						group_by(QTABLE_ID) %>%
						summarise(NOTES_SOURCE_CODE = paste(unique(NOTES_SOURCE_REF), collapse= "_")) %>% 
						ungroup() 
	X 			<- 	X %>%
						left_join(select(src_tab, QTABLE_ID = QTABLE_ID,NOTES_SOURCE_CODE = NOTES_SOURCE_CODE) %>% distinct(QTABLE_ID, .keep_all =  TRUE),by ="QTABLE_ID")
	Y 			<- 	Y %>%	
						filter(!Y$NTY_GROUP_ID%in%c("296"))
rm(src_tab)
} else {
	X 			<- 	X %>% 
						mutate(NOTES_SOURCE_CODE = as.character(NA)) 
}
invisible(gc(reset = TRUE))


# prepare notes indicator
if(!plyr:::empty(Y[Y$NTY_GROUP_ID%in%c("297"),])){          
	ind_tab 	<- 	Y %>% 
						filter(Y$NTY_GROUP_ID%in%c("297"))	%>% 
						arrange(QTABLE_ID, NOTE_TYPE_SORT, NOTE_SORT, NOTE_ID) %>% 
						group_by(QTABLE_ID) %>%
						mutate(NOTES_INDICATOR_CODE = paste0(NOTE_TYPE_CODE, ":",NOTE_ID )) %>%
						summarise(NOTES_INDICATOR_CODE = paste(unique(NOTES_INDICATOR_CODE), collapse= "_")) %>% 
						ungroup() 
	X 			<- 	X %>%
						left_join(select(ind_tab, QTABLE_ID = QTABLE_ID,NOTES_INDICATOR_CODE = NOTES_INDICATOR_CODE) %>% distinct(QTABLE_ID, .keep_all =  TRUE),by ="QTABLE_ID")
rm(ind_tab)		
} else {
	X 			<- 	X %>% 
						mutate(NOTES_INDICATOR_CODE = as.character(NA)) 
}
rm(Y)
invisible(gc(reset = TRUE))






# fetch notes at value levels (Y)
QUERY 	<- 	paste0("select
				value_id , 
				note_id ,
				note_type_code , 
				note_type_sort ,
				note_sort, 
				note_level_id 		
				from ",DB_NOTE," " 
				,data_query_data, " 
				and note_level_id  = 107 "
			)
QUERY 		<- 	gsub("\n"	," ", QUERY,fixed = TRUE)	
QUERY 		<- 	gsub("   "	," ", QUERY,fixed = TRUE)	
QUERY 		<- 	gsub("  "	," ", QUERY,fixed = TRUE)	
QUERY		<- 	toupper(QUERY)	
	
res 	<- 	dbSendQuery(ch,QUERY)
Y 		<- 	as.tbl(dbFetch(res))
dbClearResult(res)
rm(res)
invisible(gc(reset = TRUE))

Y <- Y %>% filter(VALUE_ID %in% unique(X$VALUE_ID)) 

Y <- 	Y %>% left_join(select(REF, NOTE_TYPE_CODE = NTY_CODE, NTY_GROUP_ID = NTY_GROUP_ID),by ="NOTE_TYPE_CODE")
rm(REF)



	
# prepare notes value
if(!plyr:::empty(Y[Y$NTY_GROUP_ID%in%c("298"),])){
	Y 			<- 	Y %>% 	
						arrange(VALUE_ID, NOTE_TYPE_SORT, NOTE_SORT, NOTE_ID) %>% 
						group_by(VALUE_ID) %>%
						mutate(NOTES_CLASSIF_CODE = paste0(NOTE_TYPE_CODE, ":",NOTE_ID )) %>%
						summarise(NOTES_CLASSIF_CODE = paste(unique(NOTES_CLASSIF_CODE), collapse= "_")) %>% 
						ungroup() 
	
	X 			<- 	X %>%
						left_join(select(Y, VALUE_ID = VALUE_ID,NOTES_CLASSIF_CODE = NOTES_CLASSIF_CODE),by ="VALUE_ID")

} else {
	X 			<- 	X %>% 
						mutate(NOTES_CLASSIF_CODE = as.character(NA))}
rm(Y,QUERY)
	
invisible(gc(reset = TRUE))
	

	
################ available QTA columns
# [1] "QTA_ID"                     "QTA_SERIE_ID"               "QTA_COLLECTION_ID"          "QTA_COUNTRY_ID"             "QTA_INDICATOR_ID"           "QTA_SURVEY_ID"              "QTA_YEAR"                   "QTA_SEX_VERSION_ID"        
# [9] "QTA_CLASSIF1_VERSION_ID"    "QTA_CLASSIF2_VERSION_ID"    "QTA_CLASSIF3_VERSION_ID"    "QTA_CLASSIF4_VERSION_ID"    "QTA_CLASSIF5_VERSION_ID"    "QTA_LAST_STATUS_ID"         "QTA_LAST_CHECK_STATUS_ID"   "QTA_LAST_CHECK_USER_ID"    
#[17] "QTA_LAST_CHECK_DATE"        "QTA_LAST_PUBLISHED_USER_ID" "QTA_LAST_PUBLISHED_DATE"    "QTA_NFT_IS_VALIDATE"        "QTA_CREATION_DATE"          "QTA_UPDATE_DATE"            "QTA_IS_LOCKED"              "QTA_DATA_CHANNEL_ID"       
#[25] "QTA_BATCH_NUMBER"           "QTA_LOAD_MODE_ID"           "QTA_TIME_ID"                "QTA_LAST_CHECK_FILE"        "QTA_CALCULATED_FROM"  	
	
	
if(extra){	
	# fetch info from Qtable table (Y)
	QUERY 	<- 	paste0("select distinct info.qta_id,  
					info.QTA_LAST_CHECK_STATUS_ID , 
					info.QTA_LAST_CHECK_USER_ID , 
					info.QTA_CREATION_DATE , 
					info.QTA_UPDATE_DATE ,
					info.QTA_LAST_CHECK_DATE , 
					info.QTA_LAST_PUBLISHED_DATE ,
					info.QTA_DATA_CHANNEL_ID ,
					info.QTA_BATCH_NUMBER 
					from ",DB_DATA," inner join ",DB_INFO," info  
					on info.qta_id = qtable_id " 
					,data_query_note
				)
	QUERY 		<- 	gsub("\n"	," ", QUERY,fixed = TRUE)	
	QUERY 		<- 	gsub("   "	," ", QUERY,fixed = TRUE)	
	QUERY 		<- 	gsub("  "	," ", QUERY,fixed = TRUE)	
	QUERY		<- 	toupper(QUERY)	
	
	
	res 	<- 	dbSendQuery(ch,QUERY)
	Y 		<- 	as.tbl(fetch(res))
	dbClearResult(res)
	rm(res)

	
	Y 	<- 	Y %>% 
				mutate(	QTA_CHECK_STATUS = as.character(QTA_LAST_CHECK_STATUS_ID), 
						QTA_CHECK_USER = as.character(QTA_LAST_CHECK_USER_ID),
						QTA_CHANNEL = as.character(QTA_DATA_CHANNEL_ID) ) %>%
				#left_join(select(Ariane:::CODE_ORA$T_CLT_CODELIST,QTA_LAST_CHECK_STATUS_ID = CLT_ID, QTA_CHECK_STATUS = CLT_TEXT_EN ), by = "QTA_LAST_CHECK_STATUS_ID") %>% 
				select(-QTA_LAST_CHECK_STATUS_ID) %>%
				#left_join(select(Ariane:::CODE_ORA$T_USR_USER,QTA_LAST_CHECK_USER_ID = USR_ID, QTA_CHECK_USER = USR_LASTNAME ), by = "QTA_LAST_CHECK_USER_ID") %>% 
				select(-QTA_LAST_CHECK_USER_ID) %>%
				#left_join(select(Ariane:::CODE_ORA$T_CLT_CODELIST,QTA_DATA_CHANNEL_ID = CLT_ID, QTA_CHANNEL = CLT_TEXT_EN ), by = "QTA_DATA_CHANNEL_ID") %>% 
				select(-QTA_DATA_CHANNEL_ID) %>%
				rename_( "QTABLE_ID" = "QTA_ID") %>% select(-QTA_BATCH_NUMBER, -QTA_CREATION_DATE, -QTA_UPDATE_DATE, -QTA_LAST_PUBLISHED_DATE) %>% 
				mutate(QTA_LAST_CHECK_DATE = substr(QTA_LAST_CHECK_DATE,1,10))

	if(!plyr:::empty(Y)){
		X 	<- 	X %>% 		
					left_join(Y, by="QTABLE_ID")
	}
	rm(Y)	
	invisible(gc(reset = TRUE))
}
	
# LAST_STATUS,"LOAD_MODE, QTA_CHECK_STATUS, QTA_CHECK_USER, QTA_CHANNEL
dbDisconnect(ch)
invisible(gc(reset = TRUE))

col_data_out <- c("COUNTRY_CODE","COLLECTION_CODE","INDICATOR_CODE","SOURCE_CODE","SURVEY_ID","SEX_VERSION_CODE","CLASSIF1_VERSION_CODE","CLASSIF2_VERSION_CODE","CLASSIF3_VERSION_CODE", "CLASSIF4_VERSION_CODE", "CLASSIF5_VERSION_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE","CLASSIF3_CODE","CLASSIF4_CODE","CLASSIF5_CODE","TIME_FREQ","TIME","VALUE","VALUE_STATUS_CODE","CURRENCY_CODE","NOTES_SOURCE_CODE","NOTES_INDICATOR_CODE","NOTES_CLASSIF_CODE")

if(extra){
	col_data_out <- c("COUNTRY_CODE","COLLECTION_CODE","INDICATOR_CODE","SOURCE_CODE","SURVEY_ID","SEX_VERSION_CODE","CLASSIF1_VERSION_CODE","CLASSIF2_VERSION_CODE","CLASSIF3_VERSION_CODE", "CLASSIF4_VERSION_CODE", "CLASSIF5_VERSION_CODE","SEX_CODE","CLASSIF1_CODE","CLASSIF2_CODE","CLASSIF3_CODE","CLASSIF4_CODE","CLASSIF5_CODE","TIME_FREQ","TIME","VALUE","VALUE_STATUS_CODE","CURRENCY_CODE","NOTES_SOURCE_CODE","NOTES_INDICATOR_CODE","NOTES_CLASSIF_CODE","LAST_STATUS","LOAD_MODE", "QTA_LAST_CHECK_DATE", "QTA_CHECK_STATUS", "QTA_CHECK_USER", "QTA_CHANNEL", "WEB")
}

X 	<- 	X %>% select_(.dots  = col_data_out)
rm(col_data_out)

colnames(X) <-  gsub('_' , ' ', tolower(colnames(X)), fixed = TRUE) %>% gsub('\\b(\\w)'	, '\\U\\1', ., perl=TRUE) %>%	 gsub(' ' , '_', ., fixed = TRUE)

X %>% 	mutate(Survey_Id = as.character(Survey_Id)) %>% 
		# mutate(Value = as.numeric(as.character(Value)))	%>% 	
		rename(Freq_Code = Time_Freq) %>% 	
		mutate(Currency_Code = ifelse(Currency_Code %in% NA, NA, paste0("T30:",Currency_Code)),
				Notes_Indicator_Code = ifelse(!Currency_Code%in%NA, paste0(Currency_Code, "_", Notes_Indicator_Code), Notes_Indicator_Code),
				Notes_Indicator_Code = Notes_Indicator_Code %>% str_replace_all('_NA', ''),
				Notes_Indicator_Code = ifelse(str_sub(Notes_Indicator_Code,-1,-1) %in% '_', str_sub(Notes_Indicator_Code,1,-2),Notes_Indicator_Code ) ) %>% 
		select(-Currency_Code) %>%
		mutate_if(is.character, as.factor)  %>%
		mutate(	Notes_Indicator_Code 	= plyr:::mapvalues(Notes_Indicator_Code,	from = levels(as.factor(Notes_Indicator_Code)), 
																			to = My_Resort_Notes_Type(levels(as.factor(Notes_Indicator_Code)),SEP = "_"), warn_missing = FALSE),
				Notes_Source_Code 		= plyr:::mapvalues(Notes_Source_Code,		from = levels(as.factor(Notes_Source_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Source_Code)),SEP = "_"), warn_missing = FALSE),
				Notes_Classif_Code 		= plyr:::mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_"), warn_missing = FALSE)) %>%
		sortDataOracle	%>% {invisible(gc(reset = TRUE)); if(Save) {saveRDS(.,file = paste0(ilo:::path$sys, 'ILO_Data/ON_ORACLE/',as.character(unique(.$Country_Code)),".rds"))} else .}	

		
}
