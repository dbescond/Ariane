#' ilo short term data processing  
#'
#' @param Title character string of the country alpha iso code 3.
#' @param ReadMe file containing path of each ST original files.
#' @author ILO bescond  
#' @keywords ILO
#' @export

XQ_Meta <- function(){
CODE_ORA <- Ariane:::CODE_ORA

REF_COUNTRY 		<- CODE_ORA$T_COU_COUNTRY
REF_INDICATOR 		<- CODE_ORA$T_IND_INDICATOR
REF_CLASSIFICATION 	<- CODE_ORA$T_CLA_CLASSIF
REF_NOTE 			<- CODE_ORA$T_NTE_NOTE
REF_NOTE_TYPE 		<- CODE_ORA$T_NTY_NOTE_TYPE
REF_SURVEY			<- CODE_ORA$T_SUR_SURVEY
REF_SOURCE			<- CODE_ORA$T_SRC_SOURCE


TABLE_SOURCE <- as.data.frame(cbind(Country 	= REF_SURVEY$SUR_COUNTRY_CODE,
									Type	= REF_SURVEY$SUR_SOURCE_ID,
									Label 	=REF_SURVEY$SUR_ORIGINAL_SURVEY_TITLE,
									Copy_Paste = paste("[",REF_SURVEY$SUR_ID,"] ",REF_SURVEY$SUR_ORIGINAL_SURVEY_TITLE,sep=""),
									SORT_TYPE = REF_SURVEY$SUR_SORT)
									,stringsAsFactors=FALSE)

TABLE_SOURCE <- REF_SURVEY	%>%	mutate(Country 	= SUR_COUNTRY_CODE,
									Type	= SUR_SOURCE_ID,
									Label 	=SUR_ORIGINAL_SURVEY_TITLE,
									Copy_Paste = paste("[",SUR_ID,"] ",SUR_ORIGINAL_SURVEY_TITLE,sep=""),
									SORT_TYPE = SUR_SORT) %>%
									select(Country, Type, Label, Copy_Paste, SORT_TYPE) %>%
									left_join(select(REF_COUNTRY, Country = COU_ISO3_CODE, test =  COU_TEXT_EN), by  ="Country") %>% 
									mutate(Country = paste0("[",Country,"]   ",test)) %>%
									arrange(Country, SORT_TYPE) %>%
									left_join(select(REF_SOURCE, Type = SRC_ID, TypeL = SRC_TEXT_EN), by ="Type") %>%
									mutate(Type = TypeL) %>%
									select(-TypeL, -SORT_TYPE, -test) %>%
									filter(!Label %in% c("ILO Estimates and Projections","UN-DESA Estimates and Projections"))

					
write.csv(TABLE_SOURCE,"I:/COMMON/A0 Short term indicators/Processing/ALL_META/SOURCE.csv",quote = TRUE,row.names = FALSE)



TABLE_NOTE <- as.data.frame(cbind(	Level 	= substr(REF_NOTE$NTE_TYPE_CODE,1,1),
									Type	= REF_NOTE$NTE_TYPE_ID,
									SORT_ID = REF_NOTE$NTE_TYPE_ID,
									Label	= REF_NOTE$NTE_TEXT_EN,
									Copy_Paste = paste("[",REF_NOTE$NTE_ID,"] ",REF_NOTE$NTE_TEXT_EN,sep=""),

									SORT_NOTE = REF_NOTE$NTE_SORT,
									SORT_LEVEL = NA)
									,stringsAsFactors=FALSE) %>% 
									left_join(select(REF_NOTE_TYPE,Type = NTY_ID, SORT_TYPE= NTY_SORT ), by= "Type") %>%
									left_join(select(REF_NOTE_TYPE,Type = NTY_ID, TypeL = NTY_TEXT_EN ), by= "Type") %>%
									mutate(Type = TypeL) %>%
									select(-TypeL)
									
									


REF_notes <- as.data.frame(rbind(	c("Data reference period","SOURCE"),
									c("Economic activity coverage","SOURCE"),
									c("Establishment size coverage","SOURCE"),
									c("Geographical coverage","SOURCE"),
									c("Institutional sector coverage","SOURCE"),
									c("Population coverage","SOURCE"),
									c("Reference group coverage","SOURCE"),
									c("Accounting concept","TOPIC"),
									c("Age coverage - maximum age","TOPIC"),
									c("Age coverage - minimum age","TOPIC"),
									c("Central tendency measure","TOPIC"),
									c("Coverage of occupational injuries","TOPIC"),
									c("Employment income components","TOPIC"),
									c("Job coverage","TOPIC"),
									c("Labour cost concept","TOPIC"),
									c("Labour dispute actions coverage","TOPIC"),
									c("Lower limit on coverage of strikes and lockouts","TOPIC"),
									c("Measure of output","TOPIC"),
									c("Poverty line method","TOPIC"),
									c("Threshold used to define time-related underemployment","TOPIC"),
									c("Time unit","TOPIC"),
									c("Time-related underemployment concept","TOPIC"),
									c("Type of cases of occupational injuries","TOPIC"),
									c("Type of prices","TOPIC"),
									c("UN system of national accounts","TOPIC"),
									c("Unemployment definition concept","TOPIC"),
									c("Value type","TOPIC"),
									c("Working time arrangement coverage","TOPIC"),
									c("Working time concept","TOPIC"),
									c("Minimum period of absence from work","INDICATOR"),
									c("Time period for occurrence of death","INDICATOR"),
									c("Time unit for measuring time lost","INDICATOR"),
									c("Type of rate","INDICATOR"),
									c("Type of severity rate for strikes and lockouts","INDICATOR"),
									c("Workers involved","INDICATOR")))
colnames(REF_notes) <- c("LABEL","TYPE")

REF_notes <- REF_notes %>% mutate_each(funs(as.character), everything())

TABLE_NOTE[TABLE_NOTE$Level%in%"C","SORT_LEVEL"] <- 4
TABLE_NOTE[TABLE_NOTE$Level%in%"C","Level"] <- "Classification"

CLA <- TABLE_NOTE[substr(TABLE_NOTE$Type,1,12)%in%"Nonstandard ",]



OTHER <- TABLE_NOTE %>% 
				left_join(select(REF_notes, Type = LABEL, LevelL= TYPE ), by= "Type") %>%
				mutate(Level  =LevelL) %>% select(-LevelL)
				
				
				

OTHER <- OTHER[!OTHER$Level%in%NA,]
OTHER[OTHER$Level%in%"TOPIC","SORT_LEVEL"] <- 2
OTHER[OTHER$Level%in%"SOURCE","SORT_LEVEL"] <- 1
OTHER[OTHER$Level%in%"INDICATOR","SORT_LEVEL"] <- 3

TABLE_NOTE <- CLA %>% bind_rows(OTHER)


TABLE_NOTE <- TABLE_NOTE[order(TABLE_NOTE$SORT_LEVEL,TABLE_NOTE$SORT_TYPE,TABLE_NOTE$SORT_NOTE),]

TABLE_NOTE <- TABLE_NOTE[,c("Level","Type","Label","Copy_Paste")]


write.csv(TABLE_NOTE,"I:/COMMON/A0 Short term indicators/Processing/ALL_META/NOTES.csv",quote = TRUE,row.names = FALSE,na="\"\"")

}
