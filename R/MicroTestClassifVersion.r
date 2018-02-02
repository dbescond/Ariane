#' test data with ilostat framework
#'
#' Helper function to efficiently load microdataset.
#'
#' @param df, dataframe to reduce according to the ILO collection configuration.
#' @param Collection, Collection configuration to test.
#' @author ILO bescond
#' @keywords ILO, R
#' @export

MicroTestClassifVersion <- function(df, Collection, resume = FALSE){

	test <- Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% Collection) 

	if(resume %in% TRUE) {	Ariane:::CODE_ORA$T_CLA_CLASSIF %>% 
								filter(CLA_VERSION_CODE %in% unique(test$CIC_CLASSIF_VERSION_CODE), CLA_IS_VALIDATE %in% 'Y', CLA_LEVEL %in% '1')	%>% 
								select(code = CLA_CODE, label = CLA_TEXT_EN,  CLA_SORT, CLA_IS_TOTAL, CLA_IS_DEFAULT) %>% 
								arrange(CLA_SORT) %>% 
								select(-CLA_SORT) 
					} else {

	test_version <- test %>% mutate(TEST = paste0(CIC_INDICATOR_CODE, "/", CIC_CLASSIF_VERSION_CODE)) %>% select(TEST) %>% distinct(TEST) %>% t %>% c  
	test_version <- c(test_version, paste(test$CIC_INDICATOR_CODE, "NA", sep = "/"))
	rm(test)

	df %>% 		mutate(TEST = paste0(Indicator_Code, "/", Classif1_Version_Code)) %>% 
				mutate(TEST = gsub("NA_NA", "NA",TEST)) %>%
				filter(TEST %in% test_version) %>%  
				mutate(TEST = paste0(Indicator_Code, "/", Classif2_Version_Code)) %>% 
				mutate(TEST = gsub("NA_NA", "NA",TEST)) %>%
				filter(TEST %in% test_version) %>%  
				select(-TEST) 
	}

} 

#' @export
MicroTestClassifVersionNEW <- function(df, collection, XQ = FALSE){

	test <- Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% collection) 

	if(XQ) {test <- test %>% filter(CIC_IS_XLS_QUEST %in% 'Y')}

	test_version <- test %>% mutate(TEST = paste0(CIC_INDICATOR_CODE, "/", CIC_CLASSIF_VERSION_CODE)) %>% select(TEST) %>% distinct(TEST) %>% t %>% c  
	test_version <- c(test_version, paste(test$CIC_INDICATOR_CODE, "NA", sep = "/"))
	rm(test)

	df %>% 		mutate(TEST = paste0(indicator, "/", classif1_version)) %>% 
				mutate(TEST = gsub("NA_NA", "NA",TEST)) %>%
				filter(TEST %in% test_version) %>%  
				mutate(TEST = paste0(indicator, "/", classif1_version)) %>% 
				mutate(TEST = gsub("NA_NA", "NA",TEST)) %>%
				filter(TEST %in% test_version) %>%  
				select(-TEST) 
	

} 
