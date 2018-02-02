#' Collect data from country segment predifine Rdata file, use parallel approach
#'
#' @param Query, default NULL, could received dplyr query, see example.
#' @param Path, set the string character for the path where data are available in rds files.
#' @param Internal, default FALSE, if TRUE collect usefull admin variable from Oracle.
#' @author ILO bescond  
#' @keywords ILO
#' @export
#' @examples
#' ################################## use to fetch data and metadata related
#'
#' # Collect all ILOSTAT data
#' X <- collectOracle()
#'
#' # Collect all ILOSTAT data with other meta info ( like Last_Status, Load_Mode, Qta_Check_Status, Qta_Check_User, Qta_Channel)
#' X <- collectOracle(Internal = FALSE)
#'
#' ##############
#' # Collect ILOSTAT data with filter, ie. select Annual main collection and short term main collection (excluded A from ST) 
#'
#' X <- collectOracle(	Query = 
#'							"filter(Collection_Code %in% c('YI') ,
#'							!(Collection_Code %in% 'STI' & Freq_Code %in% 'A'))"
#'		)
#'
#'
#' ##############
#' # Collect ILOSTAT data with filter and mutate and select
#'
#' X <- collectOracle(	Query = 
#'							"filter(Collection_Code %in% c('STI')) %>%  #  	filter STI
#'				 			mutate(Collection_Code = 'Main') %>%		# 	set Collection_Code to 'Main'
#'				 			select(Country_Code, Indicator_Code) %>% 	#	Select only country and indicator
#'							distinct()"									# 	take unique combinaison
#'		)
#'
#'
#' ##############
#' # Collect ILOSTAT data with minimum of columns need, delete some columns
#'
#' X <- collectOracle(Query = 	
#'							"select(-Sex_Version_Code, -Classif1_Version_Code, -Classif2_Version_Code, -Classif3_Version_Code, -Classif4_Version_Code, -Classif5_Version_Code)"
#'		)
#'
#'
#' ##############
#' # Collect ILOSTAT data with country 'AFG' only
#' # then prepare csv download format in English
#'
#' X <- collectOracle(Query = "filter(Country_Code %in% 'AFG')")
#' X <- PrepareDorwnload(X, "en")
#' write.csv(X, "./my_file.csv", na = "")
#' 
#' or in csv upload format
#' X <- PrepareUpload(X)
#' write.csv(X, "my_file.csv", na = "", row.names = FALSE)
#' write.csv(X, gzfile("my_file.csv.gz"), na = "") # save in compress gz format
#' 	
#' ##############
#' ## Advance summary table
#' # get count of statistics by Qta_Channel
#'
#' X <- collectOracle("select(Qta_Channel) %>% group_by(Qta_Channel) %>% tally()", Internal=TRUE) # get summary by country
#' X <- X %>% group_by(Qta_Channel) %>% tally() %>% ungroup # get summary for all
#'
#' # first plot
#' X %>% arrange(Qta_Channel)  %>% mutate(n = n/sum(n) * 100)%>%
#' ggplot() +   geom_bar(mapping = aes(x = Qta_Channel, fill = Qta_Channel, y = n), stat = "identity")  +   coord_polar()
#'
#' # idem by user / channel
#'
#' X <- collectOracle("select(Qta_Channel, Qta_Check_User) %>% group_by(Qta_Channel, Qta_Check_User) %>% tally()", Internal=TRUE) # get summary by country
#' X <- X %>% group_by(Qta_Channel, Qta_Check_User) %>% tally() %>% ungroup # get summary for all
#' spread(X, Qta_Channel,n) # cross tabulation
#'
#' # then plot result
#' require(ggplot2)
#' X %>% arrange(Qta_Channel)  %>% mutate(n = n/sum(n) * 100) %>%
#' ggplot() +   geom_bar(mapping = aes(x = Qta_Check_User, fill = Qta_Channel, y = n), stat = "identity")
#' 
#' # plot with facet 
#' X %>% arrange(Qta_Check_User,Qta_Channel)  %>% mutate(Percent = n/sum(n) * 100)%>%
#' ggplot() +   geom_bar(mapping = aes(x = Qta_Channel, fill = Qta_Channel, y = Percent), stat = "identity")  +   coord_polar() + 
#' facet_wrap( ~ Qta_Check_User)
#'
#' # View table on html page for small table only, even if fix(X)
#' X %>% mutate_each(funs(factor), -n) %>% DT
#'

collectOracle <- function(Query = NULL, Country = NULL, Internal = FALSE, pathfile = NULL){
	
	if(!Internal){
		Query <- paste(c("select(Country_Code, Collection_Code, Indicator_Code, Source_Code, Survey_Id, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Sex_Code, Classif1_Code, Classif2_Code, Freq_Code, Time, Value, Value_Status_Code, Notes_Source_Code, Notes_Indicator_Code, Notes_Classif_Code)", Query), collapse = " %>% ")
	}

	Path <- paste0(ilo:::path$sys, "ILO_Data/ON_ORACLE", pathfile, '/')
	cou <- list.files(Path)

	if(!is.null(Country)){cou <- cou[substr(cou, 1,3) %in% Country]}

	if(length(cou) >3){
		require(doSNOW, quietly = TRUE)
		cl 		<- parallel::makeCluster(max(1, parallel::detectCores() - 1)) ; registerDoSNOW(cl)	
		X <- foreach(i=1:length(cou), .packages = c("plyr", "dplyr", "tidyr", "Ariane")) %dopar% collectData(File = cou[i], Query, Path) %>%	bind_rows  
		
		parallel::stopCluster(cl); invisible(gc(reset = TRUE))	
	} else {
		X <- NULL
		for(i in 1:length(cou)){
			X <- X %>% bind_rows(collectData(File = cou[i], Query, Path))
			invisible(gc(reset = TRUE))
		}
	}
invisible(gc(reset = TRUE))
	return(X)
}


#' @export

collectData <- function(File, Query = NULL, Path){

	eval(parse(text=paste(c(paste0("readRDS('",paste0(Path, File),"')"), Query), collapse=" %>% "))) %>%
	mutate_each(funs(as.character), -ends_with("Value")) %>% 
	return()

}


	