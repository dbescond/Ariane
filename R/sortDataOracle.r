##' sort ilo data correctly
#'
#' @param X data frame.
#' @param Lang language, default = EN, SP, FR also available 
#' @author ILO bescond  
#' @keywords ILO
#' @export

sortDataOracle <- function(X, Lang = "EN"){

# sorting part :
X  %>% 
			mutate(	Country_Code 			= factor(Country_Code, 			select(Ariane:::CODE_ORA$T_COU_COUNTRY, COU_ISO3_CODE,COUNTRY_SORT = COU_SORT_EN) %>% 
																					filter(COU_ISO3_CODE %in% levels(as.factor(X$Country_Code))) %>% 
																					arrange(as.numeric(COUNTRY_SORT)) %>% 
																					select(Country_Code = COU_ISO3_CODE) %>% t %>% as.character
												), 
					# Source_Code 			= factor(Source_Code, 			select(Ariane:::CODE_ORA$T_SRC_SOURCE, SRC_CODE,SRC_SORT) %>% 
																					# filter(SRC_CODE %in% levels(as.factor(X$Source_Code))) %>% 
																					# arrange(as.numeric(SRC_SORT)) %>% 
																					# select(Source_Code = SRC_CODE) %>% t %>% as.character
												# ),
					Survey_Id				= factor(Survey_Id, 			select(Ariane:::CODE_ORA$T_SUR_SURVEY, SUR_ID,SUR_SORT) %>% 
																					filter(SUR_ID %in% levels(as.factor(X$Survey_Id))) %>% 
																					arrange(as.numeric(SUR_SORT)) %>% 
																					select(Survey_Id = SUR_ID) %>% t %>% as.character
												),
					Collection_Code 		= factor(Collection_Code, 		select(Ariane:::CODE_ORA$T_COL_COLLECTION, COL_CODE,COL_ID) %>% 
																					filter(COL_CODE %in% levels(as.factor(X$Collection_Code))) %>% 
																					arrange(as.numeric(COL_ID)) %>% 
																					select(Collection_Code = COL_CODE) %>% t %>% as.character
												),
					Indicator_Code 			= factor(Indicator_Code, 		select(Ariane:::CODE_ORA$T_IND_INDICATOR, IND_CODE,IND_SORT) %>% 
																					filter(IND_CODE %in% levels(as.factor(X$Indicator_Code))) %>% 
																					arrange(IND_SORT) %>% 
																					select(Indicator_Code = IND_CODE) %>% t %>% as.character
												),
					Sex_Version_Code 		= factor(Sex_Version_Code, 		select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$Sex_Version_Code))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(Sex_Version_Code = CLV_CODE) %>% t %>% as.character
												),
					Classif1_Version_Code 	= factor(Classif1_Version_Code, select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$Classif1_Version_Code))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(Classif1_Version_Code = CLV_CODE) %>% t %>% as.character
												),
					Classif2_Version_Code 	= factor(Classif2_Version_Code, select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$Classif2_Version_Code))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(Classif2_Version_Code = CLV_CODE) %>% t %>% as.character
												),
					Time 					= factor(Time, 					select(Ariane:::CODE_ORA$T_TIM_TIME, TIM_FORMAT_USER,TIM_ID) %>% 
																					filter(TIM_FORMAT_USER %in% levels(as.factor(X$Time))) %>% 
																					arrange(as.numeric(TIM_ID)) %>% 
																					select(Time = TIM_FORMAT_USER) %>% t %>% as.character
												),
					Sex_Code 				= factor(Sex_Code, 				select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$Sex_Code))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(Sex_Code = CLA_CODE) %>% t %>% as.character
												),
					Classif1_Code 			= factor(Classif1_Code, 		select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$Classif1_Code))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(Classif1_Code = CLA_CODE) %>% t %>% as.character
												),
					Classif2_Code 			= factor(Classif2_Code, 		select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$Classif2_Code))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(Classif2_Code = CLA_CODE) %>% t %>% as.character
												)
				) %>% 	
			arrange(	Country_Code, 
						#Source_Code, 
						Survey_Id, 
						Collection_Code, 
						Indicator_Code, 
						Sex_Version_Code, 
						Classif1_Version_Code, 
						Classif2_Version_Code,  
						Time, 
						Sex_Code, 
						Classif1_Code, 
						Classif2_Code
				 )#  %>%
			# mutate(	Value_Status_Code 		= factor(Value_Status_Code, 	sort(levels(as.factor(Value_Status_Code		)))),
					# Notes_Source_Code 		= factor(Notes_Source_Code, 	sort(levels(as.factor(Notes_Source_Code		)))),
					# Notes_Indicator_Code 	= factor(Notes_Indicator_Code, 	sort(levels(as.factor(Notes_Indicator_Code	)))),
					# Notes_Classif_Code 		= factor(Notes_Classif_Code, 	sort(levels(as.factor(Notes_Classif_Code	))))
				# )
}


#' @export

sortDataOracle_oracle_format <- function(X, Lang = "EN"){

# sorting part :
X  %>% 
			mutate(	ref_area 			= factor(ref_area, 			select(Ariane:::CODE_ORA$T_COU_COUNTRY, COU_ISO3_CODE,COUNTRY_SORT = COU_SORT_EN) %>% 
																					filter(COU_ISO3_CODE %in% levels(as.factor(X$ref_area))) %>% 
																					arrange(as.numeric(COUNTRY_SORT)) %>% 
																					select(ref_area = COU_ISO3_CODE) %>% t %>% as.character
												), 
					source				= factor(source, 			select(Ariane:::CODE_ORA$T_SUR_SURVEY, SUR_ID,SUR_SORT, SUR_SOURCE_CODE) %>% 
																					mutate(SUR_ID = paste0(SUR_SOURCE_CODE, ':',SUR_ID )) %>% 
																					select(-SUR_SOURCE_CODE) %>% 
																					filter(SUR_ID %in% levels(as.factor(X$source))) %>% 
																					arrange(as.numeric(SUR_SORT)) %>% 
																					select(source = SUR_ID) %>% t %>% as.character
												),
					collection 		= factor(collection, 		select(Ariane:::CODE_ORA$T_COL_COLLECTION, COL_CODE,COL_ID) %>% 
																					filter(COL_CODE %in% levels(as.factor(X$collection))) %>% 
																					arrange(as.numeric(COL_ID)) %>% 
																					select(collection = COL_CODE) %>% t %>% as.character
												),
					indicator 			= factor(indicator, 		select(Ariane:::CODE_ORA$T_IND_INDICATOR, IND_CODE,IND_SORT) %>% 
																					filter(IND_CODE %in% levels(as.factor(X$indicator))) %>% 
																					arrange(IND_SORT) %>% 
																					select(indicator = IND_CODE) %>% t %>% as.character
												),
					sex_version 		= factor(sex_version, 		select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$sex_version))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(sex_version = CLV_CODE) %>% t %>% as.character
												),
					classif1_version 	= factor(classif1_version, select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$classif1_version))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(classif1_version = CLV_CODE) %>% t %>% as.character
												),
					classif2_version 	= factor(classif2_version, select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$classif2_version))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(classif2_version = CLV_CODE) %>% t %>% as.character
												),
					time 					= factor(time, 					select(Ariane:::CODE_ORA$T_TIM_TIME, TIM_FORMAT_USER,TIM_ID) %>% 
																					filter(TIM_FORMAT_USER %in% levels(as.factor(X$time))) %>% 
																					arrange(as.numeric(TIM_ID)) %>% 
																					select(time = TIM_FORMAT_USER) %>% t %>% as.character
												),
					sex 				= factor(sex, 				select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$sex))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(sex = CLA_CODE) %>% t %>% as.character
												),
					classif1 			= factor(classif1, 		select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$classif1))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(classif1 = CLA_CODE) %>% t %>% as.character
												),
					classif2 			= factor(classif2, 		select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$classif2))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(classif2 = CLA_CODE) %>% t %>% as.character
												)
				) %>% 	
			arrange(	ref_area, 
						source, 
						collection, 
						indicator, 
						sex_version, 
						classif1_version, 
						classif2_version,  
						time, 
						sex, 
						classif1, 
						classif2
				 )  %>% 
		mutate_if(is.factor, as.character) 
				
}



#' @export

sortDataOracle_NEW <- function(X, Lang = "EN", drop = NULL){

keep <- paste0(colnames(X), collapse = '/')  #"collection/ref_area/source/indicator/sex/classif1/classif2/time"

if(!is.null(drop)) {for (i in 1:length(drop)) {keep <- gsub(drop[i], '', keep, fixed = TRUE)}} 

require(ilo)
init_ilo(-cl)
# sorting part :
X <- X  %>% 		switch_ilo(version)  %>%	
					mutate(		Source_Code = str_sub(source, 1,2),
								Survey_Id = str_sub(source, 4,-1))
					
			
if(str_detect(keep, 'ref_area')){
X <- X  %>% mutate(		ref_area 			= factor(ref_area, 			select(Ariane:::CODE_ORA$T_COU_COUNTRY, COU_ISO3_CODE,COUNTRY_SORT = COU_SORT_EN) %>% 
																					filter(COU_ISO3_CODE %in% levels(as.factor(X$ref_area))) %>% 
																					arrange(as.numeric(COUNTRY_SORT)) %>% 
																					select(ref_area = COU_ISO3_CODE) %>% t %>% as.character
												))
}
if(str_detect(keep, 'source')){											 
# X <- X  %>% mutate(		Source_Code 			= factor(Source_Code, 			select(Ariane:::CODE_ORA$T_SRC_SOURCE, SRC_CODE,SRC_SORT) %>% 
																					# filter(SRC_CODE %in% levels(as.factor(X$Source_Code))) %>% 
																					# arrange(as.numeric(SRC_SORT)) %>% 
																					# select(Source_Code = SRC_CODE) %>% t %>% as.character
												# ))
												
X <- X  %>% mutate(		Survey_Id				= factor(Survey_Id, 			select(Ariane:::CODE_ORA$T_SUR_SURVEY, SUR_ID,SUR_SORT) %>% 
																					filter(SUR_ID %in% levels(as.factor(X$Survey_Id))) %>% 
																					arrange(as.numeric(SUR_SORT)) %>% 
																					select(Survey_Id = SUR_ID) %>% t %>% as.character
												))
}	
if(str_detect(keep, 'collection')){												
X <- X  %>% mutate(		collection 		= factor(collection, 		select(Ariane:::CODE_ORA$T_COL_COLLECTION, COL_CODE,COL_ID) %>% 
																					filter(COL_CODE %in% levels(as.factor(X$collection))) %>% 
																					arrange(as.numeric(COL_ID)) %>% 
																					select(collection = COL_CODE) %>% t %>% as.character
												))
}
if(str_detect(keep, 'indicator')){
X <- X  %>% mutate(		indicator 			= factor(indicator, 		select(Ariane:::CODE_ORA$T_IND_INDICATOR, IND_CODE,IND_SORT) %>% 
																					filter(IND_CODE %in% levels(as.factor(X$indicator))) %>% 
																					arrange(IND_SORT) %>% 
																					select(indicator = IND_CODE) %>% t %>% as.character
												))
}
if(str_detect(keep, 'sex')){
X <- X  %>% mutate(		sex_version 		= factor(sex_version, 		select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$sex_version))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(sex_version = CLV_CODE) %>% t %>% as.character
												))
X <- X  %>% mutate(		sex 				= factor(sex, 				select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$sex))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(sex = CLA_CODE) %>% t %>% as.character
												))
}
if(str_detect(keep, 'classif1')){
X <- X  %>% mutate(		classif1_version 	= factor(classif1_version, select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$classif1_version))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(classif1_version = CLV_CODE) %>% t %>% as.character
												))
X <- X  %>% mutate(		classif1 			= factor(classif1, 		select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$classif1))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(classif1 = CLA_CODE) %>% t %>% as.character
												))
}
if(str_detect(keep, 'classif2')){
X <- X  %>% mutate(		classif2_version 	= factor(classif2_version, select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																					filter(CLV_CODE %in% levels(as.factor(X$classif2_version))) %>% 
																					arrange(as.numeric(CLV_SORT)) %>% 
																					select(classif2_version = CLV_CODE) %>% t %>% as.character
												))
X <- X  %>% mutate(		classif2 			= factor(classif2, 		select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																					filter(CLA_CODE %in% levels(as.factor(X$classif2))) %>% 
																					arrange(as.numeric(CLA_SORT)) %>% 
																					select(classif2 = CLA_CODE) %>% t %>% as.character
												))
}
if(str_detect(keep, 'time')){
X <- X  %>% mutate(		time 					= factor(time, 					select(Ariane:::CODE_ORA$T_TIM_TIME, TIM_FORMAT_USER,TIM_ID) %>% 
																					filter(TIM_FORMAT_USER %in% levels(as.factor(X$time))) %>% 
																					arrange(as.numeric(TIM_ID)) %>% 
																					select(time = TIM_FORMAT_USER) %>% t %>% as.character
												))
}


invisible(gc(reset = TRUE))
if(str_detect(keep, 'classif2')){ X <- X %>% arrange(classif2)}
if(str_detect(keep, 'classif1')){ X <- X %>% arrange(classif1)}
if(str_detect(keep, 'sex')){ X <- X %>% arrange(sex)}
if(str_detect(keep, 'time')){ X <- X %>% arrange(time)}
if(str_detect(keep, 'classif2_version')){ X <- X %>% arrange(classif2)}
if(str_detect(keep, 'classif1_version')){ X <- X %>% arrange(classif1)}
if(str_detect(keep, 'sex_version')){ X <- X %>% arrange(sex)}
if(str_detect(keep, 'indicator')){ X <- X %>% arrange(indicator)}

if(str_detect(keep, 'source')){ X <- X %>% arrange( Survey_Id)} # arrange(Source_Code, Survey_Id)}
if(str_detect(keep, 'ref_area')){ X <- X %>% arrange(ref_area)}
if(str_detect(keep, 'collection')){ X <- X %>% arrange(collection)}
invisible(gc(reset = TRUE))
if(str_detect(keep, 'source')){X <- X  %>%	mutate(	source = paste0(Source_Code, ':',Survey_Id)) %>% select(-Source_Code, -Survey_Id) %>% as.tbl}
if(str_detect(keep, 'sex|classif1|classif2')){ X <- X %>% select(-contains('_version'))}

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

X

}


#' @export

My_unsplit_KEY <- function(X,ID=1,KEY=1,ref="/"){
	X <- as.data.frame(X, stringsAsFactors=FALSE)
	sbt <- strsplit(as.character(X[,ID]),ref)
	ifelse(KEY%in%1, n <- max(sapply(sbt, length)), n <- length(KEY))
	l <- lapply(sbt, function(X) c(X, rep(NA, n - length(X))))
	NEW <- as.data.frame(t(do.call(cbind, l)),stringsAsFactors =FALSE)
	for (i in 1:ncol(NEW)){
		NEW[,i] <- as.character(NEW[,i])
	}
	rm(sbt,l)
	ifelse(KEY%in%1 ,colnames(NEW) <- paste(rep("PASS",n),1:n,sep=""),colnames(NEW) <- KEY)
	X <- as.data.frame(cbind(IDNEW = as.character(X[,ID]),NEW,X),stringsAsFactors =FALSE)
	X <- X[,!colnames(X)%in%ID]
	X$IDNEW <- as.character(X[,"IDNEW"])
	colnames(X)[colnames(X)%in%"IDNEW"] <- ID
	X
}

#' @export

splitid <- function(ReadMe, arguments){
	REF_COU <- ReadMe
	if(length(arguments)==0){
		REF_COU
	} 
	else
	{
		for(i in 1:length(arguments)){ 
			eval(parse(text=arguments[[i]]))
		}
		test <- tibble(country = REF_COU, BinSize = as.numeric(file.info(paste0("./Processing/ILO_Data/ON_ORACLE/",REF_COU,".rds"))$size))  %>% 
						mutate(BinSize = ifelse(BinSize%in%NA, "700", BinSize)) %>% 
						arrange(as.numeric(BinSize)) %>% mutate(ID = NA)
		new <- list(1:nb)
		for (nbbatch in 1:nb){
			new[[nbbatch]] <- test[seq(nbbatch,nrow(test), nb),] %>% select(-ID)
			test <- test %>% 
						left_join(	new[[nbbatch]] %>% mutate(ref = nbbatch),  by = c("country", "BinSize")) %>% 
						mutate(ID = ifelse(!ref %in% NA, ref, ID)) %>% select(-ref)
 			if(nbbatch%%2==0){
					new[[nbbatch]] <- new[[nbbatch]] %>% arrange(desc(as.numeric(BinSize))) 
			}	
		}
		new[[as.numeric(batch)]]$country
	}
}

#' @export

splitCountry <- function(ReadMe, arguments){
	REF_COU <- unique(unlist(strsplit(ReadMe$COUNTRY,";")))
	if(length(arguments)==0){
		REF_COU
	} 
	else
	{
		for(i in 1:length(arguments)){ 
			eval(parse(text=arguments[[i]]))
		}
		test <- tibble(country = REF_COU, BinSize = as.numeric(file.info(paste0("./Processing/ILO_Data/ON_ORACLE/",REF_COU,".rds"))$size))  %>% 
						mutate(BinSize = ifelse(BinSize%in%NA, "700", BinSize)) %>% 
						arrange(as.numeric(BinSize)) %>% mutate(ID = NA)
		new <- list(1:nb)
		for (nbbatch in 1:nb){
			new[[nbbatch]] <- test[seq(nbbatch,nrow(test), nb),] %>% select(-ID)
			test <- test %>% 
						left_join(	new[[nbbatch]] %>% mutate(ref = nbbatch),  by = c("country", "BinSize")) %>% 
						mutate(ID = ifelse(!ref %in% NA, ref, ID)) %>% select(-ref)
 			if(nbbatch%%2==0){
					new[[nbbatch]] <- new[[nbbatch]] %>% arrange(desc(as.numeric(BinSize))) 
			}	
		}
		new[[as.numeric(batch)]]$country
	}
}

#' @export

splitCountryMicro_OLD <- function(workflow, arguments){
	REF_COU <- workflow %>% count(ref_area) %>% rename(country = ref_area,BinSize = n )
	if(length(arguments)==0){
		workflow
	} 
	else
	{
		for(i in 1:length(arguments)){ 
			eval(parse(text=arguments[[i]]))
		}
		test <- REF_COU %>% 
						arrange(as.numeric(BinSize)) %>% mutate(ID = NA)
		new <- list(1:nb)
		for (nbbatch in 1:nb){
			new[[nbbatch]] <- test[seq(nbbatch,nrow(test), nb),] %>% select(-ID)
			test <- test %>% 
						left_join(	new[[nbbatch]] %>% mutate(ref = nbbatch),  by = c("country", "BinSize")) %>% 
						mutate(ID = ifelse(!ref %in% NA, ref, ID)) %>% select(-ref)
 			if(nbbatch%%2==0){
					new[[nbbatch]] <- new[[nbbatch]] %>% arrange(desc(as.numeric(BinSize))) 
			}	
		}
		workflow %>% filter(ref_area %in% new[[as.numeric(batch)]]$country)
	}
}


#' @export

cleanDf 	<-	function(df, header  = TRUE){

	if(header){		
		df %>%
			mutate_all(.funs = as.character) %>%
			mutate_all(.funs = str_trim ) %>%
			mutate_all(funs(mapvalues(.,c('NaN', '', 'NA'), c(NA,NA, NA), warn_missing = FALSE)))
	} else {
		ref <- df %>% slice(1) %>% t %>% c
		colnames(df)[!is.na(ref)] <- ref[!is.na(ref)]
		df[,ref[!is.na(ref)]] %>% as.tbl %>%
			slice(-1) %>%
			mutate_all(.funs = as.character) %>%
			mutate_all(.funs = str_trim ) %>%
			mutate_all(funs(mapvalues(.,c('NaN', '', 'NA'), c(NA,NA, NA), warn_missing = FALSE)))
	}
} 

#' @export

My_SQL_UPLOAD_FORMAT <- function(X, DELETE = FALSE, LABEL = FALSE){

# X <- DATA

if(LABEL){
X <- X %>% 	
		mutate(			# create noteQtable by combining notes source code and note indicator code
					Qtable_Notes_String = ifelse(!Notes_Source_Code%in%NA,paste(Notes_Source_Code,Notes_Indicator_Code,sep="_"),Notes_Indicator_Code),
					Qtable_Notes_String =  gsub("NA_","",Qtable_Notes_String),
					Qtable_Notes_String =  gsub("_NA","",Qtable_Notes_String)) %>%  
		select(			# delete unuse columns
					-Notes_Source_Code,-Notes_Indicator_Code) %>%	
		mutate(		Flag = as.character(Value_Status_Code), 
					Currency = ifelse(!Currency_Code%in%NA,as.character(substr(Currency_Code,5,nchar(Currency_Code))), Currency_Code),
					Value_Notes_String 	= plyr:::mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= plyr:::mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Label_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_", Lang = "EN")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Label_notesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_", Lang = "EN")))
}					
					
					
if(!LABEL){
X <- X %>% 	
		mutate(			# create noteQtable by combining notes source code and note indicator code
					Qtable_Notes_String = ifelse(!Notes_Source_Code%in%NA,paste(Notes_Source_Code,Notes_Indicator_Code,sep="_"),Notes_Indicator_Code),
					Qtable_Notes_String =  gsub("NA_","",Qtable_Notes_String),
					Qtable_Notes_String =  gsub("_NA","",Qtable_Notes_String)) %>%
		select(			# delete unuse columns
					-Notes_Source_Code,-Notes_Indicator_Code) %>%	
		mutate(		Flag = as.character(Value_Status_Code),
					Currency = ifelse(!Currency_Code%in%NA,as.character(substr(Currency_Code,5,nchar(Currency_Code))), Currency_Code),
					Value_Notes_String 	= plyr:::mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= plyr:::mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Transform_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Transform_notesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_"))) %>%
		select(-Notes_Classif_Code)
							
					
}



X <- X 	%>% mutate(	Lang = "EN",
					Country_Code = as.character(Country_Code),
					Country_Label = as.character(NA),
					Collection_Code = as.character(Collection_Code),
					Collection_Label = as.character(NA),
					Indicator_Code = as.character(Indicator_Code),
					Indicator_Label = as.character(NA),
					Survey_Code = as.character(Survey_Id),
					Survey_Label = as.character(NA),
					Sex_Version_Code  = as.character(Sex_Version_Code),
					Classif1_Version_Code = as.character(Classif1_Version_Code),
					Classif2_Version_Code = as.character(Classif2_Version_Code),
					Classif3_Version_Code = as.character(Classif3_Version_Code),
					Classif4_Version_Code = as.character(Classif4_Version_Code),
					Classif5_Version_Code = as.character(Classif5_Version_Code),
					Sex_Item_Code = as.character(Sex_Code),
					Sex_Item_Label = as.character(NA),
					Classif1_Item_Code = as.character(Classif1_Code),
					Classif1_Item_Label = as.character(NA),
					Classif2_Item_Code = as.character(Classif2_Code),
					Classif2_Item_Label = as.character(NA),
					Classif3_Item_Code = as.character(Classif3_Code),
					Classif3_Item_Label = as.character(NA),
					Classif4_Item_Code = as.character(Classif4_Code),
					Classif4_Item_Label = as.character(NA),
					Classif5_Item_Code = as.character(Classif5_Code),
					Classif5_Item_Label = as.character(NA),
					Freq = as.character(Freq_Code),
					Time  = as.character(Time),
					Obs_Value = as.numeric(Value),
					Free_Text_Notes = as.character(NA)
			)%>% 
			select(!!c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					

if(DELETE){


X <- X %>% 	group_by(Country_Code,Collection_Code,Indicator_Code,Survey_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Classif3_Version_Code,Classif4_Version_Code,Classif5_Version_Code,Time) %>%
			summarise(Lang = first(as.character(Lang)), 
						Country_Label = as.character(NA), 
						Collection_Label = as.character(NA), 
						Indicator_Label = as.character(NA),  
						Survey_Label = as.character(NA),  
						Sex_Item_Code = as.character(NA), 
						Sex_Item_Label = as.character(NA), 
						Classif1_Item_Code = as.character(NA), 
						Classif1_Item_Label = as.character(NA), 
						Classif2_Item_Code = as.character(NA), 
						Classif2_Item_Label = as.character(NA), 
						Classif3_Item_Code = as.character(NA), 
						Classif3_Item_Label = as.character(NA), 
						Classif4_Item_Code = as.character(NA), 
						Classif4_Item_Label = as.character(NA), 
						Classif5_Item_Code = as.character(NA),  
						Classif5_Item_Label = as.character(NA), 
						Freq =  as.character(NA), 
						Obs_Value = as.character(NA), 
						Flag = as.character(NA), 
						Currency = as.character(NA), 
						Value_Notes_String = as.character(NA), 
						Qtable_Notes_String = as.character(NA), 
						Free_Text_Notes = as.character(NA)) %>% 
			ungroup()  %>% 
			select(!!c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					
}
return(X)
}

#' @export

My_SQL_DOWNLOAD_FORMAT <- function(X, Lang = "EN"){


CODE_ORA <- Ariane:::CODE_ORA
attach(CODE_ORA,warn.conflicts = FALSE)

FLAG <- CODE_ORA$T_CLT_CODELIST %>% filter(CLT_COLUMN_NAME%in%"VALUE_STATUS")

X <- X %>%
		mutate(	Survey_Code = as.character(Survey_Id),
				Notes_Source_Label = plyr:::mapvalues(Notes_Source_Code,			from = levels(as.factor(Notes_Source_Code)), 
																			to = My_Label_notesJ_NEW(levels(as.factor(Notes_Source_Code)),SEP = "_", Lang = Lang)),
				Notes_Indicator_Label = plyr:::mapvalues(Notes_Indicator_Code, 	from = levels(as.factor(Notes_Indicator_Code)), 
																			to = My_Label_notesJ_NEW(levels(as.factor(Notes_Indicator_Code)),SEP = "_", Lang = Lang)),
				Notes_Classif_Label = plyr:::mapvalues(Notes_Classif_Code,			from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Label_notesJ_NEW(levels(as.factor(Notes_Classif_Code)),SEP = "_", Lang = Lang)),
				Classif1_Version_Code = as.character(Classif1_Version_Code),
				Classif2_Version_Code = as.character(Classif2_Version_Code),
				Sex_Item_Code = as.character(Sex_Code),
				Classif1_Version_Item_Code = as.character(Classif1_Code),
				Classif2_Version_Item_Code = as.character(Classif2_Code),
				Obs_Value = Value,
				Topic_code = substr(Indicator_Code,1,3),
				Sex_Code = ifelse(!Sex_Item_Code%in%NA , substr(Sex_Item_Code,1,3), as.character(NA)) ,
				Classif1_Code = ifelse(!Classif1_Version_Code%in%NA,substr(Classif1_Version_Code,1,3), as.character(NA)),
				Classif2_Code = ifelse(!Classif2_Version_Code%in%NA,substr(Classif2_Version_Code,1,3), as.character(NA)),
				Time = as.character(Time),
				Flag_Code = as.character(Value_Status_Code))  %>%
		select(-Value_Status_Code,-Freq_Code,-Value,-Survey_Id) %>%				
		left_join(select_(T_COU_COUNTRY, .dots = c(Country_Code = 'COU_ISO3_CODE', Country_Label = paste0("COU_TEXT_",Lang),COUNTRY_SORT = paste0("COU_SORT_",Lang))),by ="Country_Code") %>%
		left_join(select_(T_COL_COLLECTION, .dots  = c(Collection_Code = 'COL_CODE', Collection_Label = paste0("COL_TEXT_",Lang))),by ="Collection_Code") %>%
		left_join(select_(T_TOP_TOPIC, .dots = c(Topic_code = 'TOP_CODE',TOPIC_SORT = 'TOP_SORT' )),by ="Topic_code") %>%
		left_join(select_(T_IND_INDICATOR, .dots = c(Indicator_Code = 'IND_CODE', Indicator_Label = paste0("IND_TEXT_",Lang),INDICATOR_SORT = 'IND_SORT', Unit_Measure_Id= 'IND_UNIT_MEASURE_ID', Unit_Multiplier_Id = 'IND_UNIT_MULT_ID' )),by ="Indicator_Code") %>%
		left_join(select(T_CLT_CODELIST, Unit_Measure_Id = CLT_ID, Unit_Measure_Code = CLT_COLUMN_CODE ,Unit_Measure_Label = 'CLT_TEXT_EN'),by ="Unit_Measure_Id") %>% select(-Unit_Measure_Id) %>%
		left_join(select(T_CLT_CODELIST, Unit_Multiplier_Id = CLT_ID, Unit_Multiplier_Code = CLT_COLUMN_CODE ,Unit_Multiplier_Label = 'CLT_TEXT_EN'),by ="Unit_Multiplier_Id") %>% select(-Unit_Multiplier_Id) %>%
		left_join(select(T_SRC_SOURCE, Source_Code = SRC_CODE, Source_Label = 'SRC_TEXT_EN', SOURCE_SORT = SRC_SORT),by ="Source_Code") %>%
		left_join(select(mutate_all(T_SUR_SURVEY, .funs = as.character), Survey_Code = SUR_ID, Survey_Label = SUR_SURVEY_TITLE_EN,SURVEY_SORT = SUR_SORT),by ="Survey_Code") %>%
		
		left_join(select(T_CLY_CLASSIF_TYPE, Sex_Code = CLY_CODE, SEX_TYPE_SORT = CLY_SORT),by ="Sex_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif1_Code = CLY_CODE, CLASSIF1_TYPE_SORT = CLY_SORT),by ="Classif1_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif2_Code = CLY_CODE, CLASSIF2_TYPE_SORT = CLY_SORT),by ="Classif2_Code") %>%
		mutate(Time = as.character(Time)) %>%
		left_join(select(T_TIM_TIME, Time = TIM_FORMAT_USER, TIME_ID = TIM_ID),by ="Time") %>%
				
		left_join(select(T_CLV_CLASSIF_VERSION, Classif1_Version_Code = CLV_CODE, Classif1_Version_Label = CLV_TEXT_EN,CLASSIF1_VERSION_SORT = CLV_SORT),by ="Classif1_Version_Code") %>%
		left_join(select(T_CLV_CLASSIF_VERSION, Classif2_Version_Code = CLV_CODE, Classif2_Version_Label = CLV_TEXT_EN,CLASSIF2_VERSION_SORT = CLV_SORT),by ="Classif2_Version_Code") %>%
		
		
		left_join(select(T_CLA_CLASSIF, Sex_Item_Code = CLA_CODE, Sex_Item_Label = CLA_TEXT_EN, SEX_SORT = CLA_SORT),by ="Sex_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif1_Version_Item_Code = CLA_CODE, Classif1_Item_Label = CLA_TEXT_EN, CLASSIF1_SORT = CLA_SORT),by ="Classif1_Version_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif2_Version_Item_Code = CLA_CODE, Classif2_Item_Label = CLA_TEXT_EN, CLASSIF2_SORT = CLA_SORT),by ="Classif2_Version_Item_Code") %>%
		left_join(select(FLAG, Flag_Code = CLT_COLUMN_CODE, Flag_Label = CLT_TEXT_EN),by ="Flag_Code") %>%
		select(-Sex_Version_Code,-Classif1_Version_Code,-Classif2_Version_Code) %>%
		mutate(	Lang = tolower(Lang)) %>%
		select(-Topic_code,-COUNTRY_SORT,-SOURCE_SORT,-SURVEY_SORT,-TOPIC_SORT,-INDICATOR_SORT,-SEX_TYPE_SORT, -CLASSIF1_TYPE_SORT, -CLASSIF2_TYPE_SORT, -CLASSIF1_VERSION_SORT, -CLASSIF2_VERSION_SORT, -TIME_ID, -SEX_SORT, -CLASSIF1_SORT, -CLASSIF2_SORT)
		

COL_X <- c(
"Lang",
"Country_Code",
"Country_Label",
"Collection_Code",
"Collection_Label",
"Indicator_Code",
"Indicator_Label",
"Unit_Measure_Code",
"Unit_Measure_Label",
"Unit_Multiplier_Code",
"Unit_Multiplier_Label",
# "Currency_Code",
# "Currency_Label",
"Source_Code",
"Source_Label",
"Survey_Code",
"Survey_Label",
"Sex_Item_Code",
"Sex_Item_Label",
"Classif1_Version_Item_Code",
"Classif1_Version_Label",
"Classif1_Item_Label",
"Classif2_Version_Item_Code",
"Classif2_Version_Label",
"Classif2_Item_Label",
"Time",
"Obs_Value",
"Flag_Code",
"Flag_Label",
"Notes_Source_Code",
"Notes_Source_Label",
"Notes_Indicator_Code",
"Notes_Indicator_Label",
"Notes_Classif_Code",
"Notes_Classif_Label"
)

X <- X[,COL_X]

detach(CODE_ORA)

return(X)

}

#' @export

My_SQL_DOWNLOAD_FORMAT_NEW <- function(X, Lang = "EN"){


CODE_ORA <- Ariane:::CODE_ORA
attach(CODE_ORA,warn.conflicts = FALSE)

FLAG <- CODE_ORA$T_CLT_CODELIST %>% filter(CLT_COLUMN_NAME%in%"VALUE_STATUS")
CODE_ORA$T_NTE_NOTE %>% mutate_all(as.factor) -> CODE_ORA$T_NTE_NOTE
CODE_ORA$T_NTY_NOTE_TYPE %>% mutate_all(as.factor) -> CODE_ORA$T_NTY_NOTE_TYPE

X <- X %>%
		mutate(	Survey_Code = as.character(Survey_Id),
				Notes_Source_Label = plyr:::mapvalues(Notes_Source_Code,			from = levels(as.factor(Notes_Source_Code)), 
																			to = My_Label_notesJ_OLD(levels(as.factor(Notes_Source_Code)),SEP = "_", Lang = Lang)),
				Notes_Indicator_Label = plyr:::mapvalues(Notes_Indicator_Code, 	from = levels(as.factor(Notes_Indicator_Code)), 
																			to = My_Label_notesJ_OLD(levels(as.factor(Notes_Indicator_Code)),SEP = "_", Lang = Lang)),
				Notes_Classif_Label = plyr:::mapvalues(Notes_Classif_Code,			from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Label_notesJ_OLD(levels(as.factor(Notes_Classif_Code)),SEP = "_", Lang = Lang)),
				Classif1_Version_Code = as.character(Classif1_Version_Code),
				Classif2_Version_Code = as.character(Classif2_Version_Code),
				Sex_Item_Code = as.character(Sex_Code),
				Classif1_Version_Item_Code = as.character(Classif1_Code),
				Classif2_Version_Item_Code = as.character(Classif2_Code),
				Obs_Value = Value,
				Topic_code = substr(Indicator_Code,1,3),
				Sex_Code = ifelse(!Sex_Item_Code%in%NA , substr(Sex_Item_Code,1,3), as.character(NA)) ,
				Classif1_Code = ifelse(!Classif1_Version_Code%in%NA,substr(Classif1_Version_Code,1,3), as.character(NA)),
				Classif2_Code = ifelse(!Classif2_Version_Code%in%NA,substr(Classif2_Version_Code,1,3), as.character(NA)),
				Time = as.character(Time),
				Flag_Code = as.character(Value_Status_Code))  %>%
		select(-Value_Status_Code,-Freq_Code,-Value,-Survey_Id) %>%				
		left_join(select_(T_COU_COUNTRY, .dots = c(Country_Code = 'COU_ISO3_CODE', Country_Label = paste0("COU_TEXT_",Lang),COUNTRY_SORT = paste0("COU_SORT_",Lang))),by ="Country_Code") %>%
		left_join(select_(T_COL_COLLECTION, .dots  = c(Collection_Code = 'COL_CODE', Collection_Label = paste0("COL_TEXT_",Lang))),by ="Collection_Code") %>%
		left_join(select_(T_TOP_TOPIC, .dots = c(Topic_code = 'TOP_CODE',TOPIC_SORT = 'TOP_SORT' )),by ="Topic_code") %>%
		left_join(select_(T_IND_INDICATOR, .dots = c(Indicator_Code = 'IND_CODE', Indicator_Label = paste0("IND_TEXT_",Lang),INDICATOR_SORT = 'IND_SORT', Unit_Measure_Id= 'IND_UNIT_MEASURE_ID', Unit_Multiplier_Id = 'IND_UNIT_MULT_ID' )),by ="Indicator_Code") %>%
		left_join(select(T_CLT_CODELIST, Unit_Measure_Id = CLT_ID, Unit_Measure_Code = CLT_COLUMN_CODE ,Unit_Measure_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Unit_Measure_Id") %>% select(-Unit_Measure_Id) %>%
		left_join(select(T_CLT_CODELIST, Unit_Multiplier_Id = CLT_ID, Unit_Multiplier_Code = CLT_COLUMN_CODE ,Unit_Multiplier_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Unit_Multiplier_Id") %>% select(-Unit_Multiplier_Id) %>%
		left_join(select(T_SRC_SOURCE, Source_Code = SRC_CODE, Source_Label = eval(parse(text=paste0("SRC_TEXT_",Lang))), SOURCE_SORT = SRC_SORT),by ="Source_Code") %>%
		left_join(select(T_SUR_SURVEY, Survey_Code = SUR_ID, Survey_Label = eval(parse(text=paste0("SUR_SURVEY_TITLE_",Lang))),SURVEY_SORT = SUR_SORT),by ="Survey_Code") %>%
		
		left_join(select(T_CLY_CLASSIF_TYPE, Sex_Code = CLY_CODE, SEX_TYPE_SORT = CLY_SORT),by ="Sex_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif1_Code = CLY_CODE, CLASSIF1_TYPE_SORT = CLY_SORT),by ="Classif1_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif2_Code = CLY_CODE, CLASSIF2_TYPE_SORT = CLY_SORT),by ="Classif2_Code") %>%
		mutate(Time = as.character(Time)) %>%
		left_join(select(T_TIM_TIME, Time = TIM_FORMAT_USER, TIME_ID = TIM_ID),by ="Time") %>%
				
		left_join(select(T_CLV_CLASSIF_VERSION, Classif1_Version_Code = CLV_CODE, Classif1_Version_Label = eval(parse(text=paste0("CLV_TEXT_",Lang))),CLASSIF1_VERSION_SORT = CLV_SORT),by ="Classif1_Version_Code") %>%
		left_join(select(T_CLV_CLASSIF_VERSION, Classif2_Version_Code = CLV_CODE, Classif2_Version_Label = eval(parse(text=paste0("CLV_TEXT_",Lang))),CLASSIF2_VERSION_SORT = CLV_SORT),by ="Classif2_Version_Code") %>%
		
		
		left_join(select(T_CLA_CLASSIF, Sex_Item_Code = CLA_CODE, Sex_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), SEX_SORT = CLA_SORT),by ="Sex_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif1_Version_Item_Code = CLA_CODE, Classif1_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), CLASSIF1_SORT = CLA_SORT),by ="Classif1_Version_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif2_Version_Item_Code = CLA_CODE, Classif2_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), CLASSIF2_SORT = CLA_SORT),by ="Classif2_Version_Item_Code") %>%
		left_join(select(FLAG, Flag_Code = CLT_COLUMN_CODE, Flag_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Flag_Code") %>%
		select(-Sex_Version_Code,-Classif1_Version_Code,-Classif2_Version_Code) %>%
		mutate(	Lang = tolower(Lang)) %>%
		select(-Topic_code,-COUNTRY_SORT,-SOURCE_SORT,-SURVEY_SORT,-TOPIC_SORT,-INDICATOR_SORT,-SEX_TYPE_SORT, -CLASSIF1_TYPE_SORT, -CLASSIF2_TYPE_SORT, -CLASSIF1_VERSION_SORT, -CLASSIF2_VERSION_SORT, -TIME_ID, -SEX_SORT, -CLASSIF1_SORT, -CLASSIF2_SORT)
		

COL_X <- c(
#"Lang",
"Country_Code",
#"Country_Label",
"Collection_Code",
#"Collection_Label",
"Indicator_Code",
#"Indicator_Label",
#"Unit_Measure_Code",
#"Unit_Measure_Label",
#"Unit_Multiplier_Code",
#"Unit_Multiplier_Label",
# "Currency_Code",
# "Currency_Label",
"Source_Code",
#"Source_Label",
"Survey_Code",
#"Survey_Label",
"Sex_Item_Code",
#"Sex_Item_Label",
"Classif1_Version_Item_Code",
#"Classif1_Version_Label",
#"Classif1_Item_Label",
"Classif2_Version_Item_Code",
#"Classif2_Version_Label",
#"Classif2_Item_Label",
"Time",
"Obs_Value",
"Flag_Code",
#"Flag_Label",
"Notes_Source_Code",
#"Notes_Source_Label",
"Notes_Indicator_Code",
#"Notes_Indicator_Label",
"Notes_Classif_Code"
#"Notes_Classif_Label"
)

X <- X %>% select(!!COL_X) %>% 
			rename(collection = Collection_Code, ref_area = Country_Code, indicator = Indicator_Code, source = Source_Code, 
					sex = Sex_Item_Code, classif1 = Classif1_Version_Item_Code, classif2 = Classif2_Version_Item_Code, 
					time = Time, 
					obs_value = Obs_Value,
					obs_status = Flag_Code, 
					note_source = Notes_Source_Code, 
					note_indicator = Notes_Indicator_Code,
					note_classif = Notes_Classif_Code
					) %>% 
			mutate(source = paste0(source, ':', Survey_Code)) %>% select(-Survey_Code)


detach(CODE_ORA)

require(ilo)
init_ilo(-cl)

X %>% switch_ilo(keep)




return(X)

}

#' @export

plyDataUpload <- function(X, DELETE = FALSE, LABEL = FALSE){

X <- X %>% 	mutate_each(funs(as.character), everything()) %>%
			mutate(Value = as.numeric(Value))
			
if(LABEL){
X <- X %>% 	
		mutate(			# create noteQtable by combining notes source code and note indicator code
					Qtable_Notes_String = ifelse(!Notes_Source_Code%in%NA,paste(Notes_Source_Code,Notes_Indicator_Code,sep="_"),Notes_Indicator_Code),
					Qtable_Notes_String =  gsub("NA_","",Qtable_Notes_String),
					Qtable_Notes_String =  gsub("_NA","",Qtable_Notes_String)) %>%  
		select(			# delete unuse columns
					-Notes_Source_Code,-Notes_Indicator_Code) %>%	
		mutate(		Flag = as.character(Value_Status_Code), 
					Currency = ifelse(!Currency_Code%in%NA,as.character(substr(Currency_Code,5,nchar(Currency_Code))), Currency_Code),
					Value_Notes_String 	= plyr:::mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= plyr:::mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Label_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_", Lang = "EN")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Label_notesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_", Lang = "EN")))
}
					
			
					
if(!LABEL){
X <- X %>% 	
		mutate(			# create noteQtable by combining notes source code and note indicator code
					Qtable_Notes_String = ifelse(!Notes_Source_Code%in%NA,paste(Notes_Source_Code,Notes_Indicator_Code,sep="_"),Notes_Indicator_Code),
					Qtable_Notes_String =  gsub("NA_","",Qtable_Notes_String),
					Qtable_Notes_String =  gsub("_NA","",Qtable_Notes_String)) %>%
		select(			# delete unuse columns
					-Notes_Source_Code,-Notes_Indicator_Code) %>%	
		mutate(		Flag = as.character(Value_Status_Code),
					Currency = ifelse(!Currency_Code%in%NA,as.character(substr(Currency_Code,5,nchar(Currency_Code))), Currency_Code),
					Value_Notes_String 	= plyr:::mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= plyr:::mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Transform_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_")),
					Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Transform_notesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")))
					
}



X <- X 	%>% mutate(	Lang = "EN",
					Country_Code = as.character(Country_Code),
					Country_Label = as.character(NA),
					Collection_Code = as.character(Collection_Code),
					Collection_Label = as.character(NA),
					Indicator_Code = as.character(Indicator_Code),
					Indicator_Label = as.character(NA),
					Survey_Code = as.character(Survey_Id),
					Survey_Label = as.character(NA),
					Sex_Version_Code  = as.character(Sex_Version_Code),
					Classif1_Version_Code = as.character(Classif1_Version_Code),
					Classif2_Version_Code = as.character(Classif2_Version_Code),
					Classif3_Version_Code = as.character(Classif3_Version_Code),
					Classif4_Version_Code = as.character(Classif4_Version_Code),
					Classif5_Version_Code = as.character(Classif5_Version_Code),
					Sex_Item_Code = as.character(Sex_Code),
					Sex_Item_Label = as.character(NA),
					Classif1_Item_Code = as.character(Classif1_Code),
					Classif1_Item_Label = as.character(NA),
					Classif2_Item_Code = as.character(Classif2_Code),
					Classif2_Item_Label = as.character(NA),
					Classif3_Item_Code = as.character(Classif3_Code),
					Classif3_Item_Label = as.character(NA),
					Classif4_Item_Code = as.character(Classif4_Code),
					Classif4_Item_Label = as.character(NA),
					Classif5_Item_Code = as.character(Classif5_Code),
					Classif5_Item_Label = as.character(NA),
					Freq = as.character(Freq_Code),
					Time  = as.character(Time),
					Obs_Value = as.numeric(Value),
					Free_Text_Notes = as.character(NA)
			)%>% 
			select(!!c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					

if(DELETE){


X <- X %>% 	group_by(Country_Code,Collection_Code,Indicator_Code,Survey_Code,Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,Classif3_Version_Code,Classif4_Version_Code,Classif5_Version_Code,Time) %>%
			summarise(Lang = first(as.character(Lang)), 
						Country_Label = as.character(NA), 
						Collection_Label = as.character(NA), 
						Indicator_Label = as.character(NA),  
						Survey_Label = as.character(NA),  
						Sex_Item_Code = as.character(NA), 
						Sex_Item_Label = as.character(NA), 
						Classif1_Item_Code = as.character(NA), 
						Classif1_Item_Label = as.character(NA), 
						Classif2_Item_Code = as.character(NA), 
						Classif2_Item_Label = as.character(NA), 
						Classif3_Item_Code = as.character(NA), 
						Classif3_Item_Label = as.character(NA), 
						Classif4_Item_Code = as.character(NA), 
						Classif4_Item_Label = as.character(NA), 
						Classif5_Item_Code = as.character(NA),  
						Classif5_Item_Label = as.character(NA), 
						Freq =  as.character(NA), 
						Obs_Value = as.character(NA), 
						Flag = as.character(NA), 
						Currency = as.character(NA), 
						Value_Notes_String = as.character(NA), 
						Qtable_Notes_String = as.character(NA), 
						Free_Text_Notes = as.character(NA)) %>% 
			ungroup()  %>% 
			select(!!c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					
}
else{X}

}

#' @export

plyDataDownload <- function(X, LANG = "EN"){
X %>% select(-Sex_Version_Code) %>%
		rename(	Obs_Value = Value,	
				Sex_Item_Code = Sex_Code,		
				Classif1_Version_Item_Code = Classif1_Code,
				Classif2_Version_Item_Code = Classif2_Code,
				Classif3_Version_Item_Code = Classif3_Code,
				Classif4_Version_Item_Code = Classif4_Code,
				Classif5_Version_Item_Code = Classif5_Code,
				Survey_Code = Survey_Id,
				Flag_Code = Value_Status_Code) %>%
		mutate( Classif1_Version_Label =		plyr:::mapvalues(Classif1_Version_Code, 			from = 	levels(as.factor(Classif1_Version_Code)), 
																							to = 	select(CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE, Classif1_Version_Label = eval(parse(text=paste0("CLV_TEXT_",LANG)))) %>% filter(CLV_CODE %in% levels(Classif1_Version_Code)) %>% select(Classif1_Version_Label) %>% t %>% as.character)) %>%
		select(-Classif1_Version_Code) %>%
		mutate( Classif2_Version_Label =		plyr:::mapvalues(Classif2_Version_Code, 			from = 	levels(as.factor(Classif2_Version_Code)), 
																							to = 	select(CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE, Classif2_Version_Label = eval(parse(text=paste0("CLV_TEXT_",LANG)))) %>% filter(CLV_CODE %in% levels(Classif2_Version_Code)) %>% select(Classif2_Version_Label) %>% t %>% as.character)) %>%
		select(-Classif2_Version_Code) %>%
		mutate( Classif3_Version_Label =		plyr:::mapvalues(Classif3_Version_Code, 			from = 	levels(as.factor(Classif3_Version_Code)), 
																							to = 	select(CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE, Classif3_Version_Label = eval(parse(text=paste0("CLV_TEXT_",LANG)))) %>% filter(CLV_CODE %in% levels(Classif3_Version_Code)) %>% select(Classif3_Version_Label) %>% t %>% as.character)) %>%
		select(-Classif3_Version_Code) %>%
		mutate( Classif4_Version_Label =		plyr:::mapvalues(Classif4_Version_Code, 			from = 	levels(as.factor(Classif4_Version_Code)), 
																							to = 	select(CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE, Classif4_Version_Label = eval(parse(text=paste0("CLV_TEXT_",LANG)))) %>% filter(CLV_CODE %in% levels(Classif4_Version_Code)) %>% select(Classif4_Version_Label) %>% t %>% as.character)) %>%
		select(-Classif4_Version_Code) %>%
		mutate( Classif5_Version_Label =		plyr:::mapvalues(Classif5_Version_Code, 			from = 	levels(as.factor(Classif5_Version_Code)), 
																							to = 	select(CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE, Classif5_Version_Label = eval(parse(text=paste0("CLV_TEXT_",LANG)))) %>% filter(CLV_CODE %in% levels(Classif5_Version_Code)) %>% select(Classif5_Version_Label) %>% t %>% as.character)) %>%
		select(-Classif5_Version_Code) %>%
		mutate(	Notes_Source_Label = 	plyr:::mapvalues(Notes_Source_Code,		from = 	levels(as.factor(Notes_Source_Code)), 
																			to = 	My_Label_notesJ(levels(as.factor(Notes_Source_Code)),SEP = "_", Lang = LANG)),
				Notes_Indicator_Label = plyr:::mapvalues(Notes_Indicator_Code, 	from = 	levels(as.factor(Notes_Indicator_Code)), 
																			to = 	My_Label_notesJ(levels(as.factor(Notes_Indicator_Code)),SEP = "_", Lang = LANG)),
				Notes_Classif_Label = 	plyr:::mapvalues(Notes_Classif_Code,		from = 	levels(as.factor(Notes_Classif_Code)), 
																			to = 	My_Label_notesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_", Lang = LANG)),	
				Country_Label =			plyr:::mapvalues(Country_Code, 			from = 	levels(as.factor(Country_Code)), 
																			to = 	select(CODE_ORA$T_COU_COUNTRY, COU_ISO3_CODE, Country_Label = eval(parse(text=paste0("COU_TEXT_",LANG)))) %>% filter(COU_ISO3_CODE %in% levels(Country_Code)) %>% select(Country_Label) %>% t %>% as.character), 
				Collection_Label =		plyr:::mapvalues(Collection_Code, 			from = 	levels(as.factor(Collection_Code)), 
																			to = 	select(CODE_ORA$T_COL_COLLECTION, COL_CODE, Collection_Label = eval(parse(text=paste0("COL_TEXT_",LANG)))) %>% filter(COL_CODE %in% levels(Collection_Code)) %>% select(Collection_Label) %>% t %>% as.character), 
				Indicator_Label =		plyr:::mapvalues(Indicator_Code, 			from = 	levels(as.factor(Indicator_Code)), 
																			to = 	select(CODE_ORA$T_IND_INDICATOR, IND_CODE, Indicator_Label = eval(parse(text=paste0("IND_TEXT_",LANG)))) %>% filter(IND_CODE %in% levels(Indicator_Code)) %>% select(Indicator_Label) %>% t %>% as.character), 
				Source_Label =			plyr:::mapvalues(Source_Code, 				from = 	levels(as.factor(Source_Code)), 
																			to = 	select(CODE_ORA$T_SRC_SOURCE, SRC_CODE, Source_Label = eval(parse(text=paste0("SRC_TEXT_",LANG)))) %>% filter(SRC_CODE %in% levels(Source_Code)) %>% select(Source_Label) %>% t %>% as.character), 
				Survey_Label =			plyr:::mapvalues(Survey_Code, 				from = 	levels(as.factor(Survey_Code)), 
																			to = 	select(CODE_ORA$T_SUR_SURVEY, SUR_ID, Survey_Label = eval(parse(text=paste0("SUR_SURVEY_TITLE_",LANG)))) %>% filter(SUR_ID %in% levels(Survey_Code)) %>% select(Survey_Label) %>% t %>% as.character), 
				Currency_Label =		plyr:::mapvalues(Currency_Code, 			from = 	levels(as.factor(Currency_Code)), 
																			to = 	select(CODE_ORA$T_CUR_CURRENCY, CUR_ID, Currency_Label = eval(parse(text=paste0("CUR_TEXT_",LANG)))) %>% filter(CUR_ID %in% levels(Currency_Code)) %>% select(Currency_Label) %>% t %>% as.character), 
				Sex_Item_Label =		plyr:::mapvalues(Sex_Item_Code, 			from = 	levels(as.factor(Sex_Item_Code)), 
																			to = 	select(CODE_ORA$T_CLA_CLASSIF, CLA_CODE, Sex_Item_Label = eval(parse(text=paste0("CLA_TEXT_",LANG)))) %>% filter(CLA_CODE %in% levels(Sex_Item_Code)) %>% select(Sex_Item_Label) %>% t %>% as.character), 
				Classif1_Item_Label =	plyr:::mapvalues(Classif1_Version_Item_Code, 	from = 	levels(as.factor(Classif1_Version_Item_Code)), 
																				to = 	select(CODE_ORA$T_CLA_CLASSIF, CLA_CODE, Classif1_Item_Label = eval(parse(text=paste0("CLA_TEXT_",LANG)))) %>% filter(CLA_CODE %in% levels(Classif1_Version_Item_Code)) %>% select(Classif1_Item_Label) %>% t %>% as.character), 
				Classif2_Item_Label =	plyr:::mapvalues(Classif2_Version_Item_Code, 	from = 	levels(as.factor(Classif2_Version_Item_Code)), 
																				to = 	select(CODE_ORA$T_CLA_CLASSIF, CLA_CODE, Classif2_Item_Label = eval(parse(text=paste0("CLA_TEXT_",LANG)))) %>% filter(CLA_CODE %in% levels(Classif2_Version_Item_Code)) %>% select(Classif2_Item_Label) %>% t %>% as.character), 
				Classif3_Item_Label =	plyr:::mapvalues(Classif3_Version_Item_Code, 	from = 	levels(as.factor(Classif3_Version_Item_Code)), 
																				to = 	select(CODE_ORA$T_CLA_CLASSIF, CLA_CODE, Classif3_Item_Label = eval(parse(text=paste0("CLA_TEXT_",LANG)))) %>% filter(CLA_CODE %in% levels(Classif3_Version_Item_Code)) %>% select(Classif3_Item_Label) %>% t %>% as.character), 
				Classif4_Item_Label =	plyr:::mapvalues(Classif4_Version_Item_Code, 	from = 	levels(as.factor(Classif4_Version_Item_Code)), 
																				to = 	select(CODE_ORA$T_CLA_CLASSIF, CLA_CODE, Classif4_Item_Label = eval(parse(text=paste0("CLA_TEXT_",LANG)))) %>% filter(CLA_CODE %in% levels(Classif4_Version_Item_Code)) %>% select(Classif4_Item_Label) %>% t %>% as.character), 
				Classif5_Item_Label =	plyr:::mapvalues(Classif5_Version_Item_Code, 	from = 	levels(as.factor(Classif5_Version_Item_Code)), 
																				to = 	select(CODE_ORA$T_CLA_CLASSIF, CLA_CODE, Classif5_Item_Label = eval(parse(text=paste0("CLA_TEXT_",LANG)))) %>% filter(CLA_CODE %in% levels(Classif5_Version_Item_Code)) %>% select(Classif5_Item_Label) %>% t %>% as.character), 
				Flag_Label =			plyr:::mapvalues(Flag_Code, 				from = 	levels(as.factor(Flag_Code)), 
																			to = 	select(CODE_ORA$T_CLT_CODELIST %>% filter(CLT_COLUMN_NAME%in%"VALUE_STATUS"), CLT_COLUMN_CODE, Flag_Label = eval(parse(text=paste0("CLT_TEXT_",LANG)))) %>% filter(CLT_COLUMN_CODE %in% levels(Flag_Code)) %>% select(Flag_Label) %>% t %>% as.character), 
				Lang = 					as.factor(tolower(LANG))) %>%

		left_join(select(T_IND_INDICATOR, Indicator_Code = IND_CODE, Indicator_Label = eval(parse(text=paste0("IND_TEXT_",Lang))),INDICATOR_SORT = IND_SORT, Unit_Measure_Id= IND_UNIT_MEASURE_ID, Unit_Multiplier_Id = IND_UNIT_MULT_ID ),by ="Indicator_Code") %>%
		left_join(select(T_CLT_CODELIST, Unit_Measure_Id = CLT_ID, Unit_Measure_Code = CLT_COLUMN_CODE ,Unit_Measure_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Unit_Measure_Id") %>% select(-Unit_Measure_Id) %>%
		left_join(select(T_CLT_CODELIST, Unit_Multiplier_Id = CLT_ID, Unit_Multiplier_Code = CLT_COLUMN_CODE ,Unit_Multiplier_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Unit_Multiplier_Id") %>% select(-Unit_Multiplier_Id) %>%
			select(
				Lang,
				Country_Code,
				Country_Label,
				Collection_Code,
				Collection_Label,
				Indicator_Code,
				Indicator_Label,				
				Unit_Measure_Code,
				Unit_Measure_Label,
				Unit_Multiplier_Code,
				Unit_Multiplier_Label,
				Currency_Code,
				Currency_Label,
				Source_Code,
				Source_Label,
				Survey_Code,
				Survey_Label,
				Sex_Item_Code,
				Sex_Item_Label,				
				Classif1_Version_Item_Code,
				Classif1_Version_Label,
				Classif1_Item_Label,
				Classif2_Version_Item_Code,
				Classif2_Version_Label,
				Classif2_Item_Label,
				Classif3_Version_Item_Code,
				Classif3_Version_Label,
				Classif3_Item_Label,
				Classif4_Version_Item_Code,
				Classif4_Version_Label,
				Classif4_Item_Label,
				Classif5_Version_Item_Code,
				Classif5_Version_Label,
				Classif5_Item_Label,
				Time,
				Obs_Value,
				Flag_Code,
				Flag_Label,
				Notes_Source_Code,
				Notes_Source_Label,
				Notes_Indicator_Code,
				Notes_Indicator_Label,
				Notes_Classif_Code,
				Notes_Classif_Label
		)
}

#' @export

Clean_col_format <- function(X, col_names =  c("Country_Code", "Collection_Code", "Indicator_Code", "Source_Code", "Survey_Id", "Sex_Version_Code", "Classif1_Version_Code", "Classif2_Version_Code", "Classif3_Version_Code", "Classif4_Version_Code", "Classif5_Version_Code", "Sex_Code", "Classif1_Code", "Classif2_Code", "Classif3_Code", "Classif4_Code", "Classif5_Code", "Freq_Code", "Time", "Value", "Value_Status_Code", "Currency_Code", "Notes_Source_Code", "Notes_Topic_Code", "Notes_Indicator_Code", "Notes_Classif_Code")){

	X <- X[,colnames(X)[colnames(X)%in%col_names]]
	for (i in 1:length(col_names)){
		if(!col_names[i]%in%colnames(X)){
			X <- eval(parse(text= paste0("  X %>% mutate(",col_names[i]," = 	as.character(NA))"))) 
		}
	}
	X %>% select(!!col_names)
}

#' @export

My_Group_file_for_Oracle_NEW <- function(MODE = "NEW", Collection="STI", files=FALSE, wd){

# MODE <- "REV"; Collection <- "STI"; files <- FALSE 
# MODE <- "DEL"; Collection <- "EUROSTAT"; files <- FALSE 



	my_list <-  list.files(paste0(wd, 'ILO_Data/ON_ORACLE_To_Upload_By_Country/'))
	key_qtable     <- 	c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time")


	key_ALL     	<- 	c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","Sex_Item_Code","Classif1_Item_Code","Classif2_Item_Code")
	KEY_ORACLE 	<- c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes")

	test <- as.data.frame(cbind(country = my_list, BinSize = as.numeric(file.info(paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Country/",my_list))$size) /1000 ,obj.size = 0, records = 0 ),stringsAsFactors=FALSE) 

	cutOff <- 600000
	NAMEfile <- 1

	test <- test[substr(test$country,1,3)%in%MODE,]

	if(is.character(files)){
		test <- test[substr(test$country,5,7)%in%files,]
	}


	if(!plyr:::empty(test)){
		for (i in 1:nrow(test)){
			load(paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Country/",test[i,"country"]))
			X <- X %>% 	mutate_all(.funs = as.character) %>% 
						mutate(Value = as.numeric(Value)) %>% 
						filter(!(Classif1_Version_Code%in%NA & !Classif2_Version_Code%in%NA))

			if(Collection%in%"STI"){
				X <- X %>% 	mutate( Lang = "EN", 
									Collection_Code ="STI",
									Freq = "A",
									Freq = ifelse(substr(Time,5,5)%in%"Q","Q",Freq),
									Freq = ifelse(substr(Time,5,5)%in%"M","M",Freq)) %>%
							rename(	Obs_Value = Value, 
									Survey_Code = Source_Code,
									Sex_Item_Code = Sex_Code, 
									Classif1_Item_Code = Classif1_Code,
									Classif2_Item_Code = Classif2_Code,
									# Currency = Currency_Code, 
									Flag = Value_Status_Code
									)
		
		
				X <- Ariane::Clean_col_format(X, col_names = KEY_ORACLE)

				X <- X  %>% select(!!KEY_ORACLE)

			}

			X <- X %>% 	mutate_all(.funs = as.character ) #%>%
						#mutate_all(funs(plyr:::mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))) 
						#mutate_all(funs(recode(.,'NaN' = NA, '' = NA, ' ' = NA, 'NA' = NA))) 



			print(unique(X$Country_Code))

			if(i==1){Y <- X} else {Y <- bind_rows(Y,X) }; rm(X)

			print(paste(nrow(Y)))

			########## just sorting
			Y$ID <- do.call("paste", c(Y[key_ALL], sep = "/"))
			Y <- Y[order(Y$ID),]
			Y <- Y[,!colnames(Y)%in%"ID"]
			Y <- Y  %>%  select(!!KEY_ORACLE)

			if(floor(nrow(Y)/cutOff)>0){
				for (j in 1:floor(nrow(Y)/cutOff)){

					TO_UP <- Y %>% slice(1:cutOff)

					# Qtable key
					TO_UP$ID <- do.call("paste", c(TO_UP[key_qtable], sep = "/"))

					# delete last Qtable
					REF <- TO_UP[cutOff,"ID"]
					TO_UP <- TO_UP[!TO_UP$ID%in%REF,]


						if(MODE%in%"DEL"){
							ref_names <- c('collection', 'ref_area', 'source', 'indicator', 'sex_version', 'classif1_version', 'classif2_version',  'time')
							TO_UP <- TO_UP %>% 	mutate(test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																			to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
												unite(source, test, Survey_Code, sep = ':', remove = TRUE) %>% 
										 select(!!c("Collection_Code", "Country_Code", "source", "Indicator_Code",  "Sex_Version_Code", "Classif1_Version_Code", "Classif2_Version_Code", "Time"))
			
							colnames(TO_UP) <- ref_names
									TO_UP <- TO_UP %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version,  time)

						}else{				
							TO_UP <-  TO_UP %>% 
										select(!!c( "Country_Code", "Collection_Code", "Indicator_Code", "Survey_Code",  "Sex_Item_Code", "Classif1_Item_Code", "Classif2_Item_Code", "Time", "Obs_Value", "Flag", "Value_Notes_String", "Qtable_Notes_String")) %>% 
										rename(	collection = Collection_Code, 
												ref_area = Country_Code, 
												indicator = Indicator_Code, 
												sex = Sex_Item_Code, 
												classif1 = Classif1_Item_Code, 
												classif2 = Classif2_Item_Code, 
												time = Time, 
												obs_value = Obs_Value, 
												obs_status = Flag, 
												note_classif  = Value_Notes_String) %>% 
										mutate(	Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '_'), 
														Qtable_Notes_String = Qtable_Notes_String %>% Ariane::My_Resort_Notes_Type(SEP = '_', addsep  =TRUE)) %>% 
										mutate(Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '/')) %>% 
										separate(col = Qtable_Notes_String, into = c('note_source','note_indicator'), sep  = stringr::fixed("/"), remove = TRUE, fill = 'left') %>% 
										mutate(test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																						to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
										unite(source, test, Survey_Code, sep = ':', remove = TRUE)
						}					
					
					
						if(!plyr:::empty(TO_UP)){
							data.table:::fwrite(TO_UP ,paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Group/",MODE,"_",Collection,"_",NAMEfile,"_",Sys.Date(),".csv"))
						Y <- Y %>% slice(-c(1:nrow(TO_UP)))
						}

						rm(TO_UP,REF)
						NAMEfile <- NAMEfile + 1
				}
			}

			if(floor(nrow(Y)/cutOff)==0 & i== nrow(test) ){
				TO_UP <- Y
				# Qtable key
				TO_UP$ID <- do.call("paste", c(TO_UP[key_qtable], sep = "/"))


				if(MODE%in%"DEL"){
							ref_names <- c('collection', 'ref_area', 'source', 'indicator', 'sex_version', 'classif1_version', 'classif2_version',  'time')
							TO_UP <- TO_UP %>% 	mutate(test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																			to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
												unite(source, test, Survey_Code, sep = ':', remove = TRUE) %>% 
										select(!!c("Collection_Code", "Country_Code", "source", "Indicator_Code",  "Sex_Version_Code", "Classif1_Version_Code", "Classif2_Version_Code", "Time"))
			
							colnames(TO_UP) <- ref_names
							TO_UP <- TO_UP %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version,  time)

					print(paste(nrow(TO_UP)))
				} else{				
					TO_UP <-  TO_UP %>% 
								select(!!c( "Country_Code", "Collection_Code", "Indicator_Code", "Survey_Code",  "Sex_Item_Code", "Classif1_Item_Code", "Classif2_Item_Code", "Time", "Obs_Value", "Flag", "Value_Notes_String", "Qtable_Notes_String")) %>% 
								rename(	collection = Collection_Code, 
										ref_area = Country_Code, 
										indicator = Indicator_Code, 
										sex = Sex_Item_Code, 
										classif1 = Classif1_Item_Code, 
										classif2 = Classif2_Item_Code, 
										time = Time, 
										obs_value = Obs_Value, 
										obs_status = Flag, 
										note_classif  = Value_Notes_String) %>% 
								mutate(	Qtable_Notes_String = Qtable_Notes_String %>% My_Resort_Notes_Type(SEP = '_',addsep  =TRUE)) %>% 
								mutate(	Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '/')) %>% 
								separate(col = Qtable_Notes_String, into = c('note_source','note_indicator'), sep  = stringr::fixed("/"), remove = TRUE, fill = 'left') %>% 
								mutate(	test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																				to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
								unite(	source, test, Survey_Code, sep = ':', remove = TRUE)

				}
				if(!plyr:::empty(TO_UP)){
					data.table:::fwrite(TO_UP  ,paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Group/",MODE,"_",Collection,"_",NAMEfile,"_",Sys.Date(),".csv"))
				}
			}
		}
	}else{print("No file")}

}

#' @export

My_Group_file_for_Oracle <- function(MODE = "NEW", Collection="STI", files=FALSE, wd){

# MODE <- "REV"; Collection <- "STI"; files <- FALSE 
# MODE <- "DEL"; Collection <- "EUROSTAT"; files <- FALSE 



	my_list <-  list.files(paste0(wd, 'ILO_Data/ON_ORACLE_To_Upload_By_Country/'))
	key_qtable     <- 	c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time")


	key_ALL     	<- 	c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","Sex_Item_Code","Classif1_Item_Code","Classif2_Item_Code")
	KEY_ORACLE 	<- c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes")

	test <- as.data.frame(cbind(country = my_list, BinSize = as.numeric(file.info(paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Country/",my_list))$size) /1000 ,obj.size = 0, records = 0 ),stringsAsFactors=FALSE) 

	cutOff <- 600000
	NAMEfile <- 1

	test <- test[substr(test$country,1,3)%in%MODE,]

	if(is.character(files)){
		test <- test[substr(test$country,5,7)%in%files,]
	}


	if(!plyr:::empty(test)){
		for (i in 1:nrow(test)){
			load(paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Country/",test[i,"country"]))
			X <- X %>% 	mutate_all(as.character) %>% 
						mutate(Value = as.numeric(Value)) %>% 
						filter(!(Classif1_Version_Code%in%NA & !Classif2_Version_Code%in%NA))

			if(Collection%in%"STI"){
				X <- X %>% 	mutate( Lang = "EN", 
									Collection_Code ="STI",
									Freq = "A",
									Freq = ifelse(substr(Time,5,5)%in%"Q","Q",Freq),
									Freq = ifelse(substr(Time,5,5)%in%"M","M",Freq)) %>%
							rename(	Obs_Value = Value, 
									Survey_Code = Source_Code,
									Sex_Item_Code = Sex_Code, 
									Classif1_Item_Code = Classif1_Code,
									Classif2_Item_Code = Classif2_Code,
									# Currency = Currency_Code, 
									Flag = Value_Status_Code
									)
		
		
				X <- Ariane::Clean_col_format(X, col_names = KEY_ORACLE)

				X <- X  %>% select(!!KEY_ORACLE)

			}

			X <- X %>% 	mutate_all(as.character ) #%>%
						#mutate_all(funs(plyr:::mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))) 
						#mutate_all(funs(recode(.,'NaN' = NA, '' = NA, ' ' = NA, 'NA' = NA))) 



			print(unique(X$Country_Code))

			if(i==1){Y <- X} else {Y <- bind_rows(Y,X) }; rm(X)

			print(paste(nrow(Y)))

			########## just sorting
			Y$ID <- do.call("paste", c(Y[key_ALL], sep = "/"))
			Y <- Y[order(Y$ID),]
			Y <- Y[,!colnames(Y)%in%"ID"]
			Y <- Y  %>% select(!!KEY_ORACLE)

			if(floor(nrow(Y)/cutOff)>0){
				for (j in 1:floor(nrow(Y)/cutOff)){

					TO_UP <- Y %>% slice(1:cutOff)

					# Qtable key
					TO_UP$ID <- do.call("paste", c(TO_UP[key_qtable], sep = "/"))

					# delete last Qtable
					REF <- TO_UP[cutOff,"ID"]
					TO_UP <- TO_UP[!TO_UP$ID%in%REF,]


						if(MODE%in%"DEL"){
							ref_names <- c('collection', 'ref_area', 'source', 'indicator', 'sex_version', 'classif1_version', 'classif2_version',  'time')
							TO_UP <- TO_UP %>% 	mutate(test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																			to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
												unite(source, test, Survey_Code, sep = ':', remove = TRUE) %>% 
										select(!!c("Collection_Code", "Country_Code", "source", "Indicator_Code",  "Sex_Version_Code", "Classif1_Version_Code", "Classif2_Version_Code", "Time"))
			
							colnames(TO_UP) <- ref_names
									TO_UP <- TO_UP %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version,  time)

						}else{				
							TO_UP <-  TO_UP %>% 
										select(!!KEY_ORACLE) %>% 
										select(!!c( "Country_Code", "Collection_Code", "Indicator_Code", "Survey_Code",  "Sex_Item_Code", "Classif1_Item_Code", "Classif2_Item_Code", "Time", "Obs_Value", "Flag", "Value_Notes_String", "Qtable_Notes_String")) %>% 
										rename(	collection = Collection_Code, 
												ref_area = Country_Code, 
												indicator = Indicator_Code, 
												sex = Sex_Item_Code, 
												classif1 = Classif1_Item_Code, 
												classif2 = Classif2_Item_Code, 
												time = Time, 
												obs_value = Obs_Value, 
												obs_status = Flag, 
												note_classif  = Value_Notes_String) %>% 
										mutate(	Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '_'), 
														Qtable_Notes_String = Qtable_Notes_String %>% Ariane::My_Resort_Notes_Type(SEP = '_', addsep  =TRUE)) %>% 
										mutate(Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '/')) %>% 
										separate(col = Qtable_Notes_String, into = c('note_source','note_indicator'), sep  = stringr::fixed("/"), remove = TRUE, fill = 'left') %>% 
										mutate(test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																						to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
										unite(source, test, Survey_Code, sep = ':', remove = TRUE)
						}					
					
					
						if(!plyr:::empty(TO_UP)){
							data.table:::fwrite(TO_UP ,paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Group/",MODE,"_",Collection,"_",NAMEfile,"_",Sys.Date(),".csv"))
						Y <- Y %>% slice(-c(1:nrow(TO_UP)))
						}

						rm(TO_UP,REF)
						NAMEfile <- NAMEfile + 1
				}
			}

			if(floor(nrow(Y)/cutOff)==0 & i== nrow(test) ){
				TO_UP <- Y
				# Qtable key
				TO_UP$ID <- do.call("paste", c(TO_UP[key_qtable], sep = "/"))


				if(MODE%in%"DEL"){
							ref_names <- c('collection', 'ref_area', 'source', 'indicator', 'sex_version', 'classif1_version', 'classif2_version',  'time')
							TO_UP <- TO_UP %>% 	mutate(test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																			to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
												unite(source, test, Survey_Code, sep = ':', remove = TRUE) %>% 
										select(!!c("Collection_Code", "Country_Code", "source", "Indicator_Code",  "Sex_Version_Code", "Classif1_Version_Code", "Classif2_Version_Code", "Time"))
			
							colnames(TO_UP) <- ref_names
							TO_UP <- TO_UP %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version,  time)

					print(paste(nrow(TO_UP)))
				} else{				
					TO_UP <-  TO_UP %>% 
								select(!!c( "Country_Code", "Collection_Code", "Indicator_Code", "Survey_Code",  "Sex_Item_Code", "Classif1_Item_Code", "Classif2_Item_Code", "Time", "Obs_Value", "Flag", "Value_Notes_String", "Qtable_Notes_String")) %>% 
								rename(	collection = Collection_Code, 
										ref_area = Country_Code, 
										indicator = Indicator_Code, 
										sex = Sex_Item_Code, 
										classif1 = Classif1_Item_Code, 
										classif2 = Classif2_Item_Code, 
										time = Time, 
										obs_value = Obs_Value, 
										obs_status = Flag, 
										note_classif  = Value_Notes_String) %>% 
								mutate(	# Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '_'), 
										Qtable_Notes_String = Qtable_Notes_String %>% My_Resort_Notes_Type(SEP = '_',addsep  =TRUE)) %>% 
								mutate(	Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '/')) %>% 
								separate(col = Qtable_Notes_String, into = c('note_source','note_indicator'), sep  = stringr::fixed("/"), remove = TRUE, fill = 'left') %>% 
								mutate(	test = Survey_Code %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																				to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
								unite(	source, test, Survey_Code, sep = ':', remove = TRUE) # %>% 
										# mutate( note_indicator = ifelse(!Currency %in% NA, paste0('T30:',Currency, '_',note_indicator), note_indicator), 
										#		note_indicator = ifelse(stringr::str_sub(note_indicator, -1,-1)%in% '_', stringr::str_sub(note_indicator, 1,-2), note_indicator), 
										#		obs_value = as.numeric(obs_value))  %>% 
										# select(-Currency)

				}
				if(!plyr:::empty(TO_UP)){
					data.table:::fwrite(TO_UP  ,paste0(wd, "ILO_Data/ON_ORACLE_To_Upload_By_Group/",MODE,"_",Collection,"_",NAMEfile,"_",Sys.Date(),".csv"))
				}
			}
		}
	}else{print("No file")}

}


#' @export

Test_on_YI <- function(name_file, selectTime, Survey){

# name_file <- 'NZL'; selectTime <- '2006'; Survey <- '117'

	key_qtable     <- 	c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time")
	key_ALL     	<- 	c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","Sex_Item_Code","Classif1_Item_Code","Classif2_Item_Code","Classif3_Item_Code","Classif4_Item_Code","Classif5_Item_Code")
	KEY_ORACLE 	<- c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes")


replacetime <- 	1939:1989
replacetime <- replacetime[1:length(selectTime)]


	load(paste0("./Processing/ILO_Data/ON_STI/",name_file,".Rdata"))

	X <- X %>% 	mutate_each(funs(as.character), everything()) %>% 
				mutate(Value = as.numeric(Value)) %>% 
				filter(!(Classif1_Version_Code%in%NA & !Classif2_Version_Code%in%NA)) %>%
				filter(Time %in%selectTime) %>% 	
				mutate( Lang = "EN", 
					Collection_Code ="YI",
					Freq = "A", 
					Country_Code = 'SSD') %>%
				rename(	Obs_Value = Value, 
					Survey_Code = Source_Code,
					Sex_Item_Code = Sex_Code, 
					Classif1_Item_Code = Classif1_Code,
					Classif2_Item_Code = Classif2_Code,
					Flag = Value_Status_Code,
					Currency = Currency_Code) %>% 
				filter(Survey_Code %in% Survey ) %>% 
				filter(!Classif1_Item_Code %in% 'ECO_SECTOR_NAG') %>% 
				mutate(Survey_Code = '2210') %>%
				mutate(Time = plyr:::mapvalues(Time,selectTime, replacetime, warn_missing = FALSE))


				
		
Y <- Ariane::Clean_col_format(X, col_names = KEY_ORACLE) %>% 
				select(!!KEY_ORACLE) %>% 	
				mutate_each(funs(as.character),everything() ) %>%
				mutate_each(funs(plyr:::mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE)), everything()) 

rm(X)				

########## just sorting
Y$ID <- do.call("paste", c(Y[key_ALL], sep = "/"))
Y <- Y[order(Y$ID),]
Y <- Y[,!colnames(Y)%in%"ID"]


Y <- Y  %>% select(!!KEY_ORACLE)




TEST_YI <- Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% "YI") %>% 
				mutate(CLASS = substr(CIC_INDICATOR_CODE, 10, nchar(CIC_INDICATOR_CODE) -3)) %>% 
				filter(!CLASS %in% c("SEX_ECO2", "SEX_OCU2", "ECO2", "ECO", "TOM", "EDU", "OCU", "QTL", "NOC", "STE_INS")) %>%
				separate(CLASS, c("SEX", "CL1","CL2", "CL3"), sep  ="_", extra = "drop", remove = FALSE)
ref <- TEST_YI %>% mutate(TEST = paste0(CIC_INDICATOR_CODE, "/", CIC_CLASSIF_VERSION_CODE)) %>% select(TEST) %>% distinct(TEST) %>% t %>% c  

ref <- c(ref, paste(TEST_YI$CIC_INDICATOR_CODE, "NA", sep = "/"))
	
Y <- Y %>% filter(Indicator_Code %in% unique(TEST_YI$CIC_INDICATOR_CODE))

Y <- Y %>% 	mutate(TEST1 = paste0(Indicator_Code, "/", Classif1_Version_Code)) %>% 
			filter(TEST1 %in% ref) %>% 
			mutate(TEST1 = paste0(Indicator_Code, "/", Classif2_Version_Code)) %>% 
			filter(TEST1 %in% ref) %>% 
			mutate(TEST1 = paste0(Indicator_Code, "/", Classif3_Version_Code)) %>% 
			filter(TEST1 %in% ref) %>% 
			mutate(TEST1 = paste0(Indicator_Code, "/", Classif4_Version_Code)) %>% 
			filter(TEST1 %in% ref) 
			

####### ADD notes

Y <- Y %>% mutate(Qtable_Notes_String = ifelse(substr(Indicator_Code,1,3) %in% "UNE", paste0(Qtable_Notes_String, "#10$NA$114$NA") , paste0(Qtable_Notes_String, "#10$NA") ) )
			
			
write.csv(Y %>% select(-TEST1) , paste("./Processing/ILO_Data/TEST_",name_file,"_",Sys.Date(),".csv",sep=""),quote = TRUE,fileEncoding  = "UTF-8",row.names = FALSE,na="")

return(cbind(repTime = replacetime, OriTime = selectTime ))

}

#' @export

save_csv <-	function(X, Name = "ILOEST", Cut = 250000){		
group_number = (function(){i = 0; function() i <<- i+1 })()

KEY_ORACLE 	<- c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes")
		
	X <- X %>% 	
			mutate_each(funs(as.character), -Value) %>% 
			PrepareUpload %>%
			Clean_col_format(col_names  = KEY_ORACLE) %>%		
			group_by(Collection_Code, Country_Code, Indicator_Code, Survey_Code, Sex_Version_Code, Classif1_Version_Code, Classif2_Version_Code, Classif3_Version_Code, Classif4_Version_Code, Classif5_Version_Code, Time)%>%
			mutate(ID = group_number()) %>% ungroup %>% arrange(ID)	
			
	for (i in 1:(floor(nrow(X)/Cut)+1)){ # save csv with less than 250 000 observations
	
		row_cut <- X %>% slice(ifelse(nrow(X)>Cut, Cut, nrow(X))) %>% select(ID) %>% t %>% c 
		X %>% 	filter(ID %in% min(ID):ifelse(nrow(.)>Cut, row_cut-1, row_cut)) %>% 
				Clean_col_format(col_names = KEY_ORACLE) %>% 
				write.csv(paste0("Output/",Name,"_",i,".csv"), na = "", row.names = FALSE)	
				
		X <- X %>% filter(! ID %in% min(ID):ifelse(nrow(.) >Cut, row_cut-1, row_cut))
		print(paste0("file:",Name,"_",i,".csv"))	
	}

}

#' @export
'%!in%' <- Negate('%in%')

#' @export
build_packages <- function(wd, mypackage, myvignette  = FALSE){ 
	library("devtools")
	library(roxygen2)

	setwd(paste0(wd,mypackage ))
	
	# documentation
	document()
	if(myvignette) devtools::build_vignettes()
	setwd("..")
	install(mypackage)

	}

#' @export	
backup_dpau <- function(sys){




repo <- 'Rilostat'
ref <- list.files(paste0(sys,repo)) %>% enframe(name = NULL) %>% filter(value %in% c("DESCRIPTION", "LICENSE", "NAMESPACE", "NEWS.md", "README.md" ))

for (i in 1:5){file.copy(from = paste0(sys,repo,'/', ref$value[i]), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/', ref$value[i]))}
file.copy(from = paste0(sys,repo,'/R/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)
#file.copy(from = paste0(sys,repo,'/inst/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)


# repo <- 'Artemis'

# ref <- list.files(paste0(sys,repo)) %>% enframe(name = NULL) %>% filter(value %in% c("DESCRIPTION", "LICENSE", "NAMESPACE", "NEWS.md", "README.md" ))
# for (i in 1:5){file.copy(from = paste0(sys,repo,'/', ref$value[i]), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/', ref$value[i]))}
# file.copy(from = paste0(sys,repo,'/R/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)
# file.copy(from = paste0(sys,repo,'/inst/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)

repo <- 'Ariane'
ref <- list.files(paste0(sys,repo)) %>% enframe(name = NULL) %>% filter(value %in% c("DESCRIPTION", "LICENSE", "NAMESPACE", "NEWS.md", "README.md" ))
for (i in 1:5){file.copy(from = paste0(sys,repo,'/', ref$value[i]), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/', ref$value[i]))}
file.copy(from = paste0(sys,repo,'/R/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)



repo <- 'ilo'
ref <- list.files(paste0(sys,repo)) %>% enframe(name = NULL) %>% filter(value %in% c("DESCRIPTION", "LICENSE", "NAMESPACE", "NEWS.md", "README.md" ))
for (i in 1:5){file.copy(from = paste0(sys,repo,'/', ref$value[i]), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/', ref$value[i]))}
file.copy(from = paste0(sys,repo,'/R/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)
file.copy(from = paste0(sys,repo,'/inst/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)
file.copy(from = paste0(sys,repo,'/vignettes/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)



repo <- 'iloMicro'
ref <- list.files(paste0(sys, repo)) %>% enframe(name = NULL) %>% filter(value %in% c("DESCRIPTION", "LICENSE", "NAMESPACE", "NEWS.md", "README.md" ))
for (i in 1:5){file.copy(from = paste0(sys, repo,'/', ref$value[i]), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/', ref$value[i]))}
file.copy(from = paste0(sys, repo,'/R/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)
file.copy(from = paste0(sys, repo,'/inst/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)
file.copy(from = paste0(sys, repo,'/vignettes/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)



repo <- 'iloData'
ref <- list.files(paste0(sys, repo)) %>% enframe(name = NULL) %>% filter(value %in% c("DESCRIPTION", "LICENSE", "NAMESPACE", "NEWS.md", "README.md" ))
for (i in 1:5){file.copy(from = paste0(sys,repo,'/', ref$value[i]), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/', ref$value[i]))}
file.copy(from = paste0(sys,repo,'/R/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)
file.copy(from = paste0(sys,repo,'/inst/'), to = paste0(ilo:::path$data, '_Admin/', 'Pandora/',repo,'/'),recursive = TRUE)



}