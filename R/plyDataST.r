#' ilo short term data processing  
#'
#' @param Title character string of the country alpha iso code 3.
#' @param ReadMe file containing path of each ST original files.
#' @param mywd system work directory.
#' @author ILO bescond  
#' @keywords ILO
#' @export

plyDataST <- function(Title, ReadMe, mywd, ilo_tpl){


###### def var
Title <- unlist(Title)


# Title <- "ALB"       ; ReadMe <- ReadMeST; mywd <- ilo:::path$sys; ilo_tpl <- ilo_tpl$Mapping_indicator
KEY_ORACLE 	<- c("Country_Code","Indicator_Code","Source_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","Sex_Code","Classif1_Code","Classif2_Code", "Value","Value_Status_Code","Currency_Code","Value_Notes_String","Qtable_Notes_String")
key_QTA <- KEY_ORACLE[1:7] ; key_ALL <- KEY_ORACLE[1:10]
statistics <- c(Collected_manual 	= 0, 
				Collected_auto 		= 0,
				Migration 			= 0, 
				Calculated 			= 0)

				
invisible(gc(reset = TRUE))

# identification of files with selected country
REF_FILE <- as.tbl(ReadMe) %>% 
			select(-REF)  %>% 
			mutate( cou_ref = grepl(Title,ReadMe$COUNTRY)) %>% 
			filter(cou_ref %in%TRUE)
rm(ReadMe)
# load all file related to selected country			
X <- as.list(REF_FILE$ID) %>% 
			plyr:::ldply(function(x) {load(paste0(mywd, 'ILO_Data/ON_STI_FILES/File',x,'.Rdata')); return(X)}) %>% 
			as.tbl %>%
			filter(Country_Code%in%Title) %>% 
			filter( !(str_sub(Source_Code,1,2) %in% 'BE' & !substr(Time,5,5) %in% c('M')))
rm(REF_FILE) 
			
################################## remove exception

X <- X %>% filter(!(Country_Code %in% 'VEN' & Source_Code %in% 'BA:382' & str_sub(Time,5,5) %in% c('Q') & str_detect(Notes_Source_Code, 'R1:3513')))	

#################################### should be move to load_STI	

col_ref  <- c("Country_Code","Source_Code", "Indicator_Code","Sex_Code", "Classif1_Code","Classif2_Code","Time", "Value","Value_Status_Code","Freq_Code","Notes_Classif_Code", "Notes_Indicator_Code", "Notes_Source_Code", "Currency_Code", "Add_Repository","Add_Status")

X <- X[,colnames(X)%in%col_ref] 
X <- X[,col_ref]
rm(col_ref)


X <- X %>% 	
		mutate(		# create noteQtable by combining notes source code and note indicator code
				Qtable_Notes_String = ifelse(!Notes_Source_Code%in%NA,paste(Notes_Indicator_Code,Notes_Source_Code,sep="_"),Notes_Indicator_Code),
				Qtable_Notes_String =  gsub("NA_","",Qtable_Notes_String),
				Value_Notes_String = ifelse(Notes_Classif_Code%in%"NA",NA,Notes_Classif_Code),
				Value_Status_Code = ifelse(Value_Status_Code%in%"", NA, Value_Status_Code),
				Value_Status_Code = ifelse(!Value_Status_Code%in%NA, paste0(tolower(substr(Value_Status_Code,1,1)), substr(Value_Status_Code, 2, nchar(Value_Status_Code))), Value_Status_Code)
				) %>%
		select( -Notes_Indicator_Code,-Notes_Source_Code, -Notes_Classif_Code) %>% 
		filter(!(Add_Repository%in%c("OECD") & Value%in%NA & Value_Status_Code%in%c("u",NA)))
		# filter(!(Add_Repository%in%"EUROSTAT" & Value %in% NA & Value_Status_Code %in% "f"))
		
	####################################
invisible(gc(reset = TRUE))
			

statistics["Collected_manual"] = nrow(X[X$Add_Status%in%"M",])
statistics["Collected_auto"] = nrow(X[X$Add_Status%in%"B",]) 




# check duplicate over the various file

# X <- X %>% 
# 		unite_("ID",c(KEY_ORACLE[c(1:3,7:10)],"Freq_Code"), sep="/", remove = FALSE) 

	
TEST_DUPLICATE <- X %>% 
		# group_by(ID) %>% 
		group_by(Country_Code, Indicator_Code, Source_Code, Time, Sex_Code, Classif1_Code, Classif2_Code, Freq_Code) %>%		
		tally %>% 
		ungroup %>% 
		filter(n > 1)
		
if(nrow(TEST_DUPLICATE)> 0){
X <- X %>%
		#distinct(Country_Code, Indicator_Code, Source_Code, Time, Sex_Code, Classif1_Code, Classif2_Code, Freq_Code, Qtable_Notes_String, .keep_all = TRUE)
		distinct(Country_Code, Indicator_Code, Source_Code, Time, Sex_Code, Classif1_Code, Classif2_Code,  .keep_all = TRUE)
		# distinct(ID)
test <- X %>% distinct(Country_Code)
if(nrow(test) == 1 & unique(test$Country_Code) %in% 'CAN'){} else{		
print(paste(Title,"Duplicates, plse check result on CHECK_DUPLICATE",sep=" "))
data.table:::fwrite(TEST_DUPLICATE, paste0(mywd, 'ILO_Data/check/CHECK_DUPLICATE_combi_',Title,'.csv'),na = '')
}
rm(test)
}
# X <- X %>% 
# 	select(-ID)  
rm(TEST_DUPLICATE)


invisible(gc(reset = TRUE))
	

	
################################## start by year	
	
	

#########################################################################	
############# COMPUTE SEX NB
test <- Ariane:::COMPUTE$SEX_NB  %>% as.tbl     	
X <- X %>% 
		bind_rows(	plyDataSexST(X %>% 
						filter(	!(	str_sub(Indicator_Code,-2,-1)%in%"RT" | 
									str_sub(Indicator_Code,1,3)%in%c("CPI","HOW","EAR") | 
									(Add_Repository %in%c('MICRO', 'EUROSTAT') & !str_sub(Time,5,5) %in% c('Q', 'M'))), 
								!str_sub(Indicator_Code,1,3)%in%c('MST')), 
						test) %>% {invisible(gc(reset = TRUE)); .})  
rm(test)
	invisible(gc(reset = TRUE))											
											
#########################################################################										
############# COMPUTE CLASSIF NB
######################################################################### 
test <- Ariane:::COMPUTE$CLASS_NB  %>% as.tbl     	


if(nrow(X %>% filter(!Classif2_Code%in%"XXX_XXX_XXX"))>0){
	for (i in 1:nrow(test)){
	
	
		X <- X %>% 
			bind_rows(	plyDataClassificationST(X %>% 
							filter(	Classif2_Code %in% unlist(c( test$COMPUTE[i],
							unlist(str_split(as.character(test$VAR1[i]), ";")),
							unlist(str_split(as.character(test$VAR2[i]), ";")))), 
							!(Add_Repository %in% c('EUROSTAT') & !str_sub(Time,5,5) %in% c('Q', 'M')),
							!str_sub(Indicator_Code,1,3)%in%c('MST'),
							!(Add_Repository %in% c('MICRO') )),   
					test[i,],
					"Classif2_Code") %>% {invisible(gc(reset = TRUE)); .})
	}
}

X <- X %>% filter(!str_sub(Classif2_Code,1,3) %in% 'AGE')
	
	
	
	
invisible(gc(reset = TRUE))	
for (i in 1:nrow(test)){
	X <- X %>%
		bind_rows(	plyDataClassificationST(X %>% 
					filter(	Classif1_Code %in% unlist(c( test$COMPUTE[i],
							unlist(stringr::str_split(as.character(test$VAR1[i]), ";")),
							unlist(stringr::str_split(as.character(test$VAR2[i]), ";")))), 
							!(Add_Repository %in% c('EUROSTAT') & !str_sub(Time,5,5) %in% c('Q', 'M')), 
							!str_sub(Indicator_Code,1,3)%in%c('MST'),
							!(Add_Repository %in% c('MICRO') )),  
					test[i,],
					"Classif1_Code") %>% {invisible(gc(reset = TRUE)); .})
}
rm(test)
invisible(gc(reset = TRUE))	
X <- X %>% 	filter(!(str_sub(Indicator_Code,1,3)%in%c("POP","EAP","EIP") & str_sub(Classif1_Code,1,3) %in% c("ECO", 'STE', "OCU"))) %>% 
			filter(!(Classif1_Code %in% "XXX_XXX_XXX" & !Classif2_Code %in% "XXX_XXX_XXX")) %>% 
			filter(!str_sub(Classif2_Code,1,3) %in% 'AGE') %>% 
			mutate(test = ifelse(!Classif1_Code %in% 'XXX_XXX_XXX' & Classif1_Code == Classif2_Code, 1, 0)) %>% 
			filter(!test == 1) %>% select(-test)

			
			
################## ADD 		AGE_YTHADULT_Y15-64 & 	ECO_SECTOR_NAG for MICRO
			
test <- Ariane:::COMPUTE$CLASS_NB  %>% as.tbl     	
		
new <- bind_rows(	test %>% filter(COMPUTE %in% 'AGE_YTHADULT_Y15-64', VAR1 %in% 'AGE_AGGREGATE_Y15-24'), 
					test %>% filter(COMPUTE %in% 'ECO_SECTOR_NAG', VAR1 %in% 'ECO_SECTOR_TOTAL'))			
test <- new ; rm(new)			

invisible(gc(reset = TRUE))	
for (i in 1:nrow(test)){
	X <- X %>%
		bind_rows(	plyDataClassificationST(X %>% 
					filter(	Classif1_Code %in% unlist(c( test$COMPUTE[i],
							unlist(stringr::str_split(as.character(test$VAR1[i]), ";")),
							unlist(stringr::str_split(as.character(test$VAR2[i]), ";")))), 
							Add_Repository %in% c('MICRO') ), 
					test[i,],
					"Classif1_Code") %>% {invisible(gc(reset = TRUE)); .})
}

for (i in 1:nrow(test)){
		X <- X %>% 
			bind_rows(	plyDataClassificationST(X %>% 
							filter(	Classif2_Code %in% unlist(c( test$COMPUTE[i],
							unlist(str_split(as.character(test$VAR1[i]), ";")),
							unlist(str_split(as.character(test$VAR2[i]), ";")))), 
							Add_Repository %in% c('MICRO') ), 
					test[i,],
					"Classif2_Code") %>% {invisible(gc(reset = TRUE)); .})
	}

rm(test)
invisible(gc(reset = TRUE))			
			
################# reduce indicator structure



													
ind_ref <- Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% c("STI", 'YI', 'KI', 'YTH', 'ILOEST', 'ILMS', 'SDG')) %>%
		distinct(CIC_INDICATOR_CODE) %>% mutate(class = str_sub(CIC_INDICATOR_CODE, 10,-4)) %>% 
		distinct %>% 
		separate(class, c('sex_test','classif1_test','classif2_test'), sep = '_', fill = 'right', extra = 'drop') %>% 
		mutate(Indicator_Code = paste0(str_sub(CIC_INDICATOR_CODE, 1,9), str_sub(CIC_INDICATOR_CODE, -2,-1))) %>% 
		mutate(sex_test = ifelse(sex_test %in% c('NOC'), NA, str_sub(sex_test,1,3))) %>% 
		mutate(classif1_test = ifelse(classif1_test %in% c('NOC', NA), 'XXX', str_sub(classif1_test,1,3))) %>% 
		mutate(classif2_test = ifelse(classif2_test %in% c('NOC', NA), 'XXX', str_sub(classif2_test,1,3)))  %>% 
		mutate(keep = 1) %>% select(-CIC_INDICATOR_CODE) %>% 
		mutate(classif2_test = ifelse(!sex_test %in% c('SEX', NA), classif1_test, classif2_test)) %>% 
		mutate(classif1_test = ifelse(!sex_test %in% c('SEX', NA), sex_test, classif1_test)) %>% 
		mutate(sex_test = ifelse(!sex_test %in% c('SEX', NA), NA, sex_test)) 
ind_ref <- bind_rows(ind_ref, ind_ref %>% mutate(Indicator_Code = paste0(str_sub(Indicator_Code, 1,5), '9', str_sub(Indicator_Code, -5,-1) )))
		
		
X <- X %>% mutate(sex_test = str_sub(Sex_Code,1,3), sex_test = ifelse(sex_test %in% c('NOC', 'XXX'), NA, sex_test), 
			classif1_test = str_sub(Classif1_Code,1,3), classif1_test = ifelse(classif1_test %in% c('NOC', NA), 'XXX', classif1_test),
			classif2_test = str_sub(Classif2_Code,1,3), classif2_test = ifelse(classif2_test %in% c('NOC', NA), 'XXX', classif2_test)) %>% 
			left_join(ind_ref, by = c("Indicator_Code", "sex_test", "classif1_test", "classif2_test")) %>% 
			filter(keep == 1) %>% 
			select(-sex_test,-classif1_test, -classif2_test)
			
rm(ind_ref)			
invisible(gc(reset = TRUE))				
invisible(gc(reset = TRUE))				

################### test duplicate as micro is not process

X <- X %>%
		distinct(Country_Code, Source_Code, Indicator_Code, Sex_Code, Classif1_Code, Classif2_Code, Time, .keep_all = TRUE)
		
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))


	
#########################################################################
############# COMPUTE SEX NB
#########################################################################
test <- Ariane:::COMPUTE$SEX_NB  
X <- X %>% 
		bind_rows(	plyDataSexST(X %>% 
					filter(	!(str_sub(Indicator_Code,-2,-1)%in%"RT" | 
							str_sub(Indicator_Code,1,3)%in%c("CPI","HOW","EAR") | 
									(Add_Repository %in%c('MICRO', 'EUROSTAT') & !str_sub(Time,5,5) %in% c('Q', 'M'))),
							!str_sub(Indicator_Code,1,3)%in%c('MST')), 
					test)%>% {invisible(gc(reset = TRUE)); .}) 
rm(test)

#########################################################################
############# COMPUTE INDICATOR NB
#########################################################################
test <- Ariane:::COMPUTE$INDICATOR_NB 


for (i in 1:nrow(test)){

		X <- X %>% 
			bind_rows(	plyDataIndicatorST(X  %>% 
							filter(	Indicator_Code %in% unlist(c( test$COMPUTE[i],
															unlist(stringr::str_split(as.character(test$VAR1[i]), ";")),
															unlist(stringr::str_split(as.character(test$VAR2[i]), ";")))),
									!(str_sub(Indicator_Code,-2,-1)%in%"RT" | 
									str_sub(Indicator_Code,1,3)%in%c("CPI","HOW","EAR")) , 
							!(Add_Repository %in% c('EUROSTAT') & !str_sub(Time,5,5) %in% c('Q', 'M')), 
							!(Add_Repository %in% c('MICRO') ),
							!str_sub(Indicator_Code,1,3)%in%c('MST')), 
									test[i,])) 
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
	
}
						
rm(i, test)
invisible(gc(reset = TRUE))

################### test duplicate as micro is not process

X <- X %>%
		distinct(Country_Code, Source_Code, Indicator_Code, Sex_Code, Classif1_Code, Classif2_Code, Time, .keep_all = TRUE)
		
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))



#########################################################################
############# COMPUTE QUARTER
#########################################################################

test <- Ariane:::COMPUTE$QUARTER  
for (i in 1:nrow(test)){

X <- X %>% 
		bind_rows(	plyDataQuarterST(X %>% 
					filter(	Freq_Code %in% unlist(c(str_split(as.character(test$NOTES[i]), ";"))),
							str_sub(Time, 5, -1)%in% unlist(c(test$COMPUTE[i],str_split(as.character(test$VAR1[i]), ";"))), 
							!(substr(Time,5,5) %in% c('M') & substr(Source_Code,1,2) %in% 'BE'), # not process quarterly data for BE EULFS adjusted
							!(Country_Code %in% 'SWE' & Source_Code %in% 'BA:2519'), 
							!Add_Repository %in% c('MICRO')), 
					test[i,])%>% {invisible(gc(reset = TRUE)); .})
		
					
}
rm(test, i)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))




################### new delete quarterly from the monthly time span
X <- X %>% filter(!(str_sub(Time,5,5) %in% "M" & !Freq_Code %in% c("M", "X")))


################### test duplicate as micro is not process

X <- X %>%
		distinct(Country_Code, Source_Code, Indicator_Code, Sex_Code, Classif1_Code, Classif2_Code, Time, .keep_all = TRUE)
		
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))






#########################################################################
############# COMPUTE YEAR
#########################################################################

test <- Ariane:::COMPUTE$YEAR  
for (i in 1:nrow(test)){
X <- X %>% 
		bind_rows(	plyDataYearST(X %>% 
					filter(	Freq_Code %in% unlist(str_split(as.character(test$NOTES[i]), ";")),
							!(str_sub(Indicator_Code,8,8)%in%c("1") & str_sub(Time,5,5)%in%c("M","Q")),
							!(substr(Time,5,5) %in% c('Q','M') & substr(Source_Code,1,2) %in% 'BE'), # not process yearly data for BE EULFS adjusted
							str_sub(Time, 5, -1)%in% unlist(c("",str_split(as.character(test$VAR1[i]), ";"))), 
							!Add_Repository %in% c('MICRO', 'EUROSTAT'), 
							!(Country_Code %in% 'VEN' & Source_Code %in% 'BA:382' & str_sub(Time,1,4) %in%c('2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012')), 
							!Country_Code %in% c('EGY', 'PSE')), 
					test[i,])%>% {invisible(gc(reset = TRUE)); .})
} 
rm(test, i)
invisible(gc(reset = TRUE))

################### test duplicate as micro is not process

X <- X %>%
		distinct(Country_Code, Source_Code, Indicator_Code, Sex_Code, Classif1_Code, Classif2_Code, Time, .keep_all = TRUE)
		
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))



#########################################################################
############# COMPUTE SEX NB
#########################################################################
test <- Ariane:::COMPUTE$SEX_NB  
X <- X %>% 
		bind_rows(	plyDataSexST(X %>% 
					filter(	!(str_sub(Indicator_Code,-2,-1)%in%"RT" | 
							str_sub(Indicator_Code,1,3)%in%c("CPI","HOW","EAR") | 
									(Add_Repository %in% c('MICRO', 'EUROSTAT') & !str_sub(Time,5,5) %in% c('Q', 'M'))),
									!str_sub(Indicator_Code,1,3)%in%c('MST')), 
					test)%>% {invisible(gc(reset = TRUE)); .}) 
rm(test)
invisible(gc(reset = TRUE))
												
#########################################################################
############# COMPUTE RATE / CLASS ## former UNE_LGTD_RT
#########################################################################
# test <- Ariane:::COMPUTE$INDICATOR_CLASS_RT	
# for (i in 1:nrow(test)){
# X <- X %>% 
		# bind_rows(	plyDataIndicatorClassificationST(X %>% 
					# filter(	Indicator_Code %in% c(test$COMPUTE[i],substr(test[i,"VAR1"],1,11),substr(test$VAR2[i],1,11)), 
							# substr(Classif1_Code,1,3) %in% "AGE", 
							# Classif2_Code %in% c(test$COMPUTE[i],substr(test$VAR1[i],13,nchar(test$VAR1[i])),substr(test$VAR2[i],13,nchar(test$VAR1[i]))))
					# ,test[i,])%>% {invisible(gc(reset = TRUE)); .})
# } 
# rm(test, i)
# invisible(gc(reset = TRUE))
#########################################################################												
############# COMPUTE RATE
#########################################################################
test <- Ariane:::COMPUTE$INDICATOR_RT	
for (i in 1:nrow(test)){
X <- X %>% 
		bind_rows(	plyDataIndicatorRateST(X %>% 
					filter(	Indicator_Code %in% c(test$COMPUTE[i],str_split(test$VAR1[i],pattern = ';') %>% 
										unlist,str_split(test$VAR2[i],pattern = ';') %>% unlist))
					,test[i,])%>% {invisible(gc(reset = TRUE)); .})
} 
rm(test)			
invisible(gc(reset = TRUE))


################### test duplicate as micro is not process

X <- X %>%
		distinct(Country_Code, Source_Code, Indicator_Code, Sex_Code, Classif1_Code, Classif2_Code, Time, .keep_all = TRUE)
		
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))



########### test part   and cleaning
X <- X %>%
		mutate(Value = as.numeric(as.character(Value))) %>% 
		filter(!(Value %in% NA & Value_Status_Code %in% c('', NA, NaN))) %>%
		checkDataSourceST %>%
		checkDataFrequencyST %>%	
		mutate(		Indicator_Code = paste0(str_sub(Indicator_Code,1,8),"_",str_sub(Sex_Code,1,3),"_",str_sub(Classif1_Code,1,3),"_",str_sub(Classif2_Code,1,3),"_",str_sub(Indicator_Code,10,11)),
					Indicator_Code = gsub("_NA","",Indicator_Code, fixed = TRUE),
					Indicator_Code = gsub("_XXX","",Indicator_Code, fixed = TRUE), 
					Indicator_Code = ifelse(str_sub(Indicator_Code,6,6) %in% '9', gsub('ECO', 'ECO2', Indicator_Code), Indicator_Code),# prepare eco ocu level 2
					Indicator_Code = ifelse(str_sub(Indicator_Code,6,6) %in% '9', gsub('OCU', 'OCU2', Indicator_Code), Indicator_Code),
					Indicator_Code = ifelse(str_sub(Indicator_Code,6,6) %in% '9', gsub('9', 'E', Indicator_Code), Indicator_Code),
					Value_Status_Code = ifelse(Value_Status_Code%in%c(""),NA,Value_Status_Code),
					Value_Status_Code = as.character(gsub("b","I11:264",Value_Status_Code, fixed = TRUE)),
					Value_Status_Code = as.character(gsub("c","I11:268",Value_Status_Code, fixed = TRUE)),
					Value_Status_Code = as.character(gsub("d","I11:271",Value_Status_Code, fixed = TRUE)),
					Qtable_Notes_String = as.character(gsub("I11:264_","",Qtable_Notes_String, fixed = TRUE)),
					Qtable_Notes_String = as.character(gsub("I11:268_","",Qtable_Notes_String, fixed = TRUE)),
					Qtable_Notes_String = as.character(gsub("I11:271_","",Qtable_Notes_String, fixed = TRUE)),
					Value_Status_Code = as.character(gsub("e","E",Value_Status_Code, fixed = TRUE)),
					Value_Status_Code = as.character(gsub("f","C",Value_Status_Code, fixed = TRUE)), # confidential
					Value_Status_Code = as.character(gsub("p","P",Value_Status_Code, fixed = TRUE)),
					Value_Status_Code = as.character(gsub("s","S",Value_Status_Code, fixed = TRUE)),
					Value_Status_Code = as.character(gsub("u","U",Value_Status_Code, fixed = TRUE)),
					Qtable_Notes_String = ifelse(!Value_Status_Code%in%c("E","C","P","S","U",NA),paste(Qtable_Notes_String,Value_Status_Code,sep="_"),Qtable_Notes_String),
					Value_Status_Code = ifelse(str_sub(Value_Status_Code,1,1)%in%c("I") & !Value%in%NA,"B",Value_Status_Code),
					Qtable_Notes_String = gsub("NA_","",Qtable_Notes_String),
					Qtable_Notes_String = ifelse(Qtable_Notes_String%in%"NA",NA,Qtable_Notes_String),
					Value_Status_Code = ifelse(!Value_Status_Code%in%c("B","C","E","P","S","U",NA),NA,Value_Status_Code),
					Sex_Code = as.character(gsub("XXX_X",NA,Sex_Code, fixed = TRUE)),
					Classif1_Code = as.character(gsub("XXX_XXX_XXX",NA,Classif1_Code, fixed = TRUE)),
					Classif2_Code = as.character(gsub("XXX_XXX_XXX",NA,Classif2_Code, fixed = TRUE)))%>% 
		# create class version
		separate(	Sex_Code,"Sex_Version_Code", sep="_", extra = "drop", remove = FALSE) %>%
		separate(	Classif1_Code,c("CODE_CLACL1","CODE_VSCL1"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(		Classif1_Version_Code,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>%
		separate(	Classif2_Code,c("CODE_CLACL2","CODE_VSCL2"), sep="_", extra = "drop", remove = FALSE) %>%
		unite(		Classif2_Version_Code,CODE_CLACL2,CODE_VSCL2, sep = "_", remove = TRUE) %>%
		mutate(		Classif1_Version_Code = ifelse(Classif1_Version_Code%in%"NA_NA", NA,Classif1_Version_Code),
					Classif2_Version_Code = ifelse(Classif2_Version_Code%in%"NA_NA", NA,Classif2_Version_Code)) %>%
		left_join(	select(Ariane:::CODE_ORA$T_FRQ_FREQUENCY,Freq_Code=FRQ_CODE,NEW_CODE_ORACLE), by = "Freq_Code") %>% 
		mutate(		Qtable_Notes_String = ifelse(!NEW_CODE_ORACLE%in%NA,paste(Qtable_Notes_String,NEW_CODE_ORACLE,sep="_"),Qtable_Notes_String),
					Qtable_Notes_String = gsub("NA_","",Qtable_Notes_String)) %>%
		# prepare cleaning of notes at Qtable levels
		left_join(	select(Ariane:::CODE_ORA$T_CLA_CLASSIF, Classif1_Code = CLA_CODE,IS_CLA1_TOTAL = CLA_IS_TOTAL),by ="Classif1_Code") %>%
		left_join(	select(Ariane:::CODE_ORA$T_CLA_CLASSIF, Classif2_Code = CLA_CODE,IS_CLA2_TOTAL = CLA_IS_TOTAL),by ="Classif2_Code") %>%
		left_join(	select(Ariane:::CODE_ORA$T_CLA_CLASSIF, Sex_Code = CLA_CODE,IS_SEX_TOTAL = CLA_IS_TOTAL),by ="Sex_Code")%>%

		unite_(		"TEST", c("IS_SEX_TOTAL","IS_CLA1_TOTAL","IS_CLA2_TOTAL"), sep="/", remove = TRUE) %>%
		unite_(		"KEY",key_QTA, sep ="/", remove = FALSE ) %>%
		unite_(		"ID", c("KEY","Qtable_Notes_String"), sep ="|", remove = FALSE) %>%
		filter(		Indicator_Code%in%Ariane:::CODE_ORA[["T_IND_INDICATOR"]]$IND_CODE,
					!(Value%in%NA & Value_Status_Code%in%NA) )	 %>%	
		select(		-NEW_CODE_ORACLE,-Freq_Code) %>%
		mutate(		Value_Status_Code = ifelse((str_sub(Indicator_Code,8,8)%in%"1" & Value_Status_Code%in%"E"),NA,Value_Status_Code),					
					Currency_Code = ifelse(!Currency_Code%in%NA, as.character(str_sub(Currency_Code, 5, -1)), NA),
					Qtable_Notes_String = ifelse(!as.character(Qtable_Notes_String)%in%NA,paste(as.character(Qtable_Notes_String),paste0('T30:',as.character(Currency_Code)),sep="_"),paste0('T30:',as.character(Currency_Code))),
					Qtable_Notes_String = gsub('_T30:NA', '', Qtable_Notes_String),
					Source_Code = as.character(parse_number(Source_Code)),
					Country_Code = as.character(Country_Code)) %>%
		mutate(Value_Status_Code = toupper(Value_Status_Code)) %>% 	
		mutate_all(funs(factor)) %>% 
		mutate(Value = as.numeric(as.character(Value))) # %>%  ########## Exception IDN %>% 
		# filter(!(Country_Code %in% "IDN" & Classif1_Code %in% "AGE_AGGREGATE_YGE65")) 
		
invisible(gc(reset = TRUE))
		



# TEST version on STI for quarterly and monthly data
TEST_STI <- Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% c("STI", 'YI', 'KI', 'YTH', 'ILOEST', 'ILMS', 'SDG')) 
test_version_STI <- TEST_STI %>% mutate(TEST = paste0(CIC_INDICATOR_CODE, "/", CIC_CLASSIF_VERSION_CODE)) %>% select(TEST) %>% distinct(TEST) %>% t %>% c  
test_version_STI <- c(test_version_STI, paste(TEST_STI$CIC_INDICATOR_CODE, "NA", sep = "/"))
test_version_STI <- gsub('/NOC', '/NOC_VALUE',test_version_STI)
rm(TEST_STI)

invisible(gc(reset = TRUE))

X <- 	X  %>% 
				mutate(TEST_CL1 = paste0(Indicator_Code, "/", Classif1_Version_Code)) %>% 
				filter(TEST_CL1 %in% test_version_STI) %>% 
				mutate(TEST_CL2 = paste0(Indicator_Code, "/", Classif2_Version_Code)) %>% 
				filter(TEST_CL2 %in% test_version_STI) %>%
				select(-TEST_CL1, -TEST_CL2)

				

		
## rm(TEST_YI)		
rm(test_version_STI)		
invisible(gc(reset = TRUE))
		
if(!length(unique(X$KEY))==length(unique(X$ID))){

	NEW <- X %>% 
		filter(		!TEST%in%c("Y/NA/NA","Y/Y/NA","Y/Y/Y")) %>%
		mutate(		Nchar = nchar(as.character(Qtable_Notes_String))) %>%
		arrange(	KEY, desc(Nchar) ) %>%
		mutate(Qtable_Notes_String = as.character(Qtable_Notes_String)) %>%
		group_by(	KEY) %>%
		summarise(	n = n(), Nchar = dplyr::first(Nchar), NEW_QTABLE_NOTE  = dplyr::first(Qtable_Notes_String) ) %>%
		ungroup() %>% 
		select(		KEY,NEW_QTABLE_NOTE)
		
	if(!plyr:::empty(NEW))	{	
		X <- X %>% 
			left_join(NEW, by ="KEY") %>%
			mutate(	Qtable_Notes_String = ifelse(!NEW_QTABLE_NOTE%in%NA,NEW_QTABLE_NOTE,as.character(Qtable_Notes_String))) %>%
			select(-NEW_QTABLE_NOTE)
		
	}
	rm(NEW)
}

invisible(gc(reset = TRUE))
X <- X %>% 
		select(-TEST,-KEY,-ID) %>%
		checkDataEmptyST %>% 
		checkDataRateST(mywd) %>% 	
		filter(	as.character(Indicator_Code) %in% c(t(left_join(	Ariane:::CODE_ORA$T_CIN_COL_IND %>%  
														filter(COL_CODE %in% c('YI', 'STI', 'KI', 'YTH', 'ILOEST', 'ILMS', 'SDG')) %>% 
														select(ID = CIN_INDICATOR_ID), 
													select(Ariane:::CODE_ORA$T_IND_INDICATOR, ID = IND_ID, IND_CODE)
												, by = "ID")["IND_CODE"])), 
					!(Value%in%NA & Value_Status_Code%in%NA) 
		)

X <- X %>% mutate_all(funs(as.character))
invisible(gc(reset = TRUE))		


##################### ADD OECD data from query
REF_OECD <- c('AUS', 'AUT', 'CHE', 'COL', 'CZE', 'EST', 'FIN', 'FRA', 'HUN', 'ISL', 'ISR', 'JPN', 'KOR', 'MEX', 'NOR', 'NZL', 'POL', 'RUS', 'SVK', 'SWE', 'TUR', 'ZAF', 'CHL', 'CRI')


if(unique(X$Country_Code) %in% REF_OECD){
			
IND_OECD <- c('EAP_DWAP_SEX_AGE_RT', 'EAP_TEAP_SEX_AGE_NB', 'EES_TEES_SEX_ECO_NB', 'EIP_DWAP_SEX_AGE_RT', 'EIP_TEIP_SEX_AGE_NB', 'EMP_DWAP_SEX_AGE_RT', 'EMP_TEMP_SEX_AGE_NB', 'EMP_TEMP_SEX_ECO_NB', 'EMP_TEMP_SEX_STE_NB', 'POP_XWAP_SEX_AGE_NB', 'UNE_DEAP_SEX_AGE_RT', 'UNE_TUNE_SEX_AGE_DUR_NB', 'UNE_TUNE_SEX_AGE_NB')
test <- X %>% filter((!str_sub(Time,5,5) %in% c('Q','M') &  Indicator_Code %in% IND_OECD))


if(nrow(test)>0){
	# delete Annual data from Bulk
	X <- X %>% filter(!(!str_sub(Time,5,5) %in% c('Q','M') & Indicator_Code %in% IND_OECD))
	cou <- unique(X$Country_Code)
	# Upload eurostat Annual from Query
	Y <- read_rds(paste0(ilo:::path$data, 'REP_OECD/LFS_ANNUAL/output/',cou,'.rds')) %>% mutate_all(funs(as.character)) %>%
					mutate( source = parse_number(source) %>% as.character, 
							classif1 = ifelse(classif1 %in% NA, 'XXX_XXX_XXX', classif1), 
							classif1 = ifelse(classif1 %in% NA, 'XXX_XXX_XXX', classif1)		
						) %>% arrange(classif1) %>%
					select(ref_area, indicator, source, sex, sex_version, classif1, classif1_version, classif2, classif2_version, time, obs_value , obs_status, note_classif, note_source) %>% 
					filter(indicator %in% IND_OECD)
			
colnames(Y) <- c("Country_Code",'Indicator_Code', "Source_Code","Sex_Code", 'Sex_Version_Code', "Classif1_Code", 'Classif1_Version_Code',"Classif2_Code", 'Classif2_Version_Code',"Time", "Value",'Value_Status_Code', 'Value_Notes_String', 'Qtable_Notes_String')

	Y <- Y %>% 
			checkDataEmptyST %>% 
			checkDataRateST(mywd)

invisible(gc(reset = TRUE))
# test missing Qtable at indicator levels between eurostat bulk and query
# create key


	test	 <- test 	%>% mutate(ID = paste0(Source_Code, Time,  Indicator_Code, str_sub(Sex_Code,1,3), str_sub(Classif1_Code,1,3), str_sub(Classif2_Code,1,3))) %>% group_by(ID) %>% mutate(n = n()) %>% ungroup
	Y 		<- 	Y 		%>% mutate(ID = paste0(Source_Code, Time,  Indicator_Code, str_sub(Sex_Code,1,3), str_sub(Classif1_Code,1,3), str_sub(Classif2_Code,1,3))) %>% group_by(ID) %>% mutate(n = n()) %>% ungroup
	# delete from bulk existing indicator from eurostat query
	
	ref <- full_join( 	Y %>% distinct(ID, n) %>% select(ID, oecd = n), 
						test %>% distinct(ID, n) %>% select(ID, sti = n), by = 'ID') %>% 
						mutate( sti = ifelse(sti %in% NA, 0, sti), 
								oecd = ifelse(oecd%in% NA, 0, oecd) ,	
								TEST = ifelse(sti >= oecd, TRUE, FALSE), 
								PASS = ifelse(TEST %in% TRUE, 'sti', 'oecd')) %>%filter(PASS %in% 'oecd') %>% select(-sti, -oecd, -TEST) 
	
	Y <- Y %>% select(-n) %>% left_join(ref, by = 'ID')  %>% filter(PASS %in% 'oecd') 
	
	test <- test %>% select(-n) %>%  left_join(Y %>% distinct(Country_Code,  Source_Code,  Indicator_Code,Time, PASS ), by = c("Country_Code", 'Source_Code',"Indicator_Code", "Time")) %>% filter(!PASS %in% 'oecd')

	X <- bind_rows(X,Y, test) %>% select(-ID)
	rm(cou, Y, ref)
	invisible(gc(reset = TRUE))
}
rm(test, IND_OECD)
invisible(gc(reset = TRUE))
	
}
rm(REF_OECD)					
					
################################## end by year
		
KEY_ORACLE 	<- c("Country_Code","Indicator_Code","Source_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Time","Sex_Code","Classif1_Code","Classif2_Code", "Value","Value_Status_Code","Value_Notes_String","Qtable_Notes_String")
	
		
statistics["Calculated"] <- nrow(X %>% filter(str_sub(Add_Status,2,2)%in%"C"))

X <- X %>% select_(.dots  = KEY_ORACLE) %>% 
		mutate(	Qtable_Notes_String = stringr::str_trim(Qtable_Notes_String),
				Qtable_Notes_String = plyr:::mapvalues(Qtable_Notes_String,c('NaN', '', 'NA'), c(NA,NA, NA), warn_missing = FALSE),
				Value_Notes_String = stringr::str_trim(Value_Notes_String),
				Value_Notes_String = plyr:::mapvalues(Value_Notes_String,c('NaN', '', 'NA'), c(NA,NA, NA), warn_missing = FALSE))
					
##############################################################					
##############################################################					
############################################################## Break in methodology					
##############################################################					
##############################################################					
	
X <- X %>% 	rename(Survey_Id = Source_Code) %>% 
		mutate(	
				Collection_Code = 'STI', 
				Source_Code = Survey_Id %>% plyr:::mapvalues(	from 	= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_ID, 
																to 		= 	Ariane:::CODE_ORA$T_SUR_SURVEY$SUR_SOURCE_CODE, warn_missing = FALSE) ) %>% 
		sortDataOracle %>% 	
		rename(	collection = Collection_Code, 
				ref_area = Country_Code, 
				indicator = Indicator_Code, 
				sex = Sex_Code, 
				sex_version = Sex_Version_Code, 
				classif1 = Classif1_Code, 
				classif2 = Classif2_Code, 
				classif1_version = Classif1_Version_Code, 
				classif2_version = Classif2_Version_Code, 
				time = Time, 
				obs_value = Value, 
				obs_status = Value_Status_Code, 
				note_classif  = Value_Notes_String) %>% 
		mutate(	
				note_classif 		= plyr:::mapvalues(note_classif,		from = levels(as.factor(note_classif)), 
																			to = My_Resort_NotesJ(levels(as.factor(note_classif)),SEP = "_"), warn_missing = FALSE),
				Qtable_Notes_String = Qtable_Notes_String %>% My_Resort_Notes_Type(SEP = '_',addsep  =TRUE), 
				Qtable_Notes_String = Qtable_Notes_String %>% stringr::str_replace(stringr::fixed('|'), '/')) %>% 
		separate(col = Qtable_Notes_String, into = c('note_source','note_indicator'), sep  = stringr::fixed("/"), remove = TRUE, fill = 'left') %>% 
		
		unite(source, Source_Code, Survey_Id, sep = ':', remove = TRUE) %>% 
		mutate(	freq_code = str_sub(time, 5,5), 
				freq_code = ifelse(freq_code %in% '', 'A',freq_code), 
				obs_value = as.numeric(obs_value), 
				obs_value = round(obs_value,4)		, 
				note_source	= ifelse(note_source %in% '', as.character(NA), as.character(note_source)),
				note_indicator	= ifelse(note_indicator %in% '', as.character(NA), as.character(note_indicator)))  %>% 
		select_(.dots = c("collection", "ref_area", "source", "indicator", "sex", "sex_version", "classif1", "classif1_version", "classif2", "classif2_version", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source", "freq_code")) %>% 
		mutate_if(is.factor, as.character) %>% 
		mutate(classif1_version = ifelse(classif1 %in% 'NOC_VALUE', 'NOC', classif1_version))  %>% 
		filter(!(indicator %in% c("UNE_TUNE_SEX_OCU_NB", "UNE_TUNE_SEX_ECO_NB") & str_sub(source,1,2) %in% c('FA','FB','FX','BE'))) %>%
		filter(!(str_detect(source, 'BE') & str_sub(time, 5,5) %in% c('Q', ''))) 
		
################################# reduce scope

		
############# clean up Value_Status_Code 

X <- X %>% 
		mutate(	obs_value = ifelse(obs_status %in% 'S', as.character(NA), obs_value), 
				 obs_status = ifelse(obs_status %in% 'S', 'U', obs_status), 
				 obs_value  = as.numeric(obs_value)) 
invisible(gc(reset = TRUE))
					
					

TEST_FRAMEWORK <- ilo_tpl %>% select(Is_Validate, indicator, sex_version = sex_var, classif1_version =  classif1_var , classif2_version = classif2_var, frequency) %>%   filter(Is_Validate %in% 'TRUE') %>%
				mutate(frequency = str_split(frequency, pattern = ';') %>% as.list) %>% unnest %>% distinct %>%
				mutate(frequency = ifelse(frequency %in% 'A', '', frequency)) %>% mutate(freq_OK = 1) 
				
TEST_FRAMEWORK_indicator <- TEST_FRAMEWORK %>% distinct(indicator) %>% mutate(ind_OK = 1) 				
				
X <-  X %>% 	mutate(frequency = str_sub(time, 5,5)) %>% 
				left_join(TEST_FRAMEWORK, by = c("indicator", 'frequency', 'sex_version', 'classif1_version', 'classif2_version')) %>%
				left_join(TEST_FRAMEWORK_indicator, by = 'indicator') %>% 
				filter(ind_OK %in% NA | freq_OK %in% 1) %>% select(-frequency, -Is_Validate, -freq_OK, -ind_OK)
				
rm(TEST_FRAMEWORK, TEST_FRAMEWORK_indicator )
				
				
				
#########################################################################
############# TEST Distribution 'full'
#########################################################################
test <- Ariane:::COMPUTE$INDICATOR_FULL_DT  

for (i in 1:nrow(test)){
	checkDataIndicatorDistributionFULLST(X %>% 
					filter(	indicator %in% test$VAR1[i]), 
					test[i,], mywd)
	invisible(gc(reset = TRUE))
}
rm(test, i)
invisible(gc(reset = TRUE))
		
#########################################################################
############# COMPUTE Distribution intra
#########################################################################
test <- Ariane:::COMPUTE$INDICATOR_DT  

for (i in 1:nrow(test)){
X <- X %>% 
		bind_rows(	plyDataIndicatorDistributionST(X %>% 
					filter(	indicator %in% test$VAR1[i], !str_detect(source, 'F')), 
					test[i,], mywd)%>% {invisible(gc(reset = TRUE)); .})
}
rm(test, i)
invisible(gc(reset = TRUE))



############################################################## 				
##############################################################					
##############################################################					


############## benchmark exception


X <- X %>% filter(!(ref_area %in% 'TUR' & source %in% 'BA:2256' & str_detect(time, '2008') & str_detect(indicator, 'ECO')))
X <- X %>% filter(!(ref_area %in% 'MKD' & source %in% 'BA:2239' & str_detect(time, '2008|2009|2010') & str_detect(indicator, 'ECO'))) 

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

statistics["Migration"] <- nrow(X)


	test_collection <- bind_rows( 
							Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% c('YI', 'KI', 'YTH', 'ILOEST', 'ILMS', 'SDG')) %>% select(collection = CIC_COLLECTION_CODE, indicator = CIC_INDICATOR_CODE) %>% distinct %>% mutate(freq_code = 'A'),
							Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% c('STI')) %>% select(collection = CIC_COLLECTION_CODE, indicator = CIC_INDICATOR_CODE) %>% distinct %>% mutate(freq_code = 'Q'),
							Ariane:::CODE_ORA$T_CIC_COL_IND_CLV %>% filter(CIC_COLLECTION_CODE %in% c('STI')) %>% select(collection = CIC_COLLECTION_CODE, indicator = CIC_INDICATOR_CODE) %>% distinct %>% mutate(freq_code = 'M')
							)

	X <- X %>% 	select(-collection) %>% 
				left_join(test_collection, by = c('indicator', 'freq_code')) %>% 
				filter(!collection %in% NA) %>% 
				select_(.dots = c("collection", "ref_area", "source", "indicator", "sex", "sex_version", "classif1", "classif1_version", "classif2", "classif2_version", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source", "freq_code")) 
		
	
	#test <- unique(X$freq_code)

	ref <- X %>% distinct(collection, freq_code)
	my_return <- paste0(Title, ": Nb = ", statistics["Migration"], "/ auto = ",statistics["Collected_auto"],"/ man = ",statistics["Collected_manual"],"/ cal = ",statistics["Calculated"])
	
	for (i in 1:nrow(ref)){
	
		if (ref$freq_code[i] %in% 'A'){
		
			X %>% filter(freq_code %in% ref$freq_code[i], 
						 collection %in% ref$collection[i]) %>%
					filter(!(ref_area %in% 'ALB' & source %in% 'BA:480' & str_detect(note_source, 'R1:3903') & str_sub(indicator, -3,-1) %in% '_RT')) %>% 
					filter(!(ref_area %in% c('ALB', 'AZE', 'BHS', 'BLZ', 'BRB', 'CHL','CUB', 'HKG',  'IRN','KAZ', 'KGZ', 'LKA','LVA', 'MAR', 'MDA', 'MYS', 'QAT', 'SGP', 'SUR','SAU', 'SYR','THA', 'TTO', 'VEN') 
								& str_sub(source,1,2) %in% 'BA' 
								& str_detect(note_source, 'R1:3902') 
								& str_sub(indicator, -3,-1) %in% c('_RT', '_DT'))) %>% 
					filter(!(ref_area %in% 'BRA' & source %in% 'BA:3047'))	%>%		
					filter(!(ref_area %in% 'SMR' & source %in% 'EA:5918' & str_detect(note_source, 'R1:3902') & str_sub(indicator, -3,-1) %in% c('_RT', '_DT')))	%>% 		
					filter(!(ref_area %in% 'SGP' & source %in% 'BA:5873' & str_detect(note_source, 'R1:3902') & as.numeric(time) >2016) )	%>% 		
					filter(!(ref_area %in% 'BLZ' & source %in% 'BA:934' & str_detect(note_source, 'R1:3902') & str_detect(time, '2017') ))	%>% 		
					filter(!(ref_area %in% 'BRB' & source %in% 'BA:338' & str_detect(note_source, 'R1:3902') & str_detect(indicator, 'EAP_TEAP') ))	%>% 		
					filter(!(ref_area %in% 'MYS' & source %in% 'BA:544' & str_detect(note_source, 'R1:3902') & str_detect(indicator, '_DT') ))	%>% 		
					filter(!(ref_area %in% 'ARM' & source %in% 'BB:173' & str_detect(note_source, 'R1:3902')))	%>% 
					mutate(indicator = gsub('MST_XWAP_SEX_AGE_', 'MST_TPOP_SEX_AGE_', indicator)) %>%
					saveRDS(., file = paste0(ilo:::path$sys, 'ILO_Data/STI/',ref$collection[i],'_',Title,'_',ref$freq_code[i],'.rds'))
			
			X <- X %>% filter(!(freq_code %in% ref$freq_code[i] & collection %in% ref$collection[i]))
			
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		
		} else {
		
			X %>% filter(freq_code %in% ref$freq_code[i], 
						 collection %in% ref$collection[i]) %>%
					saveRDS(., file = paste0(ilo:::path$sys, 'ILO_Data/STI/',ref$collection[i],'_',Title,'_',ref$freq_code[i],'.rds'))
		
			X <- X %>% filter(!(freq_code %in% ref$freq_code[i] & collection %in% ref$collection[i]))
			
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		
		}
	
	
	}

	rm(X, key_ALL, KEY_ORACLE, key_QTA, mywd,test_collection, ref)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))


return(my_return)



}

#' @export

plyDataSexST <- function(X, MY_CALCULATION){	

if(nrow(X) == 0) return(NULL)
key 		<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")

for (i in 1:nrow(MY_CALCULATION)){
	X 	<- X %>% 	
			mutate(VAR = Sex_Code) %>%
			unite_("KEY", c(key[!key%in%"Sex_Code"],"Time","Freq_Code"), sep="/", remove = FALSE) %>%
			filter(!Value%in%NA | (!Value_Status_Code%in%c(NA,"",NaN) & substr(Time,5,5)%in%"M"))


 
	Y_REF 	<- X %>% filter(VAR %in% MY_CALCULATION[i,"COMPUTE"])

	VARUM 	<- unlist(strsplit(MY_CALCULATION$VAR1[i],";"))
	C_X 	<- X %>% filter(VAR %in% VARUM[1] & !KEY%in%Y_REF$KEY)

	VARUM <- unlist(strsplit(MY_CALCULATION$VAR2[i],";"))
	C_Y <-  X %>% filter(VAR %in% VARUM[1] & !KEY%in%Y_REF$KEY)
	rm(VARUM)
invisible(gc(reset = TRUE))
# sum, diff, prod
	if(!plyr:::empty(C_X) & !plyr:::empty(C_Y)){

		if(MY_CALCULATION$NOTES[i]%in%"VAR1"){
			CALCUL <- C_X %>% inner_join(select(C_Y,KEY,Value.1 = Value ),by = "KEY")
		}
		if(MY_CALCULATION$NOTES[i]%in%"VAR2"){
			CALCUL <- C_Y %>% mutate(Value.1 = Value) %>% select(-Value) %>% inner_join(select(C_X, KEY , Value ),by = "KEY")
		}
		rm(C_X,C_Y)

		if(!plyr:::empty(CALCUL)){
			if(MY_CALCULATION[i,"USE"]%in%"SUM")	{
				CALCUL <- CALCUL %>% mutate(Value = Value + Value.1)
			}
		if(MY_CALCULATION[i,"USE"]%in%"DIFF")	{
			CALCUL <- CALCUL %>% mutate(Value = Value - Value.1)
		}
			###### DIFFF
		CALCUL 	<- CALCUL %>% 
					filter(!KEY%in%Y_REF$KEY) %>% 
					mutate(Sex_Code = as.character(MY_CALCULATION[i,"COMPUTE"]),
					Value = ifelse(Value<0.00001 ,0,Value),
					Add_Status = paste0(Add_Status,"CLASS"))
		}
		CALCUL <- CALCUL %>%	
					select(-Value.1) 									
													
		X 	<- X %>% 
				bind_rows(CALCUL); rm(CALCUL)
	}
	X 	<- X %>% 
			select(-KEY,-VAR)
invisible(gc(reset = TRUE))
}
X %>% 
	filter(substr(Add_Status,nchar(Add_Status)-4, nchar(Add_Status))%in% "CLASS") %>% 
	mutate(Add_Status = gsub("CLASS", "C", Add_Status)) %>% {invisible(gc(reset = TRUE)); .}


}

#' @export

plyDataClassificationST <- function(X, MY_CALCULATION,MY_REF_CLASS ){	
if(nrow(X) == 0) return(NULL)
key 		<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")



if(!MY_CALCULATION$USE%in%"EQUAL"){
			SKIP <- X %>% filter(((!Value_Status_Code%in%NA & Value%in%NA) | substr(Indicator_Code,nchar(Indicator_Code)-1,nchar(Indicator_Code))%in%"RT" | substr(Indicator_Code,1,3)%in%c("CPI","HOW","EAR","NOM","DEN")))
			X <- X %>% filter(!((!Value_Status_Code%in%NA & Value%in%NA) | substr(Indicator_Code,nchar(Indicator_Code)-1,nchar(Indicator_Code))%in%"RT" | substr(Indicator_Code,1,3)%in%c("CPI","HOW","EAR","NOM","DEN")))
}
	
X <- eval(parse(text= paste0("  X %>% 	mutate( KEY = paste(",paste0(c(key[!key%in%MY_REF_CLASS],"Time","Freq_Code"), collapse=","),",sep='/'),",
												"VAR = ",MY_REF_CLASS,")"))) %>%
										filter(!Value%in%NA  | (!Value_Status_Code%in%c(NA,"",NaN) & substr(Time,5,5)%in%"M"))
REF <- MY_CALCULATION$COMPUTE
invisible(gc(reset = TRUE))		
Drop <- unlist(strsplit(MY_CALCULATION$Drop, "_"))
X <- X %>% mutate(test = 0)
if (length(Drop) >1 | (unique(!is.na(Drop)))) {
	for (k in 1:length(Drop)){
				X <- X %>% mutate(test = ifelse(stringr::str_extract(Qtable_Notes_String, Drop[k]) %in% Drop[k],test + 1, test ))
	}
}

Y_REF <- X %>% filter(VAR %in% REF | test >0)
invisible(gc(reset = TRUE))
VARUM <- unlist(strsplit(MY_CALCULATION$VAR1,";"))
C_X <- X %>% filter(VAR %in% VARUM[1] & !KEY%in%Y_REF$KEY)
invisible(gc(reset = TRUE))
if(length(VARUM)>1 & !plyr:::empty(C_X)){
	for (k in 2:length(VARUM)){
		ADD <- X %>% 
				filter(VAR %in% VARUM[k] & !KEY%in%Y_REF$KEY)
		C_X <- C_X %>% 
				inner_join(select(ADD,KEY,Value.1 = Value),by = "KEY") %>%
				mutate(Value = Value+ Value.1) %>%
				select(-Value.1)
	}
} 
rm(VARUM)

VARUM <- unlist(strsplit(MY_CALCULATION$VAR2,";"))
C_Y <- X %>% filter(VAR %in% VARUM[1] & !KEY%in%Y_REF$KEY)
if(length(VARUM)>1 & !plyr:::empty(C_Y)){
	for (k in 2:length(VARUM)){
		ADD <- X %>% 
				filter(VAR %in% VARUM[k] & !KEY%in%Y_REF$KEY)
		C_Y <- C_Y %>% 
				inner_join(select(ADD,KEY,Value.1 = Value),by = "KEY") %>%
				mutate(Value = Value+ Value.1) %>%
				select(-Value.1)
	}
} 
rm(VARUM)

if(!plyr:::empty(C_X) & !plyr:::empty(C_Y)){
	if(MY_CALCULATION$NOTES%in%"VAR1")	{
		CALCUL <- C_X %>% inner_join(select(C_Y,KEY,Value.1 = Value ),by = "KEY")
	}
	if(MY_CALCULATION$NOTES%in%"VAR2")	{
		CALCUL <- C_Y %>% 
					mutate(Value.1 = Value) %>% 
					select(-Value) %>% 
					inner_join(select(C_X, KEY , Value ),by = "KEY")
	}
	rm(C_X,C_Y)

	if(!plyr:::empty(CALCUL)){
		if(MY_CALCULATION$USE%in%"SUM")	{
			CALCUL <- CALCUL %>% mutate(Value = Value + Value.1)
		}
		if(MY_CALCULATION$USE%in%"DIFF")	{
			CALCUL <- CALCUL %>% mutate(Value = Value - Value.1)
		}
		if(MY_CALCULATION$USE%in%"PROD")	{
			CALCUL <- CALCUL %>% mutate(Value = Value * Value.1)
		}
		if(MY_CALCULATION$USE%in%"DIV")	{
			CALCUL <- CALCUL %>% mutate(Value = (Value / Value.1) * 100)
		}
		CALCUL <- eval(parse(text= paste0("  CALCUL %>% mutate(",MY_REF_CLASS,"  = '",paste0(REF),"')")))	%>%
						filter(	!KEY%in%Y_REF$KEY,
								!Value%in%c(NA,NaN)) %>% 
						mutate(	Value = ifelse(Value<0.000001,0,Value),
								Add_Status = paste0(Add_Status,"CLASS"))
 
	}
	CALCUL <- CALCUL %>%	
				select(-Value.1) 		

			######### changement de notes
	if(!MY_CALCULATION$Change%in%NA){
		test <- as.data.frame(cbind(ID = unlist(strsplit(MY_CALCULATION$Change,"/")),BIS = unlist(strsplit(MY_CALCULATION$Change,"/"))), stringsAsFactors=FALSE)
		test <- test %>% separate_("ID",c("OLD","NEW"), sep="=") #My_unsplit_KEY(test,"ID",c("OLD","NEW"),"=")
		test <- test[,1:3]
		for (k in 1:nrow(test)){ 
			CALCUL <- eval(parse(text= paste0("  CALCUL %>% mutate(Value_Notes_String  = gsub('",paste0(test[k,"OLD"]),"','",paste0(test[k,"NEW"]),"',Value_Notes_String, fixed = TRUE))")))
			CALCUL <- eval(parse(text= paste0("  CALCUL %>% mutate(Value_Status_Code  = gsub('",paste0(test[k,"OLD"]),"','",paste0(test[k,"NEW"]),"',Value_Status_Code, fixed = TRUE))")))
		}
		rm(test)
	}
	X <- X %>% bind_rows(CALCUL); rm(CALCUL)
	invisible(gc(reset = TRUE))
}

X %>% select(-KEY,-VAR) %>% 
		filter(substr(Add_Status,nchar(Add_Status)-4, nchar(Add_Status))%in% "CLASS") %>% 
		mutate(Add_Status = gsub("CLASS", "C", Add_Status)) %>% select(-test) %>% {invisible(gc(reset = TRUE)); .}
}

#' @export

plyDataIndicatorST <- function(X, MY_CALCULATION){	
if(nrow(X) == 0) return(NULL)
key 		<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")

for (i in 1:nrow(MY_CALCULATION)){

	X <- X %>% 
			mutate(VAR = Indicator_Code) %>%
			unite_("KEY", c(key[!key%in%"Indicator_Code"],"Time","Freq_Code"), sep="/", remove = FALSE  ) %>%
			filter(!Value%in%NA | (!Value_Status_Code%in%c(NA,"",NaN) & substr(Time,5,5)%in%"M"))
invisible(gc(reset = TRUE))
	REF <- MY_CALCULATION[i,"COMPUTE"]
	Y_REF <- X %>% filter(VAR %in% REF)

	VARUM <- MY_CALCULATION$VAR1[i]
	C_X <- X %>% filter(VAR %in% VARUM & !KEY%in%Y_REF$KEY)
invisible(gc(reset = TRUE))
	VARUM <- unlist(strsplit(MY_CALCULATION$VAR2[i],";"))
	C_Y <- X %>% filter(VAR %in% VARUM[1] & !KEY%in%Y_REF$KEY)
	if(length(VARUM)>1 & !plyr:::empty(C_Y)){
		for (k in 2:length(VARUM)){
			ADD <- X %>% filter(VAR %in% VARUM[k] & !KEY%in%Y_REF$KEY)
			C_Y <- C_Y %>% 
				inner_join(select(ADD,KEY,Value.1 = Value),by = "KEY") %>%
				mutate(Value = Value+ Value.1) %>%
				select(-Value.1)
		}
	}


invisible(gc(reset = TRUE))
# sum, diff, prod
	if(!plyr:::empty(C_X) & !plyr:::empty(C_Y)){
		CALCUL <- NULL	
		if(MY_CALCULATION$NOTES[i]%in%"VAR1"){
			CALCUL <- C_X %>% inner_join(select(C_Y,KEY,Value.1 = Value ),by = "KEY")
		}
			
			
		if(MY_CALCULATION$NOTES[i]%in%"VAR2"){
			CALCUL <- C_Y %>% mutate(Value.1 = Value) %>% select(-Value) %>% inner_join(select(C_X, KEY , Value ),by = "KEY")
		}
		rm(C_X,C_Y)


		if(!plyr:::empty(CALCUL)){
			if(MY_CALCULATION[i,"USE"]%in%"SUM")	{
				CALCUL <- CALCUL %>% mutate(Value = Value + Value.1)
			}
			if(MY_CALCULATION[i,"USE"]%in%"DIFF")	{
			CALCUL <- CALCUL %>% mutate(Value = Value - Value.1)
			}
			if(MY_CALCULATION[i,"USE"]%in%"PROD")	{
			CALCUL <- CALCUL %>% mutate(Value = Value * Value.1)
			}
			if(MY_CALCULATION[i,"USE"]%in%"DIV")	{
			CALCUL <- CALCUL %>% mutate(Value = (Value / Value.1) * 100)
			}

			CALCUL <- eval(parse(text= paste0("  CALCUL %>% mutate(Indicator_Code  = '",paste0(REF),"')")))	%>%
									filter(	!KEY%in%Y_REF$KEY,
											!Value%in%NaN) %>% 
									mutate(	Value = ifelse(Value<0.000001 ,0,Value),
											Add_Status = paste0(Add_Status,"INDIC"))
		}

		CALCUL <- CALCUL %>%	select(-Value.1)  		
		X <- X %>% bind_rows(CALCUL); rm(CALCUL)
	}
	
	X <- X %>% select(-KEY,-VAR)
	invisible(gc(reset = TRUE))

}


X %>% filter(substr(Add_Status,nchar(Add_Status)-4, nchar(Add_Status))%in% "INDIC") %>% 
			mutate(Add_Status = gsub("INDIC", "C", Add_Status, fixed = TRUE)) %>% {invisible(gc(reset = TRUE)); .}
}

#' @export

plyDataQuarterST <- function(X, MY_CALCULATION){	
if(nrow(X) == 0) return(NULL)
key <- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")

X 	<- X %>% 
		mutate(Time = ifelse(substr(Time,5,5)%in%"",paste(substr(Time,1,4),"Y00",sep=""),Time),
				Time = as.character(Time))

if(MY_CALCULATION$USE%in%"MEAN"){
	X 	<- X %>% 
			filter(!((substr(Indicator_Code,nchar(Indicator_Code)-1,nchar(Indicator_Code))%in%"RT" | substr(Indicator_Code,1,3)%in%c("CPI","HOW","EAR") ) & Freq_Code%in%"M"))
}

REF <- MY_CALCULATION$COMPUTE
REF_PER <- unlist(strsplit(MY_CALCULATION$NOTES,";"))
X <- X %>% 
		mutate(VAR = substr(Time,5,7)) %>%
		unite_("ID", c(key,"Time","Freq_Code"), sep="/", remove = FALSE) %>%
		unite_("KEY", c(key,"Freq_Code"), sep ="/", remove = FALSE) %>%
		mutate(KEY = paste(KEY,substr(Time,1,4), sep="/")) %>%
		filter(!Value%in%NA | (!Value_Status_Code%in%c("p","s","u","f",NA,"",NaN) & substr(Time,5,5)%in%"M"))
invisible(gc(reset = TRUE))
Y_REF <- X %>% filter(VAR %in% REF & Freq_Code %in% REF_PER)


VARUM <- unlist(strsplit(MY_CALCULATION$VAR1,";"))
CALCUL <- X %>%filter(	VAR %in% VARUM[1] &	Freq_Code %in% REF_PER & !KEY%in%Y_REF$KEY)

NEW_COLNAME <- colnames(CALCUL)[!colnames(CALCUL)%in%c("KEY","Value")]
invisible(gc(reset = TRUE))
if(length(VARUM)>1){
	for (k in 2:length(VARUM)){
		ADD 	<- X %>% 
					filter(VAR %in% VARUM[k] &	Freq_Code %in% REF_PER & !KEY%in%Y_REF$KEY)
		CALCUL 	<- CALCUL %>% 
					inner_join(select(ADD,KEY,Value.1 = Value, Value_Status_Code.1 = Value_Status_Code),by = "KEY") %>%
					mutate(	Value = ifelse(!Value_Status_Code.1%in%NA,NA,Value),
							Value_Status_Code= ifelse(!Value_Status_Code.1%in%c("",NA,NaN),Value_Status_Code.1,Value_Status_Code),
							Value_Status_Code = as.character(Value_Status_Code)	) 

		CALCUL <- CALCUL %>% select_(.dots  = c("KEY",NEW_COLNAME,"Value","Value.1"))

		if(MY_CALCULATION$USE%in%"MEAN")	{
			CALCUL <- CALCUL %>% 
						mutate(Value = Value + Value.1)
		}
		CALCUL <- CALCUL %>%	select(-Value.1) 
	}
}


rm(X)
invisible(gc(reset = TRUE))
	
eval(parse(text= paste0("  CALCUL %>% mutate(Time  = paste0(substr(Time,1,4),'",paste0(REF),"'))")))	 %>%	
					mutate(	Value = Value/length(VARUM), 
							Add_Status = paste0(Add_Status,"C"),
							Time = as.character(Time)) %>%
				 filter(	!KEY%in%Y_REF$KEY,
							!Value%in%NA | !Value_Status_Code%in%c(NA,"",NaN)) %>%
				select(-KEY,-VAR, -ID) %>%
				mutate(Time = ifelse(substr(Time,5,7)%in%"Y00",substr(Time,1,4),Time)) %>% {invisible(gc(reset = TRUE)); .}
}
 
#' @export

plyDataYearST <- function(X, MY_CALCULATION){	
if(nrow(X) == 0) return(NULL)
key 		<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")

X <- X %>% mutate(	Time = ifelse(substr(Time,5,5)%in%"",paste(substr(Time,1,4),"Y00",sep=""),Time),
					Time = as.character(Time))


if(MY_CALCULATION$USE%in%"MEAN"){
	X 	<- X %>% 
			filter(!((substr(Indicator_Code,nchar(Indicator_Code)-1,nchar(Indicator_Code))%in%"RT" | substr(Indicator_Code,1,3)%in%c("CPI","HOW","EAR","NOM","DEN") ) & X$Freq_Code%in%c("M","X","P","T","R","S","Q","Y","L","I","H","J","K","O","Z","G","A","B","C","V","W","D","N","E","F") | substr(X$Time,5,5)%in%"M"))
}


REF 	<- MY_CALCULATION$COMPUTE
REF_PER <- unlist(strsplit(MY_CALCULATION$NOTES,";"))

X <- eval(parse(text= paste0("  X %>% mutate(	
						ID = 	paste(",paste0(c(key,"Time","Freq_Code"), collapse=","),",sep='/'),
						KEY = 	paste(",paste0(c(key,"Freq_Code"), collapse=","),",substr(Time,1,4),sep='/'),
						VAR = substr(Time,5,7)   )"))) %>%
				filter(!Value%in%NA | (!Value_Status_Code%in%c("p","s","u","f",NA,"",NaN) & substr(Time,5,5)%in%"M"))

invisible(gc(reset = TRUE))		

Y_REF <- X %>% filter(VAR %in% REF & Freq_Code %in% REF_PER)


VARUM <- unlist(strsplit(MY_CALCULATION$VAR1,";"))
CALCUL <- X %>%filter(	VAR %in% VARUM[1] &	Freq_Code %in% REF_PER & !KEY%in%Y_REF$KEY)

NEW_COLNAME <- colnames(CALCUL)[!colnames(CALCUL)%in%c("KEY","Value")]

if(length(VARUM)>1){
	for (k in 2:length(VARUM)){
		ADD <- X %>% filter(	VAR %in% VARUM[k] &	Freq_Code %in% REF_PER & !KEY%in%Y_REF$KEY)
		CALCUL <- CALCUL %>% 
					inner_join(select(ADD,KEY,Value.1 = Value, Value_Status_Code.1 = Value_Status_Code),by = "KEY") %>%
					mutate(	Value = ifelse(!Value_Status_Code.1%in%NA,NA,Value),
							Value_Status_Code= ifelse(!Value_Status_Code.1%in%c("",NA,NaN),Value_Status_Code.1,Value_Status_Code),
							Value_Status_Code = as.character(Value_Status_Code)	) 

		CALCUL <- CALCUL %>% select_(.dots  = c("KEY",NEW_COLNAME,"Value","Value.1"))

	if(MY_CALCULATION$USE%in%"MEAN"){
		CALCUL <- CALCUL %>% mutate(Value = Value + Value.1)
	}
	CALCUL <- CALCUL %>%	select(-Value.1) 
	}
}

rm(X)
invisible(gc(reset = TRUE))
eval(parse(text= paste0("  CALCUL %>% mutate(Time  = paste0(substr(Time,1,4),'",paste0(REF),"'))")))	 %>%	
					mutate(	Value = Value/length(VARUM), 
							Add_Status = paste0(Add_Status,"C"),
							Time = as.character(Time)) %>%
				 filter(	!KEY%in%Y_REF$KEY,
							!Value%in%NA | !Value_Status_Code%in%c(NA,"",NaN))%>% 
				select(-KEY,-VAR,-ID)%>% 
				mutate(Time = ifelse(substr(Time,5,7)%in%"Y00",substr(Time,1,4),Time)) %>% {invisible(gc(reset = TRUE)) ; .}

}

#' @export

plyDataIndicatorRateST <- function(X , MY_CALCULATION){	
if(nrow(X) == 0) return(NULL)
# X <- X %>% filter(	Indicator_Code %in% c(test[i,"COMPUTE"],str_split(test[i,"VAR1"],pattern = ';') %>% unlist,str_split(test[i,"VAR2"],pattern = ';') %>% unlist))

key 		<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")


X <- X %>% 	
		unite_("KEY", c(key[!key%in%"Indicator_Code"],"Time","Freq_Code"), sep ="/", remove = FALSE) %>%
		mutate(VAR = Indicator_Code)   %>%
		filter(!Value%in%NA | (!Value_Status_Code%in%c(NA,"",NaN) & substr(Time,5,5)%in%"M"))

invisible(gc(reset = TRUE))
REF <- MY_CALCULATION$COMPUTE
Y_REF <- X %>% filter(VAR %in% REF)





VARUM <- unlist(strsplit(MY_CALCULATION$VAR1,";"))
C_X <- X %>% filter(VAR %in% VARUM[1] & !KEY%in%Y_REF$KEY)
invisible(gc(reset = TRUE))
if(length(VARUM)>1 & !plyr:::empty(C_X)){
	for (k in 2:length(VARUM)){
		ADD <- X %>% 
				filter(VAR %in% VARUM[k] & !KEY%in%Y_REF$KEY)
		C_X <- C_X %>% 
				inner_join(select(ADD,KEY,Value.1 = Value),by = "KEY") %>%
				mutate(Value = Value+ Value.1) %>%
				select(-Value.1)
	}
} 
rm(VARUM)

NEW_COLNAME <- colnames(C_X)[!colnames(C_X)%in%c("KEY","Value")]


VARUM <- unlist(strsplit(MY_CALCULATION$VAR2,";"))
C_Y <- X %>% filter(VAR %in% VARUM[1] & !KEY%in%Y_REF$KEY)
if(length(VARUM)>1 & !plyr:::empty(C_Y)){
	for (k in 2:length(VARUM)){
		ADD <- X %>% 
				filter(VAR %in% VARUM[k] & !KEY%in%Y_REF$KEY)
		C_Y <- C_Y %>% 
				inner_join(select(ADD,KEY,Value.1 = Value),by = "KEY") %>%
				mutate(Value = Value+ Value.1) %>%
				select(-Value.1)
	}
} 
rm(VARUM)


# VARUM <- MY_CALCULATION$VAR1
# C_X <- X %>% filter(VAR %in% VARUM)


# VARUM <- MY_CALCULATION$VAR2
# C_Y <- X %>% filter(VAR %in% VARUM & !KEY%in%Y_REF$KEY)
# invisible(gc(reset = TRUE))



if(!plyr:::empty(C_X) & !plyr:::empty(C_Y)){
	if(MY_CALCULATION$NOTES%in%"VAR1")	{
		CALCUL <- C_X %>% inner_join(select(C_Y,KEY,Value.1 = Value, Value_Status_Code.1 = Value_Status_Code ),by = "KEY")
	}
	if(MY_CALCULATION$NOTES%in%"VAR2")	{
		CALCUL <- C_Y %>% 	mutate(Value.1 = Value,Value_Status_Code.1 = Value_Status_Code ) %>% 
						select(-Value,-Value_Status_Code) %>% 
						inner_join(select(C_X, KEY , Value, Value_Status_Code ),by = "KEY")
	}
	rm(C_X,C_Y)

	CALCUL <- CALCUL %>% mutate(Value_Status_Code = ifelse((!Value_Status_Code.1%in%c("",NA,NaN) & Value%in%NA),Value_Status_Code.1,Value_Status_Code),
								Value_Status_Code = as.character(Value_Status_Code)	) %>% select(-Value_Status_Code.1) 

	CALCUL <- CALCUL %>% select_(.dots  = c("KEY",NEW_COLNAME,"Value","Value.1"))


	if(!plyr:::empty(CALCUL)){
		if(MY_CALCULATION$USE%in%"DIV")	{
			CALCUL <- CALCUL %>% mutate(Value = (Value / Value.1) * 100)
		}
	CALCUL <- eval(parse(text= paste0("  CALCUL %>% mutate(Indicator_Code  = '",paste0(REF),"')"))) %>% 
				 mutate(	Value_Status_Code = as.character(Value_Status_Code))	%>%
				 filter(	!KEY%in%Y_REF$KEY,
							!Value%in%c(NaN,Inf) | !Value_Status_Code%in%c(NA,"",NaN)) %>% 
				 mutate(	Value = ifelse(as.numeric(Value)<0.000001 ,0,Value),
							Add_Status = paste0(Add_Status,"C"))
	}
 CALCUL %>%	
			select(-Value.1) %>%  
			filter(!Value%in%c(NaN,Inf)) %>% 
			select(-KEY,-VAR) %>% {invisible(gc(reset = TRUE)) ; .}	

}
else {
	invisible(gc(reset = TRUE))
	NULL
}
}

#' @export

plyDataIndicatorDistributionST <- function(X , MY_CALCULATION, mywd){	
if(nrow(X) == 0) return(NULL)
# X <- DAVID ; X <- X %>% filter(	indicator %in% test$VAR1[i]); MY_CALCULATION <- test[i,]


group <- c(c("ref_area", "source", "indicator", 'time'), unlist(str_split(as.character(MY_CALCULATION$VAR2), ";")))
options(warn = -1)	
X <- X %>% 	
		group_by_(.dots = group) %>% 
		mutate(obs_value = obs_value / max(obs_value, na.rm = TRUE) * 100) %>% 
		ungroup %>% 
		mutate(	indicator = MY_CALCULATION$COMPUTE, 
				obs_value = round(obs_value, 4)) %>% 
		filter(!obs_value %in% c(NA, NaN, -Inf, 'NaN'))
options(warn = 0)	

########## test


invisible(gc(reset = TRUE))


if(!plyr:::empty(X)){
	TEST <- X %>% 	filter_(paste0(MY_CALCULATION$USE, " %in% c('", paste0(unlist(str_split(as.character(MY_CALCULATION$NOTES), ";")), collapse ="', '"), "')")) %>% 
					filter(as.numeric(obs_value) < 100) %>% 
					mutate(test = 'ERROR')
 
	ref <- X %>% 	left_join(TEST, by = c("ref_area", "indicator", "source", "sex_version", "classif1_version", "classif2_version", "time", "sex", "classif1", "classif2", "obs_value", "obs_status", "note_classif", "note_indicator","note_source", "freq_code")) %>% 
					filter(test %in% 'ERROR' ) 

					
					
	if(nrow(ref) > 0){
	
		survey <- c('453', '469', '147', '536','2257','2258','2253','2259','2249','2242','2487','2244','2518','2486','2260','2247','2240','2251','2237','772','2255','2238','2245','2261','2246','2239','2248','2236','2250','2241','2252','2235','2254','2243','2519','2256')	
	
		TEST <- ref %>% filter(!source %in%  paste0('BA:', survey))
		
		rm(survey)
		
		if(nrow(TEST) > 0){
				data.table:::fwrite(TEST,file = paste0(mywd, 'ILO_Data/check/DISTRIBUTION_',unique(ref$ref_area),'.csv'), na = '')
		}
		
		invisible(gc(reset = TRUE))
	
		ref <- ref %>% distinct(indicator, source, sex, classif1, classif2, time) %>% mutate(test = 'ERROR')
	
		X <- X %>% 	
				left_join(ref, by = c("indicator", "source", "sex", "classif1", "classif2", "time")) %>% 
				mutate(	obs_value = ifelse( test %in% 'ERROR', NA, obs_value) , 
						obs_status = ifelse( test %in% 'ERROR', 'U', obs_status)) %>% 
				select(-test) 
		
	}
	
	rm(TEST, ref)

	}

	invisible(gc(reset = TRUE))

####################### delete empty

key_REF <- c("ref_area", "indicator", "source", "sex_version","classif1_version","classif2_version", "time")

DEL 	<- c(		"GEO_COV_NAT | GEO_COV_X","GEO_COV_X | GEO_COV_NAT",
					"ECO_AGGREGATE_TOTAL | ECO_AGGREGATE_X","ECO_AGGREGATE_X | ECO_AGGREGATE_TOTAL",
					"ECO_ISIC2_TOTAL | ECO_ISIC2_0","ECO_ISIC2_0 | ECO_ISIC2_TOTAL",
					"ECO_ISIC3_TOTAL | ECO_ISIC3_X","ECO_ISIC3_X | ECO_ISIC3_TOTAL",
					"ECO_ISIC4_TOTAL | ECO_ISIC4_X","ECO_ISIC4_X | ECO_ISIC4_TOTAL",
					"ECO_SECTOR_TOTAL | ECO_SECTOR_X","ECO_SECTOR_X | ECO_SECTOR_TOTAL",
					"HOW_BANDS_TOTAL | HOW_BANDS_X", "HOW_BANDS_X | HOW_BANDS_TOTAL",
					"OCU_AGGREGATE_TOTAL | OCU_AGGREGATE_X","OCU_AGGREGATE_X | OCU_AGGREGATE_TOTAL",
					"OCU_SKILL_TOTAL | OCU_SKILL_X","OCU_SKILL_X | OCU_SKILL_TOTAL",
					"OCU_ISCO88_TOTAL | OCU_ISCO88_X","OCU_ISCO88_X | OCU_ISCO88_TOTAL",
					"OCU_ISCO08_TOTAL | OCU_ISCO08_X","OCU_ISCO08_X | OCU_ISCO08_TOTALOCU_ISCO08_X",
					"STE_ICSE93_TOTAL | STE_ICSE93_6", "STE_ICSE93_6 | STE_ICSE93_TOTAL",
					"STE_ICSE93_TOTAL | STE_ICSE93_1 | STE_ICSE93_3", "STE_ICSE93_1 | STE_ICSE93_3 | STE_ICSE93_TOTAL",
					"STE_AGGREGATE_TOTAL | STE_AGGREGATE_X","STE_AGGREGATE_X | STE_AGGREGATE_TOTAL",
					"EDU_AGGREGATE_TOTAL | EDU_AGGREGATE_X","EDU_AGGREGATE_X | EDU_AGGREGATE_TOTAL",
					"EDU_ISCED97_TOTAL | EDU_ISCED97_UNK","EDU_ISCED97_UNK | EDU_ISCED97_TOTAL",
					"EDU_ISCED11_TOTAL | EDU_ISCED11_9","EDU_ISCED11_X | EDU_ISCED11_TOTAL",
					"DUR_AGGREGATE_TOTAL | DUR_AGGREGATE_X","DUR_AGGREGATE_X | DUR_AGGREGATE_TOTAL",
					"DUR_DETAILS_TOTAL | DUR_DETAILS_X","DUR_DETAILS_X | DUR_DETAILS_TOTAL",
					"AGE_AGGREGATE_TOTAL | AGE_AGGREGATE_Y15-24","AGE_AGGREGATE_Y15-24 | AGE_AGGREGATE_TOTAL",
					"AGE_AGGREGATE_TOTAL | AGE_AGGREGATE_Y55-64","AGE_AGGREGATE_Y55-64 | AGE_AGGREGATE_TOTAL",
					"AGE_10YRBANDS_TOTAL | AGE_10YRBANDS_Y15-24","AGE_10YRBANDS_Y15-24 | AGE_10YRBANDS_TOTAL",
					"CAT_UNE_TOTAL | CAT_UNE_UNK", "CAT_UNE_UNK | CAT_UNE_TOTAL", 
					"AGE_10YRBANDS_TOTAL",
					"AGE_5YRBANDS_TOTAL",
					"JOB_TIME_TOTAL",
					"AGE_YTHADULT_Y15-64",
					"AGE_AGGREGATE_TOTAL",
					"AGE_YTHADULT_YGE15", 
					"EDU_ISCED97_TOTAL",
					'EDU_ISCED11_TOTAL',
					"EDU_AGGREGATE_TOTAL",
					"DUR_DETAILS_TOTAL",
					"DUR_AGGREGATE_TOTAL", 					
					"ECO_ISIC4_TOTAL", 
					"ECO_ISIC3_TOTAL", 
					"ECO_AGGREGATE_TOTAL",
					"ECO_SECTOR_TOTAL",
					"OCU_SKILL_TOTAL",
					"OCU_ISCO88_TOTAL",
					"OCU_ISCO08_TOTAL",
					"HOW_BANDS_TOTAL", 
					"ECO_ISIC2_TOTAL", 
					"STE_ICSE93_TOTAL", 
					"STE_AGGREGATE_TOTAL",
					"GEO_COV_NAT", 
					"CAT_UNE_TOTAL"
			)


X <- X %>% filter(!(classif1 %in% c('STE_ICSE93_6','STE_AGGREGATE_X') &  obs_value == 0 & obs_status%in%NA)) %>%
		unite_("ID", c(key_REF), sep ="/", remove = FALSE )

# start by cleaning at cl2 levels
Y <- X %>%	
		filter(!obs_value%in%NA) %>% 
		group_by(ref_area, indicator,  source, sex_version,classif1_version,classif2_version,  time) %>% 
		summarise(ID = first(ID),TEST =  paste(unique(classif2), collapse= " | ")) %>%
		ungroup() %>%
		filter(TEST%in%DEL)
		
invisible(gc(reset = TRUE))	
if(!plyr:::empty(Y)){			
	X <- X %>% 
		filter(!ID%in%Y$ID) ;rm(Y)
}

# start by cleaning at cl1 levels
Y <- X %>%	
		filter(!obs_value%in%NA) %>% 
		group_by(ref_area, indicator,  source, sex_version,classif1_version,classif2_version,   time) %>% 
		summarise(ID = first(ID), TEST =  paste(unique(classif1), collapse= " | ")) %>%
		ungroup() %>%
		filter(TEST%in%DEL | (indicator %in% c("TRU_TTRU_SEX_ECO_NB", "TRU_DEMP_SEX_ECO_RT") & TEST %in% c("ECO_SECTOR_TOTAL")))

		
if(!plyr:::empty(Y)){			
	X <- X %>% filter(!ID%in%Y$ID) ; rm(Y)
}

invisible(gc(reset = TRUE))
X <- X %>% 	filter(!(classif1_version%in%NA & !classif2_version%in%NA) ) %>%
		select(-ID)
		
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))


# desactivate
############# 
if(nrow(X) > 1) return(X)

#return(NULL)		
			
	
	
	
	
	
	
	
	
	
	
	
	
}

#' @export

plyDataIndicatorClassificationST <- function(X, MY_CALCULATION){	
if(nrow(X) == 0) return(NULL)
key 		<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")

X <- X %>% 
		unite_("ID", c(key,"Time","Freq_Code"), sep ="/", remove = FALSE) %>%
		unite_("KEY",c(key[!key%in%c("Indicator_Code","Classif2_Code")],"Time","Freq_Code"), sep="/", remove = FALSE ) %>%
		mutate(VAR = paste(Indicator_Code,Classif2_Code,sep='/')) %>%
		filter(!Value%in%NA | (!Value_Status_Code%in%c(NA,"",NaN) & substr(Time,5,5)%in%"M"))

invisible(gc(reset = TRUE))		
REF 	<- MY_CALCULATION$COMPUTE
Y_REF 	<- X[X$VAR %in% REF,]

VARUM <- unlist(strsplit(MY_CALCULATION$VAR1,";"))
C_X <- X %>% filter(VAR %in% VARUM)
NEW_COLNAME <- colnames(C_X)[!colnames(C_X)%in%c("KEY","Value")]

VARUM <- unlist(strsplit(MY_CALCULATION$VAR2,";"))
C_Y <- X %>% filter(VAR %in% VARUM & !KEY%in%Y_REF$KEY)
invisible(gc(reset = TRUE))
# sum, diff, prod
if(!plyr:::empty(C_X) & !plyr:::empty(C_Y)){


	if(MY_CALCULATION$NOTES%in%"VAR1")	{
		CALCUL <- C_X %>% inner_join(select(C_Y,KEY,Value.1 = Value, Value_Status_Code.1 = Value_Status_Code ),by = "KEY")
	}
	if(MY_CALCULATION$NOTES%in%"VAR2"){
		CALCUL <- C_Y %>% 
					mutate(Value.1 = Value,Value_Status_Code.1 = Value_Status_Code ) %>% 
					select(-Value,-Value_Status_Code) %>% inner_join(select(C_X, KEY , Value, Value_Status_Code ),by = "KEY")
	}
	rm(C_X,C_Y)
	CALCUL <- CALCUL %>% mutate(Value_Status_Code = ifelse((!Value_Status_Code.1%in%c("",NA,NaN) & Value%in%NA),Value_Status_Code.1,Value_Status_Code),
								Value_Status_Code = as.character(Value_Status_Code)	) %>% select(-Value_Status_Code.1) 

	CALCUL <- CALCUL %>% select_(.dots  = c("KEY",NEW_COLNAME,"Value","Value.1"))


	if(!plyr:::empty(CALCUL)){

		if(MY_CALCULATION$USE%in%"DIV")	{
			CALCUL <- CALCUL %>% mutate(Value = (Value / Value.1) * 100)
		}

		CALCUL <- eval(parse(text= paste0("  CALCUL %>% mutate(Indicator_Code  = '",paste0(REF),"')"))) %>% 
						mutate(	Value_Status_Code = as.character(Value_Status_Code),
								Classif2_Code =  as.character("XXX_XXX_XXX"	)
								)	%>%
						filter(	!KEY%in%Y_REF$KEY,
								!Value%in%c(NaN,Inf) | !Value_Status_Code%in%c(NA,"",NaN)) %>% 
						mutate(	Value = ifelse(Value<0 & !Value%in%NA,0,Value),
								Add_Status = paste0(Add_Status,"C"))
	}
	CALCUL %>%	
					select(-Value.1) %>%  
					filter(!Value%in%c(NaN,Inf))	%>% 	
					select(-KEY,-VAR, -ID) %>% {invisible(gc(reset = TRUE)); .}
invisible(gc(reset = TRUE))

}
else {
invisible(gc(reset = TRUE))
	NULL
}
}

#' @export

reshapeDataLongFormatST <- function(X){

X %>% 
		unite_("ID", c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code", "Year", "Value_Status_Code", "Freq_Code", "Notes_Classif_Code", "Notes_Indicator_Code", "Notes_Source_Code", "Currency_Code"), sep="/", remove = TRUE) %>% 
		gather(Time, Value, -ID) %>% mutate(Time = as.character(Time)) %>%
		separate(ID, c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code", "Year", "Value_Status_Code", "Freq_Code", "Notes_Classif_Code", "Notes_Indicator_Code", "Notes_Source_Code", "Currency_Code"), sep="/", remove = TRUE) %>% 
		mutate_all(funs(mapvalues(.,c('NaN', '', ' ', 'NA'), c(NA, NA, NA, NA), warn_missing = FALSE))) %>%
		filter(!Value%in%c(NA,"","NA") | !Value_Status_Code%in%c("",NA,"NA")) %>%
		mutate(	Time = ifelse(!Time%in%"Y",paste0(Year,Time),Year),
				Value = as.numeric(Value)) %>%		
		select(-Year) %>%
		mutate(	Value_Status_Code =  ifelse((substr(Value_Status_Code,2,3)%in%"01" & !substr(Time,5,7) %in%c("M01","Q1","")), NA, Value_Status_Code),
				Value_Status_Code =  ifelse((substr(Value_Status_Code,2,3)%in%"02" & !substr(Time,5,7) %in%c("M02","Q1","")), NA, Value_Status_Code),
				Value_Status_Code =  ifelse((substr(Value_Status_Code,2,3)%in%"03" & !substr(Time,5,7) %in%c("M03","Q1","")), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"04" & !substr(Time,5,7) %in%c("M04","Q2",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"05" & !substr(Time,5,7) %in%c("M05","Q2",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"06" & !substr(Time,5,7) %in%c("M06","Q2",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"07" & !substr(Time,5,7) %in%c("M07","Q3",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"08" & !substr(Time,5,7) %in%c("M08","Q3",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"09" & !substr(Time,5,7) %in%c("M09","Q3",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"10" & !substr(Time,5,7) %in%c("M10","Q4",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"11" & !substr(Time,5,7) %in%c("M11","Q4",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"12" & !substr(Time,5,7) %in%c("M12","Q4",""), NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"13" & !substr(Time,5,7) %in%"Q1", NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"14" & !substr(Time,5,7) %in%"Q2", NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"15" & !substr(Time,5,7) %in%"Q3", NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"16" & !substr(Time,5,7) %in%"Q4", NA, Value_Status_Code),
				Value_Status_Code =  ifelse(substr(Value_Status_Code,2,3)%in%"17" & !substr(Time,5,7) %in%"", NA, Value_Status_Code)) %>% 
		mutate(Value_Status_Code = ifelse(Value_Status_Code%in%NA, NA, substr(Value_Status_Code,1,1)))  %>% 
		filter(!Value%in%c(NA,"","NA") | !Value_Status_Code%in%c("",NA,"NA")) %>% 
		mutate(	Add_Repository = as.character(NA), 	Add_Status = as.character(NA)) %>% 
		{invisible(gc(reset = TRUE)); .}
} 

#' @export

sortDataOracleST <- function(X){
# sorting part :

# Ariane:::CODE_ORA$T_SUR_SURVEY %>% mutate(SUR_ID = as.numeric(SUR_ID)) %>% arrange(desc(SUR_ID)) %>% fix


X  %>% 
		mutate(	Country_Code = 			factor(Country_Code, 			select(Ariane:::CODE_ORA$T_COU_COUNTRY, COU_ISO3_CODE,COUNTRY_SORT = COU_SORT_EN) %>% 
																			filter(COU_ISO3_CODE %in% levels(X$Country_Code)) %>% 
																			arrange(as.numeric(COUNTRY_SORT)) %>% 
																			select(Country_Code = COU_ISO3_CODE) %>% t %>% as.character), 
				Source_Code = 			factor(Source_Code, 			select(Ariane:::CODE_ORA$T_SUR_SURVEY, SUR_ID,SUR_SORT) %>% 
																			filter(SUR_ID %in% levels(X$Source_Code)) %>% 
																			arrange(as.numeric(SUR_SORT)) %>% 
																			select(Source_Code = SUR_ID) %>% t %>% as.character),
				Indicator_Code = 		factor(Indicator_Code, 			select(Ariane:::CODE_ORA$T_IND_INDICATOR, IND_CODE,IND_SORT) %>% 
																			filter(IND_CODE %in% levels(X$Indicator_Code)) %>% 
																			arrange(IND_SORT) %>% 
																			select(Indicator_Code = IND_CODE) %>% t %>% as.character),
				Sex_Version_Code = 		factor(Sex_Version_Code, 		select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																			filter(CLV_CODE %in% levels(X$Sex_Version_Code)) %>% 
																			arrange(as.numeric(CLV_SORT)) %>% 
																			select(Sex_Version_Code = CLV_CODE) %>% t %>% as.character),
				Classif1_Version_Code = factor(Classif1_Version_Code, 	select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																			filter(CLV_CODE %in% levels(X$Classif1_Version_Code)) %>% 
																			arrange(as.numeric(CLV_SORT)) %>% 
																			select(Classif1_Version_Code = CLV_CODE) %>% t %>% as.character),
				Classif2_Version_Code = factor(Classif2_Version_Code, 	select(Ariane:::CODE_ORA$T_CLV_CLASSIF_VERSION, CLV_CODE,CLV_SORT) %>% 
																			filter(CLV_CODE %in% levels(X$Classif2_Version_Code)) %>% 
																			arrange(as.numeric(CLV_SORT)) %>% 
																			select(Classif2_Version_Code = CLV_CODE) %>% t %>% as.character),
				Time = 					factor(Time, 					select(Ariane:::CODE_ORA$T_TIM_TIME, TIM_FORMAT_USER,TIM_ID) %>% 
																			filter(TIM_FORMAT_USER %in% levels(X$Time)) %>% 
																			arrange(as.numeric(TIM_ID)) %>% 
																			select(Time = TIM_FORMAT_USER) %>% t %>% as.character),
				Sex_Code = 				factor(Sex_Code, 				select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																			filter(CLA_CODE %in% levels(X$Sex_Code)) %>% 
																			arrange(as.numeric(CLA_SORT)) %>% 
																			select(Sex_Code = CLA_CODE) %>% t %>% as.character),
				Classif1_Code = 		factor(Classif1_Code, 			select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 
																			filter(CLA_CODE %in% levels(X$Classif1_Code)) %>% 
																			arrange(as.numeric(CLA_SORT)) %>% 
																			select(Classif1_Code = CLA_CODE) %>% t %>% as.character),
				Classif2_Code = 		factor(Classif2_Code, 			select(Ariane:::CODE_ORA$T_CLA_CLASSIF, CLA_CODE,CLA_SORT) %>% 	
																			filter(CLA_CODE %in% levels(X$Classif2_Code)) %>% 
																			arrange(as.numeric(CLA_SORT)) %>% 
																			select(Classif2_Code = CLA_CODE) %>% t %>% as.character)) %>%
		{invisible(gc(reset = TRUE)); .} %>%
		arrange(Country_Code, 
				Source_Code, 
				Indicator_Code, 
				Sex_Version_Code, 
				Classif1_Version_Code, 
				Classif2_Version_Code, 
				Time, 
				Sex_Code, 
				Classif1_Code, 
				Classif2_Code) %>%
		{invisible(gc(reset = TRUE)); .} %>%
		mutate(	Value_Status_Code = 	factor(Value_Status_Code, 	sort(levels(Value_Status_Code))),
				#Currency_Code = 		factor(Currency_Code, 		sort(levels(Currency_Code))),
				Value_Notes_String = 	factor(Value_Notes_String, 	sort(levels(Value_Notes_String))),
				Qtable_Notes_String = 	factor(Qtable_Notes_String, sort(levels(Qtable_Notes_String))))

}

#' @export

checkDataEmptyST <- function(X){

key_REF <- c("Country_Code", "Indicator_Code", "Source_Code", "Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code", "Time")

DEL 	<- c(		"GEO_COV_NAT | GEO_COV_X","GEO_COV_X | GEO_COV_NAT",
					"ECO_AGGREGATE_TOTAL | ECO_AGGREGATE_X","ECO_AGGREGATE_X | ECO_AGGREGATE_TOTAL",
					"ECO_ISIC2_TOTAL | ECO_ISIC2_0","ECO_ISIC2_0 | ECO_ISIC2_TOTAL",
					"ECO_ISIC3_TOTAL | ECO_ISIC3_X","ECO_ISIC3_X | ECO_ISIC3_TOTAL",
					"ECO_ISIC4_TOTAL | ECO_ISIC4_X","ECO_ISIC4_X | ECO_ISIC4_TOTAL",
					"ECO_SECTOR_TOTAL | ECO_SECTOR_X","ECO_SECTOR_X | ECO_SECTOR_TOTAL",
					"HOW_BANDS_TOTAL | HOW_BANDS_X", "HOW_BANDS_X | HOW_BANDS_TOTAL",
					"OCU_AGGREGATE_TOTAL | OCU_AGGREGATE_X","OCU_AGGREGATE_X | OCU_AGGREGATE_TOTAL",
					"OCU_SKILL_TOTAL | OCU_SKILL_X","OCU_SKILL_X | OCU_SKILL_TOTAL",
					"OCU_ISCO88_TOTAL | OCU_ISCO88_X","OCU_ISCO88_X | OCU_ISCO88_TOTAL",
					"OCU_ISCO08_TOTAL | OCU_ISCO08_X","OCU_ISCO08_X | OCU_ISCO08_TOTALOCU_ISCO08_X",
					"STE_ICSE93_TOTAL | STE_ICSE93_6", "STE_ICSE93_6 | STE_ICSE93_TOTAL",
					"STE_ICSE93_TOTAL | STE_ICSE93_1 | STE_ICSE93_3", "STE_ICSE93_1 | STE_ICSE93_3 | STE_ICSE93_TOTAL",
					"STE_AGGREGATE_TOTAL | STE_AGGREGATE_X","STE_AGGREGATE_X | STE_AGGREGATE_TOTAL",
					"JOB_TIME_TOTAL | JOB_TIME_X","JOB_TIME_X | JOB_TIME_TOTAL",
					"JOB_CONTRACT_TOTAL | JOB_CONTRACT_X","JOB_CONTRACT_X | JOB_CONTRACT_TOTAL",
					"EDU_AGGREGATE_TOTAL | EDU_AGGREGATE_X","EDU_AGGREGATE_X | EDU_AGGREGATE_TOTAL",
					"EDU_ISCED97_TOTAL | EDU_ISCED97_UNK","EDU_ISCED97_UNK | EDU_ISCED97_TOTAL",
					"EDU_ISCED11_TOTAL | EDU_ISCED11_9","EDU_ISCED11_X | EDU_ISCED11_TOTAL",
					"DUR_AGGREGATE_TOTAL | DUR_AGGREGATE_X","DUR_AGGREGATE_X | DUR_AGGREGATE_TOTAL",
					"DUR_DETAILS_TOTAL | DUR_DETAILS_X","DUR_DETAILS_X | DUR_DETAILS_TOTAL",
					"AGE_AGGREGATE_TOTAL | AGE_AGGREGATE_Y15-24","AGE_AGGREGATE_Y15-24 | AGE_AGGREGATE_TOTAL",
					"AGE_AGGREGATE_TOTAL | AGE_AGGREGATE_Y55-64","AGE_AGGREGATE_Y55-64 | AGE_AGGREGATE_TOTAL",
					"AGE_10YRBANDS_TOTAL | AGE_10YRBANDS_Y15-24","AGE_10YRBANDS_Y15-24 | AGE_10YRBANDS_TOTAL",
					"CAT_UNE_TOTAL | CAT_UNE_UNK", "CAT_UNE_UNK | CAT_UNE_TOTAL", 
					"AGE_10YRBANDS_TOTAL",
					"AGE_5YRBANDS_TOTAL",
					"JOB_TIME_TOTAL",
					"JOB_CONTRACT_TOTAL",
					"AGE_YTHADULT_Y15-64",
					"AGE_AGGREGATE_TOTAL",
					# "AGE_YTHADULT_YGE15", #keep for storing total only
					"EDU_ISCED97_TOTAL",
					'EDU_ISCED11_TOTAL',
					"EDU_AGGREGATE_TOTAL",
					"DUR_DETAILS_TOTAL",
					"DUR_AGGREGATE_TOTAL", 					
					"ECO_ISIC4_TOTAL", 
					"ECO_ISIC3_TOTAL", 
					"ECO_AGGREGATE_TOTAL",
					# "ECO_SECTOR_TOTAL", #keep for storing total only
					"OCU_SKILL_TOTAL",
					"OCU_ISCO88_TOTAL",
					"OCU_ISCO08_TOTAL",
					"HOW_BANDS_TOTAL", 
					"ECO_ISIC2_TOTAL", 
					"STE_ICSE93_TOTAL", 
					"STE_AGGREGATE_TOTAL",
					"GEO_COV_NAT", 
					"CAT_UNE_TOTAL"
			)


X <- X %>% filter(!(Classif1_Code %in% c('STE_ICSE93_6','STE_AGGREGATE_X') &  Value == 0 & Value_Status_Code%in%NA)) %>%
		unite_("ID", c(key_REF), sep ="/", remove = FALSE )

# start by cleaning at cl2 levels
Y <- X %>%	
		filter(!Value%in%NA) %>% 
		group_by(Country_Code, Indicator_Code,  Source_Code, Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,  Time) %>% 
		summarise(ID = first(ID),TEST =  paste(unique(Classif2_Code), collapse= " | ")) %>%
		ungroup() %>%
		filter(TEST%in%c(DEL, 'ECO_SECTOR_TOTAL', 'AGE_YTHADULT_YGE15'))
invisible(gc(reset = TRUE))	
if(!plyr:::empty(Y)){			
	X <- X %>% 
		filter(!ID%in%Y$ID) ;rm(Y)
}

# start by cleaning at cl1 levels
Y <- X %>%	
		filter(!Value%in%NA) %>% 
		group_by(Country_Code, Indicator_Code,  Source_Code, Sex_Version_Code,Classif1_Version_Code,Classif2_Version_Code,   Time) %>% 
		summarise(ID = first(ID), TEST =  paste(unique(Classif1_Code), collapse= " | ")) %>%
		ungroup() %>%
		filter(TEST%in%DEL | (Indicator_Code %in% c("UNE_TUNE_SEX_ECO_NB", 'UNE_TUNE_SEX_ECO_DT', "TRU_TTRU_SEX_ECO_NB", "TRU_DEMP_SEX_ECO_RT") & TEST %in% c("ECO_SECTOR_TOTAL"))) 

		
if(!plyr:::empty(Y)){			
	X <- X %>% filter(!ID%in%Y$ID) ; rm(Y)
}

invisible(gc(reset = TRUE))
X %>% 	filter(!(Classif1_Version_Code%in%NA & !Classif2_Version_Code%in%NA) ) %>%
		select(-ID)
		
		
}


#' @export

checkDataIndicatorDistributionFULLST <- function(X , MY_CALCULATION, mywd){	

# MY_CALCULATION <- test[i,] ; X <- X %>% filter(	Indicator_Code %in% test$VAR1[i]); test <- Ariane:::COMPUTE$INDICATOR_FULL_DT  


group <- c(c("ref_area", "source", "indicator", 'time'), unlist(str_split(as.character(MY_CALCULATION$VAR2), ";")))
options(warn = -1)	
X <- X %>% 	
		mutate(obs_value = as.numeric(obs_value)) %>%
		group_by_(.dots = group) %>% 
		mutate(obs_value = obs_value / max(obs_value, na.rm = TRUE) * 100) %>% 
		ungroup %>% 
		mutate(	indicator = MY_CALCULATION$COMPUTE, 
				obs_value = round(obs_value, 4), 
				obs_value = as.character(obs_value)) %>% 
		filter(!obs_value %in% 'NaN')
options(warn = 0)	

########## test



invisible(gc(reset = TRUE))


if(!plyr:::empty(X)){
	TEST <- X %>% 	filter_(paste0(MY_CALCULATION$USE, " %in% c('", paste0(unlist(str_split(as.character(MY_CALCULATION$NOTES), ";")), collapse ="', '"), "')")) %>% 
					filter(as.numeric(obs_value) < 100) %>% 
					mutate(test = 'ERROR')
 
	ref <- X %>% 	left_join(TEST, by = c("ref_area", "indicator", "source", "sex_version", "classif1_version", "classif2_version", "time", "sex", "classif1", "classif2", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source")) %>% 
					filter(test %in% 'ERROR' ) 

					
					
	if(nrow(ref) > 0){
	
		survey <- c('453', '439', '147', '536','2257','2258','2253','2259','2249','2242','2487','2244','2518','2486','2260','2247','2240','2251','2237','772','2255','2238','2245','2261','2246','2239','2248','2236','2250','2241','2252','2235','2254','2243','2519','2256')	
	
		TEST <- ref  %>% filter(!source %in% survey)
		
		rm(survey)
		
		if(nrow(TEST) > 0){
				data.table:::fwrite(TEST,file = paste0(mywd, 'ILO_Data/check/DISTRIBUTION_FULL_',unique(ref$ref_area),'.csv'), na = '')
		}
		
		invisible(gc(reset = TRUE))
	
		ref <- ref %>% distinct(indicator, source, sex, classif1, classif2, time) %>% mutate(test = 'ERROR')
	
		X <- X %>% 	
				left_join(ref, by = c("indicator", "source", "sex", "classif1", "classif2", "time")) %>% 
				mutate(	obs_value = ifelse( test %in% 'ERROR', NA, obs_value) , 
						obs_status = ifelse( test %in% 'ERROR', 'U', obs_status)) %>% select(-test)
		
	}
	
	rm(TEST, ref)

	}
	rm(X)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))

	
	
}

#' @export


checkDataFrequencyST <- function(X){
key 		<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")

X	 <- X %>% 
		mutate(VAR = Freq_Code) %>%
		unite_("ID" , c(key,"Time","Freq_Code"), sep="/", remove = FALSE) %>%
		unite_("KEY" ,c(key,"Time"), sep="/", remove = FALSE )

												
X_MONTHLY <- X %>%
			filter(	VAR%in%c("M","X")) 
			
X <- X %>% 
			filter(	!VAR%in%c("M","X"),
					!KEY%in%X_MONTHLY$KEY)
					
X <- X_MONTHLY %>% bind_rows(X)
			
X_QUARTERLY <- X %>%
			filter(	VAR%in%c("M","X","Q")) 
			
X <- X %>% 
			filter(	!VAR%in%c("M","X","Q"),
					!KEY%in%X_QUARTERLY$KEY)
invisible(gc(reset = TRUE))					
X_QUARTERLY %>% bind_rows(X) %>% 
		select(-KEY,-VAR,-ID)
}

#' @export

checkDataRateST <- function(X, mywd){	


key_REF <- c("Country_Code", "Indicator_Code", "Source_Code", "Sex_Code", "Classif1_Code", "Classif2_Code",  "Time")

X 		<- X %>% 
			unite_("ID", key_REF, sep="/", remove = FALSE )
RATE 	<- X %>% 	
			mutate(Indicator_Code = as.character(Indicator_Code)) %>%
			filter(	str_sub(Indicator_Code,-3,-1)%in%"_RT",
					!str_sub(Indicator_Code,1,3)%in%c("EAR","HOW","CPI", 'INJ')) 
invisible(gc(reset = TRUE))
						
if(!plyr:::empty(RATE)){
	TEST <- RATE %>%
				filter(	as.numeric(Value) > 100.0000001 | as.numeric(Value) < 0)
	rm(RATE)
	

	if(!plyr:::empty(TEST)){
		ref <- TEST %>% 
				select(-ID)
		survey <- c('453', '469', '536','2257','2258','2253','2259','2249','2242','2487','2244','2518','2486','2260','2247','2240','2251','2237','772','2255','2238','2245','2261','2246','2239','2248','2236','2250','2241','2252','2235','2254','2243','2519','2256')	
		ref <- ref %>% 
		filter(!as.character(Source_Code) %in% survey) 
		if(!plyr:::empty(ref)){
			data.table:::fwrite(ref,file = paste0(mywd, 'ILO_Data/check/RATIO_',unique(ref$Country_Code),'.csv'), na = '')
		}
		rm(ref)
	}

invisible(gc(reset = TRUE))
	X <- X %>% 
			mutate(Value =  ifelse(ID %in% TEST$ID, NA,Value)) %>% 
			mutate(Value_Status_Code =  ifelse(ID %in% TEST$ID, "U",as.character(Value_Status_Code)))
	rm(TEST)
invisible(gc(reset = TRUE))
}

X %>% select(-ID)
}

#' @export

checkDataSourceST <- function(X){

key 	<- c("Country_Code", "Source_Code", "Indicator_Code","Sex_Code","Classif1_Code","Classif2_Code")

SKIP	<- X %>% 	
			filter(!substr(as.character(Source_Code),1,2)%in%"BA")

X <- X %>% 	unite_("KEY",c(key,"Time"), sep="/", remove = FALSE) %>% 
			filter(substr(as.character(Source_Code),1,2)%in%"BA")
invisible(gc(reset = TRUE))			

if(!plyr:::empty(X)){
	X_EUROSTAT	<- X %>%
					filter(Add_Repository%in%"EUROSTAT")
			
			
	X 			<- X %>%
					filter(!Add_Repository%in%"EUROSTAT")

	X_EUROSTAT 	<- X_EUROSTAT %>%
					filter(	!KEY%in%X$KEY,
							!(Value %in% NA & Value_Status_Code %in% "f"))

	X 			<- X_EUROSTAT %>% bind_rows(X)
invisible(gc(reset = TRUE))
}

X %>% 	bind_rows(SKIP) %>% 
		select(-KEY) %>%
		filter(!Value%in%NA | (!Value_Status_Code%in%c(NA,"",NaN) & substr(as.character(Time),5,5)%in%"M"))
}
