#' Prepare dataframe in csv upload format
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

PrepareUpload <- function(X, DELETE = FALSE, LABEL = FALSE){

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
					Value_Notes_String 	= mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Label_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_", Lang = "EN")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
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
					Value_Notes_String 	= mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Transform_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
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
			select_(.dots  = c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					

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
			select_(.dots  = c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					
}
return(X)
}



PrepareUpload_OLD <- function(X, DELETE = FALSE, LABEL = FALSE){

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
					Value_Notes_String 	= mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Label_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_", Lang = "EN")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
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
					Value_Notes_String 	= mapvalues(Notes_Classif_Code,		from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Resort_NotesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
																			to = My_Resort_NotesJ(levels(as.factor(Qtable_Notes_String)),SEP = "_")),
					Value_Notes_String 	= mapvalues(Value_Notes_String,		from = levels(as.factor(Value_Notes_String)), 
																			to = My_Transform_notesJ(levels(as.factor(Value_Notes_String)),SEP = "_")),
					Qtable_Notes_String = mapvalues(Qtable_Notes_String,	from = levels(as.factor(Qtable_Notes_String)), 
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
			select_(.dots  = c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					

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
			select_(.dots  = c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes"))

					
}
return(X)
}
