#' Prepare dataframe in csv download format
#'
#' fix utf-8 encoding bug
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param X data frame.
#' @param Lang default "en" English, could be "fr", and "sp".
#' @param LABEL add label to the notes, default is FALSE.
#' @author ILO bescond
#' @keywords ILO, SDMX, R
#' @export

PrepareDownload <- function(X, Lang = "en"){

####### exemple 
Lang <- toupper(Lang)


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
"Classif3_Version_Item_Code",
"Classif3_Version_Label",
"Classif3_Item_Label",
"Classif4_Version_Item_Code",
"Classif4_Version_Label",
"Classif4_Item_Label",
"Classif5_Version_Item_Code",
"Classif5_Version_Label",
"Classif5_Item_Label",
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

CODE_ORA <- Ariane:::CODE_ORA
attach(CODE_ORA,warn.conflicts = FALSE)

FLAG <- CODE_ORA$T_CLT_CODELIST %>% filter(CLT_COLUMN_NAME%in%"VALUE_STATUS")

X <- X %>%
		mutate(	
				Survey_Code = as.character(Survey_Id),
				Notes_Source_Label = mapvalues(Notes_Source_Code,			from = levels(as.factor(Notes_Source_Code)), 
																			to = My_Label_notesJ(levels(as.factor(Notes_Source_Code)),SEP = "_", Lang = Lang)),
				Notes_Indicator_Label = mapvalues(Notes_Indicator_Code, 	from = levels(as.factor(Notes_Indicator_Code)), 
																			to = My_Label_notesJ(levels(as.factor(Notes_Indicator_Code)),SEP = "_", Lang = Lang)),
				Notes_Classif_Label = mapvalues(Notes_Classif_Code,			from = levels(as.factor(Notes_Classif_Code)), 
																			to = My_Label_notesJ(levels(as.factor(Notes_Classif_Code)),SEP = "_", Lang = Lang)),
				Classif1_Version_Code = as.character(Classif1_Version_Code),
				Classif2_Version_Code = as.character(Classif2_Version_Code),
				Classif3_Version_Code = as.character(Classif3_Version_Code),
				Classif4_Version_Code = as.character(Classif4_Version_Code),
				Classif5_Version_Code = as.character(Classif5_Version_Code),
				Sex_Item_Code = as.character(Sex_Code),
				Classif1_Version_Item_Code = as.character(Classif1_Code),
				Classif2_Version_Item_Code = as.character(Classif2_Code),
				Classif3_Version_Item_Code = as.character(Classif3_Code),
				Classif4_Version_Item_Code = as.character(Classif4_Code),
				Classif5_Version_Item_Code = as.character(Classif5_Code),
				Obs_Value = Value,
				Topic_code = substr(Indicator_Code,1,3),
				Sex_Code = ifelse(!Sex_Item_Code%in%NA , substr(Sex_Item_Code,1,3), as.character(NA)) ,
				Classif1_Code = ifelse(!Classif1_Version_Code%in%NA,substr(Classif1_Version_Code,1,3), as.character(NA)),
				Classif2_Code = ifelse(!Classif2_Version_Code%in%NA,substr(Classif2_Version_Code,1,3), as.character(NA)),
				Classif3_Code = ifelse(!Classif3_Version_Code%in%NA,substr(Classif3_Version_Code,1,3), as.character(NA)),
				Classif4_Code = ifelse(!Classif4_Version_Code%in%NA,substr(Classif4_Version_Code,1,3), as.character(NA)),
				Classif5_Code = ifelse(!Classif5_Version_Code%in%NA,substr(Classif5_Version_Code,1,3), as.character(NA)),
				Time = as.character(Time),
				Flag_Code = as.character(Value_Status_Code))  %>%
		select(-Value_Status_Code,-Freq_Code,-Value,-Survey_Id) %>%				
		left_join(select(T_COU_COUNTRY, Country_Code = COU_ISO3_CODE, Country_Label = eval(parse(text=paste0("COU_TEXT_",Lang))),COUNTRY_SORT = eval(parse(text=paste0("COU_SORT_",Lang)))),by ="Country_Code") %>%
		left_join(select(T_COL_COLLECTION, Collection_Code = COL_CODE, Collection_Label = eval(parse(text=paste0("COL_TEXT_",Lang)))),by ="Collection_Code") %>%
		left_join(select(T_TOP_TOPIC, Topic_code = TOP_CODE,TOPIC_SORT = TOP_SORT ),by ="Topic_code") %>%
		left_join(select(T_IND_INDICATOR, Indicator_Code = IND_CODE, Indicator_Label = eval(parse(text=paste0("IND_TEXT_",Lang))),INDICATOR_SORT = IND_SORT, Unit_Measure_Id= IND_UNIT_MEASURE_ID, Unit_Multiplier_Id = IND_UNIT_MULT_ID ),by ="Indicator_Code") %>%
		left_join(select(T_CLT_CODELIST, Unit_Measure_Id = CLT_ID, Unit_Measure_Code = CLT_COLUMN_CODE ,Unit_Measure_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Unit_Measure_Id") %>% select(-Unit_Measure_Id) %>%
		left_join(select(T_CLT_CODELIST, Unit_Multiplier_Id = CLT_ID, Unit_Multiplier_Code = CLT_COLUMN_CODE ,Unit_Multiplier_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Unit_Multiplier_Id") %>% select(-Unit_Multiplier_Id) %>%
		left_join(select(T_SRC_SOURCE, Source_Code = SRC_CODE, Source_Label = eval(parse(text=paste0("SRC_TEXT_",Lang))), SOURCE_SORT = SRC_SORT),by ="Source_Code") %>%
		left_join(select(T_SUR_SURVEY, Survey_Code = SUR_ID, Survey_Label = eval(parse(text=paste0("SUR_SURVEY_TITLE_",Lang))),SURVEY_SORT = SUR_SORT),by ="Survey_Code") %>%
		
		left_join(select(T_CLY_CLASSIF_TYPE, Sex_Code = CLY_CODE, SEX_TYPE_SORT = CLY_SORT),by ="Sex_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif1_Code = CLY_CODE, CLASSIF1_TYPE_SORT = CLY_SORT),by ="Classif1_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif2_Code = CLY_CODE, CLASSIF2_TYPE_SORT = CLY_SORT),by ="Classif2_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif3_Code = CLY_CODE, CLASSIF3_TYPE_SORT = CLY_SORT),by ="Classif3_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif4_Code = CLY_CODE, CLASSIF4_TYPE_SORT = CLY_SORT),by ="Classif4_Code") %>%
		left_join(select(T_CLY_CLASSIF_TYPE, Classif5_Code = CLY_CODE, CLASSIF5_TYPE_SORT = CLY_SORT),by ="Classif5_Code") %>%
		mutate(Time = as.character(Time)) %>%
		left_join(select(T_TIM_TIME, Time = TIM_FORMAT_USER, TIME_ID = TIM_ID),by ="Time") %>%
				
		left_join(select(T_CLV_CLASSIF_VERSION, Classif1_Version_Code = CLV_CODE, Classif1_Version_Label = eval(parse(text=paste0("CLV_TEXT_",Lang))),CLASSIF1_VERSION_SORT = CLV_SORT),by ="Classif1_Version_Code") %>%
		left_join(select(T_CLV_CLASSIF_VERSION, Classif2_Version_Code = CLV_CODE, Classif2_Version_Label = eval(parse(text=paste0("CLV_TEXT_",Lang))),CLASSIF2_VERSION_SORT = CLV_SORT),by ="Classif2_Version_Code") %>%
		left_join(select(T_CLV_CLASSIF_VERSION, Classif3_Version_Code = CLV_CODE, Classif3_Version_Label = eval(parse(text=paste0("CLV_TEXT_",Lang))),CLASSIF3_VERSION_SORT = CLV_SORT),by ="Classif3_Version_Code") %>%
		left_join(select(T_CLV_CLASSIF_VERSION, Classif4_Version_Code = CLV_CODE, Classif4_Version_Label = eval(parse(text=paste0("CLV_TEXT_",Lang))),CLASSIF4_VERSION_SORT = CLV_SORT),by ="Classif4_Version_Code") %>%
		left_join(select(T_CLV_CLASSIF_VERSION, Classif5_Version_Code = CLV_CODE, Classif5_Version_Label = eval(parse(text=paste0("CLV_TEXT_",Lang))),CLASSIF5_VERSION_SORT = CLV_SORT),by ="Classif5_Version_Code") %>%
		
		
		left_join(select(T_CLA_CLASSIF, Sex_Item_Code = CLA_CODE, Sex_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), SEX_SORT = CLA_SORT),by ="Sex_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif1_Version_Item_Code = CLA_CODE, Classif1_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), CLASSIF1_SORT = CLA_SORT),by ="Classif1_Version_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif2_Version_Item_Code = CLA_CODE, Classif2_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), CLASSIF2_SORT = CLA_SORT),by ="Classif2_Version_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif3_Version_Item_Code = CLA_CODE, Classif3_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), CLASSIF3_SORT = CLA_SORT),by ="Classif3_Version_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif4_Version_Item_Code = CLA_CODE, Classif4_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), CLASSIF4_SORT = CLA_SORT),by ="Classif4_Version_Item_Code") %>%
		left_join(select(T_CLA_CLASSIF, Classif5_Version_Item_Code = CLA_CODE, Classif5_Item_Label = eval(parse(text=paste0("CLA_TEXT_",Lang))), CLASSIF5_SORT = CLA_SORT),by ="Classif5_Version_Item_Code") %>%
		left_join(select(FLAG, Flag_Code = CLT_COLUMN_CODE, Flag_Label = eval(parse(text=paste0("CLT_TEXT_",Lang)))),by ="Flag_Code") %>%
		select(-Sex_Version_Code,-Classif1_Version_Code,-Classif2_Version_Code,-Classif3_Version_Code,-Classif4_Version_Code,-Classif5_Version_Code) %>%
		mutate(	Lang = tolower(Lang)) %>%
		select(-Topic_code,-COUNTRY_SORT,-SOURCE_SORT,-SURVEY_SORT,-TOPIC_SORT,-INDICATOR_SORT,-SEX_TYPE_SORT, -CLASSIF1_TYPE_SORT, -CLASSIF2_TYPE_SORT, -CLASSIF3_TYPE_SORT, -CLASSIF4_TYPE_SORT, -CLASSIF5_TYPE_SORT, -CLASSIF1_VERSION_SORT, -CLASSIF2_VERSION_SORT, -CLASSIF3_VERSION_SORT, -CLASSIF4_VERSION_SORT, -CLASSIF5_VERSION_SORT, -TIME_ID, -SEX_SORT, -CLASSIF1_SORT, -CLASSIF2_SORT, -CLASSIF3_SORT, -CLASSIF4_SORT, -CLASSIF5_SORT) %>%
		select_(.dots  = COL_X)
		
}
