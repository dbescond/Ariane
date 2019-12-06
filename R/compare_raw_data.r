#' Compare STI ORA
#'
#' create NEW, REV, DEL files at ref_area, freq level
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param ref_collection select your collection.
#' @param ref_area reduce to selected ref_area.
#' @author ILO bescond
#' @keywords ILO, SDMX, R

#' @examples
#'
#'	# to do
#'
#' @export	

compare_raw_data <- function(ref_collection = 'STI', ref_ref_area = NULL, ref_freq_code  = c('Q','M')){
######## compare mainly Quarterly and monthly data

	# clean up directory
	
	ref_file_STI_by_ref_area <- list.files(paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/')) %>% enframe(name = NULL)
	
	
	for(i in 1:nrow(ref_file_STI_by_ref_area)){
		unlink(paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/', ref_file_STI_by_ref_area$value[i]))
	}
	rm(ref_file_STI_by_ref_area)


	
	

	# check files folder

	ref_file_ORA <- list.files(paste0(ilo:::path$sys, 'ILO_Data/ORA/')) %>% enframe(name = NULL) %>% 
						separate(value, c('collection','ref_area','freq_code'), remove = FALSE, sep = '_' ,  extra = 'drop') %>% 
						mutate(freq_code = str_sub(freq_code,1,1)) %>% filter(freq_code %in% ref_freq_code, collection %in% ref_collection)
						
	ref_file_STI <- list.files(paste0(ilo:::path$sys, 'ILO_Data/STI/')) %>%  enframe(name = NULL) %>% 
						separate(value, c('collection','ref_area','freq_code'), remove = FALSE,  sep = '_'  , extra = 'drop') %>% 
						mutate(freq_code = str_sub(freq_code,1,1)) %>% filter(freq_code %in% ref_freq_code, collection %in% ref_collection)

	if(!is.null(ref_ref_area)){
		ref_file_ORA <- ref_file_ORA %>% filter(ref_area %in% ref_ref_area)
		ref_file_STI <- ref_file_STI %>% filter(ref_area %in% ref_ref_area)
	
	}
	
	
	############################################################
	############################################################
	############################################################
	##########
	########## step 1 test if ORA only so then DEL ref_collection data
	##########
	####################
	
	test <- ref_file_ORA %>% 
				left_join(mutate(ref_file_STI, check = 1),by = c("value", "collection", "ref_area", "freq_code")) %>% 
				filter(check %in% NA)


	if(nrow(test)>0){
		
		for (i in 1:nrow(test)){
		
			X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i]))
			
			for (j in 1:length(ref_collection)){
				X1 <- X %>% filter(collection %in% ref_collection[j])
				if(nrow(X1) > 0){			
					X1 %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)
			}
			rm(X)
			# remove reference file
			ref_file_ORA <- ref_file_ORA %>% filter(!value %in%test$value[i])
			
			
		}
		
	}
	rm(test)
	invisible(gc(reset = TRUE))	
	invisible(gc(reset = TRUE))	

	
	############################################################
	############################################################
	############################################################
	##########
	########## step 2 test if ref_collection only so then NEW ref_collection data
	##########
	####################
	
	test <- ref_file_STI %>% 
				left_join(mutate(ref_file_ORA, check = 1), by = c("value", "collection", "ref_area", "freq_code")) %>% 
				filter(check %in% NA)
	
	if(nrow(test)>0){
		
		for (i in 1:nrow(test)){
		
			X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/STI/', test$value[i])) %>% select(!!c("collection", "ref_area", "source", "indicator", "sex", "sex_version", "classif1", "classif1_version", "classif2", "classif2_version", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source"))
			
			for (j in 1:length(ref_collection)){
				X1 <- X %>% filter(collection %in% ref_collection[j])
				if(nrow(X1) > 0){			
					X1 %>%
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV1_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)
			}
			rm(X)
			# remove reference file
			ref_file_STI <- ref_file_STI %>% filter(!value %in%test$value[i])
			
			
		}

	}	
	rm(test)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	
	
	############################################################
	############################################################
	############################################################
	##########
	########## step 3 test compare file to file
	##########
	####################	
	
	test <- ref_file_STI
	
	for(i in 1:nrow(test)){
	
		print(test$value[i])
		
		ORA <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i])) %>% mutate_all(as.character)
		STI <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/STI/', test$value[i])) %>%  mutate_all(as.character) %>%  
					select(!!c("collection", "ref_area", "source", "indicator", "sex", "sex_version", "classif1", "classif1_version", "classif2", "classif2_version", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source", "freq_code")) %>% 
					mutate(obs_value = ifelse(obs_status %in% 'U' & obs_value %in% '0', NA, obs_value))
		
		ON_STI <- dplyr:::setdiff(STI, select(ORA, -info)) %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% mutate(diff_sti = 1)
		ON_ORA <- dplyr:::setdiff(select(ORA, -info), STI) %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% mutate(diff_ora = 1)
		
		invisible(gc(reset = TRUE))

		
		# tag STI NEW (check = 0) and STI REV (check = 1)
		
		TEST_STI <-  STI %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
						left_join(ON_STI, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
						left_join(ON_ORA, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
						left_join(ORA %>% 	distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
											mutate(ora = 1), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
						mutate(	check = ifelse(diff_sti %in% 1 & diff_ora %in% 1, 1, NA), 
								check = ifelse(diff_sti %in% 1 & !diff_ora %in% 1, 0, check), 
								check = ifelse(check %in% 0 & ora %in% 1, 1, check)) %>% 
						select(-diff_sti, -diff_ora, -ora) %>% filter(!check %in% NA) 
		ON_STI <- TEST_STI 
		rm(TEST_STI)
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
		
		
		# reduce ORA to the deletion only (data not revised by STI)
		
		ON_ORA <- ON_ORA %>% select(-diff_ora) %>%  
						left_join(STI %>% 
									distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
									mutate(check = 1), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
						filter(!check %in% 1)


		
		if(nrow(ON_STI) == 0 & nrow(ON_ORA) > 0){
		
			for (j in 1:length(ref_collection)){
				X1 <- ON_ORA %>% filter(collection %in% ref_collection[j]) %>% select(-check)
				if(nrow(X1) > 0){			
					X1 %>% mutate_all(as.character) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)
			}
			ON_ORA <- ON_ORA %>% slice(0)
			ORA <- ORA %>% slice(0)
			STI <- STI %>% slice(0)
			rm(j)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		}
		
		if(nrow(ON_STI) > 0 & nrow(ON_ORA)  == 0){
		
			for (j in 1:length(ref_collection)){
				X1 <- STI %>% left_join(ON_STI, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
							filter(check %in% c(1,0), collection %in% ref_collection[j]) %>% 
							select( -freq_code)
				X1_rev <- X1 %>% filter(check %in% 1) %>% select(-check)		
							
				if(nrow(X1_rev) > 0){			
					X1_rev %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1_rev)
				X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV1_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)
			}
			ON_STI <- ON_STI %>% slice(0)
			ORA <- ORA %>% slice(0)
			STI <- STI %>% slice(0)
			rm(j)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		}
	
		if(nrow(ON_STI) > 0 & nrow(ON_ORA)  > 0){
		
			##### delete on  ORACLE
			##### delete on  ORACLE
			for (j in 1:length(ref_collection)){
				X1 <- ON_ORA %>% filter(collection %in% ref_collection[j]) %>% select(-check)
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)
			}
			ON_ORA <- ON_ORA %>% slice(0)
			ORA <- ORA %>% slice(0)
			rm(j)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		
			##### REV and NEW  from STI
			for (j in 1:length(ref_collection)){
				X1 <- STI %>% left_join(ON_STI, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
							filter(check %in% c(1,0), collection %in% ref_collection[j]) %>% 
							select( -freq_code)
				X1_rev <- X1 %>% filter(check %in% 1) %>% select(-check)		
							
				if(nrow(X1_rev) > 0){			
					X1_rev %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1_rev)
				X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV1_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)
			}
			ON_STI <- ON_STI %>% slice(0)
			STI <- STI %>% slice(0)
			rm(j)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
			
		}
	
	
	}

	rm(STI, ORA, ON_ORA, ON_STI, test, ref_file_ORA, ref_file_STI)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))

				
	return('comparison done !!!')

	
	
	
	
	
}

#' @export	
merge_raw_data <- function(ref_collection = 'STI'){
	############################################################
	############################################################
	############################################################
	##########
	########## step 4 combine by collection
	##########
	####################	
	
	
	# clean up directory
	
	ref_file_ZZZ_ilostat <- list.files(paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/')) %>%  enframe(name = NULL)  %>% filter(str_detect(value, ref_collection))
	for(i in 1:nrow(ref_file_ZZZ_ilostat)){
		unlink(paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/', ref_file_ZZZ_ilostat$value[i]))
	}	
	rm(ref_file_ZZZ_ilostat)
	
	
	ref_file_STI_by_ref_area <- list.files(paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/')) %>%  enframe(name = NULL)  %>%  
		separate(value, c('type', 'collection', 'ref_area', 'freq'), sep = '_', remove = FALSE) %>% mutate(freq = str_sub(freq, 1,1))
	
	
	if(nrow(ref_file_STI_by_ref_area) == 0) return('OK, no file to combine !!!')
	
	ref_col <- unique(ref_file_STI_by_ref_area$collection)
	
	
	for (i in 1:length(ref_col)){
	
		test <- ref_file_STI_by_ref_area %>% filter(collection %in% ref_col[i])
	
		ref_type <- unique(str_sub(test$type,1,3))
		
		for (j in 1:length(ref_type)){
		
			ref_file <- test %>% filter(str_sub(type,1,3) %in% ref_type[j])
			
			X <- NULL
			count <- 1
			for(k in 1:nrow(ref_file)){
				
				if(ref_type[j] %in% 'DEL'){
			
					X <- bind_rows(X, 
							read_csv(
									paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/',ref_file$value[k]), 
									col_types = cols(
												collection = col_character(),
												ref_area = col_character(),
												source = col_character(),
												indicator = col_character(),
												sex_version = col_character(),
												classif1_version = col_character(),
												classif2_version = col_character(),
												time = col_character()
											)))
				} else {
					print(ref_file$value[k])
					X <- bind_rows(X, 
							read_csv(
									paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/',ref_file$value[k]), 
									col_types = cols_only(
												collection = col_character(),
												ref_area = col_character(),
												source = col_character(),
  												indicator = col_character(),
  												sex = col_character(),
  												classif1 = col_character(),
  												classif2 = col_character(),
  												time = col_character(),
  												obs_value = col_double(),
  												obs_status = col_character(),
  												note_classif = col_character(),
  												note_indicator = col_character(),
 												note_source = col_character()
											)))
				}
				
				if(nrow(X) > 250000){
					X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/',str_sub(ref_type[j],1,3),'_',ref_col[i], '_', count,'_', Sys.Date(), '.csv'), na = '')
					X <- NULL
					count <- count + 1
				}
				
		
			}
			
			if(!is.null(X)){
			if(nrow(X) > 0){
					X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/',str_sub(ref_type[j],1,3),'_',ref_col[i], '_', count,'_', Sys.Date(), '.csv'), na = '')
					X <- NULL
					count <- count + 1
				}}
			
			rm(ref_file, X)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
	
		}
		rm(ref_type, test)
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
	
		
	
	}
	rm(ref_col)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	
	
	return('files ready to be upload !!!')
	
}


#' @export	
compare_raw_data_ANNUAL <- function(ref_collection = 'YI', ref_ref_area = NULL, ref_freq_code  = 'A'){


	# clean up directory
	
	ref_file_STI_by_ref_area <- list.files(paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/')) %>% enframe(name = NULL)
	
	for(i in 1:nrow(ref_file_STI_by_ref_area)){
		unlink(paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/', ref_file_STI_by_ref_area$value[i]))
	}
	rm(ref_file_STI_by_ref_area)


	# check files folder

	ref_file_ORA <- list.files(paste0(ilo:::path$sys, 'ILO_Data/ORA/')) %>%  enframe(name = NULL)  %>% 
					separate(value, c('collection','ref_area','freq_code'), remove = FALSE, sep = '_' , extra = 'drop') %>% 
					mutate(freq_code = str_sub(freq_code,1,1)) %>% filter(freq_code %in% ref_freq_code, collection %in% ref_collection)
	ref_file_STI <- list.files(paste0(ilo:::path$sys, 'ILO_Data/STI/')) %>%  enframe(name = NULL)  %>% 
					separate(value, c('collection','ref_area','freq_code'), remove = FALSE,  sep = '_', extra = 'drop' ) %>% 
				mutate(freq_code = str_sub(freq_code,1,1)) %>% filter(freq_code %in% ref_freq_code, collection %in% ref_collection)

	if(!is.null(ref_ref_area)){
		ref_file_ORA <- ref_file_ORA %>% filter(ref_area %in% ref_ref_area)
		ref_file_STI <- ref_file_STI %>% filter(ref_area %in% ref_ref_area)
	
	}
	
	if(!ref_collection %in% "ILOEST"){
	
		ref_file_ORA <- ref_file_ORA %>% filter(!str_sub(ref_area,1,1) %in% 'X')
	
	}
	
	
	############################################################
	############################################################
	############################################################
	##########
	########## step 1 test if on ORA but not on STI, delete data only if coming fro STI (tag R1:3260)
	##########
	########## ## validate
	##########
	####################
	
	test <- ref_file_ORA %>% 
				left_join(mutate(ref_file_STI, check = 1),by = c("value", "collection", "ref_area", "freq_code")) %>% 
				filter(check %in% NA)

	if(nrow(test)>0){
		
		for (i in 1:nrow(test)){
		
			X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i])) %>% filter(collection %in% ref_collection) %>% mutate_all(as.character)
			
			
			if(nrow(X)>0){ 
				for (j in 1:length(ref_collection)){
					X1 <- X %>% filter(collection %in% ref_collection[j])
				
					if(test$freq_code[i] %in% 'A'){
						X1 <- X1 %>% filter(str_detect(note_source, 'R1:3260'))    ####### if annual (YI) delete only from STI 
					}
				
				
					if(nrow(X1) > 0){			
						X1 %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
							data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
					}
					rm(X1)
				}
			rm(X)
			
			}
			# remove reference file
			ref_file_ORA <- ref_file_ORA %>% filter(!value %in%test$value[i])
			
			
		}
		
	}
	rm(test)
	invisible(gc(reset = TRUE))	
	invisible(gc(reset = TRUE))	

	
	############################################################
	############################################################
	############################################################
	##########
	########## step 2 test if produce by STI only so new ref_area / freq
	##########
	####################
	
	test <- ref_file_STI %>% 
				left_join(mutate(ref_file_ORA, check = 1),by = c("value", "collection", "ref_area", "freq_code")) %>% 
				filter(check %in% NA) 
				
	if(nrow(test)>0){
		
		for (i in 1:nrow(test)){
		
			X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/STI/', test$value[i])) %>% select(!!c("collection", "ref_area", "source", "indicator", "sex", "sex_version", "classif1", "classif1_version", "classif2", "classif2_version", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source")) %>% mutate_all(as.character)
			
				
			if(test$freq_code[i] %in% 'A'){
				

				ref_freq <- "([I][1][2])[-:.]([0-9]+)" 

				X <- 	X %>% mutate(Freq = str_extract(note_indicator, ref_freq)) 
				to_change <- unique(X$Freq)
				to_change <- to_change[!to_change %in% NA]
				X <- 	X %>% select(-Freq)
				for (j in 1:length(to_change)){
			
					X <- X %>% 	mutate(note_indicator = str_replace(note_indicator, fixed(paste0('_', to_change[j])), '')) %>% 
									mutate(note_indicator = str_replace(note_indicator, fixed(paste0(to_change[j], '_')), '')) %>% 
									mutate(note_indicator = str_replace(note_indicator, fixed(to_change[j]), ''))%>% 
									mutate(note_indicator = ifelse(note_indicator %in% '', NA, note_indicator))
				}
				X <- X %>% mutate(note_source = ifelse(note_source %in% NA, 'R1:3260', paste0('R1:3260_', note_source))) 
				
			}
				
			if(nrow(X) > 0){			
				X %>%
					data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV1_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
			}
			
			rm(X)
			# remove reference file
			ref_file_STI <- ref_file_STI %>% filter(!value %in%test$value[i])
			
			
		}

	}	
	rm(test)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	
	
	############################################################
	############################################################
	############################################################
	##########
	########## step 3 test compare file to file
	##########
	####################	
	
	test <- ref_file_STI 
	
	for(i in 1:nrow(test)){
	

			compare_annual(test, i, ref_collection)

	}

	# rm(STI, ORA, ON_ORA, ON_STI, test, ref_file_ORA, ref_file_STI)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))

				
	return('comparison done !!!')

	
	
	
	
	
}

#' @export	
merge_raw_data_ANNUAL <- function(ref_collection = 'YI'){
	############################################################
	############################################################
	############################################################
	##########
	########## step 4 combine by collection
	##########
	####################	
	
	
	# clean up directory
	
	ref_file_ZZZ_ilostat <- list.files(paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/')) %>%  enframe(name = NULL)  %>% filter(str_detect(value, ref_collection))
	for(i in 1:nrow(ref_file_ZZZ_ilostat)){
		unlink(paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/', ref_file_ZZZ_ilostat$value[i]))
	}	
	rm(ref_file_ZZZ_ilostat)
	
	

	
	
	
	ref_file_STI_by_ref_area <- list.files(paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/')) %>%  enframe(name = NULL)  %>%  
		separate(value, c('type','collection', 'ref_area', 'freq'), sep = '_', remove = FALSE) %>% mutate(freq = str_sub(freq, 1,1))
	
	
	if(nrow(ref_file_STI_by_ref_area) == 0) return('OK, no file to combine !!!')
	
	ref_col <- unique(ref_file_STI_by_ref_area$collection)
	
	
	for (i in 1:length(ref_col)){
	
		test <- ref_file_STI_by_ref_area %>% filter(collection %in% ref_col[i])
	
		ref_type <- unique(str_sub(test$type,1,3))
		
		for (j in 1:length(ref_type)){
		
			ref_file <- test %>% filter(str_sub(type,1,3) %in% ref_type[j])

			X <- NULL
			count <- 1
			for(k in 1:nrow(ref_file)){
				
				if(ref_type[j] %in% 'DEL'){
			
					X <- bind_rows(X, 
							read_csv(
									paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/',ref_file$value[k]), 
									col_types = cols(
												collection = col_character(),
												ref_area = col_character(),
												source = col_character(),
												indicator = col_character(),
												sex_version = col_character(),
												classif1_version = col_character(),
												classif2_version = col_character(),
												time = col_character()
											)))
				} else {
					print(ref_file$value[k])
					X <- bind_rows(X, 
							read_csv(
									paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/',ref_file$value[k]), 
									col_types = cols_only(
												collection = col_character(),
												ref_area = col_character(),
												source = col_character(),
  												indicator = col_character(),
  												sex = col_character(),
  												classif1 = col_character(),
  												classif2 = col_character(),
  												time = col_character(),
  												obs_value = col_double(),
  												obs_status = col_character(),
  												note_classif = col_character(),
  												note_indicator = col_character(),
 												note_source = col_character()
											)))
				}
				
				if(nrow(X) > 250000){
					X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/',str_sub(ref_type[j],1,3),'_',ref_col[i], '_', count,'_', Sys.Date(), '.csv'), na = '')
					X <- NULL
					count <- count + 1
				}
				
		
			}
			
			if(!is.null(X)){
			if(nrow(X) > 0){
					X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/',str_sub(ref_type[j],1,3),'_',ref_col[i], '_', count,'_', Sys.Date(), '.csv'), na = '')
					X <- NULL
					count <- count + 1
				}}
			
			rm(ref_file, X)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
	
		}
		rm(ref_type, test)
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
	
		
	
	}
	rm(ref_col)
	invisible(gc(reset = TRUE))
	invisible(gc(reset = TRUE))
	
	
	return('files ready to be upload !!!')
	
}



compare_annual <- function(test, i, ref_collection){


############ step 1 try to write on YI

		print(test$value[i])
		
		ORA <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i])) %>% 
					filter(collection %in% ref_collection) %>% 
					mutate(	test_STI = str_detect(note_source, 'R1:3260'), 
							test_STI = ifelse(test_STI %in% TRUE, TRUE, FALSE), 
							test_COL = str_sub(info, 5,7), TEST = paste0(test_STI, '_', test_COL))
							
		test_frame <- ORA  %>% filter(test_STI) %>% group_by(indicator) %>% summarise(test = paste0(unique(test_COL), collapse = '')) %>% filter(!test %in% 'CAL') %>% .$indicator					
							
			ORA <- ORA	%>% 
					select(-test_STI, -test_COL)  %>% 
					mutate_all(as.character)
					
					
		STI <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/STI/', test$value[i])) %>% 
					select( !!c("collection", "ref_area", "source", "indicator", "sex", "sex_version", "classif1", "classif1_version", "classif2", "classif2_version", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source", 'freq_code')) %>% 
					mutate(test_STI = TRUE, test_COL = 'COL', TEST = paste0(test_STI, '_', test_COL)) %>% 
					select(-test_STI, -test_COL) %>% 
					mutate_all(as.character) %>% 
					mutate(obs_value = ifelse(obs_status %in% 'U' & obs_value %in% '0', NA, obs_value))
					
					

# TEST indicator in YI
TEST_IND <- Ariane:::CODE_ORA$T_CIC_COL %>% filter(CIC_COLLECTION_CODE %in% ref_collection) %>% distinct(CIC_INDICATOR_CODE) %>% .$CIC_INDICATOR_CODE


invisible(gc(reset = TRUE))

STI <- 	STI  %>% filter(indicator %in% TEST_IND)
					
# TEST indicator in YI that create problem

 TEST_IND <- Ariane:::CODE_ORA$T_CIC_COL %>% filter(CIC_COLLECTION_CODE %in% ref_collection) %>% distinct(CIC_INDICATOR_CODE) %>% 
 				filter(CIC_INDICATOR_CODE %in% unique(test_frame))%>% .$CIC_INDICATOR_CODE
	
rm(test_frame)
	
ORA <- 	ORA  %>% filter(indicator %in% TEST_IND, source %in% unique(STI$source))
		
		
		# detect and remove freq tag I12:...
		ref_freq <- "([I][1][2])[-:.]([0-9]+)"  

				STI <- 	STI %>% mutate(Freq = str_extract(note_indicator, ref_freq)) 
				to_change <- unique(STI$Freq)
				to_change <- to_change[!to_change %in% NA]
				STI <- 	STI %>% select(-Freq)
				for (j in 1:length(to_change)){
			
					
					STI <- STI %>% 	mutate(note_indicator = str_replace(note_indicator, fixed(paste0('_', to_change[j])), '')) %>% 
									mutate(note_indicator = str_replace(note_indicator, fixed(paste0(to_change[j], '_')), '')) %>% 
									mutate(note_indicator = str_replace(note_indicator, fixed(to_change[j]), ''))%>% 
									mutate(note_indicator = ifelse(note_indicator %in% '', as.character(NA), note_indicator))
					
					
				}
				
				STI <- STI %>% mutate(note_source = ifelse(note_source %in% NA, 'R1:3260', paste0('R1:3260_', as.character(note_source)))) 
		
		
		
		
		
		
		ON_STI <- dplyr:::setdiff(STI , select(ORA, -info)) %>% 
					distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
					mutate(diff_sti = 1)
		ON_ORA <- dplyr:::setdiff(select(ORA, -info), STI) %>% 
					distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
					mutate(diff_ora = 1)
		
		invisible(gc(reset = TRUE))

		
		# tag STI NEW (check = 0) and STI REV (check = 1)
		
		TEST_STI <-  STI %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
						left_join(ON_STI, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						left_join(ON_ORA, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						left_join(ORA %>% 	distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
											mutate(ora = 1), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						mutate(	check = ifelse(diff_sti %in% 1 & diff_ora %in% 1, 1, NA), 
								check = ifelse(diff_sti %in% 1 & !diff_ora %in% 1, 0, check), 
								check = ifelse(check %in% 0 & ora %in% 1, 1, check)) %>% 
						select(-diff_sti, -diff_ora, -ora) %>% filter(!check %in% NA) 
		ON_STI <- TEST_STI 
		rm(TEST_STI)
		invisible(gc(reset = TRUE))
		invisible(gc(reset = TRUE))
		
		
		# reduce ORA to the deletion only (data not revised by STI)
		
		ON_ORA <- ON_ORA %>% select(-diff_ora) %>%  
						left_join(STI %>% 
									distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
									mutate(check = 1), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						filter(!check %in% 1)

		
		
		
		############## step 0 manage col data on ORA
		
			check_ORA <- ON_ORA %>% filter(TEST %in% c('FALSE_COL', 'FALSE_CAL')) %>% mutate(keep = 1)  %>% filter(!str_detect(indicator, 'CPI'))
			
			check_STI <- STI %>% mutate(collection = 'YI') %>% left_join( 
								ORA %>% filter(TEST %in% c('FALSE_COL', 'FALSE_CAL')) %>% 
								distinct(collection, ref_area, source, indicator, time) %>% mutate(keep = 1)  , 
							by = c("collection", "ref_area", "source", "indicator", "time")) %>% 
							filter(keep %in% 1) %>% 
							select( -freq_code, -keep, -TEST) # %>% 
							
			X1 <- check_ORA
			if(nrow(check_ORA)>0){
		
					X1 <- ORA %>% left_join(check_ORA, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
							filter(keep %in% 1) %>% select(-keep) %>%
							left_join(check_ORA %>% distinct(collection, ref_area, source, indicator, time) %>% mutate(keep = 1), 
							by = c("collection", "ref_area", "source", "indicator", "time") ) %>%
							filter(keep %in% 1) %>% 
							select( -freq_code, -check, -keep)
						
				if(nrow(X1) > 0){		

				###### add compare with existing test file
				
					X1 <- X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value))  
					X1  %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/check/ON_',ref_collection,'/', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
					
					X1 <- X1 %>% filter(TEST %in% c('FALSE_COL')) %>% count(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time)

				}
				# rm(X1)
			}
			ON_ORA <- ON_ORA %>% filter(!TEST %in% c('FALSE_COL', 'FALSE_CAL')) 
			
			
			
			if(nrow(check_STI)>0){

				###### add compare with existing test file
				
				check_STI <- check_STI %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value))
				check_STI %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/check/ON_STI/',ref_collection,'_FROM_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')

				check_STI <- check_STI %>% count(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, note_source)

			}
			
			if(nrow(check_ORA) > 0 & nrow(check_STI)>0 & nrow(X1)>0){
			#
				require(ilo)
				init_ilo(-cl)
				check_STI <- check_STI %>% 
							left_join(select(ilo$code$cl_classif_version, classif1_version = code, classif1_version.sort = sort) , by = "classif1_version") %>% 
							left_join(select(ilo$code$cl_classif_version, classif2_version = code, classif2_version.sort = sort) , by = "classif2_version") %>%
							left_join(select(ilo$code$cl_indicator, indicator = code, indicator.sort = sort) , by = "indicator") %>% 
							arrange(indicator.sort, classif1_version.sort, classif2_version.sort, time) %>% 
							group_by(collection, ref_area, source, indicator, time, note_source) %>% 
							summarise(sex_version = first(sex_version), 
									  classif1_version = first(classif1_version),
									  classif2_version = first(classif2_version),
									  indicator.sort = first(indicator.sort),
									  classif1_version.sort = first(classif1_version.sort),
									  classif2_version.sort = first(classif2_version.sort),
									  n = first(n),
									  ) %>% 
							ungroup %>% 
							mutate(classif1_version.sort = ifelse(classif1_version.sort %in% NA, 1000, classif1_version.sort)) %>% 
							mutate(classif2_version.sort = ifelse(classif2_version.sort %in% NA, 1000, classif2_version.sort)) %>% 
							mutate(ss = indicator.sort + classif1_version.sort + classif2_version.sort) %>% 
							select(-contains('.sort'))

				X1 <- X1 %>% 
							left_join(select(ilo$code$cl_classif_version, classif1_version = code, classif1_version.sort = sort) , by = "classif1_version") %>% 
							left_join(select(ilo$code$cl_classif_version, classif2_version = code, classif2_version.sort = sort) , by = "classif2_version") %>%
							left_join(select(ilo$code$cl_indicator, indicator = code, indicator.sort = sort) , by = "indicator") %>% 
							arrange(indicator.sort, classif1_version.sort, classif2_version.sort, time)  %>% 
							mutate(classif1_version.sort = ifelse(classif1_version.sort %in% NA, 1000, classif1_version.sort)) %>% 
							mutate(classif2_version.sort = ifelse(classif2_version.sort %in% NA, 1000, classif2_version.sort))  %>% 
							mutate(ss = indicator.sort + classif1_version.sort + classif2_version.sort) %>% 
							select(-contains('.sort'))

							
				NEW <- X1 %>% left_join(check_STI, by = c("collection", "ref_area", "source", "indicator", "time")) %>% 
					mutate(TEST = ifelse( ss.x == ss.y & n.x > n.y, paste0("equivalent_more_points_on_", ref_collection), NA)) %>% 
					mutate(TEST = ifelse( ss.x == ss.y & n.x < n.y, paste0("DEL_",ref_collection,"_equivalent_more_points_on_STI"), TEST)) %>% 
					mutate(TEST = ifelse( ss.x == ss.y & n.x == n.y, paste0("DEL_",ref_collection,"_equivalent_same_points_on_STI"), TEST)) %>% 
					mutate(TEST = ifelse( ss.x < ss.y & n.x >= n.y, paste0("better_version_on_",ref_collection,"_more_points_on_YI"), TEST)) %>% 
					mutate(TEST = ifelse( ss.x < ss.y & n.x < n.y, paste0("DEL_",ref_collection,"_better_version_on_",ref_collection,"_more_points_on_STI"), TEST)) %>% 
					mutate(TEST = ifelse( ss.x > ss.y & n.x == n.y, paste0("DEL_",ref_collection,"_better_version_on_STI_same_points_on_STI"), TEST)) %>% 
					mutate(TEST = ifelse( ss.x > ss.y & n.x < n.y, paste0("DEL_",ref_collection,"_better_version_on_STI_more_points_on_STI"), TEST)) %>% 
					mutate(TEST = ifelse( ss.x > ss.y & n.x > n.y, paste0("better_version_on_STI_more_points_on_",ref_collection ), TEST))%>% 
					mutate(TEST = ifelse( TEST %in% c(NA, 'NA') , "Only_on_YI", TEST)) 
				
				
				ref <- NEW %>% distinct(TEST) %>% .$TEST
				
				
				for (j in 1:length(ref)){
					
					try(dir.create(path = paste0(ilo:::path$sys, 'ILO_Data/check/COMPARE/',ref[j]), showWarnings = FALSE), silent = TRUE)
				
					NEW %>% filter(TEST %in% ref[j]) %>% select(-TEST) %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/check/COMPARE/',ref[j], '/',  test$value[i] %>% str_replace('.rds', '.csv')), na = '')

				}
				rm(NEW, ref)
			
			}
			
			
			ON_STI <- ON_STI %>% left_join(check_STI %>% distinct(collection, ref_area, source, indicator, time) %>% mutate(delete = 1), 
							by = c("collection", "ref_area", "source", "indicator", "time") ) %>%
							filter(!delete %in% 1)
							
			rm(check_STI, X1, check_ORA)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		
		
############### new starting point for a normal comparison
		
		
		############# first delete if disappear from STI (ie diff STI == 0 & diff ORA > 0 )
		
		
		if( nrow(ON_STI) == 0 & nrow(ON_ORA) > 0){
		
			# data from STI COL should not stay --> delete
			
			check_STI <- ON_ORA %>% filter(TEST %in% 'TRUE_COL') 
		
			if(nrow(check_STI)>0){
		
					X1 <- ON_ORA %>% filter(collection %in% ref_collection) %>% select(-check, -TEST)
					if(nrow(X1) > 0){			
						X1 %>% mutate_all(as.character) %>%
							data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
					}
					rm(X1)
			}
			ON_ORA <- ON_ORA %>% filter(!TEST %in% c('TRUE_COL', 'TRUE_CAL'))
			rm(check_STI)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))

				
			ON_ORA <- ON_ORA %>% slice(0)
			ORA <- ORA %>% slice(0)
			STI <- STI %>% slice(0)
			
			
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		
		
		
		}
		
		
		
		
		
		############# second upload data from STI if not exist on YI (ie diff STI > 0 & diff ORA ==0 )
		
	
		if(nrow(ON_STI) > 0 & nrow(ON_ORA)  == 0){
		
				ON_STI <- ON_STI %>% select(-delete)
				X1 <- STI %>% select(-TEST) %>% left_join(ON_STI, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
							filter(check %in% c(1,0), collection %in% ref_collection) %>% 
							select( -freq_code)
				X1_rev <- X1 %>% filter(check %in% 1) %>% select(-check)		
							
				if(nrow(X1_rev) > 0){			
					X1_rev %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>%  select(-contains('_version')) %>%  select(-contains('TEST')) %>%
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1_rev)
				X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% select(-contains('_version')) %>% select(-contains('TEST')) %>%
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV1_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)

			ON_STI <- ON_STI %>% slice(0)
			ORA <- ORA %>% slice(0)
			STI <- STI %>% slice(0)
			rm(j)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		}
		
		
	
		if(nrow(ON_STI) > 0 & nrow(ON_ORA)  > 0){
			ON_STI <- ON_STI %>% select(-delete)
			##### delete on  ORACLE
				ref_STI <- STI %>% distinct(collection, ref_area, source, indicator) %>% mutate(keep = 1)
				
				
				X1 <- ON_ORA %>% filter(collection %in% ref_collection) %>% 
						select(-check) %>% 
						left_join(ref_STI, by = c("collection", "ref_area", "source", "indicator") ) %>% 
						filter(keep %in% 1) %>% select(-keep)
						
				X1 <- X1 %>% left_join(
								ORA %>% 
									distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) , 
											, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")
									
									) %>% filter(!(str_sub(indicator, 1,3) %in% c('INJ', 'LAC') & TEST %in% 'TRUE_CAL')) %>% select(-TEST) 
				
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1,ref_STI )
			
			ON_ORA <- ON_ORA %>% slice(0)
			ORA <- ORA %>% slice(0)
			rm(j)
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
		
			##### REV and NEW  from STI

				X1 <- STI %>% left_join(ON_STI %>% select(-TEST), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
							filter(check %in% c(1,0), collection %in% ref_collection) %>% 
							select( -freq_code) %>% select(-TEST)
				X1_rev <- X1 %>% filter(check %in% 1) %>% select(-check)		
							
				if(nrow(X1_rev) > 0){			
					X1_rev %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% select(-contains('TEST'))  %>% select(-contains('_version'))  %>%
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1_rev)
				X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value))   %>% select(-contains('_version'))  %>%
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/_by_ref_area_',ref_collection,'/REV1_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1)
			
			ON_STI <- ON_STI %>% slice(0)
			STI <- STI %>% slice(0)
			
			invisible(gc(reset = TRUE))
			invisible(gc(reset = TRUE))
			
		}
	
	

}



# clean_up_YI <- function(){
	
	# require(ilo)
	# init_ilo()
	# ref <- list.files(paste0(ilo:::path$sys, 'ILO_Data/check/COMPARE')) %>%  enframe(name = NULL)  %>% filter(str_sub(value, 1,4) %in% 'DEL_')

	# X <- NULL
	# for (i in 1:nrow(ref)){
	
		# ref_files <- list.files(paste0(ilo:::path$sys, 'ILO_Data/check/COMPARE/', ref$value[i], '/')) 

		# for (j in 1:length(ref_files)) {
		
			# X <- bind_rows(X, read_csv(paste0(ilo:::path$sys, 'ILO_Data/check/COMPARE/', ref$value[i], '/', ref_files[j])))
		
		# }
	
	
	# }

	# X <- X %>% distinct(collection, ref_area, source, indicator, time)

	# Y <- NULL
	
	# for (i in 1:nrow(X)){
	
		# cmd <- paste0("NEW <- get_ilo(info, ref_area = '",X$ref_area[i],"', source = '",X$source[i],"' , indicator = '", X$indicator[i], "', time = '", X$time[i],"')"	)
		# eval(parse(text = cmd))
		
		# Y <- bind_rows(Y, NEW)
		# rm(NEW)
	
	
	
	# }

	# ref_cou <- unique(Y$ref_area)
	
	# for (i in 1:length(ref_cou)){
		
		# TEST <- Y %>% filter(ref_area %in% ref_cou[i])
		
		# name <- paste0(ilo:::path$data,'REP_ILO/MICRO/input/', unique(TEST$ref_area), '_', unique(TEST$source) %>% str_replace(':',''), '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
		# if(length(name) > 1) {
 
			# name <- paste0(ilo:::path$data,'REP_ILO/MICRO/input/', unique(TEST$ref_area), '_',  '_backup_',paste0(Sys.time() %>% str_sub(., 1,10) %>% str_replace_all(., '-', '_')), '.csv')
		# }
 
		# TEST %>%  data.table::fwrite( file = name)

	
	# }
	

	 # Y %>% save_ilo(format = 'del')

	


# }

# #' @export	
# extract_for_test<- function(ref_area, source, timefrom = NULL, note_source = NULL, back = TRUE){

# require(ilo)
# init_ilo()


# X <- get_ilo(collection = 'STI', ref_area = ref_area, source = source) %>% switch_ilo(version)


# TEST_STI <- ilo$code$cl_col_ind_clv %>% filter(collection %in% 'YI') %>% 
			# left_join(ilo$code$cl_classif_version %>% select(classif_vs = code, sort), by = "classif_vs") %>% 
			# arrange(indicator, sort) %>% 
			# mutate(cla = str_sub(classif_vs,1,3)) %>% group_by(indicator, cla) %>% 
			# mutate(test = first(classif_vs)) %>% 
			# ungroup() %>% 
			# select(-collection, -classif_vs, -dissem, -sort, -cla) %>% 
			# distinct(indicator, test) %>% 
			# rename(classif_vs = test)

# close_ilo()


	# invisible(gc(reset = TRUE))	
	# invisible(gc(reset = TRUE))	


# test_version_STI <- TEST_STI %>% mutate(TEST = paste0(indicator, "/", classif_vs)) %>% select(TEST) %>% distinct(TEST) %>% t %>% c  

# test_version_STI <- c(test_version_STI, paste(TEST_STI$indicator, "NA", sep = "/"))
# test_version_STI <- gsub('/NOC', '/NOC_VALUE',test_version_STI)
# rm(TEST_STI)

# invisible(gc(reset = TRUE))

# if(!is.null(note_source)){
# test <- note_source
# X <- X %>% filter(str_detect(note_source, test))

# }
# invisible(gc(reset = TRUE))
# invisible(gc(reset = TRUE))


# X <- 	X  %>% 
				# mutate(TEST_CL1 = paste0(indicator, "/", classif1_version)) %>% 
				# filter(TEST_CL1 %in% test_version_STI) %>% 
				# mutate(TEST_CL2 = paste0(indicator, "/", classif2_version)) %>% 
				# filter(TEST_CL2 %in% test_version_STI) %>%
				# select(-TEST_CL1, -TEST_CL2) %>% 
				# filter(!classif1 %in% c('AGE_YTHADULT_Y15-64', 'ECO_SECTOR_NAG')) %>% 
				# mutate(ref_area = 'ET1', collection = 'YI', source = 'BA:1120', 
						# note_source = gsub('R1:3903_', '', note_source), 
						# note_source = gsub('_R1:3903', '', note_source), 
						# note_source = gsub('R1:3513_', '', note_source), 
						# note_source = gsub('_R1:3513', '', note_source)) %>% 
				# select(-contains('_version'))

				

	# invisible(gc(reset = TRUE))	
	# invisible(gc(reset = TRUE))	


# ref_freq <- "([I][1][2])[-:.]([0-9]+)" 

# X <- 	X %>% mutate(Freq = str_extract(note_indicator, ref_freq))

# test <- unique(X$Freq)

# test <- test[!test %in% NA]
# if(length(test) >0){

	# for (i in 1:length(test)){


		# X <- X %>% mutate(note_indicator = gsub(paste0('_',test[i]), '', note_indicator))
		# X <- X %>% mutate(note_indicator = gsub(paste0(test[i], '_'), '', note_indicator))
		# X <- X %>% mutate(note_indicator = gsub(paste0(test[i]), '', note_indicator))


	# }
# }
# X <- X %>% select(-Freq) %>% mutate(note_indicator = ifelse(note_indicator %in% '', NA, note_indicator))


# ref_freq <- "([T][3][0])[-:.]([0-9]+)"  ## delete note currency

# X <- 	X %>% mutate(Freq = str_extract(note_indicator, ref_freq))

# test <- unique(X$Freq)

# test <- test[!test %in% NA]
# if(length(test) >0){

	# for (i in 1:length(test)){


		# X <- X %>% mutate(note_indicator = gsub(paste0('_',test[i]), '', note_indicator))
		# X <- X %>% mutate(note_indicator = gsub(paste0(test[i], '_'), '', note_indicator))
		# X <- X %>% mutate(note_indicator = gsub(paste0(test[i]), '', note_indicator))


	# }
# }
# X <- X %>% select(-Freq) %>% mutate(note_indicator = ifelse(note_indicator %in% '', NA, note_indicator))



	# invisible(gc(reset = TRUE))	
	# invisible(gc(reset = TRUE))	


# if(!is.null(timefrom)){

# X <- X %>% filter(as.numeric(time) >= as.numeric(str_sub(timefrom, 1,4)))

# }


	# invisible(gc(reset = TRUE))	
	# invisible(gc(reset = TRUE))	


# if (back)return(X)
# else X %>% save_ilo(format = 'csv')





# }




# #' @export	
# merge_raw_data_YTHSTAT <- function(ref_collection = 'YTH'){
	# ############################################################
	# ############################################################
	# ############################################################
	# ##########
	# ########## step 4 combine by collection
	# ##########
	# ####################	
	
	
	# # clean up directory
	
	# ref_file_ZZZ_ilostat <- list.files(paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/')) %>%  enframe(name = NULL)  %>% filter(str_detect(value, ref_collection))
	# for(i in 1:nrow(ref_file_ZZZ_ilostat)){
		# unlink(paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/', ref_file_ZZZ_ilostat$value[i]))
	# }	
	# rm(ref_file_ZZZ_ilostat)
	
	

	
	
	
	# ref_file_STI_by_ref_area <- list.files(paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/')) %>%  enframe(name = NULL)  %>%  
		# separate(value, c('collection', 'type', 'ref_area', 'freq'), sep = '_', remove = FALSE) %>% mutate(freq = str_sub(freq, 1,1))
	
	
	# if(nrow(ref_file_STI_by_ref_area) == 0) return('OK, no file to combine !!!')
	
	# ref_col <- unique(ref_file_STI_by_ref_area$collection)
	
	
	# for (i in 1:length(ref_col)){
	
		# test <- ref_file_STI_by_ref_area %>% filter(collection %in% ref_col[i])
	
		# ref_type <- unique(test$type)
		
		# for (j in 1:length(ref_type)){
		
			# ref_file <- test %>% filter(type %in% ref_type[j])
			
			# X <- NULL
			# count <- 1
			# for(k in 1:nrow(ref_file)){
				
				# if(ref_type[j] %in% 'DEL'){
			
					# X <- bind_rows(X, 
							# read_csv(
									# paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_file$value[k]), 
									# col_types = cols(
												# collection = col_character(),
												# ref_area = col_character(),
												# source = col_character(),
												# indicator = col_character(),
												# sex_version = col_character(),
												# classif1_version = col_character(),
												# classif2_version = col_character(),
												# time = col_character()
											# )))
				# } else {
					# print(ref_file$value[k])
					# X <- bind_rows(X, 
							# read_csv(
									# paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_file$value[k]), 
									# col_types = cols_only(
												# collection = col_character(),
												# ref_area = col_character(),
												# source = col_character(),
  												# indicator = col_character(),
  												# sex = col_character(),
  												# classif1 = col_character(),
  												# classif2 = col_character(),
  												# time = col_character(),
  												# obs_value = col_double(),
  												# obs_status = col_character(),
  												# note_classif = col_character(),
  												# note_indicator = col_character(),
 												# note_source = col_character()
											# )))
				# }
				
				# if(nrow(X) > 300000){
					# X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/',ref_col[i],'_',ref_type[j], '_', count,'_', Sys.Date(), '.csv'), na = '')
					# X <- NULL
					# count <- count + 1
				# }
				
		
			# }
			
			# if(!is.null(X)){
			# if(nrow(X) > 0){
					# X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/ZZZ_ilostat/',ref_col[i],'_',ref_type[j], '_', count,'_', Sys.Date(), '.csv'), na = '')
					# X <- NULL
					# count <- count + 1
				# }}
			
			# rm(ref_file, X)
			# invisible(gc(reset = TRUE))
			# invisible(gc(reset = TRUE))
	
		# }
		# rm(ref_type, test)
		# invisible(gc(reset = TRUE))
		# invisible(gc(reset = TRUE))
	
		
	
	# }
	# rm(ref_col)
	# invisible(gc(reset = TRUE))
	# invisible(gc(reset = TRUE))
	
	
	# return('files ready to be upload !!!')
	
# }


# #' @export	
# compare_raw_data_YTHSTAT <- function(ref_collection = 'YTH', ref_area = NULL, freq_code  = 'A'){


	# # clean up directory
	
	# ref_file_STI_by_ref_area <- list.files(paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/')) %>% 
	# for(i in 1:nrow(ref_file_STI_by_ref_area)){
		# unlink(paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/', ref_file_STI_by_ref_area$value[i]))
	# }
	# rm(ref_file_STI_by_ref_area)



	# # check file folder

	# ref_file_ORA <- list.files(paste0(ilo:::path$sys, 'ILO_Data/ORA/')) %>%  enframe(name = NULL)  %>% filter(str_sub(value, 5,5) %in% freq_code) %>% filter(!str_sub(value, 1,1) %in% 'X') 
	# ref_file_STI <- list.files(paste0(ilo:::path$data, '/REP_ILO/YTHSTAT/Output/')) %>%  enframe(name = NULL) %>% filter(str_sub(value, 5,5) %in% freq_code) 

	# if(!is.null(ref_area)){
		# ref_file_ORA <- ref_file_ORA %>% filter(str_sub(value,1,3 ) %in% ref_area)
		# ref_file_STI <- ref_file_STI %>% filter(str_sub(value,1,3 ) %in% ref_area)
	
	# }
	
	
	# ############################################################
	# ############################################################
	# ############################################################
	# ##########
	# ########## step 1 test if on ORA but not on STI, delete data only if coming from STI (tag R1:3260)
	# ##########
	# ########## ## validate
	# ##########
	# ####################
	
	# test <- ref_file_ORA %>% 
				# left_join(mutate(ref_file_STI, check = 1), by = "value") %>% 
				# filter(check %in% NA) %>% mutate(freq = str_sub(value, 5,5))


	# if(nrow(test)>0){
		
		# for (i in 1:nrow(test)){
		
			# X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i])) %>% filter(collection %in% ref_collection) %>% mutate_all(as.character)
			
			
			# if(nrow(X)>0){ 
				# for (j in 1:length(ref_collection)){
					# X1 <- X %>% filter(collection %in% ref_collection[j])
				
					# if(test$freq[i] %in% 'A'){
						# X1 <- X1 %>% filter(str_detect(note_source, 'R1:3260'))    ####### if annual (YI) delete only from STI 
					# }
					
					
				
					# if(nrow(X1) > 0){			
						# X1 %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
							# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_collection[j],'_DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
					# }
					# rm(X1)
				# }
			# rm(X)
			
			# }
			# # remove reference file
			# ref_file_ORA <- ref_file_ORA %>% filter(!value %in%test$value[i])
			
			
		# }
		
	# }
	# rm(test)
	# invisible(gc(reset = TRUE))	
	# invisible(gc(reset = TRUE))	

	
	# ############################################################
	# ############################################################
	# ############################################################
	# ##########
	# ########## step 2 test if produce by STI only so new ref_area / freq
	# ##########
	# ####################
	
	# test <- ref_file_STI %>% 
				# left_join(mutate(ref_file_ORA, check = 1), by = "value") %>% 
				# filter(check %in% NA) %>% mutate(freq = str_sub(value, 5,5))
	
	
	
	# if(nrow(test)>0){
		
		# for (i in 1:nrow(test)){
		
			# X <- readRDS(paste0(ilo:::path$data, '/REP_ILO/YTHSTAT/Output/', test$value[i])) %>% select(-freq_code) %>% mutate_all(as.character)
			
				
			# if(test$freq[i] %in% 'A'){
				

				# ref_freq <- "([I][1][2])[-:.]([0-9]+)" 

				# X <- 	X %>% mutate(Freq = str_extract(note_indicator, ref_freq)) 
				# to_change <- unique(X$Freq)
				# to_change <- to_change[!to_change %in% NA]
				# X <- 	X %>% select(-Freq)
				# for (j in 1:length(to_change)){
			
					# X <- X %>% 	mutate(note_indicator = str_replace(note_indicator, fixed(paste0('_', to_change[j])), '')) %>% 
									# mutate(note_indicator = str_replace(note_indicator, fixed(paste0(to_change[j], '_')), '')) %>% 
									# mutate(note_indicator = str_replace(note_indicator, fixed(to_change[j]), ''))%>% 
									# mutate(note_indicator = ifelse(note_indicator %in% '', NA, note_indicator))
				# }
				# X <- X %>% mutate(note_source = ifelse(note_source %in% NA, 'R1:3260', paste0('R1:3260_', note_source))) 
				
			# }
				
			# if(nrow(X) > 0){			
				# X %>%
					# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',unique(X$collection),'_NEW_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
			# }
			
			# rm(X)
			# # remove reference file
			# ref_file_STI <- ref_file_STI %>% filter(!value %in%test$value[i])
			
			
		# }

	# }	
	# rm(test)
	# invisible(gc(reset = TRUE))
	# invisible(gc(reset = TRUE))
	
	
	# ############################################################
	# ############################################################
	# ############################################################
	# ##########
	# ########## step 3 test compare file to file
	# ##########
	# ####################	
	
	# test <- ref_file_STI %>% mutate(freq = str_sub(value, 5,5))
	
	
	# for(i in 1:nrow(test)){
	

			# compare_annual_YTHSTAT(test, i)

	# }

	# # rm(STI, ORA, ON_ORA, ON_STI, test, ref_file_ORA, ref_file_STI)
	# invisible(gc(reset = TRUE))
	# invisible(gc(reset = TRUE))

				
	# return('comparison done !!!')

	
	
	
	
	
# }


# compare_annual_YTHSTAT <- function(test, i){


# ############ step 1 try to write on YTH

		# ref_collection <- 'YTH'
		# print(test$value[i])
		
		# ORA <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i])) %>% 
					# filter(collection %in% ref_collection) %>% 
					# mutate(test_STI = str_detect(note_source, 'R1:3260'), test_STI = ifelse(test_STI %in% TRUE, TRUE, FALSE), test_COL = str_sub(info, 5,7), TEST = paste0(test_STI, '_', test_COL)) %>% 
					# select(-test_STI, -test_COL)  %>% 
					# mutate_all(as.character)
		# STI <- readRDS(paste0(ilo:::path$data, '/REP_ILO/YTHSTAT/Output/', test$value[i])) %>% 
					# mutate(test_STI = TRUE, test_COL = 'COL', TEST = paste0(test_STI, '_', test_COL)) %>% 
					# select(-test_STI, -test_COL) %>% 
					# mutate_all(as.character) %>% mutate(freq_code  = 'A')
					
					

# # TEST indicator in YTH
# TEST_IND <- Ariane:::CODE_ORA$T_CIC_COL %>% filter(CIC_COLLECTION_CODE %in% c('YTH')) %>% distinct(CIC_INDICATOR_CODE) %>% .$CIC_INDICATOR_CODE


# invisible(gc(reset = TRUE))

# STI <- 	STI  %>% filter(indicator %in% TEST_IND)
					
# # TEST indicator in YTH
# TEST_IND <- Ariane:::CODE_ORA$T_CIC_COL %>% filter(CIC_COLLECTION_CODE %in% c('YTH')) %>% distinct( CIC_INDICATOR_CODE) %>% filter(CIC_INDICATOR_CODE %in% unique(STI$indicator))%>% .$CIC_INDICATOR_CODE
		
# ORA <- 	ORA  %>% filter(indicator %in% TEST_IND)
		
		
		# # detect and remove freq tag I12:...
		# ref_freq <- "([I][1][2])[-:.]([0-9]+)"  

				# STI <- 	STI %>% mutate(Freq = str_extract(note_indicator, ref_freq)) 
				# to_change <- unique(STI$Freq)
				# to_change <- to_change[!to_change %in% NA]
				# STI <- 	STI %>% select(-Freq)
				# for (j in 1:length(to_change)){
			
					
					# STI <- STI %>% 	mutate(note_indicator = str_replace(note_indicator, fixed(paste0('_', to_change[j])), '')) %>% 
									# mutate(note_indicator = str_replace(note_indicator, fixed(paste0(to_change[j], '_')), '')) %>% 
									# mutate(note_indicator = str_replace(note_indicator, fixed(to_change[j]), ''))%>% 
									# mutate(note_indicator = ifelse(note_indicator %in% '', as.character(NA), note_indicator))
					
					
				# }
				
				# STI <- STI %>% mutate(note_source = ifelse(note_source %in% NA, 'R1:3260', paste0('R1:3260_', as.character(note_source)))) 
		
		
		
		
		
		
		# ON_STI <- dplyr:::setdiff(STI , select(ORA, -info)) %>% 
					# distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
					# mutate(diff_sti = 1)
		# ON_ORA <- dplyr:::setdiff(select(ORA, -info), STI) %>% 
					# distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
					# mutate(diff_ora = 1)
		
		# invisible(gc(reset = TRUE))

		
		# # tag STI NEW (check = 0) and STI REV (check = 1)
		
		# TEST_STI <-  STI %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
						# left_join(ON_STI, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						# left_join(ON_ORA, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						# left_join(ORA %>% 	distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
											# mutate(ora = 1), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						# mutate(	check = ifelse(diff_sti %in% 1 & diff_ora %in% 1, 1, NA), 
								# check = ifelse(diff_sti %in% 1 & !diff_ora %in% 1, 0, check), 
								# check = ifelse(check %in% 0 & ora %in% 1, 1, check)) %>% 
						# select(-diff_sti, -diff_ora, -ora) %>% filter(!check %in% NA) 
		# ON_STI <- TEST_STI 
		# rm(TEST_STI)
		# invisible(gc(reset = TRUE))
		# invisible(gc(reset = TRUE))
		
		
		# # reduce ORA to the deletion only (data not revised by STI)
		
		# ON_ORA <- ON_ORA %>% select(-diff_ora) %>%  
						# left_join(STI %>% 
									# distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, TEST) %>% 
									# mutate(check = 1), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
						# filter(!check %in% 1)

		
		
		
		# ############## step 0 manage col data on ORA
		
			# check_ORA <- ON_ORA %>% filter(TEST %in% c('FALSE_COL', 'FALSE_CAL')) %>% mutate(keep = 1)  %>% filter(!str_detect(indicator, 'CPI'))
			
			# check_STI <- STI %>% mutate(collection = 'YTH') %>% left_join( 
								# ORA %>% filter(TEST %in% c('FALSE_COL', 'FALSE_CAL')) %>% 
								# distinct(collection, ref_area, source, indicator, time) %>% mutate(keep = 1)  , 
							# by = c("collection", "ref_area", "source", "indicator", "time")) %>% 
							# filter(keep %in% 1) %>% 
							# select( -freq_code, -keep, -TEST) # %>% 
							# # filter(!str_detect(indicator, 'CPI'))# , !str_detect(note_source, 'R1:3902'), !str_detect(note_source, 'R1:2382')) 
			# X1 <- check_ORA
			# if(nrow(check_ORA)>0){
		
					# X1 <- ORA %>% left_join(check_ORA, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time", "TEST")) %>% 
							# filter(keep %in% 1) %>% select(-keep) %>%
							# left_join(check_STI %>% distinct(collection, ref_area, source, indicator, time,note_source) %>% mutate(keep = 1), 
							# by = c("collection", "ref_area", "source", "indicator", "time") ) %>%
							# filter(keep %in% 1) %>% 
							# select( -freq_code, -check, -keep)
						
				# if(nrow(X1) > 0){		

				# ###### add compare with existing test file
				
					# X1 <- X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value))  
					# X1  %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/check/ON_YI/',ref_collection,'_FROMYI_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
					
					# X1 <- X1 %>% filter(TEST %in% c('FALSE_COL')) %>% count(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time)

				# }
				# # rm(X1)
			# }
			# ON_ORA <- ON_ORA %>% filter(!TEST %in% c('FALSE_COL', 'FALSE_CAL')) 
			
			
			
			# if(nrow(check_STI)>0){

				# ###### add compare with existing test file
				
				# check_STI <- check_STI %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value))
				# check_STI %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/check/ON_YTH/',ref_collection,'_FROMSTI_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')

				# check_STI <- check_STI %>% count(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time, note_source)



			# }
			
			# if(nrow(check_ORA) > 0 & nrow(check_STI)>0 & nrow(X1)>0){
			# #
				# require(ilo)
				# init_ilo(-cl)
				# check_STI <- check_STI %>% 
							# left_join(select(ilo$code$cl_classif_version, classif1_version = code, classif1_version.sort = sort) , by = "classif1_version") %>% 
							# left_join(select(ilo$code$cl_classif_version, classif2_version = code, classif2_version.sort = sort) , by = "classif2_version") %>%
							# left_join(select(ilo$code$cl_indicator, indicator = code, indicator.sort = sort) , by = "indicator") %>% 
							# arrange(indicator.sort, classif1_version.sort, classif2_version.sort, time) %>% 
							# group_by(collection, ref_area, source, indicator, time, note_source) %>% 
							# summarise(sex_version = first(sex_version), 
									  # classif1_version = first(classif1_version),
									  # classif2_version = first(classif2_version),
									  # indicator.sort = first(indicator.sort),
									  # classif1_version.sort = first(classif1_version.sort),
									  # classif2_version.sort = first(classif2_version.sort),
									  # n = first(n),
									  # ) %>% 
							# ungroup %>% 
							# mutate(classif1_version.sort = ifelse(classif1_version.sort %in% NA, 1000, classif1_version.sort)) %>% 
							# mutate(classif2_version.sort = ifelse(classif2_version.sort %in% NA, 1000, classif2_version.sort)) %>% 
							# mutate(ss = indicator.sort + classif1_version.sort + classif2_version.sort) %>% 
							# select(-contains('.sort'))

				# X1 <- X1 %>% 
							# left_join(select(ilo$code$cl_classif_version, classif1_version = code, classif1_version.sort = sort) , by = "classif1_version") %>% 
							# left_join(select(ilo$code$cl_classif_version, classif2_version = code, classif2_version.sort = sort) , by = "classif2_version") %>%
							# left_join(select(ilo$code$cl_indicator, indicator = code, indicator.sort = sort) , by = "indicator") %>% 
							# arrange(indicator.sort, classif1_version.sort, classif2_version.sort, time)  %>% 
							# mutate(classif1_version.sort = ifelse(classif1_version.sort %in% NA, 1000, classif1_version.sort)) %>% 
							# mutate(classif2_version.sort = ifelse(classif2_version.sort %in% NA, 1000, classif2_version.sort))  %>% 
							# mutate(ss = indicator.sort + classif1_version.sort + classif2_version.sort) %>% 
							# select(-contains('.sort'))

							
				# NEW <- X1 %>% left_join(check_STI, by = c("collection", "ref_area", "source", "indicator", "time")) %>% 
					# mutate(TEST = ifelse( ss.x == ss.y & n.x > n.y, "equivalent_more_points_on_ORA", NA)) %>% 
					# mutate(TEST = ifelse( ss.x == ss.y & n.x < n.y, "DEL_equivalent_more_points_on_MICRO", TEST)) %>% 
					# mutate(TEST = ifelse( ss.x == ss.y & n.x == n.y, "DEL_equivalent_same_points_on_MICRO", TEST)) %>% 
					# mutate(TEST = ifelse( ss.x < ss.y & n.x >= n.y, "better_version_on_ORA_more_points_on_ORA", TEST)) %>% 
					# mutate(TEST = ifelse( ss.x < ss.y & n.x < n.y, "DEL_better_version_on_ORA_more_points_on_MICRO", TEST)) %>% 
					# mutate(TEST = ifelse( ss.x > ss.y & n.x == n.y, "DEL_better_version_on_MICRO_same_points_on_MICRO", TEST)) %>% 
					# mutate(TEST = ifelse( ss.x > ss.y & n.x < n.y, "DEL_better_version_on_MICRO_more_points_on_MICRO", TEST)) %>% 
					# mutate(TEST = ifelse( ss.x > ss.y & n.x > n.y, "better_version_on_MICRO_more_points_on_ORA", TEST)) %>% 
					# mutate(TEST = ifelse( TEST %in% c(NA, 'NA') , "Only_on_YI", TEST)) 
				
				
				# ref <- NEW %>% distinct(TEST) %>% .$TEST
				
				# for (j in 1:length(ref)){
					
					# try(dir.create(path = paste0(ilo:::path$sys, 'ILO_Data/check/COMPARE/',ref[j]), showWarnings = FALSE), silent = TRUE)
				
					# NEW %>% filter(TEST %in% ref[j]) %>% select(-TEST) %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/check/COMPARE/',ref[j], '/',  test$value[i] %>% str_replace('.rds', '.csv')), na = '')

				# }
				# rm(NEW, ref)
			
			# }
			
			
			# ON_STI <- ON_STI %>% left_join(check_STI %>% distinct(collection, ref_area, source, indicator, time) %>% mutate(delete = 1), 
							# by = c("collection", "ref_area", "source", "indicator", "time") ) %>%
							# filter(!delete %in% 1)
							
							
							
			# rm(check_STI, X1, check_ORA)
			# invisible(gc(reset = TRUE))
			# invisible(gc(reset = TRUE))
		
		
# ############### new starting point for a normal comparison
		
		
		# ############# first delete if disappear from STI (ie diff STI == 0 & diff ORA > 0 )
		
		
		# if( nrow(ON_STI) == 0 & nrow(ON_ORA) > 0){
		
			# # data from STI COL should not stay --> delete
			
			# check_STI <- ON_ORA %>% filter(TEST %in% 'TRUE_COL') 
		
			# if(nrow(check_STI)>0){
		
					# X1 <- ON_ORA %>% filter(collection %in% ref_collection) %>% select(-check, -TEST)
					# if(nrow(X1) > 0){			
						# X1 %>% mutate_all(as.character) %>%
							# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_collection,'_DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
					# }
					# rm(X1)
			# }
			# ON_ORA <- ON_ORA %>% filter(!TEST %in% c('TRUE_COL', 'TRUE_CAL'))
			# rm(check_STI)
			# invisible(gc(reset = TRUE))
			# invisible(gc(reset = TRUE))

				
			# ON_ORA <- ON_ORA %>% slice(0)
			# ORA <- ORA %>% slice(0)
			# STI <- STI %>% slice(0)
			
			
			# invisible(gc(reset = TRUE))
			# invisible(gc(reset = TRUE))
		
		
		
		# }
		
		
		
		
		
		# ############# second upload data from STI if not exist on YI (ie diff STI > 0 & diff ORA ==0 )
		
	
		# if(nrow(ON_STI) > 0 & nrow(ON_ORA)  == 0){
		
				# ON_STI <- ON_STI %>% select(-delete)
				# X1 <- STI %>% left_join(ON_STI, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
							# filter(check %in% c(1,0), collection %in% ref_collection) %>% 
							# select( -freq_code)
				# X1_rev <- X1 %>% filter(check %in% 1) %>% select(-check)		
							
				# if(nrow(X1_rev) > 0){			
					# X1_rev %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>%  select(-contains('_version')) %>%  select(-contains('TEST')) %>%
						# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_collection,'_REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				# }
				# rm(X1_rev)
				# X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				# if(nrow(X1) > 0){			
					# X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% select(-contains('_version')) %>% select(-contains('TEST')) %>%
						# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_collection,'_NEW_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				# }
				# rm(X1)

			# ON_STI <- ON_STI %>% slice(0)
			# ORA <- ORA %>% slice(0)
			# STI <- STI %>% slice(0)
			# rm(j)
			# invisible(gc(reset = TRUE))
			# invisible(gc(reset = TRUE))
		# }
		
		
	
		# if(nrow(ON_STI) > 0 & nrow(ON_ORA)  > 0){
			# ON_STI <- ON_STI %>% select(-delete)
			# ##### delete on  ORACLE
				# ref_STI <- STI %>% distinct(collection, ref_area, source, indicator) %>% mutate(keep = 1)
				
				
				# X1 <- ON_ORA %>% filter(collection %in% ref_collection) %>% 
						# select(-check) %>% 
						# left_join(ref_STI, by = c("collection", "ref_area", "source", "indicator") ) %>% 
						# filter(keep %in% 1) %>% select(-keep)
						
				# X1 <- X1 %>% left_join(
								# ORA %>% 
									# distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) , 
											# , by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")
									
									# ) %>% filter(!(str_sub(indicator, 1,3) %in% c('INJ', 'LAC') & TEST %in% 'TRUE_CAL')) %>% select(-TEST) 
				
				# if(nrow(X1) > 0){			
					# X1 %>%  mutate_all(as.character) %>% 
						# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_collection,'_DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				# }
				# rm(X1,ref_STI )
			
			# ON_ORA <- ON_ORA %>% slice(0)
			# ORA <- ORA %>% slice(0)
			# rm(j)
			# invisible(gc(reset = TRUE))
			# invisible(gc(reset = TRUE))
		
			# ##### REV and NEW  from STI

				# X1 <- STI %>% left_join(ON_STI %>% select(-TEST), by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
							# filter(check %in% c(1,0), collection %in% ref_collection) %>% 
							# select( -freq_code) %>% select(-TEST)
				# X1_rev <- X1 %>% filter(check %in% 1) %>% select(-check)		
							
				# if(nrow(X1_rev) > 0){			
					# X1_rev %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% select(-contains('TEST'))  %>% select(-contains('_version'))  %>%
						# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_collection,'_REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				# }
				# rm(X1_rev)
				# X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				# if(nrow(X1) > 0){			
					# X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value))   %>% select(-contains('_version'))  %>%
						# data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/YTH_by_ref_area/',ref_collection,'_NEW_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				# }
				# rm(X1)
			
			# ON_STI <- ON_STI %>% slice(0)
			# STI <- STI %>% slice(0)
			
			# invisible(gc(reset = TRUE))
			# invisible(gc(reset = TRUE))
			
		# }
	
	

# }

