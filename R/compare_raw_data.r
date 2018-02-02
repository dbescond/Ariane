#' Compare STI ORA
#'
#' create NEW, REV, DEL files at ref_area, freq level
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param Str a character vector with UTF-8 encoding errors.
#' @author ILO bescond
#' @keywords ILO, SDMX, R
#' @seealso \code{\link{getCodelist}} \code{\link{getDataStructure}}
#' @examples
#'
#'	# to do
#'
#' @export	

compare_raw_data <- function(ref_collection = 'STI'){


	# clean up directory
	
	ref_file_STI_Upload_By_Country <- list.files(paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/')) %>% as_data_frame
	for(i in 1:nrow(ref_file_STI_Upload_By_Country)){
		unlink(paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/', ref_file_STI_Upload_By_Country$value[i]))
	}
	rm(ref_file_STI_Upload_By_Country)



	# check file folder

	ref_file_ORA <- list.files(paste0(ilo:::path$sys, 'ILO_Data/ORA/')) %>% as_data_frame
	ref_file_STI <- list.files(paste0(ilo:::path$sys, 'ILO_Data/STI/')) %>% as_data_frame


	############################################################
	############################################################
	############################################################
	##########
	########## step 1 test if ORA only so then DEL ref_collection data
	##########
	####################
	
	test <- ref_file_ORA %>% 
				left_join(mutate(ref_file_STI, check = 1), by = "value") %>% 
				filter(check %in% NA)


	if(nrow(test)>0){
		
		for (i in 1:nrow(test)){
		
			X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i])) %>% filter(collection %in% ref_collection)  
			
			for (j in 1:length(ref_collection)){
				X1 <- X %>% filter(collection %in% ref_collection[j])
				if(nrow(X1) > 0){			
					X1 %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
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
				left_join(mutate(ref_file_ORA, check = 1), by = "value") %>% 
				filter(check %in% NA)
	
	if(nrow(test)>0){
		
		for (i in 1:nrow(test)){
		
			X <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/STI/', test$value[i])) %>% select(-freq_code)
			
			for (j in 1:length(ref_collection)){
				X1 <- X %>% filter(collection %in% ref_collection[j])
				if(nrow(X1) > 0){			
					X1 %>%
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_NEW_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
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
		
		ORA <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/ORA/', test$value[i])) %>% filter(collection %in% ref_collection)
		STI <- readRDS(paste0(ilo:::path$sys, 'ILO_Data/STI/', test$value[i])) %>% filter(collection %in% ref_collection)
		
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
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
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
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1_rev)
				X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_NEW_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
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
			for (j in 1:length(ref_collection)){
				X1 <- ON_ORA %>% filter(collection %in% ref_collection[j]) %>% select(-check)
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_DEL_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
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
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_REV_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
				}
				rm(X1_rev)
				X1 <- X1 %>% filter(check %in% 0) %>% select(-check)
				
				if(nrow(X1) > 0){			
					X1 %>%  mutate_all(as.character) %>% mutate(obs_value = as.numeric(obs_value)) %>% 
						data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_collection[j],'_NEW_', test$value[i] %>% str_replace('.rds', '.csv')), na = '')
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
	
	ref_file_STI_Upload <- list.files(paste0(ilo:::path$sys, 'ILO_Data/STI_Upload/')) %>% as_data_frame
	for(i in 1:nrow(ref_file_STI_Upload)){
		unlink(paste0(ilo:::path$sys, 'ILO_Data/STI_Upload/', ref_file_STI_Upload$value[i]))
	}	
	rm(ref_file_STI_Upload)
	
	
	ref_file_STI_Upload_By_Country <- list.files(paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/')) %>% as_data_frame %>%  
		separate(value, c('collection', 'type', 'ref_area', 'freq'), sep = '_', remove = FALSE) %>% mutate(freq = str_sub(freq, 1,1))
	
	
	if(nrow(ref_file_STI_Upload_By_Country) == 0) return('OK, no file to combine !!!')
	
	ref_col <- unique(ref_file_STI_Upload_By_Country$collection)
	
	
	for (i in 1:length(ref_col)){
	
		test <- ref_file_STI_Upload_By_Country %>% filter(collection %in% ref_col[i])
	
		ref_type <- unique(test$type)
		
		for (j in 1:length(ref_type)){
		
			ref_file <- test %>% filter(type %in% ref_type[j])
			
			X <- NULL
			count <- 1
			for(k in 1:nrow(ref_file)){
			
				if(ref_type[j] %in% 'DEL'){
			
					X <- bind_rows(X, 
							read_csv(
									paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_file$value[k]), 
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
					X <- bind_rows(X, 
							read_csv(
									paste0(ilo:::path$sys, 'ILO_Data/STI_Upload_By_Country/',ref_file$value[k]), 
									col_types = cols_only(
												collection = col_character(),
												ref_area = col_character(),
												source = col_character(),
  												indicator = col_character(),
  												sex = col_character(),
  												#sex_version = col_character(),
  												classif1 = col_character(),
  												#classif1_version = col_character(),
  												classif2 = col_character(),
  												#classif2_version = col_character(),
  												time = col_character(),
  												obs_value = col_double(),
  												obs_status = col_character(),
  												note_classif = col_character(),
  												note_indicator = col_character(),
 												note_source = col_character()
											)))
				}
				
				if(nrow(X) > 300000){
					X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload/',ref_col[i],'_',ref_type[j], '_', count,'_', Sys.Date(), '.csv'), na = '')
					X <- NULL
					count <- count + 1
				}
				
		
			}
			
			if(nrow(X) > 0){
					X %>% data.table:::fwrite(file = paste0(ilo:::path$sys, 'ILO_Data/STI_Upload/',ref_col[i],'_',ref_type[j], '_', count,'_', Sys.Date(), '.csv'), na = '')
					X <- NULL
					count <- count + 1
				}
			
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

