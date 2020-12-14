run_mlm <- function(data, y='n_months', nesting = 'company'){
  
  
  data = data %>% 
    dplyr::rename(y = all_of(y))
  
  if(nesting=='company'){
    
    mlm_cos_dist <- lmer(y ~ cos_dist + (cos_dist||comp_id), 
                         data = data)
    
    mlm_euc_dist <- lmer(y ~ euc_dist + (euc_dist||comp_id), 
                         data = data)
    
    mlm_manh_dist <- lmer(y ~ manh_dist + (manh_dist||comp_id), 
                          data = data)
    
    mlm_iforest_ad <- lmer(y ~ iforest_ad + (iforest_ad||comp_id), 
                           data = data)
    
    mlm_ocsvm_ad <- lmer(y ~ ocsvm_ad + (ocsvm_ad||comp_id), 
                         data = data)
    
    mlm_sos_ad <- lmer(y ~ sos_ad + (sos_ad||comp_id), 
                       data = data)
  } 
  
  
  if(nesting=='person'){
    
    mlm_cos_dist <- lmer(y ~ cos_dist + (cos_dist||person_id), 
                         data = data)
    
    mlm_euc_dist <- lmer(y ~ euc_dist + (euc_dist||person_id), 
                         data = data)
    
    mlm_manh_dist <- lmer(y ~ manh_dist + (manh_dist||person_id), 
                          data = data)
    
    mlm_iforest_ad <- lmer(y ~ iforest_ad + (iforest_ad||person_id), 
                           data = data)
    
    mlm_ocsvm_ad <- lmer(y ~ ocsvm_ad + (ocsvm_ad||person_id), 
                         data = data)
    
    mlm_sos_ad <- lmer(y ~ sos_ad + (sos_ad||person_id), 
                       data = data)
  } 
  
  
  if(nesting=='crossed'){
    
    mlm_cos_dist <- lmer(y ~ cos_dist + 
                           (cos_dist||comp_id) + 
                           (cos_dist||person_id), 
                         data = data)
    
    mlm_euc_dist <- lmer(y ~ euc_dist
                         + (euc_dist||comp_id)
                         + (euc_dist||person_id), 
                         data = data)
    
    mlm_manh_dist <- lmer(y ~ manh_dist
                          + (manh_dist||comp_id)
                          + (manh_dist||person_id), 
                          data = data)
    
    mlm_iforest_ad <- lmer(y ~ iforest_ad
                           + (iforest_ad||comp_id)
                           + (iforest_ad||person_id), 
                           data = data)
    
    mlm_ocsvm_ad <- lmer(y ~ ocsvm_ad
                         + (ocsvm_ad||comp_id)
                         + (ocsvm_ad||person_id), 
                         data = data)
    
    mlm_sos_ad <- lmer(y ~ sos_ad
                       + (sos_ad||comp_id)
                       + (sos_ad||person_id), 
                       data = data)
    
  }
  
  
  # create list with results
  results <- list('mlm_cos_dist' = mlm_cos_dist, 
                  "mlm_euc_dist" = mlm_euc_dist,
                  "mlm_manh_dist" = mlm_manh_dist,
                  "mlm_iforest_ad" = mlm_iforest_ad,
                  "mlm_ocsvm_ad" = mlm_ocsvm_ad,
                  "mlm_sos_ad" = mlm_sos_ad)
}


mlm_aggregator <- function(model_list){
  
  model_df <- model_list %>%
    map(summary) %>%
    map(coefficients) %>%
    map(as.data.frame) %>%
    map(rownames_to_column, 'pred') %>% 
    bind_rows() %>% 
    filter(str_detect(pred, 'dist|ad'))
  
  model_df
  
}


data_combiner <- function(file_list, dir, remove_str){
  
  df <- paste0(dir, file_list) %>% 
    map(read.csv) %>% 
    set_names(str_remove(file_list, remove_str)) %>%
    bind_rows(.id = "comp_id")
  
  return(df)
}