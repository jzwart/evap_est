
evap_annual <- function(ind_file, evap_ind_file, ice_dur_ind_file, remake_file, gd_config){
  evap <- readRDS(sc_retrieve(evap_ind_file, remake_file = remake_file))

  ice_dur <- readRDS(sc_retrieve(ice_dur_ind_file, remake_file = remake_file))

  # apply ice duration days to lowest temperature days for each lake; calc total evap for rest of days

  annual_evap = lapply(ice_dur$Name, function(lake){
    cur_dur = ice_dur$ice_dur_days[ice_dur$Name==lake]
    cur_evap = evap[evap$lake==lake,]
    annual_evap = cur_evap[sort.list(cur_evap$tave),] %>% # sorting by low to high temp and taking
      slice(round(cur_dur+1):nrow(.)) %>%
      group_by(lake) %>%
      summarise(annual_evap_m = sum(evap_mm_day)/1000) # annual evap in m
  }) %>% bind_rows()

  data_file <- as_data_file(ind_file)
  saveRDS(annual_evap, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
