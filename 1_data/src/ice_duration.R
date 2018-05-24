
ice_duration <- function(ind_file, lakes_file, temp_ind_file, remake_file, gd_config){

  temp <- readRDS(sc_retrieve(temp_ind_file, remake_file = remake_file))

  evans <- read.csv(lakes_file, stringsAsFactors=FALSE)

  evans <- evans %>%
    mutate(Latitude = ifelse(grepl('N', Latitude),
                             (as.numeric(substr(Latitude, 1, 2)) +
                                (as.numeric(substr(Latitude, 5,6)) / 60)),
                             -1*(as.numeric(substr(Latitude, 1, 2)) +
                                   (as.numeric(substr(Latitude, 5,6)) / 60))),
           Longitude = ifelse(grepl('E', Longitude),
                              as.numeric(regmatches(Longitude,
                                                    gregexpr('^[0-9]*',
                                                             Longitude))) +
                                (as.numeric(regmatches(Longitude,
                                                       gregexpr(' [0-9]{2}',
                                                                Longitude)))/60),
                              180 + (180 - (as.numeric(regmatches(Longitude,
                                                                  gregexpr('^[0-9]*',
                                                                           Longitude))) +
                                              (as.numeric(regmatches(Longitude,
                                                                     gregexpr(' [0-9]{2}',
                                                                              Longitude)))/60)))))
  # ice duration function from Weyhenmeyer et al. 2013 Freshwater Biology 58: 612-623
  ice_dur_func <- function(airT_mean, airT_amp){
    ice_dur = 365.25/pi*acos(airT_mean/airT_amp)
    return(ice_dur)
  }

  # calc evap for each lake on each day
  ice_dur <- lapply(evans$Name, function(lake){
    ice_dur_func(airT_mean = mean(temp$tave[temp$lake==lake]),
                 airT_amp = diff(range(temp$tave[temp$lake==lake])))
  }) %>% unlist() %>%
    data_frame() %>%
    rename(ice_dur_days = '.') %>%
    bind_cols(evans, .) %>%
    mutate(ice_dur_days = ifelse(is.na(ice_dur_days), 0, ice_dur_days))

  data_file <- as_data_file(ind_file)
  saveRDS(ice_dur, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
