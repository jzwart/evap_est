
evap_calc <- function(ind_file, lakes_file, temp_ind_file, remake_file, gd_config){

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

  # calc evap for each lake on each day
  temp$evap_mm_day <- lapply(evans$Name, function(lake){
    evap_func(airT = temp$tave[temp$lake==lake],
              jDay = seq(1,365),
              lat = evans$Latitude[evans$Name == lake])
  }) %>% unlist()


  data_file <- as_data_file(ind_file)
  saveRDS(temp, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
