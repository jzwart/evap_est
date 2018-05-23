# evap estimates for lakes from Evans et al. 2017

get_temp <- function(ind_file, lakes_file, tmax_file, tmin_file, gd_config){

  tmax <- nc_open(tmax_file)

  tmin <- nc_open(tmin_file)

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

  lats <- tmax$dim$lat$vals
  longs <- tmax$dim$lon$vals

  temp_out <- data_frame()

  for(lake in 1:nrow(evans)){
    # find closest lat / long and extract tmax & tmin
    print(lake)

    cur_lat <- which(abs(lats - evans$Latitude[lake]) == min(abs(lats - evans$Latitude[lake])))[1]
    cur_long <- which(abs(longs - evans$Longitude[lake]) == min(abs(longs - evans$Longitude[lake])))[1]

    # dim = [degrees E, degrees N, time]
    # tmax
    cur_tmax <- ncvar_get(tmax, tmax$var$tmax,
                                   start = c(cur_long, cur_lat, 1),
                                   count = c(1, 1, -1), raw_datavals = TRUE)

    #tmin
    cur_tmin <- ncvar_get(tmin, tmin$var$tmin,
                                   start = c(cur_long, cur_lat, 1),
                                   count = c(1, 1, -1), raw_datavals = TRUE)

    #tave
    cur_tave <- (cur_tmax + cur_tmin) / 2

    temp_out <- bind_rows(temp_out,
                          data_frame(lake = rep(evans$Name[lake], length(cur_tmax)),
                                     doy = seq(1,length(cur_tmax)),
                                     tmax = cur_tmax,
                                     tmin = cur_tmin,
                                     tave = cur_tave))
  }

  data_file <- as_data_file(ind_file)
  saveRDS(temp_out, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
