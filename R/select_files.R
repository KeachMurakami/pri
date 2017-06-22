sample_files_by_time <-
  function(files, times = cumsum(1:100), init = "2017-01-01 00:00:00"){
  
    file_mtime <- 
      files %>%
      file.mtime %>%
      as.numeric
  
    init_time <-
      init %>%
      as.POSIXct %>%
      as.numeric
  
    sample_time <-
      init_time + times
  
    ip <- (sample_time + lag(sample_time))/2
    cuts <- c(-Inf, ip[-1], Inf)
  
    tibble::tibble(path = files, mtime = file_mtime) %>%
      dplyr::mutate(target_time = sample_time[findInterval(mtime, cuts)],
                    delta = file_mtime - target_time,
                    target_time_sec = target_time - init_time) %>%
      dplyr::group_by(target_time) %>%
      dplyr::arrange(abs(delta)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup %>%
      dplyr::mutate(mtime = as.POSIXct(mtime, origin = "1970-01-01"),
                    target_time = as.POSIXct(target_time, origin = "1970-01-01"),
                    init_time = init)
  }

rough_join <-
  function(files_x, files_y, acceptable_delta = 1, ...){
    df_x <- sample_files_by_time(files_x, ...)
    df_y <- sample_files_by_time(files_y, ...)
    dplyr::left_join(df_x, df_y, by = c("target_time", "target_time_sec", "init_time")) %>%
      dplyr::filter(abs(delta.x) <= acceptable_delta, abs(delta.y) <= acceptable_delta)
      
  }

## demo code for me

# library(tidyverse)
# library(magrittr)

# files_a <- dir("~/Dropbox/2017/y1702/day14_id01/", pattern = "530_", full.names = T)
# files_b <- dir("~/Dropbox/2017/y1702/day14_id01/", pattern = "570_", full.names = T)

# rough_join(files_a, files_b, times = cumsum(1:1000), init = "2017-06-18 16:10:40")
# sample_files_by_time(files_a, times = cumsum(1:1000), init = "2017-06-18 16:10:40")
