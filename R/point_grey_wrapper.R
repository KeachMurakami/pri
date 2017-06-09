extract_init_time <-
  function(files){
    files %>%
      basename %>%
      stringr::str_split("_") %>%
      purrr::map_chr(~ .[2]) %>%
      stringr::str_sub(1, 17)
  }

extract_serial_count <-
  function(files){
    files %>%
      basename %>%
      stringr::str_split("_") %>%
      purrr::map_chr(~ .[2]) %>%
      stringr::str_sub(-4, -1) %>%
      as.numeric
  }