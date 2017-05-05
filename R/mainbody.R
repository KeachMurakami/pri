# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# if(!exists("representative")) stop("set a method extracting representative values: representative")

view <-
  function(input, browser = T){
    if(browser){
      output_method <- "browser"
    } else {
      output_method <- "raster"
    }
    if(class(input) == "character"){
      input %>%
        EBImage::readImage() %>%
        EBImage::display(method = output_method)
    } else {
      EBImage::display(input, method = output_method)
    }
  }

calc_pri <-
  function(r530, r570) (r530 - r570)/(r530 + r570)


square_trim <-
  function(img_matrix, x_center, y_center, size = c(5, 5)){
    img_matrix[(x_center - size[1]):(x_center + size[1]), (y_center - size[2]):(y_center + size[2])]
  }

read_serial_image_mono <-
  function(file_paths){
    pforeach(file_path = file_paths, .multicombine = T)({
      gc();gc()
      reading <-
        file_path %>%
        EBImage::readImage() %>%
        .@.Data
      list(list(img = reading, path = file_path, mode = "grey"))
    })
  }

read_serial_image_rgb <-
  function(file_paths, band = "green"){
    pforeach(file_path = file_paths, .multicombine = T)({
      gc();gc()
      reading <-
        file_path %>%
        EBImage::readImage() %>%
        EBImage::channel(mode = band) %>%
        .@.Data
      list(list(img = reading, path = file_path, mode = band))
    })
  }



normalize_image <-
  function(img_matrix, x_center, y_center, size = c(5, 5)){
    size <-
      ifelse(length(size) == 1L, 2L, 1L) %>%
      rep(size, .)
    ref_pixels <-
      square_trim(img_matrix, x_center, y_center, size)

    img_matrix / representative(ref_pixels)
  }

normalize_image_confirm <-
  function(img_matrix, x_center, y_center, size = c(5, 5), .confirm = T){
    size <-
      ifelse(length(size) == 1L, 2L, 1L) %>%
      rep(size, .)
    ref_pixels <-
      square_trim(img_matrix, x_center, y_center, size)

    view(img_matrix, browser = F)
    rect(min(x_center - size[1]), min(y_center - size[2]),
         max(x_center + size[1]), max(y_center + size[2]),
         col = rgb(0, 1, 0, alpha = .1))
  }



normalize_mean_image <-
  function(img_list, num_accumulate, x_center, y_center, size){
    num_img <- length(img_list) / num_accumulate
    size <-
      ifelse(length(size) == 1L, 2L, 1L) %>%
      rep(size, .)
    mode <-
      img_list[[1]]$mode

    pforeach(i = 1:num_img, .multicombine = T)({
      gc();gc()
      serial_img_list <-
        img_list[num_accumulate * (i-1) + 1:num_accumulate]

      paths <-
        serial_img_list %>%
        purrr::map_chr(., "path")

      serial_img_list %>%
        purrr::map(., "img") %>%
        mean_serial_imgs %>%
        normalize_image(., x_center, y_center, size) %>%
        list(normalized_img = ., path = paths, pixels = size,
             accumulated_images = num_accumulate, mode = mode) %>%
        list
    })
  }

mean_serial_imgs <-
  function(serial_img){
    Reduce(f = `+`, serial_img) / length(serial_img)
  }


block_analyze_image <-
  function(normalized_mean_image, x_range, y_range, block_design){
    gc();gc()
    pforeach(i = 1:length(normalized_mean_image))({
      img <-
        normalized_mean_image[[i]]
      block_value <- matrix(0, ncol = block_design[2], nrow = block_design[1])
      for(x in 1:block_design[2]){
        for(y in 1:block_design[1]){
          x_trim <- split(x_range, sort(x_range%%block_design[2]))[[x]]
          y_trim <- split(y_range, sort(y_range%%block_design[1]))[[y]]
          block_value[y,x] <- representative(img$normalized_img[x_trim, y_trim])
          # cat(paste0("x range:", range(x_trim), ", y range:", range(y_trim), "\n"))
        }
      }
      img$block_means <- block_value
      list(img[-1])
    })
  }
