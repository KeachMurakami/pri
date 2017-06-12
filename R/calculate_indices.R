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


calc_pri <-
  function(ref_530, ref_570){
    (ref_530 - ref_570)/(ref_530 + ref_570)
  }

calc_ndvi <-
  function(ref_nir, ref_red){
    (ref_nir - ref_red)/(ref_nir + ref_red)
  }

calc_evi <-
  function(ref_nir, ref_red, ref_blue, gain = 2.5, coef_nir = 6, coef_blue = 7.5, intercept = 1){
    gain * (ref_nir - ref_red) / (ref_nir * coef_nir * ref_red - coef_blue * ref_blue + intercept)
  }


# overlay <-
#   function(img_a, img_b, alpha = 1){
#     third_ch <- matrix(0, nrow = nrow(img_a), ncol = ncol(img_a))
#     EBImage::rgbImage(red = (1 - img_a) * alpha, green = img_b * alpha, blue = third_ch) %>%
#       view
#   }
#
# affiner <-
#   function(matrix, angle, x, y){
#     rot_radian <- angle / (2 * pi)
#     affine <-
#       matrix(c(cos(rot_radian), sin(rot_radian), x, -sin(rot_radian), cos(rot_radian), -y), nrow=3, ncol=2)
#     EBImage::affine(matrix, affine)
#   }
#
# check_position <-
#   function(img_a, img_b, ...){
#     overlay(img_a, affiner(img_b, ...))
#   }
#
# square_trim <-
#   function(img_matrix, x_center, y_center, size = c(5, 5)){
#     img_matrix[(x_center - size[1]):(x_center + size[1]), (y_center - size[2]):(y_center + size[2])]
#   }
#
# read_serial_image_mono <-
#   function(file_paths){
#     pforeach(file_path = file_paths, .multicombine = T)({
#       gc();gc()
#       reading <-
#         file_path %>%
#         EBImage::readImage() %>%
#         .@.Data
#       list(list(img = reading, path = file_path, mode = "grey"))
#     })
#   }
#
# read_serial_image_rgb <-
#   function(file_paths, band = "green"){
#     pforeach(file_path = file_paths, .multicombine = T)({
#       gc();gc()
#       reading <-
#         file_path %>%
#         EBImage::readImage() %>%
#         EBImage::channel(mode = band) %>%
#         .@.Data
#       list(list(img = reading, path = file_path, mode = band))
#     })
#   }
#
#
#
# normalize_image <-
#   function(img_matrix, x_center, y_center, size = c(5, 5)){
#     size <-
#       ifelse(length(size) == 1L, 2L, 1L) %>%
#       rep(size, .)
#     ref_pixels <-
#       square_trim(img_matrix, x_center, y_center, size)
#
#     img_matrix / representative(ref_pixels)
#   }
#
# normalize_image_confirm <-
#   function(img_matrix, x_center, y_center, size = c(5, 5)){
#     size <-
#       ifelse(length(size) == 1L, 2L, 1L) %>%
#       rep(size, .)
#     ref_pixels <-
#       square_trim(img_matrix, x_center, y_center, size)
#
#     view(img_matrix, browser = F)
#     rect(min(x_center - size[1]), min(y_center - size[2]),
#          max(x_center + size[1]), max(y_center + size[2]),
#          col = rgb(0, 1, 0, alpha = .1))
#   }
#
#
#
# mean_image <-
#   function(img_list, num_accumulate){
#     num_img <- length(img_list) / num_accumulate
#     mode <-
#       img_list[[1]]$mode
#
#     pforeach(i = 1:num_img, .multicombine = T)({
#       gc();gc()
#       serial_img_list <-
#         img_list[num_accumulate * (i-1) + 1:num_accumulate]
#
#       paths <-
#         serial_img_list %>%
#         purrr::map_chr(., "path")
#
#       serial_img_list %>%
#         purrr::map(., "img") %>%
#         mean_serial_imgs %>%
#         list(normalized_img = ., path = paths, mode = mode) %>%
#         list
#     })
#   }
#
# normalize_mean_image <-
#   function(img_list, num_accumulate, x_center, y_center, size){
#     num_img <- length(img_list) / num_accumulate
#     size <-
#       ifelse(length(size) == 1L, 2L, 1L) %>%
#       rep(size, .)
#     mode <-
#       img_list[[1]]$mode
#
#     pforeach(i = 1:num_img, .multicombine = T)({
#       gc();gc()
#       serial_img_list <-
#         img_list[num_accumulate * (i-1) + 1:num_accumulate]
#
#       paths <-
#         serial_img_list %>%
#         purrr::map_chr(., "path")
#
#       serial_img_list %>%
#         purrr::map(., "img") %>%
#         mean_serial_imgs %>%
#         normalize_image(., x_center, y_center, size) %>%
#         list(normalized_img = ., path = paths, pixels = size,
#              accumulated_images = num_accumulate, mode = mode) %>%
#         list
#     })
#   }
#
# mean_serial_imgs <-
#   function(serial_img){
#     Reduce(f = `+`, serial_img) / length(serial_img)
#   }
#
#
# block_analyze_image <-
#   function(normalized_mean_image, x_range, y_range, block_design){
#     gc();gc()
#     pforeach(i = 1:length(normalized_mean_image))({
#       img <-
#         normalized_mean_image[[i]]
#       block_value <- matrix(0, ncol = block_design[2], nrow = block_design[1])
#       for(x in 1:block_design[2]){
#         for(y in 1:block_design[1]){
#           x_trim <- split(x_range, sort(x_range%%block_design[2]))[[x]]
#           y_trim <- split(y_range, sort(y_range%%block_design[1]))[[y]]
#           block_value[y,x] <- representative(img$normalized_img[x_trim, y_trim])
#           # cat(paste0("x range:", range(x_trim), ", y range:", range(y_trim), "\n"))
#         }
#       }
#       img$block_means <- block_value
#       list(img[-1])
#     })
#   }
