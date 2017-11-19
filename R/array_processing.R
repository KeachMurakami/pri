### reading image

add_dimension <-
  function(array){
    dim(array) <- c(dim(array), 1)
    return(array)
  }


read_jpg <-
  function(files, ...){

        EBImage::readImage(files, type = "jpg", ...)
  }

set_read_img <-
  function(img_type = "", band = 2){
    read_img <<-
      if(img_type %in% c("jpg", "JPG", "jpeg", "JPEG")){
        function(files){
          if(length(dim(read_jpg(files[1]))) == 2){
            if(length(files) == 1){
              read_jpg(files)@.Data %>%
                add_dimension
            } else {
              read_jpg(files)@.Data
            }
          } else if(length(files) == 1){
            read_jpg(files)@.Data[,,2, drop = F]
          } else {
            read_jpg(files)@.Data[,,2,]
          }
        }
      } else {
        function(files){
          if(length(dim(EBImage::readImage(files[1]))) == 2){
            if(length(files) == 1){
              EBImage::readImage(files)@.Data %>%
                add_dimension
            } else {
              EBImage::readImage(files)@.Data[,,band]
            }
          } else if(length(files) == 1){
            EBImage::readImage(files)@.Data[,,band] %>%
              add_dimension
          } else {
            EBImage::readImage(files)@.Data[,,band,]
          }
        }
      }

    read_imgs <<-
      if(img_type %in% c("jpg", "JPG", "jpeg", "JPEG")){
        function(files){
          if(length(dim(read_jpg(files[1]))) == 2){
            read_jpg(files)@.Data
          } else {
            read_jpg(files)@.Data[,,band,]
          }
        }
      } else {
        function(files){
          if(length(dim(EBImage::readImage(files[1]))) == 2){
            EBImage::readImage(files)@.Data
          } else {
            EBImage::readImage(files)@.Data[,,band,]
          }
        }
      }
  }

read_imgs_mean <-
  function(files){
    files %>%
      read_imgs %>%
      apply(MARGIN = 1:2, FUN = mean) %>%
      add_dimension
  }


read_one_shots <-
  function(files){
    unique_inits <-
      files %>%
      extract_init_time %>%
      unique

    pforeach(unq = unique_inits, .combine = abind::abind)({
      files %>%
        extract_init_time %>%
        {. == unq} %>%
        files[.] %>%
        read_imgs_mean
    })
  }

### set for affine transformation

set_affine <-
  function(angle, dx, dy){
    angle <<- angle
    dif_x <<- dx
    dif_y <<- dy
  }

get_affine <-
  function(){
    list(angle = angle, dif_x = dif_x, dif_y = dif_y)
  }

see_affine <-
  function(){
    paste0("570 band images were rotated at a degree of ", angle, "° and parallel moved at (x = ", dif_x, ", y = ", dif_y, ").") %>%
      print
  }

affiner <-
  function(matrix){
    rot_radian <- angle * pi / 180
    affine <-
      matrix(c(cos(rot_radian), sin(rot_radian), dif_x, -sin(rot_radian), cos(rot_radian), -dif_y), nrow=3, ncol=2)
    EBImage::affine(matrix, affine)
  }

rotater <-
  function(matrix, angle){
    rot_radian <- angle * pi / 180
    affine <-
      matrix(c(cos(rot_radian), sin(rot_radian), 0, -sin(rot_radian), cos(rot_radian), 0), nrow=3, ncol=2)
    EBImage::affine(matrix, affine)
  }



### select

set_region <-
  function(roi_xy, ref_xy){
    warning("Deprecated: please use `set_roi()`")
    roi_x <<- roi_xy[1]:roi_xy[2]
    roi_y <<- roi_xy[3]:roi_xy[4]
    ref_x <<- ref_xy[1]:ref_xy[2]
    ref_y <<- ref_xy[3]:ref_xy[4]
    if(sum(range(roi_x %in% ref_x), range(roi_y %in% ref_y)) != 4) cat("ERRPR: refelence region does not cover ROI.")
  }

set_roi <-
  function(roi_xy, ref_xy){
    roi_x <<- roi_xy[1]:roi_xy[2]
    roi_y <<- roi_xy[3]:roi_xy[4]
    ref_x <<- ref_xy[1]:ref_xy[2]
    ref_y <<- ref_xy[3]:ref_xy[4]
    if(sum(range(roi_x %in% ref_x), range(roi_y %in% ref_y)) != 4){
      cat("Warning: refelence region does not cover ROI.")
    }
  }

get_roi <-
  function(){
    list(roi_x = roi_x, roi_y = roi_y, ref_x = ref_x, ref_y = ref_y)
  }

see_roi <-
  function(){
    paste0("ROI: (", range(roi_x)[1], ":", range(roi_x)[2], ", ", range(roi_y)[1], ":", range(roi_y)[2], "); ",
           "reflectance values were calculated from reflection intensity on the standard at ", "(", range(ref_x)[1], ":", range(ref_x)[2], ", ", range(ref_y)[1], ":", range(ref_y)[2], ")") %>%
      print
  }

get_reg_plane <-
  function(imgs, rows, cols){
    imgs %>%
      array2df %>%
      filter(row %in% rows, col %in% cols) %>%
      group_by(time) %>%
      do({
        lm(., formula = value ~ row + col) %>%
          broom::tidy() %>%
          dplyr::select(term, estimate) %>%
          tidyr::spread(term, estimate) %>%
          set_names(nm = c("offset", "Col", "Row"))
      }) %>%
      ungroup
  }

ref_plane_all <-
  function(imgs, size = 5){
    rows <- c((min(ref_x) - size):(min(ref_x) + size), (max(ref_x) - size):(max(ref_x) + size))
    cols <- c((min(ref_y) - size):(min(ref_y) + size), (max(ref_y) - size):(max(ref_y) + size))

    reg_plane <- get_reg_plane(imgs, rows, cols)

    expand.grid(row = 1:dim(imgs)[1], col = 1:dim(imgs)[2], time = unique(reg_plane$time)) %>%
      dplyr::left_join(., reg_plane, by = "time") %>%
      dplyr::transmute(row, col, time,
                       value = row * Row + col * Col + offset) %>%
      df2array
  }

ref_plane_roi <-
  function(imgs, size = 5){
    rows <- c((min(ref_x) - size):(min(ref_x) + size), (max(ref_x) - size):(max(ref_x) + size))
    cols <- c((min(ref_y) - size):(min(ref_y) + size), (max(ref_y) - size):(max(ref_y) + size))

    reg_plane <- get_reg_plane(imgs, rows, cols)

    expand.grid(row = roi_x, col = roi_y, time = unique(reg_plane$time)) %>%
      left_join(., reg_plane, by = "time") %>%
      transmute(row, col, time,
                value = row * Row + col * Col + offset) %>%
      df2array
  }

to_reflectance <-
  function(imgs, roi = T, plane = T, ...){
    if(roi){
      if(plane){
        to_refl_roi(imgs, ...)
      } else {
        to_refl_roi_by_point(imgs, ...)
      }
    } else {
      if(plane){
        to_refl_all(imgs, ...)
      } else {
        to_refl_all_by_point(imgs, ...)
      }
    }
  }

to_refl_roi <-
  function(imgs, ...){
    imgs[roi_x, roi_y, 1:dim(imgs)[3], drop = F] / ref_plane_roi(imgs, ...)
  }

to_refl_all <-
  function(imgs, ...){
    imgs[,, 1:dim(imgs)[3], drop = F] / ref_plane_all(imgs, ...)
  }

to_refl_roi_by_point <-
  function(imgs, ...){
    ref_val <- apply(imgs[ref_x, ref_y,], MARGIN = 3, mean)
    ref_array <- array(rep(ref_val, each = length(roi_x) * length(roi_y)), dim = c(length(roi_x), length(roi_y), dim(imgs)[3]))
    imgs[roi_x, roi_y, (1:dim(imgs)[3]), drop = F]  / ref_array
  }

to_refl_all_by_point <-
  function(imgs, ...){
    ref_val <- apply(imgs[ref_x, ref_y,], MARGIN = 3, mean)
    ref_array <- array(rep(ref_val, each = dim(imgs)[1] * dim(imgs)[2]), dim = dim(imgs))
    imgs[,, (1:dim(imgs)[3]), drop = F]  / ref_array
  }


pri_processing <-
  function(list_imgs, ...){
    img_530 <- list_imgs[[1]]
    img_570 <- list_imgs[[2]] %>% affiner

    refl_530_roi <- img_530[roi_x, roi_y, 1:dim(img_530)[3], drop = F] / ref_plane_roi(img_530, ...)
    refl_570_roi <- img_570[roi_x, roi_y, 1:dim(img_570)[3], drop = F] / ref_plane_roi(img_570, ...)

    return(calc_pri(refl_530_roi, refl_570_roi))
  }

refl_processing <-
  function(list_imgs, ...){
    img_530 <- list_imgs[[1]]
    img_570 <- list_imgs[[2]] %>% affiner

    refl_530_roi <- img_530[roi_x, roi_y, 1:dim(img_530)[3], drop = F] / ref_plane_roi(img_530, ...)
    refl_570_roi <- img_570[roi_x, roi_y, 1:dim(img_570)[3], drop = F] / ref_plane_roi(img_570, ...)

    return(reflectance = list(refl_530_roi, refl_570_roi))
  }


read_img_pairs <-
  function(logs, file_530, file_570){
    logs <- logs[logs <= min(max(logs), length(file_530), length(file_570))]
    img_530 <- file_530[logs] %>% read_imgs
    img_570 <- file_570[logs] %>% read_imgs
    return(list(img_530, img_570))
  }


batch_read_imgs <-
  function(files, each = 100, times = 5){
    dim_xy <- files[1] %>% read_img %>% dim %>% .[1:2]
    result_array <-
      array(0, c(length(roi_x), length(roi_y), each * times))
    for(i in 1:times){
      processed_z <- (1:each) + (i - 1) * each
      result_array[,, processed_z] <-
        files[processed_z] %>%
        read_imgs %>%
        .[roi_x, roi_y,]
    }
    return(result_array)
  }

batch_pread_imgs <-
  function(files, each = 100, times = 5){
    dim_xy <- files[1] %>% read_img %>% dim %>% .[1:2]
    result_array <-
      array(0, c(length(roi_x), length(roi_y), each * times))
    pforeach(i = 1:times, .combine = abind)({
      processed_z <- (1:each) + (i - 1) * each
      result_array[,, processed_z] <-
        files[processed_z] %>%
        read_imgs %>%
        .[roi_x, roi_y,]
    })
    return(result_array)
  }



### array handling

z_summary <-
  function(array_3d, time = F){
    results <-
      data_frame(mean = apply(array_3d, 3, mean), sd = apply(array_3d, 3, sd))
    if(time[1]){
      results %>%
        mutate(time = time)
    }
  }

array2df <-
  function(array_3d, time = F){
    dim_ <- dim(array_3d)
    row = rep(1:dim_[1], times = dim_[3] * dim_[2])
    col = rep(rep(1:dim_[2], each = dim_[1]), times = dim_[3])
    
    if(time[1]){
      time = rep(time, each = dim_[1] * dim_[2])
    } else {
      time = rep(1:dim_[3], each = dim_[1] * dim_[2])
    }
    
    array_3d %>%
      as.vector %>%
      data_frame(value = ., row, col, time) %>%
      arrange(time, col, row)
  }

df2array <-
  function(df_3d, time = F){
    dimension <-
      df_3d %>%
      {c(length(unique(.$row)), length(unique(.$col)), length(unique(.$time)))}
    df_3d$value %>%
      array(., dimension)
  }

smooth_timecourse <-
  function(array, num){
    moved_list <- lapply(1:num, function(i) {move_3d(array, i)})
    for(i in 1:num){
      array <- array + moved_list[[i]]
    }
    return(array / (num + 1))
  }

move_3d <- # helper for smooth_timecourse
  function(array, num){
    z_array <- dim(array)[3]
    for(i in 1:num){
      array <-
        abind::abind(
          array[,,1],
          array[,,head(1:z_array, -1)]
        )
    }
    return(array)
  }

mask_gaus <-
  function(mtrx, brush_size = 5, brush_sigma = 5){
    gaus_mask <-
      EBImage::makeBrush(size = brush_size, sigma = brush_sigma,
                         shape = 'gaussian')
    EBImage::filter2(mtrx, gaus_mask)
  }

binning <-
  function(array_3d, binning_size = 3){
    if(length(dim(array_3d)) == 2) dim(array) <- c(dim(array), 1)

    dim_ <- dim(array_3d)
    dim_out <- (dim_[1:2] %/% binning_size) * binning_size
    row_out <- 1:dim_out[1]
    col_out <- 1:dim_out[2]

    row_group <- row_out[cut(row_out, dim_out[1]/binning_size)]
    col_group <- col_out[cut(col_out, dim_out[2]/binning_size)]

    array_3d[row_out, col_out, 1:dim_[3], drop = F] %>%
      array2df %>%
      dplyr::mutate(row = row_group[row],
                    col = col_group[col],
                    blocks = paste0(col, "_", row, "_", time)) %>%
      dplyr::group_by(row, col, time, blocks) %>%
      dplyr::summarize(value = mean(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-blocks) %>%
      dplyr::arrange(time, col, row)
  }



### visualize

# cut_sect <-
#   function(array_3d, by_dim = "x", by_index = 1){
#     dim_3d <- dim(array_3d)
#     if(by_dim == "x"){
#       array_3d[by_index, ,] %>%
#         as.vector %>%
#         data_frame(value = .) %>%
#         mutate(x = by_index,
#                y = rep(1:dim_3d[2], times = dim_3d[3]),
#                z = rep(1:dim_3d[3], each = dim_3d[2]))
#     } else if(by_dim == "y"){
#       array_3d[, by_index ,] %>%
#         as.vector %>%
#         data_frame(value = .) %>%
#         mutate(x = rep(1:dim_3d[1], times = dim_3d[3]),
#                y = by_index,
#                z = rep(1:dim_3d[3], each = dim_3d[1]))
#     }
#   }
# 
# plot_sect <-
#   function(df, sect = "x"){
#     if(sect == "x"){
#       ggplot(df, aes(x = z, y = value, col = factor(x), group = x)) +
#         geom_point(alpha = .25) +
#         geom_line(alpha = .25) +
#         guides(col = F)
#     } else {
#       ggplot(df, aes(x = z, y = value, col = factor(y), group = y)) +
#         geom_point(alpha = .25) +
#         geom_line(alpha = .25) +
#         guides(col = F)
#     }
#   }
# 
# plot_3d_sect <-
#   function(df, sect = "x"){
#     sect_ <- ifelse(sect == "x", "y", "x")
#     z_persp <-
#       df %>%
#       select_(paste0("-", sect)) %>%
#       tidyr::spread_(sect_, "value") %>%
#       select(-z) %>%
#       as.matrix
#     x_persp <- unique(df$z)
#     y_persp <- unique(df[, sect_] %>% unlist)
#   }

timecourse_xy <-
  function(df){
    df %>% 
      ggplot(aes(time, value)) +
      geom_point() +
      facet_grid(col ~ row)
  }

timecourse_xy2 <-
  function(df){
    df %>% 
      ggplot(aes(time, value, col = factor(col), shape = factor(row))) +
      geom_point() +
      guides(col = F, shape = F)
  }

shade_box <-
  function(color = "grey50", alpha = .5){
    ggplot2::annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = +Inf, fill = color, alpha)
  }
