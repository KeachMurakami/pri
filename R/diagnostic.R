view <-
  function(input, browser = T, ...){
    if(browser){
      output_method <- "browser"
    } else {
      output_method <- "raster"
    }
    if(class(input) == "character"){
      input %>%
        EBImage::readImage() %>%
        EBImage::display(method = output_method, ...)
    } else {
      EBImage::display(input, method = output_method, ...)
    }
  }

afn_view <-
  function(input, ...){
  input %>% affiner %>% view(...)
}

px_timecourse <-
  function(array_3d, x, y, size){
    array_3d[(x-size):(x+size), (y-size):(y+size), , drop = F] %>%
    {data_frame(mean = apply(., 3, mean), sd = apply(., 3, sd))} %>%
      mutate(position = paste0("(", x-size, ":", x+size, ", ", y-size, ":", y+size, ")"),
             z = seq_along(position))
  }


px_timecourse_plot <-
  function(df){
    df %>%
      ggplot(aes(z, mean, group = position)) +
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = position), alpha = .25) +
      geom_point(aes(color = position))
  }

timecourse <-
  function(array_3d, x_centers, y_centers, size, plot = T){
    results <-
      expand.grid(x = x_centers, y = y_centers) %>%
      mutate(id = seq_along(x)) %>%
      split(.$id) %>%
      map_df(~ px_timecourse(array_3d, x = .$x, y = .$y, size))
    
    if(plot){
      results %>% px_timecourse_plot
    } else {
      return(results)
    }
  }


px_position <-
  function(array_3d, x, y, size){
    array_3d <- array_3d[,,, drop = F]
    array_3d[(x-size):(x+size), (y-size):(y+size),] <- array_3d[(x-size):(x+size), (y-size):(y+size),] * Inf
    return(array_3d)
  }

position <-
  function(array_3d, x_centers, y_centers, size, plot = T, ...){
    points <-
      expand.grid(x = x_centers, y = y_centers) %>%
      mutate(id = seq_along(x))
    
    for(i in points$id){
      array_3d <- px_position(array_3d, points[i, "x"], points[i, "y"], size)
    }
    
    if(plot){
      view(array_3d, ...)
    } else {
      return(array_3d)
    }
  }

check_fov <-
  function(img, col_ref = "white", col_roi = "red", ...){
    view(img, ...)
    rect(xleft = min(ref_x), xright = max(ref_x),
         ytop = min(ref_y), ybottom = max(ref_y),
         border = col_ref, lwd = 3)
    rect(xleft = min(roi_x), xright = max(roi_x),
         ytop = min(roi_y), ybottom = max(roi_y),
         border = col_roi, lwd = 3)
  }

check_roi <-
  function(img, col_ref = "white", col_roi = "red", ...){
    warning("Deprecated: please use `check_fov()`")
    check_fov(img, col_ref, col_roi, ...)
  }


edge_filter <-
  function(img){
    filter_mtrx <- matrix(1, ncol = 3, nrow = 3)
    filter_mtrx[2,2] <- -8
    EBImage::filter2(img, filter_mtrx)
  }

overlay <-
  function(img_a, img_b, alpha = 1, ...){
    third_ch <- matrix(0, nrow = nrow(img_a), ncol = ncol(img_a))
    EBImage::rgbImage(red = img_a[,,1] * alpha, green = img_b[,,1] * alpha, blue = third_ch) %>%
      view(...)
  }

overlay_edge <-
  function(img_a, img_b, alpha = 1, ...){
    third_ch <- matrix(0, nrow = nrow(img_a), ncol = ncol(img_a))
    img_a <- edge_filter(img_a)
    img_b <- edge_filter(img_b)
    EBImage::rgbImage(red = img_a[,,1] * alpha, green = img_b[,,1] * alpha, blue = third_ch) %>%
      view(...)
  }

check_position <-
  function(img_a, img_b, ...){
    warning("Deprecated: please use `check_overlay()`")
    overlay(img_a, img_b, ...)
  }

check_overlay <-
  function(img_a, img_b, edge = F, ...){
    if(edge){
      overlay_edge(img_a, img_b, ...)
    } else {
      overlay(img_a, img_b, ...)
    }
  }


multi_check <-
  function(img_a, img_b, preparing){
    view(img_a, browser = preparing)
    text(x = 0, y = 0, adj = c(0, 1), "  530 nm", col = "white")
    affiner(img_b) %>% view(., browser = preparing)
    text(x = 0, y = 0, adj = c(0, 1), "  570 nm", col = "white")

    check_roi(img_a, browser = preparing)
    check_roi(affiner(img_b), browser = preparing)
    to_refl_all(img_a) %>% view(browser = preparing)
    to_refl_all(affiner(img_b)) %>% view(browser = preparing)
    check_overlay(img_a, affiner(img_b), browser = preparing)
  }