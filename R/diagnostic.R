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

afn_view <-
  function(input, ...){
  input %>% affiner %>% view(...)
}

check_roi <-
  function(img, col_ref = "white", col_roi = "red", ...){
    view(img, ...)
    rect(xleft = min(ref_x), xright = max(ref_x), ytop = min(ref_y), ybottom = max(ref_y),
         border = col_ref, lwd = 3)
    rect(xleft = min(roi_x), xright = max(roi_x), ytop = min(roi_y), ybottom = max(roi_y),
         border = col_roi, lwd = 3)
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


check_corners <-
  function(array_3d, size = 5, plot = T){
    roi_x_ <- range(roi_x)
    roi_y_ <- range(roi_y)
    z_range <- 1:dim(array_3d)[3]
    
    left <- (roi_x_[1] - size):(roi_x_[1] + size)
    right <- (roi_x_[2] - size):(roi_x_[2] + size)
    upper <- (roi_y_[1] - size):(roi_y_[1] + size)
    lower <- (roi_y_[2] - size):(roi_y_[2] + size)
    
    results <-
      bind_rows(
        array_3d[left, upper,] %>% z_summary %>% mutate(position = "left_upper", z = z_range),
        array_3d[left, lower,] %>% z_summary %>% mutate(position = "left_lower", z = z_range),
        array_3d[right, upper,] %>% z_summary %>% mutate(position = "right_upper", z = z_range),
        array_3d[right, lower,] %>% z_summary %>% mutate(position = "right_lower", z = z_range)
      )
    
    if(plot){
      results %>%
        ggplot(aes(z, mean, col = position)) +
        geom_point()
    } else {
      return(results)
    }
  }
