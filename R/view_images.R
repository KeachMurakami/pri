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
  function(img_a, img_b, ...){
    overlay(img_a, img_b, ...)
  }
