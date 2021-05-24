library(magick)
library(sf)
library(raster)
library(tidyverse)

folders = c("set14b/101MEDIA","set15b/100MEDIA","set26b/100MEDIA","set26b/200MEDIA","set27b/100MEDIA","set27b/200MEDIA")

counter = 1

metadata = data.frame()

for(folder in folders) {

  images = list.files(folder,pattern="*.JPG$")
  
  for(image_file in images) {
    
    image_id = str_sub(image_file,5,8)

    image = image_read(paste0(folder,"/",image_file))
    img_width = image_info(image)$width
    img_height= image_info(image)$height
    # 
    # image_raster = brick(image_file)
    # raster_cropped = crop(image_raster,bounds_sf[1,])

    bounds = paste0(folder,"/",image_id,".gpkg")
    bounds_sf = st_read(bounds)
    
    # ## flip on y axis
    # bounds_coords = st_coordinates(bounds_sf) %>% as.data.frame
    # bounds_coords[,"Y"] = -bounds_coords[,"Y"]
    # bounds_coords_list = split(bounds_coords,bounds_coords$L2)
    # a = lapply(bounds_coords_list,FUN=function(x) {as.matrix(x[,1:2])})
    # 
    # 
    # bounds_sf = st_as_sfc(st_as_text(st_polygon(a)))
    
    ## get coords of a tree
    
    ## for each of the bboxes
    
    for(i in 1:nrow(bounds_sf)) {

      bbox = bounds_sf[i,] %>% st_bbox() %>% as.vector()
      width = bbox[[3]] - bbox[[1]]
      height = bbox[[4]] - bbox[[2]]
      corner_x = bbox[[1]]
      corner_y = -bbox[[4]]
      
      crop_geom_string = paste0(width,"x",height,"+",corner_x,"+",-corner_y)
      
      cropped = image_crop(image,crop_geom_string)
      
      #image_write(cropped,paste0("clipped/",counter %>% str_pad(4,side="left",pad="0"),".jpg"))

      metadata_row = data.frame(folder = folder,
                                drone_image_file = image_file,
                                clip_id = counter,
                                center_x = mean(c(bbox[[3]], bbox[[1]]))/img_width,
                                center_y = mean(c(bbox[[3]], bbox[[1]]))/img_height
                                )
      
      metadata = bind_rows(metadata,metadata_row)
      
      counter = counter + 1
    }
  }
}


## bring in species IDs and write
d = read_csv("tree_labels_pre.csv")

metadata = left_join(metadata,d,by=c("clip_id" = "tree_id"))

metadata = metadata %>%
  mutate(sp = recode(sp_code,
                     p = "ponderosa",
                     i = "cedar",
                     w = "fir",
                     u = "unknown",
                     s = "sugar")) %>%
  mutate(clipped_filename = paste0(clip_id %>% str_pad(4, side="left",pad="0"), ".jpg")) %>%
  # get which set
  mutate(set = str_split(folder,"/") %>% map_chr(1))

write_csv(metadata,"tree_metadata.csv")

