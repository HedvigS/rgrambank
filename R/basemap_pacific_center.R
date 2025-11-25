#' Makes a pacific-centered map with van der Grinten projection and adjusts the longitude of the datatable accordingly.
#'
#' @param LongLatTable data-frame with columns ID, Longitude and Latitude
#' @param DataTable data-frame with columns ID and possibly additional data to be plotted.
#' @note DataTable is good to include because it will ensure that the records are matched and that the shifted Longitudes are used (necessary for making the plot pacific-centered).
#' @param ylim limits on latitude. If set to other than c(-54,75), the map projection will be rectangular instead of vandergrinten
#' @param xlim limites on longitude. If set to other than c(-180, 180)), the map projection will be rectangular instead of vandergrinten
#' @return A list, first object is a ggplot2 layer of a Pacific-centered worldmap and the second object is a combination of LongLatTable and DataTable, with Longitude adjusted to match the map.
#' @author Hedvig Skirgård
#' @export

basemap_pacific_center <- function(LongLatTable = NULL, 
                                   DataTable = NULL, 
                                   ylim =c(-54,75), 
                                   xlim = c(-180, 180),
                                   land_color = "gray87", 
                                   water_color = "white"){


if(!all(c("Longitude", "ID", "Latitude") %in% colnames(LongLatTable))){
  stop("LongLatTable lacks the columns Longitude, Latitude and/or ID.")
  }

if(!("ID"  %in% colnames(DataTable))){
    stop("DataTable lacks the columns ID.")
  }
  
if(!all(DataTable$ID %in% LongLatTable$ID)){
  stop("There are records in the DataTable that are missing in the LanguageTable.")
  }

  
  LongLatTable <- LongLatTable %>% 
    dplyr::select("ID", "Longitude", "Latitude") %>% 
    dplyr::mutate(Longitude = dplyr::if_else(.data[["Longitude"]] <= -25, 
                                      true = .data[["Longitude"]] + 360, 
                                      false = .data[["Longitude"]])) #shifting the longlat of the dataframe to match the pacific centered map

Table <- DataTable %>% 
  dplyr::left_join(LongLatTable, by = "ID")

if(all(ylim == c(-54,75) & xlim == c(-180, 180)) == TRUE){

  #IF WE ARE PLOTTING THE WHOLE WORLD WE CAN USE THE VANDERGRINTEN
world <- ggplot2::map_data('world', wrap=c(-25,335), margin=T)
lakes <- ggplot2::map_data("lakes", wrap=c(-25,335), col=water_color, border=land_color, margin=T)

#Basemap
basemap <- ggplot2::ggplot(Table) +
  ggplot2::geom_polygon(data=world, ggplot2::aes(x=.data[["long"]], y=.data[["lat"]], group=.data[["group"]]),
               colour=land_color,
               fill=land_color, linewidth = 0.5) +
  ggplot2::geom_polygon(data=lakes, ggplot2::aes(x=.data[["long"]], y=.data[["lat"]], group=.data[["group"]]),
               colour=land_color,
               fill=water_color, linewidth = 0.3) +
  ggplot2::theme(
    legend.position = "None",
    # all of these lines are just removing default things like grid lines, axes etc
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA),
    panel.background = ggplot2::element_rect(fill = water_color),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  ggplot2::coord_map(projection = "vandergrinten", ylim= ylim) +  
  ggplot2::scale_x_continuous(expand = c(0,0)) +
  ggplot2::scale_y_continuous(expand = c(0,0))

}else{
  
  world <- ggplot2::map_data('world', margin=T)
  lakes <- ggplot2::map_data("lakes", col=water_color, border=land_color, margin=T)
  
  #Basemap
  basemap <- ggplot2::ggplot(Table) +
    ggplot2::geom_polygon(data=world, ggplot2::aes(x=.data[["long"]], y=.data[["lat"]], group=.data[["group"]]),
                          colour=land_color,
                          fill=land_color, linewidth = 0.5) +
    ggplot2::geom_polygon(data=lakes, ggplot2::aes(x=.data[["long"]], y=.data[["lat"]], group=.data[["group"]]),
                          colour=land_color,
                          fill=water_color, linewidth = 0.3) +
    ggplot2::theme(
      legend.position = "None",
      # all of these lines are just removing default things like grid lines, axes etc
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      panel.background = ggplot2::element_rect(fill = water_color),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::coord_equal(xlim = xlim, ylim = ylim) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0))
  }

list(basemap = basemap, MapTable = Table)

}