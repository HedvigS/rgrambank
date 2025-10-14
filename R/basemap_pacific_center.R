#' Makes a pacific-centered map with van der Grinten projection and adjusts the longitude of the datatable accordingly.
#'
#' @param LongLatTable data-frame with columns ID, Longitude and Latitude
#' @param DataTable data-frame with columns ID and possibly additional data to be plotted.
#' @note DataTable is good to include because it will ensure that the records are matched and that the shifted Longitudes are used (necessary for making the plot pacific-centered).
#' @return A list, first object is a ggplot2 layer of a Pacific-centered worldmap and the second object is a combination of LongLatTable and DataTable, with Longitude adjusted to match the map.
#' @author Hedvig Skirgård
#' @export

basemap_pacific_center <- function(LongLatTable = NULL, 
                                   DataTable = NULL){


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

world <- ggplot2::map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- ggplot2::map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#Basemap
basemap <- ggplot2::ggplot(Table) +
  ggplot2::geom_polygon(data=world, ggplot2::aes(x=.data[["long"]], y=.data[["lat"]], group=.data[["group"]]),
               colour="gray87",
               fill="gray87", linewidth = 0.5) +
  ggplot2::geom_polygon(data=lakes, ggplot2::aes(x=.data[["long"]], y=.data[["lat"]], group=.data[["group"]]),
               colour="gray87",
               fill="white", linewidth = 0.3) +
  ggplot2::theme(
    legend.position = "None",
    # all of these lines are just removing default things like grid lines, axes etc
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA),
    panel.background = ggplot2::element_rect(fill = "white"),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  ggplot2::coord_map(projection = "vandergrinten", ylim=c(-55,73)) +
  ggplot2::expand_limits(x = Table$Longitude, y = Table$Latitude)

list(basemap = basemap, MapTable = Table)

}