#' makes a pacific-centered map with van der Grinten projection
#'
#' @param LongLatTable data-frame with columns ID, Longitude and Latitude
#' @param DataTable data-frame with columns ID and possibly additional data to be plotted.
#' @note DataTable is good to include because it will ensure that the records are matched and that the shifted Longitudes are used (necessary for making the plot pacific-centered).
#' @author Hedvig Skirgård
#' @import ggplot2 
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
    dplyr::select(ID, Longitude, Latitude) %>% 
    mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) #shifting the longlat of the dataframe to match the pacific centered map

Table <- DataTable %>% 
  left_join(LongLatTable, by = "ID")

world <- ggplot2::map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- ggplot2::map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#Basemap
basemap <- ggplot(Table) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group),
               colour="gray87",
               fill="gray87", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long, y=lat, group=group),
               colour="gray87",
               fill="white", size = 0.3) +
  theme(
    legend.position = "none",
    # all of these lines are just removing default things like grid lines, axes etc
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_map(projection = "vandergrinten", ylim=c(-55,73)) +
  expand_limits(x = Table$Longitude, y = Table$Latitude)


list(basemap = basemap, MapTable = Table)

}