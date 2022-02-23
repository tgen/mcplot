#' Plot geographic participation data
#'
#' @param mc An mc object from the mcdata package.
#' @param scale "world" for world plot or "us" for US plot

plot_geo <- function(mc, scale = "world") {
  options(tigris_use_cache = TRUE)
  if (scale == "world") {
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

    test <- as.data.frame(table(mc[["country_code"]]))
    test[["postal"]] <- test$Var1
    world_modified <- merge(x = world, y = test, by = "postal", all.x = TRUE)
  } else if (scale == "us") {
    us_geo <- tigris::shift_geometry(tigris::states(class = "sf")) %>%
      dplyr::filter(GEOID < 60)

    test <- as.data.frame(table(mc[["region"]]))
    test[["NAME"]] <- test[["Var1"]]
    world_modified <- merge(x = us_geo, y = test, by = "NAME", all.x = TRUE)
  }
  name <- paste0(scale, "_geo_plot.png")
  world_modified[["Freq"]] <- log(world_modified[["Freq"]])
  p <- ggplot2::ggplot(data = world_modified) +
    ggplot2::geom_sf(ggplot2::aes(fill = Freq, color = Freq)) +
    geo_theme() +
    viridis::scale_fill_viridis() +
    viridis::scale_color_viridis()
  ggplot2::guides(color = "none", fill = ggplot2::guide_legend("ln(Participants)\n"))
  ggplot2::ggsave(name, p, "png")
}
