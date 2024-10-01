#' Make CPUE maps
#' 
#' @param gapindex_cpue A data.frame output by gapindex::calc_cpue()
#' @param crs Coordinate reference system for the map as a type recognized by sf::st_crs()
#' @param breaks Optional break values for CPUE plots. Uses package defaults if not provided.
#' @param fig_res Figure resolution for .png output files.
#' @export
#' @import ggplot2 dplyr akgfmaps ragg scales

plot_cpue_maps <- function(gapindex_cpue, crs = "EPSG:3338", breaks = NULL, fig_res = 300) {

  # Default break values for CPUE maps, by region
  cpue_breaks <- list(
    EBS = list(
      Eelpouts = c(-1, 0, 50, 100, 500, 1000, 2000),
      Jellyfish = c(-1, 0, 100, 500, 1000, 10000, 20000),
      Poachers = c(-1, 0, 50, 100, 500, 1000, 2000),
      `Sea anemones` = c(-1, 0, 100, 500, 1000, 5000, 10000),
      `Sea pens` = c(-1, 0, 50, 100, 250, 500, 600),
      `Sea stars` = c(-1, 0, 100, 1000, 5000, 10000, 15000),
      Sponges = c(-1, 0, 100, 1000, 10000, 100000, 150000)
      
    ),
    NBS = list(
      Eelpouts = c(-1, 0, 50, 100, 500, 1000, 2000),
      Jellyfish = c(-1, 0, 100, 500, 1000, 10000, 20000),
      Poachers = c(-1, 0, 50, 100, 500, 1000, 2000),
      `Sea anemones` = c(-1, 0, 100, 500, 1000, 5000, 10000),
      `Sea pens` = c(-1, 0, 50, 100, 250, 500, 600),
      `Sea stars` = c(-1, 0, 100, 1000, 5000, 10000, 15000),
      Sponges = c(-1, 0, 100, 1000, 10000, 100000, 150000)
    ),
    AI = list(
      Corals = c(-1, 0, 100, 500, 1000, 5000, 8000), 
      Eelpouts = c(-1, 0, 25, 50, 100, 250, 400),
      Jellyfish = c(-1, 0, 50, 100, 500, 1000, 2000),
      Myctophids = c(-1, 0, 5, 10, 25, 50, 60),
      Poachers =  c(-1, 0, 10, 25, 50, 100, 200),
      `Sea anemones` = c(-1, 0, 25, 50, 100, 500, 1000),
      `Sea pens` = c(-1, 0, 10, 25, 50, 100, 200),
      `Sea stars` = c(-1, 0, 25, 50, 100, 500, 1000),
      Shrimps = c(-1, 0, 25, 50, 100, 500, 1000),
      Sponges = c(-1, 0, 500, 1000, 5000, 10000, 12000)
    ),
    GOA = list(
      Capelin = c(-1, 0, 100, 500, 1000, 5000, 7000),
      Corals = c(-1, 0, 100, 500, 1000, 5000, 7000),
      Eelpouts = c(-1, 0, 25, 50, 100, 500, 1000),
      Eulachon = c(-1, 0, 100, 500, 1000, 5000, 7000),
      Jellyfish = c(-1, 0, 100, 500, 1000, 5000, 7000),
      Myctophids = c(-1, 0, 25, 50, 100, 500, 800),
      `Pacific herring` = c(-1, 0, 100, 500, 1000, 10000, 15000),
      `Pacific sandfish` = c(-1, 0, 25, 50, 100, 500, 800),
      Poachers = c(-1, 0, 25, 50, 100, 250, 500),
      Pricklebacks = c(-1, 0, 50, 100, 500, 1000, 2000),
      Sandlances = c(-1, 0, 2, 5, 10, 25, 50),
      `Sea anemones` = c(-1, 0, 100, 500, 1000, 5000, 7000),
      `Sea pens` = c(-1, 0, 25, 50, 100, 250, 500),
      `Sea stars` = c(-1, 0, 50, 100, 500, 1000, 2000),
      Shrimps = c(-1, 0, 50, 100, 500, 1000, 2000),
      Sponges = c(-1, 0, 100, 500, 1000, 5000, 2000)
    )
  )
  
  survey <- unique(gapindex_cpue$SURVEY)
  taxon <- unique(gapindex_cpue$SPECIES_CODE)
  
  # Set region
  region <- switch(survey,
                   'BSS' = "bs.slope",
                   'EBS' = "sebs",
                   'NBS' = "nbs",
                   'AI' = "ai",
                   'GOA' = "goa")
  
  if(all(c("EBS", "NBS") %in% survey)) {
    region <- "ebs"
  }
  
  if(is.null(breaks)) {
    breaks <- cpue_breaks[[survey[1]]][[taxon]]
  }
  
  fit_cpue <- gapindex_cpue[c("YEAR",
                              "SPECIES_CODE", 
                              "LATITUDE_DD_START", 
                              "LONGITUDE_DD_START", 
                              "CPUE_KGKM2"
                              )]
  
  names(fit_cpue) <- c("YEAR", "COMMON_NAME", "LATITUDE", "LONGITUDE", "CPUE_KGHA")
  
  survey_years <- sort(unique(fit_cpue$YEAR))
  species_group <- paste(fit_cpue$COMMON_NAME[1], collapse = "_")
  
  dir.create(here::here("plots", survey, "cpue_maps"), showWarnings = FALSE)
  
  # Last 4 survey maps ----
  
  # Last 4: EBS/SEBS/NBS ----
  
  if(region %in% c("ebs", "nbs", "sebs")) {
    
    cpue_stack <- make_idw_stack(x = fit_cpue, 
                                 region = region,
                                 grouping.vars = "YEAR", 
                                 extrapolation.grid.type = "sf",
                                 in.crs = "WGS84",
                                 out.crs = crs,
                                 set.breaks = breaks)
    
    n_levels <- length(levels(cpue_stack$extrapolation.stack$var1.pred))
    
    plot_last_4 <- ggplot() +
      geom_sf(data = dplyr::filter(cpue_stack$extrapolation.stack,
                                   YEAR %in% tail(survey_years, 4)),
              mapping = aes(fill = var1.pred),
              color = NA,
              show.legend = TRUE) +
      geom_sf(data = cpue_stack$map_layers$akland, 
              fill = "grey70", 
              color = "black") +
      geom_sf(data = cpue_stack$map_layers$survey.strata, fill = NA) +
      geom_sf(data = cpue_stack$map_layers$graticule, alpha = 0.3, linewidth = 0.2) +
      scale_fill_manual(name = expression(CPUE~(kg%.%km^-2)),
                        values = c("white", scales::viridis_pal(option = "mako", 
                                                                 direction = -1)(n_levels)[2:n_levels]),
                        drop = FALSE) +
      scale_x_continuous(breaks = cpue_stack$map_layers$lon.breaks,
                         limits = cpue_stack$map_layers$plot.boundary$x) +
      scale_y_continuous(breaks = cpue_stack$map_layers$lat.breaks,
                         limits = cpue_stack$map_layers$plot.boundary$y) +
      facet_wrap(~YEAR) +
      esrindex::theme_blue_strip() +
      theme(legend.key = element_rect(color = "black"), 
            legend.title = element_text())
    
    ragg::agg_png(filename = here::here("plots", 
                                        survey, 
                                        "cpue_maps", 
                                        species_group,
                                        paste0("CPUE_", 
                                               survey, "_",
                                               species_group, "_",
                                               paste(range(tail(survey_years, 4)), collapse = "_"), 
                                               ".png")),
                  units = "in",
                  width = 6,
                  height = 6,
                  res = fig_res)
    print(plot_last_4)
    dev.off()
    
  }
  
  
  # Format labels for GOA/AI
  if(region %in% c("goa", "ai")) {
    
    alt_round <- 0
    
    if(class(breaks) == "character") {
      
      breaks <- classInt::classIntervals(fit_cpue$CPUE_KGHA, n = 5, style = breaks)$brks
      
      # Setup rounding for small CPUE
      alt_round <- floor(-1*(min((log10(breaks)-2)[abs(breaks) > 0])))
      
      breaks <- c(-1, round(breaks, alt_round))
      
    }
    
    # Ensure breaks go to zero
    if(min(breaks) > 0) {
      breaks <- c(0, breaks)
    }
    
    if(min(breaks) == 0) {
      breaks <- c(-1, breaks)
    }
    
    # Ensure breaks span the full range
    if(max(breaks) < max(fit_cpue$CPUE_KGHA)){
      breaks[length(breaks)] <- max(fit_cpue$CPUE_KGHA) + 1
    }
    
    
    # Trim breaks to significant digits to account for differences in range among species
    dig_lab <- 7
    set_levels <- cut(fit_cpue$CPUE_KGHA, breaks, right = TRUE, dig.lab = dig_lab)
    
    if(alt_round > 0) {
      while(dig_lab > alt_round) { # Rounding for small CPUE
        dig_lab <- dig_lab - 1
        set_levels <- cut(fit_cpue$CPUE_KGHA, breaks, right = TRUE, dig.lab = dig_lab)
      }
    } else { # Rounding for large CPUE
      while(length(grep("\\.", set_levels)) > 0) {
        dig_lab <- dig_lab - 1
        set_levels <- cut(fit_cpue$CPUE_KGHA, breaks, right = TRUE, dig.lab = dig_lab)
      }
    }
    
    
    # Apply discrete scale
    fit_cpue$CPUE_DISC <- cut(fit_cpue$CPUE_KGHA, 
                              breaks, 
                              right = TRUE, 
                              dig.lab = dig_lab)
    
    # Which breaks need commas?
    sig_dig <- round(breaks[which(nchar(round(breaks)) >= 4)])
    
    # Drop brackets, add commas, create 'No catch' level to legend labels
    make_level_labels <- function(vec) {
      vec <- as.character(vec)
      vec[grep("-1", vec)] <- "No catch"
      vec <- sub("\\(", "\\>", vec)
      vec <- sub("\\,", "-", vec)
      vec <- sub("\\]", "", vec)
      
      if(length(sig_dig) > 0) {
        
        sig_dig_format <- trimws(format(sort(sig_dig, decreasing = TRUE), 
                                        nsmall=0, 
                                        big.mark=","))
        
        sig_dig_desc <- trimws(format(sort(sig_dig, decreasing = TRUE)))
        
        for(kk in 1:length(sig_dig)) {
          vec <- sub(pattern = sig_dig_desc[kk], replacement = sig_dig_format[kk], x = vec)
        }
      }
      return(vec)
    }
    
    # Assign level names to breaks for plotting
    fit_cpue$CPUE_DISC <- factor(make_level_labels(fit_cpue$CPUE_DISC),
                                    levels = make_level_labels(levels(set_levels)))
    
  }
  
  
  # Setup CPUE layer for GOA, AI, and slope surveys
  cpue_obs <- fit_cpue |>
    dplyr::mutate(POSITIVE_CATCH = CPUE_KGHA > 0) |>
    sf::st_as_sf(crs = "WGS84", coords = c("LONGITUDE", "LATITUDE")) |>
    sf::st_transform(crs = "EPSG:3338")
  
  # Last 4: AI ----
  
  if(region == "ai") {
    
    map_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")
    
    # Get plot boundaries and breaks
    map_layers_w <- akgfmaps::get_base_layers(select.region = "ai.west", set.crs = "EPSG:3338")
    map_layers_c <- akgfmaps::get_base_layers(select.region = "ai.central", set.crs = "EPSG:3338")
    map_layers_e <- akgfmaps::get_base_layers(select.region = "ai.east", set.crs = "EPSG:3338")
    
    limits_w <- map_layers_w$plot.boundary
    limits_c <- map_layers_c$plot.boundary
    limits_e <- map_layers_e$plot.boundary
    
    coord_breaks_w <- list(lat.breaks = map_layers_w$lat.breaks, 
                           lon.breaks = map_layers_w$lon.breaks)
    coord_breaks_c <- list(lat.breaks = map_layers_c$lat.breaks, 
                           lon.breaks = map_layers_c$lon.breaks)
    coord_breaks_e <- list(lat.breaks = map_layers_e$lat.breaks, 
                           lon.breaks = map_layers_e$lon.breaks)
    
    graticule_w <- map_layers_w$graticule
    graticule_c <- map_layers_c$graticule
    graticule_e <- map_layers_e$graticule
    
    rm(map_layers_w, map_layers_c, map_layers_e)

    n_levels <- length(unique(cpue_obs$CPUE_DISC))
    
    plot_last_4 <- ggplot() +
      geom_sf(data = map_layers$bathymetry, color = "grey80") +
      geom_sf(data = map_layers$akland, 
              fill = "grey30", 
              color = NA) +
      geom_sf(data = dplyr::filter(cpue_obs,
                                   YEAR %in% tail(survey_years, 4)) |>
                dplyr::arrange(CPUE_KGHA),
              mapping = aes(
                color = CPUE_DISC,
                size = CPUE_DISC,
                shape = POSITIVE_CATCH),
              show.legend = TRUE) +
      scale_color_manual(name = expression(CPUE~(kg%.%km^-2)),
                         values = c("grey30", viridis::viridis_pal(option = "turbo",
                                                                   direction = 1)(n_levels)[1:(n_levels-1)]),
                         drop = FALSE) +
      scale_shape_manual(guide = "none", 
                         values = c(4, 19, 19, 19, 19, 19),
                         drop = FALSE) +
      scale_size_discrete(guide = "none", 
                          range = c(2,4), 
                          drop = FALSE) +
      facet_wrap(~YEAR) +
      esrindex::theme_blue_strip() +
      theme(legend.title = element_text())
    
    
    ragg::agg_png(filename = here::here("plots", 
                                        survey, 
                                        "cpue_maps", 
                                        species_group,
                                        paste0("CPUE_", 
                                               "WAI_",
                                               species_group, "_",
                                               paste(range(tail(survey_years, 4)), collapse = "_"), 
                                               ".png")),
                  units = "in",
                  width = 6,
                  height = 6,
                  res = fig_res)
    print(
    plot_last_4 +
      geom_sf(data = graticule_w, alpha = 0.3, linewidth = 0.2) +
    scale_x_continuous(breaks = coord_breaks_w$lon.breaks,
                       limits = limits_w$x) +
      scale_y_continuous(breaks = coord_breaks_w$lat.breaks,
                         limits = limits_w$y)
    )
    dev.off()
    
    
    ragg::agg_png(filename = here::here("plots", 
                                        survey, 
                                        "cpue_maps", 
                                        species_group,
                                        paste0("CPUE_", 
                                               "CAI_",
                                               species_group, "_",
                                               paste(range(tail(survey_years, 4)), collapse = "_"), 
                                               ".png")),
                  units = "in",
                  width = 10,
                  height = 6,
                  res = fig_res)
    print(
    plot_last_4 +
      geom_sf(data = graticule_c, alpha = 0.3, linewidth = 0.2) +
      scale_x_continuous(breaks = coord_breaks_c$lon.breaks,
                         limits = limits_c$x) +
      scale_y_continuous(breaks = coord_breaks_c$lat.breaks,
                         limits = limits_c$y)
    )
    dev.off()
    
    
    ragg::agg_png(filename = here::here("plots", 
                                        survey, 
                                        "cpue_maps", 
                                        species_group,
                                        paste0("CPUE_", 
                                               "EAI_",
                                               species_group, "_",
                                               paste(range(tail(survey_years, 4)), collapse = "_"), 
                                               ".png")),
                  units = "in",
                  width = 10,
                  height = 5,
                  res = fig_res)
    print(
    plot_last_4 +
      geom_sf(data = graticule_e, alpha = 0.3, linewidth = 0.2) +
      scale_x_continuous(breaks = coord_breaks_e$lon.breaks,
                         limits = limits_e$x) +
      scale_y_continuous(breaks = coord_breaks_e$lat.breaks,
                         limits = limits_e$y)
    )
    dev.off()
    
  }
  
  
  # Last 4: GOA ----
  
  if(region == "goa") {
    
    map_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")
    
    # Get plot boundaries and breaks
    map_layers_w <- akgfmaps::get_base_layers(select.region = "goa.west", set.crs = "EPSG:3338")
    map_layers_e <- akgfmaps::get_base_layers(select.region = "goa.east", set.crs = "EPSG:3338")
    
    limits_w <- map_layers_w$plot.boundary
    limits_e <- map_layers_e$plot.boundary
    
    coord_breaks_w <- list(lat.breaks = map_layers_w$lat.breaks, 
                           lon.breaks = map_layers_w$lon.breaks)
    coord_breaks_e <- list(lat.breaks = map_layers_e$lat.breaks, 
                           lon.breaks = map_layers_e$lon.breaks)
    
    graticule_w <- map_layers_w$graticule
    graticule_e <- map_layers_e$graticule
    
    rm(map_layers_w, map_layers_e)
    
    n_levels <- length(unique(cpue_obs$CPUE_DISC))
    
    plot_last_4 <- ggplot() +
      geom_sf(data = map_layers$bathymetry, color = "grey80") +
      geom_sf(data = map_layers$akland, 
              fill = "grey70", 
              color = NA) +
      geom_sf(data = dplyr::filter(cpue_obs,
                                   YEAR %in% tail(survey_years, 4)) |>
                dplyr::arrange(CPUE_KGHA),
              mapping = aes(
                color = CPUE_DISC,
                size = CPUE_DISC,
                shape = POSITIVE_CATCH),
              show.legend = TRUE) +
      scale_color_manual(name = expression(CPUE~(kg%.%km^-2)),
                         values = c("grey30", viridis::viridis_pal(option = "turbo",
                                                                   direction = 1)(n_levels)[1:(n_levels-1)]),
                         drop = FALSE) +
      scale_shape_manual(guide = "none", 
                         values = c(4, 19, 19, 19, 19, 19),
                         drop = FALSE) +
      scale_size_discrete(guide = "none", 
                          range = c(2,4), 
                          drop = FALSE) +
      facet_wrap(~YEAR) +
      esrindex::theme_blue_strip() +
      theme(legend.title = element_text(),
            axis.text = element_text(size = 7.5)
            )
    
    
    ragg::agg_png(filename = here::here("plots", 
                                        survey, 
                                        "cpue_maps", 
                                        species_group,
                                        paste0("CPUE_", 
                                               "WGOA_",
                                               species_group, "_",
                                               paste(range(tail(survey_years, 4)), collapse = "_"), 
                                               ".png")),
                  units = "in",
                  width = 8,
                  height = 6,
                  res = fig_res)
    print(
      plot_last_4 +
        geom_sf(data = graticule_w, alpha = 0.3, linewidth = 0.2) +
        scale_x_continuous(breaks = coord_breaks_w$lon.breaks,
                           limits = limits_w$x) +
        scale_y_continuous(breaks = coord_breaks_w$lat.breaks,
                           limits = limits_w$y)
    )
    dev.off()
    
    ragg::agg_png(filename = here::here("plots", 
                                        survey, 
                                        "cpue_maps", 
                                        species_group,
                                        paste0("CPUE_", 
                                               "EGOA_",
                                               species_group, "_",
                                               paste(range(tail(survey_years, 4)), collapse = "_"), 
                                               ".png")),
                  units = "in",
                  width = 8,
                  height = 5,
                  res = fig_res)
    print(
      plot_last_4 +
        geom_sf(data = graticule_e, alpha = 0.3, linewidth = 0.2) +
        scale_x_continuous(breaks = coord_breaks_e$lon.breaks,
                           limits = limits_e$x) +
        scale_y_continuous(breaks = coord_breaks_e$lat.breaks,
                           limits = limits_e$y)
    )
    dev.off()
    
  }
  

  # Annual maps ----
  plot_years <- vector(mode = "list", length = length(survey_years))
  
  # Annual: EBS/SEBS/NBS ----
  if(region %in% c("ebs", "sebs", "nbs")) {
    
    plot_years_fig_width <- 6
    plot_years_fig_height <- round(0.5 + 6 * 
                                     abs(diff(cpue_stack$map_layers$plot.boundary$y)) / 
                                     abs(diff(cpue_stack$map_layers$plot.boundary$x)), 1)
    
    for(ii in 1:length(unique(survey_years))) {
      
      plot_years[[ii]] <- ggplot() +
        geom_sf(data = dplyr::filter(cpue_stack$extrapolation.stack,
                                     YEAR %in% survey_years[ii]),
                mapping = aes(fill = var1.pred),
                color = NA,
                show.legend = TRUE) +
        geom_sf(data = cpue_stack$map_layers$akland, 
                fill = "grey70", 
                color = "black") +
        geom_sf(data = cpue_stack$map_layers$survey.strata, fill = NA, color = "black") +
        geom_sf(data = cpue_stack$map_layers$graticule, alpha = 0.3, linewidth = 0.2) +
        scale_fill_manual(name = expression(CPUE~(kg%.%km^-2)),
                          values = c("white", viridis::viridis_pal(option = "mako", 
                                                                   direction = -1)(n_levels)[2:n_levels]),
                          drop = FALSE) +
        scale_x_continuous(breaks = cpue_stack$map_layers$lon.breaks,
                           limits = cpue_stack$map_layers$plot.boundary$x) +
        scale_y_continuous(breaks = cpue_stack$map_layers$lat.breaks,
                           limits = cpue_stack$map_layers$plot.boundary$y) +
        facet_wrap(~YEAR) +
        esrindex::theme_blue_strip() +
        theme(legend.key = element_rect(color = "black"), 
              legend.title = element_text())
      
      ragg::agg_png(filename = here::here("plots", 
                                          survey, 
                                          "cpue_maps", 
                                          species_group,
                                          paste0("CPUE_",
                                                 survey, "_",
                                                 species_group, "_",
                                                 survey_years[ii], 
                                                 ".png")),
                    units = "in",
                    width = plot_years_fig_width,
                    height = plot_years_fig_height,
                    res = fig_res)
      print(plot_years[[ii]])
      dev.off()
      
    }
    
  }
  
  
  # Annual: AI ----
  if(region == "ai") {
    
    plot_years_fig_width <- 6
    plot_years_fig_height <- 8
    
    for(ii in 1:length(unique(survey_years))) {
      
      plot_year_base <- ggplot() +
        geom_sf(data = map_layers$bathymetry, color = "grey80") +
        geom_sf(data = map_layers$akland, 
                fill = "grey30", 
                color = NA) +
        geom_sf(data = dplyr::filter(cpue_obs,
                                     YEAR %in% survey_years[ii]) |>
                  dplyr::arrange(CPUE_KGHA),
                mapping = aes(
                  color = CPUE_DISC,
                  size = CPUE_DISC,
                  shape = POSITIVE_CATCH),
                show.legend = TRUE) +
        scale_color_manual(name = expression(CPUE~(kg%.%km^-2)),
                           values = c("grey30", viridis::viridis_pal(option = "turbo",
                                                                    direction = 1)(n_levels)[1:(n_levels-1)]),
                           drop = FALSE) +
        scale_shape_manual(guide = "none", 
                           values = c(4, 19, 19, 19, 19, 19),
                           drop = FALSE) +
        scale_size_discrete(guide = "none", 
                            range = c(2,4), 
                            drop = FALSE) +
        esrindex::theme_blue_strip() +
        theme(legend.title = element_text())
      
      
      plot_years[[ii]] <- cowplot::plot_grid(
        plot_year_base +
          geom_sf(data = graticule_w, alpha = 0.3, linewidth = 0.2) +
          scale_x_continuous(breaks = coord_breaks_w$lon.breaks,
                             limits = limits_w$x) +
          scale_y_continuous(breaks = coord_breaks_w$lat.breaks,
                             limits = limits_w$y) + 
          facet_wrap(~"Western AI") +
          theme(legend.position = "right",
                plot.margin = unit(c(5,5,0,0), units = "mm")),
        plot_year_base +
          geom_sf(data = graticule_c, alpha = 0.3, linewidth = 0.2) +
          scale_x_continuous(breaks = coord_breaks_c$lon.breaks,
                             limits = limits_c$x) +
          scale_y_continuous(breaks = coord_breaks_c$lat.breaks,
                             limits = limits_c$y) +
          facet_wrap(~"Central AI") +
          theme(legend.position = "none",
                plot.margin = unit(c(0,5,-5,5), units = "mm")
                ),
        plot_year_base +
          geom_sf(data = graticule_e, alpha = 0.3, linewidth = 0.2) +
          scale_x_continuous(breaks = coord_breaks_e$lon.breaks,
                             limits = limits_e$x) +
          scale_y_continuous(breaks = coord_breaks_e$lat.breaks,
                             limits = limits_e$y) + 
          theme(legend.position = "none",
                plot.margin = unit(c(-5,5,-5,5), units = "mm")
                )+
          facet_wrap(~"Eastern AI"),
        labels = survey_years[ii],
        nrow = 3)
      
      ragg::agg_png(filename = here::here("plots", 
                                          survey, 
                                          "cpue_maps", 
                                          species_group,
                                          paste0("CPUE_",
                                                 survey, "_",
                                                 species_group, "_",
                                                 survey_years[ii], 
                                                 ".png")),
                    units = "in",
                    width = plot_years_fig_width,
                    height = plot_years_fig_height,
                    res = fig_res)
      print(plot_years[[ii]])
      dev.off()
      
    }
    
    
  }
  
  
  # Annual: GOA ----
  if(region == "goa") {
    
    plot_years_fig_width <- 6
    plot_years_fig_height <- 7
    
    for(ii in 1:length(unique(survey_years))) {
      
      plot_year_base <- ggplot() +
        geom_sf(data = map_layers$bathymetry, color = "grey80") +
        geom_sf(data = map_layers$akland, 
                fill = "grey70", 
                color = NA) +
        geom_sf(data = dplyr::filter(cpue_obs,
                                     YEAR %in% survey_years[ii]) |>
                  dplyr::arrange(CPUE_KGHA),
                mapping = aes(
                  color = CPUE_DISC,
                  size = CPUE_DISC,
                  shape = POSITIVE_CATCH),
                show.legend = TRUE) +
        scale_color_manual(name = expression(CPUE~(kg%.%km^-2)),
                           values = c("grey30", viridis::viridis_pal(option = "turbo",
                                                                     direction = 1)(n_levels)[1:(n_levels-1)]),
                           drop = FALSE) +
        scale_shape_manual(guide = "none", 
                           values = c(4, 19, 19, 19, 19, 19),
                           drop = FALSE) +
        scale_size_discrete(guide = "none", 
                            range = c(2,4), 
                            drop = FALSE) +
        esrindex::theme_blue_strip() +
        theme(legend.title = element_text(),
              axis.text = element_text(size = 7.5))
      
      
      plot_years[[ii]] <- cowplot::plot_grid(
        plot_year_base +
          geom_sf(data = graticule_w, alpha = 0.3, linewidth = 0.2) +
          scale_x_continuous(breaks = coord_breaks_w$lon.breaks,
                             limits = limits_w$x) +
          scale_y_continuous(breaks = coord_breaks_w$lat.breaks,
                             limits = limits_w$y) + 
          facet_wrap(~"Western GOA") +
          theme(legend.position = "none",
                plot.margin = unit(c(5,5,0,0), units = "mm")),
        plot_year_base +
          geom_sf(data = graticule_e, alpha = 0.3, linewidth = 0.2) +
          scale_x_continuous(breaks = coord_breaks_e$lon.breaks,
                             limits = limits_e$x) +
          scale_y_continuous(breaks = coord_breaks_e$lat.breaks,
                             limits = limits_e$y) + 
          theme(legend.position = "bottom",
                plot.margin = unit(c(-5,5,0,5), units = "mm")
          )+
          facet_wrap(~"Eastern GOA"),
        labels = survey_years[ii],
        rel_heights = c(0.4, 0.6),
        align = "v",
        nrow = 2)
      
      ragg::agg_png(filename = here::here("plots", 
                                          survey, 
                                          "cpue_maps", 
                                          species_group,
                                          paste0("CPUE_",
                                                 survey, "_",
                                                 species_group, "_",
                                                 survey_years[ii], 
                                                 ".png")),
                    units = "in",
                    width = plot_years_fig_width,
                    height = plot_years_fig_height,
                    res = fig_res)
      print(plot_years[[ii]])
      dev.off()
      
    }
    
  }
  
  # PDF with all of the annual plots
  pdf(file = here::here("plots", 
                        survey, 
                        "cpue_maps", 
                        species_group,
                        paste0("CPUE_",
                               survey, "_",
                               species_group, "_",
                               paste(range(survey_years), collapse = "_"), 
                               ".pdf")),
      width = plot_years_fig_width,
      height = plot_years_fig_height,
      onefile = TRUE)
  
  for(ii in 1:length(unique(survey_years))) {
    
    print(plot_years[[ii]])
    
  }
  
  dev.off()
  
  # Save CPUE layers
  if(exists("cpue_stack")) {

    sf::st_write(obj = cpue_stack$extrapolation.stack,
                 dsn = here::here("output", survey, paste0("CPUE_", 
                                                           survey, "_",
                                                           species_group, "_",
                                                           paste(range(survey_years), collapse = "_"), 
                                                           ".gpkg")),
                 append = FALSE)
  }
  
  if(exists("cpue_obs")) {
    sf::st_write(obj = cpue_obs,
                 dsn = here::here("output", survey, paste0("CPUE_OBS_", 
                                                           survey, "_",
                                                           species_group, "_",
                                                           paste(range(survey_years), collapse = "_"), 
                                                           ".gpkg")),
                 append = FALSE)
  }
  
}


