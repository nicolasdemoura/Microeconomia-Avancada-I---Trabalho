###############################################################################
# Topic: Microeconomia Avançada I | Economia do Trabalho
# Goal: Functions for plotting and visualizing data for the project
# Keywords: Labor Economics, Labor Supply, Rainfall, Absenteeism, Brazil
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-06-11
###############################################################################

###############################################################################
# Style Plotting Functions
###############################################################################

# Function to create style bar charts
gg_bar <- function(plot_data, x_var, y_var, title, subtitle = NULL,
                        x_label = "", y_label = "", filename, width = 12, height = 8,
                        bar_color = "#CC0000", show_values = TRUE, value_size = 4,
                        reverse_y = FALSE, y_arrow_label = NULL) {
    
    # Base plot
    p <- ggplot(plot_data, aes_string(x = x_var, y = y_var))
    
    # Add bars without outlines
    p <- p + geom_col(fill = bar_color, color = NA, width = 0.7)
    
    # Add value labels on top of bars if requested
    if (show_values) {
        p <- p + geom_text(aes_string(label = y_var), 
                          vjust = if(reverse_y) 1.5 else -0.5, 
                          size = value_size, 
                          color = "#2c3e50", 
                          fontface = "bold")
    }
    
    # Apply style
    p <- p + 
        labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label
        ) +
        theme_void() +
        theme(
            # Text styling
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                    margin = margin(b = 10), hjust = 0),
            plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                       margin = margin(b = 20), hjust = 0),
            axis.title.x = element_text(size = 11, color = "#2c3e50", face = "bold",
                                      margin = margin(t = 10)),
            axis.title.y = element_text(size = 11, color = "#2c3e50", face = "bold",
                                      angle = 90, margin = margin(r = 10)),
            axis.text.x = element_text(size = 10, color = "#2c3e50", 
                                     margin = margin(t = 5)),
            axis.text.y = element_blank(),
            
            # Remove all gridlines and backgrounds
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        )
    
    # Reverse y-axis if requested
    if (reverse_y) {
        p <- p + scale_y_reverse(expand = expansion(mult = c(0.1, 0.1)))
    } else {
        p <- p + scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    }
    
    # Add custom y-axis label with arrow if provided
    if (!is.null(y_arrow_label)) {
        p <- p + labs(y = y_arrow_label)
    }
    
    # Save the plot
    ggsave(filename, plot = p, width = width, height = height, dpi = 300, bg = "white")
    
    return(p)
}

# Function to create style histograms
gg_histogram <- function(plot_data, x_var, title, subtitle = NULL,
                             x_label = "", y_label = "Frequency", filename, 
                             width = 12, height = 8, bar_color = "#CC0000",
                             bins = 30, show_density = FALSE) {
    
    # Base plot
    p <- ggplot(plot_data, aes_string(x = x_var))
    
    # Add histogram without outlines
    if (show_density) {
        p <- p + geom_density(fill = bar_color, color = NA, alpha = 0.7)
        y_label <- "Density"
    } else {
        p <- p + geom_histogram(fill = bar_color, color = NA, bins = bins)
    }
    
    # Apply style
    p <- p + 
        labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label
        ) +
        theme_minimal() +
        theme(
            # Text styling
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                    margin = margin(b = 10)),
            plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                       margin = margin(b = 20)),
            axis.title = element_text(size = 11, color = "#2c3e50", face = "bold"),
            axis.text = element_text(size = 10, color = "#2c3e50"),
            
            # Clean background
            panel.grid.major = element_line(color = "#f8f9fa", size = 0.3),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0.02, 0.02))) +
        scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05)))
    
    # Save the plot
    ggsave(filename, plot = p, width = width, height = height, dpi = 300, bg = "white")
    
    return(p)
}

# Function to create style stacked area charts
gg_area <- function(plot_data, x_var, y_var, fill_var, title, subtitle = NULL,
                        x_label = "", y_label = "", filename, width = 16, height = 9,
                        color_palette = NULL, show_legend = TRUE) {
    
    # Define default color palette
    default_palette <- c("#CC0000", "#46647B", "#507867", "#973B74", "#CAAF49", 
                        "#2E86AB", "#A23B72", "#F24236", "#F6AE2D", "#2F9599")
    
    # Set color palette
    if (is.null(color_palette)) {
        fill_colors <- default_palette
    } else {
        fill_colors <- color_palette
    }
    
    # Base plot
    p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, fill = fill_var))
    
    # Add stacked area without outlines
    p <- p + geom_area(color = NA, alpha = 0.8, position = "stack")
    
    # Apply style
    p <- p + 
        labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label
        ) +
        theme_minimal() +
        theme(
            # Text styling
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                    margin = margin(b = 10)),
            plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                       margin = margin(b = 20)),
            axis.title = element_text(size = 11, color = "#2c3e50", face = "bold"),
            axis.text = element_text(size = 10, color = "#2c3e50"),
            legend.title = element_blank(),
            legend.text = element_text(size = 10, color = "#2c3e50"),
            legend.position = if(show_legend) "right" else "none",
            
            # Clean background
            panel.grid.major = element_line(color = "#f8f9fa", size = 0.3),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
        scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
        scale_fill_manual(values = fill_colors)
    
    # Save the plot
    ggsave(filename, plot = p, width = width, height = height, dpi = 300, bg = "white")
    
    return(p)
}

# Function to create RMSP map with Voronoi polygons
gg_rmsp_voronoi <- function(RMSP, stations, voronoi_polygons, title = "", subtitle = NULL,
                           filename = "figures/rmsp_voronoi.png", width = 12, height = 10,
                           rmsp_color = "#f8f9fa", voronoi_color = "#46647B", 
                           station_color = "#CC0000", centroid_color = "#2c3e50",
                           show_stations = TRUE, show_centroids = TRUE, 
                           show_voronoi = TRUE, show_rmsp_fill = TRUE) {
    
    # Convert terra objects to sf for ggplot compatibility
    RMSP_sf <- st_as_sf(RMSP)
    stations_sf <- st_as_sf(stations)
    voronoi_sf <- st_as_sf(voronoi_polygons)
    
    # Calculate centroids
    centroids_sf <- st_centroid(RMSP_sf)
    
    # Base plot
    p <- ggplot()
    
    # Add RMSP polygons with fill if requested
    if (show_rmsp_fill) {
        p <- p + geom_sf(data = RMSP_sf, fill = rmsp_color, color = "white", 
                        size = 0.5, alpha = 0.7)
    } else {
        p <- p + geom_sf(data = RMSP_sf, fill = NA, color = "#2c3e50", 
                        size = 0.5)
    }
    
    # Add Voronoi polygons if requested
    if (show_voronoi) {
        p <- p + geom_sf(data = voronoi_sf, fill = NA, color = voronoi_color, 
                        size = 0.8, alpha = 0.8)
    }
    
    # Add stations if requested
    if (show_stations) {
        p <- p + geom_sf(data = stations_sf, color = station_color, 
                        size = 2, shape = 16)
    }
    
    # Add centroids if requested
    if (show_centroids) {
        p <- p + geom_sf(data = centroids_sf, color = centroid_color, 
                        size = 1.5, shape = 3, stroke = 1.2)
    }
    
    # Apply clean styling
    p <- p + 
        labs(
            title = title,
            subtitle = subtitle
        ) +
        theme_void() +
        theme(
            # Text styling
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                    margin = margin(b = 10), hjust = 0),
            plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                       margin = margin(b = 20), hjust = 0),
            
            # Clean background
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Remove axes and gridlines
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        ) +
        coord_sf(expand = FALSE)
    
    # Save the plot
    ggsave(filename, plot = p, width = width, height = height, dpi = 300, bg = "white")
    
    return(p)
}

# Function to create RMSP map with closest station assignments
gg_rmsp_closest_stations <- function(RMSP, stations, title = "", subtitle = NULL,
                                    filename = "figures/rmsp_closest_stations.png", 
                                    width = 12, height = 10,
                                    station_color = "#CC0000", centroid_color = "#2c3e50",
                                    show_stations = TRUE, show_centroids = TRUE,
                                    color_palette = NULL) {
    
    # Convert terra objects to sf for ggplot compatibility
    RMSP_sf <- st_as_sf(RMSP)
    stations_sf <- st_as_sf(stations)
    
    # Calculate centroids
    centroids_sf <- st_centroid(RMSP_sf)
    
    # Define color palette for closest stations
    if (is.null(color_palette)) {
        n_stations <- length(unique(RMSP$closest_station))
        color_palette <- colorRampPalette(c("#CC0000", "#46647B", "#507867", 
                                           "#973B74", "#CAAF49", "#2E86AB", 
                                           "#A23B72", "#F24236", "#F6AE2D", 
                                           "#2F9599"))(n_stations)
    }
    
    # Base plot
    p <- ggplot()
    
    # Add RMSP polygons colored by closest station
    p <- p + geom_sf(data = RMSP_sf, aes(fill = closest_station), 
                    color = "white", size = 0.5, alpha = 0.8)
    
    # Add stations if requested
    if (show_stations) {
        p <- p + geom_sf(data = stations_sf, color = station_color, 
                        size = 2, shape = 16)
    }
    
    # Add centroids if requested
    if (show_centroids) {
        p <- p + geom_sf(data = centroids_sf, color = centroid_color, 
                        size = 1.5, shape = 3, stroke = 1.2)
    }
    
    # Apply clean styling
    p <- p + 
        labs(
            title = title,
            subtitle = subtitle
        ) +
        theme_void() +
        theme(
            # Text styling
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                    margin = margin(b = 10), hjust = 0),
            plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                       margin = margin(b = 20), hjust = 0),
            
            # Legend styling
            legend.title = element_blank(),
            legend.text = element_text(size = 9, color = "#2c3e50"),
            legend.position = "right",
            legend.key.size = unit(0.5, "cm"),
            
            # Clean background
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Remove axes and gridlines
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        ) +
        coord_sf(expand = FALSE) +
        scale_fill_manual(values = color_palette, name = "Closest Station")
    
    # Save the plot
    ggsave(filename, plot = p, width = width, height = height, dpi = 300, bg = "white")
    
    return(p)
}

# Function to create simple RMSP base map
gg_rmsp_base <- function(RMSP, title = "", subtitle = NULL,
                        filename = "figures/rmsp_base.png", width = 12, height = 10,
                        fill_color = "#f8f9fa", border_color = "white") {
    
    # Convert terra objects to sf for ggplot compatibility
    RMSP_sf <- st_as_sf(RMSP)
    
    # Base plot
    p <- ggplot()
    
    # Add RMSP polygons
    p <- p + geom_sf(data = RMSP_sf, fill = fill_color, color = border_color, 
                    size = 0.5, alpha = 0.8)
    
    # Apply clean styling
    p <- p + 
        labs(
            title = title,
            subtitle = subtitle
        ) +
        theme_void() +
        theme(
            # Text styling
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                    margin = margin(b = 10), hjust = 0),
            plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                       margin = margin(b = 20), hjust = 0),
            
            # Clean background
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Remove axes and gridlines
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        ) +
        coord_sf(expand = FALSE)
    
    # Save the plot
    ggsave(filename, plot = p, width = width, height = height, dpi = 300, bg = "white")
    
    return(p)
}
