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

# Function to create density plots for rainfall and trips
create_rainfall_trips_density <- function(data) {
    # Create density plot for rainfall
    p_rainfall <- gg_histogram(
        plot_data = data,
        x_var = "total_rain",
        title = "Distribution of Daily Rainfall",
        subtitle = "Density distribution of total rainfall in São Paulo metropolitan area",
        x_label = "Total Rainfall (mm)",
        y_label = "Density",
        filename = "figures/rainfall_density.png",
        bar_color = "#46647B",
        show_density = TRUE
    )
    
    # Create density plot for trips
    p_trips <- gg_histogram(
        plot_data = data,
        x_var = "num_trips",
        title = "Distribution of Number of Trips",
        subtitle = "Density distribution of daily trips per person",
        x_label = "Number of Trips",
        y_label = "Density",
        filename = "figures/trips_density.png",
        bar_color = "#507867",
        show_density = TRUE
    )
    
    return(list(rainfall = p_rainfall, trips = p_trips))
}

# Function to create combined density plot
create_combined_density <- function(data) {
    # Prepare data for combined plot
    combined_data <- data %>%
        dplyr::select(total_rain, num_trips) %>%
        pivot_longer(cols = everything(), 
                    names_to = "variable", 
                    values_to = "value") %>%
        mutate(
            variable = case_when(
                variable == "total_rain" ~ "Total Rainfall (mm)",
                variable == "num_trips" ~ "Number of Trips",
                TRUE ~ variable
            )
        )
    
    # Create combined density plot
    p <- ggplot(combined_data, aes(x = value, fill = variable)) +
        geom_density(alpha = 0.7, color = NA) +
        facet_wrap(~variable, scales = "free", ncol = 1) +
        labs(
            title = "Distribution Comparison: Rainfall vs Number of Trips",
            subtitle = "Density distributions showing the relationship between weather and mobility",
            x = "Value",
            y = "Density"
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
            strip.text = element_text(size = 11, color = "#2c3e50", face = "bold"),
            legend.position = "none",
            
            # Clean background
            panel.grid.major = element_line(color = "#f8f9fa", size = 0.3),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0.02, 0.02))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        scale_fill_manual(values = c("Total Rainfall (mm)" = "#46647B", 
                                   "Number of Trips" = "#507867"))
    
    # Save the plot
    ggsave("figures/combined_density.png", plot = p, width = 12, height = 10, dpi = 300, bg = "white")
    
    return(p)
}

// ...existing code...