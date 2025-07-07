###############################################################################
# Topic: Microeconomia Avançada I | Economia do Trabalho
# Goal: Functions for running regression analysis and other statistical tests
# Keywords: Labor Economics, Labor Supply, Rainfall, Absenteeism, Brazil
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-07-07
###############################################################################

###############################################################################
# Regression Analysis Functions
###############################################################################

# Function to run main regression analysis
run_main_analysis <- function(data) {
    # Create rainfall dummy
    data <- data %>%
        mutate(
            dm_rain = ifelse(total_rain > 0, 1, 0),
            dm_employed = ifelse(cat_actv %in% c("1", "2"), 1, 0)
        )
    
    # Panel 1: Employment probability (restricted to employed/unemployed)
    panel1_data <- data %>%
        filter(cat_actv %in% c("1", "2")) %>%
        mutate(
            dm_worked = ifelse(dm_worked == 1, 1, 0),
            num_age = as.numeric(num_age),
            dm_male = as.numeric(dm_male),
            cat_educ = as.factor(cat_educ),
            cat_actv = as.factor(cat_actv),
            cat_ocup = as.factor(cat_ocup),
            cat_setr = as.factor(cat_setr),
            cat_vinc = as.factor(cat_vinc),
            cat_zone = as.factor(cat_zone)
        )
    
    # Panel 2: Duration (restricted to employed who worked)
    panel2_data <- data %>%
        filter(cat_actv %in% c("1", "2"), dm_worked == 1) %>%
        mutate(
            num_duration = as.numeric(num_duration),
            num_age = as.numeric(num_age),
            dm_male = as.numeric(dm_male),
            cat_educ = as.factor(cat_educ),
            cat_actv = as.factor(cat_actv),
            cat_ocup = as.factor(cat_ocup),
            cat_setr = as.factor(cat_setr),
            cat_vinc = as.factor(cat_vinc),
            cat_zone = as.factor(cat_zone)
        )
    
    # Panel 1 Models - Work Probability
    model1_p1 <- feols(dm_worked ~ dm_rain, data = panel1_data)
    model2_p1 <- feols(dm_worked ~ total_rain, data = panel1_data)
    model3_p1 <- feols(dm_worked ~ dm_rain + total_rain, data = panel1_data)
    model4_p1 <- feols(dm_worked ~ dm_rain + total_rain + num_age + dm_male + cat_educ + 
                       cat_actv + cat_ocup + cat_setr + cat_vinc, data = panel1_data)
    model5_p1 <- feols(dm_worked ~ dm_rain + total_rain + num_age + dm_male + cat_educ + 
                       cat_actv + cat_ocup + cat_setr + cat_vinc | cat_zone, data = panel1_data)
    
    # Panel 2 Models - Work Duration
    model1_p2 <- feols(num_duration ~ dm_rain, data = panel2_data)
    model2_p2 <- feols(num_duration ~ total_rain, data = panel2_data)
    model3_p2 <- feols(num_duration ~ dm_rain + total_rain, data = panel2_data)
    model4_p2 <- feols(num_duration ~ dm_rain + total_rain + num_age + dm_male + cat_educ + 
                       cat_actv + cat_ocup + cat_setr + cat_vinc, data = panel2_data)
    model5_p2 <- feols(num_duration ~ dm_rain + total_rain + num_age + dm_male + cat_educ + 
                       cat_actv + cat_ocup + cat_setr + cat_vinc | cat_zone, data = panel2_data)
    
    # Return list of models
    return(list(
        panel1 = list(
            model1 = model1_p1,
            model2 = model2_p1,
            model3 = model3_p1,
            model4 = model4_p1,
            model5 = model5_p1
        ),
        panel2 = list(
            model1 = model1_p2,
            model2 = model2_p2,
            model3 = model3_p2,
            model4 = model4_p2,
            model5 = model5_p2
        ),
        data = list(
            panel1 = panel1_data,
            panel2 = panel2_data
        )
    ))
}

# Function to create regression table
create_regression_table <- function(models) {
    # Extract coefficients of interest
    extract_coef <- function(model) {
        coef_summary <- summary(model)$coefficients
        
        # Initialize results
        dm_rain_coef <- NA
        dm_rain_se <- NA
        total_rain_coef <- NA
        total_rain_se <- NA
        
        # Extract dm_rain coefficient if exists
        if ("dm_rain" %in% rownames(coef_summary)) {
            dm_rain_coef <- coef_summary["dm_rain", "Estimate"]
            dm_rain_se <- coef_summary["dm_rain", "Std. Error"]
        }
        
        # Extract total_rain coefficient if exists
        if ("total_rain" %in% rownames(coef_summary)) {
            total_rain_coef <- coef_summary["total_rain", "Estimate"]
            total_rain_se <- coef_summary["total_rain", "Std. Error"]
        }
        
        return(list(
            dm_rain_coef = dm_rain_coef,
            dm_rain_se = dm_rain_se,
            total_rain_coef = total_rain_coef,
            total_rain_se = total_rain_se,
            n_obs = nobs(model),
            r_squared = summary(model)$r.squared
        ))
    }
    
    # Extract results for all models
    panel1_results <- lapply(models$panel1, extract_coef)
    panel2_results <- lapply(models$panel2, extract_coef)
    
    # Create table using modelsummary
    library(modelsummary)
    
    # Create custom coefficient mapping
    coef_map <- c(
        "dm_rain" = "Rain Dummy (>0mm)",
        "total_rain" = "Total Rainfall (mm)"
    )
    
    # Create table for both panels
    table_panel1 <- modelsummary(
        models$panel1,
        coef_map = coef_map,
        statistic = "std.error",
        fmt = 3,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
        title = "Panel A: Effect of Rainfall on Work Probability",
        output = "data.frame"
    )
    
    table_panel2 <- modelsummary(
        models$panel2,
        coef_map = coef_map,
        statistic = "std.error",
        fmt = 3,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
        title = "Panel B: Effect of Rainfall on Work Duration",
        output = "data.frame"
    )
    
    return(list(
        panel1 = table_panel1,
        panel2 = table_panel2
    ))
}

# Function to export regression table to LaTeX
export_regression_table <- function(models, filename = "output/regression_results.tex") {
    # Create directory if it doesn't exist
    if (!dir.exists("output")) {
        dir.create("output", recursive = TRUE)
    }
    
    # Create custom coefficient mapping
    coef_map <- c(
        "dm_rain" = "Rain Dummy (>0mm)",
        "total_rain" = "Total Rainfall (mm)"
    )
    
    # Create combined table
    combined_table <- modelsummary(
        list(
            "Panel A: Work Probability" = models$panel1,
            "Panel B: Work Duration (minutes)" = models$panel2
        ),
        coef_map = coef_map,
        statistic = "std.error",
        fmt = 3,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
        title = "Effect of Rainfall on Labor Supply",
        notes = c(
            "Notes: This table presents the effects of rainfall on labor supply outcomes.",
            "Panel A shows the effect on work probability (0/1) for employed individuals.",
            "Panel B shows the effect on work duration (minutes) for those who worked.",
            "Model 1: Rain dummy only. Model 2: Total rainfall only. Model 3: Both variables.",
            "Model 4: Both variables + individual controls. Model 5: Model 4 + zone fixed effects.",
            "Individual controls include age, gender, education, activity, occupation, sector, and employment type.",
            "Standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01"
        ),
        output = filename
    )
    
    message(paste("Regression table exported to:", filename))
    return(combined_table)
}

# Function to create descriptive statistics table
create_descriptive_stats <- function(data) {
    # Prepare data for descriptive statistics
    desc_data <- data %>%
        filter(cat_actv %in% c("1", "2")) %>%
        mutate(
            dm_rain = ifelse(total_rain > 0, 1, 0),
            dm_employed = ifelse(cat_actv %in% c("1", "2"), 1, 0)
        ) %>%
        dplyr::select(
            dm_worked,
            num_duration,
            total_rain,
            dm_rain,
            num_age,
            dm_male,
            num_trips
        )
    
    # Create descriptive statistics
    desc_stats <- desc_data %>%
        summarise(
            across(everything(), list(
                mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE),
                min = ~ min(.x, na.rm = TRUE),
                max = ~ max(.x, na.rm = TRUE),
                n = ~ sum(!is.na(.x))
            ))
        )
    
    return(desc_stats)
}

# Function to export descriptive statistics to LaTeX
export_descriptive_stats <- function(data, filename = "output/descriptive_stats.tex") {
    # Create directory if it doesn't exist
    if (!dir.exists("output")) {
        dir.create("output", recursive = TRUE)
    }
    
    desc_stats <- create_descriptive_stats(data)
    
    # Create table using modelsummary
    desc_table <- modelsummary(
        desc_stats,
        title = "Descriptive Statistics",
        notes = c(
            "Notes: This table presents descriptive statistics for the main variables used in the analysis.",
            "Sample restricted to employed individuals (cat_actv in 1,2).",
            "Work probability is a binary indicator for whether the individual worked on the survey day.",
            "Duration is measured in minutes for those who worked.",
            "Rainfall variables are measured in millimeters."
        ),
        output = filename
    )
    
    message(paste("Descriptive statistics table exported to:", filename))
    return(desc_table)
}

