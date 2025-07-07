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
    # Create rainfall dummy and prepare data
    data <- data %>%
        mutate(
            dm_rain = ifelse(total_rain > 0, 1, 0),
            dm_employed = ifelse(cat_actv %in% c("1", "2"), 1, 0),
            num_age = as.numeric(num_age),
            dm_male = as.numeric(dm_male),
            total_rain = as.numeric(total_rain),
            dm_worked = as.numeric(dm_worked),
            num_duration = as.numeric(num_duration)
        ) %>%
        # Convert categorical variables to factors
        mutate(
            cat_educ = as.factor(cat_educ),
            cat_actv = as.factor(cat_actv),
            cat_ocup = as.factor(cat_ocup),
            cat_setr = as.factor(cat_setr),
            cat_vinc = as.factor(cat_vinc),
            cat_zone = as.factor(cat_zone)
        )
    
    # Panel 1: Work probability (restricted to employed/unemployed)
    panel1_data <- data %>%
        filter(cat_actv %in% c("1", "2")) %>%
        filter(!is.na(dm_worked), !is.na(total_rain), !is.na(dm_rain))
    
    # Panel 2: Duration (restricted to employed who worked)
    panel2_data <- data %>%
        filter(cat_actv %in% c("1", "2"), dm_worked == 1) %>%
        filter(!is.na(num_duration), !is.na(total_rain), !is.na(dm_rain))
    
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
    
    # Return named list for modelsummary
    return(list(
        panel1_models = list(
            "Model 1" = model1_p1,
            "Model 2" = model2_p1,
            "Model 3" = model3_p1,
            "Model 4" = model4_p1,
            "Model 5" = model5_p1
        ),
        panel2_models = list(
            "Model 1" = model1_p2,
            "Model 2" = model2_p2,
            "Model 3" = model3_p2,
            "Model 4" = model4_p2,
            "Model 5" = model5_p2
        ),
        data = list(
            panel1 = panel1_data,
            panel2 = panel2_data
        )
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
    
    # Create combined model list for modelsummary
    all_models <- c(models$panel1_models, models$panel2_models)
    
    # Create panel labels
    panel_labels <- c(
        rep("Panel A: Work Probability", length(models$panel1_models)),
        rep("Panel B: Work Duration (minutes)", length(models$panel2_models))
    )
    
    # Create the table
    regression_table <- modelsummary(
        all_models,
        coef_map = coef_map,
        statistic = "std.error",
        fmt = 4,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
        title = "Effect of Rainfall on Labor Supply",
        notes = list(
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
    return(regression_table)
}

# Function to create descriptive statistics table
create_descriptive_stats <- function(data) {
    # Prepare data for descriptive statistics
    desc_data <- data %>%
        filter(cat_actv %in% c("1", "2")) %>%
        mutate(
            dm_rain = ifelse(total_rain > 0, 1, 0)
        ) %>%
        dplyr::select(
            dm_worked,
            num_duration,
            total_rain,
            dm_rain,
            num_age,
            dm_male,
            num_trips
        ) %>%
        filter_all(all_vars(!is.na(.)))
    
    # Create summary statistics
    desc_stats <- desc_data %>%
        summarise(
            across(everything(), list(
                N = ~ sum(!is.na(.x)),
                Mean = ~ mean(.x, na.rm = TRUE),
                SD = ~ sd(.x, na.rm = TRUE),
                Min = ~ min(.x, na.rm = TRUE),
                Max = ~ max(.x, na.rm = TRUE)
            ), .names = "{.col}_{.fn}")
        )
    
    return(desc_stats)
}

# Function to export descriptive statistics to LaTeX
export_descriptive_stats <- function(data, filename = "output/descriptive_stats.tex") {
    # Create directory if it doesn't exist
    if (!dir.exists("output")) {
        dir.create("output", recursive = TRUE)
    }
    
    # Prepare data for descriptive statistics
    desc_data <- data %>%
        filter(cat_actv %in% c("1", "2")) %>%
        mutate(
            "Work Probability" = as.numeric(dm_worked),
            "Work Duration (minutes)" = as.numeric(num_duration),
            "Total Rainfall (mm)" = as.numeric(total_rain),
            "Rain Dummy" = ifelse(total_rain > 0, 1, 0),
            "Age" = as.numeric(num_age),
            "Male" = as.numeric(dm_male),
            "Number of Trips" = as.numeric(num_trips)
        ) %>%
        dplyr::select(
            `Work Probability`,
            `Work Duration (minutes)`,
            `Total Rainfall (mm)`,
            `Rain Dummy`,
            Age,
            Male,
            `Number of Trips`
        )
    
    # Create descriptive statistics table
    desc_table <- modelsummary(
        desc_data,
        title = "Descriptive Statistics",
        notes = list(
            "Notes: This table presents descriptive statistics for the main variables used in the analysis.",
            "Sample restricted to employed individuals (cat_actv in 1,2).",
            "Work probability is a binary indicator for whether the individual worked on the survey day.",
            "Duration is measured in minutes for those who worked.",
            "Rainfall variables are measured in millimeters.",
            "Rain dummy equals 1 if total rainfall > 0mm, 0 otherwise."
        ),
        output = filename
    )
    
    message(paste("Descriptive statistics table exported to:", filename))
    return(desc_table)
}

# Function to display key results in console
display_results <- function(models) {
    # Display Panel 1 results (Work Probability)
    cat("Panel A: Effect on Work Probability\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    
    for(i in 1:5) {
        model_name <- paste("Model", i)
        model <- models$panel1_models[[i]]
        coef_summary <- summary(model)$coefficients
        
        if("dm_rain" %in% rownames(coef_summary)) {
            dm_rain_coef <- round(coef_summary["dm_rain", "Estimate"], 4)
            dm_rain_pval <- round(coef_summary["dm_rain", "Pr(>|t|)"], 4)
            cat(paste(model_name, "- Rain Dummy:", dm_rain_coef, "(p-value:", dm_rain_pval, ")\n"))
        }
        
        if("total_rain" %in% rownames(coef_summary)) {
            total_rain_coef <- round(coef_summary["total_rain", "Estimate"], 6)
            total_rain_pval <- round(coef_summary["total_rain", "Pr(>|t|)"], 4)
            cat(paste(model_name, "- Total Rain:", total_rain_coef, "(p-value:", total_rain_pval, ")\n"))
        }
    }
    
    # Display Panel 2 results (Work Duration)
    cat("\nPanel B: Effect on Work Duration\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    
    for(i in 1:5) {
        model_name <- paste("Model", i)
        model <- models$panel2_models[[i]]
        coef_summary <- summary(model)$coefficients
        
        if("dm_rain" %in% rownames(coef_summary)) {
            dm_rain_coef <- round(coef_summary["dm_rain", "Estimate"], 4)
            dm_rain_pval <- round(coef_summary["dm_rain", "Pr(>|t|)"], 4)
            cat(paste(model_name, "- Rain Dummy:", dm_rain_coef, "(p-value:", dm_rain_pval, ")\n"))
        }
        
        if("total_rain" %in% rownames(coef_summary)) {
            total_rain_coef <- round(coef_summary["total_rain", "Estimate"], 6)
            total_rain_pval <- round(coef_summary["total_rain", "Pr(>|t|)"], 4)
            cat(paste(model_name, "- Total Rain:", total_rain_coef, "(p-value:", total_rain_pval, ")\n"))
        }
    }
}
