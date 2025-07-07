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
    
    # Create Panel A table using modelsummary
    panel_a_table <- modelsummary(
        models$panel1_models,
        vcov = "HC1",
        coef_map = c("dm_rain" = "Rain Dummy (>0mm)", 
                     "total_rain" = "Total Rainfall (mm)"),
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|R2|Adj",
        title = "Panel A: Effect of Rainfall on Work Probability",
        notes = c("\\textbf{Sample:} Individuals with employment activity status (cat\\_actv) equal to 1 (employed) or 2 (unemployed).",
                 "\\textbf{Dependent Variable:} Binary indicator equal to 1 if individual worked on survey day, 0 otherwise.",
                 "\\textbf{Models:} (1) Rain dummy only; (2) Total rainfall only; (3) Both rainfall variables;",
                 "\\phantom{\\textbf{Models:}} (4) Both rainfall variables + individual controls; (5) Model 4 + zone fixed effects.",
                 "\\textbf{Individual Controls:} Age (num\\_age), gender dummy (dm\\_male), education level (cat\\_educ),",
                 "\\phantom{\\textbf{Individual Controls:}} activity status (cat\\_actv), occupation (cat\\_ocup), economic sector (cat\\_setr), employment type (cat\\_vinc).",
                 "\\textbf{Zone Fixed Effects:} Municipality/zone dummies (cat\\_zone) to control for time-invariant spatial characteristics.",
                 "\\textbf{Standard Errors:} Heteroskedasticity-robust (HC1) standard errors in parentheses.",
                 "\\textbf{Significance:} * p$<$0.1, ** p$<$0.05, *** p$<$0.01"),
        output = "output/panel_a.tex"
    )
    
    # Create Panel B table using modelsummary
    panel_b_table <- modelsummary(
        models$panel2_models,
        vcov = "HC1",
        coef_map = c("dm_rain" = "Rain Dummy (>0mm)", 
                     "total_rain" = "Total Rainfall (mm)"),
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|R2|Adj",
        title = "Panel B: Effect of Rainfall on Work Duration",
        notes = c("\\textbf{Sample:} Individuals with employment activity status (cat\\_actv) equal to 1 or 2 who worked on survey day (dm\\_worked = 1).",
                 "\\textbf{Dependent Variable:} Work duration in minutes (num\\_duration) for those who worked.",
                 "\\textbf{Models:} (1) Rain dummy only; (2) Total rainfall only; (3) Both rainfall variables;",
                 "\\phantom{\\textbf{Models:}} (4) Both rainfall variables + individual controls; (5) Model 4 + zone fixed effects.",
                 "\\textbf{Individual Controls:} Age (num\\_age), gender dummy (dm\\_male), education level (cat\\_educ),",
                 "\\phantom{\\textbf{Individual Controls:}} activity status (cat\\_actv), occupation (cat\\_ocup), economic sector (cat\\_setr), employment type (cat\\_vinc).",
                 "\\textbf{Zone Fixed Effects:} Municipality/zone dummies (cat\\_zone) to control for time-invariant spatial characteristics.",
                 "\\textbf{Standard Errors:} Heteroskedasticity-robust (HC1) standard errors in parentheses.",
                 "\\textbf{Significance:} * p$<$0.1, ** p$<$0.05, *** p$<$0.01"),
        output = "output/panel_b.tex"
    )
    
    # Read the generated files and modify them to add control rows
    panel_a_content <- readLines("output/panel_a.tex")
    panel_b_content <- readLines("output/panel_b.tex")
    
    # Function to add control rows to table
    add_control_rows <- function(table_content, models_list) {
        # Find the line with \bottomrule
        bottomrule_idx <- which(grepl("\\\\bottomrule", table_content))
        if (length(bottomrule_idx) > 0) {
            # Insert control rows before bottomrule
            control_rows <- c(
                "\\midrule",
                paste("Individual Controls &", 
                      paste(c("No", "No", "No", "Yes", "Yes"), collapse = " & "), 
                      "\\\\"),
                paste("Zone Fixed Effects &", 
                      paste(c("No", "No", "No", "No", "Yes"), collapse = " & "), 
                      "\\\\")
            )
            
            # Insert the control rows
            table_content <- c(
                table_content[1:(bottomrule_idx - 1)],
                control_rows,
                table_content[bottomrule_idx:length(table_content)]
            )
        }
        return(table_content)
    }
    
    # Add control rows to both panels
    panel_a_modified <- add_control_rows(panel_a_content, models$panel1_models)
    panel_b_modified <- add_control_rows(panel_b_content, models$panel2_models)
    
    # Write modified files
    writeLines(panel_a_modified, "output/panel_a.tex")
    writeLines(panel_b_modified, "output/panel_b.tex")
    
    message(paste("Panel A table exported to: output/panel_a.tex"))
    message(paste("Panel B table exported to: output/panel_b.tex"))
    
    return(list(panel_a = panel_a_table, panel_b = panel_b_table))
}

# Function to create descriptive statistics table
create_descriptive_stats <- function(data) {
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
    
    return(desc_data)
}

# Function to export descriptive statistics to LaTeX
export_descriptive_stats <- function(data, filename = "output/descriptive_stats.tex") {
    # Create directory if it doesn't exist
    if (!dir.exists("output")) {
        dir.create("output", recursive = TRUE)
    }
    
    desc_data <- create_descriptive_stats(data)
    
    # Create descriptive statistics table
    desc_table <- modelsummary(
        desc_data,
        title = "Descriptive Statistics",
        notes = c("\\textbf{Sample Description:} This table presents descriptive statistics for the main variables used in the analysis.",
                 "\\textbf{Sample Restriction:} Analysis restricted to individuals with employment activity status (cat\\_actv) equal to 1 (employed) or 2 (unemployed).",
                 "\\textbf{Variable Definitions:} Work probability is a binary indicator (0/1) for whether the individual worked on the survey day.",
                 "\\phantom{\\textbf{Variable Definitions:}} Work duration is measured in minutes for those who worked on the survey day.",
                 "\\phantom{\\textbf{Variable Definitions:}} Total rainfall is measured in millimeters from CEMADEN weather stations.",
                 "\\phantom{\\textbf{Variable Definitions:}} Rain dummy equals 1 if total rainfall > 0mm on the survey day, 0 otherwise.",
                 "\\textbf{Data Source:} Origin-Destination Survey (OD) 2017 merged with CEMADEN rainfall data."),
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
            cat(paste(model_name, "- Total Rain:", total_rain_coef, "(p-value:", total_rain_pval, ")\n"));
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
            cat(paste(model_name, "- Total Rain:", total_rain_coef, "(p-value:", total_rain_pval, ")\n"));
        }
    }
}
