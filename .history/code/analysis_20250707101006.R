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
    
    # Create Panel A table
    panel_a_table <- modelsummary(
        models$panel1_models,
        coef_map = coef_map,
        statistic = "std.error",
        vcov = "hetero",  # Heteroskedasticity robust standard errors
        fmt = 4,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
        title = "Panel A: Effect of Rainfall on Work Probability",
        output = "latex"
    )
    
    # Create Panel B table
    panel_b_table <- modelsummary(
        models$panel2_models,
        coef_map = coef_map,
        statistic = "std.error",
        vcov = "hetero",  # Heteroskedasticity robust standard errors
        fmt = 4,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
        title = "Panel B: Effect of Rainfall on Work Duration (minutes)",
        output = "latex"
    )
    
    # Combine both panels into a single LaTeX table
    combined_latex <- paste(
        "\\begin{table}[htbp]",
        "\\centering",
        "\\caption{Effect of Rainfall on Labor Supply}",
        "\\label{tab:rainfall_effects}",
        "\\begin{tabular}{lccccc}",
        "\\toprule",
        " & Model 1 & Model 2 & Model 3 & Model 4 & Model 5 \\\\",
        "\\midrule",
        "\\multicolumn{6}{l}{\\textbf{Panel A: Work Probability}} \\\\",
        "\\addlinespace[0.5em]",
        sep = "\n"
    )
    
    # Extract coefficients from Panel A
    panel_a_coefs <- extract_panel_coefs(models$panel1_models)
    combined_latex <- paste(combined_latex, panel_a_coefs, sep = "\n")
    
    # Add Panel B header
    combined_latex <- paste(
        combined_latex,
        "\\addlinespace[1em]",
        "\\multicolumn{6}{l}{\\textbf{Panel B: Work Duration (minutes)}} \\\\",
        "\\addlinespace[0.5em]",
        sep = "\n"
    )
    
    # Extract coefficients from Panel B
    panel_b_coefs <- extract_panel_coefs(models$panel2_models)
    combined_latex <- paste(combined_latex, panel_b_coefs, sep = "\n")
    
    # Add table footer
    combined_latex <- paste(
        combined_latex,
        "\\addlinespace[1em]",
        "\\midrule",
        paste("Observations (Panel A) &", nobs(models$panel1_models$`Model 1`), "&", 
              nobs(models$panel1_models$`Model 2`), "&", 
              nobs(models$panel1_models$`Model 3`), "&", 
              nobs(models$panel1_models$`Model 4`), "&", 
              nobs(models$panel1_models$`Model 5`), "\\\\"),
        paste("Observations (Panel B) &", nobs(models$panel2_models$`Model 1`), "&", 
              nobs(models$panel2_models$`Model 2`), "&", 
              nobs(models$panel2_models$`Model 3`), "&", 
              nobs(models$panel2_models$`Model 4`), "&", 
              nobs(models$panel2_models$`Model 5`), "\\\\"),
        "Individual Controls & No & No & No & Yes & Yes \\\\",
        "Zone Fixed Effects & No & No & No & No & Yes \\\\",
        "\\bottomrule",
        "\\end{tabular}",
        "\\begin{tablenotes}",
        "\\footnotesize",
        "\\item Notes: This table presents the effects of rainfall on labor supply outcomes.",
        "\\item Panel A shows the effect on work probability (0/1) for employed individuals.",
        "\\item Panel B shows the effect on work duration (minutes) for those who worked.",
        "\\item Model 1: Rain dummy only. Model 2: Total rainfall only. Model 3: Both variables.",
        "\\item Model 4: Both variables + individual controls. Model 5: Model 4 + zone fixed effects.",
        "\\item Individual controls include age, gender, education, activity, occupation, sector, and employment type.",
        "\\item Heteroskedasticity robust standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01",
        "\\end{tablenotes}",
        "\\end{table}",
        sep = "\n"
    )
    
    # Write to file
    writeLines(combined_latex, filename)
    
    message(paste("Regression table exported to:", filename))
    return(combined_latex)
}

# Helper function to extract coefficients for panels
extract_panel_coefs <- function(models) {
    coef_lines <- ""
    
    # Rain Dummy row
    rain_dummy_line <- "Rain Dummy (>0mm)"
    rain_dummy_se_line <- ""
    
    # Total Rainfall row
    total_rain_line <- "Total Rainfall (mm)"
    total_rain_se_line <- ""
    
    for(i in 1:5) {
        model <- models[[i]]
        coef_summary <- summary(model, vcov = "hetero")$coefficients
        
        # Rain dummy coefficient
        if("dm_rain" %in% rownames(coef_summary)) {
            coef_val <- round(coef_summary["dm_rain", "Estimate"], 4)
            se_val <- round(coef_summary["dm_rain", "Std. Error"], 4)
            p_val <- coef_summary["dm_rain", "Pr(>|t|)"]
            
            # Add significance stars
            stars <- ""
            if(p_val < 0.01) stars <- "***"
            else if(p_val < 0.05) stars <- "**"
            else if(p_val < 0.1) stars <- "*"
            
            rain_dummy_line <- paste(rain_dummy_line, " & ", coef_val, stars, sep = "")
            rain_dummy_se_line <- paste(rain_dummy_se_line, " & (", se_val, ")", sep = "")
        } else {
            rain_dummy_line <- paste(rain_dummy_line, " & ", sep = "")
            rain_dummy_se_line <- paste(rain_dummy_se_line, " & ", sep = "")
        }
        
        # Total rainfall coefficient
        if("total_rain" %in% rownames(coef_summary)) {
            coef_val <- round(coef_summary["total_rain", "Estimate"], 6)
            se_val <- round(coef_summary["total_rain", "Std. Error"], 6)
            p_val <- coef_summary["total_rain", "Pr(>|t|)"]
            
            # Add significance stars
            stars <- ""
            if(p_val < 0.01) stars <- "***"
            else if(p_val < 0.05) stars <- "**"
            else if(p_val < 0.1) stars <- "*"
            
            total_rain_line <- paste(total_rain_line, " & ", coef_val, stars, sep = "")
            total_rain_se_line <- paste(total_rain_se_line, " & (", se_val, ")", sep = "")
        } else {
            total_rain_line <- paste(total_rain_line, " & ", sep = "")
            total_rain_se_line <- paste(total_rain_se_line, " & ", sep = "")
        }
    }
    
    # Add line endings
    rain_dummy_line <- paste(rain_dummy_line, " \\\\", sep = "")
    rain_dummy_se_line <- paste(rain_dummy_se_line, " \\\\", sep = "")
    total_rain_line <- paste(total_rain_line, " \\\\", sep = "")
    total_rain_se_line <- paste(total_rain_se_line, " \\\\", sep = "")
    
    return(paste(rain_dummy_line, rain_dummy_se_line, total_rain_line, total_rain_se_line, sep = "\n"))
}
