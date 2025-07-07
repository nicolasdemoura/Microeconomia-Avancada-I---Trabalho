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
        escape = FALSE,
        notes = "Sample: Individuals with employment activity status equal to 1 (regularly employed) or 2 (do side gigs). The dependent variable is a binary indicator equal to 1 if the individual worked on the reference day, 0 otherwise. Individual controls include age, gender dummy, education level, activity status, occupation, economic sector, and employment type. Zone fixed effects are zone dummies to control for time-invariant spatial characteristics. Standard errors are heteroskedasticity-robust (HC1) and are shown in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01.",
        add_rows = tibble::tibble(
            term = c("Individual Controls", "Zone Fixed Effects"),
            `Model 1` = c("No", "No"),
            `Model 2` = c("No", "No"),
            `Model 3` = c("No", "No"),
            `Model 4` = c("Yes", "No"),
            `Model 5` = c("Yes", "Yes")
        ),
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
        escape = FALSE,
        notes = "Sample: Individuals with employment activity status equal to 1 (regularly employed) or 2 (do side gigs) who worked on survey day. The dependent variable is work duration in minutes for those who worked on the reference day. Individual controls include age, gender dummy, education level, activity status, occupation, economic sector, and employment type. Zone fixed effects are municipality/zone dummies to control for time-invariant spatial characteristics. Standard errors are heteroskedasticity-robust (HC1) and are shown in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01.",
        add_rows = tibble::tibble(
            term = c("Individual Controls", "Zone Fixed Effects"),
            `Model 1` = c("No", "No"),
            `Model 2` = c("No", "No"),
            `Model 3` = c("No", "No"),
            `Model 4` = c("Yes", "No"),
            `Model 5` = c("Yes", "Yes")
        ),
        output = "output/panel_b.tex"
    )
    
    message(paste("Panel A table exported to: output/panel_a.tex"))
    message(paste("Panel B table exported to: output/panel_b.tex"))
    
    return(list(panel_a = panel_a_table, panel_b = panel_b_table))
}
