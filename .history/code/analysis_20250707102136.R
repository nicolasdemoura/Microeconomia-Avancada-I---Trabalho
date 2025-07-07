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
   
    
    # Create Panel A table using stargazer
    panel_a_table <- stargazer(
        models$panel1_models$`Model 1`,
        models$panel1_models$`Model 2`,
        models$panel1_models$`Model 3`,
        models$panel1_models$`Model 4`,
        models$panel1_models$`Model 5`,
        title = "Panel A: Work Probability",
        type = "latex",
        se = list(
            sqrt(diag(vcovHC(models$panel1_models$`Model 1`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel1_models$`Model 2`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel1_models$`Model 3`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel1_models$`Model 4`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel1_models$`Model 5`, type = "HC1")))
        ),
        keep = c("dm_rain", "total_rain"),
        covariate.labels = c("Rain Dummy (>0mm)", "Total Rainfall (mm)"),
        dep.var.labels = "Work Probability",
        column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)"),
        model.numbers = FALSE,
        omit.stat = c("f", "ser", "rsq", "adj.rsq"),
        star.cutoffs = c(0.1, 0.05, 0.01),
        star.char = c("*", "**", "***"),
        add.lines = list(
            c("Individual Controls", "No", "No", "No", "Yes", "Yes"),
            c("Zone Fixed Effects", "No", "No", "No", "No", "Yes")
        ),
        notes = c("Panel A shows the effect on work probability (0/1) for employed individuals.",
                 "Individual controls include: age, gender, education, activity, occupation, sector, and employment type.",
                 "Heteroskedasticity robust standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01"),
        notes.append = FALSE,
        out = "output/panel_a.tex"
    )
    
    # Create Panel B table using stargazer
    panel_b_table <- stargazer(
        models$panel2_models$`Model 1`,
        models$panel2_models$`Model 2`,
        models$panel2_models$`Model 3`,
        models$panel2_models$`Model 4`,
        models$panel2_models$`Model 5`,
        title = "Panel B: Work Duration (minutes)",
        type = "latex",
        se = list(
            sqrt(diag(vcovHC(models$panel2_models$`Model 1`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel2_models$`Model 2`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel2_models$`Model 3`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel2_models$`Model 4`, type = "HC1"))),
            sqrt(diag(vcovHC(models$panel2_models$`Model 5`, type = "HC1")))
        ),
        keep = c("dm_rain", "total_rain"),
        covariate.labels = c("Rain Dummy (>0mm)", "Total Rainfall (mm)"),
        dep.var.labels = "Work Duration (minutes)",
        column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)"),
        model.numbers = FALSE,
        omit.stat = c("f", "ser", "rsq", "adj.rsq"),
        star.cutoffs = c(0.1, 0.05, 0.01),
        star.char = c("*", "**", "***"),
        add.lines = list(
            c("Individual Controls", "No", "No", "No", "Yes", "Yes"),
            c("Zone Fixed Effects", "No", "No", "No", "No", "Yes")
        ),
        notes = c("Panel B shows the effect on work duration (minutes) for those who worked.",
                 "Individual controls include: age, gender, education, activity, occupation, sector, and employment type.",
                 "Heteroskedasticity robust standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01"),
        notes.append = FALSE,
        out = "output/panel_b.tex"
    )
    
    # Create a combined file that includes both panels
    combined_content <- paste(
        "% Panel A: Work Probability",
        "\\input{output/panel_a.tex}",
        "",
        "\\vspace{1cm}",
        "",
        "% Panel B: Work Duration",
        "\\input{output/panel_b.tex}",
        sep = "\n"
    )
    
    # Write the combined file
    writeLines(combined_content, filename)
    
    message(paste("Panel A table exported to: output/panel_a.tex"))
    message(paste("Panel B table exported to: output/panel_b.tex"))
    message(paste("Combined table file exported to:", filename))
    
    return(list(panel_a = panel_a_table, panel_b = panel_b_table))
}
