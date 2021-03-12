library(shiny)
library(tidyverse)
library(ggthemes)
library(shinythemes)

dataset = read_csv("cleaned_data.csv")
dataset = dataset %>%
    mutate(
        steak = factor(steak, levels = c("Rare", "Medium-rare", "Medium", "Medium-well done", "Well done", "I don't eat beef"))
    ) %>%
    mutate(
        covid_tests_cat = factor(covid_tests, levels = c("0", "1", "2", "3", "4", "5", "6", "10"))
    ) %>%
    mutate(
        stress_cat = factor(stress, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    ) %>%
    mutate(
        fave_season = factor(fave_season, levels = c("Summer", "Autumn", "Winter", "Spring"))
    ) %>%
    mutate(
        dentist = factor(dentist, levels = c("Less than 6 months", "Between 6 and 12 months", "Between 12 months and 2 years", "More than 2 years"))
    ) %>%
    mutate(
        floss_freq = factor(floss_freq, levels = c("Every day", "Most days", "Weekly", "Less than once a week"))
    ) %>%
    mutate(
        social_media = factor(social_media, levels = c("bilibili", "Ed", "Facebook", "Instagram", "Messenger", "Reddit", "Snapchat", "TikTok", "Twitter", "WeChat", "YouTube", "Other or none"))
    )  %>%
    mutate(
        eye_colour = factor(eye_colour, levels = c("Black", "Dark brown", "Brown", "Hazel", "Green", "Blue", "Other"))
    )  %>%
    mutate(
        do_you_have_asthma = factor(do_you_have_asthma, levels = c("Yes", "No"))
    )  %>%
    mutate(
        dog_cat = factor(dog_cat, levels = c("Yes", "No"))
    ) %>%
    mutate(
        live_with_parents = factor(live_with_parents, levels = c("Yes", "No"))
    ) %>%
    mutate(
        eyewear = factor(eyewear, levels = c("Yes", "No"))
    )  %>%
    mutate(
        hours_studying_cat = case_when(
            is.na(hours_studying) ~ NA_character_,
            hours_studying <= 10  ~ "0-10",
            10 < hours_studying & hours_studying <= 20 ~ "11-20",
            20 < hours_studying & hours_studying <= 30 ~ "21-30",
            30 < hours_studying & hours_studying <= 40 ~ "31-40",
            40 < hours_studying & hours_studying <= 50 ~ "41-50",
            hours_studying > 50 ~ "51+")
    # ) %>% 
    # mutate(
    #     covid_tests_cat = case_when(
    #         covid_tests_cat == "10" ~ "6+",
    #         TRUE ~ covid_tests_cat)
    ) %>% 
    mutate(
        hours_exercising_cat = case_when(
            is.na(hours_exercising) ~ NA_character_,
            hours_exercising == 0  ~ "0",
            hours_exercising == 1  ~ "1",
            hours_exercising == 2  ~ "2",
            hours_exercising == 3  ~ "3",
            hours_exercising == 4  ~ "4",
            hours_exercising == 5  ~ "5",
            hours_exercising == 6  ~ "6",
            hours_exercising == 7  ~ "7",
            hours_exercising == 8  ~ "8",
            hours_exercising == 9  ~ "9",
            hours_exercising == 10  ~ "10",
            hours_exercising > 10 ~ "11+"),
        hours_exercising_cat = factor(hours_exercising_cat, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11+"))
    ) %>%
    mutate(
        hours_paid_work_cat = case_when(
            is.na(hours_paid_work) ~ NA_character_,
            hours_paid_work <= 5 ~ "0-5",
            5 < hours_paid_work & hours_paid_work <= 10 ~ "6-10",
            10 < hours_paid_work & hours_paid_work <= 15 ~ "11-15",
            15 < hours_paid_work & hours_paid_work <= 20 ~ "16-20",
            20 < hours_paid_work & hours_paid_work <= 25 ~ "21-25",
            hours_paid_work > 25 ~ "26+"),
        hours_paid_work_cat = factor(hours_paid_work_cat, levels = c("0-5","6-10","11-15","16-20","21-25","26+") )
    ) %>%
    #Making gender and handedness binary:
    mutate(
        gender_bin = case_when(
            gender == "Non-binary" ~ NA_character_,
            TRUE ~ gender)
    ) %>%
    mutate(
        dominant_hand_bin = case_when(
            dominant_hand == "Ambidextrous" ~ NA_character_,
            TRUE ~ dominant_hand)
    )

varchoices = c("Gender" = "gender",
               "Gender (males and females only)" = "gender_bin",
               "Hours spent studying per week" = "hours_studying_cat",
               "Hours spent on paid work per week" = "hours_paid_work_cat",
               "Hours spent exercising per week" = "hours_exercising_cat",
               "Reported stress level" = "stress_cat",
               "Eye colour" = "eye_colour",
               "Do they have asthma?" = "do_you_have_asthma",
               "Flossing frequency" = "floss_freq",
               "Last visit to dentist" = "dentist",
               "Number of COVID tests" = "covid_tests_cat",
               "Preferred social media" = "social_media",
               "Dog or cat growing up?" = "dog_cat",
               "Do they live with parents?" = "live_with_parents",
               "Favourite season" = "fave_season",
               "Do they wear glasses or contacts?" = "eyewear",
               "Dominant hand" = "dominant_hand",
               "Steak preference" = "steak"
               )
tvarchoices = c("Hours spent studying per week" = "hours_studying",
                "Hours spent on paid work per week" = "hours_paid_work",
                "Hours spent exercising per week" = "hours_exercising",
                "Reported stress level" = "stress",
                "Number of COVID tests" = "covid_tests",
                "Height" = "height"
                )
binarychoices = c("Gender" = "gender_bin",
                  "Do they have asthma?" = "do_you_have_asthma",
                  "Dog or cat growing up?" = "dog_cat",
                  "Do they live with parents?" = "live_with_parents",
                  "Do they wear glasses or contacts?" = "eyewear",
                  "Dominant hand" = "dominant_hand_bin"
                  )

ui <- fluidPage(
    
    theme = shinytheme("sandstone"),
    
    titlePanel("Hypothesis tests"),
    
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition = "input.tabs != 'Two-sample t-test' && input.tabs != 'One-sample t-test'",
                             selectizeInput(inputId = "var1",
                                               label = "Select first variable:",
                                               choices = varchoices)
            ),
            conditionalPanel(condition = "input.tabs == 'Chi-squared test for independence' || input.tabs == 'Fishers test'",
                             selectizeInput(inputId = "var2",
                                            "Select second variable:",
                                            choices = varchoices)
                             ),
            conditionalPanel(condition = "input.tabs == 'One-sample t-test'",
                             selectizeInput(inputId = "tvar1s",
                                            label = "Select variable:",
                                            choices = tvarchoices),
                             numericInput(inputId = "mu",
                                          label = "Expected mean",
                                          value = 0),
                             selectizeInput(inputId = "alt",
                                            label = "Alternative:",
                                            choices = c("Two sided"="two.sided",
                                                        "Greater than"="greater",
                                                        "Less than"="less"))
                             ),
            conditionalPanel(condition = "input.tabs == 'Two-sample t-test'",
                             selectizeInput(inputId = "tvar1",
                                            label = "Select variable:",
                                            choices = tvarchoices),
                             selectizeInput(inputId = "tvar2",
                                            label = "Select category to split sample by:",
                                            choices = binarychoices),
                             checkboxInput(inputId = "welch",
                                            label = "Welch t-test?",
                                            value = F)
            ),
            conditionalPanel(condition = "input.tabs != 'Two-sample t-test' && input.tabs != 'One-sample t-test'",
                                checkboxInput(inputId = "correct",
                                              label = "Simulate p-values? (using Monte-Carlo simulation)",
                                              value = F)),
            sliderInput(inputId = "alpha",
                        label = "Significance level:",
                        min = 0.01,
                        max = 0.10,
                        value = 0.05)
                             
        ),

        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tabs",
                        
                        tabPanel(
                            title = "Chi-squared test for goodness of fit",
                            strong("Null hypothesis:"),
                            "All categories are evenly distributed.",
                            br(),
                            strong("Alternative hypothesis:"),
                            "Not all of the categories are evenly distributed.",
                            br(),
                            strong("Assumptions:"),
                            textOutput("GOFassumptions"),
                            plotOutput("GOFplot"),
                            strong("Test statistic:"),
                            textOutput("GOFtest1"),
                            strong("P-value:"),
                            textOutput("GOFtest2"),
                            strong("Decision:"),
                            textOutput("GOFtest3")
                        ),
                        tabPanel(
                            title = "Chi-squared test for independence",
                            strong("Null hypothesis:"),
                            "The two variables are independent.",
                            br(),
                            strong("Alternative hypothesis:"),
                            "The two variables are not independent.",
                            br(),
                            strong("Assumptions:"),
                            textOutput("indassumptions"),
                            plotOutput("dubplot1"),
                            plotOutput("dubplot2"),
                            strong("Test statistic:"),
                            textOutput("indtest1"),
                            strong("P-value:"),
                            textOutput("indtest2"),
                            strong("Decision:"),
                            textOutput("indtest3")
                        ),
                        tabPanel(
                            title = "Fishers test",
                            strong("Null hypothesis:"),
                            "The two variables are independent.",
                            br(),
                            strong("Alternative hypothesis:"),
                            "The two variables are not independent.",
                            br(),
                            "Please see the 'test for independence' tab for visualisations of this data.",
                            br(),
                            strong("Warning:"),
                            "The Fisher's test is not recommended for variables with several categories; using it in this case may return an error.",
                            br(),
                            br(),
                            strong("P-value:"),
                            textOutput("fishtest1"),
                            strong("Decision:"),
                            textOutput("fishtest2")
                        ),
                        tabPanel(
                            title = "One-sample t-test",
                            strong("Null hypothesis:"),
                            "The population mean is equal to the entered value.",
                            br(),
                            strong("Alternative hypothesis:"),
                            "The population mean is either greater than, less than or not equal to the entered value, depending on the selected alternative.",
                            br(),
                            strong("Assumptions:"),
                            "We must assume that the sample is independently and identically distribued random variables on the normal curve.",
                            plotOutput("t1plot"),
                            strong("Test statistic:"),
                            textOutput("t1test1"),
                            strong("P-value:"),
                            textOutput("t1test2"),
                            strong("Decision:"),
                            textOutput("t1test3")
                        ),
                        tabPanel(
                            title = "Two-sample t-test",
                            strong("Null hypothesis:"),
                            "The two samples have the same mean.",
                            br(),
                            strong("Alternative hypothesis:"),
                            "The two samples do not have the same mean.",
                            br(),
                            strong("Assumptions:"),
                            "We must assume that the two samples are independently and identically distribued on the normal curve, that the two samples are independent, and that the two samples have the same variance. If the latter assumption does not hold, please check the Welch t-test option.",
                            plotOutput("tplot"),
                            strong("Test statistic:"),
                            textOutput("ttest1"),
                            strong("P-value:"),
                            textOutput("ttest2"),
                            strong("Decision:"),
                            textOutput("ttest3")
                        )
                        
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$GOFassumptions = renderText({
        dataset = dataset %>% drop_na(!!(input$var1))
        
        if (length(dataset[[input$var1]])/length(table(dataset[[input$var1]])) > 5){
            paste("We must assume that all expected categories are greater than 5. This is the case here.")
        }else{
            paste("We must assume that all expected vategories are greater than 5. However, here some categories are not greater than 5, so the test may be inappropriate. It is suggested to use simulated p-values.")
        }
    })
    
    
    output$GOFplot = renderPlot({
        
        dataset = dataset %>% drop_na(!!(input$var1))
        
        dataset %>% ggplot() +
            aes_string(x = input$var1) +
            geom_bar(fill="salmon3") +
            theme_solarized() + 
            geom_hline(yintercept = length(dataset[[input$var1]])/length(table(dataset[[input$var1]])), colour = "indianred4") +
            labs(
                title="Observed and expected frequency",
                x=names(which(varchoices == input$var1)),
                y="Frequency"
            )

    })
    
    output$GOFtest1 = renderText({
       dataset = dataset %>% drop_na(!!(input$var1))
       chitest= chisq.test(table(dataset[[input$var1]]), simulate.p.value=input$correct)
       paste(signif(chitest$statistic, digits=3))})
    output$GOFtest2 = renderText({
        dataset = dataset %>% drop_na(input$var1)
        chitest= chisq.test(table(dataset[[input$var1]]), simulate.p.value=input$correct)
       paste(signif(chitest$p.value, digits=3))})
    output$GOFtest3 = renderText({
        dataset = dataset %>% drop_na(input$var1)
        chitest= chisq.test(table(dataset[[input$var1]]), simulate.p.value=input$correct)
    if (chitest$p.value < input$alpha) {
        paste("Since this is less than your selected significance level, we reject the null hypothesis. The data suggests the categories are not evenly distributed.")
    } else {
        paste("This is not less than your selected significance level, so there is insufficient evidence to reject the null hypothesis. The categories may be evenly distributed.")
    }
        })
    
    output$indassumptions = renderText({
        dataset = dataset %>% drop_na(!!(input$var1),!!(input$var2))
        chitest= chisq.test(table(dataset[[input$var1]],dataset[[input$var2]]), simulate.p.value=input$correct)
        
        if (all(chitest$expected > 5)) {
            paste("We must assume that all expected cell counts are greater than 5. This is the case here.")
        } else{
            paste("We must assume that all expected cell counts are greater than 5. However, here some expected cell counts are not greater than 5, so the test may be inappropriate. It is suggested to use simulated p-values or the Fisher's test.")
            }
    })
    
    output$dubplot1 = renderPlot({
        
        dataset = dataset %>% drop_na(!!(input$var1), !!(input$var2))
        
        dataset %>% ggplot() +
            aes_string(x=input$var1, fill=input$var2) +
            geom_bar() +
            theme_solarized() +
            theme(legend.position = "bottom") +
            # scale_x_discrete(labels=c("<6 months", "6–12 months", "1–2 years", ">2 years")) +
            labs(
                title="Observed frequencies",
                x=names(which(varchoices == input$var1)),
                y="Count",
                fill=names(which(varchoices == input$var2))
            )
        
    })
    
    output$dubplot2 = renderPlot({
        
        dataset = dataset %>% drop_na(!!(input$var1), !!(input$var2))
        
        dataset %>% ggplot() +
            aes_string(x=input$var1, fill=input$var2) +
            geom_bar(position="fill") +
            theme_solarized() +
            theme(legend.position = "bottom") +
            # scale_x_discrete(labels=c("<6 months", "6–12 months", "1–2 years", ">2 years")) +
            labs(
                title="Observed proportions",
                x=names(which(varchoices == input$var1)),
                y="Proportion",
                fill=names(which(varchoices == input$var2))
            )
        
    })
    
    output$indtest1 = renderText({
        dataset = dataset %>% drop_na(!!(input$var1),!!(input$var2))
        chitest= chisq.test(table(dataset[[input$var1]],dataset[[input$var2]]), simulate.p.value=input$correct)
        paste(signif(chitest$statistic, digits=3))})
    output$indtest2 = renderText({
        dataset = dataset %>% drop_na(!!(input$var1),!!(input$var2))
        chitest= chisq.test(table(dataset[[input$var1]],dataset[[input$var2]]), simulate.p.value=input$correct)
        paste(signif(chitest$p.value, digits=3))})
    output$indtest3 = renderText({
        dataset = dataset %>% drop_na(!!(input$var1),!!(input$var2))
        chitest= chisq.test(table(dataset[[input$var1]],dataset[[input$var2]]), simulate.p.value=input$correct)
        if (chitest$p.value < input$alpha) {
            paste("Since this is less than your selected significance level, we reject the null hypothesis. The data suggests the variables are not independent.")
        } else {
            paste("This is not less than your selected significance level, so there is insufficient evidence to reject the null hypothesis. The variables may be independent.")
        }
    })
    
    output$fishtest1 = renderText({
        dataset = dataset %>% drop_na(!!(input$var1),!!(input$var2))
        ftest= fisher.test(table(dataset[[input$var1]],dataset[[input$var2]]), simulate.p.value=input$correct)
        paste(signif(ftest$p.value, digits=3))})
    output$fishtest2 = renderText({
        dataset = dataset %>% drop_na(!!(input$var1),!!(input$var2))
        ftest= fisher.test(table(dataset[[input$var1]],dataset[[input$var2]]), simulate.p.value=input$correct)
        if (ftest$p.value < input$alpha) {
            paste("Since this is less than your selected significance level, we reject the null hypothesis. The data suggests the variables are not independent.")
        } else {
            paste("This is not less than your selected significance level, so there is insufficient evidence to reject the null hypothesis. The variables may be independent.")
        }
    })

    output$t1plot = renderPlot({
        dataset = dataset %>% drop_na(!!(input$tvar1s))
        
        dataset %>% ggplot() +
            aes_string(y=input$tvar1s) +
            geom_boxplot(fill="salmon3") + 
            # geom_dotplot(binaxis="y", stackdir="center", colour = "indianred4") + 
            geom_hline(yintercept = input$mu, colour = "indianred4") +
            theme_solarized() + 
            labs(
                title="Distribution of sample, with expected mean",
                y=names(which(binarychoices == input$tvar1s))
            )
        
    })
    
    output$t1test1 = renderText({
        dataset = dataset %>% drop_na(!!(input$tvar1s))
        t1result = t.test(dataset[[input$tvar1s]], mu=input$mu, alternative=input$alt)
        paste(signif(t1result$statistic, digits=3))})
    output$t1test2 = renderText({
        dataset = dataset %>% drop_na(!!(input$tvar1s))
        t1result = t.test(dataset[[input$tvar1s]], mu=input$mu, alternative=input$alt)
        paste(signif(t1result$p.value, digits=3))})
    output$t1test3 = renderText({
        dataset = dataset %>% drop_na(!!(input$tvar1s))
        t1result = t.test(dataset[[input$tvar1s]], mu=input$mu, alternative=input$alt)
        if (t1result$p.value < input$alpha) {
            paste("Since this is less than your selected significance level, we reject the null hypothesis. The data suggests the population does not have the selected mean.")
        } else {
            paste("This is not less than your selected significance level, so there is insufficient evidence to reject the null hypothesis. The population may have the selected mean.")
        }
    })

    output$tplot = renderPlot({
        dataset = dataset %>% drop_na(!!(input$tvar1),!!(input$tvar2))

        dataset %>% ggplot() +
            aes_string(x = input$tvar2, y=input$tvar1) +
            geom_boxplot(fill="salmon3") + 
            geom_jitter(width=0.15, size = 1, colour = "indianred4") + 
            theme_solarized() + 
            labs(
                title="Distribution of both samples",
                x=names(which(binarychoices == input$tvar2)),
                y=names(which(tvarchoices == input$tvar1))
            )
        
    })
    
    output$ttest1 = renderText({
        dataset = dataset %>% drop_na(!!(input$tvar1),!!(input$tvar2))
        tresult = t.test(dataset[[input$tvar1]] ~ dataset[[input$tvar2]], var.equal=!input$welch)
        paste(signif(tresult$statistic, digits=3))})
    output$ttest2 = renderText({
        dataset = dataset %>% drop_na(!!(input$tvar1),!!(input$tvar2))
        tresult = t.test(dataset[[input$tvar1]] ~ dataset[[input$tvar2]], var.equal=!input$welch)
        paste(signif(tresult$p.value, digits=3))})
    output$ttest3 = renderText({
        dataset = dataset %>% drop_na(!!(input$tvar1),!!(input$tvar2))
        tresult = t.test(dataset[[input$tvar1]] ~ dataset[[input$tvar2]], var.equal=!input$welch)
        if (tresult$p.value < input$alpha) {
            paste("Since this is less than your selected significance level, we reject the null hypothesis. The data suggests the samples do not have the same mean.")
        } else {
            paste("This is not less than your selected significance level, so there is insufficient evidence to reject the null hypothesis. The samples may have the same mean.")
        }
    })

        }

# Run the application
shinyApp(ui = ui, server = server)
