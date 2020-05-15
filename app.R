#load required packages
library(shiny)
library(plotly)
library(tidyr)
library(purrr)
library(ggplot2)

#read in required csv files
general <- read.csv("coronadata_general.csv", sep = ";")
age_sex <- read.csv("coronadata_age_sex.csv", sep = ";")
log_data <- read.csv("log_data.csv", sep = ";")
deaths <- read.csv("deaths_demo.csv", sep = ";")

#Build user interface 
ui <- fluidPage(
  navbarPage("Monroe County COVID-19 Progress",
             tabPanel("Charts",
              sidebarPanel(
               selectInput("select", label = "Choose a plot to display", 
                           choices = list("Death and Total Cases", "Cumulative Cases Semi Log", "Hospitalizations and ICU", "Summary Stats", 
                                          "M vs. F Pie Chart", "Age and Sex Breakdown", "Growth Rates", "Bar Chart of Active Cases", 
                                          "Active Cases by Proportions", "Deaths by Age Group"), 
                           selected = "Cumulative Cases")),
              mainPanel(plotlyOutput("plot"))),
             tabPanel("General Data",
              mainPanel(tableOutput("table"))),
             tabPanel("Age and Sex Data",
              mainPanel(tableOutput("table2")))
            )
        )

#server dialogue
server <- function(input, output) {
  
  #Plot Outputs
  #Basic Filled Area Plot #No Hospitalization
  fa <- plot_ly(general, x=~date, y=~death_count, type='scatter', mode='lines', name = "Deaths", fill = 'tozeroy')
  # fa <- fa %>% 
  #   add_trace(y=~hosp, name = "Hospitalizations", fill = 'tonexty')
  fa <- fa %>% 
    add_trace(y=~total_conf, name = "Total Confirmed Cases", fill = 'tonexty')
  fa <- fa %>%
    layout(title="Deaths as Part of Total Cases", xaxis=list(title="Date"), yaxis=list(title="Counts"))
  
  #SemiLog
  semi_log <- plot_ly(log_data, x=~date, y=~log_total_conf, type = 'scatter', mode = 'lines')
  semi_log <- semi_log %>%
    layout(title='Total Confirmed Cases (Log Scale)', xaxis=list(title="Date"), yaxis=list(title="Case Count"))
  
  #Hospitalizations and ICU Instead
  hosp_icu <- plot_ly(general, x=~date, y=~ICU, type='scatter', mode='lines', name = "ICU", stackgroup = 'one', fillcolor = '#F5FF8D', line=list(color='#F5FF8D'))
  hosp_icu <- hosp_icu %>%
    add_trace(y=~hosp, name = "Hospitalized", fillcolor="50CB86", line=list(color="50CB86"))
  hosp_icu <- hosp_icu %>%
    layout(title = "COVID-19 Cases by Category", xaxis=list(title="Date"), yaxis=list(title="Counts"))
  
  #STACKED BAR ACTIVE CASES
  df_sb <- tail(general, 15)
  sb <- plot_ly(df_sb, x=~date, y=~unhosp_active, type = 'bar', name = "Unhospitalized")
  sb <- sb %>%
    add_trace(y=~ICU, name = "ICU")
  sb <- sb %>%
    add_trace(y=~nonICU, name = 'Non-ICU Hospitalizations')
  sb <- sb %>%
    layout(barmode = 'stack', yaxis = list(title = "Count"), title = "Visualization of Active Cases", xaxis = list(title = "Date"))
  
  #Cumulative stacked area active cases
  df_csa <- tail(general, 15)
  csa_active <- plot_ly(df_csa, x=~date, y=~unhosp_active, type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D', name = "Unhospitalized Cases")
  csa_active <- csa_active %>%
    add_trace(y=~ICU, name = "ICU Hospitalizations", fillcolor = "#50CB86")
  csa_active <- csa_active %>%
    add_trace(y=~nonICU, name = "Non-ICU Hospitalizations", fillcolor = "#4C74C9")
  csa_active <- csa_active %>%
    layout(title = "Active Cases by Proportion", xaxis = list(title = "Date"), yaxis = list(title = "Percentage"))
  
  #SEX PIE CHART
  
  males <- max(age_sex['M'])
  females <- max(age_sex['F'])
  labels = c('Male', "Female")
  values = c(males, females)
  pie_s <- plot_ly(type = 'pie', labels = labels, values = values, textinfo = 'label+percent', insidetextorientation='radial', showlegend = FALSE)
  pie_s <- pie_s %>%
    layout(title = "Confirmed Cases by Sex")
  
  #AGE DONUT CHART
    #I WANT ALL PIE CHARTS TO BE LINKED TO ONE SELECTION ON MENU-FUTURE TASK
  labels <- c('0-19(M)', '0-19(F)', '20-29(M)', '20-29(F)', '30-39(M)', '30-39(F)', '40-49(M)', 
              '40-49(F)', '50-59(M)', '50-59(F)', '60-69(M)', '60-69(F)', '70-79(M)', '70-79(F)', '80-89(M)', 
              '80-89(F)', '90-99(M)', '90-99(F)', '100+(M)', '100+(F)')
  
  last <- tail(age_sex, 1)
  
  values <- as.numeric(last[2:21])
  
  donut <- plot_ly(labels = labels, values = values)
  donut <- donut %>% add_pie(hole = 0.6)
  donut <- donut %>%
    layout(title = "Confirmed Cases by Age and Sex")
  
  #DEATHS BY AGE
  labels_d <- c('0-59', '60-69', '70-79', '80-89', '90-99')
  last_d <- tail(deaths)
  values_d <- as.numeric(last_d)
  donut_d <- plot_ly(labels = labels_d, values = values_d)
  donut_d <- donut_d %>%
    add_pie(hole = 0.4)
  donut_d <- donut_d %>%
    layout(title = "Deaths by Age Group")
  
  
  #ANOTHER LINE CHART WITH DAILY TESTS COMPLETED, NEW POSITIVES, POSITIVE RATE, CUMULATIVE CFR (DOUBLE Y AXIS PERCENT ON RIGHT SIDE COUNT ON OTHER)
  summary_line <- plot_ly(general, x=~date, y=~new_cases, type='scatter', mode='lines', name = "Daily New Confirmed Cases (Left)", line = list(color = 'rgb(22, 96, 167)'))
  summary_line <- summary_line %>%
    add_trace(y=~daily_tests, name = "Daily Tests Completed (Left)", line = list(color = 'rgb(205, 12, 24)'))
  summary_line <- summary_line %>%
    add_trace(y=~percent_pos, name = "Cumulative Positivity Rate (Right)", line = list(color = 'rgb(205, 12, 24)', dash = 'dash'), yaxis = "y2")
  summary_line <- summary_line %>%
    add_trace(y=~death_rate, name = "Cumulative CFR (Right)", line = list(color = 'rgb(22, 96, 167)', dash = 'dash'), yaxis = "y2")
  summary_line <- summary_line %>%
    add_trace(y=~daily_PR, name = "Daily Positivity Rate (Right)", line = list(color = 'rgb(12, 200, 90)'), yaxis = "y2")
  summary_line <- summary_line %>%
    layout(title = 'Monroe County Summary Stats', yaxis = list(title = "Count"), yaxis2 = list(overlaying = "y", side = "right", title = "Percentage"))
  
  #ANOTHER LINE CHART WITH DAILY GROWTH RATE OF POSITIVE CASES, DAILY GROWTH RATE OF DEATHS, DAILY GROWTH RATE OF ACTUAL INFECTIONS 
  #LAST 7 DAYS
  growth_rates <- plot_ly(general, x=~date, y=~percent_increase, type = 'scatter', mode = 'lines', name = "Daily GR of Positive Cases", line = list(color = 'rgb(22, 96, 167)'))
  growth_rates <- growth_rates %>%
    add_trace(y=~death_growth_rate, line = list(color = 'rgb(205, 12, 24)'), name = "Daily GR of Deaths")
  growth_rates <- growth_rates %>%
    layout(title = "Growth Rates of Positive Cases and Deaths", xaxis = list(title = "Date"), yaxis = list(title = "Percentage"))
  
  #RETROACTIVE?
  
  #render plots and connect them to drop-down menu choices

  output$plot <- renderPlotly({
    temp <- switch(input$select,
                   "Death and Total Cases" = fa,
                   "Cumulative Cases Semi Log" = semi_log,
                   "Hospitalizations and ICU" = hosp_icu,
                   "Summary Stats" = summary_line, 
                   "M vs. F Pie Chart" = pie_s,
                   "Age and Sex Breakdown" = donut, 
                   "Growth Rates" = growth_rates,
                   "Bar Chart of Active Cases" = sb,
                   "Active Cases by Proportions" = csa_active,
                   "Deaths by Age Group" = donut_d)
  })
  observeEvent(input$select, {
    chart <- input$select
    if (input$select == "Deaths by Age Group") {
      showNotification("Warning: This plot may not be up to date", type = "error", closeButton = FALSE, duration = NULL)
    }
  })
  
  #Table Outputs
  #Change column names so they are more readable
  general_app <- general
  colnames(general_app) <- c("Date", "Total Confirmed Cases", "New Cases", "% Inc Confirmed Cases", "Total Tests Run", "Daily Tests Run",
                             "% Inc Total Tests Run", "Total Positive Cases", "Cummulative Positivity Rate", "Daily Pos. Rate", "Current Hospitalized",
                             "Total Deaths", "CFR", "% Inc in Deaths", "Currently in ICU", "% Hosp. in ICU", "Active Cases", "Unhosp. Active Cases",
                             "% of Active Cases Hosp.", "% of Active Cases in ICU", "Active Cases Hosp. (Not in ICU)")
  #reorder data frame from most to least recent
  df <- general_app %>%
    map_df(rev)
  
  #render general data table
  output$table <- renderTable(df)
  
  #change column names for readability again
  age_sex_app <- age_sex
  colnames(age_sex_app) <- c('Date', '0-19(M)', '0-19(F)', '20-29(M)', '20-29(F)', '30-39(M)', '30-39(F)', '40-49(M)', 
                             '40-49(F)', '50-59(M)', '50-59(F)', '60-69(M)', '60-69(F)', '70-79(M)', '70-79(F)', '80-89(M)', 
                             '80-89(F)', '90-99(M)', '90-99(F)', '100+(M)', '100+(F)', 'Total Male Conf. Cases', 'Total Female Conf. Cases',
                             '% Male (of total conf. cases)', '% Female (of total conf. cases')
  
  #reorder dataframe from most to least recent
  df2 <- age_sex_app %>%
    map_df(rev)
  
  #render age and sex data table
  output$table2 <- renderTable(df2)
}

shinyApp(ui = ui, server = server)