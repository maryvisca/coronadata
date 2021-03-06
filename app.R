#load required packages
library(shiny)
library(plotly)
library(tidyr)
library(purrr)
library(ggplot2)
library(xlsx)
library(feedeR)
library(DT)

#read in required csv files
general <- read.xlsx("coronadata_general.xlsx", 1)
age_sex <- read.xlsx("coronadata_age_sex.xlsx", 1)
log_data <- read.xlsx("log_data.xlsx", 1)
deaths <- read.xlsx("deaths_demo.xlsx", 1)
link <- read.xlsx("links.xlsx", 1)
genders <- read.xlsx("coronadata_age_sex.xlsx", 2)
ethnicity <- read.xlsx("deaths_demo.xlsx", 2)
props_e <- read.xlsx("deaths_demo.xlsx", 3)

#Build user interface 
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("#shiny-notification-panel {width: 300px; top: calc(50%); right: calc(27%)}
           ")
    )
  ),
  navbarPage("Monroe County COVID-19 Tracker",
             tabPanel("Charts",
              sidebarPanel(
               selectInput("select", label = "Choose a plot to display", 
                           choices = list("Death and Total Cases", "Cumulative Cases Semi Log", "Hospitalizations and ICU", "Summary Stats", 
                                          "Gender Breakdown", "Age and Sex Breakdown", "Growth Rates", "Bar Chart of Active Cases", 
                                          "Active Cases by Proportions", "Age Group Breakdown", "Race/Ethnicity Breakdown"), 
                           selected = "Cumulative Cases")),
              mainPanel(plotlyOutput("plot"))),
             tabPanel("General Data",
              mainPanel(dataTableOutput("table"))),
             tabPanel("Age and Sex Data",
              mainPanel(dataTableOutput("table2"))),
             tabPanel("Local Health Updates",
              mainPanel(dataTableOutput("table3"))),
             tabPanel("Local Policy Updates",
              mainPanel(dataTableOutput("table4"))),
             tabPanel("Sources and Links",
              mainPanel(dataTableOutput("table5")))
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
    layout(title = "Daily Hospitalization and ICU Counts", xaxis=list(title="Date"), yaxis=list(title="Counts"))
  
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
  #Gender Breakdown
  males <- max(age_sex['M'])
  females <- max(age_sex['F'])
  labels <- c('Male', "Female")
  values <-  c(males, females)
  gb_values_h <- as.numeric(genders$Hospitalizations)
  gb_values_ICU <- as.numeric(genders$ICU)
  gb_values_deaths <- as.numeric(genders$Deaths)

  gender_pies <- plot_ly()
  gender_pies <- gender_pies %>%
    add_pie(data = age_sex, labels = labels, values = values, name = "Total Cases", domain = list(row = 0, column = 0))
  gender_pies <- gender_pies %>%
    add_pie(data = genders, labels = labels, values = gb_values_h, name = "Hospitalizations", domain = list(row = 0, column = 1))
  gender_pies <- gender_pies %>%
    add_pie(data = genders, labels = labels, values = gb_values_ICU, name = "ICU", domain = list(row = 1, column = 0))
  gender_pies <- gender_pies %>%
    add_pie(data = genders, labels = labels, values = gb_values_deaths, name = "Deaths", domain = list(row = 1, column = 1))
  gender_pies <- gender_pies %>% layout(title = "Gender Breakdown", showlegend = TRUE,
                            grid=list(rows=2, columns=2),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  
  #AGE AND SEX PYRAMID CHART
  age <- rep(c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90-99', '100-'), times = 2)
  sex <- rep(c('Male', 'Female'), each = 10)
  tail <- tail(age_sex, 1)
  last <- as.numeric(tail[2:21])
  males <- last[seq(1,length(last),2)] * -1
  females <- last[seq(2, length(last),2)]
  pop <- c(males, females)
  df_pyramid <- data.frame(age, sex, pop)
  
  df_pyramid <- df_pyramid %>%
    mutate(abs_pop = abs(pop))
  
  df_pyramid <- df_pyramid %>% 
    plot_ly(x= ~pop, y=~age,color=~sex) %>% 
    add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop) %>%
    layout(title = "Confirmed Cases Grouped by Age Group, Separated by Gender", yaxis = list(title = "Age Group"), 
           bargap = 0.1, barmode = 'overlay',
           xaxis = list(
                      tickmode = 'array', 
                      tickvals = list(-200, -100, 0, 100, 200, 300),
                      ticktext = list('200', '100', '0', '100', '200', '300'), 
                      title = "Population"))
  
  #Age Breakdown
  labels_d <- c('0-17', '18-49', '50-64', '65-74', '75-84', '85+')
  values_d <- as.numeric(deaths$Deaths)
  values_h <- as.numeric(deaths$Hospitalized)
  values_ICU <- as.numeric(deaths$ICU.Admissions)
  values_cases <- as.numeric(deaths$Cases)
  donut <- plot_ly(data = deaths)
  donut <- donut %>%
    add_pie(labels = ~labels_d, values = ~values_d, name = "Deaths", domain = list(row = 0, column = 0))
  donut <- donut %>%
    add_pie(labels = ~labels_d, values = ~values_h, name = "Hospitalized", domain = list(row = 0, column = 1))
  donut <- donut %>%
    add_pie(labels = ~labels_d, values = ~values_ICU, name = "ICU Adm", domain = list(row = 1, column = 0))
  donut <- donut %>%
    add_pie(labels = ~labels_d, values = ~values_cases, name = "Conf Cases", domain = list(row = 1, column = 1))
  donut <- donut %>% layout(title = "Age Group Breakdown", showlegend = TRUE,
                        grid=list(rows=2, columns=2),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  
  
  #ANOTHER LINE CHART WITH DAILY TESTS COMPLETED, NEW POSITIVES, POSITIVE RATE, CUMULATIVE CFR (DOUBLE Y AXIS PERCENT ON RIGHT SIDE COUNT ON OTHER)
  summary_line <- plot_ly(general, x=~date, y=~new_cases, type='scatter', mode='lines', name = "Daily New Confirmed Cases (Left)", line = list(color = 'rgb(22, 96, 167)'))
  summary_line <- summary_line %>%
    add_trace(y=~daily_tests, name = "Daily Tests Completed (Left)", line = list(color = 'rgb(205, 12, 24)'))
  summary_line <- summary_line %>%
    add_trace(y=~percent_pos, name = "Cumulative Positivity Rate (Right)", line = list(color = 'rgb(205, 12, 24)', dash = 'dash'), yaxis = "y2")
  summary_line <- summary_line %>%
    add_trace(y=~death_rate, name = "Cumulative CFR (Right)", line = list(color = 'rgb(22, 96, 167)', dash = 'dash'), yaxis = "y2")
  summary_line <- summary_line %>%
    add_trace(y=~daily_PR, name = "Daily Positivity Rate (Right)", line = list(color = 'rgb(12, 200, 90)', dash = 'dash'), yaxis = "y2")
  summary_line <- summary_line %>%
    add_trace(y=~average_TR_3, name = "3-Day Average of Daily Tests (Left)", line = list(color = 'rgb(255,140,0)'))
  summary_line <- summary_line %>%
    layout(title = 'Monroe County Summary Stats', yaxis = list(title = "Count"), yaxis2 = list(overlaying = "y", side = "right", title = "Percentage"))
  
  #ANOTHER LINE CHART WITH DAILY GROWTH RATE OF POSITIVE CASES, DAILY GROWTH RATE OF DEATHS, DAILY GROWTH RATE OF ACTUAL INFECTIONS 
  #LAST 7 DAYS
  growth_rates <- plot_ly(general, x=~date, y=~percent_increase, type = 'scatter', mode = 'lines', name = "Daily GR of Positive Cases", line = list(color = 'rgb(22, 96, 167)'))
  growth_rates <- growth_rates %>%
    add_trace(y=~death_growth_rate, line = list(color = 'rgb(205, 12, 24)'), name = "Daily GR of Deaths")
  growth_rates <- growth_rates %>%
    layout(title = "Growth Rates of Positive Cases and Deaths", xaxis = list(title = "Date"), yaxis = list(title = "Percentage"))
  
  #Race/Ethnicity Breakdown
  # labels_r <- ethnicity$Race
  # r_values_c <- as.numeric(ethnicity$Cases)
  # r_values_h <- as.numeric(ethnicity$Hospitalizations)
  # r_values_ICU <- as.numeric(ethnicity$ICU)
  # r_values_d <- as.numeric(ethnicity$Deaths)
  # 
  # pies_r <- plot_ly(data = ethnicity)
  # pies_r <- pies_r %>%
  #   add_pie(labels = labels_r, values = r_values_c, name = "Total Cases", domain = list(row = 0, column = 0))
  # pies_r <- pies_r %>%
  #   add_pie(labels = labels_r, values = r_values_h, name = "Hospitalizations", domain = list(row = 0, column = 1))
  # pies_r <- pies_r %>%
  #   add_pie(labels = labels_r, values = r_values_ICU, name = "ICU", domain = list(row = 1, column = 0))
  # pies_r <- pies_r %>%
  #   add_pie(labels = labels_r, values = r_values_d, name = "Deaths", domain = list(row = 1, column = 1))
  # pies_r <- pies_r %>% layout(title = "Race/Ethnicity Breakdown", showlegend = TRUE,
  #                           grid=list(rows=2, columns=2),
  #                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


sb_r <- plot_ly(props_e, x = ~Category, y = ~White, type = 'bar', name = "White")
sb_r <- sb_r %>%
  add_trace(y = ~African.American, name = "African American")
sb_r <- sb_r %>%
  add_trace(y = ~Latinx, name = "Latinx")
sb_r <- sb_r %>%
  add_trace(y = ~Asian, name = "Asian")
sb_r <- sb_r %>%
  add_trace(y = ~Other, name = "Other")
sb_r <- sb_r %>%
  add_trace(y = ~Unknown, name = "Unknown")
sb_r <- sb_r %>%
  layout(barmode = 'stack', yaxis = list(title = 'Percentage'), title = "Ethnicity Breakdown by Percentage")


#render plots and connect them to drop-down menu choices
  output$plot <- renderPlotly({
    temp <- switch(input$select,
                   "Death and Total Cases" = fa,
                   "Cumulative Cases Semi Log" = semi_log,
                   "Hospitalizations and ICU" = hosp_icu,
                   "Summary Stats" = summary_line, 
                   "Gender Breakdown" = gender_pies,
                   "Age and Sex Breakdown" = df_pyramid, 
                   "Growth Rates" = growth_rates,
                   "Bar Chart of Active Cases" = sb,
                   "Active Cases by Proportions" = csa_active,
                   "Age Group Breakdown" = donut,
                   "Race/Ethnicity Breakdown" = sb_r)
  })
  
  #throw warning only if data is not up to date
  observeEvent(input$select, {
    if (input$select == "Age Group Breakdown") {
      showNotification("Warning: This plot may not be up to date", type = "error", closeButton = TRUE, duration = 10)
    }
  })
  
  #Table Outputs
  #Change column names so they are more readable
  general_app <- general
  colnames(general_app) <- c("Date", "Total Confirmed Cases", "New Cases", "% Inc Confirmed Cases", "Total Tests Run", "Daily Tests Run", "3-Day Average of Daily Tests",
                             "% Inc Total Tests Run", "Total Positive Cases", "Cummulative Positivity Rate", "Daily Pos. Rate", "Current Hospitalized",
                             "Total Deaths", "New Deaths", "CFR", "% Inc in Deaths", "Currently in ICU", "% Hosp. in ICU", "Active Cases", "Unhosp. Active Cases",
                             "% of Active Cases Hosp.", "% of Active Cases in ICU", "Active Cases Hosp. (Not in ICU)")
  #reorder data frame from most to least recent
  df <- general_app %>%
    map_df(rev)
  
  #Change date type to character type to renderTable not correctly displaying dates
  df$Date <- as.character(df$Date)
  
  #render general data table
  output$table <- renderDataTable(df)
 

  #change column names for readability again
  age_sex_app <- age_sex
  colnames(age_sex_app) <- c('Date', '0-19(M)', '0-19(F)', '20-29(M)', '20-29(F)', '30-39(M)', '30-39(F)', '40-49(M)', 
                             '40-49(F)', '50-59(M)', '50-59(F)', '60-69(M)', '60-69(F)', '70-79(M)', '70-79(F)', '80-89(M)', 
                             '80-89(F)', '90-99(M)', '90-99(F)', '100+(M)', '100+(F)', 'Total Male Conf. Cases', 'Total Female Conf. Cases',
                             '% Male (of total conf. cases)', '% Female (of total conf. cases')
  
  #reorder dataframe from most to least recent
  df2 <- age_sex_app %>%
    map_df(rev)

  #Change date type to character type to renderTable not correctly displaying dates
  df2$Date <- as.character(df2$Date)
  
  #render age and sex data table
  output$table2 <- renderDataTable(df2)
  
  #get RSS feed to populate local health updates tab
  myquery <- feed.extract("https://www.urmc.rochester.edu/news/rss/feed.aspx?serviceline=93&topX=10&source=URMC,Highland,Noyes")
  
  #put RSS feed info into a data frame
  updates <- data.frame(myquery$items)
  
  #select only the informative columns
  updates <- updates %>%
    select("title", "date", "link")
  
  #change those columns' names
  colnames(updates) <- c('Title', 'Date', 'Link')
  
  
  #get the dates into a normal format
  updates$Date <- as.character(updates$Date)
  
  #make links hyperlinks
  updates$Link <- paste0("<a href='", updates$Link, "' target = '_blank'>", updates$Link, "</a>")
  
  #render local health updates data table
  output$table3 <- DT::renderDataTable({
    DT::datatable(updates[, , drop = FALSE], escape = FALSE)
  })
  
  #**************************#
  #get RSS feed to populate local policy updates tab
  myquery_p <- feed.extract("https://www.monroecounty.gov/topstory-news.rss")
  
  #put RSS feed info into a data frame
  updates_p <- data.frame(myquery_p$items)
  
  #select only the informative columns
  updates_p <- updates_p %>%
    select("title", "date", "link")
  
  #change those columns' names
  colnames(updates_p) <- c('Title', 'Date', 'Link')
  
  #get the dates into a normal format
  updates_p$Date <- as.character(updates_p$Date)
  
  #make links hyperlinks
  updates_p$Link <- paste0("<a href='", updates_p$Link, "' target = '_blank'>", updates_p$Link, "</a>")
  
  #render local updates data table
  output$table4 <- DT::renderDataTable({
    DT::datatable(updates_p[, , drop = FALSE], escape = FALSE)
  })
  
  #****************************#
  
  #make the links hyperlinks
  link$Link <- paste0("<a href='", link$Link, "' target = '_blank'>", link$Link, "</a>")
  
  #render data table
  output$table5 <- DT::renderDataTable({
    DT::datatable(link[, , drop = FALSE], escape = FALSE)
  })
}

shinyApp(ui = ui, server = server)