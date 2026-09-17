
# Deploy app
#rsconnect::deployApp(here::here("Runnr"), account = "jake-wittman")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("My Running Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateRangeInput("dates",
                     "Date range to graph",
                     start = min(running$date),
                     end = max(running$date))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("pace_date_plot"),
         plotOutput("pace_hronly_date_plot"),
         tableOutput("shoe_mileage"),
         tableOutput("weekly_mileage_table"),
         tableOutput("monthly_mileage_table")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$pace_date_plot <- renderPlot({
      all.runs.paces <- running %>% 
         filter(date >= input$dates[1] & date <= input$dates[2]) %>% 
         ggplot(aes(x = date, y = pace.sec, colour = factor(hr.zone)))
      
      all.runs.paces +
         theme_bw() +
         geom_point() +
         scale_x_date(date_labels = "%b-%y", date_breaks = "24 weeks") +
         scale_y_continuous(breaks = y_ticks_pace,
                            label = y_axis_pace,
                            limits = c(min(y_ticks_pace),
                                       max(y_ticks_pace))) +
         ylab("Pace (min/mile)") +
         theme(text = element_text(size = 18),
               axis.text.x = element_text(angle = 70, hjust = 1)) +
         labs(color = "Heart Rate Zone")
   })
   
   output$pace_hronly_date_plot <- renderPlot({
      hr_only_running <- running %>%
         filter(date >= input$dates[1] & date <= input$dates[2],
                !is.na(avg.heart.rate)) %>% 
         mutate(hr.zone = as.factor(hr.zone))
      
      hr_only_running %>%
         ggplot(aes(x = date, y = pace.sec)) +
         theme_bw() +
         geom_point(aes(colour = hr.zone)) +
         scale_x_date(date_labels = "%b-%y", date_breaks = "8 weeks") +
         scale_y_continuous(breaks = y_ticks_pace,
                            label = y_axis_pace,
                            limits = c(min(y_ticks_pace),
                                       max(y_ticks_pace))) +
         xlab("Date") +
         ylab("Pace (min/mile)") +
         theme(text = element_text(size = 18),
               axis.text.x = element_text(angle = 70, hjust = 1)) +
         labs(color = "Heart Rate Zone")
   })
   
   output$shoe_mileage <- function() {
      shoe_miles_table %>% 
         arrange(sum) %>% 
         knitr::kable(format = "html",
                      col.names = c("Shoe", "Total miles"),
                      caption = "Miles run in each pair of shoes") %>% 
         kable_styling("striped")
   }
   
   output$weekly_mileage_table <- function() {
      tail(weekly.totals, n = 9) %>%
         knitr::kable(
            format = "html",
            col.names = c("Start of week",
                          "End of week",
                          "Weekly Miles",
                          "Number of Runs"),
            caption = "Weekly mileage for the current week and the last 8 weeks"
         ) %>% 
         kable_styling("striped")
   }
   
   output$monthly_mileage_table <- function() {
      library(knitr)
      monthly.totals %>%
         mutate(
            month.start = parse_date_time(month.start, "ym"),
            Month = month(month.start, label = TRUE, abbr = TRUE),
            Year = year(month.start)
         ) %>%
         select(Year, Month, total.miles, n.runs) %>%
         slice_tail(n = 12) %>%
         kable(
            format = "html",
            col.names = c("Year",
                          "Month",
                          "Total Miles",
                          "Number of Runs"),
            caption = "Monthly mileage for the year"
         ) %>%
         kable_styling("striped")
   }
}

# Run the application 
shinyApp(ui = ui, server = server)

