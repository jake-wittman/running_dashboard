
# Deploy app
#rsconnect::deployApp(here::here("Runnr"), account = "jake-wittman")

# Libraries ---------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")

# install.packages("googlesheets")
# install.packages("shiny")
# install.packages("tidyverse")

library(googlesheets)
library(shiny)
library(tidyverse)
library(kableExtra)
library(conflicted)
library(lubridate)
conflicted::conflict_prefer("filter", "dplyr")


# Import data from googledrive --------------------------------------------
# see all google sheets I have access to
# gs_ls() 
# Import old running log
running <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ6WOA3AOFScjC94zIYvHhjCz20iWZDclAD8NJZyTlLJKYmsTWnsJiYvHqnhdRIELgovhujvIwNEUQv/pub?gid=0&single=true&output=csv")
# Import newer running log
running2 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRD-AMfL1-tco3MorSqva8nmAgkjNnd-Ujhy4vdaPs5bl9rSdiSPTqb_1_PyXe7DQQVw4eSddQJSaTb/pub?gid=1481357316&single=true&output=csv")

# Clean up columns
running <- running[, -16] # remove unused column
running$avg_hr <- as.numeric(running$avg_hr)

# Rename variables
names(running) <- c("date",
                    "time",
                    "distance.miles",
                    "avg.heart.rate",
                    "pace",
                    "notes", 
                    "training.cycle",
                    "easy",
                    "long",
                    "interval",
                    "threshold",
                    "recovery",
                    "hard",
                    "race", 
                    "cross.training")

running <- running %>% 
  mutate(run.type = case_when(easy == 1 ~ "easy",
                              long == 1 ~ "long",
                              interval == 1 ~ "vo2_max",
                              threshold == 1 ~ "lactate_threshold",
                              recovery == 1 ~ "recovery",
                              hard == 1 ~ "vo2_max",
                              race == 1 ~ "race"))
running <- running %>% 
  select(date:pace, run.type)
  
names(running2) <- c("date",
                     "distance.miles",
                     "time",
                     "avg.heart.rate",
                     "shoe",
                     "run.type",
                     "notes",
                     "running.partner",
                     "partner.name",
                     "time.stamp")

running$shoe <- NA

running_all <- bind_rows(running, running2)
running <- running_all
                     
# Convert time variable to seconds for plotting ease
# Convert time of run into total number of seconds
nruns <- length(running$time)
time <- as.character(running$time) # total time needs to be as character to seperate minutes and seconds
time <- matrix(time, nrow = nruns, ncol = 1) # turn variable time into a matrix
min <- matrix(NA, nrow = nruns, ncol = 1) # empty matrix to hold conversion of minutes to seconds
for (i in 1:nruns){ # for loop to convert minutes into seconds from time matrix
   if (nchar(time[i]) == 8){
      min[i]<-substr(time[i], 1, 2)
   } else if (nchar(time[i]) == 9){
      min[i]<-substr(time[i], 1, 3)
   } else if (nchar(time[i]) == 7){
      min[i]<-substr(time[i], 1, 1)
   } else if (nchar(time[i]) == 4){
      min[i]<-substr(time[i], 1, 1)
   } else if (nchar(time[i]) == 5){
      min[i]<-substr(time[i], 1, 2)
   }
}
sec <- matrix(NA, nrow = nruns, ncol = 1) # empty matrix to hold the seconds from the time matrix
for (i in 1:nruns){
   if (nchar(time[i]) == 8){
      sec[i]<-substr(time[i], 4, 5)
   } else if (nchar(time[i]) == 9){
      sec[i]<-substr(time[i], 5, 6)
   } else if (nchar(time[i]) == 7){
      sec[i]<-substr(time[i], 3, 4)
   } else if (nchar(time[i]) == 4){
      sec[i]<-substr(time[i], 3, 4)
   } else if (nchar(time[i]) == 5){
      sec[i]<-substr(time[i], 4, 5)
   }
}
min <- as.numeric(min) # set minute and seconds back to numeric for arithemtic
sec <- as.numeric(sec)
time.sec <- (min * 60) + sec # total number of seconds run
dist <- matrix(running$distance.miles, nrow = nruns, ncol = 1) # matrix to hold distance values

running$pace.sec <- time.sec / dist # matrix to hold pace in seconds per mile


# Shoe info
retired_shoes <- c("saucony_red",
                   "netwon_kimset_orange",
                   "kinvara_7_green",
                   "2019_10_freedom")
shoe_miles_table <- running %>% 
  group_by(shoe) %>% 
  summarise(sum = sum(distance.miles)) %>% 
  filter(!is.na(shoe)) %>% 
   mutate(active = case_when(
      shoe %in% retired_shoes ~ 0,
      TRUE ~ 1
   )) %>% 
   filter(active == 1) %>% 
   select(-active)

running$hr.zone <- cut(running$avg.heart.rate,
                       breaks = c(-Inf, 137, 146, 158, 164, 177),
                       labels = c("Recovery","Easy","Med/Long/MP", "Lac_Thresh","Interval"))

y_axis_pace <- c("5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00") 
y_ticks_pace <- c(seq(300, 720, 60))

# Clean dates and get weekly/monthly stuff
running$date <- as.Date(running$date, "%m/%d/%Y")
running$week <- as.Date(cut(running$date, breaks = "week", start.on.monday = T)) 
running$month <- as.Date(cut(running$date, breaks = "month")) # create a column to identify each month
running$month <- format(running$month, "%Y-%m")
running <- transform(running, uid.week = as.numeric(factor(week))) # give each week a unique identifier
running <- transform(running, uid.month = as.numeric(factor(month))) # give each week a unique identifier
week.id <- running$uid.week
month.id <- running$uid.month
weekly.totals <- as.data.frame(matrix(nrow = length(unique(running$week)), ncol = 4))
monthly.totals <- as.data.frame(matrix(nrow = length(unique(running$month)), ncol = 3))
colnames(weekly.totals) <- c("week.start", "week.end", "total.miles", "n.runs")
colnames(monthly.totals) <- c("month.start", "total.miles", "n.runs")

#calculate weekly totals
for (i in week.id) {
   temp.dat <- running[running$uid.week == i, ]
   weekly.totals[i, "total.miles"] <- sum(temp.dat$distance.miles)
   weekly.totals[i, "week.start"] <- as.character(temp.dat$week[1])
   weekly.totals[i, "week.end"] <- as.character(temp.dat$week[1] + 6)
   weekly.totals[i, "n.runs"] <- nrow(temp.dat)
}

#calculate monthly totals
for (i in month.id) {
   temp.dat <- running[running$uid.month == i, ]
   monthly.totals[i, "total.miles"] <- sum(temp.dat$distance.miles)
   monthly.totals[i, "month.start"] <- as.character(temp.dat$month[1])
   monthly.totals[i, "n.runs"] <- nrow(temp.dat)
}

weekly.totals <- weekly.totals %>% 
   mutate(week.start = as_date(week.start),
          week.end = as_date(week.end))

   

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

