# Run from the project root with Rscript tests/check_dashboard.R.
# Integration checks load the two published running logs.
options(error = function() { traceback(20); quit(status = 1) })
lines <- readLines("dashboard.qmd", warn = FALSE)
starts <- grep("^```\\{r\\}", lines)
ends <- grep("^```$", lines)
chunk <- function(start) paste(lines[seq.int(start + 1L, min(ends[ends > start]) - 1L)], collapse = "\n")
eval(parse(text = chunk(starts[1])))

stopifnot(identical(
  parse_run_date(c("8/8/2011", "9/10/2011", "01/11/2023", "7/27/24", "2024-07-27")),
  as.Date(c("2011-08-08", "2011-09-10", "2023-01-11", "2024-07-27", "2024-07-27"))
))
stopifnot(all(is.na(suppressWarnings(parse_run_date(
  c("1/11/0023", "3/30/0204", "invalid", "", NA_character_, "2/30/2024")
)))))
stopifnot(inherits(running$date, "Date"), inherits(weekly_totals$week_start, "Date"))

server <- function(input, output, session) {
  eval(parse(text = chunk(tail(starts, 1))))
}
shiny::testServer(server, {
  session$setInputs(dates = range(running$date, na.rm = TRUE), max_distance = 30,
                    pace_plot = character(), x_axis = "date", y_axis = "pace_sec")
  stopifnot(is.list(output$weekly_mileage_plot), is.list(output$pace_date_plot))
  session$setInputs(dates = as.Date(c("2024-01-01", "2024-12-31")), pace_plot = "facet")
  stopifnot(is.list(output$pace_date_plot))
  session$setInputs(pace_plot = "smooth")
  stopifnot(is.list(output$pace_date_plot))
})
cat("Dashboard date parsing and live plot checks passed.\n")
