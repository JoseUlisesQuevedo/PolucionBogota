### Helper functions: functions used in the ReportGeneratorFile

### Reads dataframe with a "date" column, returns 
### only rows from "reference_date" and 30 days back
    #reference_date := date as a string "Y%-m%-d%"
get_last_30_days <- function(data,reference_date){
  target_date <- as.Date(reference_date) - days(30)
  data %>% filter(date >= target_date)
}