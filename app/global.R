# Load environment variables
# Only load .env file if not running on Cloud Run (K_SERVICE is set by Cloud Run)
if (Sys.getenv("K_SERVICE") == "" && file.exists(".env")) {
  dotenv::load_dot_env()
}
# Set R_CONFIG_ACTIVE to prod (can be overridden by environment variable)
if (Sys.getenv("R_CONFIG_ACTIVE") == "") {
  Sys.setenv(R_CONFIG_ACTIVE = "prod")
}

library(shiny)
library(shinyBS)
library(bs4Dash)
library(fresh)
library(DT)
library(DBI)
library(pool)
library(dplyr)
library(shinyjs)
library(glue)
library(spsComps)
library(rutifier)
library(shinyvalidate)
library(readxl)
library(janitor)
library(highcharter)
library(tidyr)
library(stringr)
library(shinyalert)
library(dbplyr)
library(shinydisconnect) 
library(sendmailR)
library(auth0)
library(shinycssloaders)
library(lubridate)
library(waiter)
library(gt)
library(rmarkdown)
library(kableExtra)
library(knitr)
library(openxlsx)
library(bslib)
library(rvest)
# library(shinyWidgets)
library(base64enc)

# Get config based on R_CONFIG_ACTIVE environment variable
env <- config::get()

db <- env$MySQL_DB
print(paste("from global: ", db))
a0_info <- auth0::auth0_info()

# Default Telemetry with data storage backend using MariaDB
# data_storage <- DataStorageLogFile$new(
#   log_file_path = "./telemetry_storage/user_stats.txt"
# )

# telemetry <- Telemetry$new(
#   app_name = "c3d_test",
#   # data_storage = DataStorageMariaDB$new(
#   #   user = env$MySQL_USER, password = env$MySQL_PASS, hostname = env$MySQL_HOST, port = as.integer(env$MySQL_PORT), dbname = env$MySQL_DB
#   # )
#   data_storage = data_storage
#   # data_storage = DataStorageSQLite$new(
#   #   db_path = "./telemetry_storage/events.sqlite"
#   # )
# )

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = env$MySQL_DB,
  host= env$MySQL_HOST,
  port= as.integer(env$MySQL_PORT),
  user= env$MySQL_USER,
  password= env$MySQL_PASS,
  minSize = 1,
  idleTimeout = 60*60,
  # encoding = "UTF-8"
  encoding = "windows-1252"
)

tryCatch({
  conn <- poolCheckout(pool)
  if (!dbIsValid(conn)) {
    message(paste0("Connection to ", db, " has failed"))
    stop("Connection to the database failed!")
  } else {
    message(paste0("Connection to ", db, " was successful!"))
  }
  poolReturn(conn) # Return the connection to the pool
}, error = function(e) {v 
  message("An error occurred: ", e$message)
})

onStop(function() {
  # events <- data_storage$read_event_data()
  # dbExecute(pool, 'SET character set "utf8"')
  # query <- sqlAppendTable(pool, "event_log", events, row.names = FALSE)
  # result <- dbExecute(pool, query)
  # message("Events saved in DB")
  poolClose(pool)
})