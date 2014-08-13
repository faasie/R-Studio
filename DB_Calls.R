library('fImport')
library('DBI')
library('RMySQL')
source("db_functions.R")

# Get date
today <- format(Sys.time(), "%F")

# Define DB Connects
clever <- dbConnect(MySQL(), user="root", password="R@^scalK1ng", dbname='Clever401k', host='localhost')

companies <- dbGetQuery(clever, "select company_id, company_name from company")

c_id <- companies$company_id

i <- 2
# for(i in c_id) {
  symbols <- get_plans(i) #returns data.frame
  symbols <- symbols$symbol # convert to list
  d <- yahooSeries(symbols, "2005-01-01", frequency="monthly" )
# }  

dbDisconnect(clever)
