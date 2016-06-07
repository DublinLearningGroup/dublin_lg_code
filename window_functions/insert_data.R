library(data.table)
library(RPostgreSQL)

data_dt <- fread("train.csv")

setnames(data_dt, c("store","dayofweek","datadate","sales","customers","open","promo","stateholiday","schoolholiday"))

data_id[, id            := .I]
data_dt[, datadate      := as.Date(datadate)]
data_dt[, schoolholiday := (schoolholiday == 1)]

data_dt <- data_dt[, .(id,store,dayofweek,datadate,sales,customers,open,promo,stateholiday,schoolholiday)]

dbconnect <- dbConnect(dbDriver("PostgreSQL")
                      ,host   = 'localhost'
                      ,dbname = 'windowdb'
                      ,user   = 'lguser'
                      ,pass   = 'lguser')

dbWrite(dbconnect
       ,name      = 'storedata'
       ,value     = data_dt
       ,row.names = FALSE
       ,append    = TRUE)
