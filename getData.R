##

library(data.table)
library(zoo)
source("covid.R")

## jhu csse data
confirmed <- load.jhu.csse.data("Confirmed")
deaths <- load.jhu.csse.data("Deaths")

## official Italian data
italy <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
## retain day, drop time:
italy[, data := as.Date( data, format="%Y-%m-%d" ) ]

## the official Italian data are more accurate but do not cover the
## whole date range in the jhu data. here we reshape the Italian data
## in the same format and we merge them.

## confirmed cases data
italy.confirmed <-
    italy[,
          .(
              rep("All",.N),
              rep("Italy",.N),
              43,
              12,
              data,
              totale_casi
          )
          ]
setnames(
    italy.confirmed,
    c("V1","V2","V3","V4","data","totale_casi"),
    c("Subregion","Region","Lat","Long","Day","Count")
)

## deaths data
italy.deaths <-
    italy[,
          .(
              rep("All",.N),
              rep("Italy",.N),
              43,
              12,
              data,
              deceduti
          )
          ]
setnames(
    italy.deaths,
    c("V1","V2","V3","V4","data","deceduti"),
    c("Subregion","Region","Lat","Long","Day","Count")
)

## delete Italian data from JHU when we have the official data, then
## merge with the official data:

confirmed <- confirmed[
    ! (Region=="Italy" & Day %in% italy.confirmed$Day)
        ]
confirmed <- rbind(
    confirmed,
    italy.confirmed
)

deaths <- deaths[
    ! (Region=="Italy" & Day %in% italy.deaths$Day)
        ]
deaths <- rbind(
    deaths,
    italy.deaths
)

fwrite( confirmed, "covid-confirmed.csv" )
fwrite( deaths, "covid-deaths.csv" )
