##

library(data.table)
library(zoo)
source("covid.R")

## jhu csse data
confirmed <- load.jhu.csse.data("Confirmed")
deaths <- load.jhu.csse.data("Deaths")
deaths$What <- "Fatalities"

covid <- rbind( confirmed, deaths )

## official Italian data
italy <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
## retain day, drop time:
italy[, data := as.Date( data, format="%Y-%m-%d" ) ]

## the official Italian data are more accurate but do not cover the
## whole date range in the jhu data. here we reshape the Italian data
## in the same format and we merge them.

italy.what <- list(
    totale_casi="Confirmed",
    deceduti="Fatalities",
    ricoverati_con_sintomi="In Hospital",
    terapia_intensiva="In ICU"
)

for( what in names(italy.what) ) {
    italy.subset <-
        italy[,
              .(
                  rep("All",.N),
                  rep("Italy",.N),
                  43,
                  12,
              data,
              get(what)
              )
              ]
    setnames(
        italy.subset,
        c("V1","V2","V3","V4","data","V6"),
        c("Subregion","Region","Lat","Long","Day","Count")
    )
    italy.subset$What <- italy.what[[ what ]]
    ## delete this subset if already present in covid data
    days <- unique( italy.subset$Day )
    covid <- covid[
        ! (Region=="Italy" &
           What==italy.what[[ what ]] &
           Day %in% days)
    ]
    ## merge Italian data
    covid <- rbind( covid, italy.subset )
}


fwrite( covid, "covid.csv" )
