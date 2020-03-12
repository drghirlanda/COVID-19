library(data.table)
library(zoo)

source("covid.R")

deaths <- load.covid( "Deaths" )
confirmed <- load.covid( "Confirmed" )

us.confirmed.tot <- get.totals(
    confirmed[ Region=="US" & Day > "2020-03-01"
              ]
)
us.confirmed.fit <- exp.fit( us.confirmed.tot )
exp.plot( us.confirmed.fit, npred=7 )

us.deaths.tot <- get.totals(
    deaths[ Region=="US" & Day > "2020-03-01"
           ]
)
us.deaths.fit <- exp.fit( us.deaths.tot )
exp.plot( us.deaths.fit, npred=7, add=TRUE, col=2 )

ny.confirmed.tot <- get.totals(
    us.confirmed[ grep(", NY",Subregion,fixed=TRUE) ]
)
ny.confirmed.fit <- exp.fit( ny.confirmed.tot )

exp.plot( ny.confirmed.fit, npred=1 )

nyc.confirmed <- confirmed[
    Subregion %in%
    c(
        "New York County, NY",
        "Kings County, NY",
        "Queens County, NY",
        "Richmond County, NY",
        "Bronx County, NY"
    )
]

nyc.confirmed.tot <- get.totals( nyc.confirmed )
nyc.confirmed.fit <- exp.fit( nyc.confirmed.tot )
exp.plot( nyc.confirmed.fit, npred=1 )
