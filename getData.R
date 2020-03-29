## assemble our covid data set from various sources

suppressPackageStartupMessages({
    library(data.table)
    dummy <- lapply( dir("R",pattern="R$",full.names=TRUE), source )
})

## load global data from data.humdata.org:
message( "loading world data" )
covid <- load.humdata.data()

## load New York Times data for US states and counties
message( "loading US data" )
us <- load.us.states()
covid <- rbind( covid, us )
us.c <- load.us.counties()
us.c <- us.c[ ! grep("Unknown", Reg2) ]
covid <- rbind( covid, us.c )

## merge official Italian data.  we keep the world data when no
## official Italian data are available.
message( "loading Italian data" )
italy <- load.italian.data()
days <- unique( italy$Day )
covid <- covid[
    ! (Reg1=="Italy" &
       Day %in% days)
]
covid <- rbind( covid, italy )

## merge official French data
message( "loading French data" )
fr <- load.french.data()
covid <- covid[ ! Reg1=="France" ]
covid <- rbind( covid, fr )

## merge Spanish data
message( "loading Spanish data" )
sp <- load.spanish.data()
days <- sp[ Reg2=="All", Day ]
covid <- covid[ ! (Reg1=="Spain" & Day %in% days) ]
covid <- rbind( covid, sp )

## save a sorted data.table so that we don't have to sort in app
message( "sorting data" )
covid[
    What=="Fatalities",
    maxCount := max(Count),
    by=.(Reg1,Reg2,Reg3)
]
covid[ is.na(maxCount), maxCount := 0 ]
setorder( covid, -maxCount, -Count )
covid[, maxCount := NULL ]

## just in case we missed this somewhere:
covid <- covid[ Count>0 ]

## done!
message( "saving" )
fwrite( covid, "covid.csv" )
