## assemble our covid data set from various sources

library(data.table)
dummy <- lapply( dir("R",pattern="R$",full.names=TRUE), source )

## load global data from data.humdata.org:
covid <- load.humdata.data()

## load New York Times data for US states and counties
us <- load.us.states()
covid <- rbind( covid, us )
us.c <- load.us.counties()
us.c <- us.c[ ! grep("Unknown", Subregion) ]
covid <- rbind( covid, us.c[ Subregion=="New York City, NY" ] )

## merge official Italian data.  we keep the world data when no
## official Italian data are available.
italy <- load.italian.data()
days <- unique( italy$Day )
covid <- covid[
    ! (Region=="Italy" &
       Day %in% days)
]
covid <- rbind( covid, italy )

## merge official French data
fr <- load.french.data()
covid <- covid[ ! Region=="France" ]
covid <- rbind( covid, fr )

## merge Spanish data
sp <- load.spanish.data()
days <- sp[ Subregion=="All", Day ]
covid <- covid[ ! (Region=="Spain" & Day %in% days) ]
covid <- rbind( covid, sp )

## done!
fwrite( covid, "covid.csv" )
