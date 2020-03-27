## assemble our covid data set from various sources

library(data.table)
dummy <- lapply( dir("R",pattern="R$",full.names=TRUE), source )

## load global data from data.humdata.org:
covid <- load.humdata.data()

## merge JHU data for US States and Territories. JHU has some data
##  about municipalities, but they stop early and we discard them.
jhu <- load.jhu.data()
data(state)
states.and.territories <- c(
    state.name,
    "Puerto Rico",
    "Virgin Islands, U.S.",
    "United States Virgin Islands",
    "Virgin Islands",
    "American Samoa",
    "Guam",
    "Northern Mariana Islands",
    "District of Columbia"
)
us <- jhu[
    Subregion %in% states.and.territories &
    Region    ==   "US"
]
us[
    grep("Virgin Islands",Subregion),
    Subregion := "Virgin Islands"
]
covid <- rbind( covid, us )

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

## merge New York and New York City data
ny.nyc <- load.ny.nyc.data()
ny.days <- ny.nyc[ Subregion=="New York", unique(Day) ]
ny.what <- ny.nyc[ Subregion=="New York", unique(What) ]
covid <- covid[
    ! (
        Subregion ==   "New York" &
        Day       %in% ny.days    &
        What      %in% ny.what
    )
]
covid <- rbind( covid, ny.nyc ) 

## done!
fwrite( covid, "covid.csv" )
