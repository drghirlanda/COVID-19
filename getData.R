## assemble data set from various sources 
library(data.table)
dummy <- lapply( dir("R",pattern="R$",full.names=TRUE), source )

## global data

## ugly but correct
confirmed <- fread("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv")
confirmed$What <- "Confirmed Cases"

deaths <- fread("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv" )
deaths$What <- "Fatalities"

covid <- rbind( confirmed, deaths )
setnames(
    covid,
    c("Province/State","Country/Region","Date","Value"),
    c("Subregion",     "Region",        "Day", "Count")
)
covid$Day <- as.Date( covid$Day, format="%F" )
covid$Count <- as.numeric( covid$Count )
## discard some header information
covid <- covid[ ! grep("#",Region) ]
## replace empty subregion with All
covid[ Subregion=="", Subregion := "All" ]
## discard Count==0
covid <- covid[ Count>0 ]

## discard Long and Lat
covid[ , Long := NULL ]
covid[ , Lat  := NULL ]

## for US states, we use JHU data
jhu <- load.jhu.data()
data(state)
us <- jhu[
    Region=="US" &
    Subregion %in%
    c(
        state.name,
        "Puerto Rico",
        "Virgin Islands, U.S.",
        "United States Virgin Islands",
        "Virgin Islands",
        "American Samoa",
        "Guam",
        "Northern Mariana Islands",
        "District of Columbia"
        ) ]

us[
    Subregion %in%
    c("Virgin Islands, U.S.", "United States Virgin Islands"),
    Subregion := "Virgin Islands"
]

us <- us[ Count>0 ]

covid <- rbind( covid, us )

## official Italian data
## national-level data
italy <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
italy <- italy[, .(data,ricoverati_con_sintomi,terapia_intensiva,deceduti,totale_casi) ]
italy$Region <- "Italy"
italy$Subregion <- "All"
## region-level data
italy.reg <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
italy.reg <- italy.reg[, .(denominazione_regione,data,ricoverati_con_sintomi,terapia_intensiva,deceduti,totale_casi) ]
italy.reg$Region <- "Italy"
setnames( italy.reg, "denominazione_regione", "Subregion" )

italy <- rbind( italy, italy.reg )
setnames(
    italy,
    c("data","ricoverati_con_sintomi","terapia_intensiva","deceduti","totale_casi"),
    c("Day","In Hospital","In ICU","Fatalities","Confirmed Cases")
)

## retain day, drop time:
italy[, Day := as.Date( Day, format="%Y-%m-%d" ) ]

italy <- melt( italy, id.vars=c("Subregion","Region","Day") )
setnames( italy, c("variable","value"), c("What","Count") )

## the official Italian data are more accurate but do not cover the
## whole date range in the jhu data. we keep the jhu data when no
## official Italian data are available.
days <- unique( italy$Day )
covid <- covid[
    ! (Region=="Italy" &
       Day %in% days)
]
## merge Italian data
covid <- rbind( covid, italy )

## official French data
fr.raw <- fread("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv")
 fr <- rbind(
    fr.raw[ maille_code=="FRA" ],
    fr.raw[ grep("REG-",maille_code) ]
)

setnames( fr, c("date","maille_nom"), c("Day","Subregion") )
fr[ Subregion=="France", Subregion := "All" ]
fr[, Region := "France" ]

## fix some inconsistencies in names
fr[ grep("Auvergne",Subregion), Subregion := "Auvergne-Rhone-Alpes" ]
fr[ grep("Grand",Subregion), Subregion := "Grand Est" ]
fr[ grep("Bourgogne",Subregion), Subregion := "Bourgogne-Franche-Comte" ]
fr[ grep("Provence",Subregion), Subregion := "Provence-Alpes-Cote d'Azur" ]
fr[ grep("Centre",Subregion), Subregion := "Centre-Val de Loire" ]
fr[ grep("La R",Subregion), Subregion := "La Reunion" ]

fr <- fr[, .(Subregion,Region,Day,cas_confirmes,deces) ]

fr.c <- fr[, .(Subregion,Region,Day,cas_confirmes) ]
setnames( fr.c, "cas_confirmes", "Count" )
fr.c$What <- "Confirmed Cases"
fr.d <- fr[, .(Subregion,Region,Day,deces) ]
setnames( fr.d, "deces", "Count" )
fr.d$What <- "Fatalities"

fr <- rbind( fr.c, fr.d )

fr <- fr[ complete.cases(fr) ]

## get rid of some inconsistencies and duplications
fr <- fr[, mean(Count), by=.(Subregion,Region,Day,What) ]
setnames( fr, "V1", "Count" )

fr$Day <- as.Date( fr$Day )

covid <- covid[ ! Region=="France" ]
covid <- rbind( covid, fr )

## Spanish data

sp.names <- c(
    casos="Confirmed Cases",
    fallecidos="Fatalities",
    uci="In ICU"
)
sp <- NULL
for( sp.what in names(sp.names) ) {
    dt <- fread(paste0("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_",sp.what,".csv"))
    dt <- melt( dt, id.vars="CCAA" )
    dt <- dt[ variable!="cod_ine" ]
    setnames(
        dt,
        c("CCAA","variable","value"),
        c("Subregion","Day","Count")
    )
    dt[ Subregion=="Total", Subregion := "All" ]
    dt[ , Region := "Spain" ]
    dt[ , What := sp.names[[sp.what]] ]
    dt$Day <- as.Date( dt$Day )
    sp <- rbind( sp, dt )
}
## normalize region names
sp[ grep("Andaluc",Subregion), Subregion := "Andalucia" ]
sp[ grep("Arag",Subregion), Subregion := "Aragon" ]
sp[ grep("Castilla",Subregion), Subregion := "Castilla y Leon" ]
sp[ grep("Catalu",Subregion), Subregion := "Cataluna" ]
sp[ grep("Vasco",Subregion), Subregion := "Pais Vasco" ]

days <- sp[ Subregion=="All", Day ]

covid <- covid[ ! (Region=="Spain" & Day %in% days) ]
covid <- rbind( covid, sp )

## New York City data

ny <- fread("https://docs.google.com/spreadsheets/d/1ixC6SxKs_4wYBPpsqwVAaihFEHj8SAN-3hIC2dluWrg/export?format=csv")
ny <- melt( ny, id.vars="Location" )
setnames( ny, c("variable","value"), c("Day","Count") )
ny$Day <- as.Date( ny$Day, format="%F" )
ny$Count <- as.numeric( sub( ",", "", ny$Count, fixed=TRUE ) )
ny <- ny[, mean(Count), by=.(Location,Day) ]
setnames( ny, "V1", "Count" )

ny$What <- "Confirmed Cases"
ny <- ny[ complete.cases(ny) ]
ny$Region <- "US"

nyc <- ny[ Location == "New York City" ]
setnames( nyc, "Location", "Subregion" )
nyc$Region <- "US"
nyc <- rbind(
    nyc,
    data.table(
        Subregion="New York City",
        Region="US",
        Count=192,
        Day=as.Date("2020-03-24"),
        What="Fatalities"
    )
)

nyc$Count <- as.character( nyc$Count )

covid <- rbind( covid, nyc )

## ny totals
ny <- ny [ ! grep( "Total", Location), sum(Count), by=.(Day,What) ]
setnames( ny, "V1", "Count" )
ny$Region <- "US"
ny$Subregion <- "New York"

covid <- covid[
    ! (
        Subregion=="New York" &
        Day %in% unique(ny$Day) &
        What %in% unique(ny$What)
    )
]
covid <- rbind( covid, ny ) 

fwrite( covid, "covid.csv" )
