load.humdata.data <- function() {

    confirmed <- fread("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv")
    
    confirmed$What <- "Confirmed Cases"

    deaths <- fread("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv")
    
    deaths$What <- "Fatalities"

    covid <- rbind( confirmed, deaths )
    setnames(
        covid,
        c("Province/State", "Country/Region"),
        c("Reg2", "Reg1")
    )
    ## discard Long and Lat
    covid[ , Long := NULL ]
    covid[ , Lat  := NULL ]

    covid <- melt( covid, id.vars=c("Reg1","Reg2","What") )
    setnames( covid, c("variable","value"), c("Day","Count") )

    covid$Day <- as.Date( covid$Day, format="%m/%d/%y" )
    covid$Count <- as.numeric( covid$Count )

    ## replace empty subregion with All
    covid[ Reg2=="", Reg2 := "All" ]
    ## discard Count==0
    covid <- covid[ Count>0 ]

    ## add Reg2 := All to Reg1 entities that lack it
    no.all <- covid[ , sum(Reg2=="All"), by=Reg1 ][ V1==0, Reg1 ]
    no.all.sums <- covid[
        Reg1 %in% no.all,
        sum(Count),
        by=.(Reg1,Day,What)
    ]
    setnames( no.all.sums, "V1", "Count" )
    no.all.sums$Reg2 <- "All"

    covid <- rbind( covid, no.all.sums )
    
    covid$Reg3 <- "All"
    
    covid
}
