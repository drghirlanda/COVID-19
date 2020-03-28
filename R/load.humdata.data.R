load.humdata.data <- function() {

    confirmed <- fread("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv")
    confirmed$What <- "Confirmed Cases"

    deaths <- fread("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv" )
    deaths$What <- "Fatalities"

    covid <- rbind( confirmed, deaths )
    setnames(
        covid,
        c("Province/State","Country/Region","Date","Value"),
        c("Reg2",     "Reg1",        "Day", "Count")
    )
    covid$Day <- as.Date( covid$Day, format="%F" )
    covid$Count <- as.numeric( covid$Count )
    ## discard some header information
    covid <- covid[ ! grep("#",Reg1) ]
    ## replace empty subregion with All
    covid[ Reg2=="", Reg2 := "All" ]
    ## discard Count==0
    covid <- covid[ Count>0 ]

    ## discard Long and Lat
    covid[ , Long := NULL ]
    covid[ , Lat  := NULL ]

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
