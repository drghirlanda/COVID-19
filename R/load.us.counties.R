load.us.counties <- function() {
    us <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
    us[, fips := NULL ]
    data(state)
    us$state <- unlist(
        lapply(
            us$state,
            function(x) {
                if( x %in% state.name ) {
                    state.abb[ state.name==x ]
                } else {
                    NA
                }
            }
        )
    )
    us[, Subregion := paste( county, state, sep=", " ) ]
    us[, county := NULL ]
    us[, state := NULL ]
    setnames(
        us,
        c("date","cases","deaths"),
        c("Day","Confirmed Cases","Fatalities")
    )
    us <- melt( us, id.vars=c("Day","Subregion") )
    setnames(
        us,
        c("variable","value"),
        c("What","Count")
    )
    us <- us[ Count>0 ]
    us$Region <- "US"
    us$Day <- as.Date( us$Day )
    us[ complete.cases( us ) ]
}
