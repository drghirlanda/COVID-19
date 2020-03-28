load.us.states <- function() {
    us <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
    us[, fips := NULL ]
    setnames(
        us,
        c("date","state","cases","deaths"),
        c("Day","Reg2","Confirmed Cases","Fatalities")
    )
    us <- melt( us, id.vars=c("Day","Reg2") )
    setnames(
        us,
        c("variable","value"),
        c("What","Count")
    )
    us <- us[ Count>0 ]
    us$Reg1 <- "US"
    us$Reg3 <- "All"
    us$Day <- as.Date( us$Day )
    us
}
