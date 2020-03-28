load.us.counties <- function() {
    us <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
    us[, fips := NULL ]
    setnames(
        us,
        c("date","county","state","cases","deaths"),
        c("Day","Reg3","Reg2","Confirmed Cases","Fatalities")
    )
    us <- melt( us, id.vars=c("Day","Reg2","Reg3") )
    setnames(
        us,
        c("variable","value"),
        c("What","Count")
    )
    us <- us[ Count>0 ]
    ## us.all <- us[, sum(Count), by=.(Reg2,What,Day) ]
    ## setnames( us.all, "V1", "Count" )
    ## us.all$Reg3 <- "All"
    ## us <- rbind( us, us.all )
    us$Reg1 <- "US"
    us$Day <- as.Date( us$Day )
    us[ complete.cases( us ) ]
}
