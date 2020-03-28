library(zoo)

load.jhu.data <- function() {
    system( "svn checkout https://github.com/CSSEGISandData/COVID-19/trunk/csse_covid_19_data/csse_covid_19_daily_reports" )

    jhu <- NULL
    for( f in dir(path="csse_covid_19_daily_reports",pattern="csv$",full.names=TRUE) ) {
        dt <- fread(f)
        ## column names are not uniform, so we reduce them to a common
        ## form by deleting things we don't use
        for( x in c("Latitude","Longitude","FIPS","Admin2","Lat","Long_","Combined_Key","Active","Recovered") ) {
            if( x %in% names(dt) ) {
                dt[[ x ]] <- NULL
            }
        }
        ## we change / and space to _ in column names
        old.names <- names(dt)
        new.names <- gsub( "/", "_", old.names, fixed=TRUE )
        new.names <- gsub( " ", "_", new.names, fixed=TRUE )
        setnames( dt, old.names, new.names )
        jhu <- rbind( jhu, dt )
    }

    names(jhu) <- c("Reg2","Reg1","DayTime","Confirmed Cases","Fatalities") 

    ## there is extra whitespace in some cases
    jhu[, Reg2 := trimws( Reg1 ) ]

    ## very helpfully, dates are in several different formats...
    jhu[ !grep("/",DayTime), Day := as.Date( DayTime ) ]
    jhu[ is.na(Day), Day := as.Date( DayTime, format="%m/%d/%y" ) ]
    jhu[, DayTime := NULL ]

    ## an empty subregion means the total
    jhu[ Reg2=="", Reg2 := "All" ]

    jhu <- melt( jhu, id.vars=c("Reg2","Reg1","Day") )
    setnames( jhu, c("variable","value"), c("What","Count") )

    jhu$Reg3 <- "All"
    jhu
}    
