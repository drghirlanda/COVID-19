library(data.table)
source("../../covid.R")
covid <- fread("../../covid.csv")
covid$Day <- as.Date( covid$Day )


rollingEstimates <- function( dt, window ) {
    dt <- dt[ Count>0 ]
    dayMin <- min( dt$Day ) + window
    dayMax <- max( dt$Day ) - window
    days <- dayMin : dayMax
    r <- rep( NA, length(days) )
    count <- dt[ Day %in% days, Count ]
    for( i in 1:length(days) ) {
        these.days <- ( days[i] - window ) : ( days[i] + window )
        this.dt <- dt[ Day %in% these.days ] 
        if( nrow(this.dt)>=2*window+1 ) {
            fit.out <- exp.fit( this.dt )
            r[i] <- coef( fit.out$fit )[2]
        } else {
            r[i] <- NA
        }
    }
    data.table(
        Day=as.Date(days),
        R=r,
        T=1/r,
        Count=count )
}

rollingPlots <- function(
                         dt,
                         window,
                         xVar,
                         yVar,
                         ...
                         ) {

    fits <- list()
    for( w in unique( dt$What ) ) {
        fits[[w]] <- rollingEstimates( dt[What==w], window )
    }

    xVals <- unlist( lapply( fits, function(f) { f[[xVar]] } ) ) 
    xRange <- range( xVals )
    if( xVar == "Day" ) {
        xRange <- as.Date( xRange )
    }
    yVals <- unlist( lapply( fits, function(f) { f[[yVar]] } ) ) 
    yRange <- range( yVals )

    par( las=1 )
    plot(
        xRange,
        yRange,
        pch=NA,
        xlim=xRange,
        ylim=yRange,
        xlab=xVar,
        ylab=yVar,
        ...
    )
    i <- 1
    lapply( fits, function(f) {
        lines( f[[xVar]], f[[yVar]], col=i )
        i <<- i + 1
    })
    if( yVar=="T" ) {
        lg.pos <- "topleft"
    } else {
        lg.pos <- "bottomleft"
    }
    legend(
        lg.pos,
        legend=names(fits),
        lty=1,
        pch=16,
        col=1:length(names(fits)),
        bty="n"
    )
    invisible( fits )
}


italy <- covid[
    Region=="Italy" &
    Subregion=="All" &
    Day >= "2020-02-25"
]

rollingPlots( italy, window=3, xVar="Day", yVar="T" )

rollingPlots(
    covid[Subregion=="Lombardia"&Day>="2020-02-25"],
    window=3,
    xVar="Day",
    yVar="T"
)


