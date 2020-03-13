library(shiny)
library(data.table)
library(zoo)

source("covid.R")

deaths <- load.covid( "Deaths" )
confirmed <- load.covid( "Confirmed" )

ui <- fluidPage(
    titlePanel( "COVID-19: Data and Models" ),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId="selectRegion",
                label="Region/Country",
                choices=unique( confirmed[order(-RegionTotal),Region] )
            ),
            selectInput(
                inputId="selectSubregion",
                label="Province/State",
                choices=unique(
                    confirmed[order(-SubregionTotal),Subregion]
                )
            ),
            selectInput(
                inputId="selectFirstDay",
                label="Start",
                choices=unique( confirmed$Day ),
                selected=min( confirmed$Day )
            ),
            selectInput(
                inputId="selectLastDay",
                label="End",
                choices=unique( confirmed$Day ),
                selected=max( confirmed$Day )
            ),
            actionButton(
                inputId="buttonAdd",
                label="Add Data"
            ),
            actionButton(
                inputId="buttonModel",
                label="Add Model"
            ),
            actionButton(
                inputId="buttonClear",
                label="Clear"
            ),
            actionButton(
                inputId="buttonClearPoints",
                label="Clear Points"
            ),
            p(),
            radioButtons(
                inputId="radioScale",
                label="Vertical Scale",
                choices=c("Linear","Logarithmic"),
                selected="Logarithmic"
            )
        ),
        mainPanel(
            plotOutput( outputId="plot", click="plot_click" )
        )
    )
)

server <- function( input, output, session ) {

    ## names of data sets to plot, and fitted models
    sessionData <- reactiveValues(
        plots=list(),
        points=list()
    )
    
    ## adjust subregion selectInput to selected region
    observe({
        subregions <- unique(
            confirmed[
                Region == input$selectRegion ][
                order(-SubregionTotal),
                Subregion
            ]
        )
        updateSelectInput(
            session,
            inputId="selectSubregion",
            choices=subregions
        )
    })
    
    ## adjust day according to region and subregion
    observe({
        r <- input$selectRegion
        s <- input$selectSubregion
        d <- confirmed[ Region==r & Subregion==s & Count>0, Day ]
        updateSelectInput(
            session,
            inputId="selectFirstDay",
            choices=d,
            selected=min(d)
        )
        updateSelectInput(
            session,
            inputId="selectLastDay",
            choices=d,
            selected=max(d)
        )
    })

    ## helper for next function
    get.data.and.id <- function( input ) {
        r <- input$selectRegion
        s <- input$selectSubregion
        f <- input$selectFirstDay
        l <- input$selectLastDay
        list(
            dt=confirmed[
                Region==r &
                Subregion==s &
                Day >= f &
                Day <= l
            ],
            id=paste( c(r,s,f,l), collapse="+" )
        )
    }
        
    ## add data set to plot 
    observeEvent( input$buttonAdd, {
        dt.id <- get.data.and.id( input )
        if( ! dt.id$id %in% sessionData$plots ) {
            sessionData$plots[[ dt.id$id ]] <- list(
                dt=dt.id$dt,
                fit=NULL
            )
        }
    })

    ## add data set with fit to plot
    observeEvent( input$buttonModel, {
        dt.id <- get.data.and.id( input )
        if( ! dt.id$id %in% sessionData$plots ) {
            sessionData$plots[[ dt.id$id ]] <- list(
                dt=dt.id$dt,
                fit=exp.fit( dt.id$dt )
            )
        }
    })

    ## add point to plot on click
    observeEvent( input$plot_click, {
        point.id <- paste(
            input$plot_click$x,
            input$plot_click$y,
            sep="+"
        )
        sessionData$points[[ point.id ]] <- list(
            x=input$plot_click$x,
            y=input$plot_click$y
        )
    })
    
    ## clear the plot 
    observeEvent( input$buttonClear, {
        sessionData$plots <- list()
        sessionData$points <- list()
    })

    ## clear points on the plot 
    observeEvent( input$buttonClearPoints, {
        sessionData$points <- list()
    })


    output$plot <- renderPlot({
        plotNames <- names( sessionData$plots )
        if( ! length( plotNames ) ) {
            return()
        }

        ## list of region, subregion, first, and last days
        rsfl <- strsplit( plotNames, "+", fixed=TRUE )
        regions <- unlist( lapply(rsfl, `[[`, 1 ) )
        subregions <- unlist( lapply(rsfl, `[[`, 2 ) )
        first.days <- as.Date( unlist( lapply(rsfl, `[[`, 3 ) ) )
        last.days <- as.Date( unlist( lapply(rsfl, `[[`, 4 ) ) )

        ## find yMax
        yMax <- max( unlist( lapply(
            sessionData$plots,
            function(x){ max(x$dt$Count) }
        )))

        ## leave room for model overshoot
        yMax <- 2*yMax
        
        ## set logscale
        if( input$radioScale=="Linear" ) {
            logScale <- ""
        } else {
            logScale <- "y"
        }
        
        ## set the stage
        par( las=1 )
        plot(
            as.Date( NA ),
            NA,
            xlab = "Day",
            ylab = "Count",
            xlim = c( min(first.days), max(last.days) ),
            ylim = c( 1, yMax ),
            log  = logScale
        )

        ## set up legend data structures
        lg.text <- c()
        lg.col  <- c()
        ## add points
        i <- 1
        for( ds in sessionData$plots ) {
            points( ds$dt$Day, ds$dt$Count, pch=16, col=i )
            if( ! is.null( ds$fit ) ) {
                exp.plot( ds$fit, add=TRUE, col=i, model=TRUE )
                lg.fit <- paste0(
                    " ",
                    as.Date( first.days[i] ),
                    "/",
                    as.Date( last.days[i] ),
                    ", Td=",
                    format( 1 / coef(ds$fit$fit)[2], digits=3 )
                )
            } else {
                lg.fit <- ""
            }

            lg.this <- ds$dt$Region[1]
            if( ds$dt$Subregion[1] != "All" ) {
                lg.this <- paste(
                    lg.this, ds$dt$Subregion[1], sep="/"
                )
            }
            lg.this <- paste0( lg.this, lg.fit )
            lg.text <- c( lg.text, lg.this )
            lg.col <- c( lg.col, i )
            i <- i + 1
        }

        if( logScale=="" ) {
            lg.pos <- "topleft"
        } else {
            lg.pos <- "bottomright"
        }
        legend(
            lg.pos,
            legend=lg.text,
            col=lg.col,
            bty="n",
            lty=1,
            pch=16
        )

        ## points
        for( p in sessionData$points ) {
            points( p$x, p$y, pch=1 )
            text(
                p$x,
                p$y,
                labels=paste(
                    as.Date(p$x),
                    format(p$y, digits=2),
                    sep=", "
                ),
                pos=3,
                xpd=TRUE
            )
        }
    })
    
}

shinyApp( ui=ui, server=server )

