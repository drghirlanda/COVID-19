library(shiny)
library(data.table)
library(zoo)

source("covid.R")

deaths <- load.covid( "Deaths" )
confirmed <- load.covid( "Confirmed" )

ui <- fluidPage(
    titlePanel( HTML("COVID-19: Understanding Trends - Instructions are <a target=\"_blank\" href=\"https://dataworks.consulting/covid-19\">here</a>") ),
    sidebarLayout(
        sidebarPanel(
            radioButtons( inputId="radioWhat", label=NULL, choices=c("Cases","Fatalities"), selected="Cases", inline=TRUE ),
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
                choices=sort( unique( confirmed$Day ) ),
                selected=min( confirmed$Day )
            ),
            selectInput(
                inputId="selectLastDay",
                label="End",
                choices=sort( unique( confirmed$Day ) ), 
                selected=max( confirmed$Day )
            ),
            actionButton(
                inputId="buttonAdd",
                label="Add Data"
            ),
            actionButton(
                inputId="buttonModel",
                label="Build Model"
            ),
            p(),
            actionButton(
                inputId="buttonClear",
                label="Clear All"
            ),
            actionButton(
                inputId="buttonClearPoints",
                label="Clear Points"
            ),
            p(),
            checkboxInput(  
                inputId="checkboxScale",
                label="Log Scale",
                value=TRUE
            ),
            width=3
        ),
        mainPanel(
            plotOutput( outputId="plot", click="plot_click" ),
            uiOutput( outputId="info" ),
            width=9
        )
    )
)

server <- function( input, output, session ) {

    ## names of data sets to plot, and fitted models
    sessionData <- reactiveValues(
        plots=NULL,
        fits=list(),
        points=list(),
        buildModel=NULL
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
            choices=sort(d),
            selected=min(d)
        )
        updateSelectInput(
            session,
            inputId="selectLastDay",
            choices=sort(d),
            selected=max(d)
        )
    })

    ## helper for next two functions
    get.data.with.id <- function( start, end ) {
        r <- input$selectRegion
        s <- input$selectSubregion
        f <- start
        l <- end
        w <- isolate( input$radioWhat )
        if( w == "Cases" ) {
            dt <- confirmed
        } else {
            dt <- deaths
        }
        my.data <- dt[
            Region==r &
            Subregion==s &
            Day >= f &
            Day <= l
        ]
        my.data$Id <- paste( c(r,s,f,l,w), collapse="+" )
        my.data
    }
        
    ## add data set to plot 
    observeEvent( input$buttonAdd, {
        start <- input$selectFirstDay
        end <- input$selectLastDay
        dt <- get.data.with.id( start, end )
        if( is.null( sessionData$plots ) ) { # first data set
            sessionData$plots <- dt
        } else if( ! dt$Id[1] %in% unique(sessionData$plots$Id) ) {
            sessionData$plots <- rbind(
                sessionData$plots,
                dt
            )
        }
    })

    ## start or cancel model build
    observeEvent( input$buttonModel, {
        if( is.null( sessionData$buildModel ) ) {
            sessionData$buildModel <- list()
            updateActionButton( session, "buttonModel", label="Cancel Model" )
        } else {
            sessionData$buildModel <- NULL
            updateActionButton( session, "buttonModel", label="Build Model" )
        }
    })   

    ## when plot is clicked, add point to plot or build model
    observeEvent( input$plot_click, {
        print( sessionData$buildModel$start )
        print( sessionData$buildModel$end )
        if( is.null( sessionData$buildModel ) ) {
            point.id <- paste(
                input$plot_click$x,
                input$plot_click$y,
                sep="+"
            )
            sessionData$points[[ point.id ]] <- list(
                x=input$plot_click$x,
                y=input$plot_click$y
            )
        } else {
            if( is.null( sessionData$buildModel$start ) ) {
                sessionData$buildModel$start <- input$plot_click$x
                my.point <- nearPoints( sessionData$plots, input$plot_click, xvar="Day", yvar="Count", maxpoints=1 )
            } else {
                sessionData$buildModel$end <- input$plot_click$x
                my.point <- nearPoints( sessionData$plots, input$plot_click, xvar="Day", yvar="Count", maxpoints=1 )
                dt <- sessionData$plots[
                                      Id== my.point$Id &
                                      Day>=sessionData$buildModel$start &
                                      Day<=sessionData$buildModel$end
                                  ]
                id <- paste(
                    
                )
                )
                if( ! dt$Id[1] %in% names(sessionData$fits) ) {
                    sessionData$fits[[ dt$Id[1] ]] <- exp.fit( dt )
                }
                print( names( sessionData$fits ) )
                updateActionButton( session, "buttonModel", label="Build Model" )
                sessionData$buildModel <- NULL
            }
        }
    })

    ## clear the plot 
    observeEvent( input$buttonClear, {
        sessionData$plots <- NULL
        sessionData$its <- list()
        sessionData$points <- list()
        sessionData$buildModel <- NULL
    })

    ## clear points on the plot 
    observeEvent( input$buttonClearPoints, {
        sessionData$points <- list()
    })

    ## this does the heavy lifting...
    output$plot <- renderPlot({
        if( is.null( sessionData$plots ) ) {
            return()
        }

        plotNames <- unique( sessionData$plots$Id )
        print( plotNames )
        
        ## list of region, subregion, first, last days, and cases/fatalities
        rsflw <- strsplit( plotNames, "+", fixed=TRUE )
        regions <- unlist( lapply(rsflw, `[[`, 1 ) )
        subregions <- unlist( lapply(rsflw, `[[`, 2 ) )
        first.days <- as.Date( unlist( lapply(rsflw, `[[`, 3 ) ) )
        last.days <- as.Date( unlist( lapply(rsflw, `[[`, 4 ) ) )
        what <- unlist( lapply( rsflw, `[[`, 5 ) )
        
        ## find yMax
        yMax <- max( sessionData$plots$Count )
        ## leave room for model overshoot
        yMax <- 2*yMax
        
        ## set logscale
        if( input$checkboxScale ) {
            logScale <- "y"
        } else {
            logScale <- ""
        }
        
        ## set the stage
        par( las=1 )
        plot(
            as.Date( NA ),
            NA,
            xlab = "Day",
            ylab = "",
            xlim = c( min(first.days)-2, max(last.days)+2 ),
            ylim = c( 1, yMax ),
            log  = logScale
        )

        ## set up legend data structures
        lg.text <- c()
        lg.col  <- c()
        ## add points
        i <- 1
        for( id in plotNames ) {
            dt <- sessionData$plots[ Id==id ]
            points( dt$Day, dt$Count, pch=16, col=i )
            if( ! is.null( sessionData$fits[[ id ]] ) ) {
                fit <- sessionData$fits[[ id ]]
                exp.plot( fit, add=TRUE, col=i, model=TRUE )
                lg.fit <- paste0(
                    " ",
                    as.Date( first.days[i] ),
                    "/",
                    as.Date( last.days[i] ),
                    ", Td=",
                    format( 1 / coef(fit)[2], digits=3 )
                )
            } else {
                lg.fit <- ""
            }

            lg.this <- paste( dt$Region[1], what[i] )
            if( dt$Subregion[1] != "All" ) {
                lg.this <- paste(
                    lg.this, dt$Subregion[1], sep="/"
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
                labels=as.Date(p$x),
                pos=1,
                xpd=TRUE
            )
            text(
                p$x,
                p$y,
                labels=format(p$y, digits=2),
                pos=3,
                xpd=TRUE
            )
        }
    })

    output$info <- renderUI({
        tagList(
            tags$p( HTML("This tool is provided by <a href=\"https://dataworks.consulting\">DataWorks LLC</a> as is, without any implied fitness for any purpose. It may provide inaccurate information. DataWorks LLC and its representatives are not liable for any damage that may derive from the use of this tool.") ),
            tags$p( HTML("Data courtesy of <a href=\"https://systems.jhu.edu/research/public-health/ncov/\">Johns Hopkins Center for Systems Science and Engineering</a>. Retrieved from the <a href=\"https://github.com/CSSEGISandData/COVID-19\">COVID-19 github.com repository</a>.") ),
            tags$p( HTML("&copy;&nbsp;DataWorks LLC 2020") )
        )
    })
    
}

shinyApp( ui=ui, server=server )

