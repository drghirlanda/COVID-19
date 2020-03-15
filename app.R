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
                inputId="buttonClearModels",
                label="Clear Models"
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
            uiOutput( outputId="info" ),
            plotOutput( outputId="plot", click="plot_click", hover=hoverOpts(id="plot_hover",delay=50,delayType="debounce") ),
            uiOutput( outputId="appInfo" ),
            width=9
        )
    )
)

server <- function( input, output, session ) {

    ## names of data sets to plot, and fitted models
    sessionData <- reactiveValues(
        plots=NULL,
        fits=list(),
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

    pack.id <- function( r, s, f, l, w ) {
        paste( r, s, f, l, w, sep="+" )
    }

    unpack.id <- function( id ) {
        unlist( strsplit( id, "+", fixed=TRUE ) )
    }
    
    uiMessage <- function( ..., sep=" " ) {
        output$info <- renderUI({
            tags$p( paste(..., sep=sep), style="margin-top: 20px" )
        })
    }
    
    ## add data set to plot 
    observeEvent( input$buttonAdd, {

        ## get selectors for data set
        r <- input$selectRegion
        s <- input$selectSubregion
        f <- input$selectFirstDay
        l <- input$selectLastDay
        w <- isolate( input$radioWhat )
        if( w == "Cases" ) {
            dt <- confirmed
        } else {
            dt <- deaths
        }
        
        ## extract data 
        my.data <- dt[ Region    == r     &
                       Subregion == s     &
                       Day       >= f     &
                       Day       <= l
                      ]

        ## add Id and What identifiers
        my.data$Id <- pack.id( r,s,f,l,w )
        my.data$What <- w

        ## add data to sessionData, if not already there
        if( ! my.data$Id[1] %in% unique(sessionData$plots$Id) ) {
            uiMessage( "Added data for", gsub( "+", " ", my.data$Id[1], fixed=TRUE ) )
            sessionData$plots <- rbind(
                sessionData$plots,
                my.data
            )
        }
    })

    ## start or cancel model build
    observeEvent( input$buttonModel, {
        if( is.null( sessionData$buildModel ) ) {
            sessionData$buildModel <- list()
            updateActionButton( session, "buttonModel", label="Cancel Model" )
            uiMessage( "Click on the first point to include in the model" ) 
        } else {
            sessionData$buildModel <- NULL
            updateActionButton( session, "buttonModel", label="Build Model" )
            uiMessage( "Model build canceled" )
        }
    })   

    ## when plot is clicked, output point info and possibly build model
    observeEvent( input$plot_click, {
        p <- nearPoints( sessionData$plots, input$plot_click, xvar="Day", yvar="Count", maxpoints=1 )
        if( nrow(p)==0 ) {
            uiMessage( "Click too far from any point" )
            return()
        }

        if( is.null( sessionData$buildModel ) ) {
            return()
        }
        
        if( is.null( sessionData$buildModel$First ) ) {
            sessionData$buildModel$First      <- p$Day
            sessionData$buildModel$FirstCount <- p$Count
            sessionData$buildModel$Region     <- p$Region
            sessionData$buildModel$Subregion  <- p$Subregion
            sessionData$buildModel$What       <- p$What
            uiMessage( "First model point:", p$Region, p$Subregion, p$Day, p$Count, p$What )
        } else {
            ## this is the second model point: move the InProgress fit to the completed fits
            id <- pack.id(
                sessionData$buildModel$Region,
                sessionData$buildModel$Subregion,
                sessionData$buildModel$First,
                sessionData$buildModel$Last,
                sessionData$buildModel$What
            )
            if( ! id %in% names(sessionData$fits) ) {
                sessionData$fits[[ id ]] <- sessionData$buildModel$InProgress
            }
            uiMessage( "Finalized model" ) 
            updateActionButton( session, "buttonModel", label="Build Model" )
            sessionData$buildModel <- NULL
        }
    })

    ## when building a model and hovering on the plot, show an approximate fit
    observeEvent( input$plot_hover, {
        if( is.null(sessionData$plots) ) {
            return()
        }
        
        p <- nearPoints( sessionData$plots, input$plot_hover, xvar="Day", yvar="Count", maxpoints=1 )
        if( nrow(p)==0 ) {
            return()
        }

        with( p, uiMessage( Region, Subregion, What, Day, Count ) )

        if( is.null( sessionData$buildModel ) ) {
            return()
        }
        if( is.null( sessionData$buildModel$First ) ) {
            return()
        }

        if( sessionData$buildModel$Region    != p$Region    |
            sessionData$buildModel$Subregion != p$Subregion |
            sessionData$buildModel$What      != p$What ) {
            return()
        }
        sessionData$buildModel$Last      <- p$Day
        sessionData$buildModel$LastCount <- p$Count

        dt <- sessionData$plots[
                              Id==p$Id &
                              Day>=sessionData$buildModel$First &
                              Day<=sessionData$buildModel$Last
                          ]
        sessionData$buildModel$InProgress <- exp.fit( dt )
        
    })
    
    ## clear the plot 
    observeEvent( input$buttonClear, {
        sessionData$plots <- NULL
        sessionData$fits <- list()
        sessionData$points <- list()
        sessionData$buildModel <- NULL
        output$info <- renderUI( tags$p( "Messages will appear here", style="color: red; margin-top: 20px" ) )
    })

    ## clear points on the plot 
    observeEvent( input$buttonClearModels, {
        sessionData$fits <- list()
        sessionData$builModel <- NULL
        updateActionButton( session, inputId="buttonModel", label="Build Model" )
    })

    ## finally, plot!
    output$plot <- renderPlot({

        ## nothing to plot
        if( is.null( sessionData$plots ) ) {
            return()
        }

        plotNames <- unique( sessionData$plots$Id )

        xMin <- min( sessionData$plots$Day ) - 2
        xMax <- max( sessionData$plots$Day ) + 2
        
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
        par( las=1, oma=c(0,0,0,0), mar=c(4,5,0.5,0.5) )
        plot(
            as.Date( NA ),
            NA,
            xlab = "Day",
            ylab = "",
            xlim = c( xMin, xMax ),
            ylim = c( 1, yMax ),
            log  = logScale
        )

        ## set up legend data structures
        lg.text <- c()
        lg.col  <- c()

        ## add data
        i <- 1
        for( id in plotNames ) {
            dt <- sessionData$plots[ Id==id ]
            points( dt$Day, dt$Count, pch=16, col=i )
            lg.this <- paste( dt$Region[1], dt$What[1] )
            if( dt$Subregion[1] != "All" ) {
                lg.this <- paste(
                    lg.this, dt$Subregion[1], sep="/"
                )
            }
            lg.text <- c( lg.text, lg.this )
            lg.col <- c( lg.col, i )
            i <- i + 1
        } 

        ## add fits
        for( id in names( sessionData$fits ) ) {
            fit <- sessionData$fits[[ id ]]
            pp <- exp.plot( fit, add=TRUE, col=i, model=TRUE, data=TRUE )
            lg.fit <- paste0(
                " ",
                as.Date( min( pp$Day ) ),
                "/",
                as.Date( max( pp$Day ) ),
                ", Td=",
                format( 1 / coef(fit$fit)[2], digits=3 )
            )
            lg.text <- c( lg.text, lg.fit )
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

        ## build model info
        if( ! is.null( sessionData$buildModel ) ) {
            if( !is.null( sessionData$buildModel$First ) ) {
                with( sessionData$buildModel, points( First, FirstCount, pch=1, cex=1.5 ) )
            }
            if( !is.null( sessionData$buildModel$InProgress ) ) {
                fit <- sessionData$buildModel$InProgress
                exp.plot( fit, add=TRUE, col="gray", model=TRUE, data=FALSE )
            }
        }

    })

    output$info <- renderUI({
        tags$p( "Messages will appear here", style="color: red; margin-top: 20px" )
    })

    output$appInfo <- renderUI({
        tagList(
            tags$p( HTML("This tool is provided by <a href=\"https://dataworks.consulting\">DataWorks LLC</a> as is, without any implied fitness for any purpose. It may provide inaccurate information. DataWorks LLC and its representatives are not liable for any damage that may derive from the use of this tool. Data courtesy of <a href=\"https://systems.jhu.edu/research/public-health/ncov/\">Johns Hopkins Center for Systems Science and Engineering</a>. Retrieved from the <a href=\"https://github.com/CSSEGISandData/COVID-19\">COVID-19 github.com repository</a>.") ),
            tags$p( HTML("&copy;&nbsp;DataWorks LLC 2020") )
        )
    })
    
}

shinyApp( ui=ui, server=server )

