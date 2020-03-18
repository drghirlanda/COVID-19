library(shiny)
library(data.table)
library(zoo)

source("covid.R")

covid <- load.covid.data()

## some initial values for selectInputs:
sorted.regions <- covid.sorted.regions( covid )
sorted.subregions <- covid.sorted.subregions( covid, sorted.regions[1] )
initial.what <- covid[
    Region==sorted.regions[1] &
    Subregion==sorted.subregions[1],
    unique( What )
]
    
ui <- fluidPage(
    titlePanel( HTML("COVID-19: Understanding Trends - Instructions are <a target=\"_blank\" href=\"https://dataworks.consulting/covid-19\">here</a>") ),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId="selectWhat",
                label="Data Type",
                choices=initial.what,
                selected=initial.what[1]
            ),
            selectInput(
                inputId="selectRegion",
                label="Region/Country",
                choices=sorted.regions
            ),
            selectInput(
                inputId="selectSubregion",
                label="Province/State",
                choices=sorted.subregions
            ),
            dateRangeInput(
                inputId="selectDays",
                label="Days",
                min=min( covid$Day ),
                max=max( covid$Day ),
                start=min( covid$Day ),
                end=max( covid$Day )
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
            plotOutput( outputId="plot", click="plot_click", hover=hoverOpts(id="plot_hover",delay=250,delayType="throttle") ),
            width=9
        )
    ),
    fluidRow(
        column( 12, uiOutput( outputId="appInfo" ) )
    )
)

server <- function( input, output, session ) {

    ## load data here so that they are updated whenever the app is
    ## reloaded, without restarting the shiny server.
    covid <- load.covid.data()
    sorted.regions <- covid.sorted.regions( covid )
    
    updateSelectInput( 
        session,
        inputId="selectRegion",
        choices=sorted.regions
    )
    
    ## names of data sets to plot, and fitted models
    sessionData <- reactiveValues(
        plots=NULL,
        fits=list(),
        buildModel=NULL
    )
    
    ## adjust subregion selectInput to selected region
    observe({
        sorted.subregions <- covid.sorted.subregions(
            covid,
            input$selectRegion
        )
        updateSelectInput(
            session,
            inputId="selectSubregion",
            choices=sorted.subregions
        )
    })

    ## adjust data type choices to selected region and subregion
    observe({
        r <- input$selectRegion
        s <- input$selectSubregion
        w <- covid[
            Region==r &
            Subregion==s,
            unique(What)
        ]
        print( w )
        updateSelectInput(
            session,
            inputId="selectWhat",
            choices=w,
            selected=w[1]
        )
    })
    
    ## adjust day according to region and data type
    observeEvent( {input$selectRegion; input$selectWhat}, {
        r <- input$selectRegion
        w <- input$selectWhat
        d <- covid[ Region==r & What==w, unique(Day) ]
        newStart <- max( min(d), input$selectDays[1] )
        newEnd <- min( max(d), input$selectDays[2] )
        ## there seems to be a bug in updateDateRangeInput: if the two
        ## calls below are merged into one, the start date is set to
        ## NULL. making two separate calls works.
        updateDateRangeInput(
            session,
            inputId="selectDays",
            start=newStart,
            end=newEnd,
            min=min(d)
        )
        updateDateRangeInput(
            session,
            inputId="selectDays",
            max=max(d)
        )
    })

    pack.id <- function( r, s, f, l, w ) {
        paste( r, s, f, l, w, sep="+" )
    }

    unpack.id <- function( id ) {
        unlist( strsplit( id, "+", fixed=TRUE ) )
    }
    
    uiMessage <- function( ..., sep=" ", err=FALSE ) {
        style <- "margin-top: 20px;"
        if( err ) {
            style <- paste( style, "color: red;" )
        }
        output$info <- renderUI({
            tags$p( paste(..., sep=sep), style=style )
        })
    }
        
    ## add data set to plot 
    observeEvent( input$buttonAdd, {

        ## get selectors for data set
        r <- input$selectRegion
        s <- input$selectSubregion
        f <- input$selectDays[1]
        l <- input$selectDays[2]
        w <- isolate( input$selectWhat )

        if( is.na(f) | is.na(l) ) {
            uiMessage(
                "Please select both start and end dates",
                err=TRUE
            )
            return()
        }
        
        ## extract data 
        my.data <- covid[ Region    == r     &
                       Subregion == s     &
                       What      == w     &
                       Day       >= f     &
                       Day       <= l
                      ]

        ## add Id and What identifiers
        id <- pack.id( r,s,f,l,w )
        my.data$Id <- id


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
        if( is.null(sessionData$plots) ) {
            return()
        }

        p <- nearPoints( sessionData$plots, input$plot_click, xvar="Day", yvar="Count", maxpoints=1 )
        if( nrow(p)==0 ) {
            uiMessage(
                "Clicked on",
                as.Date(input$plot_click$x),
                round(input$plot_click$y)
            )
            return()
        }

        if( is.null( sessionData$buildModel ) ) {
            return()
        }
        
        if( is.null( sessionData$buildModel$First ) ) {
            ## check selected point is not the last point
            maxDay <- sessionData$plots[
                                      Region==p$Region &
                                      Subregion==p$Subregion &
                                      What==p$What,
                                      max(Day)
                                  ]
            if( p$Day == maxDay ) {
                return()
            }
            sessionData$buildModel$First      <- p$Day
            sessionData$buildModel$FirstCount <- p$Count
            sessionData$buildModel$Region     <- p$Region
            sessionData$buildModel$Subregion  <- p$Subregion
            sessionData$buildModel$What       <- p$What
            uiMessage( "First model point:", p$Region, p$Subregion, p$What, p$Day, p$Count )
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
        
        if( p$Day <= sessionData$buildModel$First ) {
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
            pp <- exp.plot( fit, add=TRUE, col=i, model=TRUE, data=FALSE )
            subregion <- fit$dt$Subregion[1]
            if( subregion == "All" ) {
                subregion <- ""
            } else {
                subregion <- paste0(subregion," ")
            }
            
            lg.fit <- paste0(
                fit$dt$Region[1],
                " ",
                subregion,
                fit$dt$What[1],
                " ",
                format( min( pp$Day ), "%m-%d" ),
                "/",
                format( max( pp$Day ), "%m-%d" ),
                ", Td=",
                format( 1 / coef(fit$fit)[2], digits=3 ),
                ", r2=",
                format( cor(pp$Count,fit$dt$Count)^2, digits=2 )
            )
            lg.text <- c( lg.text, lg.fit )
            lg.col <- c( lg.col, i )
            i <- i + 1
        }

        if( logScale=="" ) {
            lg.pos <- "topleft"
        } else {
            lg.pos <- "topleft"
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
                with( sessionData$buildModel, points( First, FirstCount, pch=1, cex=2 ) )
            }
            if( !is.null( sessionData$buildModel$InProgress ) ) {
                with( sessionData$buildModel, points( Last, LastCount, pch=1, cex=2 ) )
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
