#****************************************#
# Network Navigator                      #
# DEMO - Non-function version            #
# Bobbi Carothers, Douglas Luke          #
# 12/24/2024                             #
#****************************************#


# Preliminary Setup ####

# Libraries
library(DT) # Tooltips on table column headings
library(shiny)
# library(shinyBS) # Tooltips on buttons
library(igraph)
library(visNetwork)
# library(openxlsx) # Export data

# Load data
load('DemoShiny.Rdata')

# Updated On text
UpD <- "Last Updated: 2024.12.18"

# Legend setup
# Set up domain colors from a set of nodes objects that has all of the groups
# Used to create legend
dcol <- data.frame(group=(unique(DemoVnet$nodes$group)),
                   color=unique(DemoVnet$nodes$color),
                   stringsAsFactors=FALSE)
dcol <- dcol[order(dcol$group),]

# Need to render it twice since it's going in a slightly different position on one of the
# tabs, and we can't have an object in there with the same id twice. Save it as an object
# so it's easy to render in the server function.

# Can probably simplify this further (Doug)
LegendFunc <- function(z){
  par(mai=c(0.2,0.4,0,0.4),xpd=TRUE,bg="grey95")
  plot(x=1:nrow(z),
       y=rep(1,nrow(z)), 
       type='p', cex=4, pch= 21, bg=z$color, col=NULL, ann=FALSE, axes=FALSE
  )
  text(x=1:nrow(z),y=0.9, labels=z$group, pos=1, offset=1)
}
LegendBase <- function(){LegendFunc(dcol)}

# Set up containers for network statistics table tooltips
# non-directed
sketchND = htmltools::withTags(table(
  class='display',
  thead(
    tr(
      th("Size", title="Number of organizations"),
      th("Links",title="Number of connections"),
      th("Density",title="Percent of possible connections that actually exist"),
      th("Ave. Degree",title="Average number of connections"),
      th("Isolates",title="Number of non-connected organizations"),
      th("Degree Centralization",title="Extent network has one or a few organizations\nwith many connections"),
      th("Betweenness Centralization",title="Extent network is dependent on one or a few\norganizations to keep the network connected")
      )
      )
))
# directed
sketchD = htmltools::withTags(table(
  class='display',
  thead(
    tr(
      th("Size", title="Number of organizations"),
      th("Links",title="Number of connections"),
      th("Density",title="Percent of possible connections that actually exist"),
      th("Ave. Degree",title="Average number of connections"),
      th("Isolates",title="Number of non-connected organizations"),
      th("Out-Degree Centralization",title="Extent network has one or a few organizations\nwith many outgoing connections"),
      th("In-Degree Centralization",title="Extent network has one or a few organizations\nwith many incoming connections"),
      th("Total Degree Centralization",title="Extent network has one or a few organizations\nwith many total connections")
      )
      )
  ))

# Repeated text for bar charts when an ego network is selected (org name will get pasted between them)
ClickNode1 <- "Tie comparisons between "
ClickNode2 <- " and network average:"

# Start Shiny ####

shinyServer(function(input, output, session) {

  # Legend renders
  output$Legend1 <- renderPlot({LegendBase()})

  # Footer
  output$UpD <- renderText(
    UpD
  )
  
  ###### Contact ##############################

  # Network Setup ###
  # Once we have T2 data, will need to build a switch here
  ConVis <- DemoVnet
  # Select edge weights depending on frequency selection 
  # Convert to non-valued by only selecting from and to
  ConEdges <- reactive ({
    subset(ConVis$edgesContact, weight >= input$ContactLevel, select=c(from,to))
  })
  # Copy an igraph version - will be used for calculations
  ConIg <- reactive ({
    graph_from_data_frame(ConEdges(), directed=FALSE, ConVis$nodes)
  })
  # Set up nodes frame
  ConNodes <- reactive({
    # Calculate degree and betweenness
    {deg <- degree(ConIg())}
    {bet <- betweenness(ConIg(), directed=FALSE, weights=NULL, normalized=TRUE)}
    # Determine which to use to size nodes depending on sizing selection
    value <- switch(input$ConNodeSize,
                    '1' = 10,
                    '2' = deg,
                    '3' = bet)
    data.frame (ConVis$nodes, deg, bet, value)
  })
  
  # Plot Network ###
  # Pull Orgname of selected node for footer
  SelCon <- reactive({
    subset(ConNodes(), id==input$ConNet_selected, select=label)
  })
  output$ConEgoLabel <- renderText(paste0(ClickNode1,as.character(SelCon()),ClickNode2))
 
   # Draw network
  output$ConNet <- renderVisNetwork({
    {
      # Ego network not requested
      visNetwork(ConNodes(), ConEdges()) %>%
        visIgraphLayout(physics=FALSE, type="full", layout = "layout_nicely") %>%
        visNodes(scaling=list(min=10, max=30), font='20px arial') %>%
        visOptions(nodesIdSelection=TRUE, selectedBy="group", 
                   highlightNearest=list(
                     enabled=TRUE, labelOnly=FALSE)
        ) %>%
        visLayout(improvedLayout=TRUE) %>%
        visInteraction(hover=TRUE, navigationButtons=TRUE) %>%
        visEdges(color="#545454")
    }
  })
  
  # Table of network stats ###
  # Create a data frame that will get modified to a datatable here and also used as a download later
  ConNetDF <- reactive({
    # Network statistics
    {Size <- vcount(ConIg())}
    {Links <- ecount(ConIg())}
    {Density <- edge_density(ConIg(),loops=FALSE)}
    # The following calculates a bunch of things, a few of which we pull out
    {DegCent <- centr_degree(ConIg(), mode="total", loops=FALSE, normalized=TRUE)}
    {AverageDegree <- mean(DegCent$res)}
    {Isolates <- length(which(DegCent$res==0))}
    {BetCent <- centr_betw(ConIg(), directed=FALSE, normalized=TRUE)}
    {BetweennessCentralization <- BetCent$centralization}
    {DegreeCentralization <- DegCent$centralization}
    data.frame(Size,Links,Density,AverageDegree,Isolates,DegreeCentralization,
               BetweennessCentralization, stringsAsFactors=FALSE)
  })
  # Need to render a datatable for the column tooltips to work, but don't want any of the
  # other features, so turn them off with the options argument
  ConNetStats <- reactive({
    datatable(ConNetDF(),container=sketchND,caption="Hover over headers for more details",
              rownames=FALSE,
              options=list(ordering=FALSE,lengthChange=FALSE,paging=FALSE,searching=FALSE,
                           info=FALSE)
    ) %>%
      formatPercentage("Density", 1) %>%
      formatRound("AverageDegree", 1) %>%
      formatRound(c("DegreeCentralization","BetweennessCentralization"), 3)
  })
  # Render the table
  output$ConTable <- DT::renderDataTable(ConNetStats())
  
  
  # Selected individual & network average bar plots ###
  # Set up dataframe to drive plots
  ConBarStats <- reactive({
    req(input$ConNet_selected) # Suppresses this and bar plots until a node is selected
    data.frame(label=c('Selected','Average'),
               deg=c(ConNodes()$deg[which(ConNodes()$id==input$ConNet_selected)],
                     mean(ConNodes()$deg)),
               bet=c(ConNodes()$bet[which(ConNodes()$id==input$ConNet_selected)],
                     mean(ConNodes()$bet)),
               color=c(ConNodes()$color[which(ConNodes()$id==input$ConNet_selected)],
                       'black'),
               stringsAsFactors=FALSE)
  })
  # Render plots
  # Degree
  output$ConDegBarPlot <- renderPlot({
    ydlim <- c(0,max(ConBarStats()$deg)*1.2) # add space at top for highest bar value
    barplot(ConBarStats()$deg,names.arg=ConBarStats()$label,
            col=ConBarStats()$color,main='Number of Connections',ylim=ydlim,las=1,border=NA)
    text(x=c(0.7,1.9),y=ConBarStats()$deg,labels=round(ConBarStats()$deg,digits=1),pos=3) # bar values
  })
  
  #Betweenness
  output$ConBetBarPlot <- renderPlot({
    yblim <- c(0,max(ConBarStats()$bet)*1.2)
    barplot(ConBarStats()$bet,names.arg=ConBarStats()$label,
            col=ConBarStats()$color,main='Ability to Link Others',ylim=yblim,las=1,border=NA)
    text(x=c(0.7,1.9),y=ConBarStats()$bet,label=round(ConBarStats()$bet,digits=4),pos=3)
  })
  

  ####### Collaboration#####################
  
  # Network Setup ###
  # Once we have T2 data, will need to build a switch here
  ColVis <- DemoVnet
  # Select edge weights
  ColEdges <- reactive ({
    subset(ColVis$edgesCollab, weight >= input$CollabLevel, select=c(from,to))
  })
  # Copy an igraph version - will be used for calculations
  ColIg <- reactive ({
    graph_from_data_frame(ColEdges(), directed=FALSE, ColVis$nodes)
  })
  # Set up nodes frame
  ColNodes <- reactive({
    # Calculate degree and betweenness
    {deg <- degree(ColIg())}
    {bet <- betweenness(ColIg(),directed=FALSE, weights=NULL, normalized=TRUE)}
    # Determine which to use to size nodes depending on sizing selection
    value <- switch(input$ColNodeSize,
                    '1' = 10,
                    '2' = deg,
                    '3' = bet)
    data.frame (ColVis$nodes, deg, bet, value)
  })
  
  # Plot Network ###
  # Pull Orgname of selected node for footer
  SelCol <- reactive({
    subset(ColNodes(), id==input$ColNet_selected, select=label)
  })
  output$ColEgoLabel <- renderText(paste0(ClickNode1,as.character(SelCol()),ClickNode2))
  # Draw network
  output$ColNet <- renderVisNetwork({
    { # Ego network is requested
      visNetwork(ColNodes(), ColEdges()) %>%
        visIgraphLayout(physics=FALSE, type="full", layout = "layout_nicely") %>%
        visNodes(scaling=list(min=10, max=30), font='20px arial') %>%
        visOptions(nodesIdSelection=TRUE, selectedBy="group", 
                   highlightNearest=list(
                     enabled=TRUE, labelOnly=FALSE)
        ) %>%
        visLayout(improvedLayout=TRUE) %>%
        visInteraction(hover=TRUE, navigationButtons=TRUE) %>%
        visEdges(color="#545454")
    }
  })
  
  # Table of network stats ###
  # Create a data frame that will get modified to a datatable here and also used as a download later
  ColNetDF <- reactive({
    # Network statistics
    {Size <- vcount(ColIg())}
    {Links <- ecount(ColIg())}
    {Density <- edge_density(ColIg(),loops=FALSE)}
    # The following calculates a bunch of things, a few of which we pull out
    {DegCent <- centr_degree(ColIg(),mode="total", loops=FALSE, normalized=TRUE)}
    {AverageDegree <- mean(DegCent$res)}
    {Isolates <- length(which(DegCent$res==0))}
    {BetCent <- centr_betw(ColIg(),directed=FALSE, normalized=TRUE)}
    {BetweennessCentralization <- BetCent$centralization}
    {DegreeCentralization <- DegCent$centralization}
    data.frame(Size,Links,Density,AverageDegree,Isolates,DegreeCentralization,
               BetweennessCentralization, stringsAsFactors=FALSE)
  })
  # Need to render a datatable for the column tooltips to work, but don't want any of the
  # other features, so turn them off with the options argument
  ColNetStats <- reactive({
    datatable(ColNetDF(),container=sketchND,caption="Hover over headers for more details",
              rownames=FALSE,
              options=list(ordering=FALSE,lengthChange=FALSE,paging=FALSE,searching=FALSE,
                           info=FALSE)
    ) %>%
      formatPercentage("Density", 1) %>%
      formatRound("AverageDegree", 1) %>%
      formatRound(c("DegreeCentralization", "BetweennessCentralization"), 3)
  })
  # Render the table
  output$ColTable <- DT::renderDataTable(ColNetStats())
  
  # Selected individual & network average bar plots ###
  # Set up dataframe to drive plots
  ColBarStats <- reactive({
    req(input$ColNet_selected) # Suppresses this and bar plots until a node is selected
    data.frame(label=c('Selected','Average'),
               deg=c(ColNodes()$deg[which(ColNodes()$id==input$ColNet_selected)],
                     mean(ColNodes()$deg)),
               bet=c(ColNodes()$bet[which(ColNodes()$id==input$ColNet_selected)],
                     mean(ColNodes()$bet)),
               color=c(ColNodes()$color[which(ColNodes()$id==input$ColNet_selected)],
                       'black'),
               stringsAsFactors=FALSE)
  })
  # Render plots
  # Degree
  output$ColDegBarPlot <- renderPlot({
    ydlim <- c(0,max(ColBarStats()$deg)*1.2) # add space at top for highest bar value
    barplot(ColBarStats()$deg,names.arg=ColBarStats()$label, 
            col=ColBarStats()$color,main='Number of Connections',ylim=ydlim,las=1,border=NA)
    text(x=c(0.7,1.9),y=ColBarStats()$deg,labels=round(ColBarStats()$deg,digits=1),pos=3) # bar values
  })
  #Betweenness
  output$ColBetBarPlot <- renderPlot({
    yblim <- c(0,max(ColBarStats()$bet)*1.2)
    barplot(ColBarStats()$bet,names.arg=ColBarStats()$label,
            col=ColBarStats()$color,main='Ability to Link Others',ylim=yblim,las=1,border=NA)
    text(x=c(0.7,1.9),y=ColBarStats()$bet,label=round(ColBarStats()$bet,digits=4),pos=3)
  })  
  
  
  ####### Activities########################
  
  # Network Setup ###
  # Build switch when we have multiple years
  ActVis <- DemoVnet
  # Select edges
  ActEdges <- reactive({
    # Build frame according to selection
    frame1 <- subset(ActVis$edgesAct, select=c('from','to',input$ActSelect))
    # Select only the rows where any of the values = 1 (ignoring from and to columns)
    frame1[apply(frame1[,-2], MARGIN=1,function(x) any(x==1)), ]
  })
  # Copy an igraph version - will be used for calculations
  ActIg <- reactive ({
    graph_from_data_frame(ActEdges(), directed=FALSE, ActVis$nodes)
  })
  # Set up nodes frame
  ActNodes <- reactive({
    # Calculate degree and betweenness
    {deg <- degree(ActIg())}
    {bet <- betweenness(ActIg(),directed=FALSE,weights=NULL,normalized=TRUE)}
    # Determine which to use to size nodes depending on sizing selection
    value <- switch(input$ActNodeSize,
                    '1' = 10,
                    '2' = deg,
                    '3' = bet)
    data.frame(ActVis$nodes, deg, bet, value)
  })
  
  # Plot Network ###
  # Pull Orgname of selected node for footer
  SelAct <- reactive({
    subset(ActNodes(), id==input$ActNet_selected, select=label)
  })
  output$ActEgoLabel <- renderText(paste0(ClickNode1,as.character(SelAct()),ClickNode2))
  # Draw network
  output$ActNet <- renderVisNetwork({
    { # Ego network is notrequested
      visNetwork(ActNodes(), ActEdges()) %>%
        visIgraphLayout(physics=FALSE, type = "full", layout = "layout_nicely") %>%
        visNodes(scaling=list(min=10, max=30), font='20px arial') %>%
        visOptions(nodesIdSelection=TRUE, selectedBy="group", 
                   highlightNearest=list(
                     enabled=TRUE, labelOnly=FALSE)
        ) %>%
        visLayout(improvedLayout=TRUE) %>%
        visInteraction(hover=TRUE, navigationButtons=TRUE) %>%
        visEdges(color="#545454")
    }
  })
  
  # Table of network stats ###
  # Create a data frame that will get modified to a datatable here and also used as a download later
  ActNetDF <- reactive({
    # Network statistics
    {Size <- vcount(ActIg())}
    {Links <- ecount(ActIg())}
    {Density <- edge_density(ActIg(),loops=FALSE)}
    # The following calculates a bunch of things, a few of which we pull out
    {DegCent <- centr_degree(ActIg(),mode="total", loops=FALSE, normalized=TRUE)}
    {AverageDegree <- mean(DegCent$res)}
    {Isolates <- length(which(DegCent$res==0))}
    {BetCent <- centr_betw(ActIg(),directed=FALSE, normalized=TRUE)}
    {BetweennessCentralization <- BetCent$centralization}
    {DegreeCentralization <- DegCent$centralization}
    data.frame(Size,Links,Density,AverageDegree,Isolates,DegreeCentralization,
               BetweennessCentralization, stringsAsFactors=FALSE)
  })
  # Need to render a datatable for the column tooltips to work, but don't want any of the
  # other features, so turn them off with the options argument
  ActNetStats <- reactive({
    datatable(ActNetDF(),container=sketchND,caption="Hover over headers for more details",
              rownames=FALSE,
              options=list(ordering=FALSE,lengthChange=FALSE,paging=FALSE,searching=FALSE,
                           info=FALSE)
    ) %>%
      formatPercentage("Density", 1) %>%
      formatRound("AverageDegree", 1) %>%
      formatRound(c("DegreeCentralization", "BetweennessCentralization"), 3)
  })
  # Render the table
  output$ActTable <- DT::renderDataTable(ActNetStats())
  
  # Selected individual & network average bar plots ###
  # Set up dataframe to drive plots
  ActBarStats <- reactive({
    req(input$ActNet_selected) # Suppresses this and bar plots until a node is selected
    data.frame(label=c('Selected','Average'),
               deg=c(ActNodes()$deg[which(ActNodes()$id==input$ActNet_selected)],
                        mean(ActNodes()$deg)),
               bet=c(ActNodes()$bet[which(ActNodes()$id==input$ActNet_selected)],
                       mean(ActNodes()$bet)),
               color=c(ActNodes()$color[which(ActNodes()$id==input$ActNet_selected)],
                       'black'),
               stringsAsFactors=FALSE)
  })
  # Render plots
  # Degree
  output$ActDegBarPlot <- renderPlot({
    ydlim <- c(0,max(ActBarStats()$deg)*1.2)
    barplot(ActBarStats()$deg,names.arg=ActBarStats()$label,
            col=ActBarStats()$color,main='Number of Connections',ylim=ydlim,las=1,border=NA)
    text(x=c(0.7,1.9),y=ActBarStats()$deg,labels=round(ActBarStats()$deg,digits=1),pos=3)
  })
  # Between
  output$ActBetBarPlot <- renderPlot({
    yblim <- c(0,max(ActBarStats()$bet)*1.2)
    barplot(ActBarStats()$bet,names.arg=ActBarStats()$label,
            col=ActBarStats()$color,main='Ability to Link Others',ylim=yblim,las=1,border=NA)
    text(x=c(0.7,1.9),y=ActBarStats()$bet,labels=round(ActBarStats()$bet,digits=4),pos=3)
  })  

 
  ####### Referrals ########################
  
  # Network Setup ###
  # Build switch when we have multiple years
  RefVis <- DemoVnet
  # Select arcs
  RefEdges <- RefVis$edgesRef
  # Copy an igraph version - will be used for calculations
  RefIg <- reactive ({
    graph_from_data_frame(RefEdges, directed=TRUE, RefVis$nodes)
  })
  # Set up nodes frame
  RefNodes <- reactive({
    # Calculate indegree and outdegree
    {outdeg <- degree(RefIg(),mode='out')}
    {indeg <- degree(RefIg(),mode='in')}
    # Determine which to use to size nodes depending on sizing selection
    value <- switch(input$RefNodeSize,
                    '1' = 10,
                    '2' = outdeg,
                    '3' = indeg)
    data.frame(RefVis$nodes, indeg, outdeg, value)
  })
  

  # Plot Network ###
  # Pull Orgname of selected node for footer
  SelRef <- reactive({
    subset(RefNodes(), id==input$RefNet_selected, select=label)
  })
  output$RefEgoLabel <- renderText(paste0(ClickNode1,as.character(SelRef()),ClickNode2))
  # Draw network
  output$RefNet <- renderVisNetwork({
    {
      visNetwork(RefNodes(), RefEdges) %>%
        visIgraphLayout(physics=FALSE, type = "full", layout = "layout_nicely") %>%
        visNodes(scaling=list(min=10, max=30), font='20px arial') %>%
        visOptions(nodesIdSelection=TRUE, selectedBy="group", 
                   highlightNearest=list(
                     enabled=TRUE, labelOnly=FALSE)
        ) %>%
        visLayout(improvedLayout=TRUE) %>%
        visInteraction(hover=TRUE, navigationButtons=TRUE) %>%
        visEdges(color="#545454",
                 arrows = list(to = list(enabled = TRUE, scaleFactor = 0.7)), width=0.5)
    }
  })
  
  # Table of network stats ###
  # Create a data frame that will get modified to a datatable here and also used as a download later
  RefNetStatsDF <- reactive({
    # Network statistics
    {Size <- vcount(RefIg())}
    {Links <- ecount(RefIg())}
    {Density <- edge_density(RefIg(),loops=FALSE)}
    # The following calculates a bunch of things, a few of which we pull out
    {DegCent <- centr_degree(RefIg(), mode="total", loops=FALSE, normalized=TRUE)}
    {AverageDegree <- round(mean(DegCent$res),1)}
    {Isolates <- length(which(DegCent$res==0))}
    {OutDegCent <- centr_degree(RefIg(), mode="out", loops=FALSE, normalized=TRUE)}
    {OutDegCent2 <- OutDegCent$centralization}
    {InDegCent <- centr_degree(RefIg(), mode="in", loops=FALSE, normalized=TRUE)}
    {InDegCent2 <- InDegCent$centralization}
    {TotDegCent <- DegCent$centralization}
    df <- data.frame(Size,Links,Density,AverageDegree,Isolates,OutDegCent2,InDegCent2,TotDegCent)
    colnames(df) <- c("Size","Links","Density","AverageDegree","Isolates","OutDegreeCentralization",
                      "InDegreeCentralization","TotalDegreeCentralization")
    df
  })
  # Need to render a datatable for the column tooltips to work, but don't want any of the
  # other features, so turn them off with the options argument
  RefNetStats <- reactive({
    datatable(RefNetStatsDF(),container=sketchD,caption="Hover over headers for more details",
              rownames=FALSE,
              options=list(ordering=FALSE,lengthChange=FALSE,paging=FALSE,searching=FALSE,
                           info=FALSE)
    ) %>%
      formatPercentage("Density", 1) %>%
      formatRound("AverageDegree", 1) %>%
      formatRound(c("OutDegreeCentralization", "InDegreeCentralization", 
                    "TotalDegreeCentralization"), 3)
  })
  # Render the table
  output$RefTable <- DT::renderDataTable(RefNetStats())
  
  # Selected individual & network average bar plots ###
  # Set up dataframe to drive plots
  RefBarStats <- reactive({
    req(input$RefNet_selected) # Suppresses this and bar plots until a node is selected
    data.frame(label=c('Selected','Average'),
               outdeg=c(RefNodes()$outdeg[which(RefNodes()$id==input$RefNet_selected)],
                        mean(RefNodes()$outdeg)),
               indeg=c(RefNodes()$indeg[which(RefNodes()$id==input$RefNet_selected)],
                       mean(RefNodes()$indeg)),
               color=c(RefNodes()$color[which(RefNodes()$id==input$RefNet_selected)],
                       'black'),
               stringsAsFactors=FALSE)
  })
  # Render plots
  # Outdegree
  output$RefOutdegBarPlot <- renderPlot({
    yolim <- c(0,max(RefBarStats()$outdeg)*1.2)
    barplot(RefBarStats()$outdeg,names.arg=RefBarStats()$label,
            col=RefBarStats()$color,main='# We send to',ylim=yolim,las=1,border=NA)
    text(x=c(0.7,1.9),y=RefBarStats()$outdeg,labels=round(RefBarStats()$outdeg,digits=1),pos=3)
  })
  # Indegree
  output$RefIndegBarPlot <- renderPlot({
    yilim <- c(0,max(RefBarStats()$indeg)*1.2)
    barplot(RefBarStats()$indeg,names.arg=RefBarStats()$label,
            col=RefBarStats()$color,main='# We recieve from',ylim=yilim,las=1,border=NA)
    text(x=c(0.7,1.9),y=RefBarStats()$indeg,labels=round(RefBarStats()$indeg,digits=1),pos=3)
  })
  
 
  
})