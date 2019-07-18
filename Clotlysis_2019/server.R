library(shiny)
library(xlsx)
library(dplyr)
#library(ggplot2)
library(plotly)

        
function(input, output){
  
  readData <- reactive({
    
    inputFile <- input$data
    if (is.null(inputFile)) 
      return(read.csv(file.path("./Data/ClotLysisDoses.csv")))
    #return(read.csv(file.path("./Data/clotlysistrim.csv")))
    
    else( 
    switch(input$fil,
            "xlsx"= read.xlsx(inputFile$datapath, 1),
            "csv" = read.csv(inputFile$datapath),
            "csv2"= read.csv2(inputFile$datapath),
            "txt" = read.table(inputFile$datapath, sep ="\t", header = TRUE)
                               ))
  })
  
  readDataS <- reactive({
    
    plate00<-readData()
    plate1<-plate00[,-1]
    Ts<-plate00[,1]
    dataCols=length(plate1[1,]) #tests the width of the data, usually 96 columns
    npoints=input$spln
    yim<-matrix(rep(0, length(plate1[1,])*npoints), nrow=npoints)
    maxx<-max(Ts)
    #minx<-min(Ts)
    minx<-ifelse(input$splt0==0, Ts[1], input$splt0)
    for(i in 1:dataCols){
      splRes<-spline(Ts,plate1[,i], n=npoints, xmin = minx, xmax = maxx-input$Spltend,   method = "natural")
      yim[,i]<-splRes$y 
      
    }
    plate1s<-cbind(splRes$x, yim)
    colnames(plate1s)<-colnames(plate00)
    data.frame(plate1s)
    
  })
  
  ##Data points actual or interpolated
  
  plate0<-reactive({
    
    switch(input$spline, 
           "Raw data"=plate0<-readData(),
           "Fitted points"=plate0<-readDataS()
    )
  })
  
  
  whichfile <- reactive({
    
    inputFile <- input$data
    if (is.null(inputFile)) 
    return(file.path("./Data/ClotLysisDoses.csv"))
    #return(file.path("./Data/clotlysistrim.csv"))
    #return(file.path(readData()))
    else(file.path(inputFile))
    
    
  })
  
  
  output$which <- renderUI({
    whichfile()
   })
  
  output$whichraw <- renderUI({
    whichfile()
  })
  
  output$setfile <- renderUI({
    whichfile()
  })
  
  var<- reactive({
    plate0<-readData()
    plate1<-plate0[,-1]
    mycols<-colnames(plate1)
  })
  
  
  
  output$what<-renderUI({
    selectInput("colmnames",
                label= h5("Select a column of absorbance data"),
                choices = var())
  }) 
  
  output$nowwhat<-renderUI({
    selectInput("colmnames",
                label= h5("Select a column of absorbance data"),
                choices = var())
  })
  
  
  
          ###Table of results
 TabRes<-reactive({
   if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   
   
   Time<-round(plate0()[,1], 2)
   Datwells<-plate0()[,-1]
   
   initiation<-input$num
   pointsnum<-length(Datwells[,1])-1
   absWells<-length(Datwells[1,])
   
   
   all.zero<-1:absWells #2
   
   all.clot.Time<-1:absWells #3
   all.clot.Abs<-1:absWells #4
   
   all.max.Abs<-1:absWells #5
   all.max.Absz<-1:absWells #6
   all.max.Time<-1:absWells #7
   
   all.clot.to.max<-1:absWells #8
   
   all.lysis.Time<-1:absWells #9
   all.lysis.Abs<-1:absWells  #10
   
   all.clot.to.lysis<-1:absWells #11
   all.max.to.lysis<-1:absWells #12
   
   all.full.lysis.Time<-1:absWells #13
   all.clot.to.full.lysis.Time<-1:absWells #14
   
   all.max.to.decay<-1:absWells #peak to full lysis not included
   
   all.AUC<-1:absWells #15
   
   all.time.inc<-1:absWells #16
   all.time.dec<-1:absWells #17
   all.time.zero<-1:absWells #18
   
   all.startpoint<-1:absWells #Extra points for graphing
   all.pointmax<-1:absWells
   all.decaypoint<-1:absWells
   all.decaypointmin<-1:absWells
   
   
   samples<-names(Datwells)
   
   for(i in 1:absWells){ 
     yi<- Datwells[,i] # go through data cols (wells) 1:96
     t<-Time
     #SET ZERO
     switch(input$abini,
            "global zero"=minAbs<-input$minabs,
            "nth absorbance"=minAbs<-Datwells[input$first,i],
            "min abs + offset below"=minAbs<-min(yi)+input$offset)
     minAbs<-round(minAbs, 3)
     
     #FIND MAX
     yimax<-max(yi)    #max absorbance
     pointmax<-which.max(yi)
     upTime<-t[c(1:pointmax)] #vector of time to max
     upAbs<-yi[c(1:pointmax)]  #vector of absorbances to max
     
     MaxTime<-t[pointmax]#The time for the max absorbance
     MaxAbs<-yi[pointmax]#The maximum absorbance
     MaxAbs_z<-MaxAbs-minAbs
     
     #START
     pcChange<-initiation*(MaxAbs-minAbs)+minAbs
     startPoint<-which(abs(upAbs-pcChange)==min(abs(upAbs-pcChange)))[1]
     startTime<-upTime[startPoint]
     startAbs<-upAbs[startPoint]
     
     switch(input$interpolate,
            "Interpolate"= startAbs<-round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x,3),
            "Nearest point" = startAbs<-startAbs)
     
     switch(input$interpolate,
            "Interpolate"= startTime<-round(approx(upAbs, upTime, xout = startAbs, ties = mean)$y,3),
            "Nearest point" = startTime<-startTime)
     
     timetoStart<-upTime[1:startPoint]
     abstoStart<-upAbs[1:startPoint]
     timeafterStart<-t[-c(1:startPoint)]
     absafterStart<-yi[-c(1:startPoint)]
     
     #ALL DECAY
     decayTimeall<-t[-c(1:pointmax)] #vector of all lysis Time
     decayAbsall<-yi[-c(1:pointmax)] #vector of all lysis absorbances
     if(minAbs>=min(decayAbsall)){pointmin<-which(decayAbsall<=minAbs)[1]}  else{(pointmin<-length(decayAbsall) )}
     decayTime<-decayTimeall[1:pointmin]#vector of lysis times to set minimum
     decayAbs<-decayAbsall[1:pointmin] #vector of lysis absorbances to set minimum
     decayTimeend<-max(decayTime)
     
     switch(input$interpolate, 
            "Interpolate" = decayTimeend<-round(approx(decayAbs, decayTime, xout = minAbs, ties = mean)$y,3),
            "Nearest point" = decayTimeend<-decayTimeend)
     
     #DECAY TO CHOSEN POINT
     decayPoint<-which(abs(decayAbsall-pcChange)==min(abs(decayAbsall-pcChange)))[1]
     
     
     decayTimePC<-decayTimeall[1:decayPoint]#vector of time from maximum to chosen lysis
     decayAbsPC<-decayAbsall[1:decayPoint]#vector of abs from maximum to chosen lysis
     decayAbsPoint<-decayAbs[decayPoint]
     decayTimePoint<-decayTime[decayPoint]
     
     switch(input$interpolate,
            "Interpolate"= decayAbsPoint<-round(approx(decayTime, decayAbs, xout = pcChange, ties = mean)$x,3),
            "Nearest point" = decayAbsPoint<-decayAbsPoint)
     
     switch(input$interpolate,
            "Interpolate"= decayTimePoint<-round(approx(decayAbs, decayTime, xout = decayAbsPoint, ties = mean)$y,3),
            "Nearest point" = decayTimePoint<-decayTimePoint)
     
     #INTERNAL CALCULATIONS
     startTomax<-MaxTime-startTime
     startTodecayPC<-decayTimePoint-startTime
     maxTodecayPC<-decayTimePoint-MaxTime
     startToend<-decayTimeend-startTime
     AUC<-sum(diff(t[1:(pointmax+pointmin)]) * (head(yi[1:(pointmax+pointmin)],-1)+tail(yi[1:(pointmax+pointmin)],-1)))/2
     
     #FIRST DERIVATIVE
     #up
     yiD1u<-diff(upAbs)
     tD1u<-upTime[-1]
     timeMaxD1<-tD1u[which.max(yiD1u)]
     
     #down
     yiD1d<-diff(decayAbs)
     tD1d<-decayTime[-1]
     timeMinD1<-tD1d[which.min(yiD1d)]
     
    
     #middle
     timeStarttoPeak<-upTime[-c(1:startPoint)]
     absStarttoPeak<-upAbs[-c(1:startPoint)]
     middleT<-c(timeStarttoPeak, decayTimePC)
     middleAbs<-c(absStarttoPeak, decayAbsPC)
     Myupdn<-c(0, diff(middleAbs))
     Mychange<-(which(Myupdn<0)[1])-1 #-1 to give last point before sign change
     time0D1<-middleT[Mychange]
     #abs0D1<-middleAbs[Mychange]
    
     
     
     #COLLECT DATA
     #samples #1 names
     all.zero[i]<-minAbs #2
     all.clot.Time[i]<-startTime #3 initiation point
     all.clot.Abs[i]<-round(startAbs, 3) #4 initiation reading
     all.max.Abs[i]<-round(MaxAbs, 3) #5 peak
     all.max.Absz[i]<-round(MaxAbs_z, 3)#6 peak -zero
     all.max.Time[i]<-MaxTime #7  time to peak       
     all.clot.to.max[i]<-startTomax #8 initiation to peak
     all.lysis.Time[i]<-decayTimePoint #9 time to decay point from zero
     all.lysis.Abs[i]<-round(decayAbsPoint, 3) #10 Abs at percent lysis
     all.clot.to.lysis[i]<-startTodecayPC #11 time to decay from initiation
     all.max.to.lysis[i]<-maxTodecayPC #12 time to decay from peak
     all.full.lysis.Time[i]<-decayTimeend #13 full lysis time
     all.clot.to.full.lysis.Time[i]<-startToend #14 clotting to full lysis
     all.AUC[i]<-round(AUC, 3) #15 AUC
     
     all.time.inc[i]<-timeMaxD1 #16 max up
     all.time.dec[i]<-timeMinD1 #17 max down
     all.time.zero[i]<-time0D1 #18 sign change
     
     all.startpoint[i]<-startPoint # 19 Extra points for graphing
     all.pointmax[i]<-pointmax #20
     all.decaypoint[i]<-decayPoint+pointmax #21
     all.decaypointmin[i]<-pointmin+pointmax #22
     
   }
   
   resDat<-data.frame(samples, all.zero, all.clot.Time, all.clot.Abs,
                      all.max.Abs, all.max.Absz, all.max.Time,
                      all.clot.to.max, all.lysis.Time, all.lysis.Abs,
                      all.clot.to.lysis, all.max.to.lysis,
                      all.full.lysis.Time, all.clot.to.full.lysis.Time,
                      all.AUC, all.time.inc, all.time.dec, all.time.zero,
                      all.startpoint, all.pointmax, all.decaypoint, all.decaypointmin
                      )
  
  
   #write.table(resDat,"clipboard",  sep="\t", col.names = T, row.names = F)
   resDat
   
 }) 
 
 
 varnames<- reactive({
   mynames<-colnames(TabRes())
 })
 
 output$whatx<-renderUI({
   if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   selectInput("mynamesx",
               label= h5("On X axis"),
               choices = varnames(), selected = colnames(TabRes()[1]))
 })
 
 output$whaty<-renderUI({
   if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   selectInput("mynamesy",
               label= h5("On Y axis"),
               choices = varnames(), selected = colnames(TabRes()[9]))
   
 })
 
 output$whatheat<-renderUI({
   if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   selectInput("mynamesheat",
               label= h5("Choose data for heatmap"),
               choices = varnames(), selected = colnames(TabRes()[9]))
   
 })
 
 #output$exploreplot<-renderPlot({
   
 output$exploreplot<-renderPlotly({
   if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   N<-TabRes()[,1]
    X<-TabRes()[,input$mynamesx]
   Y<-TabRes()[,input$mynamesy]
   Z<-matrix(TabRes()[,input$mynamesheat], byrow=TRUE, nrow=input$numrows)
   switch(input$heat,
          "heat" = plot_ly(z= Z, colors = colorRamp(c("grey", "pink", "red")),  type = "heatmap") %>% layout(yaxis = list(autorange = "reversed")), 
          "scatter" = plot_ly(type = "scatter", mode = "markers") %>% 
          add_trace(x=X, y=Y, text = N, marker = list(size = 10, color = "pink", line = list(color="red", width = 2)), hoverinfo = N) )
   
 })
 ###RESULTS TABLE FOR MULTIPLE PLOTS###
 
 plotTabl<-reactive({
 if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   RowNum<-input$numrows
   
  
   data<-switch(input$tabRes, 
                "1"=matrix(TabRes()[,1], byrow=TRUE, nrow=RowNum),
                "2"=matrix(TabRes()[,2], byrow=TRUE, nrow=RowNum),
                "3"=matrix(TabRes()[,3], byrow=TRUE, nrow=RowNum),
                "4"=matrix(TabRes()[,4], byrow=TRUE, nrow=RowNum),
                "5"=matrix(TabRes()[,5], byrow=TRUE, nrow=RowNum), 
                "6"=matrix(TabRes()[,6], byrow=TRUE, nrow=RowNum),
                "7"=matrix(TabRes()[,7], byrow=TRUE, nrow=RowNum), 
                "8"=matrix(TabRes()[,8], byrow=TRUE, nrow=RowNum),
                "9"=matrix(TabRes()[,9], byrow=TRUE, nrow=RowNum),
                "10"=matrix(TabRes()[,10], byrow=TRUE, nrow=RowNum),
                "11"=matrix(TabRes()[,11], byrow=TRUE, nrow=RowNum),
                "12"=matrix(TabRes()[,12], byrow=TRUE, nrow=RowNum),
                "13"=matrix(TabRes()[,13], byrow=TRUE, nrow=RowNum),
                "14"=matrix(TabRes()[,14], byrow=TRUE, nrow=RowNum),
                "15"=matrix(TabRes()[,15], byrow=TRUE, nrow=RowNum),
                "16"=matrix(TabRes()[,16], byrow=TRUE, nrow=RowNum),
                "17"=matrix(TabRes()[,17], byrow=TRUE, nrow=RowNum),
                "18"=matrix(TabRes()[,18], byrow=TRUE, nrow=RowNum)
                
   )
   
   colnames(data) =as.character(1:(length(data)/RowNum))
   #write.table(data, "clipboard", sep="\t", col.names=F, row.names=F) #OPTIONAL- live if local
   #write.table(data, "TscopeRRes.txt", sep="\t", col.names=F, row.names=F)
   data
   
 })
  
 output$resultsTable<-renderTable({
 plotTabl()
})
 
          ###MULTIPLE PLOTS##
  output$myplotAll<-renderPlot({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    
    Time<-round(plate0()[,1], 2)
    Datwells<-plate0()[,-1]
    
    pointsnum<-length(Datwells[,1])-1
    absWells<-length(Datwells[1,])
    samples<-names(Datwells)
    
    initiation<-input$num
    RowNum<-input$numrows
    
    par(mfrow=c(RowNum,(absWells/RowNum)))
    par(mar=c(0.2,0.2,0.2,0.2)) # dimensions for figure
   
    mint<-min(Time)
      
    for (k in 1: absWells ) {
      
      yi<- Datwells[,k] 
      #t<-Time
      
      plots<-plot(Time, yi, type = "l", col= "slategrey", lwd = 3, xlim= c(input$xrange[1], input$xrange[2]), 
                  ylim=c(input$yrange[1], input$yrange[2]), xaxt="n", yaxt="n")
      lines(Time[TabRes()[k,19]:TabRes()[k,20]], yi[TabRes()[k,19]:TabRes()[k,20]],col="green", lwd=3)
      lines(Time[TabRes()[k,20]:TabRes()[k,21]], yi[TabRes()[k,20]:TabRes()[k,21]],col="blue", lwd=3)
      lines(Time[TabRes()[k,21]:TabRes()[k,22]], yi[TabRes()[k,21]:TabRes()[k,22]],col="red", lwd=3)
      
      switch(input$tabRes,
      "3"=abline("v"= TabRes()[k,3], lty=2),
      "7"=abline("v"= TabRes()[k,7], lty=2),
      "9"=abline("v"= TabRes()[k,9], lty=2),
      "13"=abline("v"= TabRes()[k,13], lty=2),
      "16"=abline("v"= TabRes()[k,16], lty=1, col="magenta"),
      "17"=abline("v"= TabRes()[k,17], lty=1, col="magenta"),
      "18"=abline("v"= TabRes()[k,18], lty=1, col="magenta"),
      "2"=abline("h"= TabRes()[k,2], lty = 2),
      "10"=arrows(mint,TabRes()[k,4], TabRes()[k,9],TabRes()[k,10],  length = 0.05, angle = 10),
      "4"=arrows(mint,TabRes()[k,4], TabRes()[k,3], TabRes()[k,4], length = 0.05, angle = 10),
      "5"=arrows(mint,TabRes()[k,5], TabRes()[k,7], TabRes()[k,5], length = 0.05, angle = 10),
      "6"=arrows(mint,TabRes()[k,5], TabRes()[k,7], TabRes()[k,5], length = 0.05, angle = 10),
      "11"=arrows(TabRes()[k,3],TabRes()[k,4], TabRes()[k,9], TabRes()[k,10], length = 0.05, angle = 10),
      "8"=arrows(TabRes()[k,3],TabRes()[k,4], TabRes()[k,7], TabRes()[k,4], length = 0.05, angle = 10),
      "12"=arrows(TabRes()[k,7], TabRes()[k,4],TabRes()[k,9],TabRes()[k,10],  length = 0.05, angle = 10),
      "14"=arrows(TabRes()[k,3], TabRes()[k,4],TabRes()[k,13],TabRes()[k,10],  length = 0.05, angle = 10)
      )
      }
    
    
  })
  
 
    ###SINGLE CURVE##
  output$myplot<-renderPlot({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   
    
    
    Time<-round(plate0()[,1], 2)
    Datwells<-plate0()[,-1]

    mint<-min(Time)
   
    yi<-Datwells[,input$colmnames] # Rather than get and attach, this just reads in the column chosen earlier
    
    k<-which(TabRes()[, 1]==input$colmnames)
    yiD1<-diff(yi)
    tiD1<-Time[-1]
    
    
    plot1<-plot(Time, yi, type = "l", col= "slategrey", lwd = 3, xlim= c(input$xrange[1], input$xrange[2]), 
                ylim=switch(input$curveRes, 
                            "All"=c(input$yrange[1], input$yrange[2]),
                            "Generation"=c(input$yrange[1], input$yrange[2]), 
                            "Decay"=c(input$yrange[1], input$yrange[2]),
                            "1st Derivative (magenta)"=c(min(yiD1), input$yrange[2])),
                main=paste("clotting and lysis for",input$colmnames), xlab = "Time", ylab = "Absorbance")
    
    lines(Time[TabRes()[k,19]:TabRes()[k,20]], yi[TabRes()[k,19]:TabRes()[k,20]],col="green", lwd=3)
    lines(Time[TabRes()[k,20]:TabRes()[k,21]], yi[TabRes()[k,20]:TabRes()[k,21]],col="blue", lwd=3)
    lines(Time[TabRes()[k,21]:TabRes()[k,22]], yi[TabRes()[k,21]:TabRes()[k,22]],col="red", lwd=3)
    switch(input$spline,
           "Raw data"=points(Time, yi, pch="o", col="slategrey"),
           "Fitted points"=points(Time, yi, pch="."))
    
    #switch(input$tabRes,  # for single curves show all lines
           "3"=abline("v"= TabRes()[k,3], lty=2)
           "7"=abline("v"= TabRes()[k,7], lty=2)
           "9"=abline("v"= TabRes()[k,9], lty=2)
           "13"=abline("v"= TabRes()[k,13], lty=2)
           "16"=abline("v"= TabRes()[k,16], lty=1, col="magenta")
           "17"=abline("v"= TabRes()[k,17], lty=1, col="magenta")
           "18"=abline("v"= TabRes()[k,18], lty=1, col="magenta")
           "2"=abline("h"= TabRes()[k,2], lty = 2)
           "10"=arrows(mint,TabRes()[k,4], TabRes()[k,9],TabRes()[k,10],  length = 0.05, angle = 10)
           "5"=arrows(mint,TabRes()[k,5], TabRes()[k,7], TabRes()[k,5], length = 0.05, angle = 10)
           "11"=arrows(TabRes()[k,3],TabRes()[k,4], TabRes()[k,9], TabRes()[k,10], length = 0.05, angle = 10)
           "8"=arrows(TabRes()[k,3],TabRes()[k,4], TabRes()[k,7], TabRes()[k,4], length = 0.05, angle = 10)
           "12"=arrows(TabRes()[k,7], TabRes()[k,4],TabRes()[k,9],TabRes()[k,10],  length = 0.05, angle = 10)
           "14"=arrows(TabRes()[k,3], TabRes()[k,4],TabRes()[k,13],TabRes()[k,10],  length = 0.05, angle = 10)
   # )
    
    yiD1<-diff(yi)
    tiD1<-Time[-1]
    
           
           plot1<-switch(input$curveRes, 
                 "All"=plot1,
                 "Generation"=plot1, 
                 "Decay"=plot1,
                 "1st Derivative (magenta)"=points(tiD1, yiD1))
    

  })
  
  ###CURVE RESULTS TABLE###
  
  output$curveTable<-renderTable({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    TabNames<-colnames(TabRes())
    ColNam<-c("Parameter", "Result")
    
    All.Res<-TabRes() %>% filter(samples == input$colmnames) %>% select(1:15)
    All.Res.Tab<-cbind(TabNames[1:15], t(All.Res))
    colnames(All.Res.Tab)<-ColNam
    
    Generation.Res<-TabRes() %>% filter(samples == input$colmnames) %>% select(1:8)
    Generation.Res.Tab<-cbind(TabNames[1:8], t(Generation.Res))
    colnames(Generation.Res.Tab)<-ColNam
    
    Decay.Res<-TabRes() %>% filter(samples == input$colmnames) %>% select(1, 9:14)
    Decay.Res.Tab<-cbind(TabNames[c(1, 9:14)], t(Decay.Res))
    colnames(Decay.Res.Tab)<-ColNam
    
    Deriv.Res<-TabRes() %>% filter(samples == input$colmnames) %>% select(1, 16:18)
    Deriv.Res.Tab<-cbind(TabNames[c(1, 16:18)], t(Deriv.Res))
    colnames(Deriv.Res.Tab)<-ColNam
    
    
    
    curveDat<-switch(input$curveRes,
                     "All"= All.Res.Tab,
                     "Generation"= Generation.Res.Tab,
                     "Decay"= Decay.Res.Tab,
                     "1st Derivative (magenta)"= Deriv.Res.Tab
                    
                     
    )
    
  })
  
  #Collection of setting for records
  setsTab<-reactive({
    
    setTable<-matrix(c( 
      "Date", format(Sys.Date(), "%d %b %Y"), "Time", format(Sys.time(), "%X"),
      
      "Read interval s", readData()[2,1]-readData()[1,1],  "Chosen % clotting or lysis", input$num*100,
      
      "Nearest point or interpolate", input$interpolate, "", "",
      
      "Raw or fitted",  input$spline,  "",  "",
      
      "Fit start", input$splt0,  "Truncate for fit", input$Spltend, 
      
      "Read interval with fit", round(readDataS()[2,1]-readDataS()[1,1], 4), "Number of points in fit", input$spln,
      
      "Zero method", input$abini,  "", "",
      
      "", "","Global zero", input$minabs, 
      
      "", "","nth point", input$first,
      
       "", "", "offset with min abs", input$offset
      
    
    ),
    
    byrow=TRUE, nrow=10)
    
    colnames(setTable)<-c("Parameter", "Value", "Parameter", "Value")
    setTable
  })
  


output$settings<-renderTable({
  if(is.null(input$colmnames)){return(NULL)}
  setTable<-setsTab()
  
  setTable
})
  
  
  output$contents<-renderDataTable({
   
 TabRes()
    
  })
  
  output$raw<-renderTable({
    
    plate0()
  })
  
  output$text3<-renderText({
    paste(names(TabRes()[as.numeric(input$tabRes)]),"for ",input$num*100, "%", "of maximum")
    
  })
  
  output$text4<-renderText({
    paste("Results for",input$tabRes)
    
  })
}