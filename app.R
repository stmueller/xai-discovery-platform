#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(ca)
library(rjson)
library(shinyWidgets)
library(ggplot2)
library(esquisse)


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$script(src = "fns.js"),
    tags$script(HTML("function setCase(id){Shiny.setInputValue(\'cases\',id);Shiny.setInputValue('tabs','cases',{priority: 'event'});};")),
    #tags$script(HTML("function setCase(id){Shiny.setInputValue('tabs','cases');}")),
  
    # Application title
    verticalLayout(
    htmlOutput("title"),
    htmlOutput("subtitle")),
    # Sidebar with a slider input for number of bins 
           tabsetPanel(id="tabs",
            tabPanel("Cases",value="cases",
                     sidebarLayout(sidebarPanel(
                             
                     helpText("Select case and see similar images"),
                     selectInput("cases","Selected Case:",selectize=F,
                        c(1:10),multiple=F),  ##1:10 is just a dummy entry
                     numericInput("casesID","or manually enter case ID",value=0),
                checkboxInput("errors","Browse errors only",value=F),
                radioButtons("selection","Select options to explore",
                         inline=FALSE,
                               choices=c( "Show random cases that match class and label"="caseMatch",
                                            "Show random correct cases"="corrMatch",
                                 "Show random error cases that match class but not label"="truthMatch" ,
                                 "Show closest correct examples" ="targCorr",
                                 "Show closest incorrect examples"="targInc",
                                 "Show closest class/label matches" = "match")
                  ),
                actionButton("redo","Resample")
             ),
             # Show a plot of the generated distribution
             mainPanel(
                 verticalLayout(
                     flowLayout(
                       
                       htmlOutput("header"),
                       plotOutput(outputId="main_plot",height="300px",width="500px")),
                       htmlOutput("browser"),
                       dataTableOutput("browserDT"))

             ))),
        tabPanel("Categories",value="categories",
                 sidebarLayout(sidebarPanel(
                 helpText("See examples of class/label comparisons"),
                 selectInput("groundB","Ground truth class:",c(1)),
                 selectInput("respB","Classification label:",c(1)),
                 radioButtons("selectCatB","What to explore?",
                              choices=c("Random cases"="random",
                                        "Most like ground truth"="bestgt",
                                        "Least like ground truth"="worstgt",
                                        "Most like label"="best",
                                        "Least like label"="worst"
                                        ))
                ),
                mainPanel(  verticalLayout(
                    htmlOutput("header2"),
                    htmlOutput("browser2"),
                    plotOutput(outputId='error_plot',height=450,width=600),
                    plotOutput(outputId='CA_plot',height=600,width=600)
                    
                )))),
        tabPanel("Contrasts",value="contrasts",
                 sidebarLayout(sidebarPanel(
                  helpText("Compare two classes and labels"),
                  selectInput("ground1C","Class1:",c(0:9),selected=11),
                  selectInput("resp1C","Label1:",c(0:9),selected=11),

                  selectInput("ground2C","Class2:",c(0:9),selected=11),
                  selectInput("resp2C","Label2:",c(0:9),selected=11),
                  radioButtons("selectCatC","What to explore?",
                              choices=c("Random cases"="random",
                                        "Worst match to contrast"="worst",
                                        "Best match to contrast"="best"))
                ),
                mainPanel(htmlOutput("header3"),
                          htmlOutput("browser3")) )),
         
    tabPanel("Data Filtering",value="data",
            sidebarLayout(
              
              sidebarPanel(
                progressBar(
                  id = "pbar", value = 100, 
                  total = 100, display_pct = TRUE
                 ),
                filterDF_UI(id="filtering")),
              mainPanel(
                   DT::dataTableOutput(outputId = "table")
            
                 
               )))),
    htmlOutput("about")
           
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  session$allowReconnect(TRUE)
  
  
  dataRaw <- reactive({
    
    dat <- read.csv("main.csv")
    dat$row <- 1:nrow(dat)
    ##make the classID and labelID are used to look up
    ## probabilities, so the need to be indexed starting at 1,
    ## and need to be in the same order that the probability matrix is in.
    ## that order is either what is  stored in info$labelnames, or
    ## if labelNames does not exist, the natural order of labelID,
    ## as determined in prob().
    info <- info()
    
    if(!is.null(info[["labelNames"]]))
    {
      levs <- info[["labelNames"]]
      
    }else{
      ## if not specified, just use the levels of the IDs
      levs <- levels(as.factor(unlist(dat$labelID)))
    } 
    
    dat$labelID <- factor(dat$labelID,levels=levs)
    
 
    
    
    dat$class <- factor(dat$class,levels= sort(unique(dat$class)))
    dat$label <- factor(dat$label,levels=sort(unique(dat$label)))
    dat
  })
  

  
  probsRaw <- reactive({
    ##probs are saved in a semi-sparse matrix format; 10 columns of labels with 10 columns of probabilities.
    ## These should be the highest-rated alternative answers.
    ## we re-create a standard matrix of columns/rows here. If this is very large
    ## there may be a sparse matrix package that could handle this. Note that the purpose of this
    ## is to measure similarity across response patterns.
    
    dat <- read.csv("probs.csv")
    
    ##IDs come from the probs file, and need to be indexed 1...N
    ids <- as.matrix(dat[,1:10])  ##there are exactly 10 columns of ids, and 10 columns of probabilities

    info <- info()    ##use the levels in the order specified in info.
                      ##We could check to here and just use the data levels if classNames

        ##You can specify labelnames in the json file as this specifies the order
    ## they will appear in relevant displays.
    data <- data()
#    levs <- data$labels
    levs <- unique(levels(as.factor(ids)))
    ##make each column a factor. There are no factor-based matrices in R, so we need
    ##to use a data frame and handle each column separately.
 #   for(i in 1:10)
#    {
#       ids[,i] <- factor(ids[,i],levels=levs)
#    }
    
    ##make temporary normal matrix.
    base <- matrix(0,nrow=nrow(dat),ncol=length(levs))
    probs <- as.matrix(dat[,11:20])    
    probs[is.na(probs)] <- 0
    for(i in 1:nrow(dat))
    {
    
      filt <- ids[i,!is.na(ids[i,])]
      base[i,filt] <-as.vector(probs[i,1:length(filt),drop=T] )
      
    }

    base
  })
  
  ##Data should be obtained from the filter pane.

data <- reactive({
     tmp <- res_filter$data_filtered()
     
     info <- info()    ##use the levels in the order specified in info.
     ##We could check to here and just use the data levels if classNames
     
  
     
     if(!is.null(info[["classNames"]]))
     {
       classes <- info[["classNames"]]
       
     }else{
       ## if not specified, just use the levels of the IDs
       classes <- levels(as.factor(tmp$classes))
     }
     
     
     ##You can specify labelnames in the json file as this specifies the order
     ## they will appear in relevant displays.
     if(!is.null(info[["labelNames"]]))
     {
       labels <- info[["labelNames"]]
       
     }else{
       ## if not specified, just use the levels of the IDs
       labels <- levels(as.factor(tmp$label))
     }
         list(dataTable=tmp,classes=classes,labels=labels)
    })
  
  
#  probs <- reactive({probsRaw()})
  probs <-reactive({ 
  
    data <- res_filter$data_filtered()
    probs <- probsRaw()[data$row,]
  
   list(probs=probs)
  })

  info <- reactive({
    fromJSON(file="input.json")
  })
  
  confusion <- reactive({
    dat1 <- data()
    dat <- dat1$dataTable
    info <- info()
  

    tab2 <- table(factor(dat$class,levels=dat1$classes),
                  factor(dat$label,levels=dat1$labels))
    
    prob <- aggregate(list(prob=as.character(dat$class)==as.character(dat$label)),list(class=dat$class),mean)
    list(tab=tab2,classAccuracy=prob)
  })
  
  ##This gets the current row as specified in the input$ caseID specification
  getProbRow <- reactive(
    {
      caseID <- as.numeric(input$casesID)
      probs <- probs()$probs

      tableDat <- data()$dataTable
      sel <- which(tableDat$case==caseID)
      row <- probs[sel,,drop=F]
   
      info <- info()
      if(nrow(row)>0)
      {
        names(row) <- info[["classNames"]]
      }
      
      ##we could filter by the levels of data() now.
      
      row      
    }
  )
  
  
  
  getRow <- reactive({
    
    tableDat <- data()$dataTable ##reactive main data
    probs <- probs()$probs  ##reactive probability data
    caseID <- as.numeric(input$casesID)
    sel <- which(tableDat$case==caseID)
    row <- (tableDat[sel,,drop=F])
    row
  })
  
##This flips to the first tab when a case button is clicked.  
observeEvent(input$tabs,{
    updateTabsetPanel(session, "tabs", input$tabs)
  })


observeEvent({res_filter$data_filtered()},
             {
        
               dat <- data()
               tableDat <- dat$dataTable
               if(input$errors)
               {     
                 choices <- c((tableDat$case)[!tableDat$corr])
                 names(choices) <- paste("Case ",choices," (",tableDat$class[!tableDat$corr],
                                         "->",tableDat$label[!tableDat$corr],")",sep="")
                 
                 choices <- c("Choose:"="0",choices)
                 updateSelectInput(session,"cases",choices=choices[1:min(c(500,length(choices)))] ,selected=NULL)
               }else{
                 
                 choices <- tableDat$case
                 if(length(choices)>0)
                 {
                   names(choices) <- paste("Case ",choices," (",tableDat$class,"->",tableDat$label,")",sep="")
                   choices <- c("Choose:"="0",choices)
                   updateSelectInput(session,"cases",choices=choices[1:min(c(500,length(choices)))],selected=NULL)
                 }
               }
 
               ##make labels for category browser
               targs <- 1:length(dat$classes)
               resps <- 1:length(dat$labels)
               names(targs) <- dat$classes
               names(resps) <- dat$labels
               
               ##now, filter these based on which values are actually present.
               targFilter <- is.element(dat$classes,tableDat$class)
               respFilter <- is.element(dat$labels,tableDat$label)
               targs <- targs[targFilter]
               resps <- resps[respFilter]


             if(length(choices)>0)
               {
                 updateSelectInput(session,"groundB",choices=targs)
                 updateSelectInput(session,"respB",choices=resps)
                 updateSelectInput(session,"ground1C",choices=targs)
                 updateSelectInput(session,"resp1C",choices=resps)
                 updateSelectInput(session,"ground2C",choices=targs)
                 updateSelectInput(session,"resp2C",choices=resps)
               }
               
             }
)

observeEvent(
  {
      input$errors
#      res_filter$data_filtered()
   },
    {
        tableDat <- data()$dataTable
      
        if(input$errors)
        {     
            choices <- c((tableDat$case)[!tableDat$corr])
            names(choices) <- paste("Case ",choices," (",tableDat$class[!tableDat$corr],
                                     "->",tableDat$label[!tableDat$corr],")",sep="")
            
            choices <- c("Choose:"="0",choices)
            updateSelectInput(session,"cases",choices=choices[1:min(c(500,length(choices)))] ,selected=NULL)
        }else{
          
            choices <- tableDat$case
            names(choices) <- paste("Case ",choices," (",tableDat$class,"->",tableDat$label,")",sep="")
            choices <- c("Choose:"="0",choices)
            updateSelectInput(session,"cases",choices=choices[1:min(c(500,length(choices)))],selected=NULL)
        }

      },
   priority=7)
        
##Initiation event; done once, to load UI etc.
observeEvent(TRUE,
            {  
              
              
              ##set UI elements from json file.
              info <- info()
              output$title <- renderText({paste("<h1>",info[["title"]],"</h1>")})
              output$subtitle <- renderText({paste("<h3>",info[["subtitle"]],"</h1>")})
              output$about <- renderText(paste(info$contact,"<br>",info$about))

                                                                    
## This is fussy code that let's you specify a particular case via url parameter.       
## It currently does not work, but earlier version did; I think another observe is 
## blocking this somehow.  Also, we no longer use this to specify a new image from the 
## browser table.
if(FALSE){
        ##once updated, we should pull the url info in and default to that.
        query <- parseQueryString(session$clientData$url_search)
        if(!is.null(query$case))
        {
            updateSelectInput(session,"cases",selected=1)
            updateNumericInput(session,"casesID", value=as.numeric(query$case)) 
        }
}
       # updateCheckboxInput(session,'errors',TRUE)
        
      },
  priority=14,once=TRUE)
  
   

    ##handle a case pulldown change.
observeEvent(input$cases,{
       
 
        ##update the number entry when the pulldown is adjusted:      
        ##but only if the 'empty' pulldown is not selected.
        caseID <- as.numeric(input$cases)

        if(!is.na(caseID)&caseID>0)
            updateNumericInput(session,"casesID",value=(caseID)) 
        
    },priority=11)

##########################################
## Case browser
    
    ##hit 'redo' button.
    observe({
      
      ##header and footer should reset if the resample button is hit.
      ##there must be a better way to do this, but I don't know how.
      if(input$redo)
           {print("rodeoing")}

    
output$header <-  renderText({
   
    
    tableDat <- data()$dataTable ##reactive main data
    probs <- probs()$probs  ##reactive probability data
    caseID <- as.numeric(input$casesID)
    sel <- which(tableDat$case==caseID)
    row <- (tableDat[sel,])

    fname<- row$fname
    top <- paste( 
                "<h4>SELECTED EXAMPLE:</h4><br>",
                "<table border=0><tr><td><strong>Case:",row$case,
                "<tr><td><strong>Ground truth class:</strong> ", row$class,"<br>",
                "<strong>Classification label:</strong> ", row$label,"<br></td><td>",
             
              '<br><img src = "',fname, '" width=100></td></tr>',
              sep="")
        top
       })
       
   output$browser <- renderText({
     numExamples <- 15
     dat <- data()
     tableDat <- dat$dataTable ##reactive main data
     probs <- probs()$probs  ##reactive probability data
     
     
     caseID <- as.numeric(input$casesID)
     sel <- which(tableDat$case==caseID)
     row <- (tableDat[sel,])
     debug <- F

              
      #  site <-paste(session$clientData$url_protocol,"//",
      #               session$clientData$url_hostname,":",
      #               session$clientData$url_port,
      #               session$clientData$url_pathname,sep="")
        
        titles <- list("corrMatch" ="Random correct",
                    "caseMatch" = "Random class/label match",
                  "truthMatch" = "Random class matches with another label" ,
                  "targCorr"="Best correct class matches" ,
                  "targInc"="Best incorrect class matches",
                  "match"="Best class/label matches")
    
        target <- (row$class)
        resp   <- (row$label)
        targetID <- row$classID
        respID <- row$labelID
        ##update the category/contrast screen

        targs <- dat$classes
        resps <- dat$labels
        tselect <- which(target==targs)
        rselect <- which(resp==resps)
  
        updateSelectInput(session,"groundB",selected=tselect)
        updateSelectInput(session,"respB",selected=rselect)
        updateSelectInput(session,"ground1C",selected=tselect)
        updateSelectInput(session,"resp1C",selected=rselect)
        updateSelectInput(session,"ground2C",selected=tselect)
        updateSelectInput(session,"resp2C",selected=rselect)

        #############################################
        ## Match the target/response categories.
        explore <- ""  
          if(input$selection=="corrMatch")
        {
            match <- which(tableDat$class==target & tableDat$corr)
            rows <- samp(match,min(c(numExamples,length(match))))
            explore<- paste(explore,"<h3 align='center'>Examples of correct ",target,"</h3>")
            
        } else if(input$selection=="caseMatch")
          {
          
          

            match <- which(tableDat$class==target & tableDat$label == resp)
        
            rows <- samp(match,min(c(numExamples,length(match))))

            explore<- paste(explore,"<h3 align='center'>Examples of class/label matches</h3>")
         
        
        } else if(input$selection=="truthMatch")
        {
         
            match <- which(tableDat$class==target & tableDat$label != resp &   tableDat$corr==F)
            rows <- samp(match,min(c(numExamples,length(match))))
     
            
            explore<- paste(explore,"<h3 align='center'>Examples of ",row$class," errors NOT labeled ",row$label,"</h3>")
            
  
        
        } else if(input$selection=="targCorr")
        {
           
 
            ##this calculates similarity to selected item based on KL-similarity to 
            ##response probability distribution.
            tableDat$rank <- klsim(sel,probs)
            
          

            ##rows that match target and are correct.
            match <- which(tableDat$class==target & tableDat$corr==TRUE)
            best <- match[order(-tableDat$rank[match])]  ##order of matching rows by rank klsim
            rows <- best[1:min(c(numExamples,length(best)))]
            #rows <- which(match & (order(tableDat$rank)<=numExamples))

        
            explore<- paste(explore,"<h3 align='center'>Most similar Correctly-identified cases</h3>")

            
        } else if(input$selection=="targInc")
        {
            ##this calculates similarity to selected item based on KL-similarity to 
            ##response probability distribution.
            tableDat$rank <- klsim(sel,probs)
            
            
            ##rows that match target and are incorrect.
            match <- which(tableDat$class==target &   tableDat$corr==FALSE)
            best <- match[order(-tableDat$rank[match])]  ##order of matching rows by rank klsim
            
            rows <- best[1:min(c(numExamples,length(best)))]
            

            explore<- paste(explore,"<h3 align='center'>Most similar Incorrectly identified cases</h3>")
            
          
     
        }else if(input$selection=="match")
        {

            ##this calculates similarity to selected item based on KL-similarity to 
            ##response probability distribution.
   
   
            tableDat$rank <- klsim(sel,probs)

            ##rows that match target and response .
            match <- which(tableDat$class==target &   tableDat$label == resp)
            best <- match[order(-tableDat$rank[match])]  ##order of matching rows by rank klsim
            rows <- best[1:min(c(numExamples,length(best)))]
            
            explore<- paste(explore,"<h3 align='center'>Most similar Target-Response matches</h3>")
            
        }
        
        ##This creates the html table we just selected in subset
        
        
        ##pick out the probabilities of Ground Truth and Class Label
        subset <- tableDat[rows,,drop=F]
        psubset <- probs[rows,,drop=F]
        

        if(nrow(subset)>0)
        {
       
          
         ##select the right columns by which to sort.
         subset$probsGT <- psubset[,tselect]
         subset$probsCL <- psubset[,rselect]
        
         explore <- paste(explore,
                         makeMatchTable(row$class,row$label,subset,numExamples=numMatches,type=titles[input$selection]))
        } else{
          explore <- paste(explore, "<h3 align=center>No cases match </h3>",sep="")
        
        }
        
        explore <- paste(explore,"<hr>")   

        
          explore
    })  ##end renderText
      
    })
      
      ##################################################3
      ## Category browser
      ##################################################3
      
      
      
      
output$header2 <-  renderText({
        out <- "<h2>Example cases of category/label</h2>"
        out
      })
      

output$browser2 <- renderText({
        numExamples <- 15
        info <- info()
        probs <- probs()$probs

        
        dat <- data()
        tableDat <- dat$dataTable ##reactive main data
     
        targetID <-as.numeric(input$groundB) 
        respID <-  as.numeric(input$respB)
          
        target <- dat$classes[targetID]
        resp <-   dat$labels[respID]
        
        
        #target <- (row$class)
        #resp   <- (row$label)
        #targetID <- row$classID
        #respID <- row$labelID
        ##update the category/contrast screen
        
        targs <- dat$classes
        resps <- dat$labels
        tselect <- which(target==targs)
        rselect <- which(resp==resps)
        
        
        
        

        explore <- paste("<h3>",target, " by ",resp,"</h3>",sep="")

       # site <-paste(session$clientData$url_protocol,"//",
      #               session$clientData$url_hostname,":",
      #               session$clientData$url_port,
      #               session$clientData$url_pathname,sep="")
        titles <- list("random"="Random cases","bestgt"="Most like Ground truth","worstgt"="Least like ground truth",
                       "best"="Most like classification","worst"="Least like classification")
        
 
        match <-tableDat$class==target & tableDat$label==resp  ##rows that match target/resp
        tableDat$probsGT <- probs[,targetID]
        tableDat$probsCL <- probs[,respID]
        matches <- tableDat[match,]
    
        type <- input$selectCatB
        if(type=="random")
        {
          rows <- samp(1:nrow(matches),min(c(numExamples,nrow(matches))))
          
        } else if(type=="bestgt")
        {
          rows <- rev(order(matches$probsGT))[1:min(c(numExamples,nrow(matches)))]
          
        } else if(type=="worstgt")
        {
          rows <- (order(matches$probsGT))[1:min(c(numExamples,nrow(matches)))]
          
        } else if(type=="best")
        {
          rows <- rev(order(matches$probsCL))[1:min(c(numExamples,nrow(matches)))]
        } else if(type=="worst")
        {
          rows <- (order(matches$probsCL))[1:min(c(numExamples,nrow(matches)))]
        }
       
        
        if(nrow(matches)==0)
        {
          explore <- paste(explore,"<h3>No pairings ",target,"->",resp," found</h3>",sep="")
          
        }else{
          
          ##Make first table:          
          explore <- paste(explore,
                           makeMatchTable(target,resp,matches[rows,],numExamples=numExamples,type=titles[type]))
        }

        explore
      })
    
##################################################3
## Contrast browser
##################################################3
      
      output$header3 <-  renderText({
          out <- "<h2>Contrast Exploration</h2>"
          out
      })
      output$browser3 <- renderText({
          numExamples <- 10
          titles <- list("random"="Random cases","best"="Best match to contrast","worst"="Worst match to contrast")
          probs <- probs()$probs
          
          
          dat <- data()
          tableDat <- dat$dataTable ##reactive main data
      
          
          targID1 <- as.numeric(input$ground1C)
          respID1 <- as.numeric(input$resp1C) 
          targID2 <- as.numeric(input$ground2C)
          respID2 <- as.numeric(input$resp2C)
          
 
          targ1 <- dat$classes[targID1]
          resp1 <-   dat$labels[respID1]
          targ2 <- dat$classes[targID2]
          resp2 <-   dat$labels[respID2]


          type<-  input$selectCatC 
          
         # site <-paste(session$clientData$url_protocol,"//",
        #               session$clientData$url_hostname,":",
        #               session$clientData$url_port,
        #               session$clientData$url_pathname,sep="")
          
          match1 <-tableDat$class==targ1 & tableDat$label==resp1  ##rows that match target/resp
          match2 <-tableDat$class==targ2 & tableDat$label==resp2  ##rows that match target/resp
          

          ##probability of target and response 1
          probs1 <- data.frame(probsGT =probs[,targID1],
                                 probsCL = probs[,respID1],
                                 probsGT2 = probs[,targID2],
                                 probsCL2 =  probs[,respID2])
          
          probs2 <- data.frame(probsGT = probs[,targID1],
                                 probsCL = probs[,respID1],  #
                                 probsGT2 = probs[,targID2],
                                 probsCL2 = probs[,respID2])  #
          subset1 <- cbind(tableDat,probs1)[match1,,drop=F]
          subset2 <- cbind(tableDat,probs2)[match2,,drop=F]
          ##Make first table:          

          if(type=="random")
          {
            rows1 <- samp(1:nrow(subset1), min(c(numExamples,nrow(subset1))))
            rows2 <- samp(1:nrow(subset2), min(c(numExamples,nrow(subset2))))
     

            
          } else if(type=="worst")
          {
            rows1 <- order(subset1$probsCL2)[1:min(c(numExamples,nrow(subset1)))]
            rows2 <- order(subset2$probsCL )[1:min(c(numExamples,nrow(subset2)))]
            
          } else if(type=="best")
          {
            ##pick the rows of 1 that are most like label2; and the rows of 2 that are
            ## most like label1. These should be the rows of subset/tabledat
            
            rows1 <- rev(order(subset1$probsCL2[match1]))[1:min(c(numExamples,nrow(subset1)))]
            rows2 <- rev(order(subset2$probsCL[match2]))[1:min(c(numExamples,nrow(subset2)))]
 
          } 

          if(length(rows1)==0)
          {
              explore <- paste("<h3>No pairings ",targ1,"->",resp1," found</h3>",sep="")
              
          }else{
            ##Make first table:          
               explore <- makeMatchTable(targ1,resp1,subset1[rows1,],numExamples=numExamples,type=titles[type])
          }
          
          
           explore <- paste(explore,"<br><br><br><p>")
          if(length(rows2)==0)
          {
            explore <- paste(explore,"<h3>No pairings ",targ2,"->",resp2," found</h3>",sep="")
            
          }else{
            ##Make second table:    
     
            explore <- paste(explore,makeMatchTable(targ2,resp2,subset2[rows2,],numExamples=10,type=titles[type]))
          }
          
     
          explore
          })
          
     
      
  
    ###########################
    ##  Filtering pane
    ###########################
     
      getColumns <- reactive({
        info <- info()
        columns <- c("case","class","label","corr",info[["otherFeatures"]])      
        columns
      }) 

  
 res_filter <- callModule(
      module = filterDF, 
      id = "filtering", 
      data_table = dataRaw,
      drop_ids =TRUE,
      data_vars = getColumns,
      picker=T
#      data_name = "dataTable"
    )

      
    observeEvent(res_filter$data_filtered(), {
      updateProgressBar(
        session = session, id = "pbar", 
        value = nrow(res_filter$data_filtered()), total = nrow(data()$dataTable)
      )
    })
    

  
    
    
    output$table <- DT::renderDT({
      res_filter$data_filtered()
    }, options = list(pageLength = 5))
    
    
    
  ############################################# 
  ## Main tab
   output$main_plot <- renderPlot(
       {   

          row <- getProbRow()
          row <- rev(sort(row[row>0]))
         if(length(row)>0)
           {par(mar=c(5,10,3,1))
            barplot(rev(row),xlim=c(0,1),col="gold",horiz=T,
                     xlab="Strength of label",
                   main="Probability of each case",las=1)
             grid()
         }
       })
    
    
   output$error_plot <- renderPlot(
       {   
         
     
           error <- confusion()
           info <- info()
           dat <- data()
           
           targID <-as.numeric(input$groundB) 
           respID <-  as.numeric(input$respB)
           
           targ <-  dat$classes[targID]
           resp <-  dat$labels[respID]
           
           
           targLevels <- dat$classes
           respLevels <- dat$labels
           
           
           
           par(mfrow=c(1,3),mar=c(6,6,6,0))

           barplot(error$classAccuracy$prob,names=error$classAccuracy$class,xlim=c(0,1),col="gold",horiz=T,
                   main="Accuracy of each class",las=1)
           grid()
           
        
           
           tab <- error$tab
           tabx <- tab/rowSums(tab)
     
           tmp <-tabx[targID,]
           tmp[targID] <- NA
          
            barplot(tmp,col="gold",horiz=T,xlim=c(0,1.1*max(c(.1,max(tmp,na.rm=T)))),
                   main=paste("Errors of ground truth",targ),las=1)
           grid()
           tmp <-tabx[,respID] 
           tmp[respID] <- NA
           
           barplot(tmp,col="gold",horiz=T, xlim=c(0,1.1*max(c(.1,max(tmp,na.rm=T)))),
                   main=paste("Errors to classification ",resp),las=1)
           grid()
   })
   
    




output$CA_plot <- renderPlot(
  {   
    dt <- data()$dataTable
    correspmodel <- ca::ca(table(dt$label,dt$class),nd=2)
    plot(correspmodel,main="Correspondence map of classes and labels",
         arrows=c(FALSE,FALSE),
         col=c("gold","black"),
         col.lab=c("grey20","black"),
         cex.lab=1.4)
    
  })


}
##end server


klsim <- function(case,distn,smooth=.001)
{
  d.out <- distn+smooth  # add a little smoothing 
  d.out <- d.out/rowSums(d.out)   # renormalize
  kl <-colSums(log(d.out[case,]/t(d.out))*d.out[case,])
  #  sim <- d.out %*% (d.out[1,]) 
  rank(kl,ties="first") 
}


## This generalizes the code for making a match-table.
##
makeMatchTable <- function(target,resp,matches,numExamples=10,type="random")
{

  ##this is a key that puts labels on different types.
  outtext <- ""
  ##select the subset based on the rows.
  subset <- matches
  
  outtext <- paste(outtext ,"<table border=1 align='left'><tr>")
  outtext<- paste(outtext,"<tr><th colspan=5><h3 align=left>Probe Case: ",target,"->",resp,
                  "  (",type,")</h3></th></tr><tr>",sep="")
 
  
  for(i in 1:nrow(subset))
  {
    if(!is.na(subset$fname[i]))
    {
      
      outtext <- paste(outtext, '<td style="text-align:center">',subset$class[i],'->',subset$label[i],
                       '<br><a href="#" onClick = "jkpopimage(\'',subset$fname[i], '\',400,400, \'',subset$case[i],'\');return false\"><img src = "',subset$fname[i],  '" width=100 ></a><br>',

                      '<button type = "button" id="d',subset$case[i],'" onclick=\"setCase(',subset$case[i] ,")\"> ",

                       subset$case[i],
                       '</button><br>',round(subset$probsGT[i],2) ,"->" ,
                       round(subset$probsCL[i],2),'</td>',sep="" )
   
      
    }
    if(i %% 5 == 0)
    {
      ##break table every 5
      outtext <- paste(outtext,"</tr>")
    }
  }
  
  outtext <- paste(outtext,"</table><br>")
  outtext
}

samp <- function(x,...){x[sample.int(length(x),...)]}
# Run the application 
shinyApp(ui = ui, server = server)
