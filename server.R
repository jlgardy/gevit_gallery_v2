library(shiny)
library(readxl)
library(tesseract)
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)


source("extras/utilityFunctions.R")

#------------------
# PREP

#Prep Images
imgs<-list.files(path="www/images/")

#Prep Data
dat<-read_excel(path="data/figure_classification_final.xlsx",na=c("","NA"))
datSub<-dat %>%
  filter(figID_initial %in% imgs)%>%
  filter(!(specialChartType %in% c("INCOMPLETE","MISSING"))) %>%
  filter(!grepl("CONSIDER REMOVING",specialChartType))

chartRln<-getChartRln(datSub)
paperData<-read_excel("data/MasterDocumentList.xlsx") %>% filter(Include == "Y")

datSub<-inner_join(datSub,paperData,by="PMID")

load("data/captionItems.RData")
commonWords<- inner_join(commonWords,select(datSub,figID_initial), by="figID_initial")

#function to filter data are return ID
filtData<-function(dat=NULL,colFilt=NULL,filtOpts=NULL){
  if(!is.null(filtOpts)){
    if(colFilt %in% c("addedMarks","reencodedMarks")){
      if(filtOpts){
        return(dat[!is.na(dat[,colFilt]),"figID_initial"])
      }else{
        return(dat[,"figID_initial"])
      }
      
    }else{
    tmp<-sapply(filtOpts,function(x){
      sapply(dat[,colFilt],function(y){grepl(x,y)})
    })
    foundItems<-rowSums(tmp)
    
    return(unique(dat[foundItems>0,"figID_initial"]))
    }
  }else{
    return(unique(dat[,"figID_initial"]))
  }
}

#------------------
# SERVER CODE
shinyServer(function(input, output,session) {
 
  #reactive dataset
  datasetInput <- reactive({
    
    #browser()
    #filter initially on figure caption content (if any is supplied)
    tmpDat<-datSub
    if(!is.null(input$captionSelect)){
      idKeep<-filtData(commonWords,"bigram",input$captionSelect)
      tmpDat<-inner_join(datSub,idKeep)
    }
    
    if(nrow(tmpDat)==0){
      empty_figs <- "There are no visualizations that match your filtering criteria."
      return(matrix(empty_figs, nrow = 1, ncol = 1)) # return empty matrix to avoid console warning
    }
    
    #filter dataset - images MUST have these items to be included
    filtID<-c(filtData(tmpDat,"chartCombinations",input$chartCombo)$figID_initial,
              filtData(tmpDat,"chartType",input$chartType)$figID_initial,
              filtData(tmpDat,"Pathogen",input$pathogenSelect)$figID_initial,
              filtData(tmpDat,"concepts",input$conceptSelect)$figID_initial,
              filtData(tmpDat,"addedMarks",input$addMarksSelect)$figID_initial,
              filtData(tmpDat,"reencodedMarks",input$rencodeMarksSelect)$figID_initial)
    
    tmp<-table(filtID)
    tmp<-tmp[tmp==6]
    
    if(length(tmp)==0){
      empty_figs <- "There are no visualizations that match your filtering criteria."
      return(matrix(empty_figs, nrow = 1, ncol = 1)) # return empty matrix to avoid console warning
    }
    
    tmp<-datSub %>% filter(figID_initial %in% names(tmp))
    
    #Banner above image to indicate if it is good practice or not
    #currently just a random indicator
    supportingText<-sapply(runif(nrow(tmp)),function(x){
      if(x>0.8){
        return('<p class="special-content">Good Practice</p>')
      }else{
        return('<p class="regular-content">Mistakes were made</p>')
      }
    })
    
    imgTxt<-paste0(supportingText,'<img class="chart" src="imagesSmaller/',tmp$figID_initial, '"data-code="', tmp$figID_initial,'"data-zoom-image="', tmp$figID_initial, ',"></img>')
    imgTxtPadded<-pad.Vector(imgTxt)
    
    #adding some labels
    if(length(imgTxtPadded)/3 > 0) {
      matrix(imgTxtPadded, ncol = 3, nrow = (length(imgTxtPadded)/3), byrow = TRUE)
    } else {
      empty_figs <- "There are no graphs that match the selected criteria."
      matrix(empty_figs, nrow = 1, ncol = 1) # return empty matrix to avoid console warning
    }
    
  })
  
  #Outputting Gallery of Images
  output$imageGrid <- renderTable({
    datasetInput()
  }, sanitize.text.function = function(x) x,
  include.colnames = FALSE, 
  include.rownames = FALSE,
  bordered=TRUE)
  
  #reactive values
  values <- reactiveValues(
    clicked = FALSE,
    code = NULL
  )
  
  
  #-----------------------------------------------
  # Reactive Functions
  #-----------------------------------------------
  output$figImage_only <- renderImage({

  filename <- normalizePath(file.path('www/images/',paste0(values$code)))

    list(src = filename,
         width = "auto",
         height = 500)
  }, deleteFile = FALSE)

  #-----------------------------------------------
  # Reactive Functions
  #-----------------------------------------------
  
  # When the app loads, if there is a hash value then load that figure
  observe({
    urlHash <- session$clientData$url_hash_initial
    if (!is.null(urlHash) && substring(urlHash, 1, 1) == "#") {
      values$code <- substring(urlHash, 2)
    }
  })
  
  # When a figure is clicked, load that figure
  observe({
    if (!is.null(input$clicked) && input$clicked == TRUE) {
  
      values$code <- input$code
     
      # Add figure name to URL so it can be retrieved later
      session$sendCustomMessage("figClick", values$code)
      updateTabsetPanel(session, "opsPanel", selected = "Figure")
      #updateMaterialSwitch(session,"enableZoom",FALSE)
    }
  })
  
  observe({
    # Automatically toggle image zooming on and off
    if(input$enableZoom){
      session$sendCustomMessage(type = 'createZoom',message=list())
    }else{
      session$sendCustomMessage(type = 'removeZoom',message=list())
    }

  })
  
  #-----------------------------------------------
  # WIDGETS
  #-----------------------------------------------
  
  # Widget : Dropdown menu for Pathogen
  output$pathogenUI<-renderUI({
    pathogen<-datSub %>%
      ungroup()%>%
      mutate(Pathogen=strsplit(Pathogen,";"))%>%
      tidyr::unnest()%>%
      group_by(Pathogen)%>%
      count()
    
    selectInput(inputId = "pathogenSelect",
                label = "Pathogen:",
                choices=pathogen$Pathogen,
                selected=NULL,
                multiple=TRUE)
  })
  
  # Widget : Dropdown menu for Concepts
  output$conceptUI<-renderUI({
    concept<-datSub %>%
      ungroup()%>%
      mutate(concepts=strsplit(concepts,";"))%>%
      tidyr::unnest()%>%
      group_by(concepts)%>%
      count()
    
    selectInput(inputId = "conceptSelect",
                label = "Topic:",
                choices=concept$concepts,
                selected=NULL,
                multiple=TRUE)
  })
  
  
  # Widget: Dropdown menu to choose chart type
  output$chartTypeUI<-renderUI({
    chartTypes<-datSub %>%
      ungroup()%>%
      mutate(chartType=strsplit(chartType,";"))%>%
      tidyr::unnest()%>%
      group_by(chartType)%>%
      count()
    
    selectInput(inputId = "chartType",
                label = "Chart Type",
                choices=chartTypes$chartType,
                selected=NULL,
                multiple=TRUE)
  })
  
  # Widget: Dropdown menu for special chart types
  output$specialChartTypeUI<-renderUI({
    if(is.null(input$chartType)){
      choiceVals<-as.character(chartRln$specialChartType)
    }else{
      tmp<-chartRln %>%
        filter(chartType %in% input$chartType)
      
      choiceVals<-as.character(tmp$specialChartType)
    }
    
    selectInput(inputId = "specialChartType",
                label = "Special Chart Type",
                choices=choiceVals,
                selected=NULL,
                multiple =TRUE)
  })
  
  #Widget : dropdown menu for data caption
  output$captionLookUp<-renderUI({
    selectInput("captionSelect",
                label="Data (from figure captions):",
                choices = c(usefulBigrams$bigram,usefulSingles$word),
                selected=NULL,
                multiple=TRUE)
  })
  
  #-----------------------------------------------
  # SUMMARIES
  #-----------------------------------------------
  #output figure metadata
  output$imageInfo<-renderUI({
    outText<-NULL
    if(length(values$code) > 0) {
      tmp<-filter(datSub,figID_initial == values$code)
      outText<-HTML(sprintf("<b>ID:</b>%s<br><em>%s</em>(%d) <a href='https://www.ncbi.nlm.nih.gov/pubmed/%s' target='_blank'>doi</a>",tmp$figID_initial,tmp$Title,tmp$YearPub,tmp$PMID))
    }
    outText
  })
  
  
  #output table for figure metadata
  
  output$summaryImageTable<-renderTable({
    if(length(values$code) == 0) {
      NULL
    } else {
      tmp<-filter(datSub,figID_initial == values$code)
  
      #chart types
      chartType<-unlist(strsplit(tmp$chartType,";"))
      specialChartType<-unlist(strsplit(tmp$specialChartType,";"))
      
      if(length(chartType) == length(specialChartType)){
        chartTypes<-data.frame(item = rep("Chart Type",length(chartType)),
                               chartType = chartType,
                               specialChartType = specialChartType,
                               stringsAsFactors = FALSE)
      }else{
        chartTypes<-cbind(tmp$chartType,tmp$specialChartType)
      }
      
      
      #added marks
      if(!is.na(tmp$addedMarks)){
        addedMarks<-unname(parseMarkText(tmp$addedMarks))
        addedMarks<-data.frame(item=rep("Added Marks",nrow(addedMarks)),
                               chartType = addedMarks[,1],
                               specialChartType = addedMarks[,2],
                               stringsAsFactors = FALSE)
        #colnames(addedMarks)<-c("item","chartType","specialChartType")
      }else{
        addedMarks<-data.frame(item="Added Marks",chartType=NA,specialChartType=NA)
      }
      
      #rencoded marks
      if(!is.na(tmp$reencodedMarks)){
        rencodedMarks<-unname(parseMarkText(tmp$reencodedMarks))
        rencodedMarks<-data.frame(item=rep("Re-encoded Marks",nrow(rencodedMarks)),
                                  chartType = rencodedMarks[,1],
                                  specialChartType = rencodedMarks[,2],
                                  stringsAsFactors = FALSE)
        #colnames(addedMarks)<-c("item","chartType","specialChartType")
      }else{
        rencodedMarks<-data.frame(item="Re-encoded Marks",chartType=NA,specialChartType=NA)
      }
      
      #annotations
      #rencoded marks
      if(!is.na(tmp$annotations)){
        annotationsMarks<-unname(parseMarkText(tmp$annotations))
        annotationsMarks<-data.frame(item=rep("Annotations",nrow(annotationsMarks)),
                                     chartType = annotationsMarks[,1],
                                     specialChartType = annotationsMarks[,2],
                                     stringsAsFactors = FALSE)
        #colnames(addedMarks)<-c("item","chartType","specialChartType")
      }else{
        annotationsMarks<-data.frame(item="Annotations",chartType=NA,specialChartType=NA)
      }
      
      #putting it together
      outfig<-rbind(chartTypes,
                    c("Chart Combinations",tmp$chartCombinations,NA),
                    addedMarks,
                    rencodedMarks,
                    annotationsMarks)
      
      outfig %>% filter(!is.na(chartType))
    }
    
  },
  colnames=FALSE,
  rownames=FALSE)

})
