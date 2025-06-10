library(shiny)
library(shinyjs)
library(ggplot2)  # for the diamonds dataset
library(openxlsx)
library(dplyr)


shinydebugList1 = list()
shinydebugList2 = list()


ui <- fluidPage(
  useShinyjs(),
  br(),
  titlePanel("Bioassay Spectrum Testing Worklist Generator"),
    sidebarPanel(
      tabsetPanel(
        id = 'experiment',
        
        tabPanel(
          id = 'protein_names',
          "Protein Info", 
          br(),
          "Enter Protein Names for EntLIMS", 
          br(),
          textInput( 
            "protein1", 
            "Protein 1 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          textInput( 
            "protein2", 
            "Protein 2 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          textInput( 
            "protein3", 
            "Protein 3 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          textInput( 
            "protein4", 
            "Protein 4 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          textInput( 
            "protein5", 
            "Protein 5 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          textInput( 
            "protein6", 
            "Protein 6 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          textInput( 
            "protein7", 
            "Protein 7 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          textInput( 
            "protein8", 
            "Protein 8 Name", 
            placeholder = "Enter text (optional)..."
          ), 
          
        ),
        
        tabPanel("Experimental Info", 
                 br(),
                 "Enter Experimental Details",  
                 br(),
                 numericInput("proteins", "Number of proteins to assay:", 8, min = 1, max = 8),
                 numericInput("standardPlates", "Number of standard plates to create:", 10, min = 0, max = 20),
                 numericInput("hemipPlates", "Number of hemipteran plates to create:", 2, min = 0, max = 2),
                 numericInput("lygusPlates", "Number of lygus plates to create:", 2, min = 0, max = 2),
                 br()
                 ),
        
        
      ),
      actionButton("update", "Update View"),
    ),
  
  sidebarPanel(
    textInput( 
      "filename", 
      "Enter Experiment Name", 
      placeholder = "Enter text..."
    )
  ),
  
  sidebarPanel(
    downloadButton("downloadWorklist", "Download All Hamilton Worklists"),
    br(),
    downloadButton("downloadUploadSheet", "Download All ENT Upload Sheets"),
    br()
  ),
    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Nice Insects", DT::dataTableOutput("mytable1")),
        tabPanel("Hemipterans", DT::dataTableOutput("mytable2")),
        tabPanel("Lygus", DT::dataTableOutput("mytable3"))
      )
    )
)

server <- function(input, output, session) {
  mytables <- list()
  shinyjs::hide("downloadWorklist")
  shinyjs::hide("downloadUploadSheet")
  shinyjs::hide("filename")
  
  sort_worklist <- function(dF)
  {
    if(!is.null(dF[[1]]))
    {
      dF <- dF %>%
        arrange(!!! rlang::syms(c("Group Id", "Conc", "Destination Plate", "SRC_ID")))
      return(dF)
    }
  }
  #arrange(!!! rlang::syms(c("Plate Number")), .locale = "en") %>%
  sort_uploadSheet <- function(dF)
  {
    
    if(!is.null(dF[[1]]))
    {
      #
      #print("sorting...")
      dF <- dF  %>% 
        arrange(!!sym('Plate Number'), substr(!!sym('Well'), 1,1),as.integer(substr(!!sym('Well'), 2, length(!!sym('Well')))),  .locale = "en") 
      return(dF)
    }
  }
  
 # mytables <<- vector('list', length = 3)
  uploadSheets <<- vector('list', length = 3)

  
  defaultWD <- getwd()
  
  observeEvent(
    eventExpr = input$update, 
    {
      print('processing worklist...')
      setwd(defaultWD)
      source("www/WorklistGenerator.R")
      
      
      proteinNames = list(input$protein1,input$protein2,input$protein3,
                          input$protein4,input$protein5,input$protein6,
                          input$protein7,input$protein8)
      
      
      mytables <<- create_worklists(input$proteins, input$standardPlates, input$hemipPlates, input$lygusPlates, proteinNames)

      sorted_worklists <<- lapply(mytables, sort_worklist)

      output$mytable1 <- DT::renderDataTable({
        DT::datatable(sorted_worklists[[1]], options = list(paging = FALSE))
          }, server = FALSE)
    
      output$mytable2 <- DT::renderDataTable({
        DT::datatable(sorted_worklists[[2]], options = list(paging = FALSE))
      }, server = FALSE)
      
      output$mytable3 <- DT::renderDataTable({
        DT::datatable(sorted_worklists[[3]], options = list(paging = FALSE))
      }, server = FALSE)
      
      
      print('processing LIMS Upload Sheet...')
      setwd(defaultWD)
      source("www/EntLIMSUploadSheetGenerator.R")

      uploadSheets <- create_upload_sheet(mytables, proteinNames, input$standardPlates, input$hemipPlates, input$lygusPlates)
      assign("shinydebugList3", uploadSheets, envir = .GlobalEnv)
      
      sortedUploadSheets <<- lapply(uploadSheets, sort_uploadSheet)
      shinyjs::show("filename")
    }
  )
  
  observeEvent(
    eventExpr = input$filename,
    {
      if(input$filename != "" )
        {
          shinyjs::show("downloadWorklist")
          shinyjs::show("downloadUploadSheet")
        }
    }
  )
  
  output$downloadWorklist <- downloadHandler(
    filename = function() {
      paste(input$filename, "_Ham_worklist.zip", sep="")
    },
    content = function(file){
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c(paste(input$filename, "_normalInsects.xlsx", sep = ""),
              paste(input$filename, "_hemipInsects.xlsx", sep = ""),
              paste(input$filename, "_lygusInsects.xlsx", sep = ""))

      normalSheet <- list("Hamilton Worklist" = sorted_worklists[[1]])
      hemipSheet <- list("Hamilton Worklist" = sorted_worklists[[2]])
      lygusSheet <- list("Hamilton Worklist" = sorted_worklists[[3]])

      write.xlsx(normalSheet, file = fs[[1]])
      write.xlsx(hemipSheet, file = fs[[2]])
      write.xlsx(lygusSheet, file = fs[[3]])
      
      zip::zip(file, fs)
    },
    contentType = "application/zip")
  
  
  output$downloadUploadSheet <- downloadHandler(
    filename = function() {
      paste(input$filename, "_ENT_LIMS", ".zip", sep="")
    },
    content = function(file){
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c(paste(input$filename, "_normalInsects_LIMSUPLOAD.xlsx", sep = ""),
              paste(input$filename, "_hemipInsects_LIMSUPLOAD.xlsx", sep = ""), 
              paste(input$filename, "_lygusInsects_LIMSUPLOAD.xlsx", sep = ""))

      normalSheet <- list("ENT_LIMS UPLOAD" = sortedUploadSheets[[1]])
      hemipSheet <- list("ENT_LIMS UPLOAD" = sortedUploadSheets[[2]])
      lygusSheet <- list("ENT_LIMS UPLOAD" = sortedUploadSheets[[3]])
      
      write.xlsx(normalSheet, file = fs[[1]])
      write.xlsx(hemipSheet, file = fs[[2]])
      write.xlsx(lygusSheet, file = fs[[3]])
      
      zip::zip(file, fs)
    },
    contentType = "application/zip")
}

shinyApp(ui, server)
