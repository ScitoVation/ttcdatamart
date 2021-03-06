#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RSQLite)
library(DT)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(shinyjs)
library(DBI)
library(shinyalert)

getOriginalData <- function(df){
    originalData <- df
  originalData <- originalData %>%
    mutate(
      TTC_Value_raw = as.factor(TTC_Value_raw),
      #IRIS_NOAEL = suppressWarnings(as.numeric(IRIS_NOAEL)),# Warning: NAs introduced by coercion
      #ToxCast_OED_5th = suppressWarnings(as.numeric(ToxCast_OED_5th)),# Warning: NAs introduced by coercion
      #ToxCast_OED_Median = suppressWarnings(as.numeric(ToxCast_OED_Median)),# Warning: NAs introduced by coercion
      TTC_Classification_raw = formatC(as.numeric(TTC_Classification_raw), format = 'e', digits = 2),
      TTC_Classification_QSAR_Ready = formatC(as.numeric(TTC_Classification_QSAR_Ready), format = 'e', digits = 2),
      CASRN = as.factor(CASRN),
      #Small_HTTK = as.factor(Small_HTTK),
      #Large_CERAPP = as.factor(Large_CERAPP),
      #Kroes_Decision = as.factor(Kroes_Decision),
      #TTC_Class = ifelse(Kroes_Decision == 'Risk assessment requires compound-specific toxicity data','TTC not applicable',TTC_Class),
      #ToxTree_TTC = ifelse(Kroes_Decision == 'Risk assessment requires compound-specific toxicity data','TTC not applicable',ToxTree_TTC),
      #TTC_Class = as.factor(gsub('Genotoxic', 'Genotoxicity Alert',TTC_Class))
    )
  return(originalData)
}

renameColumns <- function(df){
  colnames(df) = c(
"CASRN",
"DSSTox Substance ID",
"Name",
"SMILES (raw)",
"SMILES (QSAR-ready)",
"Kroes Question 1 (Raw SMILES)",
"Kroes Decision (Raw SMILES)",
"Cramer Tree (Raw SMILES)",
"Cramer Decision (Raw SMILES)",
"Structural alert: genotoxic carcinogenicity (Raw SMILES)",
"Structural alert: S typhimurium mutagenicity (Raw SMILES)",
"Structural alert: AChE inhibition FG52_2 (Raw SMILES)",
"Structural alert: AChE inhibition FG81_2 (Raw SMILES)",
"TTC Value (mg/kg/d, Raw SMILES)",
"TTC Classification (Raw SMILES)",
"Kroes Question 1 (QSAR-ready SMILES)",
"Kroes Decision (QSAR-Ready SMILES)",
"Cramer Tree (QSAR-Ready SMILES)",
"Cramer Decision (QSAR-Ready SMILES)",
"Structural alert: genotoxic carcinogenicity (QSAR-Ready SMILES)",
"Structural alert: S typhimurium mutagenicity (QSAR-Ready SMILES)",
"Structural alert: AChE inhibition FG52_2 (QSAR-Ready SMILES)",
"Structural alert: AChE inhibition FG81_2 (QSAR-Ready SMILES)",
"TTC Classification (QSAR-Ready SMILES)",
"TTC Value (mg/kg/d, QSAR-Ready SMILES)"
  )
  return(df)
}

shinyServer(function(input, output,session) {
  # initialize all ui element options
  conn <- dbConnect(SQLite(),"cerapp_ttc_june_2020.sqlite")
  withProgress(
    message = 'Retrieving data',
    value = 0,
    expr = {
       tryCatch({
        trulyOriginalData <- dbGetQuery(conn,"SELECT * FROM ttc")
        dbDisconnect(conn)
        originalData <- getOriginalData(trulyOriginalData)

        write_data <- data.frame(originalData)

        ttcData <- levels(originalData$TTC_Value_raw)

        kroesData <- levels(originalData$Kroes_Decision)

        originalData <- renameColumns(originalData)
        setProgress(1)
       },
       error = function(e) {
         shinyalert("Failed to retrieve the data.", conditionMessage(e), type = "error", closeOnClickOutside = TRUE)
       })
    })

  output$test_data <- DT::renderDataTable({
    DT::datatable(
      data = originalData,
      filter='top',
      extensions = c('FixedColumns','FixedHeader'),
      rownames = F,
      options = list(
        stateSave = TRUE,
        fixedHeader = T,
        deferRender = T,
        autoWidth = T,
        scrollX = '100%',
        scrollY = 535,
        scrollCollapse = T, # When scrollY is defined, bottom of table won't "float" below the table
        pageLength = 15, # How many rows to display by default
        dom='rfltip',
        #ajax = list(url = dataTableAjax(session, originalData, rownames = F, outputId = 'test_data')),
        columnDefs = list(
          list(
            # elipses points for long fields
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 45 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 45) + '...</span>' : data;",
              "}"
            ),
            targets = c(2,3,4,5)
          ),
          list(
            width = '100px',
            targets = c(0)
          ),
          list(
            render = JS(
              "function(data, type, row, meta) {",
              "return '<a href=\"https://comptox.epa.gov/dashboard/' + data + ' \" target=\"_PARENT\">' + data + '</a>';",
              #"return '<a href=\"https://comptox.epa.gov/dashboard/dsstoxdb/results?search=' + data + '\">' + data + '</a>';",
              "}"),
            targets = c(1)
          )
        )
      )
    )

    # conn2 <- do.call(dbConnect,list(SQLite(),"TestingCerapp.sqlite"))
    # on.exit(DBI::dbDisconnect(conn2))

  },server = T)

  proxy = dataTableProxy('test_data')

  filteredCols <- reactive({
    tmpdata <- unlist(lapply(input[["test_data_state"]]$columns,function(x){
      print(x$'visible')}))
    #vector <- c(1:length(tempdata))
    return(tmpdata)
  })

  output$download_table <- downloadHandler(

    filename = "TTCdata.csv",
    function(file){
      write.table(originalData[input[["test_data_rows_all"]],filteredCols()],file = file, sep = ",",row.names = F)
    }
  )

  # Updates visible columns
  observeEvent(
    input$col_types,
    {
      colTypes <- input$col_types
      colToHide <- setdiff(colnames(originalData),colTypes)
      col2Hide <- match(colToHide,colnames(originalData)) # list of column indexes to hide
      proxy %>%
        hideCols(col2Hide-1, reset = T)
    },
    ignoreNULL = F
  )

  # Updates filtered rows based on Dataset's input
  observeEvent(
    input$dataset,
    {
      datasetF <- input$dataset
      if('HTTK' %in% datasetF){ # Filters rows based on user's input
        HTTKVal <- '["YES"]'
      } else
        HTTKVal <- ''
      if('CERAPP' %in% datasetF){ # Filters rows based on user's input
        CERAPPVal <- '["YES"]'
      } else
        CERAPPVal <- ''

      colSearch <- input$test_data_search_columns
      colSearch[11] = HTTKVal
      colSearch[12] = CERAPPVal
      proxy %>%
        updateSearch(
          keywords = list(
            columns = colSearch
          )
        )
    },
    ignoreNULL = F
  )

  # Updates filtered rows based on TTC Class' input
  observeEvent(
    input$ttc_class,
    {
      ttcClass <- input$ttc_class
      colSearch <- input$test_data_search_columns
      if(is.null(ttcClass))
        ttcClass = ''
      ttcClass2 <- lapply(ttcClass, function(each){paste0('"',each,'"')})
      ttcClass3 <- paste(unlist(ttcClass2), collapse = ',')
      ttcClass4 <- paste('[',ttcClass3,']')
      colSearch[15] = ttcClass4
      proxy %>%
        updateSearch(
          keywords = list(
            columns = colSearch
          )
        )
    },
    ignoreNULL = F
  )

  ###############
  # UPLOAD CHEMICAL LIST
  ###############
  observeEvent(input$upload_chems,{
    path <- input$upload_chems$datapath
    if (is.null(path)){
      chem_list <- NULL
      output$chem_status <- renderText({"No file uploaded"
      })
    }else{
      chem_list <- read.csv(path,header = T,stringsAsFactors = F)[[1]]
      output$chem_status <- renderPrint({
        paste0("Uploaded list has ",
               as.character(length(chem_list)),
               " chemicals.")
        })
      search_string <-paste('[', paste(unlist(
        lapply(chem_list,function(x){paste0('"',x,'"')})
      ),collapse = ','),']')
      colSearch <- input$test_data_search_columns
      colSearch[1]= search_string
      proxy %>%
        updateSearch(
          keywords = list(
            columns = colSearch
          )
        )
    }
  })


  observeEvent(input$navbar,{
    if (input$navbar == "Quit"){
      stopApp()
    }
  })

  updatePickerInput(
    session,
    "ttc_class",
    choices = ttcData,
    selected = ttcData[c(1,2,3,4,5,6)]
  )
  # updateCheckboxGroupInput(
  #   session,
  #   "Kroes",
  #   choices = kroesData,
  #   selected = kroesData[c(1,2,3)]
  # )
  updatePickerInput(
    session,
    "col_types",
    choices = colnames(originalData),
    selected = colnames(originalData)[c(1:4,9,14,15)]
  )

  delay(
    3000,{
      colSearch <- input$test_data_search_columns
      proxy %>%
        updateSearch(
          keywords = list(
            columns = c("","","","","","","","","","","[\"YES\"]","")
          )
        )
      proxy %>%
        updateSearch(
          keywords = list(
            columns = colSearch
          )
        )
    }
  )
})
