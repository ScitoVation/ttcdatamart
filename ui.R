library(shiny)
library(RSQLite)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
#library(rintrojs)

shinyUI(
  tagList(
    tags$head(
      tags$link(
        rel='icon',
        href='cropped-ScitoVation_icon-32x32.png',#href="https://www.scitovation.com/wp-content/uploads/2019/02/cropped-ScitoVation_icon-32x32.png",
        sizes="32x32"
      )
    ),
    useShinyjs(),
    tags$head(tags$style(".shiny-notification {position: fixed; top: 30% ;left: 35%")),
    tags$head(tags$style("#navbar {height:75px;}")),
    navbarPage(
      title = tags$img(
        height = 50,
        src = 'sciv_logo_transparent.png'#src = "https://www.scitovation.com/wp-content/themes/scitovation/assets/images/logo/ScitoVation-Pioneers_in_Chemical_Safety_Assessment.svg"
      ),
      id = 'navbar',
      #position = 'fixed-top',
      windowTitle = 'TTC Data Mart',
      fluid = T,
      collapsible = T,
      # selected = 'About',
      tabPanel(
        title = 'Datatable',
        icon = icon('table'),
        fluidPage(
          #fluidRow(actionButton("help","Help",icon = icon("question-circle"))),
          fluidRow(
            column(
              3,
              #introBox(
              pickerInput(
                'ttc_class',
                'Select TTC Class',
                choices = NULL,
                multiple = TRUE,
                options = list(
                  'live-search' = TRUE,
                  'actions-box' = TRUE,
                  'selected-text-format' = 'count > 2',
                  'count-selected-text'='{0} classes selected',
                  style = 'btn-primary'
                )
              )
              #,data.step = 1#,
              #data.intro = "TTC classes",
              #data.hint = "Cramer Rules original and Cramer Rules with Extensions methods were used to assign three classes - Low - Class I, Intermediate - Class II, and High - Class III. The fourth group was Anti-cholinesterase. Here, the structural alerts for functional group identification method was employed to identify compounds that were carbamate esters or organophosphate esters based on a YES for FG52_2 or FG81_2, respectively, for each chemical in the Toxtree output file. A fifth class, Genotoxic Chemicals, was also determined. Here, a Benigni/Bossa Rulebase for Mutagenicity and Carcinogenicity method was first applied to flag compounds that might be genotoxic based on whether the result for Structural Alert for genotoxic carcinogenicity was YES, and, second, an in vitro mutagenicity -Ames test alerts by ISS was applied to flag compounds that have the potential for in vitro genotoxicity based on whether the result for Structural Alert for S. typhimurium mutagenicity was YES."
              #)
            ),
            # column(
            #   4,
            #   checkboxGroupInput(
            #     'Kroes',
            #     'Select Kroes Decision',
            #     choices = NULL
            #     ,width = '100%'
            #   )
            # ),
            column(
              2,
              checkboxGroupInput(
                'dataset',
                'Filter Dataset',
                choices = c('HTTK', 'CERAPP')
                ,width = '100%'
              )
            ),
            column(
              3,
              fluidRow(
                fileInput("upload_chems",
                          label = "Upload list of chemicals to filter",
                          multiple = F,placeholder = "List of CAS numbers")
              ),
              fluidRow(
                textOutput("chem_status",inline = T)
              )
              
              
            )
          ),
          fluidRow(
            tags$style('.datatables .display {margin-left: 0;}'), #Sets datatables' columns to 'float left'
            # style = 'float:left', # uncomment to make search bar move with table's size
            
            #hides 2nd row of table's header which should be 'filter' if enabled.
            tags$style('.datatables tr:nth-child(2) {display:none;}'),
            column(
              width = 12,
              pickerInput(
                'col_types',
                'Select Columns',
                choices = NULL,
                multiple = TRUE,
                options = list(
                  'live-search' = TRUE,
                  'actions-box' = TRUE,
                  'selected-text-format' = 'count > 3',
                  'count-selected-text'='{0} of {1} columns selected',
                  size = 10 #how many options to display in dropdown
                )
              ),
              DT::dataTableOutput('test_data')#,width = '100%')
            )
          ),
          fluidRow(
            column(
              width = 12,
              downloadButton('download_table',label = "Download Table")
            )
          )
        )
      ),
      tabPanel(
        title = 'About',
        icon = icon('info-circle'),
        # tags$style(
        #   type='text/css',
        #   'body {padding-top: 65px;}'
        # ),
        fluidPage(
          fluidRow(
            column(
              12,
              align = 'Center',
              # Application title
              h2('Thresholds of Toxicological Concern for a Broad Chemical Universe')
            )
          ),
          fluidRow(
            h4(
              p(
                HTML(
                  paste0(
                    'We have calculated Thresholds of Toxicological Concern (TTC) for a 
                  45,000-chemical universe defined previously ',
                    a(
                      href = 'https://ehp.niehs.nih.gov/doi/full/10.1289/ehp.1510267?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%3dpubmed',
                      'Mansouri et al 2016',
                      target = '_blank'
                    ),
                    '. We used Toxtree v2.6.13 to determine the TTC class and the Kroes Decision 
                  for each compound. Sort and filter the table to interact with the data, 
                  or search for a compound of interest by entering its name into the search 
                  bar. This work was funded by the American Chemistry Council - Long-range 
                  Research Initiative (ACC-LRI).'
                  )
                )
              )
            )
          )
        )
      )#,
      #tabPanel(title = "Quit",icon = icon("power-off"))#,
      # rintrojs::introjsUI()
    )
  )
)


