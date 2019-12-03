# Version 0.6

ui <- dashboardPagePlus(skin ="red",
           dashboardHeaderPlus(titleWidth=300,
                               title= tagList(
                                 span(class = "logo-lg", "PGx sample managment"), 
                                 img(src = "logo.svg")),
                         dropdownMenuOutput("DROPDOWN2"),
                         dropdownMenuOutput("DROPDOWN1"),
                         userOutput("SUPER_USER")),
         dashboardSidebar(
              sidebarMenu(
                menuItem("Register", tabName = "register", icon = icon("dashboard")),
                menuItem("LIMS Interface", tabName = "LIMS_interface", icon = icon("cloud-download-alt")),
                menuItem("Centraxx Interface", tabName = "Centraxx", icon = icon("atlas")),
                menuItem("Pre DNA", tabName = "pre_DNA", icon = icon("vial")),
                menuItem("Post DNA", tabName = "post_DNA", icon = icon("dna")),
                menuItem("Storage", tabName = "storage", icon = icon("layer-group"),
                         menuSubItem("Concentration check", tabName = "concentration_check",
                                     icon = icon("check-circle")),
                         menuSubItem("Storage position", tabName = "storage_position",
                                     icon = icon("box"))
                         ),
                menuItem("Label", tabName = "label", icon = icon("tags")),
                menuItem("Genotyping", tabName = "genotyping", icon = icon("microscope")),
                menuItemOutput("TAMENDOX_results"),
                menuItem("Database", tabName = "database", icon = icon("server")),
                menuItem("Audit", tabName = "audit", icon = icon("circle"))
                ) # end Menu
            ), # end Sidebar
          
          dashboardBody(
            useShinyjs(debug = TRUE),
            tags$script(HTML("function clickFunction(link){
                      var rndm = Math.random();
                       Shiny.onInputChange('linkClicked', {data:link, nonce: Math.random()});}"
            )),
            
            
                        tabItems(

# tab register ------------------------------------------------------------
                  tabItem(tabName = "register",
                    fluidRow( box(title = "LIMS Export files", status = "warning",
                      selectInput("study", "Study", choices = STUDIES),
                      textInput("patid", "PatID (e.g. IKP275: Studienzentrum-PatID)", value = ""),
                      textInput("ikp_id", "IKP internal ID (e.g. IKP428-0001)", value = ""),
                      dateInput("entry_date", "Sample_entry",format = "yyyy-mm-dd",weekstart = 1),
                      selectInput("entry_condition","Condition (IKP428 = Aufgetaut)", choices = CONDITION),          
                      selectInput("size", "Monovette size [ml]", choices = c("2.7", "4.9", "9"), selected = "4.9"),
                      selectInput("vial", "Vial", choices = c("A1", "A2")),
                      uiOutput("STORAGE"),
                      uiOutput("BOX"),
                      uiOutput("POSITION"),
                      uiOutput("next_sample")
                      ), 
                      box(status = "warning", plotOutput("entry_storage_plot"))),
                    fluidRow(
                     box(title ="Registered samples",status= "warning",
                         width = 12,
                         DT::DTOutput("dt_register")  
                      ))
                  ), 

# LIMS --------------------------------------------------------------------
                   tabItem(tabName = "LIMS_interface",
                           box(status="primary", fileInput("LIMS_file", "Upload LIMS Fileexport",
                                           multiple = FALSE,
                                           accept = "xlsx/xlsx"),
                               h6(help_text_lims)),
                  box(actionButton("save_lims","Save values"),solidHeader = T, status = "success",background = "green"),
                           box(status="primary",tableOutput("contents"),width = 12)
                  ),

# Centraxx ----------------------------------------------------------------
                
## add Centraxx Upload File

# pre_DNA -----------------------------------------------------------------
                  tabItem(tabName = "pre_DNA",
                       fluidRow(box(status="warning", uiOutput("DNA_ISOLATION")),
                                box(textOutput("pre_DNA_info"))),
                       fluidRow(box(title = "Up to 48 EDTA samples for DNA Isolation",
                                    p("Copy this table in DNA isolation EXCEL File at", strong("Y:/KG/PGX/1_Arbeitsunterlagen")),
									p("Excel and pdfs are saved in the default 'Download' directory"),
                                    status="warning", DTOutput("dt_preDNA"), width = 12))
                  ), 

# Label -------------------------------------------------------------------
                tabItem(tabName = "label",
                        box(solidHeader = T, status = "danger", 
                            actionBttn("save_label", "Save to LIMS_PTOUCH_DB.xlsx",
                                       style = "simple", color = "danger"),
                                       h5("Saved at: Y:/KG/PGX/2_DNA_Isolierung/1_Etiketten")), 
                        box(DTOutput("dt_label"), width = 12)
                        ),


# post DNA ----------------------------------------------------------------
                  tabItem(tabName = "post_DNA",
                          fluidRow(box(status="primary", 
                                       fileInput("inFile", "Upload DNA Isolation file",
                                                          multiple = FALSE,
                                                          accept = "xlsx/xlsx")),
                                   box(solidHeader = T, status = "success", background = "green",
                                       actionButton("save_dna","Save values"),
                                       actionButton("cancel_file","cancel"))),
                          fluidRow(box(selectInput("isolation_method", "Method", choices = DNA_METHOD),
                                       selectInput("isolation_conc_method", "DNA concentration method", choices =CONC_METHOD),
                                       textAreaInput("isolation_comment", "Comment", value = "", resize= "vertical"))),
                         fluidRow(box(dataTableOutput("table_uploaded_dna_samples"), width = 12)),
                         box(tableOutput('tbl'))
                  ),  

# Storage   -----------------------------------------------------
       tabItem(tabName = "concentration_check",
               fluidRow(box(solidHeader = T, status = "info",uiOutput("CONC_PATIENT_SELECT"))),
               fluidRow(uiOutput("graphs_ui")),
               fluidRow(box(actionButton("save_conc_check","Save new values to selected sample"),
                            solidHeader = T, status = "success",background = "green"))
             ),
       tabItem(tabName = "storage_position",
               fluidRow(box(status = "info",
               awesomeRadio("STUDY_SELECT", "Choose study", choices = STUDIES, selected = "IKP275",
                 inline = TRUE, checkbox = TRUE),
               radioGroupButtons("BOX_SELECT", "Select box",choices = 1:10,
                 justified = TRUE, checkIcon = list(yes = icon("ok",lib = "glyphicon"))
               ))),
               fluidRow(box(plotOutput("storage_plot"), width = 12)),
               fluidRow(box(DTOutput("storage_dt"), width = 12))
               
               
               
       ),


# genotyping --------------------------------------------------------------
                  tabItem(tabName = "genotyping",
                    fluidRow(box(uiOutput("GENOTYPING"))),
                    fluidRow(box(uiOutput("conc_select_genotyping")),
                    box(actionButton("save_geno","Save values to all selected samples"),solidHeader = T, status = "success",background = "green"),
                    box(dateInput("chip_date", "Date Openarray run", format = "yyyy-mm-dd", weekstart = 1,
                                  value = Sys.Date()),
                        uiOutput("CHIP_BARCODE"))),
                    fluidRow(box(width = 12, DTOutput("dt_geno")))
                  ),

# results -----------------------------------------------------------------
                  tabItem(tabName = "results",
                    fluidRow(box(
                      column(width = 12,uiOutput("SAMPLES_RESULT")),
                      column(width = 12,h5(strong("Choose Openarray Barcode for CYP2D6*7 [rs5030867; 2936T>G; H324P]"), "Note: Only one file per Chip allowed.")),
                      column(width = 6, uiOutput("LOAD_OA")), 
                      column(width = 1, actionButton("REFRESH_OA", "", icon = icon("sync-alt"))), 
                      column(width = 5, uiOutput("STAR7_GENO")),
                      br(),
                      br(),
                      br(),
                      column(width = 12,selectInput("GIMS_result", "CYP2D6 Diplotype GIMS report", choices = c("", unique(GIMS_result$Genotype_GIMS)), 
                                  selected = "")),
                      column(width = 12,selectInput("GIMS_TAMENDOX_man", "CYP2D6 phenotype according TAMENDOX (manuell)",
                                  selected = "",
                                  choices = c("", unique(GIMS_result$TAMENDOX)))),
                      column(width = 12,uiOutput("TAM_PHEN")),
                      column(width = 12,dateInput("GIMS_report_date", "Date on report (page 1)", format = "yyyy-mm-dd",weekstart = 1)),
                      column(width = 12,textAreaInput("GIMS_comment", "Comment", value = "", resize = "vertical"))),
                      box(title = "Sample Information", tableOutput("table_pat_info")),
                    box(actionButton("save_result","Save values to sample"),solidHeader = T, status = "success",background = "green")),
                    box(collapsible = T, h4("Real-life genotypes, diplotypes and related TAMENDOX phenotypes"),
                        h5(TRANSLATE_FILE), 
                        DTOutput("translation_table_tamendox"))
                  ),

# database ----------------------------------------------------------------
                  tabItem(tabName = "database",
                          fluidRow(box(status = "info", prettyRadioButtons("database_input", "Show all or selected columns",
                                                                  choices = c("all", "selected", "for Medcodes"), selected = "selected",
                                                                  inline = T,bigger = T),
                                   uiOutput("COLUMN_VARS"),
                                   prettyRadioButtons("database_rows", "Select rows",
                                                      choices = c("all", "not yet uploaded in medcodes"), selected = "all",
                                                      inline = T, bigger = T, status = "success")),
                          box(status = "info", 
                              uiOutput("DATABASE_STUDY"),
                              prettyRadioButtons("column_justification", "Justification", 
                                                 choiceNames = c("left", "center", "right"),
                                                 choiceValues = c("dt-left", "dt-center", "dt-right"),inline = T, bigger = T, status = "info"), 
                              downloadButton("download_database", "Download all as txt file"))),
                          fluidRow(box(status = "info", 
						  p("Excel and pdfs are saved in the default 'Download' directory"),
						  DTOutput("dt_database"), width = 12))),

# audit -------------------------------------------------------------------
                  tabItem(tabName = "audit",
                          box(DTOutput("dt_audit"),width = 12, status = "danger") ,
                          box(actionButton("refresh_audit", "Refresh")
                  )
                
# end UI ------------------------------------------------------------------
                  ) # end tab audit
               ) # tabItems
            ) # end Body
) # end dashboardpage
