# Version 0.6

# packages ----------------------------------------------------------------
library(shinyauthr)
library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(readxl)
library(shinyjs)
library(writexl)

# Data --------------------------------------------------------------------
col_head <- read_excel("general_items/column_header_v2.xlsx",1)
TRANSLATE_FILE <- list.files("Y:/KG/PGX/1_Arbeitsunterlagen/", "Trans", full.names = T) %>% tail(., 1)
GIMS_result <- read_excel(TRANSLATE_FILE, 2, skip = 1)
CONDITION <- c("Eingefroren", "Aufgetaut","Kein Trockeneis mehr, aber gefroren")
DNA_METHOD <- c("QIAamp DSP DNA Blood Mini Kit_manuell","QIAamp DSP DNA Blood Mini Kit_QIAcube")
CONC_METHOD <-  c("Qubit","Nanodrop")
STORAGE_TK <- c("TK20-58", "KK20-118")
STORAGE_KK <- c("KK118")
DNA_type <- c("ok", "preamp", "konz2")
user_base <- readRDS("general_items/user_base.rds")
pre_db <- as.list(rep(NA, length(col_head$col)))
names(pre_db) <- col_head$col
STUDIES <- c("IKP428", "IKP275")
POSITION_DNA2 <- tibble(box = rep(1:10, each = 81), pos = rep(sort(paste0(rep(LETTERS[1:9], 9),"_", rep(1:9, each=9))), 10)) %>% 
  unite(pos, box, pos, remove = F) %>% 
  separate(pos, into =c("box", "row", "column"), sep = "_", remove = F) %>% 
  mutate(x = rep(rep(1:9, 9), 10),
         y = rep(rep(9:1, each = 9), 10))
POSITION_DNA_POOL <- list(IKP275 = POSITION_DNA2,
                          IKP428 = POSITION_DNA2) %>% 
  bind_rows(.id = "study") 

# global functions ---------------------------------------------------------------
pretty_loginUI <-function(id, title = "Please log in", user_title = "User Name", 
                           pass_title = "Password", login_title = "Log in", error_message = "Invalid username or password!"){
  ns <- shiny::NS(id)
  shiny::div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;", 
             shiny::wellPanel(shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"), 
                              shiny::textInput(ns("user_name"), 
                              shiny::tagList(shiny::icon("user"), user_title)), 
                              shiny::passwordInput(ns("password"), 
                              shiny::tagList(shiny::icon("unlock-alt"), pass_title)), shiny::div(style = "text-align: center;", 
                               fluidRow(column(width = 6, 
                                 shinyWidgets::actionBttn("test", "Testsystem", icon = icon("child"), style = "material-flat", color = "primary")),
                                        column(width = 6,
                                 shinyWidgets::actionBttn(ns("button"), login_title,icon = shiny::icon("server"), style = "material-flat", color = "danger")))),
                              shinyjs::hidden(shiny::div(id = ns("error"), shiny::tags$p(error_message, 
                                                                                         style = "color: red; font-weight: bold; padding-top: 5px;", 
                                                                                         class = "text-center")))))}



save_audit_file <- function(x, path){
  write.table(tail(x, 10), path, 
              quote = F, row.names = F, sep = "\t")
}

my_datatable <- function(x, pageLength = 50, scrollX =F, scrollY =F, Col_dir = "dt-left"){
  datatable(x, extensions = 'Buttons',
            rownames = F,
            selection ="single",
            escape = FALSE, 
            filter =  list(position = 'top', clear = TRUE),
            options = list(pageLength = pageLength, 
                           lengthMenu =list(c(10, 50, 100, 200, -1), list('10', '50', '100', "200", 'All')),
                           search = list(regex = TRUE),
                           dom = 'Blfrtip', scrollX = scrollX,
                           scrollY =scrollY,
                           searchHighlight = TRUE,
                           columnDefs = list(
                             list(targets = "_all", className = Col_dir)),
                           buttons = list(list(extend = "copy", title = NULL), 
                                          list(extend = "excel", title = NULL),
                                          list(extend = "pdf", title = NULL))))
}

help_text_lims <- "Zur  Erstellung  einer  Liste  aller  vor-
handenen Proben wird unter „Projects & Samples“ das Projekt TAMENDOX geöffnet. Unter 
„Submit  Samples“  wird  „Modify  Samples“  ausgewählt.  Damit  wird  eine  Excel-Datei  erstellt, 
die die Daten aller in diesem Projekt enthaltenen Proben umfasst. Diese Datei kann direkt hier hochgeladen werden.
Es werden automatisch alle notwendiegn Spalten extrahiert und nur bereits nicht registrierte Proben (xxx-yyy; x=Zentrum, y=PatID)
angezeigt und hochgeladen."



