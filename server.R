# Version 0.6
server <- function(input, output, session){
options(shiny.maxRequestSize=30*1024^2)

RV <- reactiveValues(template = pre_db,
                     chips =tibble(source = list.files("Y:/KG/PGX/3_CHIP_SPFs/", pattern = "spf", recursive = T)) %>% 
                       filter(!grepl("all_spf", source)) %>% 
                       separate(source, into = c("batch", "barcode", "spf")),
                     performed_chips = list.files("Y:/KG/PGX/5_Results/Openarray_csv/", pattern = "csv", full.names = T) %>% 
                       map(readLines, n= 5, warn =F) %>% 
                       map(~.[grep("Study Name ", .)]) %>% 
                       unlist(.) %>% 
                       gsub("# Study Name : ","", .),
                     validation_chips = list.dirs("Y:/KlinStud/Studien/IKP Studien/IKP 428_PGx/Validierung_Genotyping/Experiments", recursive = F,full.names = F) %>% 
                       grep("AQ", ., value =T))


# plate set up  
plate_blood <- reactiveValues(n_row = 8, n_col =8)

# start up  ---------------------------------------------------------------
start_up_modal <- function(failed = F) {modalDialog(
    easyClose = F,
    pretty_loginUI(id = "login", login_title = "Livesystem"),
    footer = NULL
)}
# Show the model on start up
showModal(start_up_modal())

observeEvent(input$sign_out_button,{ 
  RV$super_user <- NULL
  RV$user_permissions <- NULL
  showModal(start_up_modal())
  })  

# run function....but not needed. There is no log out
logout_init <- callModule(shinyauthr::logout, 
                          id = "logout", 
                          active = reactive(credentials()$user_auth))

# login module
credentials <- callModule(shinyauthr::login, 
                          id = "login", 
                          data = user_base,
                          user_col = user,
                          pwd_col = password,
                          sodium_hashed  = TRUE,
                          log_out = reactive(logout_init()))

# Print Super User
output$SUPER_USER <- renderUser({
  req(RV$super_user)
  
  SRC <-  credentials()$info$image
  SUBTITLE <-  credentials()$info$permissions
  
  if(RV$super_user == "Max Mueller"){
    SUBTITLE <- "Tester"  
    SRC <-  "user-solid.png"}
  
  dashboardUser(
    name = RV$super_user,
    subtitle = SUBTITLE, 
    src = SRC, 
    actionButton("sign_out_button", "Sign out", icon = icon("sign-out-alt"), class = "btn-danger"))
})

observeEvent(req(credentials()$user_auth),{

     removeModal()
     RV$database = readRDS("Y:/KG/PGX/6_Database/livesystem/live_database.rds")
     RV$audit =list.files("Y:/KG/PGX/6_Database/livesystem/live_db_exports/", full.names = T, pattern = "PGx_db_export") %>%
                           map(read.table, sep ="\t",stringsAsFactors = F, header=T) %>%
                           map(~mutate(.,patid =as.character(patid))) %>%
                           map(~mutate_at(., vars("isolation1_conc_elution", "isolation2_conc_elution",
                              "isolation1_conc_spin", "isolation2_conc_spin","isolation_conc"), ~gsub(",", ".", .) %>% as.numeric)) %>% 
       
                           bind_rows() %>%
                           as_tibble() %>%
                           distinct_all()
     RV$audit_path = "Y:/KG/PGX/6_Database/livesystem/live_db_exports"
     RV$autid_export_path = paste0("Y:/KG/PGX/6_Database/livesystem/live_db_exports/live_PGx_db_export_",format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt")
     RV$db_export_path = "Y:/KG/PGX/6_Database/livesystem/live_database.rds"
     RV$db_export_path_start = "Y:/KG/PGX/6_Database/livesystem/live_database"
  RV$database %>% 
    saveRDS(., paste0(RV$db_export_path_start,"_",format(Sys.time(), "%Y_%m_%d_%Hh"),".rds"))
  
  RV$super_user <- credentials()$info$name
  RV$user_permissions <- credentials()$info$permissions
  
})  

observeEvent(input$test,{
     removeModal()
       RV$database = readRDS("testsystem/test_database.rds")
       RV$audit =list.files("testsystem/test_db_exports/", full.names = T, pattern = "PGx_db_export") %>%
                           map(read.table, sep ="\t",stringsAsFactors = F, header=T) %>%
                           map(~mutate(.,patid =as.character(patid))) %>%
                           bind_rows() %>%
                           as_tibble() %>%
                           distinct_all()
       RV$audit_path = "testsystem/test_db_exports"
       RV$autid_export_path = paste0("testsystem/test_db_exports/test_PGx_db_export_",format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt")
       RV$db_export_path = "testsystem/test_database.rds"
       RV$db_export_path_start = "testsystem/test_database"
       RV$database %>% 
      saveRDS(., paste0(RV$db_export_path_start,"_",format(Sys.time(), "%Y_%m_%d_%Hh"),".rds"))
      
       RV$super_user <- "Max Mueller"
       RV$user_permissions <- "admin"
       
       })  
  
output$DROPDOWN1 <- renderMenu({

  req(RV$super_user)
  dropdownMenu(type = "notifications", badgeStatus = "primary",
               notificationItem(icon = icon("vial"), status = "info",
                                paste("n=", nrow(PRE_DNA_data()), "samples to DNA isolate")
               ),
               notificationItem(icon = icon("dna"), status = "info",
                                paste("n=", nrow(database_data() %>% filter(is.na(chip_barcode))), "samples to genotype")
               ),
               notificationItem(icon = icon("tasks"), status = "info",
                                paste("n=", nrow(RESULTS_SAMPLES()), "samples to upload in Medcodes")
               ),
               notificationItem(icon = icon("server"), status = "success",
                                paste("n=", nrow(database_data()), "samples are registered in database")
               ))
  })


task_data <- reactive({
    RV$database %>%
    keep(~!all(is.na(.))) %>%
    bind_rows() %>% 
    filter(is.na(GIMS_result)) %>% 
    mutate(today = Sys.Date()) %>% 
    mutate(due_date = entry_date + 21) 
})



output$DROPDOWN2 <- renderMenu({
  req(RV$super_user)
  
  tmp_tasks <- task_data()
  
  if(nrow(tmp_tasks) != 0){
  tmp_tasks <- tmp_tasks %>% 
    select(patid, entry_date, due_date, today) %>% 
    mutate(days_left = round(as.numeric(due_date - today)/21*100, 0)) %>% 
    mutate(days_left = ifelse(days_left<0, 0, days_left)) %>% 
    mutate(color = as.character(cut(days_left,
									include.lowest = T,	
                                    breaks = c(0,20,40,60,80,100), 
                                    labels = c("red","yellow","aqua" ,"light-blue", "green")))) %>% 
    arrange(days_left) %>% 
    mutate(patid =factor(patid, levels = unique(patid))) %>% 
    split(.$patid) %>% 
    map(~taskItem(text = .x$patid, value = .x$days_left, color = .x$color))

  for(i in 1:length(tmp_tasks)){
    tmp_tasks[[i]]$children[[1]] <- a(href="#","onclick"=paste0("clickFunction('",names(tmp_tasks)[[i]],"'); return false;"),
                                      tmp_tasks[[i]]$children[[1]]$children)
  }
    dropdownMenu(type = "tasks", badgeStatus = "warning",
               .list =tmp_tasks)}else{
    dropdownMenu(type = "tasks", badgeStatus = "info", headerText = "There are no tasks")            
               }
})


observeEvent(input$linkClicked, {
  tmp_label <- task_data() %>% 
    filter(patid == input$linkClicked$data) %>% 
    select(study, entry_date, due_date, storage, box, position, entry_user)  %>% 
    mutate_all(as.character) %>% 
    gather %>% 
    mutate(label = paste0(key,": ", value)) 

  sendSweetAlert(
    session = session,
    title = HTML("Sample Information<br>", input$linkClicked$data),
    text = HTML(paste(tmp_label$label, collapse="<br>")),
    type = "info",
    html = TRUE,
    showCloseButton = TRUE)
})

  



output$TAMENDOX_results <- renderMenu({
   req(RV$user_permissions != "TA")
    menuItem("Results TAMENDOX", tabName = "results", icon = icon("poll"))
})

  
# register DT----------------------------------------------------------
DATAFRAME <- reactive({
  req(RV$database)
  RV$database %>%
    keep(~!all(is.na(.))) %>% 
    bind_rows() 
})



REGISTER_DATA <- reactive({
  req(RV$database)
  DATAFRAME() %>% 
    filter(!is.na(dbid)) %>%
    arrange(-entry_timestamp)

})

output$dt_register <- DT::renderDataTable({
  req(RV$database)
    REGISTER_DATA() %>% 
    filter(is.na(GIMS_result)) %>% 
    filter(is.na(chip_barcode)) %>% 
    filter(is.na(DNA)) %>%
    select(study:entry_user) %>% 
    my_datatable
})

# NEXT button only shows up if right format
output$next_sample <- renderUI({
  req(input$patid)
  tmp_pat <- input$patid
  tmp <- list("IKP428" =all(c(str_length(tmp_pat) == 10,
    !grepl("[:alpha:]",tolower(tmp_pat)))), 
    "IKP275" =all(c(str_length(tmp_pat) == 7, 
             str_split(tmp_pat,"-", simplify = T) %>% 
               map_chr(str_length) %>% unique() == 3)))
  if(tmp[[input$study]]){
    actionButton("Next","Add sample")
  }
})

# dependend storage containers
# TK
output$STORAGE <- renderUI({
  req(input$study)  
  selectInput("storage", "Storage", choices = STORAGE_TK, 
              selected = ifelse(input$study == "IKP275", STORAGE_TK[1], STORAGE_TK[2]))
})

# Box
output$BOX <- renderUI({
  # find last registered sample
  if(is_empty(REGISTER_DATA())){box <- 1 }else{
  box <- REGISTER_DATA() %>% 
    filter(study == input$study) %>% 
    slice(1) %>% 
    pull(box)
  }
  if(is_empty(box)) box=1
  
  
  numericInput("box", "Kryobox", min = 1, max = 100, step = 1, value = box)
  
})

# Position
output$POSITION <- renderUI({
  req(input$box)

  
  plate_blood$mm <- data.frame(row=rep(plate_blood$n_row:1, each = plate_blood$n_col), 
                               let = rep(rev(LETTERS[plate_blood$n_row:1]), each = plate_blood$n_col),
                               col= rep(1:plate_blood$n_col, plate_blood$n_row), loaded = F) %>% 
    unite(Pos, let, col, remove = F, sep="") %>% 
    mutate(position = row_number())
  
  POS <- plate_blood$mm$Pos
  if(!is_empty(REGISTER_DATA())){
   gr <- REGISTER_DATA() %>% 
    filter(study == input$study) %>% 
    filter(box == input$box) %>% 
    pull(position)
   
   POS <- setdiff(POS, gr) 
  }
    
  selectInput("position", "Position", choices = POS, multiple = F)
})

# Box layout
output$entry_storage_plot <- renderPlot({

  req(input$study)
  req(input$box)

  n_row <- plate_blood$n_row
  n_col <- plate_blood$n_col
  LABEL <- rev(unique(plate_blood$mm$let))
  
  pos_dat <-REGISTER_DATA() %>% 
    filter(study == input$study) %>% 
    filter(box == input$box) %>% 
    select(patid, Pos =position)

  box <- input$box

  plate_blood$mm %>%
    left_join(pos_dat, by="Pos") %>%
    mutate(loaded = ifelse(!is.na(patid), TRUE, loaded)) %>%
    ggplot(aes(col, row, color =loaded)) +
    geom_point(size = 20, shape = 16, show.legend = F) +
    geom_text(aes(label = paste0(Pos,"\n", patid)), color = "black",  show.legend = F, size =3) +
    scale_y_continuous("", breaks = 1:n_row, labels = LABEL,limits = c(1, n_row),minor_breaks = NULL) +
    scale_x_continuous("", breaks = 1:n_col, limits = c(1, n_col),minor_breaks = NULL) +
    ggtitle(paste0("Box ", box)) +
    theme_minimal(base_size = 20) +
    theme(text = element_text(face = "bold"))
})



# save sample to database
observeEvent(input$Next,{
  hide("Next")
  showNotification("Saved!", type = "message", duration = 2)
  # which fields to write
  register_fields <- col_head %>% filter(values == "register") %>% pull(col)
  # save template
  tmp <- RV$template
  #get fields from input
  tmp_input <- map(register_fields, function(x) input[[x]])
  names(tmp_input) <- register_fields
  # save in template
  tmp[names(tmp_input)] <- tmp_input
  # get dbid
  gr <- RV$database %>% map_lgl(~is.na(.x$dbid)) %>% which %>% head(.,1)
  # add missing stuff
  tmp$dbid = gr
  tmp$entry_user = RV$super_user
  tmp$entry_timestamp = as.integer(Sys.time())

  # save to all data
 RV$database[[gr]] <- tmp
 
 RV$database %>% 
   keep(~!all(is.na(.))) %>% 
   bind_rows() %>% 
   filter(!is.na(dbid)) %>% 
   save_audit_file(path = paste0(RV$autid_export_path,format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt"))
 
 RV$database %>% 
    saveRDS(., file = RV$db_export_path)
 
 updateTextInput(session, "patid", value = "")
 show("Next")
  
})


# LIMS input --------------------------------------------------------------

LIMS_import <- reactive({
  tryCatch(
    {reg_patIDS <-RV$database %>%  map("patid") %>% unlist %>% .[!is.na(.)] 
     
      read_excel(input$LIMS_file$datapath,1, skip = 2) %>% 
        filter(!grepl("<|>", `Sample/LIMSID`)) %>% 
        filter(grepl("[0-9]", `UDF/TAM_Studienzentrum`) & grepl("[0-9]", `Sample/Name`)) %>% 
        # length of three
        filter(str_length(`UDF/TAM_Studienzentrum`) == 3 & str_length(`Sample/Name`) == 3) %>% 
        # Blut
        filter(`UDF/TAM_Probentyp` == "Blut") %>% 
        unite(id, `UDF/TAM_Studienzentrum`,`Sample/Name`, sep = "-") %>% 
        filter(!id %in% reg_patIDS)
      
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
})


output$contents <- renderTable({
  req(input$LIMS_file)
  LIMS_import() %>% 
    select(id,
           `UDF/TAM_Aliquot`, `UDF/TAM_Probentyp` ,
           `UDF/TAM_Probeneingang`, `UDF/TAM_Zustand der Probe`,`UDF/TAM_Tiefkühler`,
           `UDF/TAM_Lagerbox Nr`,`UDF/TAM_Position in Box`)
})
  
# add sample to database
observeEvent(input$save_lims,{
  showNotification("Saved!", type = "message", duration = 2)
  req(input$LIMS_file)
  LIMS <- LIMS_import()
  # number of samples 
  nrows = nrow(LIMS)
  
  if(nrows < 1 ) cat("Showing no samples", nrows, "rows\n")
  gr <- RV$database %>% map_lgl(~is.na(.x$dbid)) %>% which %>% head(., nrows)
  
  for(i in 1:nrows){
  tmp <- RV$template
  # add values
  tmp$dbid = gr[i]
  tmp$study = "IKP275"
  tmp$patid = LIMS$id[i]
  tmp$entry_condition = LIMS$`UDF/TAM_Zustand der Probe`[i]
  tmp$entry_user  ="LIMS"
  tmp$entry_date = as.Date(LIMS$`UDF/TAM_Probeneingang`[i])
  tmp$vial = (LIMS$`UDF/TAM_Aliquot`[i])
  tmp$entry_timestamp = as.integer(Sys.time())
  tmp$storage = LIMS$`UDF/TAM_Tiefkühler`[i]
  tmp$box =  as.numeric(LIMS$`UDF/TAM_Lagerbox Nr`[i])
  tmp$position =LIMS$`UDF/TAM_Position in Box`[i]
  RV$database[[gr[i]]] <- tmp
  
  }
  
  RV$database %>% 
    keep(~!all(is.na(.))) %>% 
    bind_rows() %>% 
    filter(!is.na(dbid)) %>% 
    save_audit_file(path = paste0(RV$autid_export_path,format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt"))
  
  RV$database %>% 
    saveRDS(., RV$db_export_path)

})


# pre DNA ------------------------------------------------------------
PRE_DNA_data <- reactive({
  DATAFRAME() %>% 
    filter(!is.na(dbid)) %>% 
    filter(is.na(DNA) | DNA %in% c("konz2", "blut")) 
})


output$DNA_ISOLATION <- renderUI({
  choices <-PRE_DNA_data() %>% 
    pull(patid)
    multiInput("DNA_isolation", "Select Samples for DNA isolation",
              choices = choices,
              selected = choices[1:48])
})

output$dt_preDNA <- DT::renderDataTable({
  req(input$DNA_isolation)
  PRE_DNA_data() %>%
    filter(patid %in% input$DNA_isolation) %>% 
    arrange(study, patid) %>% 
    unite(Storage, storage, box, position) %>% 
    select(study, `patid_c&p` = patid, vial, Storage) %>% 
    rownames_to_column(var = "index") %>% 
    my_datatable(pageLength = 48)
})

output$pre_DNA_info <- renderText({
  paste("There are", nrow(PRE_DNA_data()), "blood samples to isolate DNA.\n")
  
})


# Label -------------------------------------------------------------------
nanodrop <- reactive({
  list.files("Y:/KG/PGX/2_DNA_Isolierung/3_Nanodrop", full.names = T) %>% 
    map(read.delim,  stringsAsFactors = F) %>% 
    bind_rows(.id = "file") %>% 
    as_tibble() %>% 
    select(Sample.ID, X260.280, X260.230) %>% 
    mutate_at(vars(X260.280, X260.230), ~gsub(",", ".", .) %>% as.numeric) %>%
    filter(!grepl("uff|ontro|AE|asse|2o", Sample.ID)) %>% 
    mutate(study = str_split(Sample.ID, " |_|-", simplify = T) %>% .[,1]) %>% 
    mutate(patid = str_split(Sample.ID, " |_|-", simplify = T, n = 2) %>% 
             .[,2] %>% str_split(., " |_", simplify = T) %>% .[,1]) %>% 
    mutate(Isolierung = ifelse(grepl("solierun",Sample.ID), "isolation2", "isolation1")) %>% 
    group_by(study, patid, Isolierung) %>% 
    summarise(Ratio_260vs280 = toString(sort(unique(X260.280))),
              Ratio_260vs230 = toString(sort(unique(X260.230)))) %>% 
    mutate(Typ = "nach Eluierung")
})

label_data <- reactive({
  req(nanodrop())
  aa <-   DATAFRAME() %>% 
    filter(!is.na(dbid)) %>% 
    unite(Blut_Lagerort, storage, box, position)

  Conc <- aa %>%
    select(study, patid, vial, isolation1_conc_elution, isolation1_conc_spin,
           isolation2_conc_elution, isolation2_conc_spin) %>% 
    gather(iso, conc, -study, -patid, -vial) %>% 
    mutate(iso2 = str_split(iso, "_", simplify = T) %>% .[,1]) %>% 
    filter(!is.na(conc))
  
  Date <- aa %>%
    select(study, patid,vial, isolation1_date, isolation2_date) %>% 
    gather(iso, date, -study, -patid, -vial) %>% 
    mutate(iso2 = gsub("_date", "", iso)) %>% 
    select(-iso)
  
  Afgbrcht <- aa %>% 
    select(study, patid,vial, isolation1_elution_afgbrcht, isolation2_elution_afgbrcht) %>% 
    gather(iso, Aufgebraucht, -study, -patid, -vial)  %>% 
    mutate(iso2 = str_split(iso, "_", simplify = T) %>% .[,1]) %>% 
    select(-iso)
  
  Conc %>% 
    left_join(Date, by = c("study", "patid", "vial", "iso2")) %>% 
    left_join(Afgbrcht, by = c("study", "patid", "vial", "iso2")) %>% 
    mutate(iso = ifelse(grepl("elution", iso), "nach Eluierung", "nach Aufkonz")) %>% 
    select(study, patid, vial, Isolierung =iso2, date, Typ=iso, Konz =conc, Aufgebraucht) %>% 
    mutate(Aufgebraucht = ifelse(Aufgebraucht, "Ja", "Nein")) %>% 
    mutate(Aufgebraucht = ifelse(Typ == "nach Aufkonz", "", Aufgebraucht)) %>% 
    arrange(study, patid, date) %>% 
    mutate(date= as.Date(date,format = "%Y-%m-%d") %>% format(., "%d.%m.%Y")) %>% 
    left_join(nanodrop(), by = c("study", "patid", "Isolierung", "Typ")) %>% 
    set_names(tolower(names(.))) 
  
})
  
output$dt_label <- DT::renderDataTable({
  req(RV$database)
  label_data() %>% 
     my_datatable()
})


observeEvent(input$save_label,{ 
  hide("save_label")   

  showNotification("Saved!", type = "message", duration = 2)
  write_xlsx(label_data(), "Y:/KG/PGX/2_DNA_Isolierung/1_Etiketten/LIMS_PTOUCH_DB.xlsx", 
                      format_headers = F)
  
  show("save_label")  
  })

# post DNA ----------------------------------------------------------------

file_uploader <- reactiveValues(
  data = NULL,
  clear = FALSE
)


observe({
  req(input$inFile)
  req(!file_uploader$clear)

    tmp <- read_excel(input$inFile$datapath, 	sheet=1, skip = 21) %>%
    select(1:2,
           patid = `patid_c&p`,
           vial = contains("ial"),
           Isolationsdatum =starts_with("Isolation"),
           Konz_Eluierung = contains("Eluierung"),
           Eluierung_aufgbcht = contains("Aufgebraucht"),
           Konz_Aufkonz =contains("Aufkonz"),
           DNA = contains("Bemerkunge")) %>%
    filter(!is.na(study) & !is.na(patid))
    
    if(!all(str_count(tmp$Isolationsdatum, "-") == 2, na.rm = T)){
      showNotification("Datumsspalte enthält falsche Werte, die nicht Daten sind.",
                       type = "error")
    }

    validate(
      need(all(str_count(tmp$Isolationsdatum, "-") == 2, na.rm = T), message  = "Spalte Isolationsdatum darf nur Datum enthalten")
    )


    tmp2 <- tmp %>%
    mutate(Isolationsdatum = as.Date(Isolationsdatum)) %>%
    mutate_at(vars("Konz_Eluierung", "Konz_Aufkonz"), ~gsub(",", ".", .) %>% as.numeric) %>%
    mutate(Eluierung_aufgbcht= ifelse(Eluierung_aufgbcht == "JA", TRUE, FALSE)) %>%
    left_join(REGISTER_DATA() %>%
                select(dbid, study, patid, vial, isolation1_date) %>%
                mutate(in_db = T), by = c("study", "patid", "vial")) %>% 
    mutate(in_db = ifelse(Isolationsdatum != isolation1_date| is.na(isolation1_date), in_db, F)) %>% 
    mutate(in_db = ifelse(is.na(Konz_Eluierung) & is.na(Konz_Aufkonz), F ,in_db))  
   
    # if no data set data to NULL
    if(nrow(tmp2) >0){file_uploader$data <- tmp2}else{
      file_uploader$data <- NULL
      file_uploader$clear <- TRUE
    }

})

observeEvent(input$inFile, {
  file_uploader$clear <- FALSE
}, priority = 1000)


observeEvent(input$cancel_file, {
  file_uploader$data <- NULL
  file_uploader$clear <- TRUE
  reset('inFile')
}, priority = 1000)

output$table_uploaded_dna_samples <- renderDataTable({
  req(input$inFile)
  req(file_uploader$data)
  req(!file_uploader$clear)
  
  isolated <- file_uploader$data$patid
  my_colors <- ifelse(file_uploader$data$in_db, "lightgreen", "lightgrey")
 
  file_uploader$data %>%
    select(-in_db, -dbid) %>%
    select(-isolation1_date) %>% 
    my_datatable() %>%
    formatStyle("patid", target = 'row',  backgroundColor = styleEqual(isolated , my_colors))
})



# # save to database    
observeEvent(input$save_dna,{
  req(input$inFile)
  req(any(file_uploader$data$in_db))
  req(!file_uploader$clear)
  
  # samples get dbid
  gr_add <- file_uploader$data %>%
    filter(in_db) %>%
    distinct(dbid, study)

  for(i in 1:nrow(gr_add)){
    x <- gr_add$dbid[i]
    x_study <- gr_add$study[i]
    
    # get patient slot
    tmp_write <- file_uploader$data %>% filter(dbid == x) %>% slice(1)
    tmp <- RV$database[[x]]

    # if iso1 not empty write in iso 2 
    if(!is.na(tmp$isolation1_conc_elution)){
      tmp$isolation2_date = tmp_write$Isolationsdatum
      tmp$isolation2_conc_elution =  tmp_write$Konz_Eluierung
      tmp$isolation2_elution_afgbrcht = tmp_write$Eluierung_aufgbcht
      tmp$isolation2_conc_spin =  tmp_write$Konz_Aufkonz
      tmp$DNA = tmp_write$DNA

      tmp$isolation2_conc_method = input$isolation_conc_method
      tmp$isolation2_method = input$isolation_method
      tmp$isolation2_comment = input$isolation_comment
      tmp$isolation2_user =  RV$super_user
      tmp$isolation2_timestamp = as.integer(Sys.time())

    }else{
      tmp$isolation1_date = tmp_write$Isolationsdatum
      tmp$isolation1_conc_elution =  tmp_write$Konz_Eluierung
      tmp$isolation1_elution_afgbrcht = tmp_write$Eluierung_aufgbcht
      tmp$isolation1_conc_spin =  tmp_write$Konz_Aufkonz
      tmp$DNA = tmp_write$DNA

      tmp$isolation1_conc_method = input$isolation_conc_method
      tmp$isolation1_method = input$isolation_method
      tmp$isolation1_comment = input$isolation_comment
      tmp$isolation1_user =  RV$super_user
      tmp$isolation1_timestamp = as.integer(Sys.time())
    }

    tmp$isolation_conc <- max(c(tmp$isolation1_conc_elution, tmp$isolation1_conc_spin,
                                tmp$isolation2_conc_elution, tmp$isolation2_conc_spin), na.rm = T)
    
   
    # override data in db
    RV$database[[x]] <- tmp

    RV$database %>%
    keep(~!all(is.na(.))) %>%
    bind_rows() %>%
    filter(!is.na(dbid)) %>%
    save_audit_file(path = paste0(RV$autid_export_path,format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt"))

  RV$database %>% saveRDS(., RV$db_export_path)
  }
    
  
  
  reset("isolation_comment")
  reset('inFile')
  file_uploader$data <- NULL

  showNotification("Saved!", type = "message", duration = 2)

})






# concentration check -----------------------------------------------------
CONCENTRATION_CHECK_data <- reactive({
  # all samples with 1 DNA measurment
   RV$database %>%
    keep(~!all(is.na(.))) %>% 
    map(~.[grep("dbid|patid|study|isolation1|isolation2|DNA", names(.))]) 
})

output$CONC_PATIENT_SELECT <- renderUI({
  
  CHOICES <- CONCENTRATION_CHECK_data() %>% 
    bind_rows() %>% 
    split(.$study) %>% 
    map(~pull(., patid))
  
  # oder by patid for IKP275
  CHOICES$IKP275 <-  CHOICES$IKP275[CHOICES$IKP275 %>% str_split(., "-", simplify = T) %>% .[,2] %>% order]
  
  
  pickerInput("conc_patient_select", "Select sample", choices = CHOICES, multiple = F, 
              options = list(style = "btn-primary",
                             `live-search` = TRUE))
  
})

free_slots <- reactive({
  
  loaded <-
    RV$database %>%
    keep(~!all(is.na(.))) %>% 
    map(~.[grep("study|strg", names(.))]) %>% 
    bind_rows() %>% 
    gather(iso, pos, -study)
  
  POSITION_DNA_POOL %>% 
    # remove position 9
    filter(column != 9) %>% 
    anti_join(loaded, by = c("study", "pos"))
  
})




output$graphs_ui <- renderUI({
  req(input$conc_patient_select)
  gr_pat <- CONCENTRATION_CHECK_data() %>%
    bind_rows() %>%
    filter(patid == input$conc_patient_select) %>%
    pull(dbid)

  validate(need(length(gr_pat != 1), "Contact admin"))

  data_tmp <-
    CONCENTRATION_CHECK_data() %>%
    .[[gr_pat]] %>%
    with(., list(iso1 = .[grep("isolation1", names(.))], iso2 =.[grep("isolation2", names(.))]))

  DNA_TYPE <- CONCENTRATION_CHECK_data() %>%  .[[gr_pat]] %>% .[[grep("DNA", names(.))]]


  x_study <- CONCENTRATION_CHECK_data()[[gr_pat]]$study

  pos_free <- free_slots() %>%
    filter(study == x_study) %>%
    pull(pos)

 
  plots_list_all <- map2(data_tmp, names(data_tmp), ~{

    data_red <- .x
    iso_num <- .y

    plots_list <- imap(names(data_red), ~{
      
      tagList(
        if(grepl(glob2rx("isolation*_date"), .x)){
          dateInput(.x, .x, format = "yyyy-mm-dd", weekstart = 1, value = data_red[[.y]], autoclose =T)
        },
        if(grepl(glob2rx("isolation*_storage"), .x)){
          selectInput(.x, .x, choices = STORAGE_KK,  selected = data_red[[.y]])
        },
        if(grepl(glob2rx("isolation*_strg_elution"), .x)){
                plchldr <- data_red[[.y]]
          if(is.na(data_red[[.y]])){ plchldr <- ""}
               selectInput(.x, gsub("strg","storage", .x), choices =c("", plchldr, pos_free),
                      selected = ifelse(is.na(data_red[[.y]]), "", data_red[[.y]]))
        },
        if(grepl(glob2rx("isolation*_strg_spin"), .x)){
          plchldr <- data_red[[.y]]
          if(is.na(data_red[[.y]])){ plchldr <- ""}
          selectInput(.x, gsub("strg","storage", .x), choices =c("",plchldr, pos_free),
                      selected = ifelse(is.na(data_red[[.y]]), "", data_red[[.y]]))
        },
        if(grepl(glob2rx("isolation*_conc_elution"), .x)){
          numericInput(.x, .x, value = data_red[[.y]], min = 0, step = 0.1)
        },
        if(grepl(glob2rx("isolation*_elution_afgbrcht"), .x)){
          
         SELECTED <-  ifelse(is.na(data_red[[.y]]), FALSE, data_red[[.y]])
          
         selectInput(.x, "used up elution?", choices = c(T, F), selected= SELECTED)
        },
        if(grepl(glob2rx("isolation*_conc_spin"), .x)){
          numericInput(.x, .x, value = data_red[[.y]], min = 0, step = 0.1)
        },
        if(grepl("comment", .x)){
          VAL = ifelse(is.na( data_red[[.y]]), "",  data_red[[.y]])
          textAreaInput(.x, .x, value =VAL)
        },
        if(grepl(glob2rx("isolation1_method|isolation2_method"), .x)){
          selectInput(.x, .x, choices = DNA_METHOD)
        },
        if(grepl("conc_method", .x)){
          selectInput(.x, .x, choices = CONC_METHOD, selected = data_red[[.y]])
        }
      )

    })

    tagList(box(title = HTML(paste0("Isolation <b>", iso_num,"</b>")),
                status = ifelse(iso_num == "iso1", "success","info"),
                width = 4,
                plots_list))
  })

  tagList(plots_list_all,
                   box(selectInput("DNA", "DNA", choices = DNA_type,
                                   selected = DNA_TYPE))
  )
  
 })


observeEvent(input$save_conc_check,{
  req(!any(duplicated(c(input$isolation1_strg_spin,
                        input$isolation2_strg_spin,
                        input$isolation1_strg_elution,
                        input$isolation2_strg_elution), incomparables = c("", NA))))

  confirmSweetAlert(
    session = session,
    inputId = "yes_save_new_conc",
    title = "Want to overwrite data?",
    closeOnClickOutside =T
  )
})



observeEvent(input$yes_save_new_conc, {
    req(input$conc_patient_select)

  if (isTRUE(input$yes_save_new_conc)) {
  hide("save_conc_check")
  # which patient 
  gr_add <- CONCENTRATION_CHECK_data() %>% 
    bind_rows() %>% 
    filter(patid == input$conc_patient_select) %>% 
    pull(dbid)
  
  validate(need(length(gr_add != 1), "Contact admin"))
  
  # which fields to write
  iso_fields <- col_head %>% filter(values == "iso") %>% pull(col) %>% .[c(1:18, 21,22)]
  #get fields from input
  tmp_input <- map(iso_fields, function(x) input[[x]])
  names(tmp_input) <- iso_fields
 
  if(length(tmp_input$isolation1_date) == 0){
    tmp_input$isolation1_date <- NA
  }
   
  if(length(tmp_input$isolation2_date) == 0){
    tmp_input$isolation2_date <- NA
  }
  
  tmp_input$isolation1_elution_afgbrcht <- as.logical(tmp_input$isolation1_elution_afgbrcht)
  tmp_input$isolation2_elution_afgbrcht <- as.logical(tmp_input$isolation2_elution_afgbrcht)

    # get patient data
    tmp <- RV$database[[gr_add]]
    # save all fields
    tmp[names(tmp_input)] <- tmp_input
   
    # add missing stuff
    tmp$isolation1_timestamp = as.integer(Sys.time())
    tmp$isolation2_timestamp = as.integer(Sys.time())
    tmp$isolation1_user = RV$super_user
    tmp$isolation2_user =  RV$super_user

    tmp$DNA <-  input$DNA 
    tmp$isolation_conc <- max(c(tmp$isolation1_conc_elution, tmp$isolation1_conc_spin,
                                 tmp$isolation2_conc_elution, tmp$isolation2_conc_spin), na.rm = T)
    RV$database[[gr_add]] <- tmp
  
  RV$database %>% 
    keep(~!all(is.na(.))) %>% 
    bind_rows() %>% 
    filter(!is.na(dbid)) %>% 
    save_audit_file(path = paste0(RV$autid_export_path,format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt"))
  
  RV$database %>% 
    saveRDS(., RV$db_export_path)
  
  showNotification("Saved!", type = "message", duration = 2)
  show("save_conc_check")
  }
}, ignoreNULL = TRUE)


# Storage ---------------------------------------------------------------------

pos_loaded <- reactive({
  req(input$STUDY_SELECT)

  stgr_pos <- RV$database %>%
    keep(~!all(is.na(.))) %>% 
    map(~.[grep("study|strg|patid", names(.))]) %>% 
    bind_rows() %>% 
    gather(iso, pos, -study, -patid) %>% 
    filter(study == input$STUDY_SELECT) %>%
    mutate(isolierung = str_split(iso, "_", simplify = T) %>% .[,1]) %>% 
    mutate(typ = ifelse(grepl("elution", iso), "nach Eluierung", "nach Aufkonz")) %>%  
    select(-iso) %>% 
    right_join(label_data(), by = c("study", "patid", "isolierung", "typ"))
  
  POSITION_DNA_POOL %>%
    filter(study == input$STUDY_SELECT) %>%
    filter(box == input$BOX_SELECT) %>%
    left_join(stgr_pos, by= c("pos", "study")) %>% 
    mutate(color =is.na(patid)) %>%  
    mutate(label =ifelse(is.na(patid), "empty",
                         paste(paste0(study, " ",patid), 
                               paste0("    ", date, "    "), 
                               paste0(vial, " - ", konz, "ng/µl"), sep ="\n"))) 
})

output$storage_plot <- renderPlot({
  n_row= 9
  n_col =9
  pos_loaded() %>% 
   ggplot(aes(x, y, fill =color)) +
    geom_tile(show.legend = F, color ="white") + 
    geom_text(aes(label = label), color = 1,  show.legend = F, size =3) +
    scale_fill_manual(values =c("TRUE" = "#0073C2FF" , "FALSE" = "#EFC000FF")) + 
    theme_minimal(base_size = 20) +
    theme(text = element_text(face = "bold"))+
    scale_x_continuous("", breaks = 1:n_col, minor_breaks = NULL,position = "top") +
    scale_y_continuous("", breaks = 1:n_row, labels = LETTERS[9:1], minor_breaks = NULL) 
})


output$storage_dt <- renderDT({
  pos_loaded() %>% 
    select(row, column, label) %>% 
    spread(column, label) %>% 
    mutate_all(~gsub("\n", "<br/>", .)) %>% 
    my_datatable(Col_dir = "dt-center")
 })

# genotyping ----------------------------------------------------------

output$GENOTYPING <- renderUI({
  
 tmp <-    DATAFRAME() %>% 
    filter(!is.na(dbid)) %>%
    filter(is.na(GIMS_result)) %>%
    filter(DNA %in% c("ok", "preamp"))

  multiInput("GENOTYPING_select", "Select Samples for genotyping (all samples wit DNA (ok, preamp) are listed.)",
             choiceNames = paste(tmp$patid, 
                                 ifelse(is.na(tmp$chip_barcode), "", paste0(" / ", tmp$chip_barcode))),
             choiceValues = tmp$patid,
             selected = tmp$patid[is.na(tmp$chip_barcode)][1:37])
})





GENOTYPING_data <- reactive({

  tmp <-  DATAFRAME() %>% 
    filter(!is.na(dbid)) %>%
    filter(is.na(GIMS_result)) %>%
    filter(DNA %in% c("ok", "preamp"))  
 
  
  stgr_pos <- RV$database %>%
    keep(~!all(is.na(.))) %>% 
    map(~.[grep("study|strg|patid", names(.))]) %>% 
    bind_rows() %>% 
    gather(iso, pos, -study, -patid) %>% 
    mutate(isolierung = str_split(iso, "_", simplify = T) %>% .[,1]) %>% 
    mutate(typ = ifelse(grepl("elution", iso), "nach Eluierung", "nach Aufkonz")) %>%  
    select(-iso) %>% 
    right_join(label_data(), by = c("study", "patid", "isolierung", "typ")) %>% 
    separate(pos, into = c("Box", "Platz"), sep = "_", extra = "merge", remove = F, fill ="left")
  
  tmp %>%   
    select(study, patid, isolation1_storage, DNA) %>% 
    left_join(stgr_pos, by = c("study", "patid")) %>% 
    filter(aufgebraucht != "Ja") %>% 
    unite(typ, isolierung, typ, sep = ": ") 
   
})
  


output$conc_select_genotyping <- renderUI({
  req(input$GENOTYPING_select)
  
  my_table <- GENOTYPING_data() %>%
    filter(patid %in% input$GENOTYPING_select) %>%
    group_by(patid, study) %>%
    slice(1)
  
  plots_list <- map(my_table$patid, ~{

    tmp_conc <- GENOTYPING_data() %>%
      filter(patid == as.character(.x)) %>% 
      arrange(-konz)
    
      validate(need(length(unique(tmp_conc$study)) == 1, "Contact admin, sample and corresponding study error"))
      
      pickerInput(inputId=paste0("rowskonz__", .x, "__", unique(tmp_conc$study)), label= .x,
                  choices= tmp_conc$konz, multiple = F,
                  choicesOpt = list(subtext = paste(tmp_conc$typ, tmp_conc$date)))

  })

  plots_list


})
 
output$dt_geno <- DT::renderDataTable({
  req(input$GENOTYPING_select)

  fields <- grep("rowskonz_", names(input), value = T)
  tmp_input <- map(fields, function(x) input[[x]])
  names(tmp_input) <- fields
  tmp <- tibble(patid= names(tmp_input), konz =as.numeric(tmp_input)) %>%
    separate(patid, into = c("delete", "patid", "study"), sep = "__") %>%
    select(-delete)

  GENOTYPING_data() %>%
    filter(patid %in% input$GENOTYPING_select) %>%
    inner_join(tmp,by = c("patid", "study", "konz")) %>%
    rownames_to_column(var = "index") %>%
    select(patid, study, konz, DNA, Lager =isolation1_storage, Box, Platz) %>% 
    group_by(patid, study) %>% 
    slice(1) %>% 
    ungroup %>% 
    my_datatable(pageLength = 37)

})


output$CHIP_BARCODE <- renderUI({
  tmp <- setdiff(RV$chips$barcode, RV$validation_chips)
  tmp <- setdiff(tmp, RV$performed_chips)
  
  CHOICES <- RV$chips %>% 
    filter(barcode %in% tmp) %>% 
    split(.$batch) %>% 
    map("barcode")
  
  pickerInput("chip_barcode", "Chip barcode", choices =append("", CHOICES), selected = "", multiple = F)
})

# add genotype to database
observeEvent(input$save_geno,{
  showModal(modalDialog(
    title = "Alle Samples ausgewählt und in Excelvorlage kopiert?",
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("save_geno_true", "Ja"))
  ))
})


observeEvent(input$save_geno_true, {
  req(input$chip_barcode)
  removeModal()
  # which patient
  gr_add <-   DATAFRAME() %>% 
    filter(!is.na(dbid)) %>%
    filter(is.na(GIMS_result)) %>%
    filter(DNA %in% c("ok", "preamp")) %>% 
    filter(patid %in% input$GENOTYPING_select) %>%
    pull(dbid)
  
  # which fields to write
  register_fields <- col_head %>% filter(values == "chip") %>% pull(col)
  #get fields from input
  tmp_input <- map(register_fields, function(x) as.character(input[[x]]))
  names(tmp_input) <- register_fields
  
  
  for(x in gr_add){
    # get patient data
    tmp <- RV$database[[x]]
    # save in template, but add data
    tmp_1 <- tmp[names(tmp_input)] 
    if(map_lgl(tmp_1, is.na) %>% all){
      tmp[names(tmp_input)] <- tmp_input
    }else{
      tmp[names(tmp_input)] <- map2(tmp[names(tmp_input)] , tmp_input, ~toString(c(.x,.y)))
    }
    # add missing stuff
    tmp$chip_user = RV$super_user
    tmp$chip_timestamp = as.integer(Sys.time())
    # update database
    RV$database[[x]] <- tmp }
  
  RV$database %>% 
    keep(~!all(is.na(.))) %>% 
    bind_rows() %>% 
    filter(!is.na(dbid)) %>% 
    save_audit_file(path = paste0(RV$autid_export_path,format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt"))
  
  RV$database %>% 
    saveRDS(., RV$db_export_path)
  
  updateSelectInput(session, "chip_barcode", selected = "")
  updateMultiInput(session, "GENOTYPING_select")
  showNotification("Saved!", type = "message", duration = 2)
})
  


# results -----------------------------------------------------------------

RESULTS_SAMPLES <- reactive({
  DATAFRAME() %>% 
    filter(!is.na(dbid)) %>% 
    filter(!is.na(chip_barcode)) %>% 
    filter(DNA %in% c("ok", "preamp")) %>%
    filter(is.na(GIMS_result)) 
})


output$SAMPLES_RESULT <- renderUI({
  choices <- RESULTS_SAMPLES() %>%
    pull(patid)
  selectInput("samples_result", "Select sample", choices = choices, multiple=F)
})



output$LOAD_OA <- renderUI({
    selectInput("load_oa", NULL, choices = c("", sort(RV$performed_chips)), multiple = F)
  })


observeEvent(input$REFRESH_OA, {
  
  RV$performed_chips <-  list.files("Y:/KG/PGX/5_Results/Openarray_csv/", pattern = "csv", full.names = T) %>% 
    map(readLines, n= 5, warn =F) %>% 
    map(~.[grep("Study Name ", .)]) %>% 
    unlist(.) %>% 
    gsub("# Study Name : ","", .)
  
})
  
output$STAR7_GENO <- renderUI({
  req(input$load_oa)
  
  gr_file <- list.files("Y:/KG/PGX/5_Results/Openarray_csv/", pattern = input$load_oa, full.names = T) %>% .[1] 
  gr_line <- readLines(gr_file) %>% 
                grep("Assay", .) 
  gr_genotype <- read.csv(gr_file,
           skip = gr_line[1]-1,
           nrows = gr_line[2]-gr_line[1]-2,stringsAsFactors = F) %>% 
    filter(Assay.ID == "C__32388575_A0") %>% 
    filter(grepl(input$samples_result, Sample.ID)) %>% 
    filter(Sample.ID == input$samples_result) %>% 
    pull(Call) %>% 
    unique()
 
  selectInput("star7_geno", NULL, choices = gr_genotype, selected = gr_genotype[1], multiple = F)

})

output$table_pat_info <- renderTable({
  RESULTS_SAMPLES() %>% 
    filter(patid %in% input$samples_result) %>% 
    select(study, patid, isolation_conc, DNA, chip_barcode) 
})
  
output$TAM_PHEN <- renderUI({
  req(input$GIMS_result)
  req(input$GIMS_TAMENDOX_man)
  req(input$star7_geno)
  choice <- GIMS_result %>% 
               filter(Genotype_GIMS == input$GIMS_result) %>%
               filter(CYP2D6_7_rs5030867_2936T_G_H324P == input$star7_geno) %>%
               pull(TAMENDOX)
  if(!input$star7_geno %in% c("T/T", "T/G", "G/G")){
    choice <- ""
  }
  
  
  selectInput("GIMS_TAMENDOX_at", "CYP2D6 phenotype according TAMENDOX (automatic)", choices = c("",unique(GIMS_result$TAMENDOX)),
              selected = choice)
})


output$translation_table_tamendox <- renderDT({
  GIMS_result %>% 
    my_datatable
})

observeEvent(input$samples_result,{
  updateSelectInput(session, "GIMS_TAMENDOX_man", selected = "")
  updateSelectInput(session, "GIMS_result", selected = "")
  updateTextAreaInput(session, "GIMS_comment", value = "")

})

#  save GIMS result to database
observeEvent(input$save_result,{
  req(input$samples_result)
  req(input$GIMS_TAMENDOX_man)
  req(input$GIMS_TAMENDOX_at)
  req(input$star7_geno)
  req(input$GIMS_report_date)
  # which patient
  gr_add <-  RESULTS_SAMPLES() %>% 
    filter(patid %in% input$samples_result) %>%
    pull(dbid)
  
  # which fields to write
  register_fields <- col_head %>% filter(values == "gims") %>% pull(col)
  #get fields from input
  tmp_input <- map(register_fields, function(x) input[[x]])
  names(tmp_input) <- register_fields

  
  # get patient data
  tmp <- RV$database[[gr_add]]
  # save in template
  tmp[names(tmp_input)] <- tmp_input
  # add missing stuff
  tmp$GIMS_user = RV$super_user
  tmp$GIMS_timestamp = as.integer(Sys.time())
  # update database
  RV$database[[gr_add]] <- tmp
  
  RV$database %>% 
    keep(~!all(is.na(.))) %>% 
    bind_rows() %>% 
    filter(!is.na(dbid)) %>% 
    save_audit_file(path = paste0(RV$autid_export_path,format(Sys.time(), "%Y_%m_%d_%Hh%Mm%Ssec"),".txt"))
  
  RV$database %>% 
    saveRDS(., RV$db_export_path)
  
  
  updateSelectInput(session, "samples_result")
  updateSelectInput(session, "GIMS_TAMENDOX_man",selected = "")
  updateSelectInput(session, "GIMS_result",selected = "")
  showNotification("Saved!", type = "message", duration = 2)
})


# database ----------------------------------------------------------------
nanodrop_database <- reactive({
  tmp <- nanodrop() %>% 
  ungroup %>%
    mutate(nanodrop = paste0("260/280=",Ratio_260vs280, "; 260/230=",Ratio_260vs230)) %>% 
    select(study, patid, Isolierung, nanodrop) %>% 
    spread(Isolierung, nanodrop) 

	if(any(names(tmp) == "isolation2")){
	tmp  %>% 
    select(study, patid, iso1_nanodrop=isolation1, iso2_nanodrop=isolation2)}
	else{
	tmp  %>% 
    select(study, patid, iso1_nanodrop=isolation1)
	}
})

database_data <- reactive({
  data <-   DATAFRAME() %>% 
    filter(!is.na(dbid)) 

  if(RV$user_permissions == "TA"){
    data <- data %>% select(-contains("GIMS"))
  }
  data 
})

output$DATABASE_STUDY <- renderUI({
  req(RV$super_user)
  studies <- RV$database %>%
    keep(~!all(is.na(.))) %>% map_chr("study") %>% unique()
  prettyRadioButtons("database_study", "Select studies",
                     choices = c("all", studies), selected = "all",
                     inline = T, bigger = T, status = "success")
})

output$COLUMN_VARS <- renderUI({
  req(RV$database)
  req(RV$super_user)
  SELECTED <-  choices <- RV$template %>% names
  SELECTED <-  unique(SELECTED, names(nanodrop_database()))
  
  if(input$database_input == "selected"){
       SELECTED <- c("study","patid", "entry_condition",
                  "storage", "box", "position", "isolation_conc", "DNA", "GIMS_result",
                  "GIMS_TAMENDOX_at") 
  }
  
  if(input$database_input == "for Medcodes"){
    
    SELECTED <- c("study","patid", "vial", "entry_condition",
                  "isolation_conc", "DNA", "GIMS_result",
                  "GIMS_TAMENDOX_man", "GIMS_comment",
                  "GIMS_TAMENDOX_at", "GIMS_report_date") 
  }
     
  varSelectInput("column_vars", "select columns to show",
                  data= database_data() %>% 
                   left_join(nanodrop_database(), by = c("study", "patid")), 
                 selected =  SELECTED,
                 multiple = T)
})
  
output$dt_database <- renderDataTable({
  req(input$column_vars)
  req(RV$super_user)
  
  tmp <- database_data() %>% 
    left_join(nanodrop_database(), by = c("study", "patid")) %>% 
    select(!!!input$column_vars)
  
  if(input$database_rows == "not yet uploaded in medcodes"){
    tmp <- tmp %>% 
      filter(is.na(GIMS_TAMENDOX_at))
  }
  
  if(input$database_study != "all"){
    
    tmp <- tmp %>% filter(study == input$database_study)
    
  }

  tmp %>% 
   my_datatable(., scrollX =T, Col_dir = input$column_justification)
  
})


output$download_database <- downloadHandler(
   filename = function(){
    paste("data-", Sys.Date(), ".txt", sep="")
  },
  content = function(file) {
    write.table(database_data(), file, quote = F, sep = "\t", row.names = F, dec = ".")
  }
)




# print audit DT ----------------------------------------------------------

output$dt_audit <- DT::renderDataTable({
  
  RV$audit %>%  
  my_datatable(., scrollX =T)  
    
})

observeEvent(input$refresh_audit,{
  RV$audit <- list.files(RV$audit_path, full.names = T, pattern = "PGx_db_export") %>%
    map(read.table, sep ="\t",stringsAsFactors = F, header=T) %>% 
    map(~mutate(.,patid =as.character(patid))) %>% 
    map(~mutate_at(., vars("isolation1_conc_elution", "isolation2_conc_elution",
                           "isolation1_conc_spin", "isolation2_conc_spin","isolation_conc"), ~gsub(",", ".", .) %>% as.numeric)) %>% 
    bind_rows() %>% 
    as_tibble() %>% 
    distinct_all() 
})

if (!interactive()) {
  session$onSessionEnded(function() {
    stopApp()
    q("no")
  })
}
  
} # end server
