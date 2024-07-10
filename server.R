# Author: Roman Tremmel
# Code for www.Pharmfreq.com
# PLease cite XXXX when you are using code or data
function(input, output, session){
  enableBookmarking("url")
  showLog()
  rv <- reactiveValues()

  observeEvent(input$go, {
    session$sendCustomMessage(
      type = "update-tabs",
      message = "map"
    )
  })

  observe({
    req(input$gene)
    req(input$mapping_method)
    
    rv$raw_data <- filter(data$freq, gene == input$gene) 
    
    # condiational allele selection:
    rv$raw_data_for_allele_picker <- aa <<- switch(input$mapping_method,
           "Countries" = data$freq_per_country,
           "Geographical groups"  = data$freq_per_subgroup) %>% 
      filter(gene == input$gene)
    rv$alleles <- gtools::mixedsort(unique(rv$raw_data_for_allele_picker$Allele))
    
    
    if(input$gene %in% unique(data$functional_alleles$gene)){
      
      rv$alleles <-   data$functional_alleles %>% 
        filter(gene == input$gene) %>% 
        right_join(distinct(rv$raw_data_for_allele_picker, Allele), by = "Allele") %>% 
        mutate(functional_status = ifelse(is.na(functional_status), 
                                          "unknown function", functional_status)) %>% 
        split(.$functional_status) %>% 
        map(pull, Allele) %>% 
        map(as.list)
    }
    updatePickerInput(session, "allele", choices =  rv$alleles) 
    freezeReactiveValue(input, "allele")
  })  

  # summarized data ---------------------------------------------------------
  observe({
    req(input$gene)
    req(input$allele)
    req(input$mapping_method)
    rv$summarized_data <- switch(input$mapping_method,
                                 "Countries" = data$freq_per_country,
                                 "Geographical groups"  = data$freq_per_subgroup) %>% 
      filter(gene == input$gene) %>% 
      filter(Allele == input$allele) 
  })
  
  
  
  output$DT_table <- DT::renderDT({
    req(input$gene)
    req(input$allele)
    req(input$mapping_method)
    
    drop_colums <- switch(input$average,
                          "Median" = "Weighted",
                          "Weighted median"  = "Frequency")
    
    
    rv$summed_data <- switch(input$mapping_method,
                             "Countries" =  rv$summarized_data %>% 
                               select(Country = country, 
                                      Subgroup =contains("subgroup"), 
                                      `Frequency [Median]` = Frequency, 
                                      `Frequency CI [95%]`= Frequency_CI,
                                      `Weighted frequency [Median]` = Weighted, 
                                      `Weighted median absolute Deviation` = Weighted_MAD,
                                      `Number of cohorts` = Number_of_cohorts,
                                      `Aggregated sampe size*`= Aggregated_sample_size,
                                      `Studies [PMID]`= Studies)  %>% 
                               select(-starts_with(drop_colums)),
                             "Geographical groups" = rv$summarized_data %>% 
                               select(
                                 Subgroup =contains("subgroup"), 
                                 `Frequency [Median]` = Frequency, 
                                 `Frequency CI [95%]`= Frequency_CI,
                                 `Weighted frequency [Median]` = Weighted, 
                                 `Weighted median absolute deviation` = Weighted_MAD,
                                 `Number of cohorts` = Number_of_cohorts,
                                 `Aggregated sample size*`= Aggregated_sample_size,
                                 `Studies [PMID]`= Studies)  %>% 
                               select(-starts_with(drop_colums)))
    
    rv$summed_data %>%
      select(-contains("PMID")) %>%
      DT::datatable(.,extensions = c('Buttons'),
                rownames = FALSE,
                escape = FALSE, 
                filter =  list(position = 'top', clear = TRUE),
                selection = 'single',
                callback = JS("
var tips = ['', '', '','', 'Please note, that some studies used the same or a subset of samples of other studies.'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);}"),
                options =  list(pageLength = 25, scrollX=TRUE, scrollCollapse=F,
                                search = list(regex = TRUE),
                                # columnDefs = list(list(width = '200px', targets = 4)),
                                dom = 'Bfrtip',
                                buttons =  list(list(extend = "copy", title = NULL),
                                                list(extend = "excel", title = glue("{input$gene}_{input$allele}_{Sys.Date()}")),
                                                list(extend = "pdf",   title = glue("{input$gene}_{input$allele}_{Sys.Date()}")))
                ))
  },server = FALSE)
 
  # Avoid double clicks 
  debounced_click_dt <- debounce(reactive(input$DT_table_row_last_clicked), 500)
  
  observeEvent(debounced_click_dt(), {
    req(input$gene)
    req(input$allele)
    
    DT_table_row_last_clicked <- debounced_click_dt()
    
    
    gr_pmid <-  rv$summed_data  %>%
      slice(DT_table_row_last_clicked) %>% 
      pull(`Studies [PMID]`) %>%
      str_split(., ", ") %>% unlist 
    
    gr_country <-  switch(input$mapping_method,
                          "Countries"= { rv$summed_data  %>%
                              slice(DT_table_row_last_clicked) %>% 
                              pull(Country)},
                          "Geographical groups" = rv$summed_data  %>%
                            slice(DT_table_row_last_clicked) %>% 
                            pull(Subgroup))
    
    
    output$raw_data_sweet <- DT::renderDT({
      req(input$gene)
      req(input$allele)
      switch(input$mapping_method, 
             "Countries"= rv$raw_data %>%
               filter(Allele == input$allele) %>%
               filter(pmid %in% gr_pmid) %>% 
               filter(country  %in% gr_country),
             "Geographical groups" = rv$raw_data %>%
               filter(Allele == input$allele) %>%
               filter(pmid %in% gr_pmid) %>%
               filter(subgroup  %in% gr_country)) %>%
        mutate(Freq = round(Freq, 3 )) %>% 
        select(Country=country, Subgroup=subgroup, Gene=gene, Allele, Frequency=Freq,`Cohort size`= n, `Studies [PMID]`=pmid)%>%
        mutate(`Studies [PMID]`=make_pmid_link(`Studies [PMID]`)) %>% 
        DT::datatable(.,escape = F, rownames = F,
                  selection = "none",
                  options = list(dom="plt", paging = T, scrollX=TRUE, scrollCollapse=F,
                                 pageLength = 5, lengthMenu = c(5,10,20),
                                 columnDefs = list(list(className = 'dt-left', targets = "_all")))
        )
    }, server=F)
    
    shinyalert::shinyalert(
      title = paste0("Raw data for ", input$gene," ",input$allele),
      text = tagList(DT::DTOutput("raw_data_sweet")),
      size = "l", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "",
      showConfirmButton = F,
      showCancelButton = T,
      cancelButtonText = "Close",
      animation = TRUE
    )
    
  })
  
  debounced_click <- debounce(reactive(input$leaflet_base_shape_click), 500)
  
  
  observeEvent( debounced_click(), {
        p <- debounced_click()
    output$raw_data_sweet2 <- DT::renderDT({
      req(input$gene)
      req(input$allele)
      
      
      switch(input$mapping_method,
             "Countries"= rv$raw_data %>%
               filter(Allele == input$allele) %>%
               filter(ISO2 %in% p),
             "Geographical groups" = rv$raw_data %>%
               filter(Allele == input$allele) %>%
               filter(subgroup %in% p)) %>%
        mutate(Freq = round(Freq, 3 )) %>% 
        select(Country=country, Subgroup=subgroup, Gene=gene, Allele, Frequency=Freq,`Cohort size`= n, `Studies [PMID]`=pmid)%>%
        mutate(`Studies [PMID]`=make_pmid_link(`Studies [PMID]`)) %>%
        DT::datatable(.,escape = F, rownames = F,
                  selection = "none",
                  options = list(dom="plt", paging = T, scrollX=TRUE, scrollCollapse=F,
                                 pageLength = 5, lengthMenu = c(5,10,20),
                                 columnDefs = list(list(className = 'dt-left', targets = "_all")))
                  )
        
    }, server=F)
    
    shinyalert::shinyalert(
      title = paste0("Raw data for ", input$gene," ",input$allele),
      text = tagList(DT::DTOutput("raw_data_sweet2")),
      size = "l", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "",
      showConfirmButton = F,
      showCancelButton = T,
      cancelButtonText = "Close",
      animation = TRUE
    )
  })
  

  observe({
    req(input$gene)
    req(input$allele)
    req(input$mapping_method)
    
    drop_colums <- switch(input$average,
                          "Median" = "Weighted",
                          "Weighted median"  = "Frequency")
    
    
    rv$plot_data <-  switch(input$mapping_method,
                            "Countries" =  rv$summarized_data %>%
                              select(-starts_with(drop_colums)) %>%
                              select(Frequency = matches("Frequency\\b|Weighted\\b"),
                                     Number_of_cohorts,
                                     Aggregated_sample_size,
                                     ISO2,
                                     country) %>%
                              mutate(pop.html.tooltip = paste0("<p align='left'>Frequency:&nbsp;",Frequency*100,
                                                               "<br>Studies:&nbsp",Number_of_cohorts,"</p>")),
                            "Geographical groups"  = rv$summarized_data %>%
                              select(-starts_with(drop_colums)) %>%
                              select(Frequency = matches("Frequency\\b|Weighted\\b"),
                                     Number_of_cohorts,subgroup,
                                     Aggregated_sample_size,
                                     ISO2,  
                                     country) %>%
                              mutate(pop.html.tooltip = paste0("<p align='left'>Frequency:&nbsp;",Frequency*100,
                                                               "<br>Studies:&nbsp",Number_of_cohorts,
                                                               "<br>Studied populations:&nbsp",country,"</p>"))
    ) # end switch
    
  })
  
# leaflet  ----------------------------------------------------------------
  map_reactive <- reactive({
    leaflet_map 
  })
    
    
  
  # Base leaflet
  output$leaflet_base <- renderLeaflet({
    map_reactive()
    })
  
  
  # data
  leaflet_freq_data <- reactive({
    req(input$gene)
    req(input$allele)
    req(input$average)
    req(input$mapping_method)
    dt_leaf <- rv$plot_data
    validate(need(n_distinct(dt_leaf)>0, "No data available"))
    switch (input$mapping_method,
            "Countries" = {
              # make subset
              leaflet_data <- subset(myspdf, myspdf@data$ISO2 %in% dt_leaf$ISO2)
              # join data
              leaflet_data_merge <- left_join(leaflet_data@data ,  dt_leaf, by = "ISO2")
              # add to data
              leaflet_data@data$TEST <- leaflet_data_merge$ISO2
              # validate merging
              validate(need(all(leaflet_data@data$TEST == leaflet_data@data$ISO2), label="matching problem"))
              leaflet_data@data$Frequency <- leaflet_data_merge$Frequency*100
              leaflet_data@data$Number_of_cohorts  <- leaflet_data_merge$Number_of_cohorts
              leaflet_data@data$Aggregated_sample_size  <- leaflet_data_merge$Aggregated_sample_size
              leaflet_data
            }, 
            "Geographical groups" = {
              # subgroup plot
              
              if(n_distinct(dt_leaf$subgroup)>1){
                tmp_fin <- raster::bind(myspdf_regions[dt_leaf$subgroup])
              }
              
              if(n_distinct(dt_leaf$subgroup)==1){
                tmp_fin <-myspdf_regions[[dt_leaf$subgroup]]
              }
              
              tmp_fin$NAMES <- names(myspdf_regions[dt_leaf$subgroup])
              tmp_fin$Frequency <- dt_leaf$Frequency*100
              tmp_fin$Number_of_cohorts <- dt_leaf$Number_of_cohorts
              tmp_fin$Aggregated_sample_size <- dt_leaf$Aggregated_sample_size
              tmp_fin$country_old <- str_wrap(paste0("<br>",dt_leaf$country), 40) %>% gsub("\\\n","<br>", .) 
              tmp_fin
            })
  })
  
  
    
  observe({
    req(input$gene)
    req(input$allele)
    req(input$average)
    req(input$mapping_method)

    if(input$mapping_method == "Countries"){

          leaflet_data <- leaflet_freq_data()

          labels <- glue("<p align='left'><strong>{leaflet_data$NAME}</strong><br/>
                      Frequency: {leaflet_data$Frequency}%<br/>
                      Studies: {leaflet_data$Number_of_cohorts }<br/>
                      Aggregated sample number (duplicated samples possible):{leaflet_data$Aggregated_sample_size }<br/>
                     <br/><i>Click on a country to load raw data<i/><p/>") %>% lapply(htmltools::HTML)

          scale_values <- c(0, unique(leaflet_data$Frequency))
          pal <- colorNumeric("viridis", scale_values)
          pal_rev <- colorNumeric("viridis", scale_values, reverse = T)

        leafletProxy("leaflet_base") %>%
             clearShapes() %>%
              clearControls() %>%
            addPolygons(data=leaflet_data,
                        layerId = ~ISO2,
                        fillColor = ~pal(Frequency), color =  "",
                        smoothFactor = 0.01, fillOpacity = 0.8,
                        label=labels,
                        highlight = highlightOptions(weight = 1,
                                                     fillOpacity = 1,
                                                     bringToFront = TRUE)) %>%
            addLegend("bottomright", pal = pal_rev, values = scale_values,
                      title = "Frequency",
                      labFormat = labelFormat(suffix = "%",
                                              transform = function(x) sort(x, decreasing = T)),
                      opacity = 1) 
        }else{

                                                          tmp_fin <- leaflet_freq_data()
                                                          labels <- glue("<p align='left'><strong>{tmp_fin$NAMES}</strong><br/>Frequency:
                                      {tmp_fin$Frequency}%<br/>Studies: {tmp_fin$Number_of_cohorts }<br/>
                                      Aggregated sample number (duplicated samples possible):{tmp_fin$Aggregated_sample_size }<br/>
                                      Studied populations: {tmp_fin$country_old}<br/>
                                      <br/><i>Click on a region to load raw data<i/><p/>")%>% lapply(htmltools::HTML)
                                                          scale_values <- c(0, unique(tmp_fin$Frequency))
                                                          pal <- colorNumeric("viridis", scale_values)
                                                          pal_rev <- colorNumeric("viridis", scale_values, reverse = T)
                                                          
                                                leafletProxy("leaflet_base") %>%
                                                            clearShapes() %>%
                                                            clearControls() %>%
                                                            addPolygons(data=tmp_fin,
                                                                        fillColor = ~pal(Frequency),  fillOpacity = 0.8,
                                                                        label=labels,
                                                                        layerId = ~NAMES,
                                                                        highlight = highlightOptions(weight = 1,
                                                                                                     fillOpacity = 1,
                                                                                                     bringToFront = TRUE),
                                                                        color =  "",
                                                                        smoothFactor = 0.01)  %>%
                                                            addLegend("bottomright", pal = pal_rev, values = scale_values,
                                                                      title = "Frequency",
                                                                      labFormat = labelFormat(suffix = "%",
                                                                                              transform = function(x) sort(x, decreasing = T)),
                                                                      opacity = 1
                                                            )  
        }
          })
      
      
      

# leaflet download handler ------------------------------------------------
  
output$dl_leaflet <- downloadHandler(
    filename = function() {paste0(input$gene,gsub("[*]","_",input$allele),".png")},
    # filename ="test.png",
   
    
    content = function(file) {
      showNotification("Be patient. Plot is rendered for download.", duration  = 5, type = "message", id="note1")
      
      if(input$mapping_method == "Countries"){
        
        
        leaflet_data <- leaflet_freq_data()
    
        labels <- glue("<p align='left'><strong>{leaflet_data$NAME}</strong><br/>
                      Frequency: {leaflet_data$Frequency}%<br/>
                      Studies: {leaflet_data$Number_of_cohorts }<br/>
                      Aggregated sample number (duplicated samples possible):{leaflet_data$Aggregated_sample_size }<br/>
                     <br/><i>Click on a country to load raw data<i/><p/>") %>% lapply(htmltools::HTML)
        
        scale_values <- c(0, unique(leaflet_data$Frequency))
        pal <- colorNumeric("viridis", scale_values)
        pal_rev <- colorNumeric("viridis", scale_values, reverse = T)
        
      map_to_plot <-   map_reactive() %>%
          addPolygons(data=leaflet_data,
                      layerId = ~ISO2,
                      fillColor = ~pal(Frequency), color =  "",
                      smoothFactor = 0.01, fillOpacity = 0.8,
                      label=labels,
                      highlight = highlightOptions(weight = 1,
                                                   fillOpacity = 1,
                                                   bringToFront = TRUE)) %>%
          addLegend("bottomright", pal = pal_rev, values = scale_values,
                    title = "Frequency",
                    labFormat = labelFormat(suffix = "%",
                                            transform = function(x) sort(x, decreasing = T)),
                    opacity = 1)
      }else{
        
        tmp_fin <- leaflet_freq_data()
        labels <- glue("<p align='left'><strong>{tmp_fin$NAMES}</strong><br/>Frequency:
                                      {tmp_fin$Frequency}%<br/>Studies: {tmp_fin$Number_of_cohorts }<br/>
                                      Aggregated sample number (duplicated samples possible):{tmp_fin$Aggregated_sample_size }<br/>
                                      Studied populations: {tmp_fin$country_old}<br/>
                                      <br/><i>Click on a region to load raw data<i/><p/>")%>% lapply(htmltools::HTML)
        scale_values <- c(0, unique(tmp_fin$Frequency))
        pal <- colorNumeric("viridis", scale_values)
        pal_rev <- colorNumeric("viridis", scale_values, reverse = T)
        map_to_plot <-   map_reactive() %>%
          addPolygons(data=tmp_fin,
                      fillColor = ~pal(Frequency),  fillOpacity = 0.8,
                      label=labels,
                      layerId = ~NAMES,
                      highlight = highlightOptions(weight = 1,
                                                   fillOpacity = 1,
                                                   bringToFront = TRUE),
                      color =  "",
                      smoothFactor = 0.01)  %>%
          addLegend("bottomright", pal = pal_rev, values = scale_values,
                    title = "Frequency",
                    labFormat = labelFormat(suffix = "%",
                                            transform = function(x) sort(x, decreasing = T)),
                    opacity = 1
          )
        
      }
      

      mapview::mapshot(
               x = map_to_plot,
               file = file,
               selfcontained = FALSE
               )
      removeNotification("note1")
    })
  

  
  
# Rendering leaflet -------------------------------------------------------
  output$render_plots <- renderUI({

      list( fluidRow(column(width = 12,align="center", leafletOutput("leaflet_base", height = "60vh", width= "auto") %>% withSpinner(color  = "steelblue"))),
                  br(),
                  downloadButton(outputId = "dl_leaflet", label = "Screenshot", icon = icon("camera")),
                  br()
      )
  })
  
  
  
  output$rrender_add_info <- renderUI({
    req(input$gene)
    req(input$allele)
    req(input$average)
    req(input$type_of_plot)
    
    hap_lgl <- nrow(rv$haplotype_allele)>0
    status_lgl <- nrow(rv$function_allele)>0  
    
    
    if(hap_lgl){
      
      b_hap <- argonCard(
        width = 5, background_color  = "primary",
        argonTextColor(h2("Variants of the star allele"), color = "white"),
        DTOutput("haplotype_allele"), 
        br(),
        div(glue("There {ifelse(n_distinct(rv$haplotype_allele$rs_id)==1, paste('is 1 core variant'), paste('are', n_distinct(rv$haplotype_allele$rs_id), ' core variants'))} in the haplotype
             according to"),
        tags$a(href=paste0("https://www.pharmvar.org/gene/", input$gene), 
               "pharmvar.",style="color: white; text-decoration: underline;",target ="_blank")) %>% argonTextColor(color = "white")
      )
      
    }
    
    if(status_lgl){
      b_status <- argonCard(width = 5, 
                            background_color = rv$function_allele$status[1],
                      
                      argonTextColor(h2(glue("Functional status - {rv$function_allele$functional_status}")) , color = "white"),     
                      argonTextColor(div(glue("{ifelse(is.na(rv$function_allele$activity_value), 'Activity score not yet available', paste0('The activity score is ',rv$function_allele$activity_value) )}."),
                      br(),
                      glue("The clinical consequence of the starallele {paste0(input$gene,input$allele)} is extracted from"),
                      tags$a(href=paste0("https://www.pharmgkb.org/page/",tolower(input$gene),"RefMaterials"),
                             tags$img(src = "https://raw.githubusercontent.com/PharmGKB/pgkb-branding/main/logo.svg", height = "20px"),
                             target="_blank")), color = "white")
      ) # end of box
      
    }

    if (hap_lgl & status_lgl)     {list(br(), fluidRow(b_status, b_hap))}
    else if (!hap_lgl & status_lgl) {list(br(), fluidRow(b_status))}
    else if (hap_lgl & !status_lgl) {list(br(), fluidRow(b_hap))}
    
    
  })
  
  observe({
    req(input$gene)
    req(input$allele)
    req(input$average)
    req(input$type_of_plot)
    rv$haplotype_allele <-  data$haplotype_alleles %>% 
      filter(gene == input$gene) %>% 
      filter(Allele == input$allele)
  })
  
  observe({
    req(input$gene)
    req(input$allele)
    req(input$average)
    req(input$type_of_plot)
    rv$function_allele <-  data$functional_alleles %>% 
      filter(gene == input$gene) %>% 
      filter(Allele == input$allele)
  })
  
  output$haplotype_allele <- DT::renderDT({
    req(input$gene)
    req(input$allele)
    req(input$average)
    req(input$type_of_plot)
    
    if(nrow(rv$haplotype_allele)>0){
      
      rv$haplotype_allele %>%
        mutate(dbSNP= glue('<a style="color: white; text-decoration: underline;" href="https://www.ncbi.nlm.nih.gov/snp/{rs_id}" target="_blank">{rs_id}</a>')) %>%
        select(dbSNP, `Position (hg19)`=variant_start,`Ocurrence in star alleles`=other_alleles) %>% 
        DT::datatable(., extensions = 'Responsive', rownames = F, escape = F, filter = "none", selection = "none", 
                  options = list(paging = FALSE, lengthChange = FALSE, searching=F,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#F0000000','color': 'white'});",
                                   "}"),
                                 dom = 't'
                  ))  %>% 
         DT::formatStyle(columns = 0:2, color="white")
    }
  })
  
  observeEvent(input$imprint,{
    sendSweetAlert(
      session = session,
      title = "",
      text = includeMarkdown("imprint.md"),
      html = TRUE,
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE,
      width= '900px',
      height = "auto")
  })    

# tools -------------------------------------------------------------------
# tool 1 ------------------------------------------------------------------
  observe({
   
    req(input$tools_act_mapping_method)
    switch (input$tools_act_mapping_method,
            "Countries" = {updatePickerInput(session, "tools_act_countries", choices = sort(unique(data$freq_per_country$country)))},
            "Geographical groups" = {updatePickerInput(session, "tools_act_countries", choices = sort(unique(data$freq_per_subgroup$subgroup)))}
    )
    rv$tool_act_plot_data_raw <-  switch (input$tools_act_mapping_method,
                                          "Countries" = {data$freq_per_country %>% select(region = country,gene, Allele, Weighted)},
                                          "Geographical groups" = {data$freq_per_subgroup %>% select(region = subgroup, gene, Allele, Weighted)}
    )
    
   })
  
  observe({
    req(input$tools_act_method)
    req(input$tools_act_countries)
    req(input$tools_act_mapping_method)
    req(rv$tool_act_plot_data_raw)
    
    
    dunnernberger_all <- str_split(input$tools_act_countries,",") %>% unlist %>% map_dfr(~mutate(dunnenberger, region = .))
    pharmvar_all <- str_split(input$tools_act_countries,",") %>% unlist %>% map_dfr(~mutate( pharmvar, region = .))
    
    
    rv$tool_act_plot_data <-  switch (input$tools_act_method,
                                      "Dunnenberger (2015)" = {
                                        rv$tool_act_plot_data_raw %>% 
                                          as_tibble() %>% 
                                          filter(region %in% str_split(input$tools_act_countries,",") %>% unlist) %>% 
                                          right_join(dunnernberger_all,by = join_by(region, gene, Allele)) 
                                      },
                                      "CPIC function" = {
                                        rv$tool_act_plot_data_raw %>% 
                                          as_tibble() %>% 
                                          filter(region %in% str_split(input$tools_act_countries,",") %>% unlist) %>% 
                                          right_join(pharmvar_all,by = join_by(region, gene, Allele)) 
                                        
                                      }) %>% 
      mutate(gr = is.na(Weighted)) %>% 
      split(.$gr) %>% 
      map_dfr(~group_by(., gene, region) %>% 
                summarise(freq =  sum(Weighted),
                          Allele = toString(unique(gtools::mixedsort(Allele)))) %>% 
                ungroup())
    
  })  
    
  
  
  
 output$tool_act_plot <- renderPlotly({
   req(input$tools_act_method)
   req(input$tools_act_countries)
   req(input$tools_act_mapping_method)
   req(rv$tool_act_plot_data)
   
   pp <- rv$tool_act_plot_data %>% 
     filter(!is.na(freq)) %>% 
     ggplot(aes(gene, freq, fill =region, text = paste0("Country/Region: ",region,"\nGene: ", gene, "\nSummarized weighted frequency: ", round(freq*100,2), "%")))  + 
     geom_col(position = position_dodge2(preserve = "single")) + 
     scale_y_continuous("", labels = scales::percent)+
     scale_x_discrete("High-risk genotype according to gene") + 
     scale_fill_viridis_d("Region") +
     theme_bw(base_size = 16) +  
     theme(legend.position = "bottom")
   
   ggplotly(pp,tooltip = "text")
   
   
 }) 
  
 output$tool_act_table <- renderDT({
   req(input$tools_act_method)
   req(input$tools_act_countries)
   req(input$tools_act_mapping_method)
   
     rv$tool_act_plot_data %>% 
     filter(!is.na(freq)) %>% 
     full_join(
       rv$tool_act_plot_data %>% 
         filter(is.na(freq)) %>% 
         select(gene, region, "Alleles without data"  = Allele), by = join_by(gene, region)) %>% 
      mutate(across(c(gene, region), as.factor)) %>% 
     select(`Country/Region` = region , Gene = gene, `Summarized alleles` = Allele, `Summarized weighted frequency` = freq, everything()) %>% 
     DT::datatable(rownames = F,
                   selection = "none", filter =  list(position = 'top', clear = TRUE),
                   options = list(dom="lftp", scrollX=TRUE))
 })
 output$tool_act_text <- renderText({
   req(input$tools_act_method)
   req(input$tools_act_countries)
   req(input$tools_act_mapping_method)
   # aa <<- rv$tool_act_plot_data
   switch (input$tools_act_method,
           "Dunnenberger (2015)" = {paste("The proportion of individuals expected to carry a high-risk genotype for",n_distinct(rv$tool_act_plot_data$gene) ,  "genes identified by the 
         Clinical Pharmacogenetics Implementation Consortium (CPIC) as having at least one known, actionable inherited variant. 
         The high-risk diplotype frequencies were estimated according (Dunnenberger et al., 2015). For",rv$tool_act_plot_data %>%
           filter(is.na(freq)) %>% pull(gene) %>% n_distinct(),"genes no allele frequency data was available in selected countries/regions as shown in the table below. 
          You can resize the plot using your mouse by dragging the right corner of the plot.")},
           "CPIC function" = {
             paste("The proportion of individuals expected to carry a high-risk genotype for",n_distinct(rv$tool_act_plot_data$gene) ,  "genes identified by the 
         Clinical Pharmacogenetics Implementation Consortium (CPIC®) resource as having at least one known, actionable inherited variant. For",rv$tool_act_plot_data %>%
                     filter(is.na(freq)) %>% pull(gene) %>% n_distinct(),"genes no allele frequency data was available in selected countries/regions as shown in the table below.
                   You can resize the plot using your mouse by dragging the right corner of the plot.")
             
           }
           )
 })
   

# tool 2 tool2------------------------------------------------------------------
 observe({
   req(input$tools_freq_gene)
   
   tmp_choices <- data$freq_per_country %>% 
     filter(gene== input$tools_freq_gene) %>% 
     distinct(Allele) %>% pull(Allele) %>%  gtools::mixedsort()
  
   if(input$tools_freq_gene %in% unique(data$functional_alleles$gene)){
     
       tmp_choices <- data$freq_per_country %>% 
                        filter(gene==input$tools_freq_gene) %>%  
                        distinct(gene, Allele) %>% 
                        left_join( data$functional_alleles,by = join_by(gene, Allele)) %>% 
         mutate(functional_status = ifelse(is.na(functional_status), 
                                           "unknown function", functional_status)) %>% 
         slice(gtools::mixedorder(Allele))
     
   updatePickerInput(session, "tools_freq_allele", choices = tmp_choices$Allele, choicesOpt = list(subtext = tmp_choices$functional_status))
     }else{
     
   updatePickerInput(session, "tools_freq_allele", choices = tmp_choices)
   }
   
   freezeReactiveValue(input, "tools_freq_allele")
 })
   
 observe({
   req(input$tools_freq_countries)
   req(input$tools_freq_gene)
   req(input$tools_freq_allele)

   rv$tools_freq_data <- data$freq_per_country %>% 
     as_tibble() %>% 
     filter(gene==input$tools_freq_gene) %>% 
     filter(Allele %in% input$tools_freq_allele ) %>% 
     group_by(country, ISO2) %>% 
     summarise(Weighted =sum(Weighted , na.rm = T)) %>% 
     ungroup() %>% 
     left_join(data$mapping_country_subgroup,
               by = join_by(ISO2)) %>% 
     filter(subgroup %in% input$tools_freq_countries)
   
 })
 
 
 
 output$tool_freq_plot <- renderPlotly({
   req(input$tools_freq_countries)
   req(input$tools_freq_gene)
   req(input$tools_freq_allele)
   
 
   pp <- rv$tools_freq_data  %>% 
     ggplot(aes(reorder(subgroup, Weighted, median),
                text =  paste0("Country: ",country, "\nWeighted frequency: ", round(Weighted*100,2), "%"),
                Weighted))+
     geom_boxplot(alpha=0.6) +
     ggbeeswarm::geom_quasirandom(aes(color=subgroup))+
     scale_y_continuous(filter(CMS_text, what=="Tool2")$text, labels = scales::percent,  limits = c(0, max(rv$tools_freq_data$Weighted)+.01))+
     scale_x_discrete("", labels = ~str_wrap(.,10)) +
     scale_fill_viridis_d("Biogeographic subgroups") +
     ggtitle("",subtitle = title)  +
     theme_bw(base_size = 16) +
     theme(legend.position = "bottom")
   ggplotly(pp, tooltip = "text") %>% remove_boxplot_outliers %>% plotly::layout(showlegend=F, yaxis=list(autorange="min"))
   
 })
   
 output$tools_freq_text <- renderText({
   req(input$tools_freq_countries)
   req(input$tools_freq_gene)
   req(input$tools_freq_allele)
   paste0(input$tools_freq_gene," allele frequency is summarized for the alleles: ", toString(input$tools_freq_allele))
 })
 
 # tool 3 frequency variability ---------------------------------------------------
 
 observe({
   req(input$tools_var_gene)
  
   tmp_choices <- data$freq_per_country %>% 
     filter(gene==input$tools_var_gene) %>% 
     distinct(Allele) %>% pull(Allele) %>% gtools::mixedsort()

   if(input$tools_var_gene %in% unique(data$functional_alleles$gene)){
     
     tmp_choices <- data$freq_per_country %>% 
       filter(gene==input$tools_var_gene) %>%  
       distinct(gene, Allele) %>% 
       left_join( data$functional_alleles, by = join_by(gene, Allele)) %>% 
       mutate(functional_status = ifelse(is.na(functional_status), 
                                         "unknown function", functional_status)) %>% 
       slice(gtools::mixedorder(Allele))
     
     updatePickerInput(session, "tools_var_allele", choices = tmp_choices$Allele, choicesOpt = list(subtext = tmp_choices$functional_status))
   }else{
     
     updatePickerInput(session, "tools_var_allele", choices = tmp_choices)
   }
   
   freezeReactiveValue(input, "tools_var_allele")
 })
   
 observe({
   req(input$tools_var_gene)
   req(input$tools_var_allele)
   
     tmp <-  data$freq_per_country %>% 
     as_tibble() %>%
     filter(gene==input$tools_var_gene) %>%
     filter(Allele %in% input$tools_var_allele) %>%
     group_by(gene, country, ISO2) %>% 
     summarise(Weighted =sum(Weighted , na.rm = T),
               Allele = toString(gtools::mixedsort(unique(Allele)))) %>% 
     ungroup() %>% 
     left_join(data$mapping_country_subgroup,by = join_by(ISO2)) %>%
     group_split(subgroup, gene)  %>%
     keep(~nrow(.)>=2) 
   
   
     if(length(tmp)>0){
     rv$freq_variability_server <- tmp %>% 
     map(~rowwise(.) %>% mutate(max_dif = max(abs(Weighted  - .$Weighted )),
                                max_country = .$country[which.max(abs(Weighted  - .$Weighted))])) %>%
     bind_rows() %>%
     select(country, ISO2, subgroup, gene, Allele, Weighted,max_dif) %>% 
     ungroup()
     }else{
       showNotification("Not enough data available")
       rv$freq_variability_server <- NULL
     }
 })
   
 observe({
   req(input$tools_var_gene)
   req(input$tools_var_allele)
   req(rv$freq_variability_server)
 
    tmp <- rv$freq_variability_server  %>% 
      group_by(subgroup) %>%
     filter(Weighted ==min(Weighted )| Weighted  == max(Weighted )) %>%
     mutate(gr = ifelse(Weighted ==min(Weighted ), "min","max")) %>% 
     group_by(subgroup, gr) %>% 
     slice(1) %>% 
     group_by(subgroup) %>% 
     filter(n_distinct(gr)==2) %>% 
     ungroup() %>% 
     select(subgroup, gr, Weighted, max_dif) %>% 
     pivot_wider(names_from = gr, values_from = Weighted ) 

     if(nrow(tmp)>0){
     rv$df_segment <- tmp %>% mutate(middle = min + ((max-min)/2))
     }else{
       rv$df_segment <- NULL
     }
 })
 

 output$tool_var_plot <- renderPlotly({
   req(input$tools_var_gene)
   req(input$tools_var_allele)
   req(rv$freq_variability_server)
 
   
 if(is.null(rv$df_segment)){

     pp <-    rv$freq_variability_server %>% 
     ggplot(aes(x=Weighted,y= subgroup)) +
     ggbeeswarm::geom_quasirandom(orientation = "y",aes(color=subgroup, text =paste0("Country: ",country,"\nWeighted frequency: ", round(Weighted*100,2), "%")))+
     geom_text(data=. %>% group_by(subgroup) %>% 
                 mutate(min_max =case_when(min(Weighted) == Weighted ~"min",max(Weighted) == Weighted ~ "max", T ~ "no")) %>% 
                 group_by(subgroup, min_max) %>% 
                 slice(1) %>% ungroup %>% 
                 filter(min_max != "no"),
               aes(label = paste(country))) +
     scale_x_continuous("", labels = scales::percent, limits = c(0, NA))+
     scale_y_discrete("")+
     theme_bw(base_size = 16) +
     theme(legend.position = "bottom")
 }else{

     pp <-    rv$freq_variability_server %>%
     ggplot(aes(x=Weighted,y= subgroup)) +
     geom_segment(data = rv$df_segment, aes(x = min, xend=max, y=subgroup, yend=subgroup)) +
     ggbeeswarm::geom_quasirandom(orientation = "y", aes(color=subgroup, text =paste0("Country: ",country,"\nWeighted frequency: ", round(Weighted*100,2), "%")))+
     geom_text(data = rv$df_segment,nudge_y =0.08, aes(x=middle, y= subgroup, label = paste0(round(max_dif*100,2),"%"))) +
     geom_text(data=. %>% group_by(subgroup) %>% 
                 mutate(min_max =case_when(min(Weighted) == Weighted ~"min",max(Weighted) == Weighted ~ "max", T ~ "no")) %>% 
                 group_by(subgroup, min_max) %>% 
                 slice(1) %>% ungroup %>% 
                 filter(min_max != "no"),
               aes(label = paste(country)), position = position_jitter(width = 0, height = 0.2)) +
     scale_x_continuous(filter(CMS_text, what=="Tool3")$text, labels = scales::percent)+
     scale_y_discrete("")+
     theme_bw(base_size = 16) +
     theme(legend.position = "bottom")
 }
     
 

  ggplotly(pp, tooltip = "text") %>% plotly::layout(showlegend=F)
   
   })
 
 output$tool_var_text <- renderText({
   req(input$tools_var_gene)
   req(input$tools_var_allele)
   req(rv$freq_variability_server)
   
   "The plot shows the maximum frequency difference in the respective biogeographic subgroups. Gene and allele selection, as well as data are shown only when enough studies are available."
 })

}