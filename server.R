function(input, output, session) {
  enableBookmarking("url")
  showLog()
  options(scipen = 999)
  observe_helpers()

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

    rv$raw_data_for_allele_picker <- switch(input$mapping_method,
      "Countries" = data$freq_per_country,
      "Geographical groups" = data$freq_per_subgroup
    ) %>%
      filter(gene == input$gene)

    if (input$gene %in% c(unique(data$functional_alleles$gene))) {
      alleles_sorted <- data$functional_alleles %>%
        filter(gene == input$gene) %>%
        right_join(distinct(rv$raw_data_for_allele_picker, Allele), by = "Allele") %>%
        mutate(functional_status = ifelse(is.na(functional_status),
          "unknown function", functional_status
        )) %>%
        left_join(rv$raw_data_for_allele_picker %>% separate_rows(Studies, sep = ", ") %>%
          distinct(Allele, Studies) %>% count(Allele), by = join_by(Allele)) %>%
        mutate(n = ifelse(is.na(n), 0, n))

      if (input$gene == "CYP4F2") {
        alleles_sorted <- alleles_sorted %>% slice(mixedorder(Allele))
      }

      if (input$gene == "DPYD") {
        alleles_sorted <- alleles_sorted %>% slice(alleles_sorted %>% pull(Allele) %>% foo_allele_sort(., PATTERN = "Ref|2A)|13)|Hap"))
      }

      if (input$gene == "G6PD") {
        alleles_sorted <- alleles_sorted %>% slice(alleles_sorted %>% pull(Allele) %>% foo_allele_sort(., PATTERN = "reference"))
      }

      if (input$gene == "MT-RNR1") {
        alleles_sorted <- alleles_sorted %>% slice(alleles_sorted %>% pull(Allele) %>% foo_allele_sort(., PATTERN = "[*]1"))
      }
      updatePickerInput(session, "allele", choices = alleles_sorted$Allele, choicesOpt = list(subtext = paste0(alleles_sorted$functional_status, " (n = ", alleles_sorted$n, ")")))
    } else {
      alleles_sorted <- rv$raw_data_for_allele_picker %>%
        separate_rows(Studies, sep = ", ") %>%
        distinct(Allele, Studies) %>%
        count(Allele)
      alleles_sorted <- alleles_sorted[mixedorder(alleles_sorted$Allele), ]
      updatePickerInput(session, "allele", choices = alleles_sorted$Allele, choicesOpt = list(subtext = paste0("(n = ", alleles_sorted$n, ")")))
    }
    freezeReactiveValue(input, "allele")
  })

  observe({
    req(input$gene)
    req(input$allele)
    req(input$mapping_method)
    rv$summarized_data <- switch(input$mapping_method,
      "Countries" = data$freq_per_country,
      "Geographical groups" = data$freq_per_subgroup
    ) %>%
      filter(gene == input$gene) %>%
      filter(Allele == input$allele)
  })

  output$DT_table <- DT::renderDT({
      req(input$gene)
      req(input$allele)
      req(input$mapping_method)

      drop_colums <- switch(input$average,
        "Median" = "Weighted",
        "Weighted median" = "Frequency"
      )
      
      rv$summed_data <- switch(input$mapping_method,
        "Countries" = rv$summarized_data %>%
          mutate(Studies = make_pmid_link(Studies)) %>%
          mutate(across(c(Frequency, Weighted, Weighted_MAD), ~ ifelse(. > 0.001, round(., 3), round(., 5)))) %>%
          arrange(country) %>%
          select(
            Country = country,
            Subgroup = contains("subgroup"),
            `Frequency [Median]` = Frequency,
            `Frequency CI [95%]` = Frequency_CI,
            `Weighted frequency [Median]` = Weighted,
            `Weighted median absolute Deviation` = Weighted_MAD,
            `Number of cohorts` = Number_of_cohorts,
            `Aggregated sampe size*` = Aggregated_sample_size,
            `Studies [PMID]` = Studies
          ) %>%
          select(-starts_with(drop_colums)) %>%
          mutate(Country = factor(Country)),
        "Geographical groups" = rv$summarized_data %>%
          mutate(Studies = make_pmid_link(Studies)) %>%
          mutate(across(c(Frequency, Weighted, Weighted_MAD), ~ ifelse(. > 0.001, round(., 3), round(., 5)))) %>%
          arrange(subgroup) %>%
          select(
            Subgroup = contains("subgroup"),
            `Frequency [Median]` = Frequency,
            `Frequency CI [95%]` = Frequency_CI,
            `Weighted frequency [Median]` = Weighted,
            `Weighted median absolute deviation` = Weighted_MAD,
            `Number of cohorts` = Number_of_cohorts,
            `Aggregated sample size*` = Aggregated_sample_size,
            `Studies [PMID]` = Studies
          ) %>%
          mutate(Subgroup = factor(Subgroup)) %>%
          select(-starts_with(drop_colums))
      )


      rv$summed_data %>%
        DT::datatable(.,
          extensions = c("Buttons"),
          rownames = FALSE,
          escape = FALSE,
          filter = list(position = "top", clear = TRUE),
          selection = "single",
          callback = JS("
var tips = ['Ethnogeographic group or country', 'The median or weighted median (default) frequency', 
'The confidence interval or the absolute deviation (default)',
'The number of cohorts available for aggregation',
'Aggregated sample size. Please note, that some studies used the same or a subset of samples of other studies',
'All studies available for aggregation displayed as hyperlinks directing to Pubmed. Since the frequencies for the *1 or reference alleles are calculated, no specific references are provided'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);}
  $('th').tooltip({
    content: function () {
        return $(this).attr('title');
    },
    tooltipClass: 'custom-tooltip'
});

// Add custom CSS for the tooltip
$('<style>')
  .prop('type', 'text/css')
  .html('.custom-tooltip { font-size: 16px; }')
  .appendTo('head');"),
          options = list(
            pageLength = 25, scrollX = TRUE, scrollCollapse = F,
            lengthMenu = list(
              c(25, 50, 100, -1),
              c("25", "50", "100", "All")
            ),
            search = list(regex = TRUE),
            dom = "lfrtip",
            buttons = list(
              list(extend = "copy", title = NULL),
              list(extend = "excel", title = glue("{input$gene}_{input$allele}_{Sys.Date()}")),
              list(extend = "pdf", title = glue("{input$gene}_{input$allele}_{Sys.Date()}"))
            )
          )
        )
    },
    server = FALSE
  )



  output$dt_dl_button <- renderUI({
    req(input$gene)
    req(input$allele)
    req(input$mapping_method)
    downloadButton("datatab_downloadData", "Download data")
  })

  output$datatab_downloadData <- downloadHandler(
    filename = function() {
      paste0("pharmfreq_data_", input$allele, Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(rv$summed_data, file, quote = F, row.names = F, sep = "\t")
    }
  )

  debounced_click_dt <- debounce(reactive(input$DT_table_row_last_clicked), 500)
  observeEvent(debounced_click_dt(), {
    req(input$gene)
    req(input$allele)

    DT_table_row_last_clicked <- debounced_click_dt()

    output$raw_data_sweet_dt <- DT::renderDT(
      {
        req(input$gene)
        req(input$allele)

        rv$raw_data_country <- gr_country <- switch(input$mapping_method,
          "Countries" = {
            rv$summed_data %>%
              slice(DT_table_row_last_clicked) %>%
              pull(Country)
          },
          "Geographical groups" = rv$summed_data %>%
            slice(DT_table_row_last_clicked) %>%
            pull(Subgroup)
        )

        gr_pmid <- switch(input$mapping_method,
          "Countries" = rv$summarized_data %>%
            filter(ISO2 %in% gr_country) %>%
            pull(Studies) %>%
            str_split(., ", ") %>% unlist(),
          "Geographical groups" = rv$summarized_data %>%
            filter(subgroup %in% gr_country) %>%
            pull(Studies) %>%
            str_split(., ", ") %>% unlist()
        )

        tmp <- switch(input$mapping_method,
          "Countries" = rv$raw_data %>%
            filter(Allele == input$allele) %>%
            filter(pmid %in% gr_pmid) %>%
            filter(country %in% gr_country),
          "Geographical groups" = rv$raw_data %>%
            filter(Allele == input$allele) %>%
            filter(pmid %in% gr_pmid) %>%
            filter(subgroup %in% gr_country)
        )

        if (input$mapping_method == "Countries" & gr_country == "USA") {
          tmp <- tmp %>%
            mutate(subgroup = subsubgroup) %>%
            mutate(subgroup = ifelse(is.na(subgroup), "No data/Mixed", subgroup))
        }

        rv$raw_data_for_download <- tmp %>%
          select(Country = country, Subgroup = subgroup, Gene = gene, Allele, Frequency = Freq, `Cohort size` = n, `Studies [PMID]` = pmid)

        rv$raw_data_for_download %>%
          mutate(Frequency = ifelse(Frequency > 0.001, round(Frequency, 3), round(Frequency, 5))) %>%
          mutate(`Studies [PMID]` = make_pmid_link(`Studies [PMID]`)) %>%
          DT::datatable(.,
            escape = F, rownames = F,
            selection = "none",
            options = list(
              dom = "lpti", paging = T, scrollX = TRUE, scrollCollapse = F,
              pageLength = 5, lengthMenu = c(5, 10, 20),
              columnDefs = list(list(className = "dt-left", targets = "_all"))
            )
          )
      },
      server = F
    )

    shinyalert::shinyalert(
      title = paste0("Raw data for ", input$gene, " ", input$allele),
      text = if (!(input$allele == "*1" | grepl("Ref|ref", input$allele))) {
        tagList(
          DT::DTOutput("raw_data_sweet_dt"),
          br(),
          downloadButton("raw_downloadData", "Download all data")
        )
      } else {
        tagList(p("Reference allele frequency was calculated based on the available data."))
      },
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

  output$raw_downloadData <- downloadHandler(
    filename = function() {
      paste0(
        "pharmfreq_rawdata_",
        make_clean_names(input$gene),
        "_",
        make_clean_names(input$allele),
        "_",
        make_clean_names(rv$raw_data_country), "_", Sys.Date(), ".txt"
      )
    },
    content = function(file) {
      write.table(rv$raw_data_for_download, file, quote = F, row.names = F, sep = "\t")
    }
  )
  
  # map raw data ------------------------------------------------------------
  debounced_click <- debounce(reactive(input$leaflet_base_shape_click), 500)
  observeEvent(debounced_click(), {
    p <- debounced_click()

    output$raw_data_sweet2 <- DT::renderDT({
        req(input$gene)
        req(input$allele)
        gr_pmid <- switch(input$mapping_method,
          "Countries" = rv$summarized_data %>%
            filter(ISO2 %in% p) %>%
            pull(Studies) %>%
            str_split(., ", ") %>% unlist(),
          "Geographical groups" = rv$summarized_data %>%
            filter(subgroup %in% p) %>%
            pull(Studies) %>%
            str_split(., ", ") %>% unlist()
        )

        tmp <- switch(input$mapping_method,
          "Countries" = rv$raw_data %>%
            filter(Allele == input$allele) %>%
            filter(pmid %in% gr_pmid) %>%
            filter(ISO2 %in% p),
          "Geographical groups" = rv$raw_data %>%
            filter(Allele == input$allele) %>%
            filter(pmid %in% gr_pmid) %>%
            filter(subgroup %in% p)
        )

        if (!(grepl("Ref|ref", input$allele) | input$allele == "*1")) {
          if (input$mapping_method == "Countries") {
            if (unique(tmp$ISO2) == "US") {
              tmp <- tmp %>%
                mutate(subgroup = subsubgroup) %>%
                mutate(subgroup = ifelse(is.na(subgroup), "No data/Mixed", subgroup))
            }
          }
        }

        rv$raw_map_country <- switch(input$mapping_method,
          "Countries" = unique(tmp$country),
          "Geographical groups" = unique(tmp$subgroup)
        )

        rv$raw_map_for_download <- tmp %>%
          select(Country = country, Subgroup = subgroup, Gene = gene, Allele, Frequency = Freq, `Cohort size` = n, `Studies [PMID]` = pmid)

        rv$raw_map_for_download %>%
          mutate(`Studies [PMID]` = make_pmid_link(`Studies [PMID]`)) %>%
          mutate(Frequency = ifelse(Frequency > 0.001, round(Frequency, 3), round(Frequency, 5))) %>%
          DT::datatable(.,
            escape = F, rownames = F,
            selection = "none",
            options = list(
              dom = "lpti", paging = T, scrollX = TRUE, scrollCollapse = F,
              pageLength = 5, lengthMenu = c(5, 10, 20),
              columnDefs = list(list(className = "dt-left", targets = "_all"))
            )
          )
      },
      server = F
    )

    shinyalert::shinyalert(
      title = paste0("Raw data for ", input$gene, " ", input$allele),
      text = if (!(input$allele == "*1" | grepl("Ref|ref", input$allele))) {
        tagList(
          DT::DTOutput("raw_data_sweet2"),
          br(),
          downloadButton("raw_downloadMap", "Download all data")
        )
      } else {
        tagList(p("Reference allele frequency was calculated based on the available data."))
      },
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

  output$raw_downloadMap <- downloadHandler(
    filename = function() {
      paste0(
        "pharmfreq_rawdata_",
        make_clean_names(input$gene),
        "_",
        make_clean_names(input$allele),
        "_",
        make_clean_names(rv$raw_map_country), "_", Sys.Date(), ".txt"
      )
    },
    content = function(file) {
      write.table(rv$raw_map_for_download, file, quote = F, row.names = F, sep = "\t")
    }
  )

  observe({
    req(input$gene)
    req(input$allele)
    req(input$mapping_method)

    drop_colums <- switch(input$average,
      "Median" = "Weighted",
      "Weighted median" = "Frequency"
    )

    rv$plot_data <- switch(input$mapping_method,
      "Countries" = rv$summarized_data %>%
        select(-starts_with(drop_colums)) %>%
        select(
          Frequency = matches("Frequency\\b|Weighted\\b"),
          Number_of_cohorts,
          Aggregated_sample_size,
          ISO2,
          country
        ) %>%
        mutate(pop.html.tooltip = paste0(
          "<p align='left'>Frequency:&nbsp;", Frequency * 100,
          "<br>Studies:&nbsp", Number_of_cohorts, "</p>"
        )),
      "Geographical groups" = rv$summarized_data %>%
        select(-starts_with(drop_colums)) %>%
        select(
          Frequency = matches("Frequency\\b|Weighted\\b"),
          Number_of_cohorts, subgroup,
          Aggregated_sample_size,
          ISO2,
          country
        ) %>%
        mutate(pop.html.tooltip = paste0(
          "<p align='left'>Frequency:&nbsp;", Frequency * 100,
          "<br>Studies:&nbsp", Number_of_cohorts,
          "<br>Studied populations:&nbsp", country, "</p>"
        ))
    )
  })

  #leaflet----------------------------------------------------------------
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
    validate(need(n_distinct(dt_leaf) > 0, "No data available"))
    switch(input$mapping_method,
      "Countries" = {
        leaflet_data <- subset(myspdf, myspdf@data$ISO2 %in% dt_leaf$ISO2)
        leaflet_data_merge <- left_join(leaflet_data@data, dt_leaf, by = "ISO2")
        leaflet_data@data$TEST <- leaflet_data_merge$ISO2
        validate(need(all(leaflet_data@data$TEST == leaflet_data@data$ISO2), label = "matching problem"))
        leaflet_data@data$Frequency <- ifelse(leaflet_data_merge$Frequency * 100 > 1, round(leaflet_data_merge$Frequency * 100, 2), round(leaflet_data_merge$Frequency * 100, 4))
        leaflet_data@data$Number_of_cohorts <- leaflet_data_merge$Number_of_cohorts
        leaflet_data@data$Aggregated_sample_size <- leaflet_data_merge$Aggregated_sample_size
        leaflet_data
      },
      "Geographical groups" = {
        if (n_distinct(dt_leaf$subgroup) > 1) {
          test_fin <- raster::bind(myspdf_regions[dt_leaf$subgroup])
        }

        if (n_distinct(dt_leaf$subgroup) == 1) {
          test_fin <- myspdf_regions[[dt_leaf$subgroup]]
        }

        test_fin$NAMES <- names(myspdf_regions[dt_leaf$subgroup])
        test_fin$Frequency <- ifelse(dt_leaf$Frequency * 100 > 1, round(dt_leaf$Frequency * 100, 2), round(dt_leaf$Frequency * 100, 4))
        test_fin$Number_of_cohorts <- dt_leaf$Number_of_cohorts
        test_fin$Aggregated_sample_size <- dt_leaf$Aggregated_sample_size
        test_fin$country_old <- str_wrap(paste0("<br>", dt_leaf$country), 40) %>% gsub("\\\n", "<br>", .)
        test_fin
      }
    )
  })



  observe({
    req(input$gene)
    req(input$allele)
    req(input$average)
    req(input$mapping_method)




    if (input$mapping_method == "Countries") {
      leaflet_data <- leaflet_freq_data()
      labels <- glue("<p align='left'><strong>{ifelse(grepl('Viet', leaflet_data$NAME),'Vietnam' ,leaflet_data$NAME)}</strong><br/>
                      Frequency: {leaflet_data$Frequency}%<br/>
                      Studies: {ifelse(input$allele == '*1' | grepl('Ref|ref', input$allele), 'NA', leaflet_data$Number_of_cohorts)}<br/>
                      Aggregated sample number: {leaflet_data$Aggregated_sample_size }<br/>
                     <br/><i>Click on a country to load raw data<i/><p/>") %>% lapply(htmltools::HTML)

      scale_values <- c(0, unique(leaflet_data$Frequency))
      pal <- colorNumeric("viridis", scale_values)
      pal_rev <- colorNumeric("viridis", scale_values, reverse = T)

      leafletProxy("leaflet_base") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = leaflet_data,
          layerId = ~ISO2,
          fillColor = ~ pal(Frequency), color = "",
          smoothFactor = 0.01, fillOpacity = 0.8,
          label = labels,
          highlight = highlightOptions(
            weight = 1,
            fillOpacity = 1,
            bringToFront = TRUE
          )
        ) %>%
        addLegend("bottomright",
          pal = pal_rev, values = scale_values,
          title = "Frequency",
          labFormat = labelFormat(
            suffix = "%",
            transform = function(x) sort(x, decreasing = T)
          ),
          opacity = 1
        )
    } else {
      test_fin <- leaflet_freq_data()
      labels <- glue("<p align='left'><strong>{test_fin$NAMES}</strong><br/>Frequency:
                                      {test_fin$Frequency}%<br/>Studies: {ifelse(input$allele == '*1' | grepl('Ref|ref', input$allele), 'NA', test_fin$Number_of_cohorts) }<br/>
                                      Aggregated sample number: {test_fin$Aggregated_sample_size }<br/>

                                      <br/><i>Click on a region to load raw data<i/><p/>") %>% lapply(htmltools::HTML)
      scale_values <- c(0, unique(test_fin$Frequency))
      pal <- colorNumeric("viridis", scale_values)
      pal_rev <- colorNumeric("viridis", scale_values, reverse = T)

      leafletProxy("leaflet_base") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = test_fin,
          fillColor = ~ pal(Frequency), fillOpacity = 0.8,
          label = labels,
          layerId = ~NAMES,
          highlight = highlightOptions(
            weight = 1,
            fillOpacity = 1,
            bringToFront = TRUE
          ),
          color = "",
          smoothFactor = 0.01
        ) %>%
        addLegend("bottomright",
          pal = pal_rev, values = scale_values,
          title = "Frequency",
          labFormat = labelFormat(
            suffix = "%",
            transform = function(x) sort(x, decreasing = T)
          ),
          opacity = 1
        )
    }
  })
# leaflet download handler ------------------------------------------------
  output$dl_leaflet <- downloadHandler(
    filename = function() {
      paste0("pharmfreq_", input$gene, gsub("[*]", "_", input$allele), ".png")
    },
    content = function(file) {
      showNotification("Be patient. Plot is rendered for download.", duration = 5, type = "message", id = "note1")

      if (input$mapping_method == "Countries") {
        leaflet_data <- leaflet_freq_data()

        # color function
        labels <- glue("<p align='left'><strong>{leaflet_data$NAME}</strong><br/>
                      Frequency: {leaflet_data$Frequency}%<br/>
                      Studies: {leaflet_data$Number_of_cohorts }<br/>
                      Aggregated sample number (duplicated samples possible):{leaflet_data$Aggregated_sample_size }<br/>
                     <br/><i>Click on a country to load raw data<i/><p/>") %>% lapply(htmltools::HTML)

        scale_values <- c(0, unique(leaflet_data$Frequency))
        pal <- colorNumeric("viridis", scale_values)
        pal_rev <- colorNumeric("viridis", scale_values, reverse = T)

        map_to_plot <- map_reactive() %>%
          addPolygons(
            data = leaflet_data,
            layerId = ~ISO2,
            fillColor = ~ pal(Frequency), color = "",
            smoothFactor = 0.01, fillOpacity = 0.8,
            label = labels,
            highlight = highlightOptions(
              weight = 1,
              fillOpacity = 1,
              bringToFront = TRUE
            )
          ) %>%
          addLegend("bottomright",
            pal = pal_rev, values = scale_values,
            title = "Frequency",
            labFormat = labelFormat(
              suffix = "%",
              transform = function(x) sort(x, decreasing = T)
            ),
            opacity = 1
          )
      } else {
        test_fin <- leaflet_freq_data()
        labels <- glue("<p align='left'><strong>{test_fin$NAMES}</strong><br/>Frequency:
                                      {test_fin$Frequency}%<br/>Studies: {test_fin$Number_of_cohorts }<br/>
                                      Aggregated sample number:{test_fin$Aggregated_sample_size }<br/>
                                      Studied populations: {test_fin$country_old}<br/>
                                      <br/><i>Click on a region to load raw data<i/><p/>") %>% lapply(htmltools::HTML)
        scale_values <- c(0, unique(test_fin$Frequency))
        pal <- colorNumeric("viridis", scale_values)
        pal_rev <- colorNumeric("viridis", scale_values, reverse = T)
        map_to_plot <- map_reactive() %>%
          addPolygons(
            data = test_fin,
            fillColor = ~ pal(Frequency), fillOpacity = 0.8,
            label = labels,
            layerId = ~NAMES,
            highlight = highlightOptions(
              weight = 1,
              fillOpacity = 1,
              bringToFront = TRUE
            ),
            color = "",
            smoothFactor = 0.01
          ) %>%
          addLegend("bottomright",
            pal = pal_rev, values = scale_values,
            title = "Frequency",
            labFormat = labelFormat(
              suffix = "%",
              transform = function(x) sort(x, decreasing = T)
            ),
            opacity = 1
          )
      }

      mapview::mapshot(
        x = map_to_plot,
        file = file,
        # cliprect = "viewport"
        selfcontained = FALSE
      )
      removeNotification("note1")
    }
  )

# final plotting function -------------------------------------------------------
  output$render_plots <- renderUI({
    list(
      fluidRow(column(width = 12, align = "center", leafletOutput("leaflet_base", height = "60vh", width = "auto") %>% withSpinner(color = "steelblue"))),
      br(),
      downloadButton(outputId = "dl_leaflet", label = "Screenshot", icon = icon("camera")),
      br()
    )
  })

  output$rrender_add_info <- renderUI({
    req(input$gene)
    req(input$allele)
    req(input$average)

    hap_lgl <- nrow(rv$haplotype_allele) > 0
    status_lgl <- nrow(rv$function_allele) > 0
    gene_lgl <- nrow(rv$gene_drugs) > 0

    if (hap_lgl) {
      tmp1 <- "There is one core variant in the pharmacogenomic allele"
      tmp2 <- "There are XX variants in the pharmacogenomic haplotype"

      tmp3 <- "according to"

      if (unique(rv$haplotype_allele$comment) == "pharmvar") {
        b_hap <- argonCard(
          width = 12, background_color = "primary",
          argonTextColor(h2("Key variants of the allele/haplotype"), color = "white"),
          DTOutput("haplotype_allele_dt"),
          br(),
          div(
            glue("{ifelse(n_distinct(rv$haplotype_allele$rs_id)==1, paste(tmp1), paste(gsub('XX', n_distinct(rv$haplotype_allele$rs_id),tmp2)))} {paste(tmp3)}"),
            tags$a(
              href = paste0("https://www.pharmvar.org/gene/", input$gene),
              "pharmvar.", style = "color: white; text-decoration: underline;", target = "_blank"
            )
          ) %>% argonTextColor(color = "white")
          # }
        )
      } else {
        b_hap <- argonCard(
          width = 12, background_color = "primary",
          argonTextColor(h2("Key variants of the allele/haplotype"), color = "white"),
          DTOutput("haplotype_allele_dt"),
          br(),
          div(glue("{ifelse(n_distinct(rv$haplotype_allele$rs_id)==1, paste(tmp1), paste(gsub('XX', n_distinct(rv$haplotype_allele$rs_id),tmp2)))}")) %>%
            argonTextColor(color = "white")
        )
      }
    }

    if (status_lgl) {
      b_status <- argonCard(
        width = 12,
        background_color = rv$function_allele$status[1],
        argonTextColor(h2(glue("Functional status - {rv$function_allele$functional_status}")), color = "white"),
        argonTextColor(div(
          glue("{ifelse(is.na(rv$function_allele$activity_value), 'Activity score not yet available', paste0('The activity score is ',rv$function_allele$activity_value) )}."),
          br(),
          glue("The clinical consequence of the starallele {paste0(input$gene,input$allele)} is extracted from"),
          tags$a(
            href = paste0("https://www.pharmgkb.org/page/", tolower(input$gene), "RefMaterials"),
            tags$img(src = "https://raw.githubusercontent.com/PharmGKB/pgkb-branding/main/logo.svg", height = "20px"),
            target = "_blank"
          )
        ), color = "white")
      ) # end of box
    }

    if (gene_lgl) {
      gd_status <- argonCard(
        width = 12,
        background_color = "default",
        argonTextColor(h2(glue("PGx-based drug dosing guidelines for {input$gene}")), color = "white"),
        HTML(paste0("<p>", paste(rv$gene_drugs$gr_link, collapse = ", "), "</p>"))
      )
    }


    if (hap_lgl & status_lgl & gene_lgl) {
      list(br(), fluidRow(
        column(
          width = 5, b_status,
          gd_status
        ),
        column(width = 7, b_hap)
      ))
    } else if (!hap_lgl & status_lgl & gene_lgl) {
      list(br(), fluidRow(column(
        width = 6, b_status,
        gd_status
      )))
    } else if (hap_lgl & !status_lgl & gene_lgl) {
      list(br(), fluidRow(
        column(width = 6, gd_status),
        column(width = 6, b_hap)
      ))
    } else if (hap_lgl & !status_lgl & !gene_lgl) {
      list(br(), fluidRow(column(width = 6, b_hap)))
    } else if (!hap_lgl & status_lgl & !gene_lgl) {
      list(br(), fluidRow(column(width = 6, b_status)))
    } else if (!hap_lgl & !status_lgl & gene_lgl) {
      list(br(), fluidRow(column(width = 6, gd_status)))
    }
  })

  observe({
    req(input$gene)
    req(input$allele)
    req(input$average)
    rv$haplotype_allele <-
      data$haplotype_alleles %>%
      bind_rows() %>%
      filter(gene == input$gene) %>%
      filter(Allele == input$allele) %>%
      select(where(~ !any(is.na(.))))
  })

  observe({
    req(input$gene)
    req(input$allele)
    req(input$average)

    rv$function_allele <- data$functional_alleles %>%
      filter(gene == input$gene) %>%
      filter(Allele == input$allele)
  })

  observe({
    req(input$gene)
    req(input$allele)
    req(input$average)

    rv$gene_drugs <- gene_drugs %>%
      filter(gene == input$gene)
  })

  output$haplotype_allele_dt <- DT::renderDT({
    req(input$gene)
    req(input$allele)
    req(input$average)

    if (nrow(rv$haplotype_allele) > 0) {
      tmp <- rv$haplotype_allele %>%
        mutate(dbSNP = glue('<a style="color: white; text-decoration: underline;" href="https://www.ncbi.nlm.nih.gov/snp/{rs_id}" target="_blank">{rs_id}</a>')) %>%
        select(dbSNP, Chr, starts_with("GRCh"), starts_with("Ref"), starts_with("M33388"), `Co-ocurrence in star alleles` = other_alleles)

      if (unique(rv$haplotype_allele$comment) != "pharmvar") {
        tmp <- tmp %>% select(-matches("ocurren"))
      }

      tmp %>%
        DT::datatable(.,
          extensions = "Responsive", rownames = F, escape = F, filter = "none", selection = "none",
          plugins = "ellipsis",
          options = list(
            paging = FALSE, lengthChange = FALSE, searching = F,
            columnDefs = list(list(
              targets = c(ncol(tmp) - 1),
              render = JS("$.fn.dataTable.render.ellipsis( 17, false )")
            )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#F0000000','color': 'white'});",
              "}"
            ),
            dom = "t"
          )
        ) %>%
        DT::formatStyle(columns = 0:(ncol(tmp) - 1), color = "white")
    }
  })

  observeEvent(input$imprint, {
    sendSweetAlert(
      session = session,
      title = "",
      text = includeMarkdown("imprint.md"),
      html = TRUE,
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE,
      width = "900px",
      height = "auto"
    )
  })

  observe({
    req(input$tools_act_mapping_method)
    switch(input$tools_act_mapping_method,
      "Countries" = {
        updatePickerInput(session, "tools_act_countries", choices = sort(unique(data$freq_per_country$country)))
      },
      "Geographical groups" = {
        updatePickerInput(session, "tools_act_countries", choices = sort(unique(data$freq_per_subgroup$subgroup)))
      }
    )
    rv$tool_act_plot_data_raw <- switch(input$tools_act_mapping_method,
      "Countries" = {
        data$freq_per_country %>% select(region = country, gene, Allele, Weighted)
      },
      "Geographical groups" = {
        data$freq_per_subgroup %>% select(region = subgroup, gene, Allele, Weighted)
      }
    )
  })

  observe({
    req(input$tools_act_method)
    req(input$tools_act_countries)
    req(input$tools_act_mapping_method)
    req(rv$tool_act_plot_data_raw)

        dunnernberger_all <- str_split(input$tools_act_countries, ",") %>%
      unlist() %>%
      map_dfr(~ mutate(dunnenberger, region = .))
    pharmvar_all <- str_split(input$tools_act_countries, ",") %>%
      unlist() %>%
      map_dfr(~ mutate(pharmvar, region = .))


    rv$tool_act_plot_data <- switch(input$tools_act_method,
      "Dunnenberger (2015)" = {
        rv$tool_act_plot_data_raw %>%
          as_tibble() %>%
          filter(region %in% str_split(input$tools_act_countries, ",") %>% unlist()) %>%
          right_join(dunnernberger_all, by = join_by(region, gene, Allele))
      },
      "CPIC function" = {
        rv$tool_act_plot_data_raw %>%
          as_tibble() %>%
          filter(region %in% str_split(input$tools_act_countries, ",") %>% unlist()) %>%
          right_join(pharmvar_all, by = join_by(region, gene, Allele))
      }
    ) %>%
      mutate(gr = is.na(Weighted)) %>%
      split(.$gr) %>%
      map_dfr(~ group_by(., gene, region) %>%
        summarise(
          freq = sum(Weighted),
          Allele = toString(unique(gtools::mixedsort(Allele)))
        ) %>%
        ungroup())
  })

  output$tool_act_plot <- renderPlotly({
    req(input$tools_act_method)
    req(input$tools_act_countries)
    req(input$tools_act_mapping_method)
    req(rv$tool_act_plot_data)

    pp <- rv$tool_act_plot_data %>%
      filter(!is.na(freq)) %>%
      ggplot(aes(gene, freq, fill = region, text = paste0("Country/Region: ", region, "\nGene: ", gene, "\nSummarized weighted frequency: ", round(freq * 100, 2), "%"))) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_y_continuous("Frequency (%)", labels = scales::percent) +
      scale_x_discrete("High-risk genotype according to gene") +
      scale_fill_viridis_d("Region") +
      theme_bw(base_size = 16) +
      theme(legend.position = "bottom")

    ggplotly(pp, tooltip = "text") %>%
      config(
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
          "hoverCompareCartesian", "toggleSpikelines", "zoomIn2d", "zoomOut2d", "sendDataToCloud", "toggleFullscreen"
        ),
        displaylogo = F
      )
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
          select(gene, region, "Alleles without data" = Allele),
        by = join_by(gene, region)
      ) %>%
      mutate(across(c(gene, region), as.factor)) %>%
      mutate(freq = ifelse(freq > 0.001, round(freq, 3), round(freq, 5))) %>%
      select(`Country/Region` = region, Gene = gene, `Summarized alleles` = Allele, `Summarized weighted frequency` = freq, everything()) %>%
      DT::datatable(
        rownames = F,
        callback = JS("
var tips = ['The selected country or region.', 
            'Genes with pharmacogenomic guideline (CPIC, DPWG).',
            'Actionable alleles with available frequencies used for summarization.',
            'The summarized frequencies. Here, the weighted frequency was used for calculation.', 
            'Actionable alleles for which no allele frequency data was available.'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);}
$('th').tooltip({
    content: function () {
        return $(this).attr('title');
    },
    tooltipClass: 'custom-tooltip'
});
$('<style>')
  .prop('type', 'text/css')
  .html('.custom-tooltip { font-size: 50px; }')
  .appendTo('head');"),
        selection = "none", filter = list(position = "top", clear = TRUE),
        options = list(
          dom = "lftip",
          scrollX = TRUE,
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          buttons = list(
            list(extend = "copy", title = NULL),
            list(extend = "excel", title = glue("highrisk_{Sys.Date()}")),
            list(extend = "pdf", title = glue("highrisk_{Sys.Date()}"))
          )
        )
      )
  })

  output$tool1_dl_button <- renderUI({
    req(input$tools_act_countries)
    req(rv$tool_act_plot_data)
    downloadButton("tool_act_table_downloadData", "Download data")
  })


  output$tool_act_table_downloadData <- downloadHandler(
    filename = function() {
      paste0("Highriskdata_", Sys.Date(), ".txt")
    },
    content = function(file) {
      rv$tool_act_plot_data %>%
        filter(!is.na(freq)) %>%
        full_join(
          rv$tool_act_plot_data %>%
            filter(is.na(freq)) %>%
            select(gene, region, "Alleles without data" = Allele),
          by = join_by(gene, region)
        ) %>%
        mutate(across(c(gene, region), as.factor)) %>%
        select(`Country/Region` = region, Gene = gene, `Summarized alleles` = Allele, `Summarized weighted frequency` = freq, everything()) %>%
        write.table(., file, quote = F, row.names = F, sep = "\t")
    }
  )

  output$tool_act_text <- renderText({
    req(input$tools_act_method)
    req(input$tools_act_countries)
    req(input$tools_act_mapping_method)
    switch(input$tools_act_method,
      "Dunnenberger (2015)" = {
        paste("The proportion of individuals expected to carry a high-risk genotype for", n_distinct(rv$tool_act_plot_data$gene), "genes identified by the 
         Clinical Pharmacogenetics Implementation Consortium (CPIC) as having at least one known, actionable inherited variant/allele. 
         The high-risk diplotype frequencies were estimated according (Dunnenberger et al., 2015). In the table below the column 'Summarized alleles' shows for which alleles frequency data was aggregated. 
         The column 'Alleles without data' shows alleles for which no allele frequency data was available in the selected countries/regions. 
          To resize the plot, simply click and drag the bottom-right corner using your mouse. All data can be downloaded by clicking the download button located below the table.")
      },
      "CPIC function" = {
        paste("The proportion of individuals expected to carry a high-risk genotype for", n_distinct(rv$tool_act_plot_data$gene), "genes identified by the 
         Clinical Pharmacogenetics Implementation Consortium (CPIC®) resource as having at least one known, actionable inherited variant/allele. In the table below the column 'Summarized alleles' shows for which alleles frequency data was aggregated.
                      The column 'Alleles without data' shows alleles for which no allele frequency data was available in the selected countries/regions.
                   To resize the plot, simply click and drag the bottom-right corner using your mouse. All data can be downloaded by clicking the download button located below the table.")
      }
    )
  })

  observe({
    req(input$tools_freq_gene)

    tmp_choices <- data$freq_per_country %>%
      filter(gene == input$tools_freq_gene) %>%
      distinct(Allele) %>%
      pull(Allele) %>%
      gtools::mixedsort()

    if (input$tools_freq_gene %in% unique(data$functional_alleles$gene)) {
      tmp_choices <- data$freq_per_country %>%
        filter(gene == input$tools_freq_gene) %>%
        distinct(gene, Allele) %>%
        left_join(data$functional_alleles, by = join_by(gene, Allele)) %>%
        mutate(functional_status = ifelse(is.na(functional_status),
          "unknown function", functional_status
        )) %>%
        slice(gtools::mixedorder(Allele))

      updatePickerInput(session, "tools_freq_allele", choices = tmp_choices$Allele, choicesOpt = list(subtext = tmp_choices$functional_status))
    } else {
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
      filter(gene == input$tools_freq_gene) %>%
      filter(Allele %in% input$tools_freq_allele) %>%
      group_by(country, ISO2) %>%
      summarise(Weighted = sum(Weighted, na.rm = T)) %>%
      ungroup() %>%
      left_join(data$mapping_country_subgroup,
        by = join_by(ISO2)
      ) %>%
      filter(subgroup %in% input$tools_freq_countries)
  })



  output$tool_freq_plot <- renderPlotly({
    req(input$tools_freq_countries)
    req(input$tools_freq_gene)
    req(input$tools_freq_allele)


    pp <- rv$tools_freq_data %>%
      ggplot(aes(reorder(subgroup, Weighted, median),
        text = paste0("Country: ", country, "\nWeighted frequency: ", round(Weighted * 100, 2), "%"),
        Weighted
      )) +
      geom_boxplot(alpha = 0.6) +
      ggbeeswarm::geom_quasirandom(aes(color = subgroup)) +
      scale_y_continuous("Frequency (%)", labels = scales::percent, limits = c(0, max(rv$tools_freq_data$Weighted) + .01)) +
      scale_x_discrete("", labels = ~ str_wrap(., 10)) +
      scale_fill_viridis_d("Biogeographic subgroups") +
      ggtitle("", subtitle = title) +
      theme_bw(base_size = 16) +
      theme(legend.position = "bottom")
    ggplotly(pp, tooltip = "text") %>%
      remove_boxplot_outliers() %>%
      plotly::layout(showlegend = F, yaxis = list(autorange = "min")) %>%
      config(
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
          "hoverCompareCartesian", "toggleSpikelines", "zoomIn2d", "zoomOut2d", "sendDataToCloud", "toggleFullscreen"
        ),
        displaylogo = F
      )
  })

  output$tools_freq_text <- renderText({
    req(input$tools_freq_countries)
    req(input$tools_freq_gene)
    req(input$tools_freq_allele)
    paste0(input$tools_freq_gene, " allele frequency is summarized for the alleles: ", toString(input$tools_freq_allele))
  })


  output$tool2_dl_button <- renderUI({
    req(input$tools_freq_countries)
    req(input$tools_freq_gene)
    req(input$tools_freq_allele)
    req(rv$tools_freq_data)
    argonRow(argonColumn(width = 12, downloadButton("tool2_downloadData", "Download data")))
  })


  output$tool2_downloadData <- downloadHandler(
    filename = function() {
      paste0("pharmfreq_frequency_comparison_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(select(rv$tools_freq_data,
        subgroup, country,
        frequency = Weighted
      ), file, quote = F, row.names = F, sep = "\t")
    }
  )

  # tool 3 frequency variability ---------------------------------------------------

  observe({
    req(input$tools_var_gene)

    tmp_choices <- data$freq_per_country %>%
      filter(gene == input$tools_var_gene) %>%
      distinct(Allele) %>%
      pull(Allele) %>%
      gtools::mixedsort()

    if (input$tools_var_gene %in% unique(data$functional_alleles$gene)) {
      tmp_choices <- data$freq_per_country %>%
        filter(gene == input$tools_var_gene) %>%
        distinct(gene, Allele) %>%
        left_join(data$functional_alleles, by = join_by(gene, Allele)) %>%
        mutate(functional_status = ifelse(is.na(functional_status),
          "unknown function", functional_status
        )) %>%
        slice(gtools::mixedorder(Allele))

      updatePickerInput(session, "tools_var_allele", choices = tmp_choices$Allele, choicesOpt = list(subtext = tmp_choices$functional_status))
    } else {
      updatePickerInput(session, "tools_var_allele", choices = tmp_choices)
    }

    freezeReactiveValue(input, "tools_var_allele")
  })

  observe({
    req(input$tools_var_gene)
    req(input$tools_var_allele)

    tmp <- data$freq_per_country %>%
      as_tibble() %>%
      filter(gene == input$tools_var_gene) %>%
      filter(Allele %in% input$tools_var_allele) %>%
      group_by(gene, country, ISO2) %>%
      summarise(
        Weighted = sum(Weighted, na.rm = T),
        Allele = toString(gtools::mixedsort(unique(Allele)))
      ) %>%
      ungroup() %>%
      left_join(data$mapping_country_subgroup, by = join_by(ISO2)) %>%
      group_split(subgroup, gene) %>%
      keep(~ nrow(.) >= 2)


    if (length(tmp) > 0) {
      rv$freq_variability_server <- tmp %>%
        map(~ rowwise(.) %>% mutate(
          max_dif = max(abs(Weighted - .$Weighted)),
          max_country = .$country[which.max(abs(Weighted - .$Weighted))]
        )) %>%
        bind_rows() %>%
        select(country, ISO2, subgroup, gene, Allele, Weighted, max_dif) %>%
        ungroup()
    } else {
      showNotification("Not enough data available")
      rv$freq_variability_server <- NULL
    }
  })

  observe({
    req(input$tools_var_gene)
    req(input$tools_var_allele)
    req(rv$freq_variability_server)

    tmp <- rv$freq_variability_server %>%
      group_by(subgroup) %>%
      filter(Weighted == min(Weighted) | Weighted == max(Weighted)) %>%
      mutate(gr = ifelse(Weighted == min(Weighted), "min", "max")) %>%
      group_by(subgroup, gr) %>%
      slice(1) %>%
      group_by(subgroup) %>%
      filter(n_distinct(gr) == 2) %>%
      ungroup() %>%
      select(subgroup, gr, Weighted, max_dif) %>%
      pivot_wider(names_from = gr, values_from = Weighted)

    if (nrow(tmp) > 0) {
      rv$df_segment <- tmp %>% mutate(middle = min + ((max - min) / 2))
    } else {
      rv$df_segment <- NULL
    }
  })


  output$tool_var_plot <- renderPlotly({
    req(input$tools_var_gene)
    req(input$tools_var_allele)
    req(rv$freq_variability_server)


    if (is.null(rv$df_segment)) {
      pp <- rv$freq_variability_server %>%
        ggplot(aes(x = Weighted, y = subgroup)) +
        ggbeeswarm::geom_quasirandom(orientation = "y", aes(color = subgroup, text = paste0("Country: ", country, "\nWeighted frequency: ", round(Weighted * 100, 2), "%"))) +
        geom_text(
          data = . %>% group_by(subgroup) %>%
            mutate(min_max = case_when(min(Weighted) == Weighted ~ "min", max(Weighted) == Weighted ~ "max", T ~ "no")) %>%
            group_by(subgroup, min_max) %>%
            slice(1) %>% ungroup() %>%
            filter(min_max != "no"),
          aes(label = paste(country))
        ) +
        scale_x_continuous("Frequency (%)", labels = scales::percent, limits = c(0, NA)) +
        scale_y_discrete("") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom")
    } else {
      pp <- rv$freq_variability_server %>%
        ggplot(aes(x = Weighted, y = subgroup)) +
        geom_segment(data = rv$df_segment, aes(x = min, xend = max, y = subgroup, yend = subgroup)) +
        ggbeeswarm::geom_quasirandom(orientation = "y", aes(color = subgroup, text = paste0("Country: ", country, "\nWeighted frequency: ", round(Weighted * 100, 2), "%"))) +
        geom_text(data = rv$df_segment, nudge_y = 0.08, aes(x = middle, y = subgroup, label = paste0(round(max_dif * 100, 2), "%"))) +
        geom_text(
          data = . %>% group_by(subgroup) %>%
            mutate(min_max = case_when(min(Weighted) == Weighted ~ "min", max(Weighted) == Weighted ~ "max", T ~ "no")) %>%
            group_by(subgroup, min_max) %>%
            slice(1) %>% ungroup() %>%
            filter(min_max != "no"),
          aes(label = paste(country)), position = position_jitter(width = 0, height = 0.2)
        ) +
        scale_x_continuous("Frequency (%)", labels = scales::percent) +
        scale_y_discrete("") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom")
    }

    ggplotly(pp, tooltip = "text") %>%
      plotly::layout(showlegend = F) %>%
      config(
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
          "hoverCompareCartesian", "toggleSpikelines", "zoomIn2d", "zoomOut2d", "sendDataToCloud", "toggleFullscreen"
        ),
        displaylogo = F
      )
  })

  output$tool3_downloadData <- downloadHandler(
    filename = function() {
      paste0("pharmfreq_intervariability_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(select(rv$freq_variability_server,
        subgroup, country, gene,
        allele = Allele, frequency = Weighted, maximal_difference = max_dif
      ), file, quote = F, row.names = F, sep = "\t")
    }
  )

  output$tool3_dl_button <- renderUI({
    req(input$tools_var_gene)
    req(input$tools_var_allele)
    req(rv$freq_variability_server)
    argonRow(argonColumn(width = 12, downloadButton("tool3_downloadData", "Download data")))
  })


  output$tool_var_text <- renderText({
    req(input$tools_var_gene)
    req(input$tools_var_allele)
    req(rv$freq_variability_server)

    "The plot shows the maximum frequency difference in the respective biogeographic subgroups. Gene and allele selection, as well as data are shown only when enough studies are available."
  })

  observe({
    req(input$tools_var_contgene_gene)
    req(input$tools_var_contgene_mapping_method)

    if (input$tools_var_contgene_mapping_method == "Geographical groups") {
      tmp <- tibble(subgroup = sort(unique(data$freq_per_subgroup$subgroup))) %>%
        left_join(data$freq_per_subgroup %>%
          filter(gene %in% input$tools_var_contgene_gene) %>%
          count(subgroup), by = join_by(subgroup)) %>%
        mutate(n = ifelse(is.na(n), 0, n))

      updatePickerInput(session, "tools_var_contgene_gene_country", label = "Select subgroups", choices = tmp$subgroup, choicesOpt = list(subtext = paste0(" (Data = ", tmp$n, ")")))
    } else {
      tmp <- tibble(country = sort(unique(data$freq_per_country$country))) %>%
        left_join(data$freq_per_country %>%
          filter(gene %in% input$tools_var_contgene_gene) %>%
          count(country), by = join_by(country)) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        filter(n != 0)
      updatePickerInput(session, "tools_var_contgene_gene_country", label = "Select countries", choices = tmp$country, choicesOpt = list(subtext = paste0(" (Data = ", tmp$n, ")")))
    }
    freezeReactiveValue(input, "tools_var_contgene_gene_country")
  })


  output$tools_var_contgene_dt <- renderDataTable({
    req(input$tools_var_contgene_gene_country)
    req(input$tools_var_contgene_gene)
    req(input$tools_var_contgene_average)
    req(input$tools_var_contgene_mapping_method)

    drop_colums <- switch(input$tools_var_contgene_average,
      "Median" = "Weighted",
      "Weighted median" = "Frequency"
    )
    if (input$tools_var_contgene_mapping_method == "Geographical groups") {
      tmp <- data$freq_per_subgroup %>%
        filter(gene %in% input$tools_var_contgene_gene) %>%
        filter(subgroup %in% input$tools_var_contgene_gene_country) %>%
        left_join(select(data$functional_alleles, gene, Allele, functional_status), by = c("gene", "Allele")) %>%
        mutate(functional_status = fct_inorder(functional_status)) %>%
        mutate(Frequency = ifelse(Frequency > 0.001, round(Frequency, 3), round(Frequency, 5))) %>%
        mutate(Weighted = ifelse(Weighted > 0.001, round(Weighted, 3), round(Weighted, 5))) %>%
        mutate(Weighted_MAD = ifelse(Weighted_MAD > 0.001, round(Weighted_MAD, 3), round(Weighted_MAD, 5))) %>%
        mutate(Allele = factor(Allele)) %>%
        select(
          Gene = gene,
          Subgroup = subgroup,
          Allele,
          Function = functional_status,
          `Frequency [Median]` = Frequency,
          `Frequency CI [95%]` = Frequency_CI,
          `Weighted frequency [Median]` = Weighted,
          `Weighted median absolute Deviation` = Weighted_MAD,
          `Aggregated sampe size*` = Aggregated_sample_size,
          `Number of cohorts` = Number_of_cohorts,
          `Studies [PMID]` = Studies
        ) %>%
        select(-starts_with(drop_colums)) %>%
        mutate(`Studies [PMID]` = make_pmid_link(`Studies [PMID]`)) %>%
        slice(gtools::mixedorder(Allele))
    } else {
      tmp <- data$freq_per_country %>%
        filter(gene %in% input$tools_var_contgene_gene) %>%
        filter(country %in% input$tools_var_contgene_gene_country) %>%
        left_join(select(data$functional_alleles, gene, Allele, functional_status), by = c("gene", "Allele")) %>%
        mutate(functional_status = fct_inorder(functional_status)) %>%
        mutate(Frequency = ifelse(Frequency > 0.001, round(Frequency, 3), round(Frequency, 5))) %>%
        mutate(Weighted = ifelse(Weighted > 0.001, round(Weighted, 3), round(Weighted, 5))) %>%
        mutate(Weighted_MAD = ifelse(Weighted_MAD > 0.001, round(Weighted_MAD, 3), round(Weighted_MAD, 5))) %>%
        mutate(Allele = factor(Allele)) %>%
        select(
          Gene = gene,
          Country = country,
          Subgroup = contains("subgroup"),
          Allele,
          Function = functional_status,
          `Frequency [Median]` = Frequency,
          `Frequency CI [95%]` = Frequency_CI,
          `Weighted frequency [Median]` = Weighted,
          `Weighted median absolute Deviation` = Weighted_MAD,
          `Aggregated sampe size*` = Aggregated_sample_size,
          `Number of cohorts` = Number_of_cohorts,
          `Studies [PMID]` = Studies
        ) %>%
        select(-starts_with(drop_colums)) %>%
        mutate(`Studies [PMID]` = make_pmid_link(`Studies [PMID]`)) %>%
        slice(gtools::mixedorder(Allele))
    }

    # DT::datatable(tmp)
    rv$DT_table_tool4 <- tmp
    DT::datatable(tmp,
      rownames = FALSE,
      escape = FALSE,
      filter = list(position = "top", clear = TRUE),
      selection = "none",
      callback = JS("
var tips = ['The selected pharmacogene', 
'The biogeographic population', 
'All alleles and haplotypes with frequency information in pharmfreq',
'The function of the alleles according to CPIC',
'The summarized frequency using the median or the weighted median (default)',
'The confidence interval or the absolute deviation (default)',
'Aggregated sample size. Please note, that some studies used the same or a subset of samples of other studies',
'The number of cohorts available for aggregation',
'All studies available for aggregation displayed as hyperlinks directing to Pubmed. Since the frequencies for the *1 or reference alleles are calculated, no specific references are provided'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);}
  $('th').tooltip({
    content: function () {
        return $(this).attr('title');
    },
    tooltipClass: 'custom-tooltip'
});

// Add custom CSS for the tooltip
$('<style>')
  .prop('type', 'text/css')
  .html('.custom-tooltip { font-size: 16px; }')
  .appendTo('head');"),
      options = list(
        pageLength = 25, scrollX = TRUE, scrollCollapse = F,
        lengthMenu = list(
          c(10, 25, 50, 100, -1),
          c("10", "25", "50", "100", "All")
        ),
        search = list(regex = TRUE),
        dom = "lfrtip",
        buttons = list(
          list(extend = "copy", title = NULL),
          list(extend = "excel", title = glue("{input$tools_var_contgene_gene}_{input$tools_var_contgene_gene_country}_{Sys.Date()}")),
          list(extend = "pdf", title = glue("{input$tools_var_contgene_gene}_{input$tools_var_contgene_gene_country}_{Sys.Date()}"))
        )
      )
    )
  })


  output$tool4_downloadData <- downloadHandler(
    filename = function() {
      paste0(ifelse(input$tools_var_contgene_mapping_method == "Countries", "country", "subgroup"), "_data_", input$tools_var_contgene_gene, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(rv$DT_table_tool4, file, quote = F, row.names = F, sep = "\t")
    }
  )

  output$tool4_dl_button <- renderUI({
    req(input$tools_var_contgene_gene_country)
    req(input$tools_var_contgene_gene)
    req(input$tools_var_contgene_average)
    req(input$tools_var_contgene_mapping_method)
    req(rv$DT_table_tool4)
    argonRow(argonColumn(width = 12, downloadButton("tool4_downloadData", "Download data")))
  })

  observe({
    req(input$tools_var_function_gene)
    req(input$tools_var_mapping_method)
    req(input$tools_var_mapping_method)

    tmp_tool <- filter(tool_function, gene == input$tools_var_function_gene)

    if (input$tools_var_mapping_method == "Geographical groups") {
      rv$tool5_data_filt <- tmp <- data$freq_per_subgroup %>%
        inner_join(tmp_tool, by = join_by(gene, Allele)) %>%
        as_tibble() %>%
        group_by(subgroup) %>%
        mutate(n_string = toString(sort(unique(Allele)))) %>%
        filter(n_string == unique(tmp_tool$n_string))
      updatePickerInput(session, "tools_var_function_country", label = "Select subgroups", choices = sort(unique(tmp$subgroup)))
    } else {
      rv$tool5_data_filt <- tmp <- data$freq_per_country %>%
        inner_join(tmp_tool, by = join_by(gene, Allele)) %>%
        as_tibble() %>%
        group_by(country) %>%
        mutate(n_string = toString(sort(unique(Allele)))) %>%
        filter(n_string == unique(tmp_tool$n_string))
    updatePickerInput(session, "tools_var_function_country", label = "Select countries", choices = sort(unique(tmp$country)))
    }
    freezeReactiveValue(input, "tools_var_function_country")
  })


  tool5_data <- reactive({
    req(input$tools_var_function_gene)
    req(input$tools_var_function_country)
    req(input$tools_var_mapping_method)
    if (input$tools_var_mapping_method == "Geographical groups") {
      data$freq_per_subgroup %>%
        filter(gene == input$tools_var_function_gene) %>%
        filter(subgroup %in% unique(rv$tool5_data_filt$subgroup)) %>%
        filter(subgroup %in% input$tools_var_function_country) %>%
        left_join(mutate(data$functional_alleles, functional_status = factor(functional_status)), by = join_by(gene, Allele)) %>%
        mutate(functional_status = fct_collapse(functional_status, `normal function` = c("unknown function", "uncertain function", "normal function"))) %>%
        split(.$subgroup) %>%
        map(foo_functionality) %>%
        bind_rows(.id = "subgroup") %>%
        mutate(gene = input$tools_var_function_gene)
    } else {
      data$freq_per_country %>%
        filter(gene == input$tools_var_function_gene) %>%
        filter(country %in% unique(rv$tool5_data_filt$country)) %>%
        filter(country %in% input$tools_var_function_country) %>%
        left_join(mutate(data$functional_alleles, functional_status = factor(functional_status)), by = join_by(gene, Allele)) %>%
        mutate(functional_status = fct_collapse(functional_status, `normal function` = c("unknown function", "uncertain function", "normal function"))) %>%
        unite(id, country, ISO2, sep = "_") %>%
        split(.$id) %>%
        map(foo_functionality) %>%
        bind_rows(.id = "id") %>%
        separate(id, into = c("country", "ISO2"), sep = "_") %>%
        left_join(data$mapping_country_subgroup, by = "ISO2") %>%
        mutate(gene = input$tools_var_function_gene)
    }
  })

  output$tool5_plot <- renderPlot({
    req(input$tools_var_function_gene)
    req(input$tools_var_function_country)
    req(input$tools_var_mapping_method)

    if (!is_empty(tool5_data())) {
      if (input$tools_var_mapping_method == "Geographical groups") {
        pp <- tool5_data() %>%
          mutate(Phenotype = fct_rev(phenotype)) %>%
          ggplot(aes(subgroup, Frequency, fill = Phenotype)) +
          geom_col() +
          coord_flip() +
          ylab("Frequency") +
          xlab("") +
          scale_y_continuous(labels = scales::percent) +
          theme_bw(base_size = 16) +
          theme(
            strip.placement = "outside",
            strip.background = element_blank(),
            strip.text.y.left = element_text(angle = 0),
            legend.position = "bottom"
          )
      } else {
        pp <- tool5_data() %>%
          mutate(Phenotype = fct_rev(phenotype)) %>%
          ggplot(aes(country, Frequency, fill = Phenotype)) +
          geom_col() +
          coord_flip() +
          ylab("Frequency") +
          xlab("") +
          scale_y_continuous(labels = scales::percent) +
          facet_grid(subgroup ~ ., scales = "free", space = "free", switch = "y") +
          theme_bw(base_size = 20) +
          theme(
            strip.placement = "outside",
            strip.background = element_blank(),
            strip.text.y.left = element_text(angle = 0),
            legend.position = "bottom"
          )
      }
      if (input$tools_var_function_gene %in% c("CYP2B6", "CYP2C19")) {
        pp + scale_fill_manual(values = rev(c("coral", "orange", "darkgreen", "lightblue", "steelblue")), guide = guide_legend(reverse = TRUE))
      } else {
        pp + scale_fill_manual(values = rev(c("coral", "orange", "darkgreen", "steelblue")), guide = guide_legend(reverse = TRUE))
      }
    }
  })

  output$tool5_dt <- renderDT({
    req(input$tools_var_function_gene)
    req(input$tools_var_mapping_method)
    req(input$tools_var_function_country)

    if (input$tools_var_mapping_method == "Geographical groups") {
      tmp <- tool5_data() %>%
        select(Subgroup = subgroup, Gene = gene, Phenotype = phenotype, Frequency, everything())
    } else {
      tmp <- tool5_data() %>%
        select(Country = country, Gene = gene, Phenotype = phenotype, Frequency, everything())
    }

    rv$tool5_DT <- tmp %>% mutate(Frequency = ifelse(Frequency > 0.001, round(Frequency, 3), round(Frequency, 5)))
    tmp %>%
      mutate(Frequency = ifelse(Frequency > 0.001, round(Frequency, 3), round(Frequency, 5))) %>%
      DT::datatable(.,
        rownames = FALSE,
        escape = FALSE,
        filter = list(position = "top", clear = TRUE),
        selection = "none",
        callback = JS("
var tips = ['The biogeographic population or country',
'The selected pharmacogene',
'The phenotype (PM = Poor Metabolizer, IM = Intermediate Metabolizer, NM = Normal Metabolizer, RM = Rapid Metabolizer, UM = Ultrarapid Metabolizer)',
'The estimated frequency of the phenotype'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);}
  $('th').tooltip({
    content: function () {
        return $(this).attr('title');
    },
    tooltipClass: 'custom-tooltip'
});

// Add custom CSS for the tooltip
$('<style>')
  .prop('type', 'text/css')
  .html('.custom-tooltip { font-size: 16px; }')
  .appendTo('head');"),
        options = list(
          pageLength = 25, scrollX = TRUE, scrollCollapse = F,
          lengthMenu = list(
            c(25, 50, 100, -1),
            c("25", "50", "100", "All")
          ),
          search = list(regex = TRUE),
          dom = "lfrtip",
          buttons = list(
            list(extend = "copy", title = NULL),
            list(extend = "excel", title = glue("{input$tools_var_contgene_gene}_{input$tools_var_contgene_gene_country}_{Sys.Date()}")),
            list(extend = "pdf", title = glue("{input$tools_var_contgene_gene}_{input$tools_var_contgene_gene_country}_{Sys.Date()}"))
          )
        )
      )
  })

  output$tool5_downloadData <- downloadHandler(
    filename = function() {
      paste0("metabolizer_data_", input$tools_var_function_gene, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(rv$tool5_DT, file, quote = F, row.names = F, sep = "\t")
    }
  )

  output$tool5_dl_button <- renderUI({
    req(input$tools_var_function_gene)
    req(input$tools_var_mapping_method)
    req(input$tools_var_function_country)
    req(rv$tool5_DT)
    argonRow(argonColumn(width = 12, downloadButton("tool5_downloadData", "Download data")))
  })
}