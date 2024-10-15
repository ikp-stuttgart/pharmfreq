body <- argonDash::argonDashBody(
  shinyjs::useShinyjs(),
  awn::useAwn(),
  
  tag_head,
  argonTabItems(
    argonTabItem(

      tabName = "home",
  br(),
  argonRow(
      argonCard(
        background_color = "primary",
        width=12,
        shadow = TRUE,
        argonRow(
        argonColumn(width = 9,
        argonTextColor(argonH1("PharmFreq", display = 1),color = "white"),
        argonTextColor(p(HTML(filter(CMS_text, what=="intro_text")$text),style = "font-size: 20px;"),color = "white"),
        br(),
        actionBttn("go", label = "Let's start!", style = "simple", color = "default",size="lg"),
        )
      ) 
    )
    ),
  
  argonRow(
                argonInfoCard(value = n_distinct(data$freq$pmid),
                                title = ("Studies"),
                                icon = icon("file", "fa-solid"),
                                icon_background = "primary",
                                shadow = T,
                                width = 4), 
                argonInfoCard(value = CMS_text$text[ CMS_text$what == "sample_size"],
                              title = ("Sample size"),
                              icon = icon("user", "fa-solid"),
                              icon_background = "primary",
                              shadow = T,
                              width = 4),
                argonInfoCard(value = n_distinct(data$freq$ISO2[data$freq$ISO2 != "NA"]),
                              title = ("Countries"),
                              icon =  icon("map"),
                              icon_background = "primary",
                              shadow = T,
                              width = 4) 
       ),
    br(),
    argonRow(
                argonInfoCard(value = n_distinct(data$freq$gene),
                                title = ("Pharmacogenes"),
                                icon = icon("dna",class = "dna1"),
                                icon_background = "primary",
                                shadow = T,
                                width = 4
                                  ), 
                argonInfoCard(value = data$freq_per_country %>% bind_rows(data$freq_per_subgroup) %>% distinct(gene, Allele) %>% nrow(),
                              title = ("Alleles"),
                              icon = icon("arrows-up-to-line"),
                              icon_background = "primary",
                              shadow = T,
                              width = 4
                ),
                argonInfoCard(value = CMS_text$text[ CMS_text$what == "drug_size"],
                              title = ("App. affected drugs"),
                              icon =  icon("pills"),
                              icon_background = "primary",
                              shadow = T,
                              width = 4) 
       ),
  
  ), # home

  argonTabItem(
    tabName = "map",
    argonRow( 
    argonColumn(width = 3,
                htmltools::tags$div(class = "card shadow", 
                                   htmltools::tags$div(class = "card-body",
                pickerInput("gene", "Select pharmacogene",choices = data$genes, options = list(title = "Select one pharmacogene")),
                pickerInput("allele", "Select allele", choices =  NULL, 
                            options = pickerOptions(style = "class_allele", liveSearch = TRUE)),
                prettyRadioButtons("mapping_method", "Choose", choices = c("Geographical groups", "Countries")), 
                prettyRadioButtons("average", "Aggregation", choices = c("Weighted median", "Median"))                                       
                  )) # card body 
              ), #  column1
    argonColumn(width = 9,
                  argonTabSet(id = "tabset_wrld",
                              width = 12,
                              iconList = list(icon("globe"), icon("list-ul")),
                              card_wrapper = T,size = "lg",
                              argonTab(tabName = "Worldmap", active = T, 
                                       uiOutput("render_plots"),
                                       uiOutput("rrender_add_info")
                              ),
                              argonTab(tabName = "Data", active = F,
                                       p("Click on a row to load the individual raw data per country or region. All data can be downloaded by clicking the download button located at the bottom of the page."),
                                       DTOutput("DT_table") %>% withSpinner(color  = "#5e72e4") ,
                                      uiOutput("dt_dl_button")
                              )
                  )
                ) #  column2
    ) # row
  ),           

# tools -------------------------------------------------------------------
  argonTabItem(
    tabName = "tools",
  
    fluidRow(
      column(
        width = 12,
        argonTabSet(horizontal =T,  id = "tabset_tool",size = "sm",
                            width = 12,
                      iconList = list( argonIcon("single-02"), argonIcon("world"),
                                       argonIcon("chart-bar-32"),argonIcon("square-pin"),
                                       argonIcon("ambulance")),

# tool 1 -------------------------------------------------------------------
                           argonTab(tabName = "High-risk genotype frequency", active = T,
                                    
                                    argonCard(
                                      status = "primary",
                                      width = 12,
                                      border_level = 1,
                                      title = argonH1(display=4, "High-risk genotype frequency"),
                                      hover_lift = F,
                                      shadow = TRUE,
                                      shadow_size=4,
                                      hover_shadow =T,
                                      floating = F,
                                      icon = argonIcon("single-02"),
                                      argonRow(p("This tool can be used to estimate the proportion of individuals in a population or country harboring high-risk genomic variants for
                                                 selected pharmacogenes with established guidelines. To resize the plot, simply click and drag the bottom-right corner using your mouse.")),
                                      argonRow(
                                        argonColumn(width = 4,
                                                     pickerInput("tools_act_method", "Select method",width = "100%",choices = c("Dunnenberger (2015)", "CPIC function"),
                                                                selected = "CPIC function")
                                                    ),
                                        argonColumn(width = 4,
                                                    pickerInput("tools_act_mapping_method", "Choose", width = "100%",
                                                                       choices = c("Countries", "Geographical groups"), selected = "Geographical groups")),
                                        argonColumn(width = 4,
                                                    pickerInput("tools_act_countries", "Countries or regions", width = "100%", choices = "", multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T))  
                                                   
                                                    )
                                        
                                      ),
                                      argonRow(center =T, argonColumn(width = 12,center =T, 
                                                                      div(jqui_resizable(plotlyOutput("tool_act_plot", height = "500px", width = "95%")), align = "center") %>% 
                                                                      helper(type = "markdown",content = "TOOL1",size ="l", style = "color: deeppink; font-size: 30px;")
                                                                      )),
                                      br(),
                                      argonRow(argonColumn(width = 12,textOutput("tool_act_text"))),
                                      br(),
                                      argonRow(argonColumn(width = 12,DTOutput("tool_act_table"))),
                                      uiOutput("tool1_dl_button")
                                      
                                    )
                                    
                                    
                                    ),


# tool2 -------------------------------------------------------------------
                           argonTab(tabName = "Frequency comparison",
                                    
                                    argonCard(
                                      status = "primary",
                                      width = 12,
                                      border_level = 1,
                                      title = argonH1(display=4, "Frequency comparison"),
                                      hover_lift = F,
                                      shadow = TRUE,
                                      shadow_size=4,
                                      hover_shadow =T,
                                      floating = F,
                                      icon = argonIcon("world"),
                                      argonRow(p("With this tool, allele frequencies can be compared between ethnogeographic subgroups. Different regions, one gene, and corresponding multiple alleles can be selected. 
                                      The allele frequencies are summarized and displayed for each country. 
                                                 A boxplot visually represents the distribution of allele frequencies across selected regions, providing a clear comparison of variations between populations.
                                                 All data can be downloaded by clicking the download button located at the bottom of the page. To resize the plot, simply click and drag the bottom-right corner using your mouse.")),
                                      argonRow(
                                        argonColumn(width = 4,
                                                    pickerInput("tools_freq_countries", "Select regions", width = "100%", choices = data$freq_per_subgroup$subgroup %>% unique() %>% sort,
                                                                selected = data$freq_per_subgroup$subgroup %>% unique() %>% sort,
                                                                multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T))),
                                        argonColumn(width = 4,
                                                    pickerInput("tools_freq_gene", "Select a gene", width = "100%", choices = data$genes, select="CYP2D6",
                                                                multiple = F)),
                                        argonColumn(width = 4,
                                                    pickerInput("tools_freq_allele", "Select at least one allele", width = "100%", choices = "",
                                                                multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T)))
                                      ),
                                      argonRow(argonColumn(width = 12,center =T, div( jqui_resizable(plotlyOutput("tool_freq_plot", height = "600px", width = "95%")), align = "center") %>% 
                                                             helper(type = "markdown",content = "TOOL2",size ="l", style = "color: deeppink; font-size: 30px;"
                                                                    ))),
                                      br(),
                                      argonRow(argonColumn(width = 12,textOutput("tools_freq_text"))),
                                      br(),
                                      uiOutput("tool2_dl_button")
                                      
                                    )), 
# tools 3 freq comparison
                    argonTab(tabName = "Intra-biogeographic variability",
                             
                             argonCard(
                               status = "primary",
                               width = 12,
                               border_level = 1,
                               title = argonH1(display=4, "Intra-biogeographic variability"),
                               hover_lift = F,
                               shadow = TRUE,
                               shadow_size=4,
                               hover_shadow =T,
                               floating = F,
                               icon = argonIcon("world"),
                               argonRow(p("This analysis examines both intra- and interindividual differences in allele frequencies among ethnogeographic subgroups. 
                                          Furthermore, it highlights the frequency differences present within the selected subgroups, providing a detailed comparison of genetic variation across different populations. 
                                          All data can be downloaded by clicking the download button located at the bottom of the page. To resize the plot, simply click and drag the bottom-right corner using your mouse.")),
                               argonRow(
                                 argonColumn(width = 4,
                                             pickerInput("tools_var_gene", "Select a gene", width = "100%", choices = sort(unique(data$freq_per_country$gene)),
                                                         selected = "CYP2D6", multiple = F)),
                                 argonColumn(width = 4,
                                             pickerInput("tools_var_allele", "Select at least one allele", width = "100%", choices = "",
                                                         multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T)))
                               ),
                               # argonRow(argonColumn(width = 12,center =T, plotlyOutput("tool_var_plot", height = "600px", width = "95%"), align = "center")),
                               argonRow(argonColumn(width = 12,center =T,  div(jqui_resizable(plotlyOutput("tool_var_plot", height = "600px", width = "95%")), align ="center") %>% 
                                                      helper(type = "markdown",content = "TOOL3",size ="l", style = "color: deeppink; font-size: 30px;")
                                                    
                                                    ) # colum
                                        
                                        ),
                               br(),
                               argonRow(argonColumn(width = 12,textOutput("tool_var_text"))),
                               br(),
                               uiOutput("tool3_dl_button")
                               
                               
                               
                             )), # freq var

# tool4 tool 4 ------------------------------------------------------------
argonTab(tabName = "Country data tool",
         
         argonCard(
           status = "primary",
           width = 12,
           border_level = 1,
           title = argonH1(display=4, "Country data tool"),
           hover_lift = F,
           shadow = TRUE,
           shadow_size=4,
           hover_shadow =T,
           floating = F,
           icon = argonIcon("square-pin"),
           argonRow(p("With this tool, you can download all available allele frequency data for a selected gene and country or ethnic subgroups. Multiple countries/subgroups can be selected simultaneously. 
                      When several studies were available, the data was summarized by the median or wighted median (defualt selection). To download all data, please use the button located below the table")),
           argonRow(
             argonColumn(width = 3,
                         pickerInput("tools_var_contgene_gene", "Select a gene", width = "100%", choices = sort(unique(data$freq_per_country$gene)),
                                     selected = "CYP2D6", multiple = F,  options = pickerOptions(actionsBox=T, liveSearch = T))),
             argonColumn(width = 2, prettyRadioButtons("tools_var_contgene_mapping_method", "Choose", choices = c("Geographical groups", "Countries")),),
             
             argonColumn(width = 4,
                         pickerInput("tools_var_contgene_gene_country", "Select a country", width = "100%", choices = NA,
                                     multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T))),
             argonColumn(width = 2,prettyRadioButtons("tools_var_contgene_average", "Aggregation", choices = c("Weighted median", "Median"))  )
             
             
           ),
           argonRow(argonColumn(width = 12,DTOutput("tools_var_contgene_dt"))),
          uiOutput("tool4_dl_button")
           
           )    
                                    
                                    
                     ), #Country-gene filter
        
# tool5 tool 5 ------------------------------------------------------------
argonTab(tabName = "Metabolizer status tool",
         
         argonCard(
           status = "primary",
           width = 12,
           border_level = 1,
           title = argonH1(display=4, "Metabolizer status tool"),
           hover_lift = F,
           shadow = TRUE,
           shadow_size=4,
           hover_shadow =T,
           floating = F,
           icon = argonIcon("ambulance"),
           argonRow(p("This tool is designed to calculate the frequency of phenotypes (PM = Poor Metabolizer, IM = Intermediate Metabolizer, NM = Normal Metabolizer, UM = Ultra-rapid Metabolizer) 
           for pharmacogenes according to established guidelines. Countries are not listed in the selection when too many key alleles for the phenotypes are missing.  
                               Furthermore, certain variants of a single phenotype may be missing, leading to incomplete results.")
                    ), 
           
           argonRow(
             argonColumn(width = 3,
                         pickerInput("tools_var_function_gene", "Select a gene", width = "100%", choices = sort(unique(tool_function$gene )),
                                     selected = "CYP2D6", multiple = F)),
             argonColumn(width = 2, prettyRadioButtons("tools_var_mapping_method", "Choose", choices = c("Geographical groups", "Countries")),),
             argonColumn(width = 4,
                         pickerInput("tools_var_function_country", "Select a country", width = "100%", choices = NA,
                                     multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T)))
             # argonColumn(width = 4, prettyRadioButtons("tools_var_function_average", "Aggregation", choices = c("Weighted median", "Median"))  )
           ), 
           br(),
           argonRow(argonColumn(width = 12,center =T, div( jqui_resizable(plotOutput("tool5_plot", height = "600px", width = "100%")) %>% withSpinner(color  = "#5e72e4"), align = "center") %>% 
                                  helper(type = "markdown",content = "TOOL5",size ="l", style = "color: deeppink; font-size: 30px;"))), 
           br(),
           argonRow(argonColumn(width = 12,DTOutput("tool5_dt") %>%  withSpinner(color  = "#5e72e4"))),
           uiOutput("tool5_dl_button")
           
           

             
           
           
           
         )) # Functional frequency

        ))



)


         
  )
  
  ) #tabitems 
) #body







