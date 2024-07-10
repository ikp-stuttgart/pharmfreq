# Author: Roman Tremmel
# Code for www.Pharmfreq.com
# PLease cite XXXX when you are using code or data

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
        argonColumn(width = 10,
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
                argonInfoCard(value = ">6 Mio.",
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
                argonInfoCard(value = n_distinct(data$freq$Allele),
                              title = ("Alleles"),
                              icon = icon("arrows-up-to-line"),
                              icon_background = "primary",
                              shadow = T,
                              width = 4
                ),
                argonInfoCard(value = 150,
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
                pickerInput("gene", "Select pharmacogene",choices = data$genes, options = list(title = "Select a gene")),
                pickerInput("allele", "Select allele", choices =  NULL, 
                            options = list(style = "class_allele")),
                prettyRadioButtons("mapping_method", "Choose", choices = c("Countries", "Geographical groups")),
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
                                       p("Click on a row to load the individual raw data per country or region."),
                                       DTOutput("DT_table") %>% withSpinner(color  = "#5e72e4")
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
                                       argonIcon("chart-bar-32")),

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
                                      argonRow(
                                        argonColumn(width = 4,
                                                    pickerInput("tools_act_method", "Select method",width = "100%",choices = c("Dunnenberger (2015)", "CPIC function"))),
                                        argonColumn(width = 4,
                                                    pickerInput("tools_act_mapping_method", "Choose", width = "100%",
                                                                       choices = c("Countries", "Geographical groups"), selected = "Geographical groups")),
                                        argonColumn(width = 4,
                                                    pickerInput("tools_act_countries", "Countries or regions", width = "100%", choices = "", multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T)))
                                        
                                      ),
                                      argonRow(center =T, argonColumn(width = 12,center =T, div(jqui_resizable(plotlyOutput("tool_act_plot", height = "500px", width = "90%")), align = "center"))),
                                      br(),
                                      argonRow(argonColumn(width = 12,textOutput("tool_act_text"))),
                                      br(),
                                      argonRow(argonColumn(width = 12,DTOutput("tool_act_table")))
                                      
                                    )
                                    
                                    
                                    ),
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
                                      argonRow(argonColumn(width = 12,center =T, div( jqui_resizable(plotlyOutput("tool_freq_plot", height = "600px", width = "85%")), align = "center"))),
                                      br(),
                                      argonRow(argonColumn(width = 12,textOutput("tools_freq_text"))),
                                      
                                      
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
                               argonRow(
                                 argonColumn(width = 4,
                                             pickerInput("tools_var_gene", "Select a gene", width = "100%", choices = sort(unique(data$freq_per_country$gene)),
                                                         selected = "CYP2D6", multiple = F)),
                                 argonColumn(width = 4,
                                             pickerInput("tools_var_allele", "Select at least one allele", width = "100%", choices = "",
                                                         multiple = T, options = pickerOptions(actionsBox=T, liveSearch = T)))
                               ),
                               argonRow(argonColumn(width = 12,center =T, div( jqui_resizable(plotlyOutput("tool_var_plot", height = "600px", width = "85%")), align = "center"))),
                               br(),
                               argonRow(argonColumn(width = 12,textOutput("tool_var_text")))
                               
                               
                               
                             )) # freq var
                                    
                                    
                                    
                     )
        
        )), 
  )
  ) #tabitems 
) #body

