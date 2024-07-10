# Author: Roman Tremmel
# Code for www.Pharmfreq.com
# PLease cite XXXX when you are using code or data
#  NEEDED: R version 4.3.0 --------------------------------------------------------
VERSION <- "Pharmfreq_v1.0"
################### ------------------------------------------------------
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(colourpicker))  
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinydashboardPlus))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(shinyvalidate))
suppressPackageStartupMessages(library(dashboardthemes))
suppressPackageStartupMessages(library(awn))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(fst))
suppressPackageStartupMessages(library(gtools))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(shinyscreenshot))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(argonR))
suppressPackageStartupMessages(library(argonDash))
suppressPackageStartupMessages(library(shinyalert))
suppressPackageStartupMessages(library(webshot))
suppressPackageStartupMessages(library(googlesheets4))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(shinyjqui))
suppressPackageStartupMessages(library(markdown))
if(!webshot::is_phantomjs_installed()){webshot::install_phantomjs()}
Sys.setenv(OPENSSL_CONF="/dev/null")

################### ------------------------------------------------------
################### ------------------------------------------------------
################### ------------------------------------------------------
# DISCLAIMER !!!!! -------------------------------------------------------
# This free version misses contact and FAQ sections since personal credentials are needed 
# to set up these functionalities. Therefore, some text elements are missing or not up to date compared to the website.
################### ------------------------------------------------------
################### ------------------------------------------------------
################### ------------------------------------------------------
CMS_text <- structure(list(what = c("Tool1", "Tool2", "Tool3", "intro_text"), text = c("Individuals affected (%)", "Individuals affected (%)", 
                                    "Individuals affected (%)", "The <strong>Pharm</strong>acogenetic allele <strong>Freq</strong>uency (<strong>PharmFreq</strong>) database 
                                    is a collated repository and search tool for country-specific pharmacogenetic allele frequency at a global scale. It allows users to explore 
                                    the ethnogeographic variability of genetically encoded drug response differences and identify outlier populations that might benefit from therapeutic adjustments.")), class = "data.frame", row.names = c(NA, -4L))
# Data ------------------------------------------------------------
data <- list()
data$freq <- fst::read.fst(path = "data/frequency_data_raw_2024-06-17.fst")
data$freq_per_country <- fst::read.fst("data/frequency_data_country_2024-06-17.fst")
data$freq_per_subgroup <- fst::read.fst("data/frequency_data_subgroup_2024-06-17.fst")
data$genes <- sort(unique(data$freq$gene))
data$functional_alleles <- fst::read.fst("data/functional_allele_information_2024-06-05.fst")  
data$mapping_country_subgroup <- data$freq_per_subgroup %>% separate_rows(ISO2, sep=", ") %>% filter(ISO2!= "No information available") %>% distinct(ISO2, subgroup)
data$haplotype_alleles <- fst::read.fst("data/haplotype_allele_information_2022-09-13.fst")
# Submodules ------------------------------------------------------------
source("modules/submodules/head.R")
source("modules/body.R")
source("modules/footer.R")
source("modules/header.R")
source("modules/sidebar.R")
# helper functions ---------------------------------------------------------------
make_pmid_link <-Vectorize( function(y){
  map(strsplit(y, ", ")[[1]], function(x){
    if(grepl("[letters]", x)){
      glue('<a href="{x}">{x}</a>')
    }else{
      glue('<a href="https://pubmed.ncbi.nlm.nih.gov/{x}" target="_blank">{x}</a>')
    }}) %>% toString()
})
# leaflet data -----------------------------------------------------------------
myspdf_regions <- readRDS("data/myspdf_region.RDS")
myspdf <- readRDS("data/myspdf.RDS")
leaflet_tiles <- providers[c("OpenStreetMap", "Esri", "OpenStreetMap.DE", "OpenStreetMap.France", "Esri.WorldGrayCanvas","Esri.NatGeoWorldMap")]
names(leaflet_tiles) <- c("OpenStreetMap","Esri (English)" , "OpenStreetMap (German)","OpenStreetMap (French)" , "Esri (Gray)", "Esri (NatGeo)")
# leaflet main plot -----------------------------------------------------------------
leaflet_map <- leaflet() %>% leaflet::setView(lat = 20, lng = 0, zoom = 2) %>%
  addEasyButton(easyButton(
    icon = "fa-globe", title = "Reset Zoom",
    onClick = JS("function(btn, map){ map.setView([20, 0], 2);}"))) %>%
  addEasyButton(easyButton(
    icon = "fa-crosshairs", title = "Locate Me",
    onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 5});}"))) %>% 
  addMiniMap(toggleDisplay = TRUE,tiles = leaflet_tiles[[1]], minimized=T) %>%
  htmlwidgets::onRender("
          function(el, x) {
            var myMap = this;
            myMap.on('baselayerchange',
              function (e) {
                myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
              })
          }") %>% 
  htmlwidgets::onRender("
        function(el, x) {
          this.doubleClickZoom.disable();
        }
      ") %>% 
  addEasyButton(easyButton(
    icon = "fa-solid fa-chart-bar",
    title = "Toggle Legend",
    onClick = JS("
          function(btn, map) {
            var legend = document.querySelector('.info.legend.leaflet-control');
            if (legend) {
              if (legend.style.display === 'none') {
                legend.style.display = 'block';
              } else {
                legend.style.display = 'none';
              }
            }
          }
        ")
  ))


for (i in seq_along(leaflet_tiles)) {
  leaflet_map <- leaflet_map %>% addProviderTiles(leaflet_tiles[[i]], group = names(leaflet_tiles)[i])
}
leaflet_map <- leaflet_map %>% addLayersControl(baseGroups = names(leaflet_tiles),
                                                options = layersControlOptions(collapsed = TRUE))

# tools; required data-------------------------------------------------------------------
dunnenberger <- structure(list(gene = c("CYP2C19", "CYP2C19", "CYP2C19", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", "CYP2D6", 
                                        "CYP2D6", "CYP2D6", "CYP2D6", "CYP2C9", "CYP2C9", "CYP3A5", "DPYD", 
                                        "DPYD", "DPYD", "HLA-B", "HLA-B", "SLCO1B1", "SLCO1B1", "TPMT", 
                                        "TPMT", "TPMT", "TPMT", "TPMT", "TPMT", "UGT1A1", "VKORC1"), 
                               Allele = c("*2", "*3", "*17", "*10", "*100", "*101", "*10x2", 
                                          "*11", "*114", "*12", "*120", "*124", "*129", "*13", "*14", 
                                          "*15", "*17", "*18", "*19", "*1x2", "*20", "*21", "*29", 
                                          "*2x2", "*3", "*31", "*35x2", "*36", "*36x2", "*38", "*3x2", 
                                          "*4", "*40", "*41", "*41x3", "*42", "*44", "*45x2", "*47", 
                                          "*49", "*4x2", "*4x>3", "*5", "*50", "*51", "*54", "*55", 
                                          "*56", "*57", "*59", "*6", "*60", "*62", "*68", "*69", "*6x2", 
                                          "*7", "*8", "*81", "*9", "*92", "*96", "*99", "*2", "*3", 
                                          "*1", "c.1905+1G>A (*2A)", "c.1129-5923C>G, c.1236G>A (HapB3)", 
                                          "c.2846A>T", "*57:01", "*58:01", "*5", "*15", "*2", "*3A", 
                                          "*3B", "*3C", "*4", "*8", "*28", "rs9923231_variant_t"), 
                               type = c("actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable", "actionable", "actionable", "actionable", 
                                        "actionable", "actionable")), row.names = c(NA, -81L), class = c("tbl_df", 
                                                                                                         "tbl", "data.frame"))
pharmvar <- 
  data$functional_alleles %>% 
  as_tibble() %>%
  filter(case_when(gene=="CYP3A5" ~ functional_status == "normal function", 
                    TRUE ~ functional_status %in% c("no function", "increased function", "decreased function"))) %>% 
  select(gene, Allele) %>% 
  mutate(type = "actionable")
remove_boxplot_outliers <- function(fig){
  stopifnot("plotly" %in% class(fig))
  fig$x$data <- lapply(
    fig$x$data,
    \(i){
      if(i$type != "box") return(i)
      i$marker = list(opacity = 0)
      i$hoverinfo = "none"
      i
    }
  )
  fig
}