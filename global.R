#  NEEDED: R version 4.3.0 --------------------------------------------------------
VERSION <- "Pharmfreq_v1.0.0"

# ################## ------------------------------------------------------
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shinyhelper))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinydashboardPlus))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(shinyvalidate))
suppressPackageStartupMessages(library(dashboardthemes))
suppressPackageStartupMessages(library(faq))
suppressPackageStartupMessages(library(googledrive))
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
suppressPackageStartupMessages(library(ggbeeswarm))
if(!webshot::is_phantomjs_installed()){webshot::install_phantomjs()}
Sys.setenv(OPENSSL_CONF="/dev/null")
# text --------------------------------------------------------------------
CMS_text <- structure(list(what = c("Tool1", "Tool2", "Tool3", "intro_text", 
                                    "sample_size", "drug_size"), text = c("Frequency (%)", "Frequency (%)", 
                                                                          "Frequency (%)", "The <strong>Pharm</strong>acogenetic allele <strong>Freq</strong>uency (<strong>PharmFreq</strong>) database is a collated repository and search tool for country-specific pharmacogenetic allele frequency at a global scale. It allows users to explore the ethnogeographic variability of genetically encoded drug response differences and identify outlier populations that might benefit from therapeutic adjustments.", 
                                                                          ">10 Mio.", "150")), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                                                                                                                       -6L))
# read drug information ---------------------------------------------------
gene_drugs <- readRDS("data/gene_drugs_15.10.2024.RDS")

# Data --------------------------------------------------------------------
COLORS <- viridis::viridis(4, direction = 1) %>% gsub("FF", "",.)
COLORS_40 <- c("#000000FF", "#333333FF", "#4D4D4DFF", "#666666FF", "#7F7F7FFF", "#999999FF", "#B3B3B3FF", "#E5E5E5FF",
               "#F5F5F5FF",  "#FFFFFFFF", "#27408BFF", "#000080FF", "#0000FFFF", "#1E90FFFF", "#63B8FFFF", "#97FFFFFF", "#00FFFFFF",
               "#00868BFF", "#008B45FF", "#458B00FF", "#008B00FF", "#00FF00FF", "#7FFF00FF", "#54FF9FFF", "#00FF7FFF",
               "#7FFFD4FF", "#8B4500FF", "#8B0000FF", "#FF0000FF", "#FF6A6AFF", "#FF7F00FF", "#FFFF00FF", "#FFF68FFF",
               "#F4A460FF", "#551A8BFF", "#8B008BFF", "#8B0A50FF", "#9400D3FF", "#FF00FFFF", "#FF1493FF")
# Map settings ------------------------------------------------------------
# region definitions according https://unstats.un.org/unsd/methodology/m49/
df_regions <- tibble(region=c("World", "Africa", "Europe" ,"Asia", "Northern America", "South America","Oceania", "Americas","Western Asia"), 
                     code = c("world", "002", "150", "142", "021", "005","009","019","145")) %>% 
  mutate(index=c(1, rep(2,n()-1))) %>% 
  arrange(index, region) 

data <- list()
data$freq <- fst::read.fst(path = "data/frequency_data_raw_2024-09-29.fst") %>% filter(!(grepl("ref|Ref", Allele) | Allele =="*1")) 
data$freq_per_country <- fst::read.fst("data/frequency_data_country_2024-09-29.fst") 
data$freq_per_subgroup <- fst::read.fst("data/frequency_data_subgroup_2024-09-29.fst") 
data$genes <- sort(unique(data$freq$gene))
data$functional_alleles <- fst::read.fst("data/functional_allele_information_2024-09-28.fst") %>% 
  mutate(Allele = case_when(gene == "G6PD" & Allele %in%  c("A- 202A_376G") ~ "A-",TRUE ~Allele))
data$mapping_country_subgroup <- data$freq_per_subgroup %>% separate_rows(ISO2, sep=", ") %>% filter(ISO2!= "No information available") %>% distinct(ISO2, subgroup)
data$haplotype_alleles <- readRDS("data/haplotype_allele_information_2024-09-28.RDS") %>% map(filter, !is.na(rs_id))
# tool5 data ---------------------------------------------------------------
tool_function <- list(
  CYP2B6 = "*6",
  CYP2C9 =  c("*2", "*3"),
  CYP2C19 = c("*2", "*3", "*17"),
  CYP2D6 = c("*4", "*10"),
  CYP3A5= "*3",
  UGT1A1 = "*28"
) %>% 
  enframe() %>% 
  unnest(value) %>% 
  select(gene=1, Allele=2) %>% 
  add_count(gene, name = "n_allele") %>% 
  group_by(gene) %>% 
  mutate(n_string = toString(sort(unique(Allele)))) %>% 
  ungroup()
#  submodules & modules ----------------------------------------------------------------
source("modules/submodules/head.R")
source("modules/body.R")
source("modules/footer.R")
source("modules/header.R")
source("modules/sidebar.R")
source("foo_functionality.R")
# functions ---------------------------------------------------------------
make_pmid_link <-Vectorize( function(y){
  map(strsplit(gsub("PMID:","",y), ", ")[[1]], function(x){
    if(x=="NA"|is.na(x)|x==""){
      "NA"
    }else if((grepl("[letters]", x)|(str_count(x, "[0-9]")<5))){
      
      if(grepl("gnomAD", x)){
        glue('<a href="https://gnomad.broadinstitute.org" target="_blank" rel="noopener noreferrer">{x}</a>') 
      }else{
      glue('<a href="{x}" target="_blank" rel="noopener noreferrer">{x}</a>')
      }
        
    }else{
      glue('<a href="https://pubmed.ncbi.nlm.nih.gov/{x}" target="_blank" rel="noopener noreferrer">{x}</a>')
    }}) %>% toString()
})

# leaflet -----------------------------------------------------------------
myspdf_regions <- readRDS("data/myspdf_region.RDS")
myspdf <- readRDS("data/myspdf.RDS")
leaflet_tiles <- providers[c("OpenStreetMap", "Esri", "OpenStreetMap.DE", "OpenStreetMap.France", "Esri.WorldGrayCanvas","Esri.NatGeoWorldMap")]
names(leaflet_tiles) <- c("OpenStreetMap","Esri (English)" , "OpenStreetMap (German)","OpenStreetMap (French)" , "Esri (Gray)", "Esri (NatGeo)")

# leaflet -----------------------------------------------------------------
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



# tools -------------------------------------------------------------------
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

foo_allele_sort <- function(x, PATTERN){
  c(grep(PATTERN, x), grep(PATTERN, x, invert = T))
}


