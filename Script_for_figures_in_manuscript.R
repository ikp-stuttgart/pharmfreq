# Required packages
# functions
library(tidyverse)
# interface
library(fst)
library(readxl)
library(rvest)
# ggplot related
library(ggsci)
library(ggrepel)
library(ggpubr)
library(ggforce)
library(ggalluvial)
library(scatterpie)
library(patchwork)
library(cowplot)
# additional datasets
library(wpp2019)
library(WDI)
# Directory to save plots -----------------------------------------------------
dir_plots <- ""
# load recent data -------------------------------------------------------
data <- list()
data$freq <- fst::read.fst(path = "data/frequency_data_raw_2024-06-17.fst")
data$freq_per_country <- fst::read.fst("data/frequency_data_country_2024-06-17.fst")
data$freq_per_subgroup <- fst::read.fst("data/frequency_data_subgroup_2024-06-17.fst")
data$genes <- sort(unique(data$freq$gene))
data$functional_alleles <- fst::read.fst("data/functional_allele_information_2024-06-05.fst")  
data$mapping_country_subgroup <- data$freq_per_subgroup %>% separate_rows(ISO2, sep=", ") %>% filter(ISO2!= "No information available") %>% distinct(ISO2, subgroup)
data$haplotype_alleles <- fst::read.fst("data/haplotype_allele_information_2022-09-13.fst")
# ####################################### ---------------------------------
# Figure 1A ---------------------------------------------------------------
# ####################################### ---------------------------------
# Save the transformed data for the plot
data$data_for_Figure1 <- data$freq %>% 
  distinct(gene, Allele) %>% 
  left_join(data$functional_alleles) %>%
  count(gene, functional_status) %>% 
  mutate(functional_status_old=functional_status) %>% 
  # harmonize the functional status for nice plotting
  mutate(functional_status=ifelse(is.na(functional_status), "no CPIC function", functional_status)) %>%
  mutate(functional_status= case_when(grepl("deficient", functional_status) ~ "Loss of function",
                                      grepl("increased risk", functional_status) ~ "Increased function",
                                      grepl("increased warfarin", functional_status) ~ "Increased function",
                                      grepl("malignant", functional_status) ~ "Increased function",
                                      grepl("normal risk", functional_status) ~ "Normal function",
                                      grepl("iv/normal", functional_status) ~ "Normal function",
                                      grepl("normal warfarin", functional_status) ~ "Normal function",
                                      grepl("uncertain risk", functional_status) ~ "uncertain function",
                                      T ~ functional_status)) %>% 
  mutate(functional_status=fct_collapse(functional_status, `uncertain function` = c("no CPIC function","uncertain function", "unknown function"))) %>% 
  mutate(functional_status=fct_collapse(functional_status, `no function` = c("no function","Loss of function"))) %>% 
  mutate(functional_status =str_to_sentence(functional_status)) %>%
  mutate(functional_status = factor(functional_status,
                                    levels = c("No function", "Decreased function",  "Normal function", "Increased function", "Uncertain function"),
                                    labels = c("No function/LOF/Deficient", "Decreased function",  "Normal function", "Increased function/risk", "Uncertain function"))) %>% 
  mutate(functional_status = fct_rev(functional_status)) 

# (optional) check the translation 
data$data_for_Figure1 %>% count(functional_status, functional_status_old) 

# Create barplot and save as object p1
p1 <-
  data$data_for_Figure1 %>%
  ggplot(aes(reorder(gene, n, sum), n, fill=functional_status)) + 
   geom_col() + 
   scale_fill_manual("", values = rev(c("#B24168", "#C97A95", "steelblue", "mediumseagreen","darkgrey")))+
   scale_y_continuous(expand = c(0,0), breaks = seq(0,75,25))+
   theme_classic(base_size = 20) +
   coord_flip() +
   guides(fill = guide_legend(reverse=TRUE)) +
   labs(y="Number of alleles", x="") +
   expand_limits(x = 0, y = 0)+
   theme(legend.position = c(0.8,0.2),
         axis.ticks.y =element_blank(),
         axis.line.y = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor  = element_blank(), legend.margin = margin(0,0,0,-1.5,unit = "inches"))

# Figure 1A Inlet
p1_inlet <-
  data$data_for_Figure1 %>% 
  group_by(functional_status)  %>%
  summarise(n = sum(n)) %>%
  mutate(perc = n/sum(n)) %>%
  ggplot(aes("Function", perc, fill=functional_status)) + 
   coord_flip()+
   geom_col(show.legend = F, width = 0.5) + 
   scale_y_continuous(labels = scales::percent, breaks = c(0,1), expand = c(0,0)) + 
   geom_text(aes(x=1.4, label=glue::glue("{scales::percent(round(perc,3))}")), position = position_stack(vjust = 0.5), size = 7) +
    scale_fill_manual("", values = rev(c("#B24168", "#C97A95", "steelblue", "mediumseagreen","darkgrey")))+
   labs(x="",y="") +
   theme_classic(base_size = 20) +
    theme(legend.position = "bottom",
          axis.ticks.y =element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor  = element_blank(),
          aspect.ratio = 0.25)
# Final Figure 1A
cowplot::ggdraw(p1) +
  cowplot::draw_plot(p1_inlet, x= 0.2, vjust = 0.02, scale= 0.5) 
# Save Figure 1A
ggsave(paste0(dir_plots, "Figure_1A.pdf"), width = 11.69, height = 8.27)

# ####################################### ---------------------------------
# Figure 1B ---------------------------------------------------------------
# ####################################### ---------------------------------
data$freq %>% 
  distinct(gene, pmid) %>% 
  count(gene) %>% 
  mutate(value= n/sum(n)) %>% 
  arrange(value) %>% 
  mutate(gene=factor(gene, levels = unique(gene))) %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos)) %>% 
  ggplot(aes(x="", value, fill=gene)) + 
   geom_col(show.legend = F) + 
   coord_polar("y", start = 0) + 
   scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = rev(ggpubr::get_palette("Set2",19))) + 
    ggrepel::geom_label_repel(data=. %>% filter(value<0.1),aes(y = pos, label = glue::glue("{gene}\n{round(value*100,1)}%")),
                     size = 4.5, nudge_x = 1, show.legend = FALSE,segment.linetype=2,segment.size=1) +
   geom_text(data=. %>% filter(value>=0.1),aes(label=glue::glue("{gene}\n{round(value*100,0)}%")),position = position_stack(vjust = 0.5), 
             color="white",size=7, fontface="bold") + 
   theme_void(base_size = 16) 
# Save Figure 1B
ggsave(paste0(dir_plots, "Figure1B.pdf"), width = 8.27, height  = 11.69)

# ####################################### ---------------------------------
# Figure 1C ---------------------------------------------------------------
# ####################################### ---------------------------------
# helper function
n_fun <- function(x, y){
  return(data.frame(y=35,label = paste0("median= ",median(x, na.rm = T))))
}
# For these genes, only studies with NGS data are included in the database. Because, 
# therefore all alleles are covered equally, they are not shown.  
genes_to_remove <- c("NUDT15", "DPYD", "RYR1", "G6PD")

# transform data
data$data_Figure1C <- data$freq %>% 
  # remove wildtype alleles or alleles strongly related to *1 alleles.  
  filter(Allele != "*1") %>%
  filter(!(gene == "CYP2C19" & Allele == "*38")) %>% 
  filter(!(gene == "SLCO1B1" & Allele == "*37")) %>% 
  filter(!(gene == "VKORC1" & Allele == "rs9923231_reference_c")) %>% 
  filter(!gene %in% genes_to_remove) %>%
  # add color code as used in 1A for functional data
  left_join(
     left_join(data$functional_alleles, distinct(data$data_for_Figure1, func_new=functional_status,functional_status= functional_status_old))%>%
     select( gene, Allele, func_new,functional_status )
 ) %>% 
  as_tibble() %>% 
  mutate(Allele = gsub("_variant_t", "_T", Allele)) %>% 
  distinct(gene, pmid, Allele, func_new) %>% 
  count(gene, Allele, func_new, sort = T) %>% 
  # Plot onyl the top 5 tested allele
  group_by(gene) %>%
  mutate(top =  n %in% tail(sort(n), 5)) %>%
  mutate(Allele = ifelse(!top, "Other", Allele)) %>% 
  group_by(gene, Allele) %>% 
  summarise(n = sum(n),
            func_new = unique(toString(func_new))) %>% 
  mutate(batch  = ifelse(sum(n)>400,1,2)) %>% 
  ungroup() %>% 
  mutate(func_new = ifelse(Allele  == "Other",  "Other",  func_new)) %>% 
  mutate(func_new = ifelse(func_new == "NA", "Uncertain function", func_new)) %>% 
  mutate(Allele = fct_reorder(Allele, n)) %>%
  mutate(Allele = fct_relevel(Allele, "Other")) %>% 
  mutate(func_new = factor(func_new,
                                    levels = c("No function/LOF/Deficient", "Decreased function",  "Normal function", "Increased function/risk", "Uncertain function", "Other"),
                                    labels = c("No function/LOF/Deficient", "Decreased function",  "Normal function", "Increased function/risk", "Uncertain function", "Other"))) %>% 
  mutate(func_new = fct_rev(func_new)) 

# right plot
p1C_right <- data$data_Figure1C  %>% 
  ggplot(aes(reorder(gene, n, sum), n, group = Allele, fill = func_new)) + 
  # geom_col(fill="white", color = 1, show.legend = F)+
  geom_col(color=1,show.legend = F)+
  coord_flip() + 
  geom_text(aes(label=Allele), position = position_stack(vjust = 0.5),size=4)+
  ggpubr::theme_pubclean(base_size = 16)+
  scale_y_continuous() + 
  scale_fill_manual("", values = rev(c("#B24168", "#C97A95", "steelblue", "mediumseagreen","darkgrey", "yellow")))+
  labs(y="Number of studies genotyping the alleles", x="") +
  facet_wrap(~batch ,scales = "free", nrow = 2)+
  theme(strip.text = element_blank(),
        # axis.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p1C_right

# get gene order from p2
gene_order <- data_Figure1C %>% 
  group_by(gene, batch) %>% 
  summarise(n=sum(n)) %>% 
  arrange(-n)
# left plot 
p1C_left <- data$freq %>% 
  filter(Allele != "*1") %>% 
  filter(ISO2 !="NA") %>% 
  filter(!gene %in% genes_to_remove) %>%
  distinct(gene, pmid, Allele) %>%   
  count(gene, pmid, sort = T) %>% 
  mutate(gene = fct_relevel(gene, gene_order$gene[!gene_order$gene %in% genes_to_remove])) %>%
  left_join(data_Figure1C %>% distinct(gene, batch)) %>% 
  left_join(data_Figure1C %>% 
              group_by(gene) %>% 
              summarise(n_sort=sum(n)) %>% 
              arrange(-n_sort)) %>% 
  ggplot( aes(reorder(gene, n_sort, unique), n)) + 
  geom_boxplot() + 
  ggbeeswarm::geom_quasirandom() +
  stat_summary(fun.data = n_fun, geom = "label", size=5, fill="white")+
  coord_flip()+
  # ylim(0,55) +
  ylab("Number of alleles genotyped per study")+
  ggpubr::theme_pubclean(base_size = 16)+
  facet_wrap(~batch ,scales = "free", nrow = 2)+
  theme(strip.text = element_blank(),
        # axis.title = element_blank(),
        axis.title.y = element_blank(),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
# final plot 1C
p1C_left + p1C_right + plot_layout(widths = c(1, 2))
ggsave(paste0(dir_plots, "Figure_1C.pdf"), height = 12, width  = 16)


# ####################################### ---------------------------------
# Figure 4A ---------------------------------------------------------------
# ####################################### ---------------------------------
data$freq %>% 
  distinct(pmid, subgroup) %>% 
  count(subgroup) %>% 
  mutate(value=n/sum(n)) %>% 
  arrange(value) %>% 
  mutate(subgroup=factor(subgroup, levels = unique(subgroup))) %>% 
  ggplot(aes("", value, fill=subgroup)) + 
  geom_col(show.legend = F) + 
  scale_y_continuous(labels = scales::percent) + 
  ggsci::scale_fill_rickandmorty()+
  geom_text(aes(label=glue::glue("{subgroup} ({round(value*100,1)}%)")), position = position_stack(vjust = 0.5), size=8)+
  ggpubr::theme_pubclean(base_size = 24)+
  labs(x="",y="Distribution of studies across\ndifferent populations") + 
  theme(axis.ticks = element_blank()) 
ggsave(paste0(dir_plots, "Figure4A.pdf"), width = 8.27, height  = 14)

# ####################################### ---------------------------------
# Figure 4B ---------------------------------------------------------------
# ####################################### ---------------------------------
world<- map_data("world")
# studies per gene and subgroup
data$data_on_studies_per_gg <- data$freq %>%
  distinct(pmid, subgroup, gene) %>%
  count(subgroup, gene) %>%
  group_by(subgroup) %>%
  mutate(value = n/sum(n)) %>%
  ungroup()


# data for scatter plot
data_on_studies_per_gg_scatter <- data$data_on_studies_per_gg %>% 
  select(-n) %>% 
  pivot_wider(names_from = gene, values_from = value, values_fill = 0) %>%  
  mutate(long = c(70.491372, 123.26095, 28.486746, 4.564534, -100.4821, 150.208591, -55.944591, 24.968774),
         lat =  c(28.381773, 26.509943, 59.459973, 30.018385, 40.869262, -32.076357, -18.117996, -8.552037),
         radius=16
  ) 
# world map
ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="lightgray",color="black") +
  coord_quickmap() + 
  theme_void()+
  geom_scatterpie(aes(x=long, y=lat-15, group=subgroup, r=radius),
                  data=data_on_studies_per_gg_scatter, 
                  cols=names(data_on_studies_per_gg_scatter)[2:20], color=NA)+
  geom_label(aes(x=long, y=lat, label=subgroup),  data=data_on_studies_per_gg_scatter)+
  scale_fill_manual("",values = c(ggsci::pal_locuszoom(palette = "default")(7),
                                  ggsci::pal_jama(palette = "default")(7)[-2],
                                  ggsci::pal_futurama(palette = "planetexpress")(6)
  )) +
  theme(legend.position = "bottom")
ggsave(paste0(dir_plots, "Figure4B.pdf"), height = 8.27, width  = 11.69)

# ####################################### ---------------------------------
# Figure 4C ---------------------------------------------------------------
# ####################################### ---------------------------------
data$drug_gene_table <- read_excel("../pharmfreq_pub/data/gene_drug_table_pharmgkb.xlsx", 1)

# scale n
tot <- data$data_on_studies_per_gg %>% 
  select(gene, subgroup, n) %>% 
  group_by(gene) %>% 
  # mutate(n_total = sum(n)) %>% 
  # filter(n<10) %>%
  summarise(total = sum(n)) %>%  
  filter(total>5) 


ta_transform <- data$drug_gene_table %>% 
  count(gene, therapeutic_area) %>% 
  inner_join(tot) %>% 
  group_by(gene) %>% 
  mutate(perc = total/sum(n)) %>% 
  mutate(n2 =n*perc) %>% 
  select(gene, therapeutic_area, n2)


p_right <- ta_transform %>% 
  ggplot(aes(y = n2, axis2 = therapeutic_area, axis1 = gene)) +
  geom_alluvium(aes(fill = gene), width = 1/20, alpha=0.8, show.legend = F)+
  geom_stratum(aes(fill = gene) , width = 1/20, , show.legend = F)+
  geom_label(
    aes(label = ifelse(grepl(paste(tot$gene, collapse="|"),after_stat(stratum)), NA, as.character(after_stat(stratum)))),
    stat = "stratum", size = 4, hjust=1, nudge_x = 0.01) + 
  theme_void()+
  scale_fill_manual("",values = c(ggsci::pal_locuszoom(palette = "default")(7),
                                  ggsci::pal_jama(palette = "default")(7)[-2],
                                  ggsci::pal_futurama(palette = "planetexpress")(6)
  )) 

p_left <-  data$data_on_studies_per_gg %>% 
  group_by(gene) %>% 
  # mutate(n_total = sum(n)) %>% 
  # filter(n<10) %>%
  mutate(total = sum(n)) %>%  
  filter(total>5) %>% 
  ungroup() %>%
  inner_join(tot) %>%
  ggplot(aes(y = n, axis1 = subgroup, axis2 = gene)) +
  geom_alluvium(aes(fill = gene), width = 1/20, alpha=0.8,show.legend = F)+
  geom_stratum(width = 1/20,aes(fill = gene),show.legend = F)+
  geom_label(
    aes(label = ifelse(grepl(paste(tot$gene, collapse="|"), after_stat(stratum)), NA, as.character(after_stat(stratum)))),
    stat = "stratum", size = 4, hjust=0) + 
  ggrepel::geom_label_repel(
    aes(label = ifelse(!grepl(paste(tot$gene, collapse="|"),after_stat(stratum)), NA, as.character(after_stat(stratum)))),
    stat = "stratum", size = 4, hjust=0.5) + 
  theme_void()+
  scale_fill_manual("",values = c(ggsci::pal_locuszoom(palette = "default")(7),
                                  ggsci::pal_jama(palette = "default")(7)[-2],
                                  ggsci::pal_futurama(palette = "planetexpress")(6)
  ))
cowplot::plot_grid(p_left, p_right)
ggsave(paste0(dir_plots, "Figure4C_all_genes.pdf"), height = 8.27, width  = 11.69)
# The figure is then finally styled using software like inkscape or illustrator

# ####################################### ---------------------------------
# Figure 5A ---------------------------------------------------------------
# ####################################### ---------------------------------
# load population data 
data(pop)
pop_2020 <- pop %>% 
  as_tibble() %>%
  janitor::clean_names() %>% 
  select(country=name, NUMC=country_code,x2020)

# load ISO2 to ISO3 translation table
ISO2_to_ISO3 <- rvest::read_html("https://en.wikipedia.org/wiki/ISO_3166-1") %>%
  rvest::html_node(".mediawiki") %>%   
  rvest::html_table() %>% 
  slice(-1:-8) %>% 
  select("country"=1, "ISO2"=2, "ISO3"=3, NUMC = 4) %>% 
  mutate(NUMC=as.numeric(NUMC))

# load gdp
# WDI::WDIsearch('gdp') %>% as_tibble() %>% filter(grepl("NY.GDP.PCAP.KD",indicator))
gdp <- WDI::WDI(indicator='NY.GDP.PCAP.KD',
                country = "all",
                start=2020) %>% 
  filter(!is.na(NY.GDP.PCAP.KD)) %>% 
  group_by(iso2c) %>% 
  top_n(year, n=1)

# plot
data$data_for_Figure5A <- data$freq %>% 
  filter(ISO2!="NA") %>% 
  distinct(pmid, ISO2, country) %>% 
  count(country, ISO2, sort=T) %>% 
  left_join(select(ISO2_to_ISO3,-country), by="ISO2") %>% 
  left_join(select(pop_2020,-country), by="NUMC") %>% 
  left_join(select(gdp, ISO2=iso2c, GDP=NY.GDP.PCAP.KD)) %>%
  mutate(GDP = ifelse(is.na(GDP), min(GDP, na.rm=T), GDP)) %>%  
  mutate(x2020= x2020/1000) %>% 
  filter(n>1) 
  
data$data_for_Figure5A %>% 
  ggplot(aes(x2020, n)) + 
  geom_point(size=5, alpha= 0.5, color="steelblue") + 
  scale_x_log10("Population estimate 2020 (in millions)",label = scales::number) + 
  scale_y_log10("Number of studies included in Pharmfreq") + 
  ggrepel::geom_text_repel(aes(label=country)) + 
  ggpubr::theme_pubclean(base_size = 16)+
  ggpubr::stat_cor(method = "sp", size=8)
ggsave(paste0(dir_plots, "Figure5A.pdf"), height = 8.27, width  = 11.69)

# ####################################### ---------------------------------
# Figure 5B ---------------------------------------------------------------
# ####################################### ---------------------------------
data$freq %>% 
  as_tibble() %>% 
  filter(ISO2!="NA") %>% 
  # filter on studies, removing allel information 
  distinct(pmid, ISO2, country, subgroup, n) %>% 
  # remove studies with almost identical sample size to avoid aggregating study cohorts twice
  group_by(country, ISO2, subgroup) %>% 
  mutate(bin=.bincode(n, seq(min(n),max(n)+5, 5), include.lowest = T)) %>%  
  group_by(country, ISO2, subgroup, bin) %>% 
  slice(1) %>% 
  # aggregate sample size
  group_by(country, ISO2, subgroup) %>% 
  summarise(total = sum(n, na.rm = T)) %>%  
  ungroup() %>% 
  # add population data
  left_join(select(ISO2_to_ISO3,-country), by="ISO2") %>% 
  left_join(select(pop_2020,-country), by="NUMC") %>% 
  # calculate fraction
  mutate(fraction = total/(x2020*1000)) %>% 
  filter(!is.na(fraction)) %>%  
  # finalizing
  mutate(subgroup = gsub(" & ", "/ ",subgroup)) %>% 
  mutate(subgroup =str_wrap(subgroup, 15)) %>% 
  # plot
  ggplot(aes(subgroup, fraction)) +
  geom_boxplot(outlier.color=NA, show.legend = F)+
  ggbeeswarm::geom_beeswarm(size=4, alpha = 0.8, aes(color = subgroup), show.legend = F)+
  scale_y_log10("Fraction of population covered", label  = function (x) paste(x*100,"%"))  +
  # Highlight top and flops
  ggrepel::geom_text_repel(data = . %>% group_by(subgroup) %>% top_n(fraction, n=3), aes(label=country), show.legend = F) +
  ggrepel::geom_text_repel(data = . %>% group_by(subgroup) %>% top_n(-fraction, n=3) %>% 
                             filter(!country %in% c("Mexico", "USA","Pakistan","Canada", "India","Sri Lanka")), 
                           aes(label=country), show.legend = F) +
  ggsci::scale_color_rickandmorty()+ 
  ggpubr::theme_pubclean(base_size = 16) +
  labs(x="") 
ggsave(paste0(dir_plots, "Figure5B.pdf"), height = 8.27, width  = 13) 
  
# inlet
data$freq %>% 
  as_tibble() %>% 
  filter(ISO2!="NA") %>% 
  # filter on studies, removing allel information 
  distinct(pmid, ISO2, country, subgroup, n) %>% 
  # remove studies with almost identical sample size to avoid aggregating study cohorts twice
  group_by(country, ISO2, subgroup) %>% 
  mutate(bin=.bincode(n, seq(min(n),max(n)+5, 5), include.lowest = T)) %>%  
  group_by(country, ISO2, subgroup, bin) %>% 
  slice(1) %>% 
  # aggregate sample size
  group_by(country, ISO2, subgroup) %>% 
  summarise(total = sum(n, na.rm = T)) %>% 
  ungroup() %>% 
  # add population data
  left_join(select(ISO2_to_ISO3,-country), by="ISO2") %>% 
  left_join(select(pop_2020,-country), by="NUMC") %>% 
  # summarize by subgroup
  group_by(subgroup) %>% 
  mutate(total_sub = sum(unique(x2020), na.rm=T)*1000) %>% 
  summarise(total = sum(total, na.rm = T),
            total_sub =unique(total_sub)) %>% 
  mutate(fraction = total/(total_sub)) %>% 
  mutate(subgroup =str_wrap(subgroup, 10)) %>% 
  ggplot(aes("Ethnicity", fraction, color = subgroup)) +
  ggbeeswarm::geom_quasirandom(show.legend = F, size = 5, alpha=0.8)+
  ggsci::scale_color_rickandmorty()+
  ggrepel::geom_label_repel(aes(label = subgroup),show.legend = F, position = ggbeeswarm::position_quasirandom())+
  scale_y_log10("", label  = function (x) paste(x*100,"%"))  +
  labs(x="")  + 
  theme_bw()
ggsave(paste0(dir_plots, "Figure5B_inlet.pdf"), height = 8.27, width  = 5)

# ####################################### ---------------------------------
# Figure 5C ---------------------------------------------------------------
# ####################################### ---------------------------------
data$freq %>% 
  as_tibble() %>% 
  filter(ISO2!="NA") %>% 
  # filter on studies, removing allel information 
  distinct(pmid, ISO2, country, subgroup, n) %>% 
  # remove studies with almost identical sample size to avoid aggregating study cohorts twice
  group_by(country, ISO2, subgroup) %>% 
  mutate(bin=.bincode(n, seq(min(n),max(n)+5, 5), include.lowest = T)) %>%  
  group_by(country, ISO2, subgroup, bin) %>% 
  slice(1) %>% 
  # aggregate sample size
  group_by(country, ISO2, subgroup) %>% 
  summarise(total = sum(n, na.rm = T)) %>%  
  ungroup() %>% 
  left_join(select(ISO2_to_ISO3,-country), by="ISO2") %>% 
  left_join(select(pop_2020,-country), by="NUMC") %>% 
  left_join(select(gdp, ISO2=iso2c, GDP=NY.GDP.PCAP.KD)) %>% 
  filter(!is.na(GDP)) %>% 
  # left join the study number
  inner_join(distinct(data$data_for_Figure5A, ISO2, n),by = join_by(ISO2)) %>%
  # finalization
  mutate(total= total/1000) %>%
  ggplot(aes(GDP, total)) + 
  geom_point(aes(size =n ),alpha= 0.5, color="red") + 
  scale_x_log10("GDP per capita (constant 2000 US$)",label = scales::number) + 
  scale_y_log10("Aggregated total cohort size in PharmFreq (in thousands)",label = scales::number, breaks = c(1,10,100,1000,10000)) + 
  scale_size_continuous("Number of studies",range = c(4, 14), breaks  =c(1,10,25,50,150,200), limits=c(0,200))+
  ggrepel::geom_text_repel(aes(label=country)) + 
  ggpubr::theme_pubclean(base_size = 16)+
  ggpubr::stat_cor(method = "sp", size=8) +
  theme(legend.position = c(0.2,0.8),
        legend.direction = "horizontal") 
ggsave(paste0(dir_plots, "Figure5C.pdf"), height = 8.27, width  = 12)


# ####################################### ---------------------------------
# End ---------------------------------------------------------------------
# ####################################### ---------------------------------
