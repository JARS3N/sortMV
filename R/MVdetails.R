MVdetails<-function(xml){
options(warn=-1)
suppressMessages(require(dplyr))
require(xml2)
require(rvest)
  cleansplit<-function(u){
    A<-strsplit(u,split=",") %>% unlist 
    A[1]<-paste0("Well: ",A[1])
    strsplit(A,"[:] ") %>% 
      lapply(.,function(u){h<-data.frame(X=u[2]);names(h)<-u[1];h}) %>% 
      bind_cols()
  }
  cleansplit_all<-function(u){
    lapply(u,cleansplit) %>% 
      bind_rows()
  }

  b<-read_xml(xml) 
  Y<-  b %>% 
    rvest::xml_nodes(.,"Details") %>% 
    .[grepl("&lt;table",.)] %>% 
    xml2::xml_text() %>% 
    gsub("Optical Window","",.) %>% 
    gsub('center-x:',',center-x:',.) %>% 
    gsub('center-y:',',center-y:',.) %>%
    gsub('radius' , ',radius',.) %>% 
    gsub('num ',',num_',.) %>% 
    gsub('mean diameter:',',mean_diameter:',.) %>% 
    gsub('inner diameter:',',inner_diameter:',.) %>% 
    gsub('outer diameter:',',outer_diameter:',.) %>% 
    gsub('total area:',',total_area:',.) %>% 
    gsub('circularity:',',circularity:',.) %>% 
    gsub('mean intensity:',',mean_intensity:',.) %>% 
    gsub('min intensity:',',min_intensity:',.) %>% 
    gsub('max intensity:',',max_intensity:',.) %>% 
    gsub('intensity stdev:',',intensity_stdev:',.) %>% 
    gsub('Drug Ports',"",.) %>% 
    gsub("Port 1, diameter:",",Port_1_diameter:",.) %>% 
    gsub("Port 2, diameter:",",Port_2_diameter:",.) %>% 
    gsub("Port 3, diameter:",",Port_3_diameter:",.) %>% 
    gsub("Port 4, diameter:",",Port_4_diameter:",.) %>% 
    gsub('Spot 1:','',.) %>% 
    read_html() %>% html_table() %>% 
    .[[1]]
  #
  Barcode<-rvest::xml_nodes(b,"Details") %>%
    .[grepl("[A-Z]{1}[0-9]{5}[A-Z,0-9]{1}[0-9]{4}B",
            .)
      ] %>% 
    xml_text()
  #
 Q1<- lapply(Y,cleansplit_all) %>% bind_rows() %>% arrange(.,Well) 
 
 Q2<-select(Q1,Well) %>% 
    mutate(Lot=paste0(substr(Barcode,1,1),substr(Barcode,7,11)),sn=substr(Barcode,2,6))
 Q3<-select(Q1,-Well) %>% mutate_all(.,as.numeric)
 list(Q2,Q3) %>% bind_cols()
}
