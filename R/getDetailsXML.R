getDetailsXML<-function(DIR){
require(parallel);require(dplyr);
require(xml2);
FLS<- list.files(DIR,recursive=T,pattern='details.xml',full.names = T);
size.of.list <- length(FLS);
cl <- makeCluster( min(size.of.list, detectCores()) );
Out<-parallel::parLapply(cl=cl,FLS,sortMV::MVdetails) %>% 
  bind_rows()
stopCluster(cl);
Out
}
