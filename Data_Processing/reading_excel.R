# Polish Mortality Tables - Polskie Tablice Trwania Zycia (PTTZ) 1990-2014 (GUS)

given_year <- read_excel('lifetables1990-2014.xlsx',sheet='1990', skip = 3)

PTTZ <- data.frame()
for(i in 1990:2014){
  given_year <- read_excel('lifetables1990-2014.xlsx',sheet=as.character(i), skip = 3)[,1:8] %>% mutate(year = as.character(i))
  colnames(given_year)[c(1,2)] <- c('sex','age')
  PTTZ <- rbind(PTTZ,given_year)
}
PTTZ <- PTTZ %>% filter(is.na(sex)==F)
rm(given_year)
