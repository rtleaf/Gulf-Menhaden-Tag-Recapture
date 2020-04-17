# Summarize adult and juvenile tag recatpure data
# Robert Leaf   April 10, 2020

require(readxl)

a.tag <- readxl::read_excel(path = "./Tag_Recapture_Data/Adult_Field_Data.xlsx") %>% 
  mutate(Stage = "Adult") %>% mutate(Tag. = paste(Tag_Series,Tag_Series_Number,sep = "."))
j.tag <- readxl::read_excel(path = "./Tag_Recapture_Data/Juv_Field_Data.xlsx") %>% 
  mutate(Stage = "Juv") %>% mutate(Tag. = paste(Tag_Series,Tag_Series_Number,sep = "."))
a2.tag <- readxl::read_excel(path = "./Tag_Recapture_Data/Adult_Field_Data_Length_1969.xlsx") %>% 
  mutate(Stage = "Adult") %>% mutate(Tag. = paste(Tag_Series,Tag_Series_Number,sep = ".")) %>% 
  mutate(Tag_Year = Tag_Year + 1900) %>% 
  mutate(Recov_Year = Recov_Year + 1900) %>% 
  select(-Fork_Length)
a.tag <- rbind(a.tag, a2.tag)

expand.tag <- function(input.tag = a.tag, label = "adult") {
  tag.template <- expand.grid(unique(input.tag$Tag.),seq(0,99)) %>% arrange(Var1) %>% as_tibble()
  names(tag.template) <- c("Tag.","Tag_Series_Number")  
  tag.template$Tag. <- as.character(tag.template$Tag.)
  
  tag.template <- left_join(tag.template, input.tag[,c(1:3,12)], by = c("Tag."))
  tag.template <- tag.template[which(!duplicated(tag.template)),]
  names(tag.template)[2] <- "Recov_Tag_ID"
  
  tag.template <- left_join(tag.template, input.tag[,6:12], by = c("Tag.","Recov_Tag_ID"))
  tag.template <- tag.template %>% mutate(Stage = label)
  
  tag.template <- tag.template %>% 
    mutate(tag.ymd = lubridate::ymd(paste(Tag_Year, Tag_Month, Tag_Day))) %>% 
    mutate(rec.ymd = lubridate::ymd(paste(Recov_Year, Recov_Month, Recov_Day))) %>% 
    mutate(at.large = rec.ymd - tag.ymd) 
  tag.template <- tag.template[-which(tag.template$at.large < 0),] 
  tag.template <- tag.template %>% 
    mutate(Tag.ID = paste(Tag., Recov_Tag_ID,sep = ".")) %>% 
    select(-c(Tag.,Recov_Tag_ID))
  
  # tag.template <- tag.template %>% select(Tag.ID, Stage, at.large, tag.ymd, rec.ymd, Recov_Plant)
  
  return(tag.template)  }

a.tag <- expand.tag(input.tag = a.tag, label = "Adult")
j.tag <- expand.tag(input.tag = j.tag, label = "Juvenile")

tag.dat <- rbind(a.tag, j.tag)
tag.dat <- tag.dat[which(!is.na(tag.dat$Tag_Year)),]

rm(a.tag, j.tag)

save(tag.dat, file = "./data/tag.recapt.sum.RData")