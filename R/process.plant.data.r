# Summarize adult and juvenile tag recatpure data
# Robert Leaf   April 10, 2020

require(readxl)

a.tag <- readxl::read_excel(path = "./Plant_Test_Data/Adult/Plant_Testing_Data_Adult.xlsx") %>% 
  mutate(Stage = "Adult") %>% mutate(Tag. = paste(Tag_Series,Tag_Series_Number,sep = "."))
j.tag <- readxl::read_excel(path = "./Plant_Test_Data/Juvenile/Juv_Plant_Test_Data.xlsx") %>% 
  mutate(Stage = "Juv") %>% mutate(Tag. = paste(Tag_Series,Tag_Series_Number,sep = "."))

tag.dat <- rbind(a.tag, j.tag)
tag.dat <- tag.dat[complete.cases(tag.dat),]

tag.dat <- tag.dat %>% 
  mutate(tag.ymd = lubridate::ymd(paste(Test_Year, Test_Month, Test_Day))) %>% 
  select(Test_Year, Test_Month, Tag., Plant, Stage)

tag.sum <- tag.dat %>% group_by(Stage,Test_Year,Tag.,Plant) %>% summarise(length. = length(Tag.))
tag.sum <- tag.sum %>% group_by(Tag.) %>% summarise(Recaptured = sum(length.),
                                                    Plant = Plant[which.max(length.)],
                                                    Year = Test_Year[which.max(length.)],
                                                    Stage = Stage[which.max(length.)])

tag.sum <- tag.sum %>% arrange(Year, Plant) %>% mutate(Recapture.Probability = Recaptured/100)

save(tag.sum, file = "./data/plant.experiments.sum.RData")

# tag.sum.by.plant <- tag.sum %>% group_by(Plant) %>% 

plant.data <- 
  ggplot() +
  geom_histogram(data = tag.sum, aes(x = Recapture.Probability, 
                                     ..ndensity..), 
                 colour="white", fill="black", binwidth = 0.1) +
  facet_grid(Plant ~ Year) + 
  labs(x = "Magnet Efficiency", y = "Density") 


plant.tag.sum <- tag.sum %>% select(-Year)

plant.data + 
  geom_density(data = plant.tag.sum, aes(x = Recapture.Probability, ..ndensity..), 
               alpha=0.75, fill="cadetblue3", bw = 0.10) + 
  theme_minimal(base_size = 7)

ggsave(filename = "./figs/Magnet.Efficiency.png", device = "png")

