# Create juv.adult.tag.recapt.summary

load("./data/tag.recapt.sum.RData")
tag.dat <- tag.dat %>% mutate(at.large = as.numeric(at.large))
tag.dat.pos <- tag.dat[which(tag.dat$at.large <= 2000),]

ggplot() +
  geom_histogram(data = tag.dat.pos, aes(x = at.large), 
                 colour="black", fill="black", binwidth = 5) +
  facet_grid( Stage~. ) + 
  labs(x = "Number of Days at Large", y = "Number of Individuals") + 
  theme_minimal(base_size = 10) + 
  ggsave(filename = "./figs/recpture.composition.png", device = "png")

tag.wide <- tag.dat %>% group_by(Tag_Year, Stage) %>% summarise('Number Tagged' = length(Tag.ID)) 
tag.wide <- tag.wide %>% pivot_wider(names_from = Stage, values_from = `Number Tagged`)
names(tag.wide)[c(2,3)] <- paste(names(tag.wide)[c(2,3)], "Tagged")

tag.wide.recapt <- tag.dat %>% group_by(Tag_Year, Stage) %>% summarise('Number Recaptured' = length(which(!is.na(at.large)))) 
tag.wide.recapt <- tag.wide.recapt %>% pivot_wider(names_from = Stage, values_from = `Number Recaptured`)
names(tag.wide.recapt)[c(2,3)] <- paste(names(tag.wide.recapt)[c(2,3)], "Recaptured")

tag.wide.recapt.perc <- tag.dat %>% group_by(Tag_Year, Stage) %>% summarise('Percent Recaptured' = length(which(!is.na(at.large)))/length(at.large)) 
tag.wide.recapt.perc <- tag.wide.recapt.perc %>% pivot_wider(names_from = Stage, values_from = `Percent Recaptured`)
names(tag.wide.recapt.perc)[c(2,3)] <- paste(names(tag.wide.recapt.perc)[c(2,3)], "Percent Recaptured")

tag.wide <- left_join(tag.wide, tag.wide.recapt, by = "Tag_Year")
tag.wide <- left_join(tag.wide, tag.wide.recapt.perc, by = "Tag_Year")

write.csv(tag.wide, file = "./data/tag.recapt.sum.csv")

