# Create time.series.tag.recapt.summary

load("./data/tag.recapt.sum.RData")
tag.dat <- tag.dat %>% mutate(at.large = as.numeric(at.large))
tag.dat <- tag.dat[which(tag.dat$at.large <= 2000),]
tag.dat <- tag.dat %>% select(-c(Tag_Year, Tag_Month, Tag_Day, Recov_Year, Recov_Month, Recov_Day))
tag.dat <- tag.dat %>% mutate(days.at.large = at.large) %>% select(-at.large)

tag.dat <- tag.dat %>% mutate(tag.md = lubridate::floor_date(tag.ymd, unit = 'months'))
tag.dat <- tag.dat %>% mutate(rec.md = lubridate::floor_date(rec.ymd, unit = 'months'))

tag.dat <- tag.dat %>% select(-c(tag.ymd, rec.ymd))

date.range <- range(c(tag.dat$tag.md, tag.dat$rec.md))
date.range <- 
  rbind(data.frame(rec.md = seq(date.range[1],date.range[2], by = 'months'),
                   Stage = "Adult"),
        data.frame(rec.md = seq(date.range[1],date.range[2], by = 'months'),
                   Stage = "Juvenile")) %>% as_tibble() %>% mutate(Stage = as.character(Stage))

tag.dat <- tag.dat %>% group_by(Stage, rec.md) %>% summarise(number.recap = length(rec.md))
tag.dat <- left_join(date.range, tag.dat, by = c("Stage", "rec.md"))

ggplot(data = tag.dat) + 
  geom_path(mapping = aes(x = rec.md, y = number.recap)) + 
  geom_point(mapping = aes(x = rec.md, y = number.recap)) + 
  facet_grid(Stage~.) +
  theme_minimal() +
  labs(x = "Time", y = "Number Recaptured") + 
  ggsave(filename = "./figs/stage.specific.recaptures.png", device = "png")

  