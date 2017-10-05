library(tidyverse)

ufo <- read.delim("ufo_awesome.tsv", sep="\t", 
                  stringsAsFactors = F, header=F, na.strings="")
head(ufo)
names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "Longdescription")

head(ufo)
glimpse(ufo)

ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo %>% 
  filter(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8) %>% 
  head(6)

good.rows <- ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8, F, T)
length(which(!good.rows))

ufo <- ufo[good.rows,]
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")

get.location <- function(l) {
  split.location <- tryCatch(strsplit(l, ",")[[1]], error=function(e)
    return(c(NA,NA)))
  clean.location <- gsub("^ ", "", split.location)
  if (length(clean.location)>2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

city.state <- lapply(ufo$Location, get.location)
head(city.state)

location.matrix <- do.call(rbind, city.state)
head(location.matrix)

ufo <- transform(ufo, USCity=location.matrix[,1],
                 USState=tolower(location.matrix[,2]),
                 stringsAsFactors=F)

ufo2 <- ufo %>% 
  mutate(USCC=location.matrix[,1],
         USSS=tolower(location.matrix[,2]))

## dplyr way is also correct

us.states <- c("ak", "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "hi", "ia", "id", "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy")
ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USState[is.na(ufo$USState)] <- NA

ufo.us <- subset(ufo, !is.na(USState))
head(ufo.us)

summary(ufo.us$DateOccurred)

quick.hist <-
  ufo.us %>% 
  ggplot(aes(x=DateOccurred)) + geom_histogram() +
  scale_x_date(date_breaks="50 years")
ggsave(plot=quick.hist, filename="quick_hist.png", height=6, width=8)

quick.hist

ufo.us <- ufo.us %>% 
  filter(DateOccurred >=as.Date("1990-01-01"))
nrow(ufo.us)

ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")

library(tidyverse)
sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)
head(sightings.counts)

sightings.counts <- ufo.us %>% 
  group_by(USState, YearMonth) %>% 
  summarise(number=n())

date.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)),
                       to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings <- strftime(date.range, "%Y-%m")
head(date.strings)

states.dates <- lapply(us.states, function(s) cbind(s, date.strings))
states.dates <- data.frame(do.call(rbind, states.dates), stringsAsFactors=F)

head(states.dates)

all.sightings <- merge(states.dates, sightings.counts, by.x=c("s", "date.strings"), 
                       by.y=c("USState", "YearMonth"), all=T)

head(all.sightings)

names(all.sightings) <- c("State", "YearMonth", "Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))


state.plot <- all.sightings %>% 
  ggplot(aes(x=YearMonth, y=Sightings)) +
  geom_line(color="darkblue") +
  facet_wrap(~State, nrow=10, ncol=5) +
  theme_bw() +
  scale_color_manual(values=c("darkblue"="darkblue"), guide="none") +
  scale_x_date(date_breaks="5 years", date_labels="%Y") +
  xlab("Years") + ylab("Number of Sightings") +
  ggtitle("Number of UFO Sightings by Month-Year and U.S. State(1990-2010)")

state.plot
