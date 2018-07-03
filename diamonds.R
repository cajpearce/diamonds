library(tidyverse)
library(xml2)

data(diamonds)

clarityrating = tolower(c("SI2","SI1","VS2","VS1","VVS2","VVS1","IF","FL"))
colorrating = tolower(c("J","I","H","G","F","E","D"))
cutrating = tolower(c("Fair", "Good", "Very Good", "Ideal", "Premium"))



read.diamonds = function(.data) {
  .data %>%
    select(price
           , carat
           , clarity
           , color
           , cut) %>%

  mutate(
    clarity = tolower(clarity),
    color = tolower(color),
    cut = gsub("super ideal",
                       "premium",
                       tolower(cut)),
    claritynum = match(clarity, clarityrating),
    colornum = match(color, colorrating),
    cutnum = match(cut, cutrating)
  ) %>%
    na.omit() %>%
    mutate(price = log(price),
           carat = log(carat))
}

diamonds = diamonds %>% read.diamonds()




diamonds.lm = lm(price~.,data=diamonds)

add.predictions = function(.data) {
  prediction = predict(diamonds.lm, .data)
  
  .data %>% mutate(
    predicted = exp(prediction)
    , price = exp(price)
    , carat = exp(carat)
    , difference = predicted - price
    , diffdiv = (predicted - price)/(price + predicted)
  )
}


brilliantearth = read.csv("brilliantearth.csv",stringsAsFactors = FALSE) %>% 
  read.diamonds() %>% 
  add.predictions() %>%
  mutate(origin = "Lab Grown")


# moissanite---------------
# conversion = read.csv("conversion.csv") %>% 
#   mutate(mm = as.numeric(gsub(" .+$","",mm)),
#          carat = as.numeric(gsub(" .+$","",carat)))
# 
# moissanite = read.csv("moissanite.csv") %>%
#   merge(conversion, all.x=TRUE) %>%
#   select(price, carat) %>%
#   mutate(origin="moissanite"
#          , clarity  = NA
#          , color  = NA
#          , cut  = NA
#          , claritynum  = NA
#          , colornum  = NA
#          , cutnum  = NA
#          , predicted  = NA
#          , difference  = NA
#          , diffdiv = NA
#          )
# 
#---------------



diamonds = diamonds %>% 
  add.predictions() %>%
  mutate(origin = "Mined") %>% 
  rbind(brilliantearth)


gooddiamonds = diamonds %>% 
  filter(diffdiv > 0.2,
         # claritynum > 3,
         # cutnum > 3,
         color %in% c("d","e","f"),
         price <= 2200,
         carat >= 0.65,
         carat <= 0.75,
         origin %in% c("RECENT","lab","bluenile"))


ggplot(diamonds %>% filter(price < 20000),
       aes(carat, price,
           colour=origin,
           group = origin
           )) +
  geom_point(alpha = 0.1) +
  # geom_smooth(se = FALSE,
  #             method="lm") + 
  ylab("Price") +
  xlab("Carat") +
  guides(fill=guide_legend(title="Origin")) +
  # facet_wrap(~origin) +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal() +
  ggtitle("Diamond plot of price vs carat size comparing lab grown diamonds vs mined diamonds")



gooddiamonds %>% arrange(desc(diffdiv))


# bluenile = as.character(read.csv("bluenile.csv",header=FALSE)[,1])
# 
# row = rep(1:1000, each=7)
# 
# data.table::rbindlist(lapply(1:1000,function(x) {
#   data.frame(t(bluenile[row == x]))
# })) %>% write.csv("bluenile.csv",row.names=FALSE,na="")



