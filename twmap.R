install.packages("ggmap")
library(ggmap)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
us.map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(us.map)


tw <- c(left = 117.5, bottom = 20, right = 125, top = 27.5)
tw.map <- get_stamenmap(tw, zoom = 5, maptype = "toner-lite")
ggmap(tw.map)
ggmap(tw.map, extent = "device")

get_googlemap("taiwan", zoom = 8) %>% ggmap()
get_googlemap("taipei", zoom = 12, maptype = "satellite") %>% ggmap()
get_googlemap("chicago", zoom = 12, maptype = "roadmap") %>% ggmap()
get_googlemap("taichung", zoom = 15, maptype = "hybrid") %>% ggmap()


