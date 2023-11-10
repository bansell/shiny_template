#plotly interactives trial

#https://plotly-r.com/client-side-linking.html 

library(tidyverse)
library(plotly)

?highlight_key
m <- highlight_key(mpg, ~class) #creates subgroups by mpg$class (cf group_by ?)

p1 <- ggplot(m, aes(displ, fill = class)) + geom_density()
p2 <- ggplot(m, aes(displ, hwy, fill = class)) + geom_point()

subplot(p1, p2) %>% hide_legend() %>% 
  highlight("plotly_hover") #this links the plots




