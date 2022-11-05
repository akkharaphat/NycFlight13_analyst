library(nycflights13)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(patchwork)

# **Box Plot**
flights %>%
    sample_n(10000)%>%
    filter(month %in% c(1,2,3))%>%
    group_by(origin,month)%>%
    count(month)%>%
    ggplot(aes(x = origin, y = n, fill = origin)) +
    geom_boxplot() +
    labs(title = "Box plot of Q1 flights at Origin airport",
         x = "Origin",
         y = "Flights",
         caption = "Nycflights") +
    theme_clean() +
    scale_fill_brewer(type = "seq", palette = "Greens")
# How many carrier in December ?
flights %>%
    select(month,origin,carrier) %>%
    filter(month == 12) %>%
    count(carrier)%>%
    arrange(desc(n))%>%
    head(9) %>%
    ggplot(mapping = aes(carrier,n,fill = n)) + 
    geom_col() + 
    labs(title = "Carrier Flight in December",
         x = "Carrier",
         y = "Count") + 
    theme_gdocs()
# "Histogram of Distance"
ggplot(flights,aes(distance))+
    geom_histogram(fill="burlywood1",bins = 50) +
    theme_pander()+
    labs(title = "Distance Per Flight")
# "Carrier flights Per Month "

fl <- flights %>% sample_n(10000)%>%
    filter(carrier %in% c("UA","B6","DL","AA","US"))
fl1 <-fl %>% transmute(
    month ,
    carrier,
    distance = distance * 1.6) %>%
    group_by(month,carrier) %>%
    summarise(distances = mean(distance)) 


fl1 %>%
    ggplot(aes(y=carrier, x=distances, fill=carrier)) +
    geom_col() +
    facet_wrap(~ month) +
    theme_minimal() +
    labs(title = 'Carrier distance Per Month',
         caption='Nycflights')

# "Relationship between Carrier & Airport "

f1<-flights %>%
    filter(carrier %in% c("UA","B6","DL","AA","US")) %>%
    group_by(origin,carrier) %>%
    summarize(flight_avg = mean(flight)) %>%
    ggplot(aes(x = origin, y = flight_avg, group = carrier, color = carrier)) + 
    geom_point() + 
    geom_line() + 
    labs(title='Carrier in Origin Airport',
         caption='1st chart') +
    theme_minimal() +
    scale_color_brewer(type = "seq",palette = "RdPu")


f2<- flights %>%
    filter(carrier %in% c("UA","B6","DL","AA"),
           dest %in% c("MIA","MCO","BOS","LAX","ORD")) %>%
    group_by(dest,carrier) %>%
    summarize(flight_avg = mean(flight))%>%
    ggplot(aes(x = dest, y = flight_avg, group = carrier, color = carrier)) + 
    geom_point() + 
    geom_line() +
    labs(title='Carrier in destination airport',
         caption='2nd chart') +
    scale_color_brewer(type = "qual",palette = "Set1")

f3 <-flights %>%
    select(dest,carrier)%>%
    filter(dest %in% c("MIA","MCO","BOS","LAX","ORD")) %>%
    sample_n(100)%>%
    ggplot(aes(y=dest,fill=carrier)) +
    geom_bar(position = 'fill') +
    labs(title='destination airport population',
         caption='3rd chart') +
    scale_fill_brewer(palette='YlGn', type='seq')

(f1+f2)/f3    

