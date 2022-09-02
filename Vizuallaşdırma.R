# Vizuallaşdırma

library(tidyverse)

data <- mpg


# “ggplot2” paketi ----

#set x,y
data %>% 
  ggplot(aes(displ, cty))

#add geometry
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point()

#add colour
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red)

#add size 
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red,
             aes(size = hwy))

#add labs 
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red,
             aes(size = hwy)) +
  labs(x = "mühərrikin həcmi", 
       y = "millər",
       title = "Yanacaq iqtisadiyyatı məlumatları",
       subtitle = "Scatterplot")

#add scales 
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red,
             aes(size = hwy)) +
  labs(x = "mühərrikin həcmi", 
       y = "millər",
       title = "Yanacaq iqtisadiyyatı məlumatları",
       subtitle = "Scatterplot") +
  scale_y_continuous(breaks = seq(0,50,2)) +
  scale_x_percent()

# add facet
data %>% 
  mutate(total = hwy + cty) %>% 
  ggplot(aes(total)) + 
  geom_histogram(aes(fill = class), colour = "Black") + 
  facet_grid(class ~ .) 

p <- data %>% 
  ggplot(aes(x=hwy, y=cty, colour=class))
p + geom_point(size=2)

p + geom_point(size=2) +
  facet_grid(class~., scales="free") +
  theme(legend.position="none")

p + geom_point(size=2) + 
  facet_grid(.~year, scales="free") +
  theme(legend.position="none")

p + geom_point(size=2) + 
  #geom_smooth(method = "loess") +
  facet_grid(class ~ year, scales="free") +
  theme(legend.position="none")


# “plotly” paketi ----

p1 <- data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$yellow,
             aes(size = hwy)) +
  labs(x = "mühərrikin həcmi", 
       y = "millər",
       title = "Yanacaq iqtisadiyyatı məlumatları",
       subtitle = "Scatterplot") +
  theme_modern_rc()

p1 %>% ggplotly()


# Histogram

p2 <- data %>% 
  ggplot(aes(hwy)) + 
  geom_histogram(aes(fill=trans), 
                 colour="Black") +
  scale_x_continuous(breaks = seq(0,60,5)) +
  scale_y_continuous(breaks = seq(0,60,2))

p2 %>% ggplotly()


# Density plot

data$year <- data$year %>% as_factor()

data %>% 
  ggplot(aes(hwy, color = year, fill = year)) + 
  geom_density(alpha = 0.4, position = 'stack') +
  scale_x_continuous(breaks = seq(10,60,5))


# Boxplot

p3 <- data %>% 
  ggplot(aes(y = hwy, colour = class)) + 
  geom_boxplot(size = 1) + #coord_flip() +
  theme_ft_rc()

p3 %>% ggplotly()


# Plotları birləşdirmək ----
library(patchwork)

p1
p2
p3

p1 + p2

p1 + p2 + p3 + 
  plot_layout(nrow=2,byrow=F)

p1 / p3

p2 | (p1 / p3)

(p2 | (p1 / p3)) +
  plot_annotation('Title')

(p2 | (p1 / p3)) +
  plot_annotation(tag_levels='1')



# “tidycharts” paketi ----
library(tidycharts)

data("tidychartsdata")
marks_data 
gender_school_earnings

gender_school_earnings %>% 
  dumbbell_chart(x1_name=Men,
                 x2_name=Women,
                 y_name=School,
                 line_color="black",
                 x1_color="blue",
                 x2_color="red",
                 show_legend=T,
                 plot_title='Gender School Earnings',
                 x_axis_title='Gender',
                 y_axis_title='School')

gender_school_earnings %>% 
  lollipop_chart(x_name=Men,
                 y_name=School,
                 line_color="green",
                 x_color="darkgreen",
                 show_legend=F,
                 plot_title='Gender School Earnings',
                 x_axis_title='Men',
                 y_axis_title='School')

marks_data %>% 
  factor_scatter_chart(x_name=marks,
                       y_name=name,
                       color_name=subject,
                       show_legend=T,
                       plot_title='Marks data',
                       x_axis_title='marks',
                       y_axis_title='name')

iris %>% 
  numeric_scatter_chart(x_name='Sepal.Length',
                        y_name='Petal.Length',
                        color_name='Species',
                        show_legend=T,
                        plot_title='Iris',
                        x_axis_title='Sepal Length',
                        y_axis_title='Petal Length')

marks_data %>% 
  group_by(name) %>% 
  summarise(marks = mean(marks)) %>% 
  bar_chart(x_name=name,
            y_name=marks,
            static_color="blue",
            border_line_color="green",
            border_line_width=2,
            stack=F,
            highlight=c('Danny','Jon'),
            show_legend=F,
            sort_colors_alphabetically=F,
            plot_title="Grouped Marks Data")

marks_data %>% 
  bar_chart(x_name=marks,
            y_name=name,
            color_name=subject,
            border_line_width=2,
            stack=F,
            show_legend=T,
            sort_colors_alphabetically=T,
            plot_title="Marks Data")

marks_data %>% 
  bar_chart(x_name=name,
            y_name=marks,
            color_name=subject,
            border_line_width=2,
            border_line_color='bold',
            stack=T,
            show_legend=T,
            sort_colors_alphabetically=T,
            plot_title="Marks Data")


# "highcharter" paketi ----
library(highcharter)
#https://api.highcharts.com/highcharts/series

data %>% 
  hchart("scatter", hcaes(x = displ, y = cty)) %>%
  hc_colors(color = "red") %>%
  hc_xAxis(visible = F) %>%
  hc_yAxis(visible = F) %>% 
  hc_title(text = "Yanacaq iqtisadiyyatı datası")

data %>% 
  hchart("area", hcaes(y = hwy)) %>% 
  hc_colors(color = "green")

data %>% 
  hchart("line", hcaes(y = hwy)) %>% 
  hc_colors(color = "red")

data %>% 
  hchart("bar", hcaes(x = manufacturer, y = cyl, 
                      color = manufacturer)) 

data %>% 
  hchart("column", hcaes(x = manufacturer, y = cyl, 
                      color = manufacturer)) 

data %>% 
  group_by(model) %>% 
  summarise(hwy = max(hwy, na.rm=T)) %>% 
  hchart("pie", hcaes(x = model, y = hwy))

data %>% 
  hchart("treemap", hcaes(name = manufacturer, value = cyl)) 


# “esquisse” paketi ----
library(esquisse)

data %>% esquisser()

