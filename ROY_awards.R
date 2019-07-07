library(data.table)
library(tidyverse)
library(ggthemes)

## Загрузка данных
dt <- fread("./data/ROY.csv")

dt1 <- copy(dt)

## Очистка стоблца Player от никнеймов игроков с сайта basketball-reference
dt1 <- dt1[, Player := str_remove(Player, "\\\\[:alnum:]{1,}")]

## Функция для замены числовых значений категориальными
replase_func <- function(x){
  if_else(x == 0, "None",
          if_else(x == 1, "Single",
                  if_else(x > 1, "Multi", "NA")))
}

cols <- colnames(dt1[, 3:5])

dt1 <- dt1[, (cols) := lapply(.SD, replase_func), .SDcols = cols]

## Превращение данных из "wild" в "long"
dt1 <- melt(dt1, measure.vars = cols, variable.name = "Awards", value.name = "Count")

dt1 <- dt1[, Count := factor(Count, levels = c("Multi", "Single", "None"))]

## Построение графика
ggplot(dt1, aes(x = Season, y = Awards, fill = Count)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis_d(option = "viridis") +
  coord_equal() +
  theme_tufte() +
  labs(title = "The number of awards to former ROY",
       subtitle = "* 1994-1995 Jason Kidd, 1999-00 Elton Brand",
       caption = "Sourse: basketball-reference\n Telegram: @NBAatlantic, twitter: @vshufinskiy") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(size = 8),
        axis.ticks = element_blank(),
        legend.title = element_blank())

## Сохранение объекта
ggsave("ROY.jpeg", last_plot(), width = 6, height = 2.5, unit = "in")
