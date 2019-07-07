library(tidyverse)
library(data.table)
library(ggthemes)
library(gridExtra)

## Загрузка функции для расчёта суммуы WS и VORP в первые 7 сезонов после драфта
source(seven_first_years.R)

## Создание таблицы для драфтов 1976 - 2011
table <- rbindlist(lapply(seq(1976, 2011), seven_first_years))

## Подсчёт сумм WS и VORP для 4 пика , а также для 8, 17 и 35 пиков вместе
dt <- table[Pk == 4 | Pk == 8 | Pk == 17 | Pk == 35][
  , Pk := as.character(Pk)][Pk == "8" | Pk == "17" | Pk == "35", Pk := "sum(8, 17, 35)"][
    , .(WS_ch = sum(WS),
               VORP_ch = sum(VORP)), by =.(Pk, Year)]

## Переход от "long" к "wild" данным. Сравнение сумм WS и VORP по каждому году
dt1 <- dcast(dt, Year ~ Pk, value.var = c("WS_ch", "VORP_ch"))
dt2 <- dt1[, .(Log = WS_ch_4 >`WS_ch_sum(8, 17, 35)`)][, .(.N), by = Log]
dt3 <- dt1[, .(Log = VORP_ch_4 >`VORP_ch_sum(8, 17, 35)`)][, .(.N), by = Log]

## Получение средних значений WS и VORP для каждого пика.
table_mean <- table[Pk == 4 | Pk == 8 | Pk == 17 | Pk == 35][
  ,.(WS_mean = round(mean(WS), digits = 2),
     VORP_mean = round(mean(VORP), digits = 2)) , by = Pk][order(Pk)]

## Создание с помощью функции lapply() 4 графиков с показателями WS и VORP для каждого пика.
cclist <- lapply(c(4, 8, 17, 35), function(pick){
  mean <- table[Pk == pick][,.(WS_mean = round(mean(WS), digits = 2),
                              VORP_mean = round(mean(VORP), digits = 2)), by = Pk][order(Pk)]
  ggplot(table[Pk == pick], aes(x = Year, y = WS)) +
    geom_point(aes(color = "WS")) +
    geom_segment(aes(xend = Year), yend=0, linetype = "dotted") +
    expand_limits(y=0) +
    geom_point(aes(x = Year, y = VORP,  color = "VORP")) +
    scale_colour_manual(name = 'Value', 
                        guide = 'legend',
                        values = c('WS' = 'green',
                                   'VORP' = 'red'), 
                        labels = c('VORP',
                                   'WS')) +
    geom_text(aes(x = Year, y = WS, label = WS), nudge_y = 2, size  = 2) +
    geom_text(aes(x = Year, y = VORP, label = VORP), nudge_y = -2,
              size  = 2, color = "red") +
    theme_tufte() +
    scale_x_continuous(breaks = seq(1976, 2011)) +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste0("The values of WS and VORP for ", pick, "th pick of the NBA draft\nin first 7 years of his career."),
         subtitle = paste0("Average WS for the ", pick, "th pick - ", mean[[2]],
                           "\nAverage VORP for the ", pick, "th pick - ", mean[[3]]),
         caption = "Sourse: basketball-reference\n Telegram: @NBAatlantic, twitter: @vshufinskiy")
})
cclist[["ncol"]] <- 1
  
output <- do.call(grid.arrange, cclist)

## Сохранение результатов
ggsave("draft_plot.jpeg", output, width = 9, height = 12, unit = "in")


