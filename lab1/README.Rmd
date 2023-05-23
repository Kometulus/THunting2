---
title: "Lab1"
output: html_document
date: "2023-04-23"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Нахождение утечки данных из сети
## Подготовка
```{r}
library(arrow)
library(dplyr)
library(stringr)
```
## Импорт датасета
```{r,warning=FALSE, message=FALSE, error=FALSE}
dataset <- arrow::read_csv_arrow("traffic_security.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))
```

## Задание 1: Найдите утечку данных из Вашей сети

### Важнейшие документы с результатами нашей исследовательской деятельности в области создания вакцин скачиваются в виде больших заархивированных дампов. Один из хостов в нашей сети используется для пересылки этой информации -- он пересылает гораздо больше информации на внешние ресурсы в Интернете, чем остальные компьютеры нашей сети. Определите его IP-адрес.

### Определение IP-адреса, который пересылает больше информации на внешние ресурсы:

```{r}
filter(dataset,str_detect(src,"^((12|13|14)\\.)"),
         str_detect(dst,"^((12|13|14)\\.)",negate=TRUE)) %>% 
  select(src,bytes) %>%
  group_by(src)%>% 
  summarise(bytes=sum(bytes))%>%
  slice_max(bytes)%>%
  select(src)
```
