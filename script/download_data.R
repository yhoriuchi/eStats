#' A sample code to download municipality-level data from e-Stats
#' Yusaku Horiuchi

# Initial settings --------------------------------------------------------

library(tidyverse)
library(estatapi)

# Add appID ---------------------------------------------------------------
# You need to get an "application ID" by following guide: https://www.e-stat.go.jp/api/api-info/api-guide

appId <- "xxxxxxx" # Enter manually in Console 

# Make a list of data sets to use -----------------------------------------

stats_IDs <- estat_getStatsList(appId = appId, 
                                searchWord = "社会・人口統計体系") %>% 
  filter(str_detect(STATISTICS_NAME, "市区町村データ 基礎データ（オリジナル）")) %>% 
  select(`@id`) %>% 
  pull()

# Make a list of variables included in the data sets ----------------------

vars_IDs <- NULL

for (i in 1:length(stats_IDs)){
  vars_IDs <- bind_rows(
    vars_IDs,
    estat_getMetaInfo(appId = appId,
                      statsDataId = stats_IDs[i])[["cat01"]] %>% 
      mutate(ID = stats_IDs[i])
  )
}

# Make a set of specific variables to use ---------------------------------

selected <- vars_IDs %>% 
  filter(`@name` %in% c("A1101_総人口",
                        "A1102_日本人人口",
                        "A1231_年齢中位数",
                        "A1414_１５歳以上人口（国勢調査結果）",
                        "A2101_住民基本台帳人口（日本人）",
                        "A210102_住民基本台帳人口（日本人）（女）",
                        "A2201_住民基本台帳人口（外国人）",
                        "A6108_昼夜間人口比率",
                        "B1101_総面積（北方地域及び竹島を除く）",
                        "B1103_可住地面積",
                        "C120110_課税対象所得",
                        "C120120_納税義務者数（所得割）",
                        "C120130_納税義務者数（均等割）",
                        "C2210_第１次産業従業者数（経済センサス‐基礎調査結果）",
                        "C2211_第２次産業従業者数（経済センサス‐基礎調査結果）",
                        "C2212_第３次産業従業者数（経済センサス‐基礎調査結果）",
                        "D2201_財政力指数（市町村財政）",
                        "D320101_地方税（市町村財政）",
                        "D320108_地方交付税（市町村財政）",
                        "D320113_国庫支出金（市町村財政）",
                        "D320115_県支出金（市町村財政）",
                        "D320122_地方特例交付金（市町村財政）",
                        "D320406_普通建設事業費（市町村財政）",
                        "E9101_最終学歴人口（卒業者総数）",
                        "E9106_最終学歴人口（大学・大学院）",
                        "F1101_労働力人口",
                        "F1107_完全失業者数",
                        "F2201_第１次産業就業者数",
                        "F2211_第２次産業就業者数",
                        "F2221_第３次産業就業者数",
                        "H3100_総世帯数"))

# Get a set of specific variables to use ----------------------------------

get_data <- function(id, year_from = 1995){
  
  out <- estat_getStatsData(appId = appId,
                            statsDataId = selected[id, "ID"],
                            cdCat01     = selected[id, "@code"])
  
  varcode <- out[1, "cat01_code"] # Variable code
  
  out %>% 
    mutate(year = str_extract(調査年, "\\d+") %>% as.numeric()) %>% 
    select(area_code, 
           year, 
           value) %>% 
    filter(year >= year_from) %>% 
    set_names(c("muncode", "year", varcode))
  
}

out <- get_data(1)

for (i in 2:nrow(selected)){
  temp <- get_data(i)
  out <- full_join(out, temp, by = c("muncode", "year")) 
}

# Save data ---------------------------------------------------------------

saveRDS(out, "output/eStat_selected.RDS")


