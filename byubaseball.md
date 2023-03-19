---
title: "BYU Baseball"
format: html
editor: visual
warning: false
code-fold: true
keep-md: true
---



## Which Utah School Produces the Most Success Pro Baseball Players?



::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(Lahman)
library(priceR)

names <- select(People, c("playerID", "nameFirst", "nameLast"))
schoolnames <- select(Schools, c("schoolID", "name_full", "state"))

data <- left_join(CollegePlaying, names, by="playerID")
data2 <- left_join(data, schoolnames, by="schoolID")
data3 <- inner_join(data2, Salaries, by="playerID")

names(data3)[3] = "School_year"
names(data3)[8] = "Pro_year"

utah_player <- data3 %>% 
  filter(state=="UT") %>% 
  unite("full_name", nameFirst, nameLast, sep=" ")

View(utah_player)

adjust_for_inflation(price = utah_player$salary, from_date = utah_player$Pro_year, country = "US", to_date = 2021)
```

::: {.cell-output .cell-output-stdout}
```
Generating URL to request all 299 results
Retrieving inflation data for US 
Generating URL to request all 62 results
```
:::

::: {.cell-output .cell-output-stdout}
```
  [1]   321381.3   465030.6   675944.6   677606.5  1343874.5  3050751.0
  [7]  4457691.5  4564358.6  5959597.8  7735218.7  6046503.8  5190937.5
 [13]  4571341.8  6994866.7  5507515.2   321381.3   465030.6   675944.6
 [19]   677606.5  1343874.5  3050751.0  4457691.5  4564358.6  5959597.8
 [25]  7735218.7  6046503.8  5190937.5  4571341.8  6994866.7  5507515.2
 [31]   321381.3   465030.6   675944.6   677606.5  1343874.5  3050751.0
 [37]  4457691.5  4564358.6  5959597.8  7735218.7  6046503.8  5190937.5
 [43]  4571341.8  6994866.7  5507515.2   253216.5   330222.8   441898.6
 [49]   253216.5   330222.8   441898.6   193825.0   193825.0   193825.0
 [55]   439537.6   517848.2   439537.6   517848.2   439537.6   517848.2
 [61]   572863.5   572863.5   496610.9   969086.3   820978.8  3727992.4
 [67]  6926655.3  9677747.5  5815872.7  9156851.8 10289252.5   519154.4
 [73]   519154.4   519154.4   314844.6   466939.5   160690.6   429259.0
 [79]   779054.7  2010961.3  3629290.6  4178203.3  8110746.8  8253361.9
 [85]  8409248.4  8535413.8  4405309.9  6414817.0  6233647.9  4622598.3
 [91]  5295238.3   306065.2   160690.6   429259.0   779054.7  2010961.3
 [97]  3629290.6  4178203.3  8110746.8  8253361.9  8409248.4  8535413.8
[103]  4405309.9  6414817.0  6233647.9  4622598.3  5295238.3   306065.2
[109]   160690.6   429259.0   779054.7  2010961.3  3629290.6  4178203.3
[115]  8110746.8  8253361.9  8409248.4  8535413.8  4405309.9  6414817.0
[121]  6233647.9  4622598.3  5295238.3   306065.2   944657.5  1112473.7
[127]  1252005.4   859251.6   928976.7   994810.3   944657.5  1112473.7
[133]  1252005.4   859251.6   928976.7   994810.3   944657.5  1112473.7
[139]  1252005.4   859251.6   928976.7   994810.3   944657.5  1112473.7
[145]  1252005.4   859251.6   928976.7   994810.3   332921.8   455892.0
[151]   473414.0   457879.3  1115645.8  1960306.2  3932980.2  5367938.6
[157]  5281322.6  6324337.4  6491172.1   872380.9   332921.8   455892.0
[163]   473414.0   457879.3  1115645.8  1960306.2  3932980.2  5367938.6
[169]  5281322.6  6324337.4  6491172.1   872380.9   502333.1   502333.1
[175]   502333.1   502333.1  2229391.7  2163143.3  4411828.7  4555178.9
[181]  4347610.9  4355148.7  7361596.3  8593529.3 10176020.0   639834.1
[187]  1828097.5  2229391.7  2163143.3  4411828.7  4555178.9  4347610.9
[193]  4355148.7  7361596.3  8593529.3 10176020.0   639834.1  1828097.5
[199]   149048.3   177578.7   318636.5   430376.4   490615.3   430376.4
[205]   490615.3   430376.4   490615.3   148329.8   214629.5   423897.4
[211]   743181.4  1451716.2  1591696.5  1091088.6  2813646.1  2742146.2
[217]   148329.8   214629.5   423897.4   743181.4  1451716.2  1591696.5
[223]  1091088.6  2813646.1  2742146.2   148329.8   214629.5   423897.4
[229]   743181.4  1451716.2  1591696.5  1091088.6  2813646.1  2742146.2
[235]   448946.6   522748.3   521041.2   820978.8   448946.6   522748.3
[241]   521041.2   820978.8
```
:::
:::

::: {.cell}

```{.r .cell-code}
averages <- utah_player %>% 
  group_by(Pro_year, name_full) %>% 
  summarise(mean=mean(salary))


salarychart <- ggplot(data=averages) +
  geom_line(aes(x=Pro_year, y=mean, color=name_full))+
  labs(title="Average MLB Salary by Season and School", x="Year", y="Average Salary (in 2021 U.S. Dollars)", color="School") +
  theme_bw()

salarychart
```

::: {.cell-output-display}
![](byubaseball_files/figure-html/unnamed-chunk-2-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
# get totals of all players career salaries
topplayers <- utah_player %>% 
  group_by(full_name) %>% 
  summarise(total=sum(salary))

# narrow down to just the top 10 players with highest career earnings
topplayers2 <- topplayers %>% 
  slice_max(total, n=10, with_ties=FALSE)

# do an inner join with the full player data to get their school names
utah_player2 <- select(utah_player, c("full_name", "name_full"))
topplayers3 <- inner_join(utah_player2, topplayers2, by="full_name")

# drop  based on name
topplayers4 <- topplayers3[!duplicated(topplayers3$full_name), ]
View(topplayers4)


playerschart <- ggplot(data=topplayers4) +
  geom_bar(stat="identity", aes(x=reorder(full_name, -total), y=total/1000000, fill=name_full)) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  labs(title="Top 10 Highest Career Earnings for Utah MLB Players", subtitle= "Adjusted for inflation to 2021 U.S. Dollars", x="", y="Total Career Earnings (in Millions)", fill="School")
  

playerschart
```

::: {.cell-output-display}
![](byubaseball_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::


The two plots above give us some insight into which Utah college has produced the most successful baseball players. The first plot is a line graph that plots the average professional salary of players for a certain school for each season. Prior to 2000, BYU appears to be the only school that had alumni in the major leagues. After 2000, players from other schools begin to show up, but BYU continued to dominate in terms of average salary. Dixie State was the only real competitor in this metric from 2000 and on.

The second plot shows the top individual players from this time period in terms of total career earnings (adjusted to inflation). Each player is colored according to their school. We see BYU highly represented in the plot, as six of the top 10 players went to school there. This shows that players from BYU were highly paid and also probably spent more time in the MLB than players who went to other schools. We can conclude that BYU is the most successful Utah school when it comes to producing successful professional baseball players.