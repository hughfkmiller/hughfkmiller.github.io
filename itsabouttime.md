---
title: "It's About Time"
format: html
editor: visual
keep-md: true
code-fold: true
---

## Who has the Best Gross Income

::: cell
``` {.r .cell-code}
library(lubridate)
```

::: {.cell-output .cell-output-stderr}
    Loading required package: timechange
:::

::: {.cell-output .cell-output-stderr}
    Attaching package: 'lubridate'
:::

::: {.cell-output .cell-output-stderr}
    The following objects are masked from 'package:base':

        date, intersect, setdiff, union
:::

``` {.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}
    ── Attaching packages
    ───────────────────────────────────────
    tidyverse 1.3.2 ──
:::

::: {.cell-output .cell-output-stderr}
    ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::as.difftime() masks base::as.difftime()
    ✖ lubridate::date()        masks base::date()
    ✖ dplyr::filter()          masks stats::filter()
    ✖ lubridate::intersect()   masks base::intersect()
    ✖ dplyr::lag()             masks stats::lag()
    ✖ lubridate::setdiff()     masks base::setdiff()
    ✖ lubridate::union()       masks base::union()
:::

``` {.r .cell-code}
library(riem)

##load in data
biz_data <- rio::import("https://byuistats.github.io/M335/data/sales.csv")

##convert time to MTN
biz_data$Time <- with_tz(biz_data$Time, "US/Mountain")
##convert negative transactions to positive
biz_data$Amount <- abs(biz_data$Amount)

###use sales floor on month, week, and day level
##floor_date to round down transactions to their respective hour
biz_data2 <- biz_data %>% 
  mutate(saleday=floor_date(Time, unit="day")) %>% 
  mutate(saleweek=floor_date(Time, unit="week")) %>% 
  mutate(salemonth=floor_date(Time, unit="month")) %>%
  mutate(salehour=floor_date(Time, unit="hour"))
  

##aggregate the sales by week
sales_per_week <- biz_data2 %>% 
  group_by(Name, saleweek) %>% 
  summarise(weeksales = sum(Amount))
```

::: {.cell-output .cell-output-stderr}
    `summarise()` has grouped output by 'Name'. You can override using the
    `.groups` argument.
:::

``` {.r .cell-code}
## remove "missing" lines
sales_per_week2 <- filter(sales_per_week, Name != "Missing")

revenuechart <- ggplot(data=sales_per_week2, aes(x=as.Date(saleweek), y=weeksales)) +
  geom_line(aes(group=Name, color=Name)) +
  theme_dark() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d, %Y") +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title="Gross Income of Local Businesses by Week", x="Week of Sales", y="Sales in U.S. Dollars", fill= "Business")

revenuechart
```

::: cell-output-display
![](itsabouttime_files/figure-html/unnamed-chunk-1-1.png){width="672"}
:::
:::

::: cell
``` {.r .cell-code}
##aggregate the sales by week
sales_per_month <- biz_data2 %>% 
  group_by(Name, salemonth) %>% 
  summarise(monthsales = sum(Amount))
```

::: {.cell-output .cell-output-stderr}
    `summarise()` has grouped output by 'Name'. You can override using the
    `.groups` argument.
:::

``` {.r .cell-code}
## remove "missing" lines
sales_per_month2 <- filter(sales_per_month, Name != "Missing")


revenuechart2 <- ggplot(data=sales_per_month2, aes(x=as.Date(salemonth), y=monthsales)) +
  geom_bar(stat="identity", aes(fill=Name)) +
  theme_classic() +
  facet_wrap(~Name, nrow=2) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b, %Y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title="Gross Income of Local Businesses by Week", x="", y="Sales in U.S. Dollars")

revenuechart2
```

::: cell-output-display
![](itsabouttime_files/figure-html/unnamed-chunk-2-1.png){width="672"}
:::
:::

## Who has the Most Transactions?

::: cell
``` {.r .cell-code}
transactions <- biz_data2 %>% 
  group_by(Name, saleweek) %>% 
  summarise(num_trans=n())
```

::: {.cell-output .cell-output-stderr}
    `summarise()` has grouped output by 'Name'. You can override using the
    `.groups` argument.
:::

``` {.r .cell-code}
transactions2 <- filter(transactions, Name != "Missing")

transactionchart <- ggplot(data=transactions2) +
  geom_freqpoly(aes(x=num_trans, color=Name)) +
  theme_bw() +
  gghighlight::gghighlight() +   
  facet_wrap(~Name, nrow=3) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = "none") +
  labs(title="Distribution of Weekly Transactions by Company", x="Transactions", y="Count")

transactionchart
```

::: {.cell-output .cell-output-stderr}
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
:::

::: cell-output-display
![](itsabouttime_files/figure-html/unnamed-chunk-3-1.png){width="672"}
:::
:::

## Recommendation

Going off of the visualizations seen about, I would recommend giving HotDiggity the business expansion loan. They had the most steady gross income during this three month period, so we know they are capable of making money. Also, that income is not dependent on a few large transactions like a few other companies are. HotDiggity's transaction figures are also the most impressive. In the frequency chart, we can see that they have multiple weeks where they experience over 1000 transactions! They have a very large customer base. For these reasons, they are the company who should be greated the loan.
