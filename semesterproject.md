---
title: "First Round QB's Vs. Later Round QBs"
format: html
editor: visual
code-fold: true
keep-md: true
---



## Introduction
In the game of football, there is perhaps no position more important than quarterback. A great quarterback can almost single-handedly make or break a franchise. Naturally, some the biggest names in the history of the NFL played this position - Terry Bradshaw, Dan Marino, Peyton Manning, Tom Brady. The question is how does a team acquire such valuable talent? In most cases, they are acquired during the annual NFL Draft, where all the teams select new players for that year's class of graduating college athletes.

Since the players who played best in college are usually selected in the first round, one would think that first round QBs go on to have the most successful careers compared to their later-drafted counterparts. The following visualizations attempt to determine whether this is in fact the case. The data used for these visualization contained each draft pick in the NFL Draft from 1970-2016 and their career statistics in the NFL.


## Do First Rounds Have Longer Career's than Late Rounders?


::: {.cell}

```{.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}
```
── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
✔ tibble  3.1.8      ✔ dplyr   1.0.10
✔ tidyr   1.2.1      ✔ stringr 1.4.1 
✔ readr   2.1.3      ✔ forcats 0.5.2 
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
```
:::

```{.r .cell-code}
## Load in data and narrow down to QBs
players <- rio::import("Draft.xlsx")
```

::: {.cell-output .cell-output-stderr}
```
New names:
• `` -> `...1`
• `Att` -> `Att...16`
• `Yds` -> `Yds...17`
• `TD` -> `TD...18`
• `Int` -> `Int...19`
• `Att` -> `Att...20`
• `Yds` -> `Yds...21`
• `TD` -> `TD...22`
• `Yds` -> `Yds...24`
• `TD` -> `TD...25`
• `Int` -> `Int...27`
```
:::

```{.r .cell-code}
names(players)[6]="Position"
qbs <- players %>% 
  filter(Position=="QB")

## rename columns
names(qbs)[16]="Attempts"
names(qbs)[17]="Yards"
names(qbs)[18]="Touchdowns"
names(qbs)[19]="Int"

## Create column for career lengths
qbs1 <- qbs %>% 
  mutate(car_length = as.numeric(To)-as.numeric(Year))

## Split by first rounds and later rounds
qbs2 <- qbs1 %>% 
  mutate(group = case_when(
    Rnd == 1 ~ "First",
    Rnd != 1 ~ "Other"
  ))


## plot duration of careers
lengthchart <-ggplot(data=qbs2) + 
  geom_density(aes(x=car_length, color=(group))) +
  facet_wrap(~group, labeller = labeller(group = c("First" = "First Rounders", "Other" = "Later Rounders"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.025, 0.05, 0.075, 0.1)) +
  geom_hline(yintercept = 0, linetype=1) +
  geom_hline(yintercept = c(0.05, 0.1), linetype=2) +
  labs(title="Length of NFL QB's Career", subtitle="Length of NFL Career for 1st Round QBs Vs. QBs Drafted in All Other Rounds", x= "Length of Career (Years)", y = "% of QBs")


lengthchart
```

::: {.cell-output-display}
![](semesterproject_files/figure-html/unnamed-chunk-1-1.png){width=672}
:::
:::

Career longevity is a great metric for success as an NFL quarterback. A long career suggests that a player was a valuable asset to his team or teams for a long time. As the average NFL career last only a handful of years, to last even five or six years in the league is a great accomplishment in itself.

The chart above shows a great contrast between first round and later round quarterbacks in terms of career longevity. The density curve for first rounders shows that a significant amount of these quarterbacks end up staying in the league for 5, 10, or even 15 years. It appears to be more common for them to last past 10 years than to drop out after one year. As for late rounders, the density curve takes on a right-skewed shape, meaning that most of them have relatively short careers. It appears that for each additional year in the NFL, the more likely a late rounder's career is going to end.



## Do Most All-Time Passers come from the First Round?


::: {.cell}

```{.r .cell-code}
qbs2$Yards <- as.numeric(qbs2$Yards)

yards <- qbs2 %>% 
  arrange(desc(Yards)) %>% 
  slice(1:15)

pbchart <- ggplot(data=yards) +
  geom_bar(stat="identity", aes(x=reorder(Player, -Yards), y=Yards, fill=group)) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title="Most Passing Yards (1970-2016)", x= "", y = "Total Career Passing Yards", fill="Round Drafted")
  

pbchart
```

::: {.cell-output-display}
![](semesterproject_files/figure-html/unnamed-chunk-2-1.png){width=672}
:::
:::

When compare first and late rounders, one question to ask is when did historic greats get selected? The chart above shows the top 15 NFL quarterbacks with the most career passing yards (as of 2016). It's worth noting that while some great quarterbacks were not initially drafted by any team (i.e. Kurt Warner and Tony Romo), all 15 of the quarterbacks above were drafted.

We can see that a majority of the quarterbacks in the top 15 were drafted in the first round. In fact, only 5 of the top 15 were drafted after the first round. This is another category where the first rounds have enjoy the better results. It's interesting to note, however, that three of the top five are late rounder quarterbacks. 



## Which Draft Round has the Most Accurate Passers?

::: {.cell}

```{.r .cell-code}
qbs3 <- filter(qbs2, !is.na(Cmp))

qbs4 <- filter(qbs3, Cmp>100)

qbs5 <- qbs4 %>% 
  mutate(comp_per = as.numeric(Cmp)/as.numeric(Attempts))

av_comp <- qbs5 %>% 
  group_by(Rnd) %>% 
  summarise(av_per=mean(comp_per))


compchart <- ggplot(data=av_comp) +
  geom_line(aes(x=Rnd, y=av_per, color="firebrick")) +
  geom_point(aes(x=Rnd, y=av_per, fill="skyblue", size=2)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(0.50, 0.52, 0.54, 0.56), linetype=1) +
  scale_y_continuous(labels = scales::percent, breaks = c(0.50, 0.52, 0.54, 0.56)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) +
  labs(title="Average Completion % Among QBs from Each Draft Round", subtitle = "Among QBs with at least 100 pass attempts", x="Draft Round QBs Were Selected", y="Completion %")

compchart
```

::: {.cell-output-display}
![](semesterproject_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::

When it comes to measuring the success of a quarterback's career, performance stats are the most telling. A first round draft pick is likely to get a big value contract when their career starts, but that doesn't mean that they always deliver on the field. With that said, the chart above attempts to answer the question of which draft round historically produces the most accurate passers.

This chart maps the average percentage of passes completed among quarterbacks drafted in their respective rounds. Given the possibility that a quarterback could have only ever thrown a handful of passes in the NFL (as is the case for many quarterbacks drafter after the first round), only quarterbacks with at least 100 career passing attempts are considered. The results that we see don't show a dominate performance by first rounders like the previous two did. While first rounders have the best average completion percentage, they share that position with quarterbacks drafted in the fourth round of the draft.

Another interesting note is that the worst average completion percentage comes from quarterbacks drafted in the fifth round. One would think that on field performance would decline with each successive round, but in reality, sixth and seventh rounds have passed more accurately overall than fifth rounders. 




## In What Round do Most Pro Bowlers Get Drafted?

::: {.cell}

```{.r .cell-code}
probowl <- qbs2 %>% 
  filter(PB>0)

probowl1 <- probowl %>% 
  group_by(Rnd) %>% 
  summarise(pbs=sum(as.numeric(PB)))

pbchart <- ggplot(data=probowl1) +
  geom_bar(stat="identity", aes(x=Rnd, y=pbs, fill="skyblue", color="black")) +
  theme_grey() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(7, 6, 5, 4, 3, 2, 1)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
  geom_hline(yintercept = c(25, 50, 75, 100, 125), linetype=2) +
  coord_flip() +
  labs(title = "Number of Pro Bowl Nods Among QBs from Each Draft Round", x="Round When QBs Were Selected", y="Number of Pro Bowls")

pbchart
```

::: {.cell-output-display}
![](semesterproject_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::

One very telling stat for an NFL quarterback is how many times he has been selected to compete in an annual Pro Bowl game. Players selected for the Pro Bowl are choosing for have an exceptionally productive year and is an honor intended to recognize the best players of a particular season. In the metric, the results are clear. First round quarterbacks are the most commonly picked for the Pro Bowl by a long shot. This is also another metric where quarterbacks drafted in the fifth round have the worst result with only three Pro Bowl selections having been rewarded to quarterbacks from this round. 

## Conclusion
While the game of football is one of comebacks, shocks, and busts, it's reasonable to suspect that QBs drafted in the first round will go one to have a fruitful career in the NFL - more so than those drafted in the later rounds of the draft. While first round "busts" garner lots of attention and stick out in the memories of fans (i.e. JaMarcus Russell and Ryan Leaf), they are certainly outliers. Late round quarterbacks also have their share of outliers and three of the top five all time passers came from the late rounds of the draft.  

