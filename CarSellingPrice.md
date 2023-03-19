---
title: "Car Selling Price"
format: html
editor: visual
keep-md: true
code-fold: true
---


::: {.panel-tabset}



## How Much Should I Get from Selling my Car?


::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(pander)

# load in data and make column name R friendly
cardata <- rio::import("malibu.xlsx")

# create regression
log.lm <- lm(log(Price) ~ Mileage, data=cardata)
b.log <- coef(log.lm)

# plot regression
ggplot(cardata, aes(x = Mileage, y = Price)) +
  geom_point(size = 1.5, color = "skyblue4", alpha = 0.5) +
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x), color="skyblue", size=1.5) +
  geom_segment(x = 170000, y = 2991.733, xend = 170000, yend = 0,
               linewidth = 1, color = "firebrick", linetype ="longdash", alpha = 0.5) +
  geom_segment(x = 151000, y = 1200, xend = 151000, yend = 0,
               linewidth = 1, color = "firebrick", linetype ="longdash", alpha = 0.5) +
  geom_segment(x = 151000, y = 1200, xend = 170000, yend = 2991.733,
               linewidth = 1, color = "firebrick", linetype ="longdash", alpha = 0.5) +
  geom_point(x = 168000, y = 2991.733, size = 3, color = "firebrick") +
  geom_point(x = 151000, y = 1200, size = 3, color = "firebrick") +
  geom_text(x = 168000, y = 3356.673, label = "$2991.73, how much I could sell my Malibu", color = "black", size = 4, vjust=-2, hjust=.65) +
  geom_text(x = 151000, y = 1200, label = "$1200, how much I paid for my Malibu", color = "black", size = 4, hjust=1.1) +
  theme_bw() +
  labs(title = "My 2004 Chevy Malibu's Resale Value as Increased", x = "Mileage", y = "Price")
```

::: {.cell-output-display}
![](CarSellingPrice_files/figure-html/unnamed-chunk-1-1.png){width=672}
:::
:::



The graph above plots the selling price for Chevy Malibu LTs based on the mileage at the time of the sale. As one would probably expect, the selling price for Malibus decreases as the mileage increases. Towards the bottom of the graph a red point plots the price I paid for my 2004 Chevy Malibu. I bought it at 151000 miles for just 1200. I plan to sell the car once it hits around 170000, and according to the regression I should be able to sell it for around 2991 - over doubling my money. This can probably be owed to the fact that Post-Covid supply chain issues have increased the demand for used vehicles.


## Background and Hypotheses

The aim of this analysis is to show that there is a negative relationship between a car's mileage and car's selling price - the greater the mileage, the less it will sell for.

As will be demonstrated shortly, it was found that doing a logarithmic transformation to the price variable helped produced the best fit regression line for the data. Therefore, the equation for our model is:


$$
\underbrace{Y_i}_\text{Selling Price} = e^{\beta_0 +\beta_1 X_i} = e^{\beta_0}e^{\beta_1 X_i} 
$$

$$
\text{Where} X_i \text{is the number of miles driven}
$$




Our null hypothesis is that there is no correlation between a car's mileage and a car's selling price and that ($\beta_1$) is 0 (which means the resale value of a car would never change no matter how long you drove it).



$$
H_0: \beta_1 = 0
$$


Our alternative hypothesis is that there is a correlation between a car's mileage and its selling price and that $\beta_1$ is not 0 (meaning each additional mile a car is driven will have an effect on its value).



$$
H_a: \beta_1 \neq 0
$$


$\alpha$ = 0.05 is the level of significance we will use in the linear regression test.

## Data Used

The data used for this regression was collected using a variety of car listings for Chevy Malibus currently on sale. These listings were primarily found on [carfox.com](https://www.carfax.com/Used-2004-Chevrolet-Malibu_z3728), [edmunds.com](https://www.edmunds.com/chevrolet/malibu/2004/), and [autolist.com](https://www.autolist.com/chevrolet-malibu-2004#year_min=2004&year_max=2004&make=Chevrolet&model=Malibu&location=98014&latitude=47.621239&longitude=-121.842349&radius=any&page=1&sort_new_cars_last=true).


::: {.cell}

```{.r .cell-code}
pander(cardata)
```

::: {.cell-output-display}
-----------------
 Mileage   Price 
--------- -------
 159101    4537  

 160000    1500  

  2e+05    1200  

  90659    5995  

 200222    1995  

 152713    2995  

  30906    8990  

 109023    5995  

  72057    5800  

  89000    4800  

  35906    8990  

 109775    4995  

 117878    4995  

 171452    5995  

  59837    6479  

 170447    4400  

 123356    3995  

 145707    4766  

  73766    9900  

 145625    5222  

  92585    4490  

 118578    4999  

  66789    6995  

    0      29195 

  2015     26226 

  5557     24881 

  2106     24922 

   554     24505 

  1650     24999 

  16398    15998 

  52286    12000 

  48889    16500 

  16398    15998 

  49101    12300 

  39668    18998 

  3961     21406 

  42364    17995 

  27667    16786 

  24401    18590 

 175622    3900  

  32465    4295  

 203255    2850  

 205725    2995  
-----------------
:::
:::


## Finding the Right Fit for the Data


::: {.cell}

```{.r .cell-code}
cars.lm <- lm(Price ~ Mileage, data=cardata)

par(mfrow=c(1,3))
plot(cars.lm, which=1:2)
plot(cars.lm$residuals)
```

::: {.cell-output-display}
![](CarSellingPrice_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::


Above are diagnostics plots used to assess how fit a linear regression is for the data. On the center Q-Q plot, we can see the normality of the data is not an issue as most of the points hug the line quite closely. The residual plot also shows no discernible patterns, so we can conclude the data points are also independent. However, the Residual vs. Fitted Values plot presents a problem. The data points take on a U-shape, which suggests that the linear relationship assumption for the data is violated. Fortunately, this can be fixed. 

As mentioned when showing the equation of this model, we found that a transformation of the price data was necessary in finding the best fit line for the analysis. Below is the results of a Box-Cox analysis that was performed on the cars data. 


::: {.cell}

```{.r .cell-code}
library(car)
boxCox(cars.lm, main="Cars Data Box-Cox Analysis")
```

::: {.cell-output-display}
![](CarSellingPrice_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::


The middle dotted band indicates a lambda value very close to 0, which is interpreted to mean that a logarithmic transformation needs to be done to the price variable in our regression analysis. The results of said regression analysis can be seen below:


::: {.cell}

```{.r .cell-code}
pander(summary(log.lm))
```

::: {.cell-output-display}
-----------------------------------------------------------------
     &nbsp;         Estimate    Std. Error   t value   Pr(>|t|)  
----------------- ------------ ------------ --------- -----------
 **(Intercept)**     9.897       0.09494      104.2    2.408e-51 

   **Mileage**     -1.114e-05   8.836e-07     -12.6    1.094e-15 
-----------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
      43              0.3824          0.7948       0.7898     
--------------------------------------------------------------

Table: Fitting linear model: log(Price) ~ Mileage
:::
:::


These results coincide with the results of the Box-Cox analysis and show that the logarithmic transformation of the price data created a more meaningful regression. We have an extremely low p-value that is well below our level of significance of $\alpha = 0.05$, which allows us to reject the null hypothesis and conclude that the selling price of Malibus and their mileage are not independent factors.

The slope is a very low negative value, which means that with each additional mile put on a Malibu, most of its value is retained. However, the value does diminish slightly and the effect becomes more evident over time.

## When is the best time to sell my Car?

According to the regression model, I got my 2004 Chevy Malibu at a pretty good price. I bought it at 151,000 miles for just 1200, while according to the regression the resale value at that mileage for the car was 3696.68. I plan to sell that car once it gets to around the 170000 and according to the regression, I could sell it at this point for around 2991.73. The cost per mile in the scenario is calculated as follows:


$$
\underbrace{cpm}_\text{Cost per mile} = \frac{3696.68-2991.73}{19000} = \underbrace{0.0371}_\text{approx. 3.7 cents per mile}
$$



By selling at the 170000, I'm projected experience a 3.7 cent decrease in the resell value of my car per mile it's driven. This appears to be a good time to sell, since a Malibu's lifespan seems to last until a little over the 200,000 mile mark. Even if I waited up until the 200,000 mile mark (assuming it lasts that long), the improvement in my cost per mile is relatively small as demonstrated; the regression projects the selling price would be around $2142.08 at the point:



$$
\underbrace{cpm}_\text{Cost per mile} = \frac{3696.68-2142.08}{49000} = \underbrace{0.0317}_\text{approx. 3.17 cents per mile}
$$



:::


