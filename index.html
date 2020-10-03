---
title: "Graph representation of the covid-19 dataset"
author: "Stavrou Athanasios (stavrouath@outlook.com)"
date: "01/10/2020"
output:
  html_document:
    toc: true
    keep_md: true
    number_sections: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Explanatory Data Analysis

## Data Curation

This is an R Markdown presentation of the covid-19 data as of September 2020. This file presents some step-by-step graphs to showcase the `ggplot` library. First, we load some libraries that we are going to use later on.

```{r libs, warning = FALSE, message = FALSE}
library(dplyr)  #data manipulation
library(ggplot2)  #data viz
library(rworldmap) #map (world) data viz
library(RColorBrewer) #map coulouring
library(plotly) #interactive plots
library(hrbrthemes) #plot themes
library(lubridate)  #data transform (dates)
library(dygraphs) #interactive time series graphs
library(xts)    # To make the convertion data-frame / xts format
library(gganimate)  #animated graphs
```

Then, we load our data and rename some variables.

```{r data, collapse = TRUE}
data <- read.csv("C:/Users/whoka/Documents/R/covid19.csv", header = T, stringsAsFactors = F);

#rename variables
new_cols <- c("date"="ï..dateRep", "country" = "countriesAndTerritories", "ISO2" = "geoId", "ISO3" =
                "countryterritoryCode", "TwoWeeksPer100k" = 
                "Cumulative_number_for_14_days_of_COVID.19_cases_per_100000");
covid19 <- rename(data, all_of(new_cols));

head(covid19)
```

The dataset consists of the above variables. All of the variables are pretty straightforward, as for the last variable, it represents the total number of cases per 100k, for the past 14 days. The next step is cleaning and reforming our data, for the purposes of our analysis.

```{r clean1}
sum(is.na(covid19))
names(which(sapply(covid19, anyNA))) #find which columns contain NA values
```

We notice that there are 3045 NA values. Note that there are a few negative ``cases`` and ``deaths`` values, which are only corrections after recounts. To start with, since the last column `TwoWeeksPer100k` is defined as the number of cases divided by the country's population, if a country has no cases the this column takes `NA` as a value. So, as a first step, we replace `NA` values with `0` for this column:

```{r clean2}
covid19$TwoWeeksPer100k[is.na(covid19$TwoWeeksPer100k)] <- 0
```

Now that we have no `NA` values for this column, we will clean `popData2019`. As for this variable, it has `NA` values because of the cases on the British "Diamond Princess" cruise ship in the waters of Japan. We will replace these values with "3711", which was the number of total passengers (including workforce) of the ship ([source](https://bit.ly/2FcvEZr)). We also input `PDN` values for these observations for the `ISO3` variable. We refer to these values as `continentExp == "Other"`, since these are the only cases that were not recorded as cases on a continent:

```{r clean3}
covid19[(covid19$continentExp=="Other"),10] <- 3711
covid19[(covid19$continentExp=="Other"),9] <- "PDN"
```

Finally, we replace the `ISO2` values for Namibia, since its name is synonymous with the `NA` values ("NA").

```{r clean4, collapse = TRUE}
covid19[(covid19$country=="Namibia"),8] <- "NM"
sum(is.na(covid19))

head(covid19)
```

Our dataset is now clean, with no missing values. An interesting graph would be a graph showing the total number of cases per month, grouped by continent. We have to transform our dataset, so that it is grouped by month and continent.

```{r, message = FALSE, collapse = TRUE}
covid_new <- covid19 %>% group_by(month, continentExp) %>%
             summarize(cases = sum(cases), deaths = sum(deaths)) 
head(covid_new)
```

## Total Cases per month / continent

After having our data sorted, we create graphs using the `ggplot2` library. We create a variable containing all of the months' names, so that we replace them with the respective integers on the horizontal axis of our graph. There are plenty ways of doing this, like the one we use below, or by using ``months`` function, which is an alternative way we will use later on.

```{r, fig.align = 'center'}
months = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

theme_set(theme_bw())

p1 <- ggplot(covid_new, aes(fill=continentExp, y=cases, x=month)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ continentExp) +
  labs(title = "Monthly cases per continent") +
  xlab("Months") + ylab("Cases") +
  scale_x_discrete(limits = months)
p1 <- p1 + theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")

options(scipen=5)

p1
```

We notice that the worst is not behind us. We see that the total number of cases has no peaked yet, probably in every continent. 

Starting with Africa, the data show low number of cases per month. However, according to news reports, there has only been a low number of tests conducted across the continent ([source(https://bbc.in/3iqR4ko)).

Regarding America, it should be clarified that it includes both North and South America. With that said, it has two of the top three worst-hit countries (as of September), the United States of America and Brazil. The total number of cases is the highest across the world for July and August. With stores, schools and universities set to open in many States this month, this number is highly unlikely to decrease in the upcoming weeks. The total number of cases has significantly decreased, but it is still very high when compared to the rest of the continents.

However, Asia takes over for September, with some concerning news; an arithmetic growth. As we know, the pandemic started from China, but the confirmed cases deminished rapidly after a month's long lockdown. Despite that, the total number of confirmed cases in India has skyrocketed in the past months, luckily, with lower death rate than the United States or Brazil (see [map](#sec2) below).

Even though many European countries (such as Italy, Spain and the United Kingdom) were hit hard by pandemic's first wave, the total number of confirmed cases in the continent has never surpassed 1.000.000 cases per month. This number is high when we consider that it translates to higher risk for human lives, but it also shows the faster and more effective policies that European countries exercised as a unit, compared to respective policies in America or Asia. 

Speaking of effective policies, we notice that Oceania was not hit hard by the pandemic so far. With a population of almost 42.800.000 ([source](https://bit.ly/2Zx4N1s)), Oceania has less than 40.000 confirmed cases, with many states yet to report a single case.

As stated in the previous section, ``Other`` refers to a single british cruise ship (Princess Diana), where no analysis needs to be conducted.

Before moving on, we should comment on the populations of the continents. We should not forget that Asia has a whopping population of 4.5 billion people (more than Earth's half population)! Thus, when the total number of cases / deaths is presented, we should keep in mind that this is a much lower percentage of the total population than the respective American numbers. To address this issue, we will mostly work with the infection and death percentages.

Finally, we should keep in mind that it is possible that many countries present entirely fake or distorted data. This dataset merely analyses the public dataset, but anyone interested in a country / region in particular should dig [deeper](https://bit.ly/2RknelH).

## Total Deaths per month / continent

Following the total number of cases, it is interesting to sum up the total number of deaths, again, grouped by month and continent:

```{r, fig.align = 'center'}
p2 <- ggplot(covid_new, aes(fill=continentExp, y=deaths, x=month)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ continentExp) +
  labs(title = "Monthly deaths per continent") +
  xlab("Months") + ylab("Deaths") +
  scale_x_discrete(limits = months)
p2 <- p2 + theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")

options(scipen=5)

p2
```

We notice no major differences as for the trends of the data, with the exception of Europe, where we notice a spike of deaths in March-April, with over 100.000 deaths, followed by a rapid decrease of this number in the following months, due to the previously-mentioned reasons. 

## Death and infection percentages per country

In this section, we calculate the death and infection percentages per country. We aggregate cases and deaths grouped by countries, and we calculate the percentage of infection by dividing with the total population and the death percentage by dividing with the total number of cases. The percentage of infection is the percentage of citizens infected by the virus, while the death percentage is the percentage of infected people that died. 

```{r, message = FALSE, collapse = TRUE}

newdata <- covid19 %>% group_by(country, ISO3, continentExp, popData2019) %>%
  summarize(cases = sum(cases), deaths = sum(deaths)) 

newdata$case_pct <- (newdata$cases / newdata$popData2019)*100
newdata$death_pct <- (newdata$deaths / newdata$cases)*100

head(newdata)
q_cr <- quantile(newdata$case_pct)
q_dr <- quantile(newdata$death_pct)
#infection percentage quantiles
q_cr

#death percentage quantiles
q_dr
```

What do these quantiles mean? These numbers represent the distribution of the variables, where 50% quantile represents the median. So, the lowest case percentage in our data set is 0.0003%, while the highest is about 19%. As for the death percentages, there are countries with no deaths recorded yet, while there is a country with 29% death percentage(!). The median is about 1.9%, meaning that in most countries, almost 1.9% of the infected have died due to the virus. This also means that there is an observation having 18.75% infection percentage, as well as a country with 29% death percentage, which is rather surprising. To find which observations have these values, we try the following chunk of commands:

```{r}
newdata[which.max(newdata$case_pct),]
newdata[which.max(newdata$death_pct),]
```

We see that the observation with highest infection rate is none other than the Diamond Princess cruise ship. After all, this is not surprising at all, however, the country with highest death percentage is Yemen, where more than 1/4 of people infected ended up dead. If we want to get a country's statistics, say Greece, we type:

```{r}
newdata[newdata$country == "Greece",]
```

The case and death rates are 0.169% and 2.14% respectively. The case rate is better than the world's median (0.27%), but the death rate is higher than the world's median (1.89%). So, we conclude that either Greece took early and effective measures to fight the virus, or there were not many tests conducted. Either way, the death rate is worse than the world's median, which is certainly not good news.

# Mapping the quantiles
## Countries on world map

We now create two new variables, `group1` and `group2`, which represent the groups each country is in with respect to case and death rates; either `Low`, `Medium`, `High` or `Very High`. Note that these values are completely arbitrary, they are assigned according to the percentile they belong in. For example, if a country has a death rate higher than 75% of the rest countries, it is assigned the `Very High` value. We also set the levels of the factors `group1` and `group1` for the graphs later on.

```{r, collapse = TRUE}
newdata$group1 <- ifelse(newdata$case_pct <= q_cr[2], 'Low',
                  ifelse(newdata$case_pct > q_cr[2] & newdata$case_pct <= q_cr[3], 'Medium',
                  ifelse(newdata$case_pct > q_cr[3] & newdata$case_pct <= q_cr[4], 'High', 'Very High')))

newdata$group2 <- ifelse(newdata$death_pct <= q_dr[2], 'Low',
                  ifelse(newdata$death_pct > q_dr[2] & newdata$death_pct <= q_dr[3], 'Medium',
                  ifelse(newdata$death_pct > q_dr[3] & newdata$death_pct <= q_dr[4], 'High', 'Very High')))

newdata$group1 <- factor(newdata$group1, levels = c("Low", "Medium", "High", "Very High"))
newdata$group2 <- factor(newdata$group2, levels = c("Low", "Medium", "High", "Very High"))
```

Now we plot both case and death rates. First, we need to save `newdata` as a data frame so that we match `ISO3` values for each country with `rworldmap` library's valYou should keep in mind to use colourblind-friendly coulors, which can be displayed with the ``display.brewer.all(colorblindFriendly = TRUE)`` command)

```{r map1, fig.align = 'center', fig.cap = "", message = FALSE}
newdata <- as.data.frame(newdata)

my_map <- joinCountryData2Map(newdata, 
                              joinCode = "ISO3",
                              nameJoinColumn = "ISO3")

display.brewer.all(colorblindFriendly = TRUE)

# def. map parameters, e.g. def. colors
mapParams <- mapCountryData(my_map, 
                            nameColumnToPlot="group1",
                            oceanCol = "azure2",
                            catMethod = "categorical",
                            missingCountryCol = gray(.8),
                            colourPalette = brewer.pal(4,'RdPu'),
                            addLegend = T,
                            mapTitle = "Infection percentage per country")

```

Moving on with the death percentage, we slightly edit the code above, replacing `group1` with `group2`:

<div id="sec2"></div>

```{r map2, fig.align = 'center', message = FALSE}

# def. map parameters, e.g. def. colors
mapParams <- mapCountryData(my_map, 
                            nameColumnToPlot="group2",
                            oceanCol = "azure2",
                            catMethod = "categorical",
                            missingCountryCol = gray(.8),
                            colourPalette = brewer.pal(4,'RdPu'),
                            addLegend = T,
                            mapTitle = "Death percentage per country")

```

We can see that plenty countries have higher infection and lower death percentages, as well as the opposite. This should be an indicator of whether a country's policies and health institutions are effective or not. For example, we see that countries like China and Egypt have lower infection percentages compared to other countries, but the countries' death percentages are high. On the contrary, countries such as Russia and Kazakhstan have high case and low death percentages, indicating that these countries coped better with the virus. 

At this point, we should remind the reader that covid19 is deadlier for the elderly, and that Africa has way lower than average life expectancy ([source](https://bit.ly/3mw6c24)). This means that even if Africa has higher case percentage, someone would expect it to have low death percentages. However, this is not the case. The lack of medical funding and healthcare infrastructure overshadows the low death rates in younger people, so we have high death rates in many African countries, especially in the Saharan region. 

Which are the countries with top 5 highest death percentages?

```{r}
newdata_sorted <- newdata[order(newdata$death_pct),]
head(newdata_sorted, 5)
tail(newdata_sorted, 5)
```

The worst handling of the virus happened by Yemen, where even though they had `Low` infection percentage, they had `Very High` death percentages (almost 1/3 of people infected by the virus died), followed by Italy and Mexico. On the other hand, there are many countries with no deaths yet, so the death rates are 0%. To get the countries with highest number of cases and no deaths recorded, we re-sort the dataset according to cases for countries with no deaths:

```{r}
newdata_sorted <- newdata_sorted[order(newdata_sorted$cases),]
tail(newdata_sorted[newdata_sorted$deaths ==0,], 5)
```

In Faroe Islands there where 460 cases with no deaths recorded so far.

## Top countries with most cases per month

Another way of showing a timeseries dataset is an animated graph. In this section, we show the total number of cases per month, for the top 10 countries (note that this chunk of code takes a few minutes to generate the graph):

```{r anim, warning = FALSE, message = FALSE}
#We transform our data, so that we keep only the top 10 countries with most cases
data_formatted <- covid19 %>%
                  group_by(month, country) %>%
                  summarize(cases = sum(cases), deaths = sum(deaths)) %>%
                  arrange(-cases) %>%
                  mutate(rank=row_number()) %>%
                  filter(rank<=10)

#We add month names as factors for the transition time                 
data_formatted$Month <- factor(months[data_formatted$month], levels = c("December",
                                                                        "January",
                                                                        "February",
                                                                        "March",
                                                                        "April",
                                                                        "May", "June",
                                                                        "July", "August",
                                                                        "September"))

#drop observations with 0 cases so that they don't appear in the graph
data_formatted <- data_formatted[data_formatted$cases!=0,] 
#replace "_" with spaces in country names
data_formatted$country <-gsub("_", " ", data_formatted$country)


p <- data_formatted %>%
  ggplot(aes(x = -rank, y = cases, fill = country)) +
  geom_tile(aes(y = cases / 2, height = cases), width = 0.9) +
  geom_text(aes(y = 0, label = country), hjust = "right", colour = "black", fontface = "bold", nudge_y = -50000) +
  geom_text(aes(label = scales::comma(cases)), hjust = "left", colour = "grey30") +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) +
  theme_ipsum(plot_title_size = 26, subtitle_size = 20, caption_size = 18, base_size = 18) +
  theme(legend.position = "none",
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y = element_blank()) +
  # gganimate code to transition by month:
  transition_states(Month, transition_length = 10, state_length = 2) +
  ease_aes('linear') +
  labs(title='Countries with most cases',
       subtitle='Total cases in {closest_state}',
       caption='Source: ECDC
                github.com/StavrouA');

animate(p, nframes = 750, fps = 25, end_pause = 50, width = 1200, height = 900, renderer = gifski_renderer(loop = T))
```

This graph may be reproduced for various sets of countries, like European countries or top countries with most deaths, or even maybe graph the top months with most infections / deaths for a country. In this graph, we may observe the rise of cases in India or the fact that the United States of America ranks top 3 for 7 of 9 months (exluding December, where there were only 27 cases in China).

# Cases per country
## Country statistics

There are two ways of tracking a country's current state and how well the country handled the virus so far. First, we can check a statistic we haven't touched yet; the number of active cases in the past 14 days per 100k population. The second one, is comparing the country's case / death history with another country, preferably with a country with same population and geographical area. Here, we create a function to graph ``TwoWeeksPer100k`` for each day for any country we want to.

```{r two_weeks}
two_weeks_ts <- function(data, country) {
  
  #transform data
  country <- data[data$country==country,]
  country$date <- mdy(country$date)
  
  ts_data <- xts(x = country$TwoWeeksPer100k, order.by = country$date)
  
  # Use the dygraph HTML widget
  fig <- dygraph(ts_data) %>%
            dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
            dyRoller(rollPeriod = 1)
  
  return(fig)
  
}

country = "Greece"
two_weeks <- two_weeks_ts(covid19, country)
two_weeks 
```

In this case, we examine the Greek cases. We see that the active cases almost 4 times higher than mid-April, which is bad news for Greece. With no doubt, Greece's case is worrisome. 

## Country comparison

In this subsection, we create a function to compare the cases of two countries. In this example, we will try comparing Germany and Italy. I chose these countries arbitrarily, the user may choose whatever countries she wishes for. Note that this graph is interactive, you may zoom is by selecting the area you are interested in, double click to zoom out.

```{r comp1}
#define function so that we can recall it later on
country1 <- "Italy"
country2 <- "Germany"

country_comp <- function(data, country1, country2) {
  
  #transform data dates
  country1 <- data[data$country==country1,]
  country1$date <- mdy(country1$date)
  
  country2 <- data[data$country==country2,]
  country2$date <- mdy(country2$date)
  
  #create plots
  fig1 <- country1 %>%
    ggplot( aes(x=date, y=cases)) +
    geom_area(fill="#66CC99", alpha=0.5) +
    geom_line(color="#66CC99") +
    ylab("Cases") +
    theme_ipsum()
  
  # Turn it interactive with ggplotly
  fig1 <- ggplotly(fig1)
  
  fig2 <- country2 %>%
    ggplot( aes(x=date, y=cases)) +
    geom_area(fill="#66CC99", alpha=0.5) +
    geom_line(color="#66CC99") +
    ylab("Cases") +
    theme_ipsum()
  
  # Turn it interactive with ggplotly
  fig2 <- ggplotly(fig2) 
    
  fig <- subplot(fig1, fig2, titleX = F, titleY = T, shareY = F, shareX = T, margin = 0.05)
  
  return(fig)
  
}

pc <- country_comp(covid19, country1, country2)
pc <- pc %>% layout(title = paste(country1, "vs.", country2, "cases comparison"))
pc

```

We see the two countries' case distributions are very similar, the new cases skyrocketed in April, while it seems like a second wave is hitting them both at this time (September), having a steady growth. The two countries had 100-300 cases mid-summer, while they get around 2000 new cases at the end of September. We could get some more info on how these countries are handling the virus spread by comparing the respective death numbers.

```{r comp2}
#define function so that we can recall it later on
country_comp_d <- function(data, country1, country2) {
  
  #transform data dates
  country1 <- data[data$country==country1,]
  country1$date <- mdy(country1$date)
  
  country2 <- data[data$country==country2,]
  country2$date <- mdy(country2$date)
  
  #create plots
  fig1 <- country1 %>%
    ggplot( aes(x=date, y=deaths)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("Deaths") +
    theme_ipsum()
  
  # Turn it interactive with ggplotly
  fig1 <- ggplotly(fig1)
  
  fig2 <- country2 %>%
    ggplot( aes(x=date, y=deaths)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("Deaths") +
    theme_ipsum()
  
  # Turn it interactive with ggplotly
  fig2 <- ggplotly(fig2) 
    
  fig <- subplot(fig1, fig2, titleX = F, titleY = T, shareY = T, shareX = T, margin = 0.05)
  
  return(fig)
  
}

pc2 <- country_comp_d(covid19, country1, country2)
pc2 <- pc2 %>% layout(title = paste(country1, "vs.", country2, "deaths comparison")) 
pc2
```

We see that the two distributions are completely different. Even though the new cases were almost identical, the death distribution is way worse in Italy, where there were almost 1000 deaths per day in late-March, where in Germany the worst day was mid-April with 315 deaths. This indicates that Germany's handle of the virus was way more effective than Italy's.

# Conclusions

This markdown presentation is a good tutorial for anyone who wants to dive into explanatory data analysis and doesn't know where to start. We saw plenty graphs, from static barcharts and maps to interactive time series. I chose this particular topic since it is a hot topic globally, but anyone can understand that there are countless cases these graphs can be applied on. 

As for the topic, we saw that the worst is not behind us. There seems to be a rise of the total confirmed cases globally, and having no vaccine on the horizon means that there will be no decline in the foreseeable future. Each country has its own tools to deal with the coronavirus, but the results vary. The reader may copy the code to investigate the handling of the virus of a particular country of interest. However, we should keep in mind that direct infections / deaths are not the only impact the virus has on our society. The virus [has taught businesses how to run remote offices](https://bit.ly/2RYI1eU), which could transform developing countries, as well as [reducing work and financial stress](https://bit.ly/36bfhId), which has probably led to lower suicide rates in countries like Japan! On the negative side, it has directly or inderictly impacted [unemploymend rates](https://bit.ly/33Wj49x), [human lives](https://bit.ly/2EzrKtC) and [loss of income](https://bit.ly/2EzVcjk) for many households.

What we all hope is that all this ends soon, having made us all stronger and more united.

Stay healthy!
