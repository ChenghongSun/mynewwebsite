---
categories:
- ""
- ""
date: "2020-10-26"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: blog1
title: COVID-19 data
---

# CDC COVID-19 Public Use Data

```{r, cache=TRUE}

#Let us revisit the [CDC Covid-19 Case Surveillance Data](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf). There are well over 3 million entries of individual, de-identified patient data. Since this is a large file, I suggest you use `vroom` to load it and you keep `cache=TRUE` in the chunk options.


# URL link to CDC to download data
url <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"

covid_data <- vroom(url)%>%
  clean_names()

```

```{r plot_1_data, echo=TRUE, results='hide'}

plot1_data <- covid_data %>% 
  
  # remove implicit NAs, "Missing", "Unknown" in columns
  filter(death_yn %in% c('Yes','No'), # contains "Missing","Unknown" 
         
         sex %in% c('Female','Male'), # contains "Other", not needed for the plot
         
         medcond_yn %in% c('Yes','No'), # contains "Missing","Unknown" 
         
         age_group != 'Unknown') %>% # contains "Unknown"
  
  # keep columns we want only for higher efficiency
  select(sex,age_group,death_yn,medcond_yn) %>% 
  
  # group_by age, sex, and co-morbidities status
  group_by(age_group,sex,medcond_yn) %>% 
  
  # count death and total cases within group
  summarise(death = sum(death_yn == 'Yes'), total = n()) %>% 
  
  # calculate death_rate = death/total
  mutate(death_rate = death/total)

#we review the dataset that we have specified
skim(plot1_data)

```

```{r plot_1_plotting}

plot1_data %>% 
  
  # create a new ggplot
  ggplot(aes(y = death_rate, 
             x = age_group)) + 
  
  # create a geom layer for the bars
  geom_col(fill = '#6b7da2') +
  
  # add data label for bars
  geom_text(size=2.5, aes(label = round(100*death_rate,digits = 1), # round number
                y = death_rate + 0.05)) + # adjust position for label
  
  # facet by sex & co-morbidities status
  facet_grid(cols = vars(sex),
             rows = vars(factor(medcond_yn, ordered = TRUE, # rename and order factor levels 
                                 levels = c('Yes','No'),
                                 labels = c('With comorbities',
                                            'Without comorbidities')))) + 
  # add labs
  labs(title = '',
       subtitle = 'Covid death % by age group, sex and presence of co-morbidities',
       x = '',
       y = '',
       caption = 'Source: CDC') +
  
  # death rate axis shown in percentage
  scale_y_continuous(labels = label_percent()) +
  
  # flip x and y axis
  coord_flip() +
  
  theme_bw()

```



```{r plot_2_data, echo=TRUE, results='hide'}

plot2_data <- covid_data %>% 
  
  # remove implicit NAs, "Missing", "Unknown" in columns
  filter(death_yn %in% c('Yes','No'), # contains "Missing","Unknown" 
         
         sex %in% c('Female','Male'), # contains "Other", not needed for the plot
         
         icu_yn %in% c('Yes','No'), # contains "Missing","Unknown" 
         
         age_group != 'Unknown') %>% # contains "Unknown"
  
  
  # keep columns we want only for higher efficiency
  select(sex,age_group,death_yn,icu_yn) %>% 
  
  # group_by age, sex, and icu status 
  group_by(age_group,sex,icu_yn) %>% 
  
  # count death and total cases within group
  summarise(death = sum(death_yn == 'Yes'), total = n()) %>% 
  
  # calculate death_rate = death/total
  mutate(death_rate = death/total)

#we review the dataset that we have specified
skim(plot2_data)

```

```{r plot_2_plotting}

plot2_data %>% 
  
  # create a new ggplot
  ggplot(aes(y = death_rate, 
             x = age_group)) + 
  
  # create a geom layer for the bars
  geom_col(fill = '#fb9584') +
  
  # add data label for bars
  geom_text(size=2.5, aes(label = round(100*death_rate,digits = 1), # round number
                y = death_rate + 0.05)) + # adjust position for label
  
  # facet by sex & icu status
  facet_grid(cols = vars(sex),
             rows = vars(factor(icu_yn, ordered = TRUE, # rename and order factor levels 
                                 levels = c('Yes','No'),
                                 labels = c('Admitted to ICU','Not admitted to ICU')))) + 

  # add labs
  labs(title = '',
       subtitle = 'Covid death % by age group, sex and whether patient was admitted to ICU',
       x = '',
       y = '',
       caption = 'Source: CDC') +
  
  # death rate axis shown in percentage
  scale_y_continuous(labels = label_percent()) +
  
  # flip x and y axis
  coord_flip() +
  
  theme_bw()

```
