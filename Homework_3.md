P8105\_hw3\_xy2404
================
Annie Yu
10/14/2018

Problem 1:
==========

``` r
library(p8105.datasets)
data(brfss_smart2010)
brfss =
  brfss_smart2010 %>% 
  janitor::clean_names() %>%
  filter(topic == "Overall Health") %>% 
  mutate(response = forcats::fct_relevel(response, c("Excellent", "Very good", "Good", "Fair", "Poor")))
```

The original dataset contains Year, Locationabbr, Locationdesc, Class, Topic, Question, Response, Sample\_Size, Data\_value, Confidence\_limit\_Low, Confidence\_limit\_High, Display\_order, Data\_value\_unit, Data\_value\_type, Data\_Value\_Footnote\_Symbol, Data\_Value\_Footnote, DataSource, ClassId, TopicId, LocationID, QuestionID, RESPID, GeoLocation variables. In original data, there are 23 variables. After data cleaning and focus on the topic of "Overall Health", there are 23 varibales for now dataset. Now the dataset is stored in the dataframe, which has 10625 rows and 23 columns. The new dataset also order the respose from "excellent" to "poor".

In 2002, which states were observed at 7 locations?
---------------------------------------------------

``` r
brfss %>%
  filter(year == '2002') %>% 
  group_by(locationabbr,locationdesc) %>% 
  summarize(n=n()) %>% 
  group_by(locationabbr) %>% 
  summarize(n=n()) %>% 
  filter(n == 7)
```

    ## # A tibble: 3 x 2
    ##   locationabbr     n
    ##   <chr>        <int>
    ## 1 CT               7
    ## 2 FL               7
    ## 3 NC               7

In this dataset, there are "CT", "FL", and "NC" which are observed at 7 locations.

Make a “spaghetti plot” that shows the number of locations in each state from 2002 to 2010.
-------------------------------------------------------------------------------------------

``` r
brfss %>% 
  group_by(locationabbr, year) %>% 
  summarise(n_obs = length(data_value)) %>% 
  ggplot(aes(x = year, y = n_obs, color = locationabbr))+
  geom_line()+
  ggtitle('The number of locations in each state from 2002 to 2010')+
  theme(legend.position = "bottom")
```

![](Homework_3_files/figure-markdown_github/unnamed-chunk-3-1.png)

Make a table showing, for the years 2002, 2006, and 2010, the mean and standard deviation of the proportion of “Excellent” responses across locations in NY State.
------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
  ny_excellent = brfss %>% 
  group_by(locationabbr) %>% 
  filter(locationabbr == "NY" & (year == "2002"|year == "2006"|year == "2010")) %>% 
  select(-c(topic, class, question, sample_size, confidence_limit_low:geo_location)) %>% 
  spread(key = response, value = data_value) %>% 
  group_by(year) %>% 
  summarize(mean_excellent = mean(Excellent),
            sd_excellent = sd(Excellent))

knitr::kable(ny_excellent)
```

|  year|  mean\_excellent|  sd\_excellent|
|-----:|----------------:|--------------:|
|  2002|         24.04000|       4.486424|
|  2006|         22.53333|       4.000833|
|  2010|         22.70000|       3.567212|

For the years 2002, the mean of "excellent" responses across locations in NY State is 24.04. For the years 2006, the mean of "excellent" responses across locations in NY State is 22.53. For the years 2010, the mean of "excellent" responses across locations in NY State is 22.79. For the years 2002, the standard deviation of "excellent" responses across locations in NY State is 4.486. For the years 2006, the standard deviation of "excellent" responses across locations in NY State is 4.001. For the years 2010, the standard deviation of "excellent" responses across locations in NY State is 3.567.

Make a five-panel plot that shows, for each response category separately, the distribution of these state-level averages over time.
-----------------------------------------------------------------------------------------------------------------------------------

``` r
five_panel <- brfss %>% 
 group_by(year, locationabbr,response) %>% 
  select(-c(topic, class, question, sample_size, confidence_limit_low:confidence_limit_high)) %>% 
  summarise(average_response = mean(data_value, na.rm = TRUE)) 
  
ggplot(five_panel, aes(x = year, y = average_response))+
    geom_point(alpha = 0.5)+
    facet_grid(~response)+
    stat_summary(fun.y = median, geom = "point", color = "blue", size =1)+
    viridis::scale_fill_viridis(discrete = TRUE)+
  labs(
    title = "Distribution of these state-level averages over time",
    x = "Year",
    y = "state level average of proportion"
  )
```

![](Homework_3_files/figure-markdown_github/unnamed-chunk-5-1.png)
