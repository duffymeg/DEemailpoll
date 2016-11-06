Dynamic Ecology Email Poll Results
================
Meghan Duffy
November 6, 2016

Analysis of poll asking Dynamic Ecology readers about their email
=================================================================

Poll appeared October 24, 2016 and closed on November 6, 2016 Poll available at: <https://dynamicecology.wordpress.com/2016/10/24/how-do-you-manage-email/>

First, more about the poll and responses
----------------------------------------

Poll received just over 417 responses. One of these was completely blank, so I deleted it, leaving 416.

### Q1: How many work-related emails are in your inbox now?

    1 = Inbox zero!!!
    2 = < 20
    3 = 20-50
    4 = 50-100
    5 = 100-500
    6 = > 500

### Q2: What is your goal for the number of work-related emails you aim have in your inbox?

    1 = Inbox zero!!!
    2 = < 20
    3 = 20-50
    4 = 50-100
    5 = 100-500
    6 = > 500
    7 = I don't care how many emails are in my inbox

### Q3: How often do you feel overwhelmed by email?

    1 = never
    2 = rarely
    3 = sometimes
    4 = most of the time
    5 = always

### Q4: Which of the following best describes your current position?

    1 = Undergrad student
    2 = Grad student
    3 = Postdoc
    4 = Research scientist in academia
    5 = Assistant professor
    6 = Associate professor
    7 = Full professor
    8 = Technician
    9 = Research scientist in industry
    10 = Research scientist in government
    11 = Scientist at an NGO/non-profit
    12 = Other government position
    13 = Other

### Q5: How old are you?

    1 = <25
    2 = 25-34
    3 = 35-44
    4 = 45-54
    5 = 55-64
    6 = >65-74
    7 = >75

Results
-------

``` r
emaildata <- read.csv("DynamicEcologyemailquiz.csv", na.strings = ".")
head(emaildata)
#>                   Timestamp CurrentEmails CurrentEmails01  TargetEmails
#> 1 2016/10/24 1:04:10 PM EST           <20               2 Inbox zero!!!
#> 2 2016/10/24 1:06:43 PM EST Inbox zero!!!               1 Inbox zero!!!
#> 3 2016/10/24 1:08:57 PM EST Inbox zero!!!               1 Inbox zero!!!
#> 4 2016/10/24 1:18:35 PM EST Inbox zero!!!               1 Inbox zero!!!
#> 5 2016/10/24 1:20:44 PM EST          >500               6          >500
#> 6 2016/10/24 1:32:15 PM EST         20-50               3         20-50
#>   TargetEmails01 Overwhelmed Overwhelmed01                CurrentPosition
#> 1              1      Always             5            Assistant professor
#> 2              1      Rarely             2                        Postdoc
#> 3              1      Rarely             2                           <NA>
#> 4              1   Sometimes             3                        Postdoc
#> 5              6   Sometimes             3            Associate professor
#> 6              3   Sometimes             3 Scientist at an NGO/non-profit
#>   CurrentPosition01   Age Age01
#> 1                 5 35-44     3
#> 2                 3 35-44     3
#> 3                NA 25-34     2
#> 4                 3 25-34     2
#> 5                 6 35-44     3
#> 6                11 25-34     2
str(emaildata)
#> 'data.frame':    416 obs. of  11 variables:
#>  $ Timestamp        : Factor w/ 413 levels "2016/10/24 1:04:10 PM EST",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ CurrentEmails    : Factor w/ 6 levels "<20",">500","100-500",..: 1 6 6 6 2 4 4 1 1 2 ...
#>  $ CurrentEmails01  : int  2 1 1 1 6 3 3 2 2 6 ...
#>  $ TargetEmails     : Factor w/ 7 levels "<20",">500","100-500",..: 7 7 7 7 2 4 1 7 1 2 ...
#>  $ TargetEmails01   : int  1 1 1 1 6 3 2 1 2 6 ...
#>  $ Overwhelmed      : Factor w/ 5 levels "Always","Most of the time",..: 1 4 4 5 5 5 2 2 4 5 ...
#>  $ Overwhelmed01    : int  5 2 2 3 3 3 4 4 2 3 ...
#>  $ CurrentPosition  : Factor w/ 12 levels "Assistant professor",..: 1 7 NA 7 2 11 1 1 7 1 ...
#>  $ CurrentPosition01: int  5 3 NA 3 6 11 5 5 3 5 ...
#>  $ Age              : Factor w/ 6 levels "<25","25-34",..: 3 3 2 2 3 2 3 3 2 2 ...
#>  $ Age01            : int  3 3 2 2 3 2 3 3 2 2 ...
```

``` r
CurrentSum <- 
  emaildata %>%
  filter(!is.na(CurrentEmails01)) %>%
  group_by(CurrentEmails01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

CurrentSum
#> # A tibble: 6 x 3
#>   CurrentEmails01     n rel.freq
#>             <int> <int>    <dbl>
#> 1               1    60       15
#> 2               2   130       32
#> 3               3    57       14
#> 4               4    31        8
#> 5               5    40       10
#> 6               6    90       22

as.factor(CurrentSum$CurrentEmails01)
#> [1] 1 2 3 4 5 6
#> Levels: 1 2 3 4 5 6

currentemailplot <- ggplot(CurrentSum,aes(x=CurrentEmails01,y=rel.freq)) +
  geom_bar(stat="identity") +
  ylab("Percent of responses") +
  theme(legend.position="none") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  annotate("text", x = 1, y = 1, label = "Inbox zero!!!", hjust=0, size=6, color = "white") +
  annotate("text", x = 2, y = 1, label = "< 20", hjust=0, size=6, color = "white") +
  annotate("text", x = 3, y = 1, label = "20-50", hjust=0, size=6, color = "white") +
  annotate("text", x = 4, y = 1, label = "50-100", hjust=0, size=6, color = "white") +
  annotate("text", x = 5, y = 1, label = "100-500", hjust=0, size=6, color = "white") +
  annotate("text", x = 6, y = 1, label = "> 500", hjust=0, size=6, color = "white") +
  ggtitle("How many work-related emails are in your inbox now?") +
  theme(plot.title=element_text(size=12)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.ticks.y = element_blank())

save_plot("currentemailplot.jpg", currentemailplot)

currentemailplot
```

![](DEemailpoll_files/figure-markdown_github/summarize%20current%20inbox%20status-1.png)

``` r
TargetSum <- 
  emaildata %>%
  filter(!is.na(TargetEmails01)) %>%
  group_by(TargetEmails01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

TargetSum
#> # A tibble: 7 x 3
#>   TargetEmails01     n rel.freq
#>            <int> <int>    <dbl>
#> 1              1   162       39
#> 2              2   125       30
#> 3              3    24        6
#> 4              4    12        3
#> 5              5     8        2
#> 6              6    13        3
#> 7              7    69       17

as.factor(TargetSum$TargetEmails01)
#> [1] 1 2 3 4 5 6 7
#> Levels: 1 2 3 4 5 6 7

targetemailplot <- ggplot(TargetSum,aes(x=TargetEmails01,y=rel.freq)) +
  geom_bar(stat="identity") +
  ylab("Percent of responses") +
  theme(legend.position="none") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  annotate("text", x = 1, y = 1, label = "Inbox zero!!!", hjust=0, size=6, color = "white") +
  annotate("text", x = 2, y = 1, label = "< 20", hjust=0, size=6, color = "white") +
  annotate("text", x = 3, y = 1, label = "20-50", hjust=0, size=6, color = "white") +
  annotate("text", x = 4, y = 4, label = "50-100", hjust=0, size=6, color = "black") +
  annotate("text", x = 5, y = 4, label = "100-500", hjust=0, size=6, color = "black") +
  annotate("text", x = 6, y = 4, label = "> 500", hjust=0, size=6, color = "black") + 
  annotate("text", x = 7, y = 1, label = "I don't care", hjust=0, size=6, color = "white") +
  ggtitle("What is your goal for the number of work-related \nemails you aim have in your inbox?") +
  theme(plot.title=element_text(size=12)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.ticks.y = element_blank())

save_plot("targetemailplot.jpg", targetemailplot)

currentemailplot
```

![](DEemailpoll_files/figure-markdown_github/summarize%20target%20inbox%20status-1.png)

``` r
OverwhelmedSum <- 
  emaildata %>%
  filter(!is.na(Overwhelmed01)) %>%
  group_by(Overwhelmed01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

OverwhelmedSum
#> # A tibble: 5 x 3
#>   Overwhelmed01     n rel.freq
#>           <int> <int>    <dbl>
#> 1             1     8        2
#> 2             2    98       24
#> 3             3   183       44
#> 4             4    90       22
#> 5             5    33        8

as.factor(OverwhelmedSum$Overwhelmed01)
#> [1] 1 2 3 4 5
#> Levels: 1 2 3 4 5

overwhelmedplot <- ggplot(OverwhelmedSum,aes(x=Overwhelmed01,y=rel.freq)) +
  geom_bar(stat="identity") +
  ylab("Percent of responses") +
  theme(legend.position="none") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  annotate("text", x = 1, y = 3, label = "never", hjust=0, size=6, color = "black") +
  annotate("text", x = 2, y = 1, label = "rarely", hjust=0, size=6, color = "white") +
  annotate("text", x = 3, y = 1, label = "sometimes", hjust=0, size=6, color = "white") +
  annotate("text", x = 4, y = 1, label = "most of the time", hjust=0, size=6, color = "white") +
  annotate("text", x = 5, y = 1, label = "always", hjust=0, size=6, color = "white") +
  ggtitle("How often do you feel overwhelmed by email?") +
  theme(plot.title=element_text(size=12)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.ticks.y = element_blank())

save_plot("overwhelmedplot.jpg", overwhelmedplot)

overwhelmedplot
```

![](DEemailpoll_files/figure-markdown_github/summarize%20overwhelmed%20status-1.png)

Now that basic results are plotted, plan to do Likert analyses, similar to ones done here: <https://github.com/duffymeg/DEAuthorshipPoll/blob/master/AuthorshipPollAnalysis.Rmd>
