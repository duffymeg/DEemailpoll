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
#> Source: local data frame [6 x 3]
#> 
#>   CurrentEmails01     n rel.freq
#>             (int) (int)    (dbl)
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
#> Source: local data frame [7 x 3]
#> 
#>   TargetEmails01     n rel.freq
#>            (int) (int)    (dbl)
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
#> Source: local data frame [5 x 3]
#> 
#>   Overwhelmed01     n rel.freq
#>           (int) (int)    (dbl)
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

Now that basic results are plotted, moving on to some more interesting things. First, some cross tabs:

``` r
xtabs(~CurrentEmails+CurrentPosition,data=emaildata)
#>                CurrentPosition
#> CurrentEmails   Assistant professor Associate professor Full professor
#>   <20                            22                  11              7
#>   >500                           21                  15             19
#>   100-500                        13                   4              5
#>   20-50                          10                   7              4
#>   50-100                         10                   5              4
#>   Inbox zero!!!                   8                   5              2
#>                CurrentPosition
#> CurrentEmails   Grad student Other Other government position Postdoc
#>   <20                     45     1                         1      25
#>   >500                     5     2                         3      12
#>   100-500                  6     2                         1       2
#>   20-50                   10     0                         2      12
#>   50-100                   6     1                         0       2
#>   Inbox zero!!!           15     1                         0      20
#>                CurrentPosition
#> CurrentEmails   Research scientist in academia
#>   <20                                        7
#>   >500                                       8
#>   100-500                                    1
#>   20-50                                      4
#>   50-100                                     0
#>   Inbox zero!!!                              3
#>                CurrentPosition
#> CurrentEmails   Research scientist in government
#>   <20                                          4
#>   >500                                         2
#>   100-500                                      1
#>   20-50                                        4
#>   50-100                                       1
#>   Inbox zero!!!                                4
#>                CurrentPosition
#> CurrentEmails   Research scientist in industry
#>   <20                                        1
#>   >500                                       2
#>   100-500                                    1
#>   20-50                                      0
#>   50-100                                     0
#>   Inbox zero!!!                              0
#>                CurrentPosition
#> CurrentEmails   Scientist at an NGO/non-profit Undergraduate student
#>   <20                                        4                     2
#>   >500                                       1                     0
#>   100-500                                    4                     0
#>   20-50                                      3                     1
#>   50-100                                     1                     1
#>   Inbox zero!!!                              0                     1
xtabs(~Overwhelmed+CurrentPosition,data=emaildata)
#>                   CurrentPosition
#> Overwhelmed        Assistant professor Associate professor Full professor
#>   Always                             9                   8              7
#>   Most of the time                  28                  16             13
#>   Never                              1                   1              0
#>   Rarely                            15                   6              5
#>   Sometimes                         33                  19             13
#>                   CurrentPosition
#> Overwhelmed        Grad student Other Other government position Postdoc
#>   Always                      0     3                         2       1
#>   Most of the time            9     2                         0      12
#>   Never                       2     0                         0       2
#>   Rarely                     28     1                         4      20
#>   Sometimes                  47     3                         1      39
#>                   CurrentPosition
#> Overwhelmed        Research scientist in academia
#>   Always                                        1
#>   Most of the time                              3
#>   Never                                         1
#>   Rarely                                        7
#>   Sometimes                                    11
#>                   CurrentPosition
#> Overwhelmed        Research scientist in government
#>   Always                                          0
#>   Most of the time                                4
#>   Never                                           1
#>   Rarely                                          7
#>   Sometimes                                       4
#>                   CurrentPosition
#> Overwhelmed        Research scientist in industry
#>   Always                                        1
#>   Most of the time                              0
#>   Never                                         0
#>   Rarely                                        0
#>   Sometimes                                     3
#>                   CurrentPosition
#> Overwhelmed        Scientist at an NGO/non-profit Undergraduate student
#>   Always                                        1                     0
#>   Most of the time                              2                     1
#>   Never                                         0                     0
#>   Rarely                                        2                     2
#>   Sometimes                                     8                     2
xtabs(~TargetEmails+CurrentPosition,data=emaildata)
#>                                               CurrentPosition
#> TargetEmails                                   Assistant professor
#>   <20                                                           26
#>   >500                                                           2
#>   100-500                                                        2
#>   20-50                                                          6
#>   50-100                                                         1
#>   I don't care how many emails are in my inbox                  19
#>   Inbox zero!!!                                                 30
#>                                               CurrentPosition
#> TargetEmails                                   Associate professor
#>   <20                                                           15
#>   >500                                                           3
#>   100-500                                                        4
#>   20-50                                                          3
#>   50-100                                                         1
#>   I don't care how many emails are in my inbox                   6
#>   Inbox zero!!!                                                 18
#>                                               CurrentPosition
#> TargetEmails                                   Full professor Grad student
#>   <20                                                       9           24
#>   >500                                                      4            3
#>   100-500                                                   0            1
#>   20-50                                                     2            3
#>   50-100                                                    5            0
#>   I don't care how many emails are in my inbox             11            8
#>   Inbox zero!!!                                             9           48
#>                                               CurrentPosition
#> TargetEmails                                   Other
#>   <20                                              1
#>   >500                                             2
#>   100-500                                          0
#>   20-50                                            1
#>   50-100                                           2
#>   I don't care how many emails are in my inbox     0
#>   Inbox zero!!!                                    3
#>                                               CurrentPosition
#> TargetEmails                                   Other government position
#>   <20                                                                  3
#>   >500                                                                 0
#>   100-500                                                              0
#>   20-50                                                                2
#>   50-100                                                               0
#>   I don't care how many emails are in my inbox                         2
#>   Inbox zero!!!                                                        0
#>                                               CurrentPosition
#> TargetEmails                                   Postdoc
#>   <20                                               23
#>   >500                                               0
#>   100-500                                            0
#>   20-50                                              2
#>   50-100                                             0
#>   I don't care how many emails are in my inbox      13
#>   Inbox zero!!!                                     35
#>                                               CurrentPosition
#> TargetEmails                                   Research scientist in academia
#>   <20                                                                       7
#>   >500                                                                      0
#>   100-500                                                                   0
#>   20-50                                                                     1
#>   50-100                                                                    1
#>   I don't care how many emails are in my inbox                              6
#>   Inbox zero!!!                                                             8
#>                                               CurrentPosition
#> TargetEmails                                   Research scientist in government
#>   <20                                                                         7
#>   >500                                                                        0
#>   100-500                                                                     0
#>   20-50                                                                       1
#>   50-100                                                                      1
#>   I don't care how many emails are in my inbox                                2
#>   Inbox zero!!!                                                               5
#>                                               CurrentPosition
#> TargetEmails                                   Research scientist in industry
#>   <20                                                                       2
#>   >500                                                                      0
#>   100-500                                                                   0
#>   20-50                                                                     0
#>   50-100                                                                    0
#>   I don't care how many emails are in my inbox                              1
#>   Inbox zero!!!                                                             1
#>                                               CurrentPosition
#> TargetEmails                                   Scientist at an NGO/non-profit
#>   <20                                                                       5
#>   >500                                                                      0
#>   100-500                                                                   1
#>   20-50                                                                     3
#>   50-100                                                                    1
#>   I don't care how many emails are in my inbox                              1
#>   Inbox zero!!!                                                             2
#>                                               CurrentPosition
#> TargetEmails                                   Undergraduate student
#>   <20                                                              3
#>   >500                                                             0
#>   100-500                                                          0
#>   20-50                                                            0
#>   50-100                                                           0
#>   I don't care how many emails are in my inbox                     0
#>   Inbox zero!!!                                                    2
```

Now, using the Likert package:

``` r
## first, subsetting the data to grad students, postdocs, asst/assoc/full profs
academicpathdata <- subset(emaildata, CurrentPosition == "Grad student" | CurrentPosition == "Postdoc" | CurrentPosition == "Assistant professor" | CurrentPosition == "Associate professor" | CurrentPosition == "Full professor")

currentdata <-
  academicpathdata %>%
  filter(CurrentEmails01 != "NA", TargetEmails01 != "NA", 
         Overwhelmed01 != "NA", Age01 != "NA") %>% 
  select(CurrentEmails)

# order the data
currentdata$CurrentEmails <- factor(currentdata$CurrentEmails, 
                                      c("Inbox zero!!!",
                                        "<20",
                                        "20-50",
                                        "50-100",
                                        "100-500",
                                        ">500"))

# changing the column name to the question
colnames(currentdata)[1] <- "How many work-related emails are in your inbox now?"

## subsetting the likert data AND the grouping variables
currentdata_grouping <-
  academicpathdata %>%
  filter(CurrentEmails01 != "NA", TargetEmails01 != "NA", 
         Overwhelmed01 != "NA", Age01 != "NA") %>%
  select(TargetEmails, Overwhelmed, CurrentPosition, Age) 

currentdata_grouping$TargetEmails <- factor(currentdata_grouping$TargetEmails, 
                                      c("Inbox zero!!!",
                                        "<20",
                                        "20-50",
                                        "50-100",
                                        "100-500",
                                        ">500",
                                        "I don't care how many emails are in my inbox"))

currentdata_grouping$Overwhelmed <- factor(currentdata_grouping$Overwhelmed, 
                                            c("Never", 
                                              "Rarely", 
                                              "Sometimes",
                                              "Most of the time",
                                              "Always"))

currentdata_grouping$CurrentPosition <- factor(currentdata_grouping$CurrentPosition, 
                                            c("Grad student", 
                                              "Postdoc",
                                              "Assistant professor",
                                              "Associate professor",
                                              "Full professor"))

likert_11 <- likert(currentdata, grouping = currentdata_grouping$CurrentPosition)
likert_11plot <- plot(likert_11)
save_plot("likert_11.jpg", likert_11plot, base_height = 4, base_width = 10)
likert_11plot
```

![](DEemailpoll_files/figure-markdown_github/Likert%20cross%20tabs%20on%20current%20email%20data-1.png)

``` r

## this next plot isn't ordering correctly. Need to figure that out.
likert_12 <- likert(currentdata, grouping = currentdata_grouping$TargetEmails)
likert_12plot <- plot(likert_12)
save_plot("likert_12.jpg", likert_12plot, base_height = 4, base_width = 10)
likert_12plot
```

![](DEemailpoll_files/figure-markdown_github/Likert%20cross%20tabs%20on%20current%20email%20data-2.png)

``` r

## this next plot would benefit from having "How often do you feel overwhelmed by email?" on the y-axis to explain the grouping variable.
likert_13 <- likert(currentdata, grouping = currentdata_grouping$Overwhelmed)
likert_13plot <- plot(likert_13) + xlab("How often do you feel \noverwhelmed by email?")
save_plot("likert_13.jpg", likert_13plot, base_height = 4, base_width = 10)
likert_13plot
```

![](DEemailpoll_files/figure-markdown_github/Likert%20cross%20tabs%20on%20current%20email%20data-3.png)

``` r
targetdata <-
  academicpathdata %>%
  filter(CurrentEmails01 != "NA", TargetEmails01 != "NA", 
         Overwhelmed01 != "NA", Age01 != "NA") %>% 
  select(TargetEmails)

# order the data
targetdata$TargetEmails <- factor(targetdata$TargetEmails, 
                                      c("Inbox zero!!!",
                                        "<20",
                                        "20-50",
                                        "50-100",
                                        "100-500",
                                        ">500",
                                        "I don't care how many emails are in my inbox"))

# changing the column name to the question
colnames(targetdata)[1] <- "What is your goal for the number of work-related emails you aim have in your inbox?"

## subsetting the likert data AND the grouping variables
targetdata_grouping <-
  academicpathdata %>%
  filter(CurrentEmails01 != "NA", TargetEmails01 != "NA", 
         Overwhelmed01 != "NA", Age01 != "NA") %>%
  select(CurrentEmails, Overwhelmed, CurrentPosition, Age) 

targetdata_grouping$CurrentEmails <- factor(targetdata_grouping$CurrentEmails, 
                                            c("Inbox zero!!!", 
                                              "<20", 
                                              "20-50",
                                              "50-100",
                                              "100-500", 
                                              ">500"))

targetdata_grouping$Overwhelmed <- factor(targetdata_grouping$Overwhelmed, 
                                            c("Never", 
                                              "Rarely", 
                                              "Sometimes",
                                              "Most of the time",
                                              "Always"))

targetdata_grouping$CurrentPosition <- factor(targetdata_grouping$CurrentPosition, 
                                            c("Grad student", 
                                              "Postdoc",
                                              "Assistant professor",
                                              "Associate professor",
                                              "Full professor"))

likert_21 <- likert(targetdata, grouping = targetdata_grouping$CurrentPosition)
likert_21plot <- plot(likert_21)
save_plot("likert_21.jpg", likert_21plot, base_height = 4, base_width = 10)
likert_21plot
```

![](DEemailpoll_files/figure-markdown_github/Likert%20cross%20tabs%20on%20target%20email%20data-1.png)

``` r

likert_22 <- likert(targetdata, grouping = targetdata_grouping$CurrentEmails)
likert_22plot <- plot(likert_22)
save_plot("likert_22.jpg", likert_22plot, base_height = 4, base_width = 10)
likert_22plot
```

![](DEemailpoll_files/figure-markdown_github/Likert%20cross%20tabs%20on%20target%20email%20data-2.png)

``` r

likert_23 <- likert(targetdata, grouping = targetdata_grouping$Overwhelmed)
likert_23plot <- plot(likert_23)
save_plot("likert_23.jpg", likert_23plot, base_height = 4, base_width = 10)
likert_23plot
```

![](DEemailpoll_files/figure-markdown_github/Likert%20cross%20tabs%20on%20target%20email%20data-3.png)

``` r
overwhelmeddata <-
  academicpathdata %>%
  filter(CurrentEmails01 != "NA", TargetEmails01 != "NA", 
         Overwhelmed01 != "NA", Age01 != "NA") %>% 
  select(Overwhelmed)

# order the data
overwhelmeddata$Overwhelmed <- factor(overwhelmeddata$Overwhelmed, 
                                      c("Never", 
                                        "Rarely", 
                                        "Sometimes",
                                        "Most of the time",
                                        "Always"))

# changing the column name to the question
colnames(overwhelmeddata)[1] <- "How often do you feel overwhelmed by email?"

## subsetting the likert data AND the grouping variables

overwhelmeddata_grouping <-
  academicpathdata %>%
  filter(CurrentEmails01 != "NA", TargetEmails01 != "NA", 
         Overwhelmed01 != "NA", Age01 != "NA") %>%
  select(CurrentEmails, TargetEmails, CurrentPosition, Age) 

overwhelmeddata_grouping$CurrentEmails <- factor(overwhelmeddata_grouping$CurrentEmails, 
                                            c("Inbox zero!!!", 
                                              "<20", 
                                              "20-50",
                                              "50-100",
                                              "100-500", 
                                              ">500"))
overwhelmeddata_grouping$TargetEmails <- factor(overwhelmeddata_grouping$TargetEmails, 
                                            c("Inbox zero!!!", 
                                              "<20", 
                                              "20-50",
                                              "50-100",
                                              "100-500", 
                                              ">500",
                                              "I don't care how many emails are in my inbox"))

overwhelmeddata_grouping$CurrentPosition <- factor(overwhelmeddata_grouping$CurrentPosition, 
                                            c("Grad student", 
                                              "Postdoc",
                                              "Assistant professor",
                                              "Associate professor",
                                              "Full professor"))

likert_31 <- likert(overwhelmeddata, grouping = overwhelmeddata_grouping$CurrentPosition)
likert_31plot <- plot(likert_31)
save_plot("likert_31.jpg", likert_31plot, base_height = 4, base_width = 10)
likert_31plot
```

![](DEemailpoll_files/figure-markdown_github/Likert%20analysis%20of%20overwhelmed%20question-1.png)

``` r
# First, make a new variable that splits these out
diffpathdata <- subset(emaildata, CurrentPosition == "AssistantProfessor" | CurrentPosition == "Associate professor" | CurrentPosition == "Full professor" | CurrentPosition == "Research scientist in government" | CurrentPosition == "Research scientist in academia" | CurrentPosition == "Research scientist in industry" | CurrentPosition == "Scientist at an NGO/non-profit" | CurrentPosition == "Other government position")

diffpathdata$path <- ifelse(diffpathdata$CurrentPosition == "AssistantProfessor" | diffpathdata$CurrentPosition == "Associate professor" | diffpathdata$CurrentPosition == "Full professor" | diffpathdata$CurrentPosition == "Research scientist in academia", "academia",
                            ifelse(diffpathdata$CurrentPosition == "Research scientist in government" | diffpathdata$CurrentPosition == "Other government position", "government",
                            ifelse(diffpathdata$CurrentPosition == "Scientist at an NGO/non-profit", "nonprofit","industry"
                            )))

PathSampleSize <-
  diffpathdata %>%
  group_by(path) %>%
  summarise(n=n())

PathSampleSize
#> Source: local data frame [4 x 2]
#> 
#>         path     n
#>        (chr) (int)
#> 1   academia   114
#> 2 government    23
#> 3   industry     4
#> 4  nonprofit    13

#hmm, small sample sizes. Not sure it would be useful to compare them, unfortunately.
```
