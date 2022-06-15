library(tidyverse)
library(openintro)

# load data
data(hsb2)

# view data
glimpse(hsb2)

# Factors
table(hsb2$schtyp)

# or
hsb2_public <- hsb2 %>% 
  filter(schtyp == "public")

hsb2_public$schtyp <- droplevels(hsb2_public$schtyp)
table(hsb2_public$schtyp)

# Discretize a variable

# calculate average reading score and show the value
avg_read <- mean(hsb2$read)
avg_read

# how many are above and below average
hsb2 <- hsb2 %>% 
  mutate(read_cat = ifelse(read < avg_read, "below average", "at or above average"))

table(hsb2$read_cat)
# or dplyr way...
hsb2 %>% 
  count(read_cat)

data(email50)
glimpse(email50)

email50_fortified <- email50 %>% 
  mutate(number_yn = case_when(
    # if number is none
    number == "none" ~ "no",
    # if number is not none
    number != "none" ~ "yes"))

# Visualize the distribution of number_yn
email50_fortified %>% 
  ggplot(aes(x = number_yn)) +
  geom_bar()

# Visualizing numerical data
# scatterplot of math vs science scores
hsb2 %>% 
  ggplot(aes(science, math)) +
  geom_point()

# scatterplot of math vs science scores, controlling for program
hsb2 %>% 
  ggplot(aes(science, math, color=prog)) +
  geom_point()

# email50, scatter plot of exclaim_mess vs. num_char
email50 %>% 
  ggplot(aes(num_char, exclaim_mess, color=factor(spam))) +
  geom_point()


# Observational Studies and Experiments
# Random Sampling and random assignments
# Simpson's paradox
data(UCBAdmissions)
dim(UCBAdmissions)
dimnames(UCBAdmissions)
glimpse(UCBAdmissions)
# examine the relationship between gender and Admit
margin.table(UCBAdmissions, c(1, 2))

# converting into dataframe
ucb_admit <- as.data.frame(UCBAdmissions)
ucb_df <- ucb_admit %>% 
  slice(rep(1:n(), Freq)) %>% 
  select(-Freq)
glimpse(ucb_df)

ucb_adm_count <- ucb_df %>% 
  count(Gender, Admit)

# proportion 
ucb_df %>% 
  with(table(Gender, Admit)) %>% 
  prop.table(margin = 2)

# proportion of admitted
ucb_adm_count %>% 
  group_by(Gender) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(Admit == "Admitted")

# count taking dept also in account
ucb_counts <- ucb_df %>% 
  count(Dept, Gender, Admit)
ucb_counts

# proportion of males admitted dept wise
males_admitted_dept <- ucb_counts %>% 
  group_by(Dept, Gender) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(Gender == "Male", Admit == "Admitted")

# proportion of females admitted dept wise
females_admitted_dept <- ucb_counts %>% 
  group_by(Dept, Gender) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(Gender == "Female", Admit == "Admitted")

# making a dataframe of male and female admision prop
dept = males_admitted_dept$Dept
males_prop <- males_admitted_dept$prop
females_prop <- females_admitted_dept$prop
admitted_prop_df <- data.frame(dept, males_prop, females_prop)
admitted_prop_df
