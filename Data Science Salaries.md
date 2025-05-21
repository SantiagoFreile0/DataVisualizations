---
title: "Data Science Salaries"
author: "Santiago Freile"
date: "2025-03-16"
output:
  github_document: #So you can add it to github (instead of html_document):
  toc: no
toc_float: yes
toc_collapsed: no
toc_depth: 2
theme: spacelab
pdf_document:
  keep_tex: true
---
  
```{=html}
<style type=text/css>
  body{ /* Normal  */
      font-size: 14px;
  }
h1.title {
  color: Black;
  font-size: 22px;
}
h1 { /* Header 1 */
    font-size: 20px;
  color: Black;
}
h2 { /* Header 2 */
    font-size: 18px;
  color: Black;
}

blockquote {
  background: #f8f8f8;
    padding: 5px 10px;
  margin: 0 0 10px;
  font-size: 15px;
  border-left: 1px solid seagreen;
  border-bottom: 1px solid seagreen;
}
</style>
```


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=T)
library(tidyverse) 
library(ggplot2) #Plots
library(naniar) #Missing Values
library(gridExtra) #Grid
library(viridis) #Colors
library(scales)
library(dplyr)
```

## Introduction

As data continues to become a central part of various industries, it has become increasingly important in the tech world, especially when it comes to salaries. Companies are eager to know what factors affect how much data scientists earn, and it's not always as simple as experience or education. Other factors like the size of the company or remote work options can also play a big role in salary differences. For example, many people think that bigger companies always pay more or that on-site work guarantees higher pay. However, is that really true? What are the key factors that play in determining a data scientist's salary?

This project try to answer these questions. I will analyze how salaries vary depending on experience, company size, work year, and remote work options. By using visual data, I'll uncover whether common beliefs about data science salaries are accurate or not. This analysis will help us understand what really drives salaries in the field and if there are any unexpected patterns.

## Data Source

The data used in this project comes from a dataset found on Kaggle, specifically the "Data Science Salaries" dataset. This dataset which is compiled from ‘aijobs.ne’ contains 3775 rows and 11 columns that provides information on salaries for data scientists, including details like years of experience, company size, remote work ratio, work year, company location, salary currency, between others.

## Methods

The visualization principles that I used in this project include position, shape, size, color, text, and orientation.

- Position is implemented by using vertical positioning for the axes, such as work year or experience level, to create clear comparisons between categories.
- Shape is incorporated through violin plots to display salary distributions, providing a clear view of salary spread and density.
- Size is utilized by adjusting the text labels, such as employee counts, to convey the volume of data for each category, making the plots more informative.
- Color is implemented by using different hues for each bar to differentiate between categories such as experience level and company size, visually highlighting important salary trends.
- Text is used for annotations to display values like median salary values and employee counts, adding contextual information to the plots.
- Orientation ensured that text annotations were aligned for readability, making the visualizations clear and understandable.

These choices principles make the data more accessible, easy to interpret, and visually engaging for the audience.

## Results

```{r, include=FALSE}
df <- read_csv('~/Desktop/Me/Linfield/2nd Semester/Data 225/Project 1/ds_salaries.csv',show_col_types = FALSE)
```

```{r, include=FALSE}
df2 <- df |>
  filter(salary_currency=='USD')

df2$work_year <- as.factor(df2$work_year)
df2$experience_level <- as.factor(df2$experience_level)
df2$remote_ratio <- as.factor(df2$remote_ratio)
df2$company_size <- as.factor(df2$company_size)

median_salary_wy <- df2 |>
  group_by(work_year) |>
  summarise(median_salary_w = median(salary_in_usd))
median_salary_el <- df2 |>
  group_by(experience_level) |>
  summarise(median_salary_e = median(salary_in_usd))
median_salary_rr <- df2 |>
  group_by(remote_ratio) |>
  summarise(median_salary_r = median(salary_in_usd))
median_salary_cs <- df2 |>
  group_by(company_size) |>
  summarise(median_salary_c = median(salary_in_usd))
```

```{r, include=FALSE}
df_counts_wy <- df2 |>
  group_by(work_year) |>
  summarise(count = n())

plot_work_year <- ggplot(df2, aes(x = work_year, y = salary_in_usd, fill = work_year)) +
  geom_violin()+
  geom_point(data=median_salary_wy, aes(x=work_year, y=median_salary_w), shape = 21, fill = 'white', color = 'black')+
  geom_text(data = median_salary_wy, aes(x = work_year, y = median_salary_w-10000, label = paste("Median: $", median_salary_w)), color = 'white', vjust = 1, size = c(2.8,2.1,2.7,3), family = "Times New Roman") +
  geom_text(data = df_counts_wy, aes(x = work_year, y = 0, label = paste("n =", count)), vjust = 1, size = 3, family = "Times New Roman") +
  labs(
    x = "Work Year",  
    y = NULL,             
    title = "Salary (USD) by Work Year",
    fill = 'Work Year') +
  scale_y_continuous(labels = scales::label_dollar(scale = 1, prefix = "$"), limits = c(0,500000))  +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold", size = 13),
    axis.title.x = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.title.y = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.title = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(family = "Times New Roman", size = 11),
    axis.text.y = element_text(family = "Times New Roman", size = 11))
```

```{r}
plot_work_year 
```
 

The results presented here illustrate how salaries were affected based on the year the job was issued. Several insights can be extract from the data:

- Salary: The visualization indicates that the highest salaries were recorded in 2020, with a maximum salary of 450,000 and a decent percentage of employees earning this amount. The median salary for this year was 105,500. In 2021, the maximum salary decreased slightly to 423,000, with a lower percentage of employees earning this amount, however, the median salary increased to 111,888. In 2022, the maximum salary dropped further to 405,000, with a very small percentage of employees earning this amount. Despite this, the median salary rose to 141,300, surpassing the previous years. Finally, in 2023, although the maximum salary was the lowest among these years at 385,000, the median salary reached its highest value at 147,100.

- Number of employees: The visualization shows that there were 38 employees in 2020, 122 employees in 2021, 1,418 employees in 2022, and 1,646 employees in 2023. Based on this data, the distribution of employees across these years was as follows:
  - 2020: 1.18%
  - 2021: 3.78%
  - 2022: 43.98%
  - 2023: 51.05%
  
With this data, we can see how Data Science jobs have been increasing over time.

```{r, include=FALSE}
df2$experience_level <- factor(df2$experience_level, levels = c("EN", "MI", "SE", "EX")) 

df_counts_el <- df2 |>
  group_by(experience_level) |>
  summarise(count = n())

plot_experience_level <- ggplot(df2, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_violin() +
  geom_point(data=median_salary_el, aes(x=experience_level, y=median_salary_e), shape = 21, fill = 'white', color = 'black')+
    geom_text(data = median_salary_el, aes(x = experience_level, y = median_salary_e-10000, label = paste("Median: ", c("$85,000", "$200,000", "$120,000", "$150,000"))), color = 'white', vjust = 1, size = c(3,2,3,3), family = "Times New Roman") + #ASK E, CAPA y SIZE
  geom_text(data = df_counts_el, aes(x = experience_level, y = 0, label = paste("n =", count)), vjust = 1, size = 3, family = "Times New Roman") +
  labs(
    x = "Experience Level",
    y = NULL,
    title = "Salary (USD) Distribution by Experience Level",
    fill = "Experience Level"
  ) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, prefix = "$"), limits = c(0,500000)) + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold", size = 13),
    axis.title.x = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.title.y = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.title = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(family = "Times New Roman", size = 11),
    axis.text.y = element_text(family = "Times New Roman", size = 11))
```

```{r}
plot_experience_level
```


This visualization shows how salary is affected by the experience level of the employee. We can observe the following:

- Salary: From the visualization, we can interpret that entry-level employees have a maximum salary of 300,000, but the median salary is 85,000. Mid/Intermediate-level employees have a maximum salary of 450,000, the highest in this visualization, but with a very small percentage in higher salary ranges, and a median of 120,000. Senior-level employees earn a maximum of 412,000, which is slightly lower but with more people earning close to this value, and a median of 150,000. Finally, executives/directors have a maximum salary of 416,000, with a significant percentage of them in the top salary range and a median of 200,000.

- Number of employees: The visualization shows that there were 230 employees being Entry-Level, 533 employees Mid-Level, 2359 employees Senior-Level, and 102 employees being Executives/Directors. Based on this data, the distribution of employees across these years was as follows:
  - Entry-Level: 7.13%
  - Mid-Level: 16.53%
  - Senior-Level: 73.17%
  - Executives/Directors: 3.16%

```{r, include=FALSE}
df_counts_rr <- df2 |>
  group_by(remote_ratio) |>
  summarise(count = n())

plot_remote_ratio <- ggplot(df2, aes(x = remote_ratio, y = salary_in_usd, fill = remote_ratio)) +
  geom_violin()+
  geom_point(data=median_salary_rr, aes(x=remote_ratio, y=median_salary_r), shape = 21, fill = 'white', color = 'black')+
  geom_text(data = median_salary_rr, aes(x = remote_ratio, y = median_salary_r-10000, label = paste("Median: ", c("$145,000", "$100,000", "$143,100"))), color = 'white', vjust = 1, size = c(3,2.5,3), family = "Times New Roman") + #Borde
  geom_text(data = df_counts_rr, aes(x = remote_ratio, y = 0, label = paste("n =", count)), vjust = 1, size = 3, family = "Times New Roman") + 
  labs(
    x = "Remote Ratio",  
    y = NULL,             
    title = "Salary (USD) by Remote Ratio",
    fill = 'Remote Ratio') +
  scale_y_continuous(labels = scales::label_dollar(scale = 1, prefix = "$"), limits = c(0,500000))  +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold", size = 13),
    axis.title.x = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.title.y = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.title = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(family = "Times New Roman", size = 11),
    axis.text.y = element_text(family = "Times New Roman", size = 11))
```

```{r}
plot_remote_ratio
```


This visualization demonstrates how salaries were affected by the amount of remote work in the job. Several insights can be derived from the data:

- Salary: From the visualization, we can see that the highest salaries are associated with in-office jobs, which have a maximum salary of 450,000. However, only a small percentage of people earn this amount, and the median salary is 145,000. For hybrid jobs, the maximum salary is 423,000, with a higher percentage of employees earning this amount, and the median salary is 100,000, which is significantly lower. Finally, full-remote jobs have a maximum salary of 416,000, but only a small percentage of people earn this amount, with the median salary also being 143,100.

- Number of employees: The visualization shows that there are 1,730 employees working in-office jobs, 66 employees working hybrid jobs, and 1,428 employees working full-remote. Based on this data, between 2020 and 2023:
  - In-office: 53.7%
  - Hybrid: 2%
  - Full-remote: 44.3%


```{r, include=FALSE}
df2$company_size <- factor(df2$company_size, levels = c("S", "M", "L")) 

df_counts_cs <- df2 |>
  group_by(company_size) |>
  summarise(count = n())

plot_company_size <- ggplot(df2, aes(x = company_size, y = salary_in_usd, fill = company_size)) +
  geom_violin() +
  geom_point(data=median_salary_cs, aes(x=company_size, y=median_salary_c), shape = 21, fill = 'white', color = 'black')+
  geom_text(data = median_salary_cs, aes(x = company_size, y = median_salary_c-10000, label = paste("Median: ", median_salary_c)), color = 'white', vjust = 1, size = 3, family = "Times New Roman") + #ASK COMO PONER CAPA NEGRA
  geom_text(data = df_counts_cs, aes(x = company_size, y = 0, label = paste("n =", count)), vjust = 1, size = 3, family = "Times New Roman") + 
  labs(
    x = "Company Size",  
    y = NULL,             
    title = "Salary (USD) by Company Size",
    fill = 'Company Size')+ 
  scale_y_continuous(labels = scales::label_dollar(scale = 1, prefix = "$"), limits = c(0,500000)) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold", size = 13),
        axis.title.x = element_text(family = "Times New Roman", face = "bold", size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "bold", size = 12),
        legend.title = element_text(family = "Times New Roman", face = "bold", size = 12),
        axis.text.x = element_text(family = "Times New Roman", size = 11),
        axis.text.y = element_text(family = "Times New Roman", size = 11))
```

```{r}
plot_company_size
```


The results presented in this visualization show how salaries were affected based on the size of the company where the job was issued. Several insights can be extracted from the data:

- Salary: The visualization indicates that small companies have a maximum salary of 416,000, but a relatively low percentage of employees reach the highest salary levels. The median salary for small companies is 81,000. In medium-sized companies, the maximum salary increases to 450,000, the highest among all company sizes, though a small percentage of employees earn at this level. The median salary for medium companies is 145,000, the highest median of all categories. Finally, large companies offer a maximum salary of 423,000, with a similarly low percentage of employees earning top salaries. Their median salary is slightly lower at 140,000.

- Number of employees: The visualization shows that there are 85 employees working in small companies, 2,847 employees working in medium companies, and 292 employees working in large companies. Based on this data, the distribution of employees across company sizes is as follows:
  - Small companies: 2.64%
  - Medium companies: 88.31%
  - Large companies: 9.06%

## Conclusion

The analysis of data science salaries reveals several important insights about how different factors influence compensation in the field. First, salary trends over the years show that while the maximum salaries have slightly decreased from 2020 to 2023, the median salary has consistently risen, highlighting a growing demand for data scientists. This shift may be indicative of the increasing importance of data science roles across industries.

Experience level plays a significant role in salary determination, with executives and senior-level professionals earning the highest median salaries. Interestingly, entry-level employees earn relatively lower salaries despite having a higher maximum salary potential, suggesting that a small percentage of employees at entry levels receive higher pay while the majority earn more modest amounts.

Remote work has also become an important factor in salary determination. In-office jobs offer the highest salaries, though these roles constitute a smaller portion of the professionals. Conversely, remote and hybrid roles show a balance between salary and the flexibility of working from home, with full-remote roles offering competitive salaries but fewer high-end earners.

The size of the company also plays a crucial role in salary distribution. Medium-sized companies offer the highest median salaries, but small and large companies offer a diverse salary range, with smaller companies seeing fewer employees in high-paying roles.

Overall, the analysis indicates that while several factors like experience, remote work, and company size all influence salary, the most significant determinant of salary appears to be the experience level, followed closely by company size. These insights should help aspiring data scientists better understand the various elements that contribute to their earning potential in this constantly evolving field.
