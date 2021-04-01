README
================
Carlos Siri

## Data Cleaning process

1)  data\_cleaning.R

Cleans files extracted from webrobots and filters for Technology
projects.

Output: Data

2)  text\_extractor.R

Input: Data

Extracts text from Data using Selenium and traditional scrapping. It
also tags pages that have the word patent in it. This is used for
auditing procedures later on.

### Patents

Output: data\_cleaned.csv

1)  Exploratory.io was used to setup the data. I need to convert this to
    R

Patents:techwtext\_with\_react

2)  Workstation.R Use to run analysis on the data

3)  Report.Rmd Markdown of analysis

### Sentiment Analysis

1)  Sentiments.R Sentiment Analysis

## Data

The dataset consists of Kickstarter **Technology** projects. The
original dataset was obtained from
[webrobots](https://webrobots.io/kickstarter-datasets/ "webrobots").
This data was used to webscrape the listed URLs.

The dataset used for this analysis can be obtained from the following
link: [Data](https://exploratory.io/data/cDb6UEd0cT/gmP2huP4Jo "Data")

## Variables

**State**: Project state \[ Successful, Failed, Cancelled and Live\].
For this analysis I eliminated Canceled and Live.

**Goal**: The amount of funds the project owners wish to collect.

**Generated variables**

**Has\_Patent**: Generated binary variable reflecting if the text within
the site has the word patent.It outputsthe web scrapping results of the
target words \[Patent|patent\].

**log\_goal**: Log of goal amount.
