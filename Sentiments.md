Sentiment Analysis Kickstarter
================
Carlos Siri

This report describes the steps taken to perform an exploratory
sentiment analysis of kickstarter projects. The report is structured as
follows:

  - Text Preparation
  - Sentiment Categorization with NRC
  - Models
  - Next Steps

## Text Preparation

The following section generates un-nested words with no stop words in
two separate dataframes: *df\_words\_blurb*: unnested campaign blurbs
*df\_words\_body*: unnested campaign text bodies

``` r
### blurb
df_words_blurb <- df %>%
  mutate(text_ID = row_number()) %>% # generate id for the project
  group_by(text_ID) %>% # group by the project
  unnest_tokens(word, blurb) %>% # generate a column for each word
  ungroup() %>% 
  anti_join(stop_words) %>% # eliminate all stop words
  mutate(n = 1, #generate a column with only 1's (useful later)
         word_ID = row_number()) # generate an ID word the word 
```

    ## Joining, by = "word"

``` r
#the following repetitive text is eliminated from all projects
df$p_HTML <- str_remove(df$p_HTML, "\na prototype is a preliminary model of something. projects that offer physical products need to show backers documentation of a working prototype. this gallery features photos, videos, and other visual documentation that will give backers a sense of what’s been accomplished so far and what’s left to do. though the development process can vary for each project, these are the stages we typically see:\n \nexplorations that test ideas and functionality.\n \ndemonstrates the functionality of the final product, but looks different.\n \nlooks like the final product, but is not functional.\n \nappearance and function match the final product, but is made with different manufacturing methods.\n \nappearance, function, and manufacturing methods match the final product.\n \nthese photos and videos provide a detailed look at this project’s development.\n")

df <- df %>% mutate(campaign_body = paste(p_HTML, react_campaign))
df_words_body <- df %>%
  mutate(text_ID = row_number()) %>% # generate id for the project
  group_by(text_ID) %>% # group by the project
  unnest_tokens(word, campaign_body) %>%  # generate a column for each word
  ungroup() %>%
  anti_join(stop_words) %>% # eliminate all stop words
  mutate(n = 1, #generate a column with only 1's (useful later)
         word_ID = row_number()) # generate an ID word the word 
```

    ## Joining, by = "word"

## Sentiment Categorization with NRC

The following section utilizes R’s *NRC* lexicon, which generates the
following binary word categories: positive, negative, anger,
anticipation, disgust, fear, joy, sadness, surprise, and trust. The
binary categories are then used to generate a variable that calculates
the proportion of words that convey a sentiment from all the words in
the blurb or body of the project. The following three dataframes are
generated:

*df\_sentiments\_blurb*: Blurb words categorized with NRC
*df\_sentiments\_body*: Body words categorized with NRC
*df\_sentiments\_all*: Blurb and body words categorized with NRC

We can also use the following lexicons offered by the Tidyr package:

*AFINN*: Assigns words with a score that runs between -5 and 5, with
negative scores indicating negative sentiment and positive scores
indicating positive sentiment. *bing*: Binary categorization of positive
and negative words *loughran*: English sentiment lexicon created for use
with financial documents. This lexicon labels words with six possible
sentiments important in financial contexts: “negative”, “positive”,
“litigious”, “uncertainty”, “constraining”, or “superfluous”.

``` r
df_sentiments_blurb <- df_words_blurb %>%
  mutate(count = n) %>% 
  inner_join(get_sentiments("nrc")) %>% #get nrc sentiments
  spread(sentiment, n, fill = 0) %>%
  group_by(text_ID) %>% # group by project
  # generate new columns with the sum the binary categorization of words 
  summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), 
            fear = sum(fear), joy = sum(joy), negative = sum(negative), positive = sum(positive),
            sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), count = sum(count), Project_Successful = first(Project_Successful)) %>% 
  # generate new columns with the proportion of words conveying the sentiment
  mutate(anger = anger/count, anticipation = anticipation/count, disgust = disgust/count,
         fear = fear/count, joy = joy/count, negative = negative/count, positive = positive/count,
         sadness = sadness/count, surprise = surprise/count, trust = trust/count,
         section = "blurb") %>%
  select(-count)
```

    ## Joining, by = "word"

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
df_sentiments_body <- df_words_body %>%
  mutate(count = n) %>%
  inner_join(get_sentiments("nrc")) %>% #get nrc sentiments
  spread(sentiment, n, fill = 0) %>%
  group_by(text_ID) %>% # group by project
    # generate new columns with the sum the binary categorization of words 
  summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
            fear = sum(fear), joy = sum(joy), negative = sum(negative), positive = sum(positive),
            sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), count = sum(count), Project_Successful = first(Project_Successful)) %>%
   # generate new columns with the proportion of words conveying the sentiment
  mutate(anger = anger/count, anticipation = anticipation/count, disgust = disgust/count,
         fear = fear/count, joy = joy/count, negative = negative/count, positive = positive/count,
         sadness = sadness/count, surprise = surprise/count, trust = trust/count,
         section = "body") %>%
  select(-count)
```

    ## Joining, by = "word"
    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
df_sentiments_all <- rbind(df_sentiments_body, df_sentiments_blurb) # appends df_sentiments_campaign and df_sentiments_blurb
```

## Models

The following section generates models with Project\_Successful as a DV,
a binary variable that categorizes if the project achieved the
campaign’s goal, and all the NRC sentiments as IVs. The following
three models are generated:

*fit\_blurb*: a model using only the blurb’s text *fit\_body*: a model
using only the body’s text *fit\_all*: a model using only all the text
*fit\_section*: a model using all the text and interacting each
sentiment with section(blurb, body)

``` r
fit_blurb <- glm(Project_Successful ~ anger + anticipation + disgust +
             fear + joy + sadness + surprise + trust + positive + negative, data=df_sentiments_blurb, family="binomial")
fit_body <- glm(Project_Successful ~ anger + anticipation + disgust +
                   fear + joy + sadness + surprise + trust + positive + negative, data=df_sentiments_body, family="binomial")
fit_all <- glm(Project_Successful ~ anger + anticipation + disgust +
                   fear + joy + sadness + surprise + trust + positive + negative, data=df_sentiments_all, family="binomial")
fit_section <- glm(Project_Successful ~ anger*section + anticipation*section + disgust*section +
             fear*section + joy*section + sadness*section + surprise*section + trust*section + positive*section + negative*section, data=df_sentiments_all, family="binomial")
```

### Campaign Model

The model below shows that campaigns are more successful when the
campaigns have the following emotions: anger\*\*\*, joy \*\*\*,
sadness\*\*\* and surprise \*\*\*

Campaigns are less successful when the words in the campaigns have the
following emotions: anticipation\*\*\*, fear\*\*\*, trust\*\*\*,
positive \*\*\*, and negative\*\*\*

``` r
summary(fit_all)
```

    ## 
    ## Call:
    ## glm(formula = Project_Successful ~ anger + anticipation + disgust + 
    ##     fear + joy + sadness + surprise + trust + positive + negative, 
    ##     family = "binomial", data = df_sentiments_all)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5953  -0.9930  -0.9145   1.3463   2.2517  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -0.17048    0.05493  -3.104 0.001912 ** 
    ## anger         0.76074    0.09398   8.095 5.74e-16 ***
    ## anticipation -0.48506    0.05569  -8.710  < 2e-16 ***
    ## disgust      -0.04428    0.13797  -0.321 0.748264    
    ## fear         -1.00153    0.09288 -10.783  < 2e-16 ***
    ## joy           0.44456    0.06413   6.932 4.15e-12 ***
    ## sadness       0.52034    0.09382   5.546 2.92e-08 ***
    ## surprise      0.54156    0.08029   6.745 1.53e-11 ***
    ## trust        -0.44949    0.05217  -8.615  < 2e-16 ***
    ## positive     -0.21828    0.06049  -3.609 0.000308 ***
    ## negative     -0.34596    0.08938  -3.871 0.000109 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32287  on 24182  degrees of freedom
    ## Residual deviance: 31882  on 24172  degrees of freedom
    ## AIC: 31904
    ## 
    ## Number of Fisher Scoring iterations: 4

### Blurb Model

The model below shows that campaigns are more successful when the blurbs
have the following emotions: anger\*\*\*, surprise \*\*\*,disgust\* and
sadness\*

Campaigns are less successful when the blurbs have the following
emotions: anticipation\*\*\*, fear\*\*\*, trust\*\*\*, and
negative\*\*\*

Notable differences between the campaign and the blurb model are:

  - disgust has marginal positive significance
  - joy has no significance
  - positive has no significance

<!-- end list -->

``` r
summary(fit_blurb)
```

    ## 
    ## Call:
    ## glm(formula = Project_Successful ~ anger + anticipation + disgust + 
    ##     fear + joy + sadness + surprise + trust + positive + negative, 
    ##     family = "binomial", data = df_sentiments_blurb)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4193  -1.0026  -0.9209   1.3167   1.8659  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -0.31755    0.06643  -4.780 1.75e-06 ***
    ## anger         0.61271    0.10682   5.736 9.70e-09 ***
    ## anticipation -0.22829    0.06546  -3.488 0.000487 ***
    ## disgust       0.30715    0.15439   1.989 0.046651 *  
    ## fear         -0.39679    0.10288  -3.857 0.000115 ***
    ## joy          -0.02691    0.07724  -0.348 0.727488    
    ## sadness       0.27991    0.10910   2.566 0.010299 *  
    ## surprise      0.34003    0.09536   3.566 0.000363 ***
    ## trust        -0.33261    0.06168  -5.392 6.95e-08 ***
    ## positive      0.01182    0.07180   0.165 0.869230    
    ## negative     -0.27280    0.10312  -2.646 0.008156 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 15007  on 11204  degrees of freedom
    ## Residual deviance: 14882  on 11194  degrees of freedom
    ## AIC: 14904
    ## 
    ## Number of Fisher Scoring iterations: 4

### Body Model

Notable differences between the body and the blurb model are:

  - higher significance levels in the body model
  - disgust changing from positive effect in the blurb model to negative
    defect in the body model
  - joy has positive effect in body model
  - positive has negative effect in body model

The model below shows that campaigns are more successful when the blurbs
have the following emotions: anger\*\*\*, surprise \*\*\*, joy \*\*\*
and sadness\*\*\*

Campaigns are less successful when the blurbs have the following
emotions: anticipation\*\*\*, disgust\*\*\*, fear\*\*\*, trust\*\*\*,
positive \*\*\*, and negative\*\*\*

``` r
summary(fit_body)
```

    ## 
    ## Call:
    ## glm(formula = Project_Successful ~ anger + anticipation + disgust + 
    ##     fear + joy + sadness + surprise + trust + positive + negative, 
    ##     family = "binomial", data = df_sentiments_body)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2643  -0.9684  -0.8260   1.2796   2.6381  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.17276    0.10123   1.707  0.08790 .  
    ## anger         1.08622    0.20027   5.424 5.84e-08 ***
    ## anticipation -1.12215    0.10680 -10.507  < 2e-16 ***
    ## disgust      -1.40641    0.32331  -4.350 1.36e-05 ***
    ## fear         -2.98718    0.20574 -14.519  < 2e-16 ***
    ## joy           1.42528    0.11915  11.962  < 2e-16 ***
    ## sadness       1.57826    0.19487   8.099 5.54e-16 ***
    ## surprise      1.08673    0.15283   7.111 1.15e-12 ***
    ## trust        -0.78543    0.09899  -7.934 2.12e-15 ***
    ## positive     -0.69297    0.11370  -6.095 1.10e-09 ***
    ## negative     -0.55508    0.18277  -3.037  0.00239 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 17278  on 12977  degrees of freedom
    ## Residual deviance: 16634  on 12967  degrees of freedom
    ## AIC: 16656
    ## 
    ## Number of Fisher Scoring iterations: 4

### Interaction Model

The following model interacts each sentiment with the section(body,
blurb).

Notable insights from this model are:

  - Sentiments in the body increase the probability of success
  - Disgust in the body interacts negatively with disgust in the blurb

<!-- end list -->

``` r
summary(fit_section)
```

    ## 
    ## Call:
    ## glm(formula = Project_Successful ~ anger * section + anticipation * 
    ##     section + disgust * section + fear * section + joy * section + 
    ##     sadness * section + surprise * section + trust * section + 
    ##     positive * section + negative * section, family = "binomial", 
    ##     data = df_sentiments_all)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2643  -0.9877  -0.8736   1.3097   2.6381  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -0.31755    0.06643  -4.780 1.75e-06 ***
    ## anger                     0.61271    0.10682   5.736 9.70e-09 ***
    ## sectionbody               0.49031    0.12109   4.049 5.14e-05 ***
    ## anticipation             -0.22829    0.06546  -3.488 0.000487 ***
    ## disgust                   0.30715    0.15439   1.989 0.046651 *  
    ## fear                     -0.39679    0.10288  -3.857 0.000115 ***
    ## joy                      -0.02691    0.07724  -0.348 0.727488    
    ## sadness                   0.27991    0.10910   2.566 0.010299 *  
    ## surprise                  0.34003    0.09536   3.566 0.000363 ***
    ## trust                    -0.33261    0.06168  -5.392 6.95e-08 ***
    ## positive                  0.01182    0.07180   0.165 0.869230    
    ## negative                 -0.27280    0.10312  -2.646 0.008156 ** 
    ## anger:sectionbody         0.47351    0.22698   2.086 0.036966 *  
    ## sectionbody:anticipation -0.89386    0.12527  -7.136 9.63e-13 ***
    ## sectionbody:disgust      -1.71356    0.35828  -4.783 1.73e-06 ***
    ## sectionbody:fear         -2.59039    0.23003 -11.261  < 2e-16 ***
    ## sectionbody:joy           1.45219    0.14199  10.227  < 2e-16 ***
    ## sectionbody:sadness       1.29835    0.22333   5.813 6.12e-09 ***
    ## sectionbody:surprise      0.74671    0.18014   4.145 3.39e-05 ***
    ## sectionbody:trust        -0.45282    0.11664  -3.882 0.000103 ***
    ## sectionbody:positive     -0.70479    0.13447  -5.241 1.60e-07 ***
    ## sectionbody:negative     -0.28228    0.20985  -1.345 0.178588    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32287  on 24182  degrees of freedom
    ## Residual deviance: 31516  on 24161  degrees of freedom
    ## AIC: 31560
    ## 
    ## Number of Fisher Scoring iterations: 4

## Next Steps

I would like to understand what the NRC lexicon is capturing and why is
the lexicon categorizing each word the way it does.For example, What are
the words categorized as “disgust” that have a positive effect on the
blurb and a negative effect in the body, and why are those words
categorized as “disgust” in the lexicon.

The following section shows the top 10 words that appear in each
sentiment. The information presented in the graph suggests that some
words might convey different meanings that the one categorized, such as
*mug* categorized in *fear*. It might be helpful to read more about
mechanisms that prevent these miscategorizations.

### Body top words per Sentiment

``` r
df_top_sentiments_body <- df_words_body %>%
  mutate(count = n) %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(word, sentiment) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  ungroup()
```

    ## Joining, by = "word"

    ## `summarise()` regrouping output by 'word' (override with `.groups` argument)

``` r
df_top_sentiments_body %>%
  group_by(sentiment) %>%
  arrange(desc(count, sentiment)) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(count, reorder(word, count), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) + 
  theme(axis.text.x = element_text(angle = 90))
```

    ## Selecting by count

![](Sentiments_files/figure-gfm/visualization%20sentiments-1.png)<!-- -->
