# To do list for Reducetarian messaging study.

## Data cleaning to do:

* wave 1 (baseline wave) data cleaning to do:
    - currentDietOther
    - startVegDiet
    - endVegDiet
    - all "CONSID" variables 
    - becomeVegSupport
    - becomeVegNegative
    - highestEDUother

* wave 2 (treatment wave) cleaning to do:
    - clean open-ended "describe main message of article" question
    - clean open-ended "enter any thoughts you had" question
    - clean intent to change meat and fruit/veg questions
    - clean attitudes questions.

* wave 3 (endline wave) cleaning to do:
    - currentDietOther
    - startVegDiet
    - endVegDiet
    - all "CONSID" variables 
    - perceptions of animal suffering
    - becomeVegSupport
    - becomeVegNegative
    - highestEDUother

* merged waves cleaning to do:
    - create variable taking on values {-1, 0, 1} for whether person increased, maintained, or decreased their meat consumption relative to baseline.

## Analyses to do:

- examine effects on wave 2 variables:
    + we measured a few variables in wave 2 right after participants read the news article, but in the current version of the working paper we only use wave 3 data. Expand the existing analyses to include these wave 2 analyses, which will give us a better sense of how participants' intentions to change their meat consumption was affected immediately after reading the article.

- examine survey attrition:
    + examine whether attrition was more acute among any particular groups (e.g. older participants, participants with higher baseline meat consumption, ...)
    + disaggregate dropout: create table of number of people who completed wave 1 but not 2 and who completed waves 1 and 2 but not 3 (use sets of mTurkIDs from each wave).

- alternative specifications:
    + add tables showing standardized effect sizes for all outcomes.
    + examine impacts on number of vegetarians.
    + examine impacts on the number of people that reduced their meat consumption between waves 1 and 3.
    + replicate all analyses when NOT dropping top 2.5% and bottom 2.5% of outliers.

- explore sub-group effects:
    + we briefly examined sub-group effects by age and gender (on total FFQ meat consumption), but didn't find any significant variation in effects across these groups. 
    + We have not explored age/gender sub-group effects on other outcomes. 
    + The cutpoints we used for age groups are pretty arbitrary. May want to experiment with other cutpoints.
    + We have not explored other sub-groups, such as education, location, baseline levels of meat consumption, baseline attitudes towards factory farming, baseline feeling thermometer towards vegetarians, et cetera.

- examine open-ended responses in wave 2
    + We collected data on a few open-ended responses after participants read the article. We should use this data to (a) check whether participants understood that the "reduce" appeal was NOT suggesting you cut out meat entirely; (b) whether participants reacted differently to the two treatments; (c) what kinds of reactions were most common.
    + If there is variation in the nature of these open-ended responses, examine what baseline variables predict positive vs. negative responses.


## Writing to do:

- explain why we removed top 2.5% and bottom 2.5% of outliers for all variables.
- be more explicit about why we focus on the "change" analyses rather than "endline"
- be explicit in the paper about which analyses were pre-specified and which are exploratory.
- elaborate on our three reasons for why social desirability does not seem to be a major concern.
- add IRB approval number to footnote on introduction page.


## Other

- extract treatment assignments from Qualtrics data to double-check that individuals were assigned to the correct treatment arm as intended.

