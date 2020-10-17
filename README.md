# Selected Covid-19 data for CZ, visualised

<!-- badges: start -->

![Render and Deploy RMarkdown Website](https://github.com/petrbouchal/covid19/workflows/Render and Deploy RMarkdown Website/badge.svg)

<!-- badges: end -->

This repository contains code that generates a dashboard using open data on Covid-19 cases and deaths in the Czech Republic, and on all-cause mortality to understand excess deaths.

The goal is to provide some of the comparisons that official sources (e.g. the Czech Health Ministry at [<https://onemocneni-aktualne.mzcr.cz/covid-19>](https://onemocneni-aktualne.mzcr.cz/covid-19){.uri}) do not provide - namely a per-district view of recent cases and the age breakdown of cases and deaths over time.

The Covid-19 data comes from UZIS, the Czech health data authority, via data published by the Czech Ministstry of Health at <https://onemocneni-aktualne.mzcr.cz/covid-19>.

The mortality data comes from the Czech Statistical Office: <https://www.czso.cz/csu/czso/obypz_cr>. It is reported with a delay of several weeks owing to the manual process of recording and reporting deaths in population registers.

## Important

The data visualisations should be taken with a grain of salt. The data is incomplete and imperfect: it can undergo backward revisions, fresh data sometimes contains errors, and there are issues inherent in how the data is collected and reported (starting from test errors, to reporting errors, to time inconsistencies etc.)

Specifically, daily figures, where reported, are subject to change and significant variation and these should be intepreted with caution.

It is deployed using Github Actions and updated twice a day, reflecting the twice-daily update cycle of the data.
