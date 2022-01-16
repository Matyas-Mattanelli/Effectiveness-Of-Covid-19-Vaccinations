### Alternative topics

- Renewable energy

- Impact of covid on government spending/natality

- Determinants of energy prices

- Impact of covid on travelling



### Impact of vaccination rates on the number of cases

- control variables: seasons (time dummies), median age, average temperature, population density, number of tests
- dataset:
  -  https://ourworldindata.org/covid-vaccinations?country=OWID_WRL
  - https://github.com/owid/covid-19-data/tree/master/public/data
- article that uses OLS: https://www.mdpi.com/1660-4601/18/2/674/htm
- interesting article: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0241165
- another interesting article: https://www.medrxiv.org/content/10.1101/2020.04.17.20069708v2
- Notes: 
  - Watch out for the measurement error (endogeneity)
  - possibly include lags?
  - I have not found any panel data analysis of the issue
  - We cannot include any time-invariant variables so we need to come up with some time-varying controls
  - average temperature would be nice but it is difficult to get the data