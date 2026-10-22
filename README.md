# terracliva: Climate Variability Analysis Tools (with spatio-temporal gridded coverages)

## Summary

Climate variability describes how variables such as precipitation and temperature fluctuate around their long-term mean, from one season or year to the next and across decades. It differs from climate change, which is a lasting shift in the mean state or in the variability itself. For hydrology, agriculture and risk management, variability matters as much as the mean. A region with adequate average rainfall can still face recurrent droughts, damaging dry spells within the rainy season, or heat waves that stress crops and people. Analysing variability therefore means quantifying how large and how frequent departures from normal conditions are, and whether these characteristics are changing over time.

**terracliva** is an R package, released under the EUPL licence, that brings these analyses to spatio-temporal gridded data. It is built on `terra`, so it works directly on multi-layer `SpatRaster` objects such as satellite-based precipitation products like CHIRPS. Its design is simple and consistent. Each analysis is first written as a function for a single time series, with the suffix `cliva`. A twin function with the suffix `apprast` applies the same analysis to every cell of a grid through a common engine, `apprast()`. This engine wraps `terra::app` and `terra::tapp` and can split large rasters into tiles to keep memory use under control. The same code therefore serves a single station and an entire region.

The core of the package is the probabilistic analysis of precipitation with L-moments. The function `lmcliva()` computes sample L-moments, fits a probability distribution (Pearson type III by default) and checks the fit with a Kolmogorov-Smirnov test. From the fitted distribution it derives the rainfall amounts expected to be undershot or exceeded on average once every 2, 5, 10, 20 or 50 years, in millimetres or as a percentage of the mean. It also answers the reverse question: given a deficit or an excess of, say, 20 per cent, what are its probability and return period? L-moments are preferred to conventional moments because they are robust with short and skewed samples, which are typical of climate records.

Drought is addressed with the Standardized Precipitation Index and its evapotranspiration variant SPEI (`spicliva()`), at several aggregation scales and against a chosen reference period. Extreme events are handled by two further analyses. Dry spell analysis (`dryspellcliva()`) counts and measures sequences of days below a rainfall threshold during the rainy season, grouped by duration. Heat and cold waves are quantified with the Heat Wave Magnitude Index daily, HWMId (`hwmidcliva()`), and its cold-wave counterpart.

Two more tools complete the picture. A distance between sets of L-moments (`lmpdistcliva()`) compares different precipitation datasets, showing whether they describe the same climate statistically. The `regress()` function detects trends with linear regression, Sen's slope and the Mann-Kendall test, and can be attached to any of the analyses above. Together, these functions describe how variable a climate is, how often extremes occur and whether that behaviour is changing, from a single pixel to a continental grid.



# Manual and Installation from R Console 

 An R package for Climate Variability Analysis with spatio-temporal gridded coverages

See  manual for further details.

## Installation from R console 

```

## Installiation from Github repo through Gituh token 
remotes::install_github("ecor/lmomPi")
remotes::install_github("ecor/terracliva")

## OBSOLETE 
remotes::install_github("ecor/terracliva",auth_token="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")
```

## Usage from R console 

```
library(terracliva)

help(package="terracliva")

help("lmcliva")

help("dryspellcliva")

help("apprast")

help("lmrast")

help("dryspellrast")



```

