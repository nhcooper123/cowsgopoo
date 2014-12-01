# cowsgopoo: for all your grazing herd nitrogen deposition needs

This repository contains functions needed to use cowsgopoo. There is also a PDF of the vignette showing practitioners how to use the package. This was written for a friend who has to calculate how many days each
of her herd spends in a number of age classes across the year, so that she can then work out how
much nitrogen they deposit on her fields (older cows = bigger cows = more poo). I was hoping to get this to work with cowsay so the results could come from a cow...

## Installing cowsgopoo

You can install directly from GitHub if you have the devtools package installed:

	library(devtools)
	install_github("cowsgopoo", username = "nhcooper123")
	library(cowsgopoo)

## Using cowsgopoo

The package has lots of internal functions but the only function you really need is cowsgopoo:
	
	cowsgopoo()
	
Check out ?cowsgopoo in R for more details.
