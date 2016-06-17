# AtxSacToolkit
A toolkit of R functions to fetch, cache, and wrangle Austin and Sacramento animal shelter open data. Written mostly in R, with inherently iterative parts written in C++ and invoked through RCpp.


## Fetching Data Sets
The toolkit supports fetching open data sets from the Austin and Sacramento municipal data portals. Fetches happen through a local directory cache.

### Fetching Raw Data
The Austin animal shelter impoundment data is broken into two data sets: one for intake events and another for outcome events.

~~~~
# Fetch Austin raw intake data set.

atxRawIntakeData <- atxLoadRawIntake()

# Same fetch, but force downloading of the remote data set, instead of
# possibly using the local copy dowloaded previously.

atxRawIntakeData <- atxLoadRawIntake(refresh = TRUE)

# Fetch Austin raw outcome data set.

atxRawOutcomeData <- atxLoadRawOutcome()

~~~~

The Sacramento animal shelter impoundment data is one data set.

~~~~
# Fetch Sacramento raw impoundment data set.

sacRawOpenData <- sacLoadRawOpenData()

~~~~

### Loading Wrangled Data
The raw open data is not in a good form for analysis with R. To fetch a raw data set and have the toolkit perform an inital pass of wrangling on it:

~~~~
# Load Austin wrangled intake data.

atxIntakeData <- atxLoadIntake()

# Load Austin wrangled outcome data.

atxOutcomeData <- atxLoadOutcome()

# Load Sacramento wrangled impoundment data.

sacOpenData <- sacLoadOpenData()

~~~~

To fetch the Austin intake and outcome raw data sets, wrangle them, and then merge the two into a single impoundment data set:

~~~~
# Load Austin wrangled impoundment data (i.e., with intake and outcome events paired up).

atxOpenData <- atxLoadOpenData()

~~~~

The wrangled impoundment data sets are joins of animal data and impoundment event data. The normalized forms (one data set for animals and another for impoundment events) are also available:

~~~~
# Load normalized tables for Austin.

frameList <- atxLoadNormalizedOpenData()
atxAnimalData <- frameList[["animal_data"]]
atxImpoundData <- frameList[["impound_data"]]

# Load normalized tables for Sacramento.

frameList <- sacLoadNormalizedOpenData()
sacAnimalData <- frameList[["animal_data"]]
sacImpoundData <- frameList[["impound_data"]]

~~~~
