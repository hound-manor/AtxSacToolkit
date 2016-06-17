#
#  File: AtxSacToolkit.R
#
#       Toolkit of functions to fetch, cache, and wrangle Austin, Texas and
#       Sacramento, California animal shelter open data.
#
    
library(plyr)
library(dplyr)
library(assertthat)
library(lubridate)
library(Rcpp)

source("AtxSacAppKeys.R")




# Module constants

# Uncomment these lines when you have put in place your own application keys
# for the Socrata (Austin) and Junar (Sacramento) web services.
# hm.SocrataAppKey <- "YOUR SOCRATA APPLICATION KEY GOES HERE"
# hm.JunarAppKey <- "YOUR JUNAR APPLICATION KEY GOES HERE"
hm.DownloadFolder <- "~/Desktop/HoundManor"
hm.DaysInMonth <- 30.436875
hm.WeeksInMonth <- hm.DaysInMonth / 7




#
#   Function: hmStringCat
#
#       Concatenates one or more string arguments.
#

hmStringCat <- function (...)
{
    # Tell paste not to introduce any separator characters.

    paste(..., sep = "")
}




#
#   Function: hmIsSameDay
#
#       Determines whether two date-times (seconds) are on the same day.
#

hmIsSameDay <- function (date_1, date_2)
{
    # Decompose date in seconds into components.
    
    date_1 <- as.POSIXlt(date_1)
    date_2 <- as.POSIXlt(date_2)

    # Same year and same day within the year.
    
    return(date_1$yday == date_2$yday && date_1$year == date_2$year)
}




#
#   Function: hmAsFactor
#
#       Converts vector of strings to vector of factors, replacing empty
#       strings with NA (i.e., to avoid ending up with a factor level that
#       is the empty string).
#

hmAsFactor <- function (stringVec)
{
    # Replace empty or blank strings with NA *before* converting to vector
    # of factor; so that NA string type maps to NA factor type.

    stringVec[trimws(stringVec) == ""] <- NA

    return(as.factor(stringVec))
}




#
#   Function: hmDownloadFile
#
#       Downloads a resource over the network, from a URL to a local file.
#
#   Parameters:
#
#       fromUrl     - URL for the remote resource.
#       toFilePath  - Path name of the local file into which to store the resource
#                     (tilde expansion is performed).
#
#   Returns:
#
#       An integer code that is non-zero when an error occurred.
#

hmDownloadFile <- function (fromUrl, toFilePath)
{
    download.file(fromUrl, destfile = toFilePath, method = "curl", extra = "--silent")
}




#
#   Function: hmMakeDownloadPath
#
#       Creates the full path name for a file in the local downloaded
#       file cache.
#
#   Parameters:
#
#       localFileName - Name to use for the local cache file.
#
#   Returns:
#
#       Full path name of the local cache file.
#
#   Example:
#
#       localFileName = foo.jpg
#
#       Assuming system is configured to store downloaded files into the cache
#       directory: /home/hm/downloads
#
#       returns:  /home/hm/downloads/foo.jpg
#

hmMakeDownloadPath <- function (localFileName)
{
    assert_that(exists("hm.DownloadFolder"))
    assert_that(!is.null(hm.DownloadFolder))
    
    path <- hmStringCat(hm.DownloadFolder, "/", localFileName)
    
    return(path)
}




#
#   Function: hmFetchFile
#
#       Fetches a remote resource through the local cache directory.
#
#   Parameters:
#
#       fileUrl  - URL of the file to fetch.
#       fileName - Name under which to save (or lookup) the file in the local
#                  cache directory.
#       refresh  - Optional flag to force fetching of the file even when a version
#                  of the file is stored in the local cache directory.
#
#   Returns:
#
#       Path name of the downloaded file stored in the local cache directory.
#       NULL is returned when the download failed.
#

hmFetchFile <- function (fileUrl, fileName, refresh = FALSE)
{
    # Make the full path name (in the cache directory) from the provided
    # file name.
    
    localFilePath <- hmMakeDownloadPath(fileName)
    
    # Download only when explicitly directed to or when there is no version
    # of the desired file already stored in the local cache directory.
    
    if (refresh || !file.exists(localFilePath))
    {
        if (hmDownloadFile(fileUrl, localFilePath) != 0)
            localFilePath = NULL
    }
    
    return(localFilePath)
}




#
#   Function: hmWrangleStrings
#
#       Maps empty or blank strings in a vector to NA and returns the
#       new vector.
#

hmWrangleStrings <- function (strings)
{
    # Map empty or blank strings to NA.
    # Trim away all left and right whitespace and then map strings left
    # empty to NA.

    strings <- sapply(strings,
        function (item)
        {
            trimws(item)
            if (nchar(item) == 0) item <- as.character(NA)
            return(item)
        })
    
    return(strings)
}




#
#   Function: hmWrangleColors
#
#       Converts the single color/coat column of data to
#       two distinct columns for primary and secondary color/coat.
#
#   Returns:
#
#       Data frame containing two columns: color_1 and color_2
#
#   Example:
#
#       Black/White --> Black + White
#       Orange Tabby/White --> Orange Tabby + White
#       Tortie --> Tortie + NA
#       Brown Merle/Blue Merle --> Brown Merle + Blue Merle
#

hmWrangleColors <- function (colors)
{
    # Break the color/coat description on the delimiter.
    # Produces a list of tuples.

    rows <- strsplit(colors, "[/]")
    
    # Pad each tuple with NA.

    rows <- lapply(rows, function (item) { length(item) <- 2; item })

    # Build a data frame of two columns, treating each tuple as a row.
    # Coerces the color/coat names to factor.

    dataFrame <- as.data.frame(do.call(rbind, rows))
    names(dataFrame) <- c("color_1", "color_2")
    
    return(dataFrame)
}




#
#   Function: hmWrangleBreeds
#
#       Converts the single breed column of data to
#       two distinct columns for primary and secondary breed.
#
#   Returns:
#
#       Data frame containing two columns: breed_1 and breed_2
#
#   Example:
#
#       Bull Terrier/Boxer --> Bull Terrier + Boxer
#       Beagle Mix --> Beagle + Mix
#       Domestic Longhair Mix --> Domestic Longhair + Mix
#       Miniature Poodle --> Miniature Poodle + NA
#

hmWrangleBreeds <- function (breeds)
{    
    # Special, special case in the data where breed entry contains
    # an unescaped delimiter character. Pre-map the delimiter to
    # another character.

    breeds <- sub("Black/Tan", "Black-Tan", breeds, ignore.case = TRUE)
    
    # Insert delimiter to separate the "Mix" suffix.

    breeds <- sub(" Mix", "/Mix", breeds, ignore.case = TRUE)
    
    # Break up the breed description via the delimiter.
    # Produces a list of tuples.
    
    tuples <- strsplit(breeds, "[/]")
    
    # Pad list items with NA to two breed categories.

    tuples <- lapply(tuples, function (item) { length(item) <- 2; item })
    
    # Build a dataframe with two columns treating the list items as rows.
    # Coerces the breed names to factors.

    dataFrame <- as.data.frame(do.call(rbind, tuples))
    names(dataFrame) <- c("breed_1", "breed_2")
    
    return(dataFrame)
}




#******************************************************************************
#
#   AUSTIN
#
#******************************************************************************

#
#   Function: atxDate
#
#       Converts date (usually a string) to POSIXct in the
#       Austin time zone.
#

atxDate <- function (date)
{
    as.POSIXct(date, tz = "America/Chicago")
}




#
#   Function: atxMakeUrl
#
#       Creates the URL for an Austin data set.
#
#   Parameters:
#
#       dataSetId  - Identifier of the data set (e.g., "jam6-aawd").
#       appKey     - Identifier of the client application (registered with
#                    the data portal).-
#       format     - Optional specific format for the data set (e.g., csv, json, xml).
#                    Default is "csv".
#       limit      - Optional limit on the number of records fetched.
#                    Default is 50,000. 
#
#   Returns:
#
#       URL for the data set.
#
#   Example:
#
#       dataSetId = jam6-aawd
#       appKey = b7J08QnVrZt12K9YHr00sBRx9
#       
#       returns: https://data.austintexas.gov/resource/jam6-aawd.csv?$$app_token=b7J08QnVrZt12K9YHr00sBRx9&$limit=50000
#

atxMakeUrl <- function (dataSetId, appKey, format = "csv", limit = 50000)
{
    # Suffix of the remote file resource is just the specified file format.
    
    dataSetName <- hmStringCat(dataSetId, ".", format)
    
    hmStringCat("https://data.austintexas.gov/resource/", dataSetName,
              "?$$app_token=", appKey, "&$limit=", limit)
}




#
#   Function: atxFetchCsv
#
#       Fetches a remote CSV file from the Austin open-data portal
#       and stores a copy of the file locally.
#
#   Parameters:
#
#       dataSetName - Name of the Austin data set to fetch.
#       fileName    - Name under which to save (or lookup) the file in the local
#                     cache directory.
#       refresh     - Optional flag to force fetching of the file even when a version
#                     of the file is stored in the local cache directory.
#
#   Returns:
#
#       Path name of the downloaded file stored in the local cache directory.
#       NULL is returned when the download failed.
#

atxFetchCsv <- function (dataSetName, fileName, refresh = FALSE)
{
    csvDataSetUrl <- atxMakeUrl(dataSetName, hm.SocrataAppKey, format = "csv")
    
    localFilePath <- hmFetchFile(csvDataSetUrl, hmStringCat(fileName, ".csv"), refresh = refresh)
    
    return(localFilePath)
}




#
#   Function: atxLoadCsv
#
#       Loads a remote CSV file from the Austin open-data portal into
#       a data frame.
#
#       The remote file is fetched through the local file cache.
#
#   Parameters:
#
#       dataSetName - Name of the Austin data set to fetch.
#       fileName    - Name under which to save (or lookup) the file in the local
#                     cache directory.
#       refresh     - Optional flag to force fetching of the file even when a version
#                     of the file is stored in the local cache directory.
#
#   Returns:
#
#       Data frame containing the contents of the CSV file.
#       NULL is returned when the download failed.
#

atxLoadCsv <- function (dataSetName, fileName, refresh = FALSE)
{
    # Fetch CSV file from the data portal through local file cache.
    # Returns absolute path name of CSV file stored locally.
    
    localFilePath <- atxFetchCsv(dataSetName, fileName, refresh = refresh)
    
    if (is.null(localFilePath))
        return(NULL)
    
    # Read the local CSV file into a data frame.

    dataFrame <- read.csv(localFilePath, header = TRUE, colClasses = "character")
    
    return(dataFrame)
}




#
#   Function: atxWrangleAnimalNames
#
#       Cleans up the animal names column for Austin data.
#
#   Returns:
#
#       New animal names column
#

atxWrangleAnimalNames <- function (animalNames)
{
    # Remove asterisk characters from animal names.

    names <- hmAsFactor(gsub("\\*", "", animalNames))
    
    # Map empty names to NA.

    names[names == ""] <- NA
    
    return(names)
}




#
#   Function: atxWrangleGenderSpayNeuter
#
#       Converts the single gender-spay-neuter column of Austin data to
#       two distinct columns.
#
#   Returns:
#
#       Data frame containing two columns: spay_neuter and gender
#
#   Example:
#
#       Spayed Female --> Altered + Female
#       Neutered Male --> Altered + Male
#       Intact Female --> Intact Female
#       Unknown --> NA NA
#

atxWrangleGenderSpayNeuter <- function (genderSpayNeuter)
{
    # Break the text items of the single column into the two pieces that
    # describe gender and spay-neuter status.
    # Produces a list of tuples.
    
    tuples <- strsplit(genderSpayNeuter, "[ ]")

    # Pad with NA (applicable only to the "Unknown" case).

    tuples <- lapply(tuples, function (item) { length(item) <- 2; item })

    # Build a data frame with two columns, treating the list items as rows.
    # Coerces the gender and spay-neuter names to factors.
    
    dataFrame <- as.data.frame(do.call(rbind, tuples))
    names(dataFrame) <- c("spay_neuter", "gender")

    # Map the two different names for sterilization to a single common name.
    # At the same time clean up the "Unknown" and null entries by mapping them to NA.

    dataFrame$spay_neuter <- revalue(dataFrame$spay_neuter, c("Spayed" = "Altered", "Neutered" = "Altered", "Unknown" = NA, "NULL" = NA))

    return(dataFrame)
}




#
#   Function: atxWrangleAge
#
#       Converts the single age column of Austin data to
#       three distinct columns.
#
#   Returns:
#
#       Data frame containing three columns: age_count, age_units, and age
#
#   Example:
#
#       1 year --> 1 + yr + {duration in seconds}
#       10 months --> 10 + mo + {duration in seconds}
#       2 weeks --> 2 + wk + {duration in seconds}
#

atxWrangleAge <- function (ageCol)
{
    # Parse apart count and units of measurement.
    # Produces a list of tuples.

    tuples <- strsplit(ageCol, "[ ]")

    # Clean up "NULL" entries by transforming to (NA, NA).

    tuples <- lapply(tuples,
        function (item)
        {
            switch(item[1],
                   NULL = c(NA, NA),
                   item)
        })
    
    # Create data frame from separated count and units of measurement columns.
    # Coerces both columns to factor.
    
    dataFrame <- as.data.frame(do.call(rbind, tuples))
    names(dataFrame) <- c("age_count", "age_units")

    # Convert count column to integer; leave units column as factor.

    dataFrame$age_count <- as.integer(dataFrame$age_count)
    
    # Consolidate alike units by remapping the units factor levels.

    dataFrame$age_units <- revalue(dataFrame$age_units, c("day" = "dy", "days" = "dy",
                                                          "month" = "mo", "months" = "mo",
                                                          "year" = "yr", "years" = "yr",
                                                          "week" = "wk", "weeks" = "wk"))
    
    # Create age column by converting age in original units to lubridate duration.
    # Clean up "0 year/years" entries by mapping to NA.

    age <- apply(dataFrame, 1,
        function (row)
        {
            ageCount <- as.integer(row["age_count"])

            switch(row["age_units"],
                    dy = ddays(ageCount),
                    wk = dweeks(ageCount),
                    mo = ddays(ageCount * hm.DaysInMonth),
                    yr = switch(row["age_count"], "0" = NA, dyears(ageCount)),
                    NA)
        })
    
    # Bind up the processed data columns to return as a data frame.

    dataFrame <- cbind(dataFrame, age)
    names(dataFrame) <- c("age_count", "age_units", "age")

    return(dataFrame)
}




#
#   Function: atxWrangleDates
#
#       Converts date column of Austin data to a column of POSIX date/time.
#
#   Example:
#
#       05/06/2016 06:21:00 PM --> {POSIX date/time}
#

atxWrangleDates <- function (dates)
{
    # Central time zone USA
    # Input format is ISO 8601 (Central time zone implied).

    #return(as.POSIXct(dates, tz = "America/Chicago", format = "%m/%d/%Y %H:%M:%S %p"))
    #return(as.POSIXct(dates, tz = "America/Chicago", format = "%m/%d/%Y"))
    return(as.POSIXct(dates, tz = "America/Chicago"))
}




#
#   Function: atxLoadRawIntake
#
#       Loads the raw (un-wrangled) Austin open-data Intake data set through
#       the local file cache.
#
#   Parameters:
#
#       refresh - Boolean flag to force fetch of data set from remote data portal.
#
#   Returns:
#
#       Data frame containing the raw Intake data set.
#       Returns NULL when the load fails.
#

atxLoadRawIntake <- function (refresh = FALSE)
{
    atxLoadCsv("fdzn-9yqv", "austin_intake_fy2013_2016", refresh = refresh)
}




#
#   Function: atxLoadRawOutcome
#
#       Loads the raw (un-wrangled) Austin open-data Outcome data set through
#       the local file cache.
#
#   Parameters:
#
#       refresh - Boolean flag to force fetch of data set from remote data portal.
#
#   Returns:
#
#       Data frame containing the raw Outcome data set.
#       Returns NULL when the load fails.
#

atxLoadRawOutcome <- function (refresh = FALSE)
{
    atxLoadCsv("hcup-htgu", "austin_outcome_fy2013_2016", refresh = refresh)
}




#
#   Function: atxWrangleIntake
#
#       Converts the given Austin raw Intake data set into a cleaned-up
#       and transformed data set.
#
#   Returns:
#
#       Data frame containing the processed Intake data set.
#       The mapping from raw columns to wrangled columns is the following:
#
#           Animal ID --> animal_id (factor)
#           Name --> name (factor)
#           DateTime --> intake_date (POSIXct)
#           MonthYear --> DROPPED
#           Found Location --> intake_location (character)
#           Intake Type --> intake_type (factor)
#           Intake Condition --> intake_condition (factor)
#           Animal Type --> kind (factor)
#           Sex upon Intake --> gender (factor) + intake_spay_neuter (factor)
#           Age upon Intake --> age_count (integer) + age_units (factor) + age (duration)
#           Breed --> breed_1 (factor) + breed_2 (factor)
#           Color --> color_1 (factor) + color_2 (factor)
#           

atxWrangleIntake <- function (atxRawIntakeData)
{
    # Transform the raw columns into one or more cleaned-up columns.

    names <- atxWrangleAnimalNames(atxRawIntakeData$name)
    genderSpayNeuter <- atxWrangleGenderSpayNeuter(atxRawIntakeData$sex_upon_intake)
    ages <- atxWrangleAge(atxRawIntakeData$age_upon_intake)
    colors <- hmWrangleColors(atxRawIntakeData$color)
    intakeDates <- atxWrangleDates(atxRawIntakeData$datetime)
    breeds <- hmWrangleBreeds(atxRawIntakeData$breed)
    intakeLocations <- hmWrangleStrings(atxRawIntakeData$found_location)

    # Construct the new data frame from the new columns.

    dataFrame <- data_frame(hmAsFactor(atxRawIntakeData$animal_id),
                            hmAsFactor(atxRawIntakeData$animal_type),
                            genderSpayNeuter$gender,
                            names,
                            colors$color_1,
                            colors$color_2,
                            breeds$breed_1,
                            breeds$breed_2,
                            intakeDates,
                            hmAsFactor(atxRawIntakeData$intake_type),
                            hmAsFactor(atxRawIntakeData$intake_condition),
                            intakeLocations, 
                            ages$age_count,
                            ages$age_units,
                            ages$age,
                            genderSpayNeuter$spay_neuter)
    
    names(dataFrame) <- c("animal_id",
                          "kind",
                          "gender",
                          "name",
                          "color_1",
                          "color_2",
                          "breed_1",
                          "breed_2",
                          "intake_date",
                          "intake_type",
                          "intake_condition",
                          "intake_location",
                          "intake_age_count",
                          "intake_age_units",
                          "intake_age",
                          "intake_spay_neuter")
    
    return(dataFrame)
}




#
#   Function: atxLoadIntake
#
#       Loads the wrangled Austin open-data Intake data set through
#       the local file cache.
#
#   Parameters:
#
#       refresh - Boolean flag to force fetch of data set from remote data portal.
#
#   Returns:
#
#       Data frame containing the wrangled Intake data set.
#

atxLoadIntake <- function (refresh = FALSE)
{
    atxWrangleIntake(atxLoadRawIntake(refresh = refresh))
}




#
#   Function: atxWrangleOutcome
#
#       Converts the given Austin raw Outcome data set into a cleaned-up
#       and transformed data set.
#
#   Returns:
#
#       Data frame containing the processed Outcome data set.
#       The mapping from raw columns to wrangled columns is the following:
#
#           animal_id --> animal_id (factor)
#           name --> name (factor)
#           animal_type --> kind (factor)
#           breed --> breed_1 (factor) + breed_2 (factor)
#           color --> color_1 (factor) + color_2 (factor)
#           date_of_birth --> DROPPED
#           datetime --> outcome_date (POSIXct)
#           monthyear --> DROPPED
#           outcome_type --> outcome_type (factor)
#           outcome_subtype --> outcome_subtype (factor)
#           sex_upon_outcome --> gender (factor) + outcome_spay_neuter (factor)
#           age_upon_outcome --> DROPPED
#           

atxWrangleOutcome <- function (atxRawOutcomeData)
{
    # Transform the raw columns into one or more cleaned-up columns.
    
    names <- atxWrangleAnimalNames(atxRawOutcomeData$name)
    genderSpayNeuter <- atxWrangleGenderSpayNeuter(atxRawOutcomeData$sex_upon_outcome)
    #ages <- atxWrangleAge(atxRawOutcomeData$age_upon_outcome)
    colors <- hmWrangleColors(atxRawOutcomeData$color)
    outcomeDates <- atxWrangleDates(atxRawOutcomeData$datetime)
    breeds <- hmWrangleBreeds(atxRawOutcomeData$breed)
    
    # Construct the new data frame from the new columns.
    
    dataFrame <- data_frame(hmAsFactor(atxRawOutcomeData$animal_id),
                            hmAsFactor(atxRawOutcomeData$animal_type),
                            genderSpayNeuter$gender,
                            names,
                            colors$color_1,
                            colors$color_2,
                            breeds$breed_1,
                            breeds$breed_2,
                            outcomeDates,
                            hmAsFactor(atxRawOutcomeData$outcome_type),
                            hmAsFactor(atxRawOutcomeData$outcome_subtype),
                            genderSpayNeuter$spay_neuter)
    
    names(dataFrame) <- c("animal_id",
                          "kind",
                          "gender",
                          "name",
                          "color_1",
                          "color_2",
                          "breed_1",
                          "breed_2",
                          "outcome_date",
                          "outcome_type",
                          "outcome_subtype",
                          "outcome_spay_neuter")
    
    return(dataFrame)
}




#
#   Function: atxLoadOutcome
#
#       Loads the wrangled Austin open-data Outcome data set through
#       the local file cache.
#
#   Parameters:
#
#       refresh - Boolean flag to force fetch of data set from remote data portal.
#
#   Returns:
#
#       Data frame containing the wrangled Outcome data set.
#

atxLoadOutcome <- function (refresh = FALSE)
{
    atxWrangleOutcome(atxLoadRawOutcome(refresh = refresh))
}




#
#   Function: atxLoadNormalizedOpenData
#
#       Loads and wrangles the Austin open data into two data sets, one
#       for animals and another for impoundment events.
#
#   Returns:
#
#       List containing two data frames: Animal data set and Impoundment
#       event data set
#

atxLoadNormalizedOpenData <- function ()
{
    # Load and wrangle the intake and outcome data sets, fetching
    # them from the remote location if necessary.
    
    atxIntake <- atxLoadIntake()
    atxOutcome <- atxLoadOutcome()
    
    # Pair up the intake and outcome events, producing two
    # normalized tables.
    
    frameList <- atxMakeTables(atxIntake, atxOutcome)
    
    return(frameList)
}



#
#   Function: atxLoadOpenData
#
#       Loads the wrangled Austin open-data set through
#       the local file cache. Merges the Intake and Outcome data
#       sets to produce a data set in which each row is an impound
#       event.
#
#   Returns:
#
#       Data frame containing the wrangled data set.
#

atxLoadOpenData <- function ()
{
    # Load (with possible remote fetch) and wrangle the intake and outcome
    # data sets into normalized tables for animals and impoundment events.

    frameList <- atxLoadNormalizedOpenData()
    atxAnimalData <- frameList[["animal_data"]]
    atxImpoundData <- frameList[["impound_data"]]

    # Relational join the two normalized tables on animal_id.

    atxImpound <- inner_join(atxImpoundData, atxAnimalData, by = "animal_id")
    
    # Remove unsupported columns.
    
    atxImpound$intake_subtype <- NULL
    atxImpound$outcome_condition <- NULL
    atxImpound$kennel <- NULL

    # Return the table of impound events augmented by animal information.

    return(atxImpound)
}




#******************************************************************************
#
#   SACRAMENTO
#
#******************************************************************************


#
#   Function: sacDate
#
#       Converts date (usually a string) to POSIXct in the
#       Sacramento time zone.
#

sacDate <- function (date)
{
    as.POSIXct(date, tz = "America/Los_Angeles")
}




#
#   Function: sacMakeUrl
#
#       Creates the URL for a Sacramento data set.
#
#   Parameters:
#
#       dataSetId  - Identifier of the data set.
#       appKey     - Identifier of the client application (registered with
#                    the data portal).-
#       format     - Optional specific format for the data set (e.g., csv, json, xml).
#                    Default is "csv".
#       limit      - Optional limit on the number of records fetched.
#                    Default is 50,000. 
#
#   Returns:
#
#       URL for the data set.
#
#   Example:
#
#       dataSetId = ANIMA-INTAK-AND-OUTCO
#       appKey = b7J08QnVrZt12K9YHr00sBRx9
#       
#       returns: http://api.data.cityofsacramento.org/datastreams/invoke/ANIMA-INTAK-AND-OUTCO?auth_key=b7J08QnVrZt12K9YHr00sBRx9&output=csv&limit=50000
#

sacMakeUrl <- function (dataSetName, appKey, format = "csv", limit = 50000)
{
    # Junar API version 1 URL
    #
    #hmStringCat("http://api.data.cityofsacramento.org/datastreams/invoke/", dataSetName,
    #            "?auth_key=", appKey, "&output=", format, "&limit=", limit)

    # Now using Junar API version 2 URL.

    hmStringCat("http://api.data.cityofsacramento.org/api/v2/datastreams/", dataSetName,
                "/data.", format, "/?auth_key=", appKey, "&limit=", limit)
}




#
#   Function: sacFetchCsv
#
#       Fetches a remote CSV file from the Sacramento open-data portal
#       and stores a copy of the file locally.
#
#   Parameters:
#
#       dataSetName - Name of the Sacramento data set to fetch.
#       fileName    - Name under which to save (or lookup) the file in the local
#                     cache directory.
#       refresh     - Optional flag to force fetching of the file even when a version
#                     of the file is stored in the local cache directory.
#
#   Returns:
#
#       Path name of the downloaded file stored in the local cache directory.
#       NULL is returned when the download failed.
#

sacFetchCsv <- function (dataSetName, fileName, refresh = FALSE)
{
    csvDataSetUrl <- sacMakeUrl(dataSetName, hm.JunarAppKey, format = "csv")

    localFilePath <- hmFetchFile(csvDataSetUrl, hmStringCat(fileName, ".csv"), refresh = refresh)

    return(localFilePath)
}
    



#
#   Function: sacLoadCsv
#
#       Loads a remote CSV file from the Sacramento open-data portal into
#       a data frame.
#
#       The remote file is fetched through the local file cache.
#
#   Parameters:
#
#       dataSetName - Name of the Sacramento data set to fetch.
#       fileName    - Name under which to save (or lookup) the file in the local
#                     cache directory.
#       refresh     - Optional flag to force fetching of the file even when a version
#                     of the file is stored in the local cache directory.
#
#   Returns:
#
#       Data frame containing the contents of the CSV file.
#       NULL is returned when the download failed.
#

sacLoadCsv <- function (dataSetName, fileName, refresh = FALSE)
{
    # Fetch CSV file from data portal through the local file cache.
    # Returns absolute path name of CSV file stored locally.
    
    localFilePath <- sacFetchCsv(dataSetName, fileName, refresh = refresh)
    
    # Read the local CSV file into a data frame.
    
    dataFrame <- read.csv(localFilePath, header = TRUE, colClasses = "character")
    
    return(dataFrame)
}




#
#   Function: sacWrangleOpenDates
#
#       Converts date column of Sacramento open data to a column of POSIX
#       date/time.
#
#   Example:
#
#       2016-04-30 00:00:00 --> {POSIX date/time}
#

sacWrangleOpenDates <- function (dates)
{
    # 24-hour time is unnecessary because there never is any time information.
    # Pacific time zone USA

    return(as.POSIXct(dates, tz = "America/Los_Angeles", format = "%Y-%m-%d %H:%M:%S"))
}




#
#   Function: sacLoadRawOpenData
#
#       Loads the raw (un-wrangled) Sacramento open-data set through
#       the local file cache.
#
#   Parameters:
#
#       refresh - Boolean flag to force fetch of data set from remote data portal.
#
#   Returns:
#
#       Data frame containing the raw data set.
#       Returns NULL when the load fails.
#

sacLoadRawOpenData <- function (refresh = FALSE)
{
    sacLoadCsv("ANIMA-INTAK-AND-OUTCO", "sacramento_2013_2015", refresh = refresh)
}




#
#   Function: sacWrangleOpenData
#
#       Converts the given Sacramento raw open data set into a cleaned-up
#       and transformed data set.
#
#   Returns:
#
#       Data frame containing the processed data set.
#       The mapping from raw columns to wrangled columns is the following:
#
#           Animal_Id --> animal_id (factor)
#           Animal_Name --> name (factor)
#           Activity_Number --> DROPPED
#           Intake Type --> intake_type (factor)
#           Picked_up_Location --> intake_location (factor)
#           Intake_Date --> intake_date (POSIXct)
#           Outcome_Type --> outcome_type (factor)
#           Outcome_Date --> outcome_date (POSIXct)
#           

sacWrangleOpenData <- function (sacRawData)
{
    # Transform the raw columns into one or more cleaned-up columns.

    intakeDates <- sacWrangleOpenDates(sacRawData$Intake_Date)
    outcomeDates <- sacWrangleOpenDates(sacRawData$Outcome_Date)
    intakeLocations <- hmWrangleStrings(sacRawData$Picked_up_Location)
    
    # Construct the new data frame from the new columns.

    dataFrame <- data.frame(hmAsFactor(sacRawData$Animal_Id),
                            hmAsFactor(sacRawData$Animal_Type),
                            hmAsFactor(sacRawData$Animal_Name),
                            intakeDates,
                            hmAsFactor(sacRawData$Intake_Type),
                            intakeLocations,
                            outcomeDates,
                            hmAsFactor(sacRawData$Outcome_Type))
    
    names(dataFrame) <- c("animal_id",
                          "kind",
                          "name",
                          "intake_date",
                          "intake_type",
                          "intake_location",
                          "outcome_date",
                          "outcome_type")
    
    return(dataFrame)
}




#
#   Function: sacWrangleCpraDates
#
#       Converts date column of Sacramento CPRA data to a column of POSIX
#       date/time.
#
#   Example:
#
#       2016-04-30 --> {POSIX date/time}
#

sacWrangleCpraDates <- function (dates)
{
    return(as.POSIXct(dates, tz = "America/Los_Angeles", format = "%Y-%m-%d"))
}




#
#   Function: sacWrangleCpraSpayNeuter
#
#       Converts boolean spay-neuter column of Sacramento CPRA data to a
#       column of factors.
#
#   Example:
#
#       0 --> Intact
#       1 --> Altered
#

sacWrangleCpraSpayNeuter <- function (spayNeuterFlag)
{
    spayNeuter <- sapply(spayNeuterFlag,
        function (elem)
        {
            switch(elem,
                    "0" = "Intact",
                    "1" = "Altered",
                    NA)
        })
    
    return(spayNeuter)
}




#
#   Function: sacWrangleCpraData
#
#       Converts the given Sacramento raw CPRA data set into a cleaned-up
#       and transformed data set.
#
#   Returns:
#
#       Data frame containing the processed data set.
#       The mapping from raw columns to wrangled columns is the following:
#
#           animal_id --> animal_id (factor)
#           kind --> kind (factor)
#           name --> name (factor)
#           gender --> gender (factor)
#           spay_neuter --> spay_neuter (factor)
#           breed --> breed_1 (factor) + breed_2 (factor)
#           color --> color_1 (factor) + color_2 (factor)
#           rec_source --> rec_source (factor)
#           intake_date --> intake_date (POSIXct)
#           intake_type --> intake_type (factor)
#           intake_subtype --> intake_subtype (factor)
#           intake_condition --> intake_condition (factor)
#           intake_location --> intake_location (factor)
#           outcome_date --> outcome_date (POSIXct)
#           outcome_type --> outcome_type (factor)
#           outcome_subtype --> outcome_subtype (factor)
#           outcome_condition --> outcome_condition (factor)
#           kennel --> kennel (factor)
#           

sacWrangleCpraData <- function (sacRawCpraData)
{
    intakeDates <- sacWrangleCpraDates(sacRawCpraData$intake_date)
    outcomeDates <- sacWrangleCpraDates(sacRawCpraData$outcome_date)
    spayNeuter <- sacWrangleCpraSpayNeuter(sacRawCpraData$spay_neuter)
    colors <- hmWrangleColors(sacRawCpraData$color)
    breeds <- hmWrangleBreeds(sacRawCpraData$breed)
    intakeLocations <- hmWrangleStrings(sacRawCpraData$intake_location)
    
    # Construct the new data frame from the new columns.

    dataFrame <- data.frame(hmAsFactor(sacRawCpraData$animal_id),
                            hmAsFactor(sacRawCpraData$kind),
                            hmAsFactor(sacRawCpraData$name),
                            hmAsFactor(sacRawCpraData$gender),
                            spayNeuter,
                            breeds$breed_1,
                            breeds$breed_2,
                            colors$color_1,
                            colors$color_2,
                            hmAsFactor(sacRawCpraData$rec_source),
                            intakeDates,
                            hmAsFactor(sacRawCpraData$intake_type),
                            hmAsFactor(sacRawCpraData$intake_subtype),
                            hmAsFactor(sacRawCpraData$intake_condition),
                            intakeLocations,
                            outcomeDates,
                            hmAsFactor(sacRawCpraData$outcome_type),
                            hmAsFactor(sacRawCpraData$outcome_subtype),
                            hmAsFactor(sacRawCpraData$outcome_condition),
                            hmAsFactor(sacRawCpraData$kennel))
    
    names(dataFrame) <- c("animal_id",
                          "kind",
                          "name",
                          "gender",
                          "spay_neuter",
                          "breed_1",
                          "breed_2",
                          "color_1",
                          "color_2",
                          "rec_source",
                          "intake_date",
                          "intake_type",
                          "intake_subtype",
                          "intake_condition",
                          "intake_location",
                          "outcome_date",
                          "outcome_type",
                          "outcome_subtype",
                          "outcome_condition",
                          "kennel")
    
    return(dataFrame)
}




#
#   Function: sacLoadCpraCsvFile
#
#       Loads a local CSV file containing Sacramento CPRA data into
#       a data frame.
#
#   Parameters:
#
#       filePath - Path name of the CPRA data CSV file.
#
#   Returns:
#
#       Data frame containing the contents of the CSV file.
#       NULL is returned when the load failed.
#

sacLoadCpraCsvFile <- function (filePath)
{
    dataFrame <- read.csv(filePath, header = TRUE, colClasses = "character")
    
    if (is.null(dataFrame))
        return(NULL)
    
    return(sacWrangleCpraData(dataFrame))
}




#
#   Function: sacLoadNormalizedOpenData
#
#       Loads and wrangles the Sacramento open data into two data sets, one
#       for animals and another for impoundment events.
#
#   Returns:
#
#       List containing two data frames: Animal data set and Impoundment
#       event data set
#

sacLoadNormalizedOpenData <- function ()
{
    # Load and wrangle the intake and outcome data sets, fetching
    # them from the remote location if necessary.
    
    sacOpenData <- sacLoadOpenData()

    # Create the two normalized tables from the joined Sacramento data.
    
    frameList <- sacMakeTables(sacOpenData)
    
    return(frameList)
}




#
#   Function: sacLoadOpenData
#
#       Loads the wrangled Sacramento open-data set through
#       the local file cache.
#
#   Parameters:
#
#       refresh - Boolean flag to force fetch of data set from remote data portal.
#
#   Returns:
#
#       Data frame containing the wrangled open data set.
#

sacLoadOpenData <- function (refresh = FALSE)
{
    openData <- sacWrangleOpenData(sacLoadRawOpenData(refresh = refresh))
    
    return(openData)
}




#
# Bring in the compiled and linked C++ code.
#

sourceCpp("AtxSacMakeTables.cpp")

