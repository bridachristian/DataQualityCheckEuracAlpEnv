DataQualityCheckEuracAlpEnv
================

### Tools for data quality check of microclimate stations network of [Institute of Alpine Environment - Eurac Research](http://www.eurac.edu/en/research/mountains/alpenv/Pages/default.aspx)

1. Introduction
---------------

The R package DataQualityCheckEuracAlpEnv provide functions and examples to manage data quality for automatic weather station network. There are about 25 stations, divided in 2 main project ( [LTER](http://lter.eurac.edu/en) and [MONALISA](http://monalisasos.eurac.edu/sos/)) that send via GSM data every hour. Our target is to collect all data, check possible bugs and storage continuos time series. To do that we use 2 main scripts. The first runs hourly in a crontab on a Linux machine, the second is manual script that could be use for an offline data quality check. This second script give us the information about problems detected in an html page, easy to read and well structured.

Next steps are to develop an user interface report for hourly script and then puts it in a container to give advandtage in term of stability and portability. Other structural work are expected to generalyze the manual script to give to everybody the possibility to configure their own files and folder. We want explore the possibility to use an external inpts file to well define path and input.

2. Download the Package
-----------------------

2.1 Download package from [Github](https://github.com/bridachristian/DataQualityCheckEuracAlpEnv)

``` r
install_packages("devtools")
library(devtools)
install_github("bridachristian/DataQualityCheckEuracAlpEnv")
library("DataQualityCheckEuracAlpEnv")
```

2.2 Dowload libraries

``` r
install_packages("zoo")
install_packages("knitr")
install_packages("ggplot2")
install_packages("reshape2")
install_packages("DT")
install_packages("htmltools")
install_packages("xtable")
```

3. Folder structure
-------------------

The script that runs hourly is customized for a specific folder structure used in our Institute and to share some data with others in a specific way. We don't descitbe this structure, for any information contact us.

For the manual script we suggest to create this structure:

-   **DQC**
    -   **DataQualityCheck\_results**
        -   **Input**: put inside input files, It should be: *LTER\_station\_station.dat* or *MONALISA\_station\_MeteoVal.dat*. Other names are not admitted!
        -   **Output**
            -   **Data**: were output data are saved, for each files we save the raw data that produce processed data
            -   **Report**: were reports are savet. For each station we create an html page and for each DQC we create a summary to have an overview of problems.
        -   **Process**: here there are some files used by DQC scripts
            -   *Logger\_number\_and\_software.csv* (**Mandatory**). An example is in package data. A file that match name of file and logger number. With a supervisor check the software detect anomalies and stop DQC if the number of logger defined here and in the datatabla doesn't match.
            -   *Range.csv* (**Mandatory**). An example is in package data. For each variable is defined a physical range. Data out of this range are considered outliers and deleted.
            -   *download\_table.csv* (**Optional**). Created by DQC the first time of DQC and updated automatically every new file process. It is used to filter data based on the most recent date
            -   *download\_table\_old.csv* (**Optional**). A copy of *download\_table.csv* whith the old value. Updated automatically. It could be used to restore old configuration.
    -   **DataQualityCheckEuracAlpEnv**: clone here the package DataQualityCheckEuracAlpEnv

How to use (not completed)
--------------------------

-   Clone the package from <https://github.com/bridachristian/DataQualityCheckEuracAlpEnv> to use the examples

-   In folder **Main\_script** there is:
    -   a script for manual quality check. It apply a quality check to a sigle data file.
-   In folder **R** there are some functions developed as specific modules

-   In folder **data** there are subfolders where put input data, support files and a folder where scripts save outputs

##### Contributors & Contacts:

-   Brida Christian [mail](Christian.Brida@eurac.edu)
-   Genova Giulio
-   Zandonai Alessandro

<!-- ## [Package Documentetion: Vignette] -->
