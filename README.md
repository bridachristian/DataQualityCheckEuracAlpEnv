DataQualityCheckEuracAlpEnv
================

### Tools for data quality check of microclimate stations network of [Institute of Alpine Environment - Eurac Research](http://www.eurac.edu/en/research/mountains/alpenv/Pages/default.aspx)

1. Introduction
---------------

The R package DataQualityCheckEuracAlpEnv provide functions and examples to manage data quality for automatic microclimatic stations network. The stations collect data of many sensors and send datatables at regular intervals via GSM using [Loggernet](https://www.campbellsci.com/loggernet), a software developed by Campbell Scientific to manage loggers. Many problems affects raw data due to missing connections, due to manual preprocessing or due to software updates. We need to check if the data downloaded were well formatted and detect, as soon as possbile, failures of the sensors installed. For these reasons we developed the DataQualityCheckEuracAlpEnv package containg usefull functions and scripts to different purpose.

The newtwork of stations managed by Institute of Alpine Environment consist in 28 microclimatic stations used mainly for research purpose, ecology, hydrology and climate change impact are the study fields.

The stations belong to 2 project:

-   [LTSER Matsch/Mazia](http://lter.eurac.edu/en) a project to monitoring climate change in term of ecological and hydrological impacts. The project belogns to [LTER network](http://www.lteritalia.it/) (Long Term Ecological Research) and the research sites are located in Matsch/Mazia valley
-   [MONALISA](http://monalisasos.eurac.edu/sos/) a project to monitoring alpine environments in South Tyrol

The first target is to manage and check real time data, collect them, detect possible bugs and outliers, and save, if it is possible in a regular time series usable from researcher. To do that we developed the script **DQC\_Hourly\_Linux\_v6.R** (further details below) that runs in a cronjob every hour. The second is a detailed analysis of troubles occoured in the last week. This is done by the script **DQC\_Hourly\_Linux\_v6.R** and is a tool for mainenace to detect anomlies on sensors that requires a repair. This script runs automatically (cronjob) every week The third is an analysis of a group of files belongs to the same stations. It is used to check old data and old files, to detect structure change and to highline the typical problem of the manual preprocesing. The script that do that is "DQC.R" and is used to prepare hystorical data.

The first and the second script run on HPCgeo01 a virtual machine Linux prepared by the [ICT](http://www.eurac.edu/en/aboutus/organisation/servicedepartments/ict/Pages/default.aspx). The third script runs locally on a Windows machine, we are working on the portability and to try to generalize input structure.

<!-- Our target is to collect all data, check possible bugs and storage continuos time series. To do that we use 2 main scripts. The first runs hourly in a crontab on a Linux machine, the second is manual script that could be use for an offline data quality check. This second script give us the information about problems detected in an html page, easy to read and well structured. -->
2. Download the Package
-----------------------

2.1 Clone the repository

Clone the entire repository from [https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git](%22https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git%22) in a local folder

2.2 Download package

Download the package from [GitLab](%22https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git%22)

``` r
install_packages("devtools")
library(devtools)
devtools::install_git("https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git",credentials = git2r::cred_user_pass("username@eurac.edu", getPass::getPass()))
library("DataQualityCheckEuracAlpEnv")
```

For credential ask directly to [Christian.Brida@eurac.edu](Christian.Brida@eurac.edu) (Institute for Alpine Environment) or [Luca.Cattani@eurac.edu](Luca.Cattani@eurac.edu) (ICT)

2.3 Dowload libraries

``` r
install_github("alexsanjoseph/compareDF")
install.packages("zoo")
install.packages("knitr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("DT")
install.packages("htmltools")
install.packages("rmarkdown")
install.packages("yaml")
install.packages("highr")
install.packages("mailR")
install.packages("XML")
install.packages("xtable")
install.packages("dygraphs")
install.packages("xts")
install.packages("hwriter")
install.packages("labeling")
install.packages("optparse")
```

3. Function description
-----------------------

The 3 scripts named before are structured in this way: a file management system to prepare files, folders and to summaryze results. Inside there is a core script that apply some function in the proper oreder, every function are indipendent but some actions need to be executed consecutively.

In this section we start to describe every single function, how it is made and what its do.

-   check\_empty\_file

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
