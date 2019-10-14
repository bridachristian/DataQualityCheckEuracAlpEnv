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

The first target is to manage and check real time data, collect them, detect possible bugs and outliers, and save, if it is possible in a regular time series usable from researcher. To do that we developed the script **DQC\_Hourly\_Linux\_v6.R** (further details below) that runs in a cronjob every hour. The second is a detailed analysis of troubles occoured in the last week. This is done by the script **DQC\_Reports.R** and is a tool for mainenace to detect anomlies on sensors that requires a repair. This script runs automatically (cronjob) every week The third is an analysis of a group of files belongs to the same stations. It is used to check old data and old files, to detect structure change and to highline the typical problem of the manual preprocesing. The script that do that is "DQC.R" and is used to prepare hystorical data.

The first and the second script run on HPCgeo01 a virtual machine Linux prepared by the [ICT](http://www.eurac.edu/en/aboutus/organisation/servicedepartments/ict/Pages/default.aspx). The third script runs locally on a Windows machine, we are working on the portability and to try to generalize input structure.

<!-- Our target is to collect all data, check possible bugs and storage continuos time series. To do that we use 2 main scripts. The first runs hourly in a crontab on a Linux machine, the second is manual script that could be use for an offline data quality check. This second script give us the information about problems detected in an html page, easy to read and well structured. -->
2. Download the Package
-----------------------

2.1 Clone the repository

Clone the entire repository from [https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git](%22https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git%22) in a local folder

2.2 Download package

Download the package from [GitLab](%22https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git%22)

For credential ask directly to [Christian.Brida@eurac.edu](Christian.Brida@eurac.edu) (Institute for Alpine Environment) or [Luca.Cattani@eurac.edu](Luca.Cattani@eurac.edu) (ICT)

2.3 Dowload libraries

3. Process
----------

This package is a collection of scripts used to manage data and pictures flow from the stations to a storage. Here we describe how the scripts work and which are the roles!

Every hour on the HPCgeo01 run three scripts (in a crontab):

-   at 15' every pictures downloaded from the station were readed from the loggernet folder by **DQC\_Pics.R**, checked and stored inside every station subfolder, so you can have under the same folder data raw, data total, data processed, pics and reports.

-   at 25' the script **DQC\_Move\_Wrong\_Files.R** move out of the loggernet folder all the files whith wrong names. What does it mean? The name of files downloaded with loggernet is composed is composed of two parts: the station name defined in loggernet and the table name defined by the station software. In turn the loggernet station name is defined as the name of the project plus the name of the station name. Here an example for the station B1: the loggernet station name is LTER\_B1 and the table name is B1, therefore the file name is LTER\_B1\_B1.dat. This script check if the station name and the table name are the same, otherwise there is an error on the IP assingment. The dynamicDNS doesn't assign the proper DNS at the station. The wrong files were archived in a storage folder to backup and to future analysis.

-   at 30' the script **DQC\_Hourly\_Linux\_v6.R** check the data downloaded from the station, highlighting

4. Feauters
-----------

X. Functions
------------

The 3 scripts named before are structured in this way: a file management system to prepare files, folders and to summaryze results. Inside there is a core script that apply some function in the proper oreder, every function are indipendent but some actions need to be executed consecutively.

A detailed function are available [here](https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv/blob/master/Functions_description.Rmd)

X+1. Scripts description
------------------------

##### Contributors & Contacts:

-   Brida Christian [mail](Christian.Brida@eurac.edu)
-   Genova Giulio
-   Zandonai Alessandro
-   Luca Cattani

<!-- ## [Package Documentetion: Vignette] -->
