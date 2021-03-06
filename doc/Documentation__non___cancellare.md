### Tools for data quality check of microclimate stations network of [Institute of Alpine Environment - Eurac Research](http://www.eurac.edu/en/research/mountains/alpenv/Pages/default.aspx)

1. Introduction
---------------

The R package DataQualityCheckEuracAlpEnv provide functions and examples
to manage data quality for automatic microclimatic stations network. The
stations collect data of many sensors and send datatables at regular
intervals via GSM using
[Loggernet](https://www.campbellsci.com/loggernet), a software developed
by Campbell Scientific to manage loggers. Many problems affects raw data
due to missing connections, due to manual preprocessing or due to
software updates. We need to check if the data downloaded were well
formatted and detect, as soon as possbile, failures of the sensors
installed. For these reasons we developed the
DataQualityCheckEuracAlpEnv package containg usefull functions and
scripts to different purpose.

The newtwork of stations managed by Institute of Alpine Environment
consist in 28 microclimatic stations used mainly for research purpose,
ecology, hydrology and climate change impact are the study fields.

The stations belong to 2 project:

-   [LTSER Matsch/Mazia](http://lter.eurac.edu/en) a project to
    monitoring climate change in term of ecological and hydrological
    impacts. The project belogns to [LTER
    network](http://www.lteritalia.it/) (Long Term Ecological Research)
    and the research sites are located in Matsch/Mazia valley
-   [MONALISA](http://monalisasos.eurac.edu/sos/) a project to
    monitoring alpine environments in South Tyrol

2. Goals
--------

-   Manage and check real time data, collect them, detect possible bugs
    and outliers, and save, if it is possible in a regular time series
    usable from researcher. To do that we developed the script
    **DQC\_Hourly\_Linux\_v6.R** that runs in a cronjob every hour. It
    is used for urgent problems.

-   Analysis of troubles occoured in the last period. This is done by
    the script **DQC\_Reports.R**. It is a tool for mainenace to have an
    overview of the healt status of the stations and sensors, detecting
    anomlies and exceptional events. This script runs automatically
    (cronjob) every week.

-   Analysis and fixing of historical data. It is used to check old data
    and old files, to detect structure change and to highline the
    typical problem of the manual preprocesing. The script that do that
    is **DQC.R** and is used to prepare hystorical data.

For stability reason the scripts run on a Linux virtual machine called
HPCgeo01 prepared by the
[ICT](http://www.eurac.edu/en/aboutus/organisation/servicedepartments/ict/Pages/default.aspx).
For the historical analysis the script was structured for an usage on a
Windows machine. We are developing an user friendly interface to help
the user to configure path and folder structure.

<!-- Our target is to collect all data, check possible bugs and storage continuos time series. To do that we use 2 main scripts. The first runs hourly in a crontab on a Linux machine, the second is manual script that could be use for an offline data quality check. This second script give us the information about problems detected in an html page, easy to read and well structured. -->
3. Process
----------

This package is a collection of scripts used to manage data and pictures
flow from the stations to a storage. Here we describe how the scripts
work and which are the roles!

Every hour on the HPCgeo01 run four scripts:

-   at 01' the script **check\_DQC\_locked.R ** check if the script
    DQC\_Hourly\_Linux\_v6.R is locked for more than an hour due to a
    bug, and check if the script DQC\_Reports.R is locked for more than
    a week (both LTER and MONALISA)

-   at 15' every pictures downloaded from the station were readed from
    the loggernet folder by **DQC\_Pics.R**, checked and stored inside
    every station subfolder, so you can have under the same folder data
    raw, data total, data processed, pics and reports.

-   at 25' the script **DQC\_Move\_Wrong\_Files.R** move out of the
    loggernet folder all the files whith wrong names. What does it mean?
    The name of files downloaded with loggernet is composed is composed
    of two parts: the station name defined in loggernet and the table
    name defined by the station software. In turn the loggernet station
    name is defined as the name of the project plus the name of the
    station name. Here an example for the station B1: the loggernet
    station name is LTER\_B1 and the table name is B1, therefore the
    file name is LTER\_B1\_B1.dat. This script check if the station name
    and the table name are the same, otherwise there is an error on the
    IP assingment. The dynamicDNS doesn't assign the proper DNS at the
    station. The wrong files were archived in a storage folder to backup
    and to future analysis.

-   at 30' the script **DQC\_Hourly\_Linux\_v6.R** check the data
    downloaded from the stations, highlighting different problems. The
    first check detect the status of the station. Based on "Date
    Modified" compared with a download table (a table with the last date
    downloaded and the last date modified). If the station are online,
    structure was checked, overlaps and date gaps recoverable (due to
    new software or record gaps). In these cases the DQC stops and
    require an action to unlock the situation. Minor problems are
    subsequently searched for and flagged. These problems doesn't
    require any action due to automatic fixing! All the problems
    detected are collected in a html report sended by email to the
    mainenance staff instantaneously and a reminder every 24h if the
    problem require an action. This is done using a rmarkdown script
    linked with the hourly script.

Every day on the HPCgeo01 run a script:

-   at 00:01 the script **DQC\_Reset\_Mail\_Status.R** reset the files
    used to manage email. The files are *mail\_status.csv* and
    *out\_of\_range.csv* in
    *../Stations\_Data/DQC/Process/email\_status/*. In these files the
    values 1 means send mail and 0 don't send mail.

Every week on the HPCgeo01 run two scripts (one with different
parametrization):

-   at 00:30 the script **DQC\_Reports.R**, with the parametrization
    \*\* --prj "LTER"\*\* check the *LTER* stations for the last week,
    or for the last downloaded data In this way we can have an overview
    on the problems occoured to the stations and an overview of
    parameters out of range. For a deeper analysis we check the range of
    standar deviation to monitoring consant value and noise signal. This
    script use rmarkdown scripts to produce html reports, with a traffic
    light system to represent the problems occured

-   at 01:30 the script **DQC\_Reports.R**, with the parametrization
    \*\* --prj "MONALISA"\*\* check the *MONALISA* stations for the last
    week, or for the last downloaded data

4. How to use
-------------

To use the package and the features of the scripts and rmarkdown

X. Functions
------------

The 3 scripts named before are structured in this way: a file management
system to prepare files, folders and to summaryze results. Inside there
is a core script that apply some function in the proper oreder, every
function are indipendent but some actions need to be executed
consecutively.

A detailed function are available
[here](https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv/blob/master/Functions_description.Rmd)

X+1. Scripts description
------------------------

##### Contributors & Contacts:

-   Brida Christian [mail](Christian.Brida@eurac.edu)
-   Genova Giulio
-   Zandonai Alessandro
-   Luca Cattani

<!-- ## [Package Documentetion: Vignette] -->
2. Download the Package
-----------------------

2.1 Clone the repository

Clone the entire repository from
[https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git](%22https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git%22)
in a local folder

2.2 Download package

Download the package from
[GitLab](%22https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git%22)

For credential ask directly to
[Christian.Brida@eurac.edu](Christian.Brida@eurac.edu) (Institute for
Alpine Environment) or [Luca.Cattani@eurac.edu](Luca.Cattani@eurac.edu)
(ICT)

2.3 Dowload libraries
