TO WRITE ALL! TO COMPLETE <!-- md_document --> <!-- github_document -->

**Requirements**
================

This file try to explain what the scripts do and what is needed to work.
Every script has a folder structure and support files used to manage the
regular data processing and to manage manual data fixing. Here we
describe the detailes of this strutcture and the features of the
scripts.

1. Real time data
-----------------

This section want to explain how the real time data was managed. In
particular how it was checked, collected, and alert when data errors
occouring. To do that we developed the script
**DQC\_Hourly\_Linux\_v6.R** that runs in a cronjob *every hour*. It is
used for urgent problems. To decide when a notice is needed this script
is paired with **DQC\_Reset\_Mail\_Status.R **.

\*\* TO COMPLETE \*\*

Real time pics
--------------

1.  Manage and check pics. The script **DQC\_Pics.R** collect and
    organize pics coming from the stations to the storage, highlighting
    possible corruption to prevent wrong pubblication of bad quality
    pictures on the website

Weekly summary
--------------

1.  Analysis of troubles occoured in the last period. This is done by
    the script **DQC\_Reports.R**. It is a tool for mainenace to have an
    overview of the healt status of the stations and sensors, detecting
    anomlies and exceptional events. This script runs automatically
    (cronjob) every week.

Historical data
---------------

1.  Analysis and fixing of historical data. It is used to check old data
    and old files, to detect structure change and to highline the
    typical problem of the manual preprocesing. The script that do that
    is **DQC.R** and is used to prepare hystorical data.

Wrong files
-----------

1.  Clean download data folder moving in a subfolder data with wrong
    file names. The script **DQC\_Move\_Wrong\_Files.R** detect possible
    IP errors analyzing the file name.

For stability reason the scripts run on a Linux virtual machine called
HPCgeo01 prepared by the
[ICT](http://www.eurac.edu/en/aboutus/organisation/servicedepartments/ict/Pages/default.aspx).
For the historical analysis the script was structured for an usage on a
Windows machine. We are developing an user friendly interface to help
the user to configure paths structure and settings.

How to install
--------------

-   Clone the repository from
    [GitLab](https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv).
    Writing on a GIT consolle (For example
    [gitforwindows](https://gitforwindows.org/),
    [gitshell](https://desktop.github.com/)):

<!-- -->

    git clone https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git

-   Install the package on R (<font size="2">*Any question about
    credentials ask to [Eurac
    ICT](http://www.eurac.edu/it/aboutus/people/Pages/staffdetails.aspx?persId=41206)*</font>)

    -   on HPCgeo01 linux machine, the credentials token is defined in
        the file .git-credentials:

            library(devtools)
            devtools::install_git("https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git", credentials = git2r::cred_token())
            library(DataQualityCheckEuracAlpEnv)

    -   on a Windows machine, the credentials token is created by the
        package develper in GitLab settings

            library(devtools)
            devtools::install_git("https://gitlab.inf.unibz.it/Christian.Brida/dataqualitycheckeuracalpenv.git",credentials = git2r::cred_user_pass("Christian.Brida@eurac.edu", getPass::getPass()))
            library(DataQualityCheckEuracAlpEnv)

-   Updates: It could happend that in the devoloping phase we found a
    bug or we develop new features. To have the last version of scripts
    and functions please:

        git pull  # from a git consolle in the package! 

    and then repeat the procedure to install the package

Repository structure
--------------------

-   **R**: functions developed for the quality check. Every single
    function is a brick used to build a complex function that check
    structure,changes and data downloaded. The most important function
    is DQC\_function(), inside is defined a workflow to apply to detect
    problems ad to manage data output. Th

-   **Main\_script**: scripts used for the different goals explained
    before.

-   **Rmd**: rmarkdonw script used to produce html reports colletcting
    problems and anomalies. These reports were sended via email to the
    mainenance gruop to fix data bugs or to program field works.

-   **man**: automatic documentation of function generated using the
    package *devtools*

-   **doc**: documentation and explanations of scripts and procedures

-   **other**: various

Contributors & Contacts:
------------------------

-   Brida Christian - Institute for Alpine Environment -
    [website](http://www.eurac.edu/it/aboutus/people/Pages/staffdetails.aspx?persId=39787),
    [GitLab](https://gitlab.inf.unibz.it/Christian.Brida)
-   Zandonai Alessandro - Institute for Alpine Environment -
    [website](http://www.eurac.edu/it/aboutus/people/Pages/staffdetails.aspx?persId=23703)
-   Genova Giulio - Institute for Alpine Environment -
    [website](http://www.eurac.edu/it/aboutus/people/Pages/staffdetails.aspx?persId=41527)
-   Luca Cattani - Information Technologies -
    [website](http://www.eurac.edu/it/aboutus/people/Pages/staffdetails.aspx?persId=41206)
