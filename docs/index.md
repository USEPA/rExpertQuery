## rExpertQuery

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/USEPA/rExpertQuery/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/ExpertQueryTMDLDashboard/actions/workflows/R-CMD-check.yaml)

R package (UNDER ACTIVE DEVELOPMENT) for retrieving ATTAINS data
efficiently via Expert Query web services
(<https://www.epa.gov/waterdata/expert-query>).

## Installation

You must first have R and R Studio installed to use the rExpert Query
(see instructions below if needed). rExpert Query is in active
development, therefore we highly recommend that you update it and all of
its dependency libraries each time you use the package.

You can install and/or update the [rExpertQuery
Package](https://github.com/USEPA/rExpertQuery) and all dependencies by
running:

\`\`\`{r} if(!“remotes”%in%installed.packages()){
install.packages(“remotes”) }

remotes::install_github(“USEPA/rExpertQuery”, ref = “develop”,
dependencies = TRUE, force = TRUE) \`\`\`

## Functions and Webservices

There are ten exported functions in rExpertQuery. The first eight allow
users to query ATTAINS data via [Expert Query web
services](https://owapps.epa.gov/expertquery/api-documentation). These
are:

1.  [EQ_Actions()](https://usepa.github.io/rExpertQuery/reference/EQ_Actions.html) -
    queries and returns ATTAINS actions data
2.  [EQ_ActionsDocuments()](https://usepa.github.io/rExpertQuery/reference/EQ_ActionsDocuments.html) -
    queries and returns ATTAINS Actions Documents, including a keyword
    search
3.  [EQ_Assessments()](https://usepa.github.io/rExpertQuery/reference/EQ_Assessments.html) -
    queries and returns ATTAINS Assessments
4.  [EQ_AssessmentUnits()](https://usepa.github.io/rExpertQuery/reference/EQ_AssessmentUnits.html) -
    queries and returns ATTAINS Assessment Units (default is for Active
    AUs)
5.  [EQ_AUsMLs()](https://usepa.github.io/rExpertQuery/reference/EQ_AUsMLs.html) -
    queries and returns ATTAINS Assessment Units with Monitoring
    Location data
6.  [EQ_CatchCorr()](https://usepa.github.io/rExpertQuery/reference/EQ_CatchCorr.html) -
    queries and returns ATTAINS Catchment Correspondence data (memory
    intensive)
7.  [EQ_Sources()](https://usepa.github.io/rExpertQuery/reference/EQ_Sources.html) -
    queries and returns ATTAINS Sources data
8.  [EQ_TMDLs()](https://usepa.github.io/rExpertQuery/reference/EQ_TMDLs.html) -
    queries and returns ATTAINS TMDL data

An additional function allows users to download the Expert Query
National Extracts of ATTAINS data
(<https://owapps.epa.gov/expertquery/national-downloads>) which are
updated weekly. This function should be used when national data are
desired or when a query designed in one of the previous eight functions
would exceed the Expert Query web services limit of one millions rows.
This function is:

9.  [EQ_NationalExtract()](https://usepa.github.io/rExpertQuery/reference/EQ_NationalExtract.html) -
    returns national extract for Actions, Assessments, Assessment Units,
    Assessment Units with Monitoring Locations, Catchment
    Correspondence, Sources, or TMDLS based on user selection

The tenth function relies on [ATTAINS web
services](https://www.epa.gov/waterdata/how-access-and-use-attains-web-services)
to provide allowable domain values for some rExpertQuery parameters:

10. [EQ_DomainValues()](https://usepa.github.io/rExpertQuery/reference/EQ_DomainValues.html) -
    returns domain values for some rExpertQuery function params

For more information on query params and function examples see:

## Open-Source Code Policy

Effective August 8, 2016, the [OMB Mandate: M-16-21; Federal Source Code
Policy: Achieving Efficiency, Transparency, and Innovation through
Reusable and Open Source
Software](https://obamawhitehouse.archives.gov/sites/default/files/omb/memoranda/2016/m_16_21.pdf)
applies to new custom-developed code created or procured by EPA
consistent with the scope and applicability requirements of Office of
Management and Budget’s (OMB’s) Federal Source Code Policy. In general,
it states that all new custom-developed code by Federal Agencies should
be made available and reusable as open-source code.

The EPA specific implementation of OMB Mandate M-16-21 is addressed in
the [System Life Cycle Management
Procedure](https://www.epa.gov/irmpoli8/policy-procedures-and-guidance-system-life-cycle-management-slcm).
EPA has chosen to use GitHub as its version control system as well as
its inventory of open-source code projects. EPA uses GitHub to inventory
its custom-developed, open-source code and generate the necessary
metadata file that is then posted to code.gov for broad reuse in
compliance with OMB Mandate M-16-21.

If you have any questions or want to read more, check out the [EPA Open
Source Project
Repo](https://www.epa.gov/developers/open-source-software-and-code-repositories)
and [EPA’s Interim Open Source Software
Policy](https://www.epa.gov/sites/default/files/2018-02/documents/interim_oss_policy_final.pdf).

## License

All contributions to this project will be released under the CCO-1.0
license file dedication. By submitting a pull request or issue, you are
agreeing to comply with this waiver of copyright interest.

## Disclaimer

This United States Environmental Protection Agency (EPA) GitHub project
code is provided on an “as is” basis and the user assumes responsibility
for its use. EPA has relinquished control of the information and no
longer has responsibility to protect the integrity, confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by EPA. The EPA seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by EPA or the United States Government.

## Contact

If you have any questions, please reach out to the ATTAINS Team at
attains@epa.gov.
