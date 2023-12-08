---
editor_options: 
  markdown: 
    wrap: 90
---

# Social Network Analysis Project Group 12

This is a project for the Social Network Analysis course at JADS.

This project is about analyzing social network within Eurovision community. Data used for
this project is taken from the Eurovision website and can be accessed here:
<https://eurovisionworld.com/eurovision/2023>

The structure of the project:

```         
├── data
│  ├── NodeList_Eurovision.xlsx
│  └── PublicEurovisionBipartite.xlsx
├── project_report
│   ├── images/
│   ├── SNA4DSProjectTemplate.Rmd
│   └── r-references.bib
└── README.md
└── AnalysisCode.R
```

`NodeList_Eurovision.xlsx` consists of information about countries such as language family
and government system.

`PublicEurovisionBipartite.xlsx` consists of information about the votes distribution.
Columns are represented by countries that were giving points and rows are represented by
countries that receive votes. Entries represent how many points were given.

`project_report` folder has all necessary files to write an acedemic paprer in papaja
(Preparing APA Journal Articles) which is an R package that provides document formats to
produce complete APA manuscripts from RMarkdown-files.

`AnalysisCode.R` is the file where the data has been pre-processed and models have been
run. Code was written in R.

Please find here the executive summary of the report:

*This study investigates community formation and voting patterns among Eurovision
countries using 2023 public vote data. The research follows up on the idea that voting
patterns occur between similarities between countries. Analysis reveals a notable presence
of small communities within the Eurovision context, underscoring the intricate social
dynamics at play. Contrary to initial hypotheses, factors such as country government
systems and language families do not significantly influence voting behavior. This
suggests that while smaller communities prevail, traditional socio-political and
linguistic ties have limited impact on public voting patterns. These findings prompt a
reevaluation of the determinants driving voting behaviors in the Eurovision song contest,
emphasizing the need for nuanced exploration of socio-cultural connections and emergent
communities within the framework of this global event.*

Necessary packages to be installed:

install.packages("snafun") (necessary for performing cug test, reading probabilities)

install.packages("readxl") [necessary to read excel files, in case you you want to use csv
file, use install.packages("readcsv") package]

tinytex::install_tinytex() (necessary to convert from rmd to pdf format)

install.packages("RColorBrewer") (Provides color schemes for maps (and other graphics)
designed by Cynthia Brewer)

List of contributors:

\- Krzysztof Wiesniakowski

\- Xavier Paulus

\- Vishal Sehgal

\- Chigozie Ifepe

\- Thierry Brands
