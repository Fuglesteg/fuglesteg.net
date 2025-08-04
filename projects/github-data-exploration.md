---
name: Github data exploration
source-link: https://github.com/Fuglesteg/Data-exploration-of-github-language-popularity
link: https://vis3000.fuglesteg.net
technologies:
    - r
    - github
date: 10.06.2022
---

# Language popularity on GitHub

> An exploration of language popularity on Github.

My submission to a university course in data analysis and visualisation. I
wanted to explore if there was any correlation between the features of any
given programming language and the amount of bugs in the software built using
that language.

While the analysis gave no conclusive evidence, and similar studies also find
no such correlation, I learned a lot from this experience. It was the first
time I had ever done any data analysis, and use R, and GraphQL. I decided to
gather my own dataset as I noticed that publicly available datasets from GitHub
were quite outdated. I wrote a few utilities in R using httpr to call the
GitHub GraphQL API however many times was required to gather the amount of data
I wanted. My final dataset contained data from 100 000 repositories. I used
this data to analyse popularity of languages, language features, and to analyse
if there was any correlation between language or language features and the
amount of bug issues on a repository.

The analysis is quite naive in many areas and doesn't account for many factors,
but I still find it very interesting to analyse and interact with. The most
interesting part for me is definitely the analysis of popularity of language
features and the trends in language popularity. Most charts are interactive
which allows you to explore the results yourself.

The report was generated using RMarkdown compiled to HTML with some inline CSS
and HTML. The interactive charts are made using Plotly.js.

You can check out the report at: [VIS3000.fuglesteg.net](https://VIS3000.fuglesteg.net)

The source code and datasets are available on my [GitHub](https://github.com/Fuglesteg/Data-exploration-of-github-language-popularity)
