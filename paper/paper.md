---
title: "effectsize: Indices of Effect Size and Standardized Parameters"
authors:
- affiliation: 1
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 2
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967
- affiliation: 3
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206

date: "01 July 2020"
output: pdf_document
bibliography: paper.bib
csl: apa.csl
tags:
- R
- easystats
- effect size
- regression
- linear models
- standardized coefficients
affiliations:
- index: 1
  name: Ben-Gurion University of the Negev, Israel
- index: 2
  name: Nanyang Technological University, Singapore
- index: 3
  name:  University Medical Center Hamburg-Eppendorf, Germany
---

# Aims of the Package

It is often of interest to to asses the size of an observed assosiation. This is typycally done to allow the judgment of the magnitude of an effect (especailly when units of measuemnt are not meanigful), to facilitate comparing predictors' importance within a given model, or both. Though some indecies of effect size, such a the correlation coefficiant (a standardized covariance coefficiant) are readly available, other measures are often harder to obtain. **effectsize** is an R-package [@rcore] that fills this important gap. Its primary goal is to provide utilities for estimating a wide variety of standardized effect sizes (i.e., effect sizes that are scale-invariant) and their confidence intervals (CI), from a variety of statistical models.

# Examples of Features

Also has functions to support the interpertation of these effect size indices...

All of CIs (ncp method)

## Indices of Effect Size

### Standardized Differences

basic t test

### Contingency Tables

### Percent Variance Explained

For linear models...

anova and lmer

Baysian (via posterior predictive distribution simulation).

## Standardized Parameters

Also for glm (where the )

$Beta$

types?

Bayesian

utilized by ***parameters*** []

## Effect Size Conversion

### From Test Statistics

### Between Effect Sizes


<!-- This is done: -->

# Licensing and Availability

**effectsize** is licensed under the GNU General Public License (v3.0), with all source code stored at GitHub (https://github.com/easystats/effectsize), and with a corresponding issue tracker for bug reporting and feature enhancements. In the spirit of honest and open science, we encourage requests/tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

**effectsize** is part of the [*easystats*](https://github.com/easystats/easystats) ecosystem, a collaborative project created to facilitate the usage of R for statistical analyses. Thus, we would like to thank the [members of easystats](https://github.com/orgs/easystats/people) as well as the users.

# References
