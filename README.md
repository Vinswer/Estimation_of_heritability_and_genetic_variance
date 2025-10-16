# Estimation_of_heritability_and_genetic_variance
*Estimation of heritability and genetic variance using linear mixed models in R.*

## Overview
This repository contains two R case studies on **genetic parameter estimation** using quantitative genetic models:

**1. Birth Weight in Pigs** – estimating genetic and environmental effects on piglet birth weight.  
**2. Courage in Labradors** – estimating heritability of a behavioral trait in dogs.  

Both cases follow the analytical framework from *“Statistical Analysis of Data – Case”*, focusing on:
- Data exploration and cleaning  
- Model building and assumption checking  
- Estimation of fixed and random effects  
- Calculation and interpretation of heritability  

## Case 1: Genetic Analysis of Birth Weight in Pigs
### Objective
Estimate **genetic parameters** for birth weight in piglets, including **genetic variance**, **environmental variance**, and **heritability**.

### Dataset Description
The dataset contains information for **3,539 piglets**, with the following variables:
| Variable | Description |
|-----------|-------------|
| `ANIMAL` | Piglet ID |
| `SIRE` | Father ID |
| `DAM` | Mother ID |
| `BIRTH_DATE` | Date of birth |
| `BIRTH_MONTH` | Month of birth |
| `BIRTH_YEAR` | Year of birth |
| `WEIGHT` | Birth weight (kg) |

## Case 2: Courage in Labradors
### Objective
Estimate **genetic and environmental effects** influencing courage in Labradors using linear mixed models.

### Dataset Description
Data collected from the Swedish Dog Training Centre, including behavioral scores for courage.
| Variable   | Description                     |
| ---------- | ------------------------------- |
| Dog_ID     | Dog identifier                  |
| Sire_ID    | Father identifier               |
| Dam_ID     | Mother identifier               |
| Sex        | Male (“hane”) or female (“tik”) |
| Littersize | Number of puppies in the litter |
| nr_male    | Number of males in the litter   |
| nr_fem     | Number of females in the litter |
| litternr   | Litter ID                       |
| PostalCode | Region code                     |
| Test_Day   | Day of behavioral test          |
| Test_Age   | Age at test (days)              |
| courage    | Courage score during test       |

Missing values are represented by ".".

## Statistical Model Framework
Both analyses are based on the **animal model** or its simplified form, described by:
y = Xβ + Zₐa + Z꜀c + e
where:
| Symbol | Description |
|---------|-------------|
| **y** | Observation vector (e.g., birth weight or courage score) |
| **Xβ** | Fixed effects (sex, month, age, etc.) |
| **Zₐa** | Random additive genetic effect (sire or animal) |
| **Z꜀c** | Common environmental effect (e.g., litter) |
| **e** | Residual error |

**Heritability** is computed as:
h² = σ²ₐ / (σ²ₐ + σ²꜀ + σ²ₑ)
where:  
- σ²ₐ = additive genetic variance  
- σ²꜀ = common environmental variance  
- σ²ₑ = residual variance
