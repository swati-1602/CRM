
# Code for  
**"Generalized M-Estimation in Censored Regression Model under Endogeneity"**  
by *Swati Shukla, Subhra Sankar Dhar, and Shalabh*

---

## 📄 Overview

This repository contains the R code used in the paper:

> **Generalized M-Estimation in Censored Regression Model under Endogeneity**

The code implements robust M-estimation methods for censored regression (Tobit-type) models with endogenous regressors using the control function approach. It includes both **simulation studies** and **real data analysis**.

---

## 📁 Repository Structure

This repository contains two main folders:

### 1️⃣ `PAPER_CODE/`  
This folder includes code for the **simulated data study**, including:

- Data generation for censored regression models with endogeneity  
- Implementation of robust M-estimators (e.g., LAD, Huber, LogCosh)  
- Monte Carlo simulations  
- Mean Squared Error (MSE) computation  
- MSE comparison plots

➡️ Use this folder to reproduce all **simulation results and figures** reported in the paper.

---

### 2️⃣ `REAL_DATA_CODE/`  
This folder includes code for the **real data application** using the NLSY97 dataset:

- Construction of the working dataset (similar to Mroz, 1987)  
- Estimation of the censored regression model with endogenous regressors  
- Control function implementation  
- Robust M-estimation  
- Plot of the **response variable `hours`**  
- Additional diagnostic and result plots

➡️ Use this folder to reproduce the **empirical results and figures** from the real data study.


