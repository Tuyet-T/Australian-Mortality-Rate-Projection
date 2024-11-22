This repository contains a comprehensive analysis of mortality rates using Australian mortality data. The analysis includes various statistical methods, such as natural cubic splines, smoothing splines, and statistical tests to validate the models. Additionally, it explores forecasting methods using the Lee-Carter (LC) and Age-Period-Cohort (APC) models.

Overview

The goal of this analysis is to model mortality rates in Australia and assess the performance of different methods to fit and predict mortality data over the years. Key steps include:

Data Preprocessing:

Extraction of mortality rates by age and year from the Human Mortality Database (HMD).

Transformation of the data to make it suitable for modeling, such as handling age-related labels.

Spline Fitting:

Fitting natural cubic splines and smoothing splines to model the mortality rates.

Comparison of models based on mean squared error (MSE) for calibration and validation years.

Statistical Testing:

Applying Chi-squared and standardized deviations tests to validate the model predictions.

The analysis includes a series of hypothesis tests to determine if the fitted models are statistically significant.

Mortality Projection:

Forecasting mortality rates using the LC and APC models.

Model performance evaluation using MSE for the year 2020 and comparisons over different forecasting horizons (e.g., 14, 34, and 54 years).
