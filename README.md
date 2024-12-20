README
================
Yile Zhu
2024-12-17

# Hospital Length of Stay Prediction

## Project Overview

This project aims to analyze the key factors influencing Length of Stay
(LOS) for hospitalized patients and predict LOS using machine learning
models. The models compared include:

1.  Linear Regression
2.  CART (Classification and Regression Trees)
3.  Random Forest

Through data cleaning, feature engineering, model training, and
performance evaluation, the results demonstrate that the Random Forest
model achieves the best prediction accuracy with the lowest RMSE.

## Data Description

- **Source File**: `INFO02v4.csv`

- The dataset contains patient admission records, costs, demographics,
  and hospital service area information.

### Key Features:

- **Type.of.Admission**: Admission type

- **Length.of.Stay**: Hospital stay duration (target variable)

- **Total.Costs** and **Total.Charges**: Costs associated with the stay

- **Age.Group**: Age groups of patients

- **Hospital.Service.Area**: Hospital service regions

- **Race** and **Ethnicity**: Patient demographic information

### Methods and Tools

### Analysis Workflow:

1.  Data Cleaning and Preprocessing

    - Removal of missing values and redundant variables.

    - Feature transformation (logarithmic scaling and encoding
      categorical variables).

2.  Feature Engineering

    - One-hot encoding for categorical features.

    - Log transformation for continuous variables.

3.  Model Training and Evaluation

    - Linear Regression: A baseline model for comparison.

    - CART: A decision tree-based model to handle nonlinearity.

    - Random Forest: An ensemble learning method for high predictive
      accuracy.

4.  Model Performance Metrics

    - RMSE (Root Mean Square Error)\*\* to evaluate model accuracy.

    - Cross-validation to ensure model stability and generalizability.

5.  Visualization

    - Boxplots, RMSE comparison tables, and variable importance plots.

### Tools and Libraries:

- Programming Language: R

- Key Libraries:

  - `ggplot2`: Visualization

  - `caret`: Model training and evaluation

  - `randomForest`: Random Forest model

  - `rpart`: CART model

  - `dplyr`, `tidyverse`: Data manipulation

## Results

### Model Performance (RMSE)

The performance of the models is summarized as follows:

| **Model** | **RMSE (No Cross-Validation)** | **RMSE (With Cross-Validation)** |
|----|----|----|
| **Linear Regression** | 13.05 | 0.4074 |
| **CART** | 9.64 | 0.5998 |
| **Random Forest** | **8.83** | **0.3968** |

**Conclusion**:  
The **Random Forest model** outperformed the other two models, achieving
the lowest RMSE. This demonstrates its ability to capture nonlinear
relationships and interactions between variables.

## Contributors

Ya Wang: Contributions include building random forest models, data
processing and cleaning, and report writing.

Suyuan Wang: Contributions include building linear regression models,
data analysis and processing, and report writing.

Yu Qing: Contributions include building cart models, data analysis and
processing, and report writing.

Yile Zhu: Contributions have identified datasets and code frameworks,
model visualisation and comparative analysis of results, integrated
reports
