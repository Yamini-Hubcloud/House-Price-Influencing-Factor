# House-Price-Influencing-Factor

This project investigates the various characteristics influencing house sale prices using a dataset from the Austin housing market. The analysis aims to assist real estate professionals—buyers, sellers, agents, and developers—by identifying key factors that drive property value. Through thorough data analysis, modeling, and visualization, we aim to uncover patterns and relationships that can lead to more informed decision-making in the real estate sector.

The dataset was analyzed using R Studio, with extensive steps including data importing, cleaning, missing value imputation, and handling of outliers. Visual and statistical tools such as boxplots, correlation matrices, and regression models were employed to explore the connections between sale price and other variables like property tax rate, number of bedrooms, city, schools nearby, house type, and features.

A total of 9 hypothesis statements were developed to test relationships between house characteristics and sale price. Methods such as t-tests and multiple linear regression were used to validate these hypotheses. Models were evaluated using R², RMSE, MAE, and checked for multicollinearity, normality, and influential data points using VIF, Durbin-Watson test, and Cook’s Distance.

Key visualizations include:

Violin plots (sale price vs. city or bedrooms)

Bar graphs (year built, features, city)

Scatter plots (stories vs. sale price, schools vs. price per sqft)

Histograms (property tax rate)

Multiple regression models were trained using a train-test split, with the final and best-performing model incorporating key variables like stories, bedrooms, property tax rate, year built, schools nearby, and lot/living area.

This project provides a valuable framework for real estate market analysis and can be extended with machine learning models for enhanced predictive performance.

