# predict-points
R project to predict how many points an NBA player will score in a game

This R project uses the hoopR package to pull traditional and advanced box score data from NBA.com, clean and combine the data, and predict how many points a player will score in a game. The goal is to create a simple linear regression model to predict points, based on historical performance and advanced metrics.

Steps:
Data Collection: The script retrieves NBA box score data for both traditional and advanced stats for the 2019-2023 seasons. Data is fetched from NBA.com's stats pages using the nba_boxscoretraditionalv3 and nba_boxscoreadvancedv3 functions from the hoopR package.

Data Cleaning:

The box scores are cleaned by splitting time in "minutes" into numerical values.
Advanced and traditional stats are merged to provide a complete picture of player and team performances.
Data for home and away players is combined and adjusted to reflect game conditions (like home/away splits).
Feature Engineering:  Rolling averages of metrics over the last five games are computed to capture recent trends for players and team. Rest days between games are also calculated

Modeling:

A linear regression model is created using several traditional and advanced metrics to predict the number of points a player will score.
The model is evaluated on its RMSE (Root Mean Square Error) to assess prediction accuracy.
Results: The RMSE for each player and the overall RMSE across all predictions is calculated to determine the model's performance.

To Run:

Install the required packages: hoopR, tidyverse, zoo, caTools, and Metrics.
Clone this repository and run the script in R or RStudio.
