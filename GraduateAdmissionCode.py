import pandas as pd
import numpy as np
import seaborn as sns
import os
import sklearn
import matplotlib.pyplot as plt
from jupyterthemes import jtplot
jtplot.style(theme='monokai', context='notebook', ticks=True, grid=False)

# Read the dataset
admission_df = pd.read_csv('Admission_Predict_ver1.1.csv')

# Print the first few rows of the dataset
print("First few rows of the dataset:")
print(admission_df.head())

# Dropping serial number placeholder column and entries
admission_df.drop('Serial No.', axis=1, inplace=True)
print("Updated dataset after dropping 'Serial No.' column:")
print(admission_df)

# Checking for null data
print("Null data check:")
print(admission_df.isnull().sum())

# Print dataset information
print("Dataset information:")
admission_df.info()

# Statistical summary of the dataset
print("Statistical summary of the dataset:")
print(admission_df.describe())

# Grouping university rating (gives us the mean statistics of GRE scores, TOEFL scores, etc. for each university rating)
df_university = admission_df.groupby(by='University Rating').mean()
print("Mean statistics for each University Rating:")
print(df_university)

# Data visualization

# Histograms of the dataset
admission_df.hist(bins=30, figsize=(20, 20), color='purple')

# Pair plot showing the relation between independent variables and dependent variable
sns.pairplot(admission_df)

# Create the correlation matrix and plot a heatmap
corr_matrix = admission_df.corr()
plt.figure(figsize=(12, 12))
sns.heatmap(corr_matrix, annot=True)
plt.show()

# Creating training and testing datasets

# Create X to hold all independent variables and Y to hold 'Chance of Admit'
X = admission_df.drop(columns=['Chance of Admit '])
Y = admission_df['Chance of Admit ']

print("Shape of X:", X.shape)
print("Shape of Y:", Y.shape)

X = np.array(X)
Y = np.array(Y)

Y = Y.reshape(-1, 1)

# Scale data in respect to min and max

from sklearn.preprocessing import StandardScaler, MinMaxScaler

scaler_X = StandardScaler()
X = scaler_X.fit_transform(X)

scaler_Y = StandardScaler()
Y = scaler_Y.fit_transform(Y)

# Split the data into training and testing sets
from sklearn.model_selection import train_test_split

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.15)

# Fit the training data and run linear regression on the test data to measure the accuracy
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Linear Regression
LinearRegression_model = LinearRegression()
LinearRegression_model.fit(X_train, Y_train)

accuracy_LinearRegression = LinearRegression_model.score(X_test, Y_test)
print("Accuracy of Linear Regression:", accuracy_LinearRegression)

# Decision Tree Regression
from sklearn.tree import DecisionTreeRegressor

DecisionTree_model = DecisionTreeRegressor()
DecisionTree_model.fit(X_train, Y_train)

accuracy_DecisionTree = DecisionTree_model.score(X_test, Y_test)
print("Accuracy of Decision Tree Regression:", accuracy_DecisionTree)

# Random Forest Regression
from sklearn.ensemble import RandomForestRegressor

RandomForest_model = RandomForestRegressor(n_estimators=100, max_depth=10)
RandomForest_model.fit(X_train, Y_train)

accuracy_RandomForest = RandomForest_model.score(X_test, Y_test)
print("Accuracy of Random Forest Regression:", accuracy_RandomForest)

# Plotting predicted vs actual values for Linear Regression
Y_predict = LinearRegression_model.predict(X_test)
plt.plot(Y_test, Y_predict, '^', color='r')
plt.title("Linear Regression: Predicted vs Actual")
plt.xlabel("Actual 'Chance of Admit'")
plt.ylabel("Predicted 'Chance of Admit'")
plt.show()

# Inverse transform the scaled values to their original scale
Y_predict_orig = scaler_Y.inverse_transform(Y_predict)
Y_test_orig = scaler_Y.inverse_transform(Y_test)

# Plotting predicted vs actual values for Linear Regression (original scale)
plt.plot(Y_test_orig, Y_predict_orig, '^', color='r')
plt.title("Linear Regression: Predicted vs Actual (Original Scale)")
plt.xlabel("Actual 'Chance of Admit'")
plt.ylabel("Predicted 'Chance of Admit'")
plt.show()

k = X_test.shape[1]
n = len(X_test)

# Calculate evaluation metrics: RMSE, MSE, MAE, R2, Adjusted R2
from sklearn.metrics import mean_absolute_error, mean_squared_error
from math import sqrt

RMSE = float(format(np.sqrt(mean_squared_error(Y_test_orig, Y_predict_orig)), '.3f'))
MSE = mean_squared_error(Y_test_orig, Y_predict_orig)
MAE = mean_absolute_error(Y_test_orig, Y_predict_orig)
r2 = r2_score(Y_test_orig, Y_predict_orig)
adj_r2 = 1 - (1 - r2) * (n - 1) / (n - k - 1)

print('Evaluation metrics:')
print('RMSE =', RMSE)
print('MSE =', MSE)
print('MAE =', MAE)
print('R2 =', r2)
print('Adjusted R2 =', adj_r2)

CollegeScores = pd.read_csv ("CollegeScores.csv")


CollegeScores_scaled = scaler_X.transform(CollegeScores)
predictions = LinearRegression_model.predict(CollegeScores_scaled)
predictions_orig = scaler_Y.inverse_transform(predictions)
print(predictions_orig)