#R Data Analysis Project

Overview
This project is a Shiny application that allows users to:

1- Perform clustering on a dataset.

2- Apply Apriori association rule learning to discover relationships within the data.

Features

- Clustering: Choose the number of clusters (k-means clustering) and view the results.
- Association Rule Mining: Configure the minimum support and confidence values to generate association rules using the Apriori algorithm.
- Interactive Data Visualization: Results are displayed in interactive tables using DT.
- UI Customization: Multiple themes are available to personalize the appearance of the app.

Prerequisites

Before running the app, ensure that the following R packages are installed:

1 - install.packages(c("RColorBrewer", "shiny", "shinythemes", "tidyverse", "arules", "DT"))
Installation

2 - Clone the repository or download the source code for this project.

Install required packages (as listed in the Prerequisites section).

3- Run the Shiny app:
Open RStudio (or another R environment).

Run the following command to launch the app: shiny::runApp("path/to/your/app")

Usage

1 - Dataset Path:
	Enter the path to your dataset in the text input field.

2 - Clustering:
	Select the number of clusters from the dropdown menu (options: 2, 3, or 4).
	
3 - Apriori Analysis:
	Adjust the sliders to set the minimum support and confidence values for the association rule mining process.
	
4 - Submit Data:
	Click the "Submit" button to load the dataset and perform analysis.

5 - Analyze:
	After loading the dataset, click the "Analyze" button to run clustering and the Apriori algorithm.


Contact
For any questions or issues, please contact: [bedobakr65@gmail.com]