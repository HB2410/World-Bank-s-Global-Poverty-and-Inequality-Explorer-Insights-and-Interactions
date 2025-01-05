# World Bank's Global Poverty and Inequality Explorer: Insights and Interactions

## Project Description
This Shiny application allows users to explore and visualize global poverty and inequality data. The app provides dynamic filters and interactive visualizations, enabling users to analyze trends and relationships between key socioeconomic variables.

---

## Features
### 1. **Data: Inequality & Poverty**
- View and filter data by **Country**, **Reporting Year**, and **Reporting Level**.
- Display filtered data in an interactive table.

### 2. **Visualizing: Inequality & Poverty Data - Country Wise**
- Plot relationships between key variables such as **Headcount**, **Poverty Severity**, and **Gini Index**.
- Filter data by **Country**, **Year Range**, and more.
- Interactive brushing to focus on specific data points in visualizations.

### 3. **Visualizing: Inequality & Poverty Data - Region Wise**
- Faceted visualizations to compare regions across variables.
- Filter data by **Year Range** and choose X/Y axes for region-based analysis.

### 4. **Significance b/w Variables**
- Analyze relationships between variables like **Gini Index** and **Headcount** using regression models.
- Display regression summaries based on selected data points.

---

## Dependencies
### Libraries Required
- **shiny**: For building the interactive web application.
- **DT**: For rendering interactive tables.
- **dplyr**: For data manipulation and filtering.
- **ggplot2**: For creating interactive plots and visualizations.

## Conclusion  
- This Shiny app provides an interactive platform to explore and analyze global poverty and inequality data.  
- Users can gain insights into key socioeconomic indicators such as **Gini Index**, **Poverty Severity**, and **Headcount Ratio**.  
- The app empowers policymakers, researchers, and data enthusiasts to make data-driven decisions by enabling dynamic filtering, visualization, and regression analysis.  
- With country-wise and region-wise perspectives, the app offers a comprehensive view of global inequality trends.

## HowToUse  
1. Clone or download the repository containing this project.  
2. Place the **pip.csv** file in the same directory as the app code.  
3. Open your **RStudio** or R console and run the following command to launch the app:  
   ```R
   shiny::runApp("app.R")
