---
title: "instructions"
author: "Mark Zais"
date: "October 2, 2018"
output: html_document
---

## SOCOM MILCON Prioritization and Allocation Tool (MPAT) Overview
MPAT is a tool designed to inform budget resource allocation decisions.  The tool incorporates optimization to maximize the total value of Military Construction (MILCON) projects subject to numerous constraints. MPAT is built using <a href="http://www.r-project.org/" target="_blank">R</a> and <a href="http://shiny.rstudio.com/" target="_blank">Shiny</a>. The imbedded optimization component uses a linear programming API called <a href="https://cran.r-project.org/web/packages/lpSolveAPI/index.html" target="_blank">lpSolveAPI</a> to solve integer programs.  

### Optimization Instructions
1. Download the CSV template (*Input_Data_Template.csv*) for required fields and formatting. See **Glossary** in the sidebar menu for data field descriptions.  <span style="color:red">**Do not delete any template columns!** </span>  
2. Import the MILCON raw data (CSV file) using the **Import** button.  
3. Adjust parameters and budget constraints. Use the slider bars to set the budget constraints for each Fiscal Year (FY). Use the numeric input boxes to set the CBPL and Rank weights (0 to 1) for the value function.  The sum of the weights must equal 1.  The run time determines how long the optimization will search for a most optimal solution.  *It is recommended to use 60 seconds or less for demonstations and ~1,000 seconds for decision-making with the current data.* 
4. Click the **Load Parameters** button to set the contraints and parameters for solving.  The **Solve** button will appear when complete.  Click the **Reset Model** button before subsequent parameter loads.  
5. Click the **Solve** button to begin solving the optimzation. When finished, a green message box with the number of projects funded will appear in the Status Report box.
6. View reports and summary statistics from the sidebar menu.


