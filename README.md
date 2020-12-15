# Menger Curvature Simulation 

This app will simulate a sigmoidal curve according to user-specified parameters. Menger Curvature is calculated along the points of the curve and returned. 

To launch the app, in R run the following:  


```R
if (!require("shiny")) install.packages("shiny"); library(shiny)
runGitHub("MengerCurvatureSimulation", "k-t-to", subdir = "bin")
```
