This repository contains the data and code used in the manuscript entitled "Adulthood is not an illusion! The impact of size and ontogeny on suction feeding kinematics in the axolotl (Ambystoma mexicanum)."
More precisely:

•	‘landmark_over_time_dlc_csv_file.zip’ contains the CSV files created by DeepLabCut with the 2D coordinates of the landmarks over time for each video sequence.

•	‘data_Scale_A_mexicanum.csv’ contains the time and spatial scales for each video.

•	‘Toussaint-Larde_et_al_SCRIPT_data_extraction.R’ is the code used to facilitate the extraction of the kinematic variable measurements from the CSV files created by DeepLabCut (DLC).

•	‘data_Ambystoma_mexicanum.csv’ contains the extracted kinematic variables, as well as the SVL and developmental stage information for each feeding sequence.

•	‘Toussaint-Larde_et_al_SCRIPT_statistical_analysis.R’ is the code used to perform the statistical analysis on data_Ambystoma_mexicanum.csv and to create the figures and tables.

All code is written in R (R Core Development Team, 2023). If you use these data, please cite Toussaint-Lardé et al., 2025.
