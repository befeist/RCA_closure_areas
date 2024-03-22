# Creating RCA polygons

This folder in the repository contains the scripts associated with generating RCA polygons based on the boundary lines established in the legislation. It pragmatically generates these areas based on the defined border lines. The 'polygonize.qmd' file contains the initial code that generated such areas. However, some of these were incorrectly created. To assess the problems in polygon generation, 'rca_exploration_and_testing.qmd' evaluates the RCA lines, and 'rca_poly_new_approach.qmd' evaluates the polygon generation.

# Repository Structure 

The code in this folder uses the following structure to read in files. The folders and read in files are stored on Ruttenberg et al collaboration's folder in DRIVE. 

```
RCA_data_exploration
  |__ Ruttenberg et al collaboration
    |__ RCA_Coordinate_CSV_FIles_celaned_2002_21
    |__ RCA_Mapping_Project_4Cal_Poly
    |__ revised-sp
    |__ spatial_ref_lines
  |__ scripts

```
