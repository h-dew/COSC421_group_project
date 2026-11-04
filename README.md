# COSC421_group_project
## Group TODO
https://docs.google.com/document/d/1clrwau86hIw5MhiOIBvkLUQ2zzZhpDJunipdRWOybMo/edit?usp=sharing

# Project Layout
ROUGH LAYOUT DRAFT. FEEL FREE TO EDIT. Output files could also be in XML or RDS files

Tasks will be broken into separate scripts. These include:
  - `main.R`.                  - Calls the rest of the scripts. Performs entire analysis from start to end
    
  - `import_collisions.R`      - Imports collision data from the `/dataset/` folder. The data is cleaned and placed at `/data/cleaned/collisions.csv`
  - `analyze_collisions.R`     - Analyze collision data and generate collision risk values for intersections/roads. Output is placed at `/data/preprocessed/...`. 
    
  - `import_map.R`             - Imports roadway data from all cities listed in cleaned collision data. Only publicly-accessible roads for cars are included.  `/data/cleaned/road_map.csv`
  - `create_collision_map.R`   - Overlays collision risk values on map. Combined graph is placed in `/data/preprocessed/risk_map.csv`
    
  - `find_safe_path.R`.        - Allows users to find safest path between 2 given intersections. 

## Folder Layout

 `/data/`               - source data
 
 `/data/cleaned`        - clean data
 
 `/data/preprocessed/`  - preprocessed data
 
 `/output/`             - plots, report
