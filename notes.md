# Packages

- dplyr
	- cuz ofc
- igraph
	- cuz ofc

- sf
	- allows our graphs to have spacial features. we only really care abt distance here but it is required for the `osmdata` package.

- osmdata
	- allows for easy importing of OpenStreetMap data.


# Data Sources
First is the traffic data, which is reported by ICBC through a tableau document. The raw CSV data is downloaded from this, then imported.

Second is road data, which I'm thinking of getting from OpenStreetMap through the `osmdata` package just for ease of use. It seems like a standard for this type of analysis in R, so I figure its our best bet. 

## Data cleaning
We will select roads, intersections, and traffic incidents from Kelowna only for now.

Certain types of crashes can be omitted, perhaps this task can be delegated.

## Data processing
The road and intersection data will be converted to a graph, with intersections being nodes and roads being edges. There will be a road length defined for every edge (sf should handle this). This doesn't account for road conditions (big hills, dangerous areas, speed limits, etc) but I think thats ok. We could potentially try to figure out a total travel time for each road? Another good idea is assigning roads as `high`, `med`, or `low` traffic. 

After we have this, we will need to use the crash data to determine a risk score for each road and assign that data to the edges.

So now, we'll have a graph with edges that have 2 separate weights, a distance weight and a risk weight.

## Data analysis
So now it's just a matter of finding paths that minimize both distance and risk. Perhaps just write a formula to combine the weights into a single value.






