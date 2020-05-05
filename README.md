# Duke_DataFest2020

2020 Duke DataFest Competition Repo

Winner of Best Visualizations! Check out the finished product as well as the other winners at https://www2.stat.duke.edu/datafest-covid19/winners.html.

Data Dictionary:

Number of Observations: 50

Number of Variables: 42

Description of Variables:

`state`: Identifying variable for a particular state's google mobility trend data

`date`: Date of community mobility report

Note that for the following variables, each mobility trend is compared to a baseline, defined as the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020.

`retail_recreation`: Mobility trends for places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters.

`grocery_pharmacy`: Mobility trends for places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies.

`parks`: Mobility trends for places like national parks, public beaches, marinas, dog parks, plazas, and public gardens.

`transit_stations`: Mobility trends for places like public transport hubs such as subway, bus, and train stations.

`workplaces`: Mobility trends for places of work.

`residential`: Mobility trends for places of residence.

`stay_at_home_order`: Type of stay at home order

`date_of_stay_at_home_order`: Date in which stay at home order is declared

`mandatory_quarantine_for_travelers`: Type of travel restrictions

`non_essential_business_closures`: Type of business closures

`large_gatherings_ban`: Type of large gatherings ban

`state_mandated_school_closures`: Date of state mandated school closure

`bar_restaurant_limits`: Type of restaurant limitations

`emergency_declaration`: Date of emergency declaration

`population`: Population

`latitude`: Latitude

`longitude`: Longitude

`area`: Area

`pop_density`: Population/Area

`confirmed_cases`: Confirmed cases through date

`rate`: Rate of spread calculated by (cases today - cases 2 days prior)/cases 2 days prior

`days_from_case_to_death`: Number of days from 1st case to 1st death

`days_from_case_to_stay_home`: Number of days from 1st case to stay at home order

`cases_per_capita`: Confirmed cases divided by population size

`euclidean_dist_avg`: Euclidean distance of social mobility from average US state

`cluster_pop`: Assigned k-means cluster based on population density and initial cases (from 3/29)

`cluster_k_means`: Assigned k-means cluster based on social mobility within each `cluster_pop`

`social_dist_score`: Calculated by the following (NOTE: each value is normalized by subtracting the mean and dividing by standard deviation):

= -1*(`retail_recreation`+`grocery_pharmacy`+`parks`+`transit_stations`+`workplaces`)+`residential`




