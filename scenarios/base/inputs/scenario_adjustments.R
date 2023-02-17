# CMAP Freight

#### copied from the CSVM update for adjustments to freight model parameters

# Scenario Specific Parameter Adjustments
# This script is read at run time and asserts adjustments to choice models in the CSVM structure

# Stops Generation
# Good deliveries (poitive adjustment = additional goods deliveries)
asc_goods_adj = 0.0

# Stops Generation
# Service calls (positive adjustment = additional goods deliveries)
asc_service_adj = 0.0

# Proximity of stops to businesses (to represent for example localization)
# Proportional adjustment (positive adjustment is a factor > 1, negative adjustment is a factor < 1)
# Since businesses (and all industries) do a mix of both activities, in application the factors are blended
# The effect of the goods factor on industries that have a higher emphasis on goods delivery is more pronounced and vice versea
base_dist_goods_factor = 1.0
base_dist_service_factor = 1.0

# Travel cost sensitivity adjustment
# This factors distance or time variables 
# (representing generalized impendance in the stop generation model,
# a factor greater than one will reduce base to stop distance and also reduce overall stop generation,
# a factor less than one will increase base to stop distance and also increase overall stop generation)
impedance_goods_factor = 1.0
impedance_service_factor = 1.0

# Vehicle Type 
# Mode specific constant adjustments by vehicle type (positive adjustment = trips shift towards that vehicle type)
asc_vehicle_light_adj = 0.0
asc_vehicle_medium_adj = 0.0
asc_vehicle_heavy_adj = 0.0


