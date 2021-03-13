# climate_change_impact
This work is about the implemetation of a statistical downscaler. It is a tool, used in the context of IAMs (integreted assessment models) to link the global temperature, i.e. the only variable present in the IAM, to the local temperature, i.e. the variable used to compute climate change impact on countries.

We used data from NASA. Data were aggregated in 57 regions (those present in the IAM). Moreover, data were processed through smoothing built using many number of basis (from 4 to 15). We built 3 different downscalers using the different data coming from the different smoothings. We pick the best downscaler according to leave one out cross-validation.
