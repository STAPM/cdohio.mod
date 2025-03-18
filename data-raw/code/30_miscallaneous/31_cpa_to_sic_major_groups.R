#### This code is create a lookup table mapping the 105 CPA product categories
#### to broad areas of economic activity according to the SIC-2007 sections

cpa_to_sector <- copy(cdohio.mod::cpa_categories)

cpa_to_sector[1:3, sector := "Agriculture, forestry and fishing"]
cpa_to_sector[4:7, sector := "Mining and quarrying"]
cpa_to_sector[8:50, sector := "Manufacturing"]
cpa_to_sector[51:52, sector := "Electricity, gas, steam and air conditional supply"]
cpa_to_sector[53:56, sector := "Water supply, sewerage, waste management and remediation activities"]
cpa_to_sector[57, sector := "Construction"]
cpa_to_sector[58:60, sector := "Wholesale and retail trade; repair of motor vehicles and motorcycles"]
cpa_to_sector[61:66, sector := "Transportation and storage"]
cpa_to_sector[67:68, sector := "Accommodation and food service activities"]
cpa_to_sector[69:73, sector := "Information and communication"]
cpa_to_sector[74:76, sector := "Financial and insurance activities"]
cpa_to_sector[77:79, sector := "Real estate activities"]
cpa_to_sector[80:87, sector := "Professional, scientific, and technical activities"]
cpa_to_sector[88:93, sector := "Administrative and support service activities"]
cpa_to_sector[94, sector := "Public administration and defence; compulsory social security"]
cpa_to_sector[95, sector := "Education"]
cpa_to_sector[96:97, sector := "Human health and social work activities"]
cpa_to_sector[98:101, sector := "Arts, entertainment and recreation"]
cpa_to_sector[102:104, sector := "Other service activities"]
cpa_to_sector[105, sector := "Activities of households as employers"]

#### More aggregated grouping

cpa_to_sector[1:7, sector_broad := "Primary industries"]
cpa_to_sector[8:50, sector_broad := "Manufacturing"]
cpa_to_sector[51:56, sector_broad := "Utilities"]
cpa_to_sector[57, sector_broad := "Construction"]
cpa_to_sector[58:60, sector_broad := "Wholesale and retail trade"]
cpa_to_sector[61:66, sector_broad := "Transportation and storage"]
cpa_to_sector[67:68, sector_broad := "Accommodation and food service activities"]
cpa_to_sector[69:73, sector_broad := "Information and communication"]
cpa_to_sector[74:93, sector_broad := "Finance, real estate, professional and administrative services"]
cpa_to_sector[94:97, sector_broad := "Public sector, education, and health"]
cpa_to_sector[98:101, sector_broad := "Arts, entertainment and recreation"]
cpa_to_sector[102:105, sector_broad := "Other service activities"]

usethis::use_data(cpa_to_sector, overwrite = TRUE)



