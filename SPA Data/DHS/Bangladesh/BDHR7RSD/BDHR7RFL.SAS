libname user 'C:\COUNTRIES\BD7R';
Proc format;
  value F00001_
     0 = "Incomplete household"
     ;
  value F00002_
     1 = "Completed"
     2 = "No Household member/no competent member at home"
     3 = "Entire Household absent for extended period of time"
     4 = "Postponed"
     5 = "Refused"
     6 = "Dwelling vacant or address not a dwelling"
     7 = "Dwelling destroyed"
     8 = "Dwelling not found"
     9 = "Other"
     ;
  value F00003_
     0 = "All woman sample"
     1 = "Ever married sample"
     ;
  value F00004_
     1 = "Barisal - City corporation"
     2 = "Barisal - Other urban"
     3 = "Barisal - Rural"
     4 = "Chittagong - City corporation"
     5 = "Chittagong - Other urban"
     6 = "Chittagong - Rural"
     7 = "Dhaka - City corporation"
     8 = "Dhaka - Other urban"
     9 = "Dhaka - Rural"
    10 = "Khulna - City corporation"
    11 = "Khulna - Other urban"
    12 = "Khulna - Rural"
    13 = "Mymensingh - Other urban"
    14 = "Mymensingh - Rural"
    15 = "Rajshahi - City corporation"
    16 = "Rajshahi - Other urban"
    17 = "Rajshahi - Rural"
    18 = "Rangpur - Other urban"
    19 = "Rangpur - Rural"
    20 = "Sylhet - City corporation"
    21 = "Sylhet - Other urban"
    22 = "Sylhet - Rural"
     ;
  value F00005_
     1 = "Barisal - City corporation"
     2 = "Barisal - Other urban"
     3 = "Barisal - Rural"
     4 = "Chittagong - City corporation"
     5 = "Chittagong - Other urban"
     6 = "Chittagong - Rural"
     7 = "Dhaka - City corporation"
     8 = "Dhaka - Other urban"
     9 = "Dhaka - Rural"
    10 = "Khulna - City corporation"
    11 = "Khulna - Other urban"
    12 = "Khulna - Rural"
    13 = "Mymensingh - Other urban"
    14 = "Mymensingh - Rural"
    15 = "Rajshahi - City corporation"
    16 = "Rajshahi - Other urban"
    17 = "Rajshahi - Rural"
    18 = "Rangpur - Other urban"
    19 = "Rangpur - Rural"
    20 = "Sylhet - City corporation"
    21 = "Sylhet - Other urban"
    22 = "Sylhet - Rural"
     ;
  value F00006_
     1 = "Barisal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Mymensingh"
     6 = "Rajshahi"
     7 = "Rangpur"
     8 = "Sylhet"
     ;
  value F00007_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00008_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value F00009_
     0 = "Not selected"
     1 = "Men's survey"
     2 = "Husband's survey"
     ;
  value F00010_
     0 = "Not selected"
     1 = "Selected"
     ;
  value F00011_
     0 = "Household not selected"
     1 = "Household selected"
     ;
  value F00012_
     1 = "English"
     2 = "Language 2"
     3 = "Language 3"
     4 = "Language 4"
     5 = "Language 5"
     6 = "Language 6"
     ;
  value F00013_
     1 = "English"
     2 = "Language 2"
     3 = "Language 3"
     4 = "Language 4"
     5 = "Language 5"
     6 = "Language 6"
     ;
  value F00014_
     1 = "English"
     2 = "Language 2"
     3 = "Language 3"
     4 = "Language 4"
     5 = "Language 5"
     6 = "Language 6"
     ;
  value F00015_
     0 = "No"
     1 = "Yes"
     ;
  value F00016_
    95 = "95+"
    96 = "2+ visits"
    97 = "Inconsistent"
     ;
  value F00017_
    10 = "PIPED WATER"
    11 = "Piped into dwelling"
    12 = "Piped to yard/plot"
    13 = "Piped to neighbor"
    14 = "Public tap/standpipe"
    20 = "TUBE WELL WATER"
    21 = "Tube well or borehole"
    30 = "DUG WELL (OPEN/PROTECTED)"
    31 = "Protected well"
    32 = "Unprotected well"
    40 = "SURFACE FROM SPRING"
    41 = "Protected spring"
    42 = "Unprotected spring"
    43 = "River/dam/lake/ponds/stream/canal/irrigation channel"
    51 = "Rainwater"
    61 = "Tanker truck"
    62 = "Cart with small tank"
    71 = "Bottled water"
    96 = "Other"
     ;
  value F00018_
    10 = "PIPED WATER"
    11 = "Piped into dwelling"
    12 = "Piped to yard/plot"
    13 = "Piped to neighbor"
    14 = "Public tap/standpipe"
    20 = "TUBE WELL WATER"
    21 = "Tube well or borehole"
    30 = "DUG WELL (OPEN/PROTECTED)"
    31 = "Protected well"
    32 = "Unprotected well"
    40 = "SURFACE FROM SPRING"
    41 = "Protected spring"
    42 = "Unprotected spring"
    43 = "River/dam/lake/ponds/stream/canal/irrigation channel"
    51 = "Rainwater"
    61 = "Tanker truck"
    62 = "Cart with small tank"
    71 = "Bottled water"
    96 = "Other"
     ;
  value F00019_
     0 = "No, not interrupted for a full day"
     1 = "Yes, interrupted for a full day or more"
     8 = "Don't know"
     ;
  value F00020_
   996 = "On premises"
   998 = "Don't know"
     ;
  value F00021_
    10 = "FLUSH TOILET"
    11 = "Flush to piped sewer system"
    12 = "Flush to septic tank"
    13 = "Flush to pit latrine"
    14 = "Flush to somewhere else"
    15 = "Flush, don't know where"
    20 = "PIT TOILET LATRINE"
    21 = "Ventilated Improved Pit latrine (VIP)"
    22 = "Pit latrine with slab"
    23 = "Pit latrine without slab/open pit"
    30 = "NO FACILITY"
    31 = "No facility/bush/field"
    41 = "Composting toilet"
    42 = "Bucket toilet"
    43 = "Hanging toilet/latrine"
    96 = "Other"
     ;
  value F00022_
     0 = "No"
     1 = "Yes"
     ;
  value F00023_
     0 = "No"
     1 = "Yes"
     ;
  value F00024_
     0 = "No"
     1 = "Yes"
     ;
  value F00025_
     0 = "No"
     1 = "Yes"
     ;
  value F00026_
     0 = "No"
     1 = "Yes"
     ;
  value F00027_
     0 = "No"
     1 = "Yes"
     ;
  value F00028_
     0 = "No"
     1 = "Yes"
     ;
  value F00029_
    10 = "NATURAL"
    11 = "Earth/sand"
    12 = "Dung"
    20 = "RUDIMENTARY"
    21 = "Wood planks"
    22 = "Palm/bamboo"
    30 = "FINISHED"
    31 = "Parquet or polished wood"
    32 = "Vinyl or asphalt strips"
    33 = "Ceramic tiles"
    34 = "Cement"
    35 = "Carpet"
    96 = "Other"
     ;
  value F00030_
    10 = "NATURAL"
    11 = "No walls"
    12 = "Cane/palm/trunks"
    13 = "Dirt"
    20 = "RUDIMENTARY"
    21 = "Bamboo with mud"
    22 = "Stone with mud"
    23 = "Uncovered adobe"
    24 = "Plywood"
    25 = "Cardboard"
    26 = "Reused wood"
    30 = "FINISHED"
    31 = "Tin"
    32 = "Cement"
    33 = "Stone with lime/cement"
    34 = "Bricks"
    35 = "Cement blocks"
    36 = "Wood planks/shingles"
    96 = "Other"
     ;
  value F00031_
    10 = "NATURAL"
    11 = "No roof"
    12 = "Thatch/palm leaf"
    13 = "Sod"
    20 = "RUDIMENTARY"
    21 = "Rustic mat"
    22 = "Palm/bamboo"
    23 = "Wood planks"
    24 = "Cardboard"
    30 = "FINISHED"
    31 = "Metal"
    32 = "Wood"
    33 = "Calamine/cement fiber"
    34 = "Ceramic tiles"
    35 = "Cement"
    36 = "Roofing shingles"
    96 = "Other"
     ;
  value F00032_
     0 = "No adults"
     1 = "One adult"
     2 = "Two adults, opposite sex"
     3 = "Two adults, same sex"
     4 = "Three+ related adults"
     5 = "Unrelated adults"
     ;
  value F00033_
     1 = "Male"
     2 = "Female"
     ;
  value F00034_
    97 = "97+"
    98 = "Don't know"
     ;
  value F00035_
     0 = "No"
     1 = "Yes"
     ;
  value F00036_
     0 = "No"
     1 = "Yes"
     ;
  value F00037_
     1 = "Electricity"
     2 = "LPG"
     3 = "Natural gas"
     4 = "Biogas"
     5 = "Kerosene"
     6 = "Coal, lignite"
     7 = "Charcoal"
     8 = "Wood"
     9 = "Straw/shrubs/grass"
    10 = "Agricultural crop"
    11 = "Animal dung"
    95 = "No food cooked in house"
    96 = "Other"
     ;
  value F00038_
     0 = "No"
     1 = "Yes"
     ;
  value F00039_
     0 = "No"
     1 = "All children"
     2 = "Some children"
     3 = "No net in household"
     ;
  value F00040_
     1 = "Observed, fixed place"
     2 = "Observed, mobile place"
     3 = "Not observed: not in dwelling"
     4 = "Not observed: no permission to see"
     5 = "Not observed: other reason"
     ;
  value F00041_
     0 = "Water not available"
     1 = "Water is available"
     ;
  value F00042_
     0 = "No"
     1 = "Yes"
     ;
  value F00043_
     0 = "No"
     1 = "Yes"
     ;
  value F00044_
     0 = "No"
     1 = "Yes"
     ;
  value F00045_
     0 = "No"
     1 = "Yes"
     ;
  value F00046_
     0 = "No"
     1 = "Yes"
     ;
  value F00047_
     0 = "No"
     1 = "Yes: no cleansing agent observed"
     ;
  value F00048_
     0 = "0 PPM (no iodine)"
     7 = "Below 15 PPM"
    15 = "15 PPM and above"
    30 = "30 PPM"
   994 = "Salt not tested"
   995 = "No salt in household"
     ;
  value F00049_
     0 = "No iodine"
     1 = "Iodine present"
     3 = "No salt in household"
     6 = "Salt not tested"
     ;
  value F00050_
     1 = "In own dwelling"
     2 = "In own yard/plot"
     3 = "Elsewhere"
     ;
  value F00051_
     1 = "Adult woman"
     2 = "Adult man"
     3 = "Female child under 15 years old"
     4 = "Male child under 15 years old"
     6 = "Other"
     ;
  value F00052_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00053_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00054_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00055_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00056_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00057_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00058_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00059_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00060_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00061_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00062_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00063_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00064_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00065_
     0 = "No"
     1 = "Yes: don't know what is done to water"
     8 = "Don't know"
     ;
  value F00066_
    95 = "10 or more households"
    98 = "Don't know"
     ;
  value F00067_
     1 = "In own dwelling"
     2 = "In own yard/plot"
     3 = "Elsewhere"
     ;
  value F00068_
     1 = "Open fire"
     2 = "Open stove"
     3 = "Closed stove with chimney"
     6 = "Other"
     ;
  value F00069_
     0 = "Neither chimney or hood"
     1 = "Chimney"
     2 = "Hood"
     ;
  value F00070_
     1 = "In the house"
     2 = "In a separate building"
     3 = "Outdoors"
     6 = "Other"
     ;
  value F00071_
     0 = "No"
     1 = "Yes"
     ;
  value F00072_
     0 = "No"
     1 = "Yes"
     ;
  value F00073_
     0 = "No"
     1 = "Yes"
     ;
  value F00074_
     0 = "No"
     1 = "Yes"
     ;
  value F00075_
     0 = "No"
     1 = "Yes"
     ;
  value F00076_
     0 = "No"
     1 = "Yes"
     ;
  value F00077_
     0 = "No"
     1 = "Yes"
     ;
  value F00078_
   950 = "95 or more"
   998 = "Unknown"
     ;
  value F00079_
     0 = "No"
     1 = "Yes"
     ;
  value F00080_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00081_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00082_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00083_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00084_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00085_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00086_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00087_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00088_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00089_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00090_
     0 = "None"
    95 = "95 or more"
    98 = "Unknown"
     ;
  value F00091_
     0 = "No"
     1 = "Yes"
     ;
  value F00092_
     0 = "Never"
     1 = "Daily"
     2 = "Weekly"
     3 = "Monthly"
     4 = "Less than once a month"
     ;
  value F00093_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00094_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00095_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00096_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00097_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00098_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00099_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00100_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00101_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00102_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00103_
     0 = "No"
     1 = "Yes: don't know who sprayed the dwelling"
     8 = "Don't know if it was sprayed"
     ;
  value F00104_
     1 = "Poorest"
     2 = "Poorer"
     3 = "Middle"
     4 = "Richer"
     5 = "Richest"
     ;
  value F00105_
     1 = "Poorest"
     2 = "Poorer"
     3 = "Middle"
     4 = "Richer"
     5 = "Richest"
     ;
  value F00106_
    98 = "Don't know"
     ;
  value F00107_
     1 = "Rural"
     2 = "City Corportation"
     3 = "Other Urban"
     ;
  value F00108_
     0 = "No"
     1 = "Yes"
     ;
  value F00109_
     1 = "Rural"
     2 = "City Corportation"
     3 = "Other Urban"
     ;
  value F00110_
     0 = "No"
     1 = "Yes"
     ;
  value F00111_
     0 = "No"
     1 = "Yes"
     ;
  value F00112_
     0 = "No"
     1 = "Yes"
     ;
  value F00113_
     0 = "No"
     1 = "Yes"
     ;
  value F00114_
     0 = "No"
     1 = "Yes"
     ;
  value F00115_
     0 = "No"
     1 = "Yes"
     ;
  value F00116_
     0 = "No"
     1 = "Yes"
     ;
  value F00117_
     0 = "No"
     1 = "Yes"
     ;
  value F00118_
     0 = "No"
     1 = "Yes"
     ;
  value F00119_
     0 = "No"
     1 = "Yes"
     ;
  value F00120_
     0 = "No"
     1 = "Yes"
     ;
  value F00121_
     1 = "Observed, fixed place within dwelling"
     2 = "Observed, fixed place not within dwelling"
     3 = "Observed, no fixed place"
     4 = "Not observed, not in dwelling/yard/plot"
     5 = "Not observed, no permission to see"
     7 = "Not observed, other reason"
     ;
  value F00122_
     1 = "Covered space (inside dwelling)"
     2 = "Open space, not shared"
     3 = "Open space, shared"
     ;
  value F00123_
     1 = "Head"
     2 = "Wife or husband"
     3 = "Son/daughter"
     4 = "Son/daughter-in-law"
     5 = "Grandchild"
     6 = "Parent"
     7 = "Parent-in-law"
     8 = "Brother/sister"
     9 = "Co-spouse"
    10 = "Other relative"
    11 = "Adopted/foster child"
    12 = "Not related"
    13 = "Niece/nephew by blood"
    14 = "Niece/nephew by marriage"
    98 = "Don't know"
     ;
  value F00124_
     0 = "No"
     1 = "Yes"
     ;
  value F00125_
     0 = "No"
     1 = "Yes"
     ;
  value F00126_
     1 = "Male"
     2 = "Female"
     ;
  value F00127_
    95 = "95+"
    98 = "Don't know"
     ;
  value F00128_
     0 = "No education, preschool"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00129_
    98 = "Don't know"
     ;
  value F00130_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00131_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     8 = "Don't know"
     ;
  value F00132_
     0 = "No"
     1 = "Yes"
     ;
  value F00133_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00134_
     0 = "Mother not in household"
     ;
  value F00135_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00136_
     0 = "Father not in household"
     ;
  value F00137_
     0 = "Never married"
     1 = "Married"
     2 = "Living together"
     3 = "Widowed"
     4 = "Divorced"
     5 = "Not living together"
     ;
  value F00138_
     0 = "Never married"
     1 = "Currently married"
     2 = "Formerly/ever married"
     ;
  value F00139_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value F00140_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value F00141_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value F00142_
     0 = "No"
     1 = "Currently attending"
     2 = "Attended at some time"
     ;
  value F00143_
     0 = "No education, preschool"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00144_
    98 = "Don't know"
     ;
  value F00145_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00146_
     0 = "No"
     1 = "Yes"
     ;
  value F00147_
     0 = "No education, preschool"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00148_
    98 = "Don't know"
     ;
  value F00149_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00150_
     0 = "Never attended"
     1 = "Entered school"
     2 = "Advanced"
     3 = "Repeating"
     4 = "Dropout"
     5 = "Left school 2+ years ago"
     8 = "Don't know"
     ;
  value F00151_
     0 = "Neither certificate or registered"
     1 = "Has certificate"
     2 = "Registered"
     8 = "Don't know"
     ;
  value F00152_
     0 = "No"
     1 = "Yes"
     ;
  value F00153_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00154_
     0 = "No"
     1 = "Yes"
     ;
  value F00155_
     0 = "No"
     1 = "Yes"
     ;
  value F00156_
     0 = "No"
     1 = "Yes"
     ;
  value F00157_
     0 = "No"
     1 = "Yes"
     ;
  value F00158_
     0 = "No"
     1 = "Yes"
     ;
  value F00159_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00160_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00161_
  9998 = "Flagged cases"
     ;
  value F00162_
  9998 = "Flagged cases"
     ;
  value F00163_
 99998 = "Flagged cases"
     ;
  value F00164_
  9998 = "Flagged cases"
     ;
  value F00165_
 99998 = "Flagged cases"
     ;
  value F00166_
 99998 = "Flagged cases"
     ;
  value F00167_
 99998 = "Flagged cases"
     ;
  value F00168_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value F00169_
     0 = "Month, year and day"
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     4 = "Year and age - year ignored"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00170_
     0 = "Doesn't smoke"
    80 = "80+"
    94 = "Smokes pipes, cigars, etc"
     ;
  value F00171_
  9998 = "Flagged cases"
     ;
  value F00172_
  9998 = "Flagged cases"
     ;
  value F00173_
     1 = "Under age 18"
     2 = "Age 18 or older"
     ;
  value F00174_
     0 = "Not in household"
     ;
  value F00175_
     1 = "Granted"
     2 = "Parent/other responsible refused"
     3 = "Respondent refused"
     4 = "Adult/parent not present"
     5 = "Minor not present"
     ;
  value F00176_
   994 = "Not present"
   995 = "Refused"
   996 = "Other"
     ;
  value F00177_
     0 = "No/don't know"
     1 = "Yes"
     ;
  value F00178_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value F00179_
   997 = "Inconsistent"
     ;
  value F00180_
     1 = "Severe"
     2 = "Moderate"
     3 = "Mild"
     4 = "Not anemic"
     ;
  value F00181_
     0 = "No"
     1 = "Yes"
     ;
  value F00182_
     1 = "Never in union"
     2 = "Other"
     ;
  value F00183_
     1 = "Granted"
     2 = "Parent/other responsible refused"
     3 = "Respondent refused"
     4 = "Adult/parent not present"
     5 = "Minor not present"
     6 = "Sample not tested/lost/not enough DBS"
     ;
  value $F00184_
99992 = "Woman's interview incomplete or woman is not de-facto"
99993 = "Sample damaged/insufficient/not found in lab"
99994 = "Not present"
99995 = "Refused"
99996 = "Other"
'?    ' = "Missing"
     ;
  value F00185_
     1 = "Blood taken"
     2 = "Not present"
     3 = "Refused"
     4 = "Sample not tested/lost/damaged/insufficient"
     5 = "Not enough DBS to complete protocol"
     6 = "Other"
     ;
  value F00186_
     1 = "Granted"
     2 = "Parent/other responsible refused"
     3 = "Respondent refused"
     6 = "Sample not tested/lost/not enough DBS"
     ;
  value F00187_
     1 = "Completed"
     2 = "Not at home"
     3 = "Postponed"
     4 = "Refused"
     5 = "Partly completed"
     6 = "Incapacitated"
     7 = "Other"
     ;
  value F00188_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00189_
    98 = "Don't know"
     ;
  value F00190_
     0 = "No education"
     1 = "Primary incomplete"
     2 = "Primary complete"
     3 = "Secondary incomplete"
     4 = "Secondary complete or higher"
     8 = "Don't know"
     ;
  value F00191_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00192_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00193_
  9998 = "Flagged cases"
     ;
  value F00194_
  9998 = "Flagged cases"
     ;
  value F00195_
 99998 = "Flagged cases"
     ;
  value F00196_
  9998 = "Flagged cases"
     ;
  value F00197_
  9998 = "Flagged cases"
     ;
  value F00198_
 99998 = "Flagged cases"
     ;
  value F00199_
  9998 = "Flagged cases"
     ;
  value F00200_
  9998 = "Flagged cases"
     ;
  value F00201_
 99998 = "Flagged cases"
     ;
  value F00202_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value F00203_
     0 = "Not measured"
     1 = "Lying"
     2 = "Standing"
     ;
  value F00204_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00205_
     1 = "Male"
     2 = "Female"
     ;
  value F00206_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00207_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00208_
     0 = "Month, year and day"
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00209_
     0 = "Not in household"
     ;
  value F00210_
     1 = "Granted"
     2 = "Refused"
     3 = "Not present/other"
     ;
  value F00211_
   994 = "Not present"
   995 = "Refused"
   996 = "Other"
     ;
  value F00212_
     0 = "Measured"
     3 = "Not present/other"
     4 = "Refused"
     6 = "Other"
     ;
  value F00213_
   997 = "Inconsistent"
     ;
  value F00214_
     1 = "Severe"
     2 = "Moderate"
     3 = "Mild"
     4 = "Not anemic"
     ;
  value F00215_
     0 = "No"
     1 = "Yes"
     ;
  value F00216_
   993 = "Mother not de facto"
   994 = "Incomplete woman's interview"
   995 = "Not in household"
     ;
  value F00217_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00218_
    98 = "Don't know"
     ;
  value F00219_
     0 = "No education"
     1 = "Primary incomplete"
     2 = "Primary complete"
     3 = "Secondary incomplete"
     4 = "Secondary complete or higher"
     8 = "Don't know"
     ;
  value F00220_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00221_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00222_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00223_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00224_
     1 = "Male"
     2 = "Female"
     ;
  value F00225_
    95 = "95+"
    98 = "DK"
     ;
  value F00226_
    95 = "95+"
     ;
  value F00227_
     1 = "Never married"
     2 = "Currently married/Divorced/Separated/Deserted/Widowed"
     ;
  value F00228_
     1 = "Age is 50+"
     2 = "Age is 18-49"
     ;
  value F00229_
     0 = "No, not pregnant"
     1 = "Yes, pregnant"
     8 = "DK"
     ;
  value F00230_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00231_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00232_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "College or higher"
     ;
  value F00233_
     0 = "No"
     1 = "Yes"
     ;
  value F00234_
     0 = "Not working"
    11 = "Land Owner"
    12 = "Farmer"
    13 = "Agricultural Worker"
    14 = "Fisherman"
    15 = "Poultry raising, Cattle raising"
    16 = "Home-based Manufacturing (Handicraft, Food products)"
    21 = "Rickshaw driver, Brick breaking, Road building, Construction worker, Boatman, and Earth work etc."
    22 = "Domestic servant"
    23 = "Non-agricultural worker (Factory worker, blue collar service)"
    31 = "Carpenter, Mason, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services"
    41 = "Doctor, Lawyer, Dentist, Accountant, Teacher, Nurse, Family welfare visitor, Mid and high level services (Government/private)"
    51 = "Big businessman"
    52 = "Small business/trader"
    61 = "Unemployed/student"
    62 = "Retired"
    96 = "Others"
    98 = "Don't know"
     ;
  value F00235_
     1 = "Granted"
     2 = "Respondent refused"
     3 = "Respondent not present"
     ;
  value F00236_
     0 = "No"
     1 = "Yes"
     ;
  value F00237_
     0 = "No"
     1 = "Yes"
     ;
  value F00238_
     0 = "No"
     1 = "Yes"
     ;
  value F00239_
     1 = "Small"
     2 = "Medium"
     3 = "Large"
     4 = "Extra large"
     ;
  value F00240_
     0 = "No"
     1 = "Yes"
     ;
  value F00241_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00242_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00243_
     0 = "No"
     1 = "Yes"
     7 = "Refused"
     ;
  value F00244_
   100 = "Days ago: 00"
   499 = "Years ago: Missing"
     ;
  value F00245_
     1 = "Medical college hospital"
     2 = "Specialized government hospital"
     3 = "District hospital"
     4 = "MCWC"
     5 = "Upazila health complex"
     6 = "UH & family welfare center"
     7 = "Community clinic"
     8 = "Satellite clinic/epi outreach"
     9 = "Government field worker (FWA)"
    10 = "Other public sector"
    21 = "NGO static clinic"
    22 = "NGO satellite clinic"
    23 = "NGO depo holder"
    24 = "NGO field worker"
    25 = "Other NGO sector"
    31 = "Private medical college hospital"
    32 = "Private hospital/clinic"
    33 = "Private clinic"
    34 = "Qualified doctor's chamber"
    35 = "Non-qualified doctor's chamber"
    37 = "Pharmacy/drug store"
    36 = "Other private medical sector"
    41 = "Home"
    42 = "Workplace"
    43 = "Shop"
    44 = "Traditional practitioner"
    96 = "Other"
     ;
  value F00246_
     0 = "No"
     1 = "Yes"
     ;
  value F00247_
     0 = "No"
     1 = "Yes"
     ;
  value F00248_
     0 = "No"
     1 = "Yes"
     ;
  value F00249_
     0 = "No"
     1 = "Yes"
     ;
  value F00250_
     0 = "No"
     1 = "Yes"
     ;
  value F00251_
     0 = "No"
     1 = "Yes"
     ;
  value F00252_
     0 = "No"
     1 = "Yes"
     ;
  value F00253_
     0 = "No"
     1 = "Yes"
     ;
  value F00254_
     0 = "No"
     1 = "Yes"
     ;
  value F00255_
     0 = "No"
     1 = "Yes"
     ;
  value F00256_
     0 = "No"
     1 = "Yes"
     ;
  value F00257_
     0 = "No"
     1 = "Yes"
     ;
  value F00258_
     0 = "No"
     1 = "Yes"
     ;
  value F00259_
     0 = "No"
     1 = "Yes"
     ;
  value F00260_
     0 = "No"
     1 = "Yes"
     ;
  value F00261_
     0 = "No"
     1 = "Yes"
     ;
  value F00262_
     0 = "No"
     1 = "Yes"
     ;
  value F00263_
     0 = "No"
     1 = "Yes"
     ;
  value F00264_
     0 = "No"
     1 = "Yes"
     ;
  value F00265_
     0 = "No"
     1 = "Yes"
     ;
  value F00266_
     0 = "No"
     1 = "Yes"
     ;
  value F00267_
     0 = "No"
     1 = "Yes"
     ;
  value F00268_
     0 = "No"
     1 = "Yes"
     ;
  value F00269_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00270_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00271_
     0 = "No"
     1 = "Yes"
     7 = "Refused"
     ;
  value F00272_
     0 = "No"
     1 = "Yes"
     4 = "Refused"
     ;
  value F00273_
     1 = "Medical college hospital"
     2 = "Specialized government hospital"
     3 = "District hospital"
     4 = "MCWC"
     5 = "Upazila health complex"
     6 = "UH & family welfare center"
     7 = "Community clinic"
     8 = "Satellite clinic/epi outreach"
     9 = "Government field worker (FWA)"
    10 = "Other public sector"
    21 = "NGO static clinic"
    22 = "NGO satellite clinic"
    23 = "NGO depo holder"
    24 = "NGO field worker"
    25 = "Other NGO sector"
    31 = "Private medical college hospital"
    32 = "Private hospital/clinic"
    33 = "Private clinic"
    34 = "Qualified doctor's chamber"
    35 = "Non-qualified doctor's chamber"
    37 = "Pharmacy/drug store"
    36 = "Other private medical sector"
    41 = "Home"
    42 = "Workplace"
    43 = "Shop"
    44 = "Traditional practitioner"
    96 = "Other"
     ;
  value F00274_
     0 = "No"
     1 = "Yes"
     ;
  value F00275_
     0 = "No"
     1 = "Yes"
     ;
  value F00276_
     0 = "No"
     1 = "Yes"
     ;
  value F00277_
     0 = "No"
     1 = "Yes"
     ;
  value F00278_
     0 = "No"
     1 = "Yes"
     ;
  value F00279_
     0 = "No"
     1 = "Yes"
     ;
  value F00280_
     0 = "No"
     1 = "Yes"
     ;
  value F00281_
     0 = "No"
     1 = "Yes"
     ;
  value F00282_
     0 = "No"
     1 = "Yes"
     ;
  value F00283_
     0 = "No"
     1 = "Yes"
     ;
  value F00284_
     0 = "No"
     1 = "Yes"
     ;
  value F00285_
     0 = "No"
     1 = "Yes"
     ;
  value F00286_
     0 = "No"
     1 = "Yes"
     ;
  value F00287_
     0 = "No"
     1 = "Yes"
     ;
  value F00288_
     0 = "No"
     1 = "Yes"
     ;
  value F00289_
     0 = "No"
     1 = "Yes"
     ;
  value F00290_
     0 = "No"
     1 = "Yes"
     ;
  value F00291_
     0 = "No"
     1 = "Yes"
     ;
  value F00292_
     0 = "No"
     1 = "Yes"
     ;
  value F00293_
     0 = "No"
     1 = "Yes"
     ;
  value F00294_
     0 = "No"
     1 = "Yes"
     ;
  value F00295_
     0 = "No"
     1 = "Yes"
     ;
  value F00296_
     0 = "No"
     1 = "Yes"
     ;
  value F00297_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00298_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00299_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00300_
   994 = "Refused"
   995 = "Technical problems"
   996 = "Other"
     ;
  value F00301_
     1 = "Systolic < 130 & Diastolic < 85"
     2 = "Systolic < 140 & Diastolic < 90"
     3 = "Systolic < 160 & Diastolic < 100"
     4 = "Systolic < 180 & Diastolic < 110"
     5 = "Systolic < 210 & Diastolic < 120"
     6 = "Systolic >= 210 or Diastolic >= 120"
     ;
  value F00302_
     1 = "Granted"
     2 = "Refused"
     3 = "Respondent not present"
     ;
  value F00303_
     1 = "Granted"
     2 = "Refused"
     3 = "Respondent not present"
     7 = "No second visit"
     ;
  value F00304_
     1 = "Granted"
     2 = "Refused"
     3 = "Respondent not present"
     7 = "No third visit"
     ;
  value F00305_
     1 = "Granted"
     2 = "Refused"
     3 = "Respondent not present"
     ;
  value F00306_
     1 = "Granted"
     2 = "Refused"
     3 = "Respondent not present"
     7 = "No second visit"
     ;
  value F00307_
     1 = "Granted"
     2 = "Refused"
     3 = "Respondent not present"
     7 = "No third visit"
     ;
  value F00308_
  9595 = "Never"
     ;
  value F00309_
  9595 = "Never"
     ;
  value F00310_
  9595 = "Never"
     ;
  value F00311_
     1 = "8 hours or more"
     2 = "Less than 8 hours"
     ;
  value F00312_
   994 = "Not present"
   995 = "Refused"
   996 = "Other"
     ;
  value F00313_
  9998 = "Flagged cases"
     ;
  value F00314_
  9998 = "Flagged cases"
     ;
  value F00315_
 99998 = "Flagged cases"
     ;
  value F00316_
 99998 = "Flagged cases"
     ;
  value F00317_
 99998 = "Flagged cases"
     ;
  value F00318_
 99998 = "Flagged cases"
     ;
  value F00319_
  9998 = "Flagged cases"
     ;
  value F00320_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value F00321_
  9998 = "Flagged cases"
     ;
  value F00322_
  9998 = "Flagged cases"
     ;
  value F00323_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     4 = "Y & age - y ignored"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value F00324_
     1 = "Completed"
     2 = "Not at home"
     3 = "Postponed"
     4 = "Refused"
     5 = "Partly completed"
     6 = "Incapacitated"
     7 = "Other"
     ;
  value F00325_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     6 = "Pre-primary"
     8 = "Don't know"
     ;
  value F00326_
     0 = "No education"
    98 = "Don't know"
     ;
  value F00327_
     0 = "No education"
     1 = "Primary incomplete"
     2 = "Primary complete"
     3 = "Secondary incomplete"
     4 = "Secondary complete or higher"
     ;
  value F00328_
     0 = "No"
     1 = "Yes"
     ;
Data user.BDHR7RFL;
  attrib HHID     length=$12 label="Case Identification";
  attrib HV000    length=$3 label="Country code and phase";
  attrib HV001    label="Cluster number";
  attrib HV002    label="Household number";
  attrib HV003    format=F00001_. label="Respondent's line number (answering Household questionnaire)";
  attrib HV004    label="Ultimate area unit";
  attrib HV005    label="Household sample weight (6 decimals)";
  attrib HV006    label="Month of interview";
  attrib HV007    label="Year of interview";
  attrib HV008    label="Date of interview (CMC)";
  attrib HV008A   label="Date of interview Century Day Code (CDC)";
  attrib HV009    label="Number of household members";
  attrib HV010    label="Number of eligible women in household";
  attrib HV011    label="NA - Number of eligible men in household";
  attrib HV012    label="Number of de jure members";
  attrib HV013    label="Number of de facto members";
  attrib HV014    label="Number of children 5 and under (de jure)";
  attrib HV015    format=F00002_. label="Result of household interview";
  attrib HV016    label="Day of interview";
  attrib HV017    label="Number of visits";
  attrib HV018    label="Interviewer identification";
  attrib HV019    label="Keyer identification";
  attrib HV020    format=F00003_. label="Ever-married sample";
  attrib HV021    label="Primary sampling unit";
  attrib HV022    format=F00004_. label="Sample strata for sampling errors";
  attrib HV023    format=F00005_. label="Stratification used in sample design";
  attrib HV024    format=F00006_. label="Division";
  attrib HV025    format=F00007_. label="Type of place of residence";
  attrib HV026    format=F00008_. label="NA - Place of residence";
  attrib HV027    format=F00009_. label="Household selected for male interview";
  attrib HV028    label="Household weight for male subsample (6 decimals)";
  attrib HV030    label="Field supervisor";
  attrib HV031    label="Field editor";
  attrib HV032    label="Office editor";
  attrib HV035    label="Number of eligible children for height and weight";
  attrib HV040    label="Cluster altitude in meters";
  attrib HV041    label="Total adults measured";
  attrib HV042    format=F00010_. label="Household selected for hemoglobin";
  attrib HV044    format=F00011_. label="NA - Household selected for Domestic Violence module";
  attrib HV045A   format=F00012_. label="NA - Language of questionnaire";
  attrib HV045B   format=F00013_. label="NA - Language of interview";
  attrib HV045C   format=F00014_. label="NA - Native language of respondent";
  attrib HV046    format=F00015_. label="NA - Translator used";
  attrib HV801    label="Time interview started (hhmm - 24 hour clock)";
  attrib HV802    label="Time interview ended (hhmm - 24 hour clock)";
  attrib HV803    format=F00016_. label="Length of interview in minutes";
  attrib HV804    label="Number of visits for biomarker";
  attrib HV807D   label="Day of biomarker visit";
  attrib HV807M   label="Month of biomarker visit";
  attrib HV807Y   label="Year of biomarker visit";
  attrib HV807C   label="Date of biomarker (CMC)";
  attrib HV807A   label="Date of biomarker Century Day Code (CDC)";
  attrib HV201    format=F00017_. label="Source of drinking water";
  attrib HV202    format=F00018_. label="NA - Source of non-drinking water";
  attrib HV201A   format=F00019_. label="NA - Water not available for at least a day last two weeks";
  attrib HV204    format=F00020_. label="NA - Time to get to water source (minutes)";
  attrib HV205    format=F00021_. label="Type of toilet facility";
  attrib HV206    format=F00022_. label="Has electricity";
  attrib HV207    format=F00023_. label="Has radio";
  attrib HV208    format=F00024_. label="Has television";
  attrib HV209    format=F00025_. label="Has refrigerator";
  attrib HV210    format=F00026_. label="Has bicycle";
  attrib HV211    format=F00027_. label="Has motorcycle/scooter";
  attrib HV212    format=F00028_. label="Has car/truck/minibus";
  attrib HV213    format=F00029_. label="Main floor material";
  attrib HV214    format=F00030_. label="Main wall material";
  attrib HV215    format=F00031_. label="Main roof material";
  attrib HV216    label="Number of rooms used for sleeping";
  attrib HV217    format=F00032_. label="Relationship structure";
  attrib HV218    label="Line number of head of household";
  attrib HV219    format=F00033_. label="Sex of head of household";
  attrib HV220    format=F00034_. label="Age of head of household";
  attrib HV221    format=F00035_. label="Has telephone (land-line)";
  attrib HV225    format=F00036_. label="Share toilet with other households";
  attrib HV226    format=F00037_. label="Type of cooking fuel";
  attrib HV227    format=F00038_. label="NA - Has mosquito bed net for sleeping";
  attrib HV228    format=F00039_. label="NA - Children under 5 slept under mosquito bed net last night";
  attrib HV230A   format=F00040_. label="Place where household members wash their hands";
  attrib HV230B   format=F00041_. label="Presence of water at hand washing place";
  attrib HV232    format=F00042_. label="Items present: Soap or detergent";
  attrib HV232B   format=F00043_. label="Items present: Ash, mud, sand";
  attrib HV232C   format=F00044_. label="NA - Items present: CS";
  attrib HV232D   format=F00045_. label="NA - Items present: CS";
  attrib HV232E   format=F00046_. label="NA - Items present: CS";
  attrib HV232Y   format=F00047_. label="Items present: None";
  attrib HV234    format=F00048_. label="NA - Result of salt test for iodine (PPM)";
  attrib HV234A   format=F00049_. label="NA - Result of salt test for iodine";
  attrib HV235    format=F00050_. label="Location of source for water";
  attrib HV236    format=F00051_. label="NA - Person fetching water";
  attrib HV237    format=F00052_. label="Anything done to water to make safe to drink";
  attrib HV237A   format=F00053_. label="Water usually treated by: boil";
  attrib HV237B   format=F00054_. label="Water usually treated by: add bleach/chlorine";
  attrib HV237C   format=F00055_. label="Water usually treated by: strain through a cloth";
  attrib HV237D   format=F00056_. label="Water usually treated by: use water filter";
  attrib HV237E   format=F00057_. label="Water usually treated by: solar disinfection";
  attrib HV237F   format=F00058_. label="Water usually treated by: let it stand and settle";
  attrib HV237G   format=F00059_. label="NA - Water usually treated by: CS";
  attrib HV237H   format=F00060_. label="NA - Water usually treated by: CS";
  attrib HV237I   format=F00061_. label="NA - Water usually treated by: CS";
  attrib HV237J   format=F00062_. label="NA - Water usually treated by: CS";
  attrib HV237K   format=F00063_. label="NA - Water usually treated by: CS";
  attrib HV237X   format=F00064_. label="Water usually treated by: other";
  attrib HV237Z   format=F00065_. label="Water usually treated by: don't know";
  attrib HV238    format=F00066_. label="Number of households sharing toilet";
  attrib HV238A   format=F00067_. label="Location of toilet facility";
  attrib HV239    format=F00068_. label="NA - Food cooked on stove or open fire";
  attrib HV240    format=F00069_. label="NA - Household has a chimney, hood or neither";
  attrib HV241    format=F00070_. label="Food cooked in the house/ separate building/ outdoors";
  attrib HV242    format=F00071_. label="Household has separate room used as kitchen";
  attrib HV243A   format=F00072_. label="Has mobile telephone";
  attrib HV243B   format=F00073_. label="NA - Has watch";
  attrib HV243C   format=F00074_. label="NA - Has animal-drawn cart";
  attrib HV243D   format=F00075_. label="Has boat with a motor";
  attrib HV243E   format=F00076_. label="Has a computer";
  attrib HV244    format=F00077_. label="Owns land usable for agriculture";
  attrib HV245    format=F00078_. label="Hectares of agricultural land (1 decimal)";
  attrib HV246    format=F00079_. label="Owns livestock, herds or farm animals";
  attrib HV246A   format=F00080_. label="NA - Owns cattle";
  attrib HV246B   format=F00081_. label="Owns cows/ bulls";
  attrib HV246C   format=F00082_. label="NA - Owns horses/ donkeys/ mules";
  attrib HV246D   format=F00083_. label="NA - Owns goats";
  attrib HV246E   format=F00084_. label="NA - Owns sheep";
  attrib HV246F   format=F00085_. label="Owns chickens/poultry";
  attrib HV246G   format=F00086_. label="Owns buffalo";
  attrib HV246H   format=F00087_. label="Owns other farm animals";
  attrib HV246I   format=F00088_. label="Owns goat/sheep";
  attrib HV246J   format=F00089_. label="NA - Owns CS";
  attrib HV246K   format=F00090_. label="NA - Owns CS";
  attrib HV247    format=F00091_. label="Has bank account";
  attrib HV252    format=F00092_. label="NA - Frequency household members smoke inside the house";
  attrib HV253    format=F00093_. label="NA - Has dwelling been sprayed against mosquitoes in last 12 months";
  attrib HV253A   format=F00094_. label="NA - Dwelling sprayed by: government worker/program";
  attrib HV253B   format=F00095_. label="NA - Dwelling sprayed by: private company";
  attrib HV253C   format=F00096_. label="NA - Dwelling sprayed by: NGO";
  attrib HV253D   format=F00097_. label="NA - Dwelling sprayed by: CS";
  attrib HV253E   format=F00098_. label="NA - Dwelling sprayed by: CS";
  attrib HV253F   format=F00099_. label="NA - Dwelling sprayed by: CS";
  attrib HV253G   format=F00100_. label="NA - Dwelling sprayed by: CS";
  attrib HV253H   format=F00101_. label="NA - Dwelling sprayed by: CS";
  attrib HV253X   format=F00102_. label="NA - Dwelling sprayed by: other";
  attrib HV253Z   format=F00103_. label="NA - Dwelling sprayed by: don't know";
  attrib HV270    format=F00104_. label="Wealth index combined";
  attrib HV271    label="Wealth index factor score combined (5 decimals)";
  attrib HV270A   format=F00105_. label="Wealth index for urban/rural";
  attrib HV271A   label="Wealth index factor score for urban/rural (5 decimals)";
  attrib HML1     format=F00106_. label="NA - Number of mosquito bed nets";
  attrib HML1A    label="NA - Number of mosquito bed nets with specific information";
  attrib HML2     label="NA - Number of children under mosquito bed net previous night";
  attrib SHLOCAL  format=F00107_. label="Locality";
  attrib SHELIG   format=F00108_. label="Household eligible for glucose/BP";
  attrib SHADULT  label="Total adults aged 18+";
  attrib SHLOCAL2 format=F00109_. label="Locality-original";
  attrib SH119    format=F00110_. label="Owns a homestead";
  attrib SH121B   format=F00111_. label="Has solar electricity";
  attrib SH121I   format=F00112_. label="Has almirah/warddrobe";
  attrib SH121J   format=F00113_. label="Has electric fan";
  attrib SH121K   format=F00114_. label="Has DVD/CD player";
  attrib SH121L   format=F00115_. label="Has water pump";
  attrib SH121M   format=F00116_. label="Has IPS/generator";
  attrib SH121N   format=F00117_. label="Has air conditioner";
  attrib SH122B   format=F00118_. label="Has autobike/tempo/CNG";
  attrib SH122C   format=F00119_. label="Has rickshaw/van";
  attrib SH122G   format=F00120_. label="Has canoe/boat without motor";
  attrib SH125    format=F00121_. label="Observed place for handwashing";
  attrib SH127A   format=F00122_. label="Observe type of place for hand washing";
  attrib HVIDX_01 label="Line number(1)";
  attrib HVIDX_02 label="Line number(2)";
  attrib HVIDX_03 label="Line number(3)";
  attrib HVIDX_04 label="Line number(4)";
  attrib HVIDX_05 label="Line number(5)";
  attrib HVIDX_06 label="Line number(6)";
  attrib HVIDX_07 label="Line number(7)";
  attrib HVIDX_08 label="Line number(8)";
  attrib HVIDX_09 label="Line number(9)";
  attrib HVIDX_10 label="Line number(10)";
  attrib HVIDX_11 label="Line number(11)";
  attrib HVIDX_12 label="Line number(12)";
  attrib HVIDX_13 label="Line number(13)";
  attrib HVIDX_14 label="Line number(14)";
  attrib HVIDX_15 label="Line number(15)";
  attrib HVIDX_16 label="Line number(16)";
  attrib HVIDX_17 label="Line number(17)";
  attrib HVIDX_18 label="Line number(18)";
  attrib HVIDX_19 label="Line number(19)";
  attrib HVIDX_20 label="Line number(20)";
  attrib HVIDX_21 label="Line number(21)";
  attrib HVIDX_22 label="Line number(22)";
  attrib HVIDX_23 label="Line number(23)";
  attrib HVIDX_24 label="Line number(24)";
  attrib HVIDX_25 label="Line number(25)";
  attrib HVIDX_26 label="Line number(26)";
  attrib HVIDX_27 label="Line number(27)";
  attrib HVIDX_28 label="Line number(28)";
  attrib HVIDX_29 label="Line number(29)";
  attrib HVIDX_30 label="Line number(30)";
  attrib HV101_01 format=F00123_. label="Relationship to head(1)";
  attrib HV101_02 format=F00123_. label="Relationship to head(2)";
  attrib HV101_03 format=F00123_. label="Relationship to head(3)";
  attrib HV101_04 format=F00123_. label="Relationship to head(4)";
  attrib HV101_05 format=F00123_. label="Relationship to head(5)";
  attrib HV101_06 format=F00123_. label="Relationship to head(6)";
  attrib HV101_07 format=F00123_. label="Relationship to head(7)";
  attrib HV101_08 format=F00123_. label="Relationship to head(8)";
  attrib HV101_09 format=F00123_. label="Relationship to head(9)";
  attrib HV101_10 format=F00123_. label="Relationship to head(10)";
  attrib HV101_11 format=F00123_. label="Relationship to head(11)";
  attrib HV101_12 format=F00123_. label="Relationship to head(12)";
  attrib HV101_13 format=F00123_. label="Relationship to head(13)";
  attrib HV101_14 format=F00123_. label="Relationship to head(14)";
  attrib HV101_15 format=F00123_. label="Relationship to head(15)";
  attrib HV101_16 format=F00123_. label="Relationship to head(16)";
  attrib HV101_17 format=F00123_. label="Relationship to head(17)";
  attrib HV101_18 format=F00123_. label="Relationship to head(18)";
  attrib HV101_19 format=F00123_. label="Relationship to head(19)";
  attrib HV101_20 format=F00123_. label="Relationship to head(20)";
  attrib HV101_21 format=F00123_. label="Relationship to head(21)";
  attrib HV101_22 format=F00123_. label="Relationship to head(22)";
  attrib HV101_23 format=F00123_. label="Relationship to head(23)";
  attrib HV101_24 format=F00123_. label="Relationship to head(24)";
  attrib HV101_25 format=F00123_. label="Relationship to head(25)";
  attrib HV101_26 format=F00123_. label="Relationship to head(26)";
  attrib HV101_27 format=F00123_. label="Relationship to head(27)";
  attrib HV101_28 format=F00123_. label="Relationship to head(28)";
  attrib HV101_29 format=F00123_. label="Relationship to head(29)";
  attrib HV101_30 format=F00123_. label="Relationship to head(30)";
  attrib HV102_01 format=F00124_. label="Usual resident(1)";
  attrib HV102_02 format=F00124_. label="Usual resident(2)";
  attrib HV102_03 format=F00124_. label="Usual resident(3)";
  attrib HV102_04 format=F00124_. label="Usual resident(4)";
  attrib HV102_05 format=F00124_. label="Usual resident(5)";
  attrib HV102_06 format=F00124_. label="Usual resident(6)";
  attrib HV102_07 format=F00124_. label="Usual resident(7)";
  attrib HV102_08 format=F00124_. label="Usual resident(8)";
  attrib HV102_09 format=F00124_. label="Usual resident(9)";
  attrib HV102_10 format=F00124_. label="Usual resident(10)";
  attrib HV102_11 format=F00124_. label="Usual resident(11)";
  attrib HV102_12 format=F00124_. label="Usual resident(12)";
  attrib HV102_13 format=F00124_. label="Usual resident(13)";
  attrib HV102_14 format=F00124_. label="Usual resident(14)";
  attrib HV102_15 format=F00124_. label="Usual resident(15)";
  attrib HV102_16 format=F00124_. label="Usual resident(16)";
  attrib HV102_17 format=F00124_. label="Usual resident(17)";
  attrib HV102_18 format=F00124_. label="Usual resident(18)";
  attrib HV102_19 format=F00124_. label="Usual resident(19)";
  attrib HV102_20 format=F00124_. label="Usual resident(20)";
  attrib HV102_21 format=F00124_. label="Usual resident(21)";
  attrib HV102_22 format=F00124_. label="Usual resident(22)";
  attrib HV102_23 format=F00124_. label="Usual resident(23)";
  attrib HV102_24 format=F00124_. label="Usual resident(24)";
  attrib HV102_25 format=F00124_. label="Usual resident(25)";
  attrib HV102_26 format=F00124_. label="Usual resident(26)";
  attrib HV102_27 format=F00124_. label="Usual resident(27)";
  attrib HV102_28 format=F00124_. label="Usual resident(28)";
  attrib HV102_29 format=F00124_. label="Usual resident(29)";
  attrib HV102_30 format=F00124_. label="Usual resident(30)";
  attrib HV103_01 format=F00125_. label="Slept last night(1)";
  attrib HV103_02 format=F00125_. label="Slept last night(2)";
  attrib HV103_03 format=F00125_. label="Slept last night(3)";
  attrib HV103_04 format=F00125_. label="Slept last night(4)";
  attrib HV103_05 format=F00125_. label="Slept last night(5)";
  attrib HV103_06 format=F00125_. label="Slept last night(6)";
  attrib HV103_07 format=F00125_. label="Slept last night(7)";
  attrib HV103_08 format=F00125_. label="Slept last night(8)";
  attrib HV103_09 format=F00125_. label="Slept last night(9)";
  attrib HV103_10 format=F00125_. label="Slept last night(10)";
  attrib HV103_11 format=F00125_. label="Slept last night(11)";
  attrib HV103_12 format=F00125_. label="Slept last night(12)";
  attrib HV103_13 format=F00125_. label="Slept last night(13)";
  attrib HV103_14 format=F00125_. label="Slept last night(14)";
  attrib HV103_15 format=F00125_. label="Slept last night(15)";
  attrib HV103_16 format=F00125_. label="Slept last night(16)";
  attrib HV103_17 format=F00125_. label="Slept last night(17)";
  attrib HV103_18 format=F00125_. label="Slept last night(18)";
  attrib HV103_19 format=F00125_. label="Slept last night(19)";
  attrib HV103_20 format=F00125_. label="Slept last night(20)";
  attrib HV103_21 format=F00125_. label="Slept last night(21)";
  attrib HV103_22 format=F00125_. label="Slept last night(22)";
  attrib HV103_23 format=F00125_. label="Slept last night(23)";
  attrib HV103_24 format=F00125_. label="Slept last night(24)";
  attrib HV103_25 format=F00125_. label="Slept last night(25)";
  attrib HV103_26 format=F00125_. label="Slept last night(26)";
  attrib HV103_27 format=F00125_. label="Slept last night(27)";
  attrib HV103_28 format=F00125_. label="Slept last night(28)";
  attrib HV103_29 format=F00125_. label="Slept last night(29)";
  attrib HV103_30 format=F00125_. label="Slept last night(30)";
  attrib HV104_01 format=F00126_. label="Sex of household member(1)";
  attrib HV104_02 format=F00126_. label="Sex of household member(2)";
  attrib HV104_03 format=F00126_. label="Sex of household member(3)";
  attrib HV104_04 format=F00126_. label="Sex of household member(4)";
  attrib HV104_05 format=F00126_. label="Sex of household member(5)";
  attrib HV104_06 format=F00126_. label="Sex of household member(6)";
  attrib HV104_07 format=F00126_. label="Sex of household member(7)";
  attrib HV104_08 format=F00126_. label="Sex of household member(8)";
  attrib HV104_09 format=F00126_. label="Sex of household member(9)";
  attrib HV104_10 format=F00126_. label="Sex of household member(10)";
  attrib HV104_11 format=F00126_. label="Sex of household member(11)";
  attrib HV104_12 format=F00126_. label="Sex of household member(12)";
  attrib HV104_13 format=F00126_. label="Sex of household member(13)";
  attrib HV104_14 format=F00126_. label="Sex of household member(14)";
  attrib HV104_15 format=F00126_. label="Sex of household member(15)";
  attrib HV104_16 format=F00126_. label="Sex of household member(16)";
  attrib HV104_17 format=F00126_. label="Sex of household member(17)";
  attrib HV104_18 format=F00126_. label="Sex of household member(18)";
  attrib HV104_19 format=F00126_. label="Sex of household member(19)";
  attrib HV104_20 format=F00126_. label="Sex of household member(20)";
  attrib HV104_21 format=F00126_. label="Sex of household member(21)";
  attrib HV104_22 format=F00126_. label="Sex of household member(22)";
  attrib HV104_23 format=F00126_. label="Sex of household member(23)";
  attrib HV104_24 format=F00126_. label="Sex of household member(24)";
  attrib HV104_25 format=F00126_. label="Sex of household member(25)";
  attrib HV104_26 format=F00126_. label="Sex of household member(26)";
  attrib HV104_27 format=F00126_. label="Sex of household member(27)";
  attrib HV104_28 format=F00126_. label="Sex of household member(28)";
  attrib HV104_29 format=F00126_. label="Sex of household member(29)";
  attrib HV104_30 format=F00126_. label="Sex of household member(30)";
  attrib HV105_01 format=F00127_. label="Age of household members(1)";
  attrib HV105_02 format=F00127_. label="Age of household members(2)";
  attrib HV105_03 format=F00127_. label="Age of household members(3)";
  attrib HV105_04 format=F00127_. label="Age of household members(4)";
  attrib HV105_05 format=F00127_. label="Age of household members(5)";
  attrib HV105_06 format=F00127_. label="Age of household members(6)";
  attrib HV105_07 format=F00127_. label="Age of household members(7)";
  attrib HV105_08 format=F00127_. label="Age of household members(8)";
  attrib HV105_09 format=F00127_. label="Age of household members(9)";
  attrib HV105_10 format=F00127_. label="Age of household members(10)";
  attrib HV105_11 format=F00127_. label="Age of household members(11)";
  attrib HV105_12 format=F00127_. label="Age of household members(12)";
  attrib HV105_13 format=F00127_. label="Age of household members(13)";
  attrib HV105_14 format=F00127_. label="Age of household members(14)";
  attrib HV105_15 format=F00127_. label="Age of household members(15)";
  attrib HV105_16 format=F00127_. label="Age of household members(16)";
  attrib HV105_17 format=F00127_. label="Age of household members(17)";
  attrib HV105_18 format=F00127_. label="Age of household members(18)";
  attrib HV105_19 format=F00127_. label="Age of household members(19)";
  attrib HV105_20 format=F00127_. label="Age of household members(20)";
  attrib HV105_21 format=F00127_. label="Age of household members(21)";
  attrib HV105_22 format=F00127_. label="Age of household members(22)";
  attrib HV105_23 format=F00127_. label="Age of household members(23)";
  attrib HV105_24 format=F00127_. label="Age of household members(24)";
  attrib HV105_25 format=F00127_. label="Age of household members(25)";
  attrib HV105_26 format=F00127_. label="Age of household members(26)";
  attrib HV105_27 format=F00127_. label="Age of household members(27)";
  attrib HV105_28 format=F00127_. label="Age of household members(28)";
  attrib HV105_29 format=F00127_. label="Age of household members(29)";
  attrib HV105_30 format=F00127_. label="Age of household members(30)";
  attrib HV106_01 format=F00128_. label="Highest educational level attained(1)";
  attrib HV106_02 format=F00128_. label="Highest educational level attained(2)";
  attrib HV106_03 format=F00128_. label="Highest educational level attained(3)";
  attrib HV106_04 format=F00128_. label="Highest educational level attained(4)";
  attrib HV106_05 format=F00128_. label="Highest educational level attained(5)";
  attrib HV106_06 format=F00128_. label="Highest educational level attained(6)";
  attrib HV106_07 format=F00128_. label="Highest educational level attained(7)";
  attrib HV106_08 format=F00128_. label="Highest educational level attained(8)";
  attrib HV106_09 format=F00128_. label="Highest educational level attained(9)";
  attrib HV106_10 format=F00128_. label="Highest educational level attained(10)";
  attrib HV106_11 format=F00128_. label="Highest educational level attained(11)";
  attrib HV106_12 format=F00128_. label="Highest educational level attained(12)";
  attrib HV106_13 format=F00128_. label="Highest educational level attained(13)";
  attrib HV106_14 format=F00128_. label="Highest educational level attained(14)";
  attrib HV106_15 format=F00128_. label="Highest educational level attained(15)";
  attrib HV106_16 format=F00128_. label="Highest educational level attained(16)";
  attrib HV106_17 format=F00128_. label="Highest educational level attained(17)";
  attrib HV106_18 format=F00128_. label="Highest educational level attained(18)";
  attrib HV106_19 format=F00128_. label="Highest educational level attained(19)";
  attrib HV106_20 format=F00128_. label="Highest educational level attained(20)";
  attrib HV106_21 format=F00128_. label="Highest educational level attained(21)";
  attrib HV106_22 format=F00128_. label="Highest educational level attained(22)";
  attrib HV106_23 format=F00128_. label="Highest educational level attained(23)";
  attrib HV106_24 format=F00128_. label="Highest educational level attained(24)";
  attrib HV106_25 format=F00128_. label="Highest educational level attained(25)";
  attrib HV106_26 format=F00128_. label="Highest educational level attained(26)";
  attrib HV106_27 format=F00128_. label="Highest educational level attained(27)";
  attrib HV106_28 format=F00128_. label="Highest educational level attained(28)";
  attrib HV106_29 format=F00128_. label="Highest educational level attained(29)";
  attrib HV106_30 format=F00128_. label="Highest educational level attained(30)";
  attrib HV107_01 format=F00129_. label="Highest year of education completed(1)";
  attrib HV107_02 format=F00129_. label="Highest year of education completed(2)";
  attrib HV107_03 format=F00129_. label="Highest year of education completed(3)";
  attrib HV107_04 format=F00129_. label="Highest year of education completed(4)";
  attrib HV107_05 format=F00129_. label="Highest year of education completed(5)";
  attrib HV107_06 format=F00129_. label="Highest year of education completed(6)";
  attrib HV107_07 format=F00129_. label="Highest year of education completed(7)";
  attrib HV107_08 format=F00129_. label="Highest year of education completed(8)";
  attrib HV107_09 format=F00129_. label="Highest year of education completed(9)";
  attrib HV107_10 format=F00129_. label="Highest year of education completed(10)";
  attrib HV107_11 format=F00129_. label="Highest year of education completed(11)";
  attrib HV107_12 format=F00129_. label="Highest year of education completed(12)";
  attrib HV107_13 format=F00129_. label="Highest year of education completed(13)";
  attrib HV107_14 format=F00129_. label="Highest year of education completed(14)";
  attrib HV107_15 format=F00129_. label="Highest year of education completed(15)";
  attrib HV107_16 format=F00129_. label="Highest year of education completed(16)";
  attrib HV107_17 format=F00129_. label="Highest year of education completed(17)";
  attrib HV107_18 format=F00129_. label="Highest year of education completed(18)";
  attrib HV107_19 format=F00129_. label="Highest year of education completed(19)";
  attrib HV107_20 format=F00129_. label="Highest year of education completed(20)";
  attrib HV107_21 format=F00129_. label="Highest year of education completed(21)";
  attrib HV107_22 format=F00129_. label="Highest year of education completed(22)";
  attrib HV107_23 format=F00129_. label="Highest year of education completed(23)";
  attrib HV107_24 format=F00129_. label="Highest year of education completed(24)";
  attrib HV107_25 format=F00129_. label="Highest year of education completed(25)";
  attrib HV107_26 format=F00129_. label="Highest year of education completed(26)";
  attrib HV107_27 format=F00129_. label="Highest year of education completed(27)";
  attrib HV107_28 format=F00129_. label="Highest year of education completed(28)";
  attrib HV107_29 format=F00129_. label="Highest year of education completed(29)";
  attrib HV107_30 format=F00129_. label="Highest year of education completed(30)";
  attrib HV108_01 format=F00130_. label="Education completed in single years(1)";
  attrib HV108_02 format=F00130_. label="Education completed in single years(2)";
  attrib HV108_03 format=F00130_. label="Education completed in single years(3)";
  attrib HV108_04 format=F00130_. label="Education completed in single years(4)";
  attrib HV108_05 format=F00130_. label="Education completed in single years(5)";
  attrib HV108_06 format=F00130_. label="Education completed in single years(6)";
  attrib HV108_07 format=F00130_. label="Education completed in single years(7)";
  attrib HV108_08 format=F00130_. label="Education completed in single years(8)";
  attrib HV108_09 format=F00130_. label="Education completed in single years(9)";
  attrib HV108_10 format=F00130_. label="Education completed in single years(10)";
  attrib HV108_11 format=F00130_. label="Education completed in single years(11)";
  attrib HV108_12 format=F00130_. label="Education completed in single years(12)";
  attrib HV108_13 format=F00130_. label="Education completed in single years(13)";
  attrib HV108_14 format=F00130_. label="Education completed in single years(14)";
  attrib HV108_15 format=F00130_. label="Education completed in single years(15)";
  attrib HV108_16 format=F00130_. label="Education completed in single years(16)";
  attrib HV108_17 format=F00130_. label="Education completed in single years(17)";
  attrib HV108_18 format=F00130_. label="Education completed in single years(18)";
  attrib HV108_19 format=F00130_. label="Education completed in single years(19)";
  attrib HV108_20 format=F00130_. label="Education completed in single years(20)";
  attrib HV108_21 format=F00130_. label="Education completed in single years(21)";
  attrib HV108_22 format=F00130_. label="Education completed in single years(22)";
  attrib HV108_23 format=F00130_. label="Education completed in single years(23)";
  attrib HV108_24 format=F00130_. label="Education completed in single years(24)";
  attrib HV108_25 format=F00130_. label="Education completed in single years(25)";
  attrib HV108_26 format=F00130_. label="Education completed in single years(26)";
  attrib HV108_27 format=F00130_. label="Education completed in single years(27)";
  attrib HV108_28 format=F00130_. label="Education completed in single years(28)";
  attrib HV108_29 format=F00130_. label="Education completed in single years(29)";
  attrib HV108_30 format=F00130_. label="Education completed in single years(30)";
  attrib HV109_01 format=F00131_. label="Educational attainment(1)";
  attrib HV109_02 format=F00131_. label="Educational attainment(2)";
  attrib HV109_03 format=F00131_. label="Educational attainment(3)";
  attrib HV109_04 format=F00131_. label="Educational attainment(4)";
  attrib HV109_05 format=F00131_. label="Educational attainment(5)";
  attrib HV109_06 format=F00131_. label="Educational attainment(6)";
  attrib HV109_07 format=F00131_. label="Educational attainment(7)";
  attrib HV109_08 format=F00131_. label="Educational attainment(8)";
  attrib HV109_09 format=F00131_. label="Educational attainment(9)";
  attrib HV109_10 format=F00131_. label="Educational attainment(10)";
  attrib HV109_11 format=F00131_. label="Educational attainment(11)";
  attrib HV109_12 format=F00131_. label="Educational attainment(12)";
  attrib HV109_13 format=F00131_. label="Educational attainment(13)";
  attrib HV109_14 format=F00131_. label="Educational attainment(14)";
  attrib HV109_15 format=F00131_. label="Educational attainment(15)";
  attrib HV109_16 format=F00131_. label="Educational attainment(16)";
  attrib HV109_17 format=F00131_. label="Educational attainment(17)";
  attrib HV109_18 format=F00131_. label="Educational attainment(18)";
  attrib HV109_19 format=F00131_. label="Educational attainment(19)";
  attrib HV109_20 format=F00131_. label="Educational attainment(20)";
  attrib HV109_21 format=F00131_. label="Educational attainment(21)";
  attrib HV109_22 format=F00131_. label="Educational attainment(22)";
  attrib HV109_23 format=F00131_. label="Educational attainment(23)";
  attrib HV109_24 format=F00131_. label="Educational attainment(24)";
  attrib HV109_25 format=F00131_. label="Educational attainment(25)";
  attrib HV109_26 format=F00131_. label="Educational attainment(26)";
  attrib HV109_27 format=F00131_. label="Educational attainment(27)";
  attrib HV109_28 format=F00131_. label="Educational attainment(28)";
  attrib HV109_29 format=F00131_. label="Educational attainment(29)";
  attrib HV109_30 format=F00131_. label="Educational attainment(30)";
  attrib HV110_01 format=F00132_. label="NA - Member still in school(1)";
  attrib HV110_02 format=F00132_. label="NA - Member still in school(2)";
  attrib HV110_03 format=F00132_. label="NA - Member still in school(3)";
  attrib HV110_04 format=F00132_. label="NA - Member still in school(4)";
  attrib HV110_05 format=F00132_. label="NA - Member still in school(5)";
  attrib HV110_06 format=F00132_. label="NA - Member still in school(6)";
  attrib HV110_07 format=F00132_. label="NA - Member still in school(7)";
  attrib HV110_08 format=F00132_. label="NA - Member still in school(8)";
  attrib HV110_09 format=F00132_. label="NA - Member still in school(9)";
  attrib HV110_10 format=F00132_. label="NA - Member still in school(10)";
  attrib HV110_11 format=F00132_. label="NA - Member still in school(11)";
  attrib HV110_12 format=F00132_. label="NA - Member still in school(12)";
  attrib HV110_13 format=F00132_. label="NA - Member still in school(13)";
  attrib HV110_14 format=F00132_. label="NA - Member still in school(14)";
  attrib HV110_15 format=F00132_. label="NA - Member still in school(15)";
  attrib HV110_16 format=F00132_. label="NA - Member still in school(16)";
  attrib HV110_17 format=F00132_. label="NA - Member still in school(17)";
  attrib HV110_18 format=F00132_. label="NA - Member still in school(18)";
  attrib HV110_19 format=F00132_. label="NA - Member still in school(19)";
  attrib HV110_20 format=F00132_. label="NA - Member still in school(20)";
  attrib HV110_21 format=F00132_. label="NA - Member still in school(21)";
  attrib HV110_22 format=F00132_. label="NA - Member still in school(22)";
  attrib HV110_23 format=F00132_. label="NA - Member still in school(23)";
  attrib HV110_24 format=F00132_. label="NA - Member still in school(24)";
  attrib HV110_25 format=F00132_. label="NA - Member still in school(25)";
  attrib HV110_26 format=F00132_. label="NA - Member still in school(26)";
  attrib HV110_27 format=F00132_. label="NA - Member still in school(27)";
  attrib HV110_28 format=F00132_. label="NA - Member still in school(28)";
  attrib HV110_29 format=F00132_. label="NA - Member still in school(29)";
  attrib HV110_30 format=F00132_. label="NA - Member still in school(30)";
  attrib HV111_01 format=F00133_. label="NA - Mother alive(1)";
  attrib HV111_02 format=F00133_. label="NA - Mother alive(2)";
  attrib HV111_03 format=F00133_. label="NA - Mother alive(3)";
  attrib HV111_04 format=F00133_. label="NA - Mother alive(4)";
  attrib HV111_05 format=F00133_. label="NA - Mother alive(5)";
  attrib HV111_06 format=F00133_. label="NA - Mother alive(6)";
  attrib HV111_07 format=F00133_. label="NA - Mother alive(7)";
  attrib HV111_08 format=F00133_. label="NA - Mother alive(8)";
  attrib HV111_09 format=F00133_. label="NA - Mother alive(9)";
  attrib HV111_10 format=F00133_. label="NA - Mother alive(10)";
  attrib HV111_11 format=F00133_. label="NA - Mother alive(11)";
  attrib HV111_12 format=F00133_. label="NA - Mother alive(12)";
  attrib HV111_13 format=F00133_. label="NA - Mother alive(13)";
  attrib HV111_14 format=F00133_. label="NA - Mother alive(14)";
  attrib HV111_15 format=F00133_. label="NA - Mother alive(15)";
  attrib HV111_16 format=F00133_. label="NA - Mother alive(16)";
  attrib HV111_17 format=F00133_. label="NA - Mother alive(17)";
  attrib HV111_18 format=F00133_. label="NA - Mother alive(18)";
  attrib HV111_19 format=F00133_. label="NA - Mother alive(19)";
  attrib HV111_20 format=F00133_. label="NA - Mother alive(20)";
  attrib HV111_21 format=F00133_. label="NA - Mother alive(21)";
  attrib HV111_22 format=F00133_. label="NA - Mother alive(22)";
  attrib HV111_23 format=F00133_. label="NA - Mother alive(23)";
  attrib HV111_24 format=F00133_. label="NA - Mother alive(24)";
  attrib HV111_25 format=F00133_. label="NA - Mother alive(25)";
  attrib HV111_26 format=F00133_. label="NA - Mother alive(26)";
  attrib HV111_27 format=F00133_. label="NA - Mother alive(27)";
  attrib HV111_28 format=F00133_. label="NA - Mother alive(28)";
  attrib HV111_29 format=F00133_. label="NA - Mother alive(29)";
  attrib HV111_30 format=F00133_. label="NA - Mother alive(30)";
  attrib HV112_01 format=F00134_. label="NA - Mother's line number(1)";
  attrib HV112_02 format=F00134_. label="NA - Mother's line number(2)";
  attrib HV112_03 format=F00134_. label="NA - Mother's line number(3)";
  attrib HV112_04 format=F00134_. label="NA - Mother's line number(4)";
  attrib HV112_05 format=F00134_. label="NA - Mother's line number(5)";
  attrib HV112_06 format=F00134_. label="NA - Mother's line number(6)";
  attrib HV112_07 format=F00134_. label="NA - Mother's line number(7)";
  attrib HV112_08 format=F00134_. label="NA - Mother's line number(8)";
  attrib HV112_09 format=F00134_. label="NA - Mother's line number(9)";
  attrib HV112_10 format=F00134_. label="NA - Mother's line number(10)";
  attrib HV112_11 format=F00134_. label="NA - Mother's line number(11)";
  attrib HV112_12 format=F00134_. label="NA - Mother's line number(12)";
  attrib HV112_13 format=F00134_. label="NA - Mother's line number(13)";
  attrib HV112_14 format=F00134_. label="NA - Mother's line number(14)";
  attrib HV112_15 format=F00134_. label="NA - Mother's line number(15)";
  attrib HV112_16 format=F00134_. label="NA - Mother's line number(16)";
  attrib HV112_17 format=F00134_. label="NA - Mother's line number(17)";
  attrib HV112_18 format=F00134_. label="NA - Mother's line number(18)";
  attrib HV112_19 format=F00134_. label="NA - Mother's line number(19)";
  attrib HV112_20 format=F00134_. label="NA - Mother's line number(20)";
  attrib HV112_21 format=F00134_. label="NA - Mother's line number(21)";
  attrib HV112_22 format=F00134_. label="NA - Mother's line number(22)";
  attrib HV112_23 format=F00134_. label="NA - Mother's line number(23)";
  attrib HV112_24 format=F00134_. label="NA - Mother's line number(24)";
  attrib HV112_25 format=F00134_. label="NA - Mother's line number(25)";
  attrib HV112_26 format=F00134_. label="NA - Mother's line number(26)";
  attrib HV112_27 format=F00134_. label="NA - Mother's line number(27)";
  attrib HV112_28 format=F00134_. label="NA - Mother's line number(28)";
  attrib HV112_29 format=F00134_. label="NA - Mother's line number(29)";
  attrib HV112_30 format=F00134_. label="NA - Mother's line number(30)";
  attrib HV113_01 format=F00135_. label="NA - Father alive(1)";
  attrib HV113_02 format=F00135_. label="NA - Father alive(2)";
  attrib HV113_03 format=F00135_. label="NA - Father alive(3)";
  attrib HV113_04 format=F00135_. label="NA - Father alive(4)";
  attrib HV113_05 format=F00135_. label="NA - Father alive(5)";
  attrib HV113_06 format=F00135_. label="NA - Father alive(6)";
  attrib HV113_07 format=F00135_. label="NA - Father alive(7)";
  attrib HV113_08 format=F00135_. label="NA - Father alive(8)";
  attrib HV113_09 format=F00135_. label="NA - Father alive(9)";
  attrib HV113_10 format=F00135_. label="NA - Father alive(10)";
  attrib HV113_11 format=F00135_. label="NA - Father alive(11)";
  attrib HV113_12 format=F00135_. label="NA - Father alive(12)";
  attrib HV113_13 format=F00135_. label="NA - Father alive(13)";
  attrib HV113_14 format=F00135_. label="NA - Father alive(14)";
  attrib HV113_15 format=F00135_. label="NA - Father alive(15)";
  attrib HV113_16 format=F00135_. label="NA - Father alive(16)";
  attrib HV113_17 format=F00135_. label="NA - Father alive(17)";
  attrib HV113_18 format=F00135_. label="NA - Father alive(18)";
  attrib HV113_19 format=F00135_. label="NA - Father alive(19)";
  attrib HV113_20 format=F00135_. label="NA - Father alive(20)";
  attrib HV113_21 format=F00135_. label="NA - Father alive(21)";
  attrib HV113_22 format=F00135_. label="NA - Father alive(22)";
  attrib HV113_23 format=F00135_. label="NA - Father alive(23)";
  attrib HV113_24 format=F00135_. label="NA - Father alive(24)";
  attrib HV113_25 format=F00135_. label="NA - Father alive(25)";
  attrib HV113_26 format=F00135_. label="NA - Father alive(26)";
  attrib HV113_27 format=F00135_. label="NA - Father alive(27)";
  attrib HV113_28 format=F00135_. label="NA - Father alive(28)";
  attrib HV113_29 format=F00135_. label="NA - Father alive(29)";
  attrib HV113_30 format=F00135_. label="NA - Father alive(30)";
  attrib HV114_01 format=F00136_. label="NA - Father's line number(1)";
  attrib HV114_02 format=F00136_. label="NA - Father's line number(2)";
  attrib HV114_03 format=F00136_. label="NA - Father's line number(3)";
  attrib HV114_04 format=F00136_. label="NA - Father's line number(4)";
  attrib HV114_05 format=F00136_. label="NA - Father's line number(5)";
  attrib HV114_06 format=F00136_. label="NA - Father's line number(6)";
  attrib HV114_07 format=F00136_. label="NA - Father's line number(7)";
  attrib HV114_08 format=F00136_. label="NA - Father's line number(8)";
  attrib HV114_09 format=F00136_. label="NA - Father's line number(9)";
  attrib HV114_10 format=F00136_. label="NA - Father's line number(10)";
  attrib HV114_11 format=F00136_. label="NA - Father's line number(11)";
  attrib HV114_12 format=F00136_. label="NA - Father's line number(12)";
  attrib HV114_13 format=F00136_. label="NA - Father's line number(13)";
  attrib HV114_14 format=F00136_. label="NA - Father's line number(14)";
  attrib HV114_15 format=F00136_. label="NA - Father's line number(15)";
  attrib HV114_16 format=F00136_. label="NA - Father's line number(16)";
  attrib HV114_17 format=F00136_. label="NA - Father's line number(17)";
  attrib HV114_18 format=F00136_. label="NA - Father's line number(18)";
  attrib HV114_19 format=F00136_. label="NA - Father's line number(19)";
  attrib HV114_20 format=F00136_. label="NA - Father's line number(20)";
  attrib HV114_21 format=F00136_. label="NA - Father's line number(21)";
  attrib HV114_22 format=F00136_. label="NA - Father's line number(22)";
  attrib HV114_23 format=F00136_. label="NA - Father's line number(23)";
  attrib HV114_24 format=F00136_. label="NA - Father's line number(24)";
  attrib HV114_25 format=F00136_. label="NA - Father's line number(25)";
  attrib HV114_26 format=F00136_. label="NA - Father's line number(26)";
  attrib HV114_27 format=F00136_. label="NA - Father's line number(27)";
  attrib HV114_28 format=F00136_. label="NA - Father's line number(28)";
  attrib HV114_29 format=F00136_. label="NA - Father's line number(29)";
  attrib HV114_30 format=F00136_. label="NA - Father's line number(30)";
  attrib HV115_01 format=F00137_. label="Current marital status(1)";
  attrib HV115_02 format=F00137_. label="Current marital status(2)";
  attrib HV115_03 format=F00137_. label="Current marital status(3)";
  attrib HV115_04 format=F00137_. label="Current marital status(4)";
  attrib HV115_05 format=F00137_. label="Current marital status(5)";
  attrib HV115_06 format=F00137_. label="Current marital status(6)";
  attrib HV115_07 format=F00137_. label="Current marital status(7)";
  attrib HV115_08 format=F00137_. label="Current marital status(8)";
  attrib HV115_09 format=F00137_. label="Current marital status(9)";
  attrib HV115_10 format=F00137_. label="Current marital status(10)";
  attrib HV115_11 format=F00137_. label="Current marital status(11)";
  attrib HV115_12 format=F00137_. label="Current marital status(12)";
  attrib HV115_13 format=F00137_. label="Current marital status(13)";
  attrib HV115_14 format=F00137_. label="Current marital status(14)";
  attrib HV115_15 format=F00137_. label="Current marital status(15)";
  attrib HV115_16 format=F00137_. label="Current marital status(16)";
  attrib HV115_17 format=F00137_. label="Current marital status(17)";
  attrib HV115_18 format=F00137_. label="Current marital status(18)";
  attrib HV115_19 format=F00137_. label="Current marital status(19)";
  attrib HV115_20 format=F00137_. label="Current marital status(20)";
  attrib HV115_21 format=F00137_. label="Current marital status(21)";
  attrib HV115_22 format=F00137_. label="Current marital status(22)";
  attrib HV115_23 format=F00137_. label="Current marital status(23)";
  attrib HV115_24 format=F00137_. label="Current marital status(24)";
  attrib HV115_25 format=F00137_. label="Current marital status(25)";
  attrib HV115_26 format=F00137_. label="Current marital status(26)";
  attrib HV115_27 format=F00137_. label="Current marital status(27)";
  attrib HV115_28 format=F00137_. label="Current marital status(28)";
  attrib HV115_29 format=F00137_. label="Current marital status(29)";
  attrib HV115_30 format=F00137_. label="Current marital status(30)";
  attrib HV116_01 format=F00138_. label="Currently, formerly, never married(1)";
  attrib HV116_02 format=F00138_. label="Currently, formerly, never married(2)";
  attrib HV116_03 format=F00138_. label="Currently, formerly, never married(3)";
  attrib HV116_04 format=F00138_. label="Currently, formerly, never married(4)";
  attrib HV116_05 format=F00138_. label="Currently, formerly, never married(5)";
  attrib HV116_06 format=F00138_. label="Currently, formerly, never married(6)";
  attrib HV116_07 format=F00138_. label="Currently, formerly, never married(7)";
  attrib HV116_08 format=F00138_. label="Currently, formerly, never married(8)";
  attrib HV116_09 format=F00138_. label="Currently, formerly, never married(9)";
  attrib HV116_10 format=F00138_. label="Currently, formerly, never married(10)";
  attrib HV116_11 format=F00138_. label="Currently, formerly, never married(11)";
  attrib HV116_12 format=F00138_. label="Currently, formerly, never married(12)";
  attrib HV116_13 format=F00138_. label="Currently, formerly, never married(13)";
  attrib HV116_14 format=F00138_. label="Currently, formerly, never married(14)";
  attrib HV116_15 format=F00138_. label="Currently, formerly, never married(15)";
  attrib HV116_16 format=F00138_. label="Currently, formerly, never married(16)";
  attrib HV116_17 format=F00138_. label="Currently, formerly, never married(17)";
  attrib HV116_18 format=F00138_. label="Currently, formerly, never married(18)";
  attrib HV116_19 format=F00138_. label="Currently, formerly, never married(19)";
  attrib HV116_20 format=F00138_. label="Currently, formerly, never married(20)";
  attrib HV116_21 format=F00138_. label="Currently, formerly, never married(21)";
  attrib HV116_22 format=F00138_. label="Currently, formerly, never married(22)";
  attrib HV116_23 format=F00138_. label="Currently, formerly, never married(23)";
  attrib HV116_24 format=F00138_. label="Currently, formerly, never married(24)";
  attrib HV116_25 format=F00138_. label="Currently, formerly, never married(25)";
  attrib HV116_26 format=F00138_. label="Currently, formerly, never married(26)";
  attrib HV116_27 format=F00138_. label="Currently, formerly, never married(27)";
  attrib HV116_28 format=F00138_. label="Currently, formerly, never married(28)";
  attrib HV116_29 format=F00138_. label="Currently, formerly, never married(29)";
  attrib HV116_30 format=F00138_. label="Currently, formerly, never married(30)";
  attrib HV117_01 format=F00139_. label="Eligibility for female interview(1)";
  attrib HV117_02 format=F00139_. label="Eligibility for female interview(2)";
  attrib HV117_03 format=F00139_. label="Eligibility for female interview(3)";
  attrib HV117_04 format=F00139_. label="Eligibility for female interview(4)";
  attrib HV117_05 format=F00139_. label="Eligibility for female interview(5)";
  attrib HV117_06 format=F00139_. label="Eligibility for female interview(6)";
  attrib HV117_07 format=F00139_. label="Eligibility for female interview(7)";
  attrib HV117_08 format=F00139_. label="Eligibility for female interview(8)";
  attrib HV117_09 format=F00139_. label="Eligibility for female interview(9)";
  attrib HV117_10 format=F00139_. label="Eligibility for female interview(10)";
  attrib HV117_11 format=F00139_. label="Eligibility for female interview(11)";
  attrib HV117_12 format=F00139_. label="Eligibility for female interview(12)";
  attrib HV117_13 format=F00139_. label="Eligibility for female interview(13)";
  attrib HV117_14 format=F00139_. label="Eligibility for female interview(14)";
  attrib HV117_15 format=F00139_. label="Eligibility for female interview(15)";
  attrib HV117_16 format=F00139_. label="Eligibility for female interview(16)";
  attrib HV117_17 format=F00139_. label="Eligibility for female interview(17)";
  attrib HV117_18 format=F00139_. label="Eligibility for female interview(18)";
  attrib HV117_19 format=F00139_. label="Eligibility for female interview(19)";
  attrib HV117_20 format=F00139_. label="Eligibility for female interview(20)";
  attrib HV117_21 format=F00139_. label="Eligibility for female interview(21)";
  attrib HV117_22 format=F00139_. label="Eligibility for female interview(22)";
  attrib HV117_23 format=F00139_. label="Eligibility for female interview(23)";
  attrib HV117_24 format=F00139_. label="Eligibility for female interview(24)";
  attrib HV117_25 format=F00139_. label="Eligibility for female interview(25)";
  attrib HV117_26 format=F00139_. label="Eligibility for female interview(26)";
  attrib HV117_27 format=F00139_. label="Eligibility for female interview(27)";
  attrib HV117_28 format=F00139_. label="Eligibility for female interview(28)";
  attrib HV117_29 format=F00139_. label="Eligibility for female interview(29)";
  attrib HV117_30 format=F00139_. label="Eligibility for female interview(30)";
  attrib HV118_01 format=F00140_. label="NA - Eligibility for male interview(1)";
  attrib HV118_02 format=F00140_. label="NA - Eligibility for male interview(2)";
  attrib HV118_03 format=F00140_. label="NA - Eligibility for male interview(3)";
  attrib HV118_04 format=F00140_. label="NA - Eligibility for male interview(4)";
  attrib HV118_05 format=F00140_. label="NA - Eligibility for male interview(5)";
  attrib HV118_06 format=F00140_. label="NA - Eligibility for male interview(6)";
  attrib HV118_07 format=F00140_. label="NA - Eligibility for male interview(7)";
  attrib HV118_08 format=F00140_. label="NA - Eligibility for male interview(8)";
  attrib HV118_09 format=F00140_. label="NA - Eligibility for male interview(9)";
  attrib HV118_10 format=F00140_. label="NA - Eligibility for male interview(10)";
  attrib HV118_11 format=F00140_. label="NA - Eligibility for male interview(11)";
  attrib HV118_12 format=F00140_. label="NA - Eligibility for male interview(12)";
  attrib HV118_13 format=F00140_. label="NA - Eligibility for male interview(13)";
  attrib HV118_14 format=F00140_. label="NA - Eligibility for male interview(14)";
  attrib HV118_15 format=F00140_. label="NA - Eligibility for male interview(15)";
  attrib HV118_16 format=F00140_. label="NA - Eligibility for male interview(16)";
  attrib HV118_17 format=F00140_. label="NA - Eligibility for male interview(17)";
  attrib HV118_18 format=F00140_. label="NA - Eligibility for male interview(18)";
  attrib HV118_19 format=F00140_. label="NA - Eligibility for male interview(19)";
  attrib HV118_20 format=F00140_. label="NA - Eligibility for male interview(20)";
  attrib HV118_21 format=F00140_. label="NA - Eligibility for male interview(21)";
  attrib HV118_22 format=F00140_. label="NA - Eligibility for male interview(22)";
  attrib HV118_23 format=F00140_. label="NA - Eligibility for male interview(23)";
  attrib HV118_24 format=F00140_. label="NA - Eligibility for male interview(24)";
  attrib HV118_25 format=F00140_. label="NA - Eligibility for male interview(25)";
  attrib HV118_26 format=F00140_. label="NA - Eligibility for male interview(26)";
  attrib HV118_27 format=F00140_. label="NA - Eligibility for male interview(27)";
  attrib HV118_28 format=F00140_. label="NA - Eligibility for male interview(28)";
  attrib HV118_29 format=F00140_. label="NA - Eligibility for male interview(29)";
  attrib HV118_30 format=F00140_. label="NA - Eligibility for male interview(30)";
  attrib HV120_01 format=F00141_. label="Children eligibility for height/weight and hemoglobin(1)";
  attrib HV120_02 format=F00141_. label="Children eligibility for height/weight and hemoglobin(2)";
  attrib HV120_03 format=F00141_. label="Children eligibility for height/weight and hemoglobin(3)";
  attrib HV120_04 format=F00141_. label="Children eligibility for height/weight and hemoglobin(4)";
  attrib HV120_05 format=F00141_. label="Children eligibility for height/weight and hemoglobin(5)";
  attrib HV120_06 format=F00141_. label="Children eligibility for height/weight and hemoglobin(6)";
  attrib HV120_07 format=F00141_. label="Children eligibility for height/weight and hemoglobin(7)";
  attrib HV120_08 format=F00141_. label="Children eligibility for height/weight and hemoglobin(8)";
  attrib HV120_09 format=F00141_. label="Children eligibility for height/weight and hemoglobin(9)";
  attrib HV120_10 format=F00141_. label="Children eligibility for height/weight and hemoglobin(10)";
  attrib HV120_11 format=F00141_. label="Children eligibility for height/weight and hemoglobin(11)";
  attrib HV120_12 format=F00141_. label="Children eligibility for height/weight and hemoglobin(12)";
  attrib HV120_13 format=F00141_. label="Children eligibility for height/weight and hemoglobin(13)";
  attrib HV120_14 format=F00141_. label="Children eligibility for height/weight and hemoglobin(14)";
  attrib HV120_15 format=F00141_. label="Children eligibility for height/weight and hemoglobin(15)";
  attrib HV120_16 format=F00141_. label="Children eligibility for height/weight and hemoglobin(16)";
  attrib HV120_17 format=F00141_. label="Children eligibility for height/weight and hemoglobin(17)";
  attrib HV120_18 format=F00141_. label="Children eligibility for height/weight and hemoglobin(18)";
  attrib HV120_19 format=F00141_. label="Children eligibility for height/weight and hemoglobin(19)";
  attrib HV120_20 format=F00141_. label="Children eligibility for height/weight and hemoglobin(20)";
  attrib HV120_21 format=F00141_. label="Children eligibility for height/weight and hemoglobin(21)";
  attrib HV120_22 format=F00141_. label="Children eligibility for height/weight and hemoglobin(22)";
  attrib HV120_23 format=F00141_. label="Children eligibility for height/weight and hemoglobin(23)";
  attrib HV120_24 format=F00141_. label="Children eligibility for height/weight and hemoglobin(24)";
  attrib HV120_25 format=F00141_. label="Children eligibility for height/weight and hemoglobin(25)";
  attrib HV120_26 format=F00141_. label="Children eligibility for height/weight and hemoglobin(26)";
  attrib HV120_27 format=F00141_. label="Children eligibility for height/weight and hemoglobin(27)";
  attrib HV120_28 format=F00141_. label="Children eligibility for height/weight and hemoglobin(28)";
  attrib HV120_29 format=F00141_. label="Children eligibility for height/weight and hemoglobin(29)";
  attrib HV120_30 format=F00141_. label="Children eligibility for height/weight and hemoglobin(30)";
  attrib HV121_01 format=F00142_. label="Member attended school during current school year(1)";
  attrib HV121_02 format=F00142_. label="Member attended school during current school year(2)";
  attrib HV121_03 format=F00142_. label="Member attended school during current school year(3)";
  attrib HV121_04 format=F00142_. label="Member attended school during current school year(4)";
  attrib HV121_05 format=F00142_. label="Member attended school during current school year(5)";
  attrib HV121_06 format=F00142_. label="Member attended school during current school year(6)";
  attrib HV121_07 format=F00142_. label="Member attended school during current school year(7)";
  attrib HV121_08 format=F00142_. label="Member attended school during current school year(8)";
  attrib HV121_09 format=F00142_. label="Member attended school during current school year(9)";
  attrib HV121_10 format=F00142_. label="Member attended school during current school year(10)";
  attrib HV121_11 format=F00142_. label="Member attended school during current school year(11)";
  attrib HV121_12 format=F00142_. label="Member attended school during current school year(12)";
  attrib HV121_13 format=F00142_. label="Member attended school during current school year(13)";
  attrib HV121_14 format=F00142_. label="Member attended school during current school year(14)";
  attrib HV121_15 format=F00142_. label="Member attended school during current school year(15)";
  attrib HV121_16 format=F00142_. label="Member attended school during current school year(16)";
  attrib HV121_17 format=F00142_. label="Member attended school during current school year(17)";
  attrib HV121_18 format=F00142_. label="Member attended school during current school year(18)";
  attrib HV121_19 format=F00142_. label="Member attended school during current school year(19)";
  attrib HV121_20 format=F00142_. label="Member attended school during current school year(20)";
  attrib HV121_21 format=F00142_. label="Member attended school during current school year(21)";
  attrib HV121_22 format=F00142_. label="Member attended school during current school year(22)";
  attrib HV121_23 format=F00142_. label="Member attended school during current school year(23)";
  attrib HV121_24 format=F00142_. label="Member attended school during current school year(24)";
  attrib HV121_25 format=F00142_. label="Member attended school during current school year(25)";
  attrib HV121_26 format=F00142_. label="Member attended school during current school year(26)";
  attrib HV121_27 format=F00142_. label="Member attended school during current school year(27)";
  attrib HV121_28 format=F00142_. label="Member attended school during current school year(28)";
  attrib HV121_29 format=F00142_. label="Member attended school during current school year(29)";
  attrib HV121_30 format=F00142_. label="Member attended school during current school year(30)";
  attrib HV122_01 format=F00143_. label="Educational level during current school year(1)";
  attrib HV122_02 format=F00143_. label="Educational level during current school year(2)";
  attrib HV122_03 format=F00143_. label="Educational level during current school year(3)";
  attrib HV122_04 format=F00143_. label="Educational level during current school year(4)";
  attrib HV122_05 format=F00143_. label="Educational level during current school year(5)";
  attrib HV122_06 format=F00143_. label="Educational level during current school year(6)";
  attrib HV122_07 format=F00143_. label="Educational level during current school year(7)";
  attrib HV122_08 format=F00143_. label="Educational level during current school year(8)";
  attrib HV122_09 format=F00143_. label="Educational level during current school year(9)";
  attrib HV122_10 format=F00143_. label="Educational level during current school year(10)";
  attrib HV122_11 format=F00143_. label="Educational level during current school year(11)";
  attrib HV122_12 format=F00143_. label="Educational level during current school year(12)";
  attrib HV122_13 format=F00143_. label="Educational level during current school year(13)";
  attrib HV122_14 format=F00143_. label="Educational level during current school year(14)";
  attrib HV122_15 format=F00143_. label="Educational level during current school year(15)";
  attrib HV122_16 format=F00143_. label="Educational level during current school year(16)";
  attrib HV122_17 format=F00143_. label="Educational level during current school year(17)";
  attrib HV122_18 format=F00143_. label="Educational level during current school year(18)";
  attrib HV122_19 format=F00143_. label="Educational level during current school year(19)";
  attrib HV122_20 format=F00143_. label="Educational level during current school year(20)";
  attrib HV122_21 format=F00143_. label="Educational level during current school year(21)";
  attrib HV122_22 format=F00143_. label="Educational level during current school year(22)";
  attrib HV122_23 format=F00143_. label="Educational level during current school year(23)";
  attrib HV122_24 format=F00143_. label="Educational level during current school year(24)";
  attrib HV122_25 format=F00143_. label="Educational level during current school year(25)";
  attrib HV122_26 format=F00143_. label="Educational level during current school year(26)";
  attrib HV122_27 format=F00143_. label="Educational level during current school year(27)";
  attrib HV122_28 format=F00143_. label="Educational level during current school year(28)";
  attrib HV122_29 format=F00143_. label="Educational level during current school year(29)";
  attrib HV122_30 format=F00143_. label="Educational level during current school year(30)";
  attrib HV123_01 format=F00144_. label="Grade of education during current school year(1)";
  attrib HV123_02 format=F00144_. label="Grade of education during current school year(2)";
  attrib HV123_03 format=F00144_. label="Grade of education during current school year(3)";
  attrib HV123_04 format=F00144_. label="Grade of education during current school year(4)";
  attrib HV123_05 format=F00144_. label="Grade of education during current school year(5)";
  attrib HV123_06 format=F00144_. label="Grade of education during current school year(6)";
  attrib HV123_07 format=F00144_. label="Grade of education during current school year(7)";
  attrib HV123_08 format=F00144_. label="Grade of education during current school year(8)";
  attrib HV123_09 format=F00144_. label="Grade of education during current school year(9)";
  attrib HV123_10 format=F00144_. label="Grade of education during current school year(10)";
  attrib HV123_11 format=F00144_. label="Grade of education during current school year(11)";
  attrib HV123_12 format=F00144_. label="Grade of education during current school year(12)";
  attrib HV123_13 format=F00144_. label="Grade of education during current school year(13)";
  attrib HV123_14 format=F00144_. label="Grade of education during current school year(14)";
  attrib HV123_15 format=F00144_. label="Grade of education during current school year(15)";
  attrib HV123_16 format=F00144_. label="Grade of education during current school year(16)";
  attrib HV123_17 format=F00144_. label="Grade of education during current school year(17)";
  attrib HV123_18 format=F00144_. label="Grade of education during current school year(18)";
  attrib HV123_19 format=F00144_. label="Grade of education during current school year(19)";
  attrib HV123_20 format=F00144_. label="Grade of education during current school year(20)";
  attrib HV123_21 format=F00144_. label="Grade of education during current school year(21)";
  attrib HV123_22 format=F00144_. label="Grade of education during current school year(22)";
  attrib HV123_23 format=F00144_. label="Grade of education during current school year(23)";
  attrib HV123_24 format=F00144_. label="Grade of education during current school year(24)";
  attrib HV123_25 format=F00144_. label="Grade of education during current school year(25)";
  attrib HV123_26 format=F00144_. label="Grade of education during current school year(26)";
  attrib HV123_27 format=F00144_. label="Grade of education during current school year(27)";
  attrib HV123_28 format=F00144_. label="Grade of education during current school year(28)";
  attrib HV123_29 format=F00144_. label="Grade of education during current school year(29)";
  attrib HV123_30 format=F00144_. label="Grade of education during current school year(30)";
  attrib HV124_01 format=F00145_. label="Education in single years - current school year(1)";
  attrib HV124_02 format=F00145_. label="Education in single years - current school year(2)";
  attrib HV124_03 format=F00145_. label="Education in single years - current school year(3)";
  attrib HV124_04 format=F00145_. label="Education in single years - current school year(4)";
  attrib HV124_05 format=F00145_. label="Education in single years - current school year(5)";
  attrib HV124_06 format=F00145_. label="Education in single years - current school year(6)";
  attrib HV124_07 format=F00145_. label="Education in single years - current school year(7)";
  attrib HV124_08 format=F00145_. label="Education in single years - current school year(8)";
  attrib HV124_09 format=F00145_. label="Education in single years - current school year(9)";
  attrib HV124_10 format=F00145_. label="Education in single years - current school year(10)";
  attrib HV124_11 format=F00145_. label="Education in single years - current school year(11)";
  attrib HV124_12 format=F00145_. label="Education in single years - current school year(12)";
  attrib HV124_13 format=F00145_. label="Education in single years - current school year(13)";
  attrib HV124_14 format=F00145_. label="Education in single years - current school year(14)";
  attrib HV124_15 format=F00145_. label="Education in single years - current school year(15)";
  attrib HV124_16 format=F00145_. label="Education in single years - current school year(16)";
  attrib HV124_17 format=F00145_. label="Education in single years - current school year(17)";
  attrib HV124_18 format=F00145_. label="Education in single years - current school year(18)";
  attrib HV124_19 format=F00145_. label="Education in single years - current school year(19)";
  attrib HV124_20 format=F00145_. label="Education in single years - current school year(20)";
  attrib HV124_21 format=F00145_. label="Education in single years - current school year(21)";
  attrib HV124_22 format=F00145_. label="Education in single years - current school year(22)";
  attrib HV124_23 format=F00145_. label="Education in single years - current school year(23)";
  attrib HV124_24 format=F00145_. label="Education in single years - current school year(24)";
  attrib HV124_25 format=F00145_. label="Education in single years - current school year(25)";
  attrib HV124_26 format=F00145_. label="Education in single years - current school year(26)";
  attrib HV124_27 format=F00145_. label="Education in single years - current school year(27)";
  attrib HV124_28 format=F00145_. label="Education in single years - current school year(28)";
  attrib HV124_29 format=F00145_. label="Education in single years - current school year(29)";
  attrib HV124_30 format=F00145_. label="Education in single years - current school year(30)";
  attrib HV125_01 format=F00146_. label="NA - Member attended school during previous school year(1)";
  attrib HV125_02 format=F00146_. label="NA - Member attended school during previous school year(2)";
  attrib HV125_03 format=F00146_. label="NA - Member attended school during previous school year(3)";
  attrib HV125_04 format=F00146_. label="NA - Member attended school during previous school year(4)";
  attrib HV125_05 format=F00146_. label="NA - Member attended school during previous school year(5)";
  attrib HV125_06 format=F00146_. label="NA - Member attended school during previous school year(6)";
  attrib HV125_07 format=F00146_. label="NA - Member attended school during previous school year(7)";
  attrib HV125_08 format=F00146_. label="NA - Member attended school during previous school year(8)";
  attrib HV125_09 format=F00146_. label="NA - Member attended school during previous school year(9)";
  attrib HV125_10 format=F00146_. label="NA - Member attended school during previous school year(10)";
  attrib HV125_11 format=F00146_. label="NA - Member attended school during previous school year(11)";
  attrib HV125_12 format=F00146_. label="NA - Member attended school during previous school year(12)";
  attrib HV125_13 format=F00146_. label="NA - Member attended school during previous school year(13)";
  attrib HV125_14 format=F00146_. label="NA - Member attended school during previous school year(14)";
  attrib HV125_15 format=F00146_. label="NA - Member attended school during previous school year(15)";
  attrib HV125_16 format=F00146_. label="NA - Member attended school during previous school year(16)";
  attrib HV125_17 format=F00146_. label="NA - Member attended school during previous school year(17)";
  attrib HV125_18 format=F00146_. label="NA - Member attended school during previous school year(18)";
  attrib HV125_19 format=F00146_. label="NA - Member attended school during previous school year(19)";
  attrib HV125_20 format=F00146_. label="NA - Member attended school during previous school year(20)";
  attrib HV125_21 format=F00146_. label="NA - Member attended school during previous school year(21)";
  attrib HV125_22 format=F00146_. label="NA - Member attended school during previous school year(22)";
  attrib HV125_23 format=F00146_. label="NA - Member attended school during previous school year(23)";
  attrib HV125_24 format=F00146_. label="NA - Member attended school during previous school year(24)";
  attrib HV125_25 format=F00146_. label="NA - Member attended school during previous school year(25)";
  attrib HV125_26 format=F00146_. label="NA - Member attended school during previous school year(26)";
  attrib HV125_27 format=F00146_. label="NA - Member attended school during previous school year(27)";
  attrib HV125_28 format=F00146_. label="NA - Member attended school during previous school year(28)";
  attrib HV125_29 format=F00146_. label="NA - Member attended school during previous school year(29)";
  attrib HV125_30 format=F00146_. label="NA - Member attended school during previous school year(30)";
  attrib HV126_01 format=F00147_. label="NA - Educational level during previous school year(1)";
  attrib HV126_02 format=F00147_. label="NA - Educational level during previous school year(2)";
  attrib HV126_03 format=F00147_. label="NA - Educational level during previous school year(3)";
  attrib HV126_04 format=F00147_. label="NA - Educational level during previous school year(4)";
  attrib HV126_05 format=F00147_. label="NA - Educational level during previous school year(5)";
  attrib HV126_06 format=F00147_. label="NA - Educational level during previous school year(6)";
  attrib HV126_07 format=F00147_. label="NA - Educational level during previous school year(7)";
  attrib HV126_08 format=F00147_. label="NA - Educational level during previous school year(8)";
  attrib HV126_09 format=F00147_. label="NA - Educational level during previous school year(9)";
  attrib HV126_10 format=F00147_. label="NA - Educational level during previous school year(10)";
  attrib HV126_11 format=F00147_. label="NA - Educational level during previous school year(11)";
  attrib HV126_12 format=F00147_. label="NA - Educational level during previous school year(12)";
  attrib HV126_13 format=F00147_. label="NA - Educational level during previous school year(13)";
  attrib HV126_14 format=F00147_. label="NA - Educational level during previous school year(14)";
  attrib HV126_15 format=F00147_. label="NA - Educational level during previous school year(15)";
  attrib HV126_16 format=F00147_. label="NA - Educational level during previous school year(16)";
  attrib HV126_17 format=F00147_. label="NA - Educational level during previous school year(17)";
  attrib HV126_18 format=F00147_. label="NA - Educational level during previous school year(18)";
  attrib HV126_19 format=F00147_. label="NA - Educational level during previous school year(19)";
  attrib HV126_20 format=F00147_. label="NA - Educational level during previous school year(20)";
  attrib HV126_21 format=F00147_. label="NA - Educational level during previous school year(21)";
  attrib HV126_22 format=F00147_. label="NA - Educational level during previous school year(22)";
  attrib HV126_23 format=F00147_. label="NA - Educational level during previous school year(23)";
  attrib HV126_24 format=F00147_. label="NA - Educational level during previous school year(24)";
  attrib HV126_25 format=F00147_. label="NA - Educational level during previous school year(25)";
  attrib HV126_26 format=F00147_. label="NA - Educational level during previous school year(26)";
  attrib HV126_27 format=F00147_. label="NA - Educational level during previous school year(27)";
  attrib HV126_28 format=F00147_. label="NA - Educational level during previous school year(28)";
  attrib HV126_29 format=F00147_. label="NA - Educational level during previous school year(29)";
  attrib HV126_30 format=F00147_. label="NA - Educational level during previous school year(30)";
  attrib HV127_01 format=F00148_. label="NA - Grade of education during previous school year(1)";
  attrib HV127_02 format=F00148_. label="NA - Grade of education during previous school year(2)";
  attrib HV127_03 format=F00148_. label="NA - Grade of education during previous school year(3)";
  attrib HV127_04 format=F00148_. label="NA - Grade of education during previous school year(4)";
  attrib HV127_05 format=F00148_. label="NA - Grade of education during previous school year(5)";
  attrib HV127_06 format=F00148_. label="NA - Grade of education during previous school year(6)";
  attrib HV127_07 format=F00148_. label="NA - Grade of education during previous school year(7)";
  attrib HV127_08 format=F00148_. label="NA - Grade of education during previous school year(8)";
  attrib HV127_09 format=F00148_. label="NA - Grade of education during previous school year(9)";
  attrib HV127_10 format=F00148_. label="NA - Grade of education during previous school year(10)";
  attrib HV127_11 format=F00148_. label="NA - Grade of education during previous school year(11)";
  attrib HV127_12 format=F00148_. label="NA - Grade of education during previous school year(12)";
  attrib HV127_13 format=F00148_. label="NA - Grade of education during previous school year(13)";
  attrib HV127_14 format=F00148_. label="NA - Grade of education during previous school year(14)";
  attrib HV127_15 format=F00148_. label="NA - Grade of education during previous school year(15)";
  attrib HV127_16 format=F00148_. label="NA - Grade of education during previous school year(16)";
  attrib HV127_17 format=F00148_. label="NA - Grade of education during previous school year(17)";
  attrib HV127_18 format=F00148_. label="NA - Grade of education during previous school year(18)";
  attrib HV127_19 format=F00148_. label="NA - Grade of education during previous school year(19)";
  attrib HV127_20 format=F00148_. label="NA - Grade of education during previous school year(20)";
  attrib HV127_21 format=F00148_. label="NA - Grade of education during previous school year(21)";
  attrib HV127_22 format=F00148_. label="NA - Grade of education during previous school year(22)";
  attrib HV127_23 format=F00148_. label="NA - Grade of education during previous school year(23)";
  attrib HV127_24 format=F00148_. label="NA - Grade of education during previous school year(24)";
  attrib HV127_25 format=F00148_. label="NA - Grade of education during previous school year(25)";
  attrib HV127_26 format=F00148_. label="NA - Grade of education during previous school year(26)";
  attrib HV127_27 format=F00148_. label="NA - Grade of education during previous school year(27)";
  attrib HV127_28 format=F00148_. label="NA - Grade of education during previous school year(28)";
  attrib HV127_29 format=F00148_. label="NA - Grade of education during previous school year(29)";
  attrib HV127_30 format=F00148_. label="NA - Grade of education during previous school year(30)";
  attrib HV128_01 format=F00149_. label="NA - Education in single years - previous school year(1)";
  attrib HV128_02 format=F00149_. label="NA - Education in single years - previous school year(2)";
  attrib HV128_03 format=F00149_. label="NA - Education in single years - previous school year(3)";
  attrib HV128_04 format=F00149_. label="NA - Education in single years - previous school year(4)";
  attrib HV128_05 format=F00149_. label="NA - Education in single years - previous school year(5)";
  attrib HV128_06 format=F00149_. label="NA - Education in single years - previous school year(6)";
  attrib HV128_07 format=F00149_. label="NA - Education in single years - previous school year(7)";
  attrib HV128_08 format=F00149_. label="NA - Education in single years - previous school year(8)";
  attrib HV128_09 format=F00149_. label="NA - Education in single years - previous school year(9)";
  attrib HV128_10 format=F00149_. label="NA - Education in single years - previous school year(10)";
  attrib HV128_11 format=F00149_. label="NA - Education in single years - previous school year(11)";
  attrib HV128_12 format=F00149_. label="NA - Education in single years - previous school year(12)";
  attrib HV128_13 format=F00149_. label="NA - Education in single years - previous school year(13)";
  attrib HV128_14 format=F00149_. label="NA - Education in single years - previous school year(14)";
  attrib HV128_15 format=F00149_. label="NA - Education in single years - previous school year(15)";
  attrib HV128_16 format=F00149_. label="NA - Education in single years - previous school year(16)";
  attrib HV128_17 format=F00149_. label="NA - Education in single years - previous school year(17)";
  attrib HV128_18 format=F00149_. label="NA - Education in single years - previous school year(18)";
  attrib HV128_19 format=F00149_. label="NA - Education in single years - previous school year(19)";
  attrib HV128_20 format=F00149_. label="NA - Education in single years - previous school year(20)";
  attrib HV128_21 format=F00149_. label="NA - Education in single years - previous school year(21)";
  attrib HV128_22 format=F00149_. label="NA - Education in single years - previous school year(22)";
  attrib HV128_23 format=F00149_. label="NA - Education in single years - previous school year(23)";
  attrib HV128_24 format=F00149_. label="NA - Education in single years - previous school year(24)";
  attrib HV128_25 format=F00149_. label="NA - Education in single years - previous school year(25)";
  attrib HV128_26 format=F00149_. label="NA - Education in single years - previous school year(26)";
  attrib HV128_27 format=F00149_. label="NA - Education in single years - previous school year(27)";
  attrib HV128_28 format=F00149_. label="NA - Education in single years - previous school year(28)";
  attrib HV128_29 format=F00149_. label="NA - Education in single years - previous school year(29)";
  attrib HV128_30 format=F00149_. label="NA - Education in single years - previous school year(30)";
  attrib HV129_01 format=F00150_. label="NA - School attendance status(1)";
  attrib HV129_02 format=F00150_. label="NA - School attendance status(2)";
  attrib HV129_03 format=F00150_. label="NA - School attendance status(3)";
  attrib HV129_04 format=F00150_. label="NA - School attendance status(4)";
  attrib HV129_05 format=F00150_. label="NA - School attendance status(5)";
  attrib HV129_06 format=F00150_. label="NA - School attendance status(6)";
  attrib HV129_07 format=F00150_. label="NA - School attendance status(7)";
  attrib HV129_08 format=F00150_. label="NA - School attendance status(8)";
  attrib HV129_09 format=F00150_. label="NA - School attendance status(9)";
  attrib HV129_10 format=F00150_. label="NA - School attendance status(10)";
  attrib HV129_11 format=F00150_. label="NA - School attendance status(11)";
  attrib HV129_12 format=F00150_. label="NA - School attendance status(12)";
  attrib HV129_13 format=F00150_. label="NA - School attendance status(13)";
  attrib HV129_14 format=F00150_. label="NA - School attendance status(14)";
  attrib HV129_15 format=F00150_. label="NA - School attendance status(15)";
  attrib HV129_16 format=F00150_. label="NA - School attendance status(16)";
  attrib HV129_17 format=F00150_. label="NA - School attendance status(17)";
  attrib HV129_18 format=F00150_. label="NA - School attendance status(18)";
  attrib HV129_19 format=F00150_. label="NA - School attendance status(19)";
  attrib HV129_20 format=F00150_. label="NA - School attendance status(20)";
  attrib HV129_21 format=F00150_. label="NA - School attendance status(21)";
  attrib HV129_22 format=F00150_. label="NA - School attendance status(22)";
  attrib HV129_23 format=F00150_. label="NA - School attendance status(23)";
  attrib HV129_24 format=F00150_. label="NA - School attendance status(24)";
  attrib HV129_25 format=F00150_. label="NA - School attendance status(25)";
  attrib HV129_26 format=F00150_. label="NA - School attendance status(26)";
  attrib HV129_27 format=F00150_. label="NA - School attendance status(27)";
  attrib HV129_28 format=F00150_. label="NA - School attendance status(28)";
  attrib HV129_29 format=F00150_. label="NA - School attendance status(29)";
  attrib HV129_30 format=F00150_. label="NA - School attendance status(30)";
  attrib HV140_01 format=F00151_. label="Member has a birth certificate(1)";
  attrib HV140_02 format=F00151_. label="Member has a birth certificate(2)";
  attrib HV140_03 format=F00151_. label="Member has a birth certificate(3)";
  attrib HV140_04 format=F00151_. label="Member has a birth certificate(4)";
  attrib HV140_05 format=F00151_. label="Member has a birth certificate(5)";
  attrib HV140_06 format=F00151_. label="Member has a birth certificate(6)";
  attrib HV140_07 format=F00151_. label="Member has a birth certificate(7)";
  attrib HV140_08 format=F00151_. label="Member has a birth certificate(8)";
  attrib HV140_09 format=F00151_. label="Member has a birth certificate(9)";
  attrib HV140_10 format=F00151_. label="Member has a birth certificate(10)";
  attrib HV140_11 format=F00151_. label="Member has a birth certificate(11)";
  attrib HV140_12 format=F00151_. label="Member has a birth certificate(12)";
  attrib HV140_13 format=F00151_. label="Member has a birth certificate(13)";
  attrib HV140_14 format=F00151_. label="Member has a birth certificate(14)";
  attrib HV140_15 format=F00151_. label="Member has a birth certificate(15)";
  attrib HV140_16 format=F00151_. label="Member has a birth certificate(16)";
  attrib HV140_17 format=F00151_. label="Member has a birth certificate(17)";
  attrib HV140_18 format=F00151_. label="Member has a birth certificate(18)";
  attrib HV140_19 format=F00151_. label="Member has a birth certificate(19)";
  attrib HV140_20 format=F00151_. label="Member has a birth certificate(20)";
  attrib HV140_21 format=F00151_. label="Member has a birth certificate(21)";
  attrib HV140_22 format=F00151_. label="Member has a birth certificate(22)";
  attrib HV140_23 format=F00151_. label="Member has a birth certificate(23)";
  attrib HV140_24 format=F00151_. label="Member has a birth certificate(24)";
  attrib HV140_25 format=F00151_. label="Member has a birth certificate(25)";
  attrib HV140_26 format=F00151_. label="Member has a birth certificate(26)";
  attrib HV140_27 format=F00151_. label="Member has a birth certificate(27)";
  attrib HV140_28 format=F00151_. label="Member has a birth certificate(28)";
  attrib HV140_29 format=F00151_. label="Member has a birth certificate(29)";
  attrib HV140_30 format=F00151_. label="Member has a birth certificate(30)";
  attrib IDXH4_01 label="Index to Household Schedule(1)";
  attrib IDXH4_02 label="Index to Household Schedule(2)";
  attrib IDXH4_03 label="Index to Household Schedule(3)";
  attrib IDXH4_04 label="Index to Household Schedule(4)";
  attrib IDXH4_05 label="Index to Household Schedule(5)";
  attrib IDXH4_06 label="Index to Household Schedule(6)";
  attrib IDXH4_07 label="Index to Household Schedule(7)";
  attrib IDXH4_08 label="Index to Household Schedule(8)";
  attrib IDXH4_09 label="Index to Household Schedule(9)";
  attrib IDXH4_10 label="Index to Household Schedule(10)";
  attrib IDXH4_11 label="Index to Household Schedule(11)";
  attrib IDXH4_12 label="Index to Household Schedule(12)";
  attrib IDXH4_13 label="Index to Household Schedule(13)";
  attrib IDXH4_14 label="Index to Household Schedule(14)";
  attrib IDXH4_15 label="Index to Household Schedule(15)";
  attrib IDXH4_16 label="Index to Household Schedule(16)";
  attrib IDXH4_17 label="Index to Household Schedule(17)";
  attrib IDXH4_18 label="Index to Household Schedule(18)";
  attrib IDXH4_19 label="Index to Household Schedule(19)";
  attrib IDXH4_20 label="Index to Household Schedule(20)";
  attrib IDXH4_21 label="Index to Household Schedule(21)";
  attrib IDXH4_22 label="Index to Household Schedule(22)";
  attrib IDXH4_23 label="Index to Household Schedule(23)";
  attrib IDXH4_24 label="Index to Household Schedule(24)";
  attrib IDXH4_25 label="Index to Household Schedule(25)";
  attrib IDXH4_26 label="Index to Household Schedule(26)";
  attrib IDXH4_27 label="Index to Household Schedule(27)";
  attrib IDXH4_28 label="Index to Household Schedule(28)";
  attrib IDXH4_29 label="Index to Household Schedule(29)";
  attrib IDXH4_30 label="Index to Household Schedule(30)";
  attrib SH13_01  format=F00152_. label="Work status(1)";
  attrib SH13_02  format=F00152_. label="Work status(2)";
  attrib SH13_03  format=F00152_. label="Work status(3)";
  attrib SH13_04  format=F00152_. label="Work status(4)";
  attrib SH13_05  format=F00152_. label="Work status(5)";
  attrib SH13_06  format=F00152_. label="Work status(6)";
  attrib SH13_07  format=F00152_. label="Work status(7)";
  attrib SH13_08  format=F00152_. label="Work status(8)";
  attrib SH13_09  format=F00152_. label="Work status(9)";
  attrib SH13_10  format=F00152_. label="Work status(10)";
  attrib SH13_11  format=F00152_. label="Work status(11)";
  attrib SH13_12  format=F00152_. label="Work status(12)";
  attrib SH13_13  format=F00152_. label="Work status(13)";
  attrib SH13_14  format=F00152_. label="Work status(14)";
  attrib SH13_15  format=F00152_. label="Work status(15)";
  attrib SH13_16  format=F00152_. label="Work status(16)";
  attrib SH13_17  format=F00152_. label="Work status(17)";
  attrib SH13_18  format=F00152_. label="Work status(18)";
  attrib SH13_19  format=F00152_. label="Work status(19)";
  attrib SH13_20  format=F00152_. label="Work status(20)";
  attrib SH13_21  format=F00152_. label="Work status(21)";
  attrib SH13_22  format=F00152_. label="Work status(22)";
  attrib SH13_23  format=F00152_. label="Work status(23)";
  attrib SH13_24  format=F00152_. label="Work status(24)";
  attrib SH13_25  format=F00152_. label="Work status(25)";
  attrib SH13_26  format=F00152_. label="Work status(26)";
  attrib SH13_27  format=F00152_. label="Work status(27)";
  attrib SH13_28  format=F00152_. label="Work status(28)";
  attrib SH13_29  format=F00152_. label="Work status(29)";
  attrib SH13_30  format=F00152_. label="Work status(30)";
  attrib SH14A_01 format=F00153_. label="National ID card(1)";
  attrib SH14A_02 format=F00153_. label="National ID card(2)";
  attrib SH14A_03 format=F00153_. label="National ID card(3)";
  attrib SH14A_04 format=F00153_. label="National ID card(4)";
  attrib SH14A_05 format=F00153_. label="National ID card(5)";
  attrib SH14A_06 format=F00153_. label="National ID card(6)";
  attrib SH14A_07 format=F00153_. label="National ID card(7)";
  attrib SH14A_08 format=F00153_. label="National ID card(8)";
  attrib SH14A_09 format=F00153_. label="National ID card(9)";
  attrib SH14A_10 format=F00153_. label="National ID card(10)";
  attrib SH14A_11 format=F00153_. label="National ID card(11)";
  attrib SH14A_12 format=F00153_. label="National ID card(12)";
  attrib SH14A_13 format=F00153_. label="National ID card(13)";
  attrib SH14A_14 format=F00153_. label="National ID card(14)";
  attrib SH14A_15 format=F00153_. label="National ID card(15)";
  attrib SH14A_16 format=F00153_. label="National ID card(16)";
  attrib SH14A_17 format=F00153_. label="National ID card(17)";
  attrib SH14A_18 format=F00153_. label="National ID card(18)";
  attrib SH14A_19 format=F00153_. label="National ID card(19)";
  attrib SH14A_20 format=F00153_. label="National ID card(20)";
  attrib SH14A_21 format=F00153_. label="National ID card(21)";
  attrib SH14A_22 format=F00153_. label="National ID card(22)";
  attrib SH14A_23 format=F00153_. label="National ID card(23)";
  attrib SH14A_24 format=F00153_. label="National ID card(24)";
  attrib SH14A_25 format=F00153_. label="National ID card(25)";
  attrib SH14A_26 format=F00153_. label="National ID card(26)";
  attrib SH14A_27 format=F00153_. label="National ID card(27)";
  attrib SH14A_28 format=F00153_. label="National ID card(28)";
  attrib SH14A_29 format=F00153_. label="National ID card(29)";
  attrib SH14A_30 format=F00153_. label="National ID card(30)";
  attrib SH15_01  format=F00154_. label="Mobile phone(1)";
  attrib SH15_02  format=F00154_. label="Mobile phone(2)";
  attrib SH15_03  format=F00154_. label="Mobile phone(3)";
  attrib SH15_04  format=F00154_. label="Mobile phone(4)";
  attrib SH15_05  format=F00154_. label="Mobile phone(5)";
  attrib SH15_06  format=F00154_. label="Mobile phone(6)";
  attrib SH15_07  format=F00154_. label="Mobile phone(7)";
  attrib SH15_08  format=F00154_. label="Mobile phone(8)";
  attrib SH15_09  format=F00154_. label="Mobile phone(9)";
  attrib SH15_10  format=F00154_. label="Mobile phone(10)";
  attrib SH15_11  format=F00154_. label="Mobile phone(11)";
  attrib SH15_12  format=F00154_. label="Mobile phone(12)";
  attrib SH15_13  format=F00154_. label="Mobile phone(13)";
  attrib SH15_14  format=F00154_. label="Mobile phone(14)";
  attrib SH15_15  format=F00154_. label="Mobile phone(15)";
  attrib SH15_16  format=F00154_. label="Mobile phone(16)";
  attrib SH15_17  format=F00154_. label="Mobile phone(17)";
  attrib SH15_18  format=F00154_. label="Mobile phone(18)";
  attrib SH15_19  format=F00154_. label="Mobile phone(19)";
  attrib SH15_20  format=F00154_. label="Mobile phone(20)";
  attrib SH15_21  format=F00154_. label="Mobile phone(21)";
  attrib SH15_22  format=F00154_. label="Mobile phone(22)";
  attrib SH15_23  format=F00154_. label="Mobile phone(23)";
  attrib SH15_24  format=F00154_. label="Mobile phone(24)";
  attrib SH15_25  format=F00154_. label="Mobile phone(25)";
  attrib SH15_26  format=F00154_. label="Mobile phone(26)";
  attrib SH15_27  format=F00154_. label="Mobile phone(27)";
  attrib SH15_28  format=F00154_. label="Mobile phone(28)";
  attrib SH15_29  format=F00154_. label="Mobile phone(29)";
  attrib SH15_30  format=F00154_. label="Mobile phone(30)";
  attrib SH16_01  format=F00155_. label="Woman selected for biomarkers(1)";
  attrib SH16_02  format=F00155_. label="Woman selected for biomarkers(2)";
  attrib SH16_03  format=F00155_. label="Woman selected for biomarkers(3)";
  attrib SH16_04  format=F00155_. label="Woman selected for biomarkers(4)";
  attrib SH16_05  format=F00155_. label="Woman selected for biomarkers(5)";
  attrib SH16_06  format=F00155_. label="Woman selected for biomarkers(6)";
  attrib SH16_07  format=F00155_. label="Woman selected for biomarkers(7)";
  attrib SH16_08  format=F00155_. label="Woman selected for biomarkers(8)";
  attrib SH16_09  format=F00155_. label="Woman selected for biomarkers(9)";
  attrib SH16_10  format=F00155_. label="Woman selected for biomarkers(10)";
  attrib SH16_11  format=F00155_. label="Woman selected for biomarkers(11)";
  attrib SH16_12  format=F00155_. label="Woman selected for biomarkers(12)";
  attrib SH16_13  format=F00155_. label="Woman selected for biomarkers(13)";
  attrib SH16_14  format=F00155_. label="Woman selected for biomarkers(14)";
  attrib SH16_15  format=F00155_. label="Woman selected for biomarkers(15)";
  attrib SH16_16  format=F00155_. label="Woman selected for biomarkers(16)";
  attrib SH16_17  format=F00155_. label="Woman selected for biomarkers(17)";
  attrib SH16_18  format=F00155_. label="Woman selected for biomarkers(18)";
  attrib SH16_19  format=F00155_. label="Woman selected for biomarkers(19)";
  attrib SH16_20  format=F00155_. label="Woman selected for biomarkers(20)";
  attrib SH16_21  format=F00155_. label="Woman selected for biomarkers(21)";
  attrib SH16_22  format=F00155_. label="Woman selected for biomarkers(22)";
  attrib SH16_23  format=F00155_. label="Woman selected for biomarkers(23)";
  attrib SH16_24  format=F00155_. label="Woman selected for biomarkers(24)";
  attrib SH16_25  format=F00155_. label="Woman selected for biomarkers(25)";
  attrib SH16_26  format=F00155_. label="Woman selected for biomarkers(26)";
  attrib SH16_27  format=F00155_. label="Woman selected for biomarkers(27)";
  attrib SH16_28  format=F00155_. label="Woman selected for biomarkers(28)";
  attrib SH16_29  format=F00155_. label="Woman selected for biomarkers(29)";
  attrib SH16_30  format=F00155_. label="Woman selected for biomarkers(30)";
  attrib SH18_01  format=F00156_. label="Woman selected for bp/glucose(1)";
  attrib SH18_02  format=F00156_. label="Woman selected for bp/glucose(2)";
  attrib SH18_03  format=F00156_. label="Woman selected for bp/glucose(3)";
  attrib SH18_04  format=F00156_. label="Woman selected for bp/glucose(4)";
  attrib SH18_05  format=F00156_. label="Woman selected for bp/glucose(5)";
  attrib SH18_06  format=F00156_. label="Woman selected for bp/glucose(6)";
  attrib SH18_07  format=F00156_. label="Woman selected for bp/glucose(7)";
  attrib SH18_08  format=F00156_. label="Woman selected for bp/glucose(8)";
  attrib SH18_09  format=F00156_. label="Woman selected for bp/glucose(9)";
  attrib SH18_10  format=F00156_. label="Woman selected for bp/glucose(10)";
  attrib SH18_11  format=F00156_. label="Woman selected for bp/glucose(11)";
  attrib SH18_12  format=F00156_. label="Woman selected for bp/glucose(12)";
  attrib SH18_13  format=F00156_. label="Woman selected for bp/glucose(13)";
  attrib SH18_14  format=F00156_. label="Woman selected for bp/glucose(14)";
  attrib SH18_15  format=F00156_. label="Woman selected for bp/glucose(15)";
  attrib SH18_16  format=F00156_. label="Woman selected for bp/glucose(16)";
  attrib SH18_17  format=F00156_. label="Woman selected for bp/glucose(17)";
  attrib SH18_18  format=F00156_. label="Woman selected for bp/glucose(18)";
  attrib SH18_19  format=F00156_. label="Woman selected for bp/glucose(19)";
  attrib SH18_20  format=F00156_. label="Woman selected for bp/glucose(20)";
  attrib SH18_21  format=F00156_. label="Woman selected for bp/glucose(21)";
  attrib SH18_22  format=F00156_. label="Woman selected for bp/glucose(22)";
  attrib SH18_23  format=F00156_. label="Woman selected for bp/glucose(23)";
  attrib SH18_24  format=F00156_. label="Woman selected for bp/glucose(24)";
  attrib SH18_25  format=F00156_. label="Woman selected for bp/glucose(25)";
  attrib SH18_26  format=F00156_. label="Woman selected for bp/glucose(26)";
  attrib SH18_27  format=F00156_. label="Woman selected for bp/glucose(27)";
  attrib SH18_28  format=F00156_. label="Woman selected for bp/glucose(28)";
  attrib SH18_29  format=F00156_. label="Woman selected for bp/glucose(29)";
  attrib SH18_30  format=F00156_. label="Woman selected for bp/glucose(30)";
  attrib SH19_01  format=F00157_. label="Woman selected for w/h/bp/glucose(1)";
  attrib SH19_02  format=F00157_. label="Woman selected for w/h/bp/glucose(2)";
  attrib SH19_03  format=F00157_. label="Woman selected for w/h/bp/glucose(3)";
  attrib SH19_04  format=F00157_. label="Woman selected for w/h/bp/glucose(4)";
  attrib SH19_05  format=F00157_. label="Woman selected for w/h/bp/glucose(5)";
  attrib SH19_06  format=F00157_. label="Woman selected for w/h/bp/glucose(6)";
  attrib SH19_07  format=F00157_. label="Woman selected for w/h/bp/glucose(7)";
  attrib SH19_08  format=F00157_. label="Woman selected for w/h/bp/glucose(8)";
  attrib SH19_09  format=F00157_. label="Woman selected for w/h/bp/glucose(9)";
  attrib SH19_10  format=F00157_. label="Woman selected for w/h/bp/glucose(10)";
  attrib SH19_11  format=F00157_. label="Woman selected for w/h/bp/glucose(11)";
  attrib SH19_12  format=F00157_. label="Woman selected for w/h/bp/glucose(12)";
  attrib SH19_13  format=F00157_. label="Woman selected for w/h/bp/glucose(13)";
  attrib SH19_14  format=F00157_. label="Woman selected for w/h/bp/glucose(14)";
  attrib SH19_15  format=F00157_. label="Woman selected for w/h/bp/glucose(15)";
  attrib SH19_16  format=F00157_. label="Woman selected for w/h/bp/glucose(16)";
  attrib SH19_17  format=F00157_. label="Woman selected for w/h/bp/glucose(17)";
  attrib SH19_18  format=F00157_. label="Woman selected for w/h/bp/glucose(18)";
  attrib SH19_19  format=F00157_. label="Woman selected for w/h/bp/glucose(19)";
  attrib SH19_20  format=F00157_. label="Woman selected for w/h/bp/glucose(20)";
  attrib SH19_21  format=F00157_. label="Woman selected for w/h/bp/glucose(21)";
  attrib SH19_22  format=F00157_. label="Woman selected for w/h/bp/glucose(22)";
  attrib SH19_23  format=F00157_. label="Woman selected for w/h/bp/glucose(23)";
  attrib SH19_24  format=F00157_. label="Woman selected for w/h/bp/glucose(24)";
  attrib SH19_25  format=F00157_. label="Woman selected for w/h/bp/glucose(25)";
  attrib SH19_26  format=F00157_. label="Woman selected for w/h/bp/glucose(26)";
  attrib SH19_27  format=F00157_. label="Woman selected for w/h/bp/glucose(27)";
  attrib SH19_28  format=F00157_. label="Woman selected for w/h/bp/glucose(28)";
  attrib SH19_29  format=F00157_. label="Woman selected for w/h/bp/glucose(29)";
  attrib SH19_30  format=F00157_. label="Woman selected for w/h/bp/glucose(30)";
  attrib SH20_01  format=F00158_. label="Man selected for w/h/bp/glucose(1)";
  attrib SH20_02  format=F00158_. label="Man selected for w/h/bp/glucose(2)";
  attrib SH20_03  format=F00158_. label="Man selected for w/h/bp/glucose(3)";
  attrib SH20_04  format=F00158_. label="Man selected for w/h/bp/glucose(4)";
  attrib SH20_05  format=F00158_. label="Man selected for w/h/bp/glucose(5)";
  attrib SH20_06  format=F00158_. label="Man selected for w/h/bp/glucose(6)";
  attrib SH20_07  format=F00158_. label="Man selected for w/h/bp/glucose(7)";
  attrib SH20_08  format=F00158_. label="Man selected for w/h/bp/glucose(8)";
  attrib SH20_09  format=F00158_. label="Man selected for w/h/bp/glucose(9)";
  attrib SH20_10  format=F00158_. label="Man selected for w/h/bp/glucose(10)";
  attrib SH20_11  format=F00158_. label="Man selected for w/h/bp/glucose(11)";
  attrib SH20_12  format=F00158_. label="Man selected for w/h/bp/glucose(12)";
  attrib SH20_13  format=F00158_. label="Man selected for w/h/bp/glucose(13)";
  attrib SH20_14  format=F00158_. label="Man selected for w/h/bp/glucose(14)";
  attrib SH20_15  format=F00158_. label="Man selected for w/h/bp/glucose(15)";
  attrib SH20_16  format=F00158_. label="Man selected for w/h/bp/glucose(16)";
  attrib SH20_17  format=F00158_. label="Man selected for w/h/bp/glucose(17)";
  attrib SH20_18  format=F00158_. label="Man selected for w/h/bp/glucose(18)";
  attrib SH20_19  format=F00158_. label="Man selected for w/h/bp/glucose(19)";
  attrib SH20_20  format=F00158_. label="Man selected for w/h/bp/glucose(20)";
  attrib SH20_21  format=F00158_. label="Man selected for w/h/bp/glucose(21)";
  attrib SH20_22  format=F00158_. label="Man selected for w/h/bp/glucose(22)";
  attrib SH20_23  format=F00158_. label="Man selected for w/h/bp/glucose(23)";
  attrib SH20_24  format=F00158_. label="Man selected for w/h/bp/glucose(24)";
  attrib SH20_25  format=F00158_. label="Man selected for w/h/bp/glucose(25)";
  attrib SH20_26  format=F00158_. label="Man selected for w/h/bp/glucose(26)";
  attrib SH20_27  format=F00158_. label="Man selected for w/h/bp/glucose(27)";
  attrib SH20_28  format=F00158_. label="Man selected for w/h/bp/glucose(28)";
  attrib SH20_29  format=F00158_. label="Man selected for w/h/bp/glucose(29)";
  attrib SH20_30  format=F00158_. label="Man selected for w/h/bp/glucose(30)";
  attrib HA0_1    label="Index to Household Schedule(1)";
  attrib HA0_2    label="Index to Household Schedule(2)";
  attrib HA0_3    label="Index to Household Schedule(3)";
  attrib HA0_4    label="Index to Household Schedule(4)";
  attrib HA0_5    label="Index to Household Schedule(5)";
  attrib HA0_6    label="Index to Household Schedule(6)";
  attrib HA1_1    label="Woman's age in years(1)";
  attrib HA1_2    label="Woman's age in years(2)";
  attrib HA1_3    label="Woman's age in years(3)";
  attrib HA1_4    label="Woman's age in years(4)";
  attrib HA1_5    label="Woman's age in years(5)";
  attrib HA1_6    label="Woman's age in years(6)";
  attrib HA2_1    format=F00159_. label="Woman's weight in kilograms (1 decimal)(1)";
  attrib HA2_2    format=F00159_. label="Woman's weight in kilograms (1 decimal)(2)";
  attrib HA2_3    format=F00159_. label="Woman's weight in kilograms (1 decimal)(3)";
  attrib HA2_4    format=F00159_. label="Woman's weight in kilograms (1 decimal)(4)";
  attrib HA2_5    format=F00159_. label="Woman's weight in kilograms (1 decimal)(5)";
  attrib HA2_6    format=F00159_. label="Woman's weight in kilograms (1 decimal)(6)";
  attrib HA3_1    format=F00160_. label="Woman's height in centimeters (1 decimal)(1)";
  attrib HA3_2    format=F00160_. label="Woman's height in centimeters (1 decimal)(2)";
  attrib HA3_3    format=F00160_. label="Woman's height in centimeters (1 decimal)(3)";
  attrib HA3_4    format=F00160_. label="Woman's height in centimeters (1 decimal)(4)";
  attrib HA3_5    format=F00160_. label="Woman's height in centimeters (1 decimal)(5)";
  attrib HA3_6    format=F00160_. label="Woman's height in centimeters (1 decimal)(6)";
  attrib HA4_1    format=F00161_. label="Height/Age percentile(1)";
  attrib HA4_2    format=F00161_. label="Height/Age percentile(2)";
  attrib HA4_3    format=F00161_. label="Height/Age percentile(3)";
  attrib HA4_4    format=F00161_. label="Height/Age percentile(4)";
  attrib HA4_5    format=F00161_. label="Height/Age percentile(5)";
  attrib HA4_6    format=F00161_. label="Height/Age percentile(6)";
  attrib HA5_1    format=F00162_. label="Height/Age standard deviation(1)";
  attrib HA5_2    format=F00162_. label="Height/Age standard deviation(2)";
  attrib HA5_3    format=F00162_. label="Height/Age standard deviation(3)";
  attrib HA5_4    format=F00162_. label="Height/Age standard deviation(4)";
  attrib HA5_5    format=F00162_. label="Height/Age standard deviation(5)";
  attrib HA5_6    format=F00162_. label="Height/Age standard deviation(6)";
  attrib HA6_1    format=F00163_. label="Height/Age percent ref. median(1)";
  attrib HA6_2    format=F00163_. label="Height/Age percent ref. median(2)";
  attrib HA6_3    format=F00163_. label="Height/Age percent ref. median(3)";
  attrib HA6_4    format=F00163_. label="Height/Age percent ref. median(4)";
  attrib HA6_5    format=F00163_. label="Height/Age percent ref. median(5)";
  attrib HA6_6    format=F00163_. label="Height/Age percent ref. median(6)";
  attrib HA11_1   format=F00164_. label="Weight/Height standard deviation (DHS)(1)";
  attrib HA11_2   format=F00164_. label="Weight/Height standard deviation (DHS)(2)";
  attrib HA11_3   format=F00164_. label="Weight/Height standard deviation (DHS)(3)";
  attrib HA11_4   format=F00164_. label="Weight/Height standard deviation (DHS)(4)";
  attrib HA11_5   format=F00164_. label="Weight/Height standard deviation (DHS)(5)";
  attrib HA11_6   format=F00164_. label="Weight/Height standard deviation (DHS)(6)";
  attrib HA12_1   format=F00165_. label="Weight/Height percent ref. median (DHS)(1)";
  attrib HA12_2   format=F00165_. label="Weight/Height percent ref. median (DHS)(2)";
  attrib HA12_3   format=F00165_. label="Weight/Height percent ref. median (DHS)(3)";
  attrib HA12_4   format=F00165_. label="Weight/Height percent ref. median (DHS)(4)";
  attrib HA12_5   format=F00165_. label="Weight/Height percent ref. median (DHS)(5)";
  attrib HA12_6   format=F00165_. label="Weight/Height percent ref. median (DHS)(6)";
  attrib HA12A_1  format=F00166_. label="Weight/Height percent ref. median (Fog)(1)";
  attrib HA12A_2  format=F00166_. label="Weight/Height percent ref. median (Fog)(2)";
  attrib HA12A_3  format=F00166_. label="Weight/Height percent ref. median (Fog)(3)";
  attrib HA12A_4  format=F00166_. label="Weight/Height percent ref. median (Fog)(4)";
  attrib HA12A_5  format=F00166_. label="Weight/Height percent ref. median (Fog)(5)";
  attrib HA12A_6  format=F00166_. label="Weight/Height percent ref. median (Fog)(6)";
  attrib HA12B_1  format=F00167_. label="Weight/Height percent ref. median (WHO)(1)";
  attrib HA12B_2  format=F00167_. label="Weight/Height percent ref. median (WHO)(2)";
  attrib HA12B_3  format=F00167_. label="Weight/Height percent ref. median (WHO)(3)";
  attrib HA12B_4  format=F00167_. label="Weight/Height percent ref. median (WHO)(4)";
  attrib HA12B_5  format=F00167_. label="Weight/Height percent ref. median (WHO)(5)";
  attrib HA12B_6  format=F00167_. label="Weight/Height percent ref. median (WHO)(6)";
  attrib HA13_1   format=F00168_. label="Result of measurement - height/weight(1)";
  attrib HA13_2   format=F00168_. label="Result of measurement - height/weight(2)";
  attrib HA13_3   format=F00168_. label="Result of measurement - height/weight(3)";
  attrib HA13_4   format=F00168_. label="Result of measurement - height/weight(4)";
  attrib HA13_5   format=F00168_. label="Result of measurement - height/weight(5)";
  attrib HA13_6   format=F00168_. label="Result of measurement - height/weight(6)";
  attrib HA21_1   label="Fieldworker measurer code(1)";
  attrib HA21_2   label="Fieldworker measurer code(2)";
  attrib HA21_3   label="Fieldworker measurer code(3)";
  attrib HA21_4   label="Fieldworker measurer code(4)";
  attrib HA21_5   label="Fieldworker measurer code(5)";
  attrib HA21_6   label="Fieldworker measurer code(6)";
  attrib HA32_1   label="Date of birth (CMC)(1)";
  attrib HA32_2   label="Date of birth (CMC)(2)";
  attrib HA32_3   label="Date of birth (CMC)(3)";
  attrib HA32_4   label="Date of birth (CMC)(4)";
  attrib HA32_5   label="Date of birth (CMC)(5)";
  attrib HA32_6   label="Date of birth (CMC)(6)";
  attrib HA33_1   format=F00169_. label="Completeness of HA32 information(1)";
  attrib HA33_2   format=F00169_. label="Completeness of HA32 information(2)";
  attrib HA33_3   format=F00169_. label="Completeness of HA32 information(3)";
  attrib HA33_4   format=F00169_. label="Completeness of HA32 information(4)";
  attrib HA33_5   format=F00169_. label="Completeness of HA32 information(5)";
  attrib HA33_6   format=F00169_. label="Completeness of HA32 information(6)";
  attrib HA35_1   format=F00170_. label="NA - Smoking (cigarettes in last 24 hours)(1)";
  attrib HA35_2   format=F00170_. label="NA - Smoking (cigarettes in last 24 hours)(2)";
  attrib HA35_3   format=F00170_. label="NA - Smoking (cigarettes in last 24 hours)(3)";
  attrib HA35_4   format=F00170_. label="NA - Smoking (cigarettes in last 24 hours)(4)";
  attrib HA35_5   format=F00170_. label="NA - Smoking (cigarettes in last 24 hours)(5)";
  attrib HA35_6   format=F00170_. label="NA - Smoking (cigarettes in last 24 hours)(6)";
  attrib HA40_1   format=F00171_. label="Body Mass Index(1)";
  attrib HA40_2   format=F00171_. label="Body Mass Index(2)";
  attrib HA40_3   format=F00171_. label="Body Mass Index(3)";
  attrib HA40_4   format=F00171_. label="Body Mass Index(4)";
  attrib HA40_5   format=F00171_. label="Body Mass Index(5)";
  attrib HA40_6   format=F00171_. label="Body Mass Index(6)";
  attrib HA41_1   format=F00172_. label="Rohrer's index(1)";
  attrib HA41_2   format=F00172_. label="Rohrer's index(2)";
  attrib HA41_3   format=F00172_. label="Rohrer's index(3)";
  attrib HA41_4   format=F00172_. label="Rohrer's index(4)";
  attrib HA41_5   format=F00172_. label="Rohrer's index(5)";
  attrib HA41_6   format=F00172_. label="Rohrer's index(6)";
  attrib HA50_1   format=F00173_. label="NA - Under age 18(1)";
  attrib HA50_2   format=F00173_. label="NA - Under age 18(2)";
  attrib HA50_3   format=F00173_. label="NA - Under age 18(3)";
  attrib HA50_4   format=F00173_. label="NA - Under age 18(4)";
  attrib HA50_5   format=F00173_. label="NA - Under age 18(5)";
  attrib HA50_6   format=F00173_. label="NA - Under age 18(6)";
  attrib HA51_1   format=F00174_. label="NA - Line number of parent/caretaker(1)";
  attrib HA51_2   format=F00174_. label="NA - Line number of parent/caretaker(2)";
  attrib HA51_3   format=F00174_. label="NA - Line number of parent/caretaker(3)";
  attrib HA51_4   format=F00174_. label="NA - Line number of parent/caretaker(4)";
  attrib HA51_5   format=F00174_. label="NA - Line number of parent/caretaker(5)";
  attrib HA51_6   format=F00174_. label="NA - Line number of parent/caretaker(6)";
  attrib HA52_1   format=F00175_. label="NA - Read consent statement - hemoglobin(1)";
  attrib HA52_2   format=F00175_. label="NA - Read consent statement - hemoglobin(2)";
  attrib HA52_3   format=F00175_. label="NA - Read consent statement - hemoglobin(3)";
  attrib HA52_4   format=F00175_. label="NA - Read consent statement - hemoglobin(4)";
  attrib HA52_5   format=F00175_. label="NA - Read consent statement - hemoglobin(5)";
  attrib HA52_6   format=F00175_. label="NA - Read consent statement - hemoglobin(6)";
  attrib HA53_1   format=F00176_. label="NA - Hemoglobin level (g/dl - 1 decimal)(1)";
  attrib HA53_2   format=F00176_. label="NA - Hemoglobin level (g/dl - 1 decimal)(2)";
  attrib HA53_3   format=F00176_. label="NA - Hemoglobin level (g/dl - 1 decimal)(3)";
  attrib HA53_4   format=F00176_. label="NA - Hemoglobin level (g/dl - 1 decimal)(4)";
  attrib HA53_5   format=F00176_. label="NA - Hemoglobin level (g/dl - 1 decimal)(5)";
  attrib HA53_6   format=F00176_. label="NA - Hemoglobin level (g/dl - 1 decimal)(6)";
  attrib HA54_1   format=F00177_. label="Currently pregnant(1)";
  attrib HA54_2   format=F00177_. label="Currently pregnant(2)";
  attrib HA54_3   format=F00177_. label="Currently pregnant(3)";
  attrib HA54_4   format=F00177_. label="Currently pregnant(4)";
  attrib HA54_5   format=F00177_. label="Currently pregnant(5)";
  attrib HA54_6   format=F00177_. label="Currently pregnant(6)";
  attrib HA55_1   format=F00178_. label="NA - Result of measurement - hemoglobin(1)";
  attrib HA55_2   format=F00178_. label="NA - Result of measurement - hemoglobin(2)";
  attrib HA55_3   format=F00178_. label="NA - Result of measurement - hemoglobin(3)";
  attrib HA55_4   format=F00178_. label="NA - Result of measurement - hemoglobin(4)";
  attrib HA55_5   format=F00178_. label="NA - Result of measurement - hemoglobin(5)";
  attrib HA55_6   format=F00178_. label="NA - Result of measurement - hemoglobin(6)";
  attrib HA56_1   format=F00179_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(1)";
  attrib HA56_2   format=F00179_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(2)";
  attrib HA56_3   format=F00179_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(3)";
  attrib HA56_4   format=F00179_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(4)";
  attrib HA56_5   format=F00179_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(5)";
  attrib HA56_6   format=F00179_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(6)";
  attrib HA57_1   format=F00180_. label="NA - Anemia level(1)";
  attrib HA57_2   format=F00180_. label="NA - Anemia level(2)";
  attrib HA57_3   format=F00180_. label="NA - Anemia level(3)";
  attrib HA57_4   format=F00180_. label="NA - Anemia level(4)";
  attrib HA57_5   format=F00180_. label="NA - Anemia level(5)";
  attrib HA57_6   format=F00180_. label="NA - Anemia level(6)";
  attrib HA58_1   format=F00181_. label="NA - Agrees to referral - anemia(1)";
  attrib HA58_2   format=F00181_. label="NA - Agrees to referral - anemia(2)";
  attrib HA58_3   format=F00181_. label="NA - Agrees to referral - anemia(3)";
  attrib HA58_4   format=F00181_. label="NA - Agrees to referral - anemia(4)";
  attrib HA58_5   format=F00181_. label="NA - Agrees to referral - anemia(5)";
  attrib HA58_6   format=F00181_. label="NA - Agrees to referral - anemia(6)";
  attrib HA60_1   format=F00182_. label="Marital status(1)";
  attrib HA60_2   format=F00182_. label="Marital status(2)";
  attrib HA60_3   format=F00182_. label="Marital status(3)";
  attrib HA60_4   format=F00182_. label="Marital status(4)";
  attrib HA60_5   format=F00182_. label="Marital status(5)";
  attrib HA60_6   format=F00182_. label="Marital status(6)";
  attrib HA61_1   format=F00183_. label="NA - Read consent statement - HIV(1)";
  attrib HA61_2   format=F00183_. label="NA - Read consent statement - HIV(2)";
  attrib HA61_3   format=F00183_. label="NA - Read consent statement - HIV(3)";
  attrib HA61_4   format=F00183_. label="NA - Read consent statement - HIV(4)";
  attrib HA61_5   format=F00183_. label="NA - Read consent statement - HIV(5)";
  attrib HA61_6   format=F00183_. label="NA - Read consent statement - HIV(6)";
  attrib HA62_1   length=$5 format=$F00184_. label="Bar code for HIV blood sample(1)";
  attrib HA62_2   length=$5 format=$F00184_. label="Bar code for HIV blood sample(2)";
  attrib HA62_3   length=$5 format=$F00184_. label="Bar code for HIV blood sample(3)";
  attrib HA62_4   length=$5 format=$F00184_. label="Bar code for HIV blood sample(4)";
  attrib HA62_5   length=$5 format=$F00184_. label="Bar code for HIV blood sample(5)";
  attrib HA62_6   length=$5 format=$F00184_. label="Bar code for HIV blood sample(6)";
  attrib HA63_1   format=F00185_. label="NA - Result of measurement - HIV(1)";
  attrib HA63_2   format=F00185_. label="NA - Result of measurement - HIV(2)";
  attrib HA63_3   format=F00185_. label="NA - Result of measurement - HIV(3)";
  attrib HA63_4   format=F00185_. label="NA - Result of measurement - HIV(4)";
  attrib HA63_5   format=F00185_. label="NA - Result of measurement - HIV(5)";
  attrib HA63_6   format=F00185_. label="NA - Result of measurement - HIV(6)";
  attrib HA64_1   format=F00186_. label="NA - Consent for additional tests(1)";
  attrib HA64_2   format=F00186_. label="NA - Consent for additional tests(2)";
  attrib HA64_3   format=F00186_. label="NA - Consent for additional tests(3)";
  attrib HA64_4   format=F00186_. label="NA - Consent for additional tests(4)";
  attrib HA64_5   format=F00186_. label="NA - Consent for additional tests(5)";
  attrib HA64_6   format=F00186_. label="NA - Consent for additional tests(6)";
  attrib HA65_1   format=F00187_. label="Result of woman's individual interview(1)";
  attrib HA65_2   format=F00187_. label="Result of woman's individual interview(2)";
  attrib HA65_3   format=F00187_. label="Result of woman's individual interview(3)";
  attrib HA65_4   format=F00187_. label="Result of woman's individual interview(4)";
  attrib HA65_5   format=F00187_. label="Result of woman's individual interview(5)";
  attrib HA65_6   format=F00187_. label="Result of woman's individual interview(6)";
  attrib HA66_1   format=F00188_. label="Woman's highest educational level(1)";
  attrib HA66_2   format=F00188_. label="Woman's highest educational level(2)";
  attrib HA66_3   format=F00188_. label="Woman's highest educational level(3)";
  attrib HA66_4   format=F00188_. label="Woman's highest educational level(4)";
  attrib HA66_5   format=F00188_. label="Woman's highest educational level(5)";
  attrib HA66_6   format=F00188_. label="Woman's highest educational level(6)";
  attrib HA67_1   format=F00189_. label="Woman's highest year of education(1)";
  attrib HA67_2   format=F00189_. label="Woman's highest year of education(2)";
  attrib HA67_3   format=F00189_. label="Woman's highest year of education(3)";
  attrib HA67_4   format=F00189_. label="Woman's highest year of education(4)";
  attrib HA67_5   format=F00189_. label="Woman's highest year of education(5)";
  attrib HA67_6   format=F00189_. label="Woman's highest year of education(6)";
  attrib HA68_1   format=F00190_. label="Woman's highest educational level (for preliminary and final report)(1)";
  attrib HA68_2   format=F00190_. label="Woman's highest educational level (for preliminary and final report)(2)";
  attrib HA68_3   format=F00190_. label="Woman's highest educational level (for preliminary and final report)(3)";
  attrib HA68_4   format=F00190_. label="Woman's highest educational level (for preliminary and final report)(4)";
  attrib HA68_5   format=F00190_. label="Woman's highest educational level (for preliminary and final report)(5)";
  attrib HA68_6   format=F00190_. label="Woman's highest educational level (for preliminary and final report)(6)";
  attrib HA69_1   label="NA - HIV weight (6 decimals)(1)";
  attrib HA69_2   label="NA - HIV weight (6 decimals)(2)";
  attrib HA69_3   label="NA - HIV weight (6 decimals)(3)";
  attrib HA69_4   label="NA - HIV weight (6 decimals)(4)";
  attrib HA69_5   label="NA - HIV weight (6 decimals)(5)";
  attrib HA69_6   label="NA - HIV weight (6 decimals)(6)";
  attrib HA70_1   label="NA - Interviewer that took blood for HIV testing(1)";
  attrib HA70_2   label="NA - Interviewer that took blood for HIV testing(2)";
  attrib HA70_3   label="NA - Interviewer that took blood for HIV testing(3)";
  attrib HA70_4   label="NA - Interviewer that took blood for HIV testing(4)";
  attrib HA70_5   label="NA - Interviewer that took blood for HIV testing(5)";
  attrib HA70_6   label="NA - Interviewer that took blood for HIV testing(6)";
  attrib HC0_1    label="Index to Household Schedule(1)";
  attrib HC0_2    label="Index to Household Schedule(2)";
  attrib HC0_3    label="Index to Household Schedule(3)";
  attrib HC0_4    label="Index to Household Schedule(4)";
  attrib HC0_5    label="Index to Household Schedule(5)";
  attrib HC0_6    label="Index to Household Schedule(6)";
  attrib HC1_1    label="Child's age in months(1)";
  attrib HC1_2    label="Child's age in months(2)";
  attrib HC1_3    label="Child's age in months(3)";
  attrib HC1_4    label="Child's age in months(4)";
  attrib HC1_5    label="Child's age in months(5)";
  attrib HC1_6    label="Child's age in months(6)";
  attrib HC1A_1   label="Child's age in days(1)";
  attrib HC1A_2   label="Child's age in days(2)";
  attrib HC1A_3   label="Child's age in days(3)";
  attrib HC1A_4   label="Child's age in days(4)";
  attrib HC1A_5   label="Child's age in days(5)";
  attrib HC1A_6   label="Child's age in days(6)";
  attrib HC2_1    format=F00191_. label="Child's weight in kilograms (1 decimal)(1)";
  attrib HC2_2    format=F00191_. label="Child's weight in kilograms (1 decimal)(2)";
  attrib HC2_3    format=F00191_. label="Child's weight in kilograms (1 decimal)(3)";
  attrib HC2_4    format=F00191_. label="Child's weight in kilograms (1 decimal)(4)";
  attrib HC2_5    format=F00191_. label="Child's weight in kilograms (1 decimal)(5)";
  attrib HC2_6    format=F00191_. label="Child's weight in kilograms (1 decimal)(6)";
  attrib HC3_1    format=F00192_. label="Child's height in centimeters (1 decimal)(1)";
  attrib HC3_2    format=F00192_. label="Child's height in centimeters (1 decimal)(2)";
  attrib HC3_3    format=F00192_. label="Child's height in centimeters (1 decimal)(3)";
  attrib HC3_4    format=F00192_. label="Child's height in centimeters (1 decimal)(4)";
  attrib HC3_5    format=F00192_. label="Child's height in centimeters (1 decimal)(5)";
  attrib HC3_6    format=F00192_. label="Child's height in centimeters (1 decimal)(6)";
  attrib HC4_1    format=F00193_. label="Height/Age percentile(1)";
  attrib HC4_2    format=F00193_. label="Height/Age percentile(2)";
  attrib HC4_3    format=F00193_. label="Height/Age percentile(3)";
  attrib HC4_4    format=F00193_. label="Height/Age percentile(4)";
  attrib HC4_5    format=F00193_. label="Height/Age percentile(5)";
  attrib HC4_6    format=F00193_. label="Height/Age percentile(6)";
  attrib HC5_1    format=F00194_. label="Height/Age standard deviation(1)";
  attrib HC5_2    format=F00194_. label="Height/Age standard deviation(2)";
  attrib HC5_3    format=F00194_. label="Height/Age standard deviation(3)";
  attrib HC5_4    format=F00194_. label="Height/Age standard deviation(4)";
  attrib HC5_5    format=F00194_. label="Height/Age standard deviation(5)";
  attrib HC5_6    format=F00194_. label="Height/Age standard deviation(6)";
  attrib HC6_1    format=F00195_. label="Height/Age percent of ref. median(1)";
  attrib HC6_2    format=F00195_. label="Height/Age percent of ref. median(2)";
  attrib HC6_3    format=F00195_. label="Height/Age percent of ref. median(3)";
  attrib HC6_4    format=F00195_. label="Height/Age percent of ref. median(4)";
  attrib HC6_5    format=F00195_. label="Height/Age percent of ref. median(5)";
  attrib HC6_6    format=F00195_. label="Height/Age percent of ref. median(6)";
  attrib HC7_1    format=F00196_. label="Weight/Age percentile(1)";
  attrib HC7_2    format=F00196_. label="Weight/Age percentile(2)";
  attrib HC7_3    format=F00196_. label="Weight/Age percentile(3)";
  attrib HC7_4    format=F00196_. label="Weight/Age percentile(4)";
  attrib HC7_5    format=F00196_. label="Weight/Age percentile(5)";
  attrib HC7_6    format=F00196_. label="Weight/Age percentile(6)";
  attrib HC8_1    format=F00197_. label="Weight/Age standard deviation(1)";
  attrib HC8_2    format=F00197_. label="Weight/Age standard deviation(2)";
  attrib HC8_3    format=F00197_. label="Weight/Age standard deviation(3)";
  attrib HC8_4    format=F00197_. label="Weight/Age standard deviation(4)";
  attrib HC8_5    format=F00197_. label="Weight/Age standard deviation(5)";
  attrib HC8_6    format=F00197_. label="Weight/Age standard deviation(6)";
  attrib HC9_1    format=F00198_. label="Weight/Age percent of ref. median(1)";
  attrib HC9_2    format=F00198_. label="Weight/Age percent of ref. median(2)";
  attrib HC9_3    format=F00198_. label="Weight/Age percent of ref. median(3)";
  attrib HC9_4    format=F00198_. label="Weight/Age percent of ref. median(4)";
  attrib HC9_5    format=F00198_. label="Weight/Age percent of ref. median(5)";
  attrib HC9_6    format=F00198_. label="Weight/Age percent of ref. median(6)";
  attrib HC10_1   format=F00199_. label="Weight/Height percentile(1)";
  attrib HC10_2   format=F00199_. label="Weight/Height percentile(2)";
  attrib HC10_3   format=F00199_. label="Weight/Height percentile(3)";
  attrib HC10_4   format=F00199_. label="Weight/Height percentile(4)";
  attrib HC10_5   format=F00199_. label="Weight/Height percentile(5)";
  attrib HC10_6   format=F00199_. label="Weight/Height percentile(6)";
  attrib HC11_1   format=F00200_. label="Weight/Height standard deviation(1)";
  attrib HC11_2   format=F00200_. label="Weight/Height standard deviation(2)";
  attrib HC11_3   format=F00200_. label="Weight/Height standard deviation(3)";
  attrib HC11_4   format=F00200_. label="Weight/Height standard deviation(4)";
  attrib HC11_5   format=F00200_. label="Weight/Height standard deviation(5)";
  attrib HC11_6   format=F00200_. label="Weight/Height standard deviation(6)";
  attrib HC12_1   format=F00201_. label="Weight/Height percent of ref. median(1)";
  attrib HC12_2   format=F00201_. label="Weight/Height percent of ref. median(2)";
  attrib HC12_3   format=F00201_. label="Weight/Height percent of ref. median(3)";
  attrib HC12_4   format=F00201_. label="Weight/Height percent of ref. median(4)";
  attrib HC12_5   format=F00201_. label="Weight/Height percent of ref. median(5)";
  attrib HC12_6   format=F00201_. label="Weight/Height percent of ref. median(6)";
  attrib HC13_1   format=F00202_. label="Result of measurement - height/weight(1)";
  attrib HC13_2   format=F00202_. label="Result of measurement - height/weight(2)";
  attrib HC13_3   format=F00202_. label="Result of measurement - height/weight(3)";
  attrib HC13_4   format=F00202_. label="Result of measurement - height/weight(4)";
  attrib HC13_5   format=F00202_. label="Result of measurement - height/weight(5)";
  attrib HC13_6   format=F00202_. label="Result of measurement - height/weight(6)";
  attrib HC15_1   format=F00203_. label="Height: lying or standing(1)";
  attrib HC15_2   format=F00203_. label="Height: lying or standing(2)";
  attrib HC15_3   format=F00203_. label="Height: lying or standing(3)";
  attrib HC15_4   format=F00203_. label="Height: lying or standing(4)";
  attrib HC15_5   format=F00203_. label="Height: lying or standing(5)";
  attrib HC15_6   format=F00203_. label="Height: lying or standing(6)";
  attrib HC16_1   format=F00204_. label="Day of birth(1)";
  attrib HC16_2   format=F00204_. label="Day of birth(2)";
  attrib HC16_3   format=F00204_. label="Day of birth(3)";
  attrib HC16_4   format=F00204_. label="Day of birth(4)";
  attrib HC16_5   format=F00204_. label="Day of birth(5)";
  attrib HC16_6   format=F00204_. label="Day of birth(6)";
  attrib HC17_1   label="Date measured (day)(1)";
  attrib HC17_2   label="Date measured (day)(2)";
  attrib HC17_3   label="Date measured (day)(3)";
  attrib HC17_4   label="Date measured (day)(4)";
  attrib HC17_5   label="Date measured (day)(5)";
  attrib HC17_6   label="Date measured (day)(6)";
  attrib HC18_1   label="Date measured (month)(1)";
  attrib HC18_2   label="Date measured (month)(2)";
  attrib HC18_3   label="Date measured (month)(3)";
  attrib HC18_4   label="Date measured (month)(4)";
  attrib HC18_5   label="Date measured (month)(5)";
  attrib HC18_6   label="Date measured (month)(6)";
  attrib HC19_1   label="Date measured (year)(1)";
  attrib HC19_2   label="Date measured (year)(2)";
  attrib HC19_3   label="Date measured (year)(3)";
  attrib HC19_4   label="Date measured (year)(4)";
  attrib HC19_5   label="Date measured (year)(5)";
  attrib HC19_6   label="Date measured (year)(6)";
  attrib HC20_1   label="Century Day Code (CDC) of measurement(1)";
  attrib HC20_2   label="Century Day Code (CDC) of measurement(2)";
  attrib HC20_3   label="Century Day Code (CDC) of measurement(3)";
  attrib HC20_4   label="Century Day Code (CDC) of measurement(4)";
  attrib HC20_5   label="Century Day Code (CDC) of measurement(5)";
  attrib HC20_6   label="Century Day Code (CDC) of measurement(6)";
  attrib HC21_1   label="Fieldworker measurer code(1)";
  attrib HC21_2   label="Fieldworker measurer code(2)";
  attrib HC21_3   label="Fieldworker measurer code(3)";
  attrib HC21_4   label="Fieldworker measurer code(4)";
  attrib HC21_5   label="Fieldworker measurer code(5)";
  attrib HC21_6   label="Fieldworker measurer code(6)";
  attrib HC27_1   format=F00205_. label="Sex(1)";
  attrib HC27_2   format=F00205_. label="Sex(2)";
  attrib HC27_3   format=F00205_. label="Sex(3)";
  attrib HC27_4   format=F00205_. label="Sex(4)";
  attrib HC27_5   format=F00205_. label="Sex(5)";
  attrib HC27_6   format=F00205_. label="Sex(6)";
  attrib HC30_1   format=F00206_. label="Month of birth(1)";
  attrib HC30_2   format=F00206_. label="Month of birth(2)";
  attrib HC30_3   format=F00206_. label="Month of birth(3)";
  attrib HC30_4   format=F00206_. label="Month of birth(4)";
  attrib HC30_5   format=F00206_. label="Month of birth(5)";
  attrib HC30_6   format=F00206_. label="Month of birth(6)";
  attrib HC31_1   format=F00207_. label="Year of birth(1)";
  attrib HC31_2   format=F00207_. label="Year of birth(2)";
  attrib HC31_3   format=F00207_. label="Year of birth(3)";
  attrib HC31_4   format=F00207_. label="Year of birth(4)";
  attrib HC31_5   format=F00207_. label="Year of birth(5)";
  attrib HC31_6   format=F00207_. label="Year of birth(6)";
  attrib HC32_1   label="Date of birth (CMC)(1)";
  attrib HC32_2   label="Date of birth (CMC)(2)";
  attrib HC32_3   label="Date of birth (CMC)(3)";
  attrib HC32_4   label="Date of birth (CMC)(4)";
  attrib HC32_5   label="Date of birth (CMC)(5)";
  attrib HC32_6   label="Date of birth (CMC)(6)";
  attrib HC32A_1  label="Century Day Code (CDC) of birth(1)";
  attrib HC32A_2  label="Century Day Code (CDC) of birth(2)";
  attrib HC32A_3  label="Century Day Code (CDC) of birth(3)";
  attrib HC32A_4  label="Century Day Code (CDC) of birth(4)";
  attrib HC32A_5  label="Century Day Code (CDC) of birth(5)";
  attrib HC32A_6  label="Century Day Code (CDC) of birth(6)";
  attrib HC33_1   format=F00208_. label="Completeness of HC32 information(1)";
  attrib HC33_2   format=F00208_. label="Completeness of HC32 information(2)";
  attrib HC33_3   format=F00208_. label="Completeness of HC32 information(3)";
  attrib HC33_4   format=F00208_. label="Completeness of HC32 information(4)";
  attrib HC33_5   format=F00208_. label="Completeness of HC32 information(5)";
  attrib HC33_6   format=F00208_. label="Completeness of HC32 information(6)";
  attrib HC51_1   format=F00209_. label="NA - Line number of parent/caretaker(1)";
  attrib HC51_2   format=F00209_. label="NA - Line number of parent/caretaker(2)";
  attrib HC51_3   format=F00209_. label="NA - Line number of parent/caretaker(3)";
  attrib HC51_4   format=F00209_. label="NA - Line number of parent/caretaker(4)";
  attrib HC51_5   format=F00209_. label="NA - Line number of parent/caretaker(5)";
  attrib HC51_6   format=F00209_. label="NA - Line number of parent/caretaker(6)";
  attrib HC52_1   format=F00210_. label="NA - Read consent statement - hemoglobin(1)";
  attrib HC52_2   format=F00210_. label="NA - Read consent statement - hemoglobin(2)";
  attrib HC52_3   format=F00210_. label="NA - Read consent statement - hemoglobin(3)";
  attrib HC52_4   format=F00210_. label="NA - Read consent statement - hemoglobin(4)";
  attrib HC52_5   format=F00210_. label="NA - Read consent statement - hemoglobin(5)";
  attrib HC52_6   format=F00210_. label="NA - Read consent statement - hemoglobin(6)";
  attrib HC53_1   format=F00211_. label="NA - Hemoglobin level (g/dl - 1 decimal)(1)";
  attrib HC53_2   format=F00211_. label="NA - Hemoglobin level (g/dl - 1 decimal)(2)";
  attrib HC53_3   format=F00211_. label="NA - Hemoglobin level (g/dl - 1 decimal)(3)";
  attrib HC53_4   format=F00211_. label="NA - Hemoglobin level (g/dl - 1 decimal)(4)";
  attrib HC53_5   format=F00211_. label="NA - Hemoglobin level (g/dl - 1 decimal)(5)";
  attrib HC53_6   format=F00211_. label="NA - Hemoglobin level (g/dl - 1 decimal)(6)";
  attrib HC55_1   format=F00212_. label="NA - Result of measurement - hemoglobin(1)";
  attrib HC55_2   format=F00212_. label="NA - Result of measurement - hemoglobin(2)";
  attrib HC55_3   format=F00212_. label="NA - Result of measurement - hemoglobin(3)";
  attrib HC55_4   format=F00212_. label="NA - Result of measurement - hemoglobin(4)";
  attrib HC55_5   format=F00212_. label="NA - Result of measurement - hemoglobin(5)";
  attrib HC55_6   format=F00212_. label="NA - Result of measurement - hemoglobin(6)";
  attrib HC56_1   format=F00213_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(1)";
  attrib HC56_2   format=F00213_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(2)";
  attrib HC56_3   format=F00213_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(3)";
  attrib HC56_4   format=F00213_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(4)";
  attrib HC56_5   format=F00213_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(5)";
  attrib HC56_6   format=F00213_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(6)";
  attrib HC57_1   format=F00214_. label="NA - Anemia level(1)";
  attrib HC57_2   format=F00214_. label="NA - Anemia level(2)";
  attrib HC57_3   format=F00214_. label="NA - Anemia level(3)";
  attrib HC57_4   format=F00214_. label="NA - Anemia level(4)";
  attrib HC57_5   format=F00214_. label="NA - Anemia level(5)";
  attrib HC57_6   format=F00214_. label="NA - Anemia level(6)";
  attrib HC58_1   format=F00215_. label="NA - Agrees to referral - anemia(1)";
  attrib HC58_2   format=F00215_. label="NA - Agrees to referral - anemia(2)";
  attrib HC58_3   format=F00215_. label="NA - Agrees to referral - anemia(3)";
  attrib HC58_4   format=F00215_. label="NA - Agrees to referral - anemia(4)";
  attrib HC58_5   format=F00215_. label="NA - Agrees to referral - anemia(5)";
  attrib HC58_6   format=F00215_. label="NA - Agrees to referral - anemia(6)";
  attrib HC60_1   format=F00216_. label="Mother's line number (woman's questionnaire)(1)";
  attrib HC60_2   format=F00216_. label="Mother's line number (woman's questionnaire)(2)";
  attrib HC60_3   format=F00216_. label="Mother's line number (woman's questionnaire)(3)";
  attrib HC60_4   format=F00216_. label="Mother's line number (woman's questionnaire)(4)";
  attrib HC60_5   format=F00216_. label="Mother's line number (woman's questionnaire)(5)";
  attrib HC60_6   format=F00216_. label="Mother's line number (woman's questionnaire)(6)";
  attrib HC61_1   format=F00217_. label="Mother's highest educational level(1)";
  attrib HC61_2   format=F00217_. label="Mother's highest educational level(2)";
  attrib HC61_3   format=F00217_. label="Mother's highest educational level(3)";
  attrib HC61_4   format=F00217_. label="Mother's highest educational level(4)";
  attrib HC61_5   format=F00217_. label="Mother's highest educational level(5)";
  attrib HC61_6   format=F00217_. label="Mother's highest educational level(6)";
  attrib HC62_1   format=F00218_. label="Mother's highest year of education(1)";
  attrib HC62_2   format=F00218_. label="Mother's highest year of education(2)";
  attrib HC62_3   format=F00218_. label="Mother's highest year of education(3)";
  attrib HC62_4   format=F00218_. label="Mother's highest year of education(4)";
  attrib HC62_5   format=F00218_. label="Mother's highest year of education(5)";
  attrib HC62_6   format=F00218_. label="Mother's highest year of education(6)";
  attrib HC63_1   label="Preceding birth interval (months)(1)";
  attrib HC63_2   label="Preceding birth interval (months)(2)";
  attrib HC63_3   label="Preceding birth interval (months)(3)";
  attrib HC63_4   label="Preceding birth interval (months)(4)";
  attrib HC63_5   label="Preceding birth interval (months)(5)";
  attrib HC63_6   label="Preceding birth interval (months)(6)";
  attrib HC64_1   label="Birth order number(1)";
  attrib HC64_2   label="Birth order number(2)";
  attrib HC64_3   label="Birth order number(3)";
  attrib HC64_4   label="Birth order number(4)";
  attrib HC64_5   label="Birth order number(5)";
  attrib HC64_6   label="Birth order number(6)";
  attrib HC68_1   format=F00219_. label="Mother's highest educational level (for preliminary and final report)(1)";
  attrib HC68_2   format=F00219_. label="Mother's highest educational level (for preliminary and final report)(2)";
  attrib HC68_3   format=F00219_. label="Mother's highest educational level (for preliminary and final report)(3)";
  attrib HC68_4   format=F00219_. label="Mother's highest educational level (for preliminary and final report)(4)";
  attrib HC68_5   format=F00219_. label="Mother's highest educational level (for preliminary and final report)(5)";
  attrib HC68_6   format=F00219_. label="Mother's highest educational level (for preliminary and final report)(6)";
  attrib HC70_1   format=F00220_. label="Height/Age standard deviation (new WHO)(1)";
  attrib HC70_2   format=F00220_. label="Height/Age standard deviation (new WHO)(2)";
  attrib HC70_3   format=F00220_. label="Height/Age standard deviation (new WHO)(3)";
  attrib HC70_4   format=F00220_. label="Height/Age standard deviation (new WHO)(4)";
  attrib HC70_5   format=F00220_. label="Height/Age standard deviation (new WHO)(5)";
  attrib HC70_6   format=F00220_. label="Height/Age standard deviation (new WHO)(6)";
  attrib HC71_1   format=F00221_. label="Weight/Age standard deviation (new WHO)(1)";
  attrib HC71_2   format=F00221_. label="Weight/Age standard deviation (new WHO)(2)";
  attrib HC71_3   format=F00221_. label="Weight/Age standard deviation (new WHO)(3)";
  attrib HC71_4   format=F00221_. label="Weight/Age standard deviation (new WHO)(4)";
  attrib HC71_5   format=F00221_. label="Weight/Age standard deviation (new WHO)(5)";
  attrib HC71_6   format=F00221_. label="Weight/Age standard deviation (new WHO)(6)";
  attrib HC72_1   format=F00222_. label="Weight/Height standard deviation (new WHO)(1)";
  attrib HC72_2   format=F00222_. label="Weight/Height standard deviation (new WHO)(2)";
  attrib HC72_3   format=F00222_. label="Weight/Height standard deviation (new WHO)(3)";
  attrib HC72_4   format=F00222_. label="Weight/Height standard deviation (new WHO)(4)";
  attrib HC72_5   format=F00222_. label="Weight/Height standard deviation (new WHO)(5)";
  attrib HC72_6   format=F00222_. label="Weight/Height standard deviation (new WHO)(6)";
  attrib HC73_1   format=F00223_. label="BMI standard deviation (new WHO)(1)";
  attrib HC73_2   format=F00223_. label="BMI standard deviation (new WHO)(2)";
  attrib HC73_3   format=F00223_. label="BMI standard deviation (new WHO)(3)";
  attrib HC73_4   format=F00223_. label="BMI standard deviation (new WHO)(4)";
  attrib HC73_5   format=F00223_. label="BMI standard deviation (new WHO)(5)";
  attrib HC73_6   format=F00223_. label="BMI standard deviation (new WHO)(6)";
  attrib SB301_01 label="Index to household schedule(1)";
  attrib SB301_02 label="Index to household schedule(2)";
  attrib SB301_03 label="Index to household schedule(3)";
  attrib SB301_04 label="Index to household schedule(4)";
  attrib SB301_05 label="Index to household schedule(5)";
  attrib SB301_06 label="Index to household schedule(6)";
  attrib SB301_07 label="Index to household schedule(7)";
  attrib SB301_08 label="Index to household schedule(8)";
  attrib SB301_09 label="Index to household schedule(9)";
  attrib SB301_10 label="Index to household schedule(10)";
  attrib SB301_11 label="Index to household schedule(11)";
  attrib SB301_12 label="Index to household schedule(12)";
  attrib SB301_13 label="Index to household schedule(13)";
  attrib SB301_14 label="Index to household schedule(14)";
  attrib SB302_01 format=F00224_. label="Sex of household member(1)";
  attrib SB302_02 format=F00224_. label="Sex of household member(2)";
  attrib SB302_03 format=F00224_. label="Sex of household member(3)";
  attrib SB302_04 format=F00224_. label="Sex of household member(4)";
  attrib SB302_05 format=F00224_. label="Sex of household member(5)";
  attrib SB302_06 format=F00224_. label="Sex of household member(6)";
  attrib SB302_07 format=F00224_. label="Sex of household member(7)";
  attrib SB302_08 format=F00224_. label="Sex of household member(8)";
  attrib SB302_09 format=F00224_. label="Sex of household member(9)";
  attrib SB302_10 format=F00224_. label="Sex of household member(10)";
  attrib SB302_11 format=F00224_. label="Sex of household member(11)";
  attrib SB302_12 format=F00224_. label="Sex of household member(12)";
  attrib SB302_13 format=F00224_. label="Sex of household member(13)";
  attrib SB302_14 format=F00224_. label="Sex of household member(14)";
  attrib SB303_01 format=F00225_. label="Age of respondent(1)";
  attrib SB303_02 format=F00225_. label="Age of respondent(2)";
  attrib SB303_03 format=F00225_. label="Age of respondent(3)";
  attrib SB303_04 format=F00225_. label="Age of respondent(4)";
  attrib SB303_05 format=F00225_. label="Age of respondent(5)";
  attrib SB303_06 format=F00225_. label="Age of respondent(6)";
  attrib SB303_07 format=F00225_. label="Age of respondent(7)";
  attrib SB303_08 format=F00225_. label="Age of respondent(8)";
  attrib SB303_09 format=F00225_. label="Age of respondent(9)";
  attrib SB303_10 format=F00225_. label="Age of respondent(10)";
  attrib SB303_11 format=F00225_. label="Age of respondent(11)";
  attrib SB303_12 format=F00225_. label="Age of respondent(12)";
  attrib SB303_13 format=F00225_. label="Age of respondent(13)";
  attrib SB303_14 format=F00225_. label="Age of respondent(14)";
  attrib SB303C_01 format=F00226_. label="Calculated age of respondent(1)";
  attrib SB303C_02 format=F00226_. label="Calculated age of respondent(2)";
  attrib SB303C_03 format=F00226_. label="Calculated age of respondent(3)";
  attrib SB303C_04 format=F00226_. label="Calculated age of respondent(4)";
  attrib SB303C_05 format=F00226_. label="Calculated age of respondent(5)";
  attrib SB303C_06 format=F00226_. label="Calculated age of respondent(6)";
  attrib SB303C_07 format=F00226_. label="Calculated age of respondent(7)";
  attrib SB303C_08 format=F00226_. label="Calculated age of respondent(8)";
  attrib SB303C_09 format=F00226_. label="Calculated age of respondent(9)";
  attrib SB303C_10 format=F00226_. label="Calculated age of respondent(10)";
  attrib SB303C_11 format=F00226_. label="Calculated age of respondent(11)";
  attrib SB303C_12 format=F00226_. label="Calculated age of respondent(12)";
  attrib SB303C_13 format=F00226_. label="Calculated age of respondent(13)";
  attrib SB303C_14 format=F00226_. label="Calculated age of respondent(14)";
  attrib SB304_01 format=F00227_. label="Marital Status - Women(1)";
  attrib SB304_02 format=F00227_. label="Marital Status - Women(2)";
  attrib SB304_03 format=F00227_. label="Marital Status - Women(3)";
  attrib SB304_04 format=F00227_. label="Marital Status - Women(4)";
  attrib SB304_05 format=F00227_. label="Marital Status - Women(5)";
  attrib SB304_06 format=F00227_. label="Marital Status - Women(6)";
  attrib SB304_07 format=F00227_. label="Marital Status - Women(7)";
  attrib SB304_08 format=F00227_. label="Marital Status - Women(8)";
  attrib SB304_09 format=F00227_. label="Marital Status - Women(9)";
  attrib SB304_10 format=F00227_. label="Marital Status - Women(10)";
  attrib SB304_11 format=F00227_. label="Marital Status - Women(11)";
  attrib SB304_12 format=F00227_. label="Marital Status - Women(12)";
  attrib SB304_13 format=F00227_. label="Marital Status - Women(13)";
  attrib SB304_14 format=F00227_. label="Marital Status - Women(14)";
  attrib SB304A_01 format=F00228_. label="Check age - Women(1)";
  attrib SB304A_02 format=F00228_. label="Check age - Women(2)";
  attrib SB304A_03 format=F00228_. label="Check age - Women(3)";
  attrib SB304A_04 format=F00228_. label="Check age - Women(4)";
  attrib SB304A_05 format=F00228_. label="Check age - Women(5)";
  attrib SB304A_06 format=F00228_. label="Check age - Women(6)";
  attrib SB304A_07 format=F00228_. label="Check age - Women(7)";
  attrib SB304A_08 format=F00228_. label="Check age - Women(8)";
  attrib SB304A_09 format=F00228_. label="Check age - Women(9)";
  attrib SB304A_10 format=F00228_. label="Check age - Women(10)";
  attrib SB304A_11 format=F00228_. label="Check age - Women(11)";
  attrib SB304A_12 format=F00228_. label="Check age - Women(12)";
  attrib SB304A_13 format=F00228_. label="Check age - Women(13)";
  attrib SB304A_14 format=F00228_. label="Check age - Women(14)";
  attrib SB304B_01 format=F00229_. label="Pregnancy status(1)";
  attrib SB304B_02 format=F00229_. label="Pregnancy status(2)";
  attrib SB304B_03 format=F00229_. label="Pregnancy status(3)";
  attrib SB304B_04 format=F00229_. label="Pregnancy status(4)";
  attrib SB304B_05 format=F00229_. label="Pregnancy status(5)";
  attrib SB304B_06 format=F00229_. label="Pregnancy status(6)";
  attrib SB304B_07 format=F00229_. label="Pregnancy status(7)";
  attrib SB304B_08 format=F00229_. label="Pregnancy status(8)";
  attrib SB304B_09 format=F00229_. label="Pregnancy status(9)";
  attrib SB304B_10 format=F00229_. label="Pregnancy status(10)";
  attrib SB304B_11 format=F00229_. label="Pregnancy status(11)";
  attrib SB304B_12 format=F00229_. label="Pregnancy status(12)";
  attrib SB304B_13 format=F00229_. label="Pregnancy status(13)";
  attrib SB304B_14 format=F00229_. label="Pregnancy status(14)";
  attrib SB304C_01 format=F00230_. label="Weight in kilograms(1)";
  attrib SB304C_02 format=F00230_. label="Weight in kilograms(2)";
  attrib SB304C_03 format=F00230_. label="Weight in kilograms(3)";
  attrib SB304C_04 format=F00230_. label="Weight in kilograms(4)";
  attrib SB304C_05 format=F00230_. label="Weight in kilograms(5)";
  attrib SB304C_06 format=F00230_. label="Weight in kilograms(6)";
  attrib SB304C_07 format=F00230_. label="Weight in kilograms(7)";
  attrib SB304C_08 format=F00230_. label="Weight in kilograms(8)";
  attrib SB304C_09 format=F00230_. label="Weight in kilograms(9)";
  attrib SB304C_10 format=F00230_. label="Weight in kilograms(10)";
  attrib SB304C_11 format=F00230_. label="Weight in kilograms(11)";
  attrib SB304C_12 format=F00230_. label="Weight in kilograms(12)";
  attrib SB304C_13 format=F00230_. label="Weight in kilograms(13)";
  attrib SB304C_14 format=F00230_. label="Weight in kilograms(14)";
  attrib SB304D_01 format=F00231_. label="Height in centimeters(1)";
  attrib SB304D_02 format=F00231_. label="Height in centimeters(2)";
  attrib SB304D_03 format=F00231_. label="Height in centimeters(3)";
  attrib SB304D_04 format=F00231_. label="Height in centimeters(4)";
  attrib SB304D_05 format=F00231_. label="Height in centimeters(5)";
  attrib SB304D_06 format=F00231_. label="Height in centimeters(6)";
  attrib SB304D_07 format=F00231_. label="Height in centimeters(7)";
  attrib SB304D_08 format=F00231_. label="Height in centimeters(8)";
  attrib SB304D_09 format=F00231_. label="Height in centimeters(9)";
  attrib SB304D_10 format=F00231_. label="Height in centimeters(10)";
  attrib SB304D_11 format=F00231_. label="Height in centimeters(11)";
  attrib SB304D_12 format=F00231_. label="Height in centimeters(12)";
  attrib SB304D_13 format=F00231_. label="Height in centimeters(13)";
  attrib SB304D_14 format=F00231_. label="Height in centimeters(14)";
  attrib SB304E_01 label="Measurer number(1)";
  attrib SB304E_02 label="Measurer number(2)";
  attrib SB304E_03 label="Measurer number(3)";
  attrib SB304E_04 label="Measurer number(4)";
  attrib SB304E_05 label="Measurer number(5)";
  attrib SB304E_06 label="Measurer number(6)";
  attrib SB304E_07 label="Measurer number(7)";
  attrib SB304E_08 label="Measurer number(8)";
  attrib SB304E_09 label="Measurer number(9)";
  attrib SB304E_10 label="Measurer number(10)";
  attrib SB304E_11 label="Measurer number(11)";
  attrib SB304E_12 label="Measurer number(12)";
  attrib SB304E_13 label="Measurer number(13)";
  attrib SB304E_14 label="Measurer number(14)";
  attrib SB306_01 format=F00232_. label="Highest level of education(1)";
  attrib SB306_02 format=F00232_. label="Highest level of education(2)";
  attrib SB306_03 format=F00232_. label="Highest level of education(3)";
  attrib SB306_04 format=F00232_. label="Highest level of education(4)";
  attrib SB306_05 format=F00232_. label="Highest level of education(5)";
  attrib SB306_06 format=F00232_. label="Highest level of education(6)";
  attrib SB306_07 format=F00232_. label="Highest level of education(7)";
  attrib SB306_08 format=F00232_. label="Highest level of education(8)";
  attrib SB306_09 format=F00232_. label="Highest level of education(9)";
  attrib SB306_10 format=F00232_. label="Highest level of education(10)";
  attrib SB306_11 format=F00232_. label="Highest level of education(11)";
  attrib SB306_12 format=F00232_. label="Highest level of education(12)";
  attrib SB306_13 format=F00232_. label="Highest level of education(13)";
  attrib SB306_14 format=F00232_. label="Highest level of education(14)";
  attrib SB307_01 format=F00233_. label="Currently working(1)";
  attrib SB307_02 format=F00233_. label="Currently working(2)";
  attrib SB307_03 format=F00233_. label="Currently working(3)";
  attrib SB307_04 format=F00233_. label="Currently working(4)";
  attrib SB307_05 format=F00233_. label="Currently working(5)";
  attrib SB307_06 format=F00233_. label="Currently working(6)";
  attrib SB307_07 format=F00233_. label="Currently working(7)";
  attrib SB307_08 format=F00233_. label="Currently working(8)";
  attrib SB307_09 format=F00233_. label="Currently working(9)";
  attrib SB307_10 format=F00233_. label="Currently working(10)";
  attrib SB307_11 format=F00233_. label="Currently working(11)";
  attrib SB307_12 format=F00233_. label="Currently working(12)";
  attrib SB307_13 format=F00233_. label="Currently working(13)";
  attrib SB307_14 format=F00233_. label="Currently working(14)";
  attrib SB308_01 format=F00234_. label="Occupation type(1)";
  attrib SB308_02 format=F00234_. label="Occupation type(2)";
  attrib SB308_03 format=F00234_. label="Occupation type(3)";
  attrib SB308_04 format=F00234_. label="Occupation type(4)";
  attrib SB308_05 format=F00234_. label="Occupation type(5)";
  attrib SB308_06 format=F00234_. label="Occupation type(6)";
  attrib SB308_07 format=F00234_. label="Occupation type(7)";
  attrib SB308_08 format=F00234_. label="Occupation type(8)";
  attrib SB308_09 format=F00234_. label="Occupation type(9)";
  attrib SB308_10 format=F00234_. label="Occupation type(10)";
  attrib SB308_11 format=F00234_. label="Occupation type(11)";
  attrib SB308_12 format=F00234_. label="Occupation type(12)";
  attrib SB308_13 format=F00234_. label="Occupation type(13)";
  attrib SB308_14 format=F00234_. label="Occupation type(14)";
  attrib SB310_01 format=F00235_. label="Consent for blood pressure measurement(1)";
  attrib SB310_02 format=F00235_. label="Consent for blood pressure measurement(2)";
  attrib SB310_03 format=F00235_. label="Consent for blood pressure measurement(3)";
  attrib SB310_04 format=F00235_. label="Consent for blood pressure measurement(4)";
  attrib SB310_05 format=F00235_. label="Consent for blood pressure measurement(5)";
  attrib SB310_06 format=F00235_. label="Consent for blood pressure measurement(6)";
  attrib SB310_07 format=F00235_. label="Consent for blood pressure measurement(7)";
  attrib SB310_08 format=F00235_. label="Consent for blood pressure measurement(8)";
  attrib SB310_09 format=F00235_. label="Consent for blood pressure measurement(9)";
  attrib SB310_10 format=F00235_. label="Consent for blood pressure measurement(10)";
  attrib SB310_11 format=F00235_. label="Consent for blood pressure measurement(11)";
  attrib SB310_12 format=F00235_. label="Consent for blood pressure measurement(12)";
  attrib SB310_13 format=F00235_. label="Consent for blood pressure measurement(13)";
  attrib SB310_14 format=F00235_. label="Consent for blood pressure measurement(14)";
  attrib SB311A_01 format=F00236_. label="Eaten anything(1)";
  attrib SB311A_02 format=F00236_. label="Eaten anything(2)";
  attrib SB311A_03 format=F00236_. label="Eaten anything(3)";
  attrib SB311A_04 format=F00236_. label="Eaten anything(4)";
  attrib SB311A_05 format=F00236_. label="Eaten anything(5)";
  attrib SB311A_06 format=F00236_. label="Eaten anything(6)";
  attrib SB311A_07 format=F00236_. label="Eaten anything(7)";
  attrib SB311A_08 format=F00236_. label="Eaten anything(8)";
  attrib SB311A_09 format=F00236_. label="Eaten anything(9)";
  attrib SB311A_10 format=F00236_. label="Eaten anything(10)";
  attrib SB311A_11 format=F00236_. label="Eaten anything(11)";
  attrib SB311A_12 format=F00236_. label="Eaten anything(12)";
  attrib SB311A_13 format=F00236_. label="Eaten anything(13)";
  attrib SB311A_14 format=F00236_. label="Eaten anything(14)";
  attrib SB311B_01 format=F00237_. label="Had caffeinated drink(1)";
  attrib SB311B_02 format=F00237_. label="Had caffeinated drink(2)";
  attrib SB311B_03 format=F00237_. label="Had caffeinated drink(3)";
  attrib SB311B_04 format=F00237_. label="Had caffeinated drink(4)";
  attrib SB311B_05 format=F00237_. label="Had caffeinated drink(5)";
  attrib SB311B_06 format=F00237_. label="Had caffeinated drink(6)";
  attrib SB311B_07 format=F00237_. label="Had caffeinated drink(7)";
  attrib SB311B_08 format=F00237_. label="Had caffeinated drink(8)";
  attrib SB311B_09 format=F00237_. label="Had caffeinated drink(9)";
  attrib SB311B_10 format=F00237_. label="Had caffeinated drink(10)";
  attrib SB311B_11 format=F00237_. label="Had caffeinated drink(11)";
  attrib SB311B_12 format=F00237_. label="Had caffeinated drink(12)";
  attrib SB311B_13 format=F00237_. label="Had caffeinated drink(13)";
  attrib SB311B_14 format=F00237_. label="Had caffeinated drink(14)";
  attrib SB311C_01 format=F00238_. label="Smoked(1)";
  attrib SB311C_02 format=F00238_. label="Smoked(2)";
  attrib SB311C_03 format=F00238_. label="Smoked(3)";
  attrib SB311C_04 format=F00238_. label="Smoked(4)";
  attrib SB311C_05 format=F00238_. label="Smoked(5)";
  attrib SB311C_06 format=F00238_. label="Smoked(6)";
  attrib SB311C_07 format=F00238_. label="Smoked(7)";
  attrib SB311C_08 format=F00238_. label="Smoked(8)";
  attrib SB311C_09 format=F00238_. label="Smoked(9)";
  attrib SB311C_10 format=F00238_. label="Smoked(10)";
  attrib SB311C_11 format=F00238_. label="Smoked(11)";
  attrib SB311C_12 format=F00238_. label="Smoked(12)";
  attrib SB311C_13 format=F00238_. label="Smoked(13)";
  attrib SB311C_14 format=F00238_. label="Smoked(14)";
  attrib SB312_01 label="Arm circumference(1)";
  attrib SB312_02 label="Arm circumference(2)";
  attrib SB312_03 label="Arm circumference(3)";
  attrib SB312_04 label="Arm circumference(4)";
  attrib SB312_05 label="Arm circumference(5)";
  attrib SB312_06 label="Arm circumference(6)";
  attrib SB312_07 label="Arm circumference(7)";
  attrib SB312_08 label="Arm circumference(8)";
  attrib SB312_09 label="Arm circumference(9)";
  attrib SB312_10 label="Arm circumference(10)";
  attrib SB312_11 label="Arm circumference(11)";
  attrib SB312_12 label="Arm circumference(12)";
  attrib SB312_13 label="Arm circumference(13)";
  attrib SB312_14 label="Arm circumference(14)";
  attrib SB313_01 format=F00239_. label="Arm Circumference measurement for blood pressure(1)";
  attrib SB313_02 format=F00239_. label="Arm Circumference measurement for blood pressure(2)";
  attrib SB313_03 format=F00239_. label="Arm Circumference measurement for blood pressure(3)";
  attrib SB313_04 format=F00239_. label="Arm Circumference measurement for blood pressure(4)";
  attrib SB313_05 format=F00239_. label="Arm Circumference measurement for blood pressure(5)";
  attrib SB313_06 format=F00239_. label="Arm Circumference measurement for blood pressure(6)";
  attrib SB313_07 format=F00239_. label="Arm Circumference measurement for blood pressure(7)";
  attrib SB313_08 format=F00239_. label="Arm Circumference measurement for blood pressure(8)";
  attrib SB313_09 format=F00239_. label="Arm Circumference measurement for blood pressure(9)";
  attrib SB313_10 format=F00239_. label="Arm Circumference measurement for blood pressure(10)";
  attrib SB313_11 format=F00239_. label="Arm Circumference measurement for blood pressure(11)";
  attrib SB313_12 format=F00239_. label="Arm Circumference measurement for blood pressure(12)";
  attrib SB313_13 format=F00239_. label="Arm Circumference measurement for blood pressure(13)";
  attrib SB313_14 format=F00239_. label="Arm Circumference measurement for blood pressure(14)";
  attrib SB314_01 label="Time for first blood pressure test(1)";
  attrib SB314_02 label="Time for first blood pressure test(2)";
  attrib SB314_03 label="Time for first blood pressure test(3)";
  attrib SB314_04 label="Time for first blood pressure test(4)";
  attrib SB314_05 label="Time for first blood pressure test(5)";
  attrib SB314_06 label="Time for first blood pressure test(6)";
  attrib SB314_07 label="Time for first blood pressure test(7)";
  attrib SB314_08 label="Time for first blood pressure test(8)";
  attrib SB314_09 label="Time for first blood pressure test(9)";
  attrib SB314_10 label="Time for first blood pressure test(10)";
  attrib SB314_11 label="Time for first blood pressure test(11)";
  attrib SB314_12 label="Time for first blood pressure test(12)";
  attrib SB314_13 label="Time for first blood pressure test(13)";
  attrib SB314_14 label="Time for first blood pressure test(14)";
  attrib SB314A_01 format=F00240_. label="Permission to take BP(1)";
  attrib SB314A_02 format=F00240_. label="Permission to take BP(2)";
  attrib SB314A_03 format=F00240_. label="Permission to take BP(3)";
  attrib SB314A_04 format=F00240_. label="Permission to take BP(4)";
  attrib SB314A_05 format=F00240_. label="Permission to take BP(5)";
  attrib SB314A_06 format=F00240_. label="Permission to take BP(6)";
  attrib SB314A_07 format=F00240_. label="Permission to take BP(7)";
  attrib SB314A_08 format=F00240_. label="Permission to take BP(8)";
  attrib SB314A_09 format=F00240_. label="Permission to take BP(9)";
  attrib SB314A_10 format=F00240_. label="Permission to take BP(10)";
  attrib SB314A_11 format=F00240_. label="Permission to take BP(11)";
  attrib SB314A_12 format=F00240_. label="Permission to take BP(12)";
  attrib SB314A_13 format=F00240_. label="Permission to take BP(13)";
  attrib SB314A_14 format=F00240_. label="Permission to take BP(14)";
  attrib SB315A_01 format=F00241_. label="Systolic blood pressure measurement(1)";
  attrib SB315A_02 format=F00241_. label="Systolic blood pressure measurement(2)";
  attrib SB315A_03 format=F00241_. label="Systolic blood pressure measurement(3)";
  attrib SB315A_04 format=F00241_. label="Systolic blood pressure measurement(4)";
  attrib SB315A_05 format=F00241_. label="Systolic blood pressure measurement(5)";
  attrib SB315A_06 format=F00241_. label="Systolic blood pressure measurement(6)";
  attrib SB315A_07 format=F00241_. label="Systolic blood pressure measurement(7)";
  attrib SB315A_08 format=F00241_. label="Systolic blood pressure measurement(8)";
  attrib SB315A_09 format=F00241_. label="Systolic blood pressure measurement(9)";
  attrib SB315A_10 format=F00241_. label="Systolic blood pressure measurement(10)";
  attrib SB315A_11 format=F00241_. label="Systolic blood pressure measurement(11)";
  attrib SB315A_12 format=F00241_. label="Systolic blood pressure measurement(12)";
  attrib SB315A_13 format=F00241_. label="Systolic blood pressure measurement(13)";
  attrib SB315A_14 format=F00241_. label="Systolic blood pressure measurement(14)";
  attrib SB315B_01 format=F00242_. label="Diastolic blood pressure measurement(1)";
  attrib SB315B_02 format=F00242_. label="Diastolic blood pressure measurement(2)";
  attrib SB315B_03 format=F00242_. label="Diastolic blood pressure measurement(3)";
  attrib SB315B_04 format=F00242_. label="Diastolic blood pressure measurement(4)";
  attrib SB315B_05 format=F00242_. label="Diastolic blood pressure measurement(5)";
  attrib SB315B_06 format=F00242_. label="Diastolic blood pressure measurement(6)";
  attrib SB315B_07 format=F00242_. label="Diastolic blood pressure measurement(7)";
  attrib SB315B_08 format=F00242_. label="Diastolic blood pressure measurement(8)";
  attrib SB315B_09 format=F00242_. label="Diastolic blood pressure measurement(9)";
  attrib SB315B_10 format=F00242_. label="Diastolic blood pressure measurement(10)";
  attrib SB315B_11 format=F00242_. label="Diastolic blood pressure measurement(11)";
  attrib SB315B_12 format=F00242_. label="Diastolic blood pressure measurement(12)";
  attrib SB315B_13 format=F00242_. label="Diastolic blood pressure measurement(13)";
  attrib SB315B_14 format=F00242_. label="Diastolic blood pressure measurement(14)";
  attrib SB316_01 format=F00243_. label="Blood pressure ever checked(1)";
  attrib SB316_02 format=F00243_. label="Blood pressure ever checked(2)";
  attrib SB316_03 format=F00243_. label="Blood pressure ever checked(3)";
  attrib SB316_04 format=F00243_. label="Blood pressure ever checked(4)";
  attrib SB316_05 format=F00243_. label="Blood pressure ever checked(5)";
  attrib SB316_06 format=F00243_. label="Blood pressure ever checked(6)";
  attrib SB316_07 format=F00243_. label="Blood pressure ever checked(7)";
  attrib SB316_08 format=F00243_. label="Blood pressure ever checked(8)";
  attrib SB316_09 format=F00243_. label="Blood pressure ever checked(9)";
  attrib SB316_10 format=F00243_. label="Blood pressure ever checked(10)";
  attrib SB316_11 format=F00243_. label="Blood pressure ever checked(11)";
  attrib SB316_12 format=F00243_. label="Blood pressure ever checked(12)";
  attrib SB316_13 format=F00243_. label="Blood pressure ever checked(13)";
  attrib SB316_14 format=F00243_. label="Blood pressure ever checked(14)";
  attrib SB317_01 format=F00244_. label="Last time for BP measure(1)";
  attrib SB317_02 format=F00244_. label="Last time for BP measure(2)";
  attrib SB317_03 format=F00244_. label="Last time for BP measure(3)";
  attrib SB317_04 format=F00244_. label="Last time for BP measure(4)";
  attrib SB317_05 format=F00244_. label="Last time for BP measure(5)";
  attrib SB317_06 format=F00244_. label="Last time for BP measure(6)";
  attrib SB317_07 format=F00244_. label="Last time for BP measure(7)";
  attrib SB317_08 format=F00244_. label="Last time for BP measure(8)";
  attrib SB317_09 format=F00244_. label="Last time for BP measure(9)";
  attrib SB317_10 format=F00244_. label="Last time for BP measure(10)";
  attrib SB317_11 format=F00244_. label="Last time for BP measure(11)";
  attrib SB317_12 format=F00244_. label="Last time for BP measure(12)";
  attrib SB317_13 format=F00244_. label="Last time for BP measure(13)";
  attrib SB317_14 format=F00244_. label="Last time for BP measure(14)";
  attrib SB317A_01 format=F00245_. label="Place for BP measurement(1)";
  attrib SB317A_02 format=F00245_. label="Place for BP measurement(2)";
  attrib SB317A_03 format=F00245_. label="Place for BP measurement(3)";
  attrib SB317A_04 format=F00245_. label="Place for BP measurement(4)";
  attrib SB317A_05 format=F00245_. label="Place for BP measurement(5)";
  attrib SB317A_06 format=F00245_. label="Place for BP measurement(6)";
  attrib SB317A_07 format=F00245_. label="Place for BP measurement(7)";
  attrib SB317A_08 format=F00245_. label="Place for BP measurement(8)";
  attrib SB317A_09 format=F00245_. label="Place for BP measurement(9)";
  attrib SB317A_10 format=F00245_. label="Place for BP measurement(10)";
  attrib SB317A_11 format=F00245_. label="Place for BP measurement(11)";
  attrib SB317A_12 format=F00245_. label="Place for BP measurement(12)";
  attrib SB317A_13 format=F00245_. label="Place for BP measurement(13)";
  attrib SB317A_14 format=F00245_. label="Place for BP measurement(14)";
  attrib SB317B_01 format=F00246_. label="Told by a doctor to have high blood pressure(1)";
  attrib SB317B_02 format=F00246_. label="Told by a doctor to have high blood pressure(2)";
  attrib SB317B_03 format=F00246_. label="Told by a doctor to have high blood pressure(3)";
  attrib SB317B_04 format=F00246_. label="Told by a doctor to have high blood pressure(4)";
  attrib SB317B_05 format=F00246_. label="Told by a doctor to have high blood pressure(5)";
  attrib SB317B_06 format=F00246_. label="Told by a doctor to have high blood pressure(6)";
  attrib SB317B_07 format=F00246_. label="Told by a doctor to have high blood pressure(7)";
  attrib SB317B_08 format=F00246_. label="Told by a doctor to have high blood pressure(8)";
  attrib SB317B_09 format=F00246_. label="Told by a doctor to have high blood pressure(9)";
  attrib SB317B_10 format=F00246_. label="Told by a doctor to have high blood pressure(10)";
  attrib SB317B_11 format=F00246_. label="Told by a doctor to have high blood pressure(11)";
  attrib SB317B_12 format=F00246_. label="Told by a doctor to have high blood pressure(12)";
  attrib SB317B_13 format=F00246_. label="Told by a doctor to have high blood pressure(13)";
  attrib SB317B_14 format=F00246_. label="Told by a doctor to have high blood pressure(14)";
  attrib SB317CA_01 format=F00247_. label="Who told they have high BP-qualified doctor(1)";
  attrib SB317CA_02 format=F00247_. label="Who told they have high BP-qualified doctor(2)";
  attrib SB317CA_03 format=F00247_. label="Who told they have high BP-qualified doctor(3)";
  attrib SB317CA_04 format=F00247_. label="Who told they have high BP-qualified doctor(4)";
  attrib SB317CA_05 format=F00247_. label="Who told they have high BP-qualified doctor(5)";
  attrib SB317CA_06 format=F00247_. label="Who told they have high BP-qualified doctor(6)";
  attrib SB317CA_07 format=F00247_. label="Who told they have high BP-qualified doctor(7)";
  attrib SB317CA_08 format=F00247_. label="Who told they have high BP-qualified doctor(8)";
  attrib SB317CA_09 format=F00247_. label="Who told they have high BP-qualified doctor(9)";
  attrib SB317CA_10 format=F00247_. label="Who told they have high BP-qualified doctor(10)";
  attrib SB317CA_11 format=F00247_. label="Who told they have high BP-qualified doctor(11)";
  attrib SB317CA_12 format=F00247_. label="Who told they have high BP-qualified doctor(12)";
  attrib SB317CA_13 format=F00247_. label="Who told they have high BP-qualified doctor(13)";
  attrib SB317CA_14 format=F00247_. label="Who told they have high BP-qualified doctor(14)";
  attrib SB317CB_01 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(1)";
  attrib SB317CB_02 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(2)";
  attrib SB317CB_03 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(3)";
  attrib SB317CB_04 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(4)";
  attrib SB317CB_05 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(5)";
  attrib SB317CB_06 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(6)";
  attrib SB317CB_07 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(7)";
  attrib SB317CB_08 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(8)";
  attrib SB317CB_09 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(9)";
  attrib SB317CB_10 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(10)";
  attrib SB317CB_11 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(11)";
  attrib SB317CB_12 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(12)";
  attrib SB317CB_13 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(13)";
  attrib SB317CB_14 format=F00248_. label="Who told they have high BP-nurse/midwife/paramedic(14)";
  attrib SB317CC_01 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(1)";
  attrib SB317CC_02 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(2)";
  attrib SB317CC_03 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(3)";
  attrib SB317CC_04 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(4)";
  attrib SB317CC_05 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(5)";
  attrib SB317CC_06 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(6)";
  attrib SB317CC_07 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(7)";
  attrib SB317CC_08 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(8)";
  attrib SB317CC_09 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(9)";
  attrib SB317CC_10 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(10)";
  attrib SB317CC_11 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(11)";
  attrib SB317CC_12 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(12)";
  attrib SB317CC_13 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(13)";
  attrib SB317CC_14 format=F00249_. label="Who told they have high BP-family welfare visitor (FWV)(14)";
  attrib SB317CD_01 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(1)";
  attrib SB317CD_02 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(2)";
  attrib SB317CD_03 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(3)";
  attrib SB317CD_04 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(4)";
  attrib SB317CD_05 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(5)";
  attrib SB317CD_06 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(6)";
  attrib SB317CD_07 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(7)";
  attrib SB317CD_08 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(8)";
  attrib SB317CD_09 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(9)";
  attrib SB317CD_10 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(10)";
  attrib SB317CD_11 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(11)";
  attrib SB317CD_12 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(12)";
  attrib SB317CD_13 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(13)";
  attrib SB317CD_14 format=F00250_. label="Who told they have high BP-community skilled birth attendant (CSBA)(14)";
  attrib SB317CE_01 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(1)";
  attrib SB317CE_02 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(2)";
  attrib SB317CE_03 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(3)";
  attrib SB317CE_04 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(4)";
  attrib SB317CE_05 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(5)";
  attrib SB317CE_06 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(6)";
  attrib SB317CE_07 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(7)";
  attrib SB317CE_08 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(8)";
  attrib SB317CE_09 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(9)";
  attrib SB317CE_10 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(10)";
  attrib SB317CE_11 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(11)";
  attrib SB317CE_12 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(12)";
  attrib SB317CE_13 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(13)";
  attrib SB317CE_14 format=F00251_. label="Who told they have high BP-sub-assistant community medical officer (SACMO)(14)";
  attrib SB317CF_01 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(1)";
  attrib SB317CF_02 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(2)";
  attrib SB317CF_03 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(3)";
  attrib SB317CF_04 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(4)";
  attrib SB317CF_05 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(5)";
  attrib SB317CF_06 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(6)";
  attrib SB317CF_07 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(7)";
  attrib SB317CF_08 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(8)";
  attrib SB317CF_09 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(9)";
  attrib SB317CF_10 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(10)";
  attrib SB317CF_11 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(11)";
  attrib SB317CF_12 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(12)";
  attrib SB317CF_13 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(13)";
  attrib SB317CF_14 format=F00252_. label="Who told they have high BP-community health care provider (CHCP)(14)";
  attrib SB317CG_01 format=F00253_. label="Who told they have high BP-health assistant (HA)(1)";
  attrib SB317CG_02 format=F00253_. label="Who told they have high BP-health assistant (HA)(2)";
  attrib SB317CG_03 format=F00253_. label="Who told they have high BP-health assistant (HA)(3)";
  attrib SB317CG_04 format=F00253_. label="Who told they have high BP-health assistant (HA)(4)";
  attrib SB317CG_05 format=F00253_. label="Who told they have high BP-health assistant (HA)(5)";
  attrib SB317CG_06 format=F00253_. label="Who told they have high BP-health assistant (HA)(6)";
  attrib SB317CG_07 format=F00253_. label="Who told they have high BP-health assistant (HA)(7)";
  attrib SB317CG_08 format=F00253_. label="Who told they have high BP-health assistant (HA)(8)";
  attrib SB317CG_09 format=F00253_. label="Who told they have high BP-health assistant (HA)(9)";
  attrib SB317CG_10 format=F00253_. label="Who told they have high BP-health assistant (HA)(10)";
  attrib SB317CG_11 format=F00253_. label="Who told they have high BP-health assistant (HA)(11)";
  attrib SB317CG_12 format=F00253_. label="Who told they have high BP-health assistant (HA)(12)";
  attrib SB317CG_13 format=F00253_. label="Who told they have high BP-health assistant (HA)(13)";
  attrib SB317CG_14 format=F00253_. label="Who told they have high BP-health assistant (HA)(14)";
  attrib SB317CH_01 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(1)";
  attrib SB317CH_02 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(2)";
  attrib SB317CH_03 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(3)";
  attrib SB317CH_04 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(4)";
  attrib SB317CH_05 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(5)";
  attrib SB317CH_06 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(6)";
  attrib SB317CH_07 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(7)";
  attrib SB317CH_08 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(8)";
  attrib SB317CH_09 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(9)";
  attrib SB317CH_10 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(10)";
  attrib SB317CH_11 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(11)";
  attrib SB317CH_12 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(12)";
  attrib SB317CH_13 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(13)";
  attrib SB317CH_14 format=F00254_. label="Who told they have high BP-family welfare assistant (FWA)(14)";
  attrib SB317CI_01 format=F00255_. label="Who told they have high BP-NGO worker(1)";
  attrib SB317CI_02 format=F00255_. label="Who told they have high BP-NGO worker(2)";
  attrib SB317CI_03 format=F00255_. label="Who told they have high BP-NGO worker(3)";
  attrib SB317CI_04 format=F00255_. label="Who told they have high BP-NGO worker(4)";
  attrib SB317CI_05 format=F00255_. label="Who told they have high BP-NGO worker(5)";
  attrib SB317CI_06 format=F00255_. label="Who told they have high BP-NGO worker(6)";
  attrib SB317CI_07 format=F00255_. label="Who told they have high BP-NGO worker(7)";
  attrib SB317CI_08 format=F00255_. label="Who told they have high BP-NGO worker(8)";
  attrib SB317CI_09 format=F00255_. label="Who told they have high BP-NGO worker(9)";
  attrib SB317CI_10 format=F00255_. label="Who told they have high BP-NGO worker(10)";
  attrib SB317CI_11 format=F00255_. label="Who told they have high BP-NGO worker(11)";
  attrib SB317CI_12 format=F00255_. label="Who told they have high BP-NGO worker(12)";
  attrib SB317CI_13 format=F00255_. label="Who told they have high BP-NGO worker(13)";
  attrib SB317CI_14 format=F00255_. label="Who told they have high BP-NGO worker(14)";
  attrib SB317CJ_01 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(1)";
  attrib SB317CJ_02 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(2)";
  attrib SB317CJ_03 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(3)";
  attrib SB317CJ_04 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(4)";
  attrib SB317CJ_05 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(5)";
  attrib SB317CJ_06 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(6)";
  attrib SB317CJ_07 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(7)";
  attrib SB317CJ_08 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(8)";
  attrib SB317CJ_09 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(9)";
  attrib SB317CJ_10 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(10)";
  attrib SB317CJ_11 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(11)";
  attrib SB317CJ_12 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(12)";
  attrib SB317CJ_13 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(13)";
  attrib SB317CJ_14 format=F00256_. label="Who told they have high BP-trained TBA (TTBA)(14)";
  attrib SB317CK_01 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(1)";
  attrib SB317CK_02 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(2)";
  attrib SB317CK_03 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(3)";
  attrib SB317CK_04 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(4)";
  attrib SB317CK_05 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(5)";
  attrib SB317CK_06 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(6)";
  attrib SB317CK_07 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(7)";
  attrib SB317CK_08 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(8)";
  attrib SB317CK_09 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(9)";
  attrib SB317CK_10 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(10)";
  attrib SB317CK_11 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(11)";
  attrib SB317CK_12 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(12)";
  attrib SB317CK_13 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(13)";
  attrib SB317CK_14 format=F00257_. label="Who told they have high BP-untrained TBA (UTBA)(14)";
  attrib SB317CL_01 format=F00258_. label="Who told they have high BP-unqualified doctor(1)";
  attrib SB317CL_02 format=F00258_. label="Who told they have high BP-unqualified doctor(2)";
  attrib SB317CL_03 format=F00258_. label="Who told they have high BP-unqualified doctor(3)";
  attrib SB317CL_04 format=F00258_. label="Who told they have high BP-unqualified doctor(4)";
  attrib SB317CL_05 format=F00258_. label="Who told they have high BP-unqualified doctor(5)";
  attrib SB317CL_06 format=F00258_. label="Who told they have high BP-unqualified doctor(6)";
  attrib SB317CL_07 format=F00258_. label="Who told they have high BP-unqualified doctor(7)";
  attrib SB317CL_08 format=F00258_. label="Who told they have high BP-unqualified doctor(8)";
  attrib SB317CL_09 format=F00258_. label="Who told they have high BP-unqualified doctor(9)";
  attrib SB317CL_10 format=F00258_. label="Who told they have high BP-unqualified doctor(10)";
  attrib SB317CL_11 format=F00258_. label="Who told they have high BP-unqualified doctor(11)";
  attrib SB317CL_12 format=F00258_. label="Who told they have high BP-unqualified doctor(12)";
  attrib SB317CL_13 format=F00258_. label="Who told they have high BP-unqualified doctor(13)";
  attrib SB317CL_14 format=F00258_. label="Who told they have high BP-unqualified doctor(14)";
  attrib SB317CM_01 format=F00259_. label="Who told they have high BP-relatives(1)";
  attrib SB317CM_02 format=F00259_. label="Who told they have high BP-relatives(2)";
  attrib SB317CM_03 format=F00259_. label="Who told they have high BP-relatives(3)";
  attrib SB317CM_04 format=F00259_. label="Who told they have high BP-relatives(4)";
  attrib SB317CM_05 format=F00259_. label="Who told they have high BP-relatives(5)";
  attrib SB317CM_06 format=F00259_. label="Who told they have high BP-relatives(6)";
  attrib SB317CM_07 format=F00259_. label="Who told they have high BP-relatives(7)";
  attrib SB317CM_08 format=F00259_. label="Who told they have high BP-relatives(8)";
  attrib SB317CM_09 format=F00259_. label="Who told they have high BP-relatives(9)";
  attrib SB317CM_10 format=F00259_. label="Who told they have high BP-relatives(10)";
  attrib SB317CM_11 format=F00259_. label="Who told they have high BP-relatives(11)";
  attrib SB317CM_12 format=F00259_. label="Who told they have high BP-relatives(12)";
  attrib SB317CM_13 format=F00259_. label="Who told they have high BP-relatives(13)";
  attrib SB317CM_14 format=F00259_. label="Who told they have high BP-relatives(14)";
  attrib SB317CN_01 format=F00260_. label="Who told they have high BP-neighbors/friends(1)";
  attrib SB317CN_02 format=F00260_. label="Who told they have high BP-neighbors/friends(2)";
  attrib SB317CN_03 format=F00260_. label="Who told they have high BP-neighbors/friends(3)";
  attrib SB317CN_04 format=F00260_. label="Who told they have high BP-neighbors/friends(4)";
  attrib SB317CN_05 format=F00260_. label="Who told they have high BP-neighbors/friends(5)";
  attrib SB317CN_06 format=F00260_. label="Who told they have high BP-neighbors/friends(6)";
  attrib SB317CN_07 format=F00260_. label="Who told they have high BP-neighbors/friends(7)";
  attrib SB317CN_08 format=F00260_. label="Who told they have high BP-neighbors/friends(8)";
  attrib SB317CN_09 format=F00260_. label="Who told they have high BP-neighbors/friends(9)";
  attrib SB317CN_10 format=F00260_. label="Who told they have high BP-neighbors/friends(10)";
  attrib SB317CN_11 format=F00260_. label="Who told they have high BP-neighbors/friends(11)";
  attrib SB317CN_12 format=F00260_. label="Who told they have high BP-neighbors/friends(12)";
  attrib SB317CN_13 format=F00260_. label="Who told they have high BP-neighbors/friends(13)";
  attrib SB317CN_14 format=F00260_. label="Who told they have high BP-neighbors/friends(14)";
  attrib SB317CX_01 format=F00261_. label="Who told they have high BP-other(1)";
  attrib SB317CX_02 format=F00261_. label="Who told they have high BP-other(2)";
  attrib SB317CX_03 format=F00261_. label="Who told they have high BP-other(3)";
  attrib SB317CX_04 format=F00261_. label="Who told they have high BP-other(4)";
  attrib SB317CX_05 format=F00261_. label="Who told they have high BP-other(5)";
  attrib SB317CX_06 format=F00261_. label="Who told they have high BP-other(6)";
  attrib SB317CX_07 format=F00261_. label="Who told they have high BP-other(7)";
  attrib SB317CX_08 format=F00261_. label="Who told they have high BP-other(8)";
  attrib SB317CX_09 format=F00261_. label="Who told they have high BP-other(9)";
  attrib SB317CX_10 format=F00261_. label="Who told they have high BP-other(10)";
  attrib SB317CX_11 format=F00261_. label="Who told they have high BP-other(11)";
  attrib SB317CX_12 format=F00261_. label="Who told they have high BP-other(12)";
  attrib SB317CX_13 format=F00261_. label="Who told they have high BP-other(13)";
  attrib SB317CX_14 format=F00261_. label="Who told they have high BP-other(14)";
  attrib SB318A_01 format=F00262_. label="Taking prescribed medicine to lower blood pressure(1)";
  attrib SB318A_02 format=F00262_. label="Taking prescribed medicine to lower blood pressure(2)";
  attrib SB318A_03 format=F00262_. label="Taking prescribed medicine to lower blood pressure(3)";
  attrib SB318A_04 format=F00262_. label="Taking prescribed medicine to lower blood pressure(4)";
  attrib SB318A_05 format=F00262_. label="Taking prescribed medicine to lower blood pressure(5)";
  attrib SB318A_06 format=F00262_. label="Taking prescribed medicine to lower blood pressure(6)";
  attrib SB318A_07 format=F00262_. label="Taking prescribed medicine to lower blood pressure(7)";
  attrib SB318A_08 format=F00262_. label="Taking prescribed medicine to lower blood pressure(8)";
  attrib SB318A_09 format=F00262_. label="Taking prescribed medicine to lower blood pressure(9)";
  attrib SB318A_10 format=F00262_. label="Taking prescribed medicine to lower blood pressure(10)";
  attrib SB318A_11 format=F00262_. label="Taking prescribed medicine to lower blood pressure(11)";
  attrib SB318A_12 format=F00262_. label="Taking prescribed medicine to lower blood pressure(12)";
  attrib SB318A_13 format=F00262_. label="Taking prescribed medicine to lower blood pressure(13)";
  attrib SB318A_14 format=F00262_. label="Taking prescribed medicine to lower blood pressure(14)";
  attrib SB318B_01 format=F00263_. label="Advised to reduce salt for high BP(1)";
  attrib SB318B_02 format=F00263_. label="Advised to reduce salt for high BP(2)";
  attrib SB318B_03 format=F00263_. label="Advised to reduce salt for high BP(3)";
  attrib SB318B_04 format=F00263_. label="Advised to reduce salt for high BP(4)";
  attrib SB318B_05 format=F00263_. label="Advised to reduce salt for high BP(5)";
  attrib SB318B_06 format=F00263_. label="Advised to reduce salt for high BP(6)";
  attrib SB318B_07 format=F00263_. label="Advised to reduce salt for high BP(7)";
  attrib SB318B_08 format=F00263_. label="Advised to reduce salt for high BP(8)";
  attrib SB318B_09 format=F00263_. label="Advised to reduce salt for high BP(9)";
  attrib SB318B_10 format=F00263_. label="Advised to reduce salt for high BP(10)";
  attrib SB318B_11 format=F00263_. label="Advised to reduce salt for high BP(11)";
  attrib SB318B_12 format=F00263_. label="Advised to reduce salt for high BP(12)";
  attrib SB318B_13 format=F00263_. label="Advised to reduce salt for high BP(13)";
  attrib SB318B_14 format=F00263_. label="Advised to reduce salt for high BP(14)";
  attrib SB318C_01 format=F00264_. label="Advised to lose weight for high BP(1)";
  attrib SB318C_02 format=F00264_. label="Advised to lose weight for high BP(2)";
  attrib SB318C_03 format=F00264_. label="Advised to lose weight for high BP(3)";
  attrib SB318C_04 format=F00264_. label="Advised to lose weight for high BP(4)";
  attrib SB318C_05 format=F00264_. label="Advised to lose weight for high BP(5)";
  attrib SB318C_06 format=F00264_. label="Advised to lose weight for high BP(6)";
  attrib SB318C_07 format=F00264_. label="Advised to lose weight for high BP(7)";
  attrib SB318C_08 format=F00264_. label="Advised to lose weight for high BP(8)";
  attrib SB318C_09 format=F00264_. label="Advised to lose weight for high BP(9)";
  attrib SB318C_10 format=F00264_. label="Advised to lose weight for high BP(10)";
  attrib SB318C_11 format=F00264_. label="Advised to lose weight for high BP(11)";
  attrib SB318C_12 format=F00264_. label="Advised to lose weight for high BP(12)";
  attrib SB318C_13 format=F00264_. label="Advised to lose weight for high BP(13)";
  attrib SB318C_14 format=F00264_. label="Advised to lose weight for high BP(14)";
  attrib SB318D_01 format=F00265_. label="Advised to stop smoking for high BP(1)";
  attrib SB318D_02 format=F00265_. label="Advised to stop smoking for high BP(2)";
  attrib SB318D_03 format=F00265_. label="Advised to stop smoking for high BP(3)";
  attrib SB318D_04 format=F00265_. label="Advised to stop smoking for high BP(4)";
  attrib SB318D_05 format=F00265_. label="Advised to stop smoking for high BP(5)";
  attrib SB318D_06 format=F00265_. label="Advised to stop smoking for high BP(6)";
  attrib SB318D_07 format=F00265_. label="Advised to stop smoking for high BP(7)";
  attrib SB318D_08 format=F00265_. label="Advised to stop smoking for high BP(8)";
  attrib SB318D_09 format=F00265_. label="Advised to stop smoking for high BP(9)";
  attrib SB318D_10 format=F00265_. label="Advised to stop smoking for high BP(10)";
  attrib SB318D_11 format=F00265_. label="Advised to stop smoking for high BP(11)";
  attrib SB318D_12 format=F00265_. label="Advised to stop smoking for high BP(12)";
  attrib SB318D_13 format=F00265_. label="Advised to stop smoking for high BP(13)";
  attrib SB318D_14 format=F00265_. label="Advised to stop smoking for high BP(14)";
  attrib SB318E_01 format=F00266_. label="Advised to exercise more(1)";
  attrib SB318E_02 format=F00266_. label="Advised to exercise more(2)";
  attrib SB318E_03 format=F00266_. label="Advised to exercise more(3)";
  attrib SB318E_04 format=F00266_. label="Advised to exercise more(4)";
  attrib SB318E_05 format=F00266_. label="Advised to exercise more(5)";
  attrib SB318E_06 format=F00266_. label="Advised to exercise more(6)";
  attrib SB318E_07 format=F00266_. label="Advised to exercise more(7)";
  attrib SB318E_08 format=F00266_. label="Advised to exercise more(8)";
  attrib SB318E_09 format=F00266_. label="Advised to exercise more(9)";
  attrib SB318E_10 format=F00266_. label="Advised to exercise more(10)";
  attrib SB318E_11 format=F00266_. label="Advised to exercise more(11)";
  attrib SB318E_12 format=F00266_. label="Advised to exercise more(12)";
  attrib SB318E_13 format=F00266_. label="Advised to exercise more(13)";
  attrib SB318E_14 format=F00266_. label="Advised to exercise more(14)";
  attrib SB319_01 format=F00267_. label="Taking natural remedies for high BP(1)";
  attrib SB319_02 format=F00267_. label="Taking natural remedies for high BP(2)";
  attrib SB319_03 format=F00267_. label="Taking natural remedies for high BP(3)";
  attrib SB319_04 format=F00267_. label="Taking natural remedies for high BP(4)";
  attrib SB319_05 format=F00267_. label="Taking natural remedies for high BP(5)";
  attrib SB319_06 format=F00267_. label="Taking natural remedies for high BP(6)";
  attrib SB319_07 format=F00267_. label="Taking natural remedies for high BP(7)";
  attrib SB319_08 format=F00267_. label="Taking natural remedies for high BP(8)";
  attrib SB319_09 format=F00267_. label="Taking natural remedies for high BP(9)";
  attrib SB319_10 format=F00267_. label="Taking natural remedies for high BP(10)";
  attrib SB319_11 format=F00267_. label="Taking natural remedies for high BP(11)";
  attrib SB319_12 format=F00267_. label="Taking natural remedies for high BP(12)";
  attrib SB319_13 format=F00267_. label="Taking natural remedies for high BP(13)";
  attrib SB319_14 format=F00267_. label="Taking natural remedies for high BP(14)";
  attrib SB321_01 label="Time for second blood pressure test(1)";
  attrib SB321_02 label="Time for second blood pressure test(2)";
  attrib SB321_03 label="Time for second blood pressure test(3)";
  attrib SB321_04 label="Time for second blood pressure test(4)";
  attrib SB321_05 label="Time for second blood pressure test(5)";
  attrib SB321_06 label="Time for second blood pressure test(6)";
  attrib SB321_07 label="Time for second blood pressure test(7)";
  attrib SB321_08 label="Time for second blood pressure test(8)";
  attrib SB321_09 label="Time for second blood pressure test(9)";
  attrib SB321_10 label="Time for second blood pressure test(10)";
  attrib SB321_11 label="Time for second blood pressure test(11)";
  attrib SB321_12 label="Time for second blood pressure test(12)";
  attrib SB321_13 label="Time for second blood pressure test(13)";
  attrib SB321_14 label="Time for second blood pressure test(14)";
  attrib SB322_01 format=F00268_. label="Consent for second blood pressure test(1)";
  attrib SB322_02 format=F00268_. label="Consent for second blood pressure test(2)";
  attrib SB322_03 format=F00268_. label="Consent for second blood pressure test(3)";
  attrib SB322_04 format=F00268_. label="Consent for second blood pressure test(4)";
  attrib SB322_05 format=F00268_. label="Consent for second blood pressure test(5)";
  attrib SB322_06 format=F00268_. label="Consent for second blood pressure test(6)";
  attrib SB322_07 format=F00268_. label="Consent for second blood pressure test(7)";
  attrib SB322_08 format=F00268_. label="Consent for second blood pressure test(8)";
  attrib SB322_09 format=F00268_. label="Consent for second blood pressure test(9)";
  attrib SB322_10 format=F00268_. label="Consent for second blood pressure test(10)";
  attrib SB322_11 format=F00268_. label="Consent for second blood pressure test(11)";
  attrib SB322_12 format=F00268_. label="Consent for second blood pressure test(12)";
  attrib SB322_13 format=F00268_. label="Consent for second blood pressure test(13)";
  attrib SB322_14 format=F00268_. label="Consent for second blood pressure test(14)";
  attrib SB323A_01 format=F00269_. label="Systolic blood pressure(1)";
  attrib SB323A_02 format=F00269_. label="Systolic blood pressure(2)";
  attrib SB323A_03 format=F00269_. label="Systolic blood pressure(3)";
  attrib SB323A_04 format=F00269_. label="Systolic blood pressure(4)";
  attrib SB323A_05 format=F00269_. label="Systolic blood pressure(5)";
  attrib SB323A_06 format=F00269_. label="Systolic blood pressure(6)";
  attrib SB323A_07 format=F00269_. label="Systolic blood pressure(7)";
  attrib SB323A_08 format=F00269_. label="Systolic blood pressure(8)";
  attrib SB323A_09 format=F00269_. label="Systolic blood pressure(9)";
  attrib SB323A_10 format=F00269_. label="Systolic blood pressure(10)";
  attrib SB323A_11 format=F00269_. label="Systolic blood pressure(11)";
  attrib SB323A_12 format=F00269_. label="Systolic blood pressure(12)";
  attrib SB323A_13 format=F00269_. label="Systolic blood pressure(13)";
  attrib SB323A_14 format=F00269_. label="Systolic blood pressure(14)";
  attrib SB323B_01 format=F00270_. label="Diastolic blood pressure(1)";
  attrib SB323B_02 format=F00270_. label="Diastolic blood pressure(2)";
  attrib SB323B_03 format=F00270_. label="Diastolic blood pressure(3)";
  attrib SB323B_04 format=F00270_. label="Diastolic blood pressure(4)";
  attrib SB323B_05 format=F00270_. label="Diastolic blood pressure(5)";
  attrib SB323B_06 format=F00270_. label="Diastolic blood pressure(6)";
  attrib SB323B_07 format=F00270_. label="Diastolic blood pressure(7)";
  attrib SB323B_08 format=F00270_. label="Diastolic blood pressure(8)";
  attrib SB323B_09 format=F00270_. label="Diastolic blood pressure(9)";
  attrib SB323B_10 format=F00270_. label="Diastolic blood pressure(10)";
  attrib SB323B_11 format=F00270_. label="Diastolic blood pressure(11)";
  attrib SB323B_12 format=F00270_. label="Diastolic blood pressure(12)";
  attrib SB323B_13 format=F00270_. label="Diastolic blood pressure(13)";
  attrib SB323B_14 format=F00270_. label="Diastolic blood pressure(14)";
  attrib SB324_01 format=F00271_. label="Heard of illness diabetes(1)";
  attrib SB324_02 format=F00271_. label="Heard of illness diabetes(2)";
  attrib SB324_03 format=F00271_. label="Heard of illness diabetes(3)";
  attrib SB324_04 format=F00271_. label="Heard of illness diabetes(4)";
  attrib SB324_05 format=F00271_. label="Heard of illness diabetes(5)";
  attrib SB324_06 format=F00271_. label="Heard of illness diabetes(6)";
  attrib SB324_07 format=F00271_. label="Heard of illness diabetes(7)";
  attrib SB324_08 format=F00271_. label="Heard of illness diabetes(8)";
  attrib SB324_09 format=F00271_. label="Heard of illness diabetes(9)";
  attrib SB324_10 format=F00271_. label="Heard of illness diabetes(10)";
  attrib SB324_11 format=F00271_. label="Heard of illness diabetes(11)";
  attrib SB324_12 format=F00271_. label="Heard of illness diabetes(12)";
  attrib SB324_13 format=F00271_. label="Heard of illness diabetes(13)";
  attrib SB324_14 format=F00271_. label="Heard of illness diabetes(14)";
  attrib SB325_01 format=F00272_. label="Glucose measured before(1)";
  attrib SB325_02 format=F00272_. label="Glucose measured before(2)";
  attrib SB325_03 format=F00272_. label="Glucose measured before(3)";
  attrib SB325_04 format=F00272_. label="Glucose measured before(4)";
  attrib SB325_05 format=F00272_. label="Glucose measured before(5)";
  attrib SB325_06 format=F00272_. label="Glucose measured before(6)";
  attrib SB325_07 format=F00272_. label="Glucose measured before(7)";
  attrib SB325_08 format=F00272_. label="Glucose measured before(8)";
  attrib SB325_09 format=F00272_. label="Glucose measured before(9)";
  attrib SB325_10 format=F00272_. label="Glucose measured before(10)";
  attrib SB325_11 format=F00272_. label="Glucose measured before(11)";
  attrib SB325_12 format=F00272_. label="Glucose measured before(12)";
  attrib SB325_13 format=F00272_. label="Glucose measured before(13)";
  attrib SB325_14 format=F00272_. label="Glucose measured before(14)";
  attrib SB326_01 label="When glucose was checked(1)";
  attrib SB326_02 label="When glucose was checked(2)";
  attrib SB326_03 label="When glucose was checked(3)";
  attrib SB326_04 label="When glucose was checked(4)";
  attrib SB326_05 label="When glucose was checked(5)";
  attrib SB326_06 label="When glucose was checked(6)";
  attrib SB326_07 label="When glucose was checked(7)";
  attrib SB326_08 label="When glucose was checked(8)";
  attrib SB326_09 label="When glucose was checked(9)";
  attrib SB326_10 label="When glucose was checked(10)";
  attrib SB326_11 label="When glucose was checked(11)";
  attrib SB326_12 label="When glucose was checked(12)";
  attrib SB326_13 label="When glucose was checked(13)";
  attrib SB326_14 label="When glucose was checked(14)";
  attrib SB326A_01 format=F00273_. label="Where glucose was checked(1)";
  attrib SB326A_02 format=F00273_. label="Where glucose was checked(2)";
  attrib SB326A_03 format=F00273_. label="Where glucose was checked(3)";
  attrib SB326A_04 format=F00273_. label="Where glucose was checked(4)";
  attrib SB326A_05 format=F00273_. label="Where glucose was checked(5)";
  attrib SB326A_06 format=F00273_. label="Where glucose was checked(6)";
  attrib SB326A_07 format=F00273_. label="Where glucose was checked(7)";
  attrib SB326A_08 format=F00273_. label="Where glucose was checked(8)";
  attrib SB326A_09 format=F00273_. label="Where glucose was checked(9)";
  attrib SB326A_10 format=F00273_. label="Where glucose was checked(10)";
  attrib SB326A_11 format=F00273_. label="Where glucose was checked(11)";
  attrib SB326A_12 format=F00273_. label="Where glucose was checked(12)";
  attrib SB326A_13 format=F00273_. label="Where glucose was checked(13)";
  attrib SB326A_14 format=F00273_. label="Where glucose was checked(14)";
  attrib SB326B_01 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(1)";
  attrib SB326B_02 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(2)";
  attrib SB326B_03 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(3)";
  attrib SB326B_04 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(4)";
  attrib SB326B_05 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(5)";
  attrib SB326B_06 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(6)";
  attrib SB326B_07 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(7)";
  attrib SB326B_08 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(8)";
  attrib SB326B_09 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(9)";
  attrib SB326B_10 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(10)";
  attrib SB326B_11 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(11)";
  attrib SB326B_12 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(12)";
  attrib SB326B_13 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(13)";
  attrib SB326B_14 format=F00274_. label="Ever told by a doctor/nurse to have diabetes(14)";
  attrib SB326CA_01 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(1)";
  attrib SB326CA_02 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(2)";
  attrib SB326CA_03 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(3)";
  attrib SB326CA_04 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(4)";
  attrib SB326CA_05 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(5)";
  attrib SB326CA_06 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(6)";
  attrib SB326CA_07 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(7)";
  attrib SB326CA_08 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(8)";
  attrib SB326CA_09 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(9)";
  attrib SB326CA_10 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(10)";
  attrib SB326CA_11 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(11)";
  attrib SB326CA_12 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(12)";
  attrib SB326CA_13 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(13)";
  attrib SB326CA_14 format=F00275_. label="Person who told has high blood glucose/diabetes-qualified doctor(14)";
  attrib SB326CB_01 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(1)";
  attrib SB326CB_02 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(2)";
  attrib SB326CB_03 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(3)";
  attrib SB326CB_04 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(4)";
  attrib SB326CB_05 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(5)";
  attrib SB326CB_06 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(6)";
  attrib SB326CB_07 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(7)";
  attrib SB326CB_08 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(8)";
  attrib SB326CB_09 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(9)";
  attrib SB326CB_10 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(10)";
  attrib SB326CB_11 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(11)";
  attrib SB326CB_12 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(12)";
  attrib SB326CB_13 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(13)";
  attrib SB326CB_14 format=F00276_. label="Person who told has high blood glucose/diabetes-nurse/midwife/paramedic(14)";
  attrib SB326CC_01 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(1)";
  attrib SB326CC_02 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(2)";
  attrib SB326CC_03 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(3)";
  attrib SB326CC_04 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(4)";
  attrib SB326CC_05 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(5)";
  attrib SB326CC_06 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(6)";
  attrib SB326CC_07 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(7)";
  attrib SB326CC_08 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(8)";
  attrib SB326CC_09 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(9)";
  attrib SB326CC_10 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(10)";
  attrib SB326CC_11 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(11)";
  attrib SB326CC_12 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(12)";
  attrib SB326CC_13 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(13)";
  attrib SB326CC_14 format=F00277_. label="Person who told has high blood glucose/diabetes-family welfare visitor (FWV)(14)";
  attrib SB326CD_01 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(1)";
  attrib SB326CD_02 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(2)";
  attrib SB326CD_03 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(3)";
  attrib SB326CD_04 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(4)";
  attrib SB326CD_05 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(5)";
  attrib SB326CD_06 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(6)";
  attrib SB326CD_07 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(7)";
  attrib SB326CD_08 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(8)";
  attrib SB326CD_09 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(9)";
  attrib SB326CD_10 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(10)";
  attrib SB326CD_11 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(11)";
  attrib SB326CD_12 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(12)";
  attrib SB326CD_13 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(13)";
  attrib SB326CD_14 format=F00278_. label="Person who told has high blood glucose/diabetes-community skilled birth attendant (CSBA)(14)";
  attrib SB326CE_01 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(1)";
  attrib SB326CE_02 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(2)";
  attrib SB326CE_03 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(3)";
  attrib SB326CE_04 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(4)";
  attrib SB326CE_05 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(5)";
  attrib SB326CE_06 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(6)";
  attrib SB326CE_07 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(7)";
  attrib SB326CE_08 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(8)";
  attrib SB326CE_09 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(9)";
  attrib SB326CE_10 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(10)";
  attrib SB326CE_11 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(11)";
  attrib SB326CE_12 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(12)";
  attrib SB326CE_13 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(13)";
  attrib SB326CE_14 format=F00279_. label="Person who told has high blood glucose/diabetes-sub-assistant community medical officer (SACMO)(14)";
  attrib SB326CF_01 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(1)";
  attrib SB326CF_02 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(2)";
  attrib SB326CF_03 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(3)";
  attrib SB326CF_04 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(4)";
  attrib SB326CF_05 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(5)";
  attrib SB326CF_06 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(6)";
  attrib SB326CF_07 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(7)";
  attrib SB326CF_08 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(8)";
  attrib SB326CF_09 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(9)";
  attrib SB326CF_10 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(10)";
  attrib SB326CF_11 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(11)";
  attrib SB326CF_12 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(12)";
  attrib SB326CF_13 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(13)";
  attrib SB326CF_14 format=F00280_. label="Person who told has high blood glucose/diabetes-community health care provider (CHCP)(14)";
  attrib SB326CG_01 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(1)";
  attrib SB326CG_02 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(2)";
  attrib SB326CG_03 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(3)";
  attrib SB326CG_04 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(4)";
  attrib SB326CG_05 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(5)";
  attrib SB326CG_06 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(6)";
  attrib SB326CG_07 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(7)";
  attrib SB326CG_08 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(8)";
  attrib SB326CG_09 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(9)";
  attrib SB326CG_10 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(10)";
  attrib SB326CG_11 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(11)";
  attrib SB326CG_12 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(12)";
  attrib SB326CG_13 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(13)";
  attrib SB326CG_14 format=F00281_. label="Person who told has high blood glucose/diabetes-health assistant (HA)(14)";
  attrib SB326CH_01 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(1)";
  attrib SB326CH_02 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(2)";
  attrib SB326CH_03 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(3)";
  attrib SB326CH_04 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(4)";
  attrib SB326CH_05 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(5)";
  attrib SB326CH_06 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(6)";
  attrib SB326CH_07 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(7)";
  attrib SB326CH_08 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(8)";
  attrib SB326CH_09 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(9)";
  attrib SB326CH_10 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(10)";
  attrib SB326CH_11 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(11)";
  attrib SB326CH_12 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(12)";
  attrib SB326CH_13 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(13)";
  attrib SB326CH_14 format=F00282_. label="Person who told has high blood glucose/diabetes-family welfare assistant (FWA)(14)";
  attrib SB326CI_01 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(1)";
  attrib SB326CI_02 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(2)";
  attrib SB326CI_03 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(3)";
  attrib SB326CI_04 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(4)";
  attrib SB326CI_05 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(5)";
  attrib SB326CI_06 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(6)";
  attrib SB326CI_07 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(7)";
  attrib SB326CI_08 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(8)";
  attrib SB326CI_09 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(9)";
  attrib SB326CI_10 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(10)";
  attrib SB326CI_11 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(11)";
  attrib SB326CI_12 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(12)";
  attrib SB326CI_13 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(13)";
  attrib SB326CI_14 format=F00283_. label="Person who told has high blood glucose/diabetes-NGO worker(14)";
  attrib SB326CJ_01 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(1)";
  attrib SB326CJ_02 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(2)";
  attrib SB326CJ_03 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(3)";
  attrib SB326CJ_04 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(4)";
  attrib SB326CJ_05 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(5)";
  attrib SB326CJ_06 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(6)";
  attrib SB326CJ_07 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(7)";
  attrib SB326CJ_08 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(8)";
  attrib SB326CJ_09 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(9)";
  attrib SB326CJ_10 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(10)";
  attrib SB326CJ_11 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(11)";
  attrib SB326CJ_12 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(12)";
  attrib SB326CJ_13 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(13)";
  attrib SB326CJ_14 format=F00284_. label="Person who told has high blood glucose/diabetes-trained TBA (TTBA)(14)";
  attrib SB326CK_01 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(1)";
  attrib SB326CK_02 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(2)";
  attrib SB326CK_03 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(3)";
  attrib SB326CK_04 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(4)";
  attrib SB326CK_05 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(5)";
  attrib SB326CK_06 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(6)";
  attrib SB326CK_07 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(7)";
  attrib SB326CK_08 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(8)";
  attrib SB326CK_09 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(9)";
  attrib SB326CK_10 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(10)";
  attrib SB326CK_11 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(11)";
  attrib SB326CK_12 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(12)";
  attrib SB326CK_13 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(13)";
  attrib SB326CK_14 format=F00285_. label="Person who told has high blood glucose/diabetes-untrained TBA (UTBA)(14)";
  attrib SB326CL_01 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(1)";
  attrib SB326CL_02 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(2)";
  attrib SB326CL_03 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(3)";
  attrib SB326CL_04 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(4)";
  attrib SB326CL_05 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(5)";
  attrib SB326CL_06 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(6)";
  attrib SB326CL_07 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(7)";
  attrib SB326CL_08 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(8)";
  attrib SB326CL_09 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(9)";
  attrib SB326CL_10 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(10)";
  attrib SB326CL_11 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(11)";
  attrib SB326CL_12 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(12)";
  attrib SB326CL_13 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(13)";
  attrib SB326CL_14 format=F00286_. label="Person who told has high blood glucose/diabetes-unqualified doctor(14)";
  attrib SB326CM_01 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(1)";
  attrib SB326CM_02 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(2)";
  attrib SB326CM_03 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(3)";
  attrib SB326CM_04 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(4)";
  attrib SB326CM_05 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(5)";
  attrib SB326CM_06 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(6)";
  attrib SB326CM_07 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(7)";
  attrib SB326CM_08 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(8)";
  attrib SB326CM_09 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(9)";
  attrib SB326CM_10 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(10)";
  attrib SB326CM_11 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(11)";
  attrib SB326CM_12 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(12)";
  attrib SB326CM_13 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(13)";
  attrib SB326CM_14 format=F00287_. label="Person who told has high blood glucose/diabetes-relatives(14)";
  attrib SB326CN_01 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(1)";
  attrib SB326CN_02 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(2)";
  attrib SB326CN_03 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(3)";
  attrib SB326CN_04 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(4)";
  attrib SB326CN_05 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(5)";
  attrib SB326CN_06 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(6)";
  attrib SB326CN_07 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(7)";
  attrib SB326CN_08 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(8)";
  attrib SB326CN_09 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(9)";
  attrib SB326CN_10 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(10)";
  attrib SB326CN_11 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(11)";
  attrib SB326CN_12 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(12)";
  attrib SB326CN_13 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(13)";
  attrib SB326CN_14 format=F00288_. label="Person who told has high blood glucose/diabetes-neighbors/friends(14)";
  attrib SB326CX_01 format=F00289_. label="Person who told has high blood glucose/diabetes-other(1)";
  attrib SB326CX_02 format=F00289_. label="Person who told has high blood glucose/diabetes-other(2)";
  attrib SB326CX_03 format=F00289_. label="Person who told has high blood glucose/diabetes-other(3)";
  attrib SB326CX_04 format=F00289_. label="Person who told has high blood glucose/diabetes-other(4)";
  attrib SB326CX_05 format=F00289_. label="Person who told has high blood glucose/diabetes-other(5)";
  attrib SB326CX_06 format=F00289_. label="Person who told has high blood glucose/diabetes-other(6)";
  attrib SB326CX_07 format=F00289_. label="Person who told has high blood glucose/diabetes-other(7)";
  attrib SB326CX_08 format=F00289_. label="Person who told has high blood glucose/diabetes-other(8)";
  attrib SB326CX_09 format=F00289_. label="Person who told has high blood glucose/diabetes-other(9)";
  attrib SB326CX_10 format=F00289_. label="Person who told has high blood glucose/diabetes-other(10)";
  attrib SB326CX_11 format=F00289_. label="Person who told has high blood glucose/diabetes-other(11)";
  attrib SB326CX_12 format=F00289_. label="Person who told has high blood glucose/diabetes-other(12)";
  attrib SB326CX_13 format=F00289_. label="Person who told has high blood glucose/diabetes-other(13)";
  attrib SB326CX_14 format=F00289_. label="Person who told has high blood glucose/diabetes-other(14)";
  attrib SB327A_01 format=F00290_. label="Taking medication for the diabetes(1)";
  attrib SB327A_02 format=F00290_. label="Taking medication for the diabetes(2)";
  attrib SB327A_03 format=F00290_. label="Taking medication for the diabetes(3)";
  attrib SB327A_04 format=F00290_. label="Taking medication for the diabetes(4)";
  attrib SB327A_05 format=F00290_. label="Taking medication for the diabetes(5)";
  attrib SB327A_06 format=F00290_. label="Taking medication for the diabetes(6)";
  attrib SB327A_07 format=F00290_. label="Taking medication for the diabetes(7)";
  attrib SB327A_08 format=F00290_. label="Taking medication for the diabetes(8)";
  attrib SB327A_09 format=F00290_. label="Taking medication for the diabetes(9)";
  attrib SB327A_10 format=F00290_. label="Taking medication for the diabetes(10)";
  attrib SB327A_11 format=F00290_. label="Taking medication for the diabetes(11)";
  attrib SB327A_12 format=F00290_. label="Taking medication for the diabetes(12)";
  attrib SB327A_13 format=F00290_. label="Taking medication for the diabetes(13)";
  attrib SB327A_14 format=F00290_. label="Taking medication for the diabetes(14)";
  attrib SB327B_01 format=F00291_. label="Advised diet for high blood glucose(1)";
  attrib SB327B_02 format=F00291_. label="Advised diet for high blood glucose(2)";
  attrib SB327B_03 format=F00291_. label="Advised diet for high blood glucose(3)";
  attrib SB327B_04 format=F00291_. label="Advised diet for high blood glucose(4)";
  attrib SB327B_05 format=F00291_. label="Advised diet for high blood glucose(5)";
  attrib SB327B_06 format=F00291_. label="Advised diet for high blood glucose(6)";
  attrib SB327B_07 format=F00291_. label="Advised diet for high blood glucose(7)";
  attrib SB327B_08 format=F00291_. label="Advised diet for high blood glucose(8)";
  attrib SB327B_09 format=F00291_. label="Advised diet for high blood glucose(9)";
  attrib SB327B_10 format=F00291_. label="Advised diet for high blood glucose(10)";
  attrib SB327B_11 format=F00291_. label="Advised diet for high blood glucose(11)";
  attrib SB327B_12 format=F00291_. label="Advised diet for high blood glucose(12)";
  attrib SB327B_13 format=F00291_. label="Advised diet for high blood glucose(13)";
  attrib SB327B_14 format=F00291_. label="Advised diet for high blood glucose(14)";
  attrib SB327C_01 format=F00292_. label="Advised to lose weight for high blood glucose(1)";
  attrib SB327C_02 format=F00292_. label="Advised to lose weight for high blood glucose(2)";
  attrib SB327C_03 format=F00292_. label="Advised to lose weight for high blood glucose(3)";
  attrib SB327C_04 format=F00292_. label="Advised to lose weight for high blood glucose(4)";
  attrib SB327C_05 format=F00292_. label="Advised to lose weight for high blood glucose(5)";
  attrib SB327C_06 format=F00292_. label="Advised to lose weight for high blood glucose(6)";
  attrib SB327C_07 format=F00292_. label="Advised to lose weight for high blood glucose(7)";
  attrib SB327C_08 format=F00292_. label="Advised to lose weight for high blood glucose(8)";
  attrib SB327C_09 format=F00292_. label="Advised to lose weight for high blood glucose(9)";
  attrib SB327C_10 format=F00292_. label="Advised to lose weight for high blood glucose(10)";
  attrib SB327C_11 format=F00292_. label="Advised to lose weight for high blood glucose(11)";
  attrib SB327C_12 format=F00292_. label="Advised to lose weight for high blood glucose(12)";
  attrib SB327C_13 format=F00292_. label="Advised to lose weight for high blood glucose(13)";
  attrib SB327C_14 format=F00292_. label="Advised to lose weight for high blood glucose(14)";
  attrib SB327D_01 format=F00293_. label="Advised to stop smoking(1)";
  attrib SB327D_02 format=F00293_. label="Advised to stop smoking(2)";
  attrib SB327D_03 format=F00293_. label="Advised to stop smoking(3)";
  attrib SB327D_04 format=F00293_. label="Advised to stop smoking(4)";
  attrib SB327D_05 format=F00293_. label="Advised to stop smoking(5)";
  attrib SB327D_06 format=F00293_. label="Advised to stop smoking(6)";
  attrib SB327D_07 format=F00293_. label="Advised to stop smoking(7)";
  attrib SB327D_08 format=F00293_. label="Advised to stop smoking(8)";
  attrib SB327D_09 format=F00293_. label="Advised to stop smoking(9)";
  attrib SB327D_10 format=F00293_. label="Advised to stop smoking(10)";
  attrib SB327D_11 format=F00293_. label="Advised to stop smoking(11)";
  attrib SB327D_12 format=F00293_. label="Advised to stop smoking(12)";
  attrib SB327D_13 format=F00293_. label="Advised to stop smoking(13)";
  attrib SB327D_14 format=F00293_. label="Advised to stop smoking(14)";
  attrib SB327E_01 format=F00294_. label="Advised to start/do more exercise(1)";
  attrib SB327E_02 format=F00294_. label="Advised to start/do more exercise(2)";
  attrib SB327E_03 format=F00294_. label="Advised to start/do more exercise(3)";
  attrib SB327E_04 format=F00294_. label="Advised to start/do more exercise(4)";
  attrib SB327E_05 format=F00294_. label="Advised to start/do more exercise(5)";
  attrib SB327E_06 format=F00294_. label="Advised to start/do more exercise(6)";
  attrib SB327E_07 format=F00294_. label="Advised to start/do more exercise(7)";
  attrib SB327E_08 format=F00294_. label="Advised to start/do more exercise(8)";
  attrib SB327E_09 format=F00294_. label="Advised to start/do more exercise(9)";
  attrib SB327E_10 format=F00294_. label="Advised to start/do more exercise(10)";
  attrib SB327E_11 format=F00294_. label="Advised to start/do more exercise(11)";
  attrib SB327E_12 format=F00294_. label="Advised to start/do more exercise(12)";
  attrib SB327E_13 format=F00294_. label="Advised to start/do more exercise(13)";
  attrib SB327E_14 format=F00294_. label="Advised to start/do more exercise(14)";
  attrib SB328_01 format=F00295_. label="Taking natural remedies for high blood glucose(1)";
  attrib SB328_02 format=F00295_. label="Taking natural remedies for high blood glucose(2)";
  attrib SB328_03 format=F00295_. label="Taking natural remedies for high blood glucose(3)";
  attrib SB328_04 format=F00295_. label="Taking natural remedies for high blood glucose(4)";
  attrib SB328_05 format=F00295_. label="Taking natural remedies for high blood glucose(5)";
  attrib SB328_06 format=F00295_. label="Taking natural remedies for high blood glucose(6)";
  attrib SB328_07 format=F00295_. label="Taking natural remedies for high blood glucose(7)";
  attrib SB328_08 format=F00295_. label="Taking natural remedies for high blood glucose(8)";
  attrib SB328_09 format=F00295_. label="Taking natural remedies for high blood glucose(9)";
  attrib SB328_10 format=F00295_. label="Taking natural remedies for high blood glucose(10)";
  attrib SB328_11 format=F00295_. label="Taking natural remedies for high blood glucose(11)";
  attrib SB328_12 format=F00295_. label="Taking natural remedies for high blood glucose(12)";
  attrib SB328_13 format=F00295_. label="Taking natural remedies for high blood glucose(13)";
  attrib SB328_14 format=F00295_. label="Taking natural remedies for high blood glucose(14)";
  attrib SB330_01 label="Time for third blood pressure test(1)";
  attrib SB330_02 label="Time for third blood pressure test(2)";
  attrib SB330_03 label="Time for third blood pressure test(3)";
  attrib SB330_04 label="Time for third blood pressure test(4)";
  attrib SB330_05 label="Time for third blood pressure test(5)";
  attrib SB330_06 label="Time for third blood pressure test(6)";
  attrib SB330_07 label="Time for third blood pressure test(7)";
  attrib SB330_08 label="Time for third blood pressure test(8)";
  attrib SB330_09 label="Time for third blood pressure test(9)";
  attrib SB330_10 label="Time for third blood pressure test(10)";
  attrib SB330_11 label="Time for third blood pressure test(11)";
  attrib SB330_12 label="Time for third blood pressure test(12)";
  attrib SB330_13 label="Time for third blood pressure test(13)";
  attrib SB330_14 label="Time for third blood pressure test(14)";
  attrib SB331_01 format=F00296_. label="Consent for third blood pressure test(1)";
  attrib SB331_02 format=F00296_. label="Consent for third blood pressure test(2)";
  attrib SB331_03 format=F00296_. label="Consent for third blood pressure test(3)";
  attrib SB331_04 format=F00296_. label="Consent for third blood pressure test(4)";
  attrib SB331_05 format=F00296_. label="Consent for third blood pressure test(5)";
  attrib SB331_06 format=F00296_. label="Consent for third blood pressure test(6)";
  attrib SB331_07 format=F00296_. label="Consent for third blood pressure test(7)";
  attrib SB331_08 format=F00296_. label="Consent for third blood pressure test(8)";
  attrib SB331_09 format=F00296_. label="Consent for third blood pressure test(9)";
  attrib SB331_10 format=F00296_. label="Consent for third blood pressure test(10)";
  attrib SB331_11 format=F00296_. label="Consent for third blood pressure test(11)";
  attrib SB331_12 format=F00296_. label="Consent for third blood pressure test(12)";
  attrib SB331_13 format=F00296_. label="Consent for third blood pressure test(13)";
  attrib SB331_14 format=F00296_. label="Consent for third blood pressure test(14)";
  attrib SB332A_01 format=F00297_. label="Systolic blood pressure(1)";
  attrib SB332A_02 format=F00297_. label="Systolic blood pressure(2)";
  attrib SB332A_03 format=F00297_. label="Systolic blood pressure(3)";
  attrib SB332A_04 format=F00297_. label="Systolic blood pressure(4)";
  attrib SB332A_05 format=F00297_. label="Systolic blood pressure(5)";
  attrib SB332A_06 format=F00297_. label="Systolic blood pressure(6)";
  attrib SB332A_07 format=F00297_. label="Systolic blood pressure(7)";
  attrib SB332A_08 format=F00297_. label="Systolic blood pressure(8)";
  attrib SB332A_09 format=F00297_. label="Systolic blood pressure(9)";
  attrib SB332A_10 format=F00297_. label="Systolic blood pressure(10)";
  attrib SB332A_11 format=F00297_. label="Systolic blood pressure(11)";
  attrib SB332A_12 format=F00297_. label="Systolic blood pressure(12)";
  attrib SB332A_13 format=F00297_. label="Systolic blood pressure(13)";
  attrib SB332A_14 format=F00297_. label="Systolic blood pressure(14)";
  attrib SB332B_01 format=F00298_. label="Diastolic blood pressure(1)";
  attrib SB332B_02 format=F00298_. label="Diastolic blood pressure(2)";
  attrib SB332B_03 format=F00298_. label="Diastolic blood pressure(3)";
  attrib SB332B_04 format=F00298_. label="Diastolic blood pressure(4)";
  attrib SB332B_05 format=F00298_. label="Diastolic blood pressure(5)";
  attrib SB332B_06 format=F00298_. label="Diastolic blood pressure(6)";
  attrib SB332B_07 format=F00298_. label="Diastolic blood pressure(7)";
  attrib SB332B_08 format=F00298_. label="Diastolic blood pressure(8)";
  attrib SB332B_09 format=F00298_. label="Diastolic blood pressure(9)";
  attrib SB332B_10 format=F00298_. label="Diastolic blood pressure(10)";
  attrib SB332B_11 format=F00298_. label="Diastolic blood pressure(11)";
  attrib SB332B_12 format=F00298_. label="Diastolic blood pressure(12)";
  attrib SB332B_13 format=F00298_. label="Diastolic blood pressure(13)";
  attrib SB332B_14 format=F00298_. label="Diastolic blood pressure(14)";
  attrib SB333AA_01 format=F00299_. label="Average of systolic(1)";
  attrib SB333AA_02 format=F00299_. label="Average of systolic(2)";
  attrib SB333AA_03 format=F00299_. label="Average of systolic(3)";
  attrib SB333AA_04 format=F00299_. label="Average of systolic(4)";
  attrib SB333AA_05 format=F00299_. label="Average of systolic(5)";
  attrib SB333AA_06 format=F00299_. label="Average of systolic(6)";
  attrib SB333AA_07 format=F00299_. label="Average of systolic(7)";
  attrib SB333AA_08 format=F00299_. label="Average of systolic(8)";
  attrib SB333AA_09 format=F00299_. label="Average of systolic(9)";
  attrib SB333AA_10 format=F00299_. label="Average of systolic(10)";
  attrib SB333AA_11 format=F00299_. label="Average of systolic(11)";
  attrib SB333AA_12 format=F00299_. label="Average of systolic(12)";
  attrib SB333AA_13 format=F00299_. label="Average of systolic(13)";
  attrib SB333AA_14 format=F00299_. label="Average of systolic(14)";
  attrib SB333AB_01 format=F00300_. label="Average of diastolic(1)";
  attrib SB333AB_02 format=F00300_. label="Average of diastolic(2)";
  attrib SB333AB_03 format=F00300_. label="Average of diastolic(3)";
  attrib SB333AB_04 format=F00300_. label="Average of diastolic(4)";
  attrib SB333AB_05 format=F00300_. label="Average of diastolic(5)";
  attrib SB333AB_06 format=F00300_. label="Average of diastolic(6)";
  attrib SB333AB_07 format=F00300_. label="Average of diastolic(7)";
  attrib SB333AB_08 format=F00300_. label="Average of diastolic(8)";
  attrib SB333AB_09 format=F00300_. label="Average of diastolic(9)";
  attrib SB333AB_10 format=F00300_. label="Average of diastolic(10)";
  attrib SB333AB_11 format=F00300_. label="Average of diastolic(11)";
  attrib SB333AB_12 format=F00300_. label="Average of diastolic(12)";
  attrib SB333AB_13 format=F00300_. label="Average of diastolic(13)";
  attrib SB333AB_14 format=F00300_. label="Average of diastolic(14)";
  attrib SB333B_01 format=F00301_. label="Referral number based on BP(1)";
  attrib SB333B_02 format=F00301_. label="Referral number based on BP(2)";
  attrib SB333B_03 format=F00301_. label="Referral number based on BP(3)";
  attrib SB333B_04 format=F00301_. label="Referral number based on BP(4)";
  attrib SB333B_05 format=F00301_. label="Referral number based on BP(5)";
  attrib SB333B_06 format=F00301_. label="Referral number based on BP(6)";
  attrib SB333B_07 format=F00301_. label="Referral number based on BP(7)";
  attrib SB333B_08 format=F00301_. label="Referral number based on BP(8)";
  attrib SB333B_09 format=F00301_. label="Referral number based on BP(9)";
  attrib SB333B_10 format=F00301_. label="Referral number based on BP(10)";
  attrib SB333B_11 format=F00301_. label="Referral number based on BP(11)";
  attrib SB333B_12 format=F00301_. label="Referral number based on BP(12)";
  attrib SB333B_13 format=F00301_. label="Referral number based on BP(13)";
  attrib SB333B_14 format=F00301_. label="Referral number based on BP(14)";
  attrib SB334BA_01 format=F00302_. label="First appointment permission(1)";
  attrib SB334BA_02 format=F00302_. label="First appointment permission(2)";
  attrib SB334BA_03 format=F00302_. label="First appointment permission(3)";
  attrib SB334BA_04 format=F00302_. label="First appointment permission(4)";
  attrib SB334BA_05 format=F00302_. label="First appointment permission(5)";
  attrib SB334BA_06 format=F00302_. label="First appointment permission(6)";
  attrib SB334BA_07 format=F00302_. label="First appointment permission(7)";
  attrib SB334BA_08 format=F00302_. label="First appointment permission(8)";
  attrib SB334BA_09 format=F00302_. label="First appointment permission(9)";
  attrib SB334BA_10 format=F00302_. label="First appointment permission(10)";
  attrib SB334BA_11 format=F00302_. label="First appointment permission(11)";
  attrib SB334BA_12 format=F00302_. label="First appointment permission(12)";
  attrib SB334BA_13 format=F00302_. label="First appointment permission(13)";
  attrib SB334BA_14 format=F00302_. label="First appointment permission(14)";
  attrib SB334BB_01 format=F00303_. label="Second appointment permission(1)";
  attrib SB334BB_02 format=F00303_. label="Second appointment permission(2)";
  attrib SB334BB_03 format=F00303_. label="Second appointment permission(3)";
  attrib SB334BB_04 format=F00303_. label="Second appointment permission(4)";
  attrib SB334BB_05 format=F00303_. label="Second appointment permission(5)";
  attrib SB334BB_06 format=F00303_. label="Second appointment permission(6)";
  attrib SB334BB_07 format=F00303_. label="Second appointment permission(7)";
  attrib SB334BB_08 format=F00303_. label="Second appointment permission(8)";
  attrib SB334BB_09 format=F00303_. label="Second appointment permission(9)";
  attrib SB334BB_10 format=F00303_. label="Second appointment permission(10)";
  attrib SB334BB_11 format=F00303_. label="Second appointment permission(11)";
  attrib SB334BB_12 format=F00303_. label="Second appointment permission(12)";
  attrib SB334BB_13 format=F00303_. label="Second appointment permission(13)";
  attrib SB334BB_14 format=F00303_. label="Second appointment permission(14)";
  attrib SB334BC_01 format=F00304_. label="Third appointment permission(1)";
  attrib SB334BC_02 format=F00304_. label="Third appointment permission(2)";
  attrib SB334BC_03 format=F00304_. label="Third appointment permission(3)";
  attrib SB334BC_04 format=F00304_. label="Third appointment permission(4)";
  attrib SB334BC_05 format=F00304_. label="Third appointment permission(5)";
  attrib SB334BC_06 format=F00304_. label="Third appointment permission(6)";
  attrib SB334BC_07 format=F00304_. label="Third appointment permission(7)";
  attrib SB334BC_08 format=F00304_. label="Third appointment permission(8)";
  attrib SB334BC_09 format=F00304_. label="Third appointment permission(9)";
  attrib SB334BC_10 format=F00304_. label="Third appointment permission(10)";
  attrib SB334BC_11 format=F00304_. label="Third appointment permission(11)";
  attrib SB334BC_12 format=F00304_. label="Third appointment permission(12)";
  attrib SB334BC_13 format=F00304_. label="Third appointment permission(13)";
  attrib SB334BC_14 format=F00304_. label="Third appointment permission(14)";
  attrib SB1D_01  label="First appointment day(1)";
  attrib SB1D_02  label="First appointment day(2)";
  attrib SB1D_03  label="First appointment day(3)";
  attrib SB1D_04  label="First appointment day(4)";
  attrib SB1D_05  label="First appointment day(5)";
  attrib SB1D_06  label="First appointment day(6)";
  attrib SB1D_07  label="First appointment day(7)";
  attrib SB1D_08  label="First appointment day(8)";
  attrib SB1D_09  label="First appointment day(9)";
  attrib SB1D_10  label="First appointment day(10)";
  attrib SB1D_11  label="First appointment day(11)";
  attrib SB1D_12  label="First appointment day(12)";
  attrib SB1D_13  label="First appointment day(13)";
  attrib SB1D_14  label="First appointment day(14)";
  attrib SB1MO_01 label="First appointment month(1)";
  attrib SB1MO_02 label="First appointment month(2)";
  attrib SB1MO_03 label="First appointment month(3)";
  attrib SB1MO_04 label="First appointment month(4)";
  attrib SB1MO_05 label="First appointment month(5)";
  attrib SB1MO_06 label="First appointment month(6)";
  attrib SB1MO_07 label="First appointment month(7)";
  attrib SB1MO_08 label="First appointment month(8)";
  attrib SB1MO_09 label="First appointment month(9)";
  attrib SB1MO_10 label="First appointment month(10)";
  attrib SB1MO_11 label="First appointment month(11)";
  attrib SB1MO_12 label="First appointment month(12)";
  attrib SB1MO_13 label="First appointment month(13)";
  attrib SB1MO_14 label="First appointment month(14)";
  attrib SB1Y_01  label="First appointment year(1)";
  attrib SB1Y_02  label="First appointment year(2)";
  attrib SB1Y_03  label="First appointment year(3)";
  attrib SB1Y_04  label="First appointment year(4)";
  attrib SB1Y_05  label="First appointment year(5)";
  attrib SB1Y_06  label="First appointment year(6)";
  attrib SB1Y_07  label="First appointment year(7)";
  attrib SB1Y_08  label="First appointment year(8)";
  attrib SB1Y_09  label="First appointment year(9)";
  attrib SB1Y_10  label="First appointment year(10)";
  attrib SB1Y_11  label="First appointment year(11)";
  attrib SB1Y_12  label="First appointment year(12)";
  attrib SB1Y_13  label="First appointment year(13)";
  attrib SB1Y_14  label="First appointment year(14)";
  attrib SB1H_01  label="First appointment hour(1)";
  attrib SB1H_02  label="First appointment hour(2)";
  attrib SB1H_03  label="First appointment hour(3)";
  attrib SB1H_04  label="First appointment hour(4)";
  attrib SB1H_05  label="First appointment hour(5)";
  attrib SB1H_06  label="First appointment hour(6)";
  attrib SB1H_07  label="First appointment hour(7)";
  attrib SB1H_08  label="First appointment hour(8)";
  attrib SB1H_09  label="First appointment hour(9)";
  attrib SB1H_10  label="First appointment hour(10)";
  attrib SB1H_11  label="First appointment hour(11)";
  attrib SB1H_12  label="First appointment hour(12)";
  attrib SB1H_13  label="First appointment hour(13)";
  attrib SB1H_14  label="First appointment hour(14)";
  attrib SB1M_01  label="First appointment minutes(1)";
  attrib SB1M_02  label="First appointment minutes(2)";
  attrib SB1M_03  label="First appointment minutes(3)";
  attrib SB1M_04  label="First appointment minutes(4)";
  attrib SB1M_05  label="First appointment minutes(5)";
  attrib SB1M_06  label="First appointment minutes(6)";
  attrib SB1M_07  label="First appointment minutes(7)";
  attrib SB1M_08  label="First appointment minutes(8)";
  attrib SB1M_09  label="First appointment minutes(9)";
  attrib SB1M_10  label="First appointment minutes(10)";
  attrib SB1M_11  label="First appointment minutes(11)";
  attrib SB1M_12  label="First appointment minutes(12)";
  attrib SB1M_13  label="First appointment minutes(13)";
  attrib SB1M_14  label="First appointment minutes(14)";
  attrib SB2D_01  label="Second appointment day(1)";
  attrib SB2D_02  label="Second appointment day(2)";
  attrib SB2D_03  label="Second appointment day(3)";
  attrib SB2D_04  label="Second appointment day(4)";
  attrib SB2D_05  label="Second appointment day(5)";
  attrib SB2D_06  label="Second appointment day(6)";
  attrib SB2D_07  label="Second appointment day(7)";
  attrib SB2D_08  label="Second appointment day(8)";
  attrib SB2D_09  label="Second appointment day(9)";
  attrib SB2D_10  label="Second appointment day(10)";
  attrib SB2D_11  label="Second appointment day(11)";
  attrib SB2D_12  label="Second appointment day(12)";
  attrib SB2D_13  label="Second appointment day(13)";
  attrib SB2D_14  label="Second appointment day(14)";
  attrib SB2MO_01 label="Second appointment month(1)";
  attrib SB2MO_02 label="Second appointment month(2)";
  attrib SB2MO_03 label="Second appointment month(3)";
  attrib SB2MO_04 label="Second appointment month(4)";
  attrib SB2MO_05 label="Second appointment month(5)";
  attrib SB2MO_06 label="Second appointment month(6)";
  attrib SB2MO_07 label="Second appointment month(7)";
  attrib SB2MO_08 label="Second appointment month(8)";
  attrib SB2MO_09 label="Second appointment month(9)";
  attrib SB2MO_10 label="Second appointment month(10)";
  attrib SB2MO_11 label="Second appointment month(11)";
  attrib SB2MO_12 label="Second appointment month(12)";
  attrib SB2MO_13 label="Second appointment month(13)";
  attrib SB2MO_14 label="Second appointment month(14)";
  attrib SB2Y_01  label="Second appointment year(1)";
  attrib SB2Y_02  label="Second appointment year(2)";
  attrib SB2Y_03  label="Second appointment year(3)";
  attrib SB2Y_04  label="Second appointment year(4)";
  attrib SB2Y_05  label="Second appointment year(5)";
  attrib SB2Y_06  label="Second appointment year(6)";
  attrib SB2Y_07  label="Second appointment year(7)";
  attrib SB2Y_08  label="Second appointment year(8)";
  attrib SB2Y_09  label="Second appointment year(9)";
  attrib SB2Y_10  label="Second appointment year(10)";
  attrib SB2Y_11  label="Second appointment year(11)";
  attrib SB2Y_12  label="Second appointment year(12)";
  attrib SB2Y_13  label="Second appointment year(13)";
  attrib SB2Y_14  label="Second appointment year(14)";
  attrib SB2H_01  label="Second appointment hour(1)";
  attrib SB2H_02  label="Second appointment hour(2)";
  attrib SB2H_03  label="Second appointment hour(3)";
  attrib SB2H_04  label="Second appointment hour(4)";
  attrib SB2H_05  label="Second appointment hour(5)";
  attrib SB2H_06  label="Second appointment hour(6)";
  attrib SB2H_07  label="Second appointment hour(7)";
  attrib SB2H_08  label="Second appointment hour(8)";
  attrib SB2H_09  label="Second appointment hour(9)";
  attrib SB2H_10  label="Second appointment hour(10)";
  attrib SB2H_11  label="Second appointment hour(11)";
  attrib SB2H_12  label="Second appointment hour(12)";
  attrib SB2H_13  label="Second appointment hour(13)";
  attrib SB2H_14  label="Second appointment hour(14)";
  attrib SB2M_01  label="Second appointment minutes(1)";
  attrib SB2M_02  label="Second appointment minutes(2)";
  attrib SB2M_03  label="Second appointment minutes(3)";
  attrib SB2M_04  label="Second appointment minutes(4)";
  attrib SB2M_05  label="Second appointment minutes(5)";
  attrib SB2M_06  label="Second appointment minutes(6)";
  attrib SB2M_07  label="Second appointment minutes(7)";
  attrib SB2M_08  label="Second appointment minutes(8)";
  attrib SB2M_09  label="Second appointment minutes(9)";
  attrib SB2M_10  label="Second appointment minutes(10)";
  attrib SB2M_11  label="Second appointment minutes(11)";
  attrib SB2M_12  label="Second appointment minutes(12)";
  attrib SB2M_13  label="Second appointment minutes(13)";
  attrib SB2M_14  label="Second appointment minutes(14)";
  attrib SB3D_01  label="Third appointment day(1)";
  attrib SB3D_02  label="Third appointment day(2)";
  attrib SB3D_03  label="Third appointment day(3)";
  attrib SB3D_04  label="Third appointment day(4)";
  attrib SB3D_05  label="Third appointment day(5)";
  attrib SB3D_06  label="Third appointment day(6)";
  attrib SB3D_07  label="Third appointment day(7)";
  attrib SB3D_08  label="Third appointment day(8)";
  attrib SB3D_09  label="Third appointment day(9)";
  attrib SB3D_10  label="Third appointment day(10)";
  attrib SB3D_11  label="Third appointment day(11)";
  attrib SB3D_12  label="Third appointment day(12)";
  attrib SB3D_13  label="Third appointment day(13)";
  attrib SB3D_14  label="Third appointment day(14)";
  attrib SB3MO_01 label="Third appointment month(1)";
  attrib SB3MO_02 label="Third appointment month(2)";
  attrib SB3MO_03 label="Third appointment month(3)";
  attrib SB3MO_04 label="Third appointment month(4)";
  attrib SB3MO_05 label="Third appointment month(5)";
  attrib SB3MO_06 label="Third appointment month(6)";
  attrib SB3MO_07 label="Third appointment month(7)";
  attrib SB3MO_08 label="Third appointment month(8)";
  attrib SB3MO_09 label="Third appointment month(9)";
  attrib SB3MO_10 label="Third appointment month(10)";
  attrib SB3MO_11 label="Third appointment month(11)";
  attrib SB3MO_12 label="Third appointment month(12)";
  attrib SB3MO_13 label="Third appointment month(13)";
  attrib SB3MO_14 label="Third appointment month(14)";
  attrib SB3Y_01  label="Third appointment year(1)";
  attrib SB3Y_02  label="Third appointment year(2)";
  attrib SB3Y_03  label="Third appointment year(3)";
  attrib SB3Y_04  label="Third appointment year(4)";
  attrib SB3Y_05  label="Third appointment year(5)";
  attrib SB3Y_06  label="Third appointment year(6)";
  attrib SB3Y_07  label="Third appointment year(7)";
  attrib SB3Y_08  label="Third appointment year(8)";
  attrib SB3Y_09  label="Third appointment year(9)";
  attrib SB3Y_10  label="Third appointment year(10)";
  attrib SB3Y_11  label="Third appointment year(11)";
  attrib SB3Y_12  label="Third appointment year(12)";
  attrib SB3Y_13  label="Third appointment year(13)";
  attrib SB3Y_14  label="Third appointment year(14)";
  attrib SB3H_01  label="Third appointment hour(1)";
  attrib SB3H_02  label="Third appointment hour(2)";
  attrib SB3H_03  label="Third appointment hour(3)";
  attrib SB3H_04  label="Third appointment hour(4)";
  attrib SB3H_05  label="Third appointment hour(5)";
  attrib SB3H_06  label="Third appointment hour(6)";
  attrib SB3H_07  label="Third appointment hour(7)";
  attrib SB3H_08  label="Third appointment hour(8)";
  attrib SB3H_09  label="Third appointment hour(9)";
  attrib SB3H_10  label="Third appointment hour(10)";
  attrib SB3H_11  label="Third appointment hour(11)";
  attrib SB3H_12  label="Third appointment hour(12)";
  attrib SB3H_13  label="Third appointment hour(13)";
  attrib SB3H_14  label="Third appointment hour(14)";
  attrib SB3M_01  label="Third appointment minutes(1)";
  attrib SB3M_02  label="Third appointment minutes(2)";
  attrib SB3M_03  label="Third appointment minutes(3)";
  attrib SB3M_04  label="Third appointment minutes(4)";
  attrib SB3M_05  label="Third appointment minutes(5)";
  attrib SB3M_06  label="Third appointment minutes(6)";
  attrib SB3M_07  label="Third appointment minutes(7)";
  attrib SB3M_08  label="Third appointment minutes(8)";
  attrib SB3M_09  label="Third appointment minutes(9)";
  attrib SB3M_10  label="Third appointment minutes(10)";
  attrib SB3M_11  label="Third appointment minutes(11)";
  attrib SB3M_12  label="Third appointment minutes(12)";
  attrib SB3M_13  label="Third appointment minutes(13)";
  attrib SB3M_14  label="Third appointment minutes(14)";
  attrib SB334EA_01 format=F00305_. label="1st appointment permission for glucose test(1)";
  attrib SB334EA_02 format=F00305_. label="1st appointment permission for glucose test(2)";
  attrib SB334EA_03 format=F00305_. label="1st appointment permission for glucose test(3)";
  attrib SB334EA_04 format=F00305_. label="1st appointment permission for glucose test(4)";
  attrib SB334EA_05 format=F00305_. label="1st appointment permission for glucose test(5)";
  attrib SB334EA_06 format=F00305_. label="1st appointment permission for glucose test(6)";
  attrib SB334EA_07 format=F00305_. label="1st appointment permission for glucose test(7)";
  attrib SB334EA_08 format=F00305_. label="1st appointment permission for glucose test(8)";
  attrib SB334EA_09 format=F00305_. label="1st appointment permission for glucose test(9)";
  attrib SB334EA_10 format=F00305_. label="1st appointment permission for glucose test(10)";
  attrib SB334EA_11 format=F00305_. label="1st appointment permission for glucose test(11)";
  attrib SB334EA_12 format=F00305_. label="1st appointment permission for glucose test(12)";
  attrib SB334EA_13 format=F00305_. label="1st appointment permission for glucose test(13)";
  attrib SB334EA_14 format=F00305_. label="1st appointment permission for glucose test(14)";
  attrib SB334EB_01 format=F00306_. label="2nd appointment permission for glucose test(1)";
  attrib SB334EB_02 format=F00306_. label="2nd appointment permission for glucose test(2)";
  attrib SB334EB_03 format=F00306_. label="2nd appointment permission for glucose test(3)";
  attrib SB334EB_04 format=F00306_. label="2nd appointment permission for glucose test(4)";
  attrib SB334EB_05 format=F00306_. label="2nd appointment permission for glucose test(5)";
  attrib SB334EB_06 format=F00306_. label="2nd appointment permission for glucose test(6)";
  attrib SB334EB_07 format=F00306_. label="2nd appointment permission for glucose test(7)";
  attrib SB334EB_08 format=F00306_. label="2nd appointment permission for glucose test(8)";
  attrib SB334EB_09 format=F00306_. label="2nd appointment permission for glucose test(9)";
  attrib SB334EB_10 format=F00306_. label="2nd appointment permission for glucose test(10)";
  attrib SB334EB_11 format=F00306_. label="2nd appointment permission for glucose test(11)";
  attrib SB334EB_12 format=F00306_. label="2nd appointment permission for glucose test(12)";
  attrib SB334EB_13 format=F00306_. label="2nd appointment permission for glucose test(13)";
  attrib SB334EB_14 format=F00306_. label="2nd appointment permission for glucose test(14)";
  attrib SB334EC_01 format=F00307_. label="3rd appointment permission for glucose test(1)";
  attrib SB334EC_02 format=F00307_. label="3rd appointment permission for glucose test(2)";
  attrib SB334EC_03 format=F00307_. label="3rd appointment permission for glucose test(3)";
  attrib SB334EC_04 format=F00307_. label="3rd appointment permission for glucose test(4)";
  attrib SB334EC_05 format=F00307_. label="3rd appointment permission for glucose test(5)";
  attrib SB334EC_06 format=F00307_. label="3rd appointment permission for glucose test(6)";
  attrib SB334EC_07 format=F00307_. label="3rd appointment permission for glucose test(7)";
  attrib SB334EC_08 format=F00307_. label="3rd appointment permission for glucose test(8)";
  attrib SB334EC_09 format=F00307_. label="3rd appointment permission for glucose test(9)";
  attrib SB334EC_10 format=F00307_. label="3rd appointment permission for glucose test(10)";
  attrib SB334EC_11 format=F00307_. label="3rd appointment permission for glucose test(11)";
  attrib SB334EC_12 format=F00307_. label="3rd appointment permission for glucose test(12)";
  attrib SB334EC_13 format=F00307_. label="3rd appointment permission for glucose test(13)";
  attrib SB334EC_14 format=F00307_. label="3rd appointment permission for glucose test(14)";
  attrib SB334FA_01 label="Last time ate-first appointment(1)";
  attrib SB334FA_02 label="Last time ate-first appointment(2)";
  attrib SB334FA_03 label="Last time ate-first appointment(3)";
  attrib SB334FA_04 label="Last time ate-first appointment(4)";
  attrib SB334FA_05 label="Last time ate-first appointment(5)";
  attrib SB334FA_06 label="Last time ate-first appointment(6)";
  attrib SB334FA_07 label="Last time ate-first appointment(7)";
  attrib SB334FA_08 label="Last time ate-first appointment(8)";
  attrib SB334FA_09 label="Last time ate-first appointment(9)";
  attrib SB334FA_10 label="Last time ate-first appointment(10)";
  attrib SB334FA_11 label="Last time ate-first appointment(11)";
  attrib SB334FA_12 label="Last time ate-first appointment(12)";
  attrib SB334FA_13 label="Last time ate-first appointment(13)";
  attrib SB334FA_14 label="Last time ate-first appointment(14)";
  attrib SB334FB_01 label="Last time ate-second appointment(1)";
  attrib SB334FB_02 label="Last time ate-second appointment(2)";
  attrib SB334FB_03 label="Last time ate-second appointment(3)";
  attrib SB334FB_04 label="Last time ate-second appointment(4)";
  attrib SB334FB_05 label="Last time ate-second appointment(5)";
  attrib SB334FB_06 label="Last time ate-second appointment(6)";
  attrib SB334FB_07 label="Last time ate-second appointment(7)";
  attrib SB334FB_08 label="Last time ate-second appointment(8)";
  attrib SB334FB_09 label="Last time ate-second appointment(9)";
  attrib SB334FB_10 label="Last time ate-second appointment(10)";
  attrib SB334FB_11 label="Last time ate-second appointment(11)";
  attrib SB334FB_12 label="Last time ate-second appointment(12)";
  attrib SB334FB_13 label="Last time ate-second appointment(13)";
  attrib SB334FB_14 label="Last time ate-second appointment(14)";
  attrib SB334FC_01 label="Last time ate-third appointment(1)";
  attrib SB334FC_02 label="Last time ate-third appointment(2)";
  attrib SB334FC_03 label="Last time ate-third appointment(3)";
  attrib SB334FC_04 label="Last time ate-third appointment(4)";
  attrib SB334FC_05 label="Last time ate-third appointment(5)";
  attrib SB334FC_06 label="Last time ate-third appointment(6)";
  attrib SB334FC_07 label="Last time ate-third appointment(7)";
  attrib SB334FC_08 label="Last time ate-third appointment(8)";
  attrib SB334FC_09 label="Last time ate-third appointment(9)";
  attrib SB334FC_10 label="Last time ate-third appointment(10)";
  attrib SB334FC_11 label="Last time ate-third appointment(11)";
  attrib SB334FC_12 label="Last time ate-third appointment(12)";
  attrib SB334FC_13 label="Last time ate-third appointment(13)";
  attrib SB334FC_14 label="Last time ate-third appointment(14)";
  attrib SB334GA_01 format=F00308_. label="Last time drank-first appointment(1)";
  attrib SB334GA_02 format=F00308_. label="Last time drank-first appointment(2)";
  attrib SB334GA_03 format=F00308_. label="Last time drank-first appointment(3)";
  attrib SB334GA_04 format=F00308_. label="Last time drank-first appointment(4)";
  attrib SB334GA_05 format=F00308_. label="Last time drank-first appointment(5)";
  attrib SB334GA_06 format=F00308_. label="Last time drank-first appointment(6)";
  attrib SB334GA_07 format=F00308_. label="Last time drank-first appointment(7)";
  attrib SB334GA_08 format=F00308_. label="Last time drank-first appointment(8)";
  attrib SB334GA_09 format=F00308_. label="Last time drank-first appointment(9)";
  attrib SB334GA_10 format=F00308_. label="Last time drank-first appointment(10)";
  attrib SB334GA_11 format=F00308_. label="Last time drank-first appointment(11)";
  attrib SB334GA_12 format=F00308_. label="Last time drank-first appointment(12)";
  attrib SB334GA_13 format=F00308_. label="Last time drank-first appointment(13)";
  attrib SB334GA_14 format=F00308_. label="Last time drank-first appointment(14)";
  attrib SB334GB_01 format=F00309_. label="Last time drank-second appointment(1)";
  attrib SB334GB_02 format=F00309_. label="Last time drank-second appointment(2)";
  attrib SB334GB_03 format=F00309_. label="Last time drank-second appointment(3)";
  attrib SB334GB_04 format=F00309_. label="Last time drank-second appointment(4)";
  attrib SB334GB_05 format=F00309_. label="Last time drank-second appointment(5)";
  attrib SB334GB_06 format=F00309_. label="Last time drank-second appointment(6)";
  attrib SB334GB_07 format=F00309_. label="Last time drank-second appointment(7)";
  attrib SB334GB_08 format=F00309_. label="Last time drank-second appointment(8)";
  attrib SB334GB_09 format=F00309_. label="Last time drank-second appointment(9)";
  attrib SB334GB_10 format=F00309_. label="Last time drank-second appointment(10)";
  attrib SB334GB_11 format=F00309_. label="Last time drank-second appointment(11)";
  attrib SB334GB_12 format=F00309_. label="Last time drank-second appointment(12)";
  attrib SB334GB_13 format=F00309_. label="Last time drank-second appointment(13)";
  attrib SB334GB_14 format=F00309_. label="Last time drank-second appointment(14)";
  attrib SB334GC_01 format=F00310_. label="Last time drank-third appointment(1)";
  attrib SB334GC_02 format=F00310_. label="Last time drank-third appointment(2)";
  attrib SB334GC_03 format=F00310_. label="Last time drank-third appointment(3)";
  attrib SB334GC_04 format=F00310_. label="Last time drank-third appointment(4)";
  attrib SB334GC_05 format=F00310_. label="Last time drank-third appointment(5)";
  attrib SB334GC_06 format=F00310_. label="Last time drank-third appointment(6)";
  attrib SB334GC_07 format=F00310_. label="Last time drank-third appointment(7)";
  attrib SB334GC_08 format=F00310_. label="Last time drank-third appointment(8)";
  attrib SB334GC_09 format=F00310_. label="Last time drank-third appointment(9)";
  attrib SB334GC_10 format=F00310_. label="Last time drank-third appointment(10)";
  attrib SB334GC_11 format=F00310_. label="Last time drank-third appointment(11)";
  attrib SB334GC_12 format=F00310_. label="Last time drank-third appointment(12)";
  attrib SB334GC_13 format=F00310_. label="Last time drank-third appointment(13)";
  attrib SB334GC_14 format=F00310_. label="Last time drank-third appointment(14)";
  attrib SB334H_01 format=F00311_. label="Last time ate(1)";
  attrib SB334H_02 format=F00311_. label="Last time ate(2)";
  attrib SB334H_03 format=F00311_. label="Last time ate(3)";
  attrib SB334H_04 format=F00311_. label="Last time ate(4)";
  attrib SB334H_05 format=F00311_. label="Last time ate(5)";
  attrib SB334H_06 format=F00311_. label="Last time ate(6)";
  attrib SB334H_07 format=F00311_. label="Last time ate(7)";
  attrib SB334H_08 format=F00311_. label="Last time ate(8)";
  attrib SB334H_09 format=F00311_. label="Last time ate(9)";
  attrib SB334H_10 format=F00311_. label="Last time ate(10)";
  attrib SB334H_11 format=F00311_. label="Last time ate(11)";
  attrib SB334H_12 format=F00311_. label="Last time ate(12)";
  attrib SB334H_13 format=F00311_. label="Last time ate(13)";
  attrib SB334H_14 format=F00311_. label="Last time ate(14)";
  attrib SB335AD_01 label="Date for glucose testing Day(1)";
  attrib SB335AD_02 label="Date for glucose testing Day(2)";
  attrib SB335AD_03 label="Date for glucose testing Day(3)";
  attrib SB335AD_04 label="Date for glucose testing Day(4)";
  attrib SB335AD_05 label="Date for glucose testing Day(5)";
  attrib SB335AD_06 label="Date for glucose testing Day(6)";
  attrib SB335AD_07 label="Date for glucose testing Day(7)";
  attrib SB335AD_08 label="Date for glucose testing Day(8)";
  attrib SB335AD_09 label="Date for glucose testing Day(9)";
  attrib SB335AD_10 label="Date for glucose testing Day(10)";
  attrib SB335AD_11 label="Date for glucose testing Day(11)";
  attrib SB335AD_12 label="Date for glucose testing Day(12)";
  attrib SB335AD_13 label="Date for glucose testing Day(13)";
  attrib SB335AD_14 label="Date for glucose testing Day(14)";
  attrib SB335AM_01 label="Date for glucose testing Month(1)";
  attrib SB335AM_02 label="Date for glucose testing Month(2)";
  attrib SB335AM_03 label="Date for glucose testing Month(3)";
  attrib SB335AM_04 label="Date for glucose testing Month(4)";
  attrib SB335AM_05 label="Date for glucose testing Month(5)";
  attrib SB335AM_06 label="Date for glucose testing Month(6)";
  attrib SB335AM_07 label="Date for glucose testing Month(7)";
  attrib SB335AM_08 label="Date for glucose testing Month(8)";
  attrib SB335AM_09 label="Date for glucose testing Month(9)";
  attrib SB335AM_10 label="Date for glucose testing Month(10)";
  attrib SB335AM_11 label="Date for glucose testing Month(11)";
  attrib SB335AM_12 label="Date for glucose testing Month(12)";
  attrib SB335AM_13 label="Date for glucose testing Month(13)";
  attrib SB335AM_14 label="Date for glucose testing Month(14)";
  attrib SB335AY_01 label="Date for glucose testing Year(1)";
  attrib SB335AY_02 label="Date for glucose testing Year(2)";
  attrib SB335AY_03 label="Date for glucose testing Year(3)";
  attrib SB335AY_04 label="Date for glucose testing Year(4)";
  attrib SB335AY_05 label="Date for glucose testing Year(5)";
  attrib SB335AY_06 label="Date for glucose testing Year(6)";
  attrib SB335AY_07 label="Date for glucose testing Year(7)";
  attrib SB335AY_08 label="Date for glucose testing Year(8)";
  attrib SB335AY_09 label="Date for glucose testing Year(9)";
  attrib SB335AY_10 label="Date for glucose testing Year(10)";
  attrib SB335AY_11 label="Date for glucose testing Year(11)";
  attrib SB335AY_12 label="Date for glucose testing Year(12)";
  attrib SB335AY_13 label="Date for glucose testing Year(13)";
  attrib SB335AY_14 label="Date for glucose testing Year(14)";
  attrib SB335AT_01 label="Time for glucose testing(1)";
  attrib SB335AT_02 label="Time for glucose testing(2)";
  attrib SB335AT_03 label="Time for glucose testing(3)";
  attrib SB335AT_04 label="Time for glucose testing(4)";
  attrib SB335AT_05 label="Time for glucose testing(5)";
  attrib SB335AT_06 label="Time for glucose testing(6)";
  attrib SB335AT_07 label="Time for glucose testing(7)";
  attrib SB335AT_08 label="Time for glucose testing(8)";
  attrib SB335AT_09 label="Time for glucose testing(9)";
  attrib SB335AT_10 label="Time for glucose testing(10)";
  attrib SB335AT_11 label="Time for glucose testing(11)";
  attrib SB335AT_12 label="Time for glucose testing(12)";
  attrib SB335AT_13 label="Time for glucose testing(13)";
  attrib SB335AT_14 label="Time for glucose testing(14)";
  attrib SB335B_01 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(1)";
  attrib SB335B_02 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(2)";
  attrib SB335B_03 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(3)";
  attrib SB335B_04 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(4)";
  attrib SB335B_05 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(5)";
  attrib SB335B_06 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(6)";
  attrib SB335B_07 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(7)";
  attrib SB335B_08 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(8)";
  attrib SB335B_09 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(9)";
  attrib SB335B_10 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(10)";
  attrib SB335B_11 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(11)";
  attrib SB335B_12 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(12)";
  attrib SB335B_13 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(13)";
  attrib SB335B_14 format=F00312_. label="Plasma blood glucose (mmol/dl) - 1 implied decimal(14)";
  attrib SBPC1_01 format=F00313_. label="Height/Age Percentile(1)";
  attrib SBPC1_02 format=F00313_. label="Height/Age Percentile(2)";
  attrib SBPC1_03 format=F00313_. label="Height/Age Percentile(3)";
  attrib SBPC1_04 format=F00313_. label="Height/Age Percentile(4)";
  attrib SBPC1_05 format=F00313_. label="Height/Age Percentile(5)";
  attrib SBPC1_06 format=F00313_. label="Height/Age Percentile(6)";
  attrib SBPC1_07 format=F00313_. label="Height/Age Percentile(7)";
  attrib SBPC1_08 format=F00313_. label="Height/Age Percentile(8)";
  attrib SBPC1_09 format=F00313_. label="Height/Age Percentile(9)";
  attrib SBPC1_10 format=F00313_. label="Height/Age Percentile(10)";
  attrib SBPC1_11 format=F00313_. label="Height/Age Percentile(11)";
  attrib SBPC1_12 format=F00313_. label="Height/Age Percentile(12)";
  attrib SBPC1_13 format=F00313_. label="Height/Age Percentile(13)";
  attrib SBPC1_14 format=F00313_. label="Height/Age Percentile(14)";
  attrib SBSD1_01 format=F00314_. label="Height/Age Standard deviations(1)";
  attrib SBSD1_02 format=F00314_. label="Height/Age Standard deviations(2)";
  attrib SBSD1_03 format=F00314_. label="Height/Age Standard deviations(3)";
  attrib SBSD1_04 format=F00314_. label="Height/Age Standard deviations(4)";
  attrib SBSD1_05 format=F00314_. label="Height/Age Standard deviations(5)";
  attrib SBSD1_06 format=F00314_. label="Height/Age Standard deviations(6)";
  attrib SBSD1_07 format=F00314_. label="Height/Age Standard deviations(7)";
  attrib SBSD1_08 format=F00314_. label="Height/Age Standard deviations(8)";
  attrib SBSD1_09 format=F00314_. label="Height/Age Standard deviations(9)";
  attrib SBSD1_10 format=F00314_. label="Height/Age Standard deviations(10)";
  attrib SBSD1_11 format=F00314_. label="Height/Age Standard deviations(11)";
  attrib SBSD1_12 format=F00314_. label="Height/Age Standard deviations(12)";
  attrib SBSD1_13 format=F00314_. label="Height/Age Standard deviations(13)";
  attrib SBSD1_14 format=F00314_. label="Height/Age Standard deviations(14)";
  attrib SBRM1_01 format=F00315_. label="Height/Age Percent reference median(1)";
  attrib SBRM1_02 format=F00315_. label="Height/Age Percent reference median(2)";
  attrib SBRM1_03 format=F00315_. label="Height/Age Percent reference median(3)";
  attrib SBRM1_04 format=F00315_. label="Height/Age Percent reference median(4)";
  attrib SBRM1_05 format=F00315_. label="Height/Age Percent reference median(5)";
  attrib SBRM1_06 format=F00315_. label="Height/Age Percent reference median(6)";
  attrib SBRM1_07 format=F00315_. label="Height/Age Percent reference median(7)";
  attrib SBRM1_08 format=F00315_. label="Height/Age Percent reference median(8)";
  attrib SBRM1_09 format=F00315_. label="Height/Age Percent reference median(9)";
  attrib SBRM1_10 format=F00315_. label="Height/Age Percent reference median(10)";
  attrib SBRM1_11 format=F00315_. label="Height/Age Percent reference median(11)";
  attrib SBRM1_12 format=F00315_. label="Height/Age Percent reference median(12)";
  attrib SBRM1_13 format=F00315_. label="Height/Age Percent reference median(13)";
  attrib SBRM1_14 format=F00315_. label="Height/Age Percent reference median(14)";
  attrib SBRM3_01 format=F00316_. label="Weight/Height Percent reference median (DHS)(1)";
  attrib SBRM3_02 format=F00316_. label="Weight/Height Percent reference median (DHS)(2)";
  attrib SBRM3_03 format=F00316_. label="Weight/Height Percent reference median (DHS)(3)";
  attrib SBRM3_04 format=F00316_. label="Weight/Height Percent reference median (DHS)(4)";
  attrib SBRM3_05 format=F00316_. label="Weight/Height Percent reference median (DHS)(5)";
  attrib SBRM3_06 format=F00316_. label="Weight/Height Percent reference median (DHS)(6)";
  attrib SBRM3_07 format=F00316_. label="Weight/Height Percent reference median (DHS)(7)";
  attrib SBRM3_08 format=F00316_. label="Weight/Height Percent reference median (DHS)(8)";
  attrib SBRM3_09 format=F00316_. label="Weight/Height Percent reference median (DHS)(9)";
  attrib SBRM3_10 format=F00316_. label="Weight/Height Percent reference median (DHS)(10)";
  attrib SBRM3_11 format=F00316_. label="Weight/Height Percent reference median (DHS)(11)";
  attrib SBRM3_12 format=F00316_. label="Weight/Height Percent reference median (DHS)(12)";
  attrib SBRM3_13 format=F00316_. label="Weight/Height Percent reference median (DHS)(13)";
  attrib SBRM3_14 format=F00316_. label="Weight/Height Percent reference median (DHS)(14)";
  attrib SBRM4_01 format=F00317_. label="Weight/Height Percent reference median (Fog)(1)";
  attrib SBRM4_02 format=F00317_. label="Weight/Height Percent reference median (Fog)(2)";
  attrib SBRM4_03 format=F00317_. label="Weight/Height Percent reference median (Fog)(3)";
  attrib SBRM4_04 format=F00317_. label="Weight/Height Percent reference median (Fog)(4)";
  attrib SBRM4_05 format=F00317_. label="Weight/Height Percent reference median (Fog)(5)";
  attrib SBRM4_06 format=F00317_. label="Weight/Height Percent reference median (Fog)(6)";
  attrib SBRM4_07 format=F00317_. label="Weight/Height Percent reference median (Fog)(7)";
  attrib SBRM4_08 format=F00317_. label="Weight/Height Percent reference median (Fog)(8)";
  attrib SBRM4_09 format=F00317_. label="Weight/Height Percent reference median (Fog)(9)";
  attrib SBRM4_10 format=F00317_. label="Weight/Height Percent reference median (Fog)(10)";
  attrib SBRM4_11 format=F00317_. label="Weight/Height Percent reference median (Fog)(11)";
  attrib SBRM4_12 format=F00317_. label="Weight/Height Percent reference median (Fog)(12)";
  attrib SBRM4_13 format=F00317_. label="Weight/Height Percent reference median (Fog)(13)";
  attrib SBRM4_14 format=F00317_. label="Weight/Height Percent reference median (Fog)(14)";
  attrib SBRM5_01 format=F00318_. label="Weight/Height Percent reference median (WHO)(1)";
  attrib SBRM5_02 format=F00318_. label="Weight/Height Percent reference median (WHO)(2)";
  attrib SBRM5_03 format=F00318_. label="Weight/Height Percent reference median (WHO)(3)";
  attrib SBRM5_04 format=F00318_. label="Weight/Height Percent reference median (WHO)(4)";
  attrib SBRM5_05 format=F00318_. label="Weight/Height Percent reference median (WHO)(5)";
  attrib SBRM5_06 format=F00318_. label="Weight/Height Percent reference median (WHO)(6)";
  attrib SBRM5_07 format=F00318_. label="Weight/Height Percent reference median (WHO)(7)";
  attrib SBRM5_08 format=F00318_. label="Weight/Height Percent reference median (WHO)(8)";
  attrib SBRM5_09 format=F00318_. label="Weight/Height Percent reference median (WHO)(9)";
  attrib SBRM5_10 format=F00318_. label="Weight/Height Percent reference median (WHO)(10)";
  attrib SBRM5_11 format=F00318_. label="Weight/Height Percent reference median (WHO)(11)";
  attrib SBRM5_12 format=F00318_. label="Weight/Height Percent reference median (WHO)(12)";
  attrib SBRM5_13 format=F00318_. label="Weight/Height Percent reference median (WHO)(13)";
  attrib SBRM5_14 format=F00318_. label="Weight/Height Percent reference median (WHO)(14)";
  attrib SBSD3_01 format=F00319_. label="Weight/Height Standard deviations (DHS)(1)";
  attrib SBSD3_02 format=F00319_. label="Weight/Height Standard deviations (DHS)(2)";
  attrib SBSD3_03 format=F00319_. label="Weight/Height Standard deviations (DHS)(3)";
  attrib SBSD3_04 format=F00319_. label="Weight/Height Standard deviations (DHS)(4)";
  attrib SBSD3_05 format=F00319_. label="Weight/Height Standard deviations (DHS)(5)";
  attrib SBSD3_06 format=F00319_. label="Weight/Height Standard deviations (DHS)(6)";
  attrib SBSD3_07 format=F00319_. label="Weight/Height Standard deviations (DHS)(7)";
  attrib SBSD3_08 format=F00319_. label="Weight/Height Standard deviations (DHS)(8)";
  attrib SBSD3_09 format=F00319_. label="Weight/Height Standard deviations (DHS)(9)";
  attrib SBSD3_10 format=F00319_. label="Weight/Height Standard deviations (DHS)(10)";
  attrib SBSD3_11 format=F00319_. label="Weight/Height Standard deviations (DHS)(11)";
  attrib SBSD3_12 format=F00319_. label="Weight/Height Standard deviations (DHS)(12)";
  attrib SBSD3_13 format=F00319_. label="Weight/Height Standard deviations (DHS)(13)";
  attrib SBSD3_14 format=F00319_. label="Weight/Height Standard deviations (DHS)(14)";
  attrib SBWH_01  format=F00320_. label="Result of measurement height-weight(1)";
  attrib SBWH_02  format=F00320_. label="Result of measurement height-weight(2)";
  attrib SBWH_03  format=F00320_. label="Result of measurement height-weight(3)";
  attrib SBWH_04  format=F00320_. label="Result of measurement height-weight(4)";
  attrib SBWH_05  format=F00320_. label="Result of measurement height-weight(5)";
  attrib SBWH_06  format=F00320_. label="Result of measurement height-weight(6)";
  attrib SBWH_07  format=F00320_. label="Result of measurement height-weight(7)";
  attrib SBWH_08  format=F00320_. label="Result of measurement height-weight(8)";
  attrib SBWH_09  format=F00320_. label="Result of measurement height-weight(9)";
  attrib SBWH_10  format=F00320_. label="Result of measurement height-weight(10)";
  attrib SBWH_11  format=F00320_. label="Result of measurement height-weight(11)";
  attrib SBWH_12  format=F00320_. label="Result of measurement height-weight(12)";
  attrib SBWH_13  format=F00320_. label="Result of measurement height-weight(13)";
  attrib SBWH_14  format=F00320_. label="Result of measurement height-weight(14)";
  attrib SBBM_01  format=F00321_. label="Body mass index for respondent(1)";
  attrib SBBM_02  format=F00321_. label="Body mass index for respondent(2)";
  attrib SBBM_03  format=F00321_. label="Body mass index for respondent(3)";
  attrib SBBM_04  format=F00321_. label="Body mass index for respondent(4)";
  attrib SBBM_05  format=F00321_. label="Body mass index for respondent(5)";
  attrib SBBM_06  format=F00321_. label="Body mass index for respondent(6)";
  attrib SBBM_07  format=F00321_. label="Body mass index for respondent(7)";
  attrib SBBM_08  format=F00321_. label="Body mass index for respondent(8)";
  attrib SBBM_09  format=F00321_. label="Body mass index for respondent(9)";
  attrib SBBM_10  format=F00321_. label="Body mass index for respondent(10)";
  attrib SBBM_11  format=F00321_. label="Body mass index for respondent(11)";
  attrib SBBM_12  format=F00321_. label="Body mass index for respondent(12)";
  attrib SBBM_13  format=F00321_. label="Body mass index for respondent(13)";
  attrib SBBM_14  format=F00321_. label="Body mass index for respondent(14)";
  attrib SBRI_01  format=F00322_. label="Rohrer's index for respondent(1)";
  attrib SBRI_02  format=F00322_. label="Rohrer's index for respondent(2)";
  attrib SBRI_03  format=F00322_. label="Rohrer's index for respondent(3)";
  attrib SBRI_04  format=F00322_. label="Rohrer's index for respondent(4)";
  attrib SBRI_05  format=F00322_. label="Rohrer's index for respondent(5)";
  attrib SBRI_06  format=F00322_. label="Rohrer's index for respondent(6)";
  attrib SBRI_07  format=F00322_. label="Rohrer's index for respondent(7)";
  attrib SBRI_08  format=F00322_. label="Rohrer's index for respondent(8)";
  attrib SBRI_09  format=F00322_. label="Rohrer's index for respondent(9)";
  attrib SBRI_10  format=F00322_. label="Rohrer's index for respondent(10)";
  attrib SBRI_11  format=F00322_. label="Rohrer's index for respondent(11)";
  attrib SBRI_12  format=F00322_. label="Rohrer's index for respondent(12)";
  attrib SBRI_13  format=F00322_. label="Rohrer's index for respondent(13)";
  attrib SBRI_14  format=F00322_. label="Rohrer's index for respondent(14)";
  attrib SBCMC_01 label="CMC of birth of member(1)";
  attrib SBCMC_02 label="CMC of birth of member(2)";
  attrib SBCMC_03 label="CMC of birth of member(3)";
  attrib SBCMC_04 label="CMC of birth of member(4)";
  attrib SBCMC_05 label="CMC of birth of member(5)";
  attrib SBCMC_06 label="CMC of birth of member(6)";
  attrib SBCMC_07 label="CMC of birth of member(7)";
  attrib SBCMC_08 label="CMC of birth of member(8)";
  attrib SBCMC_09 label="CMC of birth of member(9)";
  attrib SBCMC_10 label="CMC of birth of member(10)";
  attrib SBCMC_11 label="CMC of birth of member(11)";
  attrib SBCMC_12 label="CMC of birth of member(12)";
  attrib SBCMC_13 label="CMC of birth of member(13)";
  attrib SBCMC_14 label="CMC of birth of member(14)";
  attrib SBFLG_01 format=F00323_. label="Date flag for member's date of birth(1)";
  attrib SBFLG_02 format=F00323_. label="Date flag for member's date of birth(2)";
  attrib SBFLG_03 format=F00323_. label="Date flag for member's date of birth(3)";
  attrib SBFLG_04 format=F00323_. label="Date flag for member's date of birth(4)";
  attrib SBFLG_05 format=F00323_. label="Date flag for member's date of birth(5)";
  attrib SBFLG_06 format=F00323_. label="Date flag for member's date of birth(6)";
  attrib SBFLG_07 format=F00323_. label="Date flag for member's date of birth(7)";
  attrib SBFLG_08 format=F00323_. label="Date flag for member's date of birth(8)";
  attrib SBFLG_09 format=F00323_. label="Date flag for member's date of birth(9)";
  attrib SBFLG_10 format=F00323_. label="Date flag for member's date of birth(10)";
  attrib SBFLG_11 format=F00323_. label="Date flag for member's date of birth(11)";
  attrib SBFLG_12 format=F00323_. label="Date flag for member's date of birth(12)";
  attrib SBFLG_13 format=F00323_. label="Date flag for member's date of birth(13)";
  attrib SBFLG_14 format=F00323_. label="Date flag for member's date of birth(14)";
  attrib SBRESI_01 format=F00324_. label="Result of individual interview(1)";
  attrib SBRESI_02 format=F00324_. label="Result of individual interview(2)";
  attrib SBRESI_03 format=F00324_. label="Result of individual interview(3)";
  attrib SBRESI_04 format=F00324_. label="Result of individual interview(4)";
  attrib SBRESI_05 format=F00324_. label="Result of individual interview(5)";
  attrib SBRESI_06 format=F00324_. label="Result of individual interview(6)";
  attrib SBRESI_07 format=F00324_. label="Result of individual interview(7)";
  attrib SBRESI_08 format=F00324_. label="Result of individual interview(8)";
  attrib SBRESI_09 format=F00324_. label="Result of individual interview(9)";
  attrib SBRESI_10 format=F00324_. label="Result of individual interview(10)";
  attrib SBRESI_11 format=F00324_. label="Result of individual interview(11)";
  attrib SBRESI_12 format=F00324_. label="Result of individual interview(12)";
  attrib SBRESI_13 format=F00324_. label="Result of individual interview(13)";
  attrib SBRESI_14 format=F00324_. label="Result of individual interview(14)";
  attrib SBED1_01 format=F00325_. label="Member's highest educational level(1)";
  attrib SBED1_02 format=F00325_. label="Member's highest educational level(2)";
  attrib SBED1_03 format=F00325_. label="Member's highest educational level(3)";
  attrib SBED1_04 format=F00325_. label="Member's highest educational level(4)";
  attrib SBED1_05 format=F00325_. label="Member's highest educational level(5)";
  attrib SBED1_06 format=F00325_. label="Member's highest educational level(6)";
  attrib SBED1_07 format=F00325_. label="Member's highest educational level(7)";
  attrib SBED1_08 format=F00325_. label="Member's highest educational level(8)";
  attrib SBED1_09 format=F00325_. label="Member's highest educational level(9)";
  attrib SBED1_10 format=F00325_. label="Member's highest educational level(10)";
  attrib SBED1_11 format=F00325_. label="Member's highest educational level(11)";
  attrib SBED1_12 format=F00325_. label="Member's highest educational level(12)";
  attrib SBED1_13 format=F00325_. label="Member's highest educational level(13)";
  attrib SBED1_14 format=F00325_. label="Member's highest educational level(14)";
  attrib SBED2_01 format=F00326_. label="Member's highest year of education(1)";
  attrib SBED2_02 format=F00326_. label="Member's highest year of education(2)";
  attrib SBED2_03 format=F00326_. label="Member's highest year of education(3)";
  attrib SBED2_04 format=F00326_. label="Member's highest year of education(4)";
  attrib SBED2_05 format=F00326_. label="Member's highest year of education(5)";
  attrib SBED2_06 format=F00326_. label="Member's highest year of education(6)";
  attrib SBED2_07 format=F00326_. label="Member's highest year of education(7)";
  attrib SBED2_08 format=F00326_. label="Member's highest year of education(8)";
  attrib SBED2_09 format=F00326_. label="Member's highest year of education(9)";
  attrib SBED2_10 format=F00326_. label="Member's highest year of education(10)";
  attrib SBED2_11 format=F00326_. label="Member's highest year of education(11)";
  attrib SBED2_12 format=F00326_. label="Member's highest year of education(12)";
  attrib SBED2_13 format=F00326_. label="Member's highest year of education(13)";
  attrib SBED2_14 format=F00326_. label="Member's highest year of education(14)";
  attrib SBED3_01 format=F00327_. label="Highest educational level (CS for preliminary & final report)(1)";
  attrib SBED3_02 format=F00327_. label="Highest educational level (CS for preliminary & final report)(2)";
  attrib SBED3_03 format=F00327_. label="Highest educational level (CS for preliminary & final report)(3)";
  attrib SBED3_04 format=F00327_. label="Highest educational level (CS for preliminary & final report)(4)";
  attrib SBED3_05 format=F00327_. label="Highest educational level (CS for preliminary & final report)(5)";
  attrib SBED3_06 format=F00327_. label="Highest educational level (CS for preliminary & final report)(6)";
  attrib SBED3_07 format=F00327_. label="Highest educational level (CS for preliminary & final report)(7)";
  attrib SBED3_08 format=F00327_. label="Highest educational level (CS for preliminary & final report)(8)";
  attrib SBED3_09 format=F00327_. label="Highest educational level (CS for preliminary & final report)(9)";
  attrib SBED3_10 format=F00327_. label="Highest educational level (CS for preliminary & final report)(10)";
  attrib SBED3_11 format=F00327_. label="Highest educational level (CS for preliminary & final report)(11)";
  attrib SBED3_12 format=F00327_. label="Highest educational level (CS for preliminary & final report)(12)";
  attrib SBED3_13 format=F00327_. label="Highest educational level (CS for preliminary & final report)(13)";
  attrib SBED3_14 format=F00327_. label="Highest educational level (CS for preliminary & final report)(14)";
  attrib SB3PRG_01 format=F00328_. label="Respondent pregnancy status(1)";
  attrib SB3PRG_02 format=F00328_. label="Respondent pregnancy status(2)";
  attrib SB3PRG_03 format=F00328_. label="Respondent pregnancy status(3)";
  attrib SB3PRG_04 format=F00328_. label="Respondent pregnancy status(4)";
  attrib SB3PRG_05 format=F00328_. label="Respondent pregnancy status(5)";
  attrib SB3PRG_06 format=F00328_. label="Respondent pregnancy status(6)";
  attrib SB3PRG_07 format=F00328_. label="Respondent pregnancy status(7)";
  attrib SB3PRG_08 format=F00328_. label="Respondent pregnancy status(8)";
  attrib SB3PRG_09 format=F00328_. label="Respondent pregnancy status(9)";
  attrib SB3PRG_10 format=F00328_. label="Respondent pregnancy status(10)";
  attrib SB3PRG_11 format=F00328_. label="Respondent pregnancy status(11)";
  attrib SB3PRG_12 format=F00328_. label="Respondent pregnancy status(12)";
  attrib SB3PRG_13 format=F00328_. label="Respondent pregnancy status(13)";
  attrib SB3PRG_14 format=F00328_. label="Respondent pregnancy status(14)";
  attrib SB3MOS_01 label="Months of pregnancy(1)";
  attrib SB3MOS_02 label="Months of pregnancy(2)";
  attrib SB3MOS_03 label="Months of pregnancy(3)";
  attrib SB3MOS_04 label="Months of pregnancy(4)";
  attrib SB3MOS_05 label="Months of pregnancy(5)";
  attrib SB3MOS_06 label="Months of pregnancy(6)";
  attrib SB3MOS_07 label="Months of pregnancy(7)";
  attrib SB3MOS_08 label="Months of pregnancy(8)";
  attrib SB3MOS_09 label="Months of pregnancy(9)";
  attrib SB3MOS_10 label="Months of pregnancy(10)";
  attrib SB3MOS_11 label="Months of pregnancy(11)";
  attrib SB3MOS_12 label="Months of pregnancy(12)";
  attrib SB3MOS_13 label="Months of pregnancy(13)";
  attrib SB3MOS_14 label="Months of pregnancy(14)";
infile 'C:\COUNTRIES\BD7R\BDHR7RFL.DAT' LRECL=6994 MISSOVER ;
    input
    @1    HHID     $12.
    @13   HV000    $3.
    @16   HV001    6.0
    @22   HV002    6.0
    @28   HV003    3.0
    @31   HV004    4.0
    @35   HV005    8.0
    @43   HV006    2.0
    @45   HV007    4.0
    @49   HV008    4.0
    @53   HV008A   5.0
    @58   HV009    2.0
    @60   HV010    2.0
    @62   HV011    2.0
    @64   HV012    2.0
    @66   HV013    2.0
    @68   HV014    2.0
    @70   HV015    1.0
    @71   HV016    2.0
    @73   HV017    1.0
    @74   HV018    5.0
    @79   HV019    3.0
    @82   HV020    1.0
    @83   HV021    4.0
    @87   HV022    4.0
    @91   HV023    2.0
    @93   HV024    2.0
    @95   HV025    1.0
    @96   HV026    1.0
    @97   HV027    1.0
    @98   HV028    8.0
    @106  HV030    5.0
    @111  HV031    5.0
    @116  HV032    3.0
    @119  HV035    2.0
    @121  HV040    4.0
    @125  HV041    2.0
    @127  HV042    1.0
    @128  HV044    1.0
    @129  HV045A   3.0
    @132  HV045B   3.0
    @135  HV045C   3.0
    @138  HV046    1.0
    @139  HV801    4.0
    @143  HV802    4.0
    @147  HV803    2.0
    @149  HV804    1.0
    @150  HV807D   2.0
    @152  HV807M   2.0
    @154  HV807Y   4.0
    @158  HV807C   4.0
    @162  HV807A   5.0
    @167  HV201    2.0
    @169  HV202    2.0
    @171  HV201A   1.0
    @172  HV204    3.0
    @175  HV205    2.0
    @177  HV206    1.0
    @178  HV207    1.0
    @179  HV208    1.0
    @180  HV209    1.0
    @181  HV210    1.0
    @182  HV211    1.0
    @183  HV212    1.0
    @184  HV213    2.0
    @186  HV214    2.0
    @188  HV215    2.0
    @190  HV216    2.0
    @192  HV217    1.0
    @193  HV218    2.0
    @195  HV219    1.0
    @196  HV220    2.0
    @198  HV221    1.0
    @199  HV225    1.0
    @200  HV226    2.0
    @202  HV227    1.0
    @203  HV228    1.0
    @204  HV230A   1.0
    @205  HV230B   1.0
    @206  HV232    1.0
    @207  HV232B   1.0
    @208  HV232C   1.0
    @209  HV232D   1.0
    @210  HV232E   1.0
    @211  HV232Y   1.0
    @212  HV234    3.0
    @215  HV234A   1.0
    @216  HV235    1.0
    @217  HV236    1.0
    @218  HV237    1.0
    @219  HV237A   1.0
    @220  HV237B   1.0
    @221  HV237C   1.0
    @222  HV237D   1.0
    @223  HV237E   1.0
    @224  HV237F   1.0
    @225  HV237G   1.0
    @226  HV237H   1.0
    @227  HV237I   1.0
    @228  HV237J   1.0
    @229  HV237K   1.0
    @230  HV237X   1.0
    @231  HV237Z   1.0
    @232  HV238    2.0
    @234  HV238A   1.0
    @235  HV239    1.0
    @236  HV240    1.0
    @237  HV241    1.0
    @238  HV242    1.0
    @239  HV243A   1.0
    @240  HV243B   1.0
    @241  HV243C   1.0
    @242  HV243D   1.0
    @243  HV243E   1.0
    @244  HV244    1.0
    @245  HV245    3.0
    @248  HV246    1.0
    @249  HV246A   2.0
    @251  HV246B   2.0
    @253  HV246C   2.0
    @255  HV246D   2.0
    @257  HV246E   2.0
    @259  HV246F   2.0
    @261  HV246G   2.0
    @263  HV246H   2.0
    @265  HV246I   2.0
    @267  HV246J   2.0
    @269  HV246K   2.0
    @271  HV247    1.0
    @272  HV252    1.0
    @273  HV253    1.0
    @274  HV253A   1.0
    @275  HV253B   1.0
    @276  HV253C   1.0
    @277  HV253D   1.0
    @278  HV253E   1.0
    @279  HV253F   1.0
    @280  HV253G   1.0
    @281  HV253H   1.0
    @282  HV253X   1.0
    @283  HV253Z   1.0
    @284  HV270    1.0
    @285  HV271    8.0
    @293  HV270A   1.0
    @294  HV271A   8.0
    @302  HML1     2.0
    @304  HML1A    1.0
    @305  HML2     2.0
    @307  SHLOCAL  1.0
    @308  SHELIG   1.0
    @309  SHADULT  2.0
    @311  SHLOCAL2 1.0
    @312  SH119    1.0
    @313  SH121B   1.0
    @314  SH121I   1.0
    @315  SH121J   1.0
    @316  SH121K   1.0
    @317  SH121L   1.0
    @318  SH121M   1.0
    @319  SH121N   1.0
    @320  SH122B   1.0
    @321  SH122C   1.0
    @322  SH122G   1.0
    @323  SH125    1.0
    @324  SH127A   1.0
    @325  HVIDX_01 2.0
    @327  HVIDX_02 2.0
    @329  HVIDX_03 2.0
    @331  HVIDX_04 2.0
    @333  HVIDX_05 2.0
    @335  HVIDX_06 2.0
    @337  HVIDX_07 2.0
    @339  HVIDX_08 2.0
    @341  HVIDX_09 2.0
    @343  HVIDX_10 2.0
    @345  HVIDX_11 2.0
    @347  HVIDX_12 2.0
    @349  HVIDX_13 2.0
    @351  HVIDX_14 2.0
    @353  HVIDX_15 2.0
    @355  HVIDX_16 2.0
    @357  HVIDX_17 2.0
    @359  HVIDX_18 2.0
    @361  HVIDX_19 2.0
    @363  HVIDX_20 2.0
    @365  HVIDX_21 2.0
    @367  HVIDX_22 2.0
    @369  HVIDX_23 2.0
    @371  HVIDX_24 2.0
    @373  HVIDX_25 2.0
    @375  HVIDX_26 2.0
    @377  HVIDX_27 2.0
    @379  HVIDX_28 2.0
    @381  HVIDX_29 2.0
    @383  HVIDX_30 2.0
    @385  HV101_01 2.0
    @387  HV101_02 2.0
    @389  HV101_03 2.0
    @391  HV101_04 2.0
    @393  HV101_05 2.0
    @395  HV101_06 2.0
    @397  HV101_07 2.0
    @399  HV101_08 2.0
    @401  HV101_09 2.0
    @403  HV101_10 2.0
    @405  HV101_11 2.0
    @407  HV101_12 2.0
    @409  HV101_13 2.0
    @411  HV101_14 2.0
    @413  HV101_15 2.0
    @415  HV101_16 2.0
    @417  HV101_17 2.0
    @419  HV101_18 2.0
    @421  HV101_19 2.0
    @423  HV101_20 2.0
    @425  HV101_21 2.0
    @427  HV101_22 2.0
    @429  HV101_23 2.0
    @431  HV101_24 2.0
    @433  HV101_25 2.0
    @435  HV101_26 2.0
    @437  HV101_27 2.0
    @439  HV101_28 2.0
    @441  HV101_29 2.0
    @443  HV101_30 2.0
    @445  HV102_01 1.0
    @446  HV102_02 1.0
    @447  HV102_03 1.0
    @448  HV102_04 1.0
    @449  HV102_05 1.0
    @450  HV102_06 1.0
    @451  HV102_07 1.0
    @452  HV102_08 1.0
    @453  HV102_09 1.0
    @454  HV102_10 1.0
    @455  HV102_11 1.0
    @456  HV102_12 1.0
    @457  HV102_13 1.0
    @458  HV102_14 1.0
    @459  HV102_15 1.0
    @460  HV102_16 1.0
    @461  HV102_17 1.0
    @462  HV102_18 1.0
    @463  HV102_19 1.0
    @464  HV102_20 1.0
    @465  HV102_21 1.0
    @466  HV102_22 1.0
    @467  HV102_23 1.0
    @468  HV102_24 1.0
    @469  HV102_25 1.0
    @470  HV102_26 1.0
    @471  HV102_27 1.0
    @472  HV102_28 1.0
    @473  HV102_29 1.0
    @474  HV102_30 1.0
    @475  HV103_01 1.0
    @476  HV103_02 1.0
    @477  HV103_03 1.0
    @478  HV103_04 1.0
    @479  HV103_05 1.0
    @480  HV103_06 1.0
    @481  HV103_07 1.0
    @482  HV103_08 1.0
    @483  HV103_09 1.0
    @484  HV103_10 1.0
    @485  HV103_11 1.0
    @486  HV103_12 1.0
    @487  HV103_13 1.0
    @488  HV103_14 1.0
    @489  HV103_15 1.0
    @490  HV103_16 1.0
    @491  HV103_17 1.0
    @492  HV103_18 1.0
    @493  HV103_19 1.0
    @494  HV103_20 1.0
    @495  HV103_21 1.0
    @496  HV103_22 1.0
    @497  HV103_23 1.0
    @498  HV103_24 1.0
    @499  HV103_25 1.0
    @500  HV103_26 1.0
    @501  HV103_27 1.0
    @502  HV103_28 1.0
    @503  HV103_29 1.0
    @504  HV103_30 1.0
    @505  HV104_01 1.0
    @506  HV104_02 1.0
    @507  HV104_03 1.0
    @508  HV104_04 1.0
    @509  HV104_05 1.0
    @510  HV104_06 1.0
    @511  HV104_07 1.0
    @512  HV104_08 1.0
    @513  HV104_09 1.0
    @514  HV104_10 1.0
    @515  HV104_11 1.0
    @516  HV104_12 1.0
    @517  HV104_13 1.0
    @518  HV104_14 1.0
    @519  HV104_15 1.0
    @520  HV104_16 1.0
    @521  HV104_17 1.0
    @522  HV104_18 1.0
    @523  HV104_19 1.0
    @524  HV104_20 1.0
    @525  HV104_21 1.0
    @526  HV104_22 1.0
    @527  HV104_23 1.0
    @528  HV104_24 1.0
    @529  HV104_25 1.0
    @530  HV104_26 1.0
    @531  HV104_27 1.0
    @532  HV104_28 1.0
    @533  HV104_29 1.0
    @534  HV104_30 1.0
    @535  HV105_01 2.0
    @537  HV105_02 2.0
    @539  HV105_03 2.0
    @541  HV105_04 2.0
    @543  HV105_05 2.0
    @545  HV105_06 2.0
    @547  HV105_07 2.0
    @549  HV105_08 2.0
    @551  HV105_09 2.0
    @553  HV105_10 2.0
    @555  HV105_11 2.0
    @557  HV105_12 2.0
    @559  HV105_13 2.0
    @561  HV105_14 2.0
    @563  HV105_15 2.0
    @565  HV105_16 2.0
    @567  HV105_17 2.0
    @569  HV105_18 2.0
    @571  HV105_19 2.0
    @573  HV105_20 2.0
    @575  HV105_21 2.0
    @577  HV105_22 2.0
    @579  HV105_23 2.0
    @581  HV105_24 2.0
    @583  HV105_25 2.0
    @585  HV105_26 2.0
    @587  HV105_27 2.0
    @589  HV105_28 2.0
    @591  HV105_29 2.0
    @593  HV105_30 2.0
    @595  HV106_01 1.0
    @596  HV106_02 1.0
    @597  HV106_03 1.0
    @598  HV106_04 1.0
    @599  HV106_05 1.0
    @600  HV106_06 1.0
    @601  HV106_07 1.0
    @602  HV106_08 1.0
    @603  HV106_09 1.0
    @604  HV106_10 1.0
    @605  HV106_11 1.0
    @606  HV106_12 1.0
    @607  HV106_13 1.0
    @608  HV106_14 1.0
    @609  HV106_15 1.0
    @610  HV106_16 1.0
    @611  HV106_17 1.0
    @612  HV106_18 1.0
    @613  HV106_19 1.0
    @614  HV106_20 1.0
    @615  HV106_21 1.0
    @616  HV106_22 1.0
    @617  HV106_23 1.0
    @618  HV106_24 1.0
    @619  HV106_25 1.0
    @620  HV106_26 1.0
    @621  HV106_27 1.0
    @622  HV106_28 1.0
    @623  HV106_29 1.0
    @624  HV106_30 1.0
    @625  HV107_01 2.0
    @627  HV107_02 2.0
    @629  HV107_03 2.0
    @631  HV107_04 2.0
    @633  HV107_05 2.0
    @635  HV107_06 2.0
    @637  HV107_07 2.0
    @639  HV107_08 2.0
    @641  HV107_09 2.0
    @643  HV107_10 2.0
    @645  HV107_11 2.0
    @647  HV107_12 2.0
    @649  HV107_13 2.0
    @651  HV107_14 2.0
    @653  HV107_15 2.0
    @655  HV107_16 2.0
    @657  HV107_17 2.0
    @659  HV107_18 2.0
    @661  HV107_19 2.0
    @663  HV107_20 2.0
    @665  HV107_21 2.0
    @667  HV107_22 2.0
    @669  HV107_23 2.0
    @671  HV107_24 2.0
    @673  HV107_25 2.0
    @675  HV107_26 2.0
    @677  HV107_27 2.0
    @679  HV107_28 2.0
    @681  HV107_29 2.0
    @683  HV107_30 2.0
    @685  HV108_01 2.0
    @687  HV108_02 2.0
    @689  HV108_03 2.0
    @691  HV108_04 2.0
    @693  HV108_05 2.0
    @695  HV108_06 2.0
    @697  HV108_07 2.0
    @699  HV108_08 2.0
    @701  HV108_09 2.0
    @703  HV108_10 2.0
    @705  HV108_11 2.0
    @707  HV108_12 2.0
    @709  HV108_13 2.0
    @711  HV108_14 2.0
    @713  HV108_15 2.0
    @715  HV108_16 2.0
    @717  HV108_17 2.0
    @719  HV108_18 2.0
    @721  HV108_19 2.0
    @723  HV108_20 2.0
    @725  HV108_21 2.0
    @727  HV108_22 2.0
    @729  HV108_23 2.0
    @731  HV108_24 2.0
    @733  HV108_25 2.0
    @735  HV108_26 2.0
    @737  HV108_27 2.0
    @739  HV108_28 2.0
    @741  HV108_29 2.0
    @743  HV108_30 2.0
    @745  HV109_01 1.0
    @746  HV109_02 1.0
    @747  HV109_03 1.0
    @748  HV109_04 1.0
    @749  HV109_05 1.0
    @750  HV109_06 1.0
    @751  HV109_07 1.0
    @752  HV109_08 1.0
    @753  HV109_09 1.0
    @754  HV109_10 1.0
    @755  HV109_11 1.0
    @756  HV109_12 1.0
    @757  HV109_13 1.0
    @758  HV109_14 1.0
    @759  HV109_15 1.0
    @760  HV109_16 1.0
    @761  HV109_17 1.0
    @762  HV109_18 1.0
    @763  HV109_19 1.0
    @764  HV109_20 1.0
    @765  HV109_21 1.0
    @766  HV109_22 1.0
    @767  HV109_23 1.0
    @768  HV109_24 1.0
    @769  HV109_25 1.0
    @770  HV109_26 1.0
    @771  HV109_27 1.0
    @772  HV109_28 1.0
    @773  HV109_29 1.0
    @774  HV109_30 1.0
    @775  HV110_01 1.0
    @776  HV110_02 1.0
    @777  HV110_03 1.0
    @778  HV110_04 1.0
    @779  HV110_05 1.0
    @780  HV110_06 1.0
    @781  HV110_07 1.0
    @782  HV110_08 1.0
    @783  HV110_09 1.0
    @784  HV110_10 1.0
    @785  HV110_11 1.0
    @786  HV110_12 1.0
    @787  HV110_13 1.0
    @788  HV110_14 1.0
    @789  HV110_15 1.0
    @790  HV110_16 1.0
    @791  HV110_17 1.0
    @792  HV110_18 1.0
    @793  HV110_19 1.0
    @794  HV110_20 1.0
    @795  HV110_21 1.0
    @796  HV110_22 1.0
    @797  HV110_23 1.0
    @798  HV110_24 1.0
    @799  HV110_25 1.0
    @800  HV110_26 1.0
    @801  HV110_27 1.0
    @802  HV110_28 1.0
    @803  HV110_29 1.0
    @804  HV110_30 1.0
    @805  HV111_01 1.0
    @806  HV111_02 1.0
    @807  HV111_03 1.0
    @808  HV111_04 1.0
    @809  HV111_05 1.0
    @810  HV111_06 1.0
    @811  HV111_07 1.0
    @812  HV111_08 1.0
    @813  HV111_09 1.0
    @814  HV111_10 1.0
    @815  HV111_11 1.0
    @816  HV111_12 1.0
    @817  HV111_13 1.0
    @818  HV111_14 1.0
    @819  HV111_15 1.0
    @820  HV111_16 1.0
    @821  HV111_17 1.0
    @822  HV111_18 1.0
    @823  HV111_19 1.0
    @824  HV111_20 1.0
    @825  HV111_21 1.0
    @826  HV111_22 1.0
    @827  HV111_23 1.0
    @828  HV111_24 1.0
    @829  HV111_25 1.0
    @830  HV111_26 1.0
    @831  HV111_27 1.0
    @832  HV111_28 1.0
    @833  HV111_29 1.0
    @834  HV111_30 1.0
    @835  HV112_01 2.0
    @837  HV112_02 2.0
    @839  HV112_03 2.0
    @841  HV112_04 2.0
    @843  HV112_05 2.0
    @845  HV112_06 2.0
    @847  HV112_07 2.0
    @849  HV112_08 2.0
    @851  HV112_09 2.0
    @853  HV112_10 2.0
    @855  HV112_11 2.0
    @857  HV112_12 2.0
    @859  HV112_13 2.0
    @861  HV112_14 2.0
    @863  HV112_15 2.0
    @865  HV112_16 2.0
    @867  HV112_17 2.0
    @869  HV112_18 2.0
    @871  HV112_19 2.0
    @873  HV112_20 2.0
    @875  HV112_21 2.0
    @877  HV112_22 2.0
    @879  HV112_23 2.0
    @881  HV112_24 2.0
    @883  HV112_25 2.0
    @885  HV112_26 2.0
    @887  HV112_27 2.0
    @889  HV112_28 2.0
    @891  HV112_29 2.0
    @893  HV112_30 2.0
    @895  HV113_01 1.0
    @896  HV113_02 1.0
    @897  HV113_03 1.0
    @898  HV113_04 1.0
    @899  HV113_05 1.0
    @900  HV113_06 1.0
    @901  HV113_07 1.0
    @902  HV113_08 1.0
    @903  HV113_09 1.0
    @904  HV113_10 1.0
    @905  HV113_11 1.0
    @906  HV113_12 1.0
    @907  HV113_13 1.0
    @908  HV113_14 1.0
    @909  HV113_15 1.0
    @910  HV113_16 1.0
    @911  HV113_17 1.0
    @912  HV113_18 1.0
    @913  HV113_19 1.0
    @914  HV113_20 1.0
    @915  HV113_21 1.0
    @916  HV113_22 1.0
    @917  HV113_23 1.0
    @918  HV113_24 1.0
    @919  HV113_25 1.0
    @920  HV113_26 1.0
    @921  HV113_27 1.0
    @922  HV113_28 1.0
    @923  HV113_29 1.0
    @924  HV113_30 1.0
    @925  HV114_01 2.0
    @927  HV114_02 2.0
    @929  HV114_03 2.0
    @931  HV114_04 2.0
    @933  HV114_05 2.0
    @935  HV114_06 2.0
    @937  HV114_07 2.0
    @939  HV114_08 2.0
    @941  HV114_09 2.0
    @943  HV114_10 2.0
    @945  HV114_11 2.0
    @947  HV114_12 2.0
    @949  HV114_13 2.0
    @951  HV114_14 2.0
    @953  HV114_15 2.0
    @955  HV114_16 2.0
    @957  HV114_17 2.0
    @959  HV114_18 2.0
    @961  HV114_19 2.0
    @963  HV114_20 2.0
    @965  HV114_21 2.0
    @967  HV114_22 2.0
    @969  HV114_23 2.0
    @971  HV114_24 2.0
    @973  HV114_25 2.0
    @975  HV114_26 2.0
    @977  HV114_27 2.0
    @979  HV114_28 2.0
    @981  HV114_29 2.0
    @983  HV114_30 2.0
    @985  HV115_01 1.0
    @986  HV115_02 1.0
    @987  HV115_03 1.0
    @988  HV115_04 1.0
    @989  HV115_05 1.0
    @990  HV115_06 1.0
    @991  HV115_07 1.0
    @992  HV115_08 1.0
    @993  HV115_09 1.0
    @994  HV115_10 1.0
    @995  HV115_11 1.0
    @996  HV115_12 1.0
    @997  HV115_13 1.0
    @998  HV115_14 1.0
    @999  HV115_15 1.0
    @1000 HV115_16 1.0
    @1001 HV115_17 1.0
    @1002 HV115_18 1.0
    @1003 HV115_19 1.0
    @1004 HV115_20 1.0
    @1005 HV115_21 1.0
    @1006 HV115_22 1.0
    @1007 HV115_23 1.0
    @1008 HV115_24 1.0
    @1009 HV115_25 1.0
    @1010 HV115_26 1.0
    @1011 HV115_27 1.0
    @1012 HV115_28 1.0
    @1013 HV115_29 1.0
    @1014 HV115_30 1.0
    @1015 HV116_01 1.0
    @1016 HV116_02 1.0
    @1017 HV116_03 1.0
    @1018 HV116_04 1.0
    @1019 HV116_05 1.0
    @1020 HV116_06 1.0
    @1021 HV116_07 1.0
    @1022 HV116_08 1.0
    @1023 HV116_09 1.0
    @1024 HV116_10 1.0
    @1025 HV116_11 1.0
    @1026 HV116_12 1.0
    @1027 HV116_13 1.0
    @1028 HV116_14 1.0
    @1029 HV116_15 1.0
    @1030 HV116_16 1.0
    @1031 HV116_17 1.0
    @1032 HV116_18 1.0
    @1033 HV116_19 1.0
    @1034 HV116_20 1.0
    @1035 HV116_21 1.0
    @1036 HV116_22 1.0
    @1037 HV116_23 1.0
    @1038 HV116_24 1.0
    @1039 HV116_25 1.0
    @1040 HV116_26 1.0
    @1041 HV116_27 1.0
    @1042 HV116_28 1.0
    @1043 HV116_29 1.0
    @1044 HV116_30 1.0
    @1045 HV117_01 1.0
    @1046 HV117_02 1.0
    @1047 HV117_03 1.0
    @1048 HV117_04 1.0
    @1049 HV117_05 1.0
    @1050 HV117_06 1.0
    @1051 HV117_07 1.0
    @1052 HV117_08 1.0
    @1053 HV117_09 1.0
    @1054 HV117_10 1.0
    @1055 HV117_11 1.0
    @1056 HV117_12 1.0
    @1057 HV117_13 1.0
    @1058 HV117_14 1.0
    @1059 HV117_15 1.0
    @1060 HV117_16 1.0
    @1061 HV117_17 1.0
    @1062 HV117_18 1.0
    @1063 HV117_19 1.0
    @1064 HV117_20 1.0
    @1065 HV117_21 1.0
    @1066 HV117_22 1.0
    @1067 HV117_23 1.0
    @1068 HV117_24 1.0
    @1069 HV117_25 1.0
    @1070 HV117_26 1.0
    @1071 HV117_27 1.0
    @1072 HV117_28 1.0
    @1073 HV117_29 1.0
    @1074 HV117_30 1.0
    @1075 HV118_01 1.0
    @1076 HV118_02 1.0
    @1077 HV118_03 1.0
    @1078 HV118_04 1.0
    @1079 HV118_05 1.0
    @1080 HV118_06 1.0
    @1081 HV118_07 1.0
    @1082 HV118_08 1.0
    @1083 HV118_09 1.0
    @1084 HV118_10 1.0
    @1085 HV118_11 1.0
    @1086 HV118_12 1.0
    @1087 HV118_13 1.0
    @1088 HV118_14 1.0
    @1089 HV118_15 1.0
    @1090 HV118_16 1.0
    @1091 HV118_17 1.0
    @1092 HV118_18 1.0
    @1093 HV118_19 1.0
    @1094 HV118_20 1.0
    @1095 HV118_21 1.0
    @1096 HV118_22 1.0
    @1097 HV118_23 1.0
    @1098 HV118_24 1.0
    @1099 HV118_25 1.0
    @1100 HV118_26 1.0
    @1101 HV118_27 1.0
    @1102 HV118_28 1.0
    @1103 HV118_29 1.0
    @1104 HV118_30 1.0
    @1105 HV120_01 1.0
    @1106 HV120_02 1.0
    @1107 HV120_03 1.0
    @1108 HV120_04 1.0
    @1109 HV120_05 1.0
    @1110 HV120_06 1.0
    @1111 HV120_07 1.0
    @1112 HV120_08 1.0
    @1113 HV120_09 1.0
    @1114 HV120_10 1.0
    @1115 HV120_11 1.0
    @1116 HV120_12 1.0
    @1117 HV120_13 1.0
    @1118 HV120_14 1.0
    @1119 HV120_15 1.0
    @1120 HV120_16 1.0
    @1121 HV120_17 1.0
    @1122 HV120_18 1.0
    @1123 HV120_19 1.0
    @1124 HV120_20 1.0
    @1125 HV120_21 1.0
    @1126 HV120_22 1.0
    @1127 HV120_23 1.0
    @1128 HV120_24 1.0
    @1129 HV120_25 1.0
    @1130 HV120_26 1.0
    @1131 HV120_27 1.0
    @1132 HV120_28 1.0
    @1133 HV120_29 1.0
    @1134 HV120_30 1.0
    @1135 HV121_01 1.0
    @1136 HV121_02 1.0
    @1137 HV121_03 1.0
    @1138 HV121_04 1.0
    @1139 HV121_05 1.0
    @1140 HV121_06 1.0
    @1141 HV121_07 1.0
    @1142 HV121_08 1.0
    @1143 HV121_09 1.0
    @1144 HV121_10 1.0
    @1145 HV121_11 1.0
    @1146 HV121_12 1.0
    @1147 HV121_13 1.0
    @1148 HV121_14 1.0
    @1149 HV121_15 1.0
    @1150 HV121_16 1.0
    @1151 HV121_17 1.0
    @1152 HV121_18 1.0
    @1153 HV121_19 1.0
    @1154 HV121_20 1.0
    @1155 HV121_21 1.0
    @1156 HV121_22 1.0
    @1157 HV121_23 1.0
    @1158 HV121_24 1.0
    @1159 HV121_25 1.0
    @1160 HV121_26 1.0
    @1161 HV121_27 1.0
    @1162 HV121_28 1.0
    @1163 HV121_29 1.0
    @1164 HV121_30 1.0
    @1165 HV122_01 1.0
    @1166 HV122_02 1.0
    @1167 HV122_03 1.0
    @1168 HV122_04 1.0
    @1169 HV122_05 1.0
    @1170 HV122_06 1.0
    @1171 HV122_07 1.0
    @1172 HV122_08 1.0
    @1173 HV122_09 1.0
    @1174 HV122_10 1.0
    @1175 HV122_11 1.0
    @1176 HV122_12 1.0
    @1177 HV122_13 1.0
    @1178 HV122_14 1.0
    @1179 HV122_15 1.0
    @1180 HV122_16 1.0
    @1181 HV122_17 1.0
    @1182 HV122_18 1.0
    @1183 HV122_19 1.0
    @1184 HV122_20 1.0
    @1185 HV122_21 1.0
    @1186 HV122_22 1.0
    @1187 HV122_23 1.0
    @1188 HV122_24 1.0
    @1189 HV122_25 1.0
    @1190 HV122_26 1.0
    @1191 HV122_27 1.0
    @1192 HV122_28 1.0
    @1193 HV122_29 1.0
    @1194 HV122_30 1.0
    @1195 HV123_01 2.0
    @1197 HV123_02 2.0
    @1199 HV123_03 2.0
    @1201 HV123_04 2.0
    @1203 HV123_05 2.0
    @1205 HV123_06 2.0
    @1207 HV123_07 2.0
    @1209 HV123_08 2.0
    @1211 HV123_09 2.0
    @1213 HV123_10 2.0
    @1215 HV123_11 2.0
    @1217 HV123_12 2.0
    @1219 HV123_13 2.0
    @1221 HV123_14 2.0
    @1223 HV123_15 2.0
    @1225 HV123_16 2.0
    @1227 HV123_17 2.0
    @1229 HV123_18 2.0
    @1231 HV123_19 2.0
    @1233 HV123_20 2.0
    @1235 HV123_21 2.0
    @1237 HV123_22 2.0
    @1239 HV123_23 2.0
    @1241 HV123_24 2.0
    @1243 HV123_25 2.0
    @1245 HV123_26 2.0
    @1247 HV123_27 2.0
    @1249 HV123_28 2.0
    @1251 HV123_29 2.0
    @1253 HV123_30 2.0
    @1255 HV124_01 2.0
    @1257 HV124_02 2.0
    @1259 HV124_03 2.0
    @1261 HV124_04 2.0
    @1263 HV124_05 2.0
    @1265 HV124_06 2.0
    @1267 HV124_07 2.0
    @1269 HV124_08 2.0
    @1271 HV124_09 2.0
    @1273 HV124_10 2.0
    @1275 HV124_11 2.0
    @1277 HV124_12 2.0
    @1279 HV124_13 2.0
    @1281 HV124_14 2.0
    @1283 HV124_15 2.0
    @1285 HV124_16 2.0
    @1287 HV124_17 2.0
    @1289 HV124_18 2.0
    @1291 HV124_19 2.0
    @1293 HV124_20 2.0
    @1295 HV124_21 2.0
    @1297 HV124_22 2.0
    @1299 HV124_23 2.0
    @1301 HV124_24 2.0
    @1303 HV124_25 2.0
    @1305 HV124_26 2.0
    @1307 HV124_27 2.0
    @1309 HV124_28 2.0
    @1311 HV124_29 2.0
    @1313 HV124_30 2.0
    @1315 HV125_01 1.0
    @1316 HV125_02 1.0
    @1317 HV125_03 1.0
    @1318 HV125_04 1.0
    @1319 HV125_05 1.0
    @1320 HV125_06 1.0
    @1321 HV125_07 1.0
    @1322 HV125_08 1.0
    @1323 HV125_09 1.0
    @1324 HV125_10 1.0
    @1325 HV125_11 1.0
    @1326 HV125_12 1.0
    @1327 HV125_13 1.0
    @1328 HV125_14 1.0
    @1329 HV125_15 1.0
    @1330 HV125_16 1.0
    @1331 HV125_17 1.0
    @1332 HV125_18 1.0
    @1333 HV125_19 1.0
    @1334 HV125_20 1.0
    @1335 HV125_21 1.0
    @1336 HV125_22 1.0
    @1337 HV125_23 1.0
    @1338 HV125_24 1.0
    @1339 HV125_25 1.0
    @1340 HV125_26 1.0
    @1341 HV125_27 1.0
    @1342 HV125_28 1.0
    @1343 HV125_29 1.0
    @1344 HV125_30 1.0
    @1345 HV126_01 1.0
    @1346 HV126_02 1.0
    @1347 HV126_03 1.0
    @1348 HV126_04 1.0
    @1349 HV126_05 1.0
    @1350 HV126_06 1.0
    @1351 HV126_07 1.0
    @1352 HV126_08 1.0
    @1353 HV126_09 1.0
    @1354 HV126_10 1.0
    @1355 HV126_11 1.0
    @1356 HV126_12 1.0
    @1357 HV126_13 1.0
    @1358 HV126_14 1.0
    @1359 HV126_15 1.0
    @1360 HV126_16 1.0
    @1361 HV126_17 1.0
    @1362 HV126_18 1.0
    @1363 HV126_19 1.0
    @1364 HV126_20 1.0
    @1365 HV126_21 1.0
    @1366 HV126_22 1.0
    @1367 HV126_23 1.0
    @1368 HV126_24 1.0
    @1369 HV126_25 1.0
    @1370 HV126_26 1.0
    @1371 HV126_27 1.0
    @1372 HV126_28 1.0
    @1373 HV126_29 1.0
    @1374 HV126_30 1.0
    @1375 HV127_01 2.0
    @1377 HV127_02 2.0
    @1379 HV127_03 2.0
    @1381 HV127_04 2.0
    @1383 HV127_05 2.0
    @1385 HV127_06 2.0
    @1387 HV127_07 2.0
    @1389 HV127_08 2.0
    @1391 HV127_09 2.0
    @1393 HV127_10 2.0
    @1395 HV127_11 2.0
    @1397 HV127_12 2.0
    @1399 HV127_13 2.0
    @1401 HV127_14 2.0
    @1403 HV127_15 2.0
    @1405 HV127_16 2.0
    @1407 HV127_17 2.0
    @1409 HV127_18 2.0
    @1411 HV127_19 2.0
    @1413 HV127_20 2.0
    @1415 HV127_21 2.0
    @1417 HV127_22 2.0
    @1419 HV127_23 2.0
    @1421 HV127_24 2.0
    @1423 HV127_25 2.0
    @1425 HV127_26 2.0
    @1427 HV127_27 2.0
    @1429 HV127_28 2.0
    @1431 HV127_29 2.0
    @1433 HV127_30 2.0
    @1435 HV128_01 2.0
    @1437 HV128_02 2.0
    @1439 HV128_03 2.0
    @1441 HV128_04 2.0
    @1443 HV128_05 2.0
    @1445 HV128_06 2.0
    @1447 HV128_07 2.0
    @1449 HV128_08 2.0
    @1451 HV128_09 2.0
    @1453 HV128_10 2.0
    @1455 HV128_11 2.0
    @1457 HV128_12 2.0
    @1459 HV128_13 2.0
    @1461 HV128_14 2.0
    @1463 HV128_15 2.0
    @1465 HV128_16 2.0
    @1467 HV128_17 2.0
    @1469 HV128_18 2.0
    @1471 HV128_19 2.0
    @1473 HV128_20 2.0
    @1475 HV128_21 2.0
    @1477 HV128_22 2.0
    @1479 HV128_23 2.0
    @1481 HV128_24 2.0
    @1483 HV128_25 2.0
    @1485 HV128_26 2.0
    @1487 HV128_27 2.0
    @1489 HV128_28 2.0
    @1491 HV128_29 2.0
    @1493 HV128_30 2.0
    @1495 HV129_01 1.0
    @1496 HV129_02 1.0
    @1497 HV129_03 1.0
    @1498 HV129_04 1.0
    @1499 HV129_05 1.0
    @1500 HV129_06 1.0
    @1501 HV129_07 1.0
    @1502 HV129_08 1.0
    @1503 HV129_09 1.0
    @1504 HV129_10 1.0
    @1505 HV129_11 1.0
    @1506 HV129_12 1.0
    @1507 HV129_13 1.0
    @1508 HV129_14 1.0
    @1509 HV129_15 1.0
    @1510 HV129_16 1.0
    @1511 HV129_17 1.0
    @1512 HV129_18 1.0
    @1513 HV129_19 1.0
    @1514 HV129_20 1.0
    @1515 HV129_21 1.0
    @1516 HV129_22 1.0
    @1517 HV129_23 1.0
    @1518 HV129_24 1.0
    @1519 HV129_25 1.0
    @1520 HV129_26 1.0
    @1521 HV129_27 1.0
    @1522 HV129_28 1.0
    @1523 HV129_29 1.0
    @1524 HV129_30 1.0
    @1525 HV140_01 1.0
    @1526 HV140_02 1.0
    @1527 HV140_03 1.0
    @1528 HV140_04 1.0
    @1529 HV140_05 1.0
    @1530 HV140_06 1.0
    @1531 HV140_07 1.0
    @1532 HV140_08 1.0
    @1533 HV140_09 1.0
    @1534 HV140_10 1.0
    @1535 HV140_11 1.0
    @1536 HV140_12 1.0
    @1537 HV140_13 1.0
    @1538 HV140_14 1.0
    @1539 HV140_15 1.0
    @1540 HV140_16 1.0
    @1541 HV140_17 1.0
    @1542 HV140_18 1.0
    @1543 HV140_19 1.0
    @1544 HV140_20 1.0
    @1545 HV140_21 1.0
    @1546 HV140_22 1.0
    @1547 HV140_23 1.0
    @1548 HV140_24 1.0
    @1549 HV140_25 1.0
    @1550 HV140_26 1.0
    @1551 HV140_27 1.0
    @1552 HV140_28 1.0
    @1553 HV140_29 1.0
    @1554 HV140_30 1.0
    @1555 IDXH4_01 2.0
    @1557 IDXH4_02 2.0
    @1559 IDXH4_03 2.0
    @1561 IDXH4_04 2.0
    @1563 IDXH4_05 2.0
    @1565 IDXH4_06 2.0
    @1567 IDXH4_07 2.0
    @1569 IDXH4_08 2.0
    @1571 IDXH4_09 2.0
    @1573 IDXH4_10 2.0
    @1575 IDXH4_11 2.0
    @1577 IDXH4_12 2.0
    @1579 IDXH4_13 2.0
    @1581 IDXH4_14 2.0
    @1583 IDXH4_15 2.0
    @1585 IDXH4_16 2.0
    @1587 IDXH4_17 2.0
    @1589 IDXH4_18 2.0
    @1591 IDXH4_19 2.0
    @1593 IDXH4_20 2.0
    @1595 IDXH4_21 2.0
    @1597 IDXH4_22 2.0
    @1599 IDXH4_23 2.0
    @1601 IDXH4_24 2.0
    @1603 IDXH4_25 2.0
    @1605 IDXH4_26 2.0
    @1607 IDXH4_27 2.0
    @1609 IDXH4_28 2.0
    @1611 IDXH4_29 2.0
    @1613 IDXH4_30 2.0
    @1615 SH13_01  1.0
    @1616 SH13_02  1.0
    @1617 SH13_03  1.0
    @1618 SH13_04  1.0
    @1619 SH13_05  1.0
    @1620 SH13_06  1.0
    @1621 SH13_07  1.0
    @1622 SH13_08  1.0
    @1623 SH13_09  1.0
    @1624 SH13_10  1.0
    @1625 SH13_11  1.0
    @1626 SH13_12  1.0
    @1627 SH13_13  1.0
    @1628 SH13_14  1.0
    @1629 SH13_15  1.0
    @1630 SH13_16  1.0
    @1631 SH13_17  1.0
    @1632 SH13_18  1.0
    @1633 SH13_19  1.0
    @1634 SH13_20  1.0
    @1635 SH13_21  1.0
    @1636 SH13_22  1.0
    @1637 SH13_23  1.0
    @1638 SH13_24  1.0
    @1639 SH13_25  1.0
    @1640 SH13_26  1.0
    @1641 SH13_27  1.0
    @1642 SH13_28  1.0
    @1643 SH13_29  1.0
    @1644 SH13_30  1.0
    @1645 SH14A_01 1.0
    @1646 SH14A_02 1.0
    @1647 SH14A_03 1.0
    @1648 SH14A_04 1.0
    @1649 SH14A_05 1.0
    @1650 SH14A_06 1.0
    @1651 SH14A_07 1.0
    @1652 SH14A_08 1.0
    @1653 SH14A_09 1.0
    @1654 SH14A_10 1.0
    @1655 SH14A_11 1.0
    @1656 SH14A_12 1.0
    @1657 SH14A_13 1.0
    @1658 SH14A_14 1.0
    @1659 SH14A_15 1.0
    @1660 SH14A_16 1.0
    @1661 SH14A_17 1.0
    @1662 SH14A_18 1.0
    @1663 SH14A_19 1.0
    @1664 SH14A_20 1.0
    @1665 SH14A_21 1.0
    @1666 SH14A_22 1.0
    @1667 SH14A_23 1.0
    @1668 SH14A_24 1.0
    @1669 SH14A_25 1.0
    @1670 SH14A_26 1.0
    @1671 SH14A_27 1.0
    @1672 SH14A_28 1.0
    @1673 SH14A_29 1.0
    @1674 SH14A_30 1.0
    @1675 SH15_01  1.0
    @1676 SH15_02  1.0
    @1677 SH15_03  1.0
    @1678 SH15_04  1.0
    @1679 SH15_05  1.0
    @1680 SH15_06  1.0
    @1681 SH15_07  1.0
    @1682 SH15_08  1.0
    @1683 SH15_09  1.0
    @1684 SH15_10  1.0
    @1685 SH15_11  1.0
    @1686 SH15_12  1.0
    @1687 SH15_13  1.0
    @1688 SH15_14  1.0
    @1689 SH15_15  1.0
    @1690 SH15_16  1.0
    @1691 SH15_17  1.0
    @1692 SH15_18  1.0
    @1693 SH15_19  1.0
    @1694 SH15_20  1.0
    @1695 SH15_21  1.0
    @1696 SH15_22  1.0
    @1697 SH15_23  1.0
    @1698 SH15_24  1.0
    @1699 SH15_25  1.0
    @1700 SH15_26  1.0
    @1701 SH15_27  1.0
    @1702 SH15_28  1.0
    @1703 SH15_29  1.0
    @1704 SH15_30  1.0
    @1705 SH16_01  1.0
    @1706 SH16_02  1.0
    @1707 SH16_03  1.0
    @1708 SH16_04  1.0
    @1709 SH16_05  1.0
    @1710 SH16_06  1.0
    @1711 SH16_07  1.0
    @1712 SH16_08  1.0
    @1713 SH16_09  1.0
    @1714 SH16_10  1.0
    @1715 SH16_11  1.0
    @1716 SH16_12  1.0
    @1717 SH16_13  1.0
    @1718 SH16_14  1.0
    @1719 SH16_15  1.0
    @1720 SH16_16  1.0
    @1721 SH16_17  1.0
    @1722 SH16_18  1.0
    @1723 SH16_19  1.0
    @1724 SH16_20  1.0
    @1725 SH16_21  1.0
    @1726 SH16_22  1.0
    @1727 SH16_23  1.0
    @1728 SH16_24  1.0
    @1729 SH16_25  1.0
    @1730 SH16_26  1.0
    @1731 SH16_27  1.0
    @1732 SH16_28  1.0
    @1733 SH16_29  1.0
    @1734 SH16_30  1.0
    @1735 SH18_01  1.0
    @1736 SH18_02  1.0
    @1737 SH18_03  1.0
    @1738 SH18_04  1.0
    @1739 SH18_05  1.0
    @1740 SH18_06  1.0
    @1741 SH18_07  1.0
    @1742 SH18_08  1.0
    @1743 SH18_09  1.0
    @1744 SH18_10  1.0
    @1745 SH18_11  1.0
    @1746 SH18_12  1.0
    @1747 SH18_13  1.0
    @1748 SH18_14  1.0
    @1749 SH18_15  1.0
    @1750 SH18_16  1.0
    @1751 SH18_17  1.0
    @1752 SH18_18  1.0
    @1753 SH18_19  1.0
    @1754 SH18_20  1.0
    @1755 SH18_21  1.0
    @1756 SH18_22  1.0
    @1757 SH18_23  1.0
    @1758 SH18_24  1.0
    @1759 SH18_25  1.0
    @1760 SH18_26  1.0
    @1761 SH18_27  1.0
    @1762 SH18_28  1.0
    @1763 SH18_29  1.0
    @1764 SH18_30  1.0
    @1765 SH19_01  1.0
    @1766 SH19_02  1.0
    @1767 SH19_03  1.0
    @1768 SH19_04  1.0
    @1769 SH19_05  1.0
    @1770 SH19_06  1.0
    @1771 SH19_07  1.0
    @1772 SH19_08  1.0
    @1773 SH19_09  1.0
    @1774 SH19_10  1.0
    @1775 SH19_11  1.0
    @1776 SH19_12  1.0
    @1777 SH19_13  1.0
    @1778 SH19_14  1.0
    @1779 SH19_15  1.0
    @1780 SH19_16  1.0
    @1781 SH19_17  1.0
    @1782 SH19_18  1.0
    @1783 SH19_19  1.0
    @1784 SH19_20  1.0
    @1785 SH19_21  1.0
    @1786 SH19_22  1.0
    @1787 SH19_23  1.0
    @1788 SH19_24  1.0
    @1789 SH19_25  1.0
    @1790 SH19_26  1.0
    @1791 SH19_27  1.0
    @1792 SH19_28  1.0
    @1793 SH19_29  1.0
    @1794 SH19_30  1.0
    @1795 SH20_01  1.0
    @1796 SH20_02  1.0
    @1797 SH20_03  1.0
    @1798 SH20_04  1.0
    @1799 SH20_05  1.0
    @1800 SH20_06  1.0
    @1801 SH20_07  1.0
    @1802 SH20_08  1.0
    @1803 SH20_09  1.0
    @1804 SH20_10  1.0
    @1805 SH20_11  1.0
    @1806 SH20_12  1.0
    @1807 SH20_13  1.0
    @1808 SH20_14  1.0
    @1809 SH20_15  1.0
    @1810 SH20_16  1.0
    @1811 SH20_17  1.0
    @1812 SH20_18  1.0
    @1813 SH20_19  1.0
    @1814 SH20_20  1.0
    @1815 SH20_21  1.0
    @1816 SH20_22  1.0
    @1817 SH20_23  1.0
    @1818 SH20_24  1.0
    @1819 SH20_25  1.0
    @1820 SH20_26  1.0
    @1821 SH20_27  1.0
    @1822 SH20_28  1.0
    @1823 SH20_29  1.0
    @1824 SH20_30  1.0
    @1825 HA0_1    2.0
    @1827 HA0_2    2.0
    @1829 HA0_3    2.0
    @1831 HA0_4    2.0
    @1833 HA0_5    2.0
    @1835 HA0_6    2.0
    @1837 HA1_1    2.0
    @1839 HA1_2    2.0
    @1841 HA1_3    2.0
    @1843 HA1_4    2.0
    @1845 HA1_5    2.0
    @1847 HA1_6    2.0
    @1849 HA2_1    4.0
    @1853 HA2_2    4.0
    @1857 HA2_3    4.0
    @1861 HA2_4    4.0
    @1865 HA2_5    4.0
    @1869 HA2_6    4.0
    @1873 HA3_1    4.0
    @1877 HA3_2    4.0
    @1881 HA3_3    4.0
    @1885 HA3_4    4.0
    @1889 HA3_5    4.0
    @1893 HA3_6    4.0
    @1897 HA4_1    4.0
    @1901 HA4_2    4.0
    @1905 HA4_3    4.0
    @1909 HA4_4    4.0
    @1913 HA4_5    4.0
    @1917 HA4_6    4.0
    @1921 HA5_1    4.0
    @1925 HA5_2    4.0
    @1929 HA5_3    4.0
    @1933 HA5_4    4.0
    @1937 HA5_5    4.0
    @1941 HA5_6    4.0
    @1945 HA6_1    5.0
    @1950 HA6_2    5.0
    @1955 HA6_3    5.0
    @1960 HA6_4    5.0
    @1965 HA6_5    5.0
    @1970 HA6_6    5.0
    @1975 HA11_1   4.0
    @1979 HA11_2   4.0
    @1983 HA11_3   4.0
    @1987 HA11_4   4.0
    @1991 HA11_5   4.0
    @1995 HA11_6   4.0
    @1999 HA12_1   5.0
    @2004 HA12_2   5.0
    @2009 HA12_3   5.0
    @2014 HA12_4   5.0
    @2019 HA12_5   5.0
    @2024 HA12_6   5.0
    @2029 HA12A_1  5.0
    @2034 HA12A_2  5.0
    @2039 HA12A_3  5.0
    @2044 HA12A_4  5.0
    @2049 HA12A_5  5.0
    @2054 HA12A_6  5.0
    @2059 HA12B_1  5.0
    @2064 HA12B_2  5.0
    @2069 HA12B_3  5.0
    @2074 HA12B_4  5.0
    @2079 HA12B_5  5.0
    @2084 HA12B_6  5.0
    @2089 HA13_1   1.0
    @2090 HA13_2   1.0
    @2091 HA13_3   1.0
    @2092 HA13_4   1.0
    @2093 HA13_5   1.0
    @2094 HA13_6   1.0
    @2095 HA21_1   5.0
    @2100 HA21_2   5.0
    @2105 HA21_3   5.0
    @2110 HA21_4   5.0
    @2115 HA21_5   5.0
    @2120 HA21_6   5.0
    @2125 HA32_1   4.0
    @2129 HA32_2   4.0
    @2133 HA32_3   4.0
    @2137 HA32_4   4.0
    @2141 HA32_5   4.0
    @2145 HA32_6   4.0
    @2149 HA33_1   1.0
    @2150 HA33_2   1.0
    @2151 HA33_3   1.0
    @2152 HA33_4   1.0
    @2153 HA33_5   1.0
    @2154 HA33_6   1.0
    @2155 HA35_1   2.0
    @2157 HA35_2   2.0
    @2159 HA35_3   2.0
    @2161 HA35_4   2.0
    @2163 HA35_5   2.0
    @2165 HA35_6   2.0
    @2167 HA40_1   4.0
    @2171 HA40_2   4.0
    @2175 HA40_3   4.0
    @2179 HA40_4   4.0
    @2183 HA40_5   4.0
    @2187 HA40_6   4.0
    @2191 HA41_1   4.0
    @2195 HA41_2   4.0
    @2199 HA41_3   4.0
    @2203 HA41_4   4.0
    @2207 HA41_5   4.0
    @2211 HA41_6   4.0
    @2215 HA50_1   1.0
    @2216 HA50_2   1.0
    @2217 HA50_3   1.0
    @2218 HA50_4   1.0
    @2219 HA50_5   1.0
    @2220 HA50_6   1.0
    @2221 HA51_1   2.0
    @2223 HA51_2   2.0
    @2225 HA51_3   2.0
    @2227 HA51_4   2.0
    @2229 HA51_5   2.0
    @2231 HA51_6   2.0
    @2233 HA52_1   1.0
    @2234 HA52_2   1.0
    @2235 HA52_3   1.0
    @2236 HA52_4   1.0
    @2237 HA52_5   1.0
    @2238 HA52_6   1.0
    @2239 HA53_1   3.0
    @2242 HA53_2   3.0
    @2245 HA53_3   3.0
    @2248 HA53_4   3.0
    @2251 HA53_5   3.0
    @2254 HA53_6   3.0
    @2257 HA54_1   1.0
    @2258 HA54_2   1.0
    @2259 HA54_3   1.0
    @2260 HA54_4   1.0
    @2261 HA54_5   1.0
    @2262 HA54_6   1.0
    @2263 HA55_1   1.0
    @2264 HA55_2   1.0
    @2265 HA55_3   1.0
    @2266 HA55_4   1.0
    @2267 HA55_5   1.0
    @2268 HA55_6   1.0
    @2269 HA56_1   3.0
    @2272 HA56_2   3.0
    @2275 HA56_3   3.0
    @2278 HA56_4   3.0
    @2281 HA56_5   3.0
    @2284 HA56_6   3.0
    @2287 HA57_1   1.0
    @2288 HA57_2   1.0
    @2289 HA57_3   1.0
    @2290 HA57_4   1.0
    @2291 HA57_5   1.0
    @2292 HA57_6   1.0
    @2293 HA58_1   1.0
    @2294 HA58_2   1.0
    @2295 HA58_3   1.0
    @2296 HA58_4   1.0
    @2297 HA58_5   1.0
    @2298 HA58_6   1.0
    @2299 HA60_1   1.0
    @2300 HA60_2   1.0
    @2301 HA60_3   1.0
    @2302 HA60_4   1.0
    @2303 HA60_5   1.0
    @2304 HA60_6   1.0
    @2305 HA61_1   1.0
    @2306 HA61_2   1.0
    @2307 HA61_3   1.0
    @2308 HA61_4   1.0
    @2309 HA61_5   1.0
    @2310 HA61_6   1.0
    @2311 HA62_1   $5.
    @2316 HA62_2   $5.
    @2321 HA62_3   $5.
    @2326 HA62_4   $5.
    @2331 HA62_5   $5.
    @2336 HA62_6   $5.
    @2341 HA63_1   1.0
    @2342 HA63_2   1.0
    @2343 HA63_3   1.0
    @2344 HA63_4   1.0
    @2345 HA63_5   1.0
    @2346 HA63_6   1.0
    @2347 HA64_1   1.0
    @2348 HA64_2   1.0
    @2349 HA64_3   1.0
    @2350 HA64_4   1.0
    @2351 HA64_5   1.0
    @2352 HA64_6   1.0
    @2353 HA65_1   1.0
    @2354 HA65_2   1.0
    @2355 HA65_3   1.0
    @2356 HA65_4   1.0
    @2357 HA65_5   1.0
    @2358 HA65_6   1.0
    @2359 HA66_1   1.0
    @2360 HA66_2   1.0
    @2361 HA66_3   1.0
    @2362 HA66_4   1.0
    @2363 HA66_5   1.0
    @2364 HA66_6   1.0
    @2365 HA67_1   2.0
    @2367 HA67_2   2.0
    @2369 HA67_3   2.0
    @2371 HA67_4   2.0
    @2373 HA67_5   2.0
    @2375 HA67_6   2.0
    @2377 HA68_1   1.0
    @2378 HA68_2   1.0
    @2379 HA68_3   1.0
    @2380 HA68_4   1.0
    @2381 HA68_5   1.0
    @2382 HA68_6   1.0
    @2383 HA69_1   8.0
    @2391 HA69_2   8.0
    @2399 HA69_3   8.0
    @2407 HA69_4   8.0
    @2415 HA69_5   8.0
    @2423 HA69_6   8.0
    @2431 HA70_1   5.0
    @2436 HA70_2   5.0
    @2441 HA70_3   5.0
    @2446 HA70_4   5.0
    @2451 HA70_5   5.0
    @2456 HA70_6   5.0
    @2461 HC0_1    2.0
    @2463 HC0_2    2.0
    @2465 HC0_3    2.0
    @2467 HC0_4    2.0
    @2469 HC0_5    2.0
    @2471 HC0_6    2.0
    @2473 HC1_1    2.0
    @2475 HC1_2    2.0
    @2477 HC1_3    2.0
    @2479 HC1_4    2.0
    @2481 HC1_5    2.0
    @2483 HC1_6    2.0
    @2485 HC1A_1   5.0
    @2490 HC1A_2   5.0
    @2495 HC1A_3   5.0
    @2500 HC1A_4   5.0
    @2505 HC1A_5   5.0
    @2510 HC1A_6   5.0
    @2515 HC2_1    4.0
    @2519 HC2_2    4.0
    @2523 HC2_3    4.0
    @2527 HC2_4    4.0
    @2531 HC2_5    4.0
    @2535 HC2_6    4.0
    @2539 HC3_1    4.0
    @2543 HC3_2    4.0
    @2547 HC3_3    4.0
    @2551 HC3_4    4.0
    @2555 HC3_5    4.0
    @2559 HC3_6    4.0
    @2563 HC4_1    4.0
    @2567 HC4_2    4.0
    @2571 HC4_3    4.0
    @2575 HC4_4    4.0
    @2579 HC4_5    4.0
    @2583 HC4_6    4.0
    @2587 HC5_1    4.0
    @2591 HC5_2    4.0
    @2595 HC5_3    4.0
    @2599 HC5_4    4.0
    @2603 HC5_5    4.0
    @2607 HC5_6    4.0
    @2611 HC6_1    5.0
    @2616 HC6_2    5.0
    @2621 HC6_3    5.0
    @2626 HC6_4    5.0
    @2631 HC6_5    5.0
    @2636 HC6_6    5.0
    @2641 HC7_1    4.0
    @2645 HC7_2    4.0
    @2649 HC7_3    4.0
    @2653 HC7_4    4.0
    @2657 HC7_5    4.0
    @2661 HC7_6    4.0
    @2665 HC8_1    4.0
    @2669 HC8_2    4.0
    @2673 HC8_3    4.0
    @2677 HC8_4    4.0
    @2681 HC8_5    4.0
    @2685 HC8_6    4.0
    @2689 HC9_1    5.0
    @2694 HC9_2    5.0
    @2699 HC9_3    5.0
    @2704 HC9_4    5.0
    @2709 HC9_5    5.0
    @2714 HC9_6    5.0
    @2719 HC10_1   4.0
    @2723 HC10_2   4.0
    @2727 HC10_3   4.0
    @2731 HC10_4   4.0
    @2735 HC10_5   4.0
    @2739 HC10_6   4.0
    @2743 HC11_1   4.0
    @2747 HC11_2   4.0
    @2751 HC11_3   4.0
    @2755 HC11_4   4.0
    @2759 HC11_5   4.0
    @2763 HC11_6   4.0
    @2767 HC12_1   5.0
    @2772 HC12_2   5.0
    @2777 HC12_3   5.0
    @2782 HC12_4   5.0
    @2787 HC12_5   5.0
    @2792 HC12_6   5.0
    @2797 HC13_1   1.0
    @2798 HC13_2   1.0
    @2799 HC13_3   1.0
    @2800 HC13_4   1.0
    @2801 HC13_5   1.0
    @2802 HC13_6   1.0
    @2803 HC15_1   1.0
    @2804 HC15_2   1.0
    @2805 HC15_3   1.0
    @2806 HC15_4   1.0
    @2807 HC15_5   1.0
    @2808 HC15_6   1.0
    @2809 HC16_1   2.0
    @2811 HC16_2   2.0
    @2813 HC16_3   2.0
    @2815 HC16_4   2.0
    @2817 HC16_5   2.0
    @2819 HC16_6   2.0
    @2821 HC17_1   2.0
    @2823 HC17_2   2.0
    @2825 HC17_3   2.0
    @2827 HC17_4   2.0
    @2829 HC17_5   2.0
    @2831 HC17_6   2.0
    @2833 HC18_1   2.0
    @2835 HC18_2   2.0
    @2837 HC18_3   2.0
    @2839 HC18_4   2.0
    @2841 HC18_5   2.0
    @2843 HC18_6   2.0
    @2845 HC19_1   4.0
    @2849 HC19_2   4.0
    @2853 HC19_3   4.0
    @2857 HC19_4   4.0
    @2861 HC19_5   4.0
    @2865 HC19_6   4.0
    @2869 HC20_1   5.0
    @2874 HC20_2   5.0
    @2879 HC20_3   5.0
    @2884 HC20_4   5.0
    @2889 HC20_5   5.0
    @2894 HC20_6   5.0
    @2899 HC21_1   5.0
    @2904 HC21_2   5.0
    @2909 HC21_3   5.0
    @2914 HC21_4   5.0
    @2919 HC21_5   5.0
    @2924 HC21_6   5.0
    @2929 HC27_1   1.0
    @2930 HC27_2   1.0
    @2931 HC27_3   1.0
    @2932 HC27_4   1.0
    @2933 HC27_5   1.0
    @2934 HC27_6   1.0
    @2935 HC30_1   2.0
    @2937 HC30_2   2.0
    @2939 HC30_3   2.0
    @2941 HC30_4   2.0
    @2943 HC30_5   2.0
    @2945 HC30_6   2.0
    @2947 HC31_1   4.0
    @2951 HC31_2   4.0
    @2955 HC31_3   4.0
    @2959 HC31_4   4.0
    @2963 HC31_5   4.0
    @2967 HC31_6   4.0
    @2971 HC32_1   4.0
    @2975 HC32_2   4.0
    @2979 HC32_3   4.0
    @2983 HC32_4   4.0
    @2987 HC32_5   4.0
    @2991 HC32_6   4.0
    @2995 HC32A_1  5.0
    @3000 HC32A_2  5.0
    @3005 HC32A_3  5.0
    @3010 HC32A_4  5.0
    @3015 HC32A_5  5.0
    @3020 HC32A_6  5.0
    @3025 HC33_1   1.0
    @3026 HC33_2   1.0
    @3027 HC33_3   1.0
    @3028 HC33_4   1.0
    @3029 HC33_5   1.0
    @3030 HC33_6   1.0
    @3031 HC51_1   2.0
    @3033 HC51_2   2.0
    @3035 HC51_3   2.0
    @3037 HC51_4   2.0
    @3039 HC51_5   2.0
    @3041 HC51_6   2.0
    @3043 HC52_1   1.0
    @3044 HC52_2   1.0
    @3045 HC52_3   1.0
    @3046 HC52_4   1.0
    @3047 HC52_5   1.0
    @3048 HC52_6   1.0
    @3049 HC53_1   3.0
    @3052 HC53_2   3.0
    @3055 HC53_3   3.0
    @3058 HC53_4   3.0
    @3061 HC53_5   3.0
    @3064 HC53_6   3.0
    @3067 HC55_1   1.0
    @3068 HC55_2   1.0
    @3069 HC55_3   1.0
    @3070 HC55_4   1.0
    @3071 HC55_5   1.0
    @3072 HC55_6   1.0
    @3073 HC56_1   3.0
    @3076 HC56_2   3.0
    @3079 HC56_3   3.0
    @3082 HC56_4   3.0
    @3085 HC56_5   3.0
    @3088 HC56_6   3.0
    @3091 HC57_1   1.0
    @3092 HC57_2   1.0
    @3093 HC57_3   1.0
    @3094 HC57_4   1.0
    @3095 HC57_5   1.0
    @3096 HC57_6   1.0
    @3097 HC58_1   1.0
    @3098 HC58_2   1.0
    @3099 HC58_3   1.0
    @3100 HC58_4   1.0
    @3101 HC58_5   1.0
    @3102 HC58_6   1.0
    @3103 HC60_1   3.0
    @3106 HC60_2   3.0
    @3109 HC60_3   3.0
    @3112 HC60_4   3.0
    @3115 HC60_5   3.0
    @3118 HC60_6   3.0
    @3121 HC61_1   1.0
    @3122 HC61_2   1.0
    @3123 HC61_3   1.0
    @3124 HC61_4   1.0
    @3125 HC61_5   1.0
    @3126 HC61_6   1.0
    @3127 HC62_1   2.0
    @3129 HC62_2   2.0
    @3131 HC62_3   2.0
    @3133 HC62_4   2.0
    @3135 HC62_5   2.0
    @3137 HC62_6   2.0
    @3139 HC63_1   3.0
    @3142 HC63_2   3.0
    @3145 HC63_3   3.0
    @3148 HC63_4   3.0
    @3151 HC63_5   3.0
    @3154 HC63_6   3.0
    @3157 HC64_1   2.0
    @3159 HC64_2   2.0
    @3161 HC64_3   2.0
    @3163 HC64_4   2.0
    @3165 HC64_5   2.0
    @3167 HC64_6   2.0
    @3169 HC68_1   1.0
    @3170 HC68_2   1.0
    @3171 HC68_3   1.0
    @3172 HC68_4   1.0
    @3173 HC68_5   1.0
    @3174 HC68_6   1.0
    @3175 HC70_1   4.0
    @3179 HC70_2   4.0
    @3183 HC70_3   4.0
    @3187 HC70_4   4.0
    @3191 HC70_5   4.0
    @3195 HC70_6   4.0
    @3199 HC71_1   4.0
    @3203 HC71_2   4.0
    @3207 HC71_3   4.0
    @3211 HC71_4   4.0
    @3215 HC71_5   4.0
    @3219 HC71_6   4.0
    @3223 HC72_1   4.0
    @3227 HC72_2   4.0
    @3231 HC72_3   4.0
    @3235 HC72_4   4.0
    @3239 HC72_5   4.0
    @3243 HC72_6   4.0
    @3247 HC73_1   4.0
    @3251 HC73_2   4.0
    @3255 HC73_3   4.0
    @3259 HC73_4   4.0
    @3263 HC73_5   4.0
    @3267 HC73_6   4.0
    @3271 SB301_01 2.0
    @3273 SB301_02 2.0
    @3275 SB301_03 2.0
    @3277 SB301_04 2.0
    @3279 SB301_05 2.0
    @3281 SB301_06 2.0
    @3283 SB301_07 2.0
    @3285 SB301_08 2.0
    @3287 SB301_09 2.0
    @3289 SB301_10 2.0
    @3291 SB301_11 2.0
    @3293 SB301_12 2.0
    @3295 SB301_13 2.0
    @3297 SB301_14 2.0
    @3299 SB302_01 1.0
    @3300 SB302_02 1.0
    @3301 SB302_03 1.0
    @3302 SB302_04 1.0
    @3303 SB302_05 1.0
    @3304 SB302_06 1.0
    @3305 SB302_07 1.0
    @3306 SB302_08 1.0
    @3307 SB302_09 1.0
    @3308 SB302_10 1.0
    @3309 SB302_11 1.0
    @3310 SB302_12 1.0
    @3311 SB302_13 1.0
    @3312 SB302_14 1.0
    @3313 SB303_01 2.0
    @3315 SB303_02 2.0
    @3317 SB303_03 2.0
    @3319 SB303_04 2.0
    @3321 SB303_05 2.0
    @3323 SB303_06 2.0
    @3325 SB303_07 2.0
    @3327 SB303_08 2.0
    @3329 SB303_09 2.0
    @3331 SB303_10 2.0
    @3333 SB303_11 2.0
    @3335 SB303_12 2.0
    @3337 SB303_13 2.0
    @3339 SB303_14 2.0
    @3341 SB303C_01 2.0
    @3343 SB303C_02 2.0
    @3345 SB303C_03 2.0
    @3347 SB303C_04 2.0
    @3349 SB303C_05 2.0
    @3351 SB303C_06 2.0
    @3353 SB303C_07 2.0
    @3355 SB303C_08 2.0
    @3357 SB303C_09 2.0
    @3359 SB303C_10 2.0
    @3361 SB303C_11 2.0
    @3363 SB303C_12 2.0
    @3365 SB303C_13 2.0
    @3367 SB303C_14 2.0
    @3369 SB304_01 1.0
    @3370 SB304_02 1.0
    @3371 SB304_03 1.0
    @3372 SB304_04 1.0
    @3373 SB304_05 1.0
    @3374 SB304_06 1.0
    @3375 SB304_07 1.0
    @3376 SB304_08 1.0
    @3377 SB304_09 1.0
    @3378 SB304_10 1.0
    @3379 SB304_11 1.0
    @3380 SB304_12 1.0
    @3381 SB304_13 1.0
    @3382 SB304_14 1.0
    @3383 SB304A_01 1.0
    @3384 SB304A_02 1.0
    @3385 SB304A_03 1.0
    @3386 SB304A_04 1.0
    @3387 SB304A_05 1.0
    @3388 SB304A_06 1.0
    @3389 SB304A_07 1.0
    @3390 SB304A_08 1.0
    @3391 SB304A_09 1.0
    @3392 SB304A_10 1.0
    @3393 SB304A_11 1.0
    @3394 SB304A_12 1.0
    @3395 SB304A_13 1.0
    @3396 SB304A_14 1.0
    @3397 SB304B_01 1.0
    @3398 SB304B_02 1.0
    @3399 SB304B_03 1.0
    @3400 SB304B_04 1.0
    @3401 SB304B_05 1.0
    @3402 SB304B_06 1.0
    @3403 SB304B_07 1.0
    @3404 SB304B_08 1.0
    @3405 SB304B_09 1.0
    @3406 SB304B_10 1.0
    @3407 SB304B_11 1.0
    @3408 SB304B_12 1.0
    @3409 SB304B_13 1.0
    @3410 SB304B_14 1.0
    @3411 SB304C_01 4.0
    @3415 SB304C_02 4.0
    @3419 SB304C_03 4.0
    @3423 SB304C_04 4.0
    @3427 SB304C_05 4.0
    @3431 SB304C_06 4.0
    @3435 SB304C_07 4.0
    @3439 SB304C_08 4.0
    @3443 SB304C_09 4.0
    @3447 SB304C_10 4.0
    @3451 SB304C_11 4.0
    @3455 SB304C_12 4.0
    @3459 SB304C_13 4.0
    @3463 SB304C_14 4.0
    @3467 SB304D_01 4.0
    @3471 SB304D_02 4.0
    @3475 SB304D_03 4.0
    @3479 SB304D_04 4.0
    @3483 SB304D_05 4.0
    @3487 SB304D_06 4.0
    @3491 SB304D_07 4.0
    @3495 SB304D_08 4.0
    @3499 SB304D_09 4.0
    @3503 SB304D_10 4.0
    @3507 SB304D_11 4.0
    @3511 SB304D_12 4.0
    @3515 SB304D_13 4.0
    @3519 SB304D_14 4.0
    @3523 SB304E_01 4.0
    @3527 SB304E_02 4.0
    @3531 SB304E_03 4.0
    @3535 SB304E_04 4.0
    @3539 SB304E_05 4.0
    @3543 SB304E_06 4.0
    @3547 SB304E_07 4.0
    @3551 SB304E_08 4.0
    @3555 SB304E_09 4.0
    @3559 SB304E_10 4.0
    @3563 SB304E_11 4.0
    @3567 SB304E_12 4.0
    @3571 SB304E_13 4.0
    @3575 SB304E_14 4.0
    @3579 SB306_01 1.0
    @3580 SB306_02 1.0
    @3581 SB306_03 1.0
    @3582 SB306_04 1.0
    @3583 SB306_05 1.0
    @3584 SB306_06 1.0
    @3585 SB306_07 1.0
    @3586 SB306_08 1.0
    @3587 SB306_09 1.0
    @3588 SB306_10 1.0
    @3589 SB306_11 1.0
    @3590 SB306_12 1.0
    @3591 SB306_13 1.0
    @3592 SB306_14 1.0
    @3593 SB307_01 1.0
    @3594 SB307_02 1.0
    @3595 SB307_03 1.0
    @3596 SB307_04 1.0
    @3597 SB307_05 1.0
    @3598 SB307_06 1.0
    @3599 SB307_07 1.0
    @3600 SB307_08 1.0
    @3601 SB307_09 1.0
    @3602 SB307_10 1.0
    @3603 SB307_11 1.0
    @3604 SB307_12 1.0
    @3605 SB307_13 1.0
    @3606 SB307_14 1.0
    @3607 SB308_01 2.0
    @3609 SB308_02 2.0
    @3611 SB308_03 2.0
    @3613 SB308_04 2.0
    @3615 SB308_05 2.0
    @3617 SB308_06 2.0
    @3619 SB308_07 2.0
    @3621 SB308_08 2.0
    @3623 SB308_09 2.0
    @3625 SB308_10 2.0
    @3627 SB308_11 2.0
    @3629 SB308_12 2.0
    @3631 SB308_13 2.0
    @3633 SB308_14 2.0
    @3635 SB310_01 1.0
    @3636 SB310_02 1.0
    @3637 SB310_03 1.0
    @3638 SB310_04 1.0
    @3639 SB310_05 1.0
    @3640 SB310_06 1.0
    @3641 SB310_07 1.0
    @3642 SB310_08 1.0
    @3643 SB310_09 1.0
    @3644 SB310_10 1.0
    @3645 SB310_11 1.0
    @3646 SB310_12 1.0
    @3647 SB310_13 1.0
    @3648 SB310_14 1.0
    @3649 SB311A_01 1.0
    @3650 SB311A_02 1.0
    @3651 SB311A_03 1.0
    @3652 SB311A_04 1.0
    @3653 SB311A_05 1.0
    @3654 SB311A_06 1.0
    @3655 SB311A_07 1.0
    @3656 SB311A_08 1.0
    @3657 SB311A_09 1.0
    @3658 SB311A_10 1.0
    @3659 SB311A_11 1.0
    @3660 SB311A_12 1.0
    @3661 SB311A_13 1.0
    @3662 SB311A_14 1.0
    @3663 SB311B_01 1.0
    @3664 SB311B_02 1.0
    @3665 SB311B_03 1.0
    @3666 SB311B_04 1.0
    @3667 SB311B_05 1.0
    @3668 SB311B_06 1.0
    @3669 SB311B_07 1.0
    @3670 SB311B_08 1.0
    @3671 SB311B_09 1.0
    @3672 SB311B_10 1.0
    @3673 SB311B_11 1.0
    @3674 SB311B_12 1.0
    @3675 SB311B_13 1.0
    @3676 SB311B_14 1.0
    @3677 SB311C_01 1.0
    @3678 SB311C_02 1.0
    @3679 SB311C_03 1.0
    @3680 SB311C_04 1.0
    @3681 SB311C_05 1.0
    @3682 SB311C_06 1.0
    @3683 SB311C_07 1.0
    @3684 SB311C_08 1.0
    @3685 SB311C_09 1.0
    @3686 SB311C_10 1.0
    @3687 SB311C_11 1.0
    @3688 SB311C_12 1.0
    @3689 SB311C_13 1.0
    @3690 SB311C_14 1.0
    @3691 SB312_01 2.0
    @3693 SB312_02 2.0
    @3695 SB312_03 2.0
    @3697 SB312_04 2.0
    @3699 SB312_05 2.0
    @3701 SB312_06 2.0
    @3703 SB312_07 2.0
    @3705 SB312_08 2.0
    @3707 SB312_09 2.0
    @3709 SB312_10 2.0
    @3711 SB312_11 2.0
    @3713 SB312_12 2.0
    @3715 SB312_13 2.0
    @3717 SB312_14 2.0
    @3719 SB313_01 1.0
    @3720 SB313_02 1.0
    @3721 SB313_03 1.0
    @3722 SB313_04 1.0
    @3723 SB313_05 1.0
    @3724 SB313_06 1.0
    @3725 SB313_07 1.0
    @3726 SB313_08 1.0
    @3727 SB313_09 1.0
    @3728 SB313_10 1.0
    @3729 SB313_11 1.0
    @3730 SB313_12 1.0
    @3731 SB313_13 1.0
    @3732 SB313_14 1.0
    @3733 SB314_01 4.0
    @3737 SB314_02 4.0
    @3741 SB314_03 4.0
    @3745 SB314_04 4.0
    @3749 SB314_05 4.0
    @3753 SB314_06 4.0
    @3757 SB314_07 4.0
    @3761 SB314_08 4.0
    @3765 SB314_09 4.0
    @3769 SB314_10 4.0
    @3773 SB314_11 4.0
    @3777 SB314_12 4.0
    @3781 SB314_13 4.0
    @3785 SB314_14 4.0
    @3789 SB314A_01 1.0
    @3790 SB314A_02 1.0
    @3791 SB314A_03 1.0
    @3792 SB314A_04 1.0
    @3793 SB314A_05 1.0
    @3794 SB314A_06 1.0
    @3795 SB314A_07 1.0
    @3796 SB314A_08 1.0
    @3797 SB314A_09 1.0
    @3798 SB314A_10 1.0
    @3799 SB314A_11 1.0
    @3800 SB314A_12 1.0
    @3801 SB314A_13 1.0
    @3802 SB314A_14 1.0
    @3803 SB315A_01 3.0
    @3806 SB315A_02 3.0
    @3809 SB315A_03 3.0
    @3812 SB315A_04 3.0
    @3815 SB315A_05 3.0
    @3818 SB315A_06 3.0
    @3821 SB315A_07 3.0
    @3824 SB315A_08 3.0
    @3827 SB315A_09 3.0
    @3830 SB315A_10 3.0
    @3833 SB315A_11 3.0
    @3836 SB315A_12 3.0
    @3839 SB315A_13 3.0
    @3842 SB315A_14 3.0
    @3845 SB315B_01 3.0
    @3848 SB315B_02 3.0
    @3851 SB315B_03 3.0
    @3854 SB315B_04 3.0
    @3857 SB315B_05 3.0
    @3860 SB315B_06 3.0
    @3863 SB315B_07 3.0
    @3866 SB315B_08 3.0
    @3869 SB315B_09 3.0
    @3872 SB315B_10 3.0
    @3875 SB315B_11 3.0
    @3878 SB315B_12 3.0
    @3881 SB315B_13 3.0
    @3884 SB315B_14 3.0
    @3887 SB316_01 1.0
    @3888 SB316_02 1.0
    @3889 SB316_03 1.0
    @3890 SB316_04 1.0
    @3891 SB316_05 1.0
    @3892 SB316_06 1.0
    @3893 SB316_07 1.0
    @3894 SB316_08 1.0
    @3895 SB316_09 1.0
    @3896 SB316_10 1.0
    @3897 SB316_11 1.0
    @3898 SB316_12 1.0
    @3899 SB316_13 1.0
    @3900 SB316_14 1.0
    @3901 SB317_01 3.0
    @3904 SB317_02 3.0
    @3907 SB317_03 3.0
    @3910 SB317_04 3.0
    @3913 SB317_05 3.0
    @3916 SB317_06 3.0
    @3919 SB317_07 3.0
    @3922 SB317_08 3.0
    @3925 SB317_09 3.0
    @3928 SB317_10 3.0
    @3931 SB317_11 3.0
    @3934 SB317_12 3.0
    @3937 SB317_13 3.0
    @3940 SB317_14 3.0
    @3943 SB317A_01 2.0
    @3945 SB317A_02 2.0
    @3947 SB317A_03 2.0
    @3949 SB317A_04 2.0
    @3951 SB317A_05 2.0
    @3953 SB317A_06 2.0
    @3955 SB317A_07 2.0
    @3957 SB317A_08 2.0
    @3959 SB317A_09 2.0
    @3961 SB317A_10 2.0
    @3963 SB317A_11 2.0
    @3965 SB317A_12 2.0
    @3967 SB317A_13 2.0
    @3969 SB317A_14 2.0
    @3971 SB317B_01 1.0
    @3972 SB317B_02 1.0
    @3973 SB317B_03 1.0
    @3974 SB317B_04 1.0
    @3975 SB317B_05 1.0
    @3976 SB317B_06 1.0
    @3977 SB317B_07 1.0
    @3978 SB317B_08 1.0
    @3979 SB317B_09 1.0
    @3980 SB317B_10 1.0
    @3981 SB317B_11 1.0
    @3982 SB317B_12 1.0
    @3983 SB317B_13 1.0
    @3984 SB317B_14 1.0
    @3985 SB317CA_01 1.0
    @3986 SB317CA_02 1.0
    @3987 SB317CA_03 1.0
    @3988 SB317CA_04 1.0
    @3989 SB317CA_05 1.0
    @3990 SB317CA_06 1.0
    @3991 SB317CA_07 1.0
    @3992 SB317CA_08 1.0
    @3993 SB317CA_09 1.0
    @3994 SB317CA_10 1.0
    @3995 SB317CA_11 1.0
    @3996 SB317CA_12 1.0
    @3997 SB317CA_13 1.0
    @3998 SB317CA_14 1.0
    @3999 SB317CB_01 1.0
    @4000 SB317CB_02 1.0
    @4001 SB317CB_03 1.0
    @4002 SB317CB_04 1.0
    @4003 SB317CB_05 1.0
    @4004 SB317CB_06 1.0
    @4005 SB317CB_07 1.0
    @4006 SB317CB_08 1.0
    @4007 SB317CB_09 1.0
    @4008 SB317CB_10 1.0
    @4009 SB317CB_11 1.0
    @4010 SB317CB_12 1.0
    @4011 SB317CB_13 1.0
    @4012 SB317CB_14 1.0
    @4013 SB317CC_01 1.0
    @4014 SB317CC_02 1.0
    @4015 SB317CC_03 1.0
    @4016 SB317CC_04 1.0
    @4017 SB317CC_05 1.0
    @4018 SB317CC_06 1.0
    @4019 SB317CC_07 1.0
    @4020 SB317CC_08 1.0
    @4021 SB317CC_09 1.0
    @4022 SB317CC_10 1.0
    @4023 SB317CC_11 1.0
    @4024 SB317CC_12 1.0
    @4025 SB317CC_13 1.0
    @4026 SB317CC_14 1.0
    @4027 SB317CD_01 1.0
    @4028 SB317CD_02 1.0
    @4029 SB317CD_03 1.0
    @4030 SB317CD_04 1.0
    @4031 SB317CD_05 1.0
    @4032 SB317CD_06 1.0
    @4033 SB317CD_07 1.0
    @4034 SB317CD_08 1.0
    @4035 SB317CD_09 1.0
    @4036 SB317CD_10 1.0
    @4037 SB317CD_11 1.0
    @4038 SB317CD_12 1.0
    @4039 SB317CD_13 1.0
    @4040 SB317CD_14 1.0
    @4041 SB317CE_01 1.0
    @4042 SB317CE_02 1.0
    @4043 SB317CE_03 1.0
    @4044 SB317CE_04 1.0
    @4045 SB317CE_05 1.0
    @4046 SB317CE_06 1.0
    @4047 SB317CE_07 1.0
    @4048 SB317CE_08 1.0
    @4049 SB317CE_09 1.0
    @4050 SB317CE_10 1.0
    @4051 SB317CE_11 1.0
    @4052 SB317CE_12 1.0
    @4053 SB317CE_13 1.0
    @4054 SB317CE_14 1.0
    @4055 SB317CF_01 1.0
    @4056 SB317CF_02 1.0
    @4057 SB317CF_03 1.0
    @4058 SB317CF_04 1.0
    @4059 SB317CF_05 1.0
    @4060 SB317CF_06 1.0
    @4061 SB317CF_07 1.0
    @4062 SB317CF_08 1.0
    @4063 SB317CF_09 1.0
    @4064 SB317CF_10 1.0
    @4065 SB317CF_11 1.0
    @4066 SB317CF_12 1.0
    @4067 SB317CF_13 1.0
    @4068 SB317CF_14 1.0
    @4069 SB317CG_01 1.0
    @4070 SB317CG_02 1.0
    @4071 SB317CG_03 1.0
    @4072 SB317CG_04 1.0
    @4073 SB317CG_05 1.0
    @4074 SB317CG_06 1.0
    @4075 SB317CG_07 1.0
    @4076 SB317CG_08 1.0
    @4077 SB317CG_09 1.0
    @4078 SB317CG_10 1.0
    @4079 SB317CG_11 1.0
    @4080 SB317CG_12 1.0
    @4081 SB317CG_13 1.0
    @4082 SB317CG_14 1.0
    @4083 SB317CH_01 1.0
    @4084 SB317CH_02 1.0
    @4085 SB317CH_03 1.0
    @4086 SB317CH_04 1.0
    @4087 SB317CH_05 1.0
    @4088 SB317CH_06 1.0
    @4089 SB317CH_07 1.0
    @4090 SB317CH_08 1.0
    @4091 SB317CH_09 1.0
    @4092 SB317CH_10 1.0
    @4093 SB317CH_11 1.0
    @4094 SB317CH_12 1.0
    @4095 SB317CH_13 1.0
    @4096 SB317CH_14 1.0
    @4097 SB317CI_01 1.0
    @4098 SB317CI_02 1.0
    @4099 SB317CI_03 1.0
    @4100 SB317CI_04 1.0
    @4101 SB317CI_05 1.0
    @4102 SB317CI_06 1.0
    @4103 SB317CI_07 1.0
    @4104 SB317CI_08 1.0
    @4105 SB317CI_09 1.0
    @4106 SB317CI_10 1.0
    @4107 SB317CI_11 1.0
    @4108 SB317CI_12 1.0
    @4109 SB317CI_13 1.0
    @4110 SB317CI_14 1.0
    @4111 SB317CJ_01 1.0
    @4112 SB317CJ_02 1.0
    @4113 SB317CJ_03 1.0
    @4114 SB317CJ_04 1.0
    @4115 SB317CJ_05 1.0
    @4116 SB317CJ_06 1.0
    @4117 SB317CJ_07 1.0
    @4118 SB317CJ_08 1.0
    @4119 SB317CJ_09 1.0
    @4120 SB317CJ_10 1.0
    @4121 SB317CJ_11 1.0
    @4122 SB317CJ_12 1.0
    @4123 SB317CJ_13 1.0
    @4124 SB317CJ_14 1.0
    @4125 SB317CK_01 1.0
    @4126 SB317CK_02 1.0
    @4127 SB317CK_03 1.0
    @4128 SB317CK_04 1.0
    @4129 SB317CK_05 1.0
    @4130 SB317CK_06 1.0
    @4131 SB317CK_07 1.0
    @4132 SB317CK_08 1.0
    @4133 SB317CK_09 1.0
    @4134 SB317CK_10 1.0
    @4135 SB317CK_11 1.0
    @4136 SB317CK_12 1.0
    @4137 SB317CK_13 1.0
    @4138 SB317CK_14 1.0
    @4139 SB317CL_01 1.0
    @4140 SB317CL_02 1.0
    @4141 SB317CL_03 1.0
    @4142 SB317CL_04 1.0
    @4143 SB317CL_05 1.0
    @4144 SB317CL_06 1.0
    @4145 SB317CL_07 1.0
    @4146 SB317CL_08 1.0
    @4147 SB317CL_09 1.0
    @4148 SB317CL_10 1.0
    @4149 SB317CL_11 1.0
    @4150 SB317CL_12 1.0
    @4151 SB317CL_13 1.0
    @4152 SB317CL_14 1.0
    @4153 SB317CM_01 1.0
    @4154 SB317CM_02 1.0
    @4155 SB317CM_03 1.0
    @4156 SB317CM_04 1.0
    @4157 SB317CM_05 1.0
    @4158 SB317CM_06 1.0
    @4159 SB317CM_07 1.0
    @4160 SB317CM_08 1.0
    @4161 SB317CM_09 1.0
    @4162 SB317CM_10 1.0
    @4163 SB317CM_11 1.0
    @4164 SB317CM_12 1.0
    @4165 SB317CM_13 1.0
    @4166 SB317CM_14 1.0
    @4167 SB317CN_01 1.0
    @4168 SB317CN_02 1.0
    @4169 SB317CN_03 1.0
    @4170 SB317CN_04 1.0
    @4171 SB317CN_05 1.0
    @4172 SB317CN_06 1.0
    @4173 SB317CN_07 1.0
    @4174 SB317CN_08 1.0
    @4175 SB317CN_09 1.0
    @4176 SB317CN_10 1.0
    @4177 SB317CN_11 1.0
    @4178 SB317CN_12 1.0
    @4179 SB317CN_13 1.0
    @4180 SB317CN_14 1.0
    @4181 SB317CX_01 1.0
    @4182 SB317CX_02 1.0
    @4183 SB317CX_03 1.0
    @4184 SB317CX_04 1.0
    @4185 SB317CX_05 1.0
    @4186 SB317CX_06 1.0
    @4187 SB317CX_07 1.0
    @4188 SB317CX_08 1.0
    @4189 SB317CX_09 1.0
    @4190 SB317CX_10 1.0
    @4191 SB317CX_11 1.0
    @4192 SB317CX_12 1.0
    @4193 SB317CX_13 1.0
    @4194 SB317CX_14 1.0
    @4195 SB318A_01 1.0
    @4196 SB318A_02 1.0
    @4197 SB318A_03 1.0
    @4198 SB318A_04 1.0
    @4199 SB318A_05 1.0
    @4200 SB318A_06 1.0
    @4201 SB318A_07 1.0
    @4202 SB318A_08 1.0
    @4203 SB318A_09 1.0
    @4204 SB318A_10 1.0
    @4205 SB318A_11 1.0
    @4206 SB318A_12 1.0
    @4207 SB318A_13 1.0
    @4208 SB318A_14 1.0
    @4209 SB318B_01 1.0
    @4210 SB318B_02 1.0
    @4211 SB318B_03 1.0
    @4212 SB318B_04 1.0
    @4213 SB318B_05 1.0
    @4214 SB318B_06 1.0
    @4215 SB318B_07 1.0
    @4216 SB318B_08 1.0
    @4217 SB318B_09 1.0
    @4218 SB318B_10 1.0
    @4219 SB318B_11 1.0
    @4220 SB318B_12 1.0
    @4221 SB318B_13 1.0
    @4222 SB318B_14 1.0
    @4223 SB318C_01 1.0
    @4224 SB318C_02 1.0
    @4225 SB318C_03 1.0
    @4226 SB318C_04 1.0
    @4227 SB318C_05 1.0
    @4228 SB318C_06 1.0
    @4229 SB318C_07 1.0
    @4230 SB318C_08 1.0
    @4231 SB318C_09 1.0
    @4232 SB318C_10 1.0
    @4233 SB318C_11 1.0
    @4234 SB318C_12 1.0
    @4235 SB318C_13 1.0
    @4236 SB318C_14 1.0
    @4237 SB318D_01 1.0
    @4238 SB318D_02 1.0
    @4239 SB318D_03 1.0
    @4240 SB318D_04 1.0
    @4241 SB318D_05 1.0
    @4242 SB318D_06 1.0
    @4243 SB318D_07 1.0
    @4244 SB318D_08 1.0
    @4245 SB318D_09 1.0
    @4246 SB318D_10 1.0
    @4247 SB318D_11 1.0
    @4248 SB318D_12 1.0
    @4249 SB318D_13 1.0
    @4250 SB318D_14 1.0
    @4251 SB318E_01 1.0
    @4252 SB318E_02 1.0
    @4253 SB318E_03 1.0
    @4254 SB318E_04 1.0
    @4255 SB318E_05 1.0
    @4256 SB318E_06 1.0
    @4257 SB318E_07 1.0
    @4258 SB318E_08 1.0
    @4259 SB318E_09 1.0
    @4260 SB318E_10 1.0
    @4261 SB318E_11 1.0
    @4262 SB318E_12 1.0
    @4263 SB318E_13 1.0
    @4264 SB318E_14 1.0
    @4265 SB319_01 1.0
    @4266 SB319_02 1.0
    @4267 SB319_03 1.0
    @4268 SB319_04 1.0
    @4269 SB319_05 1.0
    @4270 SB319_06 1.0
    @4271 SB319_07 1.0
    @4272 SB319_08 1.0
    @4273 SB319_09 1.0
    @4274 SB319_10 1.0
    @4275 SB319_11 1.0
    @4276 SB319_12 1.0
    @4277 SB319_13 1.0
    @4278 SB319_14 1.0
    @4279 SB321_01 4.0
    @4283 SB321_02 4.0
    @4287 SB321_03 4.0
    @4291 SB321_04 4.0
    @4295 SB321_05 4.0
    @4299 SB321_06 4.0
    @4303 SB321_07 4.0
    @4307 SB321_08 4.0
    @4311 SB321_09 4.0
    @4315 SB321_10 4.0
    @4319 SB321_11 4.0
    @4323 SB321_12 4.0
    @4327 SB321_13 4.0
    @4331 SB321_14 4.0
    @4335 SB322_01 1.0
    @4336 SB322_02 1.0
    @4337 SB322_03 1.0
    @4338 SB322_04 1.0
    @4339 SB322_05 1.0
    @4340 SB322_06 1.0
    @4341 SB322_07 1.0
    @4342 SB322_08 1.0
    @4343 SB322_09 1.0
    @4344 SB322_10 1.0
    @4345 SB322_11 1.0
    @4346 SB322_12 1.0
    @4347 SB322_13 1.0
    @4348 SB322_14 1.0
    @4349 SB323A_01 3.0
    @4352 SB323A_02 3.0
    @4355 SB323A_03 3.0
    @4358 SB323A_04 3.0
    @4361 SB323A_05 3.0
    @4364 SB323A_06 3.0
    @4367 SB323A_07 3.0
    @4370 SB323A_08 3.0
    @4373 SB323A_09 3.0
    @4376 SB323A_10 3.0
    @4379 SB323A_11 3.0
    @4382 SB323A_12 3.0
    @4385 SB323A_13 3.0
    @4388 SB323A_14 3.0
    @4391 SB323B_01 3.0
    @4394 SB323B_02 3.0
    @4397 SB323B_03 3.0
    @4400 SB323B_04 3.0
    @4403 SB323B_05 3.0
    @4406 SB323B_06 3.0
    @4409 SB323B_07 3.0
    @4412 SB323B_08 3.0
    @4415 SB323B_09 3.0
    @4418 SB323B_10 3.0
    @4421 SB323B_11 3.0
    @4424 SB323B_12 3.0
    @4427 SB323B_13 3.0
    @4430 SB323B_14 3.0
    @4433 SB324_01 1.0
    @4434 SB324_02 1.0
    @4435 SB324_03 1.0
    @4436 SB324_04 1.0
    @4437 SB324_05 1.0
    @4438 SB324_06 1.0
    @4439 SB324_07 1.0
    @4440 SB324_08 1.0
    @4441 SB324_09 1.0
    @4442 SB324_10 1.0
    @4443 SB324_11 1.0
    @4444 SB324_12 1.0
    @4445 SB324_13 1.0
    @4446 SB324_14 1.0
    @4447 SB325_01 1.0
    @4448 SB325_02 1.0
    @4449 SB325_03 1.0
    @4450 SB325_04 1.0
    @4451 SB325_05 1.0
    @4452 SB325_06 1.0
    @4453 SB325_07 1.0
    @4454 SB325_08 1.0
    @4455 SB325_09 1.0
    @4456 SB325_10 1.0
    @4457 SB325_11 1.0
    @4458 SB325_12 1.0
    @4459 SB325_13 1.0
    @4460 SB325_14 1.0
    @4461 SB326_01 3.0
    @4464 SB326_02 3.0
    @4467 SB326_03 3.0
    @4470 SB326_04 3.0
    @4473 SB326_05 3.0
    @4476 SB326_06 3.0
    @4479 SB326_07 3.0
    @4482 SB326_08 3.0
    @4485 SB326_09 3.0
    @4488 SB326_10 3.0
    @4491 SB326_11 3.0
    @4494 SB326_12 3.0
    @4497 SB326_13 3.0
    @4500 SB326_14 3.0
    @4503 SB326A_01 2.0
    @4505 SB326A_02 2.0
    @4507 SB326A_03 2.0
    @4509 SB326A_04 2.0
    @4511 SB326A_05 2.0
    @4513 SB326A_06 2.0
    @4515 SB326A_07 2.0
    @4517 SB326A_08 2.0
    @4519 SB326A_09 2.0
    @4521 SB326A_10 2.0
    @4523 SB326A_11 2.0
    @4525 SB326A_12 2.0
    @4527 SB326A_13 2.0
    @4529 SB326A_14 2.0
    @4531 SB326B_01 1.0
    @4532 SB326B_02 1.0
    @4533 SB326B_03 1.0
    @4534 SB326B_04 1.0
    @4535 SB326B_05 1.0
    @4536 SB326B_06 1.0
    @4537 SB326B_07 1.0
    @4538 SB326B_08 1.0
    @4539 SB326B_09 1.0
    @4540 SB326B_10 1.0
    @4541 SB326B_11 1.0
    @4542 SB326B_12 1.0
    @4543 SB326B_13 1.0
    @4544 SB326B_14 1.0
    @4545 SB326CA_01 1.0
    @4546 SB326CA_02 1.0
    @4547 SB326CA_03 1.0
    @4548 SB326CA_04 1.0
    @4549 SB326CA_05 1.0
    @4550 SB326CA_06 1.0
    @4551 SB326CA_07 1.0
    @4552 SB326CA_08 1.0
    @4553 SB326CA_09 1.0
    @4554 SB326CA_10 1.0
    @4555 SB326CA_11 1.0
    @4556 SB326CA_12 1.0
    @4557 SB326CA_13 1.0
    @4558 SB326CA_14 1.0
    @4559 SB326CB_01 1.0
    @4560 SB326CB_02 1.0
    @4561 SB326CB_03 1.0
    @4562 SB326CB_04 1.0
    @4563 SB326CB_05 1.0
    @4564 SB326CB_06 1.0
    @4565 SB326CB_07 1.0
    @4566 SB326CB_08 1.0
    @4567 SB326CB_09 1.0
    @4568 SB326CB_10 1.0
    @4569 SB326CB_11 1.0
    @4570 SB326CB_12 1.0
    @4571 SB326CB_13 1.0
    @4572 SB326CB_14 1.0
    @4573 SB326CC_01 1.0
    @4574 SB326CC_02 1.0
    @4575 SB326CC_03 1.0
    @4576 SB326CC_04 1.0
    @4577 SB326CC_05 1.0
    @4578 SB326CC_06 1.0
    @4579 SB326CC_07 1.0
    @4580 SB326CC_08 1.0
    @4581 SB326CC_09 1.0
    @4582 SB326CC_10 1.0
    @4583 SB326CC_11 1.0
    @4584 SB326CC_12 1.0
    @4585 SB326CC_13 1.0
    @4586 SB326CC_14 1.0
    @4587 SB326CD_01 1.0
    @4588 SB326CD_02 1.0
    @4589 SB326CD_03 1.0
    @4590 SB326CD_04 1.0
    @4591 SB326CD_05 1.0
    @4592 SB326CD_06 1.0
    @4593 SB326CD_07 1.0
    @4594 SB326CD_08 1.0
    @4595 SB326CD_09 1.0
    @4596 SB326CD_10 1.0
    @4597 SB326CD_11 1.0
    @4598 SB326CD_12 1.0
    @4599 SB326CD_13 1.0
    @4600 SB326CD_14 1.0
    @4601 SB326CE_01 1.0
    @4602 SB326CE_02 1.0
    @4603 SB326CE_03 1.0
    @4604 SB326CE_04 1.0
    @4605 SB326CE_05 1.0
    @4606 SB326CE_06 1.0
    @4607 SB326CE_07 1.0
    @4608 SB326CE_08 1.0
    @4609 SB326CE_09 1.0
    @4610 SB326CE_10 1.0
    @4611 SB326CE_11 1.0
    @4612 SB326CE_12 1.0
    @4613 SB326CE_13 1.0
    @4614 SB326CE_14 1.0
    @4615 SB326CF_01 1.0
    @4616 SB326CF_02 1.0
    @4617 SB326CF_03 1.0
    @4618 SB326CF_04 1.0
    @4619 SB326CF_05 1.0
    @4620 SB326CF_06 1.0
    @4621 SB326CF_07 1.0
    @4622 SB326CF_08 1.0
    @4623 SB326CF_09 1.0
    @4624 SB326CF_10 1.0
    @4625 SB326CF_11 1.0
    @4626 SB326CF_12 1.0
    @4627 SB326CF_13 1.0
    @4628 SB326CF_14 1.0
    @4629 SB326CG_01 1.0
    @4630 SB326CG_02 1.0
    @4631 SB326CG_03 1.0
    @4632 SB326CG_04 1.0
    @4633 SB326CG_05 1.0
    @4634 SB326CG_06 1.0
    @4635 SB326CG_07 1.0
    @4636 SB326CG_08 1.0
    @4637 SB326CG_09 1.0
    @4638 SB326CG_10 1.0
    @4639 SB326CG_11 1.0
    @4640 SB326CG_12 1.0
    @4641 SB326CG_13 1.0
    @4642 SB326CG_14 1.0
    @4643 SB326CH_01 1.0
    @4644 SB326CH_02 1.0
    @4645 SB326CH_03 1.0
    @4646 SB326CH_04 1.0
    @4647 SB326CH_05 1.0
    @4648 SB326CH_06 1.0
    @4649 SB326CH_07 1.0
    @4650 SB326CH_08 1.0
    @4651 SB326CH_09 1.0
    @4652 SB326CH_10 1.0
    @4653 SB326CH_11 1.0
    @4654 SB326CH_12 1.0
    @4655 SB326CH_13 1.0
    @4656 SB326CH_14 1.0
    @4657 SB326CI_01 1.0
    @4658 SB326CI_02 1.0
    @4659 SB326CI_03 1.0
    @4660 SB326CI_04 1.0
    @4661 SB326CI_05 1.0
    @4662 SB326CI_06 1.0
    @4663 SB326CI_07 1.0
    @4664 SB326CI_08 1.0
    @4665 SB326CI_09 1.0
    @4666 SB326CI_10 1.0
    @4667 SB326CI_11 1.0
    @4668 SB326CI_12 1.0
    @4669 SB326CI_13 1.0
    @4670 SB326CI_14 1.0
    @4671 SB326CJ_01 1.0
    @4672 SB326CJ_02 1.0
    @4673 SB326CJ_03 1.0
    @4674 SB326CJ_04 1.0
    @4675 SB326CJ_05 1.0
    @4676 SB326CJ_06 1.0
    @4677 SB326CJ_07 1.0
    @4678 SB326CJ_08 1.0
    @4679 SB326CJ_09 1.0
    @4680 SB326CJ_10 1.0
    @4681 SB326CJ_11 1.0
    @4682 SB326CJ_12 1.0
    @4683 SB326CJ_13 1.0
    @4684 SB326CJ_14 1.0
    @4685 SB326CK_01 1.0
    @4686 SB326CK_02 1.0
    @4687 SB326CK_03 1.0
    @4688 SB326CK_04 1.0
    @4689 SB326CK_05 1.0
    @4690 SB326CK_06 1.0
    @4691 SB326CK_07 1.0
    @4692 SB326CK_08 1.0
    @4693 SB326CK_09 1.0
    @4694 SB326CK_10 1.0
    @4695 SB326CK_11 1.0
    @4696 SB326CK_12 1.0
    @4697 SB326CK_13 1.0
    @4698 SB326CK_14 1.0
    @4699 SB326CL_01 1.0
    @4700 SB326CL_02 1.0
    @4701 SB326CL_03 1.0
    @4702 SB326CL_04 1.0
    @4703 SB326CL_05 1.0
    @4704 SB326CL_06 1.0
    @4705 SB326CL_07 1.0
    @4706 SB326CL_08 1.0
    @4707 SB326CL_09 1.0
    @4708 SB326CL_10 1.0
    @4709 SB326CL_11 1.0
    @4710 SB326CL_12 1.0
    @4711 SB326CL_13 1.0
    @4712 SB326CL_14 1.0
    @4713 SB326CM_01 1.0
    @4714 SB326CM_02 1.0
    @4715 SB326CM_03 1.0
    @4716 SB326CM_04 1.0
    @4717 SB326CM_05 1.0
    @4718 SB326CM_06 1.0
    @4719 SB326CM_07 1.0
    @4720 SB326CM_08 1.0
    @4721 SB326CM_09 1.0
    @4722 SB326CM_10 1.0
    @4723 SB326CM_11 1.0
    @4724 SB326CM_12 1.0
    @4725 SB326CM_13 1.0
    @4726 SB326CM_14 1.0
    @4727 SB326CN_01 1.0
    @4728 SB326CN_02 1.0
    @4729 SB326CN_03 1.0
    @4730 SB326CN_04 1.0
    @4731 SB326CN_05 1.0
    @4732 SB326CN_06 1.0
    @4733 SB326CN_07 1.0
    @4734 SB326CN_08 1.0
    @4735 SB326CN_09 1.0
    @4736 SB326CN_10 1.0
    @4737 SB326CN_11 1.0
    @4738 SB326CN_12 1.0
    @4739 SB326CN_13 1.0
    @4740 SB326CN_14 1.0
    @4741 SB326CX_01 1.0
    @4742 SB326CX_02 1.0
    @4743 SB326CX_03 1.0
    @4744 SB326CX_04 1.0
    @4745 SB326CX_05 1.0
    @4746 SB326CX_06 1.0
    @4747 SB326CX_07 1.0
    @4748 SB326CX_08 1.0
    @4749 SB326CX_09 1.0
    @4750 SB326CX_10 1.0
    @4751 SB326CX_11 1.0
    @4752 SB326CX_12 1.0
    @4753 SB326CX_13 1.0
    @4754 SB326CX_14 1.0
    @4755 SB327A_01 1.0
    @4756 SB327A_02 1.0
    @4757 SB327A_03 1.0
    @4758 SB327A_04 1.0
    @4759 SB327A_05 1.0
    @4760 SB327A_06 1.0
    @4761 SB327A_07 1.0
    @4762 SB327A_08 1.0
    @4763 SB327A_09 1.0
    @4764 SB327A_10 1.0
    @4765 SB327A_11 1.0
    @4766 SB327A_12 1.0
    @4767 SB327A_13 1.0
    @4768 SB327A_14 1.0
    @4769 SB327B_01 1.0
    @4770 SB327B_02 1.0
    @4771 SB327B_03 1.0
    @4772 SB327B_04 1.0
    @4773 SB327B_05 1.0
    @4774 SB327B_06 1.0
    @4775 SB327B_07 1.0
    @4776 SB327B_08 1.0
    @4777 SB327B_09 1.0
    @4778 SB327B_10 1.0
    @4779 SB327B_11 1.0
    @4780 SB327B_12 1.0
    @4781 SB327B_13 1.0
    @4782 SB327B_14 1.0
    @4783 SB327C_01 1.0
    @4784 SB327C_02 1.0
    @4785 SB327C_03 1.0
    @4786 SB327C_04 1.0
    @4787 SB327C_05 1.0
    @4788 SB327C_06 1.0
    @4789 SB327C_07 1.0
    @4790 SB327C_08 1.0
    @4791 SB327C_09 1.0
    @4792 SB327C_10 1.0
    @4793 SB327C_11 1.0
    @4794 SB327C_12 1.0
    @4795 SB327C_13 1.0
    @4796 SB327C_14 1.0
    @4797 SB327D_01 1.0
    @4798 SB327D_02 1.0
    @4799 SB327D_03 1.0
    @4800 SB327D_04 1.0
    @4801 SB327D_05 1.0
    @4802 SB327D_06 1.0
    @4803 SB327D_07 1.0
    @4804 SB327D_08 1.0
    @4805 SB327D_09 1.0
    @4806 SB327D_10 1.0
    @4807 SB327D_11 1.0
    @4808 SB327D_12 1.0
    @4809 SB327D_13 1.0
    @4810 SB327D_14 1.0
    @4811 SB327E_01 1.0
    @4812 SB327E_02 1.0
    @4813 SB327E_03 1.0
    @4814 SB327E_04 1.0
    @4815 SB327E_05 1.0
    @4816 SB327E_06 1.0
    @4817 SB327E_07 1.0
    @4818 SB327E_08 1.0
    @4819 SB327E_09 1.0
    @4820 SB327E_10 1.0
    @4821 SB327E_11 1.0
    @4822 SB327E_12 1.0
    @4823 SB327E_13 1.0
    @4824 SB327E_14 1.0
    @4825 SB328_01 1.0
    @4826 SB328_02 1.0
    @4827 SB328_03 1.0
    @4828 SB328_04 1.0
    @4829 SB328_05 1.0
    @4830 SB328_06 1.0
    @4831 SB328_07 1.0
    @4832 SB328_08 1.0
    @4833 SB328_09 1.0
    @4834 SB328_10 1.0
    @4835 SB328_11 1.0
    @4836 SB328_12 1.0
    @4837 SB328_13 1.0
    @4838 SB328_14 1.0
    @4839 SB330_01 4.0
    @4843 SB330_02 4.0
    @4847 SB330_03 4.0
    @4851 SB330_04 4.0
    @4855 SB330_05 4.0
    @4859 SB330_06 4.0
    @4863 SB330_07 4.0
    @4867 SB330_08 4.0
    @4871 SB330_09 4.0
    @4875 SB330_10 4.0
    @4879 SB330_11 4.0
    @4883 SB330_12 4.0
    @4887 SB330_13 4.0
    @4891 SB330_14 4.0
    @4895 SB331_01 1.0
    @4896 SB331_02 1.0
    @4897 SB331_03 1.0
    @4898 SB331_04 1.0
    @4899 SB331_05 1.0
    @4900 SB331_06 1.0
    @4901 SB331_07 1.0
    @4902 SB331_08 1.0
    @4903 SB331_09 1.0
    @4904 SB331_10 1.0
    @4905 SB331_11 1.0
    @4906 SB331_12 1.0
    @4907 SB331_13 1.0
    @4908 SB331_14 1.0
    @4909 SB332A_01 3.0
    @4912 SB332A_02 3.0
    @4915 SB332A_03 3.0
    @4918 SB332A_04 3.0
    @4921 SB332A_05 3.0
    @4924 SB332A_06 3.0
    @4927 SB332A_07 3.0
    @4930 SB332A_08 3.0
    @4933 SB332A_09 3.0
    @4936 SB332A_10 3.0
    @4939 SB332A_11 3.0
    @4942 SB332A_12 3.0
    @4945 SB332A_13 3.0
    @4948 SB332A_14 3.0
    @4951 SB332B_01 3.0
    @4954 SB332B_02 3.0
    @4957 SB332B_03 3.0
    @4960 SB332B_04 3.0
    @4963 SB332B_05 3.0
    @4966 SB332B_06 3.0
    @4969 SB332B_07 3.0
    @4972 SB332B_08 3.0
    @4975 SB332B_09 3.0
    @4978 SB332B_10 3.0
    @4981 SB332B_11 3.0
    @4984 SB332B_12 3.0
    @4987 SB332B_13 3.0
    @4990 SB332B_14 3.0
    @4993 SB333AA_01 3.0
    @4996 SB333AA_02 3.0
    @4999 SB333AA_03 3.0
    @5002 SB333AA_04 3.0
    @5005 SB333AA_05 3.0
    @5008 SB333AA_06 3.0
    @5011 SB333AA_07 3.0
    @5014 SB333AA_08 3.0
    @5017 SB333AA_09 3.0
    @5020 SB333AA_10 3.0
    @5023 SB333AA_11 3.0
    @5026 SB333AA_12 3.0
    @5029 SB333AA_13 3.0
    @5032 SB333AA_14 3.0
    @5035 SB333AB_01 3.0
    @5038 SB333AB_02 3.0
    @5041 SB333AB_03 3.0
    @5044 SB333AB_04 3.0
    @5047 SB333AB_05 3.0
    @5050 SB333AB_06 3.0
    @5053 SB333AB_07 3.0
    @5056 SB333AB_08 3.0
    @5059 SB333AB_09 3.0
    @5062 SB333AB_10 3.0
    @5065 SB333AB_11 3.0
    @5068 SB333AB_12 3.0
    @5071 SB333AB_13 3.0
    @5074 SB333AB_14 3.0
    @5077 SB333B_01 1.0
    @5078 SB333B_02 1.0
    @5079 SB333B_03 1.0
    @5080 SB333B_04 1.0
    @5081 SB333B_05 1.0
    @5082 SB333B_06 1.0
    @5083 SB333B_07 1.0
    @5084 SB333B_08 1.0
    @5085 SB333B_09 1.0
    @5086 SB333B_10 1.0
    @5087 SB333B_11 1.0
    @5088 SB333B_12 1.0
    @5089 SB333B_13 1.0
    @5090 SB333B_14 1.0
    @5091 SB334BA_01 1.0
    @5092 SB334BA_02 1.0
    @5093 SB334BA_03 1.0
    @5094 SB334BA_04 1.0
    @5095 SB334BA_05 1.0
    @5096 SB334BA_06 1.0
    @5097 SB334BA_07 1.0
    @5098 SB334BA_08 1.0
    @5099 SB334BA_09 1.0
    @5100 SB334BA_10 1.0
    @5101 SB334BA_11 1.0
    @5102 SB334BA_12 1.0
    @5103 SB334BA_13 1.0
    @5104 SB334BA_14 1.0
    @5105 SB334BB_01 1.0
    @5106 SB334BB_02 1.0
    @5107 SB334BB_03 1.0
    @5108 SB334BB_04 1.0
    @5109 SB334BB_05 1.0
    @5110 SB334BB_06 1.0
    @5111 SB334BB_07 1.0
    @5112 SB334BB_08 1.0
    @5113 SB334BB_09 1.0
    @5114 SB334BB_10 1.0
    @5115 SB334BB_11 1.0
    @5116 SB334BB_12 1.0
    @5117 SB334BB_13 1.0
    @5118 SB334BB_14 1.0
    @5119 SB334BC_01 1.0
    @5120 SB334BC_02 1.0
    @5121 SB334BC_03 1.0
    @5122 SB334BC_04 1.0
    @5123 SB334BC_05 1.0
    @5124 SB334BC_06 1.0
    @5125 SB334BC_07 1.0
    @5126 SB334BC_08 1.0
    @5127 SB334BC_09 1.0
    @5128 SB334BC_10 1.0
    @5129 SB334BC_11 1.0
    @5130 SB334BC_12 1.0
    @5131 SB334BC_13 1.0
    @5132 SB334BC_14 1.0
    @5133 SB1D_01  2.0
    @5135 SB1D_02  2.0
    @5137 SB1D_03  2.0
    @5139 SB1D_04  2.0
    @5141 SB1D_05  2.0
    @5143 SB1D_06  2.0
    @5145 SB1D_07  2.0
    @5147 SB1D_08  2.0
    @5149 SB1D_09  2.0
    @5151 SB1D_10  2.0
    @5153 SB1D_11  2.0
    @5155 SB1D_12  2.0
    @5157 SB1D_13  2.0
    @5159 SB1D_14  2.0
    @5161 SB1MO_01 2.0
    @5163 SB1MO_02 2.0
    @5165 SB1MO_03 2.0
    @5167 SB1MO_04 2.0
    @5169 SB1MO_05 2.0
    @5171 SB1MO_06 2.0
    @5173 SB1MO_07 2.0
    @5175 SB1MO_08 2.0
    @5177 SB1MO_09 2.0
    @5179 SB1MO_10 2.0
    @5181 SB1MO_11 2.0
    @5183 SB1MO_12 2.0
    @5185 SB1MO_13 2.0
    @5187 SB1MO_14 2.0
    @5189 SB1Y_01  4.0
    @5193 SB1Y_02  4.0
    @5197 SB1Y_03  4.0
    @5201 SB1Y_04  4.0
    @5205 SB1Y_05  4.0
    @5209 SB1Y_06  4.0
    @5213 SB1Y_07  4.0
    @5217 SB1Y_08  4.0
    @5221 SB1Y_09  4.0
    @5225 SB1Y_10  4.0
    @5229 SB1Y_11  4.0
    @5233 SB1Y_12  4.0
    @5237 SB1Y_13  4.0
    @5241 SB1Y_14  4.0
    @5245 SB1H_01  2.0
    @5247 SB1H_02  2.0
    @5249 SB1H_03  2.0
    @5251 SB1H_04  2.0
    @5253 SB1H_05  2.0
    @5255 SB1H_06  2.0
    @5257 SB1H_07  2.0
    @5259 SB1H_08  2.0
    @5261 SB1H_09  2.0
    @5263 SB1H_10  2.0
    @5265 SB1H_11  2.0
    @5267 SB1H_12  2.0
    @5269 SB1H_13  2.0
    @5271 SB1H_14  2.0
    @5273 SB1M_01  2.0
    @5275 SB1M_02  2.0
    @5277 SB1M_03  2.0
    @5279 SB1M_04  2.0
    @5281 SB1M_05  2.0
    @5283 SB1M_06  2.0
    @5285 SB1M_07  2.0
    @5287 SB1M_08  2.0
    @5289 SB1M_09  2.0
    @5291 SB1M_10  2.0
    @5293 SB1M_11  2.0
    @5295 SB1M_12  2.0
    @5297 SB1M_13  2.0
    @5299 SB1M_14  2.0
    @5301 SB2D_01  2.0
    @5303 SB2D_02  2.0
    @5305 SB2D_03  2.0
    @5307 SB2D_04  2.0
    @5309 SB2D_05  2.0
    @5311 SB2D_06  2.0
    @5313 SB2D_07  2.0
    @5315 SB2D_08  2.0
    @5317 SB2D_09  2.0
    @5319 SB2D_10  2.0
    @5321 SB2D_11  2.0
    @5323 SB2D_12  2.0
    @5325 SB2D_13  2.0
    @5327 SB2D_14  2.0
    @5329 SB2MO_01 2.0
    @5331 SB2MO_02 2.0
    @5333 SB2MO_03 2.0
    @5335 SB2MO_04 2.0
    @5337 SB2MO_05 2.0
    @5339 SB2MO_06 2.0
    @5341 SB2MO_07 2.0
    @5343 SB2MO_08 2.0
    @5345 SB2MO_09 2.0
    @5347 SB2MO_10 2.0
    @5349 SB2MO_11 2.0
    @5351 SB2MO_12 2.0
    @5353 SB2MO_13 2.0
    @5355 SB2MO_14 2.0
    @5357 SB2Y_01  4.0
    @5361 SB2Y_02  4.0
    @5365 SB2Y_03  4.0
    @5369 SB2Y_04  4.0
    @5373 SB2Y_05  4.0
    @5377 SB2Y_06  4.0
    @5381 SB2Y_07  4.0
    @5385 SB2Y_08  4.0
    @5389 SB2Y_09  4.0
    @5393 SB2Y_10  4.0
    @5397 SB2Y_11  4.0
    @5401 SB2Y_12  4.0
    @5405 SB2Y_13  4.0
    @5409 SB2Y_14  4.0
    @5413 SB2H_01  2.0
    @5415 SB2H_02  2.0
    @5417 SB2H_03  2.0
    @5419 SB2H_04  2.0
    @5421 SB2H_05  2.0
    @5423 SB2H_06  2.0
    @5425 SB2H_07  2.0
    @5427 SB2H_08  2.0
    @5429 SB2H_09  2.0
    @5431 SB2H_10  2.0
    @5433 SB2H_11  2.0
    @5435 SB2H_12  2.0
    @5437 SB2H_13  2.0
    @5439 SB2H_14  2.0
    @5441 SB2M_01  2.0
    @5443 SB2M_02  2.0
    @5445 SB2M_03  2.0
    @5447 SB2M_04  2.0
    @5449 SB2M_05  2.0
    @5451 SB2M_06  2.0
    @5453 SB2M_07  2.0
    @5455 SB2M_08  2.0
    @5457 SB2M_09  2.0
    @5459 SB2M_10  2.0
    @5461 SB2M_11  2.0
    @5463 SB2M_12  2.0
    @5465 SB2M_13  2.0
    @5467 SB2M_14  2.0
    @5469 SB3D_01  2.0
    @5471 SB3D_02  2.0
    @5473 SB3D_03  2.0
    @5475 SB3D_04  2.0
    @5477 SB3D_05  2.0
    @5479 SB3D_06  2.0
    @5481 SB3D_07  2.0
    @5483 SB3D_08  2.0
    @5485 SB3D_09  2.0
    @5487 SB3D_10  2.0
    @5489 SB3D_11  2.0
    @5491 SB3D_12  2.0
    @5493 SB3D_13  2.0
    @5495 SB3D_14  2.0
    @5497 SB3MO_01 2.0
    @5499 SB3MO_02 2.0
    @5501 SB3MO_03 2.0
    @5503 SB3MO_04 2.0
    @5505 SB3MO_05 2.0
    @5507 SB3MO_06 2.0
    @5509 SB3MO_07 2.0
    @5511 SB3MO_08 2.0
    @5513 SB3MO_09 2.0
    @5515 SB3MO_10 2.0
    @5517 SB3MO_11 2.0
    @5519 SB3MO_12 2.0
    @5521 SB3MO_13 2.0
    @5523 SB3MO_14 2.0
    @5525 SB3Y_01  4.0
    @5529 SB3Y_02  4.0
    @5533 SB3Y_03  4.0
    @5537 SB3Y_04  4.0
    @5541 SB3Y_05  4.0
    @5545 SB3Y_06  4.0
    @5549 SB3Y_07  4.0
    @5553 SB3Y_08  4.0
    @5557 SB3Y_09  4.0
    @5561 SB3Y_10  4.0
    @5565 SB3Y_11  4.0
    @5569 SB3Y_12  4.0
    @5573 SB3Y_13  4.0
    @5577 SB3Y_14  4.0
    @5581 SB3H_01  2.0
    @5583 SB3H_02  2.0
    @5585 SB3H_03  2.0
    @5587 SB3H_04  2.0
    @5589 SB3H_05  2.0
    @5591 SB3H_06  2.0
    @5593 SB3H_07  2.0
    @5595 SB3H_08  2.0
    @5597 SB3H_09  2.0
    @5599 SB3H_10  2.0
    @5601 SB3H_11  2.0
    @5603 SB3H_12  2.0
    @5605 SB3H_13  2.0
    @5607 SB3H_14  2.0
    @5609 SB3M_01  2.0
    @5611 SB3M_02  2.0
    @5613 SB3M_03  2.0
    @5615 SB3M_04  2.0
    @5617 SB3M_05  2.0
    @5619 SB3M_06  2.0
    @5621 SB3M_07  2.0
    @5623 SB3M_08  2.0
    @5625 SB3M_09  2.0
    @5627 SB3M_10  2.0
    @5629 SB3M_11  2.0
    @5631 SB3M_12  2.0
    @5633 SB3M_13  2.0
    @5635 SB3M_14  2.0
    @5637 SB334EA_01 1.0
    @5638 SB334EA_02 1.0
    @5639 SB334EA_03 1.0
    @5640 SB334EA_04 1.0
    @5641 SB334EA_05 1.0
    @5642 SB334EA_06 1.0
    @5643 SB334EA_07 1.0
    @5644 SB334EA_08 1.0
    @5645 SB334EA_09 1.0
    @5646 SB334EA_10 1.0
    @5647 SB334EA_11 1.0
    @5648 SB334EA_12 1.0
    @5649 SB334EA_13 1.0
    @5650 SB334EA_14 1.0
    @5651 SB334EB_01 1.0
    @5652 SB334EB_02 1.0
    @5653 SB334EB_03 1.0
    @5654 SB334EB_04 1.0
    @5655 SB334EB_05 1.0
    @5656 SB334EB_06 1.0
    @5657 SB334EB_07 1.0
    @5658 SB334EB_08 1.0
    @5659 SB334EB_09 1.0
    @5660 SB334EB_10 1.0
    @5661 SB334EB_11 1.0
    @5662 SB334EB_12 1.0
    @5663 SB334EB_13 1.0
    @5664 SB334EB_14 1.0
    @5665 SB334EC_01 1.0
    @5666 SB334EC_02 1.0
    @5667 SB334EC_03 1.0
    @5668 SB334EC_04 1.0
    @5669 SB334EC_05 1.0
    @5670 SB334EC_06 1.0
    @5671 SB334EC_07 1.0
    @5672 SB334EC_08 1.0
    @5673 SB334EC_09 1.0
    @5674 SB334EC_10 1.0
    @5675 SB334EC_11 1.0
    @5676 SB334EC_12 1.0
    @5677 SB334EC_13 1.0
    @5678 SB334EC_14 1.0
    @5679 SB334FA_01 4.0
    @5683 SB334FA_02 4.0
    @5687 SB334FA_03 4.0
    @5691 SB334FA_04 4.0
    @5695 SB334FA_05 4.0
    @5699 SB334FA_06 4.0
    @5703 SB334FA_07 4.0
    @5707 SB334FA_08 4.0
    @5711 SB334FA_09 4.0
    @5715 SB334FA_10 4.0
    @5719 SB334FA_11 4.0
    @5723 SB334FA_12 4.0
    @5727 SB334FA_13 4.0
    @5731 SB334FA_14 4.0
    @5735 SB334FB_01 4.0
    @5739 SB334FB_02 4.0
    @5743 SB334FB_03 4.0
    @5747 SB334FB_04 4.0
    @5751 SB334FB_05 4.0
    @5755 SB334FB_06 4.0
    @5759 SB334FB_07 4.0
    @5763 SB334FB_08 4.0
    @5767 SB334FB_09 4.0
    @5771 SB334FB_10 4.0
    @5775 SB334FB_11 4.0
    @5779 SB334FB_12 4.0
    @5783 SB334FB_13 4.0
    @5787 SB334FB_14 4.0
    @5791 SB334FC_01 4.0
    @5795 SB334FC_02 4.0
    @5799 SB334FC_03 4.0
    @5803 SB334FC_04 4.0
    @5807 SB334FC_05 4.0
    @5811 SB334FC_06 4.0
    @5815 SB334FC_07 4.0
    @5819 SB334FC_08 4.0
    @5823 SB334FC_09 4.0
    @5827 SB334FC_10 4.0
    @5831 SB334FC_11 4.0
    @5835 SB334FC_12 4.0
    @5839 SB334FC_13 4.0
    @5843 SB334FC_14 4.0
    @5847 SB334GA_01 4.0
    @5851 SB334GA_02 4.0
    @5855 SB334GA_03 4.0
    @5859 SB334GA_04 4.0
    @5863 SB334GA_05 4.0
    @5867 SB334GA_06 4.0
    @5871 SB334GA_07 4.0
    @5875 SB334GA_08 4.0
    @5879 SB334GA_09 4.0
    @5883 SB334GA_10 4.0
    @5887 SB334GA_11 4.0
    @5891 SB334GA_12 4.0
    @5895 SB334GA_13 4.0
    @5899 SB334GA_14 4.0
    @5903 SB334GB_01 4.0
    @5907 SB334GB_02 4.0
    @5911 SB334GB_03 4.0
    @5915 SB334GB_04 4.0
    @5919 SB334GB_05 4.0
    @5923 SB334GB_06 4.0
    @5927 SB334GB_07 4.0
    @5931 SB334GB_08 4.0
    @5935 SB334GB_09 4.0
    @5939 SB334GB_10 4.0
    @5943 SB334GB_11 4.0
    @5947 SB334GB_12 4.0
    @5951 SB334GB_13 4.0
    @5955 SB334GB_14 4.0
    @5959 SB334GC_01 4.0
    @5963 SB334GC_02 4.0
    @5967 SB334GC_03 4.0
    @5971 SB334GC_04 4.0
    @5975 SB334GC_05 4.0
    @5979 SB334GC_06 4.0
    @5983 SB334GC_07 4.0
    @5987 SB334GC_08 4.0
    @5991 SB334GC_09 4.0
    @5995 SB334GC_10 4.0
    @5999 SB334GC_11 4.0
    @6003 SB334GC_12 4.0
    @6007 SB334GC_13 4.0
    @6011 SB334GC_14 4.0
    @6015 SB334H_01 1.0
    @6016 SB334H_02 1.0
    @6017 SB334H_03 1.0
    @6018 SB334H_04 1.0
    @6019 SB334H_05 1.0
    @6020 SB334H_06 1.0
    @6021 SB334H_07 1.0
    @6022 SB334H_08 1.0
    @6023 SB334H_09 1.0
    @6024 SB334H_10 1.0
    @6025 SB334H_11 1.0
    @6026 SB334H_12 1.0
    @6027 SB334H_13 1.0
    @6028 SB334H_14 1.0
    @6029 SB335AD_01 2.0
    @6031 SB335AD_02 2.0
    @6033 SB335AD_03 2.0
    @6035 SB335AD_04 2.0
    @6037 SB335AD_05 2.0
    @6039 SB335AD_06 2.0
    @6041 SB335AD_07 2.0
    @6043 SB335AD_08 2.0
    @6045 SB335AD_09 2.0
    @6047 SB335AD_10 2.0
    @6049 SB335AD_11 2.0
    @6051 SB335AD_12 2.0
    @6053 SB335AD_13 2.0
    @6055 SB335AD_14 2.0
    @6057 SB335AM_01 2.0
    @6059 SB335AM_02 2.0
    @6061 SB335AM_03 2.0
    @6063 SB335AM_04 2.0
    @6065 SB335AM_05 2.0
    @6067 SB335AM_06 2.0
    @6069 SB335AM_07 2.0
    @6071 SB335AM_08 2.0
    @6073 SB335AM_09 2.0
    @6075 SB335AM_10 2.0
    @6077 SB335AM_11 2.0
    @6079 SB335AM_12 2.0
    @6081 SB335AM_13 2.0
    @6083 SB335AM_14 2.0
    @6085 SB335AY_01 4.0
    @6089 SB335AY_02 4.0
    @6093 SB335AY_03 4.0
    @6097 SB335AY_04 4.0
    @6101 SB335AY_05 4.0
    @6105 SB335AY_06 4.0
    @6109 SB335AY_07 4.0
    @6113 SB335AY_08 4.0
    @6117 SB335AY_09 4.0
    @6121 SB335AY_10 4.0
    @6125 SB335AY_11 4.0
    @6129 SB335AY_12 4.0
    @6133 SB335AY_13 4.0
    @6137 SB335AY_14 4.0
    @6141 SB335AT_01 4.0
    @6145 SB335AT_02 4.0
    @6149 SB335AT_03 4.0
    @6153 SB335AT_04 4.0
    @6157 SB335AT_05 4.0
    @6161 SB335AT_06 4.0
    @6165 SB335AT_07 4.0
    @6169 SB335AT_08 4.0
    @6173 SB335AT_09 4.0
    @6177 SB335AT_10 4.0
    @6181 SB335AT_11 4.0
    @6185 SB335AT_12 4.0
    @6189 SB335AT_13 4.0
    @6193 SB335AT_14 4.0
    @6197 SB335B_01 3.0
    @6200 SB335B_02 3.0
    @6203 SB335B_03 3.0
    @6206 SB335B_04 3.0
    @6209 SB335B_05 3.0
    @6212 SB335B_06 3.0
    @6215 SB335B_07 3.0
    @6218 SB335B_08 3.0
    @6221 SB335B_09 3.0
    @6224 SB335B_10 3.0
    @6227 SB335B_11 3.0
    @6230 SB335B_12 3.0
    @6233 SB335B_13 3.0
    @6236 SB335B_14 3.0
    @6239 SBPC1_01 4.0
    @6243 SBPC1_02 4.0
    @6247 SBPC1_03 4.0
    @6251 SBPC1_04 4.0
    @6255 SBPC1_05 4.0
    @6259 SBPC1_06 4.0
    @6263 SBPC1_07 4.0
    @6267 SBPC1_08 4.0
    @6271 SBPC1_09 4.0
    @6275 SBPC1_10 4.0
    @6279 SBPC1_11 4.0
    @6283 SBPC1_12 4.0
    @6287 SBPC1_13 4.0
    @6291 SBPC1_14 4.0
    @6295 SBSD1_01 4.0
    @6299 SBSD1_02 4.0
    @6303 SBSD1_03 4.0
    @6307 SBSD1_04 4.0
    @6311 SBSD1_05 4.0
    @6315 SBSD1_06 4.0
    @6319 SBSD1_07 4.0
    @6323 SBSD1_08 4.0
    @6327 SBSD1_09 4.0
    @6331 SBSD1_10 4.0
    @6335 SBSD1_11 4.0
    @6339 SBSD1_12 4.0
    @6343 SBSD1_13 4.0
    @6347 SBSD1_14 4.0
    @6351 SBRM1_01 5.0
    @6356 SBRM1_02 5.0
    @6361 SBRM1_03 5.0
    @6366 SBRM1_04 5.0
    @6371 SBRM1_05 5.0
    @6376 SBRM1_06 5.0
    @6381 SBRM1_07 5.0
    @6386 SBRM1_08 5.0
    @6391 SBRM1_09 5.0
    @6396 SBRM1_10 5.0
    @6401 SBRM1_11 5.0
    @6406 SBRM1_12 5.0
    @6411 SBRM1_13 5.0
    @6416 SBRM1_14 5.0
    @6421 SBRM3_01 5.0
    @6426 SBRM3_02 5.0
    @6431 SBRM3_03 5.0
    @6436 SBRM3_04 5.0
    @6441 SBRM3_05 5.0
    @6446 SBRM3_06 5.0
    @6451 SBRM3_07 5.0
    @6456 SBRM3_08 5.0
    @6461 SBRM3_09 5.0
    @6466 SBRM3_10 5.0
    @6471 SBRM3_11 5.0
    @6476 SBRM3_12 5.0
    @6481 SBRM3_13 5.0
    @6486 SBRM3_14 5.0
    @6491 SBRM4_01 5.0
    @6496 SBRM4_02 5.0
    @6501 SBRM4_03 5.0
    @6506 SBRM4_04 5.0
    @6511 SBRM4_05 5.0
    @6516 SBRM4_06 5.0
    @6521 SBRM4_07 5.0
    @6526 SBRM4_08 5.0
    @6531 SBRM4_09 5.0
    @6536 SBRM4_10 5.0
    @6541 SBRM4_11 5.0
    @6546 SBRM4_12 5.0
    @6551 SBRM4_13 5.0
    @6556 SBRM4_14 5.0
    @6561 SBRM5_01 5.0
    @6566 SBRM5_02 5.0
    @6571 SBRM5_03 5.0
    @6576 SBRM5_04 5.0
    @6581 SBRM5_05 5.0
    @6586 SBRM5_06 5.0
    @6591 SBRM5_07 5.0
    @6596 SBRM5_08 5.0
    @6601 SBRM5_09 5.0
    @6606 SBRM5_10 5.0
    @6611 SBRM5_11 5.0
    @6616 SBRM5_12 5.0
    @6621 SBRM5_13 5.0
    @6626 SBRM5_14 5.0
    @6631 SBSD3_01 4.0
    @6635 SBSD3_02 4.0
    @6639 SBSD3_03 4.0
    @6643 SBSD3_04 4.0
    @6647 SBSD3_05 4.0
    @6651 SBSD3_06 4.0
    @6655 SBSD3_07 4.0
    @6659 SBSD3_08 4.0
    @6663 SBSD3_09 4.0
    @6667 SBSD3_10 4.0
    @6671 SBSD3_11 4.0
    @6675 SBSD3_12 4.0
    @6679 SBSD3_13 4.0
    @6683 SBSD3_14 4.0
    @6687 SBWH_01  1.0
    @6688 SBWH_02  1.0
    @6689 SBWH_03  1.0
    @6690 SBWH_04  1.0
    @6691 SBWH_05  1.0
    @6692 SBWH_06  1.0
    @6693 SBWH_07  1.0
    @6694 SBWH_08  1.0
    @6695 SBWH_09  1.0
    @6696 SBWH_10  1.0
    @6697 SBWH_11  1.0
    @6698 SBWH_12  1.0
    @6699 SBWH_13  1.0
    @6700 SBWH_14  1.0
    @6701 SBBM_01  4.0
    @6705 SBBM_02  4.0
    @6709 SBBM_03  4.0
    @6713 SBBM_04  4.0
    @6717 SBBM_05  4.0
    @6721 SBBM_06  4.0
    @6725 SBBM_07  4.0
    @6729 SBBM_08  4.0
    @6733 SBBM_09  4.0
    @6737 SBBM_10  4.0
    @6741 SBBM_11  4.0
    @6745 SBBM_12  4.0
    @6749 SBBM_13  4.0
    @6753 SBBM_14  4.0
    @6757 SBRI_01  4.0
    @6761 SBRI_02  4.0
    @6765 SBRI_03  4.0
    @6769 SBRI_04  4.0
    @6773 SBRI_05  4.0
    @6777 SBRI_06  4.0
    @6781 SBRI_07  4.0
    @6785 SBRI_08  4.0
    @6789 SBRI_09  4.0
    @6793 SBRI_10  4.0
    @6797 SBRI_11  4.0
    @6801 SBRI_12  4.0
    @6805 SBRI_13  4.0
    @6809 SBRI_14  4.0
    @6813 SBCMC_01 4.0
    @6817 SBCMC_02 4.0
    @6821 SBCMC_03 4.0
    @6825 SBCMC_04 4.0
    @6829 SBCMC_05 4.0
    @6833 SBCMC_06 4.0
    @6837 SBCMC_07 4.0
    @6841 SBCMC_08 4.0
    @6845 SBCMC_09 4.0
    @6849 SBCMC_10 4.0
    @6853 SBCMC_11 4.0
    @6857 SBCMC_12 4.0
    @6861 SBCMC_13 4.0
    @6865 SBCMC_14 4.0
    @6869 SBFLG_01 1.0
    @6870 SBFLG_02 1.0
    @6871 SBFLG_03 1.0
    @6872 SBFLG_04 1.0
    @6873 SBFLG_05 1.0
    @6874 SBFLG_06 1.0
    @6875 SBFLG_07 1.0
    @6876 SBFLG_08 1.0
    @6877 SBFLG_09 1.0
    @6878 SBFLG_10 1.0
    @6879 SBFLG_11 1.0
    @6880 SBFLG_12 1.0
    @6881 SBFLG_13 1.0
    @6882 SBFLG_14 1.0
    @6883 SBRESI_01 1.0
    @6884 SBRESI_02 1.0
    @6885 SBRESI_03 1.0
    @6886 SBRESI_04 1.0
    @6887 SBRESI_05 1.0
    @6888 SBRESI_06 1.0
    @6889 SBRESI_07 1.0
    @6890 SBRESI_08 1.0
    @6891 SBRESI_09 1.0
    @6892 SBRESI_10 1.0
    @6893 SBRESI_11 1.0
    @6894 SBRESI_12 1.0
    @6895 SBRESI_13 1.0
    @6896 SBRESI_14 1.0
    @6897 SBED1_01 1.0
    @6898 SBED1_02 1.0
    @6899 SBED1_03 1.0
    @6900 SBED1_04 1.0
    @6901 SBED1_05 1.0
    @6902 SBED1_06 1.0
    @6903 SBED1_07 1.0
    @6904 SBED1_08 1.0
    @6905 SBED1_09 1.0
    @6906 SBED1_10 1.0
    @6907 SBED1_11 1.0
    @6908 SBED1_12 1.0
    @6909 SBED1_13 1.0
    @6910 SBED1_14 1.0
    @6911 SBED2_01 2.0
    @6913 SBED2_02 2.0
    @6915 SBED2_03 2.0
    @6917 SBED2_04 2.0
    @6919 SBED2_05 2.0
    @6921 SBED2_06 2.0
    @6923 SBED2_07 2.0
    @6925 SBED2_08 2.0
    @6927 SBED2_09 2.0
    @6929 SBED2_10 2.0
    @6931 SBED2_11 2.0
    @6933 SBED2_12 2.0
    @6935 SBED2_13 2.0
    @6937 SBED2_14 2.0
    @6939 SBED3_01 1.0
    @6940 SBED3_02 1.0
    @6941 SBED3_03 1.0
    @6942 SBED3_04 1.0
    @6943 SBED3_05 1.0
    @6944 SBED3_06 1.0
    @6945 SBED3_07 1.0
    @6946 SBED3_08 1.0
    @6947 SBED3_09 1.0
    @6948 SBED3_10 1.0
    @6949 SBED3_11 1.0
    @6950 SBED3_12 1.0
    @6951 SBED3_13 1.0
    @6952 SBED3_14 1.0
    @6953 SB3PRG_01 1.0
    @6954 SB3PRG_02 1.0
    @6955 SB3PRG_03 1.0
    @6956 SB3PRG_04 1.0
    @6957 SB3PRG_05 1.0
    @6958 SB3PRG_06 1.0
    @6959 SB3PRG_07 1.0
    @6960 SB3PRG_08 1.0
    @6961 SB3PRG_09 1.0
    @6962 SB3PRG_10 1.0
    @6963 SB3PRG_11 1.0
    @6964 SB3PRG_12 1.0
    @6965 SB3PRG_13 1.0
    @6966 SB3PRG_14 1.0
    @6967 SB3MOS_01 2.0
    @6969 SB3MOS_02 2.0
    @6971 SB3MOS_03 2.0
    @6973 SB3MOS_04 2.0
    @6975 SB3MOS_05 2.0
    @6977 SB3MOS_06 2.0
    @6979 SB3MOS_07 2.0
    @6981 SB3MOS_08 2.0
    @6983 SB3MOS_09 2.0
    @6985 SB3MOS_10 2.0
    @6987 SB3MOS_11 2.0
    @6989 SB3MOS_12 2.0
    @6991 SB3MOS_13 2.0
    @6993 SB3MOS_14 2.0
    ;
    if HV017    = 9 then HV017 = .;
    if HV026    = 9 then HV026 = .;
    if HV040    = 9999 then HV040 = .;
    if HV801    = 9999 then HV801 = .;
    if HV802    = 9999 then HV802 = .;
    if HV803    = 99 then HV803 = .;
    if HV804    = 9 then HV804 = .;
    if HV201    = 99 then HV201 = .;
    if HV202    = 99 then HV202 = .;
    if HV201A   = 9 then HV201A = .;
    if HV204    = 999 then HV204 = .;
    if HV205    = 99 then HV205 = .;
    if HV206    = 9 then HV206 = .;
    if HV207    = 9 then HV207 = .;
    if HV208    = 9 then HV208 = .;
    if HV209    = 9 then HV209 = .;
    if HV210    = 9 then HV210 = .;
    if HV211    = 9 then HV211 = .;
    if HV212    = 9 then HV212 = .;
    if HV213    = 99 then HV213 = .;
    if HV214    = 99 then HV214 = .;
    if HV215    = 99 then HV215 = .;
    if HV216    = 99 then HV216 = .;
    if HV219    = 9 then HV219 = .;
    if HV220    = 99 then HV220 = .;
    if HV221    = 9 then HV221 = .;
    if HV225    = 9 then HV225 = .;
    if HV226    = 99 then HV226 = .;
    if HV227    = 9 then HV227 = .;
    if HV228    = 9 then HV228 = .;
    if HV230A   = 9 then HV230A = .;
    if HV230B   = 9 then HV230B = .;
    if HV232    = 9 then HV232 = .;
    if HV232B   = 9 then HV232B = .;
    if HV232C   = 9 then HV232C = .;
    if HV232D   = 9 then HV232D = .;
    if HV232E   = 9 then HV232E = .;
    if HV232Y   = 9 then HV232Y = .;
    if HV234    = 999 then HV234 = .;
    if HV234A   = 9 then HV234A = .;
    if HV235    = 9 then HV235 = .;
    if HV236    = 9 then HV236 = .;
    if HV237    = 9 then HV237 = .;
    if HV237A   = 9 then HV237A = .;
    if HV237B   = 9 then HV237B = .;
    if HV237C   = 9 then HV237C = .;
    if HV237D   = 9 then HV237D = .;
    if HV237E   = 9 then HV237E = .;
    if HV237F   = 9 then HV237F = .;
    if HV237G   = 9 then HV237G = .;
    if HV237H   = 9 then HV237H = .;
    if HV237I   = 9 then HV237I = .;
    if HV237J   = 9 then HV237J = .;
    if HV237K   = 9 then HV237K = .;
    if HV237X   = 9 then HV237X = .;
    if HV237Z   = 9 then HV237Z = .;
    if HV238    = 99 then HV238 = .;
    if HV238A   = 9 then HV238A = .;
    if HV239    = 9 then HV239 = .;
    if HV240    = 9 then HV240 = .;
    if HV241    = 9 then HV241 = .;
    if HV242    = 9 then HV242 = .;
    if HV243A   = 9 then HV243A = .;
    if HV243B   = 9 then HV243B = .;
    if HV243C   = 9 then HV243C = .;
    if HV243D   = 9 then HV243D = .;
    if HV243E   = 9 then HV243E = .;
    if HV244    = 9 then HV244 = .;
    if HV245    = 999 then HV245 = .;
    if HV246    = 9 then HV246 = .;
    if HV246A   = 99 then HV246A = .;
    if HV246B   = 99 then HV246B = .;
    if HV246C   = 99 then HV246C = .;
    if HV246D   = 99 then HV246D = .;
    if HV246E   = 99 then HV246E = .;
    if HV246F   = 99 then HV246F = .;
    if HV246G   = 99 then HV246G = .;
    if HV246H   = 99 then HV246H = .;
    if HV246I   = 99 then HV246I = .;
    if HV246J   = 99 then HV246J = .;
    if HV246K   = 99 then HV246K = .;
    if HV247    = 9 then HV247 = .;
    if HV252    = 9 then HV252 = .;
    if HV253    = 9 then HV253 = .;
    if HV253A   = 9 then HV253A = .;
    if HV253B   = 9 then HV253B = .;
    if HV253C   = 9 then HV253C = .;
    if HV253D   = 9 then HV253D = .;
    if HV253E   = 9 then HV253E = .;
    if HV253F   = 9 then HV253F = .;
    if HV253G   = 9 then HV253G = .;
    if HV253H   = 9 then HV253H = .;
    if HV253X   = 9 then HV253X = .;
    if HV253Z   = 9 then HV253Z = .;
    if HML1     = 99 then HML1 = .;
    if HML2     = 99 then HML2 = .;
    if SH119    = 9 then SH119 = .;
    if SH121B   = 9 then SH121B = .;
    if SH121I   = 9 then SH121I = .;
    if SH121J   = 9 then SH121J = .;
    if SH121K   = 9 then SH121K = .;
    if SH121L   = 9 then SH121L = .;
    if SH121M   = 9 then SH121M = .;
    if SH121N   = 9 then SH121N = .;
    if SH122B   = 9 then SH122B = .;
    if SH122C   = 9 then SH122C = .;
    if SH122G   = 9 then SH122G = .;
    if SH125    = 9 then SH125 = .;
    if SH127A   = 9 then SH127A = .;
    if HV101_01 = 99 then HV101_01 = .;
    if HV101_02 = 99 then HV101_02 = .;
    if HV101_03 = 99 then HV101_03 = .;
    if HV101_04 = 99 then HV101_04 = .;
    if HV101_05 = 99 then HV101_05 = .;
    if HV101_06 = 99 then HV101_06 = .;
    if HV101_07 = 99 then HV101_07 = .;
    if HV101_08 = 99 then HV101_08 = .;
    if HV101_09 = 99 then HV101_09 = .;
    if HV101_10 = 99 then HV101_10 = .;
    if HV101_11 = 99 then HV101_11 = .;
    if HV101_12 = 99 then HV101_12 = .;
    if HV101_13 = 99 then HV101_13 = .;
    if HV101_14 = 99 then HV101_14 = .;
    if HV101_15 = 99 then HV101_15 = .;
    if HV101_16 = 99 then HV101_16 = .;
    if HV101_17 = 99 then HV101_17 = .;
    if HV101_18 = 99 then HV101_18 = .;
    if HV101_19 = 99 then HV101_19 = .;
    if HV101_20 = 99 then HV101_20 = .;
    if HV101_21 = 99 then HV101_21 = .;
    if HV101_22 = 99 then HV101_22 = .;
    if HV101_23 = 99 then HV101_23 = .;
    if HV101_24 = 99 then HV101_24 = .;
    if HV101_25 = 99 then HV101_25 = .;
    if HV101_26 = 99 then HV101_26 = .;
    if HV101_27 = 99 then HV101_27 = .;
    if HV101_28 = 99 then HV101_28 = .;
    if HV101_29 = 99 then HV101_29 = .;
    if HV101_30 = 99 then HV101_30 = .;
    if HV102_01 = 9 then HV102_01 = .;
    if HV102_02 = 9 then HV102_02 = .;
    if HV102_03 = 9 then HV102_03 = .;
    if HV102_04 = 9 then HV102_04 = .;
    if HV102_05 = 9 then HV102_05 = .;
    if HV102_06 = 9 then HV102_06 = .;
    if HV102_07 = 9 then HV102_07 = .;
    if HV102_08 = 9 then HV102_08 = .;
    if HV102_09 = 9 then HV102_09 = .;
    if HV102_10 = 9 then HV102_10 = .;
    if HV102_11 = 9 then HV102_11 = .;
    if HV102_12 = 9 then HV102_12 = .;
    if HV102_13 = 9 then HV102_13 = .;
    if HV102_14 = 9 then HV102_14 = .;
    if HV102_15 = 9 then HV102_15 = .;
    if HV102_16 = 9 then HV102_16 = .;
    if HV102_17 = 9 then HV102_17 = .;
    if HV102_18 = 9 then HV102_18 = .;
    if HV102_19 = 9 then HV102_19 = .;
    if HV102_20 = 9 then HV102_20 = .;
    if HV102_21 = 9 then HV102_21 = .;
    if HV102_22 = 9 then HV102_22 = .;
    if HV102_23 = 9 then HV102_23 = .;
    if HV102_24 = 9 then HV102_24 = .;
    if HV102_25 = 9 then HV102_25 = .;
    if HV102_26 = 9 then HV102_26 = .;
    if HV102_27 = 9 then HV102_27 = .;
    if HV102_28 = 9 then HV102_28 = .;
    if HV102_29 = 9 then HV102_29 = .;
    if HV102_30 = 9 then HV102_30 = .;
    if HV103_01 = 9 then HV103_01 = .;
    if HV103_02 = 9 then HV103_02 = .;
    if HV103_03 = 9 then HV103_03 = .;
    if HV103_04 = 9 then HV103_04 = .;
    if HV103_05 = 9 then HV103_05 = .;
    if HV103_06 = 9 then HV103_06 = .;
    if HV103_07 = 9 then HV103_07 = .;
    if HV103_08 = 9 then HV103_08 = .;
    if HV103_09 = 9 then HV103_09 = .;
    if HV103_10 = 9 then HV103_10 = .;
    if HV103_11 = 9 then HV103_11 = .;
    if HV103_12 = 9 then HV103_12 = .;
    if HV103_13 = 9 then HV103_13 = .;
    if HV103_14 = 9 then HV103_14 = .;
    if HV103_15 = 9 then HV103_15 = .;
    if HV103_16 = 9 then HV103_16 = .;
    if HV103_17 = 9 then HV103_17 = .;
    if HV103_18 = 9 then HV103_18 = .;
    if HV103_19 = 9 then HV103_19 = .;
    if HV103_20 = 9 then HV103_20 = .;
    if HV103_21 = 9 then HV103_21 = .;
    if HV103_22 = 9 then HV103_22 = .;
    if HV103_23 = 9 then HV103_23 = .;
    if HV103_24 = 9 then HV103_24 = .;
    if HV103_25 = 9 then HV103_25 = .;
    if HV103_26 = 9 then HV103_26 = .;
    if HV103_27 = 9 then HV103_27 = .;
    if HV103_28 = 9 then HV103_28 = .;
    if HV103_29 = 9 then HV103_29 = .;
    if HV103_30 = 9 then HV103_30 = .;
    if HV104_01 = 9 then HV104_01 = .;
    if HV104_02 = 9 then HV104_02 = .;
    if HV104_03 = 9 then HV104_03 = .;
    if HV104_04 = 9 then HV104_04 = .;
    if HV104_05 = 9 then HV104_05 = .;
    if HV104_06 = 9 then HV104_06 = .;
    if HV104_07 = 9 then HV104_07 = .;
    if HV104_08 = 9 then HV104_08 = .;
    if HV104_09 = 9 then HV104_09 = .;
    if HV104_10 = 9 then HV104_10 = .;
    if HV104_11 = 9 then HV104_11 = .;
    if HV104_12 = 9 then HV104_12 = .;
    if HV104_13 = 9 then HV104_13 = .;
    if HV104_14 = 9 then HV104_14 = .;
    if HV104_15 = 9 then HV104_15 = .;
    if HV104_16 = 9 then HV104_16 = .;
    if HV104_17 = 9 then HV104_17 = .;
    if HV104_18 = 9 then HV104_18 = .;
    if HV104_19 = 9 then HV104_19 = .;
    if HV104_20 = 9 then HV104_20 = .;
    if HV104_21 = 9 then HV104_21 = .;
    if HV104_22 = 9 then HV104_22 = .;
    if HV104_23 = 9 then HV104_23 = .;
    if HV104_24 = 9 then HV104_24 = .;
    if HV104_25 = 9 then HV104_25 = .;
    if HV104_26 = 9 then HV104_26 = .;
    if HV104_27 = 9 then HV104_27 = .;
    if HV104_28 = 9 then HV104_28 = .;
    if HV104_29 = 9 then HV104_29 = .;
    if HV104_30 = 9 then HV104_30 = .;
    if HV105_01 = 99 then HV105_01 = .;
    if HV105_02 = 99 then HV105_02 = .;
    if HV105_03 = 99 then HV105_03 = .;
    if HV105_04 = 99 then HV105_04 = .;
    if HV105_05 = 99 then HV105_05 = .;
    if HV105_06 = 99 then HV105_06 = .;
    if HV105_07 = 99 then HV105_07 = .;
    if HV105_08 = 99 then HV105_08 = .;
    if HV105_09 = 99 then HV105_09 = .;
    if HV105_10 = 99 then HV105_10 = .;
    if HV105_11 = 99 then HV105_11 = .;
    if HV105_12 = 99 then HV105_12 = .;
    if HV105_13 = 99 then HV105_13 = .;
    if HV105_14 = 99 then HV105_14 = .;
    if HV105_15 = 99 then HV105_15 = .;
    if HV105_16 = 99 then HV105_16 = .;
    if HV105_17 = 99 then HV105_17 = .;
    if HV105_18 = 99 then HV105_18 = .;
    if HV105_19 = 99 then HV105_19 = .;
    if HV105_20 = 99 then HV105_20 = .;
    if HV105_21 = 99 then HV105_21 = .;
    if HV105_22 = 99 then HV105_22 = .;
    if HV105_23 = 99 then HV105_23 = .;
    if HV105_24 = 99 then HV105_24 = .;
    if HV105_25 = 99 then HV105_25 = .;
    if HV105_26 = 99 then HV105_26 = .;
    if HV105_27 = 99 then HV105_27 = .;
    if HV105_28 = 99 then HV105_28 = .;
    if HV105_29 = 99 then HV105_29 = .;
    if HV105_30 = 99 then HV105_30 = .;
    if HV106_01 = 9 then HV106_01 = .;
    if HV106_02 = 9 then HV106_02 = .;
    if HV106_03 = 9 then HV106_03 = .;
    if HV106_04 = 9 then HV106_04 = .;
    if HV106_05 = 9 then HV106_05 = .;
    if HV106_06 = 9 then HV106_06 = .;
    if HV106_07 = 9 then HV106_07 = .;
    if HV106_08 = 9 then HV106_08 = .;
    if HV106_09 = 9 then HV106_09 = .;
    if HV106_10 = 9 then HV106_10 = .;
    if HV106_11 = 9 then HV106_11 = .;
    if HV106_12 = 9 then HV106_12 = .;
    if HV106_13 = 9 then HV106_13 = .;
    if HV106_14 = 9 then HV106_14 = .;
    if HV106_15 = 9 then HV106_15 = .;
    if HV106_16 = 9 then HV106_16 = .;
    if HV106_17 = 9 then HV106_17 = .;
    if HV106_18 = 9 then HV106_18 = .;
    if HV106_19 = 9 then HV106_19 = .;
    if HV106_20 = 9 then HV106_20 = .;
    if HV106_21 = 9 then HV106_21 = .;
    if HV106_22 = 9 then HV106_22 = .;
    if HV106_23 = 9 then HV106_23 = .;
    if HV106_24 = 9 then HV106_24 = .;
    if HV106_25 = 9 then HV106_25 = .;
    if HV106_26 = 9 then HV106_26 = .;
    if HV106_27 = 9 then HV106_27 = .;
    if HV106_28 = 9 then HV106_28 = .;
    if HV106_29 = 9 then HV106_29 = .;
    if HV106_30 = 9 then HV106_30 = .;
    if HV107_01 = 99 then HV107_01 = .;
    if HV107_02 = 99 then HV107_02 = .;
    if HV107_03 = 99 then HV107_03 = .;
    if HV107_04 = 99 then HV107_04 = .;
    if HV107_05 = 99 then HV107_05 = .;
    if HV107_06 = 99 then HV107_06 = .;
    if HV107_07 = 99 then HV107_07 = .;
    if HV107_08 = 99 then HV107_08 = .;
    if HV107_09 = 99 then HV107_09 = .;
    if HV107_10 = 99 then HV107_10 = .;
    if HV107_11 = 99 then HV107_11 = .;
    if HV107_12 = 99 then HV107_12 = .;
    if HV107_13 = 99 then HV107_13 = .;
    if HV107_14 = 99 then HV107_14 = .;
    if HV107_15 = 99 then HV107_15 = .;
    if HV107_16 = 99 then HV107_16 = .;
    if HV107_17 = 99 then HV107_17 = .;
    if HV107_18 = 99 then HV107_18 = .;
    if HV107_19 = 99 then HV107_19 = .;
    if HV107_20 = 99 then HV107_20 = .;
    if HV107_21 = 99 then HV107_21 = .;
    if HV107_22 = 99 then HV107_22 = .;
    if HV107_23 = 99 then HV107_23 = .;
    if HV107_24 = 99 then HV107_24 = .;
    if HV107_25 = 99 then HV107_25 = .;
    if HV107_26 = 99 then HV107_26 = .;
    if HV107_27 = 99 then HV107_27 = .;
    if HV107_28 = 99 then HV107_28 = .;
    if HV107_29 = 99 then HV107_29 = .;
    if HV107_30 = 99 then HV107_30 = .;
    if HV108_01 = 99 then HV108_01 = .;
    if HV108_02 = 99 then HV108_02 = .;
    if HV108_03 = 99 then HV108_03 = .;
    if HV108_04 = 99 then HV108_04 = .;
    if HV108_05 = 99 then HV108_05 = .;
    if HV108_06 = 99 then HV108_06 = .;
    if HV108_07 = 99 then HV108_07 = .;
    if HV108_08 = 99 then HV108_08 = .;
    if HV108_09 = 99 then HV108_09 = .;
    if HV108_10 = 99 then HV108_10 = .;
    if HV108_11 = 99 then HV108_11 = .;
    if HV108_12 = 99 then HV108_12 = .;
    if HV108_13 = 99 then HV108_13 = .;
    if HV108_14 = 99 then HV108_14 = .;
    if HV108_15 = 99 then HV108_15 = .;
    if HV108_16 = 99 then HV108_16 = .;
    if HV108_17 = 99 then HV108_17 = .;
    if HV108_18 = 99 then HV108_18 = .;
    if HV108_19 = 99 then HV108_19 = .;
    if HV108_20 = 99 then HV108_20 = .;
    if HV108_21 = 99 then HV108_21 = .;
    if HV108_22 = 99 then HV108_22 = .;
    if HV108_23 = 99 then HV108_23 = .;
    if HV108_24 = 99 then HV108_24 = .;
    if HV108_25 = 99 then HV108_25 = .;
    if HV108_26 = 99 then HV108_26 = .;
    if HV108_27 = 99 then HV108_27 = .;
    if HV108_28 = 99 then HV108_28 = .;
    if HV108_29 = 99 then HV108_29 = .;
    if HV108_30 = 99 then HV108_30 = .;
    if HV109_01 = 9 then HV109_01 = .;
    if HV109_02 = 9 then HV109_02 = .;
    if HV109_03 = 9 then HV109_03 = .;
    if HV109_04 = 9 then HV109_04 = .;
    if HV109_05 = 9 then HV109_05 = .;
    if HV109_06 = 9 then HV109_06 = .;
    if HV109_07 = 9 then HV109_07 = .;
    if HV109_08 = 9 then HV109_08 = .;
    if HV109_09 = 9 then HV109_09 = .;
    if HV109_10 = 9 then HV109_10 = .;
    if HV109_11 = 9 then HV109_11 = .;
    if HV109_12 = 9 then HV109_12 = .;
    if HV109_13 = 9 then HV109_13 = .;
    if HV109_14 = 9 then HV109_14 = .;
    if HV109_15 = 9 then HV109_15 = .;
    if HV109_16 = 9 then HV109_16 = .;
    if HV109_17 = 9 then HV109_17 = .;
    if HV109_18 = 9 then HV109_18 = .;
    if HV109_19 = 9 then HV109_19 = .;
    if HV109_20 = 9 then HV109_20 = .;
    if HV109_21 = 9 then HV109_21 = .;
    if HV109_22 = 9 then HV109_22 = .;
    if HV109_23 = 9 then HV109_23 = .;
    if HV109_24 = 9 then HV109_24 = .;
    if HV109_25 = 9 then HV109_25 = .;
    if HV109_26 = 9 then HV109_26 = .;
    if HV109_27 = 9 then HV109_27 = .;
    if HV109_28 = 9 then HV109_28 = .;
    if HV109_29 = 9 then HV109_29 = .;
    if HV109_30 = 9 then HV109_30 = .;
    if HV110_01 = 9 then HV110_01 = .;
    if HV110_02 = 9 then HV110_02 = .;
    if HV110_03 = 9 then HV110_03 = .;
    if HV110_04 = 9 then HV110_04 = .;
    if HV110_05 = 9 then HV110_05 = .;
    if HV110_06 = 9 then HV110_06 = .;
    if HV110_07 = 9 then HV110_07 = .;
    if HV110_08 = 9 then HV110_08 = .;
    if HV110_09 = 9 then HV110_09 = .;
    if HV110_10 = 9 then HV110_10 = .;
    if HV110_11 = 9 then HV110_11 = .;
    if HV110_12 = 9 then HV110_12 = .;
    if HV110_13 = 9 then HV110_13 = .;
    if HV110_14 = 9 then HV110_14 = .;
    if HV110_15 = 9 then HV110_15 = .;
    if HV110_16 = 9 then HV110_16 = .;
    if HV110_17 = 9 then HV110_17 = .;
    if HV110_18 = 9 then HV110_18 = .;
    if HV110_19 = 9 then HV110_19 = .;
    if HV110_20 = 9 then HV110_20 = .;
    if HV110_21 = 9 then HV110_21 = .;
    if HV110_22 = 9 then HV110_22 = .;
    if HV110_23 = 9 then HV110_23 = .;
    if HV110_24 = 9 then HV110_24 = .;
    if HV110_25 = 9 then HV110_25 = .;
    if HV110_26 = 9 then HV110_26 = .;
    if HV110_27 = 9 then HV110_27 = .;
    if HV110_28 = 9 then HV110_28 = .;
    if HV110_29 = 9 then HV110_29 = .;
    if HV110_30 = 9 then HV110_30 = .;
    if HV111_01 = 9 then HV111_01 = .;
    if HV111_02 = 9 then HV111_02 = .;
    if HV111_03 = 9 then HV111_03 = .;
    if HV111_04 = 9 then HV111_04 = .;
    if HV111_05 = 9 then HV111_05 = .;
    if HV111_06 = 9 then HV111_06 = .;
    if HV111_07 = 9 then HV111_07 = .;
    if HV111_08 = 9 then HV111_08 = .;
    if HV111_09 = 9 then HV111_09 = .;
    if HV111_10 = 9 then HV111_10 = .;
    if HV111_11 = 9 then HV111_11 = .;
    if HV111_12 = 9 then HV111_12 = .;
    if HV111_13 = 9 then HV111_13 = .;
    if HV111_14 = 9 then HV111_14 = .;
    if HV111_15 = 9 then HV111_15 = .;
    if HV111_16 = 9 then HV111_16 = .;
    if HV111_17 = 9 then HV111_17 = .;
    if HV111_18 = 9 then HV111_18 = .;
    if HV111_19 = 9 then HV111_19 = .;
    if HV111_20 = 9 then HV111_20 = .;
    if HV111_21 = 9 then HV111_21 = .;
    if HV111_22 = 9 then HV111_22 = .;
    if HV111_23 = 9 then HV111_23 = .;
    if HV111_24 = 9 then HV111_24 = .;
    if HV111_25 = 9 then HV111_25 = .;
    if HV111_26 = 9 then HV111_26 = .;
    if HV111_27 = 9 then HV111_27 = .;
    if HV111_28 = 9 then HV111_28 = .;
    if HV111_29 = 9 then HV111_29 = .;
    if HV111_30 = 9 then HV111_30 = .;
    if HV112_01 = 99 then HV112_01 = .;
    if HV112_02 = 99 then HV112_02 = .;
    if HV112_03 = 99 then HV112_03 = .;
    if HV112_04 = 99 then HV112_04 = .;
    if HV112_05 = 99 then HV112_05 = .;
    if HV112_06 = 99 then HV112_06 = .;
    if HV112_07 = 99 then HV112_07 = .;
    if HV112_08 = 99 then HV112_08 = .;
    if HV112_09 = 99 then HV112_09 = .;
    if HV112_10 = 99 then HV112_10 = .;
    if HV112_11 = 99 then HV112_11 = .;
    if HV112_12 = 99 then HV112_12 = .;
    if HV112_13 = 99 then HV112_13 = .;
    if HV112_14 = 99 then HV112_14 = .;
    if HV112_15 = 99 then HV112_15 = .;
    if HV112_16 = 99 then HV112_16 = .;
    if HV112_17 = 99 then HV112_17 = .;
    if HV112_18 = 99 then HV112_18 = .;
    if HV112_19 = 99 then HV112_19 = .;
    if HV112_20 = 99 then HV112_20 = .;
    if HV112_21 = 99 then HV112_21 = .;
    if HV112_22 = 99 then HV112_22 = .;
    if HV112_23 = 99 then HV112_23 = .;
    if HV112_24 = 99 then HV112_24 = .;
    if HV112_25 = 99 then HV112_25 = .;
    if HV112_26 = 99 then HV112_26 = .;
    if HV112_27 = 99 then HV112_27 = .;
    if HV112_28 = 99 then HV112_28 = .;
    if HV112_29 = 99 then HV112_29 = .;
    if HV112_30 = 99 then HV112_30 = .;
    if HV113_01 = 9 then HV113_01 = .;
    if HV113_02 = 9 then HV113_02 = .;
    if HV113_03 = 9 then HV113_03 = .;
    if HV113_04 = 9 then HV113_04 = .;
    if HV113_05 = 9 then HV113_05 = .;
    if HV113_06 = 9 then HV113_06 = .;
    if HV113_07 = 9 then HV113_07 = .;
    if HV113_08 = 9 then HV113_08 = .;
    if HV113_09 = 9 then HV113_09 = .;
    if HV113_10 = 9 then HV113_10 = .;
    if HV113_11 = 9 then HV113_11 = .;
    if HV113_12 = 9 then HV113_12 = .;
    if HV113_13 = 9 then HV113_13 = .;
    if HV113_14 = 9 then HV113_14 = .;
    if HV113_15 = 9 then HV113_15 = .;
    if HV113_16 = 9 then HV113_16 = .;
    if HV113_17 = 9 then HV113_17 = .;
    if HV113_18 = 9 then HV113_18 = .;
    if HV113_19 = 9 then HV113_19 = .;
    if HV113_20 = 9 then HV113_20 = .;
    if HV113_21 = 9 then HV113_21 = .;
    if HV113_22 = 9 then HV113_22 = .;
    if HV113_23 = 9 then HV113_23 = .;
    if HV113_24 = 9 then HV113_24 = .;
    if HV113_25 = 9 then HV113_25 = .;
    if HV113_26 = 9 then HV113_26 = .;
    if HV113_27 = 9 then HV113_27 = .;
    if HV113_28 = 9 then HV113_28 = .;
    if HV113_29 = 9 then HV113_29 = .;
    if HV113_30 = 9 then HV113_30 = .;
    if HV114_01 = 99 then HV114_01 = .;
    if HV114_02 = 99 then HV114_02 = .;
    if HV114_03 = 99 then HV114_03 = .;
    if HV114_04 = 99 then HV114_04 = .;
    if HV114_05 = 99 then HV114_05 = .;
    if HV114_06 = 99 then HV114_06 = .;
    if HV114_07 = 99 then HV114_07 = .;
    if HV114_08 = 99 then HV114_08 = .;
    if HV114_09 = 99 then HV114_09 = .;
    if HV114_10 = 99 then HV114_10 = .;
    if HV114_11 = 99 then HV114_11 = .;
    if HV114_12 = 99 then HV114_12 = .;
    if HV114_13 = 99 then HV114_13 = .;
    if HV114_14 = 99 then HV114_14 = .;
    if HV114_15 = 99 then HV114_15 = .;
    if HV114_16 = 99 then HV114_16 = .;
    if HV114_17 = 99 then HV114_17 = .;
    if HV114_18 = 99 then HV114_18 = .;
    if HV114_19 = 99 then HV114_19 = .;
    if HV114_20 = 99 then HV114_20 = .;
    if HV114_21 = 99 then HV114_21 = .;
    if HV114_22 = 99 then HV114_22 = .;
    if HV114_23 = 99 then HV114_23 = .;
    if HV114_24 = 99 then HV114_24 = .;
    if HV114_25 = 99 then HV114_25 = .;
    if HV114_26 = 99 then HV114_26 = .;
    if HV114_27 = 99 then HV114_27 = .;
    if HV114_28 = 99 then HV114_28 = .;
    if HV114_29 = 99 then HV114_29 = .;
    if HV114_30 = 99 then HV114_30 = .;
    if HV115_01 = 9 then HV115_01 = .;
    if HV115_02 = 9 then HV115_02 = .;
    if HV115_03 = 9 then HV115_03 = .;
    if HV115_04 = 9 then HV115_04 = .;
    if HV115_05 = 9 then HV115_05 = .;
    if HV115_06 = 9 then HV115_06 = .;
    if HV115_07 = 9 then HV115_07 = .;
    if HV115_08 = 9 then HV115_08 = .;
    if HV115_09 = 9 then HV115_09 = .;
    if HV115_10 = 9 then HV115_10 = .;
    if HV115_11 = 9 then HV115_11 = .;
    if HV115_12 = 9 then HV115_12 = .;
    if HV115_13 = 9 then HV115_13 = .;
    if HV115_14 = 9 then HV115_14 = .;
    if HV115_15 = 9 then HV115_15 = .;
    if HV115_16 = 9 then HV115_16 = .;
    if HV115_17 = 9 then HV115_17 = .;
    if HV115_18 = 9 then HV115_18 = .;
    if HV115_19 = 9 then HV115_19 = .;
    if HV115_20 = 9 then HV115_20 = .;
    if HV115_21 = 9 then HV115_21 = .;
    if HV115_22 = 9 then HV115_22 = .;
    if HV115_23 = 9 then HV115_23 = .;
    if HV115_24 = 9 then HV115_24 = .;
    if HV115_25 = 9 then HV115_25 = .;
    if HV115_26 = 9 then HV115_26 = .;
    if HV115_27 = 9 then HV115_27 = .;
    if HV115_28 = 9 then HV115_28 = .;
    if HV115_29 = 9 then HV115_29 = .;
    if HV115_30 = 9 then HV115_30 = .;
    if HV116_01 = 9 then HV116_01 = .;
    if HV116_02 = 9 then HV116_02 = .;
    if HV116_03 = 9 then HV116_03 = .;
    if HV116_04 = 9 then HV116_04 = .;
    if HV116_05 = 9 then HV116_05 = .;
    if HV116_06 = 9 then HV116_06 = .;
    if HV116_07 = 9 then HV116_07 = .;
    if HV116_08 = 9 then HV116_08 = .;
    if HV116_09 = 9 then HV116_09 = .;
    if HV116_10 = 9 then HV116_10 = .;
    if HV116_11 = 9 then HV116_11 = .;
    if HV116_12 = 9 then HV116_12 = .;
    if HV116_13 = 9 then HV116_13 = .;
    if HV116_14 = 9 then HV116_14 = .;
    if HV116_15 = 9 then HV116_15 = .;
    if HV116_16 = 9 then HV116_16 = .;
    if HV116_17 = 9 then HV116_17 = .;
    if HV116_18 = 9 then HV116_18 = .;
    if HV116_19 = 9 then HV116_19 = .;
    if HV116_20 = 9 then HV116_20 = .;
    if HV116_21 = 9 then HV116_21 = .;
    if HV116_22 = 9 then HV116_22 = .;
    if HV116_23 = 9 then HV116_23 = .;
    if HV116_24 = 9 then HV116_24 = .;
    if HV116_25 = 9 then HV116_25 = .;
    if HV116_26 = 9 then HV116_26 = .;
    if HV116_27 = 9 then HV116_27 = .;
    if HV116_28 = 9 then HV116_28 = .;
    if HV116_29 = 9 then HV116_29 = .;
    if HV116_30 = 9 then HV116_30 = .;
    if HV121_01 = 9 then HV121_01 = .;
    if HV121_02 = 9 then HV121_02 = .;
    if HV121_03 = 9 then HV121_03 = .;
    if HV121_04 = 9 then HV121_04 = .;
    if HV121_05 = 9 then HV121_05 = .;
    if HV121_06 = 9 then HV121_06 = .;
    if HV121_07 = 9 then HV121_07 = .;
    if HV121_08 = 9 then HV121_08 = .;
    if HV121_09 = 9 then HV121_09 = .;
    if HV121_10 = 9 then HV121_10 = .;
    if HV121_11 = 9 then HV121_11 = .;
    if HV121_12 = 9 then HV121_12 = .;
    if HV121_13 = 9 then HV121_13 = .;
    if HV121_14 = 9 then HV121_14 = .;
    if HV121_15 = 9 then HV121_15 = .;
    if HV121_16 = 9 then HV121_16 = .;
    if HV121_17 = 9 then HV121_17 = .;
    if HV121_18 = 9 then HV121_18 = .;
    if HV121_19 = 9 then HV121_19 = .;
    if HV121_20 = 9 then HV121_20 = .;
    if HV121_21 = 9 then HV121_21 = .;
    if HV121_22 = 9 then HV121_22 = .;
    if HV121_23 = 9 then HV121_23 = .;
    if HV121_24 = 9 then HV121_24 = .;
    if HV121_25 = 9 then HV121_25 = .;
    if HV121_26 = 9 then HV121_26 = .;
    if HV121_27 = 9 then HV121_27 = .;
    if HV121_28 = 9 then HV121_28 = .;
    if HV121_29 = 9 then HV121_29 = .;
    if HV121_30 = 9 then HV121_30 = .;
    if HV122_01 = 9 then HV122_01 = .;
    if HV122_02 = 9 then HV122_02 = .;
    if HV122_03 = 9 then HV122_03 = .;
    if HV122_04 = 9 then HV122_04 = .;
    if HV122_05 = 9 then HV122_05 = .;
    if HV122_06 = 9 then HV122_06 = .;
    if HV122_07 = 9 then HV122_07 = .;
    if HV122_08 = 9 then HV122_08 = .;
    if HV122_09 = 9 then HV122_09 = .;
    if HV122_10 = 9 then HV122_10 = .;
    if HV122_11 = 9 then HV122_11 = .;
    if HV122_12 = 9 then HV122_12 = .;
    if HV122_13 = 9 then HV122_13 = .;
    if HV122_14 = 9 then HV122_14 = .;
    if HV122_15 = 9 then HV122_15 = .;
    if HV122_16 = 9 then HV122_16 = .;
    if HV122_17 = 9 then HV122_17 = .;
    if HV122_18 = 9 then HV122_18 = .;
    if HV122_19 = 9 then HV122_19 = .;
    if HV122_20 = 9 then HV122_20 = .;
    if HV122_21 = 9 then HV122_21 = .;
    if HV122_22 = 9 then HV122_22 = .;
    if HV122_23 = 9 then HV122_23 = .;
    if HV122_24 = 9 then HV122_24 = .;
    if HV122_25 = 9 then HV122_25 = .;
    if HV122_26 = 9 then HV122_26 = .;
    if HV122_27 = 9 then HV122_27 = .;
    if HV122_28 = 9 then HV122_28 = .;
    if HV122_29 = 9 then HV122_29 = .;
    if HV122_30 = 9 then HV122_30 = .;
    if HV123_01 = 99 then HV123_01 = .;
    if HV123_02 = 99 then HV123_02 = .;
    if HV123_03 = 99 then HV123_03 = .;
    if HV123_04 = 99 then HV123_04 = .;
    if HV123_05 = 99 then HV123_05 = .;
    if HV123_06 = 99 then HV123_06 = .;
    if HV123_07 = 99 then HV123_07 = .;
    if HV123_08 = 99 then HV123_08 = .;
    if HV123_09 = 99 then HV123_09 = .;
    if HV123_10 = 99 then HV123_10 = .;
    if HV123_11 = 99 then HV123_11 = .;
    if HV123_12 = 99 then HV123_12 = .;
    if HV123_13 = 99 then HV123_13 = .;
    if HV123_14 = 99 then HV123_14 = .;
    if HV123_15 = 99 then HV123_15 = .;
    if HV123_16 = 99 then HV123_16 = .;
    if HV123_17 = 99 then HV123_17 = .;
    if HV123_18 = 99 then HV123_18 = .;
    if HV123_19 = 99 then HV123_19 = .;
    if HV123_20 = 99 then HV123_20 = .;
    if HV123_21 = 99 then HV123_21 = .;
    if HV123_22 = 99 then HV123_22 = .;
    if HV123_23 = 99 then HV123_23 = .;
    if HV123_24 = 99 then HV123_24 = .;
    if HV123_25 = 99 then HV123_25 = .;
    if HV123_26 = 99 then HV123_26 = .;
    if HV123_27 = 99 then HV123_27 = .;
    if HV123_28 = 99 then HV123_28 = .;
    if HV123_29 = 99 then HV123_29 = .;
    if HV123_30 = 99 then HV123_30 = .;
    if HV124_01 = 99 then HV124_01 = .;
    if HV124_02 = 99 then HV124_02 = .;
    if HV124_03 = 99 then HV124_03 = .;
    if HV124_04 = 99 then HV124_04 = .;
    if HV124_05 = 99 then HV124_05 = .;
    if HV124_06 = 99 then HV124_06 = .;
    if HV124_07 = 99 then HV124_07 = .;
    if HV124_08 = 99 then HV124_08 = .;
    if HV124_09 = 99 then HV124_09 = .;
    if HV124_10 = 99 then HV124_10 = .;
    if HV124_11 = 99 then HV124_11 = .;
    if HV124_12 = 99 then HV124_12 = .;
    if HV124_13 = 99 then HV124_13 = .;
    if HV124_14 = 99 then HV124_14 = .;
    if HV124_15 = 99 then HV124_15 = .;
    if HV124_16 = 99 then HV124_16 = .;
    if HV124_17 = 99 then HV124_17 = .;
    if HV124_18 = 99 then HV124_18 = .;
    if HV124_19 = 99 then HV124_19 = .;
    if HV124_20 = 99 then HV124_20 = .;
    if HV124_21 = 99 then HV124_21 = .;
    if HV124_22 = 99 then HV124_22 = .;
    if HV124_23 = 99 then HV124_23 = .;
    if HV124_24 = 99 then HV124_24 = .;
    if HV124_25 = 99 then HV124_25 = .;
    if HV124_26 = 99 then HV124_26 = .;
    if HV124_27 = 99 then HV124_27 = .;
    if HV124_28 = 99 then HV124_28 = .;
    if HV124_29 = 99 then HV124_29 = .;
    if HV124_30 = 99 then HV124_30 = .;
    if HV125_01 = 9 then HV125_01 = .;
    if HV125_02 = 9 then HV125_02 = .;
    if HV125_03 = 9 then HV125_03 = .;
    if HV125_04 = 9 then HV125_04 = .;
    if HV125_05 = 9 then HV125_05 = .;
    if HV125_06 = 9 then HV125_06 = .;
    if HV125_07 = 9 then HV125_07 = .;
    if HV125_08 = 9 then HV125_08 = .;
    if HV125_09 = 9 then HV125_09 = .;
    if HV125_10 = 9 then HV125_10 = .;
    if HV125_11 = 9 then HV125_11 = .;
    if HV125_12 = 9 then HV125_12 = .;
    if HV125_13 = 9 then HV125_13 = .;
    if HV125_14 = 9 then HV125_14 = .;
    if HV125_15 = 9 then HV125_15 = .;
    if HV125_16 = 9 then HV125_16 = .;
    if HV125_17 = 9 then HV125_17 = .;
    if HV125_18 = 9 then HV125_18 = .;
    if HV125_19 = 9 then HV125_19 = .;
    if HV125_20 = 9 then HV125_20 = .;
    if HV125_21 = 9 then HV125_21 = .;
    if HV125_22 = 9 then HV125_22 = .;
    if HV125_23 = 9 then HV125_23 = .;
    if HV125_24 = 9 then HV125_24 = .;
    if HV125_25 = 9 then HV125_25 = .;
    if HV125_26 = 9 then HV125_26 = .;
    if HV125_27 = 9 then HV125_27 = .;
    if HV125_28 = 9 then HV125_28 = .;
    if HV125_29 = 9 then HV125_29 = .;
    if HV125_30 = 9 then HV125_30 = .;
    if HV126_01 = 9 then HV126_01 = .;
    if HV126_02 = 9 then HV126_02 = .;
    if HV126_03 = 9 then HV126_03 = .;
    if HV126_04 = 9 then HV126_04 = .;
    if HV126_05 = 9 then HV126_05 = .;
    if HV126_06 = 9 then HV126_06 = .;
    if HV126_07 = 9 then HV126_07 = .;
    if HV126_08 = 9 then HV126_08 = .;
    if HV126_09 = 9 then HV126_09 = .;
    if HV126_10 = 9 then HV126_10 = .;
    if HV126_11 = 9 then HV126_11 = .;
    if HV126_12 = 9 then HV126_12 = .;
    if HV126_13 = 9 then HV126_13 = .;
    if HV126_14 = 9 then HV126_14 = .;
    if HV126_15 = 9 then HV126_15 = .;
    if HV126_16 = 9 then HV126_16 = .;
    if HV126_17 = 9 then HV126_17 = .;
    if HV126_18 = 9 then HV126_18 = .;
    if HV126_19 = 9 then HV126_19 = .;
    if HV126_20 = 9 then HV126_20 = .;
    if HV126_21 = 9 then HV126_21 = .;
    if HV126_22 = 9 then HV126_22 = .;
    if HV126_23 = 9 then HV126_23 = .;
    if HV126_24 = 9 then HV126_24 = .;
    if HV126_25 = 9 then HV126_25 = .;
    if HV126_26 = 9 then HV126_26 = .;
    if HV126_27 = 9 then HV126_27 = .;
    if HV126_28 = 9 then HV126_28 = .;
    if HV126_29 = 9 then HV126_29 = .;
    if HV126_30 = 9 then HV126_30 = .;
    if HV127_01 = 99 then HV127_01 = .;
    if HV127_02 = 99 then HV127_02 = .;
    if HV127_03 = 99 then HV127_03 = .;
    if HV127_04 = 99 then HV127_04 = .;
    if HV127_05 = 99 then HV127_05 = .;
    if HV127_06 = 99 then HV127_06 = .;
    if HV127_07 = 99 then HV127_07 = .;
    if HV127_08 = 99 then HV127_08 = .;
    if HV127_09 = 99 then HV127_09 = .;
    if HV127_10 = 99 then HV127_10 = .;
    if HV127_11 = 99 then HV127_11 = .;
    if HV127_12 = 99 then HV127_12 = .;
    if HV127_13 = 99 then HV127_13 = .;
    if HV127_14 = 99 then HV127_14 = .;
    if HV127_15 = 99 then HV127_15 = .;
    if HV127_16 = 99 then HV127_16 = .;
    if HV127_17 = 99 then HV127_17 = .;
    if HV127_18 = 99 then HV127_18 = .;
    if HV127_19 = 99 then HV127_19 = .;
    if HV127_20 = 99 then HV127_20 = .;
    if HV127_21 = 99 then HV127_21 = .;
    if HV127_22 = 99 then HV127_22 = .;
    if HV127_23 = 99 then HV127_23 = .;
    if HV127_24 = 99 then HV127_24 = .;
    if HV127_25 = 99 then HV127_25 = .;
    if HV127_26 = 99 then HV127_26 = .;
    if HV127_27 = 99 then HV127_27 = .;
    if HV127_28 = 99 then HV127_28 = .;
    if HV127_29 = 99 then HV127_29 = .;
    if HV127_30 = 99 then HV127_30 = .;
    if HV128_01 = 99 then HV128_01 = .;
    if HV128_02 = 99 then HV128_02 = .;
    if HV128_03 = 99 then HV128_03 = .;
    if HV128_04 = 99 then HV128_04 = .;
    if HV128_05 = 99 then HV128_05 = .;
    if HV128_06 = 99 then HV128_06 = .;
    if HV128_07 = 99 then HV128_07 = .;
    if HV128_08 = 99 then HV128_08 = .;
    if HV128_09 = 99 then HV128_09 = .;
    if HV128_10 = 99 then HV128_10 = .;
    if HV128_11 = 99 then HV128_11 = .;
    if HV128_12 = 99 then HV128_12 = .;
    if HV128_13 = 99 then HV128_13 = .;
    if HV128_14 = 99 then HV128_14 = .;
    if HV128_15 = 99 then HV128_15 = .;
    if HV128_16 = 99 then HV128_16 = .;
    if HV128_17 = 99 then HV128_17 = .;
    if HV128_18 = 99 then HV128_18 = .;
    if HV128_19 = 99 then HV128_19 = .;
    if HV128_20 = 99 then HV128_20 = .;
    if HV128_21 = 99 then HV128_21 = .;
    if HV128_22 = 99 then HV128_22 = .;
    if HV128_23 = 99 then HV128_23 = .;
    if HV128_24 = 99 then HV128_24 = .;
    if HV128_25 = 99 then HV128_25 = .;
    if HV128_26 = 99 then HV128_26 = .;
    if HV128_27 = 99 then HV128_27 = .;
    if HV128_28 = 99 then HV128_28 = .;
    if HV128_29 = 99 then HV128_29 = .;
    if HV128_30 = 99 then HV128_30 = .;
    if HV129_01 = 9 then HV129_01 = .;
    if HV129_02 = 9 then HV129_02 = .;
    if HV129_03 = 9 then HV129_03 = .;
    if HV129_04 = 9 then HV129_04 = .;
    if HV129_05 = 9 then HV129_05 = .;
    if HV129_06 = 9 then HV129_06 = .;
    if HV129_07 = 9 then HV129_07 = .;
    if HV129_08 = 9 then HV129_08 = .;
    if HV129_09 = 9 then HV129_09 = .;
    if HV129_10 = 9 then HV129_10 = .;
    if HV129_11 = 9 then HV129_11 = .;
    if HV129_12 = 9 then HV129_12 = .;
    if HV129_13 = 9 then HV129_13 = .;
    if HV129_14 = 9 then HV129_14 = .;
    if HV129_15 = 9 then HV129_15 = .;
    if HV129_16 = 9 then HV129_16 = .;
    if HV129_17 = 9 then HV129_17 = .;
    if HV129_18 = 9 then HV129_18 = .;
    if HV129_19 = 9 then HV129_19 = .;
    if HV129_20 = 9 then HV129_20 = .;
    if HV129_21 = 9 then HV129_21 = .;
    if HV129_22 = 9 then HV129_22 = .;
    if HV129_23 = 9 then HV129_23 = .;
    if HV129_24 = 9 then HV129_24 = .;
    if HV129_25 = 9 then HV129_25 = .;
    if HV129_26 = 9 then HV129_26 = .;
    if HV129_27 = 9 then HV129_27 = .;
    if HV129_28 = 9 then HV129_28 = .;
    if HV129_29 = 9 then HV129_29 = .;
    if HV129_30 = 9 then HV129_30 = .;
    if HV140_01 = 9 then HV140_01 = .;
    if HV140_02 = 9 then HV140_02 = .;
    if HV140_03 = 9 then HV140_03 = .;
    if HV140_04 = 9 then HV140_04 = .;
    if HV140_05 = 9 then HV140_05 = .;
    if HV140_06 = 9 then HV140_06 = .;
    if HV140_07 = 9 then HV140_07 = .;
    if HV140_08 = 9 then HV140_08 = .;
    if HV140_09 = 9 then HV140_09 = .;
    if HV140_10 = 9 then HV140_10 = .;
    if HV140_11 = 9 then HV140_11 = .;
    if HV140_12 = 9 then HV140_12 = .;
    if HV140_13 = 9 then HV140_13 = .;
    if HV140_14 = 9 then HV140_14 = .;
    if HV140_15 = 9 then HV140_15 = .;
    if HV140_16 = 9 then HV140_16 = .;
    if HV140_17 = 9 then HV140_17 = .;
    if HV140_18 = 9 then HV140_18 = .;
    if HV140_19 = 9 then HV140_19 = .;
    if HV140_20 = 9 then HV140_20 = .;
    if HV140_21 = 9 then HV140_21 = .;
    if HV140_22 = 9 then HV140_22 = .;
    if HV140_23 = 9 then HV140_23 = .;
    if HV140_24 = 9 then HV140_24 = .;
    if HV140_25 = 9 then HV140_25 = .;
    if HV140_26 = 9 then HV140_26 = .;
    if HV140_27 = 9 then HV140_27 = .;
    if HV140_28 = 9 then HV140_28 = .;
    if HV140_29 = 9 then HV140_29 = .;
    if HV140_30 = 9 then HV140_30 = .;
    if SH13_01  = 9 then SH13_01 = .;
    if SH13_02  = 9 then SH13_02 = .;
    if SH13_03  = 9 then SH13_03 = .;
    if SH13_04  = 9 then SH13_04 = .;
    if SH13_05  = 9 then SH13_05 = .;
    if SH13_06  = 9 then SH13_06 = .;
    if SH13_07  = 9 then SH13_07 = .;
    if SH13_08  = 9 then SH13_08 = .;
    if SH13_09  = 9 then SH13_09 = .;
    if SH13_10  = 9 then SH13_10 = .;
    if SH13_11  = 9 then SH13_11 = .;
    if SH13_12  = 9 then SH13_12 = .;
    if SH13_13  = 9 then SH13_13 = .;
    if SH13_14  = 9 then SH13_14 = .;
    if SH13_15  = 9 then SH13_15 = .;
    if SH13_16  = 9 then SH13_16 = .;
    if SH13_17  = 9 then SH13_17 = .;
    if SH13_18  = 9 then SH13_18 = .;
    if SH13_19  = 9 then SH13_19 = .;
    if SH13_20  = 9 then SH13_20 = .;
    if SH13_21  = 9 then SH13_21 = .;
    if SH13_22  = 9 then SH13_22 = .;
    if SH13_23  = 9 then SH13_23 = .;
    if SH13_24  = 9 then SH13_24 = .;
    if SH13_25  = 9 then SH13_25 = .;
    if SH13_26  = 9 then SH13_26 = .;
    if SH13_27  = 9 then SH13_27 = .;
    if SH13_28  = 9 then SH13_28 = .;
    if SH13_29  = 9 then SH13_29 = .;
    if SH13_30  = 9 then SH13_30 = .;
    if SH14A_01 = 9 then SH14A_01 = .;
    if SH14A_02 = 9 then SH14A_02 = .;
    if SH14A_03 = 9 then SH14A_03 = .;
    if SH14A_04 = 9 then SH14A_04 = .;
    if SH14A_05 = 9 then SH14A_05 = .;
    if SH14A_06 = 9 then SH14A_06 = .;
    if SH14A_07 = 9 then SH14A_07 = .;
    if SH14A_08 = 9 then SH14A_08 = .;
    if SH14A_09 = 9 then SH14A_09 = .;
    if SH14A_10 = 9 then SH14A_10 = .;
    if SH14A_11 = 9 then SH14A_11 = .;
    if SH14A_12 = 9 then SH14A_12 = .;
    if SH14A_13 = 9 then SH14A_13 = .;
    if SH14A_14 = 9 then SH14A_14 = .;
    if SH14A_15 = 9 then SH14A_15 = .;
    if SH14A_16 = 9 then SH14A_16 = .;
    if SH14A_17 = 9 then SH14A_17 = .;
    if SH14A_18 = 9 then SH14A_18 = .;
    if SH14A_19 = 9 then SH14A_19 = .;
    if SH14A_20 = 9 then SH14A_20 = .;
    if SH14A_21 = 9 then SH14A_21 = .;
    if SH14A_22 = 9 then SH14A_22 = .;
    if SH14A_23 = 9 then SH14A_23 = .;
    if SH14A_24 = 9 then SH14A_24 = .;
    if SH14A_25 = 9 then SH14A_25 = .;
    if SH14A_26 = 9 then SH14A_26 = .;
    if SH14A_27 = 9 then SH14A_27 = .;
    if SH14A_28 = 9 then SH14A_28 = .;
    if SH14A_29 = 9 then SH14A_29 = .;
    if SH14A_30 = 9 then SH14A_30 = .;
    if SH15_01  = 9 then SH15_01 = .;
    if SH15_02  = 9 then SH15_02 = .;
    if SH15_03  = 9 then SH15_03 = .;
    if SH15_04  = 9 then SH15_04 = .;
    if SH15_05  = 9 then SH15_05 = .;
    if SH15_06  = 9 then SH15_06 = .;
    if SH15_07  = 9 then SH15_07 = .;
    if SH15_08  = 9 then SH15_08 = .;
    if SH15_09  = 9 then SH15_09 = .;
    if SH15_10  = 9 then SH15_10 = .;
    if SH15_11  = 9 then SH15_11 = .;
    if SH15_12  = 9 then SH15_12 = .;
    if SH15_13  = 9 then SH15_13 = .;
    if SH15_14  = 9 then SH15_14 = .;
    if SH15_15  = 9 then SH15_15 = .;
    if SH15_16  = 9 then SH15_16 = .;
    if SH15_17  = 9 then SH15_17 = .;
    if SH15_18  = 9 then SH15_18 = .;
    if SH15_19  = 9 then SH15_19 = .;
    if SH15_20  = 9 then SH15_20 = .;
    if SH15_21  = 9 then SH15_21 = .;
    if SH15_22  = 9 then SH15_22 = .;
    if SH15_23  = 9 then SH15_23 = .;
    if SH15_24  = 9 then SH15_24 = .;
    if SH15_25  = 9 then SH15_25 = .;
    if SH15_26  = 9 then SH15_26 = .;
    if SH15_27  = 9 then SH15_27 = .;
    if SH15_28  = 9 then SH15_28 = .;
    if SH15_29  = 9 then SH15_29 = .;
    if SH15_30  = 9 then SH15_30 = .;
    if SH16_01  = 9 then SH16_01 = .;
    if SH16_02  = 9 then SH16_02 = .;
    if SH16_03  = 9 then SH16_03 = .;
    if SH16_04  = 9 then SH16_04 = .;
    if SH16_05  = 9 then SH16_05 = .;
    if SH16_06  = 9 then SH16_06 = .;
    if SH16_07  = 9 then SH16_07 = .;
    if SH16_08  = 9 then SH16_08 = .;
    if SH16_09  = 9 then SH16_09 = .;
    if SH16_10  = 9 then SH16_10 = .;
    if SH16_11  = 9 then SH16_11 = .;
    if SH16_12  = 9 then SH16_12 = .;
    if SH16_13  = 9 then SH16_13 = .;
    if SH16_14  = 9 then SH16_14 = .;
    if SH16_15  = 9 then SH16_15 = .;
    if SH16_16  = 9 then SH16_16 = .;
    if SH16_17  = 9 then SH16_17 = .;
    if SH16_18  = 9 then SH16_18 = .;
    if SH16_19  = 9 then SH16_19 = .;
    if SH16_20  = 9 then SH16_20 = .;
    if SH16_21  = 9 then SH16_21 = .;
    if SH16_22  = 9 then SH16_22 = .;
    if SH16_23  = 9 then SH16_23 = .;
    if SH16_24  = 9 then SH16_24 = .;
    if SH16_25  = 9 then SH16_25 = .;
    if SH16_26  = 9 then SH16_26 = .;
    if SH16_27  = 9 then SH16_27 = .;
    if SH16_28  = 9 then SH16_28 = .;
    if SH16_29  = 9 then SH16_29 = .;
    if SH16_30  = 9 then SH16_30 = .;
    if SH18_01  = 9 then SH18_01 = .;
    if SH18_02  = 9 then SH18_02 = .;
    if SH18_03  = 9 then SH18_03 = .;
    if SH18_04  = 9 then SH18_04 = .;
    if SH18_05  = 9 then SH18_05 = .;
    if SH18_06  = 9 then SH18_06 = .;
    if SH18_07  = 9 then SH18_07 = .;
    if SH18_08  = 9 then SH18_08 = .;
    if SH18_09  = 9 then SH18_09 = .;
    if SH18_10  = 9 then SH18_10 = .;
    if SH18_11  = 9 then SH18_11 = .;
    if SH18_12  = 9 then SH18_12 = .;
    if SH18_13  = 9 then SH18_13 = .;
    if SH18_14  = 9 then SH18_14 = .;
    if SH18_15  = 9 then SH18_15 = .;
    if SH18_16  = 9 then SH18_16 = .;
    if SH18_17  = 9 then SH18_17 = .;
    if SH18_18  = 9 then SH18_18 = .;
    if SH18_19  = 9 then SH18_19 = .;
    if SH18_20  = 9 then SH18_20 = .;
    if SH18_21  = 9 then SH18_21 = .;
    if SH18_22  = 9 then SH18_22 = .;
    if SH18_23  = 9 then SH18_23 = .;
    if SH18_24  = 9 then SH18_24 = .;
    if SH18_25  = 9 then SH18_25 = .;
    if SH18_26  = 9 then SH18_26 = .;
    if SH18_27  = 9 then SH18_27 = .;
    if SH18_28  = 9 then SH18_28 = .;
    if SH18_29  = 9 then SH18_29 = .;
    if SH18_30  = 9 then SH18_30 = .;
    if SH19_01  = 9 then SH19_01 = .;
    if SH19_02  = 9 then SH19_02 = .;
    if SH19_03  = 9 then SH19_03 = .;
    if SH19_04  = 9 then SH19_04 = .;
    if SH19_05  = 9 then SH19_05 = .;
    if SH19_06  = 9 then SH19_06 = .;
    if SH19_07  = 9 then SH19_07 = .;
    if SH19_08  = 9 then SH19_08 = .;
    if SH19_09  = 9 then SH19_09 = .;
    if SH19_10  = 9 then SH19_10 = .;
    if SH19_11  = 9 then SH19_11 = .;
    if SH19_12  = 9 then SH19_12 = .;
    if SH19_13  = 9 then SH19_13 = .;
    if SH19_14  = 9 then SH19_14 = .;
    if SH19_15  = 9 then SH19_15 = .;
    if SH19_16  = 9 then SH19_16 = .;
    if SH19_17  = 9 then SH19_17 = .;
    if SH19_18  = 9 then SH19_18 = .;
    if SH19_19  = 9 then SH19_19 = .;
    if SH19_20  = 9 then SH19_20 = .;
    if SH19_21  = 9 then SH19_21 = .;
    if SH19_22  = 9 then SH19_22 = .;
    if SH19_23  = 9 then SH19_23 = .;
    if SH19_24  = 9 then SH19_24 = .;
    if SH19_25  = 9 then SH19_25 = .;
    if SH19_26  = 9 then SH19_26 = .;
    if SH19_27  = 9 then SH19_27 = .;
    if SH19_28  = 9 then SH19_28 = .;
    if SH19_29  = 9 then SH19_29 = .;
    if SH19_30  = 9 then SH19_30 = .;
    if SH20_01  = 9 then SH20_01 = .;
    if SH20_02  = 9 then SH20_02 = .;
    if SH20_03  = 9 then SH20_03 = .;
    if SH20_04  = 9 then SH20_04 = .;
    if SH20_05  = 9 then SH20_05 = .;
    if SH20_06  = 9 then SH20_06 = .;
    if SH20_07  = 9 then SH20_07 = .;
    if SH20_08  = 9 then SH20_08 = .;
    if SH20_09  = 9 then SH20_09 = .;
    if SH20_10  = 9 then SH20_10 = .;
    if SH20_11  = 9 then SH20_11 = .;
    if SH20_12  = 9 then SH20_12 = .;
    if SH20_13  = 9 then SH20_13 = .;
    if SH20_14  = 9 then SH20_14 = .;
    if SH20_15  = 9 then SH20_15 = .;
    if SH20_16  = 9 then SH20_16 = .;
    if SH20_17  = 9 then SH20_17 = .;
    if SH20_18  = 9 then SH20_18 = .;
    if SH20_19  = 9 then SH20_19 = .;
    if SH20_20  = 9 then SH20_20 = .;
    if SH20_21  = 9 then SH20_21 = .;
    if SH20_22  = 9 then SH20_22 = .;
    if SH20_23  = 9 then SH20_23 = .;
    if SH20_24  = 9 then SH20_24 = .;
    if SH20_25  = 9 then SH20_25 = .;
    if SH20_26  = 9 then SH20_26 = .;
    if SH20_27  = 9 then SH20_27 = .;
    if SH20_28  = 9 then SH20_28 = .;
    if SH20_29  = 9 then SH20_29 = .;
    if SH20_30  = 9 then SH20_30 = .;
    if HA2_1    = 9999 then HA2_1 = .;
    if HA2_2    = 9999 then HA2_2 = .;
    if HA2_3    = 9999 then HA2_3 = .;
    if HA2_4    = 9999 then HA2_4 = .;
    if HA2_5    = 9999 then HA2_5 = .;
    if HA2_6    = 9999 then HA2_6 = .;
    if HA3_1    = 9999 then HA3_1 = .;
    if HA3_2    = 9999 then HA3_2 = .;
    if HA3_3    = 9999 then HA3_3 = .;
    if HA3_4    = 9999 then HA3_4 = .;
    if HA3_5    = 9999 then HA3_5 = .;
    if HA3_6    = 9999 then HA3_6 = .;
    if HA4_1    = 9999 then HA4_1 = .;
    if HA4_2    = 9999 then HA4_2 = .;
    if HA4_3    = 9999 then HA4_3 = .;
    if HA4_4    = 9999 then HA4_4 = .;
    if HA4_5    = 9999 then HA4_5 = .;
    if HA4_6    = 9999 then HA4_6 = .;
    if HA5_1    = 9999 then HA5_1 = .;
    if HA5_2    = 9999 then HA5_2 = .;
    if HA5_3    = 9999 then HA5_3 = .;
    if HA5_4    = 9999 then HA5_4 = .;
    if HA5_5    = 9999 then HA5_5 = .;
    if HA5_6    = 9999 then HA5_6 = .;
    if HA6_1    = 99999 then HA6_1 = .;
    if HA6_2    = 99999 then HA6_2 = .;
    if HA6_3    = 99999 then HA6_3 = .;
    if HA6_4    = 99999 then HA6_4 = .;
    if HA6_5    = 99999 then HA6_5 = .;
    if HA6_6    = 99999 then HA6_6 = .;
    if HA11_1   = 9999 then HA11_1 = .;
    if HA11_2   = 9999 then HA11_2 = .;
    if HA11_3   = 9999 then HA11_3 = .;
    if HA11_4   = 9999 then HA11_4 = .;
    if HA11_5   = 9999 then HA11_5 = .;
    if HA11_6   = 9999 then HA11_6 = .;
    if HA12_1   = 99999 then HA12_1 = .;
    if HA12_2   = 99999 then HA12_2 = .;
    if HA12_3   = 99999 then HA12_3 = .;
    if HA12_4   = 99999 then HA12_4 = .;
    if HA12_5   = 99999 then HA12_5 = .;
    if HA12_6   = 99999 then HA12_6 = .;
    if HA12A_1  = 99999 then HA12A_1 = .;
    if HA12A_2  = 99999 then HA12A_2 = .;
    if HA12A_3  = 99999 then HA12A_3 = .;
    if HA12A_4  = 99999 then HA12A_4 = .;
    if HA12A_5  = 99999 then HA12A_5 = .;
    if HA12A_6  = 99999 then HA12A_6 = .;
    if HA12B_1  = 99999 then HA12B_1 = .;
    if HA12B_2  = 99999 then HA12B_2 = .;
    if HA12B_3  = 99999 then HA12B_3 = .;
    if HA12B_4  = 99999 then HA12B_4 = .;
    if HA12B_5  = 99999 then HA12B_5 = .;
    if HA12B_6  = 99999 then HA12B_6 = .;
    if HA13_1   = 9 then HA13_1 = .;
    if HA13_2   = 9 then HA13_2 = .;
    if HA13_3   = 9 then HA13_3 = .;
    if HA13_4   = 9 then HA13_4 = .;
    if HA13_5   = 9 then HA13_5 = .;
    if HA13_6   = 9 then HA13_6 = .;
    if HA35_1   = 99 then HA35_1 = .;
    if HA35_2   = 99 then HA35_2 = .;
    if HA35_3   = 99 then HA35_3 = .;
    if HA35_4   = 99 then HA35_4 = .;
    if HA35_5   = 99 then HA35_5 = .;
    if HA35_6   = 99 then HA35_6 = .;
    if HA40_1   = 9999 then HA40_1 = .;
    if HA40_2   = 9999 then HA40_2 = .;
    if HA40_3   = 9999 then HA40_3 = .;
    if HA40_4   = 9999 then HA40_4 = .;
    if HA40_5   = 9999 then HA40_5 = .;
    if HA40_6   = 9999 then HA40_6 = .;
    if HA41_1   = 9999 then HA41_1 = .;
    if HA41_2   = 9999 then HA41_2 = .;
    if HA41_3   = 9999 then HA41_3 = .;
    if HA41_4   = 9999 then HA41_4 = .;
    if HA41_5   = 9999 then HA41_5 = .;
    if HA41_6   = 9999 then HA41_6 = .;
    if HA50_1   = 9 then HA50_1 = .;
    if HA50_2   = 9 then HA50_2 = .;
    if HA50_3   = 9 then HA50_3 = .;
    if HA50_4   = 9 then HA50_4 = .;
    if HA50_5   = 9 then HA50_5 = .;
    if HA50_6   = 9 then HA50_6 = .;
    if HA51_1   = 99 then HA51_1 = .;
    if HA51_2   = 99 then HA51_2 = .;
    if HA51_3   = 99 then HA51_3 = .;
    if HA51_4   = 99 then HA51_4 = .;
    if HA51_5   = 99 then HA51_5 = .;
    if HA51_6   = 99 then HA51_6 = .;
    if HA52_1   = 9 then HA52_1 = .;
    if HA52_2   = 9 then HA52_2 = .;
    if HA52_3   = 9 then HA52_3 = .;
    if HA52_4   = 9 then HA52_4 = .;
    if HA52_5   = 9 then HA52_5 = .;
    if HA52_6   = 9 then HA52_6 = .;
    if HA53_1   = 999 then HA53_1 = .;
    if HA53_2   = 999 then HA53_2 = .;
    if HA53_3   = 999 then HA53_3 = .;
    if HA53_4   = 999 then HA53_4 = .;
    if HA53_5   = 999 then HA53_5 = .;
    if HA53_6   = 999 then HA53_6 = .;
    if HA54_1   = 9 then HA54_1 = .;
    if HA54_2   = 9 then HA54_2 = .;
    if HA54_3   = 9 then HA54_3 = .;
    if HA54_4   = 9 then HA54_4 = .;
    if HA54_5   = 9 then HA54_5 = .;
    if HA54_6   = 9 then HA54_6 = .;
    if HA55_1   = 9 then HA55_1 = .;
    if HA55_2   = 9 then HA55_2 = .;
    if HA55_3   = 9 then HA55_3 = .;
    if HA55_4   = 9 then HA55_4 = .;
    if HA55_5   = 9 then HA55_5 = .;
    if HA55_6   = 9 then HA55_6 = .;
    if HA56_1   = 999 then HA56_1 = .;
    if HA56_2   = 999 then HA56_2 = .;
    if HA56_3   = 999 then HA56_3 = .;
    if HA56_4   = 999 then HA56_4 = .;
    if HA56_5   = 999 then HA56_5 = .;
    if HA56_6   = 999 then HA56_6 = .;
    if HA57_1   = 9 then HA57_1 = .;
    if HA57_2   = 9 then HA57_2 = .;
    if HA57_3   = 9 then HA57_3 = .;
    if HA57_4   = 9 then HA57_4 = .;
    if HA57_5   = 9 then HA57_5 = .;
    if HA57_6   = 9 then HA57_6 = .;
    if HA58_1   = 9 then HA58_1 = .;
    if HA58_2   = 9 then HA58_2 = .;
    if HA58_3   = 9 then HA58_3 = .;
    if HA58_4   = 9 then HA58_4 = .;
    if HA58_5   = 9 then HA58_5 = .;
    if HA58_6   = 9 then HA58_6 = .;
    if HA60_1   = 9 then HA60_1 = .;
    if HA60_2   = 9 then HA60_2 = .;
    if HA60_3   = 9 then HA60_3 = .;
    if HA60_4   = 9 then HA60_4 = .;
    if HA60_5   = 9 then HA60_5 = .;
    if HA60_6   = 9 then HA60_6 = .;
    if HA61_1   = 9 then HA61_1 = .;
    if HA61_2   = 9 then HA61_2 = .;
    if HA61_3   = 9 then HA61_3 = .;
    if HA61_4   = 9 then HA61_4 = .;
    if HA61_5   = 9 then HA61_5 = .;
    if HA61_6   = 9 then HA61_6 = .;
    if HA63_1   = 9 then HA63_1 = .;
    if HA63_2   = 9 then HA63_2 = .;
    if HA63_3   = 9 then HA63_3 = .;
    if HA63_4   = 9 then HA63_4 = .;
    if HA63_5   = 9 then HA63_5 = .;
    if HA63_6   = 9 then HA63_6 = .;
    if HA64_1   = 9 then HA64_1 = .;
    if HA64_2   = 9 then HA64_2 = .;
    if HA64_3   = 9 then HA64_3 = .;
    if HA64_4   = 9 then HA64_4 = .;
    if HA64_5   = 9 then HA64_5 = .;
    if HA64_6   = 9 then HA64_6 = .;
    if HA66_1   = 9 then HA66_1 = .;
    if HA66_2   = 9 then HA66_2 = .;
    if HA66_3   = 9 then HA66_3 = .;
    if HA66_4   = 9 then HA66_4 = .;
    if HA66_5   = 9 then HA66_5 = .;
    if HA66_6   = 9 then HA66_6 = .;
    if HA67_1   = 99 then HA67_1 = .;
    if HA67_2   = 99 then HA67_2 = .;
    if HA67_3   = 99 then HA67_3 = .;
    if HA67_4   = 99 then HA67_4 = .;
    if HA67_5   = 99 then HA67_5 = .;
    if HA67_6   = 99 then HA67_6 = .;
    if HA68_1   = 9 then HA68_1 = .;
    if HA68_2   = 9 then HA68_2 = .;
    if HA68_3   = 9 then HA68_3 = .;
    if HA68_4   = 9 then HA68_4 = .;
    if HA68_5   = 9 then HA68_5 = .;
    if HA68_6   = 9 then HA68_6 = .;
    if HC2_1    = 9999 then HC2_1 = .;
    if HC2_2    = 9999 then HC2_2 = .;
    if HC2_3    = 9999 then HC2_3 = .;
    if HC2_4    = 9999 then HC2_4 = .;
    if HC2_5    = 9999 then HC2_5 = .;
    if HC2_6    = 9999 then HC2_6 = .;
    if HC3_1    = 9999 then HC3_1 = .;
    if HC3_2    = 9999 then HC3_2 = .;
    if HC3_3    = 9999 then HC3_3 = .;
    if HC3_4    = 9999 then HC3_4 = .;
    if HC3_5    = 9999 then HC3_5 = .;
    if HC3_6    = 9999 then HC3_6 = .;
    if HC4_1    = 9999 then HC4_1 = .;
    if HC4_2    = 9999 then HC4_2 = .;
    if HC4_3    = 9999 then HC4_3 = .;
    if HC4_4    = 9999 then HC4_4 = .;
    if HC4_5    = 9999 then HC4_5 = .;
    if HC4_6    = 9999 then HC4_6 = .;
    if HC5_1    = 9999 then HC5_1 = .;
    if HC5_2    = 9999 then HC5_2 = .;
    if HC5_3    = 9999 then HC5_3 = .;
    if HC5_4    = 9999 then HC5_4 = .;
    if HC5_5    = 9999 then HC5_5 = .;
    if HC5_6    = 9999 then HC5_6 = .;
    if HC6_1    = 99999 then HC6_1 = .;
    if HC6_2    = 99999 then HC6_2 = .;
    if HC6_3    = 99999 then HC6_3 = .;
    if HC6_4    = 99999 then HC6_4 = .;
    if HC6_5    = 99999 then HC6_5 = .;
    if HC6_6    = 99999 then HC6_6 = .;
    if HC7_1    = 9999 then HC7_1 = .;
    if HC7_2    = 9999 then HC7_2 = .;
    if HC7_3    = 9999 then HC7_3 = .;
    if HC7_4    = 9999 then HC7_4 = .;
    if HC7_5    = 9999 then HC7_5 = .;
    if HC7_6    = 9999 then HC7_6 = .;
    if HC8_1    = 9999 then HC8_1 = .;
    if HC8_2    = 9999 then HC8_2 = .;
    if HC8_3    = 9999 then HC8_3 = .;
    if HC8_4    = 9999 then HC8_4 = .;
    if HC8_5    = 9999 then HC8_5 = .;
    if HC8_6    = 9999 then HC8_6 = .;
    if HC9_1    = 99999 then HC9_1 = .;
    if HC9_2    = 99999 then HC9_2 = .;
    if HC9_3    = 99999 then HC9_3 = .;
    if HC9_4    = 99999 then HC9_4 = .;
    if HC9_5    = 99999 then HC9_5 = .;
    if HC9_6    = 99999 then HC9_6 = .;
    if HC10_1   = 9999 then HC10_1 = .;
    if HC10_2   = 9999 then HC10_2 = .;
    if HC10_3   = 9999 then HC10_3 = .;
    if HC10_4   = 9999 then HC10_4 = .;
    if HC10_5   = 9999 then HC10_5 = .;
    if HC10_6   = 9999 then HC10_6 = .;
    if HC11_1   = 9999 then HC11_1 = .;
    if HC11_2   = 9999 then HC11_2 = .;
    if HC11_3   = 9999 then HC11_3 = .;
    if HC11_4   = 9999 then HC11_4 = .;
    if HC11_5   = 9999 then HC11_5 = .;
    if HC11_6   = 9999 then HC11_6 = .;
    if HC12_1   = 99999 then HC12_1 = .;
    if HC12_2   = 99999 then HC12_2 = .;
    if HC12_3   = 99999 then HC12_3 = .;
    if HC12_4   = 99999 then HC12_4 = .;
    if HC12_5   = 99999 then HC12_5 = .;
    if HC12_6   = 99999 then HC12_6 = .;
    if HC13_1   = 9 then HC13_1 = .;
    if HC13_2   = 9 then HC13_2 = .;
    if HC13_3   = 9 then HC13_3 = .;
    if HC13_4   = 9 then HC13_4 = .;
    if HC13_5   = 9 then HC13_5 = .;
    if HC13_6   = 9 then HC13_6 = .;
    if HC15_1   = 9 then HC15_1 = .;
    if HC15_2   = 9 then HC15_2 = .;
    if HC15_3   = 9 then HC15_3 = .;
    if HC15_4   = 9 then HC15_4 = .;
    if HC15_5   = 9 then HC15_5 = .;
    if HC15_6   = 9 then HC15_6 = .;
    if HC16_1   = 99 then HC16_1 = .;
    if HC16_2   = 99 then HC16_2 = .;
    if HC16_3   = 99 then HC16_3 = .;
    if HC16_4   = 99 then HC16_4 = .;
    if HC16_5   = 99 then HC16_5 = .;
    if HC16_6   = 99 then HC16_6 = .;
    if HC27_1   = 9 then HC27_1 = .;
    if HC27_2   = 9 then HC27_2 = .;
    if HC27_3   = 9 then HC27_3 = .;
    if HC27_4   = 9 then HC27_4 = .;
    if HC27_5   = 9 then HC27_5 = .;
    if HC27_6   = 9 then HC27_6 = .;
    if HC30_1   = 99 then HC30_1 = .;
    if HC30_2   = 99 then HC30_2 = .;
    if HC30_3   = 99 then HC30_3 = .;
    if HC30_4   = 99 then HC30_4 = .;
    if HC30_5   = 99 then HC30_5 = .;
    if HC30_6   = 99 then HC30_6 = .;
    if HC31_1   = 9999 then HC31_1 = .;
    if HC31_2   = 9999 then HC31_2 = .;
    if HC31_3   = 9999 then HC31_3 = .;
    if HC31_4   = 9999 then HC31_4 = .;
    if HC31_5   = 9999 then HC31_5 = .;
    if HC31_6   = 9999 then HC31_6 = .;
    if HC51_1   = 99 then HC51_1 = .;
    if HC51_2   = 99 then HC51_2 = .;
    if HC51_3   = 99 then HC51_3 = .;
    if HC51_4   = 99 then HC51_4 = .;
    if HC51_5   = 99 then HC51_5 = .;
    if HC51_6   = 99 then HC51_6 = .;
    if HC52_1   = 9 then HC52_1 = .;
    if HC52_2   = 9 then HC52_2 = .;
    if HC52_3   = 9 then HC52_3 = .;
    if HC52_4   = 9 then HC52_4 = .;
    if HC52_5   = 9 then HC52_5 = .;
    if HC52_6   = 9 then HC52_6 = .;
    if HC53_1   = 999 then HC53_1 = .;
    if HC53_2   = 999 then HC53_2 = .;
    if HC53_3   = 999 then HC53_3 = .;
    if HC53_4   = 999 then HC53_4 = .;
    if HC53_5   = 999 then HC53_5 = .;
    if HC53_6   = 999 then HC53_6 = .;
    if HC55_1   = 9 then HC55_1 = .;
    if HC55_2   = 9 then HC55_2 = .;
    if HC55_3   = 9 then HC55_3 = .;
    if HC55_4   = 9 then HC55_4 = .;
    if HC55_5   = 9 then HC55_5 = .;
    if HC55_6   = 9 then HC55_6 = .;
    if HC56_1   = 999 then HC56_1 = .;
    if HC56_2   = 999 then HC56_2 = .;
    if HC56_3   = 999 then HC56_3 = .;
    if HC56_4   = 999 then HC56_4 = .;
    if HC56_5   = 999 then HC56_5 = .;
    if HC56_6   = 999 then HC56_6 = .;
    if HC57_1   = 9 then HC57_1 = .;
    if HC57_2   = 9 then HC57_2 = .;
    if HC57_3   = 9 then HC57_3 = .;
    if HC57_4   = 9 then HC57_4 = .;
    if HC57_5   = 9 then HC57_5 = .;
    if HC57_6   = 9 then HC57_6 = .;
    if HC58_1   = 9 then HC58_1 = .;
    if HC58_2   = 9 then HC58_2 = .;
    if HC58_3   = 9 then HC58_3 = .;
    if HC58_4   = 9 then HC58_4 = .;
    if HC58_5   = 9 then HC58_5 = .;
    if HC58_6   = 9 then HC58_6 = .;
    if HC60_1   = 999 then HC60_1 = .;
    if HC60_2   = 999 then HC60_2 = .;
    if HC60_3   = 999 then HC60_3 = .;
    if HC60_4   = 999 then HC60_4 = .;
    if HC60_5   = 999 then HC60_5 = .;
    if HC60_6   = 999 then HC60_6 = .;
    if HC61_1   = 9 then HC61_1 = .;
    if HC61_2   = 9 then HC61_2 = .;
    if HC61_3   = 9 then HC61_3 = .;
    if HC61_4   = 9 then HC61_4 = .;
    if HC61_5   = 9 then HC61_5 = .;
    if HC61_6   = 9 then HC61_6 = .;
    if HC62_1   = 99 then HC62_1 = .;
    if HC62_2   = 99 then HC62_2 = .;
    if HC62_3   = 99 then HC62_3 = .;
    if HC62_4   = 99 then HC62_4 = .;
    if HC62_5   = 99 then HC62_5 = .;
    if HC62_6   = 99 then HC62_6 = .;
    if HC63_1   = 999 then HC63_1 = .;
    if HC63_2   = 999 then HC63_2 = .;
    if HC63_3   = 999 then HC63_3 = .;
    if HC63_4   = 999 then HC63_4 = .;
    if HC63_5   = 999 then HC63_5 = .;
    if HC63_6   = 999 then HC63_6 = .;
    if HC64_1   = 99 then HC64_1 = .;
    if HC64_2   = 99 then HC64_2 = .;
    if HC64_3   = 99 then HC64_3 = .;
    if HC64_4   = 99 then HC64_4 = .;
    if HC64_5   = 99 then HC64_5 = .;
    if HC64_6   = 99 then HC64_6 = .;
    if HC68_1   = 9 then HC68_1 = .;
    if HC68_2   = 9 then HC68_2 = .;
    if HC68_3   = 9 then HC68_3 = .;
    if HC68_4   = 9 then HC68_4 = .;
    if HC68_5   = 9 then HC68_5 = .;
    if HC68_6   = 9 then HC68_6 = .;
    if HC70_1   = 9999 then HC70_1 = .;
    if HC70_2   = 9999 then HC70_2 = .;
    if HC70_3   = 9999 then HC70_3 = .;
    if HC70_4   = 9999 then HC70_4 = .;
    if HC70_5   = 9999 then HC70_5 = .;
    if HC70_6   = 9999 then HC70_6 = .;
    if HC71_1   = 9999 then HC71_1 = .;
    if HC71_2   = 9999 then HC71_2 = .;
    if HC71_3   = 9999 then HC71_3 = .;
    if HC71_4   = 9999 then HC71_4 = .;
    if HC71_5   = 9999 then HC71_5 = .;
    if HC71_6   = 9999 then HC71_6 = .;
    if HC72_1   = 9999 then HC72_1 = .;
    if HC72_2   = 9999 then HC72_2 = .;
    if HC72_3   = 9999 then HC72_3 = .;
    if HC72_4   = 9999 then HC72_4 = .;
    if HC72_5   = 9999 then HC72_5 = .;
    if HC72_6   = 9999 then HC72_6 = .;
    if HC73_1   = 9999 then HC73_1 = .;
    if HC73_2   = 9999 then HC73_2 = .;
    if HC73_3   = 9999 then HC73_3 = .;
    if HC73_4   = 9999 then HC73_4 = .;
    if HC73_5   = 9999 then HC73_5 = .;
    if HC73_6   = 9999 then HC73_6 = .;
    if SB302_01 = 9 then SB302_01 = .;
    if SB302_02 = 9 then SB302_02 = .;
    if SB302_03 = 9 then SB302_03 = .;
    if SB302_04 = 9 then SB302_04 = .;
    if SB302_05 = 9 then SB302_05 = .;
    if SB302_06 = 9 then SB302_06 = .;
    if SB302_07 = 9 then SB302_07 = .;
    if SB302_08 = 9 then SB302_08 = .;
    if SB302_09 = 9 then SB302_09 = .;
    if SB302_10 = 9 then SB302_10 = .;
    if SB302_11 = 9 then SB302_11 = .;
    if SB302_12 = 9 then SB302_12 = .;
    if SB302_13 = 9 then SB302_13 = .;
    if SB302_14 = 9 then SB302_14 = .;
    if SB303_01 = 99 then SB303_01 = .;
    if SB303_02 = 99 then SB303_02 = .;
    if SB303_03 = 99 then SB303_03 = .;
    if SB303_04 = 99 then SB303_04 = .;
    if SB303_05 = 99 then SB303_05 = .;
    if SB303_06 = 99 then SB303_06 = .;
    if SB303_07 = 99 then SB303_07 = .;
    if SB303_08 = 99 then SB303_08 = .;
    if SB303_09 = 99 then SB303_09 = .;
    if SB303_10 = 99 then SB303_10 = .;
    if SB303_11 = 99 then SB303_11 = .;
    if SB303_12 = 99 then SB303_12 = .;
    if SB303_13 = 99 then SB303_13 = .;
    if SB303_14 = 99 then SB303_14 = .;
    if SB304_01 = 9 then SB304_01 = .;
    if SB304_02 = 9 then SB304_02 = .;
    if SB304_03 = 9 then SB304_03 = .;
    if SB304_04 = 9 then SB304_04 = .;
    if SB304_05 = 9 then SB304_05 = .;
    if SB304_06 = 9 then SB304_06 = .;
    if SB304_07 = 9 then SB304_07 = .;
    if SB304_08 = 9 then SB304_08 = .;
    if SB304_09 = 9 then SB304_09 = .;
    if SB304_10 = 9 then SB304_10 = .;
    if SB304_11 = 9 then SB304_11 = .;
    if SB304_12 = 9 then SB304_12 = .;
    if SB304_13 = 9 then SB304_13 = .;
    if SB304_14 = 9 then SB304_14 = .;
    if SB304A_01 = 9 then SB304A_01 = .;
    if SB304A_02 = 9 then SB304A_02 = .;
    if SB304A_03 = 9 then SB304A_03 = .;
    if SB304A_04 = 9 then SB304A_04 = .;
    if SB304A_05 = 9 then SB304A_05 = .;
    if SB304A_06 = 9 then SB304A_06 = .;
    if SB304A_07 = 9 then SB304A_07 = .;
    if SB304A_08 = 9 then SB304A_08 = .;
    if SB304A_09 = 9 then SB304A_09 = .;
    if SB304A_10 = 9 then SB304A_10 = .;
    if SB304A_11 = 9 then SB304A_11 = .;
    if SB304A_12 = 9 then SB304A_12 = .;
    if SB304A_13 = 9 then SB304A_13 = .;
    if SB304A_14 = 9 then SB304A_14 = .;
    if SB304B_01 = 9 then SB304B_01 = .;
    if SB304B_02 = 9 then SB304B_02 = .;
    if SB304B_03 = 9 then SB304B_03 = .;
    if SB304B_04 = 9 then SB304B_04 = .;
    if SB304B_05 = 9 then SB304B_05 = .;
    if SB304B_06 = 9 then SB304B_06 = .;
    if SB304B_07 = 9 then SB304B_07 = .;
    if SB304B_08 = 9 then SB304B_08 = .;
    if SB304B_09 = 9 then SB304B_09 = .;
    if SB304B_10 = 9 then SB304B_10 = .;
    if SB304B_11 = 9 then SB304B_11 = .;
    if SB304B_12 = 9 then SB304B_12 = .;
    if SB304B_13 = 9 then SB304B_13 = .;
    if SB304B_14 = 9 then SB304B_14 = .;
    if SB304C_01 = 9999 then SB304C_01 = .;
    if SB304C_02 = 9999 then SB304C_02 = .;
    if SB304C_03 = 9999 then SB304C_03 = .;
    if SB304C_04 = 9999 then SB304C_04 = .;
    if SB304C_05 = 9999 then SB304C_05 = .;
    if SB304C_06 = 9999 then SB304C_06 = .;
    if SB304C_07 = 9999 then SB304C_07 = .;
    if SB304C_08 = 9999 then SB304C_08 = .;
    if SB304C_09 = 9999 then SB304C_09 = .;
    if SB304C_10 = 9999 then SB304C_10 = .;
    if SB304C_11 = 9999 then SB304C_11 = .;
    if SB304C_12 = 9999 then SB304C_12 = .;
    if SB304C_13 = 9999 then SB304C_13 = .;
    if SB304C_14 = 9999 then SB304C_14 = .;
    if SB304D_01 = 9999 then SB304D_01 = .;
    if SB304D_02 = 9999 then SB304D_02 = .;
    if SB304D_03 = 9999 then SB304D_03 = .;
    if SB304D_04 = 9999 then SB304D_04 = .;
    if SB304D_05 = 9999 then SB304D_05 = .;
    if SB304D_06 = 9999 then SB304D_06 = .;
    if SB304D_07 = 9999 then SB304D_07 = .;
    if SB304D_08 = 9999 then SB304D_08 = .;
    if SB304D_09 = 9999 then SB304D_09 = .;
    if SB304D_10 = 9999 then SB304D_10 = .;
    if SB304D_11 = 9999 then SB304D_11 = .;
    if SB304D_12 = 9999 then SB304D_12 = .;
    if SB304D_13 = 9999 then SB304D_13 = .;
    if SB304D_14 = 9999 then SB304D_14 = .;
    if SB304E_01 = 9999 then SB304E_01 = .;
    if SB304E_02 = 9999 then SB304E_02 = .;
    if SB304E_03 = 9999 then SB304E_03 = .;
    if SB304E_04 = 9999 then SB304E_04 = .;
    if SB304E_05 = 9999 then SB304E_05 = .;
    if SB304E_06 = 9999 then SB304E_06 = .;
    if SB304E_07 = 9999 then SB304E_07 = .;
    if SB304E_08 = 9999 then SB304E_08 = .;
    if SB304E_09 = 9999 then SB304E_09 = .;
    if SB304E_10 = 9999 then SB304E_10 = .;
    if SB304E_11 = 9999 then SB304E_11 = .;
    if SB304E_12 = 9999 then SB304E_12 = .;
    if SB304E_13 = 9999 then SB304E_13 = .;
    if SB304E_14 = 9999 then SB304E_14 = .;
    if SB306_01 = 9 then SB306_01 = .;
    if SB306_02 = 9 then SB306_02 = .;
    if SB306_03 = 9 then SB306_03 = .;
    if SB306_04 = 9 then SB306_04 = .;
    if SB306_05 = 9 then SB306_05 = .;
    if SB306_06 = 9 then SB306_06 = .;
    if SB306_07 = 9 then SB306_07 = .;
    if SB306_08 = 9 then SB306_08 = .;
    if SB306_09 = 9 then SB306_09 = .;
    if SB306_10 = 9 then SB306_10 = .;
    if SB306_11 = 9 then SB306_11 = .;
    if SB306_12 = 9 then SB306_12 = .;
    if SB306_13 = 9 then SB306_13 = .;
    if SB306_14 = 9 then SB306_14 = .;
    if SB307_01 = 9 then SB307_01 = .;
    if SB307_02 = 9 then SB307_02 = .;
    if SB307_03 = 9 then SB307_03 = .;
    if SB307_04 = 9 then SB307_04 = .;
    if SB307_05 = 9 then SB307_05 = .;
    if SB307_06 = 9 then SB307_06 = .;
    if SB307_07 = 9 then SB307_07 = .;
    if SB307_08 = 9 then SB307_08 = .;
    if SB307_09 = 9 then SB307_09 = .;
    if SB307_10 = 9 then SB307_10 = .;
    if SB307_11 = 9 then SB307_11 = .;
    if SB307_12 = 9 then SB307_12 = .;
    if SB307_13 = 9 then SB307_13 = .;
    if SB307_14 = 9 then SB307_14 = .;
    if SB308_01 = 99 then SB308_01 = .;
    if SB308_02 = 99 then SB308_02 = .;
    if SB308_03 = 99 then SB308_03 = .;
    if SB308_04 = 99 then SB308_04 = .;
    if SB308_05 = 99 then SB308_05 = .;
    if SB308_06 = 99 then SB308_06 = .;
    if SB308_07 = 99 then SB308_07 = .;
    if SB308_08 = 99 then SB308_08 = .;
    if SB308_09 = 99 then SB308_09 = .;
    if SB308_10 = 99 then SB308_10 = .;
    if SB308_11 = 99 then SB308_11 = .;
    if SB308_12 = 99 then SB308_12 = .;
    if SB308_13 = 99 then SB308_13 = .;
    if SB308_14 = 99 then SB308_14 = .;
    if SB310_01 = 9 then SB310_01 = .;
    if SB310_02 = 9 then SB310_02 = .;
    if SB310_03 = 9 then SB310_03 = .;
    if SB310_04 = 9 then SB310_04 = .;
    if SB310_05 = 9 then SB310_05 = .;
    if SB310_06 = 9 then SB310_06 = .;
    if SB310_07 = 9 then SB310_07 = .;
    if SB310_08 = 9 then SB310_08 = .;
    if SB310_09 = 9 then SB310_09 = .;
    if SB310_10 = 9 then SB310_10 = .;
    if SB310_11 = 9 then SB310_11 = .;
    if SB310_12 = 9 then SB310_12 = .;
    if SB310_13 = 9 then SB310_13 = .;
    if SB310_14 = 9 then SB310_14 = .;
    if SB311A_01 = 9 then SB311A_01 = .;
    if SB311A_02 = 9 then SB311A_02 = .;
    if SB311A_03 = 9 then SB311A_03 = .;
    if SB311A_04 = 9 then SB311A_04 = .;
    if SB311A_05 = 9 then SB311A_05 = .;
    if SB311A_06 = 9 then SB311A_06 = .;
    if SB311A_07 = 9 then SB311A_07 = .;
    if SB311A_08 = 9 then SB311A_08 = .;
    if SB311A_09 = 9 then SB311A_09 = .;
    if SB311A_10 = 9 then SB311A_10 = .;
    if SB311A_11 = 9 then SB311A_11 = .;
    if SB311A_12 = 9 then SB311A_12 = .;
    if SB311A_13 = 9 then SB311A_13 = .;
    if SB311A_14 = 9 then SB311A_14 = .;
    if SB311B_01 = 9 then SB311B_01 = .;
    if SB311B_02 = 9 then SB311B_02 = .;
    if SB311B_03 = 9 then SB311B_03 = .;
    if SB311B_04 = 9 then SB311B_04 = .;
    if SB311B_05 = 9 then SB311B_05 = .;
    if SB311B_06 = 9 then SB311B_06 = .;
    if SB311B_07 = 9 then SB311B_07 = .;
    if SB311B_08 = 9 then SB311B_08 = .;
    if SB311B_09 = 9 then SB311B_09 = .;
    if SB311B_10 = 9 then SB311B_10 = .;
    if SB311B_11 = 9 then SB311B_11 = .;
    if SB311B_12 = 9 then SB311B_12 = .;
    if SB311B_13 = 9 then SB311B_13 = .;
    if SB311B_14 = 9 then SB311B_14 = .;
    if SB311C_01 = 9 then SB311C_01 = .;
    if SB311C_02 = 9 then SB311C_02 = .;
    if SB311C_03 = 9 then SB311C_03 = .;
    if SB311C_04 = 9 then SB311C_04 = .;
    if SB311C_05 = 9 then SB311C_05 = .;
    if SB311C_06 = 9 then SB311C_06 = .;
    if SB311C_07 = 9 then SB311C_07 = .;
    if SB311C_08 = 9 then SB311C_08 = .;
    if SB311C_09 = 9 then SB311C_09 = .;
    if SB311C_10 = 9 then SB311C_10 = .;
    if SB311C_11 = 9 then SB311C_11 = .;
    if SB311C_12 = 9 then SB311C_12 = .;
    if SB311C_13 = 9 then SB311C_13 = .;
    if SB311C_14 = 9 then SB311C_14 = .;
    if SB312_01 = 99 then SB312_01 = .;
    if SB312_02 = 99 then SB312_02 = .;
    if SB312_03 = 99 then SB312_03 = .;
    if SB312_04 = 99 then SB312_04 = .;
    if SB312_05 = 99 then SB312_05 = .;
    if SB312_06 = 99 then SB312_06 = .;
    if SB312_07 = 99 then SB312_07 = .;
    if SB312_08 = 99 then SB312_08 = .;
    if SB312_09 = 99 then SB312_09 = .;
    if SB312_10 = 99 then SB312_10 = .;
    if SB312_11 = 99 then SB312_11 = .;
    if SB312_12 = 99 then SB312_12 = .;
    if SB312_13 = 99 then SB312_13 = .;
    if SB312_14 = 99 then SB312_14 = .;
    if SB313_01 = 9 then SB313_01 = .;
    if SB313_02 = 9 then SB313_02 = .;
    if SB313_03 = 9 then SB313_03 = .;
    if SB313_04 = 9 then SB313_04 = .;
    if SB313_05 = 9 then SB313_05 = .;
    if SB313_06 = 9 then SB313_06 = .;
    if SB313_07 = 9 then SB313_07 = .;
    if SB313_08 = 9 then SB313_08 = .;
    if SB313_09 = 9 then SB313_09 = .;
    if SB313_10 = 9 then SB313_10 = .;
    if SB313_11 = 9 then SB313_11 = .;
    if SB313_12 = 9 then SB313_12 = .;
    if SB313_13 = 9 then SB313_13 = .;
    if SB313_14 = 9 then SB313_14 = .;
    if SB314_01 = 9999 then SB314_01 = .;
    if SB314_02 = 9999 then SB314_02 = .;
    if SB314_03 = 9999 then SB314_03 = .;
    if SB314_04 = 9999 then SB314_04 = .;
    if SB314_05 = 9999 then SB314_05 = .;
    if SB314_06 = 9999 then SB314_06 = .;
    if SB314_07 = 9999 then SB314_07 = .;
    if SB314_08 = 9999 then SB314_08 = .;
    if SB314_09 = 9999 then SB314_09 = .;
    if SB314_10 = 9999 then SB314_10 = .;
    if SB314_11 = 9999 then SB314_11 = .;
    if SB314_12 = 9999 then SB314_12 = .;
    if SB314_13 = 9999 then SB314_13 = .;
    if SB314_14 = 9999 then SB314_14 = .;
    if SB314A_01 = 9 then SB314A_01 = .;
    if SB314A_02 = 9 then SB314A_02 = .;
    if SB314A_03 = 9 then SB314A_03 = .;
    if SB314A_04 = 9 then SB314A_04 = .;
    if SB314A_05 = 9 then SB314A_05 = .;
    if SB314A_06 = 9 then SB314A_06 = .;
    if SB314A_07 = 9 then SB314A_07 = .;
    if SB314A_08 = 9 then SB314A_08 = .;
    if SB314A_09 = 9 then SB314A_09 = .;
    if SB314A_10 = 9 then SB314A_10 = .;
    if SB314A_11 = 9 then SB314A_11 = .;
    if SB314A_12 = 9 then SB314A_12 = .;
    if SB314A_13 = 9 then SB314A_13 = .;
    if SB314A_14 = 9 then SB314A_14 = .;
    if SB315A_01 = 999 then SB315A_01 = .;
    if SB315A_02 = 999 then SB315A_02 = .;
    if SB315A_03 = 999 then SB315A_03 = .;
    if SB315A_04 = 999 then SB315A_04 = .;
    if SB315A_05 = 999 then SB315A_05 = .;
    if SB315A_06 = 999 then SB315A_06 = .;
    if SB315A_07 = 999 then SB315A_07 = .;
    if SB315A_08 = 999 then SB315A_08 = .;
    if SB315A_09 = 999 then SB315A_09 = .;
    if SB315A_10 = 999 then SB315A_10 = .;
    if SB315A_11 = 999 then SB315A_11 = .;
    if SB315A_12 = 999 then SB315A_12 = .;
    if SB315A_13 = 999 then SB315A_13 = .;
    if SB315A_14 = 999 then SB315A_14 = .;
    if SB315B_01 = 999 then SB315B_01 = .;
    if SB315B_02 = 999 then SB315B_02 = .;
    if SB315B_03 = 999 then SB315B_03 = .;
    if SB315B_04 = 999 then SB315B_04 = .;
    if SB315B_05 = 999 then SB315B_05 = .;
    if SB315B_06 = 999 then SB315B_06 = .;
    if SB315B_07 = 999 then SB315B_07 = .;
    if SB315B_08 = 999 then SB315B_08 = .;
    if SB315B_09 = 999 then SB315B_09 = .;
    if SB315B_10 = 999 then SB315B_10 = .;
    if SB315B_11 = 999 then SB315B_11 = .;
    if SB315B_12 = 999 then SB315B_12 = .;
    if SB315B_13 = 999 then SB315B_13 = .;
    if SB315B_14 = 999 then SB315B_14 = .;
    if SB316_01 = 9 then SB316_01 = .;
    if SB316_02 = 9 then SB316_02 = .;
    if SB316_03 = 9 then SB316_03 = .;
    if SB316_04 = 9 then SB316_04 = .;
    if SB316_05 = 9 then SB316_05 = .;
    if SB316_06 = 9 then SB316_06 = .;
    if SB316_07 = 9 then SB316_07 = .;
    if SB316_08 = 9 then SB316_08 = .;
    if SB316_09 = 9 then SB316_09 = .;
    if SB316_10 = 9 then SB316_10 = .;
    if SB316_11 = 9 then SB316_11 = .;
    if SB316_12 = 9 then SB316_12 = .;
    if SB316_13 = 9 then SB316_13 = .;
    if SB316_14 = 9 then SB316_14 = .;
    if SB317_01 = 999 then SB317_01 = .;
    if SB317_02 = 999 then SB317_02 = .;
    if SB317_03 = 999 then SB317_03 = .;
    if SB317_04 = 999 then SB317_04 = .;
    if SB317_05 = 999 then SB317_05 = .;
    if SB317_06 = 999 then SB317_06 = .;
    if SB317_07 = 999 then SB317_07 = .;
    if SB317_08 = 999 then SB317_08 = .;
    if SB317_09 = 999 then SB317_09 = .;
    if SB317_10 = 999 then SB317_10 = .;
    if SB317_11 = 999 then SB317_11 = .;
    if SB317_12 = 999 then SB317_12 = .;
    if SB317_13 = 999 then SB317_13 = .;
    if SB317_14 = 999 then SB317_14 = .;
    if SB317A_01 = 99 then SB317A_01 = .;
    if SB317A_02 = 99 then SB317A_02 = .;
    if SB317A_03 = 99 then SB317A_03 = .;
    if SB317A_04 = 99 then SB317A_04 = .;
    if SB317A_05 = 99 then SB317A_05 = .;
    if SB317A_06 = 99 then SB317A_06 = .;
    if SB317A_07 = 99 then SB317A_07 = .;
    if SB317A_08 = 99 then SB317A_08 = .;
    if SB317A_09 = 99 then SB317A_09 = .;
    if SB317A_10 = 99 then SB317A_10 = .;
    if SB317A_11 = 99 then SB317A_11 = .;
    if SB317A_12 = 99 then SB317A_12 = .;
    if SB317A_13 = 99 then SB317A_13 = .;
    if SB317A_14 = 99 then SB317A_14 = .;
    if SB317B_01 = 9 then SB317B_01 = .;
    if SB317B_02 = 9 then SB317B_02 = .;
    if SB317B_03 = 9 then SB317B_03 = .;
    if SB317B_04 = 9 then SB317B_04 = .;
    if SB317B_05 = 9 then SB317B_05 = .;
    if SB317B_06 = 9 then SB317B_06 = .;
    if SB317B_07 = 9 then SB317B_07 = .;
    if SB317B_08 = 9 then SB317B_08 = .;
    if SB317B_09 = 9 then SB317B_09 = .;
    if SB317B_10 = 9 then SB317B_10 = .;
    if SB317B_11 = 9 then SB317B_11 = .;
    if SB317B_12 = 9 then SB317B_12 = .;
    if SB317B_13 = 9 then SB317B_13 = .;
    if SB317B_14 = 9 then SB317B_14 = .;
    if SB317CA_01 = 9 then SB317CA_01 = .;
    if SB317CA_02 = 9 then SB317CA_02 = .;
    if SB317CA_03 = 9 then SB317CA_03 = .;
    if SB317CA_04 = 9 then SB317CA_04 = .;
    if SB317CA_05 = 9 then SB317CA_05 = .;
    if SB317CA_06 = 9 then SB317CA_06 = .;
    if SB317CA_07 = 9 then SB317CA_07 = .;
    if SB317CA_08 = 9 then SB317CA_08 = .;
    if SB317CA_09 = 9 then SB317CA_09 = .;
    if SB317CA_10 = 9 then SB317CA_10 = .;
    if SB317CA_11 = 9 then SB317CA_11 = .;
    if SB317CA_12 = 9 then SB317CA_12 = .;
    if SB317CA_13 = 9 then SB317CA_13 = .;
    if SB317CA_14 = 9 then SB317CA_14 = .;
    if SB317CB_01 = 9 then SB317CB_01 = .;
    if SB317CB_02 = 9 then SB317CB_02 = .;
    if SB317CB_03 = 9 then SB317CB_03 = .;
    if SB317CB_04 = 9 then SB317CB_04 = .;
    if SB317CB_05 = 9 then SB317CB_05 = .;
    if SB317CB_06 = 9 then SB317CB_06 = .;
    if SB317CB_07 = 9 then SB317CB_07 = .;
    if SB317CB_08 = 9 then SB317CB_08 = .;
    if SB317CB_09 = 9 then SB317CB_09 = .;
    if SB317CB_10 = 9 then SB317CB_10 = .;
    if SB317CB_11 = 9 then SB317CB_11 = .;
    if SB317CB_12 = 9 then SB317CB_12 = .;
    if SB317CB_13 = 9 then SB317CB_13 = .;
    if SB317CB_14 = 9 then SB317CB_14 = .;
    if SB317CC_01 = 9 then SB317CC_01 = .;
    if SB317CC_02 = 9 then SB317CC_02 = .;
    if SB317CC_03 = 9 then SB317CC_03 = .;
    if SB317CC_04 = 9 then SB317CC_04 = .;
    if SB317CC_05 = 9 then SB317CC_05 = .;
    if SB317CC_06 = 9 then SB317CC_06 = .;
    if SB317CC_07 = 9 then SB317CC_07 = .;
    if SB317CC_08 = 9 then SB317CC_08 = .;
    if SB317CC_09 = 9 then SB317CC_09 = .;
    if SB317CC_10 = 9 then SB317CC_10 = .;
    if SB317CC_11 = 9 then SB317CC_11 = .;
    if SB317CC_12 = 9 then SB317CC_12 = .;
    if SB317CC_13 = 9 then SB317CC_13 = .;
    if SB317CC_14 = 9 then SB317CC_14 = .;
    if SB317CD_01 = 9 then SB317CD_01 = .;
    if SB317CD_02 = 9 then SB317CD_02 = .;
    if SB317CD_03 = 9 then SB317CD_03 = .;
    if SB317CD_04 = 9 then SB317CD_04 = .;
    if SB317CD_05 = 9 then SB317CD_05 = .;
    if SB317CD_06 = 9 then SB317CD_06 = .;
    if SB317CD_07 = 9 then SB317CD_07 = .;
    if SB317CD_08 = 9 then SB317CD_08 = .;
    if SB317CD_09 = 9 then SB317CD_09 = .;
    if SB317CD_10 = 9 then SB317CD_10 = .;
    if SB317CD_11 = 9 then SB317CD_11 = .;
    if SB317CD_12 = 9 then SB317CD_12 = .;
    if SB317CD_13 = 9 then SB317CD_13 = .;
    if SB317CD_14 = 9 then SB317CD_14 = .;
    if SB317CE_01 = 9 then SB317CE_01 = .;
    if SB317CE_02 = 9 then SB317CE_02 = .;
    if SB317CE_03 = 9 then SB317CE_03 = .;
    if SB317CE_04 = 9 then SB317CE_04 = .;
    if SB317CE_05 = 9 then SB317CE_05 = .;
    if SB317CE_06 = 9 then SB317CE_06 = .;
    if SB317CE_07 = 9 then SB317CE_07 = .;
    if SB317CE_08 = 9 then SB317CE_08 = .;
    if SB317CE_09 = 9 then SB317CE_09 = .;
    if SB317CE_10 = 9 then SB317CE_10 = .;
    if SB317CE_11 = 9 then SB317CE_11 = .;
    if SB317CE_12 = 9 then SB317CE_12 = .;
    if SB317CE_13 = 9 then SB317CE_13 = .;
    if SB317CE_14 = 9 then SB317CE_14 = .;
    if SB317CF_01 = 9 then SB317CF_01 = .;
    if SB317CF_02 = 9 then SB317CF_02 = .;
    if SB317CF_03 = 9 then SB317CF_03 = .;
    if SB317CF_04 = 9 then SB317CF_04 = .;
    if SB317CF_05 = 9 then SB317CF_05 = .;
    if SB317CF_06 = 9 then SB317CF_06 = .;
    if SB317CF_07 = 9 then SB317CF_07 = .;
    if SB317CF_08 = 9 then SB317CF_08 = .;
    if SB317CF_09 = 9 then SB317CF_09 = .;
    if SB317CF_10 = 9 then SB317CF_10 = .;
    if SB317CF_11 = 9 then SB317CF_11 = .;
    if SB317CF_12 = 9 then SB317CF_12 = .;
    if SB317CF_13 = 9 then SB317CF_13 = .;
    if SB317CF_14 = 9 then SB317CF_14 = .;
    if SB317CG_01 = 9 then SB317CG_01 = .;
    if SB317CG_02 = 9 then SB317CG_02 = .;
    if SB317CG_03 = 9 then SB317CG_03 = .;
    if SB317CG_04 = 9 then SB317CG_04 = .;
    if SB317CG_05 = 9 then SB317CG_05 = .;
    if SB317CG_06 = 9 then SB317CG_06 = .;
    if SB317CG_07 = 9 then SB317CG_07 = .;
    if SB317CG_08 = 9 then SB317CG_08 = .;
    if SB317CG_09 = 9 then SB317CG_09 = .;
    if SB317CG_10 = 9 then SB317CG_10 = .;
    if SB317CG_11 = 9 then SB317CG_11 = .;
    if SB317CG_12 = 9 then SB317CG_12 = .;
    if SB317CG_13 = 9 then SB317CG_13 = .;
    if SB317CG_14 = 9 then SB317CG_14 = .;
    if SB317CH_01 = 9 then SB317CH_01 = .;
    if SB317CH_02 = 9 then SB317CH_02 = .;
    if SB317CH_03 = 9 then SB317CH_03 = .;
    if SB317CH_04 = 9 then SB317CH_04 = .;
    if SB317CH_05 = 9 then SB317CH_05 = .;
    if SB317CH_06 = 9 then SB317CH_06 = .;
    if SB317CH_07 = 9 then SB317CH_07 = .;
    if SB317CH_08 = 9 then SB317CH_08 = .;
    if SB317CH_09 = 9 then SB317CH_09 = .;
    if SB317CH_10 = 9 then SB317CH_10 = .;
    if SB317CH_11 = 9 then SB317CH_11 = .;
    if SB317CH_12 = 9 then SB317CH_12 = .;
    if SB317CH_13 = 9 then SB317CH_13 = .;
    if SB317CH_14 = 9 then SB317CH_14 = .;
    if SB317CI_01 = 9 then SB317CI_01 = .;
    if SB317CI_02 = 9 then SB317CI_02 = .;
    if SB317CI_03 = 9 then SB317CI_03 = .;
    if SB317CI_04 = 9 then SB317CI_04 = .;
    if SB317CI_05 = 9 then SB317CI_05 = .;
    if SB317CI_06 = 9 then SB317CI_06 = .;
    if SB317CI_07 = 9 then SB317CI_07 = .;
    if SB317CI_08 = 9 then SB317CI_08 = .;
    if SB317CI_09 = 9 then SB317CI_09 = .;
    if SB317CI_10 = 9 then SB317CI_10 = .;
    if SB317CI_11 = 9 then SB317CI_11 = .;
    if SB317CI_12 = 9 then SB317CI_12 = .;
    if SB317CI_13 = 9 then SB317CI_13 = .;
    if SB317CI_14 = 9 then SB317CI_14 = .;
    if SB317CJ_01 = 9 then SB317CJ_01 = .;
    if SB317CJ_02 = 9 then SB317CJ_02 = .;
    if SB317CJ_03 = 9 then SB317CJ_03 = .;
    if SB317CJ_04 = 9 then SB317CJ_04 = .;
    if SB317CJ_05 = 9 then SB317CJ_05 = .;
    if SB317CJ_06 = 9 then SB317CJ_06 = .;
    if SB317CJ_07 = 9 then SB317CJ_07 = .;
    if SB317CJ_08 = 9 then SB317CJ_08 = .;
    if SB317CJ_09 = 9 then SB317CJ_09 = .;
    if SB317CJ_10 = 9 then SB317CJ_10 = .;
    if SB317CJ_11 = 9 then SB317CJ_11 = .;
    if SB317CJ_12 = 9 then SB317CJ_12 = .;
    if SB317CJ_13 = 9 then SB317CJ_13 = .;
    if SB317CJ_14 = 9 then SB317CJ_14 = .;
    if SB317CK_01 = 9 then SB317CK_01 = .;
    if SB317CK_02 = 9 then SB317CK_02 = .;
    if SB317CK_03 = 9 then SB317CK_03 = .;
    if SB317CK_04 = 9 then SB317CK_04 = .;
    if SB317CK_05 = 9 then SB317CK_05 = .;
    if SB317CK_06 = 9 then SB317CK_06 = .;
    if SB317CK_07 = 9 then SB317CK_07 = .;
    if SB317CK_08 = 9 then SB317CK_08 = .;
    if SB317CK_09 = 9 then SB317CK_09 = .;
    if SB317CK_10 = 9 then SB317CK_10 = .;
    if SB317CK_11 = 9 then SB317CK_11 = .;
    if SB317CK_12 = 9 then SB317CK_12 = .;
    if SB317CK_13 = 9 then SB317CK_13 = .;
    if SB317CK_14 = 9 then SB317CK_14 = .;
    if SB317CL_01 = 9 then SB317CL_01 = .;
    if SB317CL_02 = 9 then SB317CL_02 = .;
    if SB317CL_03 = 9 then SB317CL_03 = .;
    if SB317CL_04 = 9 then SB317CL_04 = .;
    if SB317CL_05 = 9 then SB317CL_05 = .;
    if SB317CL_06 = 9 then SB317CL_06 = .;
    if SB317CL_07 = 9 then SB317CL_07 = .;
    if SB317CL_08 = 9 then SB317CL_08 = .;
    if SB317CL_09 = 9 then SB317CL_09 = .;
    if SB317CL_10 = 9 then SB317CL_10 = .;
    if SB317CL_11 = 9 then SB317CL_11 = .;
    if SB317CL_12 = 9 then SB317CL_12 = .;
    if SB317CL_13 = 9 then SB317CL_13 = .;
    if SB317CL_14 = 9 then SB317CL_14 = .;
    if SB317CM_01 = 9 then SB317CM_01 = .;
    if SB317CM_02 = 9 then SB317CM_02 = .;
    if SB317CM_03 = 9 then SB317CM_03 = .;
    if SB317CM_04 = 9 then SB317CM_04 = .;
    if SB317CM_05 = 9 then SB317CM_05 = .;
    if SB317CM_06 = 9 then SB317CM_06 = .;
    if SB317CM_07 = 9 then SB317CM_07 = .;
    if SB317CM_08 = 9 then SB317CM_08 = .;
    if SB317CM_09 = 9 then SB317CM_09 = .;
    if SB317CM_10 = 9 then SB317CM_10 = .;
    if SB317CM_11 = 9 then SB317CM_11 = .;
    if SB317CM_12 = 9 then SB317CM_12 = .;
    if SB317CM_13 = 9 then SB317CM_13 = .;
    if SB317CM_14 = 9 then SB317CM_14 = .;
    if SB317CN_01 = 9 then SB317CN_01 = .;
    if SB317CN_02 = 9 then SB317CN_02 = .;
    if SB317CN_03 = 9 then SB317CN_03 = .;
    if SB317CN_04 = 9 then SB317CN_04 = .;
    if SB317CN_05 = 9 then SB317CN_05 = .;
    if SB317CN_06 = 9 then SB317CN_06 = .;
    if SB317CN_07 = 9 then SB317CN_07 = .;
    if SB317CN_08 = 9 then SB317CN_08 = .;
    if SB317CN_09 = 9 then SB317CN_09 = .;
    if SB317CN_10 = 9 then SB317CN_10 = .;
    if SB317CN_11 = 9 then SB317CN_11 = .;
    if SB317CN_12 = 9 then SB317CN_12 = .;
    if SB317CN_13 = 9 then SB317CN_13 = .;
    if SB317CN_14 = 9 then SB317CN_14 = .;
    if SB317CX_01 = 9 then SB317CX_01 = .;
    if SB317CX_02 = 9 then SB317CX_02 = .;
    if SB317CX_03 = 9 then SB317CX_03 = .;
    if SB317CX_04 = 9 then SB317CX_04 = .;
    if SB317CX_05 = 9 then SB317CX_05 = .;
    if SB317CX_06 = 9 then SB317CX_06 = .;
    if SB317CX_07 = 9 then SB317CX_07 = .;
    if SB317CX_08 = 9 then SB317CX_08 = .;
    if SB317CX_09 = 9 then SB317CX_09 = .;
    if SB317CX_10 = 9 then SB317CX_10 = .;
    if SB317CX_11 = 9 then SB317CX_11 = .;
    if SB317CX_12 = 9 then SB317CX_12 = .;
    if SB317CX_13 = 9 then SB317CX_13 = .;
    if SB317CX_14 = 9 then SB317CX_14 = .;
    if SB318A_01 = 9 then SB318A_01 = .;
    if SB318A_02 = 9 then SB318A_02 = .;
    if SB318A_03 = 9 then SB318A_03 = .;
    if SB318A_04 = 9 then SB318A_04 = .;
    if SB318A_05 = 9 then SB318A_05 = .;
    if SB318A_06 = 9 then SB318A_06 = .;
    if SB318A_07 = 9 then SB318A_07 = .;
    if SB318A_08 = 9 then SB318A_08 = .;
    if SB318A_09 = 9 then SB318A_09 = .;
    if SB318A_10 = 9 then SB318A_10 = .;
    if SB318A_11 = 9 then SB318A_11 = .;
    if SB318A_12 = 9 then SB318A_12 = .;
    if SB318A_13 = 9 then SB318A_13 = .;
    if SB318A_14 = 9 then SB318A_14 = .;
    if SB318B_01 = 9 then SB318B_01 = .;
    if SB318B_02 = 9 then SB318B_02 = .;
    if SB318B_03 = 9 then SB318B_03 = .;
    if SB318B_04 = 9 then SB318B_04 = .;
    if SB318B_05 = 9 then SB318B_05 = .;
    if SB318B_06 = 9 then SB318B_06 = .;
    if SB318B_07 = 9 then SB318B_07 = .;
    if SB318B_08 = 9 then SB318B_08 = .;
    if SB318B_09 = 9 then SB318B_09 = .;
    if SB318B_10 = 9 then SB318B_10 = .;
    if SB318B_11 = 9 then SB318B_11 = .;
    if SB318B_12 = 9 then SB318B_12 = .;
    if SB318B_13 = 9 then SB318B_13 = .;
    if SB318B_14 = 9 then SB318B_14 = .;
    if SB318C_01 = 9 then SB318C_01 = .;
    if SB318C_02 = 9 then SB318C_02 = .;
    if SB318C_03 = 9 then SB318C_03 = .;
    if SB318C_04 = 9 then SB318C_04 = .;
    if SB318C_05 = 9 then SB318C_05 = .;
    if SB318C_06 = 9 then SB318C_06 = .;
    if SB318C_07 = 9 then SB318C_07 = .;
    if SB318C_08 = 9 then SB318C_08 = .;
    if SB318C_09 = 9 then SB318C_09 = .;
    if SB318C_10 = 9 then SB318C_10 = .;
    if SB318C_11 = 9 then SB318C_11 = .;
    if SB318C_12 = 9 then SB318C_12 = .;
    if SB318C_13 = 9 then SB318C_13 = .;
    if SB318C_14 = 9 then SB318C_14 = .;
    if SB318D_01 = 9 then SB318D_01 = .;
    if SB318D_02 = 9 then SB318D_02 = .;
    if SB318D_03 = 9 then SB318D_03 = .;
    if SB318D_04 = 9 then SB318D_04 = .;
    if SB318D_05 = 9 then SB318D_05 = .;
    if SB318D_06 = 9 then SB318D_06 = .;
    if SB318D_07 = 9 then SB318D_07 = .;
    if SB318D_08 = 9 then SB318D_08 = .;
    if SB318D_09 = 9 then SB318D_09 = .;
    if SB318D_10 = 9 then SB318D_10 = .;
    if SB318D_11 = 9 then SB318D_11 = .;
    if SB318D_12 = 9 then SB318D_12 = .;
    if SB318D_13 = 9 then SB318D_13 = .;
    if SB318D_14 = 9 then SB318D_14 = .;
    if SB318E_01 = 9 then SB318E_01 = .;
    if SB318E_02 = 9 then SB318E_02 = .;
    if SB318E_03 = 9 then SB318E_03 = .;
    if SB318E_04 = 9 then SB318E_04 = .;
    if SB318E_05 = 9 then SB318E_05 = .;
    if SB318E_06 = 9 then SB318E_06 = .;
    if SB318E_07 = 9 then SB318E_07 = .;
    if SB318E_08 = 9 then SB318E_08 = .;
    if SB318E_09 = 9 then SB318E_09 = .;
    if SB318E_10 = 9 then SB318E_10 = .;
    if SB318E_11 = 9 then SB318E_11 = .;
    if SB318E_12 = 9 then SB318E_12 = .;
    if SB318E_13 = 9 then SB318E_13 = .;
    if SB318E_14 = 9 then SB318E_14 = .;
    if SB319_01 = 9 then SB319_01 = .;
    if SB319_02 = 9 then SB319_02 = .;
    if SB319_03 = 9 then SB319_03 = .;
    if SB319_04 = 9 then SB319_04 = .;
    if SB319_05 = 9 then SB319_05 = .;
    if SB319_06 = 9 then SB319_06 = .;
    if SB319_07 = 9 then SB319_07 = .;
    if SB319_08 = 9 then SB319_08 = .;
    if SB319_09 = 9 then SB319_09 = .;
    if SB319_10 = 9 then SB319_10 = .;
    if SB319_11 = 9 then SB319_11 = .;
    if SB319_12 = 9 then SB319_12 = .;
    if SB319_13 = 9 then SB319_13 = .;
    if SB319_14 = 9 then SB319_14 = .;
    if SB321_01 = 9999 then SB321_01 = .;
    if SB321_02 = 9999 then SB321_02 = .;
    if SB321_03 = 9999 then SB321_03 = .;
    if SB321_04 = 9999 then SB321_04 = .;
    if SB321_05 = 9999 then SB321_05 = .;
    if SB321_06 = 9999 then SB321_06 = .;
    if SB321_07 = 9999 then SB321_07 = .;
    if SB321_08 = 9999 then SB321_08 = .;
    if SB321_09 = 9999 then SB321_09 = .;
    if SB321_10 = 9999 then SB321_10 = .;
    if SB321_11 = 9999 then SB321_11 = .;
    if SB321_12 = 9999 then SB321_12 = .;
    if SB321_13 = 9999 then SB321_13 = .;
    if SB321_14 = 9999 then SB321_14 = .;
    if SB322_01 = 9 then SB322_01 = .;
    if SB322_02 = 9 then SB322_02 = .;
    if SB322_03 = 9 then SB322_03 = .;
    if SB322_04 = 9 then SB322_04 = .;
    if SB322_05 = 9 then SB322_05 = .;
    if SB322_06 = 9 then SB322_06 = .;
    if SB322_07 = 9 then SB322_07 = .;
    if SB322_08 = 9 then SB322_08 = .;
    if SB322_09 = 9 then SB322_09 = .;
    if SB322_10 = 9 then SB322_10 = .;
    if SB322_11 = 9 then SB322_11 = .;
    if SB322_12 = 9 then SB322_12 = .;
    if SB322_13 = 9 then SB322_13 = .;
    if SB322_14 = 9 then SB322_14 = .;
    if SB323A_01 = 999 then SB323A_01 = .;
    if SB323A_02 = 999 then SB323A_02 = .;
    if SB323A_03 = 999 then SB323A_03 = .;
    if SB323A_04 = 999 then SB323A_04 = .;
    if SB323A_05 = 999 then SB323A_05 = .;
    if SB323A_06 = 999 then SB323A_06 = .;
    if SB323A_07 = 999 then SB323A_07 = .;
    if SB323A_08 = 999 then SB323A_08 = .;
    if SB323A_09 = 999 then SB323A_09 = .;
    if SB323A_10 = 999 then SB323A_10 = .;
    if SB323A_11 = 999 then SB323A_11 = .;
    if SB323A_12 = 999 then SB323A_12 = .;
    if SB323A_13 = 999 then SB323A_13 = .;
    if SB323A_14 = 999 then SB323A_14 = .;
    if SB323B_01 = 999 then SB323B_01 = .;
    if SB323B_02 = 999 then SB323B_02 = .;
    if SB323B_03 = 999 then SB323B_03 = .;
    if SB323B_04 = 999 then SB323B_04 = .;
    if SB323B_05 = 999 then SB323B_05 = .;
    if SB323B_06 = 999 then SB323B_06 = .;
    if SB323B_07 = 999 then SB323B_07 = .;
    if SB323B_08 = 999 then SB323B_08 = .;
    if SB323B_09 = 999 then SB323B_09 = .;
    if SB323B_10 = 999 then SB323B_10 = .;
    if SB323B_11 = 999 then SB323B_11 = .;
    if SB323B_12 = 999 then SB323B_12 = .;
    if SB323B_13 = 999 then SB323B_13 = .;
    if SB323B_14 = 999 then SB323B_14 = .;
    if SB324_01 = 9 then SB324_01 = .;
    if SB324_02 = 9 then SB324_02 = .;
    if SB324_03 = 9 then SB324_03 = .;
    if SB324_04 = 9 then SB324_04 = .;
    if SB324_05 = 9 then SB324_05 = .;
    if SB324_06 = 9 then SB324_06 = .;
    if SB324_07 = 9 then SB324_07 = .;
    if SB324_08 = 9 then SB324_08 = .;
    if SB324_09 = 9 then SB324_09 = .;
    if SB324_10 = 9 then SB324_10 = .;
    if SB324_11 = 9 then SB324_11 = .;
    if SB324_12 = 9 then SB324_12 = .;
    if SB324_13 = 9 then SB324_13 = .;
    if SB324_14 = 9 then SB324_14 = .;
    if SB325_01 = 9 then SB325_01 = .;
    if SB325_02 = 9 then SB325_02 = .;
    if SB325_03 = 9 then SB325_03 = .;
    if SB325_04 = 9 then SB325_04 = .;
    if SB325_05 = 9 then SB325_05 = .;
    if SB325_06 = 9 then SB325_06 = .;
    if SB325_07 = 9 then SB325_07 = .;
    if SB325_08 = 9 then SB325_08 = .;
    if SB325_09 = 9 then SB325_09 = .;
    if SB325_10 = 9 then SB325_10 = .;
    if SB325_11 = 9 then SB325_11 = .;
    if SB325_12 = 9 then SB325_12 = .;
    if SB325_13 = 9 then SB325_13 = .;
    if SB325_14 = 9 then SB325_14 = .;
    if SB326A_01 = 99 then SB326A_01 = .;
    if SB326A_02 = 99 then SB326A_02 = .;
    if SB326A_03 = 99 then SB326A_03 = .;
    if SB326A_04 = 99 then SB326A_04 = .;
    if SB326A_05 = 99 then SB326A_05 = .;
    if SB326A_06 = 99 then SB326A_06 = .;
    if SB326A_07 = 99 then SB326A_07 = .;
    if SB326A_08 = 99 then SB326A_08 = .;
    if SB326A_09 = 99 then SB326A_09 = .;
    if SB326A_10 = 99 then SB326A_10 = .;
    if SB326A_11 = 99 then SB326A_11 = .;
    if SB326A_12 = 99 then SB326A_12 = .;
    if SB326A_13 = 99 then SB326A_13 = .;
    if SB326A_14 = 99 then SB326A_14 = .;
    if SB326B_01 = 9 then SB326B_01 = .;
    if SB326B_02 = 9 then SB326B_02 = .;
    if SB326B_03 = 9 then SB326B_03 = .;
    if SB326B_04 = 9 then SB326B_04 = .;
    if SB326B_05 = 9 then SB326B_05 = .;
    if SB326B_06 = 9 then SB326B_06 = .;
    if SB326B_07 = 9 then SB326B_07 = .;
    if SB326B_08 = 9 then SB326B_08 = .;
    if SB326B_09 = 9 then SB326B_09 = .;
    if SB326B_10 = 9 then SB326B_10 = .;
    if SB326B_11 = 9 then SB326B_11 = .;
    if SB326B_12 = 9 then SB326B_12 = .;
    if SB326B_13 = 9 then SB326B_13 = .;
    if SB326B_14 = 9 then SB326B_14 = .;
    if SB326CA_01 = 9 then SB326CA_01 = .;
    if SB326CA_02 = 9 then SB326CA_02 = .;
    if SB326CA_03 = 9 then SB326CA_03 = .;
    if SB326CA_04 = 9 then SB326CA_04 = .;
    if SB326CA_05 = 9 then SB326CA_05 = .;
    if SB326CA_06 = 9 then SB326CA_06 = .;
    if SB326CA_07 = 9 then SB326CA_07 = .;
    if SB326CA_08 = 9 then SB326CA_08 = .;
    if SB326CA_09 = 9 then SB326CA_09 = .;
    if SB326CA_10 = 9 then SB326CA_10 = .;
    if SB326CA_11 = 9 then SB326CA_11 = .;
    if SB326CA_12 = 9 then SB326CA_12 = .;
    if SB326CA_13 = 9 then SB326CA_13 = .;
    if SB326CA_14 = 9 then SB326CA_14 = .;
    if SB326CB_01 = 9 then SB326CB_01 = .;
    if SB326CB_02 = 9 then SB326CB_02 = .;
    if SB326CB_03 = 9 then SB326CB_03 = .;
    if SB326CB_04 = 9 then SB326CB_04 = .;
    if SB326CB_05 = 9 then SB326CB_05 = .;
    if SB326CB_06 = 9 then SB326CB_06 = .;
    if SB326CB_07 = 9 then SB326CB_07 = .;
    if SB326CB_08 = 9 then SB326CB_08 = .;
    if SB326CB_09 = 9 then SB326CB_09 = .;
    if SB326CB_10 = 9 then SB326CB_10 = .;
    if SB326CB_11 = 9 then SB326CB_11 = .;
    if SB326CB_12 = 9 then SB326CB_12 = .;
    if SB326CB_13 = 9 then SB326CB_13 = .;
    if SB326CB_14 = 9 then SB326CB_14 = .;
    if SB326CC_01 = 9 then SB326CC_01 = .;
    if SB326CC_02 = 9 then SB326CC_02 = .;
    if SB326CC_03 = 9 then SB326CC_03 = .;
    if SB326CC_04 = 9 then SB326CC_04 = .;
    if SB326CC_05 = 9 then SB326CC_05 = .;
    if SB326CC_06 = 9 then SB326CC_06 = .;
    if SB326CC_07 = 9 then SB326CC_07 = .;
    if SB326CC_08 = 9 then SB326CC_08 = .;
    if SB326CC_09 = 9 then SB326CC_09 = .;
    if SB326CC_10 = 9 then SB326CC_10 = .;
    if SB326CC_11 = 9 then SB326CC_11 = .;
    if SB326CC_12 = 9 then SB326CC_12 = .;
    if SB326CC_13 = 9 then SB326CC_13 = .;
    if SB326CC_14 = 9 then SB326CC_14 = .;
    if SB326CD_01 = 9 then SB326CD_01 = .;
    if SB326CD_02 = 9 then SB326CD_02 = .;
    if SB326CD_03 = 9 then SB326CD_03 = .;
    if SB326CD_04 = 9 then SB326CD_04 = .;
    if SB326CD_05 = 9 then SB326CD_05 = .;
    if SB326CD_06 = 9 then SB326CD_06 = .;
    if SB326CD_07 = 9 then SB326CD_07 = .;
    if SB326CD_08 = 9 then SB326CD_08 = .;
    if SB326CD_09 = 9 then SB326CD_09 = .;
    if SB326CD_10 = 9 then SB326CD_10 = .;
    if SB326CD_11 = 9 then SB326CD_11 = .;
    if SB326CD_12 = 9 then SB326CD_12 = .;
    if SB326CD_13 = 9 then SB326CD_13 = .;
    if SB326CD_14 = 9 then SB326CD_14 = .;
    if SB326CE_01 = 9 then SB326CE_01 = .;
    if SB326CE_02 = 9 then SB326CE_02 = .;
    if SB326CE_03 = 9 then SB326CE_03 = .;
    if SB326CE_04 = 9 then SB326CE_04 = .;
    if SB326CE_05 = 9 then SB326CE_05 = .;
    if SB326CE_06 = 9 then SB326CE_06 = .;
    if SB326CE_07 = 9 then SB326CE_07 = .;
    if SB326CE_08 = 9 then SB326CE_08 = .;
    if SB326CE_09 = 9 then SB326CE_09 = .;
    if SB326CE_10 = 9 then SB326CE_10 = .;
    if SB326CE_11 = 9 then SB326CE_11 = .;
    if SB326CE_12 = 9 then SB326CE_12 = .;
    if SB326CE_13 = 9 then SB326CE_13 = .;
    if SB326CE_14 = 9 then SB326CE_14 = .;
    if SB326CF_01 = 9 then SB326CF_01 = .;
    if SB326CF_02 = 9 then SB326CF_02 = .;
    if SB326CF_03 = 9 then SB326CF_03 = .;
    if SB326CF_04 = 9 then SB326CF_04 = .;
    if SB326CF_05 = 9 then SB326CF_05 = .;
    if SB326CF_06 = 9 then SB326CF_06 = .;
    if SB326CF_07 = 9 then SB326CF_07 = .;
    if SB326CF_08 = 9 then SB326CF_08 = .;
    if SB326CF_09 = 9 then SB326CF_09 = .;
    if SB326CF_10 = 9 then SB326CF_10 = .;
    if SB326CF_11 = 9 then SB326CF_11 = .;
    if SB326CF_12 = 9 then SB326CF_12 = .;
    if SB326CF_13 = 9 then SB326CF_13 = .;
    if SB326CF_14 = 9 then SB326CF_14 = .;
    if SB326CG_01 = 9 then SB326CG_01 = .;
    if SB326CG_02 = 9 then SB326CG_02 = .;
    if SB326CG_03 = 9 then SB326CG_03 = .;
    if SB326CG_04 = 9 then SB326CG_04 = .;
    if SB326CG_05 = 9 then SB326CG_05 = .;
    if SB326CG_06 = 9 then SB326CG_06 = .;
    if SB326CG_07 = 9 then SB326CG_07 = .;
    if SB326CG_08 = 9 then SB326CG_08 = .;
    if SB326CG_09 = 9 then SB326CG_09 = .;
    if SB326CG_10 = 9 then SB326CG_10 = .;
    if SB326CG_11 = 9 then SB326CG_11 = .;
    if SB326CG_12 = 9 then SB326CG_12 = .;
    if SB326CG_13 = 9 then SB326CG_13 = .;
    if SB326CG_14 = 9 then SB326CG_14 = .;
    if SB326CH_01 = 9 then SB326CH_01 = .;
    if SB326CH_02 = 9 then SB326CH_02 = .;
    if SB326CH_03 = 9 then SB326CH_03 = .;
    if SB326CH_04 = 9 then SB326CH_04 = .;
    if SB326CH_05 = 9 then SB326CH_05 = .;
    if SB326CH_06 = 9 then SB326CH_06 = .;
    if SB326CH_07 = 9 then SB326CH_07 = .;
    if SB326CH_08 = 9 then SB326CH_08 = .;
    if SB326CH_09 = 9 then SB326CH_09 = .;
    if SB326CH_10 = 9 then SB326CH_10 = .;
    if SB326CH_11 = 9 then SB326CH_11 = .;
    if SB326CH_12 = 9 then SB326CH_12 = .;
    if SB326CH_13 = 9 then SB326CH_13 = .;
    if SB326CH_14 = 9 then SB326CH_14 = .;
    if SB326CI_01 = 9 then SB326CI_01 = .;
    if SB326CI_02 = 9 then SB326CI_02 = .;
    if SB326CI_03 = 9 then SB326CI_03 = .;
    if SB326CI_04 = 9 then SB326CI_04 = .;
    if SB326CI_05 = 9 then SB326CI_05 = .;
    if SB326CI_06 = 9 then SB326CI_06 = .;
    if SB326CI_07 = 9 then SB326CI_07 = .;
    if SB326CI_08 = 9 then SB326CI_08 = .;
    if SB326CI_09 = 9 then SB326CI_09 = .;
    if SB326CI_10 = 9 then SB326CI_10 = .;
    if SB326CI_11 = 9 then SB326CI_11 = .;
    if SB326CI_12 = 9 then SB326CI_12 = .;
    if SB326CI_13 = 9 then SB326CI_13 = .;
    if SB326CI_14 = 9 then SB326CI_14 = .;
    if SB326CJ_01 = 9 then SB326CJ_01 = .;
    if SB326CJ_02 = 9 then SB326CJ_02 = .;
    if SB326CJ_03 = 9 then SB326CJ_03 = .;
    if SB326CJ_04 = 9 then SB326CJ_04 = .;
    if SB326CJ_05 = 9 then SB326CJ_05 = .;
    if SB326CJ_06 = 9 then SB326CJ_06 = .;
    if SB326CJ_07 = 9 then SB326CJ_07 = .;
    if SB326CJ_08 = 9 then SB326CJ_08 = .;
    if SB326CJ_09 = 9 then SB326CJ_09 = .;
    if SB326CJ_10 = 9 then SB326CJ_10 = .;
    if SB326CJ_11 = 9 then SB326CJ_11 = .;
    if SB326CJ_12 = 9 then SB326CJ_12 = .;
    if SB326CJ_13 = 9 then SB326CJ_13 = .;
    if SB326CJ_14 = 9 then SB326CJ_14 = .;
    if SB326CK_01 = 9 then SB326CK_01 = .;
    if SB326CK_02 = 9 then SB326CK_02 = .;
    if SB326CK_03 = 9 then SB326CK_03 = .;
    if SB326CK_04 = 9 then SB326CK_04 = .;
    if SB326CK_05 = 9 then SB326CK_05 = .;
    if SB326CK_06 = 9 then SB326CK_06 = .;
    if SB326CK_07 = 9 then SB326CK_07 = .;
    if SB326CK_08 = 9 then SB326CK_08 = .;
    if SB326CK_09 = 9 then SB326CK_09 = .;
    if SB326CK_10 = 9 then SB326CK_10 = .;
    if SB326CK_11 = 9 then SB326CK_11 = .;
    if SB326CK_12 = 9 then SB326CK_12 = .;
    if SB326CK_13 = 9 then SB326CK_13 = .;
    if SB326CK_14 = 9 then SB326CK_14 = .;
    if SB326CL_01 = 9 then SB326CL_01 = .;
    if SB326CL_02 = 9 then SB326CL_02 = .;
    if SB326CL_03 = 9 then SB326CL_03 = .;
    if SB326CL_04 = 9 then SB326CL_04 = .;
    if SB326CL_05 = 9 then SB326CL_05 = .;
    if SB326CL_06 = 9 then SB326CL_06 = .;
    if SB326CL_07 = 9 then SB326CL_07 = .;
    if SB326CL_08 = 9 then SB326CL_08 = .;
    if SB326CL_09 = 9 then SB326CL_09 = .;
    if SB326CL_10 = 9 then SB326CL_10 = .;
    if SB326CL_11 = 9 then SB326CL_11 = .;
    if SB326CL_12 = 9 then SB326CL_12 = .;
    if SB326CL_13 = 9 then SB326CL_13 = .;
    if SB326CL_14 = 9 then SB326CL_14 = .;
    if SB326CM_01 = 9 then SB326CM_01 = .;
    if SB326CM_02 = 9 then SB326CM_02 = .;
    if SB326CM_03 = 9 then SB326CM_03 = .;
    if SB326CM_04 = 9 then SB326CM_04 = .;
    if SB326CM_05 = 9 then SB326CM_05 = .;
    if SB326CM_06 = 9 then SB326CM_06 = .;
    if SB326CM_07 = 9 then SB326CM_07 = .;
    if SB326CM_08 = 9 then SB326CM_08 = .;
    if SB326CM_09 = 9 then SB326CM_09 = .;
    if SB326CM_10 = 9 then SB326CM_10 = .;
    if SB326CM_11 = 9 then SB326CM_11 = .;
    if SB326CM_12 = 9 then SB326CM_12 = .;
    if SB326CM_13 = 9 then SB326CM_13 = .;
    if SB326CM_14 = 9 then SB326CM_14 = .;
    if SB326CN_01 = 9 then SB326CN_01 = .;
    if SB326CN_02 = 9 then SB326CN_02 = .;
    if SB326CN_03 = 9 then SB326CN_03 = .;
    if SB326CN_04 = 9 then SB326CN_04 = .;
    if SB326CN_05 = 9 then SB326CN_05 = .;
    if SB326CN_06 = 9 then SB326CN_06 = .;
    if SB326CN_07 = 9 then SB326CN_07 = .;
    if SB326CN_08 = 9 then SB326CN_08 = .;
    if SB326CN_09 = 9 then SB326CN_09 = .;
    if SB326CN_10 = 9 then SB326CN_10 = .;
    if SB326CN_11 = 9 then SB326CN_11 = .;
    if SB326CN_12 = 9 then SB326CN_12 = .;
    if SB326CN_13 = 9 then SB326CN_13 = .;
    if SB326CN_14 = 9 then SB326CN_14 = .;
    if SB326CX_01 = 9 then SB326CX_01 = .;
    if SB326CX_02 = 9 then SB326CX_02 = .;
    if SB326CX_03 = 9 then SB326CX_03 = .;
    if SB326CX_04 = 9 then SB326CX_04 = .;
    if SB326CX_05 = 9 then SB326CX_05 = .;
    if SB326CX_06 = 9 then SB326CX_06 = .;
    if SB326CX_07 = 9 then SB326CX_07 = .;
    if SB326CX_08 = 9 then SB326CX_08 = .;
    if SB326CX_09 = 9 then SB326CX_09 = .;
    if SB326CX_10 = 9 then SB326CX_10 = .;
    if SB326CX_11 = 9 then SB326CX_11 = .;
    if SB326CX_12 = 9 then SB326CX_12 = .;
    if SB326CX_13 = 9 then SB326CX_13 = .;
    if SB326CX_14 = 9 then SB326CX_14 = .;
    if SB327A_01 = 9 then SB327A_01 = .;
    if SB327A_02 = 9 then SB327A_02 = .;
    if SB327A_03 = 9 then SB327A_03 = .;
    if SB327A_04 = 9 then SB327A_04 = .;
    if SB327A_05 = 9 then SB327A_05 = .;
    if SB327A_06 = 9 then SB327A_06 = .;
    if SB327A_07 = 9 then SB327A_07 = .;
    if SB327A_08 = 9 then SB327A_08 = .;
    if SB327A_09 = 9 then SB327A_09 = .;
    if SB327A_10 = 9 then SB327A_10 = .;
    if SB327A_11 = 9 then SB327A_11 = .;
    if SB327A_12 = 9 then SB327A_12 = .;
    if SB327A_13 = 9 then SB327A_13 = .;
    if SB327A_14 = 9 then SB327A_14 = .;
    if SB327B_01 = 9 then SB327B_01 = .;
    if SB327B_02 = 9 then SB327B_02 = .;
    if SB327B_03 = 9 then SB327B_03 = .;
    if SB327B_04 = 9 then SB327B_04 = .;
    if SB327B_05 = 9 then SB327B_05 = .;
    if SB327B_06 = 9 then SB327B_06 = .;
    if SB327B_07 = 9 then SB327B_07 = .;
    if SB327B_08 = 9 then SB327B_08 = .;
    if SB327B_09 = 9 then SB327B_09 = .;
    if SB327B_10 = 9 then SB327B_10 = .;
    if SB327B_11 = 9 then SB327B_11 = .;
    if SB327B_12 = 9 then SB327B_12 = .;
    if SB327B_13 = 9 then SB327B_13 = .;
    if SB327B_14 = 9 then SB327B_14 = .;
    if SB327C_01 = 9 then SB327C_01 = .;
    if SB327C_02 = 9 then SB327C_02 = .;
    if SB327C_03 = 9 then SB327C_03 = .;
    if SB327C_04 = 9 then SB327C_04 = .;
    if SB327C_05 = 9 then SB327C_05 = .;
    if SB327C_06 = 9 then SB327C_06 = .;
    if SB327C_07 = 9 then SB327C_07 = .;
    if SB327C_08 = 9 then SB327C_08 = .;
    if SB327C_09 = 9 then SB327C_09 = .;
    if SB327C_10 = 9 then SB327C_10 = .;
    if SB327C_11 = 9 then SB327C_11 = .;
    if SB327C_12 = 9 then SB327C_12 = .;
    if SB327C_13 = 9 then SB327C_13 = .;
    if SB327C_14 = 9 then SB327C_14 = .;
    if SB327D_01 = 9 then SB327D_01 = .;
    if SB327D_02 = 9 then SB327D_02 = .;
    if SB327D_03 = 9 then SB327D_03 = .;
    if SB327D_04 = 9 then SB327D_04 = .;
    if SB327D_05 = 9 then SB327D_05 = .;
    if SB327D_06 = 9 then SB327D_06 = .;
    if SB327D_07 = 9 then SB327D_07 = .;
    if SB327D_08 = 9 then SB327D_08 = .;
    if SB327D_09 = 9 then SB327D_09 = .;
    if SB327D_10 = 9 then SB327D_10 = .;
    if SB327D_11 = 9 then SB327D_11 = .;
    if SB327D_12 = 9 then SB327D_12 = .;
    if SB327D_13 = 9 then SB327D_13 = .;
    if SB327D_14 = 9 then SB327D_14 = .;
    if SB327E_01 = 9 then SB327E_01 = .;
    if SB327E_02 = 9 then SB327E_02 = .;
    if SB327E_03 = 9 then SB327E_03 = .;
    if SB327E_04 = 9 then SB327E_04 = .;
    if SB327E_05 = 9 then SB327E_05 = .;
    if SB327E_06 = 9 then SB327E_06 = .;
    if SB327E_07 = 9 then SB327E_07 = .;
    if SB327E_08 = 9 then SB327E_08 = .;
    if SB327E_09 = 9 then SB327E_09 = .;
    if SB327E_10 = 9 then SB327E_10 = .;
    if SB327E_11 = 9 then SB327E_11 = .;
    if SB327E_12 = 9 then SB327E_12 = .;
    if SB327E_13 = 9 then SB327E_13 = .;
    if SB327E_14 = 9 then SB327E_14 = .;
    if SB328_01 = 9 then SB328_01 = .;
    if SB328_02 = 9 then SB328_02 = .;
    if SB328_03 = 9 then SB328_03 = .;
    if SB328_04 = 9 then SB328_04 = .;
    if SB328_05 = 9 then SB328_05 = .;
    if SB328_06 = 9 then SB328_06 = .;
    if SB328_07 = 9 then SB328_07 = .;
    if SB328_08 = 9 then SB328_08 = .;
    if SB328_09 = 9 then SB328_09 = .;
    if SB328_10 = 9 then SB328_10 = .;
    if SB328_11 = 9 then SB328_11 = .;
    if SB328_12 = 9 then SB328_12 = .;
    if SB328_13 = 9 then SB328_13 = .;
    if SB328_14 = 9 then SB328_14 = .;
    if SB330_01 = 9999 then SB330_01 = .;
    if SB330_02 = 9999 then SB330_02 = .;
    if SB330_03 = 9999 then SB330_03 = .;
    if SB330_04 = 9999 then SB330_04 = .;
    if SB330_05 = 9999 then SB330_05 = .;
    if SB330_06 = 9999 then SB330_06 = .;
    if SB330_07 = 9999 then SB330_07 = .;
    if SB330_08 = 9999 then SB330_08 = .;
    if SB330_09 = 9999 then SB330_09 = .;
    if SB330_10 = 9999 then SB330_10 = .;
    if SB330_11 = 9999 then SB330_11 = .;
    if SB330_12 = 9999 then SB330_12 = .;
    if SB330_13 = 9999 then SB330_13 = .;
    if SB330_14 = 9999 then SB330_14 = .;
    if SB331_01 = 9 then SB331_01 = .;
    if SB331_02 = 9 then SB331_02 = .;
    if SB331_03 = 9 then SB331_03 = .;
    if SB331_04 = 9 then SB331_04 = .;
    if SB331_05 = 9 then SB331_05 = .;
    if SB331_06 = 9 then SB331_06 = .;
    if SB331_07 = 9 then SB331_07 = .;
    if SB331_08 = 9 then SB331_08 = .;
    if SB331_09 = 9 then SB331_09 = .;
    if SB331_10 = 9 then SB331_10 = .;
    if SB331_11 = 9 then SB331_11 = .;
    if SB331_12 = 9 then SB331_12 = .;
    if SB331_13 = 9 then SB331_13 = .;
    if SB331_14 = 9 then SB331_14 = .;
    if SB332A_01 = 999 then SB332A_01 = .;
    if SB332A_02 = 999 then SB332A_02 = .;
    if SB332A_03 = 999 then SB332A_03 = .;
    if SB332A_04 = 999 then SB332A_04 = .;
    if SB332A_05 = 999 then SB332A_05 = .;
    if SB332A_06 = 999 then SB332A_06 = .;
    if SB332A_07 = 999 then SB332A_07 = .;
    if SB332A_08 = 999 then SB332A_08 = .;
    if SB332A_09 = 999 then SB332A_09 = .;
    if SB332A_10 = 999 then SB332A_10 = .;
    if SB332A_11 = 999 then SB332A_11 = .;
    if SB332A_12 = 999 then SB332A_12 = .;
    if SB332A_13 = 999 then SB332A_13 = .;
    if SB332A_14 = 999 then SB332A_14 = .;
    if SB332B_01 = 999 then SB332B_01 = .;
    if SB332B_02 = 999 then SB332B_02 = .;
    if SB332B_03 = 999 then SB332B_03 = .;
    if SB332B_04 = 999 then SB332B_04 = .;
    if SB332B_05 = 999 then SB332B_05 = .;
    if SB332B_06 = 999 then SB332B_06 = .;
    if SB332B_07 = 999 then SB332B_07 = .;
    if SB332B_08 = 999 then SB332B_08 = .;
    if SB332B_09 = 999 then SB332B_09 = .;
    if SB332B_10 = 999 then SB332B_10 = .;
    if SB332B_11 = 999 then SB332B_11 = .;
    if SB332B_12 = 999 then SB332B_12 = .;
    if SB332B_13 = 999 then SB332B_13 = .;
    if SB332B_14 = 999 then SB332B_14 = .;
    if SB333AA_01 = 999 then SB333AA_01 = .;
    if SB333AA_02 = 999 then SB333AA_02 = .;
    if SB333AA_03 = 999 then SB333AA_03 = .;
    if SB333AA_04 = 999 then SB333AA_04 = .;
    if SB333AA_05 = 999 then SB333AA_05 = .;
    if SB333AA_06 = 999 then SB333AA_06 = .;
    if SB333AA_07 = 999 then SB333AA_07 = .;
    if SB333AA_08 = 999 then SB333AA_08 = .;
    if SB333AA_09 = 999 then SB333AA_09 = .;
    if SB333AA_10 = 999 then SB333AA_10 = .;
    if SB333AA_11 = 999 then SB333AA_11 = .;
    if SB333AA_12 = 999 then SB333AA_12 = .;
    if SB333AA_13 = 999 then SB333AA_13 = .;
    if SB333AA_14 = 999 then SB333AA_14 = .;
    if SB333AB_01 = 999 then SB333AB_01 = .;
    if SB333AB_02 = 999 then SB333AB_02 = .;
    if SB333AB_03 = 999 then SB333AB_03 = .;
    if SB333AB_04 = 999 then SB333AB_04 = .;
    if SB333AB_05 = 999 then SB333AB_05 = .;
    if SB333AB_06 = 999 then SB333AB_06 = .;
    if SB333AB_07 = 999 then SB333AB_07 = .;
    if SB333AB_08 = 999 then SB333AB_08 = .;
    if SB333AB_09 = 999 then SB333AB_09 = .;
    if SB333AB_10 = 999 then SB333AB_10 = .;
    if SB333AB_11 = 999 then SB333AB_11 = .;
    if SB333AB_12 = 999 then SB333AB_12 = .;
    if SB333AB_13 = 999 then SB333AB_13 = .;
    if SB333AB_14 = 999 then SB333AB_14 = .;
    if SB333B_01 = 9 then SB333B_01 = .;
    if SB333B_02 = 9 then SB333B_02 = .;
    if SB333B_03 = 9 then SB333B_03 = .;
    if SB333B_04 = 9 then SB333B_04 = .;
    if SB333B_05 = 9 then SB333B_05 = .;
    if SB333B_06 = 9 then SB333B_06 = .;
    if SB333B_07 = 9 then SB333B_07 = .;
    if SB333B_08 = 9 then SB333B_08 = .;
    if SB333B_09 = 9 then SB333B_09 = .;
    if SB333B_10 = 9 then SB333B_10 = .;
    if SB333B_11 = 9 then SB333B_11 = .;
    if SB333B_12 = 9 then SB333B_12 = .;
    if SB333B_13 = 9 then SB333B_13 = .;
    if SB333B_14 = 9 then SB333B_14 = .;
    if SB334BA_01 = 9 then SB334BA_01 = .;
    if SB334BA_02 = 9 then SB334BA_02 = .;
    if SB334BA_03 = 9 then SB334BA_03 = .;
    if SB334BA_04 = 9 then SB334BA_04 = .;
    if SB334BA_05 = 9 then SB334BA_05 = .;
    if SB334BA_06 = 9 then SB334BA_06 = .;
    if SB334BA_07 = 9 then SB334BA_07 = .;
    if SB334BA_08 = 9 then SB334BA_08 = .;
    if SB334BA_09 = 9 then SB334BA_09 = .;
    if SB334BA_10 = 9 then SB334BA_10 = .;
    if SB334BA_11 = 9 then SB334BA_11 = .;
    if SB334BA_12 = 9 then SB334BA_12 = .;
    if SB334BA_13 = 9 then SB334BA_13 = .;
    if SB334BA_14 = 9 then SB334BA_14 = .;
    if SB334BB_01 = 9 then SB334BB_01 = .;
    if SB334BB_02 = 9 then SB334BB_02 = .;
    if SB334BB_03 = 9 then SB334BB_03 = .;
    if SB334BB_04 = 9 then SB334BB_04 = .;
    if SB334BB_05 = 9 then SB334BB_05 = .;
    if SB334BB_06 = 9 then SB334BB_06 = .;
    if SB334BB_07 = 9 then SB334BB_07 = .;
    if SB334BB_08 = 9 then SB334BB_08 = .;
    if SB334BB_09 = 9 then SB334BB_09 = .;
    if SB334BB_10 = 9 then SB334BB_10 = .;
    if SB334BB_11 = 9 then SB334BB_11 = .;
    if SB334BB_12 = 9 then SB334BB_12 = .;
    if SB334BB_13 = 9 then SB334BB_13 = .;
    if SB334BB_14 = 9 then SB334BB_14 = .;
    if SB334BC_01 = 9 then SB334BC_01 = .;
    if SB334BC_02 = 9 then SB334BC_02 = .;
    if SB334BC_03 = 9 then SB334BC_03 = .;
    if SB334BC_04 = 9 then SB334BC_04 = .;
    if SB334BC_05 = 9 then SB334BC_05 = .;
    if SB334BC_06 = 9 then SB334BC_06 = .;
    if SB334BC_07 = 9 then SB334BC_07 = .;
    if SB334BC_08 = 9 then SB334BC_08 = .;
    if SB334BC_09 = 9 then SB334BC_09 = .;
    if SB334BC_10 = 9 then SB334BC_10 = .;
    if SB334BC_11 = 9 then SB334BC_11 = .;
    if SB334BC_12 = 9 then SB334BC_12 = .;
    if SB334BC_13 = 9 then SB334BC_13 = .;
    if SB334BC_14 = 9 then SB334BC_14 = .;
    if SB1D_01  = 99 then SB1D_01 = .;
    if SB1D_02  = 99 then SB1D_02 = .;
    if SB1D_03  = 99 then SB1D_03 = .;
    if SB1D_04  = 99 then SB1D_04 = .;
    if SB1D_05  = 99 then SB1D_05 = .;
    if SB1D_06  = 99 then SB1D_06 = .;
    if SB1D_07  = 99 then SB1D_07 = .;
    if SB1D_08  = 99 then SB1D_08 = .;
    if SB1D_09  = 99 then SB1D_09 = .;
    if SB1D_10  = 99 then SB1D_10 = .;
    if SB1D_11  = 99 then SB1D_11 = .;
    if SB1D_12  = 99 then SB1D_12 = .;
    if SB1D_13  = 99 then SB1D_13 = .;
    if SB1D_14  = 99 then SB1D_14 = .;
    if SB1MO_01 = 99 then SB1MO_01 = .;
    if SB1MO_02 = 99 then SB1MO_02 = .;
    if SB1MO_03 = 99 then SB1MO_03 = .;
    if SB1MO_04 = 99 then SB1MO_04 = .;
    if SB1MO_05 = 99 then SB1MO_05 = .;
    if SB1MO_06 = 99 then SB1MO_06 = .;
    if SB1MO_07 = 99 then SB1MO_07 = .;
    if SB1MO_08 = 99 then SB1MO_08 = .;
    if SB1MO_09 = 99 then SB1MO_09 = .;
    if SB1MO_10 = 99 then SB1MO_10 = .;
    if SB1MO_11 = 99 then SB1MO_11 = .;
    if SB1MO_12 = 99 then SB1MO_12 = .;
    if SB1MO_13 = 99 then SB1MO_13 = .;
    if SB1MO_14 = 99 then SB1MO_14 = .;
    if SB1Y_01  = 9999 then SB1Y_01 = .;
    if SB1Y_02  = 9999 then SB1Y_02 = .;
    if SB1Y_03  = 9999 then SB1Y_03 = .;
    if SB1Y_04  = 9999 then SB1Y_04 = .;
    if SB1Y_05  = 9999 then SB1Y_05 = .;
    if SB1Y_06  = 9999 then SB1Y_06 = .;
    if SB1Y_07  = 9999 then SB1Y_07 = .;
    if SB1Y_08  = 9999 then SB1Y_08 = .;
    if SB1Y_09  = 9999 then SB1Y_09 = .;
    if SB1Y_10  = 9999 then SB1Y_10 = .;
    if SB1Y_11  = 9999 then SB1Y_11 = .;
    if SB1Y_12  = 9999 then SB1Y_12 = .;
    if SB1Y_13  = 9999 then SB1Y_13 = .;
    if SB1Y_14  = 9999 then SB1Y_14 = .;
    if SB1H_01  = 99 then SB1H_01 = .;
    if SB1H_02  = 99 then SB1H_02 = .;
    if SB1H_03  = 99 then SB1H_03 = .;
    if SB1H_04  = 99 then SB1H_04 = .;
    if SB1H_05  = 99 then SB1H_05 = .;
    if SB1H_06  = 99 then SB1H_06 = .;
    if SB1H_07  = 99 then SB1H_07 = .;
    if SB1H_08  = 99 then SB1H_08 = .;
    if SB1H_09  = 99 then SB1H_09 = .;
    if SB1H_10  = 99 then SB1H_10 = .;
    if SB1H_11  = 99 then SB1H_11 = .;
    if SB1H_12  = 99 then SB1H_12 = .;
    if SB1H_13  = 99 then SB1H_13 = .;
    if SB1H_14  = 99 then SB1H_14 = .;
    if SB1M_01  = 99 then SB1M_01 = .;
    if SB1M_02  = 99 then SB1M_02 = .;
    if SB1M_03  = 99 then SB1M_03 = .;
    if SB1M_04  = 99 then SB1M_04 = .;
    if SB1M_05  = 99 then SB1M_05 = .;
    if SB1M_06  = 99 then SB1M_06 = .;
    if SB1M_07  = 99 then SB1M_07 = .;
    if SB1M_08  = 99 then SB1M_08 = .;
    if SB1M_09  = 99 then SB1M_09 = .;
    if SB1M_10  = 99 then SB1M_10 = .;
    if SB1M_11  = 99 then SB1M_11 = .;
    if SB1M_12  = 99 then SB1M_12 = .;
    if SB1M_13  = 99 then SB1M_13 = .;
    if SB1M_14  = 99 then SB1M_14 = .;
    if SB2D_01  = 99 then SB2D_01 = .;
    if SB2D_02  = 99 then SB2D_02 = .;
    if SB2D_03  = 99 then SB2D_03 = .;
    if SB2D_04  = 99 then SB2D_04 = .;
    if SB2D_05  = 99 then SB2D_05 = .;
    if SB2D_06  = 99 then SB2D_06 = .;
    if SB2D_07  = 99 then SB2D_07 = .;
    if SB2D_08  = 99 then SB2D_08 = .;
    if SB2D_09  = 99 then SB2D_09 = .;
    if SB2D_10  = 99 then SB2D_10 = .;
    if SB2D_11  = 99 then SB2D_11 = .;
    if SB2D_12  = 99 then SB2D_12 = .;
    if SB2D_13  = 99 then SB2D_13 = .;
    if SB2D_14  = 99 then SB2D_14 = .;
    if SB2MO_01 = 99 then SB2MO_01 = .;
    if SB2MO_02 = 99 then SB2MO_02 = .;
    if SB2MO_03 = 99 then SB2MO_03 = .;
    if SB2MO_04 = 99 then SB2MO_04 = .;
    if SB2MO_05 = 99 then SB2MO_05 = .;
    if SB2MO_06 = 99 then SB2MO_06 = .;
    if SB2MO_07 = 99 then SB2MO_07 = .;
    if SB2MO_08 = 99 then SB2MO_08 = .;
    if SB2MO_09 = 99 then SB2MO_09 = .;
    if SB2MO_10 = 99 then SB2MO_10 = .;
    if SB2MO_11 = 99 then SB2MO_11 = .;
    if SB2MO_12 = 99 then SB2MO_12 = .;
    if SB2MO_13 = 99 then SB2MO_13 = .;
    if SB2MO_14 = 99 then SB2MO_14 = .;
    if SB2Y_01  = 9999 then SB2Y_01 = .;
    if SB2Y_02  = 9999 then SB2Y_02 = .;
    if SB2Y_03  = 9999 then SB2Y_03 = .;
    if SB2Y_04  = 9999 then SB2Y_04 = .;
    if SB2Y_05  = 9999 then SB2Y_05 = .;
    if SB2Y_06  = 9999 then SB2Y_06 = .;
    if SB2Y_07  = 9999 then SB2Y_07 = .;
    if SB2Y_08  = 9999 then SB2Y_08 = .;
    if SB2Y_09  = 9999 then SB2Y_09 = .;
    if SB2Y_10  = 9999 then SB2Y_10 = .;
    if SB2Y_11  = 9999 then SB2Y_11 = .;
    if SB2Y_12  = 9999 then SB2Y_12 = .;
    if SB2Y_13  = 9999 then SB2Y_13 = .;
    if SB2Y_14  = 9999 then SB2Y_14 = .;
    if SB2H_01  = 99 then SB2H_01 = .;
    if SB2H_02  = 99 then SB2H_02 = .;
    if SB2H_03  = 99 then SB2H_03 = .;
    if SB2H_04  = 99 then SB2H_04 = .;
    if SB2H_05  = 99 then SB2H_05 = .;
    if SB2H_06  = 99 then SB2H_06 = .;
    if SB2H_07  = 99 then SB2H_07 = .;
    if SB2H_08  = 99 then SB2H_08 = .;
    if SB2H_09  = 99 then SB2H_09 = .;
    if SB2H_10  = 99 then SB2H_10 = .;
    if SB2H_11  = 99 then SB2H_11 = .;
    if SB2H_12  = 99 then SB2H_12 = .;
    if SB2H_13  = 99 then SB2H_13 = .;
    if SB2H_14  = 99 then SB2H_14 = .;
    if SB2M_01  = 99 then SB2M_01 = .;
    if SB2M_02  = 99 then SB2M_02 = .;
    if SB2M_03  = 99 then SB2M_03 = .;
    if SB2M_04  = 99 then SB2M_04 = .;
    if SB2M_05  = 99 then SB2M_05 = .;
    if SB2M_06  = 99 then SB2M_06 = .;
    if SB2M_07  = 99 then SB2M_07 = .;
    if SB2M_08  = 99 then SB2M_08 = .;
    if SB2M_09  = 99 then SB2M_09 = .;
    if SB2M_10  = 99 then SB2M_10 = .;
    if SB2M_11  = 99 then SB2M_11 = .;
    if SB2M_12  = 99 then SB2M_12 = .;
    if SB2M_13  = 99 then SB2M_13 = .;
    if SB2M_14  = 99 then SB2M_14 = .;
    if SB3D_01  = 99 then SB3D_01 = .;
    if SB3D_02  = 99 then SB3D_02 = .;
    if SB3D_03  = 99 then SB3D_03 = .;
    if SB3D_04  = 99 then SB3D_04 = .;
    if SB3D_05  = 99 then SB3D_05 = .;
    if SB3D_06  = 99 then SB3D_06 = .;
    if SB3D_07  = 99 then SB3D_07 = .;
    if SB3D_08  = 99 then SB3D_08 = .;
    if SB3D_09  = 99 then SB3D_09 = .;
    if SB3D_10  = 99 then SB3D_10 = .;
    if SB3D_11  = 99 then SB3D_11 = .;
    if SB3D_12  = 99 then SB3D_12 = .;
    if SB3D_13  = 99 then SB3D_13 = .;
    if SB3D_14  = 99 then SB3D_14 = .;
    if SB3MO_01 = 99 then SB3MO_01 = .;
    if SB3MO_02 = 99 then SB3MO_02 = .;
    if SB3MO_03 = 99 then SB3MO_03 = .;
    if SB3MO_04 = 99 then SB3MO_04 = .;
    if SB3MO_05 = 99 then SB3MO_05 = .;
    if SB3MO_06 = 99 then SB3MO_06 = .;
    if SB3MO_07 = 99 then SB3MO_07 = .;
    if SB3MO_08 = 99 then SB3MO_08 = .;
    if SB3MO_09 = 99 then SB3MO_09 = .;
    if SB3MO_10 = 99 then SB3MO_10 = .;
    if SB3MO_11 = 99 then SB3MO_11 = .;
    if SB3MO_12 = 99 then SB3MO_12 = .;
    if SB3MO_13 = 99 then SB3MO_13 = .;
    if SB3MO_14 = 99 then SB3MO_14 = .;
    if SB3Y_01  = 9999 then SB3Y_01 = .;
    if SB3Y_02  = 9999 then SB3Y_02 = .;
    if SB3Y_03  = 9999 then SB3Y_03 = .;
    if SB3Y_04  = 9999 then SB3Y_04 = .;
    if SB3Y_05  = 9999 then SB3Y_05 = .;
    if SB3Y_06  = 9999 then SB3Y_06 = .;
    if SB3Y_07  = 9999 then SB3Y_07 = .;
    if SB3Y_08  = 9999 then SB3Y_08 = .;
    if SB3Y_09  = 9999 then SB3Y_09 = .;
    if SB3Y_10  = 9999 then SB3Y_10 = .;
    if SB3Y_11  = 9999 then SB3Y_11 = .;
    if SB3Y_12  = 9999 then SB3Y_12 = .;
    if SB3Y_13  = 9999 then SB3Y_13 = .;
    if SB3Y_14  = 9999 then SB3Y_14 = .;
    if SB3H_01  = 99 then SB3H_01 = .;
    if SB3H_02  = 99 then SB3H_02 = .;
    if SB3H_03  = 99 then SB3H_03 = .;
    if SB3H_04  = 99 then SB3H_04 = .;
    if SB3H_05  = 99 then SB3H_05 = .;
    if SB3H_06  = 99 then SB3H_06 = .;
    if SB3H_07  = 99 then SB3H_07 = .;
    if SB3H_08  = 99 then SB3H_08 = .;
    if SB3H_09  = 99 then SB3H_09 = .;
    if SB3H_10  = 99 then SB3H_10 = .;
    if SB3H_11  = 99 then SB3H_11 = .;
    if SB3H_12  = 99 then SB3H_12 = .;
    if SB3H_13  = 99 then SB3H_13 = .;
    if SB3H_14  = 99 then SB3H_14 = .;
    if SB3M_01  = 99 then SB3M_01 = .;
    if SB3M_02  = 99 then SB3M_02 = .;
    if SB3M_03  = 99 then SB3M_03 = .;
    if SB3M_04  = 99 then SB3M_04 = .;
    if SB3M_05  = 99 then SB3M_05 = .;
    if SB3M_06  = 99 then SB3M_06 = .;
    if SB3M_07  = 99 then SB3M_07 = .;
    if SB3M_08  = 99 then SB3M_08 = .;
    if SB3M_09  = 99 then SB3M_09 = .;
    if SB3M_10  = 99 then SB3M_10 = .;
    if SB3M_11  = 99 then SB3M_11 = .;
    if SB3M_12  = 99 then SB3M_12 = .;
    if SB3M_13  = 99 then SB3M_13 = .;
    if SB3M_14  = 99 then SB3M_14 = .;
    if SB334EA_01 = 9 then SB334EA_01 = .;
    if SB334EA_02 = 9 then SB334EA_02 = .;
    if SB334EA_03 = 9 then SB334EA_03 = .;
    if SB334EA_04 = 9 then SB334EA_04 = .;
    if SB334EA_05 = 9 then SB334EA_05 = .;
    if SB334EA_06 = 9 then SB334EA_06 = .;
    if SB334EA_07 = 9 then SB334EA_07 = .;
    if SB334EA_08 = 9 then SB334EA_08 = .;
    if SB334EA_09 = 9 then SB334EA_09 = .;
    if SB334EA_10 = 9 then SB334EA_10 = .;
    if SB334EA_11 = 9 then SB334EA_11 = .;
    if SB334EA_12 = 9 then SB334EA_12 = .;
    if SB334EA_13 = 9 then SB334EA_13 = .;
    if SB334EA_14 = 9 then SB334EA_14 = .;
    if SB334EB_01 = 9 then SB334EB_01 = .;
    if SB334EB_02 = 9 then SB334EB_02 = .;
    if SB334EB_03 = 9 then SB334EB_03 = .;
    if SB334EB_04 = 9 then SB334EB_04 = .;
    if SB334EB_05 = 9 then SB334EB_05 = .;
    if SB334EB_06 = 9 then SB334EB_06 = .;
    if SB334EB_07 = 9 then SB334EB_07 = .;
    if SB334EB_08 = 9 then SB334EB_08 = .;
    if SB334EB_09 = 9 then SB334EB_09 = .;
    if SB334EB_10 = 9 then SB334EB_10 = .;
    if SB334EB_11 = 9 then SB334EB_11 = .;
    if SB334EB_12 = 9 then SB334EB_12 = .;
    if SB334EB_13 = 9 then SB334EB_13 = .;
    if SB334EB_14 = 9 then SB334EB_14 = .;
    if SB334EC_01 = 9 then SB334EC_01 = .;
    if SB334EC_02 = 9 then SB334EC_02 = .;
    if SB334EC_03 = 9 then SB334EC_03 = .;
    if SB334EC_04 = 9 then SB334EC_04 = .;
    if SB334EC_05 = 9 then SB334EC_05 = .;
    if SB334EC_06 = 9 then SB334EC_06 = .;
    if SB334EC_07 = 9 then SB334EC_07 = .;
    if SB334EC_08 = 9 then SB334EC_08 = .;
    if SB334EC_09 = 9 then SB334EC_09 = .;
    if SB334EC_10 = 9 then SB334EC_10 = .;
    if SB334EC_11 = 9 then SB334EC_11 = .;
    if SB334EC_12 = 9 then SB334EC_12 = .;
    if SB334EC_13 = 9 then SB334EC_13 = .;
    if SB334EC_14 = 9 then SB334EC_14 = .;
    if SB334FA_01 = 9999 then SB334FA_01 = .;
    if SB334FA_02 = 9999 then SB334FA_02 = .;
    if SB334FA_03 = 9999 then SB334FA_03 = .;
    if SB334FA_04 = 9999 then SB334FA_04 = .;
    if SB334FA_05 = 9999 then SB334FA_05 = .;
    if SB334FA_06 = 9999 then SB334FA_06 = .;
    if SB334FA_07 = 9999 then SB334FA_07 = .;
    if SB334FA_08 = 9999 then SB334FA_08 = .;
    if SB334FA_09 = 9999 then SB334FA_09 = .;
    if SB334FA_10 = 9999 then SB334FA_10 = .;
    if SB334FA_11 = 9999 then SB334FA_11 = .;
    if SB334FA_12 = 9999 then SB334FA_12 = .;
    if SB334FA_13 = 9999 then SB334FA_13 = .;
    if SB334FA_14 = 9999 then SB334FA_14 = .;
    if SB334FB_01 = 9999 then SB334FB_01 = .;
    if SB334FB_02 = 9999 then SB334FB_02 = .;
    if SB334FB_03 = 9999 then SB334FB_03 = .;
    if SB334FB_04 = 9999 then SB334FB_04 = .;
    if SB334FB_05 = 9999 then SB334FB_05 = .;
    if SB334FB_06 = 9999 then SB334FB_06 = .;
    if SB334FB_07 = 9999 then SB334FB_07 = .;
    if SB334FB_08 = 9999 then SB334FB_08 = .;
    if SB334FB_09 = 9999 then SB334FB_09 = .;
    if SB334FB_10 = 9999 then SB334FB_10 = .;
    if SB334FB_11 = 9999 then SB334FB_11 = .;
    if SB334FB_12 = 9999 then SB334FB_12 = .;
    if SB334FB_13 = 9999 then SB334FB_13 = .;
    if SB334FB_14 = 9999 then SB334FB_14 = .;
    if SB334FC_01 = 9999 then SB334FC_01 = .;
    if SB334FC_02 = 9999 then SB334FC_02 = .;
    if SB334FC_03 = 9999 then SB334FC_03 = .;
    if SB334FC_04 = 9999 then SB334FC_04 = .;
    if SB334FC_05 = 9999 then SB334FC_05 = .;
    if SB334FC_06 = 9999 then SB334FC_06 = .;
    if SB334FC_07 = 9999 then SB334FC_07 = .;
    if SB334FC_08 = 9999 then SB334FC_08 = .;
    if SB334FC_09 = 9999 then SB334FC_09 = .;
    if SB334FC_10 = 9999 then SB334FC_10 = .;
    if SB334FC_11 = 9999 then SB334FC_11 = .;
    if SB334FC_12 = 9999 then SB334FC_12 = .;
    if SB334FC_13 = 9999 then SB334FC_13 = .;
    if SB334FC_14 = 9999 then SB334FC_14 = .;
    if SB334GA_01 = 9999 then SB334GA_01 = .;
    if SB334GA_02 = 9999 then SB334GA_02 = .;
    if SB334GA_03 = 9999 then SB334GA_03 = .;
    if SB334GA_04 = 9999 then SB334GA_04 = .;
    if SB334GA_05 = 9999 then SB334GA_05 = .;
    if SB334GA_06 = 9999 then SB334GA_06 = .;
    if SB334GA_07 = 9999 then SB334GA_07 = .;
    if SB334GA_08 = 9999 then SB334GA_08 = .;
    if SB334GA_09 = 9999 then SB334GA_09 = .;
    if SB334GA_10 = 9999 then SB334GA_10 = .;
    if SB334GA_11 = 9999 then SB334GA_11 = .;
    if SB334GA_12 = 9999 then SB334GA_12 = .;
    if SB334GA_13 = 9999 then SB334GA_13 = .;
    if SB334GA_14 = 9999 then SB334GA_14 = .;
    if SB334GB_01 = 9999 then SB334GB_01 = .;
    if SB334GB_02 = 9999 then SB334GB_02 = .;
    if SB334GB_03 = 9999 then SB334GB_03 = .;
    if SB334GB_04 = 9999 then SB334GB_04 = .;
    if SB334GB_05 = 9999 then SB334GB_05 = .;
    if SB334GB_06 = 9999 then SB334GB_06 = .;
    if SB334GB_07 = 9999 then SB334GB_07 = .;
    if SB334GB_08 = 9999 then SB334GB_08 = .;
    if SB334GB_09 = 9999 then SB334GB_09 = .;
    if SB334GB_10 = 9999 then SB334GB_10 = .;
    if SB334GB_11 = 9999 then SB334GB_11 = .;
    if SB334GB_12 = 9999 then SB334GB_12 = .;
    if SB334GB_13 = 9999 then SB334GB_13 = .;
    if SB334GB_14 = 9999 then SB334GB_14 = .;
    if SB334GC_01 = 9999 then SB334GC_01 = .;
    if SB334GC_02 = 9999 then SB334GC_02 = .;
    if SB334GC_03 = 9999 then SB334GC_03 = .;
    if SB334GC_04 = 9999 then SB334GC_04 = .;
    if SB334GC_05 = 9999 then SB334GC_05 = .;
    if SB334GC_06 = 9999 then SB334GC_06 = .;
    if SB334GC_07 = 9999 then SB334GC_07 = .;
    if SB334GC_08 = 9999 then SB334GC_08 = .;
    if SB334GC_09 = 9999 then SB334GC_09 = .;
    if SB334GC_10 = 9999 then SB334GC_10 = .;
    if SB334GC_11 = 9999 then SB334GC_11 = .;
    if SB334GC_12 = 9999 then SB334GC_12 = .;
    if SB334GC_13 = 9999 then SB334GC_13 = .;
    if SB334GC_14 = 9999 then SB334GC_14 = .;
    if SB334H_01 = 9 then SB334H_01 = .;
    if SB334H_02 = 9 then SB334H_02 = .;
    if SB334H_03 = 9 then SB334H_03 = .;
    if SB334H_04 = 9 then SB334H_04 = .;
    if SB334H_05 = 9 then SB334H_05 = .;
    if SB334H_06 = 9 then SB334H_06 = .;
    if SB334H_07 = 9 then SB334H_07 = .;
    if SB334H_08 = 9 then SB334H_08 = .;
    if SB334H_09 = 9 then SB334H_09 = .;
    if SB334H_10 = 9 then SB334H_10 = .;
    if SB334H_11 = 9 then SB334H_11 = .;
    if SB334H_12 = 9 then SB334H_12 = .;
    if SB334H_13 = 9 then SB334H_13 = .;
    if SB334H_14 = 9 then SB334H_14 = .;
    if SB335AD_01 = 99 then SB335AD_01 = .;
    if SB335AD_02 = 99 then SB335AD_02 = .;
    if SB335AD_03 = 99 then SB335AD_03 = .;
    if SB335AD_04 = 99 then SB335AD_04 = .;
    if SB335AD_05 = 99 then SB335AD_05 = .;
    if SB335AD_06 = 99 then SB335AD_06 = .;
    if SB335AD_07 = 99 then SB335AD_07 = .;
    if SB335AD_08 = 99 then SB335AD_08 = .;
    if SB335AD_09 = 99 then SB335AD_09 = .;
    if SB335AD_10 = 99 then SB335AD_10 = .;
    if SB335AD_11 = 99 then SB335AD_11 = .;
    if SB335AD_12 = 99 then SB335AD_12 = .;
    if SB335AD_13 = 99 then SB335AD_13 = .;
    if SB335AD_14 = 99 then SB335AD_14 = .;
    if SB335AM_01 = 99 then SB335AM_01 = .;
    if SB335AM_02 = 99 then SB335AM_02 = .;
    if SB335AM_03 = 99 then SB335AM_03 = .;
    if SB335AM_04 = 99 then SB335AM_04 = .;
    if SB335AM_05 = 99 then SB335AM_05 = .;
    if SB335AM_06 = 99 then SB335AM_06 = .;
    if SB335AM_07 = 99 then SB335AM_07 = .;
    if SB335AM_08 = 99 then SB335AM_08 = .;
    if SB335AM_09 = 99 then SB335AM_09 = .;
    if SB335AM_10 = 99 then SB335AM_10 = .;
    if SB335AM_11 = 99 then SB335AM_11 = .;
    if SB335AM_12 = 99 then SB335AM_12 = .;
    if SB335AM_13 = 99 then SB335AM_13 = .;
    if SB335AM_14 = 99 then SB335AM_14 = .;
    if SB335AY_01 = 9999 then SB335AY_01 = .;
    if SB335AY_02 = 9999 then SB335AY_02 = .;
    if SB335AY_03 = 9999 then SB335AY_03 = .;
    if SB335AY_04 = 9999 then SB335AY_04 = .;
    if SB335AY_05 = 9999 then SB335AY_05 = .;
    if SB335AY_06 = 9999 then SB335AY_06 = .;
    if SB335AY_07 = 9999 then SB335AY_07 = .;
    if SB335AY_08 = 9999 then SB335AY_08 = .;
    if SB335AY_09 = 9999 then SB335AY_09 = .;
    if SB335AY_10 = 9999 then SB335AY_10 = .;
    if SB335AY_11 = 9999 then SB335AY_11 = .;
    if SB335AY_12 = 9999 then SB335AY_12 = .;
    if SB335AY_13 = 9999 then SB335AY_13 = .;
    if SB335AY_14 = 9999 then SB335AY_14 = .;
    if SB335AT_01 = 9999 then SB335AT_01 = .;
    if SB335AT_02 = 9999 then SB335AT_02 = .;
    if SB335AT_03 = 9999 then SB335AT_03 = .;
    if SB335AT_04 = 9999 then SB335AT_04 = .;
    if SB335AT_05 = 9999 then SB335AT_05 = .;
    if SB335AT_06 = 9999 then SB335AT_06 = .;
    if SB335AT_07 = 9999 then SB335AT_07 = .;
    if SB335AT_08 = 9999 then SB335AT_08 = .;
    if SB335AT_09 = 9999 then SB335AT_09 = .;
    if SB335AT_10 = 9999 then SB335AT_10 = .;
    if SB335AT_11 = 9999 then SB335AT_11 = .;
    if SB335AT_12 = 9999 then SB335AT_12 = .;
    if SB335AT_13 = 9999 then SB335AT_13 = .;
    if SB335AT_14 = 9999 then SB335AT_14 = .;
    if SB335B_01 = 999 then SB335B_01 = .;
    if SB335B_02 = 999 then SB335B_02 = .;
    if SB335B_03 = 999 then SB335B_03 = .;
    if SB335B_04 = 999 then SB335B_04 = .;
    if SB335B_05 = 999 then SB335B_05 = .;
    if SB335B_06 = 999 then SB335B_06 = .;
    if SB335B_07 = 999 then SB335B_07 = .;
    if SB335B_08 = 999 then SB335B_08 = .;
    if SB335B_09 = 999 then SB335B_09 = .;
    if SB335B_10 = 999 then SB335B_10 = .;
    if SB335B_11 = 999 then SB335B_11 = .;
    if SB335B_12 = 999 then SB335B_12 = .;
    if SB335B_13 = 999 then SB335B_13 = .;
    if SB335B_14 = 999 then SB335B_14 = .;
    if SBPC1_01 = 9999 then SBPC1_01 = .;
    if SBPC1_02 = 9999 then SBPC1_02 = .;
    if SBPC1_03 = 9999 then SBPC1_03 = .;
    if SBPC1_04 = 9999 then SBPC1_04 = .;
    if SBPC1_05 = 9999 then SBPC1_05 = .;
    if SBPC1_06 = 9999 then SBPC1_06 = .;
    if SBPC1_07 = 9999 then SBPC1_07 = .;
    if SBPC1_08 = 9999 then SBPC1_08 = .;
    if SBPC1_09 = 9999 then SBPC1_09 = .;
    if SBPC1_10 = 9999 then SBPC1_10 = .;
    if SBPC1_11 = 9999 then SBPC1_11 = .;
    if SBPC1_12 = 9999 then SBPC1_12 = .;
    if SBPC1_13 = 9999 then SBPC1_13 = .;
    if SBPC1_14 = 9999 then SBPC1_14 = .;
    if SBSD1_01 = 9999 then SBSD1_01 = .;
    if SBSD1_02 = 9999 then SBSD1_02 = .;
    if SBSD1_03 = 9999 then SBSD1_03 = .;
    if SBSD1_04 = 9999 then SBSD1_04 = .;
    if SBSD1_05 = 9999 then SBSD1_05 = .;
    if SBSD1_06 = 9999 then SBSD1_06 = .;
    if SBSD1_07 = 9999 then SBSD1_07 = .;
    if SBSD1_08 = 9999 then SBSD1_08 = .;
    if SBSD1_09 = 9999 then SBSD1_09 = .;
    if SBSD1_10 = 9999 then SBSD1_10 = .;
    if SBSD1_11 = 9999 then SBSD1_11 = .;
    if SBSD1_12 = 9999 then SBSD1_12 = .;
    if SBSD1_13 = 9999 then SBSD1_13 = .;
    if SBSD1_14 = 9999 then SBSD1_14 = .;
    if SBRM1_01 = 99999 then SBRM1_01 = .;
    if SBRM1_02 = 99999 then SBRM1_02 = .;
    if SBRM1_03 = 99999 then SBRM1_03 = .;
    if SBRM1_04 = 99999 then SBRM1_04 = .;
    if SBRM1_05 = 99999 then SBRM1_05 = .;
    if SBRM1_06 = 99999 then SBRM1_06 = .;
    if SBRM1_07 = 99999 then SBRM1_07 = .;
    if SBRM1_08 = 99999 then SBRM1_08 = .;
    if SBRM1_09 = 99999 then SBRM1_09 = .;
    if SBRM1_10 = 99999 then SBRM1_10 = .;
    if SBRM1_11 = 99999 then SBRM1_11 = .;
    if SBRM1_12 = 99999 then SBRM1_12 = .;
    if SBRM1_13 = 99999 then SBRM1_13 = .;
    if SBRM1_14 = 99999 then SBRM1_14 = .;
    if SBRM3_01 = 99999 then SBRM3_01 = .;
    if SBRM3_02 = 99999 then SBRM3_02 = .;
    if SBRM3_03 = 99999 then SBRM3_03 = .;
    if SBRM3_04 = 99999 then SBRM3_04 = .;
    if SBRM3_05 = 99999 then SBRM3_05 = .;
    if SBRM3_06 = 99999 then SBRM3_06 = .;
    if SBRM3_07 = 99999 then SBRM3_07 = .;
    if SBRM3_08 = 99999 then SBRM3_08 = .;
    if SBRM3_09 = 99999 then SBRM3_09 = .;
    if SBRM3_10 = 99999 then SBRM3_10 = .;
    if SBRM3_11 = 99999 then SBRM3_11 = .;
    if SBRM3_12 = 99999 then SBRM3_12 = .;
    if SBRM3_13 = 99999 then SBRM3_13 = .;
    if SBRM3_14 = 99999 then SBRM3_14 = .;
    if SBRM4_01 = 99999 then SBRM4_01 = .;
    if SBRM4_02 = 99999 then SBRM4_02 = .;
    if SBRM4_03 = 99999 then SBRM4_03 = .;
    if SBRM4_04 = 99999 then SBRM4_04 = .;
    if SBRM4_05 = 99999 then SBRM4_05 = .;
    if SBRM4_06 = 99999 then SBRM4_06 = .;
    if SBRM4_07 = 99999 then SBRM4_07 = .;
    if SBRM4_08 = 99999 then SBRM4_08 = .;
    if SBRM4_09 = 99999 then SBRM4_09 = .;
    if SBRM4_10 = 99999 then SBRM4_10 = .;
    if SBRM4_11 = 99999 then SBRM4_11 = .;
    if SBRM4_12 = 99999 then SBRM4_12 = .;
    if SBRM4_13 = 99999 then SBRM4_13 = .;
    if SBRM4_14 = 99999 then SBRM4_14 = .;
    if SBRM5_01 = 99999 then SBRM5_01 = .;
    if SBRM5_02 = 99999 then SBRM5_02 = .;
    if SBRM5_03 = 99999 then SBRM5_03 = .;
    if SBRM5_04 = 99999 then SBRM5_04 = .;
    if SBRM5_05 = 99999 then SBRM5_05 = .;
    if SBRM5_06 = 99999 then SBRM5_06 = .;
    if SBRM5_07 = 99999 then SBRM5_07 = .;
    if SBRM5_08 = 99999 then SBRM5_08 = .;
    if SBRM5_09 = 99999 then SBRM5_09 = .;
    if SBRM5_10 = 99999 then SBRM5_10 = .;
    if SBRM5_11 = 99999 then SBRM5_11 = .;
    if SBRM5_12 = 99999 then SBRM5_12 = .;
    if SBRM5_13 = 99999 then SBRM5_13 = .;
    if SBRM5_14 = 99999 then SBRM5_14 = .;
    if SBSD3_01 = 9999 then SBSD3_01 = .;
    if SBSD3_02 = 9999 then SBSD3_02 = .;
    if SBSD3_03 = 9999 then SBSD3_03 = .;
    if SBSD3_04 = 9999 then SBSD3_04 = .;
    if SBSD3_05 = 9999 then SBSD3_05 = .;
    if SBSD3_06 = 9999 then SBSD3_06 = .;
    if SBSD3_07 = 9999 then SBSD3_07 = .;
    if SBSD3_08 = 9999 then SBSD3_08 = .;
    if SBSD3_09 = 9999 then SBSD3_09 = .;
    if SBSD3_10 = 9999 then SBSD3_10 = .;
    if SBSD3_11 = 9999 then SBSD3_11 = .;
    if SBSD3_12 = 9999 then SBSD3_12 = .;
    if SBSD3_13 = 9999 then SBSD3_13 = .;
    if SBSD3_14 = 9999 then SBSD3_14 = .;
    if SBWH_01  = 9 then SBWH_01 = .;
    if SBWH_02  = 9 then SBWH_02 = .;
    if SBWH_03  = 9 then SBWH_03 = .;
    if SBWH_04  = 9 then SBWH_04 = .;
    if SBWH_05  = 9 then SBWH_05 = .;
    if SBWH_06  = 9 then SBWH_06 = .;
    if SBWH_07  = 9 then SBWH_07 = .;
    if SBWH_08  = 9 then SBWH_08 = .;
    if SBWH_09  = 9 then SBWH_09 = .;
    if SBWH_10  = 9 then SBWH_10 = .;
    if SBWH_11  = 9 then SBWH_11 = .;
    if SBWH_12  = 9 then SBWH_12 = .;
    if SBWH_13  = 9 then SBWH_13 = .;
    if SBWH_14  = 9 then SBWH_14 = .;
    if SBBM_01  = 9999 then SBBM_01 = .;
    if SBBM_02  = 9999 then SBBM_02 = .;
    if SBBM_03  = 9999 then SBBM_03 = .;
    if SBBM_04  = 9999 then SBBM_04 = .;
    if SBBM_05  = 9999 then SBBM_05 = .;
    if SBBM_06  = 9999 then SBBM_06 = .;
    if SBBM_07  = 9999 then SBBM_07 = .;
    if SBBM_08  = 9999 then SBBM_08 = .;
    if SBBM_09  = 9999 then SBBM_09 = .;
    if SBBM_10  = 9999 then SBBM_10 = .;
    if SBBM_11  = 9999 then SBBM_11 = .;
    if SBBM_12  = 9999 then SBBM_12 = .;
    if SBBM_13  = 9999 then SBBM_13 = .;
    if SBBM_14  = 9999 then SBBM_14 = .;
    if SBRI_01  = 9999 then SBRI_01 = .;
    if SBRI_02  = 9999 then SBRI_02 = .;
    if SBRI_03  = 9999 then SBRI_03 = .;
    if SBRI_04  = 9999 then SBRI_04 = .;
    if SBRI_05  = 9999 then SBRI_05 = .;
    if SBRI_06  = 9999 then SBRI_06 = .;
    if SBRI_07  = 9999 then SBRI_07 = .;
    if SBRI_08  = 9999 then SBRI_08 = .;
    if SBRI_09  = 9999 then SBRI_09 = .;
    if SBRI_10  = 9999 then SBRI_10 = .;
    if SBRI_11  = 9999 then SBRI_11 = .;
    if SBRI_12  = 9999 then SBRI_12 = .;
    if SBRI_13  = 9999 then SBRI_13 = .;
    if SBRI_14  = 9999 then SBRI_14 = .;
    if SBED1_01 = 9 then SBED1_01 = .;
    if SBED1_02 = 9 then SBED1_02 = .;
    if SBED1_03 = 9 then SBED1_03 = .;
    if SBED1_04 = 9 then SBED1_04 = .;
    if SBED1_05 = 9 then SBED1_05 = .;
    if SBED1_06 = 9 then SBED1_06 = .;
    if SBED1_07 = 9 then SBED1_07 = .;
    if SBED1_08 = 9 then SBED1_08 = .;
    if SBED1_09 = 9 then SBED1_09 = .;
    if SBED1_10 = 9 then SBED1_10 = .;
    if SBED1_11 = 9 then SBED1_11 = .;
    if SBED1_12 = 9 then SBED1_12 = .;
    if SBED1_13 = 9 then SBED1_13 = .;
    if SBED1_14 = 9 then SBED1_14 = .;
    if SBED2_01 = 99 then SBED2_01 = .;
    if SBED2_02 = 99 then SBED2_02 = .;
    if SBED2_03 = 99 then SBED2_03 = .;
    if SBED2_04 = 99 then SBED2_04 = .;
    if SBED2_05 = 99 then SBED2_05 = .;
    if SBED2_06 = 99 then SBED2_06 = .;
    if SBED2_07 = 99 then SBED2_07 = .;
    if SBED2_08 = 99 then SBED2_08 = .;
    if SBED2_09 = 99 then SBED2_09 = .;
    if SBED2_10 = 99 then SBED2_10 = .;
    if SBED2_11 = 99 then SBED2_11 = .;
    if SBED2_12 = 99 then SBED2_12 = .;
    if SBED2_13 = 99 then SBED2_13 = .;
    if SBED2_14 = 99 then SBED2_14 = .;
    if SBED3_01 = 9 then SBED3_01 = .;
    if SBED3_02 = 9 then SBED3_02 = .;
    if SBED3_03 = 9 then SBED3_03 = .;
    if SBED3_04 = 9 then SBED3_04 = .;
    if SBED3_05 = 9 then SBED3_05 = .;
    if SBED3_06 = 9 then SBED3_06 = .;
    if SBED3_07 = 9 then SBED3_07 = .;
    if SBED3_08 = 9 then SBED3_08 = .;
    if SBED3_09 = 9 then SBED3_09 = .;
    if SBED3_10 = 9 then SBED3_10 = .;
    if SBED3_11 = 9 then SBED3_11 = .;
    if SBED3_12 = 9 then SBED3_12 = .;
    if SBED3_13 = 9 then SBED3_13 = .;
    if SBED3_14 = 9 then SBED3_14 = .;
    if SB3PRG_01 = 9 then SB3PRG_01 = .;
    if SB3PRG_02 = 9 then SB3PRG_02 = .;
    if SB3PRG_03 = 9 then SB3PRG_03 = .;
    if SB3PRG_04 = 9 then SB3PRG_04 = .;
    if SB3PRG_05 = 9 then SB3PRG_05 = .;
    if SB3PRG_06 = 9 then SB3PRG_06 = .;
    if SB3PRG_07 = 9 then SB3PRG_07 = .;
    if SB3PRG_08 = 9 then SB3PRG_08 = .;
    if SB3PRG_09 = 9 then SB3PRG_09 = .;
    if SB3PRG_10 = 9 then SB3PRG_10 = .;
    if SB3PRG_11 = 9 then SB3PRG_11 = .;
    if SB3PRG_12 = 9 then SB3PRG_12 = .;
    if SB3PRG_13 = 9 then SB3PRG_13 = .;
    if SB3PRG_14 = 9 then SB3PRG_14 = .;
Run;
