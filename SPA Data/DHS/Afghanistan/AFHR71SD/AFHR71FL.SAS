libname user 'C:\COUNTRIES\AF71';
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
     1 = "Kabul urban"
     2 = "Kapisa urban"
     3 = "Parwan urban"
     4 = "Wardak urban"
     5 = "Logar urban"
     6 = "Nangarhar urban"
     7 = "Laghman urban"
     8 = "Panjsher urban"
     9 = "Baghlan urban"
    10 = "Bamyan urban"
    11 = "Ghazni urban"
    12 = "Paktika urban"
    13 = "Paktya urban"
    14 = "Khost urban"
    15 = "Kunarha urban"
    16 = "Nooristan urban"
    17 = "Badakhshan urban"
    18 = "Takhar urban"
    19 = "Kunduz urban"
    20 = "Samangan urban"
    21 = "Balkh urban"
    22 = "Sar-E-Pul urban"
    23 = "Ghor urban"
    24 = "Daykundi urban"
    25 = "Urozgan urban"
    26 = "Zabul urban"
    27 = "Kandahar urban"
    28 = "Jawzjan urban"
    29 = "Faryab urban"
    30 = "Helmand urban"
    31 = "Badghis urban"
    32 = "Herat urban"
    33 = "Farah urban"
    34 = "Nimroz urban"
    35 = "Kabul rural"
    36 = "Kapisa rural"
    37 = "Parwan rural"
    38 = "Wardak rural"
    39 = "Logar rural"
    40 = "Nangarhar rural"
    41 = "Laghman rural"
    42 = "Panjsher rural"
    43 = "Baghlan rural"
    44 = "Bamyan rural"
    45 = "Ghazni rural"
    46 = "Paktika rural"
    47 = "Paktya rural"
    48 = "Khost rural"
    49 = "Kunarha rural"
    50 = "Nooristan rural"
    51 = "Badakhshan rural"
    52 = "Takhar rural"
    53 = "Kunduz rural"
    54 = "Samangan rural"
    55 = "Balkh rural"
    56 = "Sar-E-Pul rural"
    57 = "Ghor rural"
    58 = "Daykundi rural"
    59 = "Urozgan rural"
    60 = "Zabul rural"
    61 = "Kandahar rural"
    62 = "Jawzjan rural"
    63 = "Faryab rural"
    64 = "Helmand rural"
    65 = "Badghis rural"
    66 = "Herat rural"
    67 = "Farah rural"
    68 = "Nimroz rural"
     ;
  value F00005_
     1 = "Kabul urban"
     2 = "Kapisa urban"
     3 = "Parwan urban"
     4 = "Wardak urban"
     5 = "Logar urban"
     6 = "Nangarhar urban"
     7 = "Laghman urban"
     8 = "Panjsher urban"
     9 = "Baghlan urban"
    10 = "Bamyan urban"
    11 = "Ghazni urban"
    12 = "Paktika urban"
    13 = "Paktya urban"
    14 = "Khost urban"
    15 = "Kunarha urban"
    16 = "Nooristan urban"
    17 = "Badakhshan urban"
    18 = "Takhar urban"
    19 = "Kunduz urban"
    20 = "Samangan urban"
    21 = "Balkh urban"
    22 = "Sar-E-Pul urban"
    23 = "Ghor urban"
    24 = "Daykundi urban"
    25 = "Urozgan urban"
    26 = "Zabul urban"
    27 = "Kandahar urban"
    28 = "Jawzjan urban"
    29 = "Faryab urban"
    30 = "Helmand urban"
    31 = "Badghis urban"
    32 = "Herat urban"
    33 = "Farah urban"
    34 = "Nimroz urban"
    35 = "Kabul rural"
    36 = "Kapisa rural"
    37 = "Parwan rural"
    38 = "Wardak rural"
    39 = "Logar rural"
    40 = "Nangarhar rural"
    41 = "Laghman rural"
    42 = "Panjsher rural"
    43 = "Baghlan rural"
    44 = "Bamyan rural"
    45 = "Ghazni rural"
    46 = "Paktika rural"
    47 = "Paktya rural"
    48 = "Khost rural"
    49 = "Kunarha rural"
    50 = "Nooristan rural"
    51 = "Badakhshan rural"
    52 = "Takhar rural"
    53 = "Kunduz rural"
    54 = "Samangan rural"
    55 = "Balkh rural"
    56 = "Sar-E-Pul rural"
    57 = "Ghor rural"
    58 = "Daykundi rural"
    59 = "Urozgan rural"
    60 = "Zabul rural"
    61 = "Kandahar rural"
    62 = "Jawzjan rural"
    63 = "Faryab rural"
    64 = "Helmand rural"
    65 = "Badghis rural"
    66 = "Herat rural"
    67 = "Farah rural"
    68 = "Nimroz rural"
     ;
  value F00006_
     1 = "Kabul"
     2 = "Kapisa"
     3 = "Parwan"
     4 = "Wardak"
     5 = "Logar"
     6 = "Nangarhar"
     7 = "Laghman"
     8 = "Panjsher"
     9 = "Baghlan"
    10 = "Bamyan"
    11 = "Ghazni"
    12 = "Paktika"
    13 = "Paktya"
    14 = "Khost"
    15 = "Kunarha"
    16 = "Nooristan"
    17 = "Badakhshan"
    18 = "Takhar"
    19 = "Kunduz"
    20 = "Samangan"
    21 = "Balkh"
    22 = "Sar-E-Pul"
    23 = "Ghor"
    24 = "Daykundi"
    25 = "Urozgan"
    26 = "Zabul"
    27 = "Kandahar"
    28 = "Jawzjan"
    29 = "Faryab"
    30 = "Helmand"
    31 = "Badghis"
    32 = "Herat"
    33 = "Farah"
    34 = "Nimroz"
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
     1 = "Dari"
     2 = "Pashto"
     6 = "Other"
     ;
  value F00013_
     1 = "Dari"
     2 = "Pashto"
     6 = "Other"
     ;
  value F00014_
     1 = "Dari"
     2 = "Pashto"
     6 = "Other"
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
     0 = "No"
     1 = "Yes"
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
    43 = "Traditional dry vault"
    44 = "Eco sanitation"
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
    13 = "Mud and hay"
    20 = "RUDIMENTARY"
    21 = "Wood planks"
    30 = "FINISHED"
    31 = "Parquet or polished wood"
    32 = "Vinyl or asphalt strips"
    33 = "Ceramic tiles"
    34 = "Cement"
    35 = "Rug or mat"
    36 = "Carpet"
    96 = "Other"
     ;
  value F00030_
    10 = "NATURAL"
    11 = "No walls"
    12 = "Prepared mud"
    13 = "Dirt"
    20 = "RUDIMENTARY"
    21 = "Hay with mud"
    22 = "Stone with mud"
    23 = "Uncovered adobe"
    24 = "Plywood"
    25 = "Cardboard/cloth/tent"
    26 = "Reused wood"
    30 = "FINISHED"
    31 = "Cement"
    32 = "Stone with lime/cement"
    33 = "Bricks"
    34 = "Cement blocks"
    35 = "Covered adobe"
    36 = "Wood planks/shingles"
    96 = "Other"
     ;
  value F00031_
    10 = "NATURAL"
    11 = "No roof"
    12 = "Thatch/bushes/grass"
    13 = "Sod/mud with grass"
    20 = "RUDIMENTARY"
    21 = "Rustic mat"
    22 = "Mud and hay"
    23 = "Wood planks"
    24 = "Cardboard/cloth/tent"
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
     1 = "Observed"
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
  1900 = "190 hectares or more"
  9998 = "Unknown"
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
    11 = "Flush - to piped sewer system"
    12 = "Flush - to septic tank"
    13 = "Flush - to pit latrine"
    14 = "Flush - to somewhere else"
    15 = "Flush - don't know where"
    21 = "Pit latrine - ventilated improved pit (VIP)"
    22 = "Pit latrine - with slab"
    23 = "Pit latrine - without slab/open pit"
    31 = "Composting toilet"
    41 = "Bucket toilet"
    51 = "Single vault"
    52 = "Double vault"
    61 = "Eco sanitation"
    71 = "No facility, bush, field"
    96 = "Other"
     ;
  value F00108_
     0 = "No"
     1 = "Yes"
     ;
  value F00109_
     0 = "No"
     1 = "Yes"
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
  95.0 = "More than 950"
  99.8 = "Unknown"
     ;
  value F00120_
     0 = "No"
     1 = "Yes"
     ;
  value F00121_
     0 = "No"
     1 = "Yes"
     ;
  value F00122_
     0 = "No"
     1 = "Yes"
     ;
  value F00123_
     0 = "No"
     1 = "Yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00127_
     0 = "No"
     1 = "Yes"
     ;
  value F00128_
     0 = "No"
     1 = "Yes"
     ;
  value F00129_
     0 = "No"
     1 = "Yes"
     ;
  value F00130_
     0 = "No"
     1 = "Yes"
     ;
  value F00131_
     0 = "No"
     1 = "Yes"
     ;
  value F00132_
     0 = "No"
     1 = "Yes"
     ;
  value F00133_
     0 = "No"
     1 = "Yes"
     ;
  value F00134_
     0 = "No"
     1 = "Yes"
     ;
  value F00135_
     0 = "No"
     1 = "Yes"
     ;
  value F00136_
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
  value F00137_
     0 = "No"
     1 = "Yes"
     ;
  value F00138_
     0 = "No"
     1 = "Yes"
     ;
  value F00139_
     1 = "Male"
     2 = "Female"
     ;
  value F00140_
    95 = "95+"
    98 = "Don't know"
     ;
  value F00141_
     0 = "No education, preschool"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00142_
    98 = "Don't know"
     ;
  value F00143_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00144_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     8 = "Don't know"
     ;
  value F00145_
     0 = "No"
     1 = "Yes"
     ;
  value F00146_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00147_
     0 = "Mother not in household"
     ;
  value F00148_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00149_
     0 = "Father not in household"
     ;
  value F00150_
     0 = "Never married"
     1 = "Married"
     2 = "Living together"
     3 = "Widowed"
     4 = "Divorced"
     5 = "Not living together"
     ;
  value F00151_
     0 = "Never married"
     1 = "Currently married"
     2 = "Formerly/ever married"
     ;
  value F00152_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value F00153_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value F00154_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value F00155_
     0 = "No"
     1 = "Currently attending"
     2 = "Attended at some time"
     ;
  value F00156_
     0 = "No education, preschool"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00157_
    98 = "Don't know"
     ;
  value F00158_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00159_
     0 = "No"
     1 = "Yes"
     ;
  value F00160_
     0 = "No education, preschool"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00161_
    98 = "Don't know"
     ;
  value F00162_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00163_
     0 = "Never attended"
     1 = "Entered school"
     2 = "Advanced"
     3 = "Repeating"
     4 = "Dropout"
     5 = "Left school 2+ years ago"
     8 = "Don't know"
     ;
  value F00164_
     0 = "Neither certificate or registered"
     1 = "Has certificate"
     2 = "Registered"
     8 = "Don't know"
     ;
  value F00165_
     1 = "Too expensive"
     2 = "School too far"
     3 = "Insecure"
     4 = "Need to help at home"
     5 = "Parents did not send"
     6 = "Got married"
     7 = "School lacked basic facilities"
     8 = "Need to work, earn"
     9 = "Other"
     ;
  value F00166_
     1 = "Too expensive"
     2 = "School too far"
     3 = "Insecure"
     4 = "Need to help at home"
     5 = "Parents did not send"
     6 = "Got married"
     7 = "School lacked basic facilities"
     8 = "Need to work, earn"
     9 = "Other"
     ;
  value F00167_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00168_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00169_
  9998 = "Flagged cases"
     ;
  value F00170_
  9998 = "Flagged cases"
     ;
  value F00171_
 99998 = "Flagged cases"
     ;
  value F00172_
  9998 = "Flagged cases"
     ;
  value F00173_
 99998 = "Flagged cases"
     ;
  value F00174_
 99998 = "Flagged cases"
     ;
  value F00175_
 99998 = "Flagged cases"
     ;
  value F00176_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value F00177_
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     4 = "Year and age - year ignored"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00178_
     0 = "Doesn't smoke"
    80 = "80+"
    94 = "Smokes pipes, cigars, etc"
     ;
  value F00179_
  9998 = "Flagged cases"
     ;
  value F00180_
  9998 = "Flagged cases"
     ;
  value F00181_
     1 = "Under age 18"
     2 = "Age 18 or older"
     ;
  value F00182_
     0 = "Not in household"
     ;
  value F00183_
     1 = "Granted"
     2 = "Parent/other responsible refused"
     3 = "Respondent refused"
     4 = "Adult/parent not present"
     5 = "Minor not present"
     ;
  value F00184_
   994 = "Not present"
   995 = "Refused"
   996 = "Other"
     ;
  value F00185_
     0 = "No/don't know"
     1 = "Yes"
     ;
  value F00186_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value F00187_
   997 = "Inconsistent"
     ;
  value F00188_
     1 = "Severe"
     2 = "Moderate"
     3 = "Mild"
     4 = "Not anemic"
     ;
  value F00189_
     0 = "No"
     1 = "Yes"
     ;
  value F00190_
     1 = "Never in union"
     2 = "Other"
     ;
  value F00191_
     1 = "Granted"
     2 = "Parent/other responsible refused"
     3 = "Respondent refused"
     4 = "Adult/parent not present"
     5 = "Minor not present"
     6 = "Sample not tested/lost/not enough DBS"
     ;
  value $F00192_
99992 = "Woman's interview incomplete or woman is not de-facto"
99993 = "Sample damaged/insufficient/not found in lab"
99994 = "Not present"
99995 = "Refused"
99996 = "Other"
'?    ' = "Missing"
     ;
  value F00193_
     1 = "Blood taken"
     2 = "Not present"
     3 = "Refused"
     4 = "Sample not tested/lost/damaged/insufficient"
     5 = "Not enough DBS to complete protocol"
     6 = "Other"
     ;
  value F00194_
     1 = "Granted"
     2 = "Parent/other responsible refused"
     3 = "Respondent refused"
     6 = "Sample not tested/lost/not enough DBS"
     ;
  value F00195_
     1 = "Completed"
     2 = "Not at home"
     3 = "Postponed"
     4 = "Refused"
     5 = "Partly completed"
     6 = "Incapacitated"
     7 = "Other"
     ;
  value F00196_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00197_
    98 = "Don't know"
     ;
  value F00198_
     0 = "Labels to be populated according to country"
     8 = "Don't know"
     ;
  value F00199_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00200_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00201_
  9998 = "Flagged cases"
     ;
  value F00202_
  9998 = "Flagged cases"
     ;
  value F00203_
 99998 = "Flagged cases"
     ;
  value F00204_
  9998 = "Flagged cases"
     ;
  value F00205_
  9998 = "Flagged cases"
     ;
  value F00206_
 99998 = "Flagged cases"
     ;
  value F00207_
  9998 = "Flagged cases"
     ;
  value F00208_
  9998 = "Flagged cases"
     ;
  value F00209_
 99998 = "Flagged cases"
     ;
  value F00210_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value F00211_
     0 = "Not measured"
     1 = "Lying"
     2 = "Standing"
     ;
  value F00212_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00213_
     1 = "Male"
     2 = "Female"
     ;
  value F00214_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00215_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00216_
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00217_
     0 = "Not in household"
     ;
  value F00218_
     1 = "Granted"
     2 = "Refused"
     3 = "Not present/other"
     ;
  value F00219_
   994 = "Not present"
   995 = "Refused"
   996 = "Other"
     ;
  value F00220_
     0 = "Measured"
     3 = "Not present/other"
     4 = "Refused"
     6 = "Other"
     ;
  value F00221_
   997 = "Inconsistent"
     ;
  value F00222_
     1 = "Severe"
     2 = "Moderate"
     3 = "Mild"
     4 = "Not anemic"
     ;
  value F00223_
     0 = "No"
     1 = "Yes"
     ;
  value F00224_
   993 = "Mother not de facto"
   994 = "Incomplete woman's interview"
   995 = "Not in household"
     ;
  value F00225_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00226_
    98 = "Don't know"
     ;
  value F00227_
     0 = "Labels to be populated according to country"
     8 = "Don't know"
     ;
  value F00228_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00229_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00230_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00231_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00232_
     0 = "Not seen"
     1 = "Yes, seen"
     ;
  value F00233_
    96 = "More than 3 years ago"
    98 = "Don't know"
     ;
  value F00234_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00235_
     0 = "Not pretreated"
     1 = "Pretreated, permanent treatment"
     2 = "Pretreated, requiring future treatment"
     3 = "Not pretreated, came with kit and treated at home"
     4 = "Pretreated, treatment type not known"
     6 = "Other"
     8 = "Don't know"
     ;
  value F00236_
    10 = "Long-lasting insectiside treated nets (LLIN)"
    11 = "Insecticide treated net (LLIN) - Permanet"
    12 = "Insecticide treated net (LLIN) - Olyset net"
    16 = "Insecticide treated net (LLIN) - Other/don't know brand"
    20 = "Pretreated net"
    21 = "Pretreated net"
    96 = "Other brand"
    98 = "DK brand"
     ;
  value F00237_
     0 = "No"
     1 = "Yes"
     2 = "Permanent/LLIN net"
     8 = "Not sure"
     ;
  value F00238_
    95 = "25+ months ago"
    96 = "More than 3 years ago"
    98 = "Don't know"
     ;
  value F00239_
     0 = "No, don't know"
     1 = "Yes"
     ;
  value F00240_
     5 = "5+"
     ;
  value F00241_
     0 = "No"
     1 = "Yes"
     8 = "Not sure (DK)"
     ;
  value F00242_
     0 = "No one"
     ;
  value F00243_
     0 = "No"
     1 = "Yes, campaign"
     2 = "Yes, antenatal care"
     3 = "Yes, immunization visit"
     ;
  value F00244_
    10 = "Government health facility"
    20 = "Private health facility"
    30 = "Other sources"
    31 = "Pharmacy"
    32 = "Shop/market"
    33 = "CHW"
    34 = "Religious institution"
    96 = "Other"
    98 = "Don't know"
     ;
  value F00245_
     0 = "Did not sleep under a net"
     1 = "Only treated (ITN) nets"
     2 = "Both treated (ITN) and untreated nets"
     3 = "Only untreated nets"
     ;
  value F00246_
     0 = "Did not sleep under net"
     ;
  value F00247_
    97 = "97+"
    98 = "Don't know"
     ;
  value F00248_
     0 = "No data from Individual file"
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     4 = "Year and age - year ignored"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00249_
     0 = "Not pregnant, don't know"
     1 = "Pregnant"
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
     0 = "Not in household"
     ;
  value F00253_
     1 = "Granted"
     2 = "Refused"
     ;
  value F00254_
     0 = "Negative"
     1 = "Positive"
     6 = "Test undetermined"
     7 = "Sample not found in lab database"
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
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     ;
  value $F00263_
99993 = "Sample damaged/insufficient/not found in lab"
99994 = "Not present"
99995 = "Refused"
99996 = "Other"
'?    ' = "Missing"
     ;
  value F00264_
     0 = "Negative"
     1 = "Positive"
     ;
Data user.AFHR71FL;
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
  attrib HV011    label="Number of eligible men in household";
  attrib HV012    label="Number of de jure members";
  attrib HV013    label="Number of de facto members";
  attrib HV014    label="Number of children 5 and under (de jure)";
  attrib HV015    format=F00002_. label="Result of household interview";
  attrib HV016    label="Day of interview";
  attrib HV017    label="Number of visits";
  attrib HV018    label="NA - Interviewer identification";
  attrib HV019    label="Keyer identification";
  attrib HV020    format=F00003_. label="Ever-married sample";
  attrib HV021    label="Primary sampling unit";
  attrib HV022    format=F00004_. label="Sample strata for sampling errors";
  attrib HV023    format=F00005_. label="Stratification used in sample design";
  attrib HV024    format=F00006_. label="Region";
  attrib HV025    format=F00007_. label="Type of place of residence";
  attrib HV026    format=F00008_. label="NA - Place of residence";
  attrib HV027    format=F00009_. label="Household selected for male interview";
  attrib HV028    label="Household weight for male subsample (6 decimals)";
  attrib HV030    label="NA - Field supervisor";
  attrib HV031    label="NA - Field editor";
  attrib HV032    label="Office editor";
  attrib HV035    label="NA - Number of eligible children for height and weight";
  attrib HV040    label="NA - Cluster altitude in meters";
  attrib HV041    label="NA - Total adults measured";
  attrib HV042    format=F00010_. label="NA - Household selected for hemoglobin";
  attrib HV044    format=F00011_. label="Household selected for Domestic Violence module";
  attrib HV045A   format=F00012_. label="NA - Language of questionnaire";
  attrib HV045B   format=F00013_. label="Language of interview";
  attrib HV045C   format=F00014_. label="Native language of respondent";
  attrib HV046    format=F00015_. label="Translator used";
  attrib HV801    label="Time interview started (hhmm - 24 hour clock)";
  attrib HV802    label="Time interview ended (hhmm - 24 hour clock)";
  attrib HV803    format=F00016_. label="Length of interview in minutes";
  attrib HV804    label="NA - Number of visits for biomarker";
  attrib HV807D   label="NA - Day of biomarker visit";
  attrib HV807M   label="NA - Month of biomarker visit";
  attrib HV807Y   label="NA - Year of biomarker visit";
  attrib HV807C   label="NA - Date of biomarker (CMC)";
  attrib HV807A   label="NA - Date of biomarker Century Day Code (CDC)";
  attrib HV201    format=F00017_. label="Source of drinking water";
  attrib HV202    format=F00018_. label="NA - Source of non-drinking water";
  attrib HV201A   format=F00019_. label="NA - Water not available for at least a day last two weeks";
  attrib HV204    format=F00020_. label="Time to get to water source (minutes)";
  attrib HV205    format=F00021_. label="Type of toilet facility";
  attrib HV206    format=F00022_. label="Has electricity";
  attrib HV207    format=F00023_. label="Has radio";
  attrib HV208    format=F00024_. label="Has television";
  attrib HV209    format=F00025_. label="Has refrigerator";
  attrib HV210    format=F00026_. label="Has bicycle";
  attrib HV211    format=F00027_. label="Has motorcycle/scooter";
  attrib HV212    format=F00028_. label="Has car/truck";
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
  attrib HV227    format=F00038_. label="Has mosquito bed net for sleeping";
  attrib HV228    format=F00039_. label="Children under 5 slept under mosquito bed net last night";
  attrib HV230A   format=F00040_. label="Place where household members wash their hands";
  attrib HV230B   format=F00041_. label="Presence of water at hand washing place";
  attrib HV232    format=F00042_. label="Items present: Soap or detergent";
  attrib HV232B   format=F00043_. label="Items present: Ash, mud, sand";
  attrib HV232C   format=F00044_. label="NA - Items present: CS";
  attrib HV232D   format=F00045_. label="NA - Items present: CS";
  attrib HV232E   format=F00046_. label="NA - Items present: CS";
  attrib HV232Y   format=F00047_. label="Items present: None";
  attrib HV234    format=F00048_. label="NA - Result of salt test for iodine (PPM)";
  attrib HV234A   format=F00049_. label="Result of salt test for iodine";
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
  attrib HV238A   format=F00067_. label="NA - Location of toilet facility";
  attrib HV239    format=F00068_. label="NA - Food cooked on stove or open fire";
  attrib HV240    format=F00069_. label="NA - Household has a chimney, hood or neither";
  attrib HV241    format=F00070_. label="Food cooked in the house/ separate building/ outdoors";
  attrib HV242    format=F00071_. label="Household has separate room used as kitchen";
  attrib HV243A   format=F00072_. label="Has mobile telephone";
  attrib HV243B   format=F00073_. label="Has watch";
  attrib HV243C   format=F00074_. label="Has animal-drawn cart";
  attrib HV243D   format=F00075_. label="NA - Has boat with a motor";
  attrib HV243E   format=F00076_. label="NA - Has a computer";
  attrib HV244    format=F00077_. label="Owns land usable for agriculture";
  attrib HV245    format=F00078_. label="Hectares of agricultural land (1 decimal)";
  attrib HV246    format=F00079_. label="Owns livestock, herds or farm animals";
  attrib HV246A   format=F00080_. label="Owns cattle";
  attrib HV246B   format=F00081_. label="Owns cows/ bulls";
  attrib HV246C   format=F00082_. label="Owns horses/ donkeys/ mules";
  attrib HV246D   format=F00083_. label="Owns goats";
  attrib HV246E   format=F00084_. label="Owns sheep";
  attrib HV246F   format=F00085_. label="Owns chickens/poultry";
  attrib HV246G   format=F00086_. label="Owns camels";
  attrib HV246H   format=F00087_. label="Owns ducks";
  attrib HV246I   format=F00088_. label="NA - Owns CS";
  attrib HV246J   format=F00089_. label="NA - Owns CS";
  attrib HV246K   format=F00090_. label="NA - Owns CS";
  attrib HV247    format=F00091_. label="Has bank account";
  attrib HV252    format=F00092_. label="Frequency household members smoke inside the house";
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
  attrib HV270A   format=F00105_. label="NA - Wealth index for urban/rural";
  attrib HV271A   label="NA - Wealth index factor score for urban/rural (5 decimals)";
  attrib HML1     format=F00106_. label="Number of mosquito bed nets";
  attrib HML1A    label="Number of mosquito bed nets with specific information";
  attrib HML2     label="Number of children under mosquito bed net previous night";
  attrib SHDISTRICT label="District of residence";
  attrib SH107    format=F00107_. label="Type of toilet facility";
  attrib SH110G   format=F00108_. label="Table";
  attrib SH110H   format=F00109_. label="Chair";
  attrib SH110I   format=F00110_. label="Sofa";
  attrib SH110J   format=F00111_. label="Bed";
  attrib SH110K   format=F00112_. label="Cuboard";
  attrib SH110L   format=F00113_. label="Stand fan";
  attrib SH110M   format=F00114_. label="Generator";
  attrib SH110N   format=F00115_. label="Sewing machine";
  attrib SH110O   format=F00116_. label="Computer/laptop";
  attrib SH118F   format=F00117_. label="Tractor";
  attrib SH118G   format=F00118_. label="Rickshaw";
  attrib SH120    format=F00119_. label="Jeribs for agricultural land";
  attrib SH123A   format=F00120_. label="Diagnosed with cancer";
  attrib SH123BA  format=F00121_. label="Type of cancer: Breast cancer";
  attrib SH123BB  format=F00122_. label="Type of cancer: Lung cancer";
  attrib SH123BC  format=F00123_. label="Type of cancer: Liver cancer";
  attrib SH123BD  format=F00124_. label="Type of cancer: Duodenal cancer";
  attrib SH123BE  format=F00125_. label="Type of cancer: Cervical cancer";
  attrib SH123BX  format=F00126_. label="Type of cancer: Other";
  attrib SH123BZ  format=F00127_. label="Type of cancer: Don't know";
  attrib SH123C   format=F00128_. label="Died due to cancer";
  attrib SH123DA  format=F00129_. label="Type of cancer that caused death: Breast cancer";
  attrib SH123DB  format=F00130_. label="Type of cancer that caused death: Lung cancer";
  attrib SH123DC  format=F00131_. label="Type of cancer that caused death: Liver cancer";
  attrib SH123DD  format=F00132_. label="Type of cancer that caused death: Duodenal cancer";
  attrib SH123DE  format=F00133_. label="Type of cancer that caused death: Cervical cancer";
  attrib SH123DX  format=F00134_. label="Type of cancer that caused death: Other";
  attrib SH123DZ  format=F00135_. label="Type of cancer that caused death: Don't know";
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
  attrib HVIDX_31 label="Line number(31)";
  attrib HVIDX_32 label="Line number(32)";
  attrib HVIDX_33 label="Line number(33)";
  attrib HVIDX_34 label="Line number(34)";
  attrib HVIDX_35 label="Line number(35)";
  attrib HVIDX_36 label="Line number(36)";
  attrib HVIDX_37 label="Line number(37)";
  attrib HVIDX_38 label="Line number(38)";
  attrib HVIDX_39 label="Line number(39)";
  attrib HVIDX_40 label="Line number(40)";
  attrib HVIDX_41 label="Line number(41)";
  attrib HVIDX_42 label="Line number(42)";
  attrib HVIDX_43 label="Line number(43)";
  attrib HVIDX_44 label="Line number(44)";
  attrib HVIDX_45 label="Line number(45)";
  attrib HVIDX_46 label="Line number(46)";
  attrib HVIDX_47 label="Line number(47)";
  attrib HVIDX_48 label="Line number(48)";
  attrib HV101_01 format=F00136_. label="Relationship to head(1)";
  attrib HV101_02 format=F00136_. label="Relationship to head(2)";
  attrib HV101_03 format=F00136_. label="Relationship to head(3)";
  attrib HV101_04 format=F00136_. label="Relationship to head(4)";
  attrib HV101_05 format=F00136_. label="Relationship to head(5)";
  attrib HV101_06 format=F00136_. label="Relationship to head(6)";
  attrib HV101_07 format=F00136_. label="Relationship to head(7)";
  attrib HV101_08 format=F00136_. label="Relationship to head(8)";
  attrib HV101_09 format=F00136_. label="Relationship to head(9)";
  attrib HV101_10 format=F00136_. label="Relationship to head(10)";
  attrib HV101_11 format=F00136_. label="Relationship to head(11)";
  attrib HV101_12 format=F00136_. label="Relationship to head(12)";
  attrib HV101_13 format=F00136_. label="Relationship to head(13)";
  attrib HV101_14 format=F00136_. label="Relationship to head(14)";
  attrib HV101_15 format=F00136_. label="Relationship to head(15)";
  attrib HV101_16 format=F00136_. label="Relationship to head(16)";
  attrib HV101_17 format=F00136_. label="Relationship to head(17)";
  attrib HV101_18 format=F00136_. label="Relationship to head(18)";
  attrib HV101_19 format=F00136_. label="Relationship to head(19)";
  attrib HV101_20 format=F00136_. label="Relationship to head(20)";
  attrib HV101_21 format=F00136_. label="Relationship to head(21)";
  attrib HV101_22 format=F00136_. label="Relationship to head(22)";
  attrib HV101_23 format=F00136_. label="Relationship to head(23)";
  attrib HV101_24 format=F00136_. label="Relationship to head(24)";
  attrib HV101_25 format=F00136_. label="Relationship to head(25)";
  attrib HV101_26 format=F00136_. label="Relationship to head(26)";
  attrib HV101_27 format=F00136_. label="Relationship to head(27)";
  attrib HV101_28 format=F00136_. label="Relationship to head(28)";
  attrib HV101_29 format=F00136_. label="Relationship to head(29)";
  attrib HV101_30 format=F00136_. label="Relationship to head(30)";
  attrib HV101_31 format=F00136_. label="Relationship to head(31)";
  attrib HV101_32 format=F00136_. label="Relationship to head(32)";
  attrib HV101_33 format=F00136_. label="Relationship to head(33)";
  attrib HV101_34 format=F00136_. label="Relationship to head(34)";
  attrib HV101_35 format=F00136_. label="Relationship to head(35)";
  attrib HV101_36 format=F00136_. label="Relationship to head(36)";
  attrib HV101_37 format=F00136_. label="Relationship to head(37)";
  attrib HV101_38 format=F00136_. label="Relationship to head(38)";
  attrib HV101_39 format=F00136_. label="Relationship to head(39)";
  attrib HV101_40 format=F00136_. label="Relationship to head(40)";
  attrib HV101_41 format=F00136_. label="Relationship to head(41)";
  attrib HV101_42 format=F00136_. label="Relationship to head(42)";
  attrib HV101_43 format=F00136_. label="Relationship to head(43)";
  attrib HV101_44 format=F00136_. label="Relationship to head(44)";
  attrib HV101_45 format=F00136_. label="Relationship to head(45)";
  attrib HV101_46 format=F00136_. label="Relationship to head(46)";
  attrib HV101_47 format=F00136_. label="Relationship to head(47)";
  attrib HV101_48 format=F00136_. label="Relationship to head(48)";
  attrib HV102_01 format=F00137_. label="Usual resident(1)";
  attrib HV102_02 format=F00137_. label="Usual resident(2)";
  attrib HV102_03 format=F00137_. label="Usual resident(3)";
  attrib HV102_04 format=F00137_. label="Usual resident(4)";
  attrib HV102_05 format=F00137_. label="Usual resident(5)";
  attrib HV102_06 format=F00137_. label="Usual resident(6)";
  attrib HV102_07 format=F00137_. label="Usual resident(7)";
  attrib HV102_08 format=F00137_. label="Usual resident(8)";
  attrib HV102_09 format=F00137_. label="Usual resident(9)";
  attrib HV102_10 format=F00137_. label="Usual resident(10)";
  attrib HV102_11 format=F00137_. label="Usual resident(11)";
  attrib HV102_12 format=F00137_. label="Usual resident(12)";
  attrib HV102_13 format=F00137_. label="Usual resident(13)";
  attrib HV102_14 format=F00137_. label="Usual resident(14)";
  attrib HV102_15 format=F00137_. label="Usual resident(15)";
  attrib HV102_16 format=F00137_. label="Usual resident(16)";
  attrib HV102_17 format=F00137_. label="Usual resident(17)";
  attrib HV102_18 format=F00137_. label="Usual resident(18)";
  attrib HV102_19 format=F00137_. label="Usual resident(19)";
  attrib HV102_20 format=F00137_. label="Usual resident(20)";
  attrib HV102_21 format=F00137_. label="Usual resident(21)";
  attrib HV102_22 format=F00137_. label="Usual resident(22)";
  attrib HV102_23 format=F00137_. label="Usual resident(23)";
  attrib HV102_24 format=F00137_. label="Usual resident(24)";
  attrib HV102_25 format=F00137_. label="Usual resident(25)";
  attrib HV102_26 format=F00137_. label="Usual resident(26)";
  attrib HV102_27 format=F00137_. label="Usual resident(27)";
  attrib HV102_28 format=F00137_. label="Usual resident(28)";
  attrib HV102_29 format=F00137_. label="Usual resident(29)";
  attrib HV102_30 format=F00137_. label="Usual resident(30)";
  attrib HV102_31 format=F00137_. label="Usual resident(31)";
  attrib HV102_32 format=F00137_. label="Usual resident(32)";
  attrib HV102_33 format=F00137_. label="Usual resident(33)";
  attrib HV102_34 format=F00137_. label="Usual resident(34)";
  attrib HV102_35 format=F00137_. label="Usual resident(35)";
  attrib HV102_36 format=F00137_. label="Usual resident(36)";
  attrib HV102_37 format=F00137_. label="Usual resident(37)";
  attrib HV102_38 format=F00137_. label="Usual resident(38)";
  attrib HV102_39 format=F00137_. label="Usual resident(39)";
  attrib HV102_40 format=F00137_. label="Usual resident(40)";
  attrib HV102_41 format=F00137_. label="Usual resident(41)";
  attrib HV102_42 format=F00137_. label="Usual resident(42)";
  attrib HV102_43 format=F00137_. label="Usual resident(43)";
  attrib HV102_44 format=F00137_. label="Usual resident(44)";
  attrib HV102_45 format=F00137_. label="Usual resident(45)";
  attrib HV102_46 format=F00137_. label="Usual resident(46)";
  attrib HV102_47 format=F00137_. label="Usual resident(47)";
  attrib HV102_48 format=F00137_. label="Usual resident(48)";
  attrib HV103_01 format=F00138_. label="Slept last night(1)";
  attrib HV103_02 format=F00138_. label="Slept last night(2)";
  attrib HV103_03 format=F00138_. label="Slept last night(3)";
  attrib HV103_04 format=F00138_. label="Slept last night(4)";
  attrib HV103_05 format=F00138_. label="Slept last night(5)";
  attrib HV103_06 format=F00138_. label="Slept last night(6)";
  attrib HV103_07 format=F00138_. label="Slept last night(7)";
  attrib HV103_08 format=F00138_. label="Slept last night(8)";
  attrib HV103_09 format=F00138_. label="Slept last night(9)";
  attrib HV103_10 format=F00138_. label="Slept last night(10)";
  attrib HV103_11 format=F00138_. label="Slept last night(11)";
  attrib HV103_12 format=F00138_. label="Slept last night(12)";
  attrib HV103_13 format=F00138_. label="Slept last night(13)";
  attrib HV103_14 format=F00138_. label="Slept last night(14)";
  attrib HV103_15 format=F00138_. label="Slept last night(15)";
  attrib HV103_16 format=F00138_. label="Slept last night(16)";
  attrib HV103_17 format=F00138_. label="Slept last night(17)";
  attrib HV103_18 format=F00138_. label="Slept last night(18)";
  attrib HV103_19 format=F00138_. label="Slept last night(19)";
  attrib HV103_20 format=F00138_. label="Slept last night(20)";
  attrib HV103_21 format=F00138_. label="Slept last night(21)";
  attrib HV103_22 format=F00138_. label="Slept last night(22)";
  attrib HV103_23 format=F00138_. label="Slept last night(23)";
  attrib HV103_24 format=F00138_. label="Slept last night(24)";
  attrib HV103_25 format=F00138_. label="Slept last night(25)";
  attrib HV103_26 format=F00138_. label="Slept last night(26)";
  attrib HV103_27 format=F00138_. label="Slept last night(27)";
  attrib HV103_28 format=F00138_. label="Slept last night(28)";
  attrib HV103_29 format=F00138_. label="Slept last night(29)";
  attrib HV103_30 format=F00138_. label="Slept last night(30)";
  attrib HV103_31 format=F00138_. label="Slept last night(31)";
  attrib HV103_32 format=F00138_. label="Slept last night(32)";
  attrib HV103_33 format=F00138_. label="Slept last night(33)";
  attrib HV103_34 format=F00138_. label="Slept last night(34)";
  attrib HV103_35 format=F00138_. label="Slept last night(35)";
  attrib HV103_36 format=F00138_. label="Slept last night(36)";
  attrib HV103_37 format=F00138_. label="Slept last night(37)";
  attrib HV103_38 format=F00138_. label="Slept last night(38)";
  attrib HV103_39 format=F00138_. label="Slept last night(39)";
  attrib HV103_40 format=F00138_. label="Slept last night(40)";
  attrib HV103_41 format=F00138_. label="Slept last night(41)";
  attrib HV103_42 format=F00138_. label="Slept last night(42)";
  attrib HV103_43 format=F00138_. label="Slept last night(43)";
  attrib HV103_44 format=F00138_. label="Slept last night(44)";
  attrib HV103_45 format=F00138_. label="Slept last night(45)";
  attrib HV103_46 format=F00138_. label="Slept last night(46)";
  attrib HV103_47 format=F00138_. label="Slept last night(47)";
  attrib HV103_48 format=F00138_. label="Slept last night(48)";
  attrib HV104_01 format=F00139_. label="Sex of household member(1)";
  attrib HV104_02 format=F00139_. label="Sex of household member(2)";
  attrib HV104_03 format=F00139_. label="Sex of household member(3)";
  attrib HV104_04 format=F00139_. label="Sex of household member(4)";
  attrib HV104_05 format=F00139_. label="Sex of household member(5)";
  attrib HV104_06 format=F00139_. label="Sex of household member(6)";
  attrib HV104_07 format=F00139_. label="Sex of household member(7)";
  attrib HV104_08 format=F00139_. label="Sex of household member(8)";
  attrib HV104_09 format=F00139_. label="Sex of household member(9)";
  attrib HV104_10 format=F00139_. label="Sex of household member(10)";
  attrib HV104_11 format=F00139_. label="Sex of household member(11)";
  attrib HV104_12 format=F00139_. label="Sex of household member(12)";
  attrib HV104_13 format=F00139_. label="Sex of household member(13)";
  attrib HV104_14 format=F00139_. label="Sex of household member(14)";
  attrib HV104_15 format=F00139_. label="Sex of household member(15)";
  attrib HV104_16 format=F00139_. label="Sex of household member(16)";
  attrib HV104_17 format=F00139_. label="Sex of household member(17)";
  attrib HV104_18 format=F00139_. label="Sex of household member(18)";
  attrib HV104_19 format=F00139_. label="Sex of household member(19)";
  attrib HV104_20 format=F00139_. label="Sex of household member(20)";
  attrib HV104_21 format=F00139_. label="Sex of household member(21)";
  attrib HV104_22 format=F00139_. label="Sex of household member(22)";
  attrib HV104_23 format=F00139_. label="Sex of household member(23)";
  attrib HV104_24 format=F00139_. label="Sex of household member(24)";
  attrib HV104_25 format=F00139_. label="Sex of household member(25)";
  attrib HV104_26 format=F00139_. label="Sex of household member(26)";
  attrib HV104_27 format=F00139_. label="Sex of household member(27)";
  attrib HV104_28 format=F00139_. label="Sex of household member(28)";
  attrib HV104_29 format=F00139_. label="Sex of household member(29)";
  attrib HV104_30 format=F00139_. label="Sex of household member(30)";
  attrib HV104_31 format=F00139_. label="Sex of household member(31)";
  attrib HV104_32 format=F00139_. label="Sex of household member(32)";
  attrib HV104_33 format=F00139_. label="Sex of household member(33)";
  attrib HV104_34 format=F00139_. label="Sex of household member(34)";
  attrib HV104_35 format=F00139_. label="Sex of household member(35)";
  attrib HV104_36 format=F00139_. label="Sex of household member(36)";
  attrib HV104_37 format=F00139_. label="Sex of household member(37)";
  attrib HV104_38 format=F00139_. label="Sex of household member(38)";
  attrib HV104_39 format=F00139_. label="Sex of household member(39)";
  attrib HV104_40 format=F00139_. label="Sex of household member(40)";
  attrib HV104_41 format=F00139_. label="Sex of household member(41)";
  attrib HV104_42 format=F00139_. label="Sex of household member(42)";
  attrib HV104_43 format=F00139_. label="Sex of household member(43)";
  attrib HV104_44 format=F00139_. label="Sex of household member(44)";
  attrib HV104_45 format=F00139_. label="Sex of household member(45)";
  attrib HV104_46 format=F00139_. label="Sex of household member(46)";
  attrib HV104_47 format=F00139_. label="Sex of household member(47)";
  attrib HV104_48 format=F00139_. label="Sex of household member(48)";
  attrib HV105_01 format=F00140_. label="Age of household members(1)";
  attrib HV105_02 format=F00140_. label="Age of household members(2)";
  attrib HV105_03 format=F00140_. label="Age of household members(3)";
  attrib HV105_04 format=F00140_. label="Age of household members(4)";
  attrib HV105_05 format=F00140_. label="Age of household members(5)";
  attrib HV105_06 format=F00140_. label="Age of household members(6)";
  attrib HV105_07 format=F00140_. label="Age of household members(7)";
  attrib HV105_08 format=F00140_. label="Age of household members(8)";
  attrib HV105_09 format=F00140_. label="Age of household members(9)";
  attrib HV105_10 format=F00140_. label="Age of household members(10)";
  attrib HV105_11 format=F00140_. label="Age of household members(11)";
  attrib HV105_12 format=F00140_. label="Age of household members(12)";
  attrib HV105_13 format=F00140_. label="Age of household members(13)";
  attrib HV105_14 format=F00140_. label="Age of household members(14)";
  attrib HV105_15 format=F00140_. label="Age of household members(15)";
  attrib HV105_16 format=F00140_. label="Age of household members(16)";
  attrib HV105_17 format=F00140_. label="Age of household members(17)";
  attrib HV105_18 format=F00140_. label="Age of household members(18)";
  attrib HV105_19 format=F00140_. label="Age of household members(19)";
  attrib HV105_20 format=F00140_. label="Age of household members(20)";
  attrib HV105_21 format=F00140_. label="Age of household members(21)";
  attrib HV105_22 format=F00140_. label="Age of household members(22)";
  attrib HV105_23 format=F00140_. label="Age of household members(23)";
  attrib HV105_24 format=F00140_. label="Age of household members(24)";
  attrib HV105_25 format=F00140_. label="Age of household members(25)";
  attrib HV105_26 format=F00140_. label="Age of household members(26)";
  attrib HV105_27 format=F00140_. label="Age of household members(27)";
  attrib HV105_28 format=F00140_. label="Age of household members(28)";
  attrib HV105_29 format=F00140_. label="Age of household members(29)";
  attrib HV105_30 format=F00140_. label="Age of household members(30)";
  attrib HV105_31 format=F00140_. label="Age of household members(31)";
  attrib HV105_32 format=F00140_. label="Age of household members(32)";
  attrib HV105_33 format=F00140_. label="Age of household members(33)";
  attrib HV105_34 format=F00140_. label="Age of household members(34)";
  attrib HV105_35 format=F00140_. label="Age of household members(35)";
  attrib HV105_36 format=F00140_. label="Age of household members(36)";
  attrib HV105_37 format=F00140_. label="Age of household members(37)";
  attrib HV105_38 format=F00140_. label="Age of household members(38)";
  attrib HV105_39 format=F00140_. label="Age of household members(39)";
  attrib HV105_40 format=F00140_. label="Age of household members(40)";
  attrib HV105_41 format=F00140_. label="Age of household members(41)";
  attrib HV105_42 format=F00140_. label="Age of household members(42)";
  attrib HV105_43 format=F00140_. label="Age of household members(43)";
  attrib HV105_44 format=F00140_. label="Age of household members(44)";
  attrib HV105_45 format=F00140_. label="Age of household members(45)";
  attrib HV105_46 format=F00140_. label="Age of household members(46)";
  attrib HV105_47 format=F00140_. label="Age of household members(47)";
  attrib HV105_48 format=F00140_. label="Age of household members(48)";
  attrib HV106_01 format=F00141_. label="Highest educational level attained(1)";
  attrib HV106_02 format=F00141_. label="Highest educational level attained(2)";
  attrib HV106_03 format=F00141_. label="Highest educational level attained(3)";
  attrib HV106_04 format=F00141_. label="Highest educational level attained(4)";
  attrib HV106_05 format=F00141_. label="Highest educational level attained(5)";
  attrib HV106_06 format=F00141_. label="Highest educational level attained(6)";
  attrib HV106_07 format=F00141_. label="Highest educational level attained(7)";
  attrib HV106_08 format=F00141_. label="Highest educational level attained(8)";
  attrib HV106_09 format=F00141_. label="Highest educational level attained(9)";
  attrib HV106_10 format=F00141_. label="Highest educational level attained(10)";
  attrib HV106_11 format=F00141_. label="Highest educational level attained(11)";
  attrib HV106_12 format=F00141_. label="Highest educational level attained(12)";
  attrib HV106_13 format=F00141_. label="Highest educational level attained(13)";
  attrib HV106_14 format=F00141_. label="Highest educational level attained(14)";
  attrib HV106_15 format=F00141_. label="Highest educational level attained(15)";
  attrib HV106_16 format=F00141_. label="Highest educational level attained(16)";
  attrib HV106_17 format=F00141_. label="Highest educational level attained(17)";
  attrib HV106_18 format=F00141_. label="Highest educational level attained(18)";
  attrib HV106_19 format=F00141_. label="Highest educational level attained(19)";
  attrib HV106_20 format=F00141_. label="Highest educational level attained(20)";
  attrib HV106_21 format=F00141_. label="Highest educational level attained(21)";
  attrib HV106_22 format=F00141_. label="Highest educational level attained(22)";
  attrib HV106_23 format=F00141_. label="Highest educational level attained(23)";
  attrib HV106_24 format=F00141_. label="Highest educational level attained(24)";
  attrib HV106_25 format=F00141_. label="Highest educational level attained(25)";
  attrib HV106_26 format=F00141_. label="Highest educational level attained(26)";
  attrib HV106_27 format=F00141_. label="Highest educational level attained(27)";
  attrib HV106_28 format=F00141_. label="Highest educational level attained(28)";
  attrib HV106_29 format=F00141_. label="Highest educational level attained(29)";
  attrib HV106_30 format=F00141_. label="Highest educational level attained(30)";
  attrib HV106_31 format=F00141_. label="Highest educational level attained(31)";
  attrib HV106_32 format=F00141_. label="Highest educational level attained(32)";
  attrib HV106_33 format=F00141_. label="Highest educational level attained(33)";
  attrib HV106_34 format=F00141_. label="Highest educational level attained(34)";
  attrib HV106_35 format=F00141_. label="Highest educational level attained(35)";
  attrib HV106_36 format=F00141_. label="Highest educational level attained(36)";
  attrib HV106_37 format=F00141_. label="Highest educational level attained(37)";
  attrib HV106_38 format=F00141_. label="Highest educational level attained(38)";
  attrib HV106_39 format=F00141_. label="Highest educational level attained(39)";
  attrib HV106_40 format=F00141_. label="Highest educational level attained(40)";
  attrib HV106_41 format=F00141_. label="Highest educational level attained(41)";
  attrib HV106_42 format=F00141_. label="Highest educational level attained(42)";
  attrib HV106_43 format=F00141_. label="Highest educational level attained(43)";
  attrib HV106_44 format=F00141_. label="Highest educational level attained(44)";
  attrib HV106_45 format=F00141_. label="Highest educational level attained(45)";
  attrib HV106_46 format=F00141_. label="Highest educational level attained(46)";
  attrib HV106_47 format=F00141_. label="Highest educational level attained(47)";
  attrib HV106_48 format=F00141_. label="Highest educational level attained(48)";
  attrib HV107_01 format=F00142_. label="Highest year of education completed(1)";
  attrib HV107_02 format=F00142_. label="Highest year of education completed(2)";
  attrib HV107_03 format=F00142_. label="Highest year of education completed(3)";
  attrib HV107_04 format=F00142_. label="Highest year of education completed(4)";
  attrib HV107_05 format=F00142_. label="Highest year of education completed(5)";
  attrib HV107_06 format=F00142_. label="Highest year of education completed(6)";
  attrib HV107_07 format=F00142_. label="Highest year of education completed(7)";
  attrib HV107_08 format=F00142_. label="Highest year of education completed(8)";
  attrib HV107_09 format=F00142_. label="Highest year of education completed(9)";
  attrib HV107_10 format=F00142_. label="Highest year of education completed(10)";
  attrib HV107_11 format=F00142_. label="Highest year of education completed(11)";
  attrib HV107_12 format=F00142_. label="Highest year of education completed(12)";
  attrib HV107_13 format=F00142_. label="Highest year of education completed(13)";
  attrib HV107_14 format=F00142_. label="Highest year of education completed(14)";
  attrib HV107_15 format=F00142_. label="Highest year of education completed(15)";
  attrib HV107_16 format=F00142_. label="Highest year of education completed(16)";
  attrib HV107_17 format=F00142_. label="Highest year of education completed(17)";
  attrib HV107_18 format=F00142_. label="Highest year of education completed(18)";
  attrib HV107_19 format=F00142_. label="Highest year of education completed(19)";
  attrib HV107_20 format=F00142_. label="Highest year of education completed(20)";
  attrib HV107_21 format=F00142_. label="Highest year of education completed(21)";
  attrib HV107_22 format=F00142_. label="Highest year of education completed(22)";
  attrib HV107_23 format=F00142_. label="Highest year of education completed(23)";
  attrib HV107_24 format=F00142_. label="Highest year of education completed(24)";
  attrib HV107_25 format=F00142_. label="Highest year of education completed(25)";
  attrib HV107_26 format=F00142_. label="Highest year of education completed(26)";
  attrib HV107_27 format=F00142_. label="Highest year of education completed(27)";
  attrib HV107_28 format=F00142_. label="Highest year of education completed(28)";
  attrib HV107_29 format=F00142_. label="Highest year of education completed(29)";
  attrib HV107_30 format=F00142_. label="Highest year of education completed(30)";
  attrib HV107_31 format=F00142_. label="Highest year of education completed(31)";
  attrib HV107_32 format=F00142_. label="Highest year of education completed(32)";
  attrib HV107_33 format=F00142_. label="Highest year of education completed(33)";
  attrib HV107_34 format=F00142_. label="Highest year of education completed(34)";
  attrib HV107_35 format=F00142_. label="Highest year of education completed(35)";
  attrib HV107_36 format=F00142_. label="Highest year of education completed(36)";
  attrib HV107_37 format=F00142_. label="Highest year of education completed(37)";
  attrib HV107_38 format=F00142_. label="Highest year of education completed(38)";
  attrib HV107_39 format=F00142_. label="Highest year of education completed(39)";
  attrib HV107_40 format=F00142_. label="Highest year of education completed(40)";
  attrib HV107_41 format=F00142_. label="Highest year of education completed(41)";
  attrib HV107_42 format=F00142_. label="Highest year of education completed(42)";
  attrib HV107_43 format=F00142_. label="Highest year of education completed(43)";
  attrib HV107_44 format=F00142_. label="Highest year of education completed(44)";
  attrib HV107_45 format=F00142_. label="Highest year of education completed(45)";
  attrib HV107_46 format=F00142_. label="Highest year of education completed(46)";
  attrib HV107_47 format=F00142_. label="Highest year of education completed(47)";
  attrib HV107_48 format=F00142_. label="Highest year of education completed(48)";
  attrib HV108_01 format=F00143_. label="Education completed in single years(1)";
  attrib HV108_02 format=F00143_. label="Education completed in single years(2)";
  attrib HV108_03 format=F00143_. label="Education completed in single years(3)";
  attrib HV108_04 format=F00143_. label="Education completed in single years(4)";
  attrib HV108_05 format=F00143_. label="Education completed in single years(5)";
  attrib HV108_06 format=F00143_. label="Education completed in single years(6)";
  attrib HV108_07 format=F00143_. label="Education completed in single years(7)";
  attrib HV108_08 format=F00143_. label="Education completed in single years(8)";
  attrib HV108_09 format=F00143_. label="Education completed in single years(9)";
  attrib HV108_10 format=F00143_. label="Education completed in single years(10)";
  attrib HV108_11 format=F00143_. label="Education completed in single years(11)";
  attrib HV108_12 format=F00143_. label="Education completed in single years(12)";
  attrib HV108_13 format=F00143_. label="Education completed in single years(13)";
  attrib HV108_14 format=F00143_. label="Education completed in single years(14)";
  attrib HV108_15 format=F00143_. label="Education completed in single years(15)";
  attrib HV108_16 format=F00143_. label="Education completed in single years(16)";
  attrib HV108_17 format=F00143_. label="Education completed in single years(17)";
  attrib HV108_18 format=F00143_. label="Education completed in single years(18)";
  attrib HV108_19 format=F00143_. label="Education completed in single years(19)";
  attrib HV108_20 format=F00143_. label="Education completed in single years(20)";
  attrib HV108_21 format=F00143_. label="Education completed in single years(21)";
  attrib HV108_22 format=F00143_. label="Education completed in single years(22)";
  attrib HV108_23 format=F00143_. label="Education completed in single years(23)";
  attrib HV108_24 format=F00143_. label="Education completed in single years(24)";
  attrib HV108_25 format=F00143_. label="Education completed in single years(25)";
  attrib HV108_26 format=F00143_. label="Education completed in single years(26)";
  attrib HV108_27 format=F00143_. label="Education completed in single years(27)";
  attrib HV108_28 format=F00143_. label="Education completed in single years(28)";
  attrib HV108_29 format=F00143_. label="Education completed in single years(29)";
  attrib HV108_30 format=F00143_. label="Education completed in single years(30)";
  attrib HV108_31 format=F00143_. label="Education completed in single years(31)";
  attrib HV108_32 format=F00143_. label="Education completed in single years(32)";
  attrib HV108_33 format=F00143_. label="Education completed in single years(33)";
  attrib HV108_34 format=F00143_. label="Education completed in single years(34)";
  attrib HV108_35 format=F00143_. label="Education completed in single years(35)";
  attrib HV108_36 format=F00143_. label="Education completed in single years(36)";
  attrib HV108_37 format=F00143_. label="Education completed in single years(37)";
  attrib HV108_38 format=F00143_. label="Education completed in single years(38)";
  attrib HV108_39 format=F00143_. label="Education completed in single years(39)";
  attrib HV108_40 format=F00143_. label="Education completed in single years(40)";
  attrib HV108_41 format=F00143_. label="Education completed in single years(41)";
  attrib HV108_42 format=F00143_. label="Education completed in single years(42)";
  attrib HV108_43 format=F00143_. label="Education completed in single years(43)";
  attrib HV108_44 format=F00143_. label="Education completed in single years(44)";
  attrib HV108_45 format=F00143_. label="Education completed in single years(45)";
  attrib HV108_46 format=F00143_. label="Education completed in single years(46)";
  attrib HV108_47 format=F00143_. label="Education completed in single years(47)";
  attrib HV108_48 format=F00143_. label="Education completed in single years(48)";
  attrib HV109_01 format=F00144_. label="Educational attainment(1)";
  attrib HV109_02 format=F00144_. label="Educational attainment(2)";
  attrib HV109_03 format=F00144_. label="Educational attainment(3)";
  attrib HV109_04 format=F00144_. label="Educational attainment(4)";
  attrib HV109_05 format=F00144_. label="Educational attainment(5)";
  attrib HV109_06 format=F00144_. label="Educational attainment(6)";
  attrib HV109_07 format=F00144_. label="Educational attainment(7)";
  attrib HV109_08 format=F00144_. label="Educational attainment(8)";
  attrib HV109_09 format=F00144_. label="Educational attainment(9)";
  attrib HV109_10 format=F00144_. label="Educational attainment(10)";
  attrib HV109_11 format=F00144_. label="Educational attainment(11)";
  attrib HV109_12 format=F00144_. label="Educational attainment(12)";
  attrib HV109_13 format=F00144_. label="Educational attainment(13)";
  attrib HV109_14 format=F00144_. label="Educational attainment(14)";
  attrib HV109_15 format=F00144_. label="Educational attainment(15)";
  attrib HV109_16 format=F00144_. label="Educational attainment(16)";
  attrib HV109_17 format=F00144_. label="Educational attainment(17)";
  attrib HV109_18 format=F00144_. label="Educational attainment(18)";
  attrib HV109_19 format=F00144_. label="Educational attainment(19)";
  attrib HV109_20 format=F00144_. label="Educational attainment(20)";
  attrib HV109_21 format=F00144_. label="Educational attainment(21)";
  attrib HV109_22 format=F00144_. label="Educational attainment(22)";
  attrib HV109_23 format=F00144_. label="Educational attainment(23)";
  attrib HV109_24 format=F00144_. label="Educational attainment(24)";
  attrib HV109_25 format=F00144_. label="Educational attainment(25)";
  attrib HV109_26 format=F00144_. label="Educational attainment(26)";
  attrib HV109_27 format=F00144_. label="Educational attainment(27)";
  attrib HV109_28 format=F00144_. label="Educational attainment(28)";
  attrib HV109_29 format=F00144_. label="Educational attainment(29)";
  attrib HV109_30 format=F00144_. label="Educational attainment(30)";
  attrib HV109_31 format=F00144_. label="Educational attainment(31)";
  attrib HV109_32 format=F00144_. label="Educational attainment(32)";
  attrib HV109_33 format=F00144_. label="Educational attainment(33)";
  attrib HV109_34 format=F00144_. label="Educational attainment(34)";
  attrib HV109_35 format=F00144_. label="Educational attainment(35)";
  attrib HV109_36 format=F00144_. label="Educational attainment(36)";
  attrib HV109_37 format=F00144_. label="Educational attainment(37)";
  attrib HV109_38 format=F00144_. label="Educational attainment(38)";
  attrib HV109_39 format=F00144_. label="Educational attainment(39)";
  attrib HV109_40 format=F00144_. label="Educational attainment(40)";
  attrib HV109_41 format=F00144_. label="Educational attainment(41)";
  attrib HV109_42 format=F00144_. label="Educational attainment(42)";
  attrib HV109_43 format=F00144_. label="Educational attainment(43)";
  attrib HV109_44 format=F00144_. label="Educational attainment(44)";
  attrib HV109_45 format=F00144_. label="Educational attainment(45)";
  attrib HV109_46 format=F00144_. label="Educational attainment(46)";
  attrib HV109_47 format=F00144_. label="Educational attainment(47)";
  attrib HV109_48 format=F00144_. label="Educational attainment(48)";
  attrib HV110_01 format=F00145_. label="NA - Member still in school(1)";
  attrib HV110_02 format=F00145_. label="NA - Member still in school(2)";
  attrib HV110_03 format=F00145_. label="NA - Member still in school(3)";
  attrib HV110_04 format=F00145_. label="NA - Member still in school(4)";
  attrib HV110_05 format=F00145_. label="NA - Member still in school(5)";
  attrib HV110_06 format=F00145_. label="NA - Member still in school(6)";
  attrib HV110_07 format=F00145_. label="NA - Member still in school(7)";
  attrib HV110_08 format=F00145_. label="NA - Member still in school(8)";
  attrib HV110_09 format=F00145_. label="NA - Member still in school(9)";
  attrib HV110_10 format=F00145_. label="NA - Member still in school(10)";
  attrib HV110_11 format=F00145_. label="NA - Member still in school(11)";
  attrib HV110_12 format=F00145_. label="NA - Member still in school(12)";
  attrib HV110_13 format=F00145_. label="NA - Member still in school(13)";
  attrib HV110_14 format=F00145_. label="NA - Member still in school(14)";
  attrib HV110_15 format=F00145_. label="NA - Member still in school(15)";
  attrib HV110_16 format=F00145_. label="NA - Member still in school(16)";
  attrib HV110_17 format=F00145_. label="NA - Member still in school(17)";
  attrib HV110_18 format=F00145_. label="NA - Member still in school(18)";
  attrib HV110_19 format=F00145_. label="NA - Member still in school(19)";
  attrib HV110_20 format=F00145_. label="NA - Member still in school(20)";
  attrib HV110_21 format=F00145_. label="NA - Member still in school(21)";
  attrib HV110_22 format=F00145_. label="NA - Member still in school(22)";
  attrib HV110_23 format=F00145_. label="NA - Member still in school(23)";
  attrib HV110_24 format=F00145_. label="NA - Member still in school(24)";
  attrib HV110_25 format=F00145_. label="NA - Member still in school(25)";
  attrib HV110_26 format=F00145_. label="NA - Member still in school(26)";
  attrib HV110_27 format=F00145_. label="NA - Member still in school(27)";
  attrib HV110_28 format=F00145_. label="NA - Member still in school(28)";
  attrib HV110_29 format=F00145_. label="NA - Member still in school(29)";
  attrib HV110_30 format=F00145_. label="NA - Member still in school(30)";
  attrib HV110_31 format=F00145_. label="NA - Member still in school(31)";
  attrib HV110_32 format=F00145_. label="NA - Member still in school(32)";
  attrib HV110_33 format=F00145_. label="NA - Member still in school(33)";
  attrib HV110_34 format=F00145_. label="NA - Member still in school(34)";
  attrib HV110_35 format=F00145_. label="NA - Member still in school(35)";
  attrib HV110_36 format=F00145_. label="NA - Member still in school(36)";
  attrib HV110_37 format=F00145_. label="NA - Member still in school(37)";
  attrib HV110_38 format=F00145_. label="NA - Member still in school(38)";
  attrib HV110_39 format=F00145_. label="NA - Member still in school(39)";
  attrib HV110_40 format=F00145_. label="NA - Member still in school(40)";
  attrib HV110_41 format=F00145_. label="NA - Member still in school(41)";
  attrib HV110_42 format=F00145_. label="NA - Member still in school(42)";
  attrib HV110_43 format=F00145_. label="NA - Member still in school(43)";
  attrib HV110_44 format=F00145_. label="NA - Member still in school(44)";
  attrib HV110_45 format=F00145_. label="NA - Member still in school(45)";
  attrib HV110_46 format=F00145_. label="NA - Member still in school(46)";
  attrib HV110_47 format=F00145_. label="NA - Member still in school(47)";
  attrib HV110_48 format=F00145_. label="NA - Member still in school(48)";
  attrib HV111_01 format=F00146_. label="Mother alive(1)";
  attrib HV111_02 format=F00146_. label="Mother alive(2)";
  attrib HV111_03 format=F00146_. label="Mother alive(3)";
  attrib HV111_04 format=F00146_. label="Mother alive(4)";
  attrib HV111_05 format=F00146_. label="Mother alive(5)";
  attrib HV111_06 format=F00146_. label="Mother alive(6)";
  attrib HV111_07 format=F00146_. label="Mother alive(7)";
  attrib HV111_08 format=F00146_. label="Mother alive(8)";
  attrib HV111_09 format=F00146_. label="Mother alive(9)";
  attrib HV111_10 format=F00146_. label="Mother alive(10)";
  attrib HV111_11 format=F00146_. label="Mother alive(11)";
  attrib HV111_12 format=F00146_. label="Mother alive(12)";
  attrib HV111_13 format=F00146_. label="Mother alive(13)";
  attrib HV111_14 format=F00146_. label="Mother alive(14)";
  attrib HV111_15 format=F00146_. label="Mother alive(15)";
  attrib HV111_16 format=F00146_. label="Mother alive(16)";
  attrib HV111_17 format=F00146_. label="Mother alive(17)";
  attrib HV111_18 format=F00146_. label="Mother alive(18)";
  attrib HV111_19 format=F00146_. label="Mother alive(19)";
  attrib HV111_20 format=F00146_. label="Mother alive(20)";
  attrib HV111_21 format=F00146_. label="Mother alive(21)";
  attrib HV111_22 format=F00146_. label="Mother alive(22)";
  attrib HV111_23 format=F00146_. label="Mother alive(23)";
  attrib HV111_24 format=F00146_. label="Mother alive(24)";
  attrib HV111_25 format=F00146_. label="Mother alive(25)";
  attrib HV111_26 format=F00146_. label="Mother alive(26)";
  attrib HV111_27 format=F00146_. label="Mother alive(27)";
  attrib HV111_28 format=F00146_. label="Mother alive(28)";
  attrib HV111_29 format=F00146_. label="Mother alive(29)";
  attrib HV111_30 format=F00146_. label="Mother alive(30)";
  attrib HV111_31 format=F00146_. label="Mother alive(31)";
  attrib HV111_32 format=F00146_. label="Mother alive(32)";
  attrib HV111_33 format=F00146_. label="Mother alive(33)";
  attrib HV111_34 format=F00146_. label="Mother alive(34)";
  attrib HV111_35 format=F00146_. label="Mother alive(35)";
  attrib HV111_36 format=F00146_. label="Mother alive(36)";
  attrib HV111_37 format=F00146_. label="Mother alive(37)";
  attrib HV111_38 format=F00146_. label="Mother alive(38)";
  attrib HV111_39 format=F00146_. label="Mother alive(39)";
  attrib HV111_40 format=F00146_. label="Mother alive(40)";
  attrib HV111_41 format=F00146_. label="Mother alive(41)";
  attrib HV111_42 format=F00146_. label="Mother alive(42)";
  attrib HV111_43 format=F00146_. label="Mother alive(43)";
  attrib HV111_44 format=F00146_. label="Mother alive(44)";
  attrib HV111_45 format=F00146_. label="Mother alive(45)";
  attrib HV111_46 format=F00146_. label="Mother alive(46)";
  attrib HV111_47 format=F00146_. label="Mother alive(47)";
  attrib HV111_48 format=F00146_. label="Mother alive(48)";
  attrib HV112_01 format=F00147_. label="Mother's line number(1)";
  attrib HV112_02 format=F00147_. label="Mother's line number(2)";
  attrib HV112_03 format=F00147_. label="Mother's line number(3)";
  attrib HV112_04 format=F00147_. label="Mother's line number(4)";
  attrib HV112_05 format=F00147_. label="Mother's line number(5)";
  attrib HV112_06 format=F00147_. label="Mother's line number(6)";
  attrib HV112_07 format=F00147_. label="Mother's line number(7)";
  attrib HV112_08 format=F00147_. label="Mother's line number(8)";
  attrib HV112_09 format=F00147_. label="Mother's line number(9)";
  attrib HV112_10 format=F00147_. label="Mother's line number(10)";
  attrib HV112_11 format=F00147_. label="Mother's line number(11)";
  attrib HV112_12 format=F00147_. label="Mother's line number(12)";
  attrib HV112_13 format=F00147_. label="Mother's line number(13)";
  attrib HV112_14 format=F00147_. label="Mother's line number(14)";
  attrib HV112_15 format=F00147_. label="Mother's line number(15)";
  attrib HV112_16 format=F00147_. label="Mother's line number(16)";
  attrib HV112_17 format=F00147_. label="Mother's line number(17)";
  attrib HV112_18 format=F00147_. label="Mother's line number(18)";
  attrib HV112_19 format=F00147_. label="Mother's line number(19)";
  attrib HV112_20 format=F00147_. label="Mother's line number(20)";
  attrib HV112_21 format=F00147_. label="Mother's line number(21)";
  attrib HV112_22 format=F00147_. label="Mother's line number(22)";
  attrib HV112_23 format=F00147_. label="Mother's line number(23)";
  attrib HV112_24 format=F00147_. label="Mother's line number(24)";
  attrib HV112_25 format=F00147_. label="Mother's line number(25)";
  attrib HV112_26 format=F00147_. label="Mother's line number(26)";
  attrib HV112_27 format=F00147_. label="Mother's line number(27)";
  attrib HV112_28 format=F00147_. label="Mother's line number(28)";
  attrib HV112_29 format=F00147_. label="Mother's line number(29)";
  attrib HV112_30 format=F00147_. label="Mother's line number(30)";
  attrib HV112_31 format=F00147_. label="Mother's line number(31)";
  attrib HV112_32 format=F00147_. label="Mother's line number(32)";
  attrib HV112_33 format=F00147_. label="Mother's line number(33)";
  attrib HV112_34 format=F00147_. label="Mother's line number(34)";
  attrib HV112_35 format=F00147_. label="Mother's line number(35)";
  attrib HV112_36 format=F00147_. label="Mother's line number(36)";
  attrib HV112_37 format=F00147_. label="Mother's line number(37)";
  attrib HV112_38 format=F00147_. label="Mother's line number(38)";
  attrib HV112_39 format=F00147_. label="Mother's line number(39)";
  attrib HV112_40 format=F00147_. label="Mother's line number(40)";
  attrib HV112_41 format=F00147_. label="Mother's line number(41)";
  attrib HV112_42 format=F00147_. label="Mother's line number(42)";
  attrib HV112_43 format=F00147_. label="Mother's line number(43)";
  attrib HV112_44 format=F00147_. label="Mother's line number(44)";
  attrib HV112_45 format=F00147_. label="Mother's line number(45)";
  attrib HV112_46 format=F00147_. label="Mother's line number(46)";
  attrib HV112_47 format=F00147_. label="Mother's line number(47)";
  attrib HV112_48 format=F00147_. label="Mother's line number(48)";
  attrib HV113_01 format=F00148_. label="Father alive(1)";
  attrib HV113_02 format=F00148_. label="Father alive(2)";
  attrib HV113_03 format=F00148_. label="Father alive(3)";
  attrib HV113_04 format=F00148_. label="Father alive(4)";
  attrib HV113_05 format=F00148_. label="Father alive(5)";
  attrib HV113_06 format=F00148_. label="Father alive(6)";
  attrib HV113_07 format=F00148_. label="Father alive(7)";
  attrib HV113_08 format=F00148_. label="Father alive(8)";
  attrib HV113_09 format=F00148_. label="Father alive(9)";
  attrib HV113_10 format=F00148_. label="Father alive(10)";
  attrib HV113_11 format=F00148_. label="Father alive(11)";
  attrib HV113_12 format=F00148_. label="Father alive(12)";
  attrib HV113_13 format=F00148_. label="Father alive(13)";
  attrib HV113_14 format=F00148_. label="Father alive(14)";
  attrib HV113_15 format=F00148_. label="Father alive(15)";
  attrib HV113_16 format=F00148_. label="Father alive(16)";
  attrib HV113_17 format=F00148_. label="Father alive(17)";
  attrib HV113_18 format=F00148_. label="Father alive(18)";
  attrib HV113_19 format=F00148_. label="Father alive(19)";
  attrib HV113_20 format=F00148_. label="Father alive(20)";
  attrib HV113_21 format=F00148_. label="Father alive(21)";
  attrib HV113_22 format=F00148_. label="Father alive(22)";
  attrib HV113_23 format=F00148_. label="Father alive(23)";
  attrib HV113_24 format=F00148_. label="Father alive(24)";
  attrib HV113_25 format=F00148_. label="Father alive(25)";
  attrib HV113_26 format=F00148_. label="Father alive(26)";
  attrib HV113_27 format=F00148_. label="Father alive(27)";
  attrib HV113_28 format=F00148_. label="Father alive(28)";
  attrib HV113_29 format=F00148_. label="Father alive(29)";
  attrib HV113_30 format=F00148_. label="Father alive(30)";
  attrib HV113_31 format=F00148_. label="Father alive(31)";
  attrib HV113_32 format=F00148_. label="Father alive(32)";
  attrib HV113_33 format=F00148_. label="Father alive(33)";
  attrib HV113_34 format=F00148_. label="Father alive(34)";
  attrib HV113_35 format=F00148_. label="Father alive(35)";
  attrib HV113_36 format=F00148_. label="Father alive(36)";
  attrib HV113_37 format=F00148_. label="Father alive(37)";
  attrib HV113_38 format=F00148_. label="Father alive(38)";
  attrib HV113_39 format=F00148_. label="Father alive(39)";
  attrib HV113_40 format=F00148_. label="Father alive(40)";
  attrib HV113_41 format=F00148_. label="Father alive(41)";
  attrib HV113_42 format=F00148_. label="Father alive(42)";
  attrib HV113_43 format=F00148_. label="Father alive(43)";
  attrib HV113_44 format=F00148_. label="Father alive(44)";
  attrib HV113_45 format=F00148_. label="Father alive(45)";
  attrib HV113_46 format=F00148_. label="Father alive(46)";
  attrib HV113_47 format=F00148_. label="Father alive(47)";
  attrib HV113_48 format=F00148_. label="Father alive(48)";
  attrib HV114_01 format=F00149_. label="Father's line number(1)";
  attrib HV114_02 format=F00149_. label="Father's line number(2)";
  attrib HV114_03 format=F00149_. label="Father's line number(3)";
  attrib HV114_04 format=F00149_. label="Father's line number(4)";
  attrib HV114_05 format=F00149_. label="Father's line number(5)";
  attrib HV114_06 format=F00149_. label="Father's line number(6)";
  attrib HV114_07 format=F00149_. label="Father's line number(7)";
  attrib HV114_08 format=F00149_. label="Father's line number(8)";
  attrib HV114_09 format=F00149_. label="Father's line number(9)";
  attrib HV114_10 format=F00149_. label="Father's line number(10)";
  attrib HV114_11 format=F00149_. label="Father's line number(11)";
  attrib HV114_12 format=F00149_. label="Father's line number(12)";
  attrib HV114_13 format=F00149_. label="Father's line number(13)";
  attrib HV114_14 format=F00149_. label="Father's line number(14)";
  attrib HV114_15 format=F00149_. label="Father's line number(15)";
  attrib HV114_16 format=F00149_. label="Father's line number(16)";
  attrib HV114_17 format=F00149_. label="Father's line number(17)";
  attrib HV114_18 format=F00149_. label="Father's line number(18)";
  attrib HV114_19 format=F00149_. label="Father's line number(19)";
  attrib HV114_20 format=F00149_. label="Father's line number(20)";
  attrib HV114_21 format=F00149_. label="Father's line number(21)";
  attrib HV114_22 format=F00149_. label="Father's line number(22)";
  attrib HV114_23 format=F00149_. label="Father's line number(23)";
  attrib HV114_24 format=F00149_. label="Father's line number(24)";
  attrib HV114_25 format=F00149_. label="Father's line number(25)";
  attrib HV114_26 format=F00149_. label="Father's line number(26)";
  attrib HV114_27 format=F00149_. label="Father's line number(27)";
  attrib HV114_28 format=F00149_. label="Father's line number(28)";
  attrib HV114_29 format=F00149_. label="Father's line number(29)";
  attrib HV114_30 format=F00149_. label="Father's line number(30)";
  attrib HV114_31 format=F00149_. label="Father's line number(31)";
  attrib HV114_32 format=F00149_. label="Father's line number(32)";
  attrib HV114_33 format=F00149_. label="Father's line number(33)";
  attrib HV114_34 format=F00149_. label="Father's line number(34)";
  attrib HV114_35 format=F00149_. label="Father's line number(35)";
  attrib HV114_36 format=F00149_. label="Father's line number(36)";
  attrib HV114_37 format=F00149_. label="Father's line number(37)";
  attrib HV114_38 format=F00149_. label="Father's line number(38)";
  attrib HV114_39 format=F00149_. label="Father's line number(39)";
  attrib HV114_40 format=F00149_. label="Father's line number(40)";
  attrib HV114_41 format=F00149_. label="Father's line number(41)";
  attrib HV114_42 format=F00149_. label="Father's line number(42)";
  attrib HV114_43 format=F00149_. label="Father's line number(43)";
  attrib HV114_44 format=F00149_. label="Father's line number(44)";
  attrib HV114_45 format=F00149_. label="Father's line number(45)";
  attrib HV114_46 format=F00149_. label="Father's line number(46)";
  attrib HV114_47 format=F00149_. label="Father's line number(47)";
  attrib HV114_48 format=F00149_. label="Father's line number(48)";
  attrib HV115_01 format=F00150_. label="Current marital status(1)";
  attrib HV115_02 format=F00150_. label="Current marital status(2)";
  attrib HV115_03 format=F00150_. label="Current marital status(3)";
  attrib HV115_04 format=F00150_. label="Current marital status(4)";
  attrib HV115_05 format=F00150_. label="Current marital status(5)";
  attrib HV115_06 format=F00150_. label="Current marital status(6)";
  attrib HV115_07 format=F00150_. label="Current marital status(7)";
  attrib HV115_08 format=F00150_. label="Current marital status(8)";
  attrib HV115_09 format=F00150_. label="Current marital status(9)";
  attrib HV115_10 format=F00150_. label="Current marital status(10)";
  attrib HV115_11 format=F00150_. label="Current marital status(11)";
  attrib HV115_12 format=F00150_. label="Current marital status(12)";
  attrib HV115_13 format=F00150_. label="Current marital status(13)";
  attrib HV115_14 format=F00150_. label="Current marital status(14)";
  attrib HV115_15 format=F00150_. label="Current marital status(15)";
  attrib HV115_16 format=F00150_. label="Current marital status(16)";
  attrib HV115_17 format=F00150_. label="Current marital status(17)";
  attrib HV115_18 format=F00150_. label="Current marital status(18)";
  attrib HV115_19 format=F00150_. label="Current marital status(19)";
  attrib HV115_20 format=F00150_. label="Current marital status(20)";
  attrib HV115_21 format=F00150_. label="Current marital status(21)";
  attrib HV115_22 format=F00150_. label="Current marital status(22)";
  attrib HV115_23 format=F00150_. label="Current marital status(23)";
  attrib HV115_24 format=F00150_. label="Current marital status(24)";
  attrib HV115_25 format=F00150_. label="Current marital status(25)";
  attrib HV115_26 format=F00150_. label="Current marital status(26)";
  attrib HV115_27 format=F00150_. label="Current marital status(27)";
  attrib HV115_28 format=F00150_. label="Current marital status(28)";
  attrib HV115_29 format=F00150_. label="Current marital status(29)";
  attrib HV115_30 format=F00150_. label="Current marital status(30)";
  attrib HV115_31 format=F00150_. label="Current marital status(31)";
  attrib HV115_32 format=F00150_. label="Current marital status(32)";
  attrib HV115_33 format=F00150_. label="Current marital status(33)";
  attrib HV115_34 format=F00150_. label="Current marital status(34)";
  attrib HV115_35 format=F00150_. label="Current marital status(35)";
  attrib HV115_36 format=F00150_. label="Current marital status(36)";
  attrib HV115_37 format=F00150_. label="Current marital status(37)";
  attrib HV115_38 format=F00150_. label="Current marital status(38)";
  attrib HV115_39 format=F00150_. label="Current marital status(39)";
  attrib HV115_40 format=F00150_. label="Current marital status(40)";
  attrib HV115_41 format=F00150_. label="Current marital status(41)";
  attrib HV115_42 format=F00150_. label="Current marital status(42)";
  attrib HV115_43 format=F00150_. label="Current marital status(43)";
  attrib HV115_44 format=F00150_. label="Current marital status(44)";
  attrib HV115_45 format=F00150_. label="Current marital status(45)";
  attrib HV115_46 format=F00150_. label="Current marital status(46)";
  attrib HV115_47 format=F00150_. label="Current marital status(47)";
  attrib HV115_48 format=F00150_. label="Current marital status(48)";
  attrib HV116_01 format=F00151_. label="Currently, formerly, never married(1)";
  attrib HV116_02 format=F00151_. label="Currently, formerly, never married(2)";
  attrib HV116_03 format=F00151_. label="Currently, formerly, never married(3)";
  attrib HV116_04 format=F00151_. label="Currently, formerly, never married(4)";
  attrib HV116_05 format=F00151_. label="Currently, formerly, never married(5)";
  attrib HV116_06 format=F00151_. label="Currently, formerly, never married(6)";
  attrib HV116_07 format=F00151_. label="Currently, formerly, never married(7)";
  attrib HV116_08 format=F00151_. label="Currently, formerly, never married(8)";
  attrib HV116_09 format=F00151_. label="Currently, formerly, never married(9)";
  attrib HV116_10 format=F00151_. label="Currently, formerly, never married(10)";
  attrib HV116_11 format=F00151_. label="Currently, formerly, never married(11)";
  attrib HV116_12 format=F00151_. label="Currently, formerly, never married(12)";
  attrib HV116_13 format=F00151_. label="Currently, formerly, never married(13)";
  attrib HV116_14 format=F00151_. label="Currently, formerly, never married(14)";
  attrib HV116_15 format=F00151_. label="Currently, formerly, never married(15)";
  attrib HV116_16 format=F00151_. label="Currently, formerly, never married(16)";
  attrib HV116_17 format=F00151_. label="Currently, formerly, never married(17)";
  attrib HV116_18 format=F00151_. label="Currently, formerly, never married(18)";
  attrib HV116_19 format=F00151_. label="Currently, formerly, never married(19)";
  attrib HV116_20 format=F00151_. label="Currently, formerly, never married(20)";
  attrib HV116_21 format=F00151_. label="Currently, formerly, never married(21)";
  attrib HV116_22 format=F00151_. label="Currently, formerly, never married(22)";
  attrib HV116_23 format=F00151_. label="Currently, formerly, never married(23)";
  attrib HV116_24 format=F00151_. label="Currently, formerly, never married(24)";
  attrib HV116_25 format=F00151_. label="Currently, formerly, never married(25)";
  attrib HV116_26 format=F00151_. label="Currently, formerly, never married(26)";
  attrib HV116_27 format=F00151_. label="Currently, formerly, never married(27)";
  attrib HV116_28 format=F00151_. label="Currently, formerly, never married(28)";
  attrib HV116_29 format=F00151_. label="Currently, formerly, never married(29)";
  attrib HV116_30 format=F00151_. label="Currently, formerly, never married(30)";
  attrib HV116_31 format=F00151_. label="Currently, formerly, never married(31)";
  attrib HV116_32 format=F00151_. label="Currently, formerly, never married(32)";
  attrib HV116_33 format=F00151_. label="Currently, formerly, never married(33)";
  attrib HV116_34 format=F00151_. label="Currently, formerly, never married(34)";
  attrib HV116_35 format=F00151_. label="Currently, formerly, never married(35)";
  attrib HV116_36 format=F00151_. label="Currently, formerly, never married(36)";
  attrib HV116_37 format=F00151_. label="Currently, formerly, never married(37)";
  attrib HV116_38 format=F00151_. label="Currently, formerly, never married(38)";
  attrib HV116_39 format=F00151_. label="Currently, formerly, never married(39)";
  attrib HV116_40 format=F00151_. label="Currently, formerly, never married(40)";
  attrib HV116_41 format=F00151_. label="Currently, formerly, never married(41)";
  attrib HV116_42 format=F00151_. label="Currently, formerly, never married(42)";
  attrib HV116_43 format=F00151_. label="Currently, formerly, never married(43)";
  attrib HV116_44 format=F00151_. label="Currently, formerly, never married(44)";
  attrib HV116_45 format=F00151_. label="Currently, formerly, never married(45)";
  attrib HV116_46 format=F00151_. label="Currently, formerly, never married(46)";
  attrib HV116_47 format=F00151_. label="Currently, formerly, never married(47)";
  attrib HV116_48 format=F00151_. label="Currently, formerly, never married(48)";
  attrib HV117_01 format=F00152_. label="Eligibility for female interview(1)";
  attrib HV117_02 format=F00152_. label="Eligibility for female interview(2)";
  attrib HV117_03 format=F00152_. label="Eligibility for female interview(3)";
  attrib HV117_04 format=F00152_. label="Eligibility for female interview(4)";
  attrib HV117_05 format=F00152_. label="Eligibility for female interview(5)";
  attrib HV117_06 format=F00152_. label="Eligibility for female interview(6)";
  attrib HV117_07 format=F00152_. label="Eligibility for female interview(7)";
  attrib HV117_08 format=F00152_. label="Eligibility for female interview(8)";
  attrib HV117_09 format=F00152_. label="Eligibility for female interview(9)";
  attrib HV117_10 format=F00152_. label="Eligibility for female interview(10)";
  attrib HV117_11 format=F00152_. label="Eligibility for female interview(11)";
  attrib HV117_12 format=F00152_. label="Eligibility for female interview(12)";
  attrib HV117_13 format=F00152_. label="Eligibility for female interview(13)";
  attrib HV117_14 format=F00152_. label="Eligibility for female interview(14)";
  attrib HV117_15 format=F00152_. label="Eligibility for female interview(15)";
  attrib HV117_16 format=F00152_. label="Eligibility for female interview(16)";
  attrib HV117_17 format=F00152_. label="Eligibility for female interview(17)";
  attrib HV117_18 format=F00152_. label="Eligibility for female interview(18)";
  attrib HV117_19 format=F00152_. label="Eligibility for female interview(19)";
  attrib HV117_20 format=F00152_. label="Eligibility for female interview(20)";
  attrib HV117_21 format=F00152_. label="Eligibility for female interview(21)";
  attrib HV117_22 format=F00152_. label="Eligibility for female interview(22)";
  attrib HV117_23 format=F00152_. label="Eligibility for female interview(23)";
  attrib HV117_24 format=F00152_. label="Eligibility for female interview(24)";
  attrib HV117_25 format=F00152_. label="Eligibility for female interview(25)";
  attrib HV117_26 format=F00152_. label="Eligibility for female interview(26)";
  attrib HV117_27 format=F00152_. label="Eligibility for female interview(27)";
  attrib HV117_28 format=F00152_. label="Eligibility for female interview(28)";
  attrib HV117_29 format=F00152_. label="Eligibility for female interview(29)";
  attrib HV117_30 format=F00152_. label="Eligibility for female interview(30)";
  attrib HV117_31 format=F00152_. label="Eligibility for female interview(31)";
  attrib HV117_32 format=F00152_. label="Eligibility for female interview(32)";
  attrib HV117_33 format=F00152_. label="Eligibility for female interview(33)";
  attrib HV117_34 format=F00152_. label="Eligibility for female interview(34)";
  attrib HV117_35 format=F00152_. label="Eligibility for female interview(35)";
  attrib HV117_36 format=F00152_. label="Eligibility for female interview(36)";
  attrib HV117_37 format=F00152_. label="Eligibility for female interview(37)";
  attrib HV117_38 format=F00152_. label="Eligibility for female interview(38)";
  attrib HV117_39 format=F00152_. label="Eligibility for female interview(39)";
  attrib HV117_40 format=F00152_. label="Eligibility for female interview(40)";
  attrib HV117_41 format=F00152_. label="Eligibility for female interview(41)";
  attrib HV117_42 format=F00152_. label="Eligibility for female interview(42)";
  attrib HV117_43 format=F00152_. label="Eligibility for female interview(43)";
  attrib HV117_44 format=F00152_. label="Eligibility for female interview(44)";
  attrib HV117_45 format=F00152_. label="Eligibility for female interview(45)";
  attrib HV117_46 format=F00152_. label="Eligibility for female interview(46)";
  attrib HV117_47 format=F00152_. label="Eligibility for female interview(47)";
  attrib HV117_48 format=F00152_. label="Eligibility for female interview(48)";
  attrib HV118_01 format=F00153_. label="Eligibility for male interview(1)";
  attrib HV118_02 format=F00153_. label="Eligibility for male interview(2)";
  attrib HV118_03 format=F00153_. label="Eligibility for male interview(3)";
  attrib HV118_04 format=F00153_. label="Eligibility for male interview(4)";
  attrib HV118_05 format=F00153_. label="Eligibility for male interview(5)";
  attrib HV118_06 format=F00153_. label="Eligibility for male interview(6)";
  attrib HV118_07 format=F00153_. label="Eligibility for male interview(7)";
  attrib HV118_08 format=F00153_. label="Eligibility for male interview(8)";
  attrib HV118_09 format=F00153_. label="Eligibility for male interview(9)";
  attrib HV118_10 format=F00153_. label="Eligibility for male interview(10)";
  attrib HV118_11 format=F00153_. label="Eligibility for male interview(11)";
  attrib HV118_12 format=F00153_. label="Eligibility for male interview(12)";
  attrib HV118_13 format=F00153_. label="Eligibility for male interview(13)";
  attrib HV118_14 format=F00153_. label="Eligibility for male interview(14)";
  attrib HV118_15 format=F00153_. label="Eligibility for male interview(15)";
  attrib HV118_16 format=F00153_. label="Eligibility for male interview(16)";
  attrib HV118_17 format=F00153_. label="Eligibility for male interview(17)";
  attrib HV118_18 format=F00153_. label="Eligibility for male interview(18)";
  attrib HV118_19 format=F00153_. label="Eligibility for male interview(19)";
  attrib HV118_20 format=F00153_. label="Eligibility for male interview(20)";
  attrib HV118_21 format=F00153_. label="Eligibility for male interview(21)";
  attrib HV118_22 format=F00153_. label="Eligibility for male interview(22)";
  attrib HV118_23 format=F00153_. label="Eligibility for male interview(23)";
  attrib HV118_24 format=F00153_. label="Eligibility for male interview(24)";
  attrib HV118_25 format=F00153_. label="Eligibility for male interview(25)";
  attrib HV118_26 format=F00153_. label="Eligibility for male interview(26)";
  attrib HV118_27 format=F00153_. label="Eligibility for male interview(27)";
  attrib HV118_28 format=F00153_. label="Eligibility for male interview(28)";
  attrib HV118_29 format=F00153_. label="Eligibility for male interview(29)";
  attrib HV118_30 format=F00153_. label="Eligibility for male interview(30)";
  attrib HV118_31 format=F00153_. label="Eligibility for male interview(31)";
  attrib HV118_32 format=F00153_. label="Eligibility for male interview(32)";
  attrib HV118_33 format=F00153_. label="Eligibility for male interview(33)";
  attrib HV118_34 format=F00153_. label="Eligibility for male interview(34)";
  attrib HV118_35 format=F00153_. label="Eligibility for male interview(35)";
  attrib HV118_36 format=F00153_. label="Eligibility for male interview(36)";
  attrib HV118_37 format=F00153_. label="Eligibility for male interview(37)";
  attrib HV118_38 format=F00153_. label="Eligibility for male interview(38)";
  attrib HV118_39 format=F00153_. label="Eligibility for male interview(39)";
  attrib HV118_40 format=F00153_. label="Eligibility for male interview(40)";
  attrib HV118_41 format=F00153_. label="Eligibility for male interview(41)";
  attrib HV118_42 format=F00153_. label="Eligibility for male interview(42)";
  attrib HV118_43 format=F00153_. label="Eligibility for male interview(43)";
  attrib HV118_44 format=F00153_. label="Eligibility for male interview(44)";
  attrib HV118_45 format=F00153_. label="Eligibility for male interview(45)";
  attrib HV118_46 format=F00153_. label="Eligibility for male interview(46)";
  attrib HV118_47 format=F00153_. label="Eligibility for male interview(47)";
  attrib HV118_48 format=F00153_. label="Eligibility for male interview(48)";
  attrib HV120_01 format=F00154_. label="Children eligibility for height/weight and hemoglobin(1)";
  attrib HV120_02 format=F00154_. label="Children eligibility for height/weight and hemoglobin(2)";
  attrib HV120_03 format=F00154_. label="Children eligibility for height/weight and hemoglobin(3)";
  attrib HV120_04 format=F00154_. label="Children eligibility for height/weight and hemoglobin(4)";
  attrib HV120_05 format=F00154_. label="Children eligibility for height/weight and hemoglobin(5)";
  attrib HV120_06 format=F00154_. label="Children eligibility for height/weight and hemoglobin(6)";
  attrib HV120_07 format=F00154_. label="Children eligibility for height/weight and hemoglobin(7)";
  attrib HV120_08 format=F00154_. label="Children eligibility for height/weight and hemoglobin(8)";
  attrib HV120_09 format=F00154_. label="Children eligibility for height/weight and hemoglobin(9)";
  attrib HV120_10 format=F00154_. label="Children eligibility for height/weight and hemoglobin(10)";
  attrib HV120_11 format=F00154_. label="Children eligibility for height/weight and hemoglobin(11)";
  attrib HV120_12 format=F00154_. label="Children eligibility for height/weight and hemoglobin(12)";
  attrib HV120_13 format=F00154_. label="Children eligibility for height/weight and hemoglobin(13)";
  attrib HV120_14 format=F00154_. label="Children eligibility for height/weight and hemoglobin(14)";
  attrib HV120_15 format=F00154_. label="Children eligibility for height/weight and hemoglobin(15)";
  attrib HV120_16 format=F00154_. label="Children eligibility for height/weight and hemoglobin(16)";
  attrib HV120_17 format=F00154_. label="Children eligibility for height/weight and hemoglobin(17)";
  attrib HV120_18 format=F00154_. label="Children eligibility for height/weight and hemoglobin(18)";
  attrib HV120_19 format=F00154_. label="Children eligibility for height/weight and hemoglobin(19)";
  attrib HV120_20 format=F00154_. label="Children eligibility for height/weight and hemoglobin(20)";
  attrib HV120_21 format=F00154_. label="Children eligibility for height/weight and hemoglobin(21)";
  attrib HV120_22 format=F00154_. label="Children eligibility for height/weight and hemoglobin(22)";
  attrib HV120_23 format=F00154_. label="Children eligibility for height/weight and hemoglobin(23)";
  attrib HV120_24 format=F00154_. label="Children eligibility for height/weight and hemoglobin(24)";
  attrib HV120_25 format=F00154_. label="Children eligibility for height/weight and hemoglobin(25)";
  attrib HV120_26 format=F00154_. label="Children eligibility for height/weight and hemoglobin(26)";
  attrib HV120_27 format=F00154_. label="Children eligibility for height/weight and hemoglobin(27)";
  attrib HV120_28 format=F00154_. label="Children eligibility for height/weight and hemoglobin(28)";
  attrib HV120_29 format=F00154_. label="Children eligibility for height/weight and hemoglobin(29)";
  attrib HV120_30 format=F00154_. label="Children eligibility for height/weight and hemoglobin(30)";
  attrib HV120_31 format=F00154_. label="Children eligibility for height/weight and hemoglobin(31)";
  attrib HV120_32 format=F00154_. label="Children eligibility for height/weight and hemoglobin(32)";
  attrib HV120_33 format=F00154_. label="Children eligibility for height/weight and hemoglobin(33)";
  attrib HV120_34 format=F00154_. label="Children eligibility for height/weight and hemoglobin(34)";
  attrib HV120_35 format=F00154_. label="Children eligibility for height/weight and hemoglobin(35)";
  attrib HV120_36 format=F00154_. label="Children eligibility for height/weight and hemoglobin(36)";
  attrib HV120_37 format=F00154_. label="Children eligibility for height/weight and hemoglobin(37)";
  attrib HV120_38 format=F00154_. label="Children eligibility for height/weight and hemoglobin(38)";
  attrib HV120_39 format=F00154_. label="Children eligibility for height/weight and hemoglobin(39)";
  attrib HV120_40 format=F00154_. label="Children eligibility for height/weight and hemoglobin(40)";
  attrib HV120_41 format=F00154_. label="Children eligibility for height/weight and hemoglobin(41)";
  attrib HV120_42 format=F00154_. label="Children eligibility for height/weight and hemoglobin(42)";
  attrib HV120_43 format=F00154_. label="Children eligibility for height/weight and hemoglobin(43)";
  attrib HV120_44 format=F00154_. label="Children eligibility for height/weight and hemoglobin(44)";
  attrib HV120_45 format=F00154_. label="Children eligibility for height/weight and hemoglobin(45)";
  attrib HV120_46 format=F00154_. label="Children eligibility for height/weight and hemoglobin(46)";
  attrib HV120_47 format=F00154_. label="Children eligibility for height/weight and hemoglobin(47)";
  attrib HV120_48 format=F00154_. label="Children eligibility for height/weight and hemoglobin(48)";
  attrib HV121_01 format=F00155_. label="Member attended school during current school year(1)";
  attrib HV121_02 format=F00155_. label="Member attended school during current school year(2)";
  attrib HV121_03 format=F00155_. label="Member attended school during current school year(3)";
  attrib HV121_04 format=F00155_. label="Member attended school during current school year(4)";
  attrib HV121_05 format=F00155_. label="Member attended school during current school year(5)";
  attrib HV121_06 format=F00155_. label="Member attended school during current school year(6)";
  attrib HV121_07 format=F00155_. label="Member attended school during current school year(7)";
  attrib HV121_08 format=F00155_. label="Member attended school during current school year(8)";
  attrib HV121_09 format=F00155_. label="Member attended school during current school year(9)";
  attrib HV121_10 format=F00155_. label="Member attended school during current school year(10)";
  attrib HV121_11 format=F00155_. label="Member attended school during current school year(11)";
  attrib HV121_12 format=F00155_. label="Member attended school during current school year(12)";
  attrib HV121_13 format=F00155_. label="Member attended school during current school year(13)";
  attrib HV121_14 format=F00155_. label="Member attended school during current school year(14)";
  attrib HV121_15 format=F00155_. label="Member attended school during current school year(15)";
  attrib HV121_16 format=F00155_. label="Member attended school during current school year(16)";
  attrib HV121_17 format=F00155_. label="Member attended school during current school year(17)";
  attrib HV121_18 format=F00155_. label="Member attended school during current school year(18)";
  attrib HV121_19 format=F00155_. label="Member attended school during current school year(19)";
  attrib HV121_20 format=F00155_. label="Member attended school during current school year(20)";
  attrib HV121_21 format=F00155_. label="Member attended school during current school year(21)";
  attrib HV121_22 format=F00155_. label="Member attended school during current school year(22)";
  attrib HV121_23 format=F00155_. label="Member attended school during current school year(23)";
  attrib HV121_24 format=F00155_. label="Member attended school during current school year(24)";
  attrib HV121_25 format=F00155_. label="Member attended school during current school year(25)";
  attrib HV121_26 format=F00155_. label="Member attended school during current school year(26)";
  attrib HV121_27 format=F00155_. label="Member attended school during current school year(27)";
  attrib HV121_28 format=F00155_. label="Member attended school during current school year(28)";
  attrib HV121_29 format=F00155_. label="Member attended school during current school year(29)";
  attrib HV121_30 format=F00155_. label="Member attended school during current school year(30)";
  attrib HV121_31 format=F00155_. label="Member attended school during current school year(31)";
  attrib HV121_32 format=F00155_. label="Member attended school during current school year(32)";
  attrib HV121_33 format=F00155_. label="Member attended school during current school year(33)";
  attrib HV121_34 format=F00155_. label="Member attended school during current school year(34)";
  attrib HV121_35 format=F00155_. label="Member attended school during current school year(35)";
  attrib HV121_36 format=F00155_. label="Member attended school during current school year(36)";
  attrib HV121_37 format=F00155_. label="Member attended school during current school year(37)";
  attrib HV121_38 format=F00155_. label="Member attended school during current school year(38)";
  attrib HV121_39 format=F00155_. label="Member attended school during current school year(39)";
  attrib HV121_40 format=F00155_. label="Member attended school during current school year(40)";
  attrib HV121_41 format=F00155_. label="Member attended school during current school year(41)";
  attrib HV121_42 format=F00155_. label="Member attended school during current school year(42)";
  attrib HV121_43 format=F00155_. label="Member attended school during current school year(43)";
  attrib HV121_44 format=F00155_. label="Member attended school during current school year(44)";
  attrib HV121_45 format=F00155_. label="Member attended school during current school year(45)";
  attrib HV121_46 format=F00155_. label="Member attended school during current school year(46)";
  attrib HV121_47 format=F00155_. label="Member attended school during current school year(47)";
  attrib HV121_48 format=F00155_. label="Member attended school during current school year(48)";
  attrib HV122_01 format=F00156_. label="Educational level during current school year(1)";
  attrib HV122_02 format=F00156_. label="Educational level during current school year(2)";
  attrib HV122_03 format=F00156_. label="Educational level during current school year(3)";
  attrib HV122_04 format=F00156_. label="Educational level during current school year(4)";
  attrib HV122_05 format=F00156_. label="Educational level during current school year(5)";
  attrib HV122_06 format=F00156_. label="Educational level during current school year(6)";
  attrib HV122_07 format=F00156_. label="Educational level during current school year(7)";
  attrib HV122_08 format=F00156_. label="Educational level during current school year(8)";
  attrib HV122_09 format=F00156_. label="Educational level during current school year(9)";
  attrib HV122_10 format=F00156_. label="Educational level during current school year(10)";
  attrib HV122_11 format=F00156_. label="Educational level during current school year(11)";
  attrib HV122_12 format=F00156_. label="Educational level during current school year(12)";
  attrib HV122_13 format=F00156_. label="Educational level during current school year(13)";
  attrib HV122_14 format=F00156_. label="Educational level during current school year(14)";
  attrib HV122_15 format=F00156_. label="Educational level during current school year(15)";
  attrib HV122_16 format=F00156_. label="Educational level during current school year(16)";
  attrib HV122_17 format=F00156_. label="Educational level during current school year(17)";
  attrib HV122_18 format=F00156_. label="Educational level during current school year(18)";
  attrib HV122_19 format=F00156_. label="Educational level during current school year(19)";
  attrib HV122_20 format=F00156_. label="Educational level during current school year(20)";
  attrib HV122_21 format=F00156_. label="Educational level during current school year(21)";
  attrib HV122_22 format=F00156_. label="Educational level during current school year(22)";
  attrib HV122_23 format=F00156_. label="Educational level during current school year(23)";
  attrib HV122_24 format=F00156_. label="Educational level during current school year(24)";
  attrib HV122_25 format=F00156_. label="Educational level during current school year(25)";
  attrib HV122_26 format=F00156_. label="Educational level during current school year(26)";
  attrib HV122_27 format=F00156_. label="Educational level during current school year(27)";
  attrib HV122_28 format=F00156_. label="Educational level during current school year(28)";
  attrib HV122_29 format=F00156_. label="Educational level during current school year(29)";
  attrib HV122_30 format=F00156_. label="Educational level during current school year(30)";
  attrib HV122_31 format=F00156_. label="Educational level during current school year(31)";
  attrib HV122_32 format=F00156_. label="Educational level during current school year(32)";
  attrib HV122_33 format=F00156_. label="Educational level during current school year(33)";
  attrib HV122_34 format=F00156_. label="Educational level during current school year(34)";
  attrib HV122_35 format=F00156_. label="Educational level during current school year(35)";
  attrib HV122_36 format=F00156_. label="Educational level during current school year(36)";
  attrib HV122_37 format=F00156_. label="Educational level during current school year(37)";
  attrib HV122_38 format=F00156_. label="Educational level during current school year(38)";
  attrib HV122_39 format=F00156_. label="Educational level during current school year(39)";
  attrib HV122_40 format=F00156_. label="Educational level during current school year(40)";
  attrib HV122_41 format=F00156_. label="Educational level during current school year(41)";
  attrib HV122_42 format=F00156_. label="Educational level during current school year(42)";
  attrib HV122_43 format=F00156_. label="Educational level during current school year(43)";
  attrib HV122_44 format=F00156_. label="Educational level during current school year(44)";
  attrib HV122_45 format=F00156_. label="Educational level during current school year(45)";
  attrib HV122_46 format=F00156_. label="Educational level during current school year(46)";
  attrib HV122_47 format=F00156_. label="Educational level during current school year(47)";
  attrib HV122_48 format=F00156_. label="Educational level during current school year(48)";
  attrib HV123_01 format=F00157_. label="Grade of education during current school year(1)";
  attrib HV123_02 format=F00157_. label="Grade of education during current school year(2)";
  attrib HV123_03 format=F00157_. label="Grade of education during current school year(3)";
  attrib HV123_04 format=F00157_. label="Grade of education during current school year(4)";
  attrib HV123_05 format=F00157_. label="Grade of education during current school year(5)";
  attrib HV123_06 format=F00157_. label="Grade of education during current school year(6)";
  attrib HV123_07 format=F00157_. label="Grade of education during current school year(7)";
  attrib HV123_08 format=F00157_. label="Grade of education during current school year(8)";
  attrib HV123_09 format=F00157_. label="Grade of education during current school year(9)";
  attrib HV123_10 format=F00157_. label="Grade of education during current school year(10)";
  attrib HV123_11 format=F00157_. label="Grade of education during current school year(11)";
  attrib HV123_12 format=F00157_. label="Grade of education during current school year(12)";
  attrib HV123_13 format=F00157_. label="Grade of education during current school year(13)";
  attrib HV123_14 format=F00157_. label="Grade of education during current school year(14)";
  attrib HV123_15 format=F00157_. label="Grade of education during current school year(15)";
  attrib HV123_16 format=F00157_. label="Grade of education during current school year(16)";
  attrib HV123_17 format=F00157_. label="Grade of education during current school year(17)";
  attrib HV123_18 format=F00157_. label="Grade of education during current school year(18)";
  attrib HV123_19 format=F00157_. label="Grade of education during current school year(19)";
  attrib HV123_20 format=F00157_. label="Grade of education during current school year(20)";
  attrib HV123_21 format=F00157_. label="Grade of education during current school year(21)";
  attrib HV123_22 format=F00157_. label="Grade of education during current school year(22)";
  attrib HV123_23 format=F00157_. label="Grade of education during current school year(23)";
  attrib HV123_24 format=F00157_. label="Grade of education during current school year(24)";
  attrib HV123_25 format=F00157_. label="Grade of education during current school year(25)";
  attrib HV123_26 format=F00157_. label="Grade of education during current school year(26)";
  attrib HV123_27 format=F00157_. label="Grade of education during current school year(27)";
  attrib HV123_28 format=F00157_. label="Grade of education during current school year(28)";
  attrib HV123_29 format=F00157_. label="Grade of education during current school year(29)";
  attrib HV123_30 format=F00157_. label="Grade of education during current school year(30)";
  attrib HV123_31 format=F00157_. label="Grade of education during current school year(31)";
  attrib HV123_32 format=F00157_. label="Grade of education during current school year(32)";
  attrib HV123_33 format=F00157_. label="Grade of education during current school year(33)";
  attrib HV123_34 format=F00157_. label="Grade of education during current school year(34)";
  attrib HV123_35 format=F00157_. label="Grade of education during current school year(35)";
  attrib HV123_36 format=F00157_. label="Grade of education during current school year(36)";
  attrib HV123_37 format=F00157_. label="Grade of education during current school year(37)";
  attrib HV123_38 format=F00157_. label="Grade of education during current school year(38)";
  attrib HV123_39 format=F00157_. label="Grade of education during current school year(39)";
  attrib HV123_40 format=F00157_. label="Grade of education during current school year(40)";
  attrib HV123_41 format=F00157_. label="Grade of education during current school year(41)";
  attrib HV123_42 format=F00157_. label="Grade of education during current school year(42)";
  attrib HV123_43 format=F00157_. label="Grade of education during current school year(43)";
  attrib HV123_44 format=F00157_. label="Grade of education during current school year(44)";
  attrib HV123_45 format=F00157_. label="Grade of education during current school year(45)";
  attrib HV123_46 format=F00157_. label="Grade of education during current school year(46)";
  attrib HV123_47 format=F00157_. label="Grade of education during current school year(47)";
  attrib HV123_48 format=F00157_. label="Grade of education during current school year(48)";
  attrib HV124_01 format=F00158_. label="Education in single years - current school year(1)";
  attrib HV124_02 format=F00158_. label="Education in single years - current school year(2)";
  attrib HV124_03 format=F00158_. label="Education in single years - current school year(3)";
  attrib HV124_04 format=F00158_. label="Education in single years - current school year(4)";
  attrib HV124_05 format=F00158_. label="Education in single years - current school year(5)";
  attrib HV124_06 format=F00158_. label="Education in single years - current school year(6)";
  attrib HV124_07 format=F00158_. label="Education in single years - current school year(7)";
  attrib HV124_08 format=F00158_. label="Education in single years - current school year(8)";
  attrib HV124_09 format=F00158_. label="Education in single years - current school year(9)";
  attrib HV124_10 format=F00158_. label="Education in single years - current school year(10)";
  attrib HV124_11 format=F00158_. label="Education in single years - current school year(11)";
  attrib HV124_12 format=F00158_. label="Education in single years - current school year(12)";
  attrib HV124_13 format=F00158_. label="Education in single years - current school year(13)";
  attrib HV124_14 format=F00158_. label="Education in single years - current school year(14)";
  attrib HV124_15 format=F00158_. label="Education in single years - current school year(15)";
  attrib HV124_16 format=F00158_. label="Education in single years - current school year(16)";
  attrib HV124_17 format=F00158_. label="Education in single years - current school year(17)";
  attrib HV124_18 format=F00158_. label="Education in single years - current school year(18)";
  attrib HV124_19 format=F00158_. label="Education in single years - current school year(19)";
  attrib HV124_20 format=F00158_. label="Education in single years - current school year(20)";
  attrib HV124_21 format=F00158_. label="Education in single years - current school year(21)";
  attrib HV124_22 format=F00158_. label="Education in single years - current school year(22)";
  attrib HV124_23 format=F00158_. label="Education in single years - current school year(23)";
  attrib HV124_24 format=F00158_. label="Education in single years - current school year(24)";
  attrib HV124_25 format=F00158_. label="Education in single years - current school year(25)";
  attrib HV124_26 format=F00158_. label="Education in single years - current school year(26)";
  attrib HV124_27 format=F00158_. label="Education in single years - current school year(27)";
  attrib HV124_28 format=F00158_. label="Education in single years - current school year(28)";
  attrib HV124_29 format=F00158_. label="Education in single years - current school year(29)";
  attrib HV124_30 format=F00158_. label="Education in single years - current school year(30)";
  attrib HV124_31 format=F00158_. label="Education in single years - current school year(31)";
  attrib HV124_32 format=F00158_. label="Education in single years - current school year(32)";
  attrib HV124_33 format=F00158_. label="Education in single years - current school year(33)";
  attrib HV124_34 format=F00158_. label="Education in single years - current school year(34)";
  attrib HV124_35 format=F00158_. label="Education in single years - current school year(35)";
  attrib HV124_36 format=F00158_. label="Education in single years - current school year(36)";
  attrib HV124_37 format=F00158_. label="Education in single years - current school year(37)";
  attrib HV124_38 format=F00158_. label="Education in single years - current school year(38)";
  attrib HV124_39 format=F00158_. label="Education in single years - current school year(39)";
  attrib HV124_40 format=F00158_. label="Education in single years - current school year(40)";
  attrib HV124_41 format=F00158_. label="Education in single years - current school year(41)";
  attrib HV124_42 format=F00158_. label="Education in single years - current school year(42)";
  attrib HV124_43 format=F00158_. label="Education in single years - current school year(43)";
  attrib HV124_44 format=F00158_. label="Education in single years - current school year(44)";
  attrib HV124_45 format=F00158_. label="Education in single years - current school year(45)";
  attrib HV124_46 format=F00158_. label="Education in single years - current school year(46)";
  attrib HV124_47 format=F00158_. label="Education in single years - current school year(47)";
  attrib HV124_48 format=F00158_. label="Education in single years - current school year(48)";
  attrib HV125_01 format=F00159_. label="NA - Member attended school during previous school year(1)";
  attrib HV125_02 format=F00159_. label="NA - Member attended school during previous school year(2)";
  attrib HV125_03 format=F00159_. label="NA - Member attended school during previous school year(3)";
  attrib HV125_04 format=F00159_. label="NA - Member attended school during previous school year(4)";
  attrib HV125_05 format=F00159_. label="NA - Member attended school during previous school year(5)";
  attrib HV125_06 format=F00159_. label="NA - Member attended school during previous school year(6)";
  attrib HV125_07 format=F00159_. label="NA - Member attended school during previous school year(7)";
  attrib HV125_08 format=F00159_. label="NA - Member attended school during previous school year(8)";
  attrib HV125_09 format=F00159_. label="NA - Member attended school during previous school year(9)";
  attrib HV125_10 format=F00159_. label="NA - Member attended school during previous school year(10)";
  attrib HV125_11 format=F00159_. label="NA - Member attended school during previous school year(11)";
  attrib HV125_12 format=F00159_. label="NA - Member attended school during previous school year(12)";
  attrib HV125_13 format=F00159_. label="NA - Member attended school during previous school year(13)";
  attrib HV125_14 format=F00159_. label="NA - Member attended school during previous school year(14)";
  attrib HV125_15 format=F00159_. label="NA - Member attended school during previous school year(15)";
  attrib HV125_16 format=F00159_. label="NA - Member attended school during previous school year(16)";
  attrib HV125_17 format=F00159_. label="NA - Member attended school during previous school year(17)";
  attrib HV125_18 format=F00159_. label="NA - Member attended school during previous school year(18)";
  attrib HV125_19 format=F00159_. label="NA - Member attended school during previous school year(19)";
  attrib HV125_20 format=F00159_. label="NA - Member attended school during previous school year(20)";
  attrib HV125_21 format=F00159_. label="NA - Member attended school during previous school year(21)";
  attrib HV125_22 format=F00159_. label="NA - Member attended school during previous school year(22)";
  attrib HV125_23 format=F00159_. label="NA - Member attended school during previous school year(23)";
  attrib HV125_24 format=F00159_. label="NA - Member attended school during previous school year(24)";
  attrib HV125_25 format=F00159_. label="NA - Member attended school during previous school year(25)";
  attrib HV125_26 format=F00159_. label="NA - Member attended school during previous school year(26)";
  attrib HV125_27 format=F00159_. label="NA - Member attended school during previous school year(27)";
  attrib HV125_28 format=F00159_. label="NA - Member attended school during previous school year(28)";
  attrib HV125_29 format=F00159_. label="NA - Member attended school during previous school year(29)";
  attrib HV125_30 format=F00159_. label="NA - Member attended school during previous school year(30)";
  attrib HV125_31 format=F00159_. label="NA - Member attended school during previous school year(31)";
  attrib HV125_32 format=F00159_. label="NA - Member attended school during previous school year(32)";
  attrib HV125_33 format=F00159_. label="NA - Member attended school during previous school year(33)";
  attrib HV125_34 format=F00159_. label="NA - Member attended school during previous school year(34)";
  attrib HV125_35 format=F00159_. label="NA - Member attended school during previous school year(35)";
  attrib HV125_36 format=F00159_. label="NA - Member attended school during previous school year(36)";
  attrib HV125_37 format=F00159_. label="NA - Member attended school during previous school year(37)";
  attrib HV125_38 format=F00159_. label="NA - Member attended school during previous school year(38)";
  attrib HV125_39 format=F00159_. label="NA - Member attended school during previous school year(39)";
  attrib HV125_40 format=F00159_. label="NA - Member attended school during previous school year(40)";
  attrib HV125_41 format=F00159_. label="NA - Member attended school during previous school year(41)";
  attrib HV125_42 format=F00159_. label="NA - Member attended school during previous school year(42)";
  attrib HV125_43 format=F00159_. label="NA - Member attended school during previous school year(43)";
  attrib HV125_44 format=F00159_. label="NA - Member attended school during previous school year(44)";
  attrib HV125_45 format=F00159_. label="NA - Member attended school during previous school year(45)";
  attrib HV125_46 format=F00159_. label="NA - Member attended school during previous school year(46)";
  attrib HV125_47 format=F00159_. label="NA - Member attended school during previous school year(47)";
  attrib HV125_48 format=F00159_. label="NA - Member attended school during previous school year(48)";
  attrib HV126_01 format=F00160_. label="NA - Educational level during previous school year(1)";
  attrib HV126_02 format=F00160_. label="NA - Educational level during previous school year(2)";
  attrib HV126_03 format=F00160_. label="NA - Educational level during previous school year(3)";
  attrib HV126_04 format=F00160_. label="NA - Educational level during previous school year(4)";
  attrib HV126_05 format=F00160_. label="NA - Educational level during previous school year(5)";
  attrib HV126_06 format=F00160_. label="NA - Educational level during previous school year(6)";
  attrib HV126_07 format=F00160_. label="NA - Educational level during previous school year(7)";
  attrib HV126_08 format=F00160_. label="NA - Educational level during previous school year(8)";
  attrib HV126_09 format=F00160_. label="NA - Educational level during previous school year(9)";
  attrib HV126_10 format=F00160_. label="NA - Educational level during previous school year(10)";
  attrib HV126_11 format=F00160_. label="NA - Educational level during previous school year(11)";
  attrib HV126_12 format=F00160_. label="NA - Educational level during previous school year(12)";
  attrib HV126_13 format=F00160_. label="NA - Educational level during previous school year(13)";
  attrib HV126_14 format=F00160_. label="NA - Educational level during previous school year(14)";
  attrib HV126_15 format=F00160_. label="NA - Educational level during previous school year(15)";
  attrib HV126_16 format=F00160_. label="NA - Educational level during previous school year(16)";
  attrib HV126_17 format=F00160_. label="NA - Educational level during previous school year(17)";
  attrib HV126_18 format=F00160_. label="NA - Educational level during previous school year(18)";
  attrib HV126_19 format=F00160_. label="NA - Educational level during previous school year(19)";
  attrib HV126_20 format=F00160_. label="NA - Educational level during previous school year(20)";
  attrib HV126_21 format=F00160_. label="NA - Educational level during previous school year(21)";
  attrib HV126_22 format=F00160_. label="NA - Educational level during previous school year(22)";
  attrib HV126_23 format=F00160_. label="NA - Educational level during previous school year(23)";
  attrib HV126_24 format=F00160_. label="NA - Educational level during previous school year(24)";
  attrib HV126_25 format=F00160_. label="NA - Educational level during previous school year(25)";
  attrib HV126_26 format=F00160_. label="NA - Educational level during previous school year(26)";
  attrib HV126_27 format=F00160_. label="NA - Educational level during previous school year(27)";
  attrib HV126_28 format=F00160_. label="NA - Educational level during previous school year(28)";
  attrib HV126_29 format=F00160_. label="NA - Educational level during previous school year(29)";
  attrib HV126_30 format=F00160_. label="NA - Educational level during previous school year(30)";
  attrib HV126_31 format=F00160_. label="NA - Educational level during previous school year(31)";
  attrib HV126_32 format=F00160_. label="NA - Educational level during previous school year(32)";
  attrib HV126_33 format=F00160_. label="NA - Educational level during previous school year(33)";
  attrib HV126_34 format=F00160_. label="NA - Educational level during previous school year(34)";
  attrib HV126_35 format=F00160_. label="NA - Educational level during previous school year(35)";
  attrib HV126_36 format=F00160_. label="NA - Educational level during previous school year(36)";
  attrib HV126_37 format=F00160_. label="NA - Educational level during previous school year(37)";
  attrib HV126_38 format=F00160_. label="NA - Educational level during previous school year(38)";
  attrib HV126_39 format=F00160_. label="NA - Educational level during previous school year(39)";
  attrib HV126_40 format=F00160_. label="NA - Educational level during previous school year(40)";
  attrib HV126_41 format=F00160_. label="NA - Educational level during previous school year(41)";
  attrib HV126_42 format=F00160_. label="NA - Educational level during previous school year(42)";
  attrib HV126_43 format=F00160_. label="NA - Educational level during previous school year(43)";
  attrib HV126_44 format=F00160_. label="NA - Educational level during previous school year(44)";
  attrib HV126_45 format=F00160_. label="NA - Educational level during previous school year(45)";
  attrib HV126_46 format=F00160_. label="NA - Educational level during previous school year(46)";
  attrib HV126_47 format=F00160_. label="NA - Educational level during previous school year(47)";
  attrib HV126_48 format=F00160_. label="NA - Educational level during previous school year(48)";
  attrib HV127_01 format=F00161_. label="NA - Grade of education during previous school year(1)";
  attrib HV127_02 format=F00161_. label="NA - Grade of education during previous school year(2)";
  attrib HV127_03 format=F00161_. label="NA - Grade of education during previous school year(3)";
  attrib HV127_04 format=F00161_. label="NA - Grade of education during previous school year(4)";
  attrib HV127_05 format=F00161_. label="NA - Grade of education during previous school year(5)";
  attrib HV127_06 format=F00161_. label="NA - Grade of education during previous school year(6)";
  attrib HV127_07 format=F00161_. label="NA - Grade of education during previous school year(7)";
  attrib HV127_08 format=F00161_. label="NA - Grade of education during previous school year(8)";
  attrib HV127_09 format=F00161_. label="NA - Grade of education during previous school year(9)";
  attrib HV127_10 format=F00161_. label="NA - Grade of education during previous school year(10)";
  attrib HV127_11 format=F00161_. label="NA - Grade of education during previous school year(11)";
  attrib HV127_12 format=F00161_. label="NA - Grade of education during previous school year(12)";
  attrib HV127_13 format=F00161_. label="NA - Grade of education during previous school year(13)";
  attrib HV127_14 format=F00161_. label="NA - Grade of education during previous school year(14)";
  attrib HV127_15 format=F00161_. label="NA - Grade of education during previous school year(15)";
  attrib HV127_16 format=F00161_. label="NA - Grade of education during previous school year(16)";
  attrib HV127_17 format=F00161_. label="NA - Grade of education during previous school year(17)";
  attrib HV127_18 format=F00161_. label="NA - Grade of education during previous school year(18)";
  attrib HV127_19 format=F00161_. label="NA - Grade of education during previous school year(19)";
  attrib HV127_20 format=F00161_. label="NA - Grade of education during previous school year(20)";
  attrib HV127_21 format=F00161_. label="NA - Grade of education during previous school year(21)";
  attrib HV127_22 format=F00161_. label="NA - Grade of education during previous school year(22)";
  attrib HV127_23 format=F00161_. label="NA - Grade of education during previous school year(23)";
  attrib HV127_24 format=F00161_. label="NA - Grade of education during previous school year(24)";
  attrib HV127_25 format=F00161_. label="NA - Grade of education during previous school year(25)";
  attrib HV127_26 format=F00161_. label="NA - Grade of education during previous school year(26)";
  attrib HV127_27 format=F00161_. label="NA - Grade of education during previous school year(27)";
  attrib HV127_28 format=F00161_. label="NA - Grade of education during previous school year(28)";
  attrib HV127_29 format=F00161_. label="NA - Grade of education during previous school year(29)";
  attrib HV127_30 format=F00161_. label="NA - Grade of education during previous school year(30)";
  attrib HV127_31 format=F00161_. label="NA - Grade of education during previous school year(31)";
  attrib HV127_32 format=F00161_. label="NA - Grade of education during previous school year(32)";
  attrib HV127_33 format=F00161_. label="NA - Grade of education during previous school year(33)";
  attrib HV127_34 format=F00161_. label="NA - Grade of education during previous school year(34)";
  attrib HV127_35 format=F00161_. label="NA - Grade of education during previous school year(35)";
  attrib HV127_36 format=F00161_. label="NA - Grade of education during previous school year(36)";
  attrib HV127_37 format=F00161_. label="NA - Grade of education during previous school year(37)";
  attrib HV127_38 format=F00161_. label="NA - Grade of education during previous school year(38)";
  attrib HV127_39 format=F00161_. label="NA - Grade of education during previous school year(39)";
  attrib HV127_40 format=F00161_. label="NA - Grade of education during previous school year(40)";
  attrib HV127_41 format=F00161_. label="NA - Grade of education during previous school year(41)";
  attrib HV127_42 format=F00161_. label="NA - Grade of education during previous school year(42)";
  attrib HV127_43 format=F00161_. label="NA - Grade of education during previous school year(43)";
  attrib HV127_44 format=F00161_. label="NA - Grade of education during previous school year(44)";
  attrib HV127_45 format=F00161_. label="NA - Grade of education during previous school year(45)";
  attrib HV127_46 format=F00161_. label="NA - Grade of education during previous school year(46)";
  attrib HV127_47 format=F00161_. label="NA - Grade of education during previous school year(47)";
  attrib HV127_48 format=F00161_. label="NA - Grade of education during previous school year(48)";
  attrib HV128_01 format=F00162_. label="NA - Education in single years - previous school year(1)";
  attrib HV128_02 format=F00162_. label="NA - Education in single years - previous school year(2)";
  attrib HV128_03 format=F00162_. label="NA - Education in single years - previous school year(3)";
  attrib HV128_04 format=F00162_. label="NA - Education in single years - previous school year(4)";
  attrib HV128_05 format=F00162_. label="NA - Education in single years - previous school year(5)";
  attrib HV128_06 format=F00162_. label="NA - Education in single years - previous school year(6)";
  attrib HV128_07 format=F00162_. label="NA - Education in single years - previous school year(7)";
  attrib HV128_08 format=F00162_. label="NA - Education in single years - previous school year(8)";
  attrib HV128_09 format=F00162_. label="NA - Education in single years - previous school year(9)";
  attrib HV128_10 format=F00162_. label="NA - Education in single years - previous school year(10)";
  attrib HV128_11 format=F00162_. label="NA - Education in single years - previous school year(11)";
  attrib HV128_12 format=F00162_. label="NA - Education in single years - previous school year(12)";
  attrib HV128_13 format=F00162_. label="NA - Education in single years - previous school year(13)";
  attrib HV128_14 format=F00162_. label="NA - Education in single years - previous school year(14)";
  attrib HV128_15 format=F00162_. label="NA - Education in single years - previous school year(15)";
  attrib HV128_16 format=F00162_. label="NA - Education in single years - previous school year(16)";
  attrib HV128_17 format=F00162_. label="NA - Education in single years - previous school year(17)";
  attrib HV128_18 format=F00162_. label="NA - Education in single years - previous school year(18)";
  attrib HV128_19 format=F00162_. label="NA - Education in single years - previous school year(19)";
  attrib HV128_20 format=F00162_. label="NA - Education in single years - previous school year(20)";
  attrib HV128_21 format=F00162_. label="NA - Education in single years - previous school year(21)";
  attrib HV128_22 format=F00162_. label="NA - Education in single years - previous school year(22)";
  attrib HV128_23 format=F00162_. label="NA - Education in single years - previous school year(23)";
  attrib HV128_24 format=F00162_. label="NA - Education in single years - previous school year(24)";
  attrib HV128_25 format=F00162_. label="NA - Education in single years - previous school year(25)";
  attrib HV128_26 format=F00162_. label="NA - Education in single years - previous school year(26)";
  attrib HV128_27 format=F00162_. label="NA - Education in single years - previous school year(27)";
  attrib HV128_28 format=F00162_. label="NA - Education in single years - previous school year(28)";
  attrib HV128_29 format=F00162_. label="NA - Education in single years - previous school year(29)";
  attrib HV128_30 format=F00162_. label="NA - Education in single years - previous school year(30)";
  attrib HV128_31 format=F00162_. label="NA - Education in single years - previous school year(31)";
  attrib HV128_32 format=F00162_. label="NA - Education in single years - previous school year(32)";
  attrib HV128_33 format=F00162_. label="NA - Education in single years - previous school year(33)";
  attrib HV128_34 format=F00162_. label="NA - Education in single years - previous school year(34)";
  attrib HV128_35 format=F00162_. label="NA - Education in single years - previous school year(35)";
  attrib HV128_36 format=F00162_. label="NA - Education in single years - previous school year(36)";
  attrib HV128_37 format=F00162_. label="NA - Education in single years - previous school year(37)";
  attrib HV128_38 format=F00162_. label="NA - Education in single years - previous school year(38)";
  attrib HV128_39 format=F00162_. label="NA - Education in single years - previous school year(39)";
  attrib HV128_40 format=F00162_. label="NA - Education in single years - previous school year(40)";
  attrib HV128_41 format=F00162_. label="NA - Education in single years - previous school year(41)";
  attrib HV128_42 format=F00162_. label="NA - Education in single years - previous school year(42)";
  attrib HV128_43 format=F00162_. label="NA - Education in single years - previous school year(43)";
  attrib HV128_44 format=F00162_. label="NA - Education in single years - previous school year(44)";
  attrib HV128_45 format=F00162_. label="NA - Education in single years - previous school year(45)";
  attrib HV128_46 format=F00162_. label="NA - Education in single years - previous school year(46)";
  attrib HV128_47 format=F00162_. label="NA - Education in single years - previous school year(47)";
  attrib HV128_48 format=F00162_. label="NA - Education in single years - previous school year(48)";
  attrib HV129_01 format=F00163_. label="NA - School attendance status(1)";
  attrib HV129_02 format=F00163_. label="NA - School attendance status(2)";
  attrib HV129_03 format=F00163_. label="NA - School attendance status(3)";
  attrib HV129_04 format=F00163_. label="NA - School attendance status(4)";
  attrib HV129_05 format=F00163_. label="NA - School attendance status(5)";
  attrib HV129_06 format=F00163_. label="NA - School attendance status(6)";
  attrib HV129_07 format=F00163_. label="NA - School attendance status(7)";
  attrib HV129_08 format=F00163_. label="NA - School attendance status(8)";
  attrib HV129_09 format=F00163_. label="NA - School attendance status(9)";
  attrib HV129_10 format=F00163_. label="NA - School attendance status(10)";
  attrib HV129_11 format=F00163_. label="NA - School attendance status(11)";
  attrib HV129_12 format=F00163_. label="NA - School attendance status(12)";
  attrib HV129_13 format=F00163_. label="NA - School attendance status(13)";
  attrib HV129_14 format=F00163_. label="NA - School attendance status(14)";
  attrib HV129_15 format=F00163_. label="NA - School attendance status(15)";
  attrib HV129_16 format=F00163_. label="NA - School attendance status(16)";
  attrib HV129_17 format=F00163_. label="NA - School attendance status(17)";
  attrib HV129_18 format=F00163_. label="NA - School attendance status(18)";
  attrib HV129_19 format=F00163_. label="NA - School attendance status(19)";
  attrib HV129_20 format=F00163_. label="NA - School attendance status(20)";
  attrib HV129_21 format=F00163_. label="NA - School attendance status(21)";
  attrib HV129_22 format=F00163_. label="NA - School attendance status(22)";
  attrib HV129_23 format=F00163_. label="NA - School attendance status(23)";
  attrib HV129_24 format=F00163_. label="NA - School attendance status(24)";
  attrib HV129_25 format=F00163_. label="NA - School attendance status(25)";
  attrib HV129_26 format=F00163_. label="NA - School attendance status(26)";
  attrib HV129_27 format=F00163_. label="NA - School attendance status(27)";
  attrib HV129_28 format=F00163_. label="NA - School attendance status(28)";
  attrib HV129_29 format=F00163_. label="NA - School attendance status(29)";
  attrib HV129_30 format=F00163_. label="NA - School attendance status(30)";
  attrib HV129_31 format=F00163_. label="NA - School attendance status(31)";
  attrib HV129_32 format=F00163_. label="NA - School attendance status(32)";
  attrib HV129_33 format=F00163_. label="NA - School attendance status(33)";
  attrib HV129_34 format=F00163_. label="NA - School attendance status(34)";
  attrib HV129_35 format=F00163_. label="NA - School attendance status(35)";
  attrib HV129_36 format=F00163_. label="NA - School attendance status(36)";
  attrib HV129_37 format=F00163_. label="NA - School attendance status(37)";
  attrib HV129_38 format=F00163_. label="NA - School attendance status(38)";
  attrib HV129_39 format=F00163_. label="NA - School attendance status(39)";
  attrib HV129_40 format=F00163_. label="NA - School attendance status(40)";
  attrib HV129_41 format=F00163_. label="NA - School attendance status(41)";
  attrib HV129_42 format=F00163_. label="NA - School attendance status(42)";
  attrib HV129_43 format=F00163_. label="NA - School attendance status(43)";
  attrib HV129_44 format=F00163_. label="NA - School attendance status(44)";
  attrib HV129_45 format=F00163_. label="NA - School attendance status(45)";
  attrib HV129_46 format=F00163_. label="NA - School attendance status(46)";
  attrib HV129_47 format=F00163_. label="NA - School attendance status(47)";
  attrib HV129_48 format=F00163_. label="NA - School attendance status(48)";
  attrib HV140_01 format=F00164_. label="Member has a birth certificate(1)";
  attrib HV140_02 format=F00164_. label="Member has a birth certificate(2)";
  attrib HV140_03 format=F00164_. label="Member has a birth certificate(3)";
  attrib HV140_04 format=F00164_. label="Member has a birth certificate(4)";
  attrib HV140_05 format=F00164_. label="Member has a birth certificate(5)";
  attrib HV140_06 format=F00164_. label="Member has a birth certificate(6)";
  attrib HV140_07 format=F00164_. label="Member has a birth certificate(7)";
  attrib HV140_08 format=F00164_. label="Member has a birth certificate(8)";
  attrib HV140_09 format=F00164_. label="Member has a birth certificate(9)";
  attrib HV140_10 format=F00164_. label="Member has a birth certificate(10)";
  attrib HV140_11 format=F00164_. label="Member has a birth certificate(11)";
  attrib HV140_12 format=F00164_. label="Member has a birth certificate(12)";
  attrib HV140_13 format=F00164_. label="Member has a birth certificate(13)";
  attrib HV140_14 format=F00164_. label="Member has a birth certificate(14)";
  attrib HV140_15 format=F00164_. label="Member has a birth certificate(15)";
  attrib HV140_16 format=F00164_. label="Member has a birth certificate(16)";
  attrib HV140_17 format=F00164_. label="Member has a birth certificate(17)";
  attrib HV140_18 format=F00164_. label="Member has a birth certificate(18)";
  attrib HV140_19 format=F00164_. label="Member has a birth certificate(19)";
  attrib HV140_20 format=F00164_. label="Member has a birth certificate(20)";
  attrib HV140_21 format=F00164_. label="Member has a birth certificate(21)";
  attrib HV140_22 format=F00164_. label="Member has a birth certificate(22)";
  attrib HV140_23 format=F00164_. label="Member has a birth certificate(23)";
  attrib HV140_24 format=F00164_. label="Member has a birth certificate(24)";
  attrib HV140_25 format=F00164_. label="Member has a birth certificate(25)";
  attrib HV140_26 format=F00164_. label="Member has a birth certificate(26)";
  attrib HV140_27 format=F00164_. label="Member has a birth certificate(27)";
  attrib HV140_28 format=F00164_. label="Member has a birth certificate(28)";
  attrib HV140_29 format=F00164_. label="Member has a birth certificate(29)";
  attrib HV140_30 format=F00164_. label="Member has a birth certificate(30)";
  attrib HV140_31 format=F00164_. label="Member has a birth certificate(31)";
  attrib HV140_32 format=F00164_. label="Member has a birth certificate(32)";
  attrib HV140_33 format=F00164_. label="Member has a birth certificate(33)";
  attrib HV140_34 format=F00164_. label="Member has a birth certificate(34)";
  attrib HV140_35 format=F00164_. label="Member has a birth certificate(35)";
  attrib HV140_36 format=F00164_. label="Member has a birth certificate(36)";
  attrib HV140_37 format=F00164_. label="Member has a birth certificate(37)";
  attrib HV140_38 format=F00164_. label="Member has a birth certificate(38)";
  attrib HV140_39 format=F00164_. label="Member has a birth certificate(39)";
  attrib HV140_40 format=F00164_. label="Member has a birth certificate(40)";
  attrib HV140_41 format=F00164_. label="Member has a birth certificate(41)";
  attrib HV140_42 format=F00164_. label="Member has a birth certificate(42)";
  attrib HV140_43 format=F00164_. label="Member has a birth certificate(43)";
  attrib HV140_44 format=F00164_. label="Member has a birth certificate(44)";
  attrib HV140_45 format=F00164_. label="Member has a birth certificate(45)";
  attrib HV140_46 format=F00164_. label="Member has a birth certificate(46)";
  attrib HV140_47 format=F00164_. label="Member has a birth certificate(47)";
  attrib HV140_48 format=F00164_. label="Member has a birth certificate(48)";
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
  attrib IDXH4_31 label="Index to Household Schedule(31)";
  attrib IDXH4_32 label="Index to Household Schedule(32)";
  attrib IDXH4_33 label="Index to Household Schedule(33)";
  attrib IDXH4_34 label="Index to Household Schedule(34)";
  attrib IDXH4_35 label="Index to Household Schedule(35)";
  attrib IDXH4_36 label="Index to Household Schedule(36)";
  attrib IDXH4_37 label="Index to Household Schedule(37)";
  attrib IDXH4_38 label="Index to Household Schedule(38)";
  attrib IDXH4_39 label="Index to Household Schedule(39)";
  attrib IDXH4_40 label="Index to Household Schedule(40)";
  attrib IDXH4_41 label="Index to Household Schedule(41)";
  attrib IDXH4_42 label="Index to Household Schedule(42)";
  attrib IDXH4_43 label="Index to Household Schedule(43)";
  attrib IDXH4_44 label="Index to Household Schedule(44)";
  attrib IDXH4_45 label="Index to Household Schedule(45)";
  attrib IDXH4_46 label="Index to Household Schedule(46)";
  attrib IDXH4_47 label="Index to Household Schedule(47)";
  attrib IDXH4_48 label="Index to Household Schedule(48)";
  attrib SH17A_01 format=F00165_. label="Reason for never attending school(1)";
  attrib SH17A_02 format=F00165_. label="Reason for never attending school(2)";
  attrib SH17A_03 format=F00165_. label="Reason for never attending school(3)";
  attrib SH17A_04 format=F00165_. label="Reason for never attending school(4)";
  attrib SH17A_05 format=F00165_. label="Reason for never attending school(5)";
  attrib SH17A_06 format=F00165_. label="Reason for never attending school(6)";
  attrib SH17A_07 format=F00165_. label="Reason for never attending school(7)";
  attrib SH17A_08 format=F00165_. label="Reason for never attending school(8)";
  attrib SH17A_09 format=F00165_. label="Reason for never attending school(9)";
  attrib SH17A_10 format=F00165_. label="Reason for never attending school(10)";
  attrib SH17A_11 format=F00165_. label="Reason for never attending school(11)";
  attrib SH17A_12 format=F00165_. label="Reason for never attending school(12)";
  attrib SH17A_13 format=F00165_. label="Reason for never attending school(13)";
  attrib SH17A_14 format=F00165_. label="Reason for never attending school(14)";
  attrib SH17A_15 format=F00165_. label="Reason for never attending school(15)";
  attrib SH17A_16 format=F00165_. label="Reason for never attending school(16)";
  attrib SH17A_17 format=F00165_. label="Reason for never attending school(17)";
  attrib SH17A_18 format=F00165_. label="Reason for never attending school(18)";
  attrib SH17A_19 format=F00165_. label="Reason for never attending school(19)";
  attrib SH17A_20 format=F00165_. label="Reason for never attending school(20)";
  attrib SH17A_21 format=F00165_. label="Reason for never attending school(21)";
  attrib SH17A_22 format=F00165_. label="Reason for never attending school(22)";
  attrib SH17A_23 format=F00165_. label="Reason for never attending school(23)";
  attrib SH17A_24 format=F00165_. label="Reason for never attending school(24)";
  attrib SH17A_25 format=F00165_. label="Reason for never attending school(25)";
  attrib SH17A_26 format=F00165_. label="Reason for never attending school(26)";
  attrib SH17A_27 format=F00165_. label="Reason for never attending school(27)";
  attrib SH17A_28 format=F00165_. label="Reason for never attending school(28)";
  attrib SH17A_29 format=F00165_. label="Reason for never attending school(29)";
  attrib SH17A_30 format=F00165_. label="Reason for never attending school(30)";
  attrib SH17A_31 format=F00165_. label="Reason for never attending school(31)";
  attrib SH17A_32 format=F00165_. label="Reason for never attending school(32)";
  attrib SH17A_33 format=F00165_. label="Reason for never attending school(33)";
  attrib SH17A_34 format=F00165_. label="Reason for never attending school(34)";
  attrib SH17A_35 format=F00165_. label="Reason for never attending school(35)";
  attrib SH17A_36 format=F00165_. label="Reason for never attending school(36)";
  attrib SH17A_37 format=F00165_. label="Reason for never attending school(37)";
  attrib SH17A_38 format=F00165_. label="Reason for never attending school(38)";
  attrib SH17A_39 format=F00165_. label="Reason for never attending school(39)";
  attrib SH17A_40 format=F00165_. label="Reason for never attending school(40)";
  attrib SH17A_41 format=F00165_. label="Reason for never attending school(41)";
  attrib SH17A_42 format=F00165_. label="Reason for never attending school(42)";
  attrib SH17A_43 format=F00165_. label="Reason for never attending school(43)";
  attrib SH17A_44 format=F00165_. label="Reason for never attending school(44)";
  attrib SH17A_45 format=F00165_. label="Reason for never attending school(45)";
  attrib SH17A_46 format=F00165_. label="Reason for never attending school(46)";
  attrib SH17A_47 format=F00165_. label="Reason for never attending school(47)";
  attrib SH17A_48 format=F00165_. label="Reason for never attending school(48)";
  attrib SH19A_01 format=F00166_. label="Reason for not attending school(1)";
  attrib SH19A_02 format=F00166_. label="Reason for not attending school(2)";
  attrib SH19A_03 format=F00166_. label="Reason for not attending school(3)";
  attrib SH19A_04 format=F00166_. label="Reason for not attending school(4)";
  attrib SH19A_05 format=F00166_. label="Reason for not attending school(5)";
  attrib SH19A_06 format=F00166_. label="Reason for not attending school(6)";
  attrib SH19A_07 format=F00166_. label="Reason for not attending school(7)";
  attrib SH19A_08 format=F00166_. label="Reason for not attending school(8)";
  attrib SH19A_09 format=F00166_. label="Reason for not attending school(9)";
  attrib SH19A_10 format=F00166_. label="Reason for not attending school(10)";
  attrib SH19A_11 format=F00166_. label="Reason for not attending school(11)";
  attrib SH19A_12 format=F00166_. label="Reason for not attending school(12)";
  attrib SH19A_13 format=F00166_. label="Reason for not attending school(13)";
  attrib SH19A_14 format=F00166_. label="Reason for not attending school(14)";
  attrib SH19A_15 format=F00166_. label="Reason for not attending school(15)";
  attrib SH19A_16 format=F00166_. label="Reason for not attending school(16)";
  attrib SH19A_17 format=F00166_. label="Reason for not attending school(17)";
  attrib SH19A_18 format=F00166_. label="Reason for not attending school(18)";
  attrib SH19A_19 format=F00166_. label="Reason for not attending school(19)";
  attrib SH19A_20 format=F00166_. label="Reason for not attending school(20)";
  attrib SH19A_21 format=F00166_. label="Reason for not attending school(21)";
  attrib SH19A_22 format=F00166_. label="Reason for not attending school(22)";
  attrib SH19A_23 format=F00166_. label="Reason for not attending school(23)";
  attrib SH19A_24 format=F00166_. label="Reason for not attending school(24)";
  attrib SH19A_25 format=F00166_. label="Reason for not attending school(25)";
  attrib SH19A_26 format=F00166_. label="Reason for not attending school(26)";
  attrib SH19A_27 format=F00166_. label="Reason for not attending school(27)";
  attrib SH19A_28 format=F00166_. label="Reason for not attending school(28)";
  attrib SH19A_29 format=F00166_. label="Reason for not attending school(29)";
  attrib SH19A_30 format=F00166_. label="Reason for not attending school(30)";
  attrib SH19A_31 format=F00166_. label="Reason for not attending school(31)";
  attrib SH19A_32 format=F00166_. label="Reason for not attending school(32)";
  attrib SH19A_33 format=F00166_. label="Reason for not attending school(33)";
  attrib SH19A_34 format=F00166_. label="Reason for not attending school(34)";
  attrib SH19A_35 format=F00166_. label="Reason for not attending school(35)";
  attrib SH19A_36 format=F00166_. label="Reason for not attending school(36)";
  attrib SH19A_37 format=F00166_. label="Reason for not attending school(37)";
  attrib SH19A_38 format=F00166_. label="Reason for not attending school(38)";
  attrib SH19A_39 format=F00166_. label="Reason for not attending school(39)";
  attrib SH19A_40 format=F00166_. label="Reason for not attending school(40)";
  attrib SH19A_41 format=F00166_. label="Reason for not attending school(41)";
  attrib SH19A_42 format=F00166_. label="Reason for not attending school(42)";
  attrib SH19A_43 format=F00166_. label="Reason for not attending school(43)";
  attrib SH19A_44 format=F00166_. label="Reason for not attending school(44)";
  attrib SH19A_45 format=F00166_. label="Reason for not attending school(45)";
  attrib SH19A_46 format=F00166_. label="Reason for not attending school(46)";
  attrib SH19A_47 format=F00166_. label="Reason for not attending school(47)";
  attrib SH19A_48 format=F00166_. label="Reason for not attending school(48)";
  attrib HA0_1    label="NA - Index to Household Schedule(1)";
  attrib HA0_2    label="NA - Index to Household Schedule(2)";
  attrib HA1_1    label="NA - Woman's age in years(1)";
  attrib HA1_2    label="NA - Woman's age in years(2)";
  attrib HA2_1    format=F00167_. label="NA - Woman's weight in kilograms (1 decimal)(1)";
  attrib HA2_2    format=F00167_. label="NA - Woman's weight in kilograms (1 decimal)(2)";
  attrib HA3_1    format=F00168_. label="NA - Woman's height in centimeters (1 decimal)(1)";
  attrib HA3_2    format=F00168_. label="NA - Woman's height in centimeters (1 decimal)(2)";
  attrib HA4_1    format=F00169_. label="NA - Height/Age percentile(1)";
  attrib HA4_2    format=F00169_. label="NA - Height/Age percentile(2)";
  attrib HA5_1    format=F00170_. label="NA - Height/Age standard deviation(1)";
  attrib HA5_2    format=F00170_. label="NA - Height/Age standard deviation(2)";
  attrib HA6_1    format=F00171_. label="NA - Height/Age percent ref. median(1)";
  attrib HA6_2    format=F00171_. label="NA - Height/Age percent ref. median(2)";
  attrib HA11_1   format=F00172_. label="NA - Weight/Height standard deviation (DHS)(1)";
  attrib HA11_2   format=F00172_. label="NA - Weight/Height standard deviation (DHS)(2)";
  attrib HA12_1   format=F00173_. label="NA - Weight/Height percent ref. median (DHS)(1)";
  attrib HA12_2   format=F00173_. label="NA - Weight/Height percent ref. median (DHS)(2)";
  attrib HA12A_1  format=F00174_. label="NA - Weight/Height percent ref. median (Fog)(1)";
  attrib HA12A_2  format=F00174_. label="NA - Weight/Height percent ref. median (Fog)(2)";
  attrib HA12B_1  format=F00175_. label="NA - Weight/Height percent ref. median (WHO)(1)";
  attrib HA12B_2  format=F00175_. label="NA - Weight/Height percent ref. median (WHO)(2)";
  attrib HA13_1   format=F00176_. label="NA - Result of measurement - height/weight(1)";
  attrib HA13_2   format=F00176_. label="NA - Result of measurement - height/weight(2)";
  attrib HA21_1   label="NA - Filedworker measurer code(1)";
  attrib HA21_2   label="NA - Filedworker measurer code(2)";
  attrib HA32_1   label="NA - Date of birth (CMC)(1)";
  attrib HA32_2   label="NA - Date of birth (CMC)(2)";
  attrib HA33_1   format=F00177_. label="NA - Completeness of HA32 information(1)";
  attrib HA33_2   format=F00177_. label="NA - Completeness of HA32 information(2)";
  attrib HA35_1   format=F00178_. label="NA - Smoking (cigarettes in last 24 hours)(1)";
  attrib HA35_2   format=F00178_. label="NA - Smoking (cigarettes in last 24 hours)(2)";
  attrib HA40_1   format=F00179_. label="NA - Body Mass Index(1)";
  attrib HA40_2   format=F00179_. label="NA - Body Mass Index(2)";
  attrib HA41_1   format=F00180_. label="NA - Rohrer's index(1)";
  attrib HA41_2   format=F00180_. label="NA - Rohrer's index(2)";
  attrib HA50_1   format=F00181_. label="NA - Under age 18(1)";
  attrib HA50_2   format=F00181_. label="NA - Under age 18(2)";
  attrib HA51_1   format=F00182_. label="NA - Line number of parent/caretaker(1)";
  attrib HA51_2   format=F00182_. label="NA - Line number of parent/caretaker(2)";
  attrib HA52_1   format=F00183_. label="NA - Read consent statement - hemoglobin(1)";
  attrib HA52_2   format=F00183_. label="NA - Read consent statement - hemoglobin(2)";
  attrib HA53_1   format=F00184_. label="NA - Hemoglobin level (g/dl - 1 decimal)(1)";
  attrib HA53_2   format=F00184_. label="NA - Hemoglobin level (g/dl - 1 decimal)(2)";
  attrib HA54_1   format=F00185_. label="NA - Currently pregnant(1)";
  attrib HA54_2   format=F00185_. label="NA - Currently pregnant(2)";
  attrib HA55_1   format=F00186_. label="NA - Result of measurement - hemoglobin(1)";
  attrib HA55_2   format=F00186_. label="NA - Result of measurement - hemoglobin(2)";
  attrib HA56_1   format=F00187_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(1)";
  attrib HA56_2   format=F00187_. label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)(2)";
  attrib HA57_1   format=F00188_. label="NA - Anemia level(1)";
  attrib HA57_2   format=F00188_. label="NA - Anemia level(2)";
  attrib HA58_1   format=F00189_. label="NA - Agrees to referral - anemia(1)";
  attrib HA58_2   format=F00189_. label="NA - Agrees to referral - anemia(2)";
  attrib HA60_1   format=F00190_. label="NA - Marital status(1)";
  attrib HA60_2   format=F00190_. label="NA - Marital status(2)";
  attrib HA61_1   format=F00191_. label="NA - Read consent statement - HIV(1)";
  attrib HA61_2   format=F00191_. label="NA - Read consent statement - HIV(2)";
  attrib HA62_1   length=$5 format=$F00192_. label="NA - Bar code for HIV blood sample(1)";
  attrib HA62_2   length=$5 format=$F00192_. label="NA - Bar code for HIV blood sample(2)";
  attrib HA63_1   format=F00193_. label="NA - Result of measurement - HIV(1)";
  attrib HA63_2   format=F00193_. label="NA - Result of measurement - HIV(2)";
  attrib HA64_1   format=F00194_. label="NA - Consent for additional tests(1)";
  attrib HA64_2   format=F00194_. label="NA - Consent for additional tests(2)";
  attrib HA65_1   format=F00195_. label="NA - Result of woman's individual interview(1)";
  attrib HA65_2   format=F00195_. label="NA - Result of woman's individual interview(2)";
  attrib HA66_1   format=F00196_. label="NA - Woman's highest educational level(1)";
  attrib HA66_2   format=F00196_. label="NA - Woman's highest educational level(2)";
  attrib HA67_1   format=F00197_. label="NA - Woman's highest year of education(1)";
  attrib HA67_2   format=F00197_. label="NA - Woman's highest year of education(2)";
  attrib HA68_1   format=F00198_. label="NA - Woman's highest educational level (for preliminary and final report)(1)";
  attrib HA68_2   format=F00198_. label="NA - Woman's highest educational level (for preliminary and final report)(2)";
  attrib HA69_1   label="NA - HIV weight (6 decimals)(1)";
  attrib HA69_2   label="NA - HIV weight (6 decimals)(2)";
  attrib HA70_1   label="NA - Interviewer that took blood for HIV testing(1)";
  attrib HA70_2   label="NA - Interviewer that took blood for HIV testing(2)";
  attrib HC0_1    label="NA - Index to Household Schedule(1)";
  attrib HC0_2    label="NA - Index to Household Schedule(2)";
  attrib HC1_1    label="NA - Child's age in months(1)";
  attrib HC1_2    label="NA - Child's age in months(2)";
  attrib HC1A_1   label="NA - Child's age in days(1)";
  attrib HC1A_2   label="NA - Child's age in days(2)";
  attrib HC2_1    format=F00199_. label="NA - Child's weight in kilograms (1 decimal)(1)";
  attrib HC2_2    format=F00199_. label="NA - Child's weight in kilograms (1 decimal)(2)";
  attrib HC3_1    format=F00200_. label="NA - Child's height in centimeters (1 decimal)(1)";
  attrib HC3_2    format=F00200_. label="NA - Child's height in centimeters (1 decimal)(2)";
  attrib HC4_1    format=F00201_. label="NA - Height/Age percentile(1)";
  attrib HC4_2    format=F00201_. label="NA - Height/Age percentile(2)";
  attrib HC5_1    format=F00202_. label="NA - Height/Age standard deviation(1)";
  attrib HC5_2    format=F00202_. label="NA - Height/Age standard deviation(2)";
  attrib HC6_1    format=F00203_. label="NA - Height/Age percent of ref. median(1)";
  attrib HC6_2    format=F00203_. label="NA - Height/Age percent of ref. median(2)";
  attrib HC7_1    format=F00204_. label="NA - Weight/Age percentile(1)";
  attrib HC7_2    format=F00204_. label="NA - Weight/Age percentile(2)";
  attrib HC8_1    format=F00205_. label="NA - Weight/Age standard deviation(1)";
  attrib HC8_2    format=F00205_. label="NA - Weight/Age standard deviation(2)";
  attrib HC9_1    format=F00206_. label="NA - Weight/Age percent of ref. median(1)";
  attrib HC9_2    format=F00206_. label="NA - Weight/Age percent of ref. median(2)";
  attrib HC10_1   format=F00207_. label="NA - Weight/Height percentile(1)";
  attrib HC10_2   format=F00207_. label="NA - Weight/Height percentile(2)";
  attrib HC11_1   format=F00208_. label="NA - Weight/Height standard deviation(1)";
  attrib HC11_2   format=F00208_. label="NA - Weight/Height standard deviation(2)";
  attrib HC12_1   format=F00209_. label="NA - Weight/Height percent of ref. median(1)";
  attrib HC12_2   format=F00209_. label="NA - Weight/Height percent of ref. median(2)";
  attrib HC13_1   format=F00210_. label="NA - Result of measurement - height/weight(1)";
  attrib HC13_2   format=F00210_. label="NA - Result of measurement - height/weight(2)";
  attrib HC15_1   format=F00211_. label="NA - Height: lying or standing(1)";
  attrib HC15_2   format=F00211_. label="NA - Height: lying or standing(2)";
  attrib HC16_1   format=F00212_. label="NA - Day of birth(1)";
  attrib HC16_2   format=F00212_. label="NA - Day of birth(2)";
  attrib HC17_1   label="NA - Date measured (day)(1)";
  attrib HC17_2   label="NA - Date measured (day)(2)";
  attrib HC18_1   label="NA - Date measured (month)(1)";
  attrib HC18_2   label="NA - Date measured (month)(2)";
  attrib HC19_1   label="NA - Date measured (year)(1)";
  attrib HC19_2   label="NA - Date measured (year)(2)";
  attrib HC20_1   label="NA - Century Day Code (CDC) of measurement(1)";
  attrib HC20_2   label="NA - Century Day Code (CDC) of measurement(2)";
  attrib HC21_1   label="NA - Filedworker measurer code(1)";
  attrib HC21_2   label="NA - Filedworker measurer code(2)";
  attrib HC27_1   format=F00213_. label="NA - Sex(1)";
  attrib HC27_2   format=F00213_. label="NA - Sex(2)";
  attrib HC30_1   format=F00214_. label="NA - Month of birth(1)";
  attrib HC30_2   format=F00214_. label="NA - Month of birth(2)";
  attrib HC31_1   format=F00215_. label="NA - Year of birth(1)";
  attrib HC31_2   format=F00215_. label="NA - Year of birth(2)";
  attrib HC32_1   label="NA - Date of birth (CMC)(1)";
  attrib HC32_2   label="NA - Date of birth (CMC)(2)";
  attrib HC32A_1  label="NA - Century Day Code (CDC) of birth(1)";
  attrib HC32A_2  label="NA - Century Day Code (CDC) of birth(2)";
  attrib HC33_1   format=F00216_. label="NA - Completeness of HC32 information(1)";
  attrib HC33_2   format=F00216_. label="NA - Completeness of HC32 information(2)";
  attrib HC51_1   format=F00217_. label="NA - Line number of parent/caretaker(1)";
  attrib HC51_2   format=F00217_. label="NA - Line number of parent/caretaker(2)";
  attrib HC52_1   format=F00218_. label="NA - Read consent statement - hemoglobin(1)";
  attrib HC52_2   format=F00218_. label="NA - Read consent statement - hemoglobin(2)";
  attrib HC53_1   format=F00219_. label="NA - Hemoglobin level (g/dl - 1 decimal)(1)";
  attrib HC53_2   format=F00219_. label="NA - Hemoglobin level (g/dl - 1 decimal)(2)";
  attrib HC55_1   format=F00220_. label="NA - Result of measurement - hemoglobin(1)";
  attrib HC55_2   format=F00220_. label="NA - Result of measurement - hemoglobin(2)";
  attrib HC56_1   format=F00221_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(1)";
  attrib HC56_2   format=F00221_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(2)";
  attrib HC57_1   format=F00222_. label="NA - Anemia level(1)";
  attrib HC57_2   format=F00222_. label="NA - Anemia level(2)";
  attrib HC58_1   format=F00223_. label="NA - Agrees to referral - anemia(1)";
  attrib HC58_2   format=F00223_. label="NA - Agrees to referral - anemia(2)";
  attrib HC60_1   format=F00224_. label="NA - Mother's line number (woman's questionnaire)(1)";
  attrib HC60_2   format=F00224_. label="NA - Mother's line number (woman's questionnaire)(2)";
  attrib HC61_1   format=F00225_. label="NA - Mother's highest educational level(1)";
  attrib HC61_2   format=F00225_. label="NA - Mother's highest educational level(2)";
  attrib HC62_1   format=F00226_. label="NA - Mother's highest year of education(1)";
  attrib HC62_2   format=F00226_. label="NA - Mother's highest year of education(2)";
  attrib HC63_1   label="NA - Preceding birth interval (months)(1)";
  attrib HC63_2   label="NA - Preceding birth interval (months)(2)";
  attrib HC64_1   label="NA - Birth order number(1)";
  attrib HC64_2   label="NA - Birth order number(2)";
  attrib HC68_1   format=F00227_. label="NA - Mother's highest educational level (for preliminary and final report)(1)";
  attrib HC68_2   format=F00227_. label="NA - Mother's highest educational level (for preliminary and final report)(2)";
  attrib HC70_1   format=F00228_. label="NA - Height/Age standard deviation (new WHO)(1)";
  attrib HC70_2   format=F00228_. label="NA - Height/Age standard deviation (new WHO)(2)";
  attrib HC71_1   format=F00229_. label="NA - Weight/Age standard deviation (new WHO)(1)";
  attrib HC71_2   format=F00229_. label="NA - Weight/Age standard deviation (new WHO)(2)";
  attrib HC72_1   format=F00230_. label="NA - Weight/Height standard deviation (new WHO)(1)";
  attrib HC72_2   format=F00230_. label="NA - Weight/Height standard deviation (new WHO)(2)";
  attrib HC73_1   format=F00231_. label="NA - BMI standard deviation (new WHO)(1)";
  attrib HC73_2   format=F00231_. label="NA - BMI standard deviation (new WHO)(2)";
  attrib HMLIDX_1 label="Mosquito Bed Net Designation Number(1)";
  attrib HMLIDX_2 label="Mosquito Bed Net Designation Number(2)";
  attrib HMLIDX_3 label="Mosquito Bed Net Designation Number(3)";
  attrib HMLIDX_4 label="Mosquito Bed Net Designation Number(4)";
  attrib HMLIDX_5 label="Mosquito Bed Net Designation Number(5)";
  attrib HMLIDX_6 label="Mosquito Bed Net Designation Number(6)";
  attrib HMLIDX_7 label="Mosquito Bed Net Designation Number(7)";
  attrib HML3_1   format=F00232_. label="Net observed by interviewer(1)";
  attrib HML3_2   format=F00232_. label="Net observed by interviewer(2)";
  attrib HML3_3   format=F00232_. label="Net observed by interviewer(3)";
  attrib HML3_4   format=F00232_. label="Net observed by interviewer(4)";
  attrib HML3_5   format=F00232_. label="Net observed by interviewer(5)";
  attrib HML3_6   format=F00232_. label="Net observed by interviewer(6)";
  attrib HML3_7   format=F00232_. label="Net observed by interviewer(7)";
  attrib HML4_1   format=F00233_. label="Months ago net obtained(1)";
  attrib HML4_2   format=F00233_. label="Months ago net obtained(2)";
  attrib HML4_3   format=F00233_. label="Months ago net obtained(3)";
  attrib HML4_4   format=F00233_. label="Months ago net obtained(4)";
  attrib HML4_5   format=F00233_. label="Months ago net obtained(5)";
  attrib HML4_6   format=F00233_. label="Months ago net obtained(6)";
  attrib HML4_7   format=F00233_. label="Months ago net obtained(7)";
  attrib HML5_1   format=F00234_. label="Net treated with insecticide when bought(1)";
  attrib HML5_2   format=F00234_. label="Net treated with insecticide when bought(2)";
  attrib HML5_3   format=F00234_. label="Net treated with insecticide when bought(3)";
  attrib HML5_4   format=F00234_. label="Net treated with insecticide when bought(4)";
  attrib HML5_5   format=F00234_. label="Net treated with insecticide when bought(5)";
  attrib HML5_6   format=F00234_. label="Net treated with insecticide when bought(6)";
  attrib HML5_7   format=F00234_. label="Net treated with insecticide when bought(7)";
  attrib HML6_1   format=F00235_. label="Net treatment status(1)";
  attrib HML6_2   format=F00235_. label="Net treatment status(2)";
  attrib HML6_3   format=F00235_. label="Net treatment status(3)";
  attrib HML6_4   format=F00235_. label="Net treatment status(4)";
  attrib HML6_5   format=F00235_. label="Net treatment status(5)";
  attrib HML6_6   format=F00235_. label="Net treatment status(6)";
  attrib HML6_7   format=F00235_. label="Net treatment status(7)";
  attrib HML7_1   format=F00236_. label="Brand of net(1)";
  attrib HML7_2   format=F00236_. label="Brand of net(2)";
  attrib HML7_3   format=F00236_. label="Brand of net(3)";
  attrib HML7_4   format=F00236_. label="Brand of net(4)";
  attrib HML7_5   format=F00236_. label="Brand of net(5)";
  attrib HML7_6   format=F00236_. label="Brand of net(6)";
  attrib HML7_7   format=F00236_. label="Brand of net(7)";
  attrib HML8_1   format=F00237_. label="Net treated since receiving(1)";
  attrib HML8_2   format=F00237_. label="Net treated since receiving(2)";
  attrib HML8_3   format=F00237_. label="Net treated since receiving(3)";
  attrib HML8_4   format=F00237_. label="Net treated since receiving(4)";
  attrib HML8_5   format=F00237_. label="Net treated since receiving(5)";
  attrib HML8_6   format=F00237_. label="Net treated since receiving(6)";
  attrib HML8_7   format=F00237_. label="Net treated since receiving(7)";
  attrib HML9_1   format=F00238_. label="Time since last re-treatment (months)(1)";
  attrib HML9_2   format=F00238_. label="Time since last re-treatment (months)(2)";
  attrib HML9_3   format=F00238_. label="Time since last re-treatment (months)(3)";
  attrib HML9_4   format=F00238_. label="Time since last re-treatment (months)(4)";
  attrib HML9_5   format=F00238_. label="Time since last re-treatment (months)(5)";
  attrib HML9_6   format=F00238_. label="Time since last re-treatment (months)(6)";
  attrib HML9_7   format=F00238_. label="Time since last re-treatment (months)(7)";
  attrib HML10_1  format=F00239_. label="Insecticide-Treated Net (ITN)(1)";
  attrib HML10_2  format=F00239_. label="Insecticide-Treated Net (ITN)(2)";
  attrib HML10_3  format=F00239_. label="Insecticide-Treated Net (ITN)(3)";
  attrib HML10_4  format=F00239_. label="Insecticide-Treated Net (ITN)(4)";
  attrib HML10_5  format=F00239_. label="Insecticide-Treated Net (ITN)(5)";
  attrib HML10_6  format=F00239_. label="Insecticide-Treated Net (ITN)(6)";
  attrib HML10_7  format=F00239_. label="Insecticide-Treated Net (ITN)(7)";
  attrib HML11_1  format=F00240_. label="Number of persons who slept under this net(1)";
  attrib HML11_2  format=F00240_. label="Number of persons who slept under this net(2)";
  attrib HML11_3  format=F00240_. label="Number of persons who slept under this net(3)";
  attrib HML11_4  format=F00240_. label="Number of persons who slept under this net(4)";
  attrib HML11_5  format=F00240_. label="Number of persons who slept under this net(5)";
  attrib HML11_6  format=F00240_. label="Number of persons who slept under this net(6)";
  attrib HML11_7  format=F00240_. label="Number of persons who slept under this net(7)";
  attrib HML21_1  format=F00241_. label="Someone slept under this net last night(1)";
  attrib HML21_2  format=F00241_. label="Someone slept under this net last night(2)";
  attrib HML21_3  format=F00241_. label="Someone slept under this net last night(3)";
  attrib HML21_4  format=F00241_. label="Someone slept under this net last night(4)";
  attrib HML21_5  format=F00241_. label="Someone slept under this net last night(5)";
  attrib HML21_6  format=F00241_. label="Someone slept under this net last night(6)";
  attrib HML21_7  format=F00241_. label="Someone slept under this net last night(7)";
  attrib HMLA_1   format=F00242_. label="Line number of person who slept in this net(1)";
  attrib HMLA_2   format=F00242_. label="Line number of person who slept in this net(2)";
  attrib HMLA_3   format=F00242_. label="Line number of person who slept in this net(3)";
  attrib HMLA_4   format=F00242_. label="Line number of person who slept in this net(4)";
  attrib HMLA_5   format=F00242_. label="Line number of person who slept in this net(5)";
  attrib HMLA_6   format=F00242_. label="Line number of person who slept in this net(6)";
  attrib HMLA_7   format=F00242_. label="Line number of person who slept in this net(7)";
  attrib HMLB_1   label="Line number of person who slept in this net(1)";
  attrib HMLB_2   label="Line number of person who slept in this net(2)";
  attrib HMLB_3   label="Line number of person who slept in this net(3)";
  attrib HMLB_4   label="Line number of person who slept in this net(4)";
  attrib HMLB_5   label="Line number of person who slept in this net(5)";
  attrib HMLB_6   label="Line number of person who slept in this net(6)";
  attrib HMLB_7   label="Line number of person who slept in this net(7)";
  attrib HMLC_1   label="Line number of person who slept in this net(1)";
  attrib HMLC_2   label="Line number of person who slept in this net(2)";
  attrib HMLC_3   label="Line number of person who slept in this net(3)";
  attrib HMLC_4   label="Line number of person who slept in this net(4)";
  attrib HMLC_5   label="Line number of person who slept in this net(5)";
  attrib HMLC_6   label="Line number of person who slept in this net(6)";
  attrib HMLC_7   label="Line number of person who slept in this net(7)";
  attrib HMLD_1   label="Line number of person who slept in this net(1)";
  attrib HMLD_2   label="Line number of person who slept in this net(2)";
  attrib HMLD_3   label="Line number of person who slept in this net(3)";
  attrib HMLD_4   label="Line number of person who slept in this net(4)";
  attrib HMLD_5   label="Line number of person who slept in this net(5)";
  attrib HMLD_6   label="Line number of person who slept in this net(6)";
  attrib HMLD_7   label="Line number of person who slept in this net(7)";
  attrib HMLE_1   label="NA - Line number of person who slept in this net(1)";
  attrib HMLE_2   label="NA - Line number of person who slept in this net(2)";
  attrib HMLE_3   label="NA - Line number of person who slept in this net(3)";
  attrib HMLE_4   label="NA - Line number of person who slept in this net(4)";
  attrib HMLE_5   label="NA - Line number of person who slept in this net(5)";
  attrib HMLE_6   label="NA - Line number of person who slept in this net(6)";
  attrib HMLE_7   label="NA - Line number of person who slept in this net(7)";
  attrib HML22_1  format=F00243_. label="NA - Obtained net from campaign, antenatal or immunization visit(1)";
  attrib HML22_2  format=F00243_. label="NA - Obtained net from campaign, antenatal or immunization visit(2)";
  attrib HML22_3  format=F00243_. label="NA - Obtained net from campaign, antenatal or immunization visit(3)";
  attrib HML22_4  format=F00243_. label="NA - Obtained net from campaign, antenatal or immunization visit(4)";
  attrib HML22_5  format=F00243_. label="NA - Obtained net from campaign, antenatal or immunization visit(5)";
  attrib HML22_6  format=F00243_. label="NA - Obtained net from campaign, antenatal or immunization visit(6)";
  attrib HML22_7  format=F00243_. label="NA - Obtained net from campaign, antenatal or immunization visit(7)";
  attrib HML23_1  format=F00244_. label="NA - Place where net was obtained(1)";
  attrib HML23_2  format=F00244_. label="NA - Place where net was obtained(2)";
  attrib HML23_3  format=F00244_. label="NA - Place where net was obtained(3)";
  attrib HML23_4  format=F00244_. label="NA - Place where net was obtained(4)";
  attrib HML23_5  format=F00244_. label="NA - Place where net was obtained(5)";
  attrib HML23_6  format=F00244_. label="NA - Place where net was obtained(6)";
  attrib HML23_7  format=F00244_. label="NA - Place where net was obtained(7)";
  attrib HMHIDX_01 label="Index to Household Schedule(1)";
  attrib HMHIDX_02 label="Index to Household Schedule(2)";
  attrib HMHIDX_03 label="Index to Household Schedule(3)";
  attrib HMHIDX_04 label="Index to Household Schedule(4)";
  attrib HMHIDX_05 label="Index to Household Schedule(5)";
  attrib HMHIDX_06 label="Index to Household Schedule(6)";
  attrib HMHIDX_07 label="Index to Household Schedule(7)";
  attrib HMHIDX_08 label="Index to Household Schedule(8)";
  attrib HMHIDX_09 label="Index to Household Schedule(9)";
  attrib HMHIDX_10 label="Index to Household Schedule(10)";
  attrib HMHIDX_11 label="Index to Household Schedule(11)";
  attrib HMHIDX_12 label="Index to Household Schedule(12)";
  attrib HMHIDX_13 label="Index to Household Schedule(13)";
  attrib HMHIDX_14 label="Index to Household Schedule(14)";
  attrib HMHIDX_15 label="Index to Household Schedule(15)";
  attrib HMHIDX_16 label="Index to Household Schedule(16)";
  attrib HMHIDX_17 label="Index to Household Schedule(17)";
  attrib HMHIDX_18 label="Index to Household Schedule(18)";
  attrib HMHIDX_19 label="Index to Household Schedule(19)";
  attrib HMHIDX_20 label="Index to Household Schedule(20)";
  attrib HMHIDX_21 label="Index to Household Schedule(21)";
  attrib HMHIDX_22 label="Index to Household Schedule(22)";
  attrib HMHIDX_23 label="Index to Household Schedule(23)";
  attrib HMHIDX_24 label="Index to Household Schedule(24)";
  attrib HMHIDX_25 label="Index to Household Schedule(25)";
  attrib HMHIDX_26 label="Index to Household Schedule(26)";
  attrib HMHIDX_27 label="Index to Household Schedule(27)";
  attrib HMHIDX_28 label="Index to Household Schedule(28)";
  attrib HMHIDX_29 label="Index to Household Schedule(29)";
  attrib HMHIDX_30 label="Index to Household Schedule(30)";
  attrib HMHIDX_31 label="Index to Household Schedule(31)";
  attrib HMHIDX_32 label="Index to Household Schedule(32)";
  attrib HMHIDX_33 label="Index to Household Schedule(33)";
  attrib HMHIDX_34 label="Index to Household Schedule(34)";
  attrib HMHIDX_35 label="Index to Household Schedule(35)";
  attrib HMHIDX_36 label="Index to Household Schedule(36)";
  attrib HMHIDX_37 label="Index to Household Schedule(37)";
  attrib HMHIDX_38 label="Index to Household Schedule(38)";
  attrib HMHIDX_39 label="Index to Household Schedule(39)";
  attrib HMHIDX_40 label="Index to Household Schedule(40)";
  attrib HMHIDX_41 label="Index to Household Schedule(41)";
  attrib HMHIDX_42 label="Index to Household Schedule(42)";
  attrib HMHIDX_43 label="Index to Household Schedule(43)";
  attrib HMHIDX_44 label="Index to Household Schedule(44)";
  attrib HMHIDX_45 label="Index to Household Schedule(45)";
  attrib HMHIDX_46 label="Index to Household Schedule(46)";
  attrib HMHIDX_47 label="Index to Household Schedule(47)";
  attrib HMHIDX_48 label="Index to Household Schedule(48)";
  attrib HML12_01 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(1)";
  attrib HML12_02 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(2)";
  attrib HML12_03 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(3)";
  attrib HML12_04 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(4)";
  attrib HML12_05 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(5)";
  attrib HML12_06 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(6)";
  attrib HML12_07 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(7)";
  attrib HML12_08 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(8)";
  attrib HML12_09 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(9)";
  attrib HML12_10 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(10)";
  attrib HML12_11 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(11)";
  attrib HML12_12 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(12)";
  attrib HML12_13 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(13)";
  attrib HML12_14 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(14)";
  attrib HML12_15 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(15)";
  attrib HML12_16 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(16)";
  attrib HML12_17 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(17)";
  attrib HML12_18 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(18)";
  attrib HML12_19 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(19)";
  attrib HML12_20 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(20)";
  attrib HML12_21 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(21)";
  attrib HML12_22 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(22)";
  attrib HML12_23 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(23)";
  attrib HML12_24 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(24)";
  attrib HML12_25 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(25)";
  attrib HML12_26 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(26)";
  attrib HML12_27 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(27)";
  attrib HML12_28 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(28)";
  attrib HML12_29 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(29)";
  attrib HML12_30 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(30)";
  attrib HML12_31 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(31)";
  attrib HML12_32 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(32)";
  attrib HML12_33 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(33)";
  attrib HML12_34 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(34)";
  attrib HML12_35 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(35)";
  attrib HML12_36 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(36)";
  attrib HML12_37 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(37)";
  attrib HML12_38 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(38)";
  attrib HML12_39 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(39)";
  attrib HML12_40 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(40)";
  attrib HML12_41 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(41)";
  attrib HML12_42 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(42)";
  attrib HML12_43 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(43)";
  attrib HML12_44 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(44)";
  attrib HML12_45 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(45)";
  attrib HML12_46 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(46)";
  attrib HML12_47 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(47)";
  attrib HML12_48 format=F00245_. label="Type of Mosquito Bed Net(s) person slept under last night(48)";
  attrib HML13_01 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(1)";
  attrib HML13_02 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(2)";
  attrib HML13_03 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(3)";
  attrib HML13_04 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(4)";
  attrib HML13_05 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(5)";
  attrib HML13_06 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(6)";
  attrib HML13_07 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(7)";
  attrib HML13_08 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(8)";
  attrib HML13_09 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(9)";
  attrib HML13_10 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(10)";
  attrib HML13_11 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(11)";
  attrib HML13_12 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(12)";
  attrib HML13_13 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(13)";
  attrib HML13_14 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(14)";
  attrib HML13_15 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(15)";
  attrib HML13_16 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(16)";
  attrib HML13_17 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(17)";
  attrib HML13_18 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(18)";
  attrib HML13_19 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(19)";
  attrib HML13_20 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(20)";
  attrib HML13_21 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(21)";
  attrib HML13_22 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(22)";
  attrib HML13_23 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(23)";
  attrib HML13_24 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(24)";
  attrib HML13_25 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(25)";
  attrib HML13_26 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(26)";
  attrib HML13_27 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(27)";
  attrib HML13_28 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(28)";
  attrib HML13_29 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(29)";
  attrib HML13_30 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(30)";
  attrib HML13_31 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(31)";
  attrib HML13_32 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(32)";
  attrib HML13_33 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(33)";
  attrib HML13_34 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(34)";
  attrib HML13_35 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(35)";
  attrib HML13_36 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(36)";
  attrib HML13_37 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(37)";
  attrib HML13_38 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(38)";
  attrib HML13_39 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(39)";
  attrib HML13_40 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(40)";
  attrib HML13_41 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(41)";
  attrib HML13_42 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(42)";
  attrib HML13_43 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(43)";
  attrib HML13_44 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(44)";
  attrib HML13_45 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(45)";
  attrib HML13_46 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(46)";
  attrib HML13_47 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(47)";
  attrib HML13_48 format=F00246_. label="Net Designation Number (HMLIDX) for 1st net person slept under last night(48)";
  attrib HML14_01 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(1)";
  attrib HML14_02 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(2)";
  attrib HML14_03 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(3)";
  attrib HML14_04 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(4)";
  attrib HML14_05 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(5)";
  attrib HML14_06 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(6)";
  attrib HML14_07 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(7)";
  attrib HML14_08 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(8)";
  attrib HML14_09 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(9)";
  attrib HML14_10 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(10)";
  attrib HML14_11 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(11)";
  attrib HML14_12 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(12)";
  attrib HML14_13 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(13)";
  attrib HML14_14 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(14)";
  attrib HML14_15 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(15)";
  attrib HML14_16 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(16)";
  attrib HML14_17 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(17)";
  attrib HML14_18 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(18)";
  attrib HML14_19 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(19)";
  attrib HML14_20 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(20)";
  attrib HML14_21 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(21)";
  attrib HML14_22 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(22)";
  attrib HML14_23 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(23)";
  attrib HML14_24 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(24)";
  attrib HML14_25 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(25)";
  attrib HML14_26 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(26)";
  attrib HML14_27 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(27)";
  attrib HML14_28 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(28)";
  attrib HML14_29 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(29)";
  attrib HML14_30 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(30)";
  attrib HML14_31 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(31)";
  attrib HML14_32 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(32)";
  attrib HML14_33 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(33)";
  attrib HML14_34 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(34)";
  attrib HML14_35 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(35)";
  attrib HML14_36 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(36)";
  attrib HML14_37 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(37)";
  attrib HML14_38 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(38)";
  attrib HML14_39 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(39)";
  attrib HML14_40 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(40)";
  attrib HML14_41 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(41)";
  attrib HML14_42 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(42)";
  attrib HML14_43 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(43)";
  attrib HML14_44 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(44)";
  attrib HML14_45 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(45)";
  attrib HML14_46 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(46)";
  attrib HML14_47 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(47)";
  attrib HML14_48 label="Net Designation Number (HMLIDX) for 2nd net person slept under last night(48)";
  attrib HML15_01 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(1)";
  attrib HML15_02 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(2)";
  attrib HML15_03 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(3)";
  attrib HML15_04 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(4)";
  attrib HML15_05 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(5)";
  attrib HML15_06 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(6)";
  attrib HML15_07 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(7)";
  attrib HML15_08 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(8)";
  attrib HML15_09 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(9)";
  attrib HML15_10 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(10)";
  attrib HML15_11 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(11)";
  attrib HML15_12 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(12)";
  attrib HML15_13 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(13)";
  attrib HML15_14 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(14)";
  attrib HML15_15 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(15)";
  attrib HML15_16 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(16)";
  attrib HML15_17 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(17)";
  attrib HML15_18 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(18)";
  attrib HML15_19 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(19)";
  attrib HML15_20 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(20)";
  attrib HML15_21 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(21)";
  attrib HML15_22 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(22)";
  attrib HML15_23 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(23)";
  attrib HML15_24 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(24)";
  attrib HML15_25 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(25)";
  attrib HML15_26 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(26)";
  attrib HML15_27 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(27)";
  attrib HML15_28 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(28)";
  attrib HML15_29 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(29)";
  attrib HML15_30 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(30)";
  attrib HML15_31 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(31)";
  attrib HML15_32 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(32)";
  attrib HML15_33 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(33)";
  attrib HML15_34 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(34)";
  attrib HML15_35 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(35)";
  attrib HML15_36 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(36)";
  attrib HML15_37 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(37)";
  attrib HML15_38 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(38)";
  attrib HML15_39 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(39)";
  attrib HML15_40 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(40)";
  attrib HML15_41 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(41)";
  attrib HML15_42 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(42)";
  attrib HML15_43 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(43)";
  attrib HML15_44 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(44)";
  attrib HML15_45 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(45)";
  attrib HML15_46 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(46)";
  attrib HML15_47 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(47)";
  attrib HML15_48 label="NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night(48)";
  attrib HML16_01 format=F00247_. label="Corrected age from Individual file(1)";
  attrib HML16_02 format=F00247_. label="Corrected age from Individual file(2)";
  attrib HML16_03 format=F00247_. label="Corrected age from Individual file(3)";
  attrib HML16_04 format=F00247_. label="Corrected age from Individual file(4)";
  attrib HML16_05 format=F00247_. label="Corrected age from Individual file(5)";
  attrib HML16_06 format=F00247_. label="Corrected age from Individual file(6)";
  attrib HML16_07 format=F00247_. label="Corrected age from Individual file(7)";
  attrib HML16_08 format=F00247_. label="Corrected age from Individual file(8)";
  attrib HML16_09 format=F00247_. label="Corrected age from Individual file(9)";
  attrib HML16_10 format=F00247_. label="Corrected age from Individual file(10)";
  attrib HML16_11 format=F00247_. label="Corrected age from Individual file(11)";
  attrib HML16_12 format=F00247_. label="Corrected age from Individual file(12)";
  attrib HML16_13 format=F00247_. label="Corrected age from Individual file(13)";
  attrib HML16_14 format=F00247_. label="Corrected age from Individual file(14)";
  attrib HML16_15 format=F00247_. label="Corrected age from Individual file(15)";
  attrib HML16_16 format=F00247_. label="Corrected age from Individual file(16)";
  attrib HML16_17 format=F00247_. label="Corrected age from Individual file(17)";
  attrib HML16_18 format=F00247_. label="Corrected age from Individual file(18)";
  attrib HML16_19 format=F00247_. label="Corrected age from Individual file(19)";
  attrib HML16_20 format=F00247_. label="Corrected age from Individual file(20)";
  attrib HML16_21 format=F00247_. label="Corrected age from Individual file(21)";
  attrib HML16_22 format=F00247_. label="Corrected age from Individual file(22)";
  attrib HML16_23 format=F00247_. label="Corrected age from Individual file(23)";
  attrib HML16_24 format=F00247_. label="Corrected age from Individual file(24)";
  attrib HML16_25 format=F00247_. label="Corrected age from Individual file(25)";
  attrib HML16_26 format=F00247_. label="Corrected age from Individual file(26)";
  attrib HML16_27 format=F00247_. label="Corrected age from Individual file(27)";
  attrib HML16_28 format=F00247_. label="Corrected age from Individual file(28)";
  attrib HML16_29 format=F00247_. label="Corrected age from Individual file(29)";
  attrib HML16_30 format=F00247_. label="Corrected age from Individual file(30)";
  attrib HML16_31 format=F00247_. label="Corrected age from Individual file(31)";
  attrib HML16_32 format=F00247_. label="Corrected age from Individual file(32)";
  attrib HML16_33 format=F00247_. label="Corrected age from Individual file(33)";
  attrib HML16_34 format=F00247_. label="Corrected age from Individual file(34)";
  attrib HML16_35 format=F00247_. label="Corrected age from Individual file(35)";
  attrib HML16_36 format=F00247_. label="Corrected age from Individual file(36)";
  attrib HML16_37 format=F00247_. label="Corrected age from Individual file(37)";
  attrib HML16_38 format=F00247_. label="Corrected age from Individual file(38)";
  attrib HML16_39 format=F00247_. label="Corrected age from Individual file(39)";
  attrib HML16_40 format=F00247_. label="Corrected age from Individual file(40)";
  attrib HML16_41 format=F00247_. label="Corrected age from Individual file(41)";
  attrib HML16_42 format=F00247_. label="Corrected age from Individual file(42)";
  attrib HML16_43 format=F00247_. label="Corrected age from Individual file(43)";
  attrib HML16_44 format=F00247_. label="Corrected age from Individual file(44)";
  attrib HML16_45 format=F00247_. label="Corrected age from Individual file(45)";
  attrib HML16_46 format=F00247_. label="Corrected age from Individual file(46)";
  attrib HML16_47 format=F00247_. label="Corrected age from Individual file(47)";
  attrib HML16_48 format=F00247_. label="Corrected age from Individual file(48)";
  attrib HML16A_01 label="NA - Age in months (for children)(1)";
  attrib HML16A_02 label="NA - Age in months (for children)(2)";
  attrib HML16A_03 label="NA - Age in months (for children)(3)";
  attrib HML16A_04 label="NA - Age in months (for children)(4)";
  attrib HML16A_05 label="NA - Age in months (for children)(5)";
  attrib HML16A_06 label="NA - Age in months (for children)(6)";
  attrib HML16A_07 label="NA - Age in months (for children)(7)";
  attrib HML16A_08 label="NA - Age in months (for children)(8)";
  attrib HML16A_09 label="NA - Age in months (for children)(9)";
  attrib HML16A_10 label="NA - Age in months (for children)(10)";
  attrib HML16A_11 label="NA - Age in months (for children)(11)";
  attrib HML16A_12 label="NA - Age in months (for children)(12)";
  attrib HML16A_13 label="NA - Age in months (for children)(13)";
  attrib HML16A_14 label="NA - Age in months (for children)(14)";
  attrib HML16A_15 label="NA - Age in months (for children)(15)";
  attrib HML16A_16 label="NA - Age in months (for children)(16)";
  attrib HML16A_17 label="NA - Age in months (for children)(17)";
  attrib HML16A_18 label="NA - Age in months (for children)(18)";
  attrib HML16A_19 label="NA - Age in months (for children)(19)";
  attrib HML16A_20 label="NA - Age in months (for children)(20)";
  attrib HML16A_21 label="NA - Age in months (for children)(21)";
  attrib HML16A_22 label="NA - Age in months (for children)(22)";
  attrib HML16A_23 label="NA - Age in months (for children)(23)";
  attrib HML16A_24 label="NA - Age in months (for children)(24)";
  attrib HML16A_25 label="NA - Age in months (for children)(25)";
  attrib HML16A_26 label="NA - Age in months (for children)(26)";
  attrib HML16A_27 label="NA - Age in months (for children)(27)";
  attrib HML16A_28 label="NA - Age in months (for children)(28)";
  attrib HML16A_29 label="NA - Age in months (for children)(29)";
  attrib HML16A_30 label="NA - Age in months (for children)(30)";
  attrib HML16A_31 label="NA - Age in months (for children)(31)";
  attrib HML16A_32 label="NA - Age in months (for children)(32)";
  attrib HML16A_33 label="NA - Age in months (for children)(33)";
  attrib HML16A_34 label="NA - Age in months (for children)(34)";
  attrib HML16A_35 label="NA - Age in months (for children)(35)";
  attrib HML16A_36 label="NA - Age in months (for children)(36)";
  attrib HML16A_37 label="NA - Age in months (for children)(37)";
  attrib HML16A_38 label="NA - Age in months (for children)(38)";
  attrib HML16A_39 label="NA - Age in months (for children)(39)";
  attrib HML16A_40 label="NA - Age in months (for children)(40)";
  attrib HML16A_41 label="NA - Age in months (for children)(41)";
  attrib HML16A_42 label="NA - Age in months (for children)(42)";
  attrib HML16A_43 label="NA - Age in months (for children)(43)";
  attrib HML16A_44 label="NA - Age in months (for children)(44)";
  attrib HML16A_45 label="NA - Age in months (for children)(45)";
  attrib HML16A_46 label="NA - Age in months (for children)(46)";
  attrib HML16A_47 label="NA - Age in months (for children)(47)";
  attrib HML16A_48 label="NA - Age in months (for children)(48)";
  attrib HML17_01 format=F00248_. label="Flag for age from Individual file(1)";
  attrib HML17_02 format=F00248_. label="Flag for age from Individual file(2)";
  attrib HML17_03 format=F00248_. label="Flag for age from Individual file(3)";
  attrib HML17_04 format=F00248_. label="Flag for age from Individual file(4)";
  attrib HML17_05 format=F00248_. label="Flag for age from Individual file(5)";
  attrib HML17_06 format=F00248_. label="Flag for age from Individual file(6)";
  attrib HML17_07 format=F00248_. label="Flag for age from Individual file(7)";
  attrib HML17_08 format=F00248_. label="Flag for age from Individual file(8)";
  attrib HML17_09 format=F00248_. label="Flag for age from Individual file(9)";
  attrib HML17_10 format=F00248_. label="Flag for age from Individual file(10)";
  attrib HML17_11 format=F00248_. label="Flag for age from Individual file(11)";
  attrib HML17_12 format=F00248_. label="Flag for age from Individual file(12)";
  attrib HML17_13 format=F00248_. label="Flag for age from Individual file(13)";
  attrib HML17_14 format=F00248_. label="Flag for age from Individual file(14)";
  attrib HML17_15 format=F00248_. label="Flag for age from Individual file(15)";
  attrib HML17_16 format=F00248_. label="Flag for age from Individual file(16)";
  attrib HML17_17 format=F00248_. label="Flag for age from Individual file(17)";
  attrib HML17_18 format=F00248_. label="Flag for age from Individual file(18)";
  attrib HML17_19 format=F00248_. label="Flag for age from Individual file(19)";
  attrib HML17_20 format=F00248_. label="Flag for age from Individual file(20)";
  attrib HML17_21 format=F00248_. label="Flag for age from Individual file(21)";
  attrib HML17_22 format=F00248_. label="Flag for age from Individual file(22)";
  attrib HML17_23 format=F00248_. label="Flag for age from Individual file(23)";
  attrib HML17_24 format=F00248_. label="Flag for age from Individual file(24)";
  attrib HML17_25 format=F00248_. label="Flag for age from Individual file(25)";
  attrib HML17_26 format=F00248_. label="Flag for age from Individual file(26)";
  attrib HML17_27 format=F00248_. label="Flag for age from Individual file(27)";
  attrib HML17_28 format=F00248_. label="Flag for age from Individual file(28)";
  attrib HML17_29 format=F00248_. label="Flag for age from Individual file(29)";
  attrib HML17_30 format=F00248_. label="Flag for age from Individual file(30)";
  attrib HML17_31 format=F00248_. label="Flag for age from Individual file(31)";
  attrib HML17_32 format=F00248_. label="Flag for age from Individual file(32)";
  attrib HML17_33 format=F00248_. label="Flag for age from Individual file(33)";
  attrib HML17_34 format=F00248_. label="Flag for age from Individual file(34)";
  attrib HML17_35 format=F00248_. label="Flag for age from Individual file(35)";
  attrib HML17_36 format=F00248_. label="Flag for age from Individual file(36)";
  attrib HML17_37 format=F00248_. label="Flag for age from Individual file(37)";
  attrib HML17_38 format=F00248_. label="Flag for age from Individual file(38)";
  attrib HML17_39 format=F00248_. label="Flag for age from Individual file(39)";
  attrib HML17_40 format=F00248_. label="Flag for age from Individual file(40)";
  attrib HML17_41 format=F00248_. label="Flag for age from Individual file(41)";
  attrib HML17_42 format=F00248_. label="Flag for age from Individual file(42)";
  attrib HML17_43 format=F00248_. label="Flag for age from Individual file(43)";
  attrib HML17_44 format=F00248_. label="Flag for age from Individual file(44)";
  attrib HML17_45 format=F00248_. label="Flag for age from Individual file(45)";
  attrib HML17_46 format=F00248_. label="Flag for age from Individual file(46)";
  attrib HML17_47 format=F00248_. label="Flag for age from Individual file(47)";
  attrib HML17_48 format=F00248_. label="Flag for age from Individual file(48)";
  attrib HML18_01 format=F00249_. label="Pregnancy status from Individual file(1)";
  attrib HML18_02 format=F00249_. label="Pregnancy status from Individual file(2)";
  attrib HML18_03 format=F00249_. label="Pregnancy status from Individual file(3)";
  attrib HML18_04 format=F00249_. label="Pregnancy status from Individual file(4)";
  attrib HML18_05 format=F00249_. label="Pregnancy status from Individual file(5)";
  attrib HML18_06 format=F00249_. label="Pregnancy status from Individual file(6)";
  attrib HML18_07 format=F00249_. label="Pregnancy status from Individual file(7)";
  attrib HML18_08 format=F00249_. label="Pregnancy status from Individual file(8)";
  attrib HML18_09 format=F00249_. label="Pregnancy status from Individual file(9)";
  attrib HML18_10 format=F00249_. label="Pregnancy status from Individual file(10)";
  attrib HML18_11 format=F00249_. label="Pregnancy status from Individual file(11)";
  attrib HML18_12 format=F00249_. label="Pregnancy status from Individual file(12)";
  attrib HML18_13 format=F00249_. label="Pregnancy status from Individual file(13)";
  attrib HML18_14 format=F00249_. label="Pregnancy status from Individual file(14)";
  attrib HML18_15 format=F00249_. label="Pregnancy status from Individual file(15)";
  attrib HML18_16 format=F00249_. label="Pregnancy status from Individual file(16)";
  attrib HML18_17 format=F00249_. label="Pregnancy status from Individual file(17)";
  attrib HML18_18 format=F00249_. label="Pregnancy status from Individual file(18)";
  attrib HML18_19 format=F00249_. label="Pregnancy status from Individual file(19)";
  attrib HML18_20 format=F00249_. label="Pregnancy status from Individual file(20)";
  attrib HML18_21 format=F00249_. label="Pregnancy status from Individual file(21)";
  attrib HML18_22 format=F00249_. label="Pregnancy status from Individual file(22)";
  attrib HML18_23 format=F00249_. label="Pregnancy status from Individual file(23)";
  attrib HML18_24 format=F00249_. label="Pregnancy status from Individual file(24)";
  attrib HML18_25 format=F00249_. label="Pregnancy status from Individual file(25)";
  attrib HML18_26 format=F00249_. label="Pregnancy status from Individual file(26)";
  attrib HML18_27 format=F00249_. label="Pregnancy status from Individual file(27)";
  attrib HML18_28 format=F00249_. label="Pregnancy status from Individual file(28)";
  attrib HML18_29 format=F00249_. label="Pregnancy status from Individual file(29)";
  attrib HML18_30 format=F00249_. label="Pregnancy status from Individual file(30)";
  attrib HML18_31 format=F00249_. label="Pregnancy status from Individual file(31)";
  attrib HML18_32 format=F00249_. label="Pregnancy status from Individual file(32)";
  attrib HML18_33 format=F00249_. label="Pregnancy status from Individual file(33)";
  attrib HML18_34 format=F00249_. label="Pregnancy status from Individual file(34)";
  attrib HML18_35 format=F00249_. label="Pregnancy status from Individual file(35)";
  attrib HML18_36 format=F00249_. label="Pregnancy status from Individual file(36)";
  attrib HML18_37 format=F00249_. label="Pregnancy status from Individual file(37)";
  attrib HML18_38 format=F00249_. label="Pregnancy status from Individual file(38)";
  attrib HML18_39 format=F00249_. label="Pregnancy status from Individual file(39)";
  attrib HML18_40 format=F00249_. label="Pregnancy status from Individual file(40)";
  attrib HML18_41 format=F00249_. label="Pregnancy status from Individual file(41)";
  attrib HML18_42 format=F00249_. label="Pregnancy status from Individual file(42)";
  attrib HML18_43 format=F00249_. label="Pregnancy status from Individual file(43)";
  attrib HML18_44 format=F00249_. label="Pregnancy status from Individual file(44)";
  attrib HML18_45 format=F00249_. label="Pregnancy status from Individual file(45)";
  attrib HML18_46 format=F00249_. label="Pregnancy status from Individual file(46)";
  attrib HML18_47 format=F00249_. label="Pregnancy status from Individual file(47)";
  attrib HML18_48 format=F00249_. label="Pregnancy status from Individual file(48)";
  attrib HML19_01 format=F00250_. label="Person slept under an ever-treated net(1)";
  attrib HML19_02 format=F00250_. label="Person slept under an ever-treated net(2)";
  attrib HML19_03 format=F00250_. label="Person slept under an ever-treated net(3)";
  attrib HML19_04 format=F00250_. label="Person slept under an ever-treated net(4)";
  attrib HML19_05 format=F00250_. label="Person slept under an ever-treated net(5)";
  attrib HML19_06 format=F00250_. label="Person slept under an ever-treated net(6)";
  attrib HML19_07 format=F00250_. label="Person slept under an ever-treated net(7)";
  attrib HML19_08 format=F00250_. label="Person slept under an ever-treated net(8)";
  attrib HML19_09 format=F00250_. label="Person slept under an ever-treated net(9)";
  attrib HML19_10 format=F00250_. label="Person slept under an ever-treated net(10)";
  attrib HML19_11 format=F00250_. label="Person slept under an ever-treated net(11)";
  attrib HML19_12 format=F00250_. label="Person slept under an ever-treated net(12)";
  attrib HML19_13 format=F00250_. label="Person slept under an ever-treated net(13)";
  attrib HML19_14 format=F00250_. label="Person slept under an ever-treated net(14)";
  attrib HML19_15 format=F00250_. label="Person slept under an ever-treated net(15)";
  attrib HML19_16 format=F00250_. label="Person slept under an ever-treated net(16)";
  attrib HML19_17 format=F00250_. label="Person slept under an ever-treated net(17)";
  attrib HML19_18 format=F00250_. label="Person slept under an ever-treated net(18)";
  attrib HML19_19 format=F00250_. label="Person slept under an ever-treated net(19)";
  attrib HML19_20 format=F00250_. label="Person slept under an ever-treated net(20)";
  attrib HML19_21 format=F00250_. label="Person slept under an ever-treated net(21)";
  attrib HML19_22 format=F00250_. label="Person slept under an ever-treated net(22)";
  attrib HML19_23 format=F00250_. label="Person slept under an ever-treated net(23)";
  attrib HML19_24 format=F00250_. label="Person slept under an ever-treated net(24)";
  attrib HML19_25 format=F00250_. label="Person slept under an ever-treated net(25)";
  attrib HML19_26 format=F00250_. label="Person slept under an ever-treated net(26)";
  attrib HML19_27 format=F00250_. label="Person slept under an ever-treated net(27)";
  attrib HML19_28 format=F00250_. label="Person slept under an ever-treated net(28)";
  attrib HML19_29 format=F00250_. label="Person slept under an ever-treated net(29)";
  attrib HML19_30 format=F00250_. label="Person slept under an ever-treated net(30)";
  attrib HML19_31 format=F00250_. label="Person slept under an ever-treated net(31)";
  attrib HML19_32 format=F00250_. label="Person slept under an ever-treated net(32)";
  attrib HML19_33 format=F00250_. label="Person slept under an ever-treated net(33)";
  attrib HML19_34 format=F00250_. label="Person slept under an ever-treated net(34)";
  attrib HML19_35 format=F00250_. label="Person slept under an ever-treated net(35)";
  attrib HML19_36 format=F00250_. label="Person slept under an ever-treated net(36)";
  attrib HML19_37 format=F00250_. label="Person slept under an ever-treated net(37)";
  attrib HML19_38 format=F00250_. label="Person slept under an ever-treated net(38)";
  attrib HML19_39 format=F00250_. label="Person slept under an ever-treated net(39)";
  attrib HML19_40 format=F00250_. label="Person slept under an ever-treated net(40)";
  attrib HML19_41 format=F00250_. label="Person slept under an ever-treated net(41)";
  attrib HML19_42 format=F00250_. label="Person slept under an ever-treated net(42)";
  attrib HML19_43 format=F00250_. label="Person slept under an ever-treated net(43)";
  attrib HML19_44 format=F00250_. label="Person slept under an ever-treated net(44)";
  attrib HML19_45 format=F00250_. label="Person slept under an ever-treated net(45)";
  attrib HML19_46 format=F00250_. label="Person slept under an ever-treated net(46)";
  attrib HML19_47 format=F00250_. label="Person slept under an ever-treated net(47)";
  attrib HML19_48 format=F00250_. label="Person slept under an ever-treated net(48)";
  attrib HML20_01 format=F00251_. label="Person slept under an LLIN net(1)";
  attrib HML20_02 format=F00251_. label="Person slept under an LLIN net(2)";
  attrib HML20_03 format=F00251_. label="Person slept under an LLIN net(3)";
  attrib HML20_04 format=F00251_. label="Person slept under an LLIN net(4)";
  attrib HML20_05 format=F00251_. label="Person slept under an LLIN net(5)";
  attrib HML20_06 format=F00251_. label="Person slept under an LLIN net(6)";
  attrib HML20_07 format=F00251_. label="Person slept under an LLIN net(7)";
  attrib HML20_08 format=F00251_. label="Person slept under an LLIN net(8)";
  attrib HML20_09 format=F00251_. label="Person slept under an LLIN net(9)";
  attrib HML20_10 format=F00251_. label="Person slept under an LLIN net(10)";
  attrib HML20_11 format=F00251_. label="Person slept under an LLIN net(11)";
  attrib HML20_12 format=F00251_. label="Person slept under an LLIN net(12)";
  attrib HML20_13 format=F00251_. label="Person slept under an LLIN net(13)";
  attrib HML20_14 format=F00251_. label="Person slept under an LLIN net(14)";
  attrib HML20_15 format=F00251_. label="Person slept under an LLIN net(15)";
  attrib HML20_16 format=F00251_. label="Person slept under an LLIN net(16)";
  attrib HML20_17 format=F00251_. label="Person slept under an LLIN net(17)";
  attrib HML20_18 format=F00251_. label="Person slept under an LLIN net(18)";
  attrib HML20_19 format=F00251_. label="Person slept under an LLIN net(19)";
  attrib HML20_20 format=F00251_. label="Person slept under an LLIN net(20)";
  attrib HML20_21 format=F00251_. label="Person slept under an LLIN net(21)";
  attrib HML20_22 format=F00251_. label="Person slept under an LLIN net(22)";
  attrib HML20_23 format=F00251_. label="Person slept under an LLIN net(23)";
  attrib HML20_24 format=F00251_. label="Person slept under an LLIN net(24)";
  attrib HML20_25 format=F00251_. label="Person slept under an LLIN net(25)";
  attrib HML20_26 format=F00251_. label="Person slept under an LLIN net(26)";
  attrib HML20_27 format=F00251_. label="Person slept under an LLIN net(27)";
  attrib HML20_28 format=F00251_. label="Person slept under an LLIN net(28)";
  attrib HML20_29 format=F00251_. label="Person slept under an LLIN net(29)";
  attrib HML20_30 format=F00251_. label="Person slept under an LLIN net(30)";
  attrib HML20_31 format=F00251_. label="Person slept under an LLIN net(31)";
  attrib HML20_32 format=F00251_. label="Person slept under an LLIN net(32)";
  attrib HML20_33 format=F00251_. label="Person slept under an LLIN net(33)";
  attrib HML20_34 format=F00251_. label="Person slept under an LLIN net(34)";
  attrib HML20_35 format=F00251_. label="Person slept under an LLIN net(35)";
  attrib HML20_36 format=F00251_. label="Person slept under an LLIN net(36)";
  attrib HML20_37 format=F00251_. label="Person slept under an LLIN net(37)";
  attrib HML20_38 format=F00251_. label="Person slept under an LLIN net(38)";
  attrib HML20_39 format=F00251_. label="Person slept under an LLIN net(39)";
  attrib HML20_40 format=F00251_. label="Person slept under an LLIN net(40)";
  attrib HML20_41 format=F00251_. label="Person slept under an LLIN net(41)";
  attrib HML20_42 format=F00251_. label="Person slept under an LLIN net(42)";
  attrib HML20_43 format=F00251_. label="Person slept under an LLIN net(43)";
  attrib HML20_44 format=F00251_. label="Person slept under an LLIN net(44)";
  attrib HML20_45 format=F00251_. label="Person slept under an LLIN net(45)";
  attrib HML20_46 format=F00251_. label="Person slept under an LLIN net(46)";
  attrib HML20_47 format=F00251_. label="Person slept under an LLIN net(47)";
  attrib HML20_48 format=F00251_. label="Person slept under an LLIN net(48)";
  attrib HML30_01 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(1)";
  attrib HML30_02 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(2)";
  attrib HML30_03 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(3)";
  attrib HML30_04 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(4)";
  attrib HML30_05 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(5)";
  attrib HML30_06 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(6)";
  attrib HML30_07 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(7)";
  attrib HML30_08 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(8)";
  attrib HML30_09 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(9)";
  attrib HML30_10 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(10)";
  attrib HML30_11 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(11)";
  attrib HML30_12 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(12)";
  attrib HML30_13 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(13)";
  attrib HML30_14 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(14)";
  attrib HML30_15 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(15)";
  attrib HML30_16 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(16)";
  attrib HML30_17 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(17)";
  attrib HML30_18 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(18)";
  attrib HML30_19 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(19)";
  attrib HML30_20 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(20)";
  attrib HML30_21 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(21)";
  attrib HML30_22 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(22)";
  attrib HML30_23 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(23)";
  attrib HML30_24 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(24)";
  attrib HML30_25 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(25)";
  attrib HML30_26 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(26)";
  attrib HML30_27 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(27)";
  attrib HML30_28 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(28)";
  attrib HML30_29 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(29)";
  attrib HML30_30 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(30)";
  attrib HML30_31 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(31)";
  attrib HML30_32 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(32)";
  attrib HML30_33 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(33)";
  attrib HML30_34 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(34)";
  attrib HML30_35 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(35)";
  attrib HML30_36 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(36)";
  attrib HML30_37 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(37)";
  attrib HML30_38 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(38)";
  attrib HML30_39 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(39)";
  attrib HML30_40 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(40)";
  attrib HML30_41 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(41)";
  attrib HML30_42 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(42)";
  attrib HML30_43 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(43)";
  attrib HML30_44 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(44)";
  attrib HML30_45 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(45)";
  attrib HML30_46 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(46)";
  attrib HML30_47 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(47)";
  attrib HML30_48 format=F00252_. label="NA - Line number of parent/caretaker (for malaria testing)(48)";
  attrib HML31_01 format=F00253_. label="NA - Read consent statement for malaria(1)";
  attrib HML31_02 format=F00253_. label="NA - Read consent statement for malaria(2)";
  attrib HML31_03 format=F00253_. label="NA - Read consent statement for malaria(3)";
  attrib HML31_04 format=F00253_. label="NA - Read consent statement for malaria(4)";
  attrib HML31_05 format=F00253_. label="NA - Read consent statement for malaria(5)";
  attrib HML31_06 format=F00253_. label="NA - Read consent statement for malaria(6)";
  attrib HML31_07 format=F00253_. label="NA - Read consent statement for malaria(7)";
  attrib HML31_08 format=F00253_. label="NA - Read consent statement for malaria(8)";
  attrib HML31_09 format=F00253_. label="NA - Read consent statement for malaria(9)";
  attrib HML31_10 format=F00253_. label="NA - Read consent statement for malaria(10)";
  attrib HML31_11 format=F00253_. label="NA - Read consent statement for malaria(11)";
  attrib HML31_12 format=F00253_. label="NA - Read consent statement for malaria(12)";
  attrib HML31_13 format=F00253_. label="NA - Read consent statement for malaria(13)";
  attrib HML31_14 format=F00253_. label="NA - Read consent statement for malaria(14)";
  attrib HML31_15 format=F00253_. label="NA - Read consent statement for malaria(15)";
  attrib HML31_16 format=F00253_. label="NA - Read consent statement for malaria(16)";
  attrib HML31_17 format=F00253_. label="NA - Read consent statement for malaria(17)";
  attrib HML31_18 format=F00253_. label="NA - Read consent statement for malaria(18)";
  attrib HML31_19 format=F00253_. label="NA - Read consent statement for malaria(19)";
  attrib HML31_20 format=F00253_. label="NA - Read consent statement for malaria(20)";
  attrib HML31_21 format=F00253_. label="NA - Read consent statement for malaria(21)";
  attrib HML31_22 format=F00253_. label="NA - Read consent statement for malaria(22)";
  attrib HML31_23 format=F00253_. label="NA - Read consent statement for malaria(23)";
  attrib HML31_24 format=F00253_. label="NA - Read consent statement for malaria(24)";
  attrib HML31_25 format=F00253_. label="NA - Read consent statement for malaria(25)";
  attrib HML31_26 format=F00253_. label="NA - Read consent statement for malaria(26)";
  attrib HML31_27 format=F00253_. label="NA - Read consent statement for malaria(27)";
  attrib HML31_28 format=F00253_. label="NA - Read consent statement for malaria(28)";
  attrib HML31_29 format=F00253_. label="NA - Read consent statement for malaria(29)";
  attrib HML31_30 format=F00253_. label="NA - Read consent statement for malaria(30)";
  attrib HML31_31 format=F00253_. label="NA - Read consent statement for malaria(31)";
  attrib HML31_32 format=F00253_. label="NA - Read consent statement for malaria(32)";
  attrib HML31_33 format=F00253_. label="NA - Read consent statement for malaria(33)";
  attrib HML31_34 format=F00253_. label="NA - Read consent statement for malaria(34)";
  attrib HML31_35 format=F00253_. label="NA - Read consent statement for malaria(35)";
  attrib HML31_36 format=F00253_. label="NA - Read consent statement for malaria(36)";
  attrib HML31_37 format=F00253_. label="NA - Read consent statement for malaria(37)";
  attrib HML31_38 format=F00253_. label="NA - Read consent statement for malaria(38)";
  attrib HML31_39 format=F00253_. label="NA - Read consent statement for malaria(39)";
  attrib HML31_40 format=F00253_. label="NA - Read consent statement for malaria(40)";
  attrib HML31_41 format=F00253_. label="NA - Read consent statement for malaria(41)";
  attrib HML31_42 format=F00253_. label="NA - Read consent statement for malaria(42)";
  attrib HML31_43 format=F00253_. label="NA - Read consent statement for malaria(43)";
  attrib HML31_44 format=F00253_. label="NA - Read consent statement for malaria(44)";
  attrib HML31_45 format=F00253_. label="NA - Read consent statement for malaria(45)";
  attrib HML31_46 format=F00253_. label="NA - Read consent statement for malaria(46)";
  attrib HML31_47 format=F00253_. label="NA - Read consent statement for malaria(47)";
  attrib HML31_48 format=F00253_. label="NA - Read consent statement for malaria(48)";
  attrib HML32_01 format=F00254_. label="NA - Final result of malaria from blood smear test(1)";
  attrib HML32_02 format=F00254_. label="NA - Final result of malaria from blood smear test(2)";
  attrib HML32_03 format=F00254_. label="NA - Final result of malaria from blood smear test(3)";
  attrib HML32_04 format=F00254_. label="NA - Final result of malaria from blood smear test(4)";
  attrib HML32_05 format=F00254_. label="NA - Final result of malaria from blood smear test(5)";
  attrib HML32_06 format=F00254_. label="NA - Final result of malaria from blood smear test(6)";
  attrib HML32_07 format=F00254_. label="NA - Final result of malaria from blood smear test(7)";
  attrib HML32_08 format=F00254_. label="NA - Final result of malaria from blood smear test(8)";
  attrib HML32_09 format=F00254_. label="NA - Final result of malaria from blood smear test(9)";
  attrib HML32_10 format=F00254_. label="NA - Final result of malaria from blood smear test(10)";
  attrib HML32_11 format=F00254_. label="NA - Final result of malaria from blood smear test(11)";
  attrib HML32_12 format=F00254_. label="NA - Final result of malaria from blood smear test(12)";
  attrib HML32_13 format=F00254_. label="NA - Final result of malaria from blood smear test(13)";
  attrib HML32_14 format=F00254_. label="NA - Final result of malaria from blood smear test(14)";
  attrib HML32_15 format=F00254_. label="NA - Final result of malaria from blood smear test(15)";
  attrib HML32_16 format=F00254_. label="NA - Final result of malaria from blood smear test(16)";
  attrib HML32_17 format=F00254_. label="NA - Final result of malaria from blood smear test(17)";
  attrib HML32_18 format=F00254_. label="NA - Final result of malaria from blood smear test(18)";
  attrib HML32_19 format=F00254_. label="NA - Final result of malaria from blood smear test(19)";
  attrib HML32_20 format=F00254_. label="NA - Final result of malaria from blood smear test(20)";
  attrib HML32_21 format=F00254_. label="NA - Final result of malaria from blood smear test(21)";
  attrib HML32_22 format=F00254_. label="NA - Final result of malaria from blood smear test(22)";
  attrib HML32_23 format=F00254_. label="NA - Final result of malaria from blood smear test(23)";
  attrib HML32_24 format=F00254_. label="NA - Final result of malaria from blood smear test(24)";
  attrib HML32_25 format=F00254_. label="NA - Final result of malaria from blood smear test(25)";
  attrib HML32_26 format=F00254_. label="NA - Final result of malaria from blood smear test(26)";
  attrib HML32_27 format=F00254_. label="NA - Final result of malaria from blood smear test(27)";
  attrib HML32_28 format=F00254_. label="NA - Final result of malaria from blood smear test(28)";
  attrib HML32_29 format=F00254_. label="NA - Final result of malaria from blood smear test(29)";
  attrib HML32_30 format=F00254_. label="NA - Final result of malaria from blood smear test(30)";
  attrib HML32_31 format=F00254_. label="NA - Final result of malaria from blood smear test(31)";
  attrib HML32_32 format=F00254_. label="NA - Final result of malaria from blood smear test(32)";
  attrib HML32_33 format=F00254_. label="NA - Final result of malaria from blood smear test(33)";
  attrib HML32_34 format=F00254_. label="NA - Final result of malaria from blood smear test(34)";
  attrib HML32_35 format=F00254_. label="NA - Final result of malaria from blood smear test(35)";
  attrib HML32_36 format=F00254_. label="NA - Final result of malaria from blood smear test(36)";
  attrib HML32_37 format=F00254_. label="NA - Final result of malaria from blood smear test(37)";
  attrib HML32_38 format=F00254_. label="NA - Final result of malaria from blood smear test(38)";
  attrib HML32_39 format=F00254_. label="NA - Final result of malaria from blood smear test(39)";
  attrib HML32_40 format=F00254_. label="NA - Final result of malaria from blood smear test(40)";
  attrib HML32_41 format=F00254_. label="NA - Final result of malaria from blood smear test(41)";
  attrib HML32_42 format=F00254_. label="NA - Final result of malaria from blood smear test(42)";
  attrib HML32_43 format=F00254_. label="NA - Final result of malaria from blood smear test(43)";
  attrib HML32_44 format=F00254_. label="NA - Final result of malaria from blood smear test(44)";
  attrib HML32_45 format=F00254_. label="NA - Final result of malaria from blood smear test(45)";
  attrib HML32_46 format=F00254_. label="NA - Final result of malaria from blood smear test(46)";
  attrib HML32_47 format=F00254_. label="NA - Final result of malaria from blood smear test(47)";
  attrib HML32_48 format=F00254_. label="NA - Final result of malaria from blood smear test(48)";
  attrib HML32A_01 format=F00255_. label="NA - Presence of species: falciparum (Pf)(1)";
  attrib HML32A_02 format=F00255_. label="NA - Presence of species: falciparum (Pf)(2)";
  attrib HML32A_03 format=F00255_. label="NA - Presence of species: falciparum (Pf)(3)";
  attrib HML32A_04 format=F00255_. label="NA - Presence of species: falciparum (Pf)(4)";
  attrib HML32A_05 format=F00255_. label="NA - Presence of species: falciparum (Pf)(5)";
  attrib HML32A_06 format=F00255_. label="NA - Presence of species: falciparum (Pf)(6)";
  attrib HML32A_07 format=F00255_. label="NA - Presence of species: falciparum (Pf)(7)";
  attrib HML32A_08 format=F00255_. label="NA - Presence of species: falciparum (Pf)(8)";
  attrib HML32A_09 format=F00255_. label="NA - Presence of species: falciparum (Pf)(9)";
  attrib HML32A_10 format=F00255_. label="NA - Presence of species: falciparum (Pf)(10)";
  attrib HML32A_11 format=F00255_. label="NA - Presence of species: falciparum (Pf)(11)";
  attrib HML32A_12 format=F00255_. label="NA - Presence of species: falciparum (Pf)(12)";
  attrib HML32A_13 format=F00255_. label="NA - Presence of species: falciparum (Pf)(13)";
  attrib HML32A_14 format=F00255_. label="NA - Presence of species: falciparum (Pf)(14)";
  attrib HML32A_15 format=F00255_. label="NA - Presence of species: falciparum (Pf)(15)";
  attrib HML32A_16 format=F00255_. label="NA - Presence of species: falciparum (Pf)(16)";
  attrib HML32A_17 format=F00255_. label="NA - Presence of species: falciparum (Pf)(17)";
  attrib HML32A_18 format=F00255_. label="NA - Presence of species: falciparum (Pf)(18)";
  attrib HML32A_19 format=F00255_. label="NA - Presence of species: falciparum (Pf)(19)";
  attrib HML32A_20 format=F00255_. label="NA - Presence of species: falciparum (Pf)(20)";
  attrib HML32A_21 format=F00255_. label="NA - Presence of species: falciparum (Pf)(21)";
  attrib HML32A_22 format=F00255_. label="NA - Presence of species: falciparum (Pf)(22)";
  attrib HML32A_23 format=F00255_. label="NA - Presence of species: falciparum (Pf)(23)";
  attrib HML32A_24 format=F00255_. label="NA - Presence of species: falciparum (Pf)(24)";
  attrib HML32A_25 format=F00255_. label="NA - Presence of species: falciparum (Pf)(25)";
  attrib HML32A_26 format=F00255_. label="NA - Presence of species: falciparum (Pf)(26)";
  attrib HML32A_27 format=F00255_. label="NA - Presence of species: falciparum (Pf)(27)";
  attrib HML32A_28 format=F00255_. label="NA - Presence of species: falciparum (Pf)(28)";
  attrib HML32A_29 format=F00255_. label="NA - Presence of species: falciparum (Pf)(29)";
  attrib HML32A_30 format=F00255_. label="NA - Presence of species: falciparum (Pf)(30)";
  attrib HML32A_31 format=F00255_. label="NA - Presence of species: falciparum (Pf)(31)";
  attrib HML32A_32 format=F00255_. label="NA - Presence of species: falciparum (Pf)(32)";
  attrib HML32A_33 format=F00255_. label="NA - Presence of species: falciparum (Pf)(33)";
  attrib HML32A_34 format=F00255_. label="NA - Presence of species: falciparum (Pf)(34)";
  attrib HML32A_35 format=F00255_. label="NA - Presence of species: falciparum (Pf)(35)";
  attrib HML32A_36 format=F00255_. label="NA - Presence of species: falciparum (Pf)(36)";
  attrib HML32A_37 format=F00255_. label="NA - Presence of species: falciparum (Pf)(37)";
  attrib HML32A_38 format=F00255_. label="NA - Presence of species: falciparum (Pf)(38)";
  attrib HML32A_39 format=F00255_. label="NA - Presence of species: falciparum (Pf)(39)";
  attrib HML32A_40 format=F00255_. label="NA - Presence of species: falciparum (Pf)(40)";
  attrib HML32A_41 format=F00255_. label="NA - Presence of species: falciparum (Pf)(41)";
  attrib HML32A_42 format=F00255_. label="NA - Presence of species: falciparum (Pf)(42)";
  attrib HML32A_43 format=F00255_. label="NA - Presence of species: falciparum (Pf)(43)";
  attrib HML32A_44 format=F00255_. label="NA - Presence of species: falciparum (Pf)(44)";
  attrib HML32A_45 format=F00255_. label="NA - Presence of species: falciparum (Pf)(45)";
  attrib HML32A_46 format=F00255_. label="NA - Presence of species: falciparum (Pf)(46)";
  attrib HML32A_47 format=F00255_. label="NA - Presence of species: falciparum (Pf)(47)";
  attrib HML32A_48 format=F00255_. label="NA - Presence of species: falciparum (Pf)(48)";
  attrib HML32B_01 format=F00256_. label="NA - Presence of species: malariae (Pm)(1)";
  attrib HML32B_02 format=F00256_. label="NA - Presence of species: malariae (Pm)(2)";
  attrib HML32B_03 format=F00256_. label="NA - Presence of species: malariae (Pm)(3)";
  attrib HML32B_04 format=F00256_. label="NA - Presence of species: malariae (Pm)(4)";
  attrib HML32B_05 format=F00256_. label="NA - Presence of species: malariae (Pm)(5)";
  attrib HML32B_06 format=F00256_. label="NA - Presence of species: malariae (Pm)(6)";
  attrib HML32B_07 format=F00256_. label="NA - Presence of species: malariae (Pm)(7)";
  attrib HML32B_08 format=F00256_. label="NA - Presence of species: malariae (Pm)(8)";
  attrib HML32B_09 format=F00256_. label="NA - Presence of species: malariae (Pm)(9)";
  attrib HML32B_10 format=F00256_. label="NA - Presence of species: malariae (Pm)(10)";
  attrib HML32B_11 format=F00256_. label="NA - Presence of species: malariae (Pm)(11)";
  attrib HML32B_12 format=F00256_. label="NA - Presence of species: malariae (Pm)(12)";
  attrib HML32B_13 format=F00256_. label="NA - Presence of species: malariae (Pm)(13)";
  attrib HML32B_14 format=F00256_. label="NA - Presence of species: malariae (Pm)(14)";
  attrib HML32B_15 format=F00256_. label="NA - Presence of species: malariae (Pm)(15)";
  attrib HML32B_16 format=F00256_. label="NA - Presence of species: malariae (Pm)(16)";
  attrib HML32B_17 format=F00256_. label="NA - Presence of species: malariae (Pm)(17)";
  attrib HML32B_18 format=F00256_. label="NA - Presence of species: malariae (Pm)(18)";
  attrib HML32B_19 format=F00256_. label="NA - Presence of species: malariae (Pm)(19)";
  attrib HML32B_20 format=F00256_. label="NA - Presence of species: malariae (Pm)(20)";
  attrib HML32B_21 format=F00256_. label="NA - Presence of species: malariae (Pm)(21)";
  attrib HML32B_22 format=F00256_. label="NA - Presence of species: malariae (Pm)(22)";
  attrib HML32B_23 format=F00256_. label="NA - Presence of species: malariae (Pm)(23)";
  attrib HML32B_24 format=F00256_. label="NA - Presence of species: malariae (Pm)(24)";
  attrib HML32B_25 format=F00256_. label="NA - Presence of species: malariae (Pm)(25)";
  attrib HML32B_26 format=F00256_. label="NA - Presence of species: malariae (Pm)(26)";
  attrib HML32B_27 format=F00256_. label="NA - Presence of species: malariae (Pm)(27)";
  attrib HML32B_28 format=F00256_. label="NA - Presence of species: malariae (Pm)(28)";
  attrib HML32B_29 format=F00256_. label="NA - Presence of species: malariae (Pm)(29)";
  attrib HML32B_30 format=F00256_. label="NA - Presence of species: malariae (Pm)(30)";
  attrib HML32B_31 format=F00256_. label="NA - Presence of species: malariae (Pm)(31)";
  attrib HML32B_32 format=F00256_. label="NA - Presence of species: malariae (Pm)(32)";
  attrib HML32B_33 format=F00256_. label="NA - Presence of species: malariae (Pm)(33)";
  attrib HML32B_34 format=F00256_. label="NA - Presence of species: malariae (Pm)(34)";
  attrib HML32B_35 format=F00256_. label="NA - Presence of species: malariae (Pm)(35)";
  attrib HML32B_36 format=F00256_. label="NA - Presence of species: malariae (Pm)(36)";
  attrib HML32B_37 format=F00256_. label="NA - Presence of species: malariae (Pm)(37)";
  attrib HML32B_38 format=F00256_. label="NA - Presence of species: malariae (Pm)(38)";
  attrib HML32B_39 format=F00256_. label="NA - Presence of species: malariae (Pm)(39)";
  attrib HML32B_40 format=F00256_. label="NA - Presence of species: malariae (Pm)(40)";
  attrib HML32B_41 format=F00256_. label="NA - Presence of species: malariae (Pm)(41)";
  attrib HML32B_42 format=F00256_. label="NA - Presence of species: malariae (Pm)(42)";
  attrib HML32B_43 format=F00256_. label="NA - Presence of species: malariae (Pm)(43)";
  attrib HML32B_44 format=F00256_. label="NA - Presence of species: malariae (Pm)(44)";
  attrib HML32B_45 format=F00256_. label="NA - Presence of species: malariae (Pm)(45)";
  attrib HML32B_46 format=F00256_. label="NA - Presence of species: malariae (Pm)(46)";
  attrib HML32B_47 format=F00256_. label="NA - Presence of species: malariae (Pm)(47)";
  attrib HML32B_48 format=F00256_. label="NA - Presence of species: malariae (Pm)(48)";
  attrib HML32C_01 format=F00257_. label="NA - Presence of species: ovale (Po)(1)";
  attrib HML32C_02 format=F00257_. label="NA - Presence of species: ovale (Po)(2)";
  attrib HML32C_03 format=F00257_. label="NA - Presence of species: ovale (Po)(3)";
  attrib HML32C_04 format=F00257_. label="NA - Presence of species: ovale (Po)(4)";
  attrib HML32C_05 format=F00257_. label="NA - Presence of species: ovale (Po)(5)";
  attrib HML32C_06 format=F00257_. label="NA - Presence of species: ovale (Po)(6)";
  attrib HML32C_07 format=F00257_. label="NA - Presence of species: ovale (Po)(7)";
  attrib HML32C_08 format=F00257_. label="NA - Presence of species: ovale (Po)(8)";
  attrib HML32C_09 format=F00257_. label="NA - Presence of species: ovale (Po)(9)";
  attrib HML32C_10 format=F00257_. label="NA - Presence of species: ovale (Po)(10)";
  attrib HML32C_11 format=F00257_. label="NA - Presence of species: ovale (Po)(11)";
  attrib HML32C_12 format=F00257_. label="NA - Presence of species: ovale (Po)(12)";
  attrib HML32C_13 format=F00257_. label="NA - Presence of species: ovale (Po)(13)";
  attrib HML32C_14 format=F00257_. label="NA - Presence of species: ovale (Po)(14)";
  attrib HML32C_15 format=F00257_. label="NA - Presence of species: ovale (Po)(15)";
  attrib HML32C_16 format=F00257_. label="NA - Presence of species: ovale (Po)(16)";
  attrib HML32C_17 format=F00257_. label="NA - Presence of species: ovale (Po)(17)";
  attrib HML32C_18 format=F00257_. label="NA - Presence of species: ovale (Po)(18)";
  attrib HML32C_19 format=F00257_. label="NA - Presence of species: ovale (Po)(19)";
  attrib HML32C_20 format=F00257_. label="NA - Presence of species: ovale (Po)(20)";
  attrib HML32C_21 format=F00257_. label="NA - Presence of species: ovale (Po)(21)";
  attrib HML32C_22 format=F00257_. label="NA - Presence of species: ovale (Po)(22)";
  attrib HML32C_23 format=F00257_. label="NA - Presence of species: ovale (Po)(23)";
  attrib HML32C_24 format=F00257_. label="NA - Presence of species: ovale (Po)(24)";
  attrib HML32C_25 format=F00257_. label="NA - Presence of species: ovale (Po)(25)";
  attrib HML32C_26 format=F00257_. label="NA - Presence of species: ovale (Po)(26)";
  attrib HML32C_27 format=F00257_. label="NA - Presence of species: ovale (Po)(27)";
  attrib HML32C_28 format=F00257_. label="NA - Presence of species: ovale (Po)(28)";
  attrib HML32C_29 format=F00257_. label="NA - Presence of species: ovale (Po)(29)";
  attrib HML32C_30 format=F00257_. label="NA - Presence of species: ovale (Po)(30)";
  attrib HML32C_31 format=F00257_. label="NA - Presence of species: ovale (Po)(31)";
  attrib HML32C_32 format=F00257_. label="NA - Presence of species: ovale (Po)(32)";
  attrib HML32C_33 format=F00257_. label="NA - Presence of species: ovale (Po)(33)";
  attrib HML32C_34 format=F00257_. label="NA - Presence of species: ovale (Po)(34)";
  attrib HML32C_35 format=F00257_. label="NA - Presence of species: ovale (Po)(35)";
  attrib HML32C_36 format=F00257_. label="NA - Presence of species: ovale (Po)(36)";
  attrib HML32C_37 format=F00257_. label="NA - Presence of species: ovale (Po)(37)";
  attrib HML32C_38 format=F00257_. label="NA - Presence of species: ovale (Po)(38)";
  attrib HML32C_39 format=F00257_. label="NA - Presence of species: ovale (Po)(39)";
  attrib HML32C_40 format=F00257_. label="NA - Presence of species: ovale (Po)(40)";
  attrib HML32C_41 format=F00257_. label="NA - Presence of species: ovale (Po)(41)";
  attrib HML32C_42 format=F00257_. label="NA - Presence of species: ovale (Po)(42)";
  attrib HML32C_43 format=F00257_. label="NA - Presence of species: ovale (Po)(43)";
  attrib HML32C_44 format=F00257_. label="NA - Presence of species: ovale (Po)(44)";
  attrib HML32C_45 format=F00257_. label="NA - Presence of species: ovale (Po)(45)";
  attrib HML32C_46 format=F00257_. label="NA - Presence of species: ovale (Po)(46)";
  attrib HML32C_47 format=F00257_. label="NA - Presence of species: ovale (Po)(47)";
  attrib HML32C_48 format=F00257_. label="NA - Presence of species: ovale (Po)(48)";
  attrib HML32D_01 format=F00258_. label="NA - Presence of species: vivax (Pv)(1)";
  attrib HML32D_02 format=F00258_. label="NA - Presence of species: vivax (Pv)(2)";
  attrib HML32D_03 format=F00258_. label="NA - Presence of species: vivax (Pv)(3)";
  attrib HML32D_04 format=F00258_. label="NA - Presence of species: vivax (Pv)(4)";
  attrib HML32D_05 format=F00258_. label="NA - Presence of species: vivax (Pv)(5)";
  attrib HML32D_06 format=F00258_. label="NA - Presence of species: vivax (Pv)(6)";
  attrib HML32D_07 format=F00258_. label="NA - Presence of species: vivax (Pv)(7)";
  attrib HML32D_08 format=F00258_. label="NA - Presence of species: vivax (Pv)(8)";
  attrib HML32D_09 format=F00258_. label="NA - Presence of species: vivax (Pv)(9)";
  attrib HML32D_10 format=F00258_. label="NA - Presence of species: vivax (Pv)(10)";
  attrib HML32D_11 format=F00258_. label="NA - Presence of species: vivax (Pv)(11)";
  attrib HML32D_12 format=F00258_. label="NA - Presence of species: vivax (Pv)(12)";
  attrib HML32D_13 format=F00258_. label="NA - Presence of species: vivax (Pv)(13)";
  attrib HML32D_14 format=F00258_. label="NA - Presence of species: vivax (Pv)(14)";
  attrib HML32D_15 format=F00258_. label="NA - Presence of species: vivax (Pv)(15)";
  attrib HML32D_16 format=F00258_. label="NA - Presence of species: vivax (Pv)(16)";
  attrib HML32D_17 format=F00258_. label="NA - Presence of species: vivax (Pv)(17)";
  attrib HML32D_18 format=F00258_. label="NA - Presence of species: vivax (Pv)(18)";
  attrib HML32D_19 format=F00258_. label="NA - Presence of species: vivax (Pv)(19)";
  attrib HML32D_20 format=F00258_. label="NA - Presence of species: vivax (Pv)(20)";
  attrib HML32D_21 format=F00258_. label="NA - Presence of species: vivax (Pv)(21)";
  attrib HML32D_22 format=F00258_. label="NA - Presence of species: vivax (Pv)(22)";
  attrib HML32D_23 format=F00258_. label="NA - Presence of species: vivax (Pv)(23)";
  attrib HML32D_24 format=F00258_. label="NA - Presence of species: vivax (Pv)(24)";
  attrib HML32D_25 format=F00258_. label="NA - Presence of species: vivax (Pv)(25)";
  attrib HML32D_26 format=F00258_. label="NA - Presence of species: vivax (Pv)(26)";
  attrib HML32D_27 format=F00258_. label="NA - Presence of species: vivax (Pv)(27)";
  attrib HML32D_28 format=F00258_. label="NA - Presence of species: vivax (Pv)(28)";
  attrib HML32D_29 format=F00258_. label="NA - Presence of species: vivax (Pv)(29)";
  attrib HML32D_30 format=F00258_. label="NA - Presence of species: vivax (Pv)(30)";
  attrib HML32D_31 format=F00258_. label="NA - Presence of species: vivax (Pv)(31)";
  attrib HML32D_32 format=F00258_. label="NA - Presence of species: vivax (Pv)(32)";
  attrib HML32D_33 format=F00258_. label="NA - Presence of species: vivax (Pv)(33)";
  attrib HML32D_34 format=F00258_. label="NA - Presence of species: vivax (Pv)(34)";
  attrib HML32D_35 format=F00258_. label="NA - Presence of species: vivax (Pv)(35)";
  attrib HML32D_36 format=F00258_. label="NA - Presence of species: vivax (Pv)(36)";
  attrib HML32D_37 format=F00258_. label="NA - Presence of species: vivax (Pv)(37)";
  attrib HML32D_38 format=F00258_. label="NA - Presence of species: vivax (Pv)(38)";
  attrib HML32D_39 format=F00258_. label="NA - Presence of species: vivax (Pv)(39)";
  attrib HML32D_40 format=F00258_. label="NA - Presence of species: vivax (Pv)(40)";
  attrib HML32D_41 format=F00258_. label="NA - Presence of species: vivax (Pv)(41)";
  attrib HML32D_42 format=F00258_. label="NA - Presence of species: vivax (Pv)(42)";
  attrib HML32D_43 format=F00258_. label="NA - Presence of species: vivax (Pv)(43)";
  attrib HML32D_44 format=F00258_. label="NA - Presence of species: vivax (Pv)(44)";
  attrib HML32D_45 format=F00258_. label="NA - Presence of species: vivax (Pv)(45)";
  attrib HML32D_46 format=F00258_. label="NA - Presence of species: vivax (Pv)(46)";
  attrib HML32D_47 format=F00258_. label="NA - Presence of species: vivax (Pv)(47)";
  attrib HML32D_48 format=F00258_. label="NA - Presence of species: vivax (Pv)(48)";
  attrib HML32E_01 format=F00259_. label="NA - Presence of species: CS(1)";
  attrib HML32E_02 format=F00259_. label="NA - Presence of species: CS(2)";
  attrib HML32E_03 format=F00259_. label="NA - Presence of species: CS(3)";
  attrib HML32E_04 format=F00259_. label="NA - Presence of species: CS(4)";
  attrib HML32E_05 format=F00259_. label="NA - Presence of species: CS(5)";
  attrib HML32E_06 format=F00259_. label="NA - Presence of species: CS(6)";
  attrib HML32E_07 format=F00259_. label="NA - Presence of species: CS(7)";
  attrib HML32E_08 format=F00259_. label="NA - Presence of species: CS(8)";
  attrib HML32E_09 format=F00259_. label="NA - Presence of species: CS(9)";
  attrib HML32E_10 format=F00259_. label="NA - Presence of species: CS(10)";
  attrib HML32E_11 format=F00259_. label="NA - Presence of species: CS(11)";
  attrib HML32E_12 format=F00259_. label="NA - Presence of species: CS(12)";
  attrib HML32E_13 format=F00259_. label="NA - Presence of species: CS(13)";
  attrib HML32E_14 format=F00259_. label="NA - Presence of species: CS(14)";
  attrib HML32E_15 format=F00259_. label="NA - Presence of species: CS(15)";
  attrib HML32E_16 format=F00259_. label="NA - Presence of species: CS(16)";
  attrib HML32E_17 format=F00259_. label="NA - Presence of species: CS(17)";
  attrib HML32E_18 format=F00259_. label="NA - Presence of species: CS(18)";
  attrib HML32E_19 format=F00259_. label="NA - Presence of species: CS(19)";
  attrib HML32E_20 format=F00259_. label="NA - Presence of species: CS(20)";
  attrib HML32E_21 format=F00259_. label="NA - Presence of species: CS(21)";
  attrib HML32E_22 format=F00259_. label="NA - Presence of species: CS(22)";
  attrib HML32E_23 format=F00259_. label="NA - Presence of species: CS(23)";
  attrib HML32E_24 format=F00259_. label="NA - Presence of species: CS(24)";
  attrib HML32E_25 format=F00259_. label="NA - Presence of species: CS(25)";
  attrib HML32E_26 format=F00259_. label="NA - Presence of species: CS(26)";
  attrib HML32E_27 format=F00259_. label="NA - Presence of species: CS(27)";
  attrib HML32E_28 format=F00259_. label="NA - Presence of species: CS(28)";
  attrib HML32E_29 format=F00259_. label="NA - Presence of species: CS(29)";
  attrib HML32E_30 format=F00259_. label="NA - Presence of species: CS(30)";
  attrib HML32E_31 format=F00259_. label="NA - Presence of species: CS(31)";
  attrib HML32E_32 format=F00259_. label="NA - Presence of species: CS(32)";
  attrib HML32E_33 format=F00259_. label="NA - Presence of species: CS(33)";
  attrib HML32E_34 format=F00259_. label="NA - Presence of species: CS(34)";
  attrib HML32E_35 format=F00259_. label="NA - Presence of species: CS(35)";
  attrib HML32E_36 format=F00259_. label="NA - Presence of species: CS(36)";
  attrib HML32E_37 format=F00259_. label="NA - Presence of species: CS(37)";
  attrib HML32E_38 format=F00259_. label="NA - Presence of species: CS(38)";
  attrib HML32E_39 format=F00259_. label="NA - Presence of species: CS(39)";
  attrib HML32E_40 format=F00259_. label="NA - Presence of species: CS(40)";
  attrib HML32E_41 format=F00259_. label="NA - Presence of species: CS(41)";
  attrib HML32E_42 format=F00259_. label="NA - Presence of species: CS(42)";
  attrib HML32E_43 format=F00259_. label="NA - Presence of species: CS(43)";
  attrib HML32E_44 format=F00259_. label="NA - Presence of species: CS(44)";
  attrib HML32E_45 format=F00259_. label="NA - Presence of species: CS(45)";
  attrib HML32E_46 format=F00259_. label="NA - Presence of species: CS(46)";
  attrib HML32E_47 format=F00259_. label="NA - Presence of species: CS(47)";
  attrib HML32E_48 format=F00259_. label="NA - Presence of species: CS(48)";
  attrib HML32F_01 format=F00260_. label="NA - Presence of species: CS(1)";
  attrib HML32F_02 format=F00260_. label="NA - Presence of species: CS(2)";
  attrib HML32F_03 format=F00260_. label="NA - Presence of species: CS(3)";
  attrib HML32F_04 format=F00260_. label="NA - Presence of species: CS(4)";
  attrib HML32F_05 format=F00260_. label="NA - Presence of species: CS(5)";
  attrib HML32F_06 format=F00260_. label="NA - Presence of species: CS(6)";
  attrib HML32F_07 format=F00260_. label="NA - Presence of species: CS(7)";
  attrib HML32F_08 format=F00260_. label="NA - Presence of species: CS(8)";
  attrib HML32F_09 format=F00260_. label="NA - Presence of species: CS(9)";
  attrib HML32F_10 format=F00260_. label="NA - Presence of species: CS(10)";
  attrib HML32F_11 format=F00260_. label="NA - Presence of species: CS(11)";
  attrib HML32F_12 format=F00260_. label="NA - Presence of species: CS(12)";
  attrib HML32F_13 format=F00260_. label="NA - Presence of species: CS(13)";
  attrib HML32F_14 format=F00260_. label="NA - Presence of species: CS(14)";
  attrib HML32F_15 format=F00260_. label="NA - Presence of species: CS(15)";
  attrib HML32F_16 format=F00260_. label="NA - Presence of species: CS(16)";
  attrib HML32F_17 format=F00260_. label="NA - Presence of species: CS(17)";
  attrib HML32F_18 format=F00260_. label="NA - Presence of species: CS(18)";
  attrib HML32F_19 format=F00260_. label="NA - Presence of species: CS(19)";
  attrib HML32F_20 format=F00260_. label="NA - Presence of species: CS(20)";
  attrib HML32F_21 format=F00260_. label="NA - Presence of species: CS(21)";
  attrib HML32F_22 format=F00260_. label="NA - Presence of species: CS(22)";
  attrib HML32F_23 format=F00260_. label="NA - Presence of species: CS(23)";
  attrib HML32F_24 format=F00260_. label="NA - Presence of species: CS(24)";
  attrib HML32F_25 format=F00260_. label="NA - Presence of species: CS(25)";
  attrib HML32F_26 format=F00260_. label="NA - Presence of species: CS(26)";
  attrib HML32F_27 format=F00260_. label="NA - Presence of species: CS(27)";
  attrib HML32F_28 format=F00260_. label="NA - Presence of species: CS(28)";
  attrib HML32F_29 format=F00260_. label="NA - Presence of species: CS(29)";
  attrib HML32F_30 format=F00260_. label="NA - Presence of species: CS(30)";
  attrib HML32F_31 format=F00260_. label="NA - Presence of species: CS(31)";
  attrib HML32F_32 format=F00260_. label="NA - Presence of species: CS(32)";
  attrib HML32F_33 format=F00260_. label="NA - Presence of species: CS(33)";
  attrib HML32F_34 format=F00260_. label="NA - Presence of species: CS(34)";
  attrib HML32F_35 format=F00260_. label="NA - Presence of species: CS(35)";
  attrib HML32F_36 format=F00260_. label="NA - Presence of species: CS(36)";
  attrib HML32F_37 format=F00260_. label="NA - Presence of species: CS(37)";
  attrib HML32F_38 format=F00260_. label="NA - Presence of species: CS(38)";
  attrib HML32F_39 format=F00260_. label="NA - Presence of species: CS(39)";
  attrib HML32F_40 format=F00260_. label="NA - Presence of species: CS(40)";
  attrib HML32F_41 format=F00260_. label="NA - Presence of species: CS(41)";
  attrib HML32F_42 format=F00260_. label="NA - Presence of species: CS(42)";
  attrib HML32F_43 format=F00260_. label="NA - Presence of species: CS(43)";
  attrib HML32F_44 format=F00260_. label="NA - Presence of species: CS(44)";
  attrib HML32F_45 format=F00260_. label="NA - Presence of species: CS(45)";
  attrib HML32F_46 format=F00260_. label="NA - Presence of species: CS(46)";
  attrib HML32F_47 format=F00260_. label="NA - Presence of species: CS(47)";
  attrib HML32F_48 format=F00260_. label="NA - Presence of species: CS(48)";
  attrib HML32G_01 format=F00261_. label="NA - Presence of species: CS(1)";
  attrib HML32G_02 format=F00261_. label="NA - Presence of species: CS(2)";
  attrib HML32G_03 format=F00261_. label="NA - Presence of species: CS(3)";
  attrib HML32G_04 format=F00261_. label="NA - Presence of species: CS(4)";
  attrib HML32G_05 format=F00261_. label="NA - Presence of species: CS(5)";
  attrib HML32G_06 format=F00261_. label="NA - Presence of species: CS(6)";
  attrib HML32G_07 format=F00261_. label="NA - Presence of species: CS(7)";
  attrib HML32G_08 format=F00261_. label="NA - Presence of species: CS(8)";
  attrib HML32G_09 format=F00261_. label="NA - Presence of species: CS(9)";
  attrib HML32G_10 format=F00261_. label="NA - Presence of species: CS(10)";
  attrib HML32G_11 format=F00261_. label="NA - Presence of species: CS(11)";
  attrib HML32G_12 format=F00261_. label="NA - Presence of species: CS(12)";
  attrib HML32G_13 format=F00261_. label="NA - Presence of species: CS(13)";
  attrib HML32G_14 format=F00261_. label="NA - Presence of species: CS(14)";
  attrib HML32G_15 format=F00261_. label="NA - Presence of species: CS(15)";
  attrib HML32G_16 format=F00261_. label="NA - Presence of species: CS(16)";
  attrib HML32G_17 format=F00261_. label="NA - Presence of species: CS(17)";
  attrib HML32G_18 format=F00261_. label="NA - Presence of species: CS(18)";
  attrib HML32G_19 format=F00261_. label="NA - Presence of species: CS(19)";
  attrib HML32G_20 format=F00261_. label="NA - Presence of species: CS(20)";
  attrib HML32G_21 format=F00261_. label="NA - Presence of species: CS(21)";
  attrib HML32G_22 format=F00261_. label="NA - Presence of species: CS(22)";
  attrib HML32G_23 format=F00261_. label="NA - Presence of species: CS(23)";
  attrib HML32G_24 format=F00261_. label="NA - Presence of species: CS(24)";
  attrib HML32G_25 format=F00261_. label="NA - Presence of species: CS(25)";
  attrib HML32G_26 format=F00261_. label="NA - Presence of species: CS(26)";
  attrib HML32G_27 format=F00261_. label="NA - Presence of species: CS(27)";
  attrib HML32G_28 format=F00261_. label="NA - Presence of species: CS(28)";
  attrib HML32G_29 format=F00261_. label="NA - Presence of species: CS(29)";
  attrib HML32G_30 format=F00261_. label="NA - Presence of species: CS(30)";
  attrib HML32G_31 format=F00261_. label="NA - Presence of species: CS(31)";
  attrib HML32G_32 format=F00261_. label="NA - Presence of species: CS(32)";
  attrib HML32G_33 format=F00261_. label="NA - Presence of species: CS(33)";
  attrib HML32G_34 format=F00261_. label="NA - Presence of species: CS(34)";
  attrib HML32G_35 format=F00261_. label="NA - Presence of species: CS(35)";
  attrib HML32G_36 format=F00261_. label="NA - Presence of species: CS(36)";
  attrib HML32G_37 format=F00261_. label="NA - Presence of species: CS(37)";
  attrib HML32G_38 format=F00261_. label="NA - Presence of species: CS(38)";
  attrib HML32G_39 format=F00261_. label="NA - Presence of species: CS(39)";
  attrib HML32G_40 format=F00261_. label="NA - Presence of species: CS(40)";
  attrib HML32G_41 format=F00261_. label="NA - Presence of species: CS(41)";
  attrib HML32G_42 format=F00261_. label="NA - Presence of species: CS(42)";
  attrib HML32G_43 format=F00261_. label="NA - Presence of species: CS(43)";
  attrib HML32G_44 format=F00261_. label="NA - Presence of species: CS(44)";
  attrib HML32G_45 format=F00261_. label="NA - Presence of species: CS(45)";
  attrib HML32G_46 format=F00261_. label="NA - Presence of species: CS(46)";
  attrib HML32G_47 format=F00261_. label="NA - Presence of species: CS(47)";
  attrib HML32G_48 format=F00261_. label="NA - Presence of species: CS(48)";
  attrib HML33_01 format=F00262_. label="NA - Result of malaria measurement(1)";
  attrib HML33_02 format=F00262_. label="NA - Result of malaria measurement(2)";
  attrib HML33_03 format=F00262_. label="NA - Result of malaria measurement(3)";
  attrib HML33_04 format=F00262_. label="NA - Result of malaria measurement(4)";
  attrib HML33_05 format=F00262_. label="NA - Result of malaria measurement(5)";
  attrib HML33_06 format=F00262_. label="NA - Result of malaria measurement(6)";
  attrib HML33_07 format=F00262_. label="NA - Result of malaria measurement(7)";
  attrib HML33_08 format=F00262_. label="NA - Result of malaria measurement(8)";
  attrib HML33_09 format=F00262_. label="NA - Result of malaria measurement(9)";
  attrib HML33_10 format=F00262_. label="NA - Result of malaria measurement(10)";
  attrib HML33_11 format=F00262_. label="NA - Result of malaria measurement(11)";
  attrib HML33_12 format=F00262_. label="NA - Result of malaria measurement(12)";
  attrib HML33_13 format=F00262_. label="NA - Result of malaria measurement(13)";
  attrib HML33_14 format=F00262_. label="NA - Result of malaria measurement(14)";
  attrib HML33_15 format=F00262_. label="NA - Result of malaria measurement(15)";
  attrib HML33_16 format=F00262_. label="NA - Result of malaria measurement(16)";
  attrib HML33_17 format=F00262_. label="NA - Result of malaria measurement(17)";
  attrib HML33_18 format=F00262_. label="NA - Result of malaria measurement(18)";
  attrib HML33_19 format=F00262_. label="NA - Result of malaria measurement(19)";
  attrib HML33_20 format=F00262_. label="NA - Result of malaria measurement(20)";
  attrib HML33_21 format=F00262_. label="NA - Result of malaria measurement(21)";
  attrib HML33_22 format=F00262_. label="NA - Result of malaria measurement(22)";
  attrib HML33_23 format=F00262_. label="NA - Result of malaria measurement(23)";
  attrib HML33_24 format=F00262_. label="NA - Result of malaria measurement(24)";
  attrib HML33_25 format=F00262_. label="NA - Result of malaria measurement(25)";
  attrib HML33_26 format=F00262_. label="NA - Result of malaria measurement(26)";
  attrib HML33_27 format=F00262_. label="NA - Result of malaria measurement(27)";
  attrib HML33_28 format=F00262_. label="NA - Result of malaria measurement(28)";
  attrib HML33_29 format=F00262_. label="NA - Result of malaria measurement(29)";
  attrib HML33_30 format=F00262_. label="NA - Result of malaria measurement(30)";
  attrib HML33_31 format=F00262_. label="NA - Result of malaria measurement(31)";
  attrib HML33_32 format=F00262_. label="NA - Result of malaria measurement(32)";
  attrib HML33_33 format=F00262_. label="NA - Result of malaria measurement(33)";
  attrib HML33_34 format=F00262_. label="NA - Result of malaria measurement(34)";
  attrib HML33_35 format=F00262_. label="NA - Result of malaria measurement(35)";
  attrib HML33_36 format=F00262_. label="NA - Result of malaria measurement(36)";
  attrib HML33_37 format=F00262_. label="NA - Result of malaria measurement(37)";
  attrib HML33_38 format=F00262_. label="NA - Result of malaria measurement(38)";
  attrib HML33_39 format=F00262_. label="NA - Result of malaria measurement(39)";
  attrib HML33_40 format=F00262_. label="NA - Result of malaria measurement(40)";
  attrib HML33_41 format=F00262_. label="NA - Result of malaria measurement(41)";
  attrib HML33_42 format=F00262_. label="NA - Result of malaria measurement(42)";
  attrib HML33_43 format=F00262_. label="NA - Result of malaria measurement(43)";
  attrib HML33_44 format=F00262_. label="NA - Result of malaria measurement(44)";
  attrib HML33_45 format=F00262_. label="NA - Result of malaria measurement(45)";
  attrib HML33_46 format=F00262_. label="NA - Result of malaria measurement(46)";
  attrib HML33_47 format=F00262_. label="NA - Result of malaria measurement(47)";
  attrib HML33_48 format=F00262_. label="NA - Result of malaria measurement(48)";
  attrib HML34_01 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(1)";
  attrib HML34_02 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(2)";
  attrib HML34_03 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(3)";
  attrib HML34_04 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(4)";
  attrib HML34_05 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(5)";
  attrib HML34_06 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(6)";
  attrib HML34_07 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(7)";
  attrib HML34_08 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(8)";
  attrib HML34_09 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(9)";
  attrib HML34_10 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(10)";
  attrib HML34_11 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(11)";
  attrib HML34_12 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(12)";
  attrib HML34_13 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(13)";
  attrib HML34_14 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(14)";
  attrib HML34_15 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(15)";
  attrib HML34_16 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(16)";
  attrib HML34_17 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(17)";
  attrib HML34_18 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(18)";
  attrib HML34_19 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(19)";
  attrib HML34_20 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(20)";
  attrib HML34_21 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(21)";
  attrib HML34_22 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(22)";
  attrib HML34_23 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(23)";
  attrib HML34_24 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(24)";
  attrib HML34_25 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(25)";
  attrib HML34_26 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(26)";
  attrib HML34_27 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(27)";
  attrib HML34_28 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(28)";
  attrib HML34_29 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(29)";
  attrib HML34_30 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(30)";
  attrib HML34_31 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(31)";
  attrib HML34_32 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(32)";
  attrib HML34_33 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(33)";
  attrib HML34_34 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(34)";
  attrib HML34_35 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(35)";
  attrib HML34_36 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(36)";
  attrib HML34_37 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(37)";
  attrib HML34_38 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(38)";
  attrib HML34_39 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(39)";
  attrib HML34_40 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(40)";
  attrib HML34_41 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(41)";
  attrib HML34_42 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(42)";
  attrib HML34_43 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(43)";
  attrib HML34_44 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(44)";
  attrib HML34_45 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(45)";
  attrib HML34_46 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(46)";
  attrib HML34_47 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(47)";
  attrib HML34_48 length=$5 format=$F00263_. label="NA - Bar code for blood smear sample(48)";
  attrib HML35_01 format=F00264_. label="NA - Result of malaria rapid test(1)";
  attrib HML35_02 format=F00264_. label="NA - Result of malaria rapid test(2)";
  attrib HML35_03 format=F00264_. label="NA - Result of malaria rapid test(3)";
  attrib HML35_04 format=F00264_. label="NA - Result of malaria rapid test(4)";
  attrib HML35_05 format=F00264_. label="NA - Result of malaria rapid test(5)";
  attrib HML35_06 format=F00264_. label="NA - Result of malaria rapid test(6)";
  attrib HML35_07 format=F00264_. label="NA - Result of malaria rapid test(7)";
  attrib HML35_08 format=F00264_. label="NA - Result of malaria rapid test(8)";
  attrib HML35_09 format=F00264_. label="NA - Result of malaria rapid test(9)";
  attrib HML35_10 format=F00264_. label="NA - Result of malaria rapid test(10)";
  attrib HML35_11 format=F00264_. label="NA - Result of malaria rapid test(11)";
  attrib HML35_12 format=F00264_. label="NA - Result of malaria rapid test(12)";
  attrib HML35_13 format=F00264_. label="NA - Result of malaria rapid test(13)";
  attrib HML35_14 format=F00264_. label="NA - Result of malaria rapid test(14)";
  attrib HML35_15 format=F00264_. label="NA - Result of malaria rapid test(15)";
  attrib HML35_16 format=F00264_. label="NA - Result of malaria rapid test(16)";
  attrib HML35_17 format=F00264_. label="NA - Result of malaria rapid test(17)";
  attrib HML35_18 format=F00264_. label="NA - Result of malaria rapid test(18)";
  attrib HML35_19 format=F00264_. label="NA - Result of malaria rapid test(19)";
  attrib HML35_20 format=F00264_. label="NA - Result of malaria rapid test(20)";
  attrib HML35_21 format=F00264_. label="NA - Result of malaria rapid test(21)";
  attrib HML35_22 format=F00264_. label="NA - Result of malaria rapid test(22)";
  attrib HML35_23 format=F00264_. label="NA - Result of malaria rapid test(23)";
  attrib HML35_24 format=F00264_. label="NA - Result of malaria rapid test(24)";
  attrib HML35_25 format=F00264_. label="NA - Result of malaria rapid test(25)";
  attrib HML35_26 format=F00264_. label="NA - Result of malaria rapid test(26)";
  attrib HML35_27 format=F00264_. label="NA - Result of malaria rapid test(27)";
  attrib HML35_28 format=F00264_. label="NA - Result of malaria rapid test(28)";
  attrib HML35_29 format=F00264_. label="NA - Result of malaria rapid test(29)";
  attrib HML35_30 format=F00264_. label="NA - Result of malaria rapid test(30)";
  attrib HML35_31 format=F00264_. label="NA - Result of malaria rapid test(31)";
  attrib HML35_32 format=F00264_. label="NA - Result of malaria rapid test(32)";
  attrib HML35_33 format=F00264_. label="NA - Result of malaria rapid test(33)";
  attrib HML35_34 format=F00264_. label="NA - Result of malaria rapid test(34)";
  attrib HML35_35 format=F00264_. label="NA - Result of malaria rapid test(35)";
  attrib HML35_36 format=F00264_. label="NA - Result of malaria rapid test(36)";
  attrib HML35_37 format=F00264_. label="NA - Result of malaria rapid test(37)";
  attrib HML35_38 format=F00264_. label="NA - Result of malaria rapid test(38)";
  attrib HML35_39 format=F00264_. label="NA - Result of malaria rapid test(39)";
  attrib HML35_40 format=F00264_. label="NA - Result of malaria rapid test(40)";
  attrib HML35_41 format=F00264_. label="NA - Result of malaria rapid test(41)";
  attrib HML35_42 format=F00264_. label="NA - Result of malaria rapid test(42)";
  attrib HML35_43 format=F00264_. label="NA - Result of malaria rapid test(43)";
  attrib HML35_44 format=F00264_. label="NA - Result of malaria rapid test(44)";
  attrib HML35_45 format=F00264_. label="NA - Result of malaria rapid test(45)";
  attrib HML35_46 format=F00264_. label="NA - Result of malaria rapid test(46)";
  attrib HML35_47 format=F00264_. label="NA - Result of malaria rapid test(47)";
  attrib HML35_48 format=F00264_. label="NA - Result of malaria rapid test(48)";
infile 'C:\COUNTRIES\AF71\AFHR71FL.DAT' LRECL=4716 MISSOVER ;
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
    @245  HV245    4.0
    @249  HV246    1.0
    @250  HV246A   2.0
    @252  HV246B   2.0
    @254  HV246C   2.0
    @256  HV246D   2.0
    @258  HV246E   2.0
    @260  HV246F   2.0
    @262  HV246G   2.0
    @264  HV246H   2.0
    @266  HV246I   2.0
    @268  HV246J   2.0
    @270  HV246K   2.0
    @272  HV247    1.0
    @273  HV252    1.0
    @274  HV253    1.0
    @275  HV253A   1.0
    @276  HV253B   1.0
    @277  HV253C   1.0
    @278  HV253D   1.0
    @279  HV253E   1.0
    @280  HV253F   1.0
    @281  HV253G   1.0
    @282  HV253H   1.0
    @283  HV253X   1.0
    @284  HV253Z   1.0
    @285  HV270    1.0
    @286  HV271    8.0
    @294  HV270A   1.0
    @295  HV271A   8.0
    @303  HML1     2.0
    @305  HML1A    1.0
    @306  HML2     2.0
    @308  SHDISTRICT 2.0
    @310  SH107    2.0
    @312  SH110G   1.0
    @313  SH110H   1.0
    @314  SH110I   1.0
    @315  SH110J   1.0
    @316  SH110K   1.0
    @317  SH110L   1.0
    @318  SH110M   1.0
    @319  SH110N   1.0
    @320  SH110O   1.0
    @321  SH118F   1.0
    @322  SH118G   1.0
    @323  SH120    4.1
    @327  SH123A   1.0
    @328  SH123BA  1.0
    @329  SH123BB  1.0
    @330  SH123BC  1.0
    @331  SH123BD  1.0
    @332  SH123BE  1.0
    @333  SH123BX  1.0
    @334  SH123BZ  1.0
    @335  SH123C   1.0
    @336  SH123DA  1.0
    @337  SH123DB  1.0
    @338  SH123DC  1.0
    @339  SH123DD  1.0
    @340  SH123DE  1.0
    @341  SH123DX  1.0
    @342  SH123DZ  1.0
    @343  HVIDX_01 2.0
    @345  HVIDX_02 2.0
    @347  HVIDX_03 2.0
    @349  HVIDX_04 2.0
    @351  HVIDX_05 2.0
    @353  HVIDX_06 2.0
    @355  HVIDX_07 2.0
    @357  HVIDX_08 2.0
    @359  HVIDX_09 2.0
    @361  HVIDX_10 2.0
    @363  HVIDX_11 2.0
    @365  HVIDX_12 2.0
    @367  HVIDX_13 2.0
    @369  HVIDX_14 2.0
    @371  HVIDX_15 2.0
    @373  HVIDX_16 2.0
    @375  HVIDX_17 2.0
    @377  HVIDX_18 2.0
    @379  HVIDX_19 2.0
    @381  HVIDX_20 2.0
    @383  HVIDX_21 2.0
    @385  HVIDX_22 2.0
    @387  HVIDX_23 2.0
    @389  HVIDX_24 2.0
    @391  HVIDX_25 2.0
    @393  HVIDX_26 2.0
    @395  HVIDX_27 2.0
    @397  HVIDX_28 2.0
    @399  HVIDX_29 2.0
    @401  HVIDX_30 2.0
    @403  HVIDX_31 2.0
    @405  HVIDX_32 2.0
    @407  HVIDX_33 2.0
    @409  HVIDX_34 2.0
    @411  HVIDX_35 2.0
    @413  HVIDX_36 2.0
    @415  HVIDX_37 2.0
    @417  HVIDX_38 2.0
    @419  HVIDX_39 2.0
    @421  HVIDX_40 2.0
    @423  HVIDX_41 2.0
    @425  HVIDX_42 2.0
    @427  HVIDX_43 2.0
    @429  HVIDX_44 2.0
    @431  HVIDX_45 2.0
    @433  HVIDX_46 2.0
    @435  HVIDX_47 2.0
    @437  HVIDX_48 2.0
    @439  HV101_01 2.0
    @441  HV101_02 2.0
    @443  HV101_03 2.0
    @445  HV101_04 2.0
    @447  HV101_05 2.0
    @449  HV101_06 2.0
    @451  HV101_07 2.0
    @453  HV101_08 2.0
    @455  HV101_09 2.0
    @457  HV101_10 2.0
    @459  HV101_11 2.0
    @461  HV101_12 2.0
    @463  HV101_13 2.0
    @465  HV101_14 2.0
    @467  HV101_15 2.0
    @469  HV101_16 2.0
    @471  HV101_17 2.0
    @473  HV101_18 2.0
    @475  HV101_19 2.0
    @477  HV101_20 2.0
    @479  HV101_21 2.0
    @481  HV101_22 2.0
    @483  HV101_23 2.0
    @485  HV101_24 2.0
    @487  HV101_25 2.0
    @489  HV101_26 2.0
    @491  HV101_27 2.0
    @493  HV101_28 2.0
    @495  HV101_29 2.0
    @497  HV101_30 2.0
    @499  HV101_31 2.0
    @501  HV101_32 2.0
    @503  HV101_33 2.0
    @505  HV101_34 2.0
    @507  HV101_35 2.0
    @509  HV101_36 2.0
    @511  HV101_37 2.0
    @513  HV101_38 2.0
    @515  HV101_39 2.0
    @517  HV101_40 2.0
    @519  HV101_41 2.0
    @521  HV101_42 2.0
    @523  HV101_43 2.0
    @525  HV101_44 2.0
    @527  HV101_45 2.0
    @529  HV101_46 2.0
    @531  HV101_47 2.0
    @533  HV101_48 2.0
    @535  HV102_01 1.0
    @536  HV102_02 1.0
    @537  HV102_03 1.0
    @538  HV102_04 1.0
    @539  HV102_05 1.0
    @540  HV102_06 1.0
    @541  HV102_07 1.0
    @542  HV102_08 1.0
    @543  HV102_09 1.0
    @544  HV102_10 1.0
    @545  HV102_11 1.0
    @546  HV102_12 1.0
    @547  HV102_13 1.0
    @548  HV102_14 1.0
    @549  HV102_15 1.0
    @550  HV102_16 1.0
    @551  HV102_17 1.0
    @552  HV102_18 1.0
    @553  HV102_19 1.0
    @554  HV102_20 1.0
    @555  HV102_21 1.0
    @556  HV102_22 1.0
    @557  HV102_23 1.0
    @558  HV102_24 1.0
    @559  HV102_25 1.0
    @560  HV102_26 1.0
    @561  HV102_27 1.0
    @562  HV102_28 1.0
    @563  HV102_29 1.0
    @564  HV102_30 1.0
    @565  HV102_31 1.0
    @566  HV102_32 1.0
    @567  HV102_33 1.0
    @568  HV102_34 1.0
    @569  HV102_35 1.0
    @570  HV102_36 1.0
    @571  HV102_37 1.0
    @572  HV102_38 1.0
    @573  HV102_39 1.0
    @574  HV102_40 1.0
    @575  HV102_41 1.0
    @576  HV102_42 1.0
    @577  HV102_43 1.0
    @578  HV102_44 1.0
    @579  HV102_45 1.0
    @580  HV102_46 1.0
    @581  HV102_47 1.0
    @582  HV102_48 1.0
    @583  HV103_01 1.0
    @584  HV103_02 1.0
    @585  HV103_03 1.0
    @586  HV103_04 1.0
    @587  HV103_05 1.0
    @588  HV103_06 1.0
    @589  HV103_07 1.0
    @590  HV103_08 1.0
    @591  HV103_09 1.0
    @592  HV103_10 1.0
    @593  HV103_11 1.0
    @594  HV103_12 1.0
    @595  HV103_13 1.0
    @596  HV103_14 1.0
    @597  HV103_15 1.0
    @598  HV103_16 1.0
    @599  HV103_17 1.0
    @600  HV103_18 1.0
    @601  HV103_19 1.0
    @602  HV103_20 1.0
    @603  HV103_21 1.0
    @604  HV103_22 1.0
    @605  HV103_23 1.0
    @606  HV103_24 1.0
    @607  HV103_25 1.0
    @608  HV103_26 1.0
    @609  HV103_27 1.0
    @610  HV103_28 1.0
    @611  HV103_29 1.0
    @612  HV103_30 1.0
    @613  HV103_31 1.0
    @614  HV103_32 1.0
    @615  HV103_33 1.0
    @616  HV103_34 1.0
    @617  HV103_35 1.0
    @618  HV103_36 1.0
    @619  HV103_37 1.0
    @620  HV103_38 1.0
    @621  HV103_39 1.0
    @622  HV103_40 1.0
    @623  HV103_41 1.0
    @624  HV103_42 1.0
    @625  HV103_43 1.0
    @626  HV103_44 1.0
    @627  HV103_45 1.0
    @628  HV103_46 1.0
    @629  HV103_47 1.0
    @630  HV103_48 1.0
    @631  HV104_01 1.0
    @632  HV104_02 1.0
    @633  HV104_03 1.0
    @634  HV104_04 1.0
    @635  HV104_05 1.0
    @636  HV104_06 1.0
    @637  HV104_07 1.0
    @638  HV104_08 1.0
    @639  HV104_09 1.0
    @640  HV104_10 1.0
    @641  HV104_11 1.0
    @642  HV104_12 1.0
    @643  HV104_13 1.0
    @644  HV104_14 1.0
    @645  HV104_15 1.0
    @646  HV104_16 1.0
    @647  HV104_17 1.0
    @648  HV104_18 1.0
    @649  HV104_19 1.0
    @650  HV104_20 1.0
    @651  HV104_21 1.0
    @652  HV104_22 1.0
    @653  HV104_23 1.0
    @654  HV104_24 1.0
    @655  HV104_25 1.0
    @656  HV104_26 1.0
    @657  HV104_27 1.0
    @658  HV104_28 1.0
    @659  HV104_29 1.0
    @660  HV104_30 1.0
    @661  HV104_31 1.0
    @662  HV104_32 1.0
    @663  HV104_33 1.0
    @664  HV104_34 1.0
    @665  HV104_35 1.0
    @666  HV104_36 1.0
    @667  HV104_37 1.0
    @668  HV104_38 1.0
    @669  HV104_39 1.0
    @670  HV104_40 1.0
    @671  HV104_41 1.0
    @672  HV104_42 1.0
    @673  HV104_43 1.0
    @674  HV104_44 1.0
    @675  HV104_45 1.0
    @676  HV104_46 1.0
    @677  HV104_47 1.0
    @678  HV104_48 1.0
    @679  HV105_01 2.0
    @681  HV105_02 2.0
    @683  HV105_03 2.0
    @685  HV105_04 2.0
    @687  HV105_05 2.0
    @689  HV105_06 2.0
    @691  HV105_07 2.0
    @693  HV105_08 2.0
    @695  HV105_09 2.0
    @697  HV105_10 2.0
    @699  HV105_11 2.0
    @701  HV105_12 2.0
    @703  HV105_13 2.0
    @705  HV105_14 2.0
    @707  HV105_15 2.0
    @709  HV105_16 2.0
    @711  HV105_17 2.0
    @713  HV105_18 2.0
    @715  HV105_19 2.0
    @717  HV105_20 2.0
    @719  HV105_21 2.0
    @721  HV105_22 2.0
    @723  HV105_23 2.0
    @725  HV105_24 2.0
    @727  HV105_25 2.0
    @729  HV105_26 2.0
    @731  HV105_27 2.0
    @733  HV105_28 2.0
    @735  HV105_29 2.0
    @737  HV105_30 2.0
    @739  HV105_31 2.0
    @741  HV105_32 2.0
    @743  HV105_33 2.0
    @745  HV105_34 2.0
    @747  HV105_35 2.0
    @749  HV105_36 2.0
    @751  HV105_37 2.0
    @753  HV105_38 2.0
    @755  HV105_39 2.0
    @757  HV105_40 2.0
    @759  HV105_41 2.0
    @761  HV105_42 2.0
    @763  HV105_43 2.0
    @765  HV105_44 2.0
    @767  HV105_45 2.0
    @769  HV105_46 2.0
    @771  HV105_47 2.0
    @773  HV105_48 2.0
    @775  HV106_01 1.0
    @776  HV106_02 1.0
    @777  HV106_03 1.0
    @778  HV106_04 1.0
    @779  HV106_05 1.0
    @780  HV106_06 1.0
    @781  HV106_07 1.0
    @782  HV106_08 1.0
    @783  HV106_09 1.0
    @784  HV106_10 1.0
    @785  HV106_11 1.0
    @786  HV106_12 1.0
    @787  HV106_13 1.0
    @788  HV106_14 1.0
    @789  HV106_15 1.0
    @790  HV106_16 1.0
    @791  HV106_17 1.0
    @792  HV106_18 1.0
    @793  HV106_19 1.0
    @794  HV106_20 1.0
    @795  HV106_21 1.0
    @796  HV106_22 1.0
    @797  HV106_23 1.0
    @798  HV106_24 1.0
    @799  HV106_25 1.0
    @800  HV106_26 1.0
    @801  HV106_27 1.0
    @802  HV106_28 1.0
    @803  HV106_29 1.0
    @804  HV106_30 1.0
    @805  HV106_31 1.0
    @806  HV106_32 1.0
    @807  HV106_33 1.0
    @808  HV106_34 1.0
    @809  HV106_35 1.0
    @810  HV106_36 1.0
    @811  HV106_37 1.0
    @812  HV106_38 1.0
    @813  HV106_39 1.0
    @814  HV106_40 1.0
    @815  HV106_41 1.0
    @816  HV106_42 1.0
    @817  HV106_43 1.0
    @818  HV106_44 1.0
    @819  HV106_45 1.0
    @820  HV106_46 1.0
    @821  HV106_47 1.0
    @822  HV106_48 1.0
    @823  HV107_01 2.0
    @825  HV107_02 2.0
    @827  HV107_03 2.0
    @829  HV107_04 2.0
    @831  HV107_05 2.0
    @833  HV107_06 2.0
    @835  HV107_07 2.0
    @837  HV107_08 2.0
    @839  HV107_09 2.0
    @841  HV107_10 2.0
    @843  HV107_11 2.0
    @845  HV107_12 2.0
    @847  HV107_13 2.0
    @849  HV107_14 2.0
    @851  HV107_15 2.0
    @853  HV107_16 2.0
    @855  HV107_17 2.0
    @857  HV107_18 2.0
    @859  HV107_19 2.0
    @861  HV107_20 2.0
    @863  HV107_21 2.0
    @865  HV107_22 2.0
    @867  HV107_23 2.0
    @869  HV107_24 2.0
    @871  HV107_25 2.0
    @873  HV107_26 2.0
    @875  HV107_27 2.0
    @877  HV107_28 2.0
    @879  HV107_29 2.0
    @881  HV107_30 2.0
    @883  HV107_31 2.0
    @885  HV107_32 2.0
    @887  HV107_33 2.0
    @889  HV107_34 2.0
    @891  HV107_35 2.0
    @893  HV107_36 2.0
    @895  HV107_37 2.0
    @897  HV107_38 2.0
    @899  HV107_39 2.0
    @901  HV107_40 2.0
    @903  HV107_41 2.0
    @905  HV107_42 2.0
    @907  HV107_43 2.0
    @909  HV107_44 2.0
    @911  HV107_45 2.0
    @913  HV107_46 2.0
    @915  HV107_47 2.0
    @917  HV107_48 2.0
    @919  HV108_01 2.0
    @921  HV108_02 2.0
    @923  HV108_03 2.0
    @925  HV108_04 2.0
    @927  HV108_05 2.0
    @929  HV108_06 2.0
    @931  HV108_07 2.0
    @933  HV108_08 2.0
    @935  HV108_09 2.0
    @937  HV108_10 2.0
    @939  HV108_11 2.0
    @941  HV108_12 2.0
    @943  HV108_13 2.0
    @945  HV108_14 2.0
    @947  HV108_15 2.0
    @949  HV108_16 2.0
    @951  HV108_17 2.0
    @953  HV108_18 2.0
    @955  HV108_19 2.0
    @957  HV108_20 2.0
    @959  HV108_21 2.0
    @961  HV108_22 2.0
    @963  HV108_23 2.0
    @965  HV108_24 2.0
    @967  HV108_25 2.0
    @969  HV108_26 2.0
    @971  HV108_27 2.0
    @973  HV108_28 2.0
    @975  HV108_29 2.0
    @977  HV108_30 2.0
    @979  HV108_31 2.0
    @981  HV108_32 2.0
    @983  HV108_33 2.0
    @985  HV108_34 2.0
    @987  HV108_35 2.0
    @989  HV108_36 2.0
    @991  HV108_37 2.0
    @993  HV108_38 2.0
    @995  HV108_39 2.0
    @997  HV108_40 2.0
    @999  HV108_41 2.0
    @1001 HV108_42 2.0
    @1003 HV108_43 2.0
    @1005 HV108_44 2.0
    @1007 HV108_45 2.0
    @1009 HV108_46 2.0
    @1011 HV108_47 2.0
    @1013 HV108_48 2.0
    @1015 HV109_01 1.0
    @1016 HV109_02 1.0
    @1017 HV109_03 1.0
    @1018 HV109_04 1.0
    @1019 HV109_05 1.0
    @1020 HV109_06 1.0
    @1021 HV109_07 1.0
    @1022 HV109_08 1.0
    @1023 HV109_09 1.0
    @1024 HV109_10 1.0
    @1025 HV109_11 1.0
    @1026 HV109_12 1.0
    @1027 HV109_13 1.0
    @1028 HV109_14 1.0
    @1029 HV109_15 1.0
    @1030 HV109_16 1.0
    @1031 HV109_17 1.0
    @1032 HV109_18 1.0
    @1033 HV109_19 1.0
    @1034 HV109_20 1.0
    @1035 HV109_21 1.0
    @1036 HV109_22 1.0
    @1037 HV109_23 1.0
    @1038 HV109_24 1.0
    @1039 HV109_25 1.0
    @1040 HV109_26 1.0
    @1041 HV109_27 1.0
    @1042 HV109_28 1.0
    @1043 HV109_29 1.0
    @1044 HV109_30 1.0
    @1045 HV109_31 1.0
    @1046 HV109_32 1.0
    @1047 HV109_33 1.0
    @1048 HV109_34 1.0
    @1049 HV109_35 1.0
    @1050 HV109_36 1.0
    @1051 HV109_37 1.0
    @1052 HV109_38 1.0
    @1053 HV109_39 1.0
    @1054 HV109_40 1.0
    @1055 HV109_41 1.0
    @1056 HV109_42 1.0
    @1057 HV109_43 1.0
    @1058 HV109_44 1.0
    @1059 HV109_45 1.0
    @1060 HV109_46 1.0
    @1061 HV109_47 1.0
    @1062 HV109_48 1.0
    @1063 HV110_01 1.0
    @1064 HV110_02 1.0
    @1065 HV110_03 1.0
    @1066 HV110_04 1.0
    @1067 HV110_05 1.0
    @1068 HV110_06 1.0
    @1069 HV110_07 1.0
    @1070 HV110_08 1.0
    @1071 HV110_09 1.0
    @1072 HV110_10 1.0
    @1073 HV110_11 1.0
    @1074 HV110_12 1.0
    @1075 HV110_13 1.0
    @1076 HV110_14 1.0
    @1077 HV110_15 1.0
    @1078 HV110_16 1.0
    @1079 HV110_17 1.0
    @1080 HV110_18 1.0
    @1081 HV110_19 1.0
    @1082 HV110_20 1.0
    @1083 HV110_21 1.0
    @1084 HV110_22 1.0
    @1085 HV110_23 1.0
    @1086 HV110_24 1.0
    @1087 HV110_25 1.0
    @1088 HV110_26 1.0
    @1089 HV110_27 1.0
    @1090 HV110_28 1.0
    @1091 HV110_29 1.0
    @1092 HV110_30 1.0
    @1093 HV110_31 1.0
    @1094 HV110_32 1.0
    @1095 HV110_33 1.0
    @1096 HV110_34 1.0
    @1097 HV110_35 1.0
    @1098 HV110_36 1.0
    @1099 HV110_37 1.0
    @1100 HV110_38 1.0
    @1101 HV110_39 1.0
    @1102 HV110_40 1.0
    @1103 HV110_41 1.0
    @1104 HV110_42 1.0
    @1105 HV110_43 1.0
    @1106 HV110_44 1.0
    @1107 HV110_45 1.0
    @1108 HV110_46 1.0
    @1109 HV110_47 1.0
    @1110 HV110_48 1.0
    @1111 HV111_01 1.0
    @1112 HV111_02 1.0
    @1113 HV111_03 1.0
    @1114 HV111_04 1.0
    @1115 HV111_05 1.0
    @1116 HV111_06 1.0
    @1117 HV111_07 1.0
    @1118 HV111_08 1.0
    @1119 HV111_09 1.0
    @1120 HV111_10 1.0
    @1121 HV111_11 1.0
    @1122 HV111_12 1.0
    @1123 HV111_13 1.0
    @1124 HV111_14 1.0
    @1125 HV111_15 1.0
    @1126 HV111_16 1.0
    @1127 HV111_17 1.0
    @1128 HV111_18 1.0
    @1129 HV111_19 1.0
    @1130 HV111_20 1.0
    @1131 HV111_21 1.0
    @1132 HV111_22 1.0
    @1133 HV111_23 1.0
    @1134 HV111_24 1.0
    @1135 HV111_25 1.0
    @1136 HV111_26 1.0
    @1137 HV111_27 1.0
    @1138 HV111_28 1.0
    @1139 HV111_29 1.0
    @1140 HV111_30 1.0
    @1141 HV111_31 1.0
    @1142 HV111_32 1.0
    @1143 HV111_33 1.0
    @1144 HV111_34 1.0
    @1145 HV111_35 1.0
    @1146 HV111_36 1.0
    @1147 HV111_37 1.0
    @1148 HV111_38 1.0
    @1149 HV111_39 1.0
    @1150 HV111_40 1.0
    @1151 HV111_41 1.0
    @1152 HV111_42 1.0
    @1153 HV111_43 1.0
    @1154 HV111_44 1.0
    @1155 HV111_45 1.0
    @1156 HV111_46 1.0
    @1157 HV111_47 1.0
    @1158 HV111_48 1.0
    @1159 HV112_01 2.0
    @1161 HV112_02 2.0
    @1163 HV112_03 2.0
    @1165 HV112_04 2.0
    @1167 HV112_05 2.0
    @1169 HV112_06 2.0
    @1171 HV112_07 2.0
    @1173 HV112_08 2.0
    @1175 HV112_09 2.0
    @1177 HV112_10 2.0
    @1179 HV112_11 2.0
    @1181 HV112_12 2.0
    @1183 HV112_13 2.0
    @1185 HV112_14 2.0
    @1187 HV112_15 2.0
    @1189 HV112_16 2.0
    @1191 HV112_17 2.0
    @1193 HV112_18 2.0
    @1195 HV112_19 2.0
    @1197 HV112_20 2.0
    @1199 HV112_21 2.0
    @1201 HV112_22 2.0
    @1203 HV112_23 2.0
    @1205 HV112_24 2.0
    @1207 HV112_25 2.0
    @1209 HV112_26 2.0
    @1211 HV112_27 2.0
    @1213 HV112_28 2.0
    @1215 HV112_29 2.0
    @1217 HV112_30 2.0
    @1219 HV112_31 2.0
    @1221 HV112_32 2.0
    @1223 HV112_33 2.0
    @1225 HV112_34 2.0
    @1227 HV112_35 2.0
    @1229 HV112_36 2.0
    @1231 HV112_37 2.0
    @1233 HV112_38 2.0
    @1235 HV112_39 2.0
    @1237 HV112_40 2.0
    @1239 HV112_41 2.0
    @1241 HV112_42 2.0
    @1243 HV112_43 2.0
    @1245 HV112_44 2.0
    @1247 HV112_45 2.0
    @1249 HV112_46 2.0
    @1251 HV112_47 2.0
    @1253 HV112_48 2.0
    @1255 HV113_01 1.0
    @1256 HV113_02 1.0
    @1257 HV113_03 1.0
    @1258 HV113_04 1.0
    @1259 HV113_05 1.0
    @1260 HV113_06 1.0
    @1261 HV113_07 1.0
    @1262 HV113_08 1.0
    @1263 HV113_09 1.0
    @1264 HV113_10 1.0
    @1265 HV113_11 1.0
    @1266 HV113_12 1.0
    @1267 HV113_13 1.0
    @1268 HV113_14 1.0
    @1269 HV113_15 1.0
    @1270 HV113_16 1.0
    @1271 HV113_17 1.0
    @1272 HV113_18 1.0
    @1273 HV113_19 1.0
    @1274 HV113_20 1.0
    @1275 HV113_21 1.0
    @1276 HV113_22 1.0
    @1277 HV113_23 1.0
    @1278 HV113_24 1.0
    @1279 HV113_25 1.0
    @1280 HV113_26 1.0
    @1281 HV113_27 1.0
    @1282 HV113_28 1.0
    @1283 HV113_29 1.0
    @1284 HV113_30 1.0
    @1285 HV113_31 1.0
    @1286 HV113_32 1.0
    @1287 HV113_33 1.0
    @1288 HV113_34 1.0
    @1289 HV113_35 1.0
    @1290 HV113_36 1.0
    @1291 HV113_37 1.0
    @1292 HV113_38 1.0
    @1293 HV113_39 1.0
    @1294 HV113_40 1.0
    @1295 HV113_41 1.0
    @1296 HV113_42 1.0
    @1297 HV113_43 1.0
    @1298 HV113_44 1.0
    @1299 HV113_45 1.0
    @1300 HV113_46 1.0
    @1301 HV113_47 1.0
    @1302 HV113_48 1.0
    @1303 HV114_01 2.0
    @1305 HV114_02 2.0
    @1307 HV114_03 2.0
    @1309 HV114_04 2.0
    @1311 HV114_05 2.0
    @1313 HV114_06 2.0
    @1315 HV114_07 2.0
    @1317 HV114_08 2.0
    @1319 HV114_09 2.0
    @1321 HV114_10 2.0
    @1323 HV114_11 2.0
    @1325 HV114_12 2.0
    @1327 HV114_13 2.0
    @1329 HV114_14 2.0
    @1331 HV114_15 2.0
    @1333 HV114_16 2.0
    @1335 HV114_17 2.0
    @1337 HV114_18 2.0
    @1339 HV114_19 2.0
    @1341 HV114_20 2.0
    @1343 HV114_21 2.0
    @1345 HV114_22 2.0
    @1347 HV114_23 2.0
    @1349 HV114_24 2.0
    @1351 HV114_25 2.0
    @1353 HV114_26 2.0
    @1355 HV114_27 2.0
    @1357 HV114_28 2.0
    @1359 HV114_29 2.0
    @1361 HV114_30 2.0
    @1363 HV114_31 2.0
    @1365 HV114_32 2.0
    @1367 HV114_33 2.0
    @1369 HV114_34 2.0
    @1371 HV114_35 2.0
    @1373 HV114_36 2.0
    @1375 HV114_37 2.0
    @1377 HV114_38 2.0
    @1379 HV114_39 2.0
    @1381 HV114_40 2.0
    @1383 HV114_41 2.0
    @1385 HV114_42 2.0
    @1387 HV114_43 2.0
    @1389 HV114_44 2.0
    @1391 HV114_45 2.0
    @1393 HV114_46 2.0
    @1395 HV114_47 2.0
    @1397 HV114_48 2.0
    @1399 HV115_01 1.0
    @1400 HV115_02 1.0
    @1401 HV115_03 1.0
    @1402 HV115_04 1.0
    @1403 HV115_05 1.0
    @1404 HV115_06 1.0
    @1405 HV115_07 1.0
    @1406 HV115_08 1.0
    @1407 HV115_09 1.0
    @1408 HV115_10 1.0
    @1409 HV115_11 1.0
    @1410 HV115_12 1.0
    @1411 HV115_13 1.0
    @1412 HV115_14 1.0
    @1413 HV115_15 1.0
    @1414 HV115_16 1.0
    @1415 HV115_17 1.0
    @1416 HV115_18 1.0
    @1417 HV115_19 1.0
    @1418 HV115_20 1.0
    @1419 HV115_21 1.0
    @1420 HV115_22 1.0
    @1421 HV115_23 1.0
    @1422 HV115_24 1.0
    @1423 HV115_25 1.0
    @1424 HV115_26 1.0
    @1425 HV115_27 1.0
    @1426 HV115_28 1.0
    @1427 HV115_29 1.0
    @1428 HV115_30 1.0
    @1429 HV115_31 1.0
    @1430 HV115_32 1.0
    @1431 HV115_33 1.0
    @1432 HV115_34 1.0
    @1433 HV115_35 1.0
    @1434 HV115_36 1.0
    @1435 HV115_37 1.0
    @1436 HV115_38 1.0
    @1437 HV115_39 1.0
    @1438 HV115_40 1.0
    @1439 HV115_41 1.0
    @1440 HV115_42 1.0
    @1441 HV115_43 1.0
    @1442 HV115_44 1.0
    @1443 HV115_45 1.0
    @1444 HV115_46 1.0
    @1445 HV115_47 1.0
    @1446 HV115_48 1.0
    @1447 HV116_01 1.0
    @1448 HV116_02 1.0
    @1449 HV116_03 1.0
    @1450 HV116_04 1.0
    @1451 HV116_05 1.0
    @1452 HV116_06 1.0
    @1453 HV116_07 1.0
    @1454 HV116_08 1.0
    @1455 HV116_09 1.0
    @1456 HV116_10 1.0
    @1457 HV116_11 1.0
    @1458 HV116_12 1.0
    @1459 HV116_13 1.0
    @1460 HV116_14 1.0
    @1461 HV116_15 1.0
    @1462 HV116_16 1.0
    @1463 HV116_17 1.0
    @1464 HV116_18 1.0
    @1465 HV116_19 1.0
    @1466 HV116_20 1.0
    @1467 HV116_21 1.0
    @1468 HV116_22 1.0
    @1469 HV116_23 1.0
    @1470 HV116_24 1.0
    @1471 HV116_25 1.0
    @1472 HV116_26 1.0
    @1473 HV116_27 1.0
    @1474 HV116_28 1.0
    @1475 HV116_29 1.0
    @1476 HV116_30 1.0
    @1477 HV116_31 1.0
    @1478 HV116_32 1.0
    @1479 HV116_33 1.0
    @1480 HV116_34 1.0
    @1481 HV116_35 1.0
    @1482 HV116_36 1.0
    @1483 HV116_37 1.0
    @1484 HV116_38 1.0
    @1485 HV116_39 1.0
    @1486 HV116_40 1.0
    @1487 HV116_41 1.0
    @1488 HV116_42 1.0
    @1489 HV116_43 1.0
    @1490 HV116_44 1.0
    @1491 HV116_45 1.0
    @1492 HV116_46 1.0
    @1493 HV116_47 1.0
    @1494 HV116_48 1.0
    @1495 HV117_01 1.0
    @1496 HV117_02 1.0
    @1497 HV117_03 1.0
    @1498 HV117_04 1.0
    @1499 HV117_05 1.0
    @1500 HV117_06 1.0
    @1501 HV117_07 1.0
    @1502 HV117_08 1.0
    @1503 HV117_09 1.0
    @1504 HV117_10 1.0
    @1505 HV117_11 1.0
    @1506 HV117_12 1.0
    @1507 HV117_13 1.0
    @1508 HV117_14 1.0
    @1509 HV117_15 1.0
    @1510 HV117_16 1.0
    @1511 HV117_17 1.0
    @1512 HV117_18 1.0
    @1513 HV117_19 1.0
    @1514 HV117_20 1.0
    @1515 HV117_21 1.0
    @1516 HV117_22 1.0
    @1517 HV117_23 1.0
    @1518 HV117_24 1.0
    @1519 HV117_25 1.0
    @1520 HV117_26 1.0
    @1521 HV117_27 1.0
    @1522 HV117_28 1.0
    @1523 HV117_29 1.0
    @1524 HV117_30 1.0
    @1525 HV117_31 1.0
    @1526 HV117_32 1.0
    @1527 HV117_33 1.0
    @1528 HV117_34 1.0
    @1529 HV117_35 1.0
    @1530 HV117_36 1.0
    @1531 HV117_37 1.0
    @1532 HV117_38 1.0
    @1533 HV117_39 1.0
    @1534 HV117_40 1.0
    @1535 HV117_41 1.0
    @1536 HV117_42 1.0
    @1537 HV117_43 1.0
    @1538 HV117_44 1.0
    @1539 HV117_45 1.0
    @1540 HV117_46 1.0
    @1541 HV117_47 1.0
    @1542 HV117_48 1.0
    @1543 HV118_01 1.0
    @1544 HV118_02 1.0
    @1545 HV118_03 1.0
    @1546 HV118_04 1.0
    @1547 HV118_05 1.0
    @1548 HV118_06 1.0
    @1549 HV118_07 1.0
    @1550 HV118_08 1.0
    @1551 HV118_09 1.0
    @1552 HV118_10 1.0
    @1553 HV118_11 1.0
    @1554 HV118_12 1.0
    @1555 HV118_13 1.0
    @1556 HV118_14 1.0
    @1557 HV118_15 1.0
    @1558 HV118_16 1.0
    @1559 HV118_17 1.0
    @1560 HV118_18 1.0
    @1561 HV118_19 1.0
    @1562 HV118_20 1.0
    @1563 HV118_21 1.0
    @1564 HV118_22 1.0
    @1565 HV118_23 1.0
    @1566 HV118_24 1.0
    @1567 HV118_25 1.0
    @1568 HV118_26 1.0
    @1569 HV118_27 1.0
    @1570 HV118_28 1.0
    @1571 HV118_29 1.0
    @1572 HV118_30 1.0
    @1573 HV118_31 1.0
    @1574 HV118_32 1.0
    @1575 HV118_33 1.0
    @1576 HV118_34 1.0
    @1577 HV118_35 1.0
    @1578 HV118_36 1.0
    @1579 HV118_37 1.0
    @1580 HV118_38 1.0
    @1581 HV118_39 1.0
    @1582 HV118_40 1.0
    @1583 HV118_41 1.0
    @1584 HV118_42 1.0
    @1585 HV118_43 1.0
    @1586 HV118_44 1.0
    @1587 HV118_45 1.0
    @1588 HV118_46 1.0
    @1589 HV118_47 1.0
    @1590 HV118_48 1.0
    @1591 HV120_01 1.0
    @1592 HV120_02 1.0
    @1593 HV120_03 1.0
    @1594 HV120_04 1.0
    @1595 HV120_05 1.0
    @1596 HV120_06 1.0
    @1597 HV120_07 1.0
    @1598 HV120_08 1.0
    @1599 HV120_09 1.0
    @1600 HV120_10 1.0
    @1601 HV120_11 1.0
    @1602 HV120_12 1.0
    @1603 HV120_13 1.0
    @1604 HV120_14 1.0
    @1605 HV120_15 1.0
    @1606 HV120_16 1.0
    @1607 HV120_17 1.0
    @1608 HV120_18 1.0
    @1609 HV120_19 1.0
    @1610 HV120_20 1.0
    @1611 HV120_21 1.0
    @1612 HV120_22 1.0
    @1613 HV120_23 1.0
    @1614 HV120_24 1.0
    @1615 HV120_25 1.0
    @1616 HV120_26 1.0
    @1617 HV120_27 1.0
    @1618 HV120_28 1.0
    @1619 HV120_29 1.0
    @1620 HV120_30 1.0
    @1621 HV120_31 1.0
    @1622 HV120_32 1.0
    @1623 HV120_33 1.0
    @1624 HV120_34 1.0
    @1625 HV120_35 1.0
    @1626 HV120_36 1.0
    @1627 HV120_37 1.0
    @1628 HV120_38 1.0
    @1629 HV120_39 1.0
    @1630 HV120_40 1.0
    @1631 HV120_41 1.0
    @1632 HV120_42 1.0
    @1633 HV120_43 1.0
    @1634 HV120_44 1.0
    @1635 HV120_45 1.0
    @1636 HV120_46 1.0
    @1637 HV120_47 1.0
    @1638 HV120_48 1.0
    @1639 HV121_01 1.0
    @1640 HV121_02 1.0
    @1641 HV121_03 1.0
    @1642 HV121_04 1.0
    @1643 HV121_05 1.0
    @1644 HV121_06 1.0
    @1645 HV121_07 1.0
    @1646 HV121_08 1.0
    @1647 HV121_09 1.0
    @1648 HV121_10 1.0
    @1649 HV121_11 1.0
    @1650 HV121_12 1.0
    @1651 HV121_13 1.0
    @1652 HV121_14 1.0
    @1653 HV121_15 1.0
    @1654 HV121_16 1.0
    @1655 HV121_17 1.0
    @1656 HV121_18 1.0
    @1657 HV121_19 1.0
    @1658 HV121_20 1.0
    @1659 HV121_21 1.0
    @1660 HV121_22 1.0
    @1661 HV121_23 1.0
    @1662 HV121_24 1.0
    @1663 HV121_25 1.0
    @1664 HV121_26 1.0
    @1665 HV121_27 1.0
    @1666 HV121_28 1.0
    @1667 HV121_29 1.0
    @1668 HV121_30 1.0
    @1669 HV121_31 1.0
    @1670 HV121_32 1.0
    @1671 HV121_33 1.0
    @1672 HV121_34 1.0
    @1673 HV121_35 1.0
    @1674 HV121_36 1.0
    @1675 HV121_37 1.0
    @1676 HV121_38 1.0
    @1677 HV121_39 1.0
    @1678 HV121_40 1.0
    @1679 HV121_41 1.0
    @1680 HV121_42 1.0
    @1681 HV121_43 1.0
    @1682 HV121_44 1.0
    @1683 HV121_45 1.0
    @1684 HV121_46 1.0
    @1685 HV121_47 1.0
    @1686 HV121_48 1.0
    @1687 HV122_01 1.0
    @1688 HV122_02 1.0
    @1689 HV122_03 1.0
    @1690 HV122_04 1.0
    @1691 HV122_05 1.0
    @1692 HV122_06 1.0
    @1693 HV122_07 1.0
    @1694 HV122_08 1.0
    @1695 HV122_09 1.0
    @1696 HV122_10 1.0
    @1697 HV122_11 1.0
    @1698 HV122_12 1.0
    @1699 HV122_13 1.0
    @1700 HV122_14 1.0
    @1701 HV122_15 1.0
    @1702 HV122_16 1.0
    @1703 HV122_17 1.0
    @1704 HV122_18 1.0
    @1705 HV122_19 1.0
    @1706 HV122_20 1.0
    @1707 HV122_21 1.0
    @1708 HV122_22 1.0
    @1709 HV122_23 1.0
    @1710 HV122_24 1.0
    @1711 HV122_25 1.0
    @1712 HV122_26 1.0
    @1713 HV122_27 1.0
    @1714 HV122_28 1.0
    @1715 HV122_29 1.0
    @1716 HV122_30 1.0
    @1717 HV122_31 1.0
    @1718 HV122_32 1.0
    @1719 HV122_33 1.0
    @1720 HV122_34 1.0
    @1721 HV122_35 1.0
    @1722 HV122_36 1.0
    @1723 HV122_37 1.0
    @1724 HV122_38 1.0
    @1725 HV122_39 1.0
    @1726 HV122_40 1.0
    @1727 HV122_41 1.0
    @1728 HV122_42 1.0
    @1729 HV122_43 1.0
    @1730 HV122_44 1.0
    @1731 HV122_45 1.0
    @1732 HV122_46 1.0
    @1733 HV122_47 1.0
    @1734 HV122_48 1.0
    @1735 HV123_01 2.0
    @1737 HV123_02 2.0
    @1739 HV123_03 2.0
    @1741 HV123_04 2.0
    @1743 HV123_05 2.0
    @1745 HV123_06 2.0
    @1747 HV123_07 2.0
    @1749 HV123_08 2.0
    @1751 HV123_09 2.0
    @1753 HV123_10 2.0
    @1755 HV123_11 2.0
    @1757 HV123_12 2.0
    @1759 HV123_13 2.0
    @1761 HV123_14 2.0
    @1763 HV123_15 2.0
    @1765 HV123_16 2.0
    @1767 HV123_17 2.0
    @1769 HV123_18 2.0
    @1771 HV123_19 2.0
    @1773 HV123_20 2.0
    @1775 HV123_21 2.0
    @1777 HV123_22 2.0
    @1779 HV123_23 2.0
    @1781 HV123_24 2.0
    @1783 HV123_25 2.0
    @1785 HV123_26 2.0
    @1787 HV123_27 2.0
    @1789 HV123_28 2.0
    @1791 HV123_29 2.0
    @1793 HV123_30 2.0
    @1795 HV123_31 2.0
    @1797 HV123_32 2.0
    @1799 HV123_33 2.0
    @1801 HV123_34 2.0
    @1803 HV123_35 2.0
    @1805 HV123_36 2.0
    @1807 HV123_37 2.0
    @1809 HV123_38 2.0
    @1811 HV123_39 2.0
    @1813 HV123_40 2.0
    @1815 HV123_41 2.0
    @1817 HV123_42 2.0
    @1819 HV123_43 2.0
    @1821 HV123_44 2.0
    @1823 HV123_45 2.0
    @1825 HV123_46 2.0
    @1827 HV123_47 2.0
    @1829 HV123_48 2.0
    @1831 HV124_01 2.0
    @1833 HV124_02 2.0
    @1835 HV124_03 2.0
    @1837 HV124_04 2.0
    @1839 HV124_05 2.0
    @1841 HV124_06 2.0
    @1843 HV124_07 2.0
    @1845 HV124_08 2.0
    @1847 HV124_09 2.0
    @1849 HV124_10 2.0
    @1851 HV124_11 2.0
    @1853 HV124_12 2.0
    @1855 HV124_13 2.0
    @1857 HV124_14 2.0
    @1859 HV124_15 2.0
    @1861 HV124_16 2.0
    @1863 HV124_17 2.0
    @1865 HV124_18 2.0
    @1867 HV124_19 2.0
    @1869 HV124_20 2.0
    @1871 HV124_21 2.0
    @1873 HV124_22 2.0
    @1875 HV124_23 2.0
    @1877 HV124_24 2.0
    @1879 HV124_25 2.0
    @1881 HV124_26 2.0
    @1883 HV124_27 2.0
    @1885 HV124_28 2.0
    @1887 HV124_29 2.0
    @1889 HV124_30 2.0
    @1891 HV124_31 2.0
    @1893 HV124_32 2.0
    @1895 HV124_33 2.0
    @1897 HV124_34 2.0
    @1899 HV124_35 2.0
    @1901 HV124_36 2.0
    @1903 HV124_37 2.0
    @1905 HV124_38 2.0
    @1907 HV124_39 2.0
    @1909 HV124_40 2.0
    @1911 HV124_41 2.0
    @1913 HV124_42 2.0
    @1915 HV124_43 2.0
    @1917 HV124_44 2.0
    @1919 HV124_45 2.0
    @1921 HV124_46 2.0
    @1923 HV124_47 2.0
    @1925 HV124_48 2.0
    @1927 HV125_01 1.0
    @1928 HV125_02 1.0
    @1929 HV125_03 1.0
    @1930 HV125_04 1.0
    @1931 HV125_05 1.0
    @1932 HV125_06 1.0
    @1933 HV125_07 1.0
    @1934 HV125_08 1.0
    @1935 HV125_09 1.0
    @1936 HV125_10 1.0
    @1937 HV125_11 1.0
    @1938 HV125_12 1.0
    @1939 HV125_13 1.0
    @1940 HV125_14 1.0
    @1941 HV125_15 1.0
    @1942 HV125_16 1.0
    @1943 HV125_17 1.0
    @1944 HV125_18 1.0
    @1945 HV125_19 1.0
    @1946 HV125_20 1.0
    @1947 HV125_21 1.0
    @1948 HV125_22 1.0
    @1949 HV125_23 1.0
    @1950 HV125_24 1.0
    @1951 HV125_25 1.0
    @1952 HV125_26 1.0
    @1953 HV125_27 1.0
    @1954 HV125_28 1.0
    @1955 HV125_29 1.0
    @1956 HV125_30 1.0
    @1957 HV125_31 1.0
    @1958 HV125_32 1.0
    @1959 HV125_33 1.0
    @1960 HV125_34 1.0
    @1961 HV125_35 1.0
    @1962 HV125_36 1.0
    @1963 HV125_37 1.0
    @1964 HV125_38 1.0
    @1965 HV125_39 1.0
    @1966 HV125_40 1.0
    @1967 HV125_41 1.0
    @1968 HV125_42 1.0
    @1969 HV125_43 1.0
    @1970 HV125_44 1.0
    @1971 HV125_45 1.0
    @1972 HV125_46 1.0
    @1973 HV125_47 1.0
    @1974 HV125_48 1.0
    @1975 HV126_01 1.0
    @1976 HV126_02 1.0
    @1977 HV126_03 1.0
    @1978 HV126_04 1.0
    @1979 HV126_05 1.0
    @1980 HV126_06 1.0
    @1981 HV126_07 1.0
    @1982 HV126_08 1.0
    @1983 HV126_09 1.0
    @1984 HV126_10 1.0
    @1985 HV126_11 1.0
    @1986 HV126_12 1.0
    @1987 HV126_13 1.0
    @1988 HV126_14 1.0
    @1989 HV126_15 1.0
    @1990 HV126_16 1.0
    @1991 HV126_17 1.0
    @1992 HV126_18 1.0
    @1993 HV126_19 1.0
    @1994 HV126_20 1.0
    @1995 HV126_21 1.0
    @1996 HV126_22 1.0
    @1997 HV126_23 1.0
    @1998 HV126_24 1.0
    @1999 HV126_25 1.0
    @2000 HV126_26 1.0
    @2001 HV126_27 1.0
    @2002 HV126_28 1.0
    @2003 HV126_29 1.0
    @2004 HV126_30 1.0
    @2005 HV126_31 1.0
    @2006 HV126_32 1.0
    @2007 HV126_33 1.0
    @2008 HV126_34 1.0
    @2009 HV126_35 1.0
    @2010 HV126_36 1.0
    @2011 HV126_37 1.0
    @2012 HV126_38 1.0
    @2013 HV126_39 1.0
    @2014 HV126_40 1.0
    @2015 HV126_41 1.0
    @2016 HV126_42 1.0
    @2017 HV126_43 1.0
    @2018 HV126_44 1.0
    @2019 HV126_45 1.0
    @2020 HV126_46 1.0
    @2021 HV126_47 1.0
    @2022 HV126_48 1.0
    @2023 HV127_01 2.0
    @2025 HV127_02 2.0
    @2027 HV127_03 2.0
    @2029 HV127_04 2.0
    @2031 HV127_05 2.0
    @2033 HV127_06 2.0
    @2035 HV127_07 2.0
    @2037 HV127_08 2.0
    @2039 HV127_09 2.0
    @2041 HV127_10 2.0
    @2043 HV127_11 2.0
    @2045 HV127_12 2.0
    @2047 HV127_13 2.0
    @2049 HV127_14 2.0
    @2051 HV127_15 2.0
    @2053 HV127_16 2.0
    @2055 HV127_17 2.0
    @2057 HV127_18 2.0
    @2059 HV127_19 2.0
    @2061 HV127_20 2.0
    @2063 HV127_21 2.0
    @2065 HV127_22 2.0
    @2067 HV127_23 2.0
    @2069 HV127_24 2.0
    @2071 HV127_25 2.0
    @2073 HV127_26 2.0
    @2075 HV127_27 2.0
    @2077 HV127_28 2.0
    @2079 HV127_29 2.0
    @2081 HV127_30 2.0
    @2083 HV127_31 2.0
    @2085 HV127_32 2.0
    @2087 HV127_33 2.0
    @2089 HV127_34 2.0
    @2091 HV127_35 2.0
    @2093 HV127_36 2.0
    @2095 HV127_37 2.0
    @2097 HV127_38 2.0
    @2099 HV127_39 2.0
    @2101 HV127_40 2.0
    @2103 HV127_41 2.0
    @2105 HV127_42 2.0
    @2107 HV127_43 2.0
    @2109 HV127_44 2.0
    @2111 HV127_45 2.0
    @2113 HV127_46 2.0
    @2115 HV127_47 2.0
    @2117 HV127_48 2.0
    @2119 HV128_01 2.0
    @2121 HV128_02 2.0
    @2123 HV128_03 2.0
    @2125 HV128_04 2.0
    @2127 HV128_05 2.0
    @2129 HV128_06 2.0
    @2131 HV128_07 2.0
    @2133 HV128_08 2.0
    @2135 HV128_09 2.0
    @2137 HV128_10 2.0
    @2139 HV128_11 2.0
    @2141 HV128_12 2.0
    @2143 HV128_13 2.0
    @2145 HV128_14 2.0
    @2147 HV128_15 2.0
    @2149 HV128_16 2.0
    @2151 HV128_17 2.0
    @2153 HV128_18 2.0
    @2155 HV128_19 2.0
    @2157 HV128_20 2.0
    @2159 HV128_21 2.0
    @2161 HV128_22 2.0
    @2163 HV128_23 2.0
    @2165 HV128_24 2.0
    @2167 HV128_25 2.0
    @2169 HV128_26 2.0
    @2171 HV128_27 2.0
    @2173 HV128_28 2.0
    @2175 HV128_29 2.0
    @2177 HV128_30 2.0
    @2179 HV128_31 2.0
    @2181 HV128_32 2.0
    @2183 HV128_33 2.0
    @2185 HV128_34 2.0
    @2187 HV128_35 2.0
    @2189 HV128_36 2.0
    @2191 HV128_37 2.0
    @2193 HV128_38 2.0
    @2195 HV128_39 2.0
    @2197 HV128_40 2.0
    @2199 HV128_41 2.0
    @2201 HV128_42 2.0
    @2203 HV128_43 2.0
    @2205 HV128_44 2.0
    @2207 HV128_45 2.0
    @2209 HV128_46 2.0
    @2211 HV128_47 2.0
    @2213 HV128_48 2.0
    @2215 HV129_01 1.0
    @2216 HV129_02 1.0
    @2217 HV129_03 1.0
    @2218 HV129_04 1.0
    @2219 HV129_05 1.0
    @2220 HV129_06 1.0
    @2221 HV129_07 1.0
    @2222 HV129_08 1.0
    @2223 HV129_09 1.0
    @2224 HV129_10 1.0
    @2225 HV129_11 1.0
    @2226 HV129_12 1.0
    @2227 HV129_13 1.0
    @2228 HV129_14 1.0
    @2229 HV129_15 1.0
    @2230 HV129_16 1.0
    @2231 HV129_17 1.0
    @2232 HV129_18 1.0
    @2233 HV129_19 1.0
    @2234 HV129_20 1.0
    @2235 HV129_21 1.0
    @2236 HV129_22 1.0
    @2237 HV129_23 1.0
    @2238 HV129_24 1.0
    @2239 HV129_25 1.0
    @2240 HV129_26 1.0
    @2241 HV129_27 1.0
    @2242 HV129_28 1.0
    @2243 HV129_29 1.0
    @2244 HV129_30 1.0
    @2245 HV129_31 1.0
    @2246 HV129_32 1.0
    @2247 HV129_33 1.0
    @2248 HV129_34 1.0
    @2249 HV129_35 1.0
    @2250 HV129_36 1.0
    @2251 HV129_37 1.0
    @2252 HV129_38 1.0
    @2253 HV129_39 1.0
    @2254 HV129_40 1.0
    @2255 HV129_41 1.0
    @2256 HV129_42 1.0
    @2257 HV129_43 1.0
    @2258 HV129_44 1.0
    @2259 HV129_45 1.0
    @2260 HV129_46 1.0
    @2261 HV129_47 1.0
    @2262 HV129_48 1.0
    @2263 HV140_01 1.0
    @2264 HV140_02 1.0
    @2265 HV140_03 1.0
    @2266 HV140_04 1.0
    @2267 HV140_05 1.0
    @2268 HV140_06 1.0
    @2269 HV140_07 1.0
    @2270 HV140_08 1.0
    @2271 HV140_09 1.0
    @2272 HV140_10 1.0
    @2273 HV140_11 1.0
    @2274 HV140_12 1.0
    @2275 HV140_13 1.0
    @2276 HV140_14 1.0
    @2277 HV140_15 1.0
    @2278 HV140_16 1.0
    @2279 HV140_17 1.0
    @2280 HV140_18 1.0
    @2281 HV140_19 1.0
    @2282 HV140_20 1.0
    @2283 HV140_21 1.0
    @2284 HV140_22 1.0
    @2285 HV140_23 1.0
    @2286 HV140_24 1.0
    @2287 HV140_25 1.0
    @2288 HV140_26 1.0
    @2289 HV140_27 1.0
    @2290 HV140_28 1.0
    @2291 HV140_29 1.0
    @2292 HV140_30 1.0
    @2293 HV140_31 1.0
    @2294 HV140_32 1.0
    @2295 HV140_33 1.0
    @2296 HV140_34 1.0
    @2297 HV140_35 1.0
    @2298 HV140_36 1.0
    @2299 HV140_37 1.0
    @2300 HV140_38 1.0
    @2301 HV140_39 1.0
    @2302 HV140_40 1.0
    @2303 HV140_41 1.0
    @2304 HV140_42 1.0
    @2305 HV140_43 1.0
    @2306 HV140_44 1.0
    @2307 HV140_45 1.0
    @2308 HV140_46 1.0
    @2309 HV140_47 1.0
    @2310 HV140_48 1.0
    @2311 IDXH4_01 2.0
    @2313 IDXH4_02 2.0
    @2315 IDXH4_03 2.0
    @2317 IDXH4_04 2.0
    @2319 IDXH4_05 2.0
    @2321 IDXH4_06 2.0
    @2323 IDXH4_07 2.0
    @2325 IDXH4_08 2.0
    @2327 IDXH4_09 2.0
    @2329 IDXH4_10 2.0
    @2331 IDXH4_11 2.0
    @2333 IDXH4_12 2.0
    @2335 IDXH4_13 2.0
    @2337 IDXH4_14 2.0
    @2339 IDXH4_15 2.0
    @2341 IDXH4_16 2.0
    @2343 IDXH4_17 2.0
    @2345 IDXH4_18 2.0
    @2347 IDXH4_19 2.0
    @2349 IDXH4_20 2.0
    @2351 IDXH4_21 2.0
    @2353 IDXH4_22 2.0
    @2355 IDXH4_23 2.0
    @2357 IDXH4_24 2.0
    @2359 IDXH4_25 2.0
    @2361 IDXH4_26 2.0
    @2363 IDXH4_27 2.0
    @2365 IDXH4_28 2.0
    @2367 IDXH4_29 2.0
    @2369 IDXH4_30 2.0
    @2371 IDXH4_31 2.0
    @2373 IDXH4_32 2.0
    @2375 IDXH4_33 2.0
    @2377 IDXH4_34 2.0
    @2379 IDXH4_35 2.0
    @2381 IDXH4_36 2.0
    @2383 IDXH4_37 2.0
    @2385 IDXH4_38 2.0
    @2387 IDXH4_39 2.0
    @2389 IDXH4_40 2.0
    @2391 IDXH4_41 2.0
    @2393 IDXH4_42 2.0
    @2395 IDXH4_43 2.0
    @2397 IDXH4_44 2.0
    @2399 IDXH4_45 2.0
    @2401 IDXH4_46 2.0
    @2403 IDXH4_47 2.0
    @2405 IDXH4_48 2.0
    @2407 SH17A_01 1.0
    @2408 SH17A_02 1.0
    @2409 SH17A_03 1.0
    @2410 SH17A_04 1.0
    @2411 SH17A_05 1.0
    @2412 SH17A_06 1.0
    @2413 SH17A_07 1.0
    @2414 SH17A_08 1.0
    @2415 SH17A_09 1.0
    @2416 SH17A_10 1.0
    @2417 SH17A_11 1.0
    @2418 SH17A_12 1.0
    @2419 SH17A_13 1.0
    @2420 SH17A_14 1.0
    @2421 SH17A_15 1.0
    @2422 SH17A_16 1.0
    @2423 SH17A_17 1.0
    @2424 SH17A_18 1.0
    @2425 SH17A_19 1.0
    @2426 SH17A_20 1.0
    @2427 SH17A_21 1.0
    @2428 SH17A_22 1.0
    @2429 SH17A_23 1.0
    @2430 SH17A_24 1.0
    @2431 SH17A_25 1.0
    @2432 SH17A_26 1.0
    @2433 SH17A_27 1.0
    @2434 SH17A_28 1.0
    @2435 SH17A_29 1.0
    @2436 SH17A_30 1.0
    @2437 SH17A_31 1.0
    @2438 SH17A_32 1.0
    @2439 SH17A_33 1.0
    @2440 SH17A_34 1.0
    @2441 SH17A_35 1.0
    @2442 SH17A_36 1.0
    @2443 SH17A_37 1.0
    @2444 SH17A_38 1.0
    @2445 SH17A_39 1.0
    @2446 SH17A_40 1.0
    @2447 SH17A_41 1.0
    @2448 SH17A_42 1.0
    @2449 SH17A_43 1.0
    @2450 SH17A_44 1.0
    @2451 SH17A_45 1.0
    @2452 SH17A_46 1.0
    @2453 SH17A_47 1.0
    @2454 SH17A_48 1.0
    @2455 SH19A_01 1.0
    @2456 SH19A_02 1.0
    @2457 SH19A_03 1.0
    @2458 SH19A_04 1.0
    @2459 SH19A_05 1.0
    @2460 SH19A_06 1.0
    @2461 SH19A_07 1.0
    @2462 SH19A_08 1.0
    @2463 SH19A_09 1.0
    @2464 SH19A_10 1.0
    @2465 SH19A_11 1.0
    @2466 SH19A_12 1.0
    @2467 SH19A_13 1.0
    @2468 SH19A_14 1.0
    @2469 SH19A_15 1.0
    @2470 SH19A_16 1.0
    @2471 SH19A_17 1.0
    @2472 SH19A_18 1.0
    @2473 SH19A_19 1.0
    @2474 SH19A_20 1.0
    @2475 SH19A_21 1.0
    @2476 SH19A_22 1.0
    @2477 SH19A_23 1.0
    @2478 SH19A_24 1.0
    @2479 SH19A_25 1.0
    @2480 SH19A_26 1.0
    @2481 SH19A_27 1.0
    @2482 SH19A_28 1.0
    @2483 SH19A_29 1.0
    @2484 SH19A_30 1.0
    @2485 SH19A_31 1.0
    @2486 SH19A_32 1.0
    @2487 SH19A_33 1.0
    @2488 SH19A_34 1.0
    @2489 SH19A_35 1.0
    @2490 SH19A_36 1.0
    @2491 SH19A_37 1.0
    @2492 SH19A_38 1.0
    @2493 SH19A_39 1.0
    @2494 SH19A_40 1.0
    @2495 SH19A_41 1.0
    @2496 SH19A_42 1.0
    @2497 SH19A_43 1.0
    @2498 SH19A_44 1.0
    @2499 SH19A_45 1.0
    @2500 SH19A_46 1.0
    @2501 SH19A_47 1.0
    @2502 SH19A_48 1.0
    @2503 HA0_1    2.0
    @2505 HA0_2    2.0
    @2507 HA1_1    2.0
    @2509 HA1_2    2.0
    @2511 HA2_1    4.0
    @2515 HA2_2    4.0
    @2519 HA3_1    4.0
    @2523 HA3_2    4.0
    @2527 HA4_1    4.0
    @2531 HA4_2    4.0
    @2535 HA5_1    4.0
    @2539 HA5_2    4.0
    @2543 HA6_1    5.0
    @2548 HA6_2    5.0
    @2553 HA11_1   4.0
    @2557 HA11_2   4.0
    @2561 HA12_1   5.0
    @2566 HA12_2   5.0
    @2571 HA12A_1  5.0
    @2576 HA12A_2  5.0
    @2581 HA12B_1  5.0
    @2586 HA12B_2  5.0
    @2591 HA13_1   1.0
    @2592 HA13_2   1.0
    @2593 HA21_1   5.0
    @2598 HA21_2   5.0
    @2603 HA32_1   4.0
    @2607 HA32_2   4.0
    @2611 HA33_1   1.0
    @2612 HA33_2   1.0
    @2613 HA35_1   2.0
    @2615 HA35_2   2.0
    @2617 HA40_1   4.0
    @2621 HA40_2   4.0
    @2625 HA41_1   4.0
    @2629 HA41_2   4.0
    @2633 HA50_1   1.0
    @2634 HA50_2   1.0
    @2635 HA51_1   2.0
    @2637 HA51_2   2.0
    @2639 HA52_1   1.0
    @2640 HA52_2   1.0
    @2641 HA53_1   3.0
    @2644 HA53_2   3.0
    @2647 HA54_1   1.0
    @2648 HA54_2   1.0
    @2649 HA55_1   1.0
    @2650 HA55_2   1.0
    @2651 HA56_1   3.0
    @2654 HA56_2   3.0
    @2657 HA57_1   1.0
    @2658 HA57_2   1.0
    @2659 HA58_1   1.0
    @2660 HA58_2   1.0
    @2661 HA60_1   1.0
    @2662 HA60_2   1.0
    @2663 HA61_1   1.0
    @2664 HA61_2   1.0
    @2665 HA62_1   $5.
    @2670 HA62_2   $5.
    @2675 HA63_1   1.0
    @2676 HA63_2   1.0
    @2677 HA64_1   1.0
    @2678 HA64_2   1.0
    @2679 HA65_1   1.0
    @2680 HA65_2   1.0
    @2681 HA66_1   1.0
    @2682 HA66_2   1.0
    @2683 HA67_1   2.0
    @2685 HA67_2   2.0
    @2687 HA68_1   1.0
    @2688 HA68_2   1.0
    @2689 HA69_1   8.0
    @2697 HA69_2   8.0
    @2705 HA70_1   5.0
    @2710 HA70_2   5.0
    @2715 HC0_1    2.0
    @2717 HC0_2    2.0
    @2719 HC1_1    2.0
    @2721 HC1_2    2.0
    @2723 HC1A_1   5.0
    @2728 HC1A_2   5.0
    @2733 HC2_1    4.0
    @2737 HC2_2    4.0
    @2741 HC3_1    4.0
    @2745 HC3_2    4.0
    @2749 HC4_1    4.0
    @2753 HC4_2    4.0
    @2757 HC5_1    4.0
    @2761 HC5_2    4.0
    @2765 HC6_1    5.0
    @2770 HC6_2    5.0
    @2775 HC7_1    4.0
    @2779 HC7_2    4.0
    @2783 HC8_1    4.0
    @2787 HC8_2    4.0
    @2791 HC9_1    5.0
    @2796 HC9_2    5.0
    @2801 HC10_1   4.0
    @2805 HC10_2   4.0
    @2809 HC11_1   4.0
    @2813 HC11_2   4.0
    @2817 HC12_1   5.0
    @2822 HC12_2   5.0
    @2827 HC13_1   1.0
    @2828 HC13_2   1.0
    @2829 HC15_1   1.0
    @2830 HC15_2   1.0
    @2831 HC16_1   2.0
    @2833 HC16_2   2.0
    @2835 HC17_1   2.0
    @2837 HC17_2   2.0
    @2839 HC18_1   2.0
    @2841 HC18_2   2.0
    @2843 HC19_1   4.0
    @2847 HC19_2   4.0
    @2851 HC20_1   5.0
    @2856 HC20_2   5.0
    @2861 HC21_1   5.0
    @2866 HC21_2   5.0
    @2871 HC27_1   1.0
    @2872 HC27_2   1.0
    @2873 HC30_1   2.0
    @2875 HC30_2   2.0
    @2877 HC31_1   4.0
    @2881 HC31_2   4.0
    @2885 HC32_1   4.0
    @2889 HC32_2   4.0
    @2893 HC32A_1  5.0
    @2898 HC32A_2  5.0
    @2903 HC33_1   1.0
    @2904 HC33_2   1.0
    @2905 HC51_1   2.0
    @2907 HC51_2   2.0
    @2909 HC52_1   1.0
    @2910 HC52_2   1.0
    @2911 HC53_1   3.0
    @2914 HC53_2   3.0
    @2917 HC55_1   1.0
    @2918 HC55_2   1.0
    @2919 HC56_1   3.0
    @2922 HC56_2   3.0
    @2925 HC57_1   1.0
    @2926 HC57_2   1.0
    @2927 HC58_1   1.0
    @2928 HC58_2   1.0
    @2929 HC60_1   3.0
    @2932 HC60_2   3.0
    @2935 HC61_1   1.0
    @2936 HC61_2   1.0
    @2937 HC62_1   2.0
    @2939 HC62_2   2.0
    @2941 HC63_1   3.0
    @2944 HC63_2   3.0
    @2947 HC64_1   2.0
    @2949 HC64_2   2.0
    @2951 HC68_1   1.0
    @2952 HC68_2   1.0
    @2953 HC70_1   4.0
    @2957 HC70_2   4.0
    @2961 HC71_1   4.0
    @2965 HC71_2   4.0
    @2969 HC72_1   4.0
    @2973 HC72_2   4.0
    @2977 HC73_1   4.0
    @2981 HC73_2   4.0
    @2985 HMLIDX_1 2.0
    @2987 HMLIDX_2 2.0
    @2989 HMLIDX_3 2.0
    @2991 HMLIDX_4 2.0
    @2993 HMLIDX_5 2.0
    @2995 HMLIDX_6 2.0
    @2997 HMLIDX_7 2.0
    @2999 HML3_1   1.0
    @3000 HML3_2   1.0
    @3001 HML3_3   1.0
    @3002 HML3_4   1.0
    @3003 HML3_5   1.0
    @3004 HML3_6   1.0
    @3005 HML3_7   1.0
    @3006 HML4_1   2.0
    @3008 HML4_2   2.0
    @3010 HML4_3   2.0
    @3012 HML4_4   2.0
    @3014 HML4_5   2.0
    @3016 HML4_6   2.0
    @3018 HML4_7   2.0
    @3020 HML5_1   1.0
    @3021 HML5_2   1.0
    @3022 HML5_3   1.0
    @3023 HML5_4   1.0
    @3024 HML5_5   1.0
    @3025 HML5_6   1.0
    @3026 HML5_7   1.0
    @3027 HML6_1   1.0
    @3028 HML6_2   1.0
    @3029 HML6_3   1.0
    @3030 HML6_4   1.0
    @3031 HML6_5   1.0
    @3032 HML6_6   1.0
    @3033 HML6_7   1.0
    @3034 HML7_1   2.0
    @3036 HML7_2   2.0
    @3038 HML7_3   2.0
    @3040 HML7_4   2.0
    @3042 HML7_5   2.0
    @3044 HML7_6   2.0
    @3046 HML7_7   2.0
    @3048 HML8_1   1.0
    @3049 HML8_2   1.0
    @3050 HML8_3   1.0
    @3051 HML8_4   1.0
    @3052 HML8_5   1.0
    @3053 HML8_6   1.0
    @3054 HML8_7   1.0
    @3055 HML9_1   2.0
    @3057 HML9_2   2.0
    @3059 HML9_3   2.0
    @3061 HML9_4   2.0
    @3063 HML9_5   2.0
    @3065 HML9_6   2.0
    @3067 HML9_7   2.0
    @3069 HML10_1  1.0
    @3070 HML10_2  1.0
    @3071 HML10_3  1.0
    @3072 HML10_4  1.0
    @3073 HML10_5  1.0
    @3074 HML10_6  1.0
    @3075 HML10_7  1.0
    @3076 HML11_1  1.0
    @3077 HML11_2  1.0
    @3078 HML11_3  1.0
    @3079 HML11_4  1.0
    @3080 HML11_5  1.0
    @3081 HML11_6  1.0
    @3082 HML11_7  1.0
    @3083 HML21_1  1.0
    @3084 HML21_2  1.0
    @3085 HML21_3  1.0
    @3086 HML21_4  1.0
    @3087 HML21_5  1.0
    @3088 HML21_6  1.0
    @3089 HML21_7  1.0
    @3090 HMLA_1   2.0
    @3092 HMLA_2   2.0
    @3094 HMLA_3   2.0
    @3096 HMLA_4   2.0
    @3098 HMLA_5   2.0
    @3100 HMLA_6   2.0
    @3102 HMLA_7   2.0
    @3104 HMLB_1   2.0
    @3106 HMLB_2   2.0
    @3108 HMLB_3   2.0
    @3110 HMLB_4   2.0
    @3112 HMLB_5   2.0
    @3114 HMLB_6   2.0
    @3116 HMLB_7   2.0
    @3118 HMLC_1   2.0
    @3120 HMLC_2   2.0
    @3122 HMLC_3   2.0
    @3124 HMLC_4   2.0
    @3126 HMLC_5   2.0
    @3128 HMLC_6   2.0
    @3130 HMLC_7   2.0
    @3132 HMLD_1   2.0
    @3134 HMLD_2   2.0
    @3136 HMLD_3   2.0
    @3138 HMLD_4   2.0
    @3140 HMLD_5   2.0
    @3142 HMLD_6   2.0
    @3144 HMLD_7   2.0
    @3146 HMLE_1   2.0
    @3148 HMLE_2   2.0
    @3150 HMLE_3   2.0
    @3152 HMLE_4   2.0
    @3154 HMLE_5   2.0
    @3156 HMLE_6   2.0
    @3158 HMLE_7   2.0
    @3160 HML22_1  1.0
    @3161 HML22_2  1.0
    @3162 HML22_3  1.0
    @3163 HML22_4  1.0
    @3164 HML22_5  1.0
    @3165 HML22_6  1.0
    @3166 HML22_7  1.0
    @3167 HML23_1  2.0
    @3169 HML23_2  2.0
    @3171 HML23_3  2.0
    @3173 HML23_4  2.0
    @3175 HML23_5  2.0
    @3177 HML23_6  2.0
    @3179 HML23_7  2.0
    @3181 HMHIDX_01 2.0
    @3183 HMHIDX_02 2.0
    @3185 HMHIDX_03 2.0
    @3187 HMHIDX_04 2.0
    @3189 HMHIDX_05 2.0
    @3191 HMHIDX_06 2.0
    @3193 HMHIDX_07 2.0
    @3195 HMHIDX_08 2.0
    @3197 HMHIDX_09 2.0
    @3199 HMHIDX_10 2.0
    @3201 HMHIDX_11 2.0
    @3203 HMHIDX_12 2.0
    @3205 HMHIDX_13 2.0
    @3207 HMHIDX_14 2.0
    @3209 HMHIDX_15 2.0
    @3211 HMHIDX_16 2.0
    @3213 HMHIDX_17 2.0
    @3215 HMHIDX_18 2.0
    @3217 HMHIDX_19 2.0
    @3219 HMHIDX_20 2.0
    @3221 HMHIDX_21 2.0
    @3223 HMHIDX_22 2.0
    @3225 HMHIDX_23 2.0
    @3227 HMHIDX_24 2.0
    @3229 HMHIDX_25 2.0
    @3231 HMHIDX_26 2.0
    @3233 HMHIDX_27 2.0
    @3235 HMHIDX_28 2.0
    @3237 HMHIDX_29 2.0
    @3239 HMHIDX_30 2.0
    @3241 HMHIDX_31 2.0
    @3243 HMHIDX_32 2.0
    @3245 HMHIDX_33 2.0
    @3247 HMHIDX_34 2.0
    @3249 HMHIDX_35 2.0
    @3251 HMHIDX_36 2.0
    @3253 HMHIDX_37 2.0
    @3255 HMHIDX_38 2.0
    @3257 HMHIDX_39 2.0
    @3259 HMHIDX_40 2.0
    @3261 HMHIDX_41 2.0
    @3263 HMHIDX_42 2.0
    @3265 HMHIDX_43 2.0
    @3267 HMHIDX_44 2.0
    @3269 HMHIDX_45 2.0
    @3271 HMHIDX_46 2.0
    @3273 HMHIDX_47 2.0
    @3275 HMHIDX_48 2.0
    @3277 HML12_01 1.0
    @3278 HML12_02 1.0
    @3279 HML12_03 1.0
    @3280 HML12_04 1.0
    @3281 HML12_05 1.0
    @3282 HML12_06 1.0
    @3283 HML12_07 1.0
    @3284 HML12_08 1.0
    @3285 HML12_09 1.0
    @3286 HML12_10 1.0
    @3287 HML12_11 1.0
    @3288 HML12_12 1.0
    @3289 HML12_13 1.0
    @3290 HML12_14 1.0
    @3291 HML12_15 1.0
    @3292 HML12_16 1.0
    @3293 HML12_17 1.0
    @3294 HML12_18 1.0
    @3295 HML12_19 1.0
    @3296 HML12_20 1.0
    @3297 HML12_21 1.0
    @3298 HML12_22 1.0
    @3299 HML12_23 1.0
    @3300 HML12_24 1.0
    @3301 HML12_25 1.0
    @3302 HML12_26 1.0
    @3303 HML12_27 1.0
    @3304 HML12_28 1.0
    @3305 HML12_29 1.0
    @3306 HML12_30 1.0
    @3307 HML12_31 1.0
    @3308 HML12_32 1.0
    @3309 HML12_33 1.0
    @3310 HML12_34 1.0
    @3311 HML12_35 1.0
    @3312 HML12_36 1.0
    @3313 HML12_37 1.0
    @3314 HML12_38 1.0
    @3315 HML12_39 1.0
    @3316 HML12_40 1.0
    @3317 HML12_41 1.0
    @3318 HML12_42 1.0
    @3319 HML12_43 1.0
    @3320 HML12_44 1.0
    @3321 HML12_45 1.0
    @3322 HML12_46 1.0
    @3323 HML12_47 1.0
    @3324 HML12_48 1.0
    @3325 HML13_01 1.0
    @3326 HML13_02 1.0
    @3327 HML13_03 1.0
    @3328 HML13_04 1.0
    @3329 HML13_05 1.0
    @3330 HML13_06 1.0
    @3331 HML13_07 1.0
    @3332 HML13_08 1.0
    @3333 HML13_09 1.0
    @3334 HML13_10 1.0
    @3335 HML13_11 1.0
    @3336 HML13_12 1.0
    @3337 HML13_13 1.0
    @3338 HML13_14 1.0
    @3339 HML13_15 1.0
    @3340 HML13_16 1.0
    @3341 HML13_17 1.0
    @3342 HML13_18 1.0
    @3343 HML13_19 1.0
    @3344 HML13_20 1.0
    @3345 HML13_21 1.0
    @3346 HML13_22 1.0
    @3347 HML13_23 1.0
    @3348 HML13_24 1.0
    @3349 HML13_25 1.0
    @3350 HML13_26 1.0
    @3351 HML13_27 1.0
    @3352 HML13_28 1.0
    @3353 HML13_29 1.0
    @3354 HML13_30 1.0
    @3355 HML13_31 1.0
    @3356 HML13_32 1.0
    @3357 HML13_33 1.0
    @3358 HML13_34 1.0
    @3359 HML13_35 1.0
    @3360 HML13_36 1.0
    @3361 HML13_37 1.0
    @3362 HML13_38 1.0
    @3363 HML13_39 1.0
    @3364 HML13_40 1.0
    @3365 HML13_41 1.0
    @3366 HML13_42 1.0
    @3367 HML13_43 1.0
    @3368 HML13_44 1.0
    @3369 HML13_45 1.0
    @3370 HML13_46 1.0
    @3371 HML13_47 1.0
    @3372 HML13_48 1.0
    @3373 HML14_01 1.0
    @3374 HML14_02 1.0
    @3375 HML14_03 1.0
    @3376 HML14_04 1.0
    @3377 HML14_05 1.0
    @3378 HML14_06 1.0
    @3379 HML14_07 1.0
    @3380 HML14_08 1.0
    @3381 HML14_09 1.0
    @3382 HML14_10 1.0
    @3383 HML14_11 1.0
    @3384 HML14_12 1.0
    @3385 HML14_13 1.0
    @3386 HML14_14 1.0
    @3387 HML14_15 1.0
    @3388 HML14_16 1.0
    @3389 HML14_17 1.0
    @3390 HML14_18 1.0
    @3391 HML14_19 1.0
    @3392 HML14_20 1.0
    @3393 HML14_21 1.0
    @3394 HML14_22 1.0
    @3395 HML14_23 1.0
    @3396 HML14_24 1.0
    @3397 HML14_25 1.0
    @3398 HML14_26 1.0
    @3399 HML14_27 1.0
    @3400 HML14_28 1.0
    @3401 HML14_29 1.0
    @3402 HML14_30 1.0
    @3403 HML14_31 1.0
    @3404 HML14_32 1.0
    @3405 HML14_33 1.0
    @3406 HML14_34 1.0
    @3407 HML14_35 1.0
    @3408 HML14_36 1.0
    @3409 HML14_37 1.0
    @3410 HML14_38 1.0
    @3411 HML14_39 1.0
    @3412 HML14_40 1.0
    @3413 HML14_41 1.0
    @3414 HML14_42 1.0
    @3415 HML14_43 1.0
    @3416 HML14_44 1.0
    @3417 HML14_45 1.0
    @3418 HML14_46 1.0
    @3419 HML14_47 1.0
    @3420 HML14_48 1.0
    @3421 HML15_01 1.0
    @3422 HML15_02 1.0
    @3423 HML15_03 1.0
    @3424 HML15_04 1.0
    @3425 HML15_05 1.0
    @3426 HML15_06 1.0
    @3427 HML15_07 1.0
    @3428 HML15_08 1.0
    @3429 HML15_09 1.0
    @3430 HML15_10 1.0
    @3431 HML15_11 1.0
    @3432 HML15_12 1.0
    @3433 HML15_13 1.0
    @3434 HML15_14 1.0
    @3435 HML15_15 1.0
    @3436 HML15_16 1.0
    @3437 HML15_17 1.0
    @3438 HML15_18 1.0
    @3439 HML15_19 1.0
    @3440 HML15_20 1.0
    @3441 HML15_21 1.0
    @3442 HML15_22 1.0
    @3443 HML15_23 1.0
    @3444 HML15_24 1.0
    @3445 HML15_25 1.0
    @3446 HML15_26 1.0
    @3447 HML15_27 1.0
    @3448 HML15_28 1.0
    @3449 HML15_29 1.0
    @3450 HML15_30 1.0
    @3451 HML15_31 1.0
    @3452 HML15_32 1.0
    @3453 HML15_33 1.0
    @3454 HML15_34 1.0
    @3455 HML15_35 1.0
    @3456 HML15_36 1.0
    @3457 HML15_37 1.0
    @3458 HML15_38 1.0
    @3459 HML15_39 1.0
    @3460 HML15_40 1.0
    @3461 HML15_41 1.0
    @3462 HML15_42 1.0
    @3463 HML15_43 1.0
    @3464 HML15_44 1.0
    @3465 HML15_45 1.0
    @3466 HML15_46 1.0
    @3467 HML15_47 1.0
    @3468 HML15_48 1.0
    @3469 HML16_01 2.0
    @3471 HML16_02 2.0
    @3473 HML16_03 2.0
    @3475 HML16_04 2.0
    @3477 HML16_05 2.0
    @3479 HML16_06 2.0
    @3481 HML16_07 2.0
    @3483 HML16_08 2.0
    @3485 HML16_09 2.0
    @3487 HML16_10 2.0
    @3489 HML16_11 2.0
    @3491 HML16_12 2.0
    @3493 HML16_13 2.0
    @3495 HML16_14 2.0
    @3497 HML16_15 2.0
    @3499 HML16_16 2.0
    @3501 HML16_17 2.0
    @3503 HML16_18 2.0
    @3505 HML16_19 2.0
    @3507 HML16_20 2.0
    @3509 HML16_21 2.0
    @3511 HML16_22 2.0
    @3513 HML16_23 2.0
    @3515 HML16_24 2.0
    @3517 HML16_25 2.0
    @3519 HML16_26 2.0
    @3521 HML16_27 2.0
    @3523 HML16_28 2.0
    @3525 HML16_29 2.0
    @3527 HML16_30 2.0
    @3529 HML16_31 2.0
    @3531 HML16_32 2.0
    @3533 HML16_33 2.0
    @3535 HML16_34 2.0
    @3537 HML16_35 2.0
    @3539 HML16_36 2.0
    @3541 HML16_37 2.0
    @3543 HML16_38 2.0
    @3545 HML16_39 2.0
    @3547 HML16_40 2.0
    @3549 HML16_41 2.0
    @3551 HML16_42 2.0
    @3553 HML16_43 2.0
    @3555 HML16_44 2.0
    @3557 HML16_45 2.0
    @3559 HML16_46 2.0
    @3561 HML16_47 2.0
    @3563 HML16_48 2.0
    @3565 HML16A_01 2.0
    @3567 HML16A_02 2.0
    @3569 HML16A_03 2.0
    @3571 HML16A_04 2.0
    @3573 HML16A_05 2.0
    @3575 HML16A_06 2.0
    @3577 HML16A_07 2.0
    @3579 HML16A_08 2.0
    @3581 HML16A_09 2.0
    @3583 HML16A_10 2.0
    @3585 HML16A_11 2.0
    @3587 HML16A_12 2.0
    @3589 HML16A_13 2.0
    @3591 HML16A_14 2.0
    @3593 HML16A_15 2.0
    @3595 HML16A_16 2.0
    @3597 HML16A_17 2.0
    @3599 HML16A_18 2.0
    @3601 HML16A_19 2.0
    @3603 HML16A_20 2.0
    @3605 HML16A_21 2.0
    @3607 HML16A_22 2.0
    @3609 HML16A_23 2.0
    @3611 HML16A_24 2.0
    @3613 HML16A_25 2.0
    @3615 HML16A_26 2.0
    @3617 HML16A_27 2.0
    @3619 HML16A_28 2.0
    @3621 HML16A_29 2.0
    @3623 HML16A_30 2.0
    @3625 HML16A_31 2.0
    @3627 HML16A_32 2.0
    @3629 HML16A_33 2.0
    @3631 HML16A_34 2.0
    @3633 HML16A_35 2.0
    @3635 HML16A_36 2.0
    @3637 HML16A_37 2.0
    @3639 HML16A_38 2.0
    @3641 HML16A_39 2.0
    @3643 HML16A_40 2.0
    @3645 HML16A_41 2.0
    @3647 HML16A_42 2.0
    @3649 HML16A_43 2.0
    @3651 HML16A_44 2.0
    @3653 HML16A_45 2.0
    @3655 HML16A_46 2.0
    @3657 HML16A_47 2.0
    @3659 HML16A_48 2.0
    @3661 HML17_01 1.0
    @3662 HML17_02 1.0
    @3663 HML17_03 1.0
    @3664 HML17_04 1.0
    @3665 HML17_05 1.0
    @3666 HML17_06 1.0
    @3667 HML17_07 1.0
    @3668 HML17_08 1.0
    @3669 HML17_09 1.0
    @3670 HML17_10 1.0
    @3671 HML17_11 1.0
    @3672 HML17_12 1.0
    @3673 HML17_13 1.0
    @3674 HML17_14 1.0
    @3675 HML17_15 1.0
    @3676 HML17_16 1.0
    @3677 HML17_17 1.0
    @3678 HML17_18 1.0
    @3679 HML17_19 1.0
    @3680 HML17_20 1.0
    @3681 HML17_21 1.0
    @3682 HML17_22 1.0
    @3683 HML17_23 1.0
    @3684 HML17_24 1.0
    @3685 HML17_25 1.0
    @3686 HML17_26 1.0
    @3687 HML17_27 1.0
    @3688 HML17_28 1.0
    @3689 HML17_29 1.0
    @3690 HML17_30 1.0
    @3691 HML17_31 1.0
    @3692 HML17_32 1.0
    @3693 HML17_33 1.0
    @3694 HML17_34 1.0
    @3695 HML17_35 1.0
    @3696 HML17_36 1.0
    @3697 HML17_37 1.0
    @3698 HML17_38 1.0
    @3699 HML17_39 1.0
    @3700 HML17_40 1.0
    @3701 HML17_41 1.0
    @3702 HML17_42 1.0
    @3703 HML17_43 1.0
    @3704 HML17_44 1.0
    @3705 HML17_45 1.0
    @3706 HML17_46 1.0
    @3707 HML17_47 1.0
    @3708 HML17_48 1.0
    @3709 HML18_01 1.0
    @3710 HML18_02 1.0
    @3711 HML18_03 1.0
    @3712 HML18_04 1.0
    @3713 HML18_05 1.0
    @3714 HML18_06 1.0
    @3715 HML18_07 1.0
    @3716 HML18_08 1.0
    @3717 HML18_09 1.0
    @3718 HML18_10 1.0
    @3719 HML18_11 1.0
    @3720 HML18_12 1.0
    @3721 HML18_13 1.0
    @3722 HML18_14 1.0
    @3723 HML18_15 1.0
    @3724 HML18_16 1.0
    @3725 HML18_17 1.0
    @3726 HML18_18 1.0
    @3727 HML18_19 1.0
    @3728 HML18_20 1.0
    @3729 HML18_21 1.0
    @3730 HML18_22 1.0
    @3731 HML18_23 1.0
    @3732 HML18_24 1.0
    @3733 HML18_25 1.0
    @3734 HML18_26 1.0
    @3735 HML18_27 1.0
    @3736 HML18_28 1.0
    @3737 HML18_29 1.0
    @3738 HML18_30 1.0
    @3739 HML18_31 1.0
    @3740 HML18_32 1.0
    @3741 HML18_33 1.0
    @3742 HML18_34 1.0
    @3743 HML18_35 1.0
    @3744 HML18_36 1.0
    @3745 HML18_37 1.0
    @3746 HML18_38 1.0
    @3747 HML18_39 1.0
    @3748 HML18_40 1.0
    @3749 HML18_41 1.0
    @3750 HML18_42 1.0
    @3751 HML18_43 1.0
    @3752 HML18_44 1.0
    @3753 HML18_45 1.0
    @3754 HML18_46 1.0
    @3755 HML18_47 1.0
    @3756 HML18_48 1.0
    @3757 HML19_01 1.0
    @3758 HML19_02 1.0
    @3759 HML19_03 1.0
    @3760 HML19_04 1.0
    @3761 HML19_05 1.0
    @3762 HML19_06 1.0
    @3763 HML19_07 1.0
    @3764 HML19_08 1.0
    @3765 HML19_09 1.0
    @3766 HML19_10 1.0
    @3767 HML19_11 1.0
    @3768 HML19_12 1.0
    @3769 HML19_13 1.0
    @3770 HML19_14 1.0
    @3771 HML19_15 1.0
    @3772 HML19_16 1.0
    @3773 HML19_17 1.0
    @3774 HML19_18 1.0
    @3775 HML19_19 1.0
    @3776 HML19_20 1.0
    @3777 HML19_21 1.0
    @3778 HML19_22 1.0
    @3779 HML19_23 1.0
    @3780 HML19_24 1.0
    @3781 HML19_25 1.0
    @3782 HML19_26 1.0
    @3783 HML19_27 1.0
    @3784 HML19_28 1.0
    @3785 HML19_29 1.0
    @3786 HML19_30 1.0
    @3787 HML19_31 1.0
    @3788 HML19_32 1.0
    @3789 HML19_33 1.0
    @3790 HML19_34 1.0
    @3791 HML19_35 1.0
    @3792 HML19_36 1.0
    @3793 HML19_37 1.0
    @3794 HML19_38 1.0
    @3795 HML19_39 1.0
    @3796 HML19_40 1.0
    @3797 HML19_41 1.0
    @3798 HML19_42 1.0
    @3799 HML19_43 1.0
    @3800 HML19_44 1.0
    @3801 HML19_45 1.0
    @3802 HML19_46 1.0
    @3803 HML19_47 1.0
    @3804 HML19_48 1.0
    @3805 HML20_01 1.0
    @3806 HML20_02 1.0
    @3807 HML20_03 1.0
    @3808 HML20_04 1.0
    @3809 HML20_05 1.0
    @3810 HML20_06 1.0
    @3811 HML20_07 1.0
    @3812 HML20_08 1.0
    @3813 HML20_09 1.0
    @3814 HML20_10 1.0
    @3815 HML20_11 1.0
    @3816 HML20_12 1.0
    @3817 HML20_13 1.0
    @3818 HML20_14 1.0
    @3819 HML20_15 1.0
    @3820 HML20_16 1.0
    @3821 HML20_17 1.0
    @3822 HML20_18 1.0
    @3823 HML20_19 1.0
    @3824 HML20_20 1.0
    @3825 HML20_21 1.0
    @3826 HML20_22 1.0
    @3827 HML20_23 1.0
    @3828 HML20_24 1.0
    @3829 HML20_25 1.0
    @3830 HML20_26 1.0
    @3831 HML20_27 1.0
    @3832 HML20_28 1.0
    @3833 HML20_29 1.0
    @3834 HML20_30 1.0
    @3835 HML20_31 1.0
    @3836 HML20_32 1.0
    @3837 HML20_33 1.0
    @3838 HML20_34 1.0
    @3839 HML20_35 1.0
    @3840 HML20_36 1.0
    @3841 HML20_37 1.0
    @3842 HML20_38 1.0
    @3843 HML20_39 1.0
    @3844 HML20_40 1.0
    @3845 HML20_41 1.0
    @3846 HML20_42 1.0
    @3847 HML20_43 1.0
    @3848 HML20_44 1.0
    @3849 HML20_45 1.0
    @3850 HML20_46 1.0
    @3851 HML20_47 1.0
    @3852 HML20_48 1.0
    @3853 HML30_01 2.0
    @3855 HML30_02 2.0
    @3857 HML30_03 2.0
    @3859 HML30_04 2.0
    @3861 HML30_05 2.0
    @3863 HML30_06 2.0
    @3865 HML30_07 2.0
    @3867 HML30_08 2.0
    @3869 HML30_09 2.0
    @3871 HML30_10 2.0
    @3873 HML30_11 2.0
    @3875 HML30_12 2.0
    @3877 HML30_13 2.0
    @3879 HML30_14 2.0
    @3881 HML30_15 2.0
    @3883 HML30_16 2.0
    @3885 HML30_17 2.0
    @3887 HML30_18 2.0
    @3889 HML30_19 2.0
    @3891 HML30_20 2.0
    @3893 HML30_21 2.0
    @3895 HML30_22 2.0
    @3897 HML30_23 2.0
    @3899 HML30_24 2.0
    @3901 HML30_25 2.0
    @3903 HML30_26 2.0
    @3905 HML30_27 2.0
    @3907 HML30_28 2.0
    @3909 HML30_29 2.0
    @3911 HML30_30 2.0
    @3913 HML30_31 2.0
    @3915 HML30_32 2.0
    @3917 HML30_33 2.0
    @3919 HML30_34 2.0
    @3921 HML30_35 2.0
    @3923 HML30_36 2.0
    @3925 HML30_37 2.0
    @3927 HML30_38 2.0
    @3929 HML30_39 2.0
    @3931 HML30_40 2.0
    @3933 HML30_41 2.0
    @3935 HML30_42 2.0
    @3937 HML30_43 2.0
    @3939 HML30_44 2.0
    @3941 HML30_45 2.0
    @3943 HML30_46 2.0
    @3945 HML30_47 2.0
    @3947 HML30_48 2.0
    @3949 HML31_01 1.0
    @3950 HML31_02 1.0
    @3951 HML31_03 1.0
    @3952 HML31_04 1.0
    @3953 HML31_05 1.0
    @3954 HML31_06 1.0
    @3955 HML31_07 1.0
    @3956 HML31_08 1.0
    @3957 HML31_09 1.0
    @3958 HML31_10 1.0
    @3959 HML31_11 1.0
    @3960 HML31_12 1.0
    @3961 HML31_13 1.0
    @3962 HML31_14 1.0
    @3963 HML31_15 1.0
    @3964 HML31_16 1.0
    @3965 HML31_17 1.0
    @3966 HML31_18 1.0
    @3967 HML31_19 1.0
    @3968 HML31_20 1.0
    @3969 HML31_21 1.0
    @3970 HML31_22 1.0
    @3971 HML31_23 1.0
    @3972 HML31_24 1.0
    @3973 HML31_25 1.0
    @3974 HML31_26 1.0
    @3975 HML31_27 1.0
    @3976 HML31_28 1.0
    @3977 HML31_29 1.0
    @3978 HML31_30 1.0
    @3979 HML31_31 1.0
    @3980 HML31_32 1.0
    @3981 HML31_33 1.0
    @3982 HML31_34 1.0
    @3983 HML31_35 1.0
    @3984 HML31_36 1.0
    @3985 HML31_37 1.0
    @3986 HML31_38 1.0
    @3987 HML31_39 1.0
    @3988 HML31_40 1.0
    @3989 HML31_41 1.0
    @3990 HML31_42 1.0
    @3991 HML31_43 1.0
    @3992 HML31_44 1.0
    @3993 HML31_45 1.0
    @3994 HML31_46 1.0
    @3995 HML31_47 1.0
    @3996 HML31_48 1.0
    @3997 HML32_01 1.0
    @3998 HML32_02 1.0
    @3999 HML32_03 1.0
    @4000 HML32_04 1.0
    @4001 HML32_05 1.0
    @4002 HML32_06 1.0
    @4003 HML32_07 1.0
    @4004 HML32_08 1.0
    @4005 HML32_09 1.0
    @4006 HML32_10 1.0
    @4007 HML32_11 1.0
    @4008 HML32_12 1.0
    @4009 HML32_13 1.0
    @4010 HML32_14 1.0
    @4011 HML32_15 1.0
    @4012 HML32_16 1.0
    @4013 HML32_17 1.0
    @4014 HML32_18 1.0
    @4015 HML32_19 1.0
    @4016 HML32_20 1.0
    @4017 HML32_21 1.0
    @4018 HML32_22 1.0
    @4019 HML32_23 1.0
    @4020 HML32_24 1.0
    @4021 HML32_25 1.0
    @4022 HML32_26 1.0
    @4023 HML32_27 1.0
    @4024 HML32_28 1.0
    @4025 HML32_29 1.0
    @4026 HML32_30 1.0
    @4027 HML32_31 1.0
    @4028 HML32_32 1.0
    @4029 HML32_33 1.0
    @4030 HML32_34 1.0
    @4031 HML32_35 1.0
    @4032 HML32_36 1.0
    @4033 HML32_37 1.0
    @4034 HML32_38 1.0
    @4035 HML32_39 1.0
    @4036 HML32_40 1.0
    @4037 HML32_41 1.0
    @4038 HML32_42 1.0
    @4039 HML32_43 1.0
    @4040 HML32_44 1.0
    @4041 HML32_45 1.0
    @4042 HML32_46 1.0
    @4043 HML32_47 1.0
    @4044 HML32_48 1.0
    @4045 HML32A_01 1.0
    @4046 HML32A_02 1.0
    @4047 HML32A_03 1.0
    @4048 HML32A_04 1.0
    @4049 HML32A_05 1.0
    @4050 HML32A_06 1.0
    @4051 HML32A_07 1.0
    @4052 HML32A_08 1.0
    @4053 HML32A_09 1.0
    @4054 HML32A_10 1.0
    @4055 HML32A_11 1.0
    @4056 HML32A_12 1.0
    @4057 HML32A_13 1.0
    @4058 HML32A_14 1.0
    @4059 HML32A_15 1.0
    @4060 HML32A_16 1.0
    @4061 HML32A_17 1.0
    @4062 HML32A_18 1.0
    @4063 HML32A_19 1.0
    @4064 HML32A_20 1.0
    @4065 HML32A_21 1.0
    @4066 HML32A_22 1.0
    @4067 HML32A_23 1.0
    @4068 HML32A_24 1.0
    @4069 HML32A_25 1.0
    @4070 HML32A_26 1.0
    @4071 HML32A_27 1.0
    @4072 HML32A_28 1.0
    @4073 HML32A_29 1.0
    @4074 HML32A_30 1.0
    @4075 HML32A_31 1.0
    @4076 HML32A_32 1.0
    @4077 HML32A_33 1.0
    @4078 HML32A_34 1.0
    @4079 HML32A_35 1.0
    @4080 HML32A_36 1.0
    @4081 HML32A_37 1.0
    @4082 HML32A_38 1.0
    @4083 HML32A_39 1.0
    @4084 HML32A_40 1.0
    @4085 HML32A_41 1.0
    @4086 HML32A_42 1.0
    @4087 HML32A_43 1.0
    @4088 HML32A_44 1.0
    @4089 HML32A_45 1.0
    @4090 HML32A_46 1.0
    @4091 HML32A_47 1.0
    @4092 HML32A_48 1.0
    @4093 HML32B_01 1.0
    @4094 HML32B_02 1.0
    @4095 HML32B_03 1.0
    @4096 HML32B_04 1.0
    @4097 HML32B_05 1.0
    @4098 HML32B_06 1.0
    @4099 HML32B_07 1.0
    @4100 HML32B_08 1.0
    @4101 HML32B_09 1.0
    @4102 HML32B_10 1.0
    @4103 HML32B_11 1.0
    @4104 HML32B_12 1.0
    @4105 HML32B_13 1.0
    @4106 HML32B_14 1.0
    @4107 HML32B_15 1.0
    @4108 HML32B_16 1.0
    @4109 HML32B_17 1.0
    @4110 HML32B_18 1.0
    @4111 HML32B_19 1.0
    @4112 HML32B_20 1.0
    @4113 HML32B_21 1.0
    @4114 HML32B_22 1.0
    @4115 HML32B_23 1.0
    @4116 HML32B_24 1.0
    @4117 HML32B_25 1.0
    @4118 HML32B_26 1.0
    @4119 HML32B_27 1.0
    @4120 HML32B_28 1.0
    @4121 HML32B_29 1.0
    @4122 HML32B_30 1.0
    @4123 HML32B_31 1.0
    @4124 HML32B_32 1.0
    @4125 HML32B_33 1.0
    @4126 HML32B_34 1.0
    @4127 HML32B_35 1.0
    @4128 HML32B_36 1.0
    @4129 HML32B_37 1.0
    @4130 HML32B_38 1.0
    @4131 HML32B_39 1.0
    @4132 HML32B_40 1.0
    @4133 HML32B_41 1.0
    @4134 HML32B_42 1.0
    @4135 HML32B_43 1.0
    @4136 HML32B_44 1.0
    @4137 HML32B_45 1.0
    @4138 HML32B_46 1.0
    @4139 HML32B_47 1.0
    @4140 HML32B_48 1.0
    @4141 HML32C_01 1.0
    @4142 HML32C_02 1.0
    @4143 HML32C_03 1.0
    @4144 HML32C_04 1.0
    @4145 HML32C_05 1.0
    @4146 HML32C_06 1.0
    @4147 HML32C_07 1.0
    @4148 HML32C_08 1.0
    @4149 HML32C_09 1.0
    @4150 HML32C_10 1.0
    @4151 HML32C_11 1.0
    @4152 HML32C_12 1.0
    @4153 HML32C_13 1.0
    @4154 HML32C_14 1.0
    @4155 HML32C_15 1.0
    @4156 HML32C_16 1.0
    @4157 HML32C_17 1.0
    @4158 HML32C_18 1.0
    @4159 HML32C_19 1.0
    @4160 HML32C_20 1.0
    @4161 HML32C_21 1.0
    @4162 HML32C_22 1.0
    @4163 HML32C_23 1.0
    @4164 HML32C_24 1.0
    @4165 HML32C_25 1.0
    @4166 HML32C_26 1.0
    @4167 HML32C_27 1.0
    @4168 HML32C_28 1.0
    @4169 HML32C_29 1.0
    @4170 HML32C_30 1.0
    @4171 HML32C_31 1.0
    @4172 HML32C_32 1.0
    @4173 HML32C_33 1.0
    @4174 HML32C_34 1.0
    @4175 HML32C_35 1.0
    @4176 HML32C_36 1.0
    @4177 HML32C_37 1.0
    @4178 HML32C_38 1.0
    @4179 HML32C_39 1.0
    @4180 HML32C_40 1.0
    @4181 HML32C_41 1.0
    @4182 HML32C_42 1.0
    @4183 HML32C_43 1.0
    @4184 HML32C_44 1.0
    @4185 HML32C_45 1.0
    @4186 HML32C_46 1.0
    @4187 HML32C_47 1.0
    @4188 HML32C_48 1.0
    @4189 HML32D_01 1.0
    @4190 HML32D_02 1.0
    @4191 HML32D_03 1.0
    @4192 HML32D_04 1.0
    @4193 HML32D_05 1.0
    @4194 HML32D_06 1.0
    @4195 HML32D_07 1.0
    @4196 HML32D_08 1.0
    @4197 HML32D_09 1.0
    @4198 HML32D_10 1.0
    @4199 HML32D_11 1.0
    @4200 HML32D_12 1.0
    @4201 HML32D_13 1.0
    @4202 HML32D_14 1.0
    @4203 HML32D_15 1.0
    @4204 HML32D_16 1.0
    @4205 HML32D_17 1.0
    @4206 HML32D_18 1.0
    @4207 HML32D_19 1.0
    @4208 HML32D_20 1.0
    @4209 HML32D_21 1.0
    @4210 HML32D_22 1.0
    @4211 HML32D_23 1.0
    @4212 HML32D_24 1.0
    @4213 HML32D_25 1.0
    @4214 HML32D_26 1.0
    @4215 HML32D_27 1.0
    @4216 HML32D_28 1.0
    @4217 HML32D_29 1.0
    @4218 HML32D_30 1.0
    @4219 HML32D_31 1.0
    @4220 HML32D_32 1.0
    @4221 HML32D_33 1.0
    @4222 HML32D_34 1.0
    @4223 HML32D_35 1.0
    @4224 HML32D_36 1.0
    @4225 HML32D_37 1.0
    @4226 HML32D_38 1.0
    @4227 HML32D_39 1.0
    @4228 HML32D_40 1.0
    @4229 HML32D_41 1.0
    @4230 HML32D_42 1.0
    @4231 HML32D_43 1.0
    @4232 HML32D_44 1.0
    @4233 HML32D_45 1.0
    @4234 HML32D_46 1.0
    @4235 HML32D_47 1.0
    @4236 HML32D_48 1.0
    @4237 HML32E_01 1.0
    @4238 HML32E_02 1.0
    @4239 HML32E_03 1.0
    @4240 HML32E_04 1.0
    @4241 HML32E_05 1.0
    @4242 HML32E_06 1.0
    @4243 HML32E_07 1.0
    @4244 HML32E_08 1.0
    @4245 HML32E_09 1.0
    @4246 HML32E_10 1.0
    @4247 HML32E_11 1.0
    @4248 HML32E_12 1.0
    @4249 HML32E_13 1.0
    @4250 HML32E_14 1.0
    @4251 HML32E_15 1.0
    @4252 HML32E_16 1.0
    @4253 HML32E_17 1.0
    @4254 HML32E_18 1.0
    @4255 HML32E_19 1.0
    @4256 HML32E_20 1.0
    @4257 HML32E_21 1.0
    @4258 HML32E_22 1.0
    @4259 HML32E_23 1.0
    @4260 HML32E_24 1.0
    @4261 HML32E_25 1.0
    @4262 HML32E_26 1.0
    @4263 HML32E_27 1.0
    @4264 HML32E_28 1.0
    @4265 HML32E_29 1.0
    @4266 HML32E_30 1.0
    @4267 HML32E_31 1.0
    @4268 HML32E_32 1.0
    @4269 HML32E_33 1.0
    @4270 HML32E_34 1.0
    @4271 HML32E_35 1.0
    @4272 HML32E_36 1.0
    @4273 HML32E_37 1.0
    @4274 HML32E_38 1.0
    @4275 HML32E_39 1.0
    @4276 HML32E_40 1.0
    @4277 HML32E_41 1.0
    @4278 HML32E_42 1.0
    @4279 HML32E_43 1.0
    @4280 HML32E_44 1.0
    @4281 HML32E_45 1.0
    @4282 HML32E_46 1.0
    @4283 HML32E_47 1.0
    @4284 HML32E_48 1.0
    @4285 HML32F_01 1.0
    @4286 HML32F_02 1.0
    @4287 HML32F_03 1.0
    @4288 HML32F_04 1.0
    @4289 HML32F_05 1.0
    @4290 HML32F_06 1.0
    @4291 HML32F_07 1.0
    @4292 HML32F_08 1.0
    @4293 HML32F_09 1.0
    @4294 HML32F_10 1.0
    @4295 HML32F_11 1.0
    @4296 HML32F_12 1.0
    @4297 HML32F_13 1.0
    @4298 HML32F_14 1.0
    @4299 HML32F_15 1.0
    @4300 HML32F_16 1.0
    @4301 HML32F_17 1.0
    @4302 HML32F_18 1.0
    @4303 HML32F_19 1.0
    @4304 HML32F_20 1.0
    @4305 HML32F_21 1.0
    @4306 HML32F_22 1.0
    @4307 HML32F_23 1.0
    @4308 HML32F_24 1.0
    @4309 HML32F_25 1.0
    @4310 HML32F_26 1.0
    @4311 HML32F_27 1.0
    @4312 HML32F_28 1.0
    @4313 HML32F_29 1.0
    @4314 HML32F_30 1.0
    @4315 HML32F_31 1.0
    @4316 HML32F_32 1.0
    @4317 HML32F_33 1.0
    @4318 HML32F_34 1.0
    @4319 HML32F_35 1.0
    @4320 HML32F_36 1.0
    @4321 HML32F_37 1.0
    @4322 HML32F_38 1.0
    @4323 HML32F_39 1.0
    @4324 HML32F_40 1.0
    @4325 HML32F_41 1.0
    @4326 HML32F_42 1.0
    @4327 HML32F_43 1.0
    @4328 HML32F_44 1.0
    @4329 HML32F_45 1.0
    @4330 HML32F_46 1.0
    @4331 HML32F_47 1.0
    @4332 HML32F_48 1.0
    @4333 HML32G_01 1.0
    @4334 HML32G_02 1.0
    @4335 HML32G_03 1.0
    @4336 HML32G_04 1.0
    @4337 HML32G_05 1.0
    @4338 HML32G_06 1.0
    @4339 HML32G_07 1.0
    @4340 HML32G_08 1.0
    @4341 HML32G_09 1.0
    @4342 HML32G_10 1.0
    @4343 HML32G_11 1.0
    @4344 HML32G_12 1.0
    @4345 HML32G_13 1.0
    @4346 HML32G_14 1.0
    @4347 HML32G_15 1.0
    @4348 HML32G_16 1.0
    @4349 HML32G_17 1.0
    @4350 HML32G_18 1.0
    @4351 HML32G_19 1.0
    @4352 HML32G_20 1.0
    @4353 HML32G_21 1.0
    @4354 HML32G_22 1.0
    @4355 HML32G_23 1.0
    @4356 HML32G_24 1.0
    @4357 HML32G_25 1.0
    @4358 HML32G_26 1.0
    @4359 HML32G_27 1.0
    @4360 HML32G_28 1.0
    @4361 HML32G_29 1.0
    @4362 HML32G_30 1.0
    @4363 HML32G_31 1.0
    @4364 HML32G_32 1.0
    @4365 HML32G_33 1.0
    @4366 HML32G_34 1.0
    @4367 HML32G_35 1.0
    @4368 HML32G_36 1.0
    @4369 HML32G_37 1.0
    @4370 HML32G_38 1.0
    @4371 HML32G_39 1.0
    @4372 HML32G_40 1.0
    @4373 HML32G_41 1.0
    @4374 HML32G_42 1.0
    @4375 HML32G_43 1.0
    @4376 HML32G_44 1.0
    @4377 HML32G_45 1.0
    @4378 HML32G_46 1.0
    @4379 HML32G_47 1.0
    @4380 HML32G_48 1.0
    @4381 HML33_01 1.0
    @4382 HML33_02 1.0
    @4383 HML33_03 1.0
    @4384 HML33_04 1.0
    @4385 HML33_05 1.0
    @4386 HML33_06 1.0
    @4387 HML33_07 1.0
    @4388 HML33_08 1.0
    @4389 HML33_09 1.0
    @4390 HML33_10 1.0
    @4391 HML33_11 1.0
    @4392 HML33_12 1.0
    @4393 HML33_13 1.0
    @4394 HML33_14 1.0
    @4395 HML33_15 1.0
    @4396 HML33_16 1.0
    @4397 HML33_17 1.0
    @4398 HML33_18 1.0
    @4399 HML33_19 1.0
    @4400 HML33_20 1.0
    @4401 HML33_21 1.0
    @4402 HML33_22 1.0
    @4403 HML33_23 1.0
    @4404 HML33_24 1.0
    @4405 HML33_25 1.0
    @4406 HML33_26 1.0
    @4407 HML33_27 1.0
    @4408 HML33_28 1.0
    @4409 HML33_29 1.0
    @4410 HML33_30 1.0
    @4411 HML33_31 1.0
    @4412 HML33_32 1.0
    @4413 HML33_33 1.0
    @4414 HML33_34 1.0
    @4415 HML33_35 1.0
    @4416 HML33_36 1.0
    @4417 HML33_37 1.0
    @4418 HML33_38 1.0
    @4419 HML33_39 1.0
    @4420 HML33_40 1.0
    @4421 HML33_41 1.0
    @4422 HML33_42 1.0
    @4423 HML33_43 1.0
    @4424 HML33_44 1.0
    @4425 HML33_45 1.0
    @4426 HML33_46 1.0
    @4427 HML33_47 1.0
    @4428 HML33_48 1.0
    @4429 HML34_01 $5.
    @4434 HML34_02 $5.
    @4439 HML34_03 $5.
    @4444 HML34_04 $5.
    @4449 HML34_05 $5.
    @4454 HML34_06 $5.
    @4459 HML34_07 $5.
    @4464 HML34_08 $5.
    @4469 HML34_09 $5.
    @4474 HML34_10 $5.
    @4479 HML34_11 $5.
    @4484 HML34_12 $5.
    @4489 HML34_13 $5.
    @4494 HML34_14 $5.
    @4499 HML34_15 $5.
    @4504 HML34_16 $5.
    @4509 HML34_17 $5.
    @4514 HML34_18 $5.
    @4519 HML34_19 $5.
    @4524 HML34_20 $5.
    @4529 HML34_21 $5.
    @4534 HML34_22 $5.
    @4539 HML34_23 $5.
    @4544 HML34_24 $5.
    @4549 HML34_25 $5.
    @4554 HML34_26 $5.
    @4559 HML34_27 $5.
    @4564 HML34_28 $5.
    @4569 HML34_29 $5.
    @4574 HML34_30 $5.
    @4579 HML34_31 $5.
    @4584 HML34_32 $5.
    @4589 HML34_33 $5.
    @4594 HML34_34 $5.
    @4599 HML34_35 $5.
    @4604 HML34_36 $5.
    @4609 HML34_37 $5.
    @4614 HML34_38 $5.
    @4619 HML34_39 $5.
    @4624 HML34_40 $5.
    @4629 HML34_41 $5.
    @4634 HML34_42 $5.
    @4639 HML34_43 $5.
    @4644 HML34_44 $5.
    @4649 HML34_45 $5.
    @4654 HML34_46 $5.
    @4659 HML34_47 $5.
    @4664 HML34_48 $5.
    @4669 HML35_01 1.0
    @4670 HML35_02 1.0
    @4671 HML35_03 1.0
    @4672 HML35_04 1.0
    @4673 HML35_05 1.0
    @4674 HML35_06 1.0
    @4675 HML35_07 1.0
    @4676 HML35_08 1.0
    @4677 HML35_09 1.0
    @4678 HML35_10 1.0
    @4679 HML35_11 1.0
    @4680 HML35_12 1.0
    @4681 HML35_13 1.0
    @4682 HML35_14 1.0
    @4683 HML35_15 1.0
    @4684 HML35_16 1.0
    @4685 HML35_17 1.0
    @4686 HML35_18 1.0
    @4687 HML35_19 1.0
    @4688 HML35_20 1.0
    @4689 HML35_21 1.0
    @4690 HML35_22 1.0
    @4691 HML35_23 1.0
    @4692 HML35_24 1.0
    @4693 HML35_25 1.0
    @4694 HML35_26 1.0
    @4695 HML35_27 1.0
    @4696 HML35_28 1.0
    @4697 HML35_29 1.0
    @4698 HML35_30 1.0
    @4699 HML35_31 1.0
    @4700 HML35_32 1.0
    @4701 HML35_33 1.0
    @4702 HML35_34 1.0
    @4703 HML35_35 1.0
    @4704 HML35_36 1.0
    @4705 HML35_37 1.0
    @4706 HML35_38 1.0
    @4707 HML35_39 1.0
    @4708 HML35_40 1.0
    @4709 HML35_41 1.0
    @4710 HML35_42 1.0
    @4711 HML35_43 1.0
    @4712 HML35_44 1.0
    @4713 HML35_45 1.0
    @4714 HML35_46 1.0
    @4715 HML35_47 1.0
    @4716 HML35_48 1.0
    ;
    if HV017    = 9 then HV017 = .;
    if HV018    = 99999 then HV018 = .;
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
    if HV245    = 9999 then HV245 = .;
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
    if SH107    = 99 then SH107 = .;
    if SH110G   = 9 then SH110G = .;
    if SH110H   = 9 then SH110H = .;
    if SH110I   = 9 then SH110I = .;
    if SH110J   = 9 then SH110J = .;
    if SH110K   = 9 then SH110K = .;
    if SH110L   = 9 then SH110L = .;
    if SH110M   = 9 then SH110M = .;
    if SH110N   = 9 then SH110N = .;
    if SH110O   = 9 then SH110O = .;
    if SH118F   = 9 then SH118F = .;
    if SH118G   = 9 then SH118G = .;
    if SH120    = 99.9 then SH120 = .;
    if SH123A   = 9 then SH123A = .;
    if SH123BA  = 9 then SH123BA = .;
    if SH123BB  = 9 then SH123BB = .;
    if SH123BC  = 9 then SH123BC = .;
    if SH123BD  = 9 then SH123BD = .;
    if SH123BE  = 9 then SH123BE = .;
    if SH123BX  = 9 then SH123BX = .;
    if SH123BZ  = 9 then SH123BZ = .;
    if SH123C   = 9 then SH123C = .;
    if SH123DA  = 9 then SH123DA = .;
    if SH123DB  = 9 then SH123DB = .;
    if SH123DC  = 9 then SH123DC = .;
    if SH123DD  = 9 then SH123DD = .;
    if SH123DE  = 9 then SH123DE = .;
    if SH123DX  = 9 then SH123DX = .;
    if SH123DZ  = 9 then SH123DZ = .;
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
    if HV101_31 = 99 then HV101_31 = .;
    if HV101_32 = 99 then HV101_32 = .;
    if HV101_33 = 99 then HV101_33 = .;
    if HV101_34 = 99 then HV101_34 = .;
    if HV101_35 = 99 then HV101_35 = .;
    if HV101_36 = 99 then HV101_36 = .;
    if HV101_37 = 99 then HV101_37 = .;
    if HV101_38 = 99 then HV101_38 = .;
    if HV101_39 = 99 then HV101_39 = .;
    if HV101_40 = 99 then HV101_40 = .;
    if HV101_41 = 99 then HV101_41 = .;
    if HV101_42 = 99 then HV101_42 = .;
    if HV101_43 = 99 then HV101_43 = .;
    if HV101_44 = 99 then HV101_44 = .;
    if HV101_45 = 99 then HV101_45 = .;
    if HV101_46 = 99 then HV101_46 = .;
    if HV101_47 = 99 then HV101_47 = .;
    if HV101_48 = 99 then HV101_48 = .;
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
    if HV102_31 = 9 then HV102_31 = .;
    if HV102_32 = 9 then HV102_32 = .;
    if HV102_33 = 9 then HV102_33 = .;
    if HV102_34 = 9 then HV102_34 = .;
    if HV102_35 = 9 then HV102_35 = .;
    if HV102_36 = 9 then HV102_36 = .;
    if HV102_37 = 9 then HV102_37 = .;
    if HV102_38 = 9 then HV102_38 = .;
    if HV102_39 = 9 then HV102_39 = .;
    if HV102_40 = 9 then HV102_40 = .;
    if HV102_41 = 9 then HV102_41 = .;
    if HV102_42 = 9 then HV102_42 = .;
    if HV102_43 = 9 then HV102_43 = .;
    if HV102_44 = 9 then HV102_44 = .;
    if HV102_45 = 9 then HV102_45 = .;
    if HV102_46 = 9 then HV102_46 = .;
    if HV102_47 = 9 then HV102_47 = .;
    if HV102_48 = 9 then HV102_48 = .;
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
    if HV103_31 = 9 then HV103_31 = .;
    if HV103_32 = 9 then HV103_32 = .;
    if HV103_33 = 9 then HV103_33 = .;
    if HV103_34 = 9 then HV103_34 = .;
    if HV103_35 = 9 then HV103_35 = .;
    if HV103_36 = 9 then HV103_36 = .;
    if HV103_37 = 9 then HV103_37 = .;
    if HV103_38 = 9 then HV103_38 = .;
    if HV103_39 = 9 then HV103_39 = .;
    if HV103_40 = 9 then HV103_40 = .;
    if HV103_41 = 9 then HV103_41 = .;
    if HV103_42 = 9 then HV103_42 = .;
    if HV103_43 = 9 then HV103_43 = .;
    if HV103_44 = 9 then HV103_44 = .;
    if HV103_45 = 9 then HV103_45 = .;
    if HV103_46 = 9 then HV103_46 = .;
    if HV103_47 = 9 then HV103_47 = .;
    if HV103_48 = 9 then HV103_48 = .;
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
    if HV104_31 = 9 then HV104_31 = .;
    if HV104_32 = 9 then HV104_32 = .;
    if HV104_33 = 9 then HV104_33 = .;
    if HV104_34 = 9 then HV104_34 = .;
    if HV104_35 = 9 then HV104_35 = .;
    if HV104_36 = 9 then HV104_36 = .;
    if HV104_37 = 9 then HV104_37 = .;
    if HV104_38 = 9 then HV104_38 = .;
    if HV104_39 = 9 then HV104_39 = .;
    if HV104_40 = 9 then HV104_40 = .;
    if HV104_41 = 9 then HV104_41 = .;
    if HV104_42 = 9 then HV104_42 = .;
    if HV104_43 = 9 then HV104_43 = .;
    if HV104_44 = 9 then HV104_44 = .;
    if HV104_45 = 9 then HV104_45 = .;
    if HV104_46 = 9 then HV104_46 = .;
    if HV104_47 = 9 then HV104_47 = .;
    if HV104_48 = 9 then HV104_48 = .;
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
    if HV105_31 = 99 then HV105_31 = .;
    if HV105_32 = 99 then HV105_32 = .;
    if HV105_33 = 99 then HV105_33 = .;
    if HV105_34 = 99 then HV105_34 = .;
    if HV105_35 = 99 then HV105_35 = .;
    if HV105_36 = 99 then HV105_36 = .;
    if HV105_37 = 99 then HV105_37 = .;
    if HV105_38 = 99 then HV105_38 = .;
    if HV105_39 = 99 then HV105_39 = .;
    if HV105_40 = 99 then HV105_40 = .;
    if HV105_41 = 99 then HV105_41 = .;
    if HV105_42 = 99 then HV105_42 = .;
    if HV105_43 = 99 then HV105_43 = .;
    if HV105_44 = 99 then HV105_44 = .;
    if HV105_45 = 99 then HV105_45 = .;
    if HV105_46 = 99 then HV105_46 = .;
    if HV105_47 = 99 then HV105_47 = .;
    if HV105_48 = 99 then HV105_48 = .;
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
    if HV106_31 = 9 then HV106_31 = .;
    if HV106_32 = 9 then HV106_32 = .;
    if HV106_33 = 9 then HV106_33 = .;
    if HV106_34 = 9 then HV106_34 = .;
    if HV106_35 = 9 then HV106_35 = .;
    if HV106_36 = 9 then HV106_36 = .;
    if HV106_37 = 9 then HV106_37 = .;
    if HV106_38 = 9 then HV106_38 = .;
    if HV106_39 = 9 then HV106_39 = .;
    if HV106_40 = 9 then HV106_40 = .;
    if HV106_41 = 9 then HV106_41 = .;
    if HV106_42 = 9 then HV106_42 = .;
    if HV106_43 = 9 then HV106_43 = .;
    if HV106_44 = 9 then HV106_44 = .;
    if HV106_45 = 9 then HV106_45 = .;
    if HV106_46 = 9 then HV106_46 = .;
    if HV106_47 = 9 then HV106_47 = .;
    if HV106_48 = 9 then HV106_48 = .;
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
    if HV107_31 = 99 then HV107_31 = .;
    if HV107_32 = 99 then HV107_32 = .;
    if HV107_33 = 99 then HV107_33 = .;
    if HV107_34 = 99 then HV107_34 = .;
    if HV107_35 = 99 then HV107_35 = .;
    if HV107_36 = 99 then HV107_36 = .;
    if HV107_37 = 99 then HV107_37 = .;
    if HV107_38 = 99 then HV107_38 = .;
    if HV107_39 = 99 then HV107_39 = .;
    if HV107_40 = 99 then HV107_40 = .;
    if HV107_41 = 99 then HV107_41 = .;
    if HV107_42 = 99 then HV107_42 = .;
    if HV107_43 = 99 then HV107_43 = .;
    if HV107_44 = 99 then HV107_44 = .;
    if HV107_45 = 99 then HV107_45 = .;
    if HV107_46 = 99 then HV107_46 = .;
    if HV107_47 = 99 then HV107_47 = .;
    if HV107_48 = 99 then HV107_48 = .;
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
    if HV108_31 = 99 then HV108_31 = .;
    if HV108_32 = 99 then HV108_32 = .;
    if HV108_33 = 99 then HV108_33 = .;
    if HV108_34 = 99 then HV108_34 = .;
    if HV108_35 = 99 then HV108_35 = .;
    if HV108_36 = 99 then HV108_36 = .;
    if HV108_37 = 99 then HV108_37 = .;
    if HV108_38 = 99 then HV108_38 = .;
    if HV108_39 = 99 then HV108_39 = .;
    if HV108_40 = 99 then HV108_40 = .;
    if HV108_41 = 99 then HV108_41 = .;
    if HV108_42 = 99 then HV108_42 = .;
    if HV108_43 = 99 then HV108_43 = .;
    if HV108_44 = 99 then HV108_44 = .;
    if HV108_45 = 99 then HV108_45 = .;
    if HV108_46 = 99 then HV108_46 = .;
    if HV108_47 = 99 then HV108_47 = .;
    if HV108_48 = 99 then HV108_48 = .;
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
    if HV109_31 = 9 then HV109_31 = .;
    if HV109_32 = 9 then HV109_32 = .;
    if HV109_33 = 9 then HV109_33 = .;
    if HV109_34 = 9 then HV109_34 = .;
    if HV109_35 = 9 then HV109_35 = .;
    if HV109_36 = 9 then HV109_36 = .;
    if HV109_37 = 9 then HV109_37 = .;
    if HV109_38 = 9 then HV109_38 = .;
    if HV109_39 = 9 then HV109_39 = .;
    if HV109_40 = 9 then HV109_40 = .;
    if HV109_41 = 9 then HV109_41 = .;
    if HV109_42 = 9 then HV109_42 = .;
    if HV109_43 = 9 then HV109_43 = .;
    if HV109_44 = 9 then HV109_44 = .;
    if HV109_45 = 9 then HV109_45 = .;
    if HV109_46 = 9 then HV109_46 = .;
    if HV109_47 = 9 then HV109_47 = .;
    if HV109_48 = 9 then HV109_48 = .;
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
    if HV110_31 = 9 then HV110_31 = .;
    if HV110_32 = 9 then HV110_32 = .;
    if HV110_33 = 9 then HV110_33 = .;
    if HV110_34 = 9 then HV110_34 = .;
    if HV110_35 = 9 then HV110_35 = .;
    if HV110_36 = 9 then HV110_36 = .;
    if HV110_37 = 9 then HV110_37 = .;
    if HV110_38 = 9 then HV110_38 = .;
    if HV110_39 = 9 then HV110_39 = .;
    if HV110_40 = 9 then HV110_40 = .;
    if HV110_41 = 9 then HV110_41 = .;
    if HV110_42 = 9 then HV110_42 = .;
    if HV110_43 = 9 then HV110_43 = .;
    if HV110_44 = 9 then HV110_44 = .;
    if HV110_45 = 9 then HV110_45 = .;
    if HV110_46 = 9 then HV110_46 = .;
    if HV110_47 = 9 then HV110_47 = .;
    if HV110_48 = 9 then HV110_48 = .;
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
    if HV111_31 = 9 then HV111_31 = .;
    if HV111_32 = 9 then HV111_32 = .;
    if HV111_33 = 9 then HV111_33 = .;
    if HV111_34 = 9 then HV111_34 = .;
    if HV111_35 = 9 then HV111_35 = .;
    if HV111_36 = 9 then HV111_36 = .;
    if HV111_37 = 9 then HV111_37 = .;
    if HV111_38 = 9 then HV111_38 = .;
    if HV111_39 = 9 then HV111_39 = .;
    if HV111_40 = 9 then HV111_40 = .;
    if HV111_41 = 9 then HV111_41 = .;
    if HV111_42 = 9 then HV111_42 = .;
    if HV111_43 = 9 then HV111_43 = .;
    if HV111_44 = 9 then HV111_44 = .;
    if HV111_45 = 9 then HV111_45 = .;
    if HV111_46 = 9 then HV111_46 = .;
    if HV111_47 = 9 then HV111_47 = .;
    if HV111_48 = 9 then HV111_48 = .;
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
    if HV112_31 = 99 then HV112_31 = .;
    if HV112_32 = 99 then HV112_32 = .;
    if HV112_33 = 99 then HV112_33 = .;
    if HV112_34 = 99 then HV112_34 = .;
    if HV112_35 = 99 then HV112_35 = .;
    if HV112_36 = 99 then HV112_36 = .;
    if HV112_37 = 99 then HV112_37 = .;
    if HV112_38 = 99 then HV112_38 = .;
    if HV112_39 = 99 then HV112_39 = .;
    if HV112_40 = 99 then HV112_40 = .;
    if HV112_41 = 99 then HV112_41 = .;
    if HV112_42 = 99 then HV112_42 = .;
    if HV112_43 = 99 then HV112_43 = .;
    if HV112_44 = 99 then HV112_44 = .;
    if HV112_45 = 99 then HV112_45 = .;
    if HV112_46 = 99 then HV112_46 = .;
    if HV112_47 = 99 then HV112_47 = .;
    if HV112_48 = 99 then HV112_48 = .;
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
    if HV113_31 = 9 then HV113_31 = .;
    if HV113_32 = 9 then HV113_32 = .;
    if HV113_33 = 9 then HV113_33 = .;
    if HV113_34 = 9 then HV113_34 = .;
    if HV113_35 = 9 then HV113_35 = .;
    if HV113_36 = 9 then HV113_36 = .;
    if HV113_37 = 9 then HV113_37 = .;
    if HV113_38 = 9 then HV113_38 = .;
    if HV113_39 = 9 then HV113_39 = .;
    if HV113_40 = 9 then HV113_40 = .;
    if HV113_41 = 9 then HV113_41 = .;
    if HV113_42 = 9 then HV113_42 = .;
    if HV113_43 = 9 then HV113_43 = .;
    if HV113_44 = 9 then HV113_44 = .;
    if HV113_45 = 9 then HV113_45 = .;
    if HV113_46 = 9 then HV113_46 = .;
    if HV113_47 = 9 then HV113_47 = .;
    if HV113_48 = 9 then HV113_48 = .;
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
    if HV114_31 = 99 then HV114_31 = .;
    if HV114_32 = 99 then HV114_32 = .;
    if HV114_33 = 99 then HV114_33 = .;
    if HV114_34 = 99 then HV114_34 = .;
    if HV114_35 = 99 then HV114_35 = .;
    if HV114_36 = 99 then HV114_36 = .;
    if HV114_37 = 99 then HV114_37 = .;
    if HV114_38 = 99 then HV114_38 = .;
    if HV114_39 = 99 then HV114_39 = .;
    if HV114_40 = 99 then HV114_40 = .;
    if HV114_41 = 99 then HV114_41 = .;
    if HV114_42 = 99 then HV114_42 = .;
    if HV114_43 = 99 then HV114_43 = .;
    if HV114_44 = 99 then HV114_44 = .;
    if HV114_45 = 99 then HV114_45 = .;
    if HV114_46 = 99 then HV114_46 = .;
    if HV114_47 = 99 then HV114_47 = .;
    if HV114_48 = 99 then HV114_48 = .;
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
    if HV115_31 = 9 then HV115_31 = .;
    if HV115_32 = 9 then HV115_32 = .;
    if HV115_33 = 9 then HV115_33 = .;
    if HV115_34 = 9 then HV115_34 = .;
    if HV115_35 = 9 then HV115_35 = .;
    if HV115_36 = 9 then HV115_36 = .;
    if HV115_37 = 9 then HV115_37 = .;
    if HV115_38 = 9 then HV115_38 = .;
    if HV115_39 = 9 then HV115_39 = .;
    if HV115_40 = 9 then HV115_40 = .;
    if HV115_41 = 9 then HV115_41 = .;
    if HV115_42 = 9 then HV115_42 = .;
    if HV115_43 = 9 then HV115_43 = .;
    if HV115_44 = 9 then HV115_44 = .;
    if HV115_45 = 9 then HV115_45 = .;
    if HV115_46 = 9 then HV115_46 = .;
    if HV115_47 = 9 then HV115_47 = .;
    if HV115_48 = 9 then HV115_48 = .;
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
    if HV116_31 = 9 then HV116_31 = .;
    if HV116_32 = 9 then HV116_32 = .;
    if HV116_33 = 9 then HV116_33 = .;
    if HV116_34 = 9 then HV116_34 = .;
    if HV116_35 = 9 then HV116_35 = .;
    if HV116_36 = 9 then HV116_36 = .;
    if HV116_37 = 9 then HV116_37 = .;
    if HV116_38 = 9 then HV116_38 = .;
    if HV116_39 = 9 then HV116_39 = .;
    if HV116_40 = 9 then HV116_40 = .;
    if HV116_41 = 9 then HV116_41 = .;
    if HV116_42 = 9 then HV116_42 = .;
    if HV116_43 = 9 then HV116_43 = .;
    if HV116_44 = 9 then HV116_44 = .;
    if HV116_45 = 9 then HV116_45 = .;
    if HV116_46 = 9 then HV116_46 = .;
    if HV116_47 = 9 then HV116_47 = .;
    if HV116_48 = 9 then HV116_48 = .;
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
    if HV121_31 = 9 then HV121_31 = .;
    if HV121_32 = 9 then HV121_32 = .;
    if HV121_33 = 9 then HV121_33 = .;
    if HV121_34 = 9 then HV121_34 = .;
    if HV121_35 = 9 then HV121_35 = .;
    if HV121_36 = 9 then HV121_36 = .;
    if HV121_37 = 9 then HV121_37 = .;
    if HV121_38 = 9 then HV121_38 = .;
    if HV121_39 = 9 then HV121_39 = .;
    if HV121_40 = 9 then HV121_40 = .;
    if HV121_41 = 9 then HV121_41 = .;
    if HV121_42 = 9 then HV121_42 = .;
    if HV121_43 = 9 then HV121_43 = .;
    if HV121_44 = 9 then HV121_44 = .;
    if HV121_45 = 9 then HV121_45 = .;
    if HV121_46 = 9 then HV121_46 = .;
    if HV121_47 = 9 then HV121_47 = .;
    if HV121_48 = 9 then HV121_48 = .;
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
    if HV122_31 = 9 then HV122_31 = .;
    if HV122_32 = 9 then HV122_32 = .;
    if HV122_33 = 9 then HV122_33 = .;
    if HV122_34 = 9 then HV122_34 = .;
    if HV122_35 = 9 then HV122_35 = .;
    if HV122_36 = 9 then HV122_36 = .;
    if HV122_37 = 9 then HV122_37 = .;
    if HV122_38 = 9 then HV122_38 = .;
    if HV122_39 = 9 then HV122_39 = .;
    if HV122_40 = 9 then HV122_40 = .;
    if HV122_41 = 9 then HV122_41 = .;
    if HV122_42 = 9 then HV122_42 = .;
    if HV122_43 = 9 then HV122_43 = .;
    if HV122_44 = 9 then HV122_44 = .;
    if HV122_45 = 9 then HV122_45 = .;
    if HV122_46 = 9 then HV122_46 = .;
    if HV122_47 = 9 then HV122_47 = .;
    if HV122_48 = 9 then HV122_48 = .;
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
    if HV123_31 = 99 then HV123_31 = .;
    if HV123_32 = 99 then HV123_32 = .;
    if HV123_33 = 99 then HV123_33 = .;
    if HV123_34 = 99 then HV123_34 = .;
    if HV123_35 = 99 then HV123_35 = .;
    if HV123_36 = 99 then HV123_36 = .;
    if HV123_37 = 99 then HV123_37 = .;
    if HV123_38 = 99 then HV123_38 = .;
    if HV123_39 = 99 then HV123_39 = .;
    if HV123_40 = 99 then HV123_40 = .;
    if HV123_41 = 99 then HV123_41 = .;
    if HV123_42 = 99 then HV123_42 = .;
    if HV123_43 = 99 then HV123_43 = .;
    if HV123_44 = 99 then HV123_44 = .;
    if HV123_45 = 99 then HV123_45 = .;
    if HV123_46 = 99 then HV123_46 = .;
    if HV123_47 = 99 then HV123_47 = .;
    if HV123_48 = 99 then HV123_48 = .;
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
    if HV124_31 = 99 then HV124_31 = .;
    if HV124_32 = 99 then HV124_32 = .;
    if HV124_33 = 99 then HV124_33 = .;
    if HV124_34 = 99 then HV124_34 = .;
    if HV124_35 = 99 then HV124_35 = .;
    if HV124_36 = 99 then HV124_36 = .;
    if HV124_37 = 99 then HV124_37 = .;
    if HV124_38 = 99 then HV124_38 = .;
    if HV124_39 = 99 then HV124_39 = .;
    if HV124_40 = 99 then HV124_40 = .;
    if HV124_41 = 99 then HV124_41 = .;
    if HV124_42 = 99 then HV124_42 = .;
    if HV124_43 = 99 then HV124_43 = .;
    if HV124_44 = 99 then HV124_44 = .;
    if HV124_45 = 99 then HV124_45 = .;
    if HV124_46 = 99 then HV124_46 = .;
    if HV124_47 = 99 then HV124_47 = .;
    if HV124_48 = 99 then HV124_48 = .;
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
    if HV125_31 = 9 then HV125_31 = .;
    if HV125_32 = 9 then HV125_32 = .;
    if HV125_33 = 9 then HV125_33 = .;
    if HV125_34 = 9 then HV125_34 = .;
    if HV125_35 = 9 then HV125_35 = .;
    if HV125_36 = 9 then HV125_36 = .;
    if HV125_37 = 9 then HV125_37 = .;
    if HV125_38 = 9 then HV125_38 = .;
    if HV125_39 = 9 then HV125_39 = .;
    if HV125_40 = 9 then HV125_40 = .;
    if HV125_41 = 9 then HV125_41 = .;
    if HV125_42 = 9 then HV125_42 = .;
    if HV125_43 = 9 then HV125_43 = .;
    if HV125_44 = 9 then HV125_44 = .;
    if HV125_45 = 9 then HV125_45 = .;
    if HV125_46 = 9 then HV125_46 = .;
    if HV125_47 = 9 then HV125_47 = .;
    if HV125_48 = 9 then HV125_48 = .;
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
    if HV126_31 = 9 then HV126_31 = .;
    if HV126_32 = 9 then HV126_32 = .;
    if HV126_33 = 9 then HV126_33 = .;
    if HV126_34 = 9 then HV126_34 = .;
    if HV126_35 = 9 then HV126_35 = .;
    if HV126_36 = 9 then HV126_36 = .;
    if HV126_37 = 9 then HV126_37 = .;
    if HV126_38 = 9 then HV126_38 = .;
    if HV126_39 = 9 then HV126_39 = .;
    if HV126_40 = 9 then HV126_40 = .;
    if HV126_41 = 9 then HV126_41 = .;
    if HV126_42 = 9 then HV126_42 = .;
    if HV126_43 = 9 then HV126_43 = .;
    if HV126_44 = 9 then HV126_44 = .;
    if HV126_45 = 9 then HV126_45 = .;
    if HV126_46 = 9 then HV126_46 = .;
    if HV126_47 = 9 then HV126_47 = .;
    if HV126_48 = 9 then HV126_48 = .;
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
    if HV127_31 = 99 then HV127_31 = .;
    if HV127_32 = 99 then HV127_32 = .;
    if HV127_33 = 99 then HV127_33 = .;
    if HV127_34 = 99 then HV127_34 = .;
    if HV127_35 = 99 then HV127_35 = .;
    if HV127_36 = 99 then HV127_36 = .;
    if HV127_37 = 99 then HV127_37 = .;
    if HV127_38 = 99 then HV127_38 = .;
    if HV127_39 = 99 then HV127_39 = .;
    if HV127_40 = 99 then HV127_40 = .;
    if HV127_41 = 99 then HV127_41 = .;
    if HV127_42 = 99 then HV127_42 = .;
    if HV127_43 = 99 then HV127_43 = .;
    if HV127_44 = 99 then HV127_44 = .;
    if HV127_45 = 99 then HV127_45 = .;
    if HV127_46 = 99 then HV127_46 = .;
    if HV127_47 = 99 then HV127_47 = .;
    if HV127_48 = 99 then HV127_48 = .;
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
    if HV128_31 = 99 then HV128_31 = .;
    if HV128_32 = 99 then HV128_32 = .;
    if HV128_33 = 99 then HV128_33 = .;
    if HV128_34 = 99 then HV128_34 = .;
    if HV128_35 = 99 then HV128_35 = .;
    if HV128_36 = 99 then HV128_36 = .;
    if HV128_37 = 99 then HV128_37 = .;
    if HV128_38 = 99 then HV128_38 = .;
    if HV128_39 = 99 then HV128_39 = .;
    if HV128_40 = 99 then HV128_40 = .;
    if HV128_41 = 99 then HV128_41 = .;
    if HV128_42 = 99 then HV128_42 = .;
    if HV128_43 = 99 then HV128_43 = .;
    if HV128_44 = 99 then HV128_44 = .;
    if HV128_45 = 99 then HV128_45 = .;
    if HV128_46 = 99 then HV128_46 = .;
    if HV128_47 = 99 then HV128_47 = .;
    if HV128_48 = 99 then HV128_48 = .;
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
    if HV129_31 = 9 then HV129_31 = .;
    if HV129_32 = 9 then HV129_32 = .;
    if HV129_33 = 9 then HV129_33 = .;
    if HV129_34 = 9 then HV129_34 = .;
    if HV129_35 = 9 then HV129_35 = .;
    if HV129_36 = 9 then HV129_36 = .;
    if HV129_37 = 9 then HV129_37 = .;
    if HV129_38 = 9 then HV129_38 = .;
    if HV129_39 = 9 then HV129_39 = .;
    if HV129_40 = 9 then HV129_40 = .;
    if HV129_41 = 9 then HV129_41 = .;
    if HV129_42 = 9 then HV129_42 = .;
    if HV129_43 = 9 then HV129_43 = .;
    if HV129_44 = 9 then HV129_44 = .;
    if HV129_45 = 9 then HV129_45 = .;
    if HV129_46 = 9 then HV129_46 = .;
    if HV129_47 = 9 then HV129_47 = .;
    if HV129_48 = 9 then HV129_48 = .;
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
    if HV140_31 = 9 then HV140_31 = .;
    if HV140_32 = 9 then HV140_32 = .;
    if HV140_33 = 9 then HV140_33 = .;
    if HV140_34 = 9 then HV140_34 = .;
    if HV140_35 = 9 then HV140_35 = .;
    if HV140_36 = 9 then HV140_36 = .;
    if HV140_37 = 9 then HV140_37 = .;
    if HV140_38 = 9 then HV140_38 = .;
    if HV140_39 = 9 then HV140_39 = .;
    if HV140_40 = 9 then HV140_40 = .;
    if HV140_41 = 9 then HV140_41 = .;
    if HV140_42 = 9 then HV140_42 = .;
    if HV140_43 = 9 then HV140_43 = .;
    if HV140_44 = 9 then HV140_44 = .;
    if HV140_45 = 9 then HV140_45 = .;
    if HV140_46 = 9 then HV140_46 = .;
    if HV140_47 = 9 then HV140_47 = .;
    if HV140_48 = 9 then HV140_48 = .;
    if HA2_1    = 9999 then HA2_1 = .;
    if HA2_2    = 9999 then HA2_2 = .;
    if HA3_1    = 9999 then HA3_1 = .;
    if HA3_2    = 9999 then HA3_2 = .;
    if HA4_1    = 9999 then HA4_1 = .;
    if HA4_2    = 9999 then HA4_2 = .;
    if HA5_1    = 9999 then HA5_1 = .;
    if HA5_2    = 9999 then HA5_2 = .;
    if HA6_1    = 99999 then HA6_1 = .;
    if HA6_2    = 99999 then HA6_2 = .;
    if HA11_1   = 9999 then HA11_1 = .;
    if HA11_2   = 9999 then HA11_2 = .;
    if HA12_1   = 99999 then HA12_1 = .;
    if HA12_2   = 99999 then HA12_2 = .;
    if HA12A_1  = 99999 then HA12A_1 = .;
    if HA12A_2  = 99999 then HA12A_2 = .;
    if HA12B_1  = 99999 then HA12B_1 = .;
    if HA12B_2  = 99999 then HA12B_2 = .;
    if HA13_1   = 9 then HA13_1 = .;
    if HA13_2   = 9 then HA13_2 = .;
    if HA35_1   = 99 then HA35_1 = .;
    if HA35_2   = 99 then HA35_2 = .;
    if HA40_1   = 9999 then HA40_1 = .;
    if HA40_2   = 9999 then HA40_2 = .;
    if HA41_1   = 9999 then HA41_1 = .;
    if HA41_2   = 9999 then HA41_2 = .;
    if HA50_1   = 9 then HA50_1 = .;
    if HA50_2   = 9 then HA50_2 = .;
    if HA51_1   = 99 then HA51_1 = .;
    if HA51_2   = 99 then HA51_2 = .;
    if HA52_1   = 9 then HA52_1 = .;
    if HA52_2   = 9 then HA52_2 = .;
    if HA53_1   = 999 then HA53_1 = .;
    if HA53_2   = 999 then HA53_2 = .;
    if HA54_1   = 9 then HA54_1 = .;
    if HA54_2   = 9 then HA54_2 = .;
    if HA55_1   = 9 then HA55_1 = .;
    if HA55_2   = 9 then HA55_2 = .;
    if HA56_1   = 999 then HA56_1 = .;
    if HA56_2   = 999 then HA56_2 = .;
    if HA57_1   = 9 then HA57_1 = .;
    if HA57_2   = 9 then HA57_2 = .;
    if HA58_1   = 9 then HA58_1 = .;
    if HA58_2   = 9 then HA58_2 = .;
    if HA60_1   = 9 then HA60_1 = .;
    if HA60_2   = 9 then HA60_2 = .;
    if HA61_1   = 9 then HA61_1 = .;
    if HA61_2   = 9 then HA61_2 = .;
    if HA63_1   = 9 then HA63_1 = .;
    if HA63_2   = 9 then HA63_2 = .;
    if HA64_1   = 9 then HA64_1 = .;
    if HA64_2   = 9 then HA64_2 = .;
    if HA66_1   = 9 then HA66_1 = .;
    if HA66_2   = 9 then HA66_2 = .;
    if HA67_1   = 99 then HA67_1 = .;
    if HA67_2   = 99 then HA67_2 = .;
    if HA68_1   = 9 then HA68_1 = .;
    if HA68_2   = 9 then HA68_2 = .;
    if HC2_1    = 9999 then HC2_1 = .;
    if HC2_2    = 9999 then HC2_2 = .;
    if HC3_1    = 9999 then HC3_1 = .;
    if HC3_2    = 9999 then HC3_2 = .;
    if HC4_1    = 9999 then HC4_1 = .;
    if HC4_2    = 9999 then HC4_2 = .;
    if HC5_1    = 9999 then HC5_1 = .;
    if HC5_2    = 9999 then HC5_2 = .;
    if HC6_1    = 99999 then HC6_1 = .;
    if HC6_2    = 99999 then HC6_2 = .;
    if HC7_1    = 9999 then HC7_1 = .;
    if HC7_2    = 9999 then HC7_2 = .;
    if HC8_1    = 9999 then HC8_1 = .;
    if HC8_2    = 9999 then HC8_2 = .;
    if HC9_1    = 99999 then HC9_1 = .;
    if HC9_2    = 99999 then HC9_2 = .;
    if HC10_1   = 9999 then HC10_1 = .;
    if HC10_2   = 9999 then HC10_2 = .;
    if HC11_1   = 9999 then HC11_1 = .;
    if HC11_2   = 9999 then HC11_2 = .;
    if HC12_1   = 99999 then HC12_1 = .;
    if HC12_2   = 99999 then HC12_2 = .;
    if HC13_1   = 9 then HC13_1 = .;
    if HC13_2   = 9 then HC13_2 = .;
    if HC15_1   = 9 then HC15_1 = .;
    if HC15_2   = 9 then HC15_2 = .;
    if HC16_1   = 99 then HC16_1 = .;
    if HC16_2   = 99 then HC16_2 = .;
    if HC27_1   = 9 then HC27_1 = .;
    if HC27_2   = 9 then HC27_2 = .;
    if HC30_1   = 99 then HC30_1 = .;
    if HC30_2   = 99 then HC30_2 = .;
    if HC31_1   = 9999 then HC31_1 = .;
    if HC31_2   = 9999 then HC31_2 = .;
    if HC51_1   = 99 then HC51_1 = .;
    if HC51_2   = 99 then HC51_2 = .;
    if HC52_1   = 9 then HC52_1 = .;
    if HC52_2   = 9 then HC52_2 = .;
    if HC53_1   = 999 then HC53_1 = .;
    if HC53_2   = 999 then HC53_2 = .;
    if HC55_1   = 9 then HC55_1 = .;
    if HC55_2   = 9 then HC55_2 = .;
    if HC56_1   = 999 then HC56_1 = .;
    if HC56_2   = 999 then HC56_2 = .;
    if HC57_1   = 9 then HC57_1 = .;
    if HC57_2   = 9 then HC57_2 = .;
    if HC58_1   = 9 then HC58_1 = .;
    if HC58_2   = 9 then HC58_2 = .;
    if HC60_1   = 999 then HC60_1 = .;
    if HC60_2   = 999 then HC60_2 = .;
    if HC61_1   = 9 then HC61_1 = .;
    if HC61_2   = 9 then HC61_2 = .;
    if HC62_1   = 99 then HC62_1 = .;
    if HC62_2   = 99 then HC62_2 = .;
    if HC63_1   = 999 then HC63_1 = .;
    if HC63_2   = 999 then HC63_2 = .;
    if HC64_1   = 99 then HC64_1 = .;
    if HC64_2   = 99 then HC64_2 = .;
    if HC68_1   = 9 then HC68_1 = .;
    if HC68_2   = 9 then HC68_2 = .;
    if HC70_1   = 9999 then HC70_1 = .;
    if HC70_2   = 9999 then HC70_2 = .;
    if HC71_1   = 9999 then HC71_1 = .;
    if HC71_2   = 9999 then HC71_2 = .;
    if HC72_1   = 9999 then HC72_1 = .;
    if HC72_2   = 9999 then HC72_2 = .;
    if HC73_1   = 9999 then HC73_1 = .;
    if HC73_2   = 9999 then HC73_2 = .;
    if HML3_1   = 9 then HML3_1 = .;
    if HML3_2   = 9 then HML3_2 = .;
    if HML3_3   = 9 then HML3_3 = .;
    if HML3_4   = 9 then HML3_4 = .;
    if HML3_5   = 9 then HML3_5 = .;
    if HML3_6   = 9 then HML3_6 = .;
    if HML3_7   = 9 then HML3_7 = .;
    if HML4_1   = 99 then HML4_1 = .;
    if HML4_2   = 99 then HML4_2 = .;
    if HML4_3   = 99 then HML4_3 = .;
    if HML4_4   = 99 then HML4_4 = .;
    if HML4_5   = 99 then HML4_5 = .;
    if HML4_6   = 99 then HML4_6 = .;
    if HML4_7   = 99 then HML4_7 = .;
    if HML5_1   = 9 then HML5_1 = .;
    if HML5_2   = 9 then HML5_2 = .;
    if HML5_3   = 9 then HML5_3 = .;
    if HML5_4   = 9 then HML5_4 = .;
    if HML5_5   = 9 then HML5_5 = .;
    if HML5_6   = 9 then HML5_6 = .;
    if HML5_7   = 9 then HML5_7 = .;
    if HML6_1   = 9 then HML6_1 = .;
    if HML6_2   = 9 then HML6_2 = .;
    if HML6_3   = 9 then HML6_3 = .;
    if HML6_4   = 9 then HML6_4 = .;
    if HML6_5   = 9 then HML6_5 = .;
    if HML6_6   = 9 then HML6_6 = .;
    if HML6_7   = 9 then HML6_7 = .;
    if HML7_1   = 99 then HML7_1 = .;
    if HML7_2   = 99 then HML7_2 = .;
    if HML7_3   = 99 then HML7_3 = .;
    if HML7_4   = 99 then HML7_4 = .;
    if HML7_5   = 99 then HML7_5 = .;
    if HML7_6   = 99 then HML7_6 = .;
    if HML7_7   = 99 then HML7_7 = .;
    if HML8_1   = 9 then HML8_1 = .;
    if HML8_2   = 9 then HML8_2 = .;
    if HML8_3   = 9 then HML8_3 = .;
    if HML8_4   = 9 then HML8_4 = .;
    if HML8_5   = 9 then HML8_5 = .;
    if HML8_6   = 9 then HML8_6 = .;
    if HML8_7   = 9 then HML8_7 = .;
    if HML9_1   = 99 then HML9_1 = .;
    if HML9_2   = 99 then HML9_2 = .;
    if HML9_3   = 99 then HML9_3 = .;
    if HML9_4   = 99 then HML9_4 = .;
    if HML9_5   = 99 then HML9_5 = .;
    if HML9_6   = 99 then HML9_6 = .;
    if HML9_7   = 99 then HML9_7 = .;
    if HML10_1  = 9 then HML10_1 = .;
    if HML10_2  = 9 then HML10_2 = .;
    if HML10_3  = 9 then HML10_3 = .;
    if HML10_4  = 9 then HML10_4 = .;
    if HML10_5  = 9 then HML10_5 = .;
    if HML10_6  = 9 then HML10_6 = .;
    if HML10_7  = 9 then HML10_7 = .;
    if HML11_1  = 9 then HML11_1 = .;
    if HML11_2  = 9 then HML11_2 = .;
    if HML11_3  = 9 then HML11_3 = .;
    if HML11_4  = 9 then HML11_4 = .;
    if HML11_5  = 9 then HML11_5 = .;
    if HML11_6  = 9 then HML11_6 = .;
    if HML11_7  = 9 then HML11_7 = .;
    if HML21_1  = 9 then HML21_1 = .;
    if HML21_2  = 9 then HML21_2 = .;
    if HML21_3  = 9 then HML21_3 = .;
    if HML21_4  = 9 then HML21_4 = .;
    if HML21_5  = 9 then HML21_5 = .;
    if HML21_6  = 9 then HML21_6 = .;
    if HML21_7  = 9 then HML21_7 = .;
    if HMLA_1   = 99 then HMLA_1 = .;
    if HMLA_2   = 99 then HMLA_2 = .;
    if HMLA_3   = 99 then HMLA_3 = .;
    if HMLA_4   = 99 then HMLA_4 = .;
    if HMLA_5   = 99 then HMLA_5 = .;
    if HMLA_6   = 99 then HMLA_6 = .;
    if HMLA_7   = 99 then HMLA_7 = .;
    if HMLB_1   = 99 then HMLB_1 = .;
    if HMLB_2   = 99 then HMLB_2 = .;
    if HMLB_3   = 99 then HMLB_3 = .;
    if HMLB_4   = 99 then HMLB_4 = .;
    if HMLB_5   = 99 then HMLB_5 = .;
    if HMLB_6   = 99 then HMLB_6 = .;
    if HMLB_7   = 99 then HMLB_7 = .;
    if HMLC_1   = 99 then HMLC_1 = .;
    if HMLC_2   = 99 then HMLC_2 = .;
    if HMLC_3   = 99 then HMLC_3 = .;
    if HMLC_4   = 99 then HMLC_4 = .;
    if HMLC_5   = 99 then HMLC_5 = .;
    if HMLC_6   = 99 then HMLC_6 = .;
    if HMLC_7   = 99 then HMLC_7 = .;
    if HML22_1  = 9 then HML22_1 = .;
    if HML22_2  = 9 then HML22_2 = .;
    if HML22_3  = 9 then HML22_3 = .;
    if HML22_4  = 9 then HML22_4 = .;
    if HML22_5  = 9 then HML22_5 = .;
    if HML22_6  = 9 then HML22_6 = .;
    if HML22_7  = 9 then HML22_7 = .;
    if HML23_1  = 99 then HML23_1 = .;
    if HML23_2  = 99 then HML23_2 = .;
    if HML23_3  = 99 then HML23_3 = .;
    if HML23_4  = 99 then HML23_4 = .;
    if HML23_5  = 99 then HML23_5 = .;
    if HML23_6  = 99 then HML23_6 = .;
    if HML23_7  = 99 then HML23_7 = .;
    if HML12_01 = 9 then HML12_01 = .;
    if HML12_02 = 9 then HML12_02 = .;
    if HML12_03 = 9 then HML12_03 = .;
    if HML12_04 = 9 then HML12_04 = .;
    if HML12_05 = 9 then HML12_05 = .;
    if HML12_06 = 9 then HML12_06 = .;
    if HML12_07 = 9 then HML12_07 = .;
    if HML12_08 = 9 then HML12_08 = .;
    if HML12_09 = 9 then HML12_09 = .;
    if HML12_10 = 9 then HML12_10 = .;
    if HML12_11 = 9 then HML12_11 = .;
    if HML12_12 = 9 then HML12_12 = .;
    if HML12_13 = 9 then HML12_13 = .;
    if HML12_14 = 9 then HML12_14 = .;
    if HML12_15 = 9 then HML12_15 = .;
    if HML12_16 = 9 then HML12_16 = .;
    if HML12_17 = 9 then HML12_17 = .;
    if HML12_18 = 9 then HML12_18 = .;
    if HML12_19 = 9 then HML12_19 = .;
    if HML12_20 = 9 then HML12_20 = .;
    if HML12_21 = 9 then HML12_21 = .;
    if HML12_22 = 9 then HML12_22 = .;
    if HML12_23 = 9 then HML12_23 = .;
    if HML12_24 = 9 then HML12_24 = .;
    if HML12_25 = 9 then HML12_25 = .;
    if HML12_26 = 9 then HML12_26 = .;
    if HML12_27 = 9 then HML12_27 = .;
    if HML12_28 = 9 then HML12_28 = .;
    if HML12_29 = 9 then HML12_29 = .;
    if HML12_30 = 9 then HML12_30 = .;
    if HML12_31 = 9 then HML12_31 = .;
    if HML12_32 = 9 then HML12_32 = .;
    if HML12_33 = 9 then HML12_33 = .;
    if HML12_34 = 9 then HML12_34 = .;
    if HML12_35 = 9 then HML12_35 = .;
    if HML12_36 = 9 then HML12_36 = .;
    if HML12_37 = 9 then HML12_37 = .;
    if HML12_38 = 9 then HML12_38 = .;
    if HML12_39 = 9 then HML12_39 = .;
    if HML12_40 = 9 then HML12_40 = .;
    if HML12_41 = 9 then HML12_41 = .;
    if HML12_42 = 9 then HML12_42 = .;
    if HML12_43 = 9 then HML12_43 = .;
    if HML12_44 = 9 then HML12_44 = .;
    if HML12_45 = 9 then HML12_45 = .;
    if HML12_46 = 9 then HML12_46 = .;
    if HML12_47 = 9 then HML12_47 = .;
    if HML12_48 = 9 then HML12_48 = .;
    if HML16_01 = 99 then HML16_01 = .;
    if HML16_02 = 99 then HML16_02 = .;
    if HML16_03 = 99 then HML16_03 = .;
    if HML16_04 = 99 then HML16_04 = .;
    if HML16_05 = 99 then HML16_05 = .;
    if HML16_06 = 99 then HML16_06 = .;
    if HML16_07 = 99 then HML16_07 = .;
    if HML16_08 = 99 then HML16_08 = .;
    if HML16_09 = 99 then HML16_09 = .;
    if HML16_10 = 99 then HML16_10 = .;
    if HML16_11 = 99 then HML16_11 = .;
    if HML16_12 = 99 then HML16_12 = .;
    if HML16_13 = 99 then HML16_13 = .;
    if HML16_14 = 99 then HML16_14 = .;
    if HML16_15 = 99 then HML16_15 = .;
    if HML16_16 = 99 then HML16_16 = .;
    if HML16_17 = 99 then HML16_17 = .;
    if HML16_18 = 99 then HML16_18 = .;
    if HML16_19 = 99 then HML16_19 = .;
    if HML16_20 = 99 then HML16_20 = .;
    if HML16_21 = 99 then HML16_21 = .;
    if HML16_22 = 99 then HML16_22 = .;
    if HML16_23 = 99 then HML16_23 = .;
    if HML16_24 = 99 then HML16_24 = .;
    if HML16_25 = 99 then HML16_25 = .;
    if HML16_26 = 99 then HML16_26 = .;
    if HML16_27 = 99 then HML16_27 = .;
    if HML16_28 = 99 then HML16_28 = .;
    if HML16_29 = 99 then HML16_29 = .;
    if HML16_30 = 99 then HML16_30 = .;
    if HML16_31 = 99 then HML16_31 = .;
    if HML16_32 = 99 then HML16_32 = .;
    if HML16_33 = 99 then HML16_33 = .;
    if HML16_34 = 99 then HML16_34 = .;
    if HML16_35 = 99 then HML16_35 = .;
    if HML16_36 = 99 then HML16_36 = .;
    if HML16_37 = 99 then HML16_37 = .;
    if HML16_38 = 99 then HML16_38 = .;
    if HML16_39 = 99 then HML16_39 = .;
    if HML16_40 = 99 then HML16_40 = .;
    if HML16_41 = 99 then HML16_41 = .;
    if HML16_42 = 99 then HML16_42 = .;
    if HML16_43 = 99 then HML16_43 = .;
    if HML16_44 = 99 then HML16_44 = .;
    if HML16_45 = 99 then HML16_45 = .;
    if HML16_46 = 99 then HML16_46 = .;
    if HML16_47 = 99 then HML16_47 = .;
    if HML16_48 = 99 then HML16_48 = .;
    if HML18_01 = 9 then HML18_01 = .;
    if HML18_02 = 9 then HML18_02 = .;
    if HML18_03 = 9 then HML18_03 = .;
    if HML18_04 = 9 then HML18_04 = .;
    if HML18_05 = 9 then HML18_05 = .;
    if HML18_06 = 9 then HML18_06 = .;
    if HML18_07 = 9 then HML18_07 = .;
    if HML18_08 = 9 then HML18_08 = .;
    if HML18_09 = 9 then HML18_09 = .;
    if HML18_10 = 9 then HML18_10 = .;
    if HML18_11 = 9 then HML18_11 = .;
    if HML18_12 = 9 then HML18_12 = .;
    if HML18_13 = 9 then HML18_13 = .;
    if HML18_14 = 9 then HML18_14 = .;
    if HML18_15 = 9 then HML18_15 = .;
    if HML18_16 = 9 then HML18_16 = .;
    if HML18_17 = 9 then HML18_17 = .;
    if HML18_18 = 9 then HML18_18 = .;
    if HML18_19 = 9 then HML18_19 = .;
    if HML18_20 = 9 then HML18_20 = .;
    if HML18_21 = 9 then HML18_21 = .;
    if HML18_22 = 9 then HML18_22 = .;
    if HML18_23 = 9 then HML18_23 = .;
    if HML18_24 = 9 then HML18_24 = .;
    if HML18_25 = 9 then HML18_25 = .;
    if HML18_26 = 9 then HML18_26 = .;
    if HML18_27 = 9 then HML18_27 = .;
    if HML18_28 = 9 then HML18_28 = .;
    if HML18_29 = 9 then HML18_29 = .;
    if HML18_30 = 9 then HML18_30 = .;
    if HML18_31 = 9 then HML18_31 = .;
    if HML18_32 = 9 then HML18_32 = .;
    if HML18_33 = 9 then HML18_33 = .;
    if HML18_34 = 9 then HML18_34 = .;
    if HML18_35 = 9 then HML18_35 = .;
    if HML18_36 = 9 then HML18_36 = .;
    if HML18_37 = 9 then HML18_37 = .;
    if HML18_38 = 9 then HML18_38 = .;
    if HML18_39 = 9 then HML18_39 = .;
    if HML18_40 = 9 then HML18_40 = .;
    if HML18_41 = 9 then HML18_41 = .;
    if HML18_42 = 9 then HML18_42 = .;
    if HML18_43 = 9 then HML18_43 = .;
    if HML18_44 = 9 then HML18_44 = .;
    if HML18_45 = 9 then HML18_45 = .;
    if HML18_46 = 9 then HML18_46 = .;
    if HML18_47 = 9 then HML18_47 = .;
    if HML18_48 = 9 then HML18_48 = .;
    if HML31_01 = 9 then HML31_01 = .;
    if HML31_02 = 9 then HML31_02 = .;
    if HML31_03 = 9 then HML31_03 = .;
    if HML31_04 = 9 then HML31_04 = .;
    if HML31_05 = 9 then HML31_05 = .;
    if HML31_06 = 9 then HML31_06 = .;
    if HML31_07 = 9 then HML31_07 = .;
    if HML31_08 = 9 then HML31_08 = .;
    if HML31_09 = 9 then HML31_09 = .;
    if HML31_10 = 9 then HML31_10 = .;
    if HML31_11 = 9 then HML31_11 = .;
    if HML31_12 = 9 then HML31_12 = .;
    if HML31_13 = 9 then HML31_13 = .;
    if HML31_14 = 9 then HML31_14 = .;
    if HML31_15 = 9 then HML31_15 = .;
    if HML31_16 = 9 then HML31_16 = .;
    if HML31_17 = 9 then HML31_17 = .;
    if HML31_18 = 9 then HML31_18 = .;
    if HML31_19 = 9 then HML31_19 = .;
    if HML31_20 = 9 then HML31_20 = .;
    if HML31_21 = 9 then HML31_21 = .;
    if HML31_22 = 9 then HML31_22 = .;
    if HML31_23 = 9 then HML31_23 = .;
    if HML31_24 = 9 then HML31_24 = .;
    if HML31_25 = 9 then HML31_25 = .;
    if HML31_26 = 9 then HML31_26 = .;
    if HML31_27 = 9 then HML31_27 = .;
    if HML31_28 = 9 then HML31_28 = .;
    if HML31_29 = 9 then HML31_29 = .;
    if HML31_30 = 9 then HML31_30 = .;
    if HML31_31 = 9 then HML31_31 = .;
    if HML31_32 = 9 then HML31_32 = .;
    if HML31_33 = 9 then HML31_33 = .;
    if HML31_34 = 9 then HML31_34 = .;
    if HML31_35 = 9 then HML31_35 = .;
    if HML31_36 = 9 then HML31_36 = .;
    if HML31_37 = 9 then HML31_37 = .;
    if HML31_38 = 9 then HML31_38 = .;
    if HML31_39 = 9 then HML31_39 = .;
    if HML31_40 = 9 then HML31_40 = .;
    if HML31_41 = 9 then HML31_41 = .;
    if HML31_42 = 9 then HML31_42 = .;
    if HML31_43 = 9 then HML31_43 = .;
    if HML31_44 = 9 then HML31_44 = .;
    if HML31_45 = 9 then HML31_45 = .;
    if HML31_46 = 9 then HML31_46 = .;
    if HML31_47 = 9 then HML31_47 = .;
    if HML31_48 = 9 then HML31_48 = .;
    if HML32_01 = 9 then HML32_01 = .;
    if HML32_02 = 9 then HML32_02 = .;
    if HML32_03 = 9 then HML32_03 = .;
    if HML32_04 = 9 then HML32_04 = .;
    if HML32_05 = 9 then HML32_05 = .;
    if HML32_06 = 9 then HML32_06 = .;
    if HML32_07 = 9 then HML32_07 = .;
    if HML32_08 = 9 then HML32_08 = .;
    if HML32_09 = 9 then HML32_09 = .;
    if HML32_10 = 9 then HML32_10 = .;
    if HML32_11 = 9 then HML32_11 = .;
    if HML32_12 = 9 then HML32_12 = .;
    if HML32_13 = 9 then HML32_13 = .;
    if HML32_14 = 9 then HML32_14 = .;
    if HML32_15 = 9 then HML32_15 = .;
    if HML32_16 = 9 then HML32_16 = .;
    if HML32_17 = 9 then HML32_17 = .;
    if HML32_18 = 9 then HML32_18 = .;
    if HML32_19 = 9 then HML32_19 = .;
    if HML32_20 = 9 then HML32_20 = .;
    if HML32_21 = 9 then HML32_21 = .;
    if HML32_22 = 9 then HML32_22 = .;
    if HML32_23 = 9 then HML32_23 = .;
    if HML32_24 = 9 then HML32_24 = .;
    if HML32_25 = 9 then HML32_25 = .;
    if HML32_26 = 9 then HML32_26 = .;
    if HML32_27 = 9 then HML32_27 = .;
    if HML32_28 = 9 then HML32_28 = .;
    if HML32_29 = 9 then HML32_29 = .;
    if HML32_30 = 9 then HML32_30 = .;
    if HML32_31 = 9 then HML32_31 = .;
    if HML32_32 = 9 then HML32_32 = .;
    if HML32_33 = 9 then HML32_33 = .;
    if HML32_34 = 9 then HML32_34 = .;
    if HML32_35 = 9 then HML32_35 = .;
    if HML32_36 = 9 then HML32_36 = .;
    if HML32_37 = 9 then HML32_37 = .;
    if HML32_38 = 9 then HML32_38 = .;
    if HML32_39 = 9 then HML32_39 = .;
    if HML32_40 = 9 then HML32_40 = .;
    if HML32_41 = 9 then HML32_41 = .;
    if HML32_42 = 9 then HML32_42 = .;
    if HML32_43 = 9 then HML32_43 = .;
    if HML32_44 = 9 then HML32_44 = .;
    if HML32_45 = 9 then HML32_45 = .;
    if HML32_46 = 9 then HML32_46 = .;
    if HML32_47 = 9 then HML32_47 = .;
    if HML32_48 = 9 then HML32_48 = .;
    if HML32A_01 = 9 then HML32A_01 = .;
    if HML32A_02 = 9 then HML32A_02 = .;
    if HML32A_03 = 9 then HML32A_03 = .;
    if HML32A_04 = 9 then HML32A_04 = .;
    if HML32A_05 = 9 then HML32A_05 = .;
    if HML32A_06 = 9 then HML32A_06 = .;
    if HML32A_07 = 9 then HML32A_07 = .;
    if HML32A_08 = 9 then HML32A_08 = .;
    if HML32A_09 = 9 then HML32A_09 = .;
    if HML32A_10 = 9 then HML32A_10 = .;
    if HML32A_11 = 9 then HML32A_11 = .;
    if HML32A_12 = 9 then HML32A_12 = .;
    if HML32A_13 = 9 then HML32A_13 = .;
    if HML32A_14 = 9 then HML32A_14 = .;
    if HML32A_15 = 9 then HML32A_15 = .;
    if HML32A_16 = 9 then HML32A_16 = .;
    if HML32A_17 = 9 then HML32A_17 = .;
    if HML32A_18 = 9 then HML32A_18 = .;
    if HML32A_19 = 9 then HML32A_19 = .;
    if HML32A_20 = 9 then HML32A_20 = .;
    if HML32A_21 = 9 then HML32A_21 = .;
    if HML32A_22 = 9 then HML32A_22 = .;
    if HML32A_23 = 9 then HML32A_23 = .;
    if HML32A_24 = 9 then HML32A_24 = .;
    if HML32A_25 = 9 then HML32A_25 = .;
    if HML32A_26 = 9 then HML32A_26 = .;
    if HML32A_27 = 9 then HML32A_27 = .;
    if HML32A_28 = 9 then HML32A_28 = .;
    if HML32A_29 = 9 then HML32A_29 = .;
    if HML32A_30 = 9 then HML32A_30 = .;
    if HML32A_31 = 9 then HML32A_31 = .;
    if HML32A_32 = 9 then HML32A_32 = .;
    if HML32A_33 = 9 then HML32A_33 = .;
    if HML32A_34 = 9 then HML32A_34 = .;
    if HML32A_35 = 9 then HML32A_35 = .;
    if HML32A_36 = 9 then HML32A_36 = .;
    if HML32A_37 = 9 then HML32A_37 = .;
    if HML32A_38 = 9 then HML32A_38 = .;
    if HML32A_39 = 9 then HML32A_39 = .;
    if HML32A_40 = 9 then HML32A_40 = .;
    if HML32A_41 = 9 then HML32A_41 = .;
    if HML32A_42 = 9 then HML32A_42 = .;
    if HML32A_43 = 9 then HML32A_43 = .;
    if HML32A_44 = 9 then HML32A_44 = .;
    if HML32A_45 = 9 then HML32A_45 = .;
    if HML32A_46 = 9 then HML32A_46 = .;
    if HML32A_47 = 9 then HML32A_47 = .;
    if HML32A_48 = 9 then HML32A_48 = .;
    if HML32B_01 = 9 then HML32B_01 = .;
    if HML32B_02 = 9 then HML32B_02 = .;
    if HML32B_03 = 9 then HML32B_03 = .;
    if HML32B_04 = 9 then HML32B_04 = .;
    if HML32B_05 = 9 then HML32B_05 = .;
    if HML32B_06 = 9 then HML32B_06 = .;
    if HML32B_07 = 9 then HML32B_07 = .;
    if HML32B_08 = 9 then HML32B_08 = .;
    if HML32B_09 = 9 then HML32B_09 = .;
    if HML32B_10 = 9 then HML32B_10 = .;
    if HML32B_11 = 9 then HML32B_11 = .;
    if HML32B_12 = 9 then HML32B_12 = .;
    if HML32B_13 = 9 then HML32B_13 = .;
    if HML32B_14 = 9 then HML32B_14 = .;
    if HML32B_15 = 9 then HML32B_15 = .;
    if HML32B_16 = 9 then HML32B_16 = .;
    if HML32B_17 = 9 then HML32B_17 = .;
    if HML32B_18 = 9 then HML32B_18 = .;
    if HML32B_19 = 9 then HML32B_19 = .;
    if HML32B_20 = 9 then HML32B_20 = .;
    if HML32B_21 = 9 then HML32B_21 = .;
    if HML32B_22 = 9 then HML32B_22 = .;
    if HML32B_23 = 9 then HML32B_23 = .;
    if HML32B_24 = 9 then HML32B_24 = .;
    if HML32B_25 = 9 then HML32B_25 = .;
    if HML32B_26 = 9 then HML32B_26 = .;
    if HML32B_27 = 9 then HML32B_27 = .;
    if HML32B_28 = 9 then HML32B_28 = .;
    if HML32B_29 = 9 then HML32B_29 = .;
    if HML32B_30 = 9 then HML32B_30 = .;
    if HML32B_31 = 9 then HML32B_31 = .;
    if HML32B_32 = 9 then HML32B_32 = .;
    if HML32B_33 = 9 then HML32B_33 = .;
    if HML32B_34 = 9 then HML32B_34 = .;
    if HML32B_35 = 9 then HML32B_35 = .;
    if HML32B_36 = 9 then HML32B_36 = .;
    if HML32B_37 = 9 then HML32B_37 = .;
    if HML32B_38 = 9 then HML32B_38 = .;
    if HML32B_39 = 9 then HML32B_39 = .;
    if HML32B_40 = 9 then HML32B_40 = .;
    if HML32B_41 = 9 then HML32B_41 = .;
    if HML32B_42 = 9 then HML32B_42 = .;
    if HML32B_43 = 9 then HML32B_43 = .;
    if HML32B_44 = 9 then HML32B_44 = .;
    if HML32B_45 = 9 then HML32B_45 = .;
    if HML32B_46 = 9 then HML32B_46 = .;
    if HML32B_47 = 9 then HML32B_47 = .;
    if HML32B_48 = 9 then HML32B_48 = .;
    if HML32C_01 = 9 then HML32C_01 = .;
    if HML32C_02 = 9 then HML32C_02 = .;
    if HML32C_03 = 9 then HML32C_03 = .;
    if HML32C_04 = 9 then HML32C_04 = .;
    if HML32C_05 = 9 then HML32C_05 = .;
    if HML32C_06 = 9 then HML32C_06 = .;
    if HML32C_07 = 9 then HML32C_07 = .;
    if HML32C_08 = 9 then HML32C_08 = .;
    if HML32C_09 = 9 then HML32C_09 = .;
    if HML32C_10 = 9 then HML32C_10 = .;
    if HML32C_11 = 9 then HML32C_11 = .;
    if HML32C_12 = 9 then HML32C_12 = .;
    if HML32C_13 = 9 then HML32C_13 = .;
    if HML32C_14 = 9 then HML32C_14 = .;
    if HML32C_15 = 9 then HML32C_15 = .;
    if HML32C_16 = 9 then HML32C_16 = .;
    if HML32C_17 = 9 then HML32C_17 = .;
    if HML32C_18 = 9 then HML32C_18 = .;
    if HML32C_19 = 9 then HML32C_19 = .;
    if HML32C_20 = 9 then HML32C_20 = .;
    if HML32C_21 = 9 then HML32C_21 = .;
    if HML32C_22 = 9 then HML32C_22 = .;
    if HML32C_23 = 9 then HML32C_23 = .;
    if HML32C_24 = 9 then HML32C_24 = .;
    if HML32C_25 = 9 then HML32C_25 = .;
    if HML32C_26 = 9 then HML32C_26 = .;
    if HML32C_27 = 9 then HML32C_27 = .;
    if HML32C_28 = 9 then HML32C_28 = .;
    if HML32C_29 = 9 then HML32C_29 = .;
    if HML32C_30 = 9 then HML32C_30 = .;
    if HML32C_31 = 9 then HML32C_31 = .;
    if HML32C_32 = 9 then HML32C_32 = .;
    if HML32C_33 = 9 then HML32C_33 = .;
    if HML32C_34 = 9 then HML32C_34 = .;
    if HML32C_35 = 9 then HML32C_35 = .;
    if HML32C_36 = 9 then HML32C_36 = .;
    if HML32C_37 = 9 then HML32C_37 = .;
    if HML32C_38 = 9 then HML32C_38 = .;
    if HML32C_39 = 9 then HML32C_39 = .;
    if HML32C_40 = 9 then HML32C_40 = .;
    if HML32C_41 = 9 then HML32C_41 = .;
    if HML32C_42 = 9 then HML32C_42 = .;
    if HML32C_43 = 9 then HML32C_43 = .;
    if HML32C_44 = 9 then HML32C_44 = .;
    if HML32C_45 = 9 then HML32C_45 = .;
    if HML32C_46 = 9 then HML32C_46 = .;
    if HML32C_47 = 9 then HML32C_47 = .;
    if HML32C_48 = 9 then HML32C_48 = .;
    if HML32D_01 = 9 then HML32D_01 = .;
    if HML32D_02 = 9 then HML32D_02 = .;
    if HML32D_03 = 9 then HML32D_03 = .;
    if HML32D_04 = 9 then HML32D_04 = .;
    if HML32D_05 = 9 then HML32D_05 = .;
    if HML32D_06 = 9 then HML32D_06 = .;
    if HML32D_07 = 9 then HML32D_07 = .;
    if HML32D_08 = 9 then HML32D_08 = .;
    if HML32D_09 = 9 then HML32D_09 = .;
    if HML32D_10 = 9 then HML32D_10 = .;
    if HML32D_11 = 9 then HML32D_11 = .;
    if HML32D_12 = 9 then HML32D_12 = .;
    if HML32D_13 = 9 then HML32D_13 = .;
    if HML32D_14 = 9 then HML32D_14 = .;
    if HML32D_15 = 9 then HML32D_15 = .;
    if HML32D_16 = 9 then HML32D_16 = .;
    if HML32D_17 = 9 then HML32D_17 = .;
    if HML32D_18 = 9 then HML32D_18 = .;
    if HML32D_19 = 9 then HML32D_19 = .;
    if HML32D_20 = 9 then HML32D_20 = .;
    if HML32D_21 = 9 then HML32D_21 = .;
    if HML32D_22 = 9 then HML32D_22 = .;
    if HML32D_23 = 9 then HML32D_23 = .;
    if HML32D_24 = 9 then HML32D_24 = .;
    if HML32D_25 = 9 then HML32D_25 = .;
    if HML32D_26 = 9 then HML32D_26 = .;
    if HML32D_27 = 9 then HML32D_27 = .;
    if HML32D_28 = 9 then HML32D_28 = .;
    if HML32D_29 = 9 then HML32D_29 = .;
    if HML32D_30 = 9 then HML32D_30 = .;
    if HML32D_31 = 9 then HML32D_31 = .;
    if HML32D_32 = 9 then HML32D_32 = .;
    if HML32D_33 = 9 then HML32D_33 = .;
    if HML32D_34 = 9 then HML32D_34 = .;
    if HML32D_35 = 9 then HML32D_35 = .;
    if HML32D_36 = 9 then HML32D_36 = .;
    if HML32D_37 = 9 then HML32D_37 = .;
    if HML32D_38 = 9 then HML32D_38 = .;
    if HML32D_39 = 9 then HML32D_39 = .;
    if HML32D_40 = 9 then HML32D_40 = .;
    if HML32D_41 = 9 then HML32D_41 = .;
    if HML32D_42 = 9 then HML32D_42 = .;
    if HML32D_43 = 9 then HML32D_43 = .;
    if HML32D_44 = 9 then HML32D_44 = .;
    if HML32D_45 = 9 then HML32D_45 = .;
    if HML32D_46 = 9 then HML32D_46 = .;
    if HML32D_47 = 9 then HML32D_47 = .;
    if HML32D_48 = 9 then HML32D_48 = .;
    if HML32E_01 = 9 then HML32E_01 = .;
    if HML32E_02 = 9 then HML32E_02 = .;
    if HML32E_03 = 9 then HML32E_03 = .;
    if HML32E_04 = 9 then HML32E_04 = .;
    if HML32E_05 = 9 then HML32E_05 = .;
    if HML32E_06 = 9 then HML32E_06 = .;
    if HML32E_07 = 9 then HML32E_07 = .;
    if HML32E_08 = 9 then HML32E_08 = .;
    if HML32E_09 = 9 then HML32E_09 = .;
    if HML32E_10 = 9 then HML32E_10 = .;
    if HML32E_11 = 9 then HML32E_11 = .;
    if HML32E_12 = 9 then HML32E_12 = .;
    if HML32E_13 = 9 then HML32E_13 = .;
    if HML32E_14 = 9 then HML32E_14 = .;
    if HML32E_15 = 9 then HML32E_15 = .;
    if HML32E_16 = 9 then HML32E_16 = .;
    if HML32E_17 = 9 then HML32E_17 = .;
    if HML32E_18 = 9 then HML32E_18 = .;
    if HML32E_19 = 9 then HML32E_19 = .;
    if HML32E_20 = 9 then HML32E_20 = .;
    if HML32E_21 = 9 then HML32E_21 = .;
    if HML32E_22 = 9 then HML32E_22 = .;
    if HML32E_23 = 9 then HML32E_23 = .;
    if HML32E_24 = 9 then HML32E_24 = .;
    if HML32E_25 = 9 then HML32E_25 = .;
    if HML32E_26 = 9 then HML32E_26 = .;
    if HML32E_27 = 9 then HML32E_27 = .;
    if HML32E_28 = 9 then HML32E_28 = .;
    if HML32E_29 = 9 then HML32E_29 = .;
    if HML32E_30 = 9 then HML32E_30 = .;
    if HML32E_31 = 9 then HML32E_31 = .;
    if HML32E_32 = 9 then HML32E_32 = .;
    if HML32E_33 = 9 then HML32E_33 = .;
    if HML32E_34 = 9 then HML32E_34 = .;
    if HML32E_35 = 9 then HML32E_35 = .;
    if HML32E_36 = 9 then HML32E_36 = .;
    if HML32E_37 = 9 then HML32E_37 = .;
    if HML32E_38 = 9 then HML32E_38 = .;
    if HML32E_39 = 9 then HML32E_39 = .;
    if HML32E_40 = 9 then HML32E_40 = .;
    if HML32E_41 = 9 then HML32E_41 = .;
    if HML32E_42 = 9 then HML32E_42 = .;
    if HML32E_43 = 9 then HML32E_43 = .;
    if HML32E_44 = 9 then HML32E_44 = .;
    if HML32E_45 = 9 then HML32E_45 = .;
    if HML32E_46 = 9 then HML32E_46 = .;
    if HML32E_47 = 9 then HML32E_47 = .;
    if HML32E_48 = 9 then HML32E_48 = .;
    if HML32F_01 = 9 then HML32F_01 = .;
    if HML32F_02 = 9 then HML32F_02 = .;
    if HML32F_03 = 9 then HML32F_03 = .;
    if HML32F_04 = 9 then HML32F_04 = .;
    if HML32F_05 = 9 then HML32F_05 = .;
    if HML32F_06 = 9 then HML32F_06 = .;
    if HML32F_07 = 9 then HML32F_07 = .;
    if HML32F_08 = 9 then HML32F_08 = .;
    if HML32F_09 = 9 then HML32F_09 = .;
    if HML32F_10 = 9 then HML32F_10 = .;
    if HML32F_11 = 9 then HML32F_11 = .;
    if HML32F_12 = 9 then HML32F_12 = .;
    if HML32F_13 = 9 then HML32F_13 = .;
    if HML32F_14 = 9 then HML32F_14 = .;
    if HML32F_15 = 9 then HML32F_15 = .;
    if HML32F_16 = 9 then HML32F_16 = .;
    if HML32F_17 = 9 then HML32F_17 = .;
    if HML32F_18 = 9 then HML32F_18 = .;
    if HML32F_19 = 9 then HML32F_19 = .;
    if HML32F_20 = 9 then HML32F_20 = .;
    if HML32F_21 = 9 then HML32F_21 = .;
    if HML32F_22 = 9 then HML32F_22 = .;
    if HML32F_23 = 9 then HML32F_23 = .;
    if HML32F_24 = 9 then HML32F_24 = .;
    if HML32F_25 = 9 then HML32F_25 = .;
    if HML32F_26 = 9 then HML32F_26 = .;
    if HML32F_27 = 9 then HML32F_27 = .;
    if HML32F_28 = 9 then HML32F_28 = .;
    if HML32F_29 = 9 then HML32F_29 = .;
    if HML32F_30 = 9 then HML32F_30 = .;
    if HML32F_31 = 9 then HML32F_31 = .;
    if HML32F_32 = 9 then HML32F_32 = .;
    if HML32F_33 = 9 then HML32F_33 = .;
    if HML32F_34 = 9 then HML32F_34 = .;
    if HML32F_35 = 9 then HML32F_35 = .;
    if HML32F_36 = 9 then HML32F_36 = .;
    if HML32F_37 = 9 then HML32F_37 = .;
    if HML32F_38 = 9 then HML32F_38 = .;
    if HML32F_39 = 9 then HML32F_39 = .;
    if HML32F_40 = 9 then HML32F_40 = .;
    if HML32F_41 = 9 then HML32F_41 = .;
    if HML32F_42 = 9 then HML32F_42 = .;
    if HML32F_43 = 9 then HML32F_43 = .;
    if HML32F_44 = 9 then HML32F_44 = .;
    if HML32F_45 = 9 then HML32F_45 = .;
    if HML32F_46 = 9 then HML32F_46 = .;
    if HML32F_47 = 9 then HML32F_47 = .;
    if HML32F_48 = 9 then HML32F_48 = .;
    if HML32G_01 = 9 then HML32G_01 = .;
    if HML32G_02 = 9 then HML32G_02 = .;
    if HML32G_03 = 9 then HML32G_03 = .;
    if HML32G_04 = 9 then HML32G_04 = .;
    if HML32G_05 = 9 then HML32G_05 = .;
    if HML32G_06 = 9 then HML32G_06 = .;
    if HML32G_07 = 9 then HML32G_07 = .;
    if HML32G_08 = 9 then HML32G_08 = .;
    if HML32G_09 = 9 then HML32G_09 = .;
    if HML32G_10 = 9 then HML32G_10 = .;
    if HML32G_11 = 9 then HML32G_11 = .;
    if HML32G_12 = 9 then HML32G_12 = .;
    if HML32G_13 = 9 then HML32G_13 = .;
    if HML32G_14 = 9 then HML32G_14 = .;
    if HML32G_15 = 9 then HML32G_15 = .;
    if HML32G_16 = 9 then HML32G_16 = .;
    if HML32G_17 = 9 then HML32G_17 = .;
    if HML32G_18 = 9 then HML32G_18 = .;
    if HML32G_19 = 9 then HML32G_19 = .;
    if HML32G_20 = 9 then HML32G_20 = .;
    if HML32G_21 = 9 then HML32G_21 = .;
    if HML32G_22 = 9 then HML32G_22 = .;
    if HML32G_23 = 9 then HML32G_23 = .;
    if HML32G_24 = 9 then HML32G_24 = .;
    if HML32G_25 = 9 then HML32G_25 = .;
    if HML32G_26 = 9 then HML32G_26 = .;
    if HML32G_27 = 9 then HML32G_27 = .;
    if HML32G_28 = 9 then HML32G_28 = .;
    if HML32G_29 = 9 then HML32G_29 = .;
    if HML32G_30 = 9 then HML32G_30 = .;
    if HML32G_31 = 9 then HML32G_31 = .;
    if HML32G_32 = 9 then HML32G_32 = .;
    if HML32G_33 = 9 then HML32G_33 = .;
    if HML32G_34 = 9 then HML32G_34 = .;
    if HML32G_35 = 9 then HML32G_35 = .;
    if HML32G_36 = 9 then HML32G_36 = .;
    if HML32G_37 = 9 then HML32G_37 = .;
    if HML32G_38 = 9 then HML32G_38 = .;
    if HML32G_39 = 9 then HML32G_39 = .;
    if HML32G_40 = 9 then HML32G_40 = .;
    if HML32G_41 = 9 then HML32G_41 = .;
    if HML32G_42 = 9 then HML32G_42 = .;
    if HML32G_43 = 9 then HML32G_43 = .;
    if HML32G_44 = 9 then HML32G_44 = .;
    if HML32G_45 = 9 then HML32G_45 = .;
    if HML32G_46 = 9 then HML32G_46 = .;
    if HML32G_47 = 9 then HML32G_47 = .;
    if HML32G_48 = 9 then HML32G_48 = .;
    if HML33_01 = 9 then HML33_01 = .;
    if HML33_02 = 9 then HML33_02 = .;
    if HML33_03 = 9 then HML33_03 = .;
    if HML33_04 = 9 then HML33_04 = .;
    if HML33_05 = 9 then HML33_05 = .;
    if HML33_06 = 9 then HML33_06 = .;
    if HML33_07 = 9 then HML33_07 = .;
    if HML33_08 = 9 then HML33_08 = .;
    if HML33_09 = 9 then HML33_09 = .;
    if HML33_10 = 9 then HML33_10 = .;
    if HML33_11 = 9 then HML33_11 = .;
    if HML33_12 = 9 then HML33_12 = .;
    if HML33_13 = 9 then HML33_13 = .;
    if HML33_14 = 9 then HML33_14 = .;
    if HML33_15 = 9 then HML33_15 = .;
    if HML33_16 = 9 then HML33_16 = .;
    if HML33_17 = 9 then HML33_17 = .;
    if HML33_18 = 9 then HML33_18 = .;
    if HML33_19 = 9 then HML33_19 = .;
    if HML33_20 = 9 then HML33_20 = .;
    if HML33_21 = 9 then HML33_21 = .;
    if HML33_22 = 9 then HML33_22 = .;
    if HML33_23 = 9 then HML33_23 = .;
    if HML33_24 = 9 then HML33_24 = .;
    if HML33_25 = 9 then HML33_25 = .;
    if HML33_26 = 9 then HML33_26 = .;
    if HML33_27 = 9 then HML33_27 = .;
    if HML33_28 = 9 then HML33_28 = .;
    if HML33_29 = 9 then HML33_29 = .;
    if HML33_30 = 9 then HML33_30 = .;
    if HML33_31 = 9 then HML33_31 = .;
    if HML33_32 = 9 then HML33_32 = .;
    if HML33_33 = 9 then HML33_33 = .;
    if HML33_34 = 9 then HML33_34 = .;
    if HML33_35 = 9 then HML33_35 = .;
    if HML33_36 = 9 then HML33_36 = .;
    if HML33_37 = 9 then HML33_37 = .;
    if HML33_38 = 9 then HML33_38 = .;
    if HML33_39 = 9 then HML33_39 = .;
    if HML33_40 = 9 then HML33_40 = .;
    if HML33_41 = 9 then HML33_41 = .;
    if HML33_42 = 9 then HML33_42 = .;
    if HML33_43 = 9 then HML33_43 = .;
    if HML33_44 = 9 then HML33_44 = .;
    if HML33_45 = 9 then HML33_45 = .;
    if HML33_46 = 9 then HML33_46 = .;
    if HML33_47 = 9 then HML33_47 = .;
    if HML33_48 = 9 then HML33_48 = .;
    if HML35_01 = 9 then HML35_01 = .;
    if HML35_02 = 9 then HML35_02 = .;
    if HML35_03 = 9 then HML35_03 = .;
    if HML35_04 = 9 then HML35_04 = .;
    if HML35_05 = 9 then HML35_05 = .;
    if HML35_06 = 9 then HML35_06 = .;
    if HML35_07 = 9 then HML35_07 = .;
    if HML35_08 = 9 then HML35_08 = .;
    if HML35_09 = 9 then HML35_09 = .;
    if HML35_10 = 9 then HML35_10 = .;
    if HML35_11 = 9 then HML35_11 = .;
    if HML35_12 = 9 then HML35_12 = .;
    if HML35_13 = 9 then HML35_13 = .;
    if HML35_14 = 9 then HML35_14 = .;
    if HML35_15 = 9 then HML35_15 = .;
    if HML35_16 = 9 then HML35_16 = .;
    if HML35_17 = 9 then HML35_17 = .;
    if HML35_18 = 9 then HML35_18 = .;
    if HML35_19 = 9 then HML35_19 = .;
    if HML35_20 = 9 then HML35_20 = .;
    if HML35_21 = 9 then HML35_21 = .;
    if HML35_22 = 9 then HML35_22 = .;
    if HML35_23 = 9 then HML35_23 = .;
    if HML35_24 = 9 then HML35_24 = .;
    if HML35_25 = 9 then HML35_25 = .;
    if HML35_26 = 9 then HML35_26 = .;
    if HML35_27 = 9 then HML35_27 = .;
    if HML35_28 = 9 then HML35_28 = .;
    if HML35_29 = 9 then HML35_29 = .;
    if HML35_30 = 9 then HML35_30 = .;
    if HML35_31 = 9 then HML35_31 = .;
    if HML35_32 = 9 then HML35_32 = .;
    if HML35_33 = 9 then HML35_33 = .;
    if HML35_34 = 9 then HML35_34 = .;
    if HML35_35 = 9 then HML35_35 = .;
    if HML35_36 = 9 then HML35_36 = .;
    if HML35_37 = 9 then HML35_37 = .;
    if HML35_38 = 9 then HML35_38 = .;
    if HML35_39 = 9 then HML35_39 = .;
    if HML35_40 = 9 then HML35_40 = .;
    if HML35_41 = 9 then HML35_41 = .;
    if HML35_42 = 9 then HML35_42 = .;
    if HML35_43 = 9 then HML35_43 = .;
    if HML35_44 = 9 then HML35_44 = .;
    if HML35_45 = 9 then HML35_45 = .;
    if HML35_46 = 9 then HML35_46 = .;
    if HML35_47 = 9 then HML35_47 = .;
    if HML35_48 = 9 then HML35_48 = .;
Run;
