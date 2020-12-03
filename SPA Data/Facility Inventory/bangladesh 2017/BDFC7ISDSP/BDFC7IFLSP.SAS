libname user 'C:\BDHFS\EXPORT\EXPORTEDFILES';
Proc format;
  value F00001_
     1 = "Barisal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshahi"
     6 = "Rangpur"
     7 = "Sylhet"
     8 = "Mymensingh"
     ;
  value F00002_
     1 = "Bagerhat"
     3 = "Bandarban"
     4 = "Barguna"
     6 = "Barisal"
     9 = "Bhola (M)"
    10 = "Bogra"
    12 = "Brahmanbaria (M)"
    13 = "Chandpur"
    15 = "Chittagong"
    18 = "Chuadandga"
    19 = "Comilla"
    22 = "Cox's Bazar"
    26 = "Dhaka"
    27 = "Dinajpur"
    29 = "Faridpur"
    30 = "Feni"
    32 = "Gaibandha"
    33 = "Gazipur"
    35 = "Gopalganj"
    36 = "Hobigonj (M)"
    38 = "Joypurhat"
    39 = "Jamalpur"
    41 = "Jessore"
    42 = "Jhaolkathi (M)"
    44 = "Jhenaidah"
    46 = "Khagrachari"
    47 = "Khulna"
    48 = "Kishoreganj"
    49 = "Kurigram"
    50 = "Kushtia"
    51 = "Laxmipur (M)"
    52 = "Lalmonirhat"
    54 = "Madaripur"
    55 = "Magura"
    56 = "Manikganj"
    57 = "Meherpur"
    58 = "Moulavibazar"
    59 = "Munshiganj"
    61 = "Mymensingh"
    64 = "Naogaon"
    65 = "Narail"
    67 = "Narayanganj"
    68 = "Narsingdi"
    69 = "Natore"
    70 = "Chapai Nawabganj"
    72 = "Netrokona"
    73 = "Nilphamari"
    75 = "Noakhali (M)"
    76 = "Pabna"
    77 = "Panchagarh"
    78 = "Patuakhali"
    79 = "Pirojpur (M)"
    81 = "Rajshahi"
    82 = "Rajbari"
    84 = "Rangamati"
    85 = "Rangpur"
    86 = "Shariatpur"
    87 = "Satkhira"
    88 = "Sirajganj"
    89 = "Shreepur"
    90 = "Sunamganj"
    91 = "Sylhet"
    93 = "Tangail"
    94 = "Thakurgaon"
     ;
  value F00003_
     1 = "District Hospital (DH)"
     2 = "Upzila Health Complex (UHC)"
     3 = "Mother and Child Welfare Center (MCWC)"
     4 = "Union Health and Family Welfare Center (UnHFWC)"
     5 = "Union Health and Family Welfare Center (UnHFWC - Upgraded)"
     6 = "Union Subcenter (UnSC) / Rural Dispensary"
     8 = "Community Clinic"
     9 = "NGO Clinic (other than Smiling Sun Clinics)"
    10 = "Privtate Hospital (with >20 beds)"
    11 = "NGO Hospital"
    12 = "Smiling Sun Clinic of NGO Health Service Delivery Project (NHSDP)"
     ;
  value F00004_
     1 = "Government/Public (MOHFW)"
     2 = "Local Government"
     3 = "NGO"
     4 = "Private for Profit"
     ;
  value F00005_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00006_
     1 = "Yes"
     2 = "No"
     ;
  value F00007_
     1 = "1st visit"
     2 = "2nd visit"
     3 = "3rd visit"
     4 = "4th visit"
     5 = "5th visit"
     ;
  value F00008_
     5 = "May"
     6 = "June"
     7 = "July"
     8 = "August"
     9 = "Septemeber"
    10 = "October"
    11 = "November"
    12 = "December"
     ;
  value F00009_
     1 = "Begin interview/Facility Completed"
     2 = "Facility respondent not available"
     3 = "Postponed/Partially completed"
     4 = "Facility Refused"
     5 = "Closed/Not yet operational"
     6 = "Other"
     ;
  value F00010_
     1 = "Yes"
     2 = "No"
     ;
  value F00011_
     0 = "no"
     1 = "yes"
     ;
  value F00012_
     0 = "no"
     1 = "yes"
     ;
  value F00013_
     0 = "no"
     1 = "yes"
     ;
  value F00014_
     0 = "no"
     1 = "yes"
     ;
  value F00015_
     0 = "no"
     1 = "yes"
     ;
  value F00016_
     0 = "no"
     1 = "yes"
     ;
  value F00017_
     0 = "no"
     1 = "yes"
     ;
  value F00018_
     0 = "no"
     1 = "yes"
     ;
  value F00019_
     0 = "no"
     1 = "yes"
     ;
  value F00020_
     0 = "no"
     1 = "yes"
     ;
  value F00021_
     0 = "no"
     1 = "yes"
     ;
  value F00022_
     0 = "no"
     1 = "yes"
     ;
  value F00023_
     0 = "no"
     1 = "yes"
     ;
  value F00024_
     0 = "no"
     1 = "yes"
     ;
  value F00025_
     0 = "no"
     1 = "yes"
     ;
  value F00026_
     0 = "no"
     1 = "yes"
     ;
  value F00027_
     0 = "no"
     1 = "yes"
     ;
  value F00028_
     0 = "no"
     1 = "yes"
     ;
  value F00029_
     0 = "no"
     1 = "yes"
     ;
  value F00030_
     0 = "no"
     1 = "yes"
     ;
  value F00031_
     0 = "no"
     1 = "yes"
     ;
  value F00032_
     0 = "no"
     1 = "yes"
     ;
  value F00033_
     0 = "no"
     1 = "yes"
     ;
  value F00034_
     0 = "no"
     1 = "yes"
     ;
  value F00035_
     0 = "no"
     1 = "yes"
     ;
  value F00036_
     0 = "no"
     1 = "yes"
     ;
  value F00037_
     0 = "no"
     1 = "yes"
     ;
  value F00038_
     0 = "no"
     1 = "yes"
     ;
  value F00039_
     0 = "no"
     1 = "yes"
     ;
  value F00040_
     0 = "no"
     1 = "yes"
     ;
  value F00041_
     0 = "no"
     1 = "yes"
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
     1 = "Yes"
     ;
  value F00048_
     0 = "No"
     1 = "Yes"
     ;
  value F00049_
     0 = "No"
     1 = "Yes"
     ;
  value F00050_
     0 = "No"
     1 = "Yes"
     ;
  value F00051_
     0 = "No"
     1 = "Yes"
     ;
  value F00052_
     0 = "No"
     1 = "Yes"
     ;
  value F00053_
     0 = "No"
     1 = "Yes"
     ;
  value F00054_
     0 = "No"
     1 = "Yes"
     ;
  value F00055_
     0 = "No"
     1 = "Yes"
     ;
  value F00056_
     0 = "No"
     1 = "Yes"
     ;
  value F00057_
     0 = "No"
     1 = "Yes"
     ;
  value F00058_
     0 = "No"
     1 = "Yes"
     ;
  value F00059_
     0 = "No"
     1 = "Yes"
     ;
  value F00060_
     0 = "No"
     1 = "Yes"
     ;
  value F00061_
     0 = "No"
     1 = "Yes"
     ;
  value F00062_
     0 = "No"
     1 = "Yes"
     ;
  value F00063_
     0 = "No"
     1 = "Yes"
     ;
  value F00064_
     0 = "No"
     1 = "Yes"
     ;
  value F00065_
     0 = "No"
     1 = "Yes"
     ;
  value F00066_
     0 = "No"
     1 = "Yes"
     ;
  value F00067_
     0 = "No"
     1 = "Yes"
     ;
  value F00068_
     0 = "No"
     1 = "Yes"
     ;
  value F00069_
     0 = "No"
     1 = "Yes"
     ;
  value F00070_
     0 = "No"
     1 = "Yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00079_
     0 = "No"
     1 = "Yes"
     ;
  value F00080_
     0 = "No"
     1 = "Yes"
     ;
  value F00081_
     0 = "No"
     1 = "Yes"
     ;
  value F00082_
     0 = "No"
     1 = "Yes"
     ;
  value F00083_
     0 = "No"
     1 = "Yes"
     ;
  value F00084_
     0 = "no"
     1 = "yes"
     ;
  value F00085_
     0 = "no"
     1 = "yes"
     ;
  value F00086_
     0 = "no"
     1 = "yes"
     ;
  value F00087_
     0 = "No"
     1 = "Yes"
     ;
  value F00088_
     1 = "Yes"
     2 = "No"
     ;
  value F00089_
     1 = "Yes"
     2 = "No"
     ;
  value F00090_
     1 = "Yes"
     2 = "No"
     ;
  value F00091_
   990 = "More than 990"
   998 = "Don't know"
     ;
  value F00092_
   990 = "More than 990"
   998 = "Don't know"
     ;
  value F00093_
   990 = "More than 990"
   998 = "Don't know"
     ;
  value F00094_
     1 = "Yes"
     2 = "No"
     ;
  value F00095_
     1 = "Only in this facility"
     2 = "Both in facility and outside"
     3 = "Only at an outside facility"
     ;
  value F00096_
     1 = "Yes"
     2 = "Facility stocks no medicines"
     ;
  value F00097_
     1 = "Stored in FP service area"
     2 = "Stored with other medicines"
     3 = "FP commodities not stocked"
     ;
  value F00098_
     1 = "Stored in TB service site"
     2 = "Stored with other medicines"
     3 = "TB medicines not stocked"
     ;
  value F00099_
     0 = "Postpone for now, will enter later"
     ;
  value F00100_
     1 = "Yes, 24-Hour Staff"
     2 = "No, 24-Hour Staff"
     ;
  value F00101_
     1 = "Yes"
     2 = "Duty schedule not maintained"
     ;
  value F00102_
     1 = "Schedule observed"
     2 = "Schedule reported not seen"
     ;
  value F00103_
     1 = "Yes"
     2 = "No"
     ;
  value F00104_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00105_
     1 = "Yes"
     2 = "No"
     ;
  value F00106_
     1 = "Yes"
     2 = "No"
     ;
  value F00107_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00108_
     1 = "Yes"
     2 = "No"
     ;
  value F00109_
     1 = "Yes"
     2 = "No"
     ;
  value F00110_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00111_
     1 = "Yes"
     2 = "No"
     ;
  value F00112_
     1 = "Yes"
     2 = "No"
     ;
  value F00113_
     1 = "Yes"
     2 = "No"
     ;
  value F00114_
     1 = "Piped into facility"
     2 = "Piped onto facility grounds"
     3 = "Public tap/ standpipe"
     4 = "Tubewell/ borehole"
     5 = "Protected dug well"
     6 = "Unprotected dug well"
     7 = "Protected spring"
     8 = "Unprotected spring"
     9 = "Rainwater"
    10 = "Bottled water"
    11 = "Cart with small tank/ drum"
    12 = "Tanker truck"
    13 = "Surface water (river/dam/lake/pond"
    96 = "Other"
    98 = "Don't know"
     0 = "No water source"
     ;
  value F00115_
     1 = "Onsite"
     2 = "Within 500 m of facility"
     3 = "Beyond 500 m of facility"
     ;
  value F00116_
     1 = "Yes"
     2 = "No"
     ;
  value F00117_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00118_
     1 = "Always available"
     2 = "Sometimes interrupted"
     8 = "Don't know"
     ;
  value F00119_
     1 = "Yes"
     2 = "No other source"
     ;
  value $F00120_
'A  ' = "Fuel operated generator"
'B  ' = "Battery operated generator"
'C  ' = "Solar system"
'X  ' = "OTHER"
     ;
  value F00121_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00122_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00123_
     1 = "Yes"
     2 = "No routine maintenance"
     8 = "Don't know"
     ;
  value F00124_
     1 = "Onsite staff"
     2 = "Outside support"
     3 = "Both outside and onsite staff"
     8 = "Don't know"
     ;
  value F00125_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00126_
     1 = "Yes"
     2 = "No"
     ;
  value F00127_
     1 = "Within the past 6 months"
     2 = "More than 6 months ago"
     ;
  value F00128_
     1 = "No. of visits"
    98 = "Don't know"
     ;
  value F00129_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00130_
     1 = "Yes, records maintained"
     2 = "No, records not maintained"
     ;
  value F00131_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00132_
     1 = "Yes"
     2 = "No"
     ;
  value F00133_
     1 = "Yes"
     2 = "No"
     ;
  value F00134_
     1 = "Yes"
     2 = "No"
     ;
  value F00135_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00136_
    95 = ">= 95 times"
    98 = "Don't know"
     ;
  value F00137_
     1 = "Yes"
     2 = "No"
     ;
  value F00138_
     1 = "Fixed fee covering all services"
     2 = "No, charge fee for separate items"
     ;
  value F00139_
     1 = "Yes"
     2 = "No"
     ;
  value F00140_
     1 = "Yes"
     2 = "No posted fees"
     ;
  value F00141_
     1 = "Observed, All fees posted"
     2 = "Observed, Some but not all fees posted"
     ;
  value F00142_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00143_
     1 = "Yes"
     2 = "No"
     ;
  value F00144_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value $F00145_
'A           ' = "Ministry of Health"
'B           ' = "Other public ministries"
'C           ' = "Medical schemes (insurance)"
'D           ' = "Social security fund"
'E           ' = "Reimbursement by employer"
'F           ' = "Government contribution to private"
'G           ' = "Donor agencies/ NGOs"
'H           ' = "Faith-based "
'I           ' = "Community programs"
'J           ' = "User fee    "
'X           ' = "Other       "
'Y           ' = "None        "
     ;
  value F00146_
     1 = "Yes"
     2 = "No"
     ;
  value $F00147_
'A          ' = "Suggestion box"
'B          ' = "Client survey form"
'C          ' = "Client interview form"
'D          ' = "Official meeting with community leaders"
'E          ' = "Informal discussion with clients/ community"
'F          ' = "Email      "
'G          ' = "Facility's website"
'H          ' = "Letters from clients/ community"
'I          ' = "Text/SMS messages/calls"
'X          ' = "Other      "
'Z          ' = "Don't know "
     ;
  value F00148_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00149_
     1 = "Yes,"
     2 = "No records not maintained"
     ;
  value F00150_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00151_
     1 = "Yes"
     2 = "No"
     ;
  value F00152_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00153_
     1 = "Yes"
     2 = "No"
     ;
  value F00154_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00155_
     0 = "None"
     ;
  value F00156_
     0 = "None"
     ;
  value F00157_
     0 = "None"
     ;
  value F00158_
     0 = "None"
     ;
  value F00159_
     0 = "None"
     ;
  value F00160_
     0 = "None"
     ;
  value F00161_
     0 = "None"
     ;
  value F00162_
     1 = "Yes"
     2 = "No"
     ;
  value F00163_
     1 = "Yes"
     2 = "No"
     ;
  value F00164_
     1 = "Monthly or more often"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less often than every 6 months"
     ;
  value F00165_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00166_
     1 = "Yes"
     2 = "No dedicated person"
     ;
  value F00167_
     1 = "Data manager/HMIS person/Statistician"
     2 = "Facility in-charge"
     3 = "Other service provider"
     ;
  value F00168_
  9990 = "More than 9990"
  9998 = "Don't know"
     ;
  value F00169_
  9990 = "More than 9990"
  9998 = "Don't know"
     ;
  value F00170_
     1 = "Used Observed"
     2 = "Used Reported not seen"
     3 = "Not used/Not available"
     ;
  value F00171_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00172_
     1 = "Used"
     2 = "Not used"
     ;
  value F00173_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00174_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00175_
     1 = "KG/sq cm"
     2 = "ATM pressure"
     3 = "Kilopascal lb/in2"
     4 = "mm HG"
     8 = "Don't know"
     ;
  value F00176_
   666 = "Automatic"
   995 = "Not used"
   998 = "Don't know"
     ;
  value F00177_
   666 = "Automatic"
   995 = "Not used"
   998 = "Don't know"
     ;
  value F00178_
     1 = "Alcohol"
     2 = "Betadine"
     3 = "Chlorine"
     4 = "Cidex"
     5 = "Formaldehyde"
     6 = "Gluteraldehyde"
     7 = "Lysol"
    98 = "Don't know"
     ;
  value F00179_
     1 = "Yes"
     2 = "No"
     ;
  value F00180_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00181_
     2 = "Incinerator, 2-chamber industrial (800-1000+ C)"
     3 = "Incinerator, 1-chamber drum/ brick"
     4 = "Open burning, Flat gound no protection"
     5 = "Open burning, pit or protected ground"
     6 = "Dump no burning, flat ground, no protection"
     7 = "Dump no burning, covered pit or pit latrine"
     8 = "Dump no burning, open pit, no protection"
     9 = "Dump no burning, protected ground or pit"
    10 = "Remove offsite, stored in covered container"
    11 = "Remove offsite, stored in other protected environment"
    12 = "Remove offsite, stored unprotected"
    95 = "Never have sharps waste"
    96 = "Other"
     ;
  value F00182_
     1 = "Same as for sharps items"
     2 = "Incinerator, 2-chamber industrial  (800-1000+ C)"
     3 = "Incinerator, 1-chamber drum/ brick"
     4 = "Open burning, Flat gound no protection"
     5 = "Open burning, pit or protected ground"
     6 = "Dump no burning, flat ground, no protection"
     7 = "Dump no burning, covered pit or pit latrine"
     8 = "Dump no burning, open pit, no protection"
     9 = "Dump no burning, protected ground or pit"
    10 = "Remove offsite, stored in covered container"
    11 = "Remove offsite, stored in other protected environment"
    12 = "Remove offsite, stored unprotected"
    95 = "Never have other medical waste"
    96 = "Other"
     ;
  value F00183_
     1 = "No waste visible"
     2 = "Waste visible, but protected area"
     3 = "Waste visible, not protected area"
     8 = "Waste site not inspected"
     ;
  value F00184_
     1 = "No waste visible"
     2 = "Waste visible, but protected area"
     3 = "Waste visible, not protected area"
     8 = "Waste site not inspected"
     ;
  value F00185_
     1 = "Incinerator observed"
     2 = "Incinerator reported not seen"
     ;
  value F00186_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00187_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00188_
     1 = "Yes"
     2 = "No guideline available"
     ;
  value F00189_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00190_
    11 = "Flush to piped sewer system"
    12 = "Flush to septic tank"
    13 = "Flush to pit latrine"
    14 = "Flush to somewhere else"
    15 = "Flush don't know where"
    21 = "Ventilated improved pit latrine"
    22 = "Pit latrine with slab"
    23 = "Pit latrine without slab/ open pit"
    31 = "Composting toilet"
    41 = "Bucket toilet"
    51 = "Hanging toilet/ hanging latrine"
    61 = "No functioning facility/bush/field"
     ;
  value $F00191_
'A          ' = "Privacy: Unlocked door when not in use"
'B          ' = "Privacy: Can be locked from inside when use"
'C          ' = "Privacy: Toilet stall have walls without major holes"
'D          ' = "Function: Water available"
'E          ' = "Function: Soap available"
'F          ' = "Function: No crack or leak in the toilet structure"
'G          ' = "Function: Hole or pit is not blocked"
'H          ' = "Cleanliness: Dry"
'I          ' = "Cleanliness: Minimal odor"
'J          ' = "Cleanliness: Clean appearance"
'K          ' = "Cleanliness: Cleaning products visible"
     ;
  value F00192_
     1 = "Yes, seperate toilet for female clients"
     2 = "No seperate toilet for female clients,only combined"
     ;
  value F00193_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00194_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00195_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00196_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00197_
     1 = "Yes"
     2 = "No protected client waiting area"
     ;
  value F00198_
     1 = "Yes"
     2 = "No"
     ;
  value F00199_
     1 = "Yes"
     2 = "No"
     ;
  value F00200_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00201_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00202_
     1 = "Yes"
     2 = "No"
     ;
  value F00203_
     1 = "Yes"
     2 = "No"
     ;
  value F00204_
     1 = "Yes"
     2 = "No"
     ;
  value F00205_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00206_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00207_
     1 = "Yes"
     2 = "No"
     ;
  value F00208_
     1 = "Yes"
     2 = "No"
     ;
  value F00209_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00210_
     1 = "Yes"
     2 = "No"
     ;
  value F00211_
     1 = "Yes"
     2 = "No"
     ;
  value F00212_
     1 = "Yes"
     2 = "No"
     ;
  value F00213_
     1 = "Observed, at least 1 valid"
     2 = "Available, none valid"
     3 = "Reported available, not seen"
     4 = "None available today"
     ;
  value F00214_
     1 = "Yes"
     2 = "No"
     ;
  value F00215_
     1 = "Yes"
     2 = "No"
     ;
  value F00216_
     1 = "Yes"
     2 = "No"
     ;
  value F00217_
     1 = "Yes"
     2 = "No"
     ;
  value F00218_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00219_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00220_
     1 = "Yes"
     2 = "No"
     ;
  value F00221_
     1 = "Yes"
     2 = "No"
     ;
  value F00222_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00223_
     1 = "Yes"
     2 = "No"
     ;
  value F00224_
     1 = "Yes"
     2 = "No"
     ;
  value F00225_
     1 = "Yes"
     2 = "No"
     ;
  value F00226_
     1 = "Yes"
     2 = "No"
     ;
  value F00227_
     1 = "Yes"
     2 = "No"
     ;
  value F00228_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00229_
     1 = "Yes"
     2 = "No"
     ;
  value F00230_
     1 = "Yes"
     2 = "No"
     ;
  value F00231_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     4 = "None available today"
     ;
  value F00232_
     1 = "Yes"
     2 = "No"
     ;
  value F00233_
     1 = "Yes"
     2 = "No"
     ;
  value F00234_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00235_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00236_
     1 = "Yes"
     2 = "No"
     ;
  value F00237_
     1 = "Yes"
     2 = "No"
     ;
  value F00238_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00239_
     1 = "Yes"
     2 = "No"
     ;
  value F00240_
     1 = "Yes"
     2 = "No"
     ;
  value F00241_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00242_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00243_
     1 = "Yes"
     2 = "No"
     ;
  value F00244_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     4 = "Non available today"
     ;
  value F00245_
     1 = "Yes"
     2 = "No"
     ;
  value F00246_
     1 = "Observe"
     3 = "Reported not seen"
     4 = "None available today"
     ;
  value F00247_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00248_
     1 = "Yes"
     2 = "No"
     ;
  value F00249_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00250_
     1 = "Yes"
     2 = "No"
     ;
  value F00251_
     1 = "Internal QC/QA only"
     2 = "External QC/QA only"
     3 = "Both, internal and external QC/QA"
     4 = "Send slide for re-reading"
     6 = "Other"
     ;
  value F00252_
     1 = "Yes"
     2 = "No"
     ;
  value F00253_
     1 = "Records for IQC / IQA only"
     2 = "Records for EQC / EQA only"
     3 = "Records for both QC / QA procedures"
     ;
  value F00254_
     1 = "Yes"
     2 = "No"
     ;
  value F00255_
     1 = "Yes"
     2 = "No"
     ;
  value F00256_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00257_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00258_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00259_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00260_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00261_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00262_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00263_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00264_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00265_
     1 = "Yes"
     2 = "No"
     ;
  value F00266_
     1 = "Yes, all medicines"
     2 = "Yes, only some medicines"
     3 = "No"
     ;
  value F00267_
     1 = "Computer system updated daily"
     2 = "Ledger/ stock card updated daily"
     3 = "Computer system not updated daily, but there is daily record of distributed medicines"
     4 = "Ledger/ stock card not updated daily, but there is a daily record of distributed medicines"
     6 = "Other"
     ;
  value F00268_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00269_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00270_
     1 = "Yes"
     2 = "No"
     ;
  value F00271_
     1 = "Yes, all commodities"
     2 = "Not all commodities"
     3 = "No"
     ;
  value F00272_
     1 = "Computer system updated daily"
     2 = "Ledger/ stock card updated daily"
     3 = "Computer system not updated daily, but there is daily record of distributed commodities"
     4 = "Ledger/ stock card not updated daily, but there is a daily record of distributed commodities"
     6 = "Other system"
     ;
  value F00273_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00274_
     0 = "No service"
     ;
  value F00275_
     0 = "No service"
     ;
  value F00276_
     1 = "Yes"
     2 = "No"
     ;
  value F00277_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00278_
     1 = "Yes"
     2 = "No"
     ;
  value F00279_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00280_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00281_
     1 = "Routinely store vaccines"
     2 = "Stores no vaccines"
     ;
  value F00282_
     1 = "Refrigerator observed"
     2 = "Refrigerator not observed"
     ;
  value F00283_
     1 = "Yes"
     2 = "No"
     ;
  value F00284_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00285_
     1 = "Yes completed"
     2 = "No, not completed"
     ;
  value F00286_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00287_
     1 = "Between +2 and +8 degrees"
     2 = "Above +8 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functional"
     5 = "No Thermometer"
     ;
  value F00288_
     1 = "One set"
     2 = "Two or more sets"
     3 = "None"
     ;
  value F00289_
     1 = "One set"
     2 = "Two or more sets"
     3 = "No ice packs, use purchased ice"
     4 = "No ice packs"
     ;
  value F00290_
     0 = "No service"
     ;
  value F00291_
     0 = "No service"
     ;
  value F00292_
     1 = "Yes"
     2 = "No"
     ;
  value F00293_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00294_
     1 = "Yes"
     2 = "No"
     ;
  value F00295_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00296_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00297_
     0 = "No service"
     ;
  value F00298_
     0 = "No service"
     ;
  value F00299_
     1 = "Yes"
     2 = "No"
     ;
  value F00300_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00301_
     1 = "Yes"
     2 = "No"
     ;
  value F00302_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00303_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00304_
    11 = "GENERAL INFORMATION  [Q710]"
    13 = "CHILD CURATIVE CARE  [Q1251]"
    14 = "FAMILY PLANNING  [Q1351]"
    15 = "ANTENATAL CARE  [Q1451]"
    17 = "DELIVERY  [Q1651]"
    19 = "TUBERCULOSIS  [Q1951]"
    22 = "NCD  [Q2351]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00305_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00306_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00307_
     1 = "Yes"
     2 = "No"
     ;
  value F00308_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00309_
     1 = "Yes"
     2 = "No"
     ;
  value F00310_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00311_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00312_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00313_
     1 = "Yes"
     2 = "No"
     ;
  value F00314_
     1 = "Yes"
     2 = "No"
     ;
  value F00315_
     1 = "Yes"
     2 = "No"
     ;
  value F00316_
     1 = "Yes"
     2 = "No"
     ;
  value F00317_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00318_
     1 = "Yes"
     2 = "No"
     ;
  value F00319_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00320_
     1 = "Yes"
     2 = "No"
     ;
  value F00321_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not routinely conducted"
     8 = "Don't know"
     ;
  value F00322_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00323_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00324_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00325_
     1 = "Yes"
     2 = "No"
     ;
  value F00326_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00327_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1051]"
    14 = "FAMILY PLANNING  [Q1351]"
    15 = "ANTENATAL CARE  [Q1451]"
    17 = "DELIVERY  [Q1651]"
    19 = "TUBERCULOSIS  [Q1951]"
    22 = "NCD  [Q2351]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00328_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00329_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00330_
     1 = "Yes"
     2 = "No"
     ;
  value F00331_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00332_
     0 = "Less than once a week"
     1 = "One day per week"
     2 = "Two days per week"
     3 = "Three days per week"
     4 = "Four days per week"
     5 = "Five days per week"
     6 = "Six days per week"
     7 = "All seven days a week"
     ;
  value F00333_
     1 = "Yes"
     2 = "No"
     ;
  value F00334_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00335_
     1 = "Yes"
     2 = "No"
     ;
  value F00336_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00337_
     1 = "Yes"
     2 = "No"
     ;
  value F00338_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00339_
     0 = "Less than once a week"
     1 = "One day per week"
     2 = "Two days per week"
     3 = "Three days per week"
     4 = "Four days per week"
     5 = "Five days per week"
     6 = "Six days per week"
     7 = "All seven days a week"
     ;
  value F00340_
     1 = "Yes"
     2 = "No"
     ;
  value F00341_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00342_
     1 = "Yes"
     2 = "No"
     ;
  value F00343_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00344_
     1 = "Provide (stock the commodity)"
     2 = "Prescribe/ counsel/ refer"
     3 = "No"
     ;
  value F00345_
     1 = "Yes"
     2 = "No"
     ;
  value F00346_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00347_
     1 = "Yes"
     2 = "No"
     ;
  value F00348_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00349_
     1 = "Yes"
     2 = "No"
     ;
  value F00350_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00351_
     1 = "Yes"
     2 = "No"
     ;
  value F00352_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not routinely done"
     8 = "Don't know"
     ;
  value F00353_
     1 = "Routinely diagnose and treat STIs"
     2 = "Diagnose but refer elsewhere for treatment"
     3 = "Refer elsewhere in facility for diagnosis and treatment"
     4 = "Refer outside facility for diagnosis and treatment"
     5 = "No diagnosis, treatment or referral"
     ;
  value F00354_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00355_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00356_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00357_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00358_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00359_
     1 = "FP service site"
     2 = "Central location in facility"
     3 = "Both locations"
     4 = "No equipment processed in facility"
     ;
  value $F00360_
'A     ' = "Autocave"
'B     ' = "Dry heat sterilization"
'C     ' = "Soak in chlorine solution"
'D     ' = "Boil or steam"
'E     ' = "Wash with soap and water"
'F     ' = "Soak in other chemical solution"
     ;
  value F00361_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1051]"
    13 = "CHILD CURATIVE CARE  [Q1251]"
    15 = "ANTENATAL CARE  [Q1451]"
    17 = "DELIVERY  [Q1651]"
    19 = "TUBERCULOSIS  [Q1951]"
    22 = "NCD  [Q2351]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00362_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00363_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00364_
    11 = "ROOM / AREA WILL BE ASSESSED IN Q1353H (NA)"
    31 = "NOT PREVIOUSLY ASSESSED"
     ;
  value F00365_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00366_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00367_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00368_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00369_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00370_
    11 = "ROOM / AREA ASSESSED IN Q1353C"
    31 = "NOT PREVIOUSLY ASSESSED IN Q1353C"
     ;
  value F00371_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00372_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00373_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00374_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00375_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00376_
     1 = "Yes"
     2 = "No"
     ;
  value F00377_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today"
     5 = "No or Never available"
     6 = "Available elsewhere in facility"
     ;
  value F00378_
     1 = "Routinely diagnose and treat RTIs/STIs"
     2 = "Diagnose but refer elsewhere for treatment"
     3 = "Refer elsewhere in facility for diagnosis and treatment"
     4 = "Refer outside facility for diagnosis and treatment"
     5 = "No diagnosis, treatment or referral"
     ;
  value F00379_
     1 = "Yes"
     2 = "No"
     ;
  value F00380_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00381_
     1 = "Yes"
     2 = "No"
     ;
  value F00382_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00383_
     1 = "Yes"
     2 = "No"
     ;
  value F00384_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00385_
     1 = "Yes"
     2 = "No"
     ;
  value F00386_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00387_
     1 = "Yes"
     2 = "No"
     ;
  value F00388_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Activity not routinely done"
     8 = "Don't know"
     ;
  value F00389_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00390_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00391_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00392_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1051]"
    13 = "CHILD CURATIVE CARE  [Q1251]"
    14 = "FAMILY PLANNING  [Q1351]"
    17 = "DELIVERY  [Q1651]"
    19 = "TUBERCULOSIS  [Q1951]"
    22 = "NCD  [Q2351]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00393_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00394_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00395_
     1 = "Yes"
     2 = "No"
     ;
  value F00396_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00397_
     0 = "Less than once a week"
     1 = "One day per week"
     2 = "Two days per week"
     3 = "Three days per week"
     4 = "Four days per week"
     5 = "Five days per week"
     6 = "Six days per week"
     7 = "All seven days a week"
     ;
  value F00398_
     1 = "Yes"
     2 = "No"
     ;
  value F00399_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00400_
     1 = "Yes"
     2 = "No"
     ;
  value F00401_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00402_
     1 = "Yes"
     2 = "No"
     ;
  value F00403_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00404_
     1 = "Yes"
     2 = "No"
     ;
  value F00405_
     1 = "Yes"
     2 = "No"
     ;
  value F00406_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00407_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00408_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00409_
     1 = "Yes"
     2 = "No"
     ;
  value F00410_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00411_
     1 = "Yes"
     2 = "No"
     ;
  value F00412_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00413_
     1 = "Yes"
     2 = "No"
     ;
  value F00414_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00415_
     1 = "Yes"
     2 = "No"
     ;
  value F00416_
     1 = "Yes, separate room"
     2 = "Yes, integrated"
     ;
  value F00417_
     1 = "Yes"
     2 = "No use of partograph"
     ;
  value F00418_
     1 = "Routinely"
     2 = "Selectively"
     ;
  value F00419_
   998 = "Don't know"
     ;
  value F00420_
   998 = "Don't know"
     ;
  value F00421_
     1 = "Yes"
     2 = "No, does not participate"
     ;
  value F00422_
     1 = "For mothers only"
     2 = "For newborns only"
     3 = "For both mothers and newborns"
     ;
  value F00423_
    53 = "Only when case occurs"
    98 = "Don't know"
     ;
  value F00424_
    53 = "Only when case occurs"
    95 = "Always with maternal reviews"
    98 = "Don't know"
     ;
  value F00425_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00426_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00427_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00428_
     1 = "Yes"
     2 = "No"
     ;
  value F00429_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00430_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00431_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1051]"
    13 = "CHILD CURATIVE CARE  [Q1251]"
    14 = "FAMILY PLANNING  [Q1351]"
    15 = "ANTENATAL CARE  [Q1451]"
    19 = "TUBERCULOSIS  [Q1951]"
    22 = "NCD  [Q2351]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00432_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00433_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00434_
     1 = "Yes"
     2 = "No"
     ;
  value F00435_
     1 = "Sputum smear only"
     2 = "X-Ray only"
     3 = "Either sputum or X-Ray"
     4 = "Both sputum and X-Ray"
     5 = "Clinical symptoms only"
     ;
  value F00436_
     1 = "Yes"
     2 = "No"
     ;
  value F00437_
     1 = "Yes"
     2 = "No"
     ;
  value F00438_
     1 = "Yes"
     2 = "Register not kept"
     ;
  value F00439_
     1 = "Register seen (paper)"
     2 = "Register seen (electronic)"
     3 = "Register reported, not seen"
     ;
  value F00440_
     1 = "Yes"
     2 = "No"
     ;
  value F00441_
     1 = "2M intensive phase, 4M continuation phase"
     2 = "6M intensive phase"
     3 = "Follow up clients only after first 2m intensive observation elsewhere"
     4 = "Diagnose and treat while inpatient, discharge elsewhere for F-up"
     5 = "Provide full treatment, with no routine direct observation phase"
     6 = "Diagnose, prescribe/ provide medicines only, no F/up"
     7 = "Diagnose only, no treatment or prescription of medicine"
     ;
  value F00442_
     1 = "Yes"
     2 = "No system"
     ;
  value F00443_
     1 = "System or register observed"
     2 = "System or register reported, not seen"
     ;
  value F00444_
     1 = "Yes"
     2 = "No"
     ;
  value F00445_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     4 = "None available today"
     ;
  value F00446_
     1 = "Yes"
     2 = "No"
     ;
  value F00447_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00448_
     1 = "Yes"
     2 = "No"
     ;
  value F00449_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00450_
     1 = "Yes"
     2 = "No"
     ;
  value F00451_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00452_
     1 = "Yes"
     2 = "No"
     ;
  value F00453_
     1 = "Observed"
     2 = "Reported available, not seen"
     4 = "None available today"
     ;
  value F00454_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1051]"
    13 = "CHILD CURATIVE CARE  [Q1251]"
    14 = "FAMILY PLANNING  [Q1351]"
    15 = "ANTENATAL CARE  [Q1451]"
    17 = "DELIVERY  [Q1651]"
    22 = "NCD  [Q2351]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00455_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00456_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00457_
     1 = "YES, DIAGNOSE ONLY"
     2 = "YES, TREAT ONLY"
     3 = "YES, DIAGNOSE AND TREAT"
     4 = "NO"
     ;
  value F00458_
     1 = "Yes"
     2 = "No"
     ;
  value F00459_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00460_
     1 = "Yes"
     2 = "No"
     ;
  value F00461_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00462_
     1 = "YES, DIAGNOSE ONLY"
     2 = "YES, TREAT ONLY"
     3 = "YES, DIAGNOSE AND TREAT"
     4 = "NO"
     ;
  value F00463_
     1 = "Yes"
     2 = "No"
     ;
  value F00464_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00465_
     1 = "Yes"
     2 = "No"
     ;
  value F00466_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00467_
     1 = "YES, DIAGNOSE ONLY"
     2 = "YES, TREAT ONLY"
     3 = "YES, DIAGNOSE AND TREAT"
     4 = "NO"
     ;
  value F00468_
     1 = "Yes"
     2 = "No"
     ;
  value F00469_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00470_
     1 = "Yes"
     2 = "No"
     ;
  value F00471_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00472_
     1 = "YES, DIAGNOSE ONLY"
     2 = "YES, TREAT ONLY"
     3 = "YES, DIAGNOSE AND TREAT"
     4 = "NO"
     ;
  value F00473_
     1 = "Yes"
     2 = "No"
     ;
  value F00474_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00475_
     1 = "Yes"
     2 = "No"
     ;
  value F00476_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00477_
     1 = "YES"
     2 = "NO"
     ;
  value F00478_
     1 = "Yes"
     2 = "No"
     ;
  value F00479_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00480_
     1 = "Yes"
     2 = "No"
     ;
  value F00481_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00482_
     1 = "General information section [Q700]"
     2 = "Not previously seen"
     ;
  value F00483_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00484_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00485_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1051]"
    13 = "CHILD CURATIVE CARE  [Q1251]"
    14 = "FAMILY PLANNING  [Q1351]"
    15 = "ANTENATAL CARE  [Q1451]"
    17 = "DELIVERY  [Q1651]"
    19 = "TUBERCULOSIS  [Q1951]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00486_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00487_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00488_
     1 = "Yes"
     2 = "No"
     ;
  value F00489_
     1 = "Yes"
     2 = "24-hr duty schedule not maintained"
     ;
  value F00490_
     1 = "Schedule observed"
     2 = "Schedule reported, not seen"
     ;
  value F00491_
     1 = "Yes"
     2 = "No"
     ;
  value F00492_
     1 = "Yes"
     2 = "24-hr duty schedule not maintained"
     ;
  value F00493_
     1 = "Schedule observed"
     2 = "Schedule reported, not seen"
     ;
  value F00494_
     1 = "Yes"
     2 = "No"
     ;
  value F00495_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00496_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00497_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value $F00498_
'A    ' = "Medical college hospital blood bank"
'B    ' = "District hospital blood bank"
'C    ' = "Blood bank in the facility"
'D    ' = "Relatives donating directly"
'E    ' = "Red crescent"
'X    ' = "Other sources"
     ;
  value F00499_
     1 = "Yes"
     2 = "No"
     ;
  value F00500_
     1 = "Yes"
     2 = "No"
     ;
  value F00501_
     1 = "Only in this facility"
     2 = "Only at an external facility"
     3 = "Both internally and externally"
     ;
  value F00502_
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     4 = "No"
     ;
  value F00503_
     1 = "Yes"
     2 = "No"
     ;
  value F00504_
     1 = "Yes"
     2 = "No"
     ;
  value F00505_
     1 = "Yes"
     2 = "No"
     ;
  value F00506_
     1 = "Yes"
     2 = "No"
     ;
  value F00507_
     1 = "Yes"
     2 = "No"
     ;
  value F00508_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00509_
     1 = "Between +2 and +6 degrees"
     2 = "Above +6 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functional"
     ;
  value F00510_
     1 = "Yes"
     2 = "No"
     ;
  value F00511_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00512_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00513_
     1 = "Yes"
     2 = "No"
     ;
Data user.BDFC7IFLSP;
  attrib FACIL    label="Facility identification";
  attrib REC_TYPE length=$1 label="Record Type (length 1)";
  attrib REGION   format=F00001_. label="Division";
  attrib DISTRICT format=F00002_. label="District (Zila)";
  attrib FACTYPE  format=F00003_. label="Facility type";
  attrib MGA      format=F00004_. label="Ownership (Managing authority)";
  attrib FTYPE    format=F00005_. label="Urban/rural";
  attrib IPD_OPD  format=F00006_. label="Inpatient only";
  attrib NUMVISIT format=F00007_. label="No. of visits";
  attrib DAY      label="Day of interview";
  attrib MONTH    format=F00008_. label="Month of interview";
  attrib YEAR     label="Year of interview";
  attrib INTVNO   label="Interviewer number";
  attrib RESULT   format=F00009_. label="Result of last visit";
  attrib PROVDERS label="Total provider selected for interview";
  attrib Q100     format=F00010_. label="Permission to interview";
  attrib Q101H    label="Start hour";
  attrib Q101M    label="Start minute";
  attrib FACWT    label="Sample weight";
  attrib STEAMF   format=F00011_. label="Steam facility";
  attrib STEAMC   format=F00012_. label="Steam capacity";
  attrib STEAMA   format=F00013_. label="Steam A";
  attrib STEAMB   format=F00014_. label="Steam B";
  attrib STEAMT   format=F00015_. label="Steam timer";
  attrib AUTOF    format=F00016_. label="Autoclave facility";
  attrib AUTOC    format=F00017_. label="Autoclave capacity";
  attrib AUTOWA   format=F00018_. label="AUTOClave wrapped A";
  attrib AUTOWB   format=F00019_. label="AUTOClave wrapped B";
  attrib AUTOUA   format=F00020_. label="AUTOClave unwrapped A";
  attrib AUTOUB   format=F00021_. label="AUTOClave unwrapped B";
  attrib AUTOT    format=F00022_. label="AUTOClave timer";
  attrib PRESSE   format=F00023_. label="EXCellent pressure";
  attrib PRESSG   format=F00024_. label="Good pressure";
  attrib TEMPE    format=F00025_. label="EXCellent TEMPErature";
  attrib TEMPG    format=F00026_. label="Good TEMPErature";
  attrib DHSFAC   format=F00027_. label="Dry heat facility";
  attrib DHSCAP   format=F00028_. label="Dry heat capacity";
  attrib DHSA     format=F00029_. label="Dry heat A";
  attrib DHSB     format=F00030_. label="Dry heat B";
  attrib DHSC     format=F00031_. label="Dry heat C";
  attrib DHST     format=F00032_. label="Dry heat timer";
  attrib HLDFAC   format=F00033_. label="HLD facility";
  attrib HLDCAP   label="HLD capacity";
  attrib HLDA     format=F00034_. label="HLD A";
  attrib HLDB     format=F00035_. label="HLD B";
  attrib HLDC     format=F00036_. label="HLD C";
  attrib HLDT     format=F00037_. label="HLD timer";
  attrib CHEMF    format=F00038_. label="Chemical facilty";
  attrib CHEMA    format=F00039_. label="Chemical A";
  attrib CHEMB    format=F00040_. label="Chemical B";
  attrib CHEMT    format=F00041_. label="Chemical timer";
  attrib SRC      format=F00042_. label="Source for information";
  attrib FACEPI   format=F00043_. label="Facility offers EPI";
  attrib MODTMPFP format=F00044_. label="Facility offers any modern temporary FP";
  attrib STERFP   format=F00045_. label="Facility offers sterilization";
  attrib PAFP     label="Facility offers PA temporary FP";
  attrib LAMFP    label="Facility offers LAM FP";
  attrib FACPMTCT format=F00046_. label="Facility offers PMTCT";
  attrib FACART   format=F00047_. label="Facility offers ART";
  attrib FACCSS   format=F00048_. label="Facility offers HIV care and support services";
  attrib FACMICROSC format=F00049_. label="Facility has functional microscope";
  attrib FACSLIDES format=F00050_. label="Facility has microscope slides";
  attrib FACTUBES format=F00051_. label="Facility has test tubes";
  attrib FACINCUB format=F00052_. label="Facility has functional incubator";
  attrib FACSHAKER format=F00053_. label="Facility has rotater/shaker";
  attrib FACHIVRDT format=F00054_. label="Facility offers HIV RDT";
  attrib FACHIVLAB format=F00055_. label="Facility offers HIV lab test";
  attrib FACHIVINTRNL format=F00056_. label="Facility offers HIV test internally";
  attrib FACHIVEXTRNL format=F00057_. label="Facility offers HIV test external source";
  attrib FACHIVTST format=F00058_. label="Facility offers HIV test (lab or RDT)";
  attrib FACHIVDXTX format=F00059_. label="Facility offers HIV dx/tx";
  attrib FACDBS   format=F00060_. label="Facility offers dried blood spot testing";
  attrib FACTBRDT format=F00061_. label="Facility offers TB RDT";
  attrib FACTBMICRO format=F00062_. label="Facility offers TB microscopy";
  attrib FACTBCULT format=F00063_. label="Facility offers TB culture";
  attrib FACTBXRAY format=F00064_. label="Facility offers TB xray";
  attrib FACTBINTRNL format=F00065_. label="Facility offers TB test internally";
  attrib FACTBEXTRNL format=F00066_. label="Facility offers TB test external source";
  attrib FACTBTST format=F00067_. label="Facility offers any TB test (lab or RDT)";
  attrib FACTBDXTX format=F00068_. label="Facility offers TB dx/tx";
  attrib FACMALRDT format=F00069_. label="Facility offers malaria RDT";
  attrib FACMALLAB format=F00070_. label="Facility offers malaria lab test";
  attrib FACMALTST format=F00071_. label="Facility offers any malaria test (lab or RDT)";
  attrib FACMALDXTX format=F00072_. label="Facility offers malaria dx/tx";
  attrib SHARPWASTE format=F00073_. label="Facility has good sharps waste disposal";
  attrib MEDWASTE format=F00074_. label="Facility has good medical waste disposal";
  attrib STEREQUIP format=F00075_. label="Facility has good sterilization equipment";
  attrib HLDEQUIP format=F00076_. label="Facility has good high level disinfection (HLD) equipment";
  attrib SHARPSTORE format=F00077_. label="Facility has good sharps waste storage";
  attrib MEDSTORE format=F00078_. label="Facility has good medical waste storage";
  attrib ELIGPROV label="Number of eligible providers (codes 1-9, result 1)";
  attrib ELIGTRAIN label="Number of providers with any training last 2 years";
  attrib ELIGSUPERV label="Number of providers supervised last 6 months";
  attrib ELIGBOTH label="Number of providers with supervision & any training last 2 years";
  attrib PROVIMCI label="IMCI training in last 24 months";
  attrib PROVGM   label="Growth management training  in last 24 months";
  attrib PROVEPI  label="EPI training in last 24 months";
  attrib PROVANCT label="ANC training in last 24 months";
  attrib HEMOGLOB format=F00079_. label="Test: Hemoglobin";
  attrib CD4      format=F00080_. label="Test: CD4";
  attrib BLOODGLUC format=F00081_. label="Test: Blood glucose";
  attrib SYPHSEROL format=F00082_. label="Test: Syph serology";
  attrib STOOLMICRO format=F00083_. label="Test: Stool microscopy";
  attrib TOTFP    label="Total de clients dans la liste FP";
  attrib TOTSC    label="Total de clients dans la liste Enfants Malades";
  attrib ANYCAP   format=F00084_. label="Any sterilization/HLD capacity";
  attrib ANYKNOWL format=F00085_. label="Any sterilization/HLD knowledge";
  attrib ANYTIMER format=F00086_. label="Any sterilization/HLD timer";
  attrib FACEPI2  format=F00087_. label="Facility offers EPI (All Vacc DPT, Polio, Measles, BCG and CS if any))";
  attrib Q400V_01 label="No. of vacant positions (Q400AA-Q400A)(1)";
  attrib Q400V_02 label="No. of vacant positions (Q400AA-Q400A)(2)";
  attrib Q400V_03 label="No. of vacant positions (Q400AA-Q400A)(3)";
  attrib Q400V_04 label="No. of vacant positions (Q400AA-Q400A)(4)";
  attrib Q400V_05 label="No. of vacant positions (Q400AA-Q400A)(5)";
  attrib Q400V_06 label="No. of vacant positions (Q400AA-Q400A)(6)";
  attrib Q400V_07 label="No. of vacant positions (Q400AA-Q400A)(7)";
  attrib Q400V_08 label="No. of vacant positions (Q400AA-Q400A)(8)";
  attrib Q400V_09 label="No. of vacant positions (Q400AA-Q400A)(9)";
  attrib Q400V_10 label="No. of vacant positions (Q400AA-Q400A)(10)";
  attrib Q400V_11 label="No. of vacant positions (Q400AA-Q400A)(11)";
  attrib Q400V_12 label="No. of vacant positions (Q400AA-Q400A)(12)";
  attrib Q400V_13 label="No. of vacant positions (Q400AA-Q400A)(13)";
  attrib Q400V_14 label="No. of vacant positions (Q400AA-Q400A)(14)";
  attrib Q400V_15 label="No. of vacant positions (Q400AA-Q400A)(15)";
  attrib Q400V_16 label="No. of vacant positions (Q400AA-Q400A)(16)";
  attrib Q400V_17 label="No. of vacant positions (Q400AA-Q400A)(17)";
  attrib Q400V_18 label="No. of vacant positions (Q400AA-Q400A)(18)";
  attrib Q400V_19 label="No. of vacant positions (Q400AA-Q400A)(19)";
  attrib Q400V_20 label="No. of vacant positions (Q400AA-Q400A)(20)";
  attrib Q400V_21 label="No. of vacant positions (Q400AA-Q400A)(21)";
  attrib Q400V_22 label="No. of vacant positions (Q400AA-Q400A)(22)";
  attrib Q400V_23 label="No. of vacant positions (Q400AA-Q400A)(23)";
  attrib Q400V_24 label="No. of vacant positions (Q400AA-Q400A)(24)";
  attrib Q400V_25 label="No. of vacant positions (Q400AA-Q400A)(25)";
  attrib Q400V_26 label="No. of vacant positions (Q400AA-Q400A)(26)";
  attrib Q400V_27 label="No. of vacant positions (Q400AA-Q400A)(27)";
  attrib Q400V_28 label="No. of vacant positions (Q400AA-Q400A)(28)";
  attrib Q400V_29 label="No. of vacant positions (Q400AA-Q400A)(29)";
  attrib Q400V_30 label="No. of vacant positions (Q400AA-Q400A)(30)";
  attrib Q400V_31 label="No. of vacant positions (Q400AA-Q400A)(31)";
  attrib Q400V_32 label="No. of vacant positions (Q400AA-Q400A)(32)";
  attrib Q400V_33 label="No. of vacant positions (Q400AA-Q400A)(33)";
  attrib Q400V_34 label="No. of vacant positions (Q400AA-Q400A)(34)";
  attrib Q400V_35 label="No. of vacant positions (Q400AA-Q400A)(35)";
  attrib Q400V_36 label="No. of vacant positions (Q400AA-Q400A)(36)";
  attrib Q400V_37 label="No. of vacant positions (Q400AA-Q400A)(37)";
  attrib Q400V_38 label="No. of vacant positions (Q400AA-Q400A)(38)";
  attrib Q400V_39 label="No. of vacant positions (Q400AA-Q400A)(39)";
  attrib Q400V_40 label="No. of vacant positions (Q400AA-Q400A)(40)";
  attrib Q400V_41 label="No. of vacant positions (Q400AA-Q400A)(41)";
  attrib Q400V_42 label="No. of vacant positions (Q400AA-Q400A)(42)";
  attrib Q400V_43 label="No. of vacant positions (Q400AA-Q400A)(43)";
  attrib Q400V_44 label="No. of vacant positions (Q400AA-Q400A)(44)";
  attrib Q400V_45 label="No. of vacant positions (Q400AA-Q400A)(45)";
  attrib Q400V_46 label="No. of vacant positions (Q400AA-Q400A)(46)";
  attrib Q400V_47 label="No. of vacant positions (Q400AA-Q400A)(47)";
  attrib Q400V_48 label="No. of vacant positions (Q400AA-Q400A)(48)";
  attrib Q400V_49 label="No. of vacant positions (Q400AA-Q400A)(49)";
  attrib Q400V_50 label="No. of vacant positions (Q400AA-Q400A)(50)";
  attrib Q400V_51 label="No. of vacant positions (Q400AA-Q400A)(51)";
  attrib Q400V_52 label="No. of vacant positions (Q400AA-Q400A)(52)";
  attrib Q400V_53 label="No. of vacant positions (Q400AA-Q400A)(53)";
  attrib Q400V_54 label="No. of vacant positions (Q400AA-Q400A)(54)";
  attrib Q400V_55 label="No. of vacant positions (Q400AA-Q400A)(55)";
  attrib Q400V_56 label="No. of vacant positions (Q400AA-Q400A)(56)";
  attrib Q400V_57 label="No. of vacant positions (Q400AA-Q400A)(57)";
  attrib Q400V_58 label="No. of vacant positions (Q400AA-Q400A)(58)";
  attrib Q400V_59 label="No. of vacant positions (Q400AA-Q400A)(59)";
  attrib Q400V_60 label="No. of vacant positions (Q400AA-Q400A)(60)";
  attrib Q400V_61 label="No. of vacant positions (Q400AA-Q400A)(61)";
  attrib Q400V_62 label="No. of vacant positions (Q400AA-Q400A)(62)";
  attrib Q400V_63 label="No. of vacant positions (Q400AA-Q400A)(63)";
  attrib Q400V_64 label="No. of vacant positions (Q400AA-Q400A)(64)";
  attrib Q400V_65 label="No. of vacant positions (Q400AA-Q400A)(65)";
  attrib Q400V_66 label="No. of vacant positions (Q400AA-Q400A)(66)";
  attrib Q400V_67 label="No. of vacant positions (Q400AA-Q400A)(67)";
  attrib Q400V_68 label="No. of vacant positions (Q400AA-Q400A)(68)";
  attrib Q400V_69 label="No. of vacant positions (Q400AA-Q400A)(69)";
  attrib Q400V_70 label="No. of vacant positions (Q400AA-Q400A)(70)";
  attrib Q400V_71 label="No. of vacant positions (Q400AA-Q400A)(71)";
  attrib Q400V_72 label="No. of vacant positions (Q400AA-Q400A)(72)";
  attrib Q400V_73 label="No. of vacant positions (Q400AA-Q400A)(73)";
  attrib Q400V_74 label="No. of vacant positions (Q400AA-Q400A)(74)";
  attrib Q400V_75 label="No. of vacant positions (Q400AA-Q400A)(75)";
  attrib Q400V_76 label="No. of vacant positions (Q400AA-Q400A)(76)";
  attrib Q400V_77 label="No. of vacant positions (Q400AA-Q400A)(77)";
  attrib Q400V_78 label="No. of vacant positions (Q400AA-Q400A)(78)";
  attrib Q400V_79 label="No. of vacant positions (Q400AA-Q400A)(79)";
  attrib Q400V_80 label="No. of vacant positions (Q400AA-Q400A)(80)";
  attrib Q400V_81 label="No. of vacant positions (Q400AA-Q400A)(81)";
  attrib Q400V_82 label="No. of vacant positions (Q400AA-Q400A)(82)";
  attrib Q400V_83 label="No. of vacant positions (Q400AA-Q400A)(83)";
  attrib Q400V_84 label="No. of vacant positions (Q400AA-Q400A)(84)";
  attrib Q400V_85 label="No. of vacant positions (Q400AA-Q400A)(85)";
  attrib Q400V_86 label="No. of vacant positions (Q400AA-Q400A)(86)";
  attrib Q400V_87 label="No. of vacant positions (Q400AA-Q400A)(87)";
  attrib Q400V_88 label="No. of vacant positions (Q400AA-Q400A)(88)";
  attrib Q400V_89 label="No. of vacant positions (Q400AA-Q400A)(89)";
  attrib Q400V_90 label="No. of vacant positions (Q400AA-Q400A)(90)";
  attrib Q400V_91 label="No. of vacant positions (Q400AA-Q400A)(91)";
  attrib Q400V_92 label="No. of vacant positions (Q400AA-Q400A)(92)";
  attrib Q400V_93 label="No. of vacant positions (Q400AA-Q400A)(93)";
  attrib Q400V_94 label="No. of vacant positions (Q400AA-Q400A)(94)";
  attrib Q400V_95 label="No. of vacant positions (Q400AA-Q400A)(95)";
  attrib PROVIMCI1 label="IMCI training at any time";
  attrib PROVGM1  label="Growth management training at any time";
  attrib PROVEPI1 label="EPI training at any time";
  attrib PROVANCT1 label="ANC training at any time";
  attrib PRVIMPAC label="Training on IMPAC training in last 24 months";
  attrib PRVIMPAC1 label="Training on IMPAC training at any time";
  attrib PRVLD    label="Training on routine care for labor and delivery in last 24 months";
  attrib PRVLD1   label="Training on routine care for labor and delivery training at any time";
  attrib PRVAMTSL label="Training on active management of child stage of labor (AMTSL)  in last 24 months";
  attrib PRVAMTSL1 label="Training on active management of child stage of labor (AMTSL) at any time";
  attrib PRVEMGOBS label="Training on emergency obstetric care/lifesaving skills in last 24 months";
  attrib PRVEMGOBS1 label="Training on emergency obstetric care/lifesaving skills at any time";
  attrib PRVPAC   label="Training on post-abortion care in last 24 months";
  attrib PRVPAC1  label="Training on post-abortion care at any time";
  attrib PRVNEORES label="Training on neonatal resuscitation in last 24 months";
  attrib PRVNEORES1 label="Training on neonatal resuscitation at any time";
  attrib Q102_01  format=F00088_. label="Services table(1)";
  attrib Q102_02  format=F00088_. label="Services table(2)";
  attrib Q102_03  format=F00088_. label="Services table(3)";
  attrib Q102_04  format=F00088_. label="Services table(4)";
  attrib Q102_05  format=F00088_. label="Services table(5)";
  attrib Q102_06  format=F00088_. label="Services table(6)";
  attrib Q102_07  format=F00088_. label="Services table(7)";
  attrib Q102_08  format=F00088_. label="Services table(8)";
  attrib Q102_09  format=F00088_. label="Services table(9)";
  attrib Q102_10  format=F00088_. label="Services table(10)";
  attrib Q102_11  format=F00088_. label="Services table(11)";
  attrib Q102_12  format=F00088_. label="Services table(12)";
  attrib Q102_13  format=F00088_. label="Services table(13)";
  attrib Q102_14  format=F00088_. label="Services table(14)";
  attrib Q102_15  format=F00088_. label="Services table(15)";
  attrib Q102_16  format=F00088_. label="Services table(16)";
  attrib Q102_17  format=F00088_. label="Services table(17)";
  attrib Q102_18  format=F00088_. label="Services table(18)";
  attrib Q102_19  format=F00088_. label="Services table(19)";
  attrib Q102_20  format=F00088_. label="Services table(20)";
  attrib Q102_21  format=F00088_. label="Services table(21)";
  attrib Q102_22  format=F00088_. label="Services table(22)";
  attrib Q102_23  format=F00088_. label="Services table(23)";
  attrib Q110     format=F00089_. label="Routine in-patient care";
  attrib Q111     format=F00090_. label="Overnight observation";
  attrib Q112     format=F00091_. label="Number of overnight/in-patient beds";
  attrib Q113     format=F00092_. label="Number of total beds according to GOB circular/license";
  attrib Q114     format=F00093_. label="Current number of beds with budget allocation";
  attrib Q200     format=F00094_. label="Processing equipment in facility";
  attrib Q201     format=F00095_. label="Location for processing equipment";
  attrib Q210     format=F00096_. label="Store meds (ARVs, vaccines, contraceptives)";
  attrib Q212     format=F00097_. label="Storage of contraceptive commodities";
  attrib Q214     format=F00098_. label="Storage of anti-TB medicines";
  attrib PROVLNF  format=F00099_. label="Total lines for provider list";
  attrib Q300     format=F00100_. label="24-hour staff available or on call";
  attrib Q301     format=F00101_. label="Duty schedule or call list are maintained";
  attrib Q302     format=F00102_. label="Duty schedule or call list observed";
  attrib Q310     format=F00103_. label="Has landline telephone";
  attrib Q311     format=F00104_. label="Landline telephone observed";
  attrib Q312     format=F00105_. label="Landline telephone functioning";
  attrib Q313     format=F00106_. label="Has cellphone";
  attrib Q314     format=F00107_. label="Cellphone observed";
  attrib Q315     format=F00108_. label="Cellphone functioning";
  attrib Q319     format=F00109_. label="Has computer";
  attrib Q320     format=F00110_. label="Computer observed";
  attrib Q321     format=F00111_. label="Computer functioning";
  attrib Q322     format=F00112_. label="Access to email/internet";
  attrib Q323     format=F00113_. label="Access to internet at least 2 hours on days of client services";
  attrib Q330     format=F00114_. label="Most commonly used source of water";
  attrib Q331     format=F00115_. label="Water onsite";
  attrib Q332     format=F00116_. label="Routine time of year for water shortage";
  attrib Q340     format=F00117_. label="Facility connected to central electrical grid";
  attrib Q341     format=F00118_. label="Availability of electricity";
  attrib Q342     format=F00119_. label="Other sources of electricity such as generator or solar system";
  attrib Q343     length=$3 format=$F00120_. label="Type of other source of electricity";
  attrib Q345     format=F00121_. label="Functional generator";
  attrib Q346     format=F00122_. label="Fuel (or charged battery available";
  attrib Q346A    format=F00123_. label="Routine maintenance of the generator";
  attrib Q346B    format=F00124_. label="Responsible person for maintenance of generator";
  attrib Q346C    format=F00125_. label="Dedicated person for operating the generator";
  attrib Q350     format=F00126_. label="External supervision";
  attrib Q351     format=F00127_. label="Last supervisory visit to facility";
  attrib Q351A    format=F00128_. label="Supervision visits received in last 6 months";
  attrib Q352_1   format=F00129_. label="Activities during supervisory visit(1)";
  attrib Q352_2   format=F00129_. label="Activities during supervisory visit(2)";
  attrib Q352_3   format=F00129_. label="Activities during supervisory visit(3)";
  attrib Q352A    format=F00130_. label="Maintain records of written comments by supervisors";
  attrib Q352B    format=F00131_. label="Comments observed";
  attrib Q352C    format=F00132_. label="Work with CHW";
  attrib Q352D    format=F00133_. label="Facilty supervise CHW";
  attrib Q352E    format=F00134_. label="Monthly community level supervision schedule exist";
  attrib Q352F    format=F00135_. label="Monthly community supervison schedule";
  attrib Q352G    format=F00136_. label="No. of community level supervision visits in last 6 months";
  attrib Q360     format=F00137_. label="Routine user fees";
  attrib Q361     format=F00138_. label="Fixed or separate fees";
  attrib Q362_01  format=F00139_. label="Fee schedule(1)";
  attrib Q362_02  format=F00139_. label="Fee schedule(2)";
  attrib Q362_03  format=F00139_. label="Fee schedule(3)";
  attrib Q362_04  format=F00139_. label="Fee schedule(4)";
  attrib Q362_05  format=F00139_. label="Fee schedule(5)";
  attrib Q362_06  format=F00139_. label="Fee schedule(6)";
  attrib Q362_07  format=F00139_. label="Fee schedule(7)";
  attrib Q362_08  format=F00139_. label="Fee schedule(8)";
  attrib Q362_09  format=F00139_. label="Fee schedule(9)";
  attrib Q362_10  format=F00139_. label="Fee schedule(10)";
  attrib Q362_11  format=F00139_. label="Fee schedule(11)";
  attrib Q362_12  format=F00139_. label="Fee schedule(12)";
  attrib Q362_13  format=F00139_. label="Fee schedule(13)";
  attrib Q362_14  format=F00139_. label="Fee schedule(14)";
  attrib Q362_15  format=F00139_. label="Fee schedule(15)";
  attrib Q362_16  format=F00139_. label="Fee schedule(16)";
  attrib Q362_17  format=F00139_. label="Fee schedule(17)";
  attrib Q362_18  format=F00139_. label="Fee schedule(18)";
  attrib Q362_19  format=F00139_. label="Fee schedule(19)";
  attrib Q363     format=F00140_. label="Fees posted";
  attrib Q364     format=F00141_. label="Fees posted observed";
  attrib Q364A    format=F00142_. label="Ever exempt for user fees";
  attrib Q364B    format=F00143_. label="Guidelines on exemption of user fees";
  attrib Q364C    format=F00144_. label="Observe guidelines on exemption of user fees";
  attrib Q370     length=$12 format=$F00145_. label="Source of revenue";
  attrib Q400SN_01 label="Serial number(1)";
  attrib Q400SN_02 label="Serial number(2)";
  attrib Q400SN_03 label="Serial number(3)";
  attrib Q400SN_04 label="Serial number(4)";
  attrib Q400SN_05 label="Serial number(5)";
  attrib Q400SN_06 label="Serial number(6)";
  attrib Q400SN_07 label="Serial number(7)";
  attrib Q400SN_08 label="Serial number(8)";
  attrib Q400SN_09 label="Serial number(9)";
  attrib Q400SN_10 label="Serial number(10)";
  attrib Q400SN_11 label="Serial number(11)";
  attrib Q400SN_12 label="Serial number(12)";
  attrib Q400SN_13 label="Serial number(13)";
  attrib Q400SN_14 label="Serial number(14)";
  attrib Q400SN_15 label="Serial number(15)";
  attrib Q400SN_16 label="Serial number(16)";
  attrib Q400SN_17 label="Serial number(17)";
  attrib Q400SN_18 label="Serial number(18)";
  attrib Q400SN_19 label="Serial number(19)";
  attrib Q400SN_20 label="Serial number(20)";
  attrib Q400SN_21 label="Serial number(21)";
  attrib Q400SN_22 label="Serial number(22)";
  attrib Q400SN_23 label="Serial number(23)";
  attrib Q400SN_24 label="Serial number(24)";
  attrib Q400SN_25 label="Serial number(25)";
  attrib Q400SN_26 label="Serial number(26)";
  attrib Q400SN_27 label="Serial number(27)";
  attrib Q400SN_28 label="Serial number(28)";
  attrib Q400SN_29 label="Serial number(29)";
  attrib Q400SN_30 label="Serial number(30)";
  attrib Q400SN_31 label="Serial number(31)";
  attrib Q400SN_32 label="Serial number(32)";
  attrib Q400SN_33 label="Serial number(33)";
  attrib Q400SN_34 label="Serial number(34)";
  attrib Q400SN_35 label="Serial number(35)";
  attrib Q400SN_36 label="Serial number(36)";
  attrib Q400SN_37 label="Serial number(37)";
  attrib Q400SN_38 label="Serial number(38)";
  attrib Q400SN_39 label="Serial number(39)";
  attrib Q400SN_40 label="Serial number(40)";
  attrib Q400SN_41 label="Serial number(41)";
  attrib Q400SN_42 label="Serial number(42)";
  attrib Q400SN_43 label="Serial number(43)";
  attrib Q400SN_44 label="Serial number(44)";
  attrib Q400SN_45 label="Serial number(45)";
  attrib Q400SN_46 label="Serial number(46)";
  attrib Q400SN_47 label="Serial number(47)";
  attrib Q400SN_48 label="Serial number(48)";
  attrib Q400SN_49 label="Serial number(49)";
  attrib Q400SN_50 label="Serial number(50)";
  attrib Q400SN_51 label="Serial number(51)";
  attrib Q400SN_52 label="Serial number(52)";
  attrib Q400SN_53 label="Serial number(53)";
  attrib Q400SN_54 label="Serial number(54)";
  attrib Q400SN_55 label="Serial number(55)";
  attrib Q400SN_56 label="Serial number(56)";
  attrib Q400SN_57 label="Serial number(57)";
  attrib Q400SN_58 label="Serial number(58)";
  attrib Q400SN_59 label="Serial number(59)";
  attrib Q400SN_60 label="Serial number(60)";
  attrib Q400SN_61 label="Serial number(61)";
  attrib Q400SN_62 label="Serial number(62)";
  attrib Q400SN_63 label="Serial number(63)";
  attrib Q400SN_64 label="Serial number(64)";
  attrib Q400SN_65 label="Serial number(65)";
  attrib Q400SN_66 label="Serial number(66)";
  attrib Q400SN_67 label="Serial number(67)";
  attrib Q400SN_68 label="Serial number(68)";
  attrib Q400SN_69 label="Serial number(69)";
  attrib Q400SN_70 label="Serial number(70)";
  attrib Q400SN_71 label="Serial number(71)";
  attrib Q400SN_72 label="Serial number(72)";
  attrib Q400SN_73 label="Serial number(73)";
  attrib Q400SN_74 label="Serial number(74)";
  attrib Q400SN_75 label="Serial number(75)";
  attrib Q400SN_76 label="Serial number(76)";
  attrib Q400SN_77 label="Serial number(77)";
  attrib Q400SN_78 label="Serial number(78)";
  attrib Q400SN_79 label="Serial number(79)";
  attrib Q400SN_80 label="Serial number(80)";
  attrib Q400SN_81 label="Serial number(81)";
  attrib Q400SN_82 label="Serial number(82)";
  attrib Q400SN_83 label="Serial number(83)";
  attrib Q400SN_84 label="Serial number(84)";
  attrib Q400SN_85 label="Serial number(85)";
  attrib Q400SN_86 label="Serial number(86)";
  attrib Q400SN_87 label="Serial number(87)";
  attrib Q400SN_88 label="Serial number(88)";
  attrib Q400SN_89 label="Serial number(89)";
  attrib Q400SN_90 label="Serial number(90)";
  attrib Q400SN_91 label="Serial number(91)";
  attrib Q400SN_92 label="Serial number(92)";
  attrib Q400SN_93 label="Serial number(93)";
  attrib Q400SN_94 label="Serial number(94)";
  attrib Q400SN_95 label="Serial number(95)";
  attrib Q400SN_96 label="Serial number(96)";
  attrib Q400SN_97 label="Serial number(97)";
  attrib Q400SN_98 label="Serial number(98)";
  attrib Q400SN_99 label="Serial number(99)";
  attrib Q400AA_01 label="a. No. of Sanctioned (expected) - CS(1)";
  attrib Q400AA_02 label="a. No. of Sanctioned (expected) - CS(2)";
  attrib Q400AA_03 label="a. No. of Sanctioned (expected) - CS(3)";
  attrib Q400AA_04 label="a. No. of Sanctioned (expected) - CS(4)";
  attrib Q400AA_05 label="a. No. of Sanctioned (expected) - CS(5)";
  attrib Q400AA_06 label="a. No. of Sanctioned (expected) - CS(6)";
  attrib Q400AA_07 label="a. No. of Sanctioned (expected) - CS(7)";
  attrib Q400AA_08 label="a. No. of Sanctioned (expected) - CS(8)";
  attrib Q400AA_09 label="a. No. of Sanctioned (expected) - CS(9)";
  attrib Q400AA_10 label="a. No. of Sanctioned (expected) - CS(10)";
  attrib Q400AA_11 label="a. No. of Sanctioned (expected) - CS(11)";
  attrib Q400AA_12 label="a. No. of Sanctioned (expected) - CS(12)";
  attrib Q400AA_13 label="a. No. of Sanctioned (expected) - CS(13)";
  attrib Q400AA_14 label="a. No. of Sanctioned (expected) - CS(14)";
  attrib Q400AA_15 label="a. No. of Sanctioned (expected) - CS(15)";
  attrib Q400AA_16 label="a. No. of Sanctioned (expected) - CS(16)";
  attrib Q400AA_17 label="a. No. of Sanctioned (expected) - CS(17)";
  attrib Q400AA_18 label="a. No. of Sanctioned (expected) - CS(18)";
  attrib Q400AA_19 label="a. No. of Sanctioned (expected) - CS(19)";
  attrib Q400AA_20 label="a. No. of Sanctioned (expected) - CS(20)";
  attrib Q400AA_21 label="a. No. of Sanctioned (expected) - CS(21)";
  attrib Q400AA_22 label="a. No. of Sanctioned (expected) - CS(22)";
  attrib Q400AA_23 label="a. No. of Sanctioned (expected) - CS(23)";
  attrib Q400AA_24 label="a. No. of Sanctioned (expected) - CS(24)";
  attrib Q400AA_25 label="a. No. of Sanctioned (expected) - CS(25)";
  attrib Q400AA_26 label="a. No. of Sanctioned (expected) - CS(26)";
  attrib Q400AA_27 label="a. No. of Sanctioned (expected) - CS(27)";
  attrib Q400AA_28 label="a. No. of Sanctioned (expected) - CS(28)";
  attrib Q400AA_29 label="a. No. of Sanctioned (expected) - CS(29)";
  attrib Q400AA_30 label="a. No. of Sanctioned (expected) - CS(30)";
  attrib Q400AA_31 label="a. No. of Sanctioned (expected) - CS(31)";
  attrib Q400AA_32 label="a. No. of Sanctioned (expected) - CS(32)";
  attrib Q400AA_33 label="a. No. of Sanctioned (expected) - CS(33)";
  attrib Q400AA_34 label="a. No. of Sanctioned (expected) - CS(34)";
  attrib Q400AA_35 label="a. No. of Sanctioned (expected) - CS(35)";
  attrib Q400AA_36 label="a. No. of Sanctioned (expected) - CS(36)";
  attrib Q400AA_37 label="a. No. of Sanctioned (expected) - CS(37)";
  attrib Q400AA_38 label="a. No. of Sanctioned (expected) - CS(38)";
  attrib Q400AA_39 label="a. No. of Sanctioned (expected) - CS(39)";
  attrib Q400AA_40 label="a. No. of Sanctioned (expected) - CS(40)";
  attrib Q400AA_41 label="a. No. of Sanctioned (expected) - CS(41)";
  attrib Q400AA_42 label="a. No. of Sanctioned (expected) - CS(42)";
  attrib Q400AA_43 label="a. No. of Sanctioned (expected) - CS(43)";
  attrib Q400AA_44 label="a. No. of Sanctioned (expected) - CS(44)";
  attrib Q400AA_45 label="a. No. of Sanctioned (expected) - CS(45)";
  attrib Q400AA_46 label="a. No. of Sanctioned (expected) - CS(46)";
  attrib Q400AA_47 label="a. No. of Sanctioned (expected) - CS(47)";
  attrib Q400AA_48 label="a. No. of Sanctioned (expected) - CS(48)";
  attrib Q400AA_49 label="a. No. of Sanctioned (expected) - CS(49)";
  attrib Q400AA_50 label="a. No. of Sanctioned (expected) - CS(50)";
  attrib Q400AA_51 label="a. No. of Sanctioned (expected) - CS(51)";
  attrib Q400AA_52 label="a. No. of Sanctioned (expected) - CS(52)";
  attrib Q400AA_53 label="a. No. of Sanctioned (expected) - CS(53)";
  attrib Q400AA_54 label="a. No. of Sanctioned (expected) - CS(54)";
  attrib Q400AA_55 label="a. No. of Sanctioned (expected) - CS(55)";
  attrib Q400AA_56 label="a. No. of Sanctioned (expected) - CS(56)";
  attrib Q400AA_57 label="a. No. of Sanctioned (expected) - CS(57)";
  attrib Q400AA_58 label="a. No. of Sanctioned (expected) - CS(58)";
  attrib Q400AA_59 label="a. No. of Sanctioned (expected) - CS(59)";
  attrib Q400AA_60 label="a. No. of Sanctioned (expected) - CS(60)";
  attrib Q400AA_61 label="a. No. of Sanctioned (expected) - CS(61)";
  attrib Q400AA_62 label="a. No. of Sanctioned (expected) - CS(62)";
  attrib Q400AA_63 label="a. No. of Sanctioned (expected) - CS(63)";
  attrib Q400AA_64 label="a. No. of Sanctioned (expected) - CS(64)";
  attrib Q400AA_65 label="a. No. of Sanctioned (expected) - CS(65)";
  attrib Q400AA_66 label="a. No. of Sanctioned (expected) - CS(66)";
  attrib Q400AA_67 label="a. No. of Sanctioned (expected) - CS(67)";
  attrib Q400AA_68 label="a. No. of Sanctioned (expected) - CS(68)";
  attrib Q400AA_69 label="a. No. of Sanctioned (expected) - CS(69)";
  attrib Q400AA_70 label="a. No. of Sanctioned (expected) - CS(70)";
  attrib Q400AA_71 label="a. No. of Sanctioned (expected) - CS(71)";
  attrib Q400AA_72 label="a. No. of Sanctioned (expected) - CS(72)";
  attrib Q400AA_73 label="a. No. of Sanctioned (expected) - CS(73)";
  attrib Q400AA_74 label="a. No. of Sanctioned (expected) - CS(74)";
  attrib Q400AA_75 label="a. No. of Sanctioned (expected) - CS(75)";
  attrib Q400AA_76 label="a. No. of Sanctioned (expected) - CS(76)";
  attrib Q400AA_77 label="a. No. of Sanctioned (expected) - CS(77)";
  attrib Q400AA_78 label="a. No. of Sanctioned (expected) - CS(78)";
  attrib Q400AA_79 label="a. No. of Sanctioned (expected) - CS(79)";
  attrib Q400AA_80 label="a. No. of Sanctioned (expected) - CS(80)";
  attrib Q400AA_81 label="a. No. of Sanctioned (expected) - CS(81)";
  attrib Q400AA_82 label="a. No. of Sanctioned (expected) - CS(82)";
  attrib Q400AA_83 label="a. No. of Sanctioned (expected) - CS(83)";
  attrib Q400AA_84 label="a. No. of Sanctioned (expected) - CS(84)";
  attrib Q400AA_85 label="a. No. of Sanctioned (expected) - CS(85)";
  attrib Q400AA_86 label="a. No. of Sanctioned (expected) - CS(86)";
  attrib Q400AA_87 label="a. No. of Sanctioned (expected) - CS(87)";
  attrib Q400AA_88 label="a. No. of Sanctioned (expected) - CS(88)";
  attrib Q400AA_89 label="a. No. of Sanctioned (expected) - CS(89)";
  attrib Q400AA_90 label="a. No. of Sanctioned (expected) - CS(90)";
  attrib Q400AA_91 label="a. No. of Sanctioned (expected) - CS(91)";
  attrib Q400AA_92 label="a. No. of Sanctioned (expected) - CS(92)";
  attrib Q400AA_93 label="a. No. of Sanctioned (expected) - CS(93)";
  attrib Q400AA_94 label="a. No. of Sanctioned (expected) - CS(94)";
  attrib Q400AA_95 label="a. No. of Sanctioned (expected) - CS(95)";
  attrib Q400AA_96 label="a. No. of Sanctioned (expected) - CS(96)";
  attrib Q400AA_97 label="a. No. of Sanctioned (expected) - CS(97)";
  attrib Q400AA_98 label="a. No. of Sanctioned (expected) - CS(98)";
  attrib Q400AA_99 label="a. No. of Sanctioned (expected) - CS(99)";
  attrib Q400A_01 label="b1. No. of posted staff(1)";
  attrib Q400A_02 label="b1. No. of posted staff(2)";
  attrib Q400A_03 label="b1. No. of posted staff(3)";
  attrib Q400A_04 label="b1. No. of posted staff(4)";
  attrib Q400A_05 label="b1. No. of posted staff(5)";
  attrib Q400A_06 label="b1. No. of posted staff(6)";
  attrib Q400A_07 label="b1. No. of posted staff(7)";
  attrib Q400A_08 label="b1. No. of posted staff(8)";
  attrib Q400A_09 label="b1. No. of posted staff(9)";
  attrib Q400A_10 label="b1. No. of posted staff(10)";
  attrib Q400A_11 label="b1. No. of posted staff(11)";
  attrib Q400A_12 label="b1. No. of posted staff(12)";
  attrib Q400A_13 label="b1. No. of posted staff(13)";
  attrib Q400A_14 label="b1. No. of posted staff(14)";
  attrib Q400A_15 label="b1. No. of posted staff(15)";
  attrib Q400A_16 label="b1. No. of posted staff(16)";
  attrib Q400A_17 label="b1. No. of posted staff(17)";
  attrib Q400A_18 label="b1. No. of posted staff(18)";
  attrib Q400A_19 label="b1. No. of posted staff(19)";
  attrib Q400A_20 label="b1. No. of posted staff(20)";
  attrib Q400A_21 label="b1. No. of posted staff(21)";
  attrib Q400A_22 label="b1. No. of posted staff(22)";
  attrib Q400A_23 label="b1. No. of posted staff(23)";
  attrib Q400A_24 label="b1. No. of posted staff(24)";
  attrib Q400A_25 label="b1. No. of posted staff(25)";
  attrib Q400A_26 label="b1. No. of posted staff(26)";
  attrib Q400A_27 label="b1. No. of posted staff(27)";
  attrib Q400A_28 label="b1. No. of posted staff(28)";
  attrib Q400A_29 label="b1. No. of posted staff(29)";
  attrib Q400A_30 label="b1. No. of posted staff(30)";
  attrib Q400A_31 label="b1. No. of posted staff(31)";
  attrib Q400A_32 label="b1. No. of posted staff(32)";
  attrib Q400A_33 label="b1. No. of posted staff(33)";
  attrib Q400A_34 label="b1. No. of posted staff(34)";
  attrib Q400A_35 label="b1. No. of posted staff(35)";
  attrib Q400A_36 label="b1. No. of posted staff(36)";
  attrib Q400A_37 label="b1. No. of posted staff(37)";
  attrib Q400A_38 label="b1. No. of posted staff(38)";
  attrib Q400A_39 label="b1. No. of posted staff(39)";
  attrib Q400A_40 label="b1. No. of posted staff(40)";
  attrib Q400A_41 label="b1. No. of posted staff(41)";
  attrib Q400A_42 label="b1. No. of posted staff(42)";
  attrib Q400A_43 label="b1. No. of posted staff(43)";
  attrib Q400A_44 label="b1. No. of posted staff(44)";
  attrib Q400A_45 label="b1. No. of posted staff(45)";
  attrib Q400A_46 label="b1. No. of posted staff(46)";
  attrib Q400A_47 label="b1. No. of posted staff(47)";
  attrib Q400A_48 label="b1. No. of posted staff(48)";
  attrib Q400A_49 label="b1. No. of posted staff(49)";
  attrib Q400A_50 label="b1. No. of posted staff(50)";
  attrib Q400A_51 label="b1. No. of posted staff(51)";
  attrib Q400A_52 label="b1. No. of posted staff(52)";
  attrib Q400A_53 label="b1. No. of posted staff(53)";
  attrib Q400A_54 label="b1. No. of posted staff(54)";
  attrib Q400A_55 label="b1. No. of posted staff(55)";
  attrib Q400A_56 label="b1. No. of posted staff(56)";
  attrib Q400A_57 label="b1. No. of posted staff(57)";
  attrib Q400A_58 label="b1. No. of posted staff(58)";
  attrib Q400A_59 label="b1. No. of posted staff(59)";
  attrib Q400A_60 label="b1. No. of posted staff(60)";
  attrib Q400A_61 label="b1. No. of posted staff(61)";
  attrib Q400A_62 label="b1. No. of posted staff(62)";
  attrib Q400A_63 label="b1. No. of posted staff(63)";
  attrib Q400A_64 label="b1. No. of posted staff(64)";
  attrib Q400A_65 label="b1. No. of posted staff(65)";
  attrib Q400A_66 label="b1. No. of posted staff(66)";
  attrib Q400A_67 label="b1. No. of posted staff(67)";
  attrib Q400A_68 label="b1. No. of posted staff(68)";
  attrib Q400A_69 label="b1. No. of posted staff(69)";
  attrib Q400A_70 label="b1. No. of posted staff(70)";
  attrib Q400A_71 label="b1. No. of posted staff(71)";
  attrib Q400A_72 label="b1. No. of posted staff(72)";
  attrib Q400A_73 label="b1. No. of posted staff(73)";
  attrib Q400A_74 label="b1. No. of posted staff(74)";
  attrib Q400A_75 label="b1. No. of posted staff(75)";
  attrib Q400A_76 label="b1. No. of posted staff(76)";
  attrib Q400A_77 label="b1. No. of posted staff(77)";
  attrib Q400A_78 label="b1. No. of posted staff(78)";
  attrib Q400A_79 label="b1. No. of posted staff(79)";
  attrib Q400A_80 label="b1. No. of posted staff(80)";
  attrib Q400A_81 label="b1. No. of posted staff(81)";
  attrib Q400A_82 label="b1. No. of posted staff(82)";
  attrib Q400A_83 label="b1. No. of posted staff(83)";
  attrib Q400A_84 label="b1. No. of posted staff(84)";
  attrib Q400A_85 label="b1. No. of posted staff(85)";
  attrib Q400A_86 label="b1. No. of posted staff(86)";
  attrib Q400A_87 label="b1. No. of posted staff(87)";
  attrib Q400A_88 label="b1. No. of posted staff(88)";
  attrib Q400A_89 label="b1. No. of posted staff(89)";
  attrib Q400A_90 label="b1. No. of posted staff(90)";
  attrib Q400A_91 label="b1. No. of posted staff(91)";
  attrib Q400A_92 label="b1. No. of posted staff(92)";
  attrib Q400A_93 label="b1. No. of posted staff(93)";
  attrib Q400A_94 label="b1. No. of posted staff(94)";
  attrib Q400A_95 label="b1. No. of posted staff(95)";
  attrib Q400A_96 label="b1. No. of posted staff(96)";
  attrib Q400A_97 label="b1. No. of posted staff(97)";
  attrib Q400A_98 label="b1. No. of posted staff(98)";
  attrib Q400A_99 label="b1. No. of posted staff(99)";
  attrib Q400B2_01 label="b2. No. of deputed in/ attachment - CS(1)";
  attrib Q400B2_02 label="b2. No. of deputed in/ attachment - CS(2)";
  attrib Q400B2_03 label="b2. No. of deputed in/ attachment - CS(3)";
  attrib Q400B2_04 label="b2. No. of deputed in/ attachment - CS(4)";
  attrib Q400B2_05 label="b2. No. of deputed in/ attachment - CS(5)";
  attrib Q400B2_06 label="b2. No. of deputed in/ attachment - CS(6)";
  attrib Q400B2_07 label="b2. No. of deputed in/ attachment - CS(7)";
  attrib Q400B2_08 label="b2. No. of deputed in/ attachment - CS(8)";
  attrib Q400B2_09 label="b2. No. of deputed in/ attachment - CS(9)";
  attrib Q400B2_10 label="b2. No. of deputed in/ attachment - CS(10)";
  attrib Q400B2_11 label="b2. No. of deputed in/ attachment - CS(11)";
  attrib Q400B2_12 label="b2. No. of deputed in/ attachment - CS(12)";
  attrib Q400B2_13 label="b2. No. of deputed in/ attachment - CS(13)";
  attrib Q400B2_14 label="b2. No. of deputed in/ attachment - CS(14)";
  attrib Q400B2_15 label="b2. No. of deputed in/ attachment - CS(15)";
  attrib Q400B2_16 label="b2. No. of deputed in/ attachment - CS(16)";
  attrib Q400B2_17 label="b2. No. of deputed in/ attachment - CS(17)";
  attrib Q400B2_18 label="b2. No. of deputed in/ attachment - CS(18)";
  attrib Q400B2_19 label="b2. No. of deputed in/ attachment - CS(19)";
  attrib Q400B2_20 label="b2. No. of deputed in/ attachment - CS(20)";
  attrib Q400B2_21 label="b2. No. of deputed in/ attachment - CS(21)";
  attrib Q400B2_22 label="b2. No. of deputed in/ attachment - CS(22)";
  attrib Q400B2_23 label="b2. No. of deputed in/ attachment - CS(23)";
  attrib Q400B2_24 label="b2. No. of deputed in/ attachment - CS(24)";
  attrib Q400B2_25 label="b2. No. of deputed in/ attachment - CS(25)";
  attrib Q400B2_26 label="b2. No. of deputed in/ attachment - CS(26)";
  attrib Q400B2_27 label="b2. No. of deputed in/ attachment - CS(27)";
  attrib Q400B2_28 label="b2. No. of deputed in/ attachment - CS(28)";
  attrib Q400B2_29 label="b2. No. of deputed in/ attachment - CS(29)";
  attrib Q400B2_30 label="b2. No. of deputed in/ attachment - CS(30)";
  attrib Q400B2_31 label="b2. No. of deputed in/ attachment - CS(31)";
  attrib Q400B2_32 label="b2. No. of deputed in/ attachment - CS(32)";
  attrib Q400B2_33 label="b2. No. of deputed in/ attachment - CS(33)";
  attrib Q400B2_34 label="b2. No. of deputed in/ attachment - CS(34)";
  attrib Q400B2_35 label="b2. No. of deputed in/ attachment - CS(35)";
  attrib Q400B2_36 label="b2. No. of deputed in/ attachment - CS(36)";
  attrib Q400B2_37 label="b2. No. of deputed in/ attachment - CS(37)";
  attrib Q400B2_38 label="b2. No. of deputed in/ attachment - CS(38)";
  attrib Q400B2_39 label="b2. No. of deputed in/ attachment - CS(39)";
  attrib Q400B2_40 label="b2. No. of deputed in/ attachment - CS(40)";
  attrib Q400B2_41 label="b2. No. of deputed in/ attachment - CS(41)";
  attrib Q400B2_42 label="b2. No. of deputed in/ attachment - CS(42)";
  attrib Q400B2_43 label="b2. No. of deputed in/ attachment - CS(43)";
  attrib Q400B2_44 label="b2. No. of deputed in/ attachment - CS(44)";
  attrib Q400B2_45 label="b2. No. of deputed in/ attachment - CS(45)";
  attrib Q400B2_46 label="b2. No. of deputed in/ attachment - CS(46)";
  attrib Q400B2_47 label="b2. No. of deputed in/ attachment - CS(47)";
  attrib Q400B2_48 label="b2. No. of deputed in/ attachment - CS(48)";
  attrib Q400B2_49 label="b2. No. of deputed in/ attachment - CS(49)";
  attrib Q400B2_50 label="b2. No. of deputed in/ attachment - CS(50)";
  attrib Q400B2_51 label="b2. No. of deputed in/ attachment - CS(51)";
  attrib Q400B2_52 label="b2. No. of deputed in/ attachment - CS(52)";
  attrib Q400B2_53 label="b2. No. of deputed in/ attachment - CS(53)";
  attrib Q400B2_54 label="b2. No. of deputed in/ attachment - CS(54)";
  attrib Q400B2_55 label="b2. No. of deputed in/ attachment - CS(55)";
  attrib Q400B2_56 label="b2. No. of deputed in/ attachment - CS(56)";
  attrib Q400B2_57 label="b2. No. of deputed in/ attachment - CS(57)";
  attrib Q400B2_58 label="b2. No. of deputed in/ attachment - CS(58)";
  attrib Q400B2_59 label="b2. No. of deputed in/ attachment - CS(59)";
  attrib Q400B2_60 label="b2. No. of deputed in/ attachment - CS(60)";
  attrib Q400B2_61 label="b2. No. of deputed in/ attachment - CS(61)";
  attrib Q400B2_62 label="b2. No. of deputed in/ attachment - CS(62)";
  attrib Q400B2_63 label="b2. No. of deputed in/ attachment - CS(63)";
  attrib Q400B2_64 label="b2. No. of deputed in/ attachment - CS(64)";
  attrib Q400B2_65 label="b2. No. of deputed in/ attachment - CS(65)";
  attrib Q400B2_66 label="b2. No. of deputed in/ attachment - CS(66)";
  attrib Q400B2_67 label="b2. No. of deputed in/ attachment - CS(67)";
  attrib Q400B2_68 label="b2. No. of deputed in/ attachment - CS(68)";
  attrib Q400B2_69 label="b2. No. of deputed in/ attachment - CS(69)";
  attrib Q400B2_70 label="b2. No. of deputed in/ attachment - CS(70)";
  attrib Q400B2_71 label="b2. No. of deputed in/ attachment - CS(71)";
  attrib Q400B2_72 label="b2. No. of deputed in/ attachment - CS(72)";
  attrib Q400B2_73 label="b2. No. of deputed in/ attachment - CS(73)";
  attrib Q400B2_74 label="b2. No. of deputed in/ attachment - CS(74)";
  attrib Q400B2_75 label="b2. No. of deputed in/ attachment - CS(75)";
  attrib Q400B2_76 label="b2. No. of deputed in/ attachment - CS(76)";
  attrib Q400B2_77 label="b2. No. of deputed in/ attachment - CS(77)";
  attrib Q400B2_78 label="b2. No. of deputed in/ attachment - CS(78)";
  attrib Q400B2_79 label="b2. No. of deputed in/ attachment - CS(79)";
  attrib Q400B2_80 label="b2. No. of deputed in/ attachment - CS(80)";
  attrib Q400B2_81 label="b2. No. of deputed in/ attachment - CS(81)";
  attrib Q400B2_82 label="b2. No. of deputed in/ attachment - CS(82)";
  attrib Q400B2_83 label="b2. No. of deputed in/ attachment - CS(83)";
  attrib Q400B2_84 label="b2. No. of deputed in/ attachment - CS(84)";
  attrib Q400B2_85 label="b2. No. of deputed in/ attachment - CS(85)";
  attrib Q400B2_86 label="b2. No. of deputed in/ attachment - CS(86)";
  attrib Q400B2_87 label="b2. No. of deputed in/ attachment - CS(87)";
  attrib Q400B2_88 label="b2. No. of deputed in/ attachment - CS(88)";
  attrib Q400B2_89 label="b2. No. of deputed in/ attachment - CS(89)";
  attrib Q400B2_90 label="b2. No. of deputed in/ attachment - CS(90)";
  attrib Q400B2_91 label="b2. No. of deputed in/ attachment - CS(91)";
  attrib Q400B2_92 label="b2. No. of deputed in/ attachment - CS(92)";
  attrib Q400B2_93 label="b2. No. of deputed in/ attachment - CS(93)";
  attrib Q400B2_94 label="b2. No. of deputed in/ attachment - CS(94)";
  attrib Q400B2_95 label="b2. No. of deputed in/ attachment - CS(95)";
  attrib Q400B2_96 label="b2. No. of deputed in/ attachment - CS(96)";
  attrib Q400B2_97 label="b2. No. of deputed in/ attachment - CS(97)";
  attrib Q400B2_98 label="b2. No. of deputed in/ attachment - CS(98)";
  attrib Q400B2_99 label="b2. No. of deputed in/ attachment - CS(99)";
  attrib Q400B3_01 label="b3. No. of deputed out - CS(1)";
  attrib Q400B3_02 label="b3. No. of deputed out - CS(2)";
  attrib Q400B3_03 label="b3. No. of deputed out - CS(3)";
  attrib Q400B3_04 label="b3. No. of deputed out - CS(4)";
  attrib Q400B3_05 label="b3. No. of deputed out - CS(5)";
  attrib Q400B3_06 label="b3. No. of deputed out - CS(6)";
  attrib Q400B3_07 label="b3. No. of deputed out - CS(7)";
  attrib Q400B3_08 label="b3. No. of deputed out - CS(8)";
  attrib Q400B3_09 label="b3. No. of deputed out - CS(9)";
  attrib Q400B3_10 label="b3. No. of deputed out - CS(10)";
  attrib Q400B3_11 label="b3. No. of deputed out - CS(11)";
  attrib Q400B3_12 label="b3. No. of deputed out - CS(12)";
  attrib Q400B3_13 label="b3. No. of deputed out - CS(13)";
  attrib Q400B3_14 label="b3. No. of deputed out - CS(14)";
  attrib Q400B3_15 label="b3. No. of deputed out - CS(15)";
  attrib Q400B3_16 label="b3. No. of deputed out - CS(16)";
  attrib Q400B3_17 label="b3. No. of deputed out - CS(17)";
  attrib Q400B3_18 label="b3. No. of deputed out - CS(18)";
  attrib Q400B3_19 label="b3. No. of deputed out - CS(19)";
  attrib Q400B3_20 label="b3. No. of deputed out - CS(20)";
  attrib Q400B3_21 label="b3. No. of deputed out - CS(21)";
  attrib Q400B3_22 label="b3. No. of deputed out - CS(22)";
  attrib Q400B3_23 label="b3. No. of deputed out - CS(23)";
  attrib Q400B3_24 label="b3. No. of deputed out - CS(24)";
  attrib Q400B3_25 label="b3. No. of deputed out - CS(25)";
  attrib Q400B3_26 label="b3. No. of deputed out - CS(26)";
  attrib Q400B3_27 label="b3. No. of deputed out - CS(27)";
  attrib Q400B3_28 label="b3. No. of deputed out - CS(28)";
  attrib Q400B3_29 label="b3. No. of deputed out - CS(29)";
  attrib Q400B3_30 label="b3. No. of deputed out - CS(30)";
  attrib Q400B3_31 label="b3. No. of deputed out - CS(31)";
  attrib Q400B3_32 label="b3. No. of deputed out - CS(32)";
  attrib Q400B3_33 label="b3. No. of deputed out - CS(33)";
  attrib Q400B3_34 label="b3. No. of deputed out - CS(34)";
  attrib Q400B3_35 label="b3. No. of deputed out - CS(35)";
  attrib Q400B3_36 label="b3. No. of deputed out - CS(36)";
  attrib Q400B3_37 label="b3. No. of deputed out - CS(37)";
  attrib Q400B3_38 label="b3. No. of deputed out - CS(38)";
  attrib Q400B3_39 label="b3. No. of deputed out - CS(39)";
  attrib Q400B3_40 label="b3. No. of deputed out - CS(40)";
  attrib Q400B3_41 label="b3. No. of deputed out - CS(41)";
  attrib Q400B3_42 label="b3. No. of deputed out - CS(42)";
  attrib Q400B3_43 label="b3. No. of deputed out - CS(43)";
  attrib Q400B3_44 label="b3. No. of deputed out - CS(44)";
  attrib Q400B3_45 label="b3. No. of deputed out - CS(45)";
  attrib Q400B3_46 label="b3. No. of deputed out - CS(46)";
  attrib Q400B3_47 label="b3. No. of deputed out - CS(47)";
  attrib Q400B3_48 label="b3. No. of deputed out - CS(48)";
  attrib Q400B3_49 label="b3. No. of deputed out - CS(49)";
  attrib Q400B3_50 label="b3. No. of deputed out - CS(50)";
  attrib Q400B3_51 label="b3. No. of deputed out - CS(51)";
  attrib Q400B3_52 label="b3. No. of deputed out - CS(52)";
  attrib Q400B3_53 label="b3. No. of deputed out - CS(53)";
  attrib Q400B3_54 label="b3. No. of deputed out - CS(54)";
  attrib Q400B3_55 label="b3. No. of deputed out - CS(55)";
  attrib Q400B3_56 label="b3. No. of deputed out - CS(56)";
  attrib Q400B3_57 label="b3. No. of deputed out - CS(57)";
  attrib Q400B3_58 label="b3. No. of deputed out - CS(58)";
  attrib Q400B3_59 label="b3. No. of deputed out - CS(59)";
  attrib Q400B3_60 label="b3. No. of deputed out - CS(60)";
  attrib Q400B3_61 label="b3. No. of deputed out - CS(61)";
  attrib Q400B3_62 label="b3. No. of deputed out - CS(62)";
  attrib Q400B3_63 label="b3. No. of deputed out - CS(63)";
  attrib Q400B3_64 label="b3. No. of deputed out - CS(64)";
  attrib Q400B3_65 label="b3. No. of deputed out - CS(65)";
  attrib Q400B3_66 label="b3. No. of deputed out - CS(66)";
  attrib Q400B3_67 label="b3. No. of deputed out - CS(67)";
  attrib Q400B3_68 label="b3. No. of deputed out - CS(68)";
  attrib Q400B3_69 label="b3. No. of deputed out - CS(69)";
  attrib Q400B3_70 label="b3. No. of deputed out - CS(70)";
  attrib Q400B3_71 label="b3. No. of deputed out - CS(71)";
  attrib Q400B3_72 label="b3. No. of deputed out - CS(72)";
  attrib Q400B3_73 label="b3. No. of deputed out - CS(73)";
  attrib Q400B3_74 label="b3. No. of deputed out - CS(74)";
  attrib Q400B3_75 label="b3. No. of deputed out - CS(75)";
  attrib Q400B3_76 label="b3. No. of deputed out - CS(76)";
  attrib Q400B3_77 label="b3. No. of deputed out - CS(77)";
  attrib Q400B3_78 label="b3. No. of deputed out - CS(78)";
  attrib Q400B3_79 label="b3. No. of deputed out - CS(79)";
  attrib Q400B3_80 label="b3. No. of deputed out - CS(80)";
  attrib Q400B3_81 label="b3. No. of deputed out - CS(81)";
  attrib Q400B3_82 label="b3. No. of deputed out - CS(82)";
  attrib Q400B3_83 label="b3. No. of deputed out - CS(83)";
  attrib Q400B3_84 label="b3. No. of deputed out - CS(84)";
  attrib Q400B3_85 label="b3. No. of deputed out - CS(85)";
  attrib Q400B3_86 label="b3. No. of deputed out - CS(86)";
  attrib Q400B3_87 label="b3. No. of deputed out - CS(87)";
  attrib Q400B3_88 label="b3. No. of deputed out - CS(88)";
  attrib Q400B3_89 label="b3. No. of deputed out - CS(89)";
  attrib Q400B3_90 label="b3. No. of deputed out - CS(90)";
  attrib Q400B3_91 label="b3. No. of deputed out - CS(91)";
  attrib Q400B3_92 label="b3. No. of deputed out - CS(92)";
  attrib Q400B3_93 label="b3. No. of deputed out - CS(93)";
  attrib Q400B3_94 label="b3. No. of deputed out - CS(94)";
  attrib Q400B3_95 label="b3. No. of deputed out - CS(95)";
  attrib Q400B3_96 label="b3. No. of deputed out - CS(96)";
  attrib Q400B3_97 label="b3. No. of deputed out - CS(97)";
  attrib Q400B3_98 label="b3. No. of deputed out - CS(98)";
  attrib Q400B3_99 label="b3. No. of deputed out - CS(99)";
  attrib Q400B_01 label="c. Number of part time(1)";
  attrib Q400B_02 label="c. Number of part time(2)";
  attrib Q400B_03 label="c. Number of part time(3)";
  attrib Q400B_04 label="c. Number of part time(4)";
  attrib Q400B_05 label="c. Number of part time(5)";
  attrib Q400B_06 label="c. Number of part time(6)";
  attrib Q400B_07 label="c. Number of part time(7)";
  attrib Q400B_08 label="c. Number of part time(8)";
  attrib Q400B_09 label="c. Number of part time(9)";
  attrib Q400B_10 label="c. Number of part time(10)";
  attrib Q400B_11 label="c. Number of part time(11)";
  attrib Q400B_12 label="c. Number of part time(12)";
  attrib Q400B_13 label="c. Number of part time(13)";
  attrib Q400B_14 label="c. Number of part time(14)";
  attrib Q400B_15 label="c. Number of part time(15)";
  attrib Q400B_16 label="c. Number of part time(16)";
  attrib Q400B_17 label="c. Number of part time(17)";
  attrib Q400B_18 label="c. Number of part time(18)";
  attrib Q400B_19 label="c. Number of part time(19)";
  attrib Q400B_20 label="c. Number of part time(20)";
  attrib Q400B_21 label="c. Number of part time(21)";
  attrib Q400B_22 label="c. Number of part time(22)";
  attrib Q400B_23 label="c. Number of part time(23)";
  attrib Q400B_24 label="c. Number of part time(24)";
  attrib Q400B_25 label="c. Number of part time(25)";
  attrib Q400B_26 label="c. Number of part time(26)";
  attrib Q400B_27 label="c. Number of part time(27)";
  attrib Q400B_28 label="c. Number of part time(28)";
  attrib Q400B_29 label="c. Number of part time(29)";
  attrib Q400B_30 label="c. Number of part time(30)";
  attrib Q400B_31 label="c. Number of part time(31)";
  attrib Q400B_32 label="c. Number of part time(32)";
  attrib Q400B_33 label="c. Number of part time(33)";
  attrib Q400B_34 label="c. Number of part time(34)";
  attrib Q400B_35 label="c. Number of part time(35)";
  attrib Q400B_36 label="c. Number of part time(36)";
  attrib Q400B_37 label="c. Number of part time(37)";
  attrib Q400B_38 label="c. Number of part time(38)";
  attrib Q400B_39 label="c. Number of part time(39)";
  attrib Q400B_40 label="c. Number of part time(40)";
  attrib Q400B_41 label="c. Number of part time(41)";
  attrib Q400B_42 label="c. Number of part time(42)";
  attrib Q400B_43 label="c. Number of part time(43)";
  attrib Q400B_44 label="c. Number of part time(44)";
  attrib Q400B_45 label="c. Number of part time(45)";
  attrib Q400B_46 label="c. Number of part time(46)";
  attrib Q400B_47 label="c. Number of part time(47)";
  attrib Q400B_48 label="c. Number of part time(48)";
  attrib Q400B_49 label="c. Number of part time(49)";
  attrib Q400B_50 label="c. Number of part time(50)";
  attrib Q400B_51 label="c. Number of part time(51)";
  attrib Q400B_52 label="c. Number of part time(52)";
  attrib Q400B_53 label="c. Number of part time(53)";
  attrib Q400B_54 label="c. Number of part time(54)";
  attrib Q400B_55 label="c. Number of part time(55)";
  attrib Q400B_56 label="c. Number of part time(56)";
  attrib Q400B_57 label="c. Number of part time(57)";
  attrib Q400B_58 label="c. Number of part time(58)";
  attrib Q400B_59 label="c. Number of part time(59)";
  attrib Q400B_60 label="c. Number of part time(60)";
  attrib Q400B_61 label="c. Number of part time(61)";
  attrib Q400B_62 label="c. Number of part time(62)";
  attrib Q400B_63 label="c. Number of part time(63)";
  attrib Q400B_64 label="c. Number of part time(64)";
  attrib Q400B_65 label="c. Number of part time(65)";
  attrib Q400B_66 label="c. Number of part time(66)";
  attrib Q400B_67 label="c. Number of part time(67)";
  attrib Q400B_68 label="c. Number of part time(68)";
  attrib Q400B_69 label="c. Number of part time(69)";
  attrib Q400B_70 label="c. Number of part time(70)";
  attrib Q400B_71 label="c. Number of part time(71)";
  attrib Q400B_72 label="c. Number of part time(72)";
  attrib Q400B_73 label="c. Number of part time(73)";
  attrib Q400B_74 label="c. Number of part time(74)";
  attrib Q400B_75 label="c. Number of part time(75)";
  attrib Q400B_76 label="c. Number of part time(76)";
  attrib Q400B_77 label="c. Number of part time(77)";
  attrib Q400B_78 label="c. Number of part time(78)";
  attrib Q400B_79 label="c. Number of part time(79)";
  attrib Q400B_80 label="c. Number of part time(80)";
  attrib Q400B_81 label="c. Number of part time(81)";
  attrib Q400B_82 label="c. Number of part time(82)";
  attrib Q400B_83 label="c. Number of part time(83)";
  attrib Q400B_84 label="c. Number of part time(84)";
  attrib Q400B_85 label="c. Number of part time(85)";
  attrib Q400B_86 label="c. Number of part time(86)";
  attrib Q400B_87 label="c. Number of part time(87)";
  attrib Q400B_88 label="c. Number of part time(88)";
  attrib Q400B_89 label="c. Number of part time(89)";
  attrib Q400B_90 label="c. Number of part time(90)";
  attrib Q400B_91 label="c. Number of part time(91)";
  attrib Q400B_92 label="c. Number of part time(92)";
  attrib Q400B_93 label="c. Number of part time(93)";
  attrib Q400B_94 label="c. Number of part time(94)";
  attrib Q400B_95 label="c. Number of part time(95)";
  attrib Q400B_96 label="c. Number of part time(96)";
  attrib Q400B_97 label="c. Number of part time(97)";
  attrib Q400B_98 label="c. Number of part time(98)";
  attrib Q400B_99 label="c. Number of part time(99)";
  attrib Q400AAT  label="Total Sanctioned (expected)";
  attrib Q400AT   label="Total posted";
  attrib Q400B2T  label="Total deputed in/ attachment";
  attrib Q400B3T  label="Total deputed out";
  attrib Q400BT   label="Total part time";
  attrib Q430     format=F00146_. label="System to determine client opinions";
  attrib Q431     length=$11 format=$F00147_. label="Methods uses to elicit client opinions";
  attrib Q440     format=F00148_. label="Routine QA activities";
  attrib Q441     format=F00149_. label="Official record of QA activities in past year";
  attrib Q442     format=F00150_. label="Official record of QA activities in past year - observed";
  attrib Q450     format=F00151_. label="Functional ambulance at facility";
  attrib Q451     format=F00152_. label="Functional ambulance at facility - observed";
  attrib Q452     format=F00153_. label="Ambulance available elsewhere";
  attrib Q453     format=F00154_. label="Fuel available";
  attrib Q454A    format=F00155_. label="Number of ambulance";
  attrib Q454B    format=F00156_. label="Number of Richaw Van";
  attrib Q454C    format=F00157_. label="Number of Boat";
  attrib Q454D    format=F00158_. label="Number of Engine boat";
  attrib Q454E    format=F00159_. label="Number of Speed boat";
  attrib Q454F    format=F00160_. label="Number of Auto/Three wheeler";
  attrib Q454X    format=F00161_. label="Number of other vehicles";
  attrib Q460     format=F00162_. label="System to compile health services data";
  attrib Q461A_01 format=F00163_. label="Facility regularly compiles information (core-Q461)(1)";
  attrib Q461A_02 format=F00163_. label="Facility regularly compiles information (core-Q461)(2)";
  attrib Q461A_03 format=F00163_. label="Facility regularly compiles information (core-Q461)(3)";
  attrib Q461A_04 format=F00163_. label="Facility regularly compiles information (core-Q461)(4)";
  attrib Q461A_05 format=F00163_. label="Facility regularly compiles information (core-Q461)(5)";
  attrib Q461A_06 format=F00163_. label="Facility regularly compiles information (core-Q461)(6)";
  attrib Q461A_07 format=F00163_. label="Facility regularly compiles information (core-Q461)(7)";
  attrib Q461A_08 format=F00163_. label="Facility regularly compiles information (core-Q461)(8)";
  attrib Q461A_09 format=F00163_. label="Facility regularly compiles information (core-Q461)(9)";
  attrib Q461A_10 format=F00163_. label="Facility regularly compiles information (core-Q461)(10)";
  attrib Q461A_11 format=F00163_. label="Facility regularly compiles information (core-Q461)(11)";
  attrib Q461A_12 format=F00163_. label="Facility regularly compiles information (core-Q461)(12)";
  attrib Q461A_13 format=F00163_. label="Facility regularly compiles information (core-Q461)(13)";
  attrib Q461A_14 format=F00163_. label="Facility regularly compiles information (core-Q461)(14)";
  attrib Q461A_15 format=F00163_. label="Facility regularly compiles information (core-Q461)(15)";
  attrib Q461A_16 format=F00163_. label="Facility regularly compiles information (core-Q461)(16)";
  attrib Q461B_01 format=F00164_. label="Frequency of compilation (core-Q462)(1)";
  attrib Q461B_02 format=F00164_. label="Frequency of compilation (core-Q462)(2)";
  attrib Q461B_03 format=F00164_. label="Frequency of compilation (core-Q462)(3)";
  attrib Q461B_04 format=F00164_. label="Frequency of compilation (core-Q462)(4)";
  attrib Q461B_05 format=F00164_. label="Frequency of compilation (core-Q462)(5)";
  attrib Q461B_06 format=F00164_. label="Frequency of compilation (core-Q462)(6)";
  attrib Q461B_07 format=F00164_. label="Frequency of compilation (core-Q462)(7)";
  attrib Q461B_08 format=F00164_. label="Frequency of compilation (core-Q462)(8)";
  attrib Q461B_09 format=F00164_. label="Frequency of compilation (core-Q462)(9)";
  attrib Q461B_10 format=F00164_. label="Frequency of compilation (core-Q462)(10)";
  attrib Q461B_11 format=F00164_. label="Frequency of compilation (core-Q462)(11)";
  attrib Q461B_12 format=F00164_. label="Frequency of compilation (core-Q462)(12)";
  attrib Q461B_13 format=F00164_. label="Frequency of compilation (core-Q462)(13)";
  attrib Q461B_14 format=F00164_. label="Frequency of compilation (core-Q462)(14)";
  attrib Q461B_15 format=F00164_. label="Frequency of compilation (core-Q462)(15)";
  attrib Q461B_16 format=F00164_. label="Frequency of compilation (core-Q462)(16)";
  attrib Q461C_01 format=F00165_. label="Copy of most recent report seen (core-Q463)(1)";
  attrib Q461C_02 format=F00165_. label="Copy of most recent report seen (core-Q463)(2)";
  attrib Q461C_03 format=F00165_. label="Copy of most recent report seen (core-Q463)(3)";
  attrib Q461C_04 format=F00165_. label="Copy of most recent report seen (core-Q463)(4)";
  attrib Q461C_05 format=F00165_. label="Copy of most recent report seen (core-Q463)(5)";
  attrib Q461C_06 format=F00165_. label="Copy of most recent report seen (core-Q463)(6)";
  attrib Q461C_07 format=F00165_. label="Copy of most recent report seen (core-Q463)(7)";
  attrib Q461C_08 format=F00165_. label="Copy of most recent report seen (core-Q463)(8)";
  attrib Q461C_09 format=F00165_. label="Copy of most recent report seen (core-Q463)(9)";
  attrib Q461C_10 format=F00165_. label="Copy of most recent report seen (core-Q463)(10)";
  attrib Q461C_11 format=F00165_. label="Copy of most recent report seen (core-Q463)(11)";
  attrib Q461C_12 format=F00165_. label="Copy of most recent report seen (core-Q463)(12)";
  attrib Q461C_13 format=F00165_. label="Copy of most recent report seen (core-Q463)(13)";
  attrib Q461C_14 format=F00165_. label="Copy of most recent report seen (core-Q463)(14)";
  attrib Q461C_15 format=F00165_. label="Copy of most recent report seen (core-Q463)(15)";
  attrib Q461C_16 format=F00165_. label="Copy of most recent report seen (core-Q463)(16)";
  attrib Q464     format=F00166_. label="Designated person for health statistics";
  attrib Q465     format=F00167_. label="Designated person for health statistics";
  attrib Q471     format=F00168_. label="Number of live discharges last full-month";
  attrib Q472     format=F00169_. label="Number of outpatient client visits last full month";
  attrib Q501A_1  format=F00170_. label="Use and availability(1)";
  attrib Q501A_2  format=F00170_. label="Use and availability(2)";
  attrib Q501A_3  format=F00170_. label="Use and availability(3)";
  attrib Q501A_4  format=F00170_. label="Use and availability(4)";
  attrib Q501A_5  format=F00170_. label="Use and availability(5)";
  attrib Q501A_6  format=F00170_. label="Use and availability(6)";
  attrib Q501A_7  format=F00170_. label="Use and availability(7)";
  attrib Q501A_8  format=F00170_. label="Use and availability(8)";
  attrib Q501A_9  format=F00170_. label="Use and availability(9)";
  attrib Q501B_1  format=F00171_. label="Equipment functioning(1)";
  attrib Q501B_2  format=F00171_. label="Equipment functioning(2)";
  attrib Q501B_3  format=F00171_. label="Equipment functioning(3)";
  attrib Q501B_4  format=F00171_. label="Equipment functioning(4)";
  attrib Q501B_5  format=F00171_. label="Equipment functioning(5)";
  attrib Q501B_6  format=F00171_. label="Equipment functioning(6)";
  attrib Q501B_7  format=F00171_. label="Equipment functioning(7)";
  attrib Q501B_8  format=F00171_. label="Equipment functioning(8)";
  attrib Q501B_9  format=F00171_. label="Equipment functioning(9)";
  attrib Q502A_1  format=F00172_. label="Method used(1)";
  attrib Q502A_2  format=F00172_. label="Method used(2)";
  attrib Q502A_3  format=F00172_. label="Method used(3)";
  attrib Q502A_4  format=F00172_. label="Method used(4)";
  attrib Q502A_5  format=F00172_. label="Method used(5)";
  attrib Q502B_1  format=F00173_. label="Temperature(1)";
  attrib Q502B_2  format=F00173_. label="Temperature(2)";
  attrib Q502B_3  format=F00173_. label="Temperature(3)";
  attrib Q502B_4  format=F00173_. label="Temperature(4)";
  attrib Q502B_5  format=F00173_. label="Temperature(5)";
  attrib Q502C_1  format=F00174_. label="Pressure(1)";
  attrib Q502C_2  format=F00174_. label="Pressure(2)";
  attrib Q502C_3  format=F00174_. label="Pressure(3)";
  attrib Q502C_4  format=F00174_. label="Pressure(4)";
  attrib Q502C_5  format=F00174_. label="Pressure(5)";
  attrib Q502D_1  format=F00175_. label="Units of pressure(1)";
  attrib Q502D_2  format=F00175_. label="Units of pressure(2)";
  attrib Q502D_3  format=F00175_. label="Units of pressure(3)";
  attrib Q502D_4  format=F00175_. label="Units of pressure(4)";
  attrib Q502D_5  format=F00175_. label="Units of pressure(5)";
  attrib Q502E_1  format=F00176_. label="Time not wrapped(1)";
  attrib Q502E_2  format=F00176_. label="Time not wrapped(2)";
  attrib Q502E_3  format=F00176_. label="Time not wrapped(3)";
  attrib Q502E_4  format=F00176_. label="Time not wrapped(4)";
  attrib Q502E_5  format=F00176_. label="Time not wrapped(5)";
  attrib Q502F_1  format=F00177_. label="Time wrapped(1)";
  attrib Q502F_2  format=F00177_. label="Time wrapped(2)";
  attrib Q502F_3  format=F00177_. label="Time wrapped(3)";
  attrib Q502F_4  format=F00177_. label="Time wrapped(4)";
  attrib Q502F_5  format=F00177_. label="Time wrapped(5)";
  attrib Q502G_1  format=F00178_. label="Chemical used(1)";
  attrib Q502G_2  format=F00178_. label="Chemical used(2)";
  attrib Q502G_3  format=F00178_. label="Chemical used(3)";
  attrib Q502G_4  format=F00178_. label="Chemical used(4)";
  attrib Q502G_5  format=F00178_. label="Chemical used(5)";
  attrib Q503     format=F00179_. label="Guidelines for sterilization";
  attrib Q504     format=F00180_. label="Guidelines for sterilization - observed";
  attrib Q600     format=F00181_. label="Disposal of sharps waste";
  attrib Q601     format=F00182_. label="Disposal of medical waste";
  attrib Q603     format=F00183_. label="Sharps waste disposal site";
  attrib Q605     format=F00184_. label="Medical waste disposal site";
  attrib Q607     format=F00185_. label="Incinerator observation";
  attrib Q608     format=F00186_. label="Incinerator functional";
  attrib Q609     format=F00187_. label="Fuel available";
  attrib Q610     format=F00188_. label="Guidelines for waste management in service area";
  attrib Q611     format=F00189_. label="Guidelines for waste management in service area - observed";
  attrib Q620     format=F00190_. label="Toilet available for client use";
  attrib Q620A    length=$11 format=$F00191_. label="Observation of toilet";
  attrib Q620B    format=F00192_. label="Seperate toilet for female clients";
  attrib Q700A_01 format=F00193_. label="Availability of basic supplies(1)";
  attrib Q700A_02 format=F00193_. label="Availability of basic supplies(2)";
  attrib Q700A_03 format=F00193_. label="Availability of basic supplies(3)";
  attrib Q700A_04 format=F00193_. label="Availability of basic supplies(4)";
  attrib Q700A_05 format=F00193_. label="Availability of basic supplies(5)";
  attrib Q700A_06 format=F00193_. label="Availability of basic supplies(6)";
  attrib Q700A_07 format=F00193_. label="Availability of basic supplies(7)";
  attrib Q700A_08 format=F00193_. label="Availability of basic supplies(8)";
  attrib Q700A_09 format=F00193_. label="Availability of basic supplies(9)";
  attrib Q700A_10 format=F00193_. label="Availability of basic supplies(10)";
  attrib Q700A_11 format=F00193_. label="Availability of basic supplies(11)";
  attrib Q700A_12 format=F00193_. label="Availability of basic supplies(12)";
  attrib Q700A_13 format=F00193_. label="Availability of basic supplies(13)";
  attrib Q700A_14 format=F00193_. label="Availability of basic supplies(14)";
  attrib Q700A_15 format=F00193_. label="Availability of basic supplies(15)";
  attrib Q700A_16 format=F00193_. label="Availability of basic supplies(16)";
  attrib Q700A_17 format=F00193_. label="Availability of basic supplies(17)";
  attrib Q700A_18 format=F00193_. label="Availability of basic supplies(18)";
  attrib Q700A_19 format=F00193_. label="Availability of basic supplies(19)";
  attrib Q700A_20 format=F00193_. label="Availability of basic supplies(20)";
  attrib Q700A_21 format=F00193_. label="Availability of basic supplies(21)";
  attrib Q700A_22 format=F00193_. label="Availability of basic supplies(22)";
  attrib Q700A_23 format=F00193_. label="Availability of basic supplies(23)";
  attrib Q700A_24 format=F00193_. label="Availability of basic supplies(24)";
  attrib Q700A_25 format=F00193_. label="Availability of basic supplies(25)";
  attrib Q700B_01 format=F00194_. label="Basic supplies functioning(1)";
  attrib Q700B_02 format=F00194_. label="Basic supplies functioning(2)";
  attrib Q700B_03 format=F00194_. label="Basic supplies functioning(3)";
  attrib Q700B_04 format=F00194_. label="Basic supplies functioning(4)";
  attrib Q700B_05 format=F00194_. label="Basic supplies functioning(5)";
  attrib Q700B_06 format=F00194_. label="Basic supplies functioning(6)";
  attrib Q700B_07 format=F00194_. label="Basic supplies functioning(7)";
  attrib Q700B_08 format=F00194_. label="Basic supplies functioning(8)";
  attrib Q700B_09 format=F00194_. label="Basic supplies functioning(9)";
  attrib Q700B_10 format=F00194_. label="Basic supplies functioning(10)";
  attrib Q700B_11 format=F00194_. label="Basic supplies functioning(11)";
  attrib Q700B_12 format=F00194_. label="Basic supplies functioning(12)";
  attrib Q700B_13 format=F00194_. label="Basic supplies functioning(13)";
  attrib Q700B_14 format=F00194_. label="Basic supplies functioning(14)";
  attrib Q700B_15 format=F00194_. label="Basic supplies functioning(15)";
  attrib Q700B_16 format=F00194_. label="Basic supplies functioning(16)";
  attrib Q700B_17 format=F00194_. label="Basic supplies functioning(17)";
  attrib Q700B_18 format=F00194_. label="Basic supplies functioning(18)";
  attrib Q700B_19 format=F00194_. label="Basic supplies functioning(19)";
  attrib Q700B_20 format=F00194_. label="Basic supplies functioning(20)";
  attrib Q700B_21 format=F00194_. label="Basic supplies functioning(21)";
  attrib Q700B_22 format=F00194_. label="Basic supplies functioning(22)";
  attrib Q700B_23 format=F00194_. label="Basic supplies functioning(23)";
  attrib Q700B_24 format=F00194_. label="Basic supplies functioning(24)";
  attrib Q700B_25 format=F00194_. label="Basic supplies functioning(25)";
  attrib Q710_01  format=F00195_. label="Precaution and condition of client examination room(1)";
  attrib Q710_02  format=F00195_. label="Precaution and condition of client examination room(2)";
  attrib Q710_03  format=F00195_. label="Precaution and condition of client examination room(3)";
  attrib Q710_04  format=F00195_. label="Precaution and condition of client examination room(4)";
  attrib Q710_05  format=F00195_. label="Precaution and condition of client examination room(5)";
  attrib Q710_06  format=F00195_. label="Precaution and condition of client examination room(6)";
  attrib Q710_07  format=F00195_. label="Precaution and condition of client examination room(7)";
  attrib Q710_08  format=F00195_. label="Precaution and condition of client examination room(8)";
  attrib Q710_09  format=F00195_. label="Precaution and condition of client examination room(9)";
  attrib Q710_10  format=F00195_. label="Precaution and condition of client examination room(10)";
  attrib Q710_11  format=F00195_. label="Precaution and condition of client examination room(11)";
  attrib Q710_12  format=F00195_. label="Precaution and condition of client examination room(12)";
  attrib Q710_13  format=F00195_. label="Precaution and condition of client examination room(13)";
  attrib Q711     format=F00196_. label="Setting of examination room";
  attrib Q720     format=F00197_. label="Protected client waiting area";
  attrib Q801     format=F00198_. label="Anemia testing";
  attrib Q802A_1  format=F00199_. label="Anemia equipment used(1)";
  attrib Q802A_2  format=F00199_. label="Anemia equipment used(2)";
  attrib Q802A_3  format=F00199_. label="Anemia equipment used(3)";
  attrib Q802A_4  format=F00199_. label="Anemia equipment used(4)";
  attrib Q802A_5  format=F00199_. label="Anemia equipment used(5)";
  attrib Q802A_6  format=F00199_. label="Anemia equipment used(6)";
  attrib Q802A_7  format=F00199_. label="Anemia equipment used(7)";
  attrib Q802B_1  format=F00200_. label="Observed equipment(1)";
  attrib Q802B_2  format=F00200_. label="Observed equipment(2)";
  attrib Q802B_3  format=F00200_. label="Observed equipment(3)";
  attrib Q802B_4  format=F00200_. label="Observed equipment(4)";
  attrib Q802B_5  format=F00200_. label="Observed equipment(5)";
  attrib Q802B_6  format=F00200_. label="Observed equipment(6)";
  attrib Q802B_7  format=F00200_. label="Observed equipment(7)";
  attrib Q802C_1  format=F00201_. label="Functioning(1)";
  attrib Q802C_2  format=F00201_. label="Functioning(2)";
  attrib Q802C_3  format=F00201_. label="Functioning(3)";
  attrib Q802C_4  format=F00201_. label="Functioning(4)";
  attrib Q802C_5  format=F00201_. label="Functioning(5)";
  attrib Q802C_6  format=F00201_. label="Functioning(6)";
  attrib Q802C_7  format=F00201_. label="Functioning(7)";
  attrib Q829     format=F00202_. label="Blood cholesterol level testing";
  attrib Q830     format=F00203_. label="Blood glucose tests";
  attrib Q831A_1  format=F00204_. label="Glucose equipment used(1)";
  attrib Q831A_2  format=F00204_. label="Glucose equipment used(2)";
  attrib Q831B_1  format=F00205_. label="Observed equipment(1)";
  attrib Q831B_2  format=F00205_. label="Observed equipment(2)";
  attrib Q831C_1  format=F00206_. label="Functioning(1)";
  attrib Q831C_2  format=F00206_. label="Functioning(2)";
  attrib Q832     format=F00207_. label="Lever or renal function tests";
  attrib Q833     format=F00208_. label="Blood chemistry analyzer";
  attrib Q834     format=F00209_. label="Blood chemistry analyzer - Observed";
  attrib Q835     format=F00210_. label="Blood chemistry analyzer - Functioning";
  attrib Q836     format=F00211_. label="Urine chemistry testing/ urine pregnancy tests";
  attrib Q837A_1  format=F00212_. label="Dipsticks used(1)";
  attrib Q837A_2  format=F00212_. label="Dipsticks used(2)";
  attrib Q837A_3  format=F00212_. label="Dipsticks used(3)";
  attrib Q837B_1  format=F00213_. label="Dipsticks observed(1)";
  attrib Q837B_2  format=F00213_. label="Dipsticks observed(2)";
  attrib Q837B_3  format=F00213_. label="Dipsticks observed(3)";
  attrib Q838     format=F00214_. label="Send blood/ urine outside";
  attrib Q839A_1  format=F00215_. label="Tests outside(1)";
  attrib Q839A_2  format=F00215_. label="Tests outside(2)";
  attrib Q839A_3  format=F00215_. label="Tests outside(3)";
  attrib Q839A_4  format=F00215_. label="Tests outside(4)";
  attrib Q839B_1  format=F00216_. label="Record of outside test results(1)";
  attrib Q839B_2  format=F00216_. label="Record of outside test results(2)";
  attrib Q839B_3  format=F00216_. label="Record of outside test results(3)";
  attrib Q839B_4  format=F00216_. label="Record of outside test results(4)";
  attrib Q840A_1  format=F00217_. label="Parasite equipment used(1)";
  attrib Q840A_2  format=F00217_. label="Parasite equipment used(2)";
  attrib Q840A_3  format=F00217_. label="Parasite equipment used(3)";
  attrib Q840A_4  format=F00217_. label="Parasite equipment used(4)";
  attrib Q840A_5  format=F00217_. label="Parasite equipment used(5)";
  attrib Q840A_6  format=F00217_. label="Parasite equipment used(6)";
  attrib Q840A_7  format=F00217_. label="Parasite equipment used(7)";
  attrib Q840A_8  format=F00217_. label="Parasite equipment used(8)";
  attrib Q840B_1  format=F00218_. label="Parasite equipment observed(1)";
  attrib Q840B_2  format=F00218_. label="Parasite equipment observed(2)";
  attrib Q840B_3  format=F00218_. label="Parasite equipment observed(3)";
  attrib Q840B_4  format=F00218_. label="Parasite equipment observed(4)";
  attrib Q840B_5  format=F00218_. label="Parasite equipment observed(5)";
  attrib Q840B_6  format=F00218_. label="Parasite equipment observed(6)";
  attrib Q840B_7  format=F00218_. label="Parasite equipment observed(7)";
  attrib Q840B_8  format=F00218_. label="Parasite equipment observed(8)";
  attrib Q840C_1  format=F00219_. label="Functioning(1)";
  attrib Q840C_2  format=F00219_. label="Functioning(2)";
  attrib Q840C_3  format=F00219_. label="Functioning(3)";
  attrib Q840C_4  format=F00219_. label="Functioning(4)";
  attrib Q840C_5  format=F00219_. label="Functioning(5)";
  attrib Q840C_6  format=F00219_. label="Functioning(6)";
  attrib Q840C_7  format=F00219_. label="Functioning(7)";
  attrib Q840C_8  format=F00219_. label="Functioning(8)";
  attrib Q848     format=F00220_. label="Gram staining";
  attrib Q849A_1  format=F00221_. label="Gram staining used(1)";
  attrib Q849A_2  format=F00221_. label="Gram staining used(2)";
  attrib Q849A_3  format=F00221_. label="Gram staining used(3)";
  attrib Q849A_4  format=F00221_. label="Gram staining used(4)";
  attrib Q849B_1  format=F00222_. label="Gram staining available(1)";
  attrib Q849B_2  format=F00222_. label="Gram staining available(2)";
  attrib Q849B_3  format=F00222_. label="Gram staining available(3)";
  attrib Q849B_4  format=F00222_. label="Gram staining available(4)";
  attrib Q850     format=F00223_. label="Specimens sent outside for staining or malaria testing";
  attrib Q851A_1  format=F00224_. label="Send specimen outside for test(1)";
  attrib Q851A_2  format=F00224_. label="Send specimen outside for test(2)";
  attrib Q851A_3  format=F00224_. label="Send specimen outside for test(3)";
  attrib Q851A_4  format=F00224_. label="Send specimen outside for test(4)";
  attrib Q851B_1  format=F00225_. label="Record of test results observed(1)";
  attrib Q851B_2  format=F00225_. label="Record of test results observed(2)";
  attrib Q851B_3  format=F00225_. label="Record of test results observed(3)";
  attrib Q851B_4  format=F00225_. label="Record of test results observed(4)";
  attrib Q852     format=F00226_. label="Stool microscopy";
  attrib Q853A_1  format=F00227_. label="Equipment used(1)";
  attrib Q853A_2  format=F00227_. label="Equipment used(2)";
  attrib Q853A_3  format=F00227_. label="Equipment used(3)";
  attrib Q853B_1  format=F00228_. label="Equipment available(1)";
  attrib Q853B_2  format=F00228_. label="Equipment available(2)";
  attrib Q853B_3  format=F00228_. label="Equipment available(3)";
  attrib Q854     format=F00229_. label="Syphilis tests";
  attrib Q855     format=F00230_. label="Syphilis rapid diagnostic test (RDT) kit";
  attrib Q856     format=F00231_. label="Observe Syphilis rapid test kit observed";
  attrib Q857     format=F00232_. label="Conduct any other syphilis testing";
  attrib Q858A_1  format=F00233_. label="Test conducted(1)";
  attrib Q858A_2  format=F00233_. label="Test conducted(2)";
  attrib Q858A_3  format=F00233_. label="Test conducted(3)";
  attrib Q858A_4  format=F00233_. label="Test conducted(4)";
  attrib Q858A_5  format=F00233_. label="Test conducted(5)";
  attrib Q858B_1  format=F00234_. label="Items for test available(1)";
  attrib Q858B_2  format=F00234_. label="Items for test available(2)";
  attrib Q858B_3  format=F00234_. label="Items for test available(3)";
  attrib Q858B_4  format=F00234_. label="Items for test available(4)";
  attrib Q858B_5  format=F00234_. label="Items for test available(5)";
  attrib Q858C_1  format=F00235_. label="Functioning(1)";
  attrib Q858C_2  format=F00235_. label="Functioning(2)";
  attrib Q858C_3  format=F00235_. label="Functioning(3)";
  attrib Q858C_4  format=F00235_. label="Functioning(4)";
  attrib Q858C_5  format=F00235_. label="Functioning(5)";
  attrib Q859     format=F00236_. label="Chlamydia test";
  attrib Q860A_1  format=F00237_. label="Chlamydia test conducted(1)";
  attrib Q860A_2  format=F00237_. label="Chlamydia test conducted(2)";
  attrib Q860B_1  format=F00238_. label="Items for test available(1)";
  attrib Q860B_2  format=F00238_. label="Items for test available(2)";
  attrib Q861     format=F00239_. label="TB tests";
  attrib Q862A_1  format=F00240_. label="TB test used(1)";
  attrib Q862A_2  format=F00240_. label="TB test used(2)";
  attrib Q862A_3  format=F00240_. label="TB test used(3)";
  attrib Q862A_4  format=F00240_. label="TB test used(4)";
  attrib Q862A_5  format=F00240_. label="TB test used(5)";
  attrib Q862A_6  format=F00240_. label="TB test used(6)";
  attrib Q862A_7  format=F00240_. label="TB test used(7)";
  attrib Q862A_8  format=F00240_. label="TB test used(8)";
  attrib Q862B_1  format=F00241_. label="TB equipment available(1)";
  attrib Q862B_2  format=F00241_. label="TB equipment available(2)";
  attrib Q862B_3  format=F00241_. label="TB equipment available(3)";
  attrib Q862B_4  format=F00241_. label="TB equipment available(4)";
  attrib Q862B_5  format=F00241_. label="TB equipment available(5)";
  attrib Q862B_6  format=F00241_. label="TB equipment available(6)";
  attrib Q862B_7  format=F00241_. label="TB equipment available(7)";
  attrib Q862B_8  format=F00241_. label="TB equipment available(8)";
  attrib Q862C_1  format=F00242_. label="Functioning(1)";
  attrib Q862C_2  format=F00242_. label="Functioning(2)";
  attrib Q862C_3  format=F00242_. label="Functioning(3)";
  attrib Q862C_4  format=F00242_. label="Functioning(4)";
  attrib Q862C_5  format=F00242_. label="Functioning(5)";
  attrib Q862C_6  format=F00242_. label="Functioning(6)";
  attrib Q862C_7  format=F00242_. label="Functioning(7)";
  attrib Q862C_8  format=F00242_. label="Functioning(8)";
  attrib Q863     format=F00243_. label="Use TB diagnostic test";
  attrib Q864     format=F00244_. label="Observe TB rapid diagnostic test (RDT) kit";
  attrib Q865     format=F00245_. label="Maintain any sputum containers";
  attrib Q866     format=F00246_. label="Observe sample sputum container";
  attrib Q867     format=F00247_. label="Sputum sent outside for TB testing";
  attrib Q868     format=F00248_. label="Record of outside sputum tests";
  attrib Q869     format=F00249_. label="Record of outside sputum tests - observed";
  attrib Q870     format=F00250_. label="System for internal/ external QC for TB sputum smears";
  attrib Q871     format=F00251_. label="Type of quality control practice";
  attrib Q872     format=F00252_. label="Record of results from QC procedures for TB sputum";
  attrib Q873     format=F00253_. label="Record maintained for IQC/EQC/Both";
  attrib Q880     format=F00254_. label="Perform diagnostic X-rays";
  attrib Q881A_1  format=F00255_. label="Imaging Equipment used(1)";
  attrib Q881A_2  format=F00255_. label="Imaging Equipment used(2)";
  attrib Q881A_3  format=F00255_. label="Imaging Equipment used(3)";
  attrib Q881A_4  format=F00255_. label="Imaging Equipment used(4)";
  attrib Q881A_5  format=F00255_. label="Imaging Equipment used(5)";
  attrib Q881B_1  format=F00256_. label="Equipment available(1)";
  attrib Q881B_2  format=F00256_. label="Equipment available(2)";
  attrib Q881B_3  format=F00256_. label="Equipment available(3)";
  attrib Q881B_4  format=F00256_. label="Equipment available(4)";
  attrib Q881B_5  format=F00256_. label="Equipment available(5)";
  attrib Q881C_1  format=F00257_. label="Equipment working order(1)";
  attrib Q881C_2  format=F00257_. label="Equipment working order(2)";
  attrib Q881C_3  format=F00257_. label="Equipment working order(3)";
  attrib Q881C_4  format=F00257_. label="Equipment working order(4)";
  attrib Q881C_5  format=F00257_. label="Equipment working order(5)";
  attrib Q901_01  format=F00258_. label="Antibiotics drugs(1)";
  attrib Q901_02  format=F00258_. label="Antibiotics drugs(2)";
  attrib Q901_03  format=F00258_. label="Antibiotics drugs(3)";
  attrib Q901_04  format=F00258_. label="Antibiotics drugs(4)";
  attrib Q901_05  format=F00258_. label="Antibiotics drugs(5)";
  attrib Q901_06  format=F00258_. label="Antibiotics drugs(6)";
  attrib Q901_07  format=F00258_. label="Antibiotics drugs(7)";
  attrib Q901_08  format=F00258_. label="Antibiotics drugs(8)";
  attrib Q901_09  format=F00258_. label="Antibiotics drugs(9)";
  attrib Q901_10  format=F00258_. label="Antibiotics drugs(10)";
  attrib Q901_11  format=F00258_. label="Antibiotics drugs(11)";
  attrib Q901_12  format=F00258_. label="Antibiotics drugs(12)";
  attrib Q901_13  format=F00258_. label="Antibiotics drugs(13)";
  attrib Q901_14  format=F00258_. label="Antibiotics drugs(14)";
  attrib Q901_15  format=F00258_. label="Antibiotics drugs(15)";
  attrib Q901_16  format=F00258_. label="Antibiotics drugs(16)";
  attrib Q901_17  format=F00258_. label="Antibiotics drugs(17)";
  attrib Q901_18  format=F00258_. label="Antibiotics drugs(18)";
  attrib Q901_19  format=F00258_. label="Antibiotics drugs(19)";
  attrib Q901_20  format=F00258_. label="Antibiotics drugs(20)";
  attrib Q901_21  format=F00258_. label="Antibiotics drugs(21)";
  attrib Q901_22  format=F00258_. label="Antibiotics drugs(22)";
  attrib Q901_23  format=F00258_. label="Antibiotics drugs(23)";
  attrib Q901_24  format=F00258_. label="Antibiotics drugs(24)";
  attrib Q901_25  format=F00258_. label="Antibiotics drugs(25)";
  attrib Q901_26  format=F00258_. label="Antibiotics drugs(26)";
  attrib Q901_27  format=F00258_. label="Antibiotics drugs(27)";
  attrib Q901_28  format=F00258_. label="Antibiotics drugs(28)";
  attrib Q901_29  format=F00258_. label="Antibiotics drugs(29)";
  attrib Q901_30  format=F00258_. label="Antibiotics drugs(30)";
  attrib Q901_31  format=F00258_. label="Antibiotics drugs(31)";
  attrib Q901_32  format=F00258_. label="Antibiotics drugs(32)";
  attrib Q901_33  format=F00258_. label="Antibiotics drugs(33)";
  attrib Q901_34  format=F00258_. label="Antibiotics drugs(34)";
  attrib Q901_35  format=F00258_. label="Antibiotics drugs(35)";
  attrib Q901_36  format=F00258_. label="Antibiotics drugs(36)";
  attrib Q901_37  format=F00258_. label="Antibiotics drugs(37)";
  attrib Q901_38  format=F00258_. label="Antibiotics drugs(38)";
  attrib Q902_1   format=F00259_. label="Worm infestation drugs(1)";
  attrib Q902_2   format=F00259_. label="Worm infestation drugs(2)";
  attrib Q902_3   format=F00259_. label="Worm infestation drugs(3)";
  attrib Q902_4   format=F00259_. label="Worm infestation drugs(4)";
  attrib Q902_5   format=F00259_. label="Worm infestation drugs(5)";
  attrib Q903_01  format=F00260_. label="Drugs for Non communicable diseases(1)";
  attrib Q903_02  format=F00260_. label="Drugs for Non communicable diseases(2)";
  attrib Q903_03  format=F00260_. label="Drugs for Non communicable diseases(3)";
  attrib Q903_04  format=F00260_. label="Drugs for Non communicable diseases(4)";
  attrib Q903_05  format=F00260_. label="Drugs for Non communicable diseases(5)";
  attrib Q903_06  format=F00260_. label="Drugs for Non communicable diseases(6)";
  attrib Q903_07  format=F00260_. label="Drugs for Non communicable diseases(7)";
  attrib Q903_08  format=F00260_. label="Drugs for Non communicable diseases(8)";
  attrib Q903_09  format=F00260_. label="Drugs for Non communicable diseases(9)";
  attrib Q903_10  format=F00260_. label="Drugs for Non communicable diseases(10)";
  attrib Q903_11  format=F00260_. label="Drugs for Non communicable diseases(11)";
  attrib Q903_12  format=F00260_. label="Drugs for Non communicable diseases(12)";
  attrib Q903_13  format=F00260_. label="Drugs for Non communicable diseases(13)";
  attrib Q903_14  format=F00260_. label="Drugs for Non communicable diseases(14)";
  attrib Q903_15  format=F00260_. label="Drugs for Non communicable diseases(15)";
  attrib Q903_16  format=F00260_. label="Drugs for Non communicable diseases(16)";
  attrib Q903_17  format=F00260_. label="Drugs for Non communicable diseases(17)";
  attrib Q903_18  format=F00260_. label="Drugs for Non communicable diseases(18)";
  attrib Q903_19  format=F00260_. label="Drugs for Non communicable diseases(19)";
  attrib Q903_20  format=F00260_. label="Drugs for Non communicable diseases(20)";
  attrib Q903_21  format=F00260_. label="Drugs for Non communicable diseases(21)";
  attrib Q903_22  format=F00260_. label="Drugs for Non communicable diseases(22)";
  attrib Q903_23  format=F00260_. label="Drugs for Non communicable diseases(23)";
  attrib Q903_24  format=F00260_. label="Drugs for Non communicable diseases(24)";
  attrib Q903_25  format=F00260_. label="Drugs for Non communicable diseases(25)";
  attrib Q903_26  format=F00260_. label="Drugs for Non communicable diseases(26)";
  attrib Q903_27  format=F00260_. label="Drugs for Non communicable diseases(27)";
  attrib Q903_28  format=F00260_. label="Drugs for Non communicable diseases(28)";
  attrib Q903_29  format=F00260_. label="Drugs for Non communicable diseases(29)";
  attrib Q903_30  format=F00260_. label="Drugs for Non communicable diseases(30)";
  attrib Q903_31  format=F00260_. label="Drugs for Non communicable diseases(31)";
  attrib Q903_32  format=F00260_. label="Drugs for Non communicable diseases(32)";
  attrib Q903_33  format=F00260_. label="Drugs for Non communicable diseases(33)";
  attrib Q903_34  format=F00260_. label="Drugs for Non communicable diseases(34)";
  attrib Q903_35  format=F00260_. label="Drugs for Non communicable diseases(35)";
  attrib Q903_36  format=F00260_. label="Drugs for Non communicable diseases(36)";
  attrib Q903_37  format=F00260_. label="Drugs for Non communicable diseases(37)";
  attrib Q903_38  format=F00260_. label="Drugs for Non communicable diseases(38)";
  attrib Q903_39  format=F00260_. label="Drugs for Non communicable diseases(39)";
  attrib Q903_40  format=F00260_. label="Drugs for Non communicable diseases(40)";
  attrib Q903_41  format=F00260_. label="Drugs for Non communicable diseases(41)";
  attrib Q903_42  format=F00260_. label="Drugs for Non communicable diseases(42)";
  attrib Q903_43  format=F00260_. label="Drugs for Non communicable diseases(43)";
  attrib Q903_44  format=F00260_. label="Drugs for Non communicable diseases(44)";
  attrib Q903_45  format=F00260_. label="Drugs for Non communicable diseases(45)";
  attrib Q903_46  format=F00260_. label="Drugs for Non communicable diseases(46)";
  attrib Q903_47  format=F00260_. label="Drugs for Non communicable diseases(47)";
  attrib Q904_01  format=F00261_. label="Anti-fungal medicines(1)";
  attrib Q904_02  format=F00261_. label="Anti-fungal medicines(2)";
  attrib Q904_03  format=F00261_. label="Anti-fungal medicines(3)";
  attrib Q904_04  format=F00261_. label="Anti-fungal medicines(4)";
  attrib Q904_05  format=F00261_. label="Anti-fungal medicines(5)";
  attrib Q904_06  format=F00261_. label="Anti-fungal medicines(6)";
  attrib Q904_07  format=F00261_. label="Anti-fungal medicines(7)";
  attrib Q904_08  format=F00261_. label="Anti-fungal medicines(8)";
  attrib Q904_09  format=F00261_. label="Anti-fungal medicines(9)";
  attrib Q904_10  format=F00261_. label="Anti-fungal medicines(10)";
  attrib Q904_11  format=F00261_. label="Anti-fungal medicines(11)";
  attrib Q906_01  format=F00262_. label="Maternal and child health drugs(1)";
  attrib Q906_02  format=F00262_. label="Maternal and child health drugs(2)";
  attrib Q906_03  format=F00262_. label="Maternal and child health drugs(3)";
  attrib Q906_04  format=F00262_. label="Maternal and child health drugs(4)";
  attrib Q906_05  format=F00262_. label="Maternal and child health drugs(5)";
  attrib Q906_06  format=F00262_. label="Maternal and child health drugs(6)";
  attrib Q906_07  format=F00262_. label="Maternal and child health drugs(7)";
  attrib Q906_08  format=F00262_. label="Maternal and child health drugs(8)";
  attrib Q906_09  format=F00262_. label="Maternal and child health drugs(9)";
  attrib Q906_10  format=F00262_. label="Maternal and child health drugs(10)";
  attrib Q906_11  format=F00262_. label="Maternal and child health drugs(11)";
  attrib Q906_12  format=F00262_. label="Maternal and child health drugs(12)";
  attrib Q906_13  format=F00262_. label="Maternal and child health drugs(13)";
  attrib Q906_14  format=F00262_. label="Maternal and child health drugs(14)";
  attrib Q906_15  format=F00262_. label="Maternal and child health drugs(15)";
  attrib Q906_16  format=F00262_. label="Maternal and child health drugs(16)";
  attrib Q906_17  format=F00262_. label="Maternal and child health drugs(17)";
  attrib Q906_18  format=F00262_. label="Maternal and child health drugs(18)";
  attrib Q906_19  format=F00262_. label="Maternal and child health drugs(19)";
  attrib Q906_20  format=F00262_. label="Maternal and child health drugs(20)";
  attrib Q906_21  format=F00262_. label="Maternal and child health drugs(21)";
  attrib Q906_22  format=F00262_. label="Maternal and child health drugs(22)";
  attrib Q906_23  format=F00262_. label="Maternal and child health drugs(23)";
  attrib Q906_24  format=F00262_. label="Maternal and child health drugs(24)";
  attrib Q906_25  format=F00262_. label="Maternal and child health drugs(25)";
  attrib Q906_26  format=F00262_. label="Maternal and child health drugs(26)";
  attrib Q907_1   format=F00263_. label="Intravenous fluids(1)";
  attrib Q907_2   format=F00263_. label="Intravenous fluids(2)";
  attrib Q907_3   format=F00263_. label="Intravenous fluids(3)";
  attrib Q907_4   format=F00263_. label="Intravenous fluids(4)";
  attrib Q907_5   format=F00263_. label="Intravenous fluids(5)";
  attrib Q907_6   format=F00263_. label="Intravenous fluids(6)";
  attrib Q908_01  format=F00264_. label="Fever reducing and pain medicines(1)";
  attrib Q908_02  format=F00264_. label="Fever reducing and pain medicines(2)";
  attrib Q908_03  format=F00264_. label="Fever reducing and pain medicines(3)";
  attrib Q908_04  format=F00264_. label="Fever reducing and pain medicines(4)";
  attrib Q908_05  format=F00264_. label="Fever reducing and pain medicines(5)";
  attrib Q908_06  format=F00264_. label="Fever reducing and pain medicines(6)";
  attrib Q908_07  format=F00264_. label="Fever reducing and pain medicines(7)";
  attrib Q908_08  format=F00264_. label="Fever reducing and pain medicines(8)";
  attrib Q908_09  format=F00264_. label="Fever reducing and pain medicines(9)";
  attrib Q908_10  format=F00264_. label="Fever reducing and pain medicines(10)";
  attrib Q908_11  format=F00264_. label="Fever reducing and pain medicines(11)";
  attrib Q908_12  format=F00264_. label="Fever reducing and pain medicines(12)";
  attrib Q908_13  format=F00264_. label="Fever reducing and pain medicines(13)";
  attrib Q908_14  format=F00264_. label="Fever reducing and pain medicines(14)";
  attrib Q908_15  format=F00264_. label="Fever reducing and pain medicines(15)";
  attrib Q908_16  format=F00264_. label="Fever reducing and pain medicines(16)";
  attrib Q908_17  format=F00264_. label="Fever reducing and pain medicines(17)";
  attrib Q909_1   format=F00265_. label="Storage condition: Antibiotics & general medicines(1)";
  attrib Q909_2   format=F00265_. label="Storage condition: Antibiotics & general medicines(2)";
  attrib Q909_3   format=F00265_. label="Storage condition: Antibiotics & general medicines(3)";
  attrib Q909_4   format=F00265_. label="Storage condition: Antibiotics & general medicines(4)";
  attrib Q909_5   format=F00265_. label="Storage condition: Antibiotics & general medicines(5)";
  attrib Q910     format=F00266_. label="Medicines organized (FIFO) - Antibiotics & general";
  attrib Q911     format=F00267_. label="System used to monitor amount of meds received, issued, present each day  - Antibiotics & general";
  attrib Q912_1   format=F00268_. label="Supply items(1)";
  attrib Q912_2   format=F00268_. label="Supply items(2)";
  attrib Q912_3   format=F00268_. label="Supply items(3)";
  attrib Q912_4   format=F00268_. label="Supply items(4)";
  attrib Q912_5   format=F00268_. label="Supply items(5)";
  attrib Q912_6   format=F00268_. label="Supply items(6)";
  attrib Q912_7   format=F00268_. label="Supply items(7)";
  attrib Q912_8   format=F00268_. label="Supply items(8)";
  attrib Q921_01  format=F00269_. label="Contraceptives(1)";
  attrib Q921_02  format=F00269_. label="Contraceptives(2)";
  attrib Q921_03  format=F00269_. label="Contraceptives(3)";
  attrib Q921_04  format=F00269_. label="Contraceptives(4)";
  attrib Q921_05  format=F00269_. label="Contraceptives(5)";
  attrib Q921_06  format=F00269_. label="Contraceptives(6)";
  attrib Q921_07  format=F00269_. label="Contraceptives(7)";
  attrib Q921_08  format=F00269_. label="Contraceptives(8)";
  attrib Q921_09  format=F00269_. label="Contraceptives(9)";
  attrib Q921_10  format=F00269_. label="Contraceptives(10)";
  attrib Q921_11  format=F00269_. label="Contraceptives(11)";
  attrib Q922_1   format=F00270_. label="Storage conditions of contraceptives(1)";
  attrib Q922_2   format=F00270_. label="Storage conditions of contraceptives(2)";
  attrib Q922_3   format=F00270_. label="Storage conditions of contraceptives(3)";
  attrib Q922_4   format=F00270_. label="Storage conditions of contraceptives(4)";
  attrib Q922_5   format=F00270_. label="Storage conditions of contraceptives(5)";
  attrib Q923     format=F00271_. label="Medicines organized (FIFO) - conraceptive";
  attrib Q924     format=F00272_. label="System used to monitor amount of meds received, issued, present each day - contraceptive";
  attrib Q931_01  format=F00273_. label="Anti TB drugs(1)";
  attrib Q931_02  format=F00273_. label="Anti TB drugs(2)";
  attrib Q931_03  format=F00273_. label="Anti TB drugs(3)";
  attrib Q931_04  format=F00273_. label="Anti TB drugs(4)";
  attrib Q931_05  format=F00273_. label="Anti TB drugs(5)";
  attrib Q931_06  format=F00273_. label="Anti TB drugs(6)";
  attrib Q931_07  format=F00273_. label="Anti TB drugs(7)";
  attrib Q931_08  format=F00273_. label="Anti TB drugs(8)";
  attrib Q931_09  format=F00273_. label="Anti TB drugs(9)";
  attrib Q931_10  format=F00273_. label="Anti TB drugs(10)";
  attrib Q1001A_1 format=F00274_. label="Days/month immunization is available at facility(1)";
  attrib Q1001A_2 format=F00274_. label="Days/month immunization is available at facility(2)";
  attrib Q1001A_3 format=F00274_. label="Days/month immunization is available at facility(3)";
  attrib Q1001A_4 format=F00274_. label="Days/month immunization is available at facility(4)";
  attrib Q1001A_5 format=F00274_. label="Days/month immunization is available at facility(5)";
  attrib Q1001A_6 format=F00274_. label="Days/month immunization is available at facility(6)";
  attrib Q1001B_1 format=F00275_. label="Days/month immunization is available through outreach(1)";
  attrib Q1001B_2 format=F00275_. label="Days/month immunization is available through outreach(2)";
  attrib Q1001B_3 format=F00275_. label="Days/month immunization is available through outreach(3)";
  attrib Q1001B_4 format=F00275_. label="Days/month immunization is available through outreach(4)";
  attrib Q1001B_5 format=F00275_. label="Days/month immunization is available through outreach(5)";
  attrib Q1001B_6 format=F00275_. label="Days/month immunization is available through outreach(6)";
  attrib Q1002    format=F00276_. label="National guidelines available";
  attrib Q1003    format=F00277_. label="Observe national guidelines available";
  attrib Q1004    format=F00278_. label="Other guidelines";
  attrib Q1005    format=F00279_. label="Observe other guidelines";
  attrib Q1006_1  format=F00280_. label="Items for immunization available(1)";
  attrib Q1006_2  format=F00280_. label="Items for immunization available(2)";
  attrib Q1006_3  format=F00280_. label="Items for immunization available(3)";
  attrib Q1007    format=F00281_. label="Routinely store vaccines";
  attrib Q1008    format=F00282_. label="Refrigerator available - observec";
  attrib Q1009    format=F00283_. label="Cold-chain monitoring system";
  attrib Q1010    format=F00284_. label="Observe cold-chain monitoring system";
  attrib Q1011    format=F00285_. label="Temp record completed past 30 days";
  attrib Q1012_1  format=F00286_. label="Vaccines available(1)";
  attrib Q1012_2  format=F00286_. label="Vaccines available(2)";
  attrib Q1012_3  format=F00286_. label="Vaccines available(3)";
  attrib Q1012_4  format=F00286_. label="Vaccines available(4)";
  attrib Q1012_5  format=F00286_. label="Vaccines available(5)";
  attrib Q1012_6  format=F00286_. label="Vaccines available(6)";
  attrib Q1013    format=F00287_. label="Temperature of refrigerator for vaccines";
  attrib Q1014    format=F00288_. label="Vaccine carriers";
  attrib Q1015    format=F00289_. label="Ice packs for vaccine carriers";
  attrib Q1016A   format=F00290_. label="Days/month TT for adolescent available at facility";
  attrib Q1016B   format=F00291_. label="Days/month TT for adolescent available through outreach";
  attrib Q1017    format=F00292_. label="National guidelines available for adolescent vaccinnations";
  attrib Q1018    format=F00293_. label="Observe national guidelines available";
  attrib Q1019    format=F00294_. label="Other guidelines for adolescent vaccinations";
  attrib Q1020    format=F00295_. label="Observe other guidelines";
  attrib Q1021    format=F00296_. label="TT vaccine available";
  attrib Q1022A   format=F00297_. label="Days/month TT for mother available at facility";
  attrib Q1022B   format=F00298_. label="Days/month TT for mother available through outreach";
  attrib Q1023    format=F00299_. label="National guidelines available for mother vaccinnations";
  attrib Q1024    format=F00300_. label="Observe national guidelines available";
  attrib Q1025    format=F00301_. label="Other guidelines for adolescent vaccinations";
  attrib Q1026    format=F00302_. label="Observe other guidelines";
  attrib Q1027    format=F00303_. label="TT vaccine available";
  attrib Q1050    format=F00304_. label="Data is already recorded for already assessed room/area";
  attrib Q1051_01 format=F00305_. label="Precaution and condition in client examination room(1)";
  attrib Q1051_02 format=F00305_. label="Precaution and condition in client examination room(2)";
  attrib Q1051_03 format=F00305_. label="Precaution and condition in client examination room(3)";
  attrib Q1051_04 format=F00305_. label="Precaution and condition in client examination room(4)";
  attrib Q1051_05 format=F00305_. label="Precaution and condition in client examination room(5)";
  attrib Q1051_06 format=F00305_. label="Precaution and condition in client examination room(6)";
  attrib Q1051_07 format=F00305_. label="Precaution and condition in client examination room(7)";
  attrib Q1051_08 format=F00305_. label="Precaution and condition in client examination room(8)";
  attrib Q1051_09 format=F00305_. label="Precaution and condition in client examination room(9)";
  attrib Q1051_10 format=F00305_. label="Precaution and condition in client examination room(10)";
  attrib Q1051_11 format=F00305_. label="Precaution and condition in client examination room(11)";
  attrib Q1051_12 format=F00305_. label="Precaution and condition in client examination room(12)";
  attrib Q1051_13 format=F00305_. label="Precaution and condition in client examination room(13)";
  attrib Q1052    format=F00306_. label="Setting of examination room";
  attrib Q1101A   label="Days/month child growth monitoring is available at facility";
  attrib Q1101B   label="Days/month immunization is available through outreach";
  attrib Q1102    format=F00307_. label="Guidelines for growth monitoring available";
  attrib Q1103    format=F00308_. label="Guidelines for growth monitoring observed";
  attrib Q1103A   format=F00309_. label="Guidelines for SAM and MAM available";
  attrib Q1103B   format=F00310_. label="Guidelines for SAM and MAM observed";
  attrib Q1104A_1 format=F00311_. label="Items available(1)";
  attrib Q1104A_2 format=F00311_. label="Items available(2)";
  attrib Q1104A_3 format=F00311_. label="Items available(3)";
  attrib Q1104A_4 format=F00311_. label="Items available(4)";
  attrib Q1104A_5 format=F00311_. label="Items available(5)";
  attrib Q1104A_6 format=F00311_. label="Items available(6)";
  attrib Q1104B_1 format=F00312_. label="Items functioning(1)";
  attrib Q1104B_2 format=F00312_. label="Items functioning(2)";
  attrib Q1104B_3 format=F00312_. label="Items functioning(3)";
  attrib Q1104B_4 format=F00312_. label="Items functioning(4)";
  attrib Q1104B_5 format=F00312_. label="Items functioning(5)";
  attrib Q1104B_6 format=F00312_. label="Items functioning(6)";
  attrib Q1201A   label="Days/month child curative care is available at facility";
  attrib Q1201B   label="Days/month child curative care is available through outreach";
  attrib Q1202_01 format=F00313_. label="Services provided at facility(1)";
  attrib Q1202_02 format=F00313_. label="Services provided at facility(2)";
  attrib Q1202_03 format=F00313_. label="Services provided at facility(3)";
  attrib Q1202_04 format=F00313_. label="Services provided at facility(4)";
  attrib Q1202_05 format=F00313_. label="Services provided at facility(5)";
  attrib Q1202_06 format=F00313_. label="Services provided at facility(6)";
  attrib Q1202_07 format=F00313_. label="Services provided at facility(7)";
  attrib Q1202_08 format=F00313_. label="Services provided at facility(8)";
  attrib Q1202_09 format=F00313_. label="Services provided at facility(9)";
  attrib Q1202_10 format=F00313_. label="Services provided at facility(10)";
  attrib Q1202_11 format=F00313_. label="Services provided at facility(11)";
  attrib Q1202_12 format=F00313_. label="Services provided at facility(12)";
  attrib Q1203    format=F00314_. label="Follow IMCI guidelines";
  attrib Q1203A   format=F00315_. label="Follow IMCI guidelines";
  attrib Q1204    format=F00316_. label="IMCI guidelines available";
  attrib Q1205    format=F00317_. label="IMCI guidelines observed";
  attrib Q1206    format=F00318_. label="Other guidelines available";
  attrib Q1207    format=F00319_. label="Other guidelines observed";
  attrib Q1208    format=F00320_. label="Routine sick child observations before consult";
  attrib Q1209_1  format=F00321_. label="Routine activities(1)";
  attrib Q1209_2  format=F00321_. label="Routine activities(2)";
  attrib Q1209_3  format=F00321_. label="Routine activities(3)";
  attrib Q1209_4  format=F00321_. label="Routine activities(4)";
  attrib Q1209_5  format=F00321_. label="Routine activities(5)";
  attrib Q1209_6  format=F00321_. label="Routine activities(6)";
  attrib Q1209_7  format=F00321_. label="Routine activities(7)";
  attrib Q1209_8  format=F00321_. label="Routine activities(8)";
  attrib Q1209_9  format=F00321_. label="Routine activities(9)";
  attrib Q1210A_01 format=F00322_. label="Items available(1)";
  attrib Q1210A_02 format=F00322_. label="Items available(2)";
  attrib Q1210A_03 format=F00322_. label="Items available(3)";
  attrib Q1210A_04 format=F00322_. label="Items available(4)";
  attrib Q1210A_05 format=F00322_. label="Items available(5)";
  attrib Q1210A_06 format=F00322_. label="Items available(6)";
  attrib Q1210A_07 format=F00322_. label="Items available(7)";
  attrib Q1210A_08 format=F00322_. label="Items available(8)";
  attrib Q1210A_09 format=F00322_. label="Items available(9)";
  attrib Q1210A_10 format=F00322_. label="Items available(10)";
  attrib Q1210A_11 format=F00322_. label="Items available(11)";
  attrib Q1210B_01 format=F00323_. label="Items functioning(1)";
  attrib Q1210B_02 format=F00323_. label="Items functioning(2)";
  attrib Q1210B_03 format=F00323_. label="Items functioning(3)";
  attrib Q1210B_04 format=F00323_. label="Items functioning(4)";
  attrib Q1210B_05 format=F00323_. label="Items functioning(5)";
  attrib Q1210B_06 format=F00323_. label="Items functioning(6)";
  attrib Q1210B_07 format=F00323_. label="Items functioning(7)";
  attrib Q1210B_08 format=F00323_. label="Items functioning(8)";
  attrib Q1210B_09 format=F00323_. label="Items functioning(9)";
  attrib Q1210B_10 format=F00323_. label="Items functioning(10)";
  attrib Q1210B_11 format=F00323_. label="Items functioning(11)";
  attrib Q1211_1  format=F00324_. label="Visual aids for sick child service(1)";
  attrib Q1211_2  format=F00324_. label="Visual aids for sick child service(2)";
  attrib Q1211_3  format=F00324_. label="Visual aids for sick child service(3)";
  attrib Q1211_4  format=F00324_. label="Visual aids for sick child service(4)";
  attrib Q1211_5  format=F00324_. label="Visual aids for sick child service(5)";
  attrib Q1212    format=F00325_. label="Individual health cards";
  attrib Q1213    format=F00326_. label="Observe Individual health cards";
  attrib Q1250    format=F00327_. label="Data is already recorded for already assessed room/area";
  attrib Q1251_01 format=F00328_. label="Precaution and condition in client examination room(1)";
  attrib Q1251_02 format=F00328_. label="Precaution and condition in client examination room(2)";
  attrib Q1251_03 format=F00328_. label="Precaution and condition in client examination room(3)";
  attrib Q1251_04 format=F00328_. label="Precaution and condition in client examination room(4)";
  attrib Q1251_05 format=F00328_. label="Precaution and condition in client examination room(5)";
  attrib Q1251_06 format=F00328_. label="Precaution and condition in client examination room(6)";
  attrib Q1251_07 format=F00328_. label="Precaution and condition in client examination room(7)";
  attrib Q1251_08 format=F00328_. label="Precaution and condition in client examination room(8)";
  attrib Q1251_09 format=F00328_. label="Precaution and condition in client examination room(9)";
  attrib Q1251_10 format=F00328_. label="Precaution and condition in client examination room(10)";
  attrib Q1251_11 format=F00328_. label="Precaution and condition in client examination room(11)";
  attrib Q1251_12 format=F00328_. label="Precaution and condition in client examination room(12)";
  attrib Q1251_13 format=F00328_. label="Precaution and condition in client examination room(13)";
  attrib Q1252    format=F00329_. label="Setting of examination room";
  attrib Q1254    format=F00330_. label="Dedicated room/area for adolescent health service";
  attrib Q1255    format=F00331_. label="Setting of examination room for adolescent";
  attrib Q1256    format=F00332_. label="Days per week adolescent health service available";
  attrib Q1257    format=F00333_. label="Adolescent health guidelines available";
  attrib Q1258    format=F00334_. label="Adolescent health  guidelines observed";
  attrib Q1259    format=F00335_. label="Other adolescent health guidelines available";
  attrib Q1260    format=F00336_. label="Other adolescent health guidelines observed";
  attrib Q1262    format=F00337_. label="Dedicated room/area for nutrition service";
  attrib Q1263    format=F00338_. label="Setting of examination room for nutrition service";
  attrib Q1264    format=F00339_. label="Days per week nutrition service available";
  attrib Q1265    format=F00340_. label="Nutrition service guidelines available";
  attrib Q1266    format=F00341_. label="Nutrition service guidelines observed";
  attrib Q1267    format=F00342_. label="Other nutrition service guidelines available";
  attrib Q1268    format=F00343_. label="Other nutrition service guidelines observed";
  attrib Q1301    label="Number of days family planning services offered at facility";
  attrib Q1302_01 format=F00344_. label="Family planning methods provided/counseled(1)";
  attrib Q1302_02 format=F00344_. label="Family planning methods provided/counseled(2)";
  attrib Q1302_03 format=F00344_. label="Family planning methods provided/counseled(3)";
  attrib Q1302_04 format=F00344_. label="Family planning methods provided/counseled(4)";
  attrib Q1302_05 format=F00344_. label="Family planning methods provided/counseled(5)";
  attrib Q1302_06 format=F00344_. label="Family planning methods provided/counseled(6)";
  attrib Q1302_07 format=F00344_. label="Family planning methods provided/counseled(7)";
  attrib Q1302_08 format=F00344_. label="Family planning methods provided/counseled(8)";
  attrib Q1302_09 format=F00344_. label="Family planning methods provided/counseled(9)";
  attrib Q1302_10 format=F00344_. label="Family planning methods provided/counseled(10)";
  attrib Q1302_11 format=F00344_. label="Family planning methods provided/counseled(11)";
  attrib Q1302_12 format=F00344_. label="Family planning methods provided/counseled(12)";
  attrib Q1302_13 format=F00344_. label="Family planning methods provided/counseled(13)";
  attrib Q1302_14 format=F00344_. label="Family planning methods provided/counseled(14)";
  attrib Q1302_15 format=F00344_. label="Family planning methods provided/counseled(15)";
  attrib Q1303    format=F00345_. label="FP guidelines/manual";
  attrib Q1304    format=F00346_. label="Observe FP guidelines/manual";
  attrib Q1305    format=F00347_. label="Other guidelines";
  attrib Q1306    format=F00348_. label="Observe other guidelines";
  attrib Q1307    format=F00349_. label="Individual records/cards for clients";
  attrib Q1308    format=F00350_. label="Observe Individual records/cards for clients";
  attrib Q1309    format=F00351_. label="Routine actions for clients";
  attrib Q1310_1  format=F00352_. label="Type of routine actions(1)";
  attrib Q1310_2  format=F00352_. label="Type of routine actions(2)";
  attrib Q1310_3  format=F00352_. label="Type of routine actions(3)";
  attrib Q1311    format=F00353_. label="FP providers routeinly dx/tx STIs";
  attrib Q1314A_1 format=F00354_. label="FP equipment available(1)";
  attrib Q1314A_2 format=F00354_. label="FP equipment available(2)";
  attrib Q1314A_3 format=F00354_. label="FP equipment available(3)";
  attrib Q1314A_4 format=F00354_. label="FP equipment available(4)";
  attrib Q1314A_5 format=F00354_. label="FP equipment available(5)";
  attrib Q1314A_6 format=F00354_. label="FP equipment available(6)";
  attrib Q1314A_7 format=F00354_. label="FP equipment available(7)";
  attrib Q1314A_8 format=F00354_. label="FP equipment available(8)";
  attrib Q1314A_9 format=F00354_. label="FP equipment available(9)";
  attrib Q1314B_1 format=F00355_. label="FP equipment functioning(1)";
  attrib Q1314B_2 format=F00355_. label="FP equipment functioning(2)";
  attrib Q1314B_3 format=F00355_. label="FP equipment functioning(3)";
  attrib Q1314B_4 format=F00355_. label="FP equipment functioning(4)";
  attrib Q1314B_5 format=F00355_. label="FP equipment functioning(5)";
  attrib Q1314B_6 format=F00355_. label="FP equipment functioning(6)";
  attrib Q1314B_7 format=F00355_. label="FP equipment functioning(7)";
  attrib Q1314B_8 format=F00355_. label="FP equipment functioning(8)";
  attrib Q1314B_9 format=F00355_. label="FP equipment functioning(9)";
  attrib Q1316_1  format=F00356_. label="IUD/implant equipment(1)";
  attrib Q1316_2  format=F00356_. label="IUD/implant equipment(2)";
  attrib Q1316_3  format=F00356_. label="IUD/implant equipment(3)";
  attrib Q1316_4  format=F00356_. label="IUD/implant equipment(4)";
  attrib Q1316_5  format=F00356_. label="IUD/implant equipment(5)";
  attrib Q1318_01 format=F00357_. label="IUD equipment(1)";
  attrib Q1318_02 format=F00357_. label="IUD equipment(2)";
  attrib Q1318_03 format=F00357_. label="IUD equipment(3)";
  attrib Q1318_04 format=F00357_. label="IUD equipment(4)";
  attrib Q1318_05 format=F00357_. label="IUD equipment(5)";
  attrib Q1318_06 format=F00357_. label="IUD equipment(6)";
  attrib Q1318_07 format=F00357_. label="IUD equipment(7)";
  attrib Q1318_08 format=F00357_. label="IUD equipment(8)";
  attrib Q1318_09 format=F00357_. label="IUD equipment(9)";
  attrib Q1318_10 format=F00357_. label="IUD equipment(10)";
  attrib Q1318_11 format=F00357_. label="IUD equipment(11)";
  attrib Q1318_12 format=F00357_. label="IUD equipment(12)";
  attrib Q1320_01 format=F00358_. label="Implant equipment(1)";
  attrib Q1320_02 format=F00358_. label="Implant equipment(2)";
  attrib Q1320_03 format=F00358_. label="Implant equipment(3)";
  attrib Q1320_04 format=F00358_. label="Implant equipment(4)";
  attrib Q1320_05 format=F00358_. label="Implant equipment(5)";
  attrib Q1320_06 format=F00358_. label="Implant equipment(6)";
  attrib Q1320_07 format=F00358_. label="Implant equipment(7)";
  attrib Q1320_08 format=F00358_. label="Implant equipment(8)";
  attrib Q1320_09 format=F00358_. label="Implant equipment(9)";
  attrib Q1320_10 format=F00358_. label="Implant equipment(10)";
  attrib Q1320_11 format=F00358_. label="Implant equipment(11)";
  attrib Q1320_12 format=F00358_. label="Implant equipment(12)";
  attrib Q1321    format=F00359_. label="Equipment placed in the FP services processed for re-use";
  attrib Q1322    length=$6 format=$F00360_. label="Final processing method used for FP equipment";
  attrib Q1350    format=F00361_. label="Data is already recorded for already assessed room/area";
  attrib Q1351_01 format=F00362_. label="Precaution and condition in client examination room(1)";
  attrib Q1351_02 format=F00362_. label="Precaution and condition in client examination room(2)";
  attrib Q1351_03 format=F00362_. label="Precaution and condition in client examination room(3)";
  attrib Q1351_04 format=F00362_. label="Precaution and condition in client examination room(4)";
  attrib Q1351_05 format=F00362_. label="Precaution and condition in client examination room(5)";
  attrib Q1351_06 format=F00362_. label="Precaution and condition in client examination room(6)";
  attrib Q1351_07 format=F00362_. label="Precaution and condition in client examination room(7)";
  attrib Q1351_08 format=F00362_. label="Precaution and condition in client examination room(8)";
  attrib Q1351_09 format=F00362_. label="Precaution and condition in client examination room(9)";
  attrib Q1351_10 format=F00362_. label="Precaution and condition in client examination room(10)";
  attrib Q1351_11 format=F00362_. label="Precaution and condition in client examination room(11)";
  attrib Q1351_12 format=F00362_. label="Precaution and condition in client examination room(12)";
  attrib Q1351_13 format=F00362_. label="Precaution and condition in client examination room(13)";
  attrib Q1352    format=F00363_. label="Setting of FP service room";
  attrib Q1353B   format=F00364_. label="Room/area for Vasectomy already assessed room/area";
  attrib Q1353CA_01 format=F00365_. label="Vasectomy equipment available(1)";
  attrib Q1353CA_02 format=F00365_. label="Vasectomy equipment available(2)";
  attrib Q1353CA_03 format=F00365_. label="Vasectomy equipment available(3)";
  attrib Q1353CA_04 format=F00365_. label="Vasectomy equipment available(4)";
  attrib Q1353CA_05 format=F00365_. label="Vasectomy equipment available(5)";
  attrib Q1353CA_06 format=F00365_. label="Vasectomy equipment available(6)";
  attrib Q1353CA_07 format=F00365_. label="Vasectomy equipment available(7)";
  attrib Q1353CA_08 format=F00365_. label="Vasectomy equipment available(8)";
  attrib Q1353CA_09 format=F00365_. label="Vasectomy equipment available(9)";
  attrib Q1353CA_10 format=F00365_. label="Vasectomy equipment available(10)";
  attrib Q1353CA_11 format=F00365_. label="Vasectomy equipment available(11)";
  attrib Q1353CA_12 format=F00365_. label="Vasectomy equipment available(12)";
  attrib Q1353CA_13 format=F00365_. label="Vasectomy equipment available(13)";
  attrib Q1353CA_14 format=F00365_. label="Vasectomy equipment available(14)";
  attrib Q1353CA_15 format=F00365_. label="Vasectomy equipment available(15)";
  attrib Q1353CA_16 format=F00365_. label="Vasectomy equipment available(16)";
  attrib Q1353CA_17 format=F00365_. label="Vasectomy equipment available(17)";
  attrib Q1353CA_18 format=F00365_. label="Vasectomy equipment available(18)";
  attrib Q1353CA_19 format=F00365_. label="Vasectomy equipment available(19)";
  attrib Q1353CA_20 format=F00365_. label="Vasectomy equipment available(20)";
  attrib Q1353CA_21 format=F00365_. label="Vasectomy equipment available(21)";
  attrib Q1353CA_22 format=F00365_. label="Vasectomy equipment available(22)";
  attrib Q1353CA_23 format=F00365_. label="Vasectomy equipment available(23)";
  attrib Q1353CA_24 format=F00365_. label="Vasectomy equipment available(24)";
  attrib Q1353CA_25 format=F00365_. label="Vasectomy equipment available(25)";
  attrib Q1353CA_26 format=F00365_. label="Vasectomy equipment available(26)";
  attrib Q1353CA_27 format=F00365_. label="Vasectomy equipment available(27)";
  attrib Q1353CA_28 format=F00365_. label="Vasectomy equipment available(28)";
  attrib Q1353CA_29 format=F00365_. label="Vasectomy equipment available(29)";
  attrib Q1353CA_30 format=F00365_. label="Vasectomy equipment available(30)";
  attrib Q1353CA_31 format=F00365_. label="Vasectomy equipment available(31)";
  attrib Q1353CA_32 format=F00365_. label="Vasectomy equipment available(32)";
  attrib Q1353CA_33 format=F00365_. label="Vasectomy equipment available(33)";
  attrib Q1353CA_34 format=F00365_. label="Vasectomy equipment available(34)";
  attrib Q1353CA_35 format=F00365_. label="Vasectomy equipment available(35)";
  attrib Q1353CA_36 format=F00365_. label="Vasectomy equipment available(36)";
  attrib Q1353CA_37 format=F00365_. label="Vasectomy equipment available(37)";
  attrib Q1353CA_38 format=F00365_. label="Vasectomy equipment available(38)";
  attrib Q1353CA_39 format=F00365_. label="Vasectomy equipment available(39)";
  attrib Q1353CA_40 format=F00365_. label="Vasectomy equipment available(40)";
  attrib Q1353CA_41 format=F00365_. label="Vasectomy equipment available(41)";
  attrib Q1353CA_42 format=F00365_. label="Vasectomy equipment available(42)";
  attrib Q1353CA_43 format=F00365_. label="Vasectomy equipment available(43)";
  attrib Q1353CA_44 format=F00365_. label="Vasectomy equipment available(44)";
  attrib Q1353CB_01 format=F00366_. label="Vasectomy equipment functioning(1)";
  attrib Q1353CB_02 format=F00366_. label="Vasectomy equipment functioning(2)";
  attrib Q1353CB_03 format=F00366_. label="Vasectomy equipment functioning(3)";
  attrib Q1353CB_04 format=F00366_. label="Vasectomy equipment functioning(4)";
  attrib Q1353CB_05 format=F00366_. label="Vasectomy equipment functioning(5)";
  attrib Q1353CB_06 format=F00366_. label="Vasectomy equipment functioning(6)";
  attrib Q1353CB_07 format=F00366_. label="Vasectomy equipment functioning(7)";
  attrib Q1353CB_08 format=F00366_. label="Vasectomy equipment functioning(8)";
  attrib Q1353CB_09 format=F00366_. label="Vasectomy equipment functioning(9)";
  attrib Q1353CB_10 format=F00366_. label="Vasectomy equipment functioning(10)";
  attrib Q1353CB_11 format=F00366_. label="Vasectomy equipment functioning(11)";
  attrib Q1353CB_12 format=F00366_. label="Vasectomy equipment functioning(12)";
  attrib Q1353CB_13 format=F00366_. label="Vasectomy equipment functioning(13)";
  attrib Q1353CB_14 format=F00366_. label="Vasectomy equipment functioning(14)";
  attrib Q1353CB_15 format=F00366_. label="Vasectomy equipment functioning(15)";
  attrib Q1353CB_16 format=F00366_. label="Vasectomy equipment functioning(16)";
  attrib Q1353CB_17 format=F00366_. label="Vasectomy equipment functioning(17)";
  attrib Q1353CB_18 format=F00366_. label="Vasectomy equipment functioning(18)";
  attrib Q1353CB_19 format=F00366_. label="Vasectomy equipment functioning(19)";
  attrib Q1353CB_20 format=F00366_. label="Vasectomy equipment functioning(20)";
  attrib Q1353CB_21 format=F00366_. label="Vasectomy equipment functioning(21)";
  attrib Q1353CB_22 format=F00366_. label="Vasectomy equipment functioning(22)";
  attrib Q1353CB_23 format=F00366_. label="Vasectomy equipment functioning(23)";
  attrib Q1353CB_24 format=F00366_. label="Vasectomy equipment functioning(24)";
  attrib Q1353CB_25 format=F00366_. label="Vasectomy equipment functioning(25)";
  attrib Q1353CB_26 format=F00366_. label="Vasectomy equipment functioning(26)";
  attrib Q1353CB_27 format=F00366_. label="Vasectomy equipment functioning(27)";
  attrib Q1353CB_28 format=F00366_. label="Vasectomy equipment functioning(28)";
  attrib Q1353CB_29 format=F00366_. label="Vasectomy equipment functioning(29)";
  attrib Q1353CB_30 format=F00366_. label="Vasectomy equipment functioning(30)";
  attrib Q1353CB_31 format=F00366_. label="Vasectomy equipment functioning(31)";
  attrib Q1353CB_32 format=F00366_. label="Vasectomy equipment functioning(32)";
  attrib Q1353CB_33 format=F00366_. label="Vasectomy equipment functioning(33)";
  attrib Q1353CB_34 format=F00366_. label="Vasectomy equipment functioning(34)";
  attrib Q1353CB_35 format=F00366_. label="Vasectomy equipment functioning(35)";
  attrib Q1353CB_36 format=F00366_. label="Vasectomy equipment functioning(36)";
  attrib Q1353CB_37 format=F00366_. label="Vasectomy equipment functioning(37)";
  attrib Q1353CB_38 format=F00366_. label="Vasectomy equipment functioning(38)";
  attrib Q1353CB_39 format=F00366_. label="Vasectomy equipment functioning(39)";
  attrib Q1353CB_40 format=F00366_. label="Vasectomy equipment functioning(40)";
  attrib Q1353CB_41 format=F00366_. label="Vasectomy equipment functioning(41)";
  attrib Q1353CB_42 format=F00366_. label="Vasectomy equipment functioning(42)";
  attrib Q1353CB_43 format=F00366_. label="Vasectomy equipment functioning(43)";
  attrib Q1353CB_44 format=F00366_. label="Vasectomy equipment functioning(44)";
  attrib Q1353DA_1 format=F00367_. label="Vasectomy items available(1)";
  attrib Q1353DA_2 format=F00367_. label="Vasectomy items available(2)";
  attrib Q1353DA_3 format=F00367_. label="Vasectomy items available(3)";
  attrib Q1353DA_4 format=F00367_. label="Vasectomy items available(4)";
  attrib Q1353DA_5 format=F00367_. label="Vasectomy items available(5)";
  attrib Q1353DB_1 format=F00368_. label="Vasectomy  items functioning(1)";
  attrib Q1353DB_2 format=F00368_. label="Vasectomy  items functioning(2)";
  attrib Q1353DB_3 format=F00368_. label="Vasectomy  items functioning(3)";
  attrib Q1353DB_4 format=F00368_. label="Vasectomy  items functioning(4)";
  attrib Q1353DB_5 format=F00368_. label="Vasectomy  items functioning(5)";
  attrib Q1353E_1 format=F00369_. label="Vasectomymedicine available(1)";
  attrib Q1353E_2 format=F00369_. label="Vasectomymedicine available(2)";
  attrib Q1353E_3 format=F00369_. label="Vasectomymedicine available(3)";
  attrib Q1353E_4 format=F00369_. label="Vasectomymedicine available(4)";
  attrib Q1353G   format=F00370_. label="Room/area for Tubal Ligation already assessed room/area";
  attrib Q1353HA_01 format=F00371_. label="Tubal Ligation equipment available(1)";
  attrib Q1353HA_02 format=F00371_. label="Tubal Ligation equipment available(2)";
  attrib Q1353HA_03 format=F00371_. label="Tubal Ligation equipment available(3)";
  attrib Q1353HA_04 format=F00371_. label="Tubal Ligation equipment available(4)";
  attrib Q1353HA_05 format=F00371_. label="Tubal Ligation equipment available(5)";
  attrib Q1353HA_06 format=F00371_. label="Tubal Ligation equipment available(6)";
  attrib Q1353HA_07 format=F00371_. label="Tubal Ligation equipment available(7)";
  attrib Q1353HA_08 format=F00371_. label="Tubal Ligation equipment available(8)";
  attrib Q1353HA_09 format=F00371_. label="Tubal Ligation equipment available(9)";
  attrib Q1353HA_10 format=F00371_. label="Tubal Ligation equipment available(10)";
  attrib Q1353HA_11 format=F00371_. label="Tubal Ligation equipment available(11)";
  attrib Q1353HA_12 format=F00371_. label="Tubal Ligation equipment available(12)";
  attrib Q1353HA_13 format=F00371_. label="Tubal Ligation equipment available(13)";
  attrib Q1353HA_14 format=F00371_. label="Tubal Ligation equipment available(14)";
  attrib Q1353HA_15 format=F00371_. label="Tubal Ligation equipment available(15)";
  attrib Q1353HA_16 format=F00371_. label="Tubal Ligation equipment available(16)";
  attrib Q1353HA_17 format=F00371_. label="Tubal Ligation equipment available(17)";
  attrib Q1353HA_18 format=F00371_. label="Tubal Ligation equipment available(18)";
  attrib Q1353HA_19 format=F00371_. label="Tubal Ligation equipment available(19)";
  attrib Q1353HA_20 format=F00371_. label="Tubal Ligation equipment available(20)";
  attrib Q1353HA_21 format=F00371_. label="Tubal Ligation equipment available(21)";
  attrib Q1353HA_22 format=F00371_. label="Tubal Ligation equipment available(22)";
  attrib Q1353HA_23 format=F00371_. label="Tubal Ligation equipment available(23)";
  attrib Q1353HA_24 format=F00371_. label="Tubal Ligation equipment available(24)";
  attrib Q1353HA_25 format=F00371_. label="Tubal Ligation equipment available(25)";
  attrib Q1353HA_26 format=F00371_. label="Tubal Ligation equipment available(26)";
  attrib Q1353HA_27 format=F00371_. label="Tubal Ligation equipment available(27)";
  attrib Q1353HA_28 format=F00371_. label="Tubal Ligation equipment available(28)";
  attrib Q1353HA_29 format=F00371_. label="Tubal Ligation equipment available(29)";
  attrib Q1353HA_30 format=F00371_. label="Tubal Ligation equipment available(30)";
  attrib Q1353HA_31 format=F00371_. label="Tubal Ligation equipment available(31)";
  attrib Q1353HA_32 format=F00371_. label="Tubal Ligation equipment available(32)";
  attrib Q1353HA_33 format=F00371_. label="Tubal Ligation equipment available(33)";
  attrib Q1353HA_34 format=F00371_. label="Tubal Ligation equipment available(34)";
  attrib Q1353HA_35 format=F00371_. label="Tubal Ligation equipment available(35)";
  attrib Q1353HA_36 format=F00371_. label="Tubal Ligation equipment available(36)";
  attrib Q1353HA_37 format=F00371_. label="Tubal Ligation equipment available(37)";
  attrib Q1353HA_38 format=F00371_. label="Tubal Ligation equipment available(38)";
  attrib Q1353HA_39 format=F00371_. label="Tubal Ligation equipment available(39)";
  attrib Q1353HA_40 format=F00371_. label="Tubal Ligation equipment available(40)";
  attrib Q1353HA_41 format=F00371_. label="Tubal Ligation equipment available(41)";
  attrib Q1353HA_42 format=F00371_. label="Tubal Ligation equipment available(42)";
  attrib Q1353HA_43 format=F00371_. label="Tubal Ligation equipment available(43)";
  attrib Q1353HA_44 format=F00371_. label="Tubal Ligation equipment available(44)";
  attrib Q1353HB_01 format=F00372_. label="Tubal Ligation equipment functioning(1)";
  attrib Q1353HB_02 format=F00372_. label="Tubal Ligation equipment functioning(2)";
  attrib Q1353HB_03 format=F00372_. label="Tubal Ligation equipment functioning(3)";
  attrib Q1353HB_04 format=F00372_. label="Tubal Ligation equipment functioning(4)";
  attrib Q1353HB_05 format=F00372_. label="Tubal Ligation equipment functioning(5)";
  attrib Q1353HB_06 format=F00372_. label="Tubal Ligation equipment functioning(6)";
  attrib Q1353HB_07 format=F00372_. label="Tubal Ligation equipment functioning(7)";
  attrib Q1353HB_08 format=F00372_. label="Tubal Ligation equipment functioning(8)";
  attrib Q1353HB_09 format=F00372_. label="Tubal Ligation equipment functioning(9)";
  attrib Q1353HB_10 format=F00372_. label="Tubal Ligation equipment functioning(10)";
  attrib Q1353HB_11 format=F00372_. label="Tubal Ligation equipment functioning(11)";
  attrib Q1353HB_12 format=F00372_. label="Tubal Ligation equipment functioning(12)";
  attrib Q1353HB_13 format=F00372_. label="Tubal Ligation equipment functioning(13)";
  attrib Q1353HB_14 format=F00372_. label="Tubal Ligation equipment functioning(14)";
  attrib Q1353HB_15 format=F00372_. label="Tubal Ligation equipment functioning(15)";
  attrib Q1353HB_16 format=F00372_. label="Tubal Ligation equipment functioning(16)";
  attrib Q1353HB_17 format=F00372_. label="Tubal Ligation equipment functioning(17)";
  attrib Q1353HB_18 format=F00372_. label="Tubal Ligation equipment functioning(18)";
  attrib Q1353HB_19 format=F00372_. label="Tubal Ligation equipment functioning(19)";
  attrib Q1353HB_20 format=F00372_. label="Tubal Ligation equipment functioning(20)";
  attrib Q1353HB_21 format=F00372_. label="Tubal Ligation equipment functioning(21)";
  attrib Q1353HB_22 format=F00372_. label="Tubal Ligation equipment functioning(22)";
  attrib Q1353HB_23 format=F00372_. label="Tubal Ligation equipment functioning(23)";
  attrib Q1353HB_24 format=F00372_. label="Tubal Ligation equipment functioning(24)";
  attrib Q1353HB_25 format=F00372_. label="Tubal Ligation equipment functioning(25)";
  attrib Q1353HB_26 format=F00372_. label="Tubal Ligation equipment functioning(26)";
  attrib Q1353HB_27 format=F00372_. label="Tubal Ligation equipment functioning(27)";
  attrib Q1353HB_28 format=F00372_. label="Tubal Ligation equipment functioning(28)";
  attrib Q1353HB_29 format=F00372_. label="Tubal Ligation equipment functioning(29)";
  attrib Q1353HB_30 format=F00372_. label="Tubal Ligation equipment functioning(30)";
  attrib Q1353HB_31 format=F00372_. label="Tubal Ligation equipment functioning(31)";
  attrib Q1353HB_32 format=F00372_. label="Tubal Ligation equipment functioning(32)";
  attrib Q1353HB_33 format=F00372_. label="Tubal Ligation equipment functioning(33)";
  attrib Q1353HB_34 format=F00372_. label="Tubal Ligation equipment functioning(34)";
  attrib Q1353HB_35 format=F00372_. label="Tubal Ligation equipment functioning(35)";
  attrib Q1353HB_36 format=F00372_. label="Tubal Ligation equipment functioning(36)";
  attrib Q1353HB_37 format=F00372_. label="Tubal Ligation equipment functioning(37)";
  attrib Q1353HB_38 format=F00372_. label="Tubal Ligation equipment functioning(38)";
  attrib Q1353HB_39 format=F00372_. label="Tubal Ligation equipment functioning(39)";
  attrib Q1353HB_40 format=F00372_. label="Tubal Ligation equipment functioning(40)";
  attrib Q1353HB_41 format=F00372_. label="Tubal Ligation equipment functioning(41)";
  attrib Q1353HB_42 format=F00372_. label="Tubal Ligation equipment functioning(42)";
  attrib Q1353HB_43 format=F00372_. label="Tubal Ligation equipment functioning(43)";
  attrib Q1353HB_44 format=F00372_. label="Tubal Ligation equipment functioning(44)";
  attrib Q1353IA_01 format=F00373_. label="Tubal Ligation items available(1)";
  attrib Q1353IA_02 format=F00373_. label="Tubal Ligation items available(2)";
  attrib Q1353IA_03 format=F00373_. label="Tubal Ligation items available(3)";
  attrib Q1353IA_04 format=F00373_. label="Tubal Ligation items available(4)";
  attrib Q1353IA_05 format=F00373_. label="Tubal Ligation items available(5)";
  attrib Q1353IA_06 format=F00373_. label="Tubal Ligation items available(6)";
  attrib Q1353IA_07 format=F00373_. label="Tubal Ligation items available(7)";
  attrib Q1353IA_08 format=F00373_. label="Tubal Ligation items available(8)";
  attrib Q1353IA_09 format=F00373_. label="Tubal Ligation items available(9)";
  attrib Q1353IA_10 format=F00373_. label="Tubal Ligation items available(10)";
  attrib Q1353IA_11 format=F00373_. label="Tubal Ligation items available(11)";
  attrib Q1353IA_12 format=F00373_. label="Tubal Ligation items available(12)";
  attrib Q1353IA_13 format=F00373_. label="Tubal Ligation items available(13)";
  attrib Q1353IA_14 format=F00373_. label="Tubal Ligation items available(14)";
  attrib Q1353IA_15 format=F00373_. label="Tubal Ligation items available(15)";
  attrib Q1353IA_16 format=F00373_. label="Tubal Ligation items available(16)";
  attrib Q1353IA_17 format=F00373_. label="Tubal Ligation items available(17)";
  attrib Q1353IB_01 format=F00374_. label="Tubal Ligation items functioning(1)";
  attrib Q1353IB_02 format=F00374_. label="Tubal Ligation items functioning(2)";
  attrib Q1353IB_03 format=F00374_. label="Tubal Ligation items functioning(3)";
  attrib Q1353IB_04 format=F00374_. label="Tubal Ligation items functioning(4)";
  attrib Q1353IB_05 format=F00374_. label="Tubal Ligation items functioning(5)";
  attrib Q1353IB_06 format=F00374_. label="Tubal Ligation items functioning(6)";
  attrib Q1353IB_07 format=F00374_. label="Tubal Ligation items functioning(7)";
  attrib Q1353IB_08 format=F00374_. label="Tubal Ligation items functioning(8)";
  attrib Q1353IB_09 format=F00374_. label="Tubal Ligation items functioning(9)";
  attrib Q1353IB_10 format=F00374_. label="Tubal Ligation items functioning(10)";
  attrib Q1353IB_11 format=F00374_. label="Tubal Ligation items functioning(11)";
  attrib Q1353IB_12 format=F00374_. label="Tubal Ligation items functioning(12)";
  attrib Q1353IB_13 format=F00374_. label="Tubal Ligation items functioning(13)";
  attrib Q1353IB_14 format=F00374_. label="Tubal Ligation items functioning(14)";
  attrib Q1353IB_15 format=F00374_. label="Tubal Ligation items functioning(15)";
  attrib Q1353IB_16 format=F00374_. label="Tubal Ligation items functioning(16)";
  attrib Q1353IB_17 format=F00374_. label="Tubal Ligation items functioning(17)";
  attrib Q1353J_1 format=F00375_. label="Tubal Ligation medicine available(1)";
  attrib Q1353J_2 format=F00375_. label="Tubal Ligation medicine available(2)";
  attrib Q1353J_3 format=F00375_. label="Tubal Ligation medicine available(3)";
  attrib Q1353J_4 format=F00375_. label="Tubal Ligation medicine available(4)";
  attrib Q1353J_5 format=F00375_. label="Tubal Ligation medicine available(5)";
  attrib Q1353J_6 format=F00375_. label="Tubal Ligation medicine available(6)";
  attrib Q1353J_7 format=F00375_. label="Tubal Ligation medicine available(7)";
  attrib Q1353J_8 format=F00375_. label="Tubal Ligation medicine available(8)";
  attrib Q1353J_9 format=F00375_. label="Tubal Ligation medicine available(9)";
  attrib Q1401    label="Number of days ANC available in facility";
  attrib Q1402_1  format=F00376_. label="Services offered(1)";
  attrib Q1402_2  format=F00376_. label="Services offered(2)";
  attrib Q1402_3  format=F00376_. label="Services offered(3)";
  attrib Q1402_4  format=F00376_. label="Services offered(4)";
  attrib Q1402_5  format=F00376_. label="Services offered(5)";
  attrib Q1402_6  format=F00376_. label="Services offered(6)";
  attrib Q1406_1  format=F00377_. label="Tests offered(1)";
  attrib Q1406_2  format=F00377_. label="Tests offered(2)";
  attrib Q1406_3  format=F00377_. label="Tests offered(3)";
  attrib Q1406_4  format=F00377_. label="Tests offered(4)";
  attrib Q1406_5  format=F00377_. label="Tests offered(5)";
  attrib Q1406_6  format=F00377_. label="Tests offered(6)";
  attrib Q1408    format=F00378_. label="ANC providers routinely dx,tx RTIs/STIs";
  attrib Q1409    format=F00379_. label="ANC guidelines/manuals";
  attrib Q1410    format=F00380_. label="ANC guidelines/manual observed";
  attrib Q1411    format=F00381_. label="Other ANC guidelines/forms/register";
  attrib Q1412    format=F00382_. label="Other ANC guidelines/forms/register observed";
  attrib Q1415    format=F00383_. label="Visual aids for client education";
  attrib Q1416    format=F00384_. label="Visual aids for client education observed";
  attrib Q1417    format=F00385_. label="Individual cards maintained";
  attrib Q1418    format=F00386_. label="Individual cards maintained observed";
  attrib Q1419    format=F00387_. label="System of routine actions for clients";
  attrib Q1420_01 format=F00388_. label="Specific routine actions(1)";
  attrib Q1420_02 format=F00388_. label="Specific routine actions(2)";
  attrib Q1420_03 format=F00388_. label="Specific routine actions(3)";
  attrib Q1420_04 format=F00388_. label="Specific routine actions(4)";
  attrib Q1420_05 format=F00388_. label="Specific routine actions(5)";
  attrib Q1420_06 format=F00388_. label="Specific routine actions(6)";
  attrib Q1420_07 format=F00388_. label="Specific routine actions(7)";
  attrib Q1420_08 format=F00388_. label="Specific routine actions(8)";
  attrib Q1420_09 format=F00388_. label="Specific routine actions(9)";
  attrib Q1420_10 format=F00388_. label="Specific routine actions(10)";
  attrib Q1420_11 format=F00388_. label="Specific routine actions(11)";
  attrib Q1420_12 format=F00388_. label="Specific routine actions(12)";
  attrib Q1421A_1 format=F00389_. label="ANC equipment available(1)";
  attrib Q1421A_2 format=F00389_. label="ANC equipment available(2)";
  attrib Q1421A_3 format=F00389_. label="ANC equipment available(3)";
  attrib Q1421A_4 format=F00389_. label="ANC equipment available(4)";
  attrib Q1421A_5 format=F00389_. label="ANC equipment available(5)";
  attrib Q1421A_6 format=F00389_. label="ANC equipment available(6)";
  attrib Q1421A_7 format=F00389_. label="ANC equipment available(7)";
  attrib Q1421A_8 format=F00389_. label="ANC equipment available(8)";
  attrib Q1421B_1 format=F00390_. label="ANC equipment functioning(1)";
  attrib Q1421B_2 format=F00390_. label="ANC equipment functioning(2)";
  attrib Q1421B_3 format=F00390_. label="ANC equipment functioning(3)";
  attrib Q1421B_4 format=F00390_. label="ANC equipment functioning(4)";
  attrib Q1421B_5 format=F00390_. label="ANC equipment functioning(5)";
  attrib Q1421B_6 format=F00390_. label="ANC equipment functioning(6)";
  attrib Q1421B_7 format=F00390_. label="ANC equipment functioning(7)";
  attrib Q1421B_8 format=F00390_. label="ANC equipment functioning(8)";
  attrib Q1422_1  format=F00391_. label="Medicine available today(1)";
  attrib Q1422_2  format=F00391_. label="Medicine available today(2)";
  attrib Q1422_3  format=F00391_. label="Medicine available today(3)";
  attrib Q1422_4  format=F00391_. label="Medicine available today(4)";
  attrib Q1422_5  format=F00391_. label="Medicine available today(5)";
  attrib Q1422_6  format=F00391_. label="Medicine available today(6)";
  attrib Q1422_7  format=F00391_. label="Medicine available today(7)";
  attrib Q1450    format=F00392_. label="Data is already recorded for already assessed room/area";
  attrib Q1451_01 format=F00393_. label="Precaution and condition in client examination room(1)";
  attrib Q1451_02 format=F00393_. label="Precaution and condition in client examination room(2)";
  attrib Q1451_03 format=F00393_. label="Precaution and condition in client examination room(3)";
  attrib Q1451_04 format=F00393_. label="Precaution and condition in client examination room(4)";
  attrib Q1451_05 format=F00393_. label="Precaution and condition in client examination room(5)";
  attrib Q1451_06 format=F00393_. label="Precaution and condition in client examination room(6)";
  attrib Q1451_07 format=F00393_. label="Precaution and condition in client examination room(7)";
  attrib Q1451_08 format=F00393_. label="Precaution and condition in client examination room(8)";
  attrib Q1451_09 format=F00393_. label="Precaution and condition in client examination room(9)";
  attrib Q1451_10 format=F00393_. label="Precaution and condition in client examination room(10)";
  attrib Q1451_11 format=F00393_. label="Precaution and condition in client examination room(11)";
  attrib Q1451_12 format=F00393_. label="Precaution and condition in client examination room(12)";
  attrib Q1451_13 format=F00393_. label="Precaution and condition in client examination room(13)";
  attrib Q1452    format=F00394_. label="Setting of ANC service";
  attrib Q1454    format=F00395_. label="Dedicated room/area for PNC service";
  attrib Q1455    format=F00396_. label="Setting of examination room for PNC service";
  attrib Q1456    format=F00397_. label="Days per week PNC service available";
  attrib Q1457    format=F00398_. label="PNC service guidelines available";
  attrib Q1458    format=F00399_. label="PNC service guidelines observed";
  attrib Q1459    format=F00400_. label="Other PNC service guidelines available";
  attrib Q1460    format=F00401_. label="Other PNC service guidelines observed";
  attrib Q1461    format=F00402_. label="Postpartum FP guidelines available";
  attrib Q1462    format=F00403_. label="Postpartum FP guidelines observed";
  attrib Q1601    format=F00404_. label="24-hour staff in facility or on-call";
  attrib Q1602    format=F00405_. label="Duty schedule/call list for 24-hour staff";
  attrib Q1603    format=F00406_. label="Duty schedule/call list for 24-hour staff observed";
  attrib Q1604A_1 format=F00407_. label="Interventions ever carried out in facilty(1)";
  attrib Q1604A_2 format=F00407_. label="Interventions ever carried out in facilty(2)";
  attrib Q1604A_3 format=F00407_. label="Interventions ever carried out in facilty(3)";
  attrib Q1604A_4 format=F00407_. label="Interventions ever carried out in facilty(4)";
  attrib Q1604A_5 format=F00407_. label="Interventions ever carried out in facilty(5)";
  attrib Q1604A_6 format=F00407_. label="Interventions ever carried out in facilty(6)";
  attrib Q1604A_7 format=F00407_. label="Interventions ever carried out in facilty(7)";
  attrib Q1604A_8 format=F00407_. label="Interventions ever carried out in facilty(8)";
  attrib Q1604B_1 format=F00408_. label="Interventions carried out in past 3 months(1)";
  attrib Q1604B_2 format=F00408_. label="Interventions carried out in past 3 months(2)";
  attrib Q1604B_3 format=F00408_. label="Interventions carried out in past 3 months(3)";
  attrib Q1604B_4 format=F00408_. label="Interventions carried out in past 3 months(4)";
  attrib Q1604B_5 format=F00408_. label="Interventions carried out in past 3 months(5)";
  attrib Q1604B_6 format=F00408_. label="Interventions carried out in past 3 months(6)";
  attrib Q1604B_7 format=F00408_. label="Interventions carried out in past 3 months(7)";
  attrib Q1604B_8 format=F00408_. label="Interventions carried out in past 3 months(8)";
  attrib Q1605    format=F00409_. label="National gujidelines for IMPAC";
  attrib Q1606    format=F00410_. label="National gujidelines for IMPAC observed";
  attrib Q1607    format=F00411_. label="National guidelines for CEmOC";
  attrib Q1608    format=F00412_. label="National guidelines for CEmOC observed";
  attrib Q1609    format=F00413_. label="Guidelines for management of pre-term labor";
  attrib Q1610    format=F00414_. label="Guidelines for management of pre-term labor observed";
  attrib Q1611    format=F00415_. label="Practice Kangaroo Mother Care in low-weight babies";
  attrib Q1612    format=F00416_. label="Separate room or space for Kangaroo Mother Care of intergrated in main postnatal ward";
  attrib Q1613    format=F00417_. label="Use partographs to monitor labor/delivery";
  attrib Q1614    format=F00418_. label="Routinely or selectively use partograph";
  attrib Q1615    format=F00419_. label="Number of dedicated maternity beds";
  attrib Q1616    format=F00420_. label="Number of dedicated delivery beds";
  attrib Q1617    format=F00421_. label="Regular reviews of maternal or newborn deaths or 'near misses'";
  attrib Q1618    format=F00422_. label="Reviews of mother, newborn or both";
  attrib Q1619    format=F00423_. label="Frequency of maternal deaths/near-misses reviews";
  attrib Q1621    format=F00424_. label="Frequency of newborn deaths/near misses reviews";
  attrib Q1622A_01 format=F00425_. label="Delivery area equipment available(1)";
  attrib Q1622A_02 format=F00425_. label="Delivery area equipment available(2)";
  attrib Q1622A_03 format=F00425_. label="Delivery area equipment available(3)";
  attrib Q1622A_04 format=F00425_. label="Delivery area equipment available(4)";
  attrib Q1622A_05 format=F00425_. label="Delivery area equipment available(5)";
  attrib Q1622A_06 format=F00425_. label="Delivery area equipment available(6)";
  attrib Q1622A_07 format=F00425_. label="Delivery area equipment available(7)";
  attrib Q1622A_08 format=F00425_. label="Delivery area equipment available(8)";
  attrib Q1622A_09 format=F00425_. label="Delivery area equipment available(9)";
  attrib Q1622A_10 format=F00425_. label="Delivery area equipment available(10)";
  attrib Q1622A_11 format=F00425_. label="Delivery area equipment available(11)";
  attrib Q1622A_12 format=F00425_. label="Delivery area equipment available(12)";
  attrib Q1622A_13 format=F00425_. label="Delivery area equipment available(13)";
  attrib Q1622A_14 format=F00425_. label="Delivery area equipment available(14)";
  attrib Q1622A_15 format=F00425_. label="Delivery area equipment available(15)";
  attrib Q1622B_01 format=F00426_. label="Equipment functioning(1)";
  attrib Q1622B_02 format=F00426_. label="Equipment functioning(2)";
  attrib Q1622B_03 format=F00426_. label="Equipment functioning(3)";
  attrib Q1622B_04 format=F00426_. label="Equipment functioning(4)";
  attrib Q1622B_05 format=F00426_. label="Equipment functioning(5)";
  attrib Q1622B_06 format=F00426_. label="Equipment functioning(6)";
  attrib Q1622B_07 format=F00426_. label="Equipment functioning(7)";
  attrib Q1622B_08 format=F00426_. label="Equipment functioning(8)";
  attrib Q1622B_09 format=F00426_. label="Equipment functioning(9)";
  attrib Q1622B_10 format=F00426_. label="Equipment functioning(10)";
  attrib Q1622B_11 format=F00426_. label="Equipment functioning(11)";
  attrib Q1622B_12 format=F00426_. label="Equipment functioning(12)";
  attrib Q1622B_13 format=F00426_. label="Equipment functioning(13)";
  attrib Q1622B_14 format=F00426_. label="Equipment functioning(14)";
  attrib Q1622B_15 format=F00426_. label="Equipment functioning(15)";
  attrib Q1623_01 format=F00427_. label="Other delivery items available(1)";
  attrib Q1623_02 format=F00427_. label="Other delivery items available(2)";
  attrib Q1623_03 format=F00427_. label="Other delivery items available(3)";
  attrib Q1623_04 format=F00427_. label="Other delivery items available(4)";
  attrib Q1623_05 format=F00427_. label="Other delivery items available(5)";
  attrib Q1623_06 format=F00427_. label="Other delivery items available(6)";
  attrib Q1623_07 format=F00427_. label="Other delivery items available(7)";
  attrib Q1623_08 format=F00427_. label="Other delivery items available(8)";
  attrib Q1623_09 format=F00427_. label="Other delivery items available(9)";
  attrib Q1623_10 format=F00427_. label="Other delivery items available(10)";
  attrib Q1623_11 format=F00427_. label="Other delivery items available(11)";
  attrib Q1623_12 format=F00427_. label="Other delivery items available(12)";
  attrib Q1623_13 format=F00427_. label="Other delivery items available(13)";
  attrib Q1623A   format=F00428_. label="Routinely provide AMTSL";
  attrib Q1624_01 format=F00429_. label="Routine newborn activities(1)";
  attrib Q1624_02 format=F00429_. label="Routine newborn activities(2)";
  attrib Q1624_03 format=F00429_. label="Routine newborn activities(3)";
  attrib Q1624_04 format=F00429_. label="Routine newborn activities(4)";
  attrib Q1624_05 format=F00429_. label="Routine newborn activities(5)";
  attrib Q1624_06 format=F00429_. label="Routine newborn activities(6)";
  attrib Q1624_07 format=F00429_. label="Routine newborn activities(7)";
  attrib Q1624_08 format=F00429_. label="Routine newborn activities(8)";
  attrib Q1624_09 format=F00429_. label="Routine newborn activities(9)";
  attrib Q1624_10 format=F00429_. label="Routine newborn activities(10)";
  attrib Q1624_11 format=F00429_. label="Routine newborn activities(11)";
  attrib Q1624_12 format=F00429_. label="Routine newborn activities(12)";
  attrib Q1624_13 format=F00429_. label="Routine newborn activities(13)";
  attrib Q1624_14 format=F00429_. label="Routine newborn activities(14)";
  attrib Q1624_15 format=F00429_. label="Routine newborn activities(15)";
  attrib Q1624_16 format=F00429_. label="Routine newborn activities(16)";
  attrib Q1625_01 format=F00430_. label="Medicines in delivery area(1)";
  attrib Q1625_02 format=F00430_. label="Medicines in delivery area(2)";
  attrib Q1625_03 format=F00430_. label="Medicines in delivery area(3)";
  attrib Q1625_04 format=F00430_. label="Medicines in delivery area(4)";
  attrib Q1625_05 format=F00430_. label="Medicines in delivery area(5)";
  attrib Q1625_06 format=F00430_. label="Medicines in delivery area(6)";
  attrib Q1625_07 format=F00430_. label="Medicines in delivery area(7)";
  attrib Q1625_08 format=F00430_. label="Medicines in delivery area(8)";
  attrib Q1625_09 format=F00430_. label="Medicines in delivery area(9)";
  attrib Q1625_10 format=F00430_. label="Medicines in delivery area(10)";
  attrib Q1650    format=F00431_. label="Data is already recorded for already assessed room/area";
  attrib Q1651_01 format=F00432_. label="Precaution and condition in client examination room(1)";
  attrib Q1651_02 format=F00432_. label="Precaution and condition in client examination room(2)";
  attrib Q1651_03 format=F00432_. label="Precaution and condition in client examination room(3)";
  attrib Q1651_04 format=F00432_. label="Precaution and condition in client examination room(4)";
  attrib Q1651_05 format=F00432_. label="Precaution and condition in client examination room(5)";
  attrib Q1651_06 format=F00432_. label="Precaution and condition in client examination room(6)";
  attrib Q1651_07 format=F00432_. label="Precaution and condition in client examination room(7)";
  attrib Q1651_08 format=F00432_. label="Precaution and condition in client examination room(8)";
  attrib Q1651_09 format=F00432_. label="Precaution and condition in client examination room(9)";
  attrib Q1651_10 format=F00432_. label="Precaution and condition in client examination room(10)";
  attrib Q1651_11 format=F00432_. label="Precaution and condition in client examination room(11)";
  attrib Q1651_12 format=F00432_. label="Precaution and condition in client examination room(12)";
  attrib Q1651_13 format=F00432_. label="Precaution and condition in client examination room(13)";
  attrib Q1652    format=F00433_. label="Setting of delivery service room";
  attrib Q1901    label="Days/ month TB services offered";
  attrib Q1902    format=F00434_. label="Providers dx TB";
  attrib Q1903    format=F00435_. label="Most common method of TB dx";
  attrib Q1904    format=F00436_. label="Providers refer outside facility for TB dx";
  attrib Q1905    format=F00437_. label="Facility has agreement with referral facility for test results to be returned";
  attrib Q1906    format=F00438_. label="Register of referred clients for TB dx";
  attrib Q1907    format=F00439_. label="Register of referred clients for TB dx observed";
  attrib Q1908    format=F00440_. label="Providers prescribe tx for TB or manage TB clients";
  attrib Q1909    format=F00441_. label="Treatment strategy";
  attrib Q1911    format=F00442_. label="System for testing TB clients for HIV";
  attrib Q1912    format=F00443_. label="System for testing TB clients for HIV observed";
  attrib Q1913    format=F00444_. label="HIV rapid test kit available";
  attrib Q1914    format=F00445_. label="Observe HIV rapid test kit available";
  attrib Q1915    format=F00446_. label="National guidelines for TB dx/tx";
  attrib Q1916    format=F00447_. label="National guidelines for TB dx/tx observed";
  attrib Q1917    format=F00448_. label="Guidelines for management of HIV and TB co-infection";
  attrib Q1918    format=F00449_. label="Guidelines for management of HIV and TB co-infection observed";
  attrib Q1919    format=F00450_. label="Guidelines related to MDR-TB tx";
  attrib Q1920    format=F00451_. label="Guidelines related to MDR-TB tx observed";
  attrib Q1922    format=F00452_. label="Maintain any sputum containers";
  attrib Q1923    format=F00453_. label="Maintain any sputum containers observed";
  attrib Q1950    format=F00454_. label="Data is already recorded for already assessed room/area";
  attrib Q1951_01 format=F00455_. label="Precaution and condition in client examination room(1)";
  attrib Q1951_02 format=F00455_. label="Precaution and condition in client examination room(2)";
  attrib Q1951_03 format=F00455_. label="Precaution and condition in client examination room(3)";
  attrib Q1951_04 format=F00455_. label="Precaution and condition in client examination room(4)";
  attrib Q1951_05 format=F00455_. label="Precaution and condition in client examination room(5)";
  attrib Q1951_06 format=F00455_. label="Precaution and condition in client examination room(6)";
  attrib Q1951_07 format=F00455_. label="Precaution and condition in client examination room(7)";
  attrib Q1951_08 format=F00455_. label="Precaution and condition in client examination room(8)";
  attrib Q1951_09 format=F00455_. label="Precaution and condition in client examination room(9)";
  attrib Q1951_10 format=F00455_. label="Precaution and condition in client examination room(10)";
  attrib Q1951_11 format=F00455_. label="Precaution and condition in client examination room(11)";
  attrib Q1951_12 format=F00455_. label="Precaution and condition in client examination room(12)";
  attrib Q1951_13 format=F00455_. label="Precaution and condition in client examination room(13)";
  attrib Q1952    format=F00456_. label="Setting of examination room";
  attrib Q2301    format=F00457_. label="Providers dx or manage diabetes";
  attrib Q2302    format=F00458_. label="National guidelines for diabetes";
  attrib Q2303    format=F00459_. label="National guidelines for diabetes observed";
  attrib Q2304    format=F00460_. label="Other guidelines for diabetes";
  attrib Q2305    format=F00461_. label="Other guidelines for diabetes observed";
  attrib Q2310    format=F00462_. label="Providers dx or manage hypertension/cardio-vascular disease";
  attrib Q2311    format=F00463_. label="National guidelines for hypertension/cardio-vascular disease";
  attrib Q2312    format=F00464_. label="National guidelines for hypertension/cardio-vascular disease observed";
  attrib Q2313    format=F00465_. label="Other guidelines for hypertension/cardio-vascular disease";
  attrib Q2314    format=F00466_. label="Other guidelines for hypertension/cardio-vascular disease observed";
  attrib Q2315    format=F00467_. label="Providers dx or manage coronary heart disease";
  attrib Q2316    format=F00468_. label="National guidelines for coronary heart disease";
  attrib Q2317    format=F00469_. label="National guidelines for coronary heart disease observed";
  attrib Q2318    format=F00470_. label="Other guidelines for coronary heart disease";
  attrib Q2319    format=F00471_. label="Other guidelines forcoronary heart disease observed";
  attrib Q2320    format=F00472_. label="Providers dx or manage chronic respiratory diseases";
  attrib Q2321    format=F00473_. label="National guidelines for chronic respiratory diseases";
  attrib Q2322    format=F00474_. label="National guidelines for chronic respiratory diseases observed";
  attrib Q2323    format=F00475_. label="Other guidelines for chronic respiratory diseases";
  attrib Q2324    format=F00476_. label="Other guidelines for chronic respiratory diseasesobserved";
  attrib Q2325    format=F00477_. label="Providers dx or manage cervical cancer";
  attrib Q2326    format=F00478_. label="National guidelines for cervical cancer";
  attrib Q2327    format=F00479_. label="National guidelines for chronic respiratory diseases observed";
  attrib Q2328    format=F00480_. label="Other guidelines for cervical cancer";
  attrib Q2329    format=F00481_. label="Other guidelines for cervical cancer disease bserved";
  attrib Q2330    format=F00482_. label="Asses the room/area for basic supplies & equipment (recorded in Q700)";
  attrib Q2331A_01 format=F00483_. label="Availability of basic supplies(1)";
  attrib Q2331A_02 format=F00483_. label="Availability of basic supplies(2)";
  attrib Q2331A_03 format=F00483_. label="Availability of basic supplies(3)";
  attrib Q2331A_04 format=F00483_. label="Availability of basic supplies(4)";
  attrib Q2331A_05 format=F00483_. label="Availability of basic supplies(5)";
  attrib Q2331A_06 format=F00483_. label="Availability of basic supplies(6)";
  attrib Q2331A_07 format=F00483_. label="Availability of basic supplies(7)";
  attrib Q2331A_08 format=F00483_. label="Availability of basic supplies(8)";
  attrib Q2331A_09 format=F00483_. label="Availability of basic supplies(9)";
  attrib Q2331A_10 format=F00483_. label="Availability of basic supplies(10)";
  attrib Q2331A_11 format=F00483_. label="Availability of basic supplies(11)";
  attrib Q2331A_12 format=F00483_. label="Availability of basic supplies(12)";
  attrib Q2331A_13 format=F00483_. label="Availability of basic supplies(13)";
  attrib Q2331A_14 format=F00483_. label="Availability of basic supplies(14)";
  attrib Q2331A_15 format=F00483_. label="Availability of basic supplies(15)";
  attrib Q2331A_16 format=F00483_. label="Availability of basic supplies(16)";
  attrib Q2331A_17 format=F00483_. label="Availability of basic supplies(17)";
  attrib Q2331A_18 format=F00483_. label="Availability of basic supplies(18)";
  attrib Q2331A_19 format=F00483_. label="Availability of basic supplies(19)";
  attrib Q2331A_20 format=F00483_. label="Availability of basic supplies(20)";
  attrib Q2331A_21 format=F00483_. label="Availability of basic supplies(21)";
  attrib Q2331B_01 format=F00484_. label="Basic supplies functioning(1)";
  attrib Q2331B_02 format=F00484_. label="Basic supplies functioning(2)";
  attrib Q2331B_03 format=F00484_. label="Basic supplies functioning(3)";
  attrib Q2331B_04 format=F00484_. label="Basic supplies functioning(4)";
  attrib Q2331B_05 format=F00484_. label="Basic supplies functioning(5)";
  attrib Q2331B_06 format=F00484_. label="Basic supplies functioning(6)";
  attrib Q2331B_07 format=F00484_. label="Basic supplies functioning(7)";
  attrib Q2331B_08 format=F00484_. label="Basic supplies functioning(8)";
  attrib Q2331B_09 format=F00484_. label="Basic supplies functioning(9)";
  attrib Q2331B_10 format=F00484_. label="Basic supplies functioning(10)";
  attrib Q2331B_11 format=F00484_. label="Basic supplies functioning(11)";
  attrib Q2331B_12 format=F00484_. label="Basic supplies functioning(12)";
  attrib Q2331B_13 format=F00484_. label="Basic supplies functioning(13)";
  attrib Q2331B_14 format=F00484_. label="Basic supplies functioning(14)";
  attrib Q2331B_15 format=F00484_. label="Basic supplies functioning(15)";
  attrib Q2331B_16 format=F00484_. label="Basic supplies functioning(16)";
  attrib Q2331B_17 format=F00484_. label="Basic supplies functioning(17)";
  attrib Q2331B_18 format=F00484_. label="Basic supplies functioning(18)";
  attrib Q2331B_19 format=F00484_. label="Basic supplies functioning(19)";
  attrib Q2331B_20 format=F00484_. label="Basic supplies functioning(20)";
  attrib Q2331B_21 format=F00484_. label="Basic supplies functioning(21)";
  attrib Q2350    format=F00485_. label="Data is already recorded for already assessed room/area";
  attrib Q2351_01 format=F00486_. label="Precaution and condition in client examination room(1)";
  attrib Q2351_02 format=F00486_. label="Precaution and condition in client examination room(2)";
  attrib Q2351_03 format=F00486_. label="Precaution and condition in client examination room(3)";
  attrib Q2351_04 format=F00486_. label="Precaution and condition in client examination room(4)";
  attrib Q2351_05 format=F00486_. label="Precaution and condition in client examination room(5)";
  attrib Q2351_06 format=F00486_. label="Precaution and condition in client examination room(6)";
  attrib Q2351_07 format=F00486_. label="Precaution and condition in client examination room(7)";
  attrib Q2351_08 format=F00486_. label="Precaution and condition in client examination room(8)";
  attrib Q2351_09 format=F00486_. label="Precaution and condition in client examination room(9)";
  attrib Q2351_10 format=F00486_. label="Precaution and condition in client examination room(10)";
  attrib Q2351_11 format=F00486_. label="Precaution and condition in client examination room(11)";
  attrib Q2351_12 format=F00486_. label="Precaution and condition in client examination room(12)";
  attrib Q2351_13 format=F00486_. label="Precaution and condition in client examination room(13)";
  attrib Q2352    format=F00487_. label="Setting of examination room";
  attrib Q2501    format=F00488_. label="C-section provider present or on-call 24 hours";
  attrib Q2502    format=F00489_. label="Duty schedule or call list for 24-hr for C-section";
  attrib Q2503    format=F00490_. label="Duty schedule or call list for 24-hr for C-section observed";
  attrib Q2504    format=F00491_. label="Anesthetist present or on-call 24 hours";
  attrib Q2505    format=F00492_. label="Duty schedule or call list for 24-hr for Anesthetist";
  attrib Q2506    format=F00493_. label="Duty schedule or call list for 24-hr for Anesthetist observed";
  attrib Q2507    format=F00494_. label="Has CS been performed during past 3 months";
  attrib Q2510A_01 format=F00495_. label="Anesthesia equipment available(1)";
  attrib Q2510A_02 format=F00495_. label="Anesthesia equipment available(2)";
  attrib Q2510A_03 format=F00495_. label="Anesthesia equipment available(3)";
  attrib Q2510A_04 format=F00495_. label="Anesthesia equipment available(4)";
  attrib Q2510A_05 format=F00495_. label="Anesthesia equipment available(5)";
  attrib Q2510A_06 format=F00495_. label="Anesthesia equipment available(6)";
  attrib Q2510A_07 format=F00495_. label="Anesthesia equipment available(7)";
  attrib Q2510A_08 format=F00495_. label="Anesthesia equipment available(8)";
  attrib Q2510A_09 format=F00495_. label="Anesthesia equipment available(9)";
  attrib Q2510A_10 format=F00495_. label="Anesthesia equipment available(10)";
  attrib Q2510A_11 format=F00495_. label="Anesthesia equipment available(11)";
  attrib Q2510A_12 format=F00495_. label="Anesthesia equipment available(12)";
  attrib Q2510A_13 format=F00495_. label="Anesthesia equipment available(13)";
  attrib Q2510A_14 format=F00495_. label="Anesthesia equipment available(14)";
  attrib Q2510A_15 format=F00495_. label="Anesthesia equipment available(15)";
  attrib Q2510A_16 format=F00495_. label="Anesthesia equipment available(16)";
  attrib Q2510A_17 format=F00495_. label="Anesthesia equipment available(17)";
  attrib Q2510A_18 format=F00495_. label="Anesthesia equipment available(18)";
  attrib Q2510A_19 format=F00495_. label="Anesthesia equipment available(19)";
  attrib Q2510A_20 format=F00495_. label="Anesthesia equipment available(20)";
  attrib Q2510B_01 format=F00496_. label="Anesthesia equipment functioning(1)";
  attrib Q2510B_02 format=F00496_. label="Anesthesia equipment functioning(2)";
  attrib Q2510B_03 format=F00496_. label="Anesthesia equipment functioning(3)";
  attrib Q2510B_04 format=F00496_. label="Anesthesia equipment functioning(4)";
  attrib Q2510B_05 format=F00496_. label="Anesthesia equipment functioning(5)";
  attrib Q2510B_06 format=F00496_. label="Anesthesia equipment functioning(6)";
  attrib Q2510B_07 format=F00496_. label="Anesthesia equipment functioning(7)";
  attrib Q2510B_08 format=F00496_. label="Anesthesia equipment functioning(8)";
  attrib Q2510B_09 format=F00496_. label="Anesthesia equipment functioning(9)";
  attrib Q2510B_10 format=F00496_. label="Anesthesia equipment functioning(10)";
  attrib Q2510B_11 format=F00496_. label="Anesthesia equipment functioning(11)";
  attrib Q2510B_12 format=F00496_. label="Anesthesia equipment functioning(12)";
  attrib Q2510B_13 format=F00496_. label="Anesthesia equipment functioning(13)";
  attrib Q2510B_14 format=F00496_. label="Anesthesia equipment functioning(14)";
  attrib Q2510B_15 format=F00496_. label="Anesthesia equipment functioning(15)";
  attrib Q2510B_16 format=F00496_. label="Anesthesia equipment functioning(16)";
  attrib Q2510B_17 format=F00496_. label="Anesthesia equipment functioning(17)";
  attrib Q2510B_18 format=F00496_. label="Anesthesia equipment functioning(18)";
  attrib Q2510B_19 format=F00496_. label="Anesthesia equipment functioning(19)";
  attrib Q2510B_20 format=F00496_. label="Anesthesia equipment functioning(20)";
  attrib Q2601_1  format=F00497_. label="Reagents available in facility(1)";
  attrib Q2601_2  format=F00497_. label="Reagents available in facility(2)";
  attrib Q2601_3  format=F00497_. label="Reagents available in facility(3)";
  attrib Q2601_4  format=F00497_. label="Reagents available in facility(4)";
  attrib Q2601_5  format=F00497_. label="Reagents available in facility(5)";
  attrib Q2701    length=$5 format=$F00498_. label="Source of blood for transfusion";
  attrib Q2702    format=F00499_. label="Blood transfusion an obstetric context during past 3 months";
  attrib Q2710    format=F00500_. label="Blood screening";
  attrib Q2711    format=F00501_. label="Blood screening location";
  attrib Q2712_1  format=F00502_. label="Screening for specific diseases(1)";
  attrib Q2712_2  format=F00502_. label="Screening for specific diseases(2)";
  attrib Q2712_3  format=F00502_. label="Screening for specific diseases(3)";
  attrib Q2712_4  format=F00502_. label="Screening for specific diseases(4)";
  attrib Q2712_5  format=F00502_. label="Screening for specific diseases(5)";
  attrib Q2713    format=F00503_. label="Send blood outside for screening";
  attrib Q2714A_1 format=F00504_. label="Blood sent out for specific diseases(1)";
  attrib Q2714A_2 format=F00504_. label="Blood sent out for specific diseases(2)";
  attrib Q2714A_3 format=F00504_. label="Blood sent out for specific diseases(3)";
  attrib Q2714A_4 format=F00504_. label="Blood sent out for specific diseases(4)";
  attrib Q2714A_5 format=F00504_. label="Blood sent out for specific diseases(5)";
  attrib Q2714B_1 format=F00505_. label="Record of outside test for specific diseases(1)";
  attrib Q2714B_2 format=F00505_. label="Record of outside test for specific diseases(2)";
  attrib Q2714B_3 format=F00505_. label="Record of outside test for specific diseases(3)";
  attrib Q2714B_4 format=F00505_. label="Record of outside test for specific diseases(4)";
  attrib Q2714B_5 format=F00505_. label="Record of outside test for specific diseases(5)";
  attrib Q2720    format=F00506_. label="Facility has run out of blood in past 3 months";
  attrib Q2721    format=F00507_. label="Refrigerator for storing blood";
  attrib Q2722    format=F00508_. label="Refrigerator for storing blood observed";
  attrib Q2723    format=F00509_. label="Temperature of refrigerator for blood";
  attrib Q2724    format=F00510_. label="Guidelines on appropriate use of blood & safe transfusion practices";
  attrib Q2725    format=F00511_. label="Guidelines on appropriate use of blood & safe transfusion practices observed";
  attrib Q2725A_1 format=F00512_. label="Reagents available in facility(1)";
  attrib Q2725A_2 format=F00512_. label="Reagents available in facility(2)";
  attrib Q3000_01 format=F00513_. label="Facility cleanliness table(1)";
  attrib Q3000_02 format=F00513_. label="Facility cleanliness table(2)";
  attrib Q3000_03 format=F00513_. label="Facility cleanliness table(3)";
  attrib Q3000_04 format=F00513_. label="Facility cleanliness table(4)";
  attrib Q3000_05 format=F00513_. label="Facility cleanliness table(5)";
  attrib Q3000_06 format=F00513_. label="Facility cleanliness table(6)";
  attrib Q3000_07 format=F00513_. label="Facility cleanliness table(7)";
  attrib Q3000_08 format=F00513_. label="Facility cleanliness table(8)";
  attrib Q3000_09 format=F00513_. label="Facility cleanliness table(9)";
  attrib Q3000_10 format=F00513_. label="Facility cleanliness table(10)";
  attrib Q3000_11 format=F00513_. label="Facility cleanliness table(11)";
  attrib Q3000_12 format=F00513_. label="Facility cleanliness table(12)";
  attrib ENDHH    label="Ending hour";
  attrib ENDMM    label="Ending minute";
infile 'C:\BDHFS\EXPORT\EXPORTEDFILES\BDFC7IFLSP.DAT' LRECL=3396 MISSOVER ;
    input
    @1    FACIL    5.0
    @6    REC_TYPE $1.
    @7    REGION   2.0
    @9    DISTRICT 2.0
    @11   FACTYPE  2.0
    @13   MGA      1.0
    @14   FTYPE    1.0
    @15   IPD_OPD  1.0
    @16   NUMVISIT 1.0
    @17   DAY      2.0
    @19   MONTH    2.0
    @21   YEAR     4.0
    @25   INTVNO   3.0
    @28   RESULT   1.0
    @29   PROVDERS 2.0
    @31   Q100     1.0
    @32   Q101H    2.0
    @34   Q101M    2.0
    @36   FACWT    8.0
    @44   STEAMF   1.0
    @45   STEAMC   1.0
    @46   STEAMA   1.0
    @47   STEAMB   1.0
    @48   STEAMT   1.0
    @49   AUTOF    1.0
    @50   AUTOC    1.0
    @51   AUTOWA   1.0
    @52   AUTOWB   1.0
    @53   AUTOUA   1.0
    @54   AUTOUB   1.0
    @55   AUTOT    1.0
    @56   PRESSE   1.0
    @57   PRESSG   1.0
    @58   TEMPE    1.0
    @59   TEMPG    1.0
    @60   DHSFAC   1.0
    @61   DHSCAP   1.0
    @62   DHSA     1.0
    @63   DHSB     1.0
    @64   DHSC     1.0
    @65   DHST     1.0
    @66   HLDFAC   1.0
    @67   HLDCAP   1.0
    @68   HLDA     1.0
    @69   HLDB     1.0
    @70   HLDC     1.0
    @71   HLDT     1.0
    @72   CHEMF    1.0
    @73   CHEMA    1.0
    @74   CHEMB    1.0
    @75   CHEMT    1.0
    @76   SRC      1.0
    @77   FACEPI   1.0
    @78   MODTMPFP 1.0
    @79   STERFP   1.0
    @80   PAFP     1.0
    @81   LAMFP    1.0
    @82   FACPMTCT 1.0
    @83   FACART   1.0
    @84   FACCSS   1.0
    @85   FACMICROSC 1.0
    @86   FACSLIDES 1.0
    @87   FACTUBES 1.0
    @88   FACINCUB 1.0
    @89   FACSHAKER 1.0
    @90   FACHIVRDT 1.0
    @91   FACHIVLAB 1.0
    @92   FACHIVINTRNL 1.0
    @93   FACHIVEXTRNL 1.0
    @94   FACHIVTST 1.0
    @95   FACHIVDXTX 1.0
    @96   FACDBS   1.0
    @97   FACTBRDT 1.0
    @98   FACTBMICRO 1.0
    @99   FACTBCULT 1.0
    @100  FACTBXRAY 1.0
    @101  FACTBINTRNL 1.0
    @102  FACTBEXTRNL 1.0
    @103  FACTBTST 1.0
    @104  FACTBDXTX 1.0
    @105  FACMALRDT 1.0
    @106  FACMALLAB 1.0
    @107  FACMALTST 1.0
    @108  FACMALDXTX 1.0
    @109  SHARPWASTE 1.0
    @110  MEDWASTE 1.0
    @111  STEREQUIP 1.0
    @112  HLDEQUIP 1.0
    @113  SHARPSTORE 1.0
    @114  MEDSTORE 1.0
    @115  ELIGPROV 2.0
    @117  ELIGTRAIN 2.0
    @119  ELIGSUPERV 2.0
    @121  ELIGBOTH 2.0
    @123  PROVIMCI 2.0
    @125  PROVGM   2.0
    @127  PROVEPI  2.0
    @129  PROVANCT 2.0
    @131  HEMOGLOB 1.0
    @132  CD4      1.0
    @133  BLOODGLUC 1.0
    @134  SYPHSEROL 1.0
    @135  STOOLMICRO 1.0
    @136  TOTFP    3.0
    @139  TOTSC    3.0
    @142  ANYCAP   1.0
    @143  ANYKNOWL 1.0
    @144  ANYTIMER 1.0
    @145  FACEPI2  1.0
    @146  Q400V_01 3.0
    @149  Q400V_02 3.0
    @152  Q400V_03 3.0
    @155  Q400V_04 3.0
    @158  Q400V_05 3.0
    @161  Q400V_06 3.0
    @164  Q400V_07 3.0
    @167  Q400V_08 3.0
    @170  Q400V_09 3.0
    @173  Q400V_10 3.0
    @176  Q400V_11 3.0
    @179  Q400V_12 3.0
    @182  Q400V_13 3.0
    @185  Q400V_14 3.0
    @188  Q400V_15 3.0
    @191  Q400V_16 3.0
    @194  Q400V_17 3.0
    @197  Q400V_18 3.0
    @200  Q400V_19 3.0
    @203  Q400V_20 3.0
    @206  Q400V_21 3.0
    @209  Q400V_22 3.0
    @212  Q400V_23 3.0
    @215  Q400V_24 3.0
    @218  Q400V_25 3.0
    @221  Q400V_26 3.0
    @224  Q400V_27 3.0
    @227  Q400V_28 3.0
    @230  Q400V_29 3.0
    @233  Q400V_30 3.0
    @236  Q400V_31 3.0
    @239  Q400V_32 3.0
    @242  Q400V_33 3.0
    @245  Q400V_34 3.0
    @248  Q400V_35 3.0
    @251  Q400V_36 3.0
    @254  Q400V_37 3.0
    @257  Q400V_38 3.0
    @260  Q400V_39 3.0
    @263  Q400V_40 3.0
    @266  Q400V_41 3.0
    @269  Q400V_42 3.0
    @272  Q400V_43 3.0
    @275  Q400V_44 3.0
    @278  Q400V_45 3.0
    @281  Q400V_46 3.0
    @284  Q400V_47 3.0
    @287  Q400V_48 3.0
    @290  Q400V_49 3.0
    @293  Q400V_50 3.0
    @296  Q400V_51 3.0
    @299  Q400V_52 3.0
    @302  Q400V_53 3.0
    @305  Q400V_54 3.0
    @308  Q400V_55 3.0
    @311  Q400V_56 3.0
    @314  Q400V_57 3.0
    @317  Q400V_58 3.0
    @320  Q400V_59 3.0
    @323  Q400V_60 3.0
    @326  Q400V_61 3.0
    @329  Q400V_62 3.0
    @332  Q400V_63 3.0
    @335  Q400V_64 3.0
    @338  Q400V_65 3.0
    @341  Q400V_66 3.0
    @344  Q400V_67 3.0
    @347  Q400V_68 3.0
    @350  Q400V_69 3.0
    @353  Q400V_70 3.0
    @356  Q400V_71 3.0
    @359  Q400V_72 3.0
    @362  Q400V_73 3.0
    @365  Q400V_74 3.0
    @368  Q400V_75 3.0
    @371  Q400V_76 3.0
    @374  Q400V_77 3.0
    @377  Q400V_78 3.0
    @380  Q400V_79 3.0
    @383  Q400V_80 3.0
    @386  Q400V_81 3.0
    @389  Q400V_82 3.0
    @392  Q400V_83 3.0
    @395  Q400V_84 3.0
    @398  Q400V_85 3.0
    @401  Q400V_86 3.0
    @404  Q400V_87 3.0
    @407  Q400V_88 3.0
    @410  Q400V_89 3.0
    @413  Q400V_90 3.0
    @416  Q400V_91 3.0
    @419  Q400V_92 3.0
    @422  Q400V_93 3.0
    @425  Q400V_94 3.0
    @428  Q400V_95 3.0
    @431  PROVIMCI1 2.0
    @433  PROVGM1  2.0
    @435  PROVEPI1 2.0
    @437  PROVANCT1 2.0
    @439  PRVIMPAC 2.0
    @441  PRVIMPAC1 2.0
    @443  PRVLD    2.0
    @445  PRVLD1   2.0
    @447  PRVAMTSL 2.0
    @449  PRVAMTSL1 2.0
    @451  PRVEMGOBS 2.0
    @453  PRVEMGOBS1 2.0
    @455  PRVPAC   2.0
    @457  PRVPAC1  2.0
    @459  PRVNEORES 2.0
    @461  PRVNEORES1 2.0
    @463  Q102_01  1.0
    @464  Q102_02  1.0
    @465  Q102_03  1.0
    @466  Q102_04  1.0
    @467  Q102_05  1.0
    @468  Q102_06  1.0
    @469  Q102_07  1.0
    @470  Q102_08  1.0
    @471  Q102_09  1.0
    @472  Q102_10  1.0
    @473  Q102_11  1.0
    @474  Q102_12  1.0
    @475  Q102_13  1.0
    @476  Q102_14  1.0
    @477  Q102_15  1.0
    @478  Q102_16  1.0
    @479  Q102_17  1.0
    @480  Q102_18  1.0
    @481  Q102_19  1.0
    @482  Q102_20  1.0
    @483  Q102_21  1.0
    @484  Q102_22  1.0
    @485  Q102_23  1.0
    @486  Q110     1.0
    @487  Q111     1.0
    @488  Q112     3.0
    @491  Q113     3.0
    @494  Q114     3.0
    @497  Q200     1.0
    @498  Q201     1.0
    @499  Q210     1.0
    @500  Q212     1.0
    @501  Q214     1.0
    @502  PROVLNF  2.0
    @504  Q300     1.0
    @505  Q301     1.0
    @506  Q302     1.0
    @507  Q310     1.0
    @508  Q311     1.0
    @509  Q312     1.0
    @510  Q313     1.0
    @511  Q314     1.0
    @512  Q315     1.0
    @513  Q319     1.0
    @514  Q320     1.0
    @515  Q321     1.0
    @516  Q322     1.0
    @517  Q323     1.0
    @518  Q330     2.0
    @520  Q331     1.0
    @521  Q332     1.0
    @522  Q340     1.0
    @523  Q341     1.0
    @524  Q342     1.0
    @525  Q343     $3.
    @528  Q345     1.0
    @529  Q346     1.0
    @530  Q346A    1.0
    @531  Q346B    1.0
    @532  Q346C    1.0
    @533  Q350     1.0
    @534  Q351     1.0
    @535  Q351A    2.0
    @537  Q352_1   1.0
    @538  Q352_2   1.0
    @539  Q352_3   1.0
    @540  Q352A    1.0
    @541  Q352B    1.0
    @542  Q352C    1.0
    @543  Q352D    1.0
    @544  Q352E    1.0
    @545  Q352F    1.0
    @546  Q352G    2.0
    @548  Q360     1.0
    @549  Q361     1.0
    @550  Q362_01  1.0
    @551  Q362_02  1.0
    @552  Q362_03  1.0
    @553  Q362_04  1.0
    @554  Q362_05  1.0
    @555  Q362_06  1.0
    @556  Q362_07  1.0
    @557  Q362_08  1.0
    @558  Q362_09  1.0
    @559  Q362_10  1.0
    @560  Q362_11  1.0
    @561  Q362_12  1.0
    @562  Q362_13  1.0
    @563  Q362_14  1.0
    @564  Q362_15  1.0
    @565  Q362_16  1.0
    @566  Q362_17  1.0
    @567  Q362_18  1.0
    @568  Q362_19  1.0
    @569  Q363     1.0
    @570  Q364     1.0
    @571  Q364A    1.0
    @572  Q364B    1.0
    @573  Q364C    1.0
    @574  Q370     $12.
    @586  Q400SN_01 2.0
    @588  Q400SN_02 2.0
    @590  Q400SN_03 2.0
    @592  Q400SN_04 2.0
    @594  Q400SN_05 2.0
    @596  Q400SN_06 2.0
    @598  Q400SN_07 2.0
    @600  Q400SN_08 2.0
    @602  Q400SN_09 2.0
    @604  Q400SN_10 2.0
    @606  Q400SN_11 2.0
    @608  Q400SN_12 2.0
    @610  Q400SN_13 2.0
    @612  Q400SN_14 2.0
    @614  Q400SN_15 2.0
    @616  Q400SN_16 2.0
    @618  Q400SN_17 2.0
    @620  Q400SN_18 2.0
    @622  Q400SN_19 2.0
    @624  Q400SN_20 2.0
    @626  Q400SN_21 2.0
    @628  Q400SN_22 2.0
    @630  Q400SN_23 2.0
    @632  Q400SN_24 2.0
    @634  Q400SN_25 2.0
    @636  Q400SN_26 2.0
    @638  Q400SN_27 2.0
    @640  Q400SN_28 2.0
    @642  Q400SN_29 2.0
    @644  Q400SN_30 2.0
    @646  Q400SN_31 2.0
    @648  Q400SN_32 2.0
    @650  Q400SN_33 2.0
    @652  Q400SN_34 2.0
    @654  Q400SN_35 2.0
    @656  Q400SN_36 2.0
    @658  Q400SN_37 2.0
    @660  Q400SN_38 2.0
    @662  Q400SN_39 2.0
    @664  Q400SN_40 2.0
    @666  Q400SN_41 2.0
    @668  Q400SN_42 2.0
    @670  Q400SN_43 2.0
    @672  Q400SN_44 2.0
    @674  Q400SN_45 2.0
    @676  Q400SN_46 2.0
    @678  Q400SN_47 2.0
    @680  Q400SN_48 2.0
    @682  Q400SN_49 2.0
    @684  Q400SN_50 2.0
    @686  Q400SN_51 2.0
    @688  Q400SN_52 2.0
    @690  Q400SN_53 2.0
    @692  Q400SN_54 2.0
    @694  Q400SN_55 2.0
    @696  Q400SN_56 2.0
    @698  Q400SN_57 2.0
    @700  Q400SN_58 2.0
    @702  Q400SN_59 2.0
    @704  Q400SN_60 2.0
    @706  Q400SN_61 2.0
    @708  Q400SN_62 2.0
    @710  Q400SN_63 2.0
    @712  Q400SN_64 2.0
    @714  Q400SN_65 2.0
    @716  Q400SN_66 2.0
    @718  Q400SN_67 2.0
    @720  Q400SN_68 2.0
    @722  Q400SN_69 2.0
    @724  Q400SN_70 2.0
    @726  Q400SN_71 2.0
    @728  Q400SN_72 2.0
    @730  Q400SN_73 2.0
    @732  Q400SN_74 2.0
    @734  Q400SN_75 2.0
    @736  Q400SN_76 2.0
    @738  Q400SN_77 2.0
    @740  Q400SN_78 2.0
    @742  Q400SN_79 2.0
    @744  Q400SN_80 2.0
    @746  Q400SN_81 2.0
    @748  Q400SN_82 2.0
    @750  Q400SN_83 2.0
    @752  Q400SN_84 2.0
    @754  Q400SN_85 2.0
    @756  Q400SN_86 2.0
    @758  Q400SN_87 2.0
    @760  Q400SN_88 2.0
    @762  Q400SN_89 2.0
    @764  Q400SN_90 2.0
    @766  Q400SN_91 2.0
    @768  Q400SN_92 2.0
    @770  Q400SN_93 2.0
    @772  Q400SN_94 2.0
    @774  Q400SN_95 2.0
    @776  Q400SN_96 2.0
    @778  Q400SN_97 2.0
    @780  Q400SN_98 2.0
    @782  Q400SN_99 2.0
    @784  Q400AA_01 2.0
    @786  Q400AA_02 2.0
    @788  Q400AA_03 2.0
    @790  Q400AA_04 2.0
    @792  Q400AA_05 2.0
    @794  Q400AA_06 2.0
    @796  Q400AA_07 2.0
    @798  Q400AA_08 2.0
    @800  Q400AA_09 2.0
    @802  Q400AA_10 2.0
    @804  Q400AA_11 2.0
    @806  Q400AA_12 2.0
    @808  Q400AA_13 2.0
    @810  Q400AA_14 2.0
    @812  Q400AA_15 2.0
    @814  Q400AA_16 2.0
    @816  Q400AA_17 2.0
    @818  Q400AA_18 2.0
    @820  Q400AA_19 2.0
    @822  Q400AA_20 2.0
    @824  Q400AA_21 2.0
    @826  Q400AA_22 2.0
    @828  Q400AA_23 2.0
    @830  Q400AA_24 2.0
    @832  Q400AA_25 2.0
    @834  Q400AA_26 2.0
    @836  Q400AA_27 2.0
    @838  Q400AA_28 2.0
    @840  Q400AA_29 2.0
    @842  Q400AA_30 2.0
    @844  Q400AA_31 2.0
    @846  Q400AA_32 2.0
    @848  Q400AA_33 2.0
    @850  Q400AA_34 2.0
    @852  Q400AA_35 2.0
    @854  Q400AA_36 2.0
    @856  Q400AA_37 2.0
    @858  Q400AA_38 2.0
    @860  Q400AA_39 2.0
    @862  Q400AA_40 2.0
    @864  Q400AA_41 2.0
    @866  Q400AA_42 2.0
    @868  Q400AA_43 2.0
    @870  Q400AA_44 2.0
    @872  Q400AA_45 2.0
    @874  Q400AA_46 2.0
    @876  Q400AA_47 2.0
    @878  Q400AA_48 2.0
    @880  Q400AA_49 2.0
    @882  Q400AA_50 2.0
    @884  Q400AA_51 2.0
    @886  Q400AA_52 2.0
    @888  Q400AA_53 2.0
    @890  Q400AA_54 2.0
    @892  Q400AA_55 2.0
    @894  Q400AA_56 2.0
    @896  Q400AA_57 2.0
    @898  Q400AA_58 2.0
    @900  Q400AA_59 2.0
    @902  Q400AA_60 2.0
    @904  Q400AA_61 2.0
    @906  Q400AA_62 2.0
    @908  Q400AA_63 2.0
    @910  Q400AA_64 2.0
    @912  Q400AA_65 2.0
    @914  Q400AA_66 2.0
    @916  Q400AA_67 2.0
    @918  Q400AA_68 2.0
    @920  Q400AA_69 2.0
    @922  Q400AA_70 2.0
    @924  Q400AA_71 2.0
    @926  Q400AA_72 2.0
    @928  Q400AA_73 2.0
    @930  Q400AA_74 2.0
    @932  Q400AA_75 2.0
    @934  Q400AA_76 2.0
    @936  Q400AA_77 2.0
    @938  Q400AA_78 2.0
    @940  Q400AA_79 2.0
    @942  Q400AA_80 2.0
    @944  Q400AA_81 2.0
    @946  Q400AA_82 2.0
    @948  Q400AA_83 2.0
    @950  Q400AA_84 2.0
    @952  Q400AA_85 2.0
    @954  Q400AA_86 2.0
    @956  Q400AA_87 2.0
    @958  Q400AA_88 2.0
    @960  Q400AA_89 2.0
    @962  Q400AA_90 2.0
    @964  Q400AA_91 2.0
    @966  Q400AA_92 2.0
    @968  Q400AA_93 2.0
    @970  Q400AA_94 2.0
    @972  Q400AA_95 2.0
    @974  Q400AA_96 2.0
    @976  Q400AA_97 2.0
    @978  Q400AA_98 2.0
    @980  Q400AA_99 2.0
    @982  Q400A_01 2.0
    @984  Q400A_02 2.0
    @986  Q400A_03 2.0
    @988  Q400A_04 2.0
    @990  Q400A_05 2.0
    @992  Q400A_06 2.0
    @994  Q400A_07 2.0
    @996  Q400A_08 2.0
    @998  Q400A_09 2.0
    @1000 Q400A_10 2.0
    @1002 Q400A_11 2.0
    @1004 Q400A_12 2.0
    @1006 Q400A_13 2.0
    @1008 Q400A_14 2.0
    @1010 Q400A_15 2.0
    @1012 Q400A_16 2.0
    @1014 Q400A_17 2.0
    @1016 Q400A_18 2.0
    @1018 Q400A_19 2.0
    @1020 Q400A_20 2.0
    @1022 Q400A_21 2.0
    @1024 Q400A_22 2.0
    @1026 Q400A_23 2.0
    @1028 Q400A_24 2.0
    @1030 Q400A_25 2.0
    @1032 Q400A_26 2.0
    @1034 Q400A_27 2.0
    @1036 Q400A_28 2.0
    @1038 Q400A_29 2.0
    @1040 Q400A_30 2.0
    @1042 Q400A_31 2.0
    @1044 Q400A_32 2.0
    @1046 Q400A_33 2.0
    @1048 Q400A_34 2.0
    @1050 Q400A_35 2.0
    @1052 Q400A_36 2.0
    @1054 Q400A_37 2.0
    @1056 Q400A_38 2.0
    @1058 Q400A_39 2.0
    @1060 Q400A_40 2.0
    @1062 Q400A_41 2.0
    @1064 Q400A_42 2.0
    @1066 Q400A_43 2.0
    @1068 Q400A_44 2.0
    @1070 Q400A_45 2.0
    @1072 Q400A_46 2.0
    @1074 Q400A_47 2.0
    @1076 Q400A_48 2.0
    @1078 Q400A_49 2.0
    @1080 Q400A_50 2.0
    @1082 Q400A_51 2.0
    @1084 Q400A_52 2.0
    @1086 Q400A_53 2.0
    @1088 Q400A_54 2.0
    @1090 Q400A_55 2.0
    @1092 Q400A_56 2.0
    @1094 Q400A_57 2.0
    @1096 Q400A_58 2.0
    @1098 Q400A_59 2.0
    @1100 Q400A_60 2.0
    @1102 Q400A_61 2.0
    @1104 Q400A_62 2.0
    @1106 Q400A_63 2.0
    @1108 Q400A_64 2.0
    @1110 Q400A_65 2.0
    @1112 Q400A_66 2.0
    @1114 Q400A_67 2.0
    @1116 Q400A_68 2.0
    @1118 Q400A_69 2.0
    @1120 Q400A_70 2.0
    @1122 Q400A_71 2.0
    @1124 Q400A_72 2.0
    @1126 Q400A_73 2.0
    @1128 Q400A_74 2.0
    @1130 Q400A_75 2.0
    @1132 Q400A_76 2.0
    @1134 Q400A_77 2.0
    @1136 Q400A_78 2.0
    @1138 Q400A_79 2.0
    @1140 Q400A_80 2.0
    @1142 Q400A_81 2.0
    @1144 Q400A_82 2.0
    @1146 Q400A_83 2.0
    @1148 Q400A_84 2.0
    @1150 Q400A_85 2.0
    @1152 Q400A_86 2.0
    @1154 Q400A_87 2.0
    @1156 Q400A_88 2.0
    @1158 Q400A_89 2.0
    @1160 Q400A_90 2.0
    @1162 Q400A_91 2.0
    @1164 Q400A_92 2.0
    @1166 Q400A_93 2.0
    @1168 Q400A_94 2.0
    @1170 Q400A_95 2.0
    @1172 Q400A_96 2.0
    @1174 Q400A_97 2.0
    @1176 Q400A_98 2.0
    @1178 Q400A_99 2.0
    @1180 Q400B2_01 2.0
    @1182 Q400B2_02 2.0
    @1184 Q400B2_03 2.0
    @1186 Q400B2_04 2.0
    @1188 Q400B2_05 2.0
    @1190 Q400B2_06 2.0
    @1192 Q400B2_07 2.0
    @1194 Q400B2_08 2.0
    @1196 Q400B2_09 2.0
    @1198 Q400B2_10 2.0
    @1200 Q400B2_11 2.0
    @1202 Q400B2_12 2.0
    @1204 Q400B2_13 2.0
    @1206 Q400B2_14 2.0
    @1208 Q400B2_15 2.0
    @1210 Q400B2_16 2.0
    @1212 Q400B2_17 2.0
    @1214 Q400B2_18 2.0
    @1216 Q400B2_19 2.0
    @1218 Q400B2_20 2.0
    @1220 Q400B2_21 2.0
    @1222 Q400B2_22 2.0
    @1224 Q400B2_23 2.0
    @1226 Q400B2_24 2.0
    @1228 Q400B2_25 2.0
    @1230 Q400B2_26 2.0
    @1232 Q400B2_27 2.0
    @1234 Q400B2_28 2.0
    @1236 Q400B2_29 2.0
    @1238 Q400B2_30 2.0
    @1240 Q400B2_31 2.0
    @1242 Q400B2_32 2.0
    @1244 Q400B2_33 2.0
    @1246 Q400B2_34 2.0
    @1248 Q400B2_35 2.0
    @1250 Q400B2_36 2.0
    @1252 Q400B2_37 2.0
    @1254 Q400B2_38 2.0
    @1256 Q400B2_39 2.0
    @1258 Q400B2_40 2.0
    @1260 Q400B2_41 2.0
    @1262 Q400B2_42 2.0
    @1264 Q400B2_43 2.0
    @1266 Q400B2_44 2.0
    @1268 Q400B2_45 2.0
    @1270 Q400B2_46 2.0
    @1272 Q400B2_47 2.0
    @1274 Q400B2_48 2.0
    @1276 Q400B2_49 2.0
    @1278 Q400B2_50 2.0
    @1280 Q400B2_51 2.0
    @1282 Q400B2_52 2.0
    @1284 Q400B2_53 2.0
    @1286 Q400B2_54 2.0
    @1288 Q400B2_55 2.0
    @1290 Q400B2_56 2.0
    @1292 Q400B2_57 2.0
    @1294 Q400B2_58 2.0
    @1296 Q400B2_59 2.0
    @1298 Q400B2_60 2.0
    @1300 Q400B2_61 2.0
    @1302 Q400B2_62 2.0
    @1304 Q400B2_63 2.0
    @1306 Q400B2_64 2.0
    @1308 Q400B2_65 2.0
    @1310 Q400B2_66 2.0
    @1312 Q400B2_67 2.0
    @1314 Q400B2_68 2.0
    @1316 Q400B2_69 2.0
    @1318 Q400B2_70 2.0
    @1320 Q400B2_71 2.0
    @1322 Q400B2_72 2.0
    @1324 Q400B2_73 2.0
    @1326 Q400B2_74 2.0
    @1328 Q400B2_75 2.0
    @1330 Q400B2_76 2.0
    @1332 Q400B2_77 2.0
    @1334 Q400B2_78 2.0
    @1336 Q400B2_79 2.0
    @1338 Q400B2_80 2.0
    @1340 Q400B2_81 2.0
    @1342 Q400B2_82 2.0
    @1344 Q400B2_83 2.0
    @1346 Q400B2_84 2.0
    @1348 Q400B2_85 2.0
    @1350 Q400B2_86 2.0
    @1352 Q400B2_87 2.0
    @1354 Q400B2_88 2.0
    @1356 Q400B2_89 2.0
    @1358 Q400B2_90 2.0
    @1360 Q400B2_91 2.0
    @1362 Q400B2_92 2.0
    @1364 Q400B2_93 2.0
    @1366 Q400B2_94 2.0
    @1368 Q400B2_95 2.0
    @1370 Q400B2_96 2.0
    @1372 Q400B2_97 2.0
    @1374 Q400B2_98 2.0
    @1376 Q400B2_99 2.0
    @1378 Q400B3_01 2.0
    @1380 Q400B3_02 2.0
    @1382 Q400B3_03 2.0
    @1384 Q400B3_04 2.0
    @1386 Q400B3_05 2.0
    @1388 Q400B3_06 2.0
    @1390 Q400B3_07 2.0
    @1392 Q400B3_08 2.0
    @1394 Q400B3_09 2.0
    @1396 Q400B3_10 2.0
    @1398 Q400B3_11 2.0
    @1400 Q400B3_12 2.0
    @1402 Q400B3_13 2.0
    @1404 Q400B3_14 2.0
    @1406 Q400B3_15 2.0
    @1408 Q400B3_16 2.0
    @1410 Q400B3_17 2.0
    @1412 Q400B3_18 2.0
    @1414 Q400B3_19 2.0
    @1416 Q400B3_20 2.0
    @1418 Q400B3_21 2.0
    @1420 Q400B3_22 2.0
    @1422 Q400B3_23 2.0
    @1424 Q400B3_24 2.0
    @1426 Q400B3_25 2.0
    @1428 Q400B3_26 2.0
    @1430 Q400B3_27 2.0
    @1432 Q400B3_28 2.0
    @1434 Q400B3_29 2.0
    @1436 Q400B3_30 2.0
    @1438 Q400B3_31 2.0
    @1440 Q400B3_32 2.0
    @1442 Q400B3_33 2.0
    @1444 Q400B3_34 2.0
    @1446 Q400B3_35 2.0
    @1448 Q400B3_36 2.0
    @1450 Q400B3_37 2.0
    @1452 Q400B3_38 2.0
    @1454 Q400B3_39 2.0
    @1456 Q400B3_40 2.0
    @1458 Q400B3_41 2.0
    @1460 Q400B3_42 2.0
    @1462 Q400B3_43 2.0
    @1464 Q400B3_44 2.0
    @1466 Q400B3_45 2.0
    @1468 Q400B3_46 2.0
    @1470 Q400B3_47 2.0
    @1472 Q400B3_48 2.0
    @1474 Q400B3_49 2.0
    @1476 Q400B3_50 2.0
    @1478 Q400B3_51 2.0
    @1480 Q400B3_52 2.0
    @1482 Q400B3_53 2.0
    @1484 Q400B3_54 2.0
    @1486 Q400B3_55 2.0
    @1488 Q400B3_56 2.0
    @1490 Q400B3_57 2.0
    @1492 Q400B3_58 2.0
    @1494 Q400B3_59 2.0
    @1496 Q400B3_60 2.0
    @1498 Q400B3_61 2.0
    @1500 Q400B3_62 2.0
    @1502 Q400B3_63 2.0
    @1504 Q400B3_64 2.0
    @1506 Q400B3_65 2.0
    @1508 Q400B3_66 2.0
    @1510 Q400B3_67 2.0
    @1512 Q400B3_68 2.0
    @1514 Q400B3_69 2.0
    @1516 Q400B3_70 2.0
    @1518 Q400B3_71 2.0
    @1520 Q400B3_72 2.0
    @1522 Q400B3_73 2.0
    @1524 Q400B3_74 2.0
    @1526 Q400B3_75 2.0
    @1528 Q400B3_76 2.0
    @1530 Q400B3_77 2.0
    @1532 Q400B3_78 2.0
    @1534 Q400B3_79 2.0
    @1536 Q400B3_80 2.0
    @1538 Q400B3_81 2.0
    @1540 Q400B3_82 2.0
    @1542 Q400B3_83 2.0
    @1544 Q400B3_84 2.0
    @1546 Q400B3_85 2.0
    @1548 Q400B3_86 2.0
    @1550 Q400B3_87 2.0
    @1552 Q400B3_88 2.0
    @1554 Q400B3_89 2.0
    @1556 Q400B3_90 2.0
    @1558 Q400B3_91 2.0
    @1560 Q400B3_92 2.0
    @1562 Q400B3_93 2.0
    @1564 Q400B3_94 2.0
    @1566 Q400B3_95 2.0
    @1568 Q400B3_96 2.0
    @1570 Q400B3_97 2.0
    @1572 Q400B3_98 2.0
    @1574 Q400B3_99 2.0
    @1576 Q400B_01 2.0
    @1578 Q400B_02 2.0
    @1580 Q400B_03 2.0
    @1582 Q400B_04 2.0
    @1584 Q400B_05 2.0
    @1586 Q400B_06 2.0
    @1588 Q400B_07 2.0
    @1590 Q400B_08 2.0
    @1592 Q400B_09 2.0
    @1594 Q400B_10 2.0
    @1596 Q400B_11 2.0
    @1598 Q400B_12 2.0
    @1600 Q400B_13 2.0
    @1602 Q400B_14 2.0
    @1604 Q400B_15 2.0
    @1606 Q400B_16 2.0
    @1608 Q400B_17 2.0
    @1610 Q400B_18 2.0
    @1612 Q400B_19 2.0
    @1614 Q400B_20 2.0
    @1616 Q400B_21 2.0
    @1618 Q400B_22 2.0
    @1620 Q400B_23 2.0
    @1622 Q400B_24 2.0
    @1624 Q400B_25 2.0
    @1626 Q400B_26 2.0
    @1628 Q400B_27 2.0
    @1630 Q400B_28 2.0
    @1632 Q400B_29 2.0
    @1634 Q400B_30 2.0
    @1636 Q400B_31 2.0
    @1638 Q400B_32 2.0
    @1640 Q400B_33 2.0
    @1642 Q400B_34 2.0
    @1644 Q400B_35 2.0
    @1646 Q400B_36 2.0
    @1648 Q400B_37 2.0
    @1650 Q400B_38 2.0
    @1652 Q400B_39 2.0
    @1654 Q400B_40 2.0
    @1656 Q400B_41 2.0
    @1658 Q400B_42 2.0
    @1660 Q400B_43 2.0
    @1662 Q400B_44 2.0
    @1664 Q400B_45 2.0
    @1666 Q400B_46 2.0
    @1668 Q400B_47 2.0
    @1670 Q400B_48 2.0
    @1672 Q400B_49 2.0
    @1674 Q400B_50 2.0
    @1676 Q400B_51 2.0
    @1678 Q400B_52 2.0
    @1680 Q400B_53 2.0
    @1682 Q400B_54 2.0
    @1684 Q400B_55 2.0
    @1686 Q400B_56 2.0
    @1688 Q400B_57 2.0
    @1690 Q400B_58 2.0
    @1692 Q400B_59 2.0
    @1694 Q400B_60 2.0
    @1696 Q400B_61 2.0
    @1698 Q400B_62 2.0
    @1700 Q400B_63 2.0
    @1702 Q400B_64 2.0
    @1704 Q400B_65 2.0
    @1706 Q400B_66 2.0
    @1708 Q400B_67 2.0
    @1710 Q400B_68 2.0
    @1712 Q400B_69 2.0
    @1714 Q400B_70 2.0
    @1716 Q400B_71 2.0
    @1718 Q400B_72 2.0
    @1720 Q400B_73 2.0
    @1722 Q400B_74 2.0
    @1724 Q400B_75 2.0
    @1726 Q400B_76 2.0
    @1728 Q400B_77 2.0
    @1730 Q400B_78 2.0
    @1732 Q400B_79 2.0
    @1734 Q400B_80 2.0
    @1736 Q400B_81 2.0
    @1738 Q400B_82 2.0
    @1740 Q400B_83 2.0
    @1742 Q400B_84 2.0
    @1744 Q400B_85 2.0
    @1746 Q400B_86 2.0
    @1748 Q400B_87 2.0
    @1750 Q400B_88 2.0
    @1752 Q400B_89 2.0
    @1754 Q400B_90 2.0
    @1756 Q400B_91 2.0
    @1758 Q400B_92 2.0
    @1760 Q400B_93 2.0
    @1762 Q400B_94 2.0
    @1764 Q400B_95 2.0
    @1766 Q400B_96 2.0
    @1768 Q400B_97 2.0
    @1770 Q400B_98 2.0
    @1772 Q400B_99 2.0
    @1774 Q400AAT  3.0
    @1777 Q400AT   3.0
    @1780 Q400B2T  3.0
    @1783 Q400B3T  3.0
    @1786 Q400BT   3.0
    @1789 Q430     1.0
    @1790 Q431     $11.
    @1801 Q440     1.0
    @1802 Q441     1.0
    @1803 Q442     1.0
    @1804 Q450     1.0
    @1805 Q451     1.0
    @1806 Q452     1.0
    @1807 Q453     1.0
    @1808 Q454A    2.0
    @1810 Q454B    2.0
    @1812 Q454C    2.0
    @1814 Q454D    2.0
    @1816 Q454E    2.0
    @1818 Q454F    2.0
    @1820 Q454X    2.0
    @1822 Q460     1.0
    @1823 Q461A_01 1.0
    @1824 Q461A_02 1.0
    @1825 Q461A_03 1.0
    @1826 Q461A_04 1.0
    @1827 Q461A_05 1.0
    @1828 Q461A_06 1.0
    @1829 Q461A_07 1.0
    @1830 Q461A_08 1.0
    @1831 Q461A_09 1.0
    @1832 Q461A_10 1.0
    @1833 Q461A_11 1.0
    @1834 Q461A_12 1.0
    @1835 Q461A_13 1.0
    @1836 Q461A_14 1.0
    @1837 Q461A_15 1.0
    @1838 Q461A_16 1.0
    @1839 Q461B_01 1.0
    @1840 Q461B_02 1.0
    @1841 Q461B_03 1.0
    @1842 Q461B_04 1.0
    @1843 Q461B_05 1.0
    @1844 Q461B_06 1.0
    @1845 Q461B_07 1.0
    @1846 Q461B_08 1.0
    @1847 Q461B_09 1.0
    @1848 Q461B_10 1.0
    @1849 Q461B_11 1.0
    @1850 Q461B_12 1.0
    @1851 Q461B_13 1.0
    @1852 Q461B_14 1.0
    @1853 Q461B_15 1.0
    @1854 Q461B_16 1.0
    @1855 Q461C_01 1.0
    @1856 Q461C_02 1.0
    @1857 Q461C_03 1.0
    @1858 Q461C_04 1.0
    @1859 Q461C_05 1.0
    @1860 Q461C_06 1.0
    @1861 Q461C_07 1.0
    @1862 Q461C_08 1.0
    @1863 Q461C_09 1.0
    @1864 Q461C_10 1.0
    @1865 Q461C_11 1.0
    @1866 Q461C_12 1.0
    @1867 Q461C_13 1.0
    @1868 Q461C_14 1.0
    @1869 Q461C_15 1.0
    @1870 Q461C_16 1.0
    @1871 Q464     1.0
    @1872 Q465     1.0
    @1873 Q471     4.0
    @1877 Q472     4.0
    @1881 Q501A_1  1.0
    @1882 Q501A_2  1.0
    @1883 Q501A_3  1.0
    @1884 Q501A_4  1.0
    @1885 Q501A_5  1.0
    @1886 Q501A_6  1.0
    @1887 Q501A_7  1.0
    @1888 Q501A_8  1.0
    @1889 Q501A_9  1.0
    @1890 Q501B_1  1.0
    @1891 Q501B_2  1.0
    @1892 Q501B_3  1.0
    @1893 Q501B_4  1.0
    @1894 Q501B_5  1.0
    @1895 Q501B_6  1.0
    @1896 Q501B_7  1.0
    @1897 Q501B_8  1.0
    @1898 Q501B_9  1.0
    @1899 Q502A_1  1.0
    @1900 Q502A_2  1.0
    @1901 Q502A_3  1.0
    @1902 Q502A_4  1.0
    @1903 Q502A_5  1.0
    @1904 Q502B_1  3.0
    @1907 Q502B_2  3.0
    @1910 Q502B_3  3.0
    @1913 Q502B_4  3.0
    @1916 Q502B_5  3.0
    @1919 Q502C_1  3.0
    @1922 Q502C_2  3.0
    @1925 Q502C_3  3.0
    @1928 Q502C_4  3.0
    @1931 Q502C_5  3.0
    @1934 Q502D_1  1.0
    @1935 Q502D_2  1.0
    @1936 Q502D_3  1.0
    @1937 Q502D_4  1.0
    @1938 Q502D_5  1.0
    @1939 Q502E_1  3.0
    @1942 Q502E_2  3.0
    @1945 Q502E_3  3.0
    @1948 Q502E_4  3.0
    @1951 Q502E_5  3.0
    @1954 Q502F_1  3.0
    @1957 Q502F_2  3.0
    @1960 Q502F_3  3.0
    @1963 Q502F_4  3.0
    @1966 Q502F_5  3.0
    @1969 Q502G_1  2.0
    @1971 Q502G_2  2.0
    @1973 Q502G_3  2.0
    @1975 Q502G_4  2.0
    @1977 Q502G_5  2.0
    @1979 Q503     1.0
    @1980 Q504     1.0
    @1981 Q600     2.0
    @1983 Q601     2.0
    @1985 Q603     1.0
    @1986 Q605     1.0
    @1987 Q607     1.0
    @1988 Q608     1.0
    @1989 Q609     1.0
    @1990 Q610     1.0
    @1991 Q611     1.0
    @1992 Q620     2.0
    @1994 Q620A    $11.
    @2005 Q620B    1.0
    @2006 Q700A_01 1.0
    @2007 Q700A_02 1.0
    @2008 Q700A_03 1.0
    @2009 Q700A_04 1.0
    @2010 Q700A_05 1.0
    @2011 Q700A_06 1.0
    @2012 Q700A_07 1.0
    @2013 Q700A_08 1.0
    @2014 Q700A_09 1.0
    @2015 Q700A_10 1.0
    @2016 Q700A_11 1.0
    @2017 Q700A_12 1.0
    @2018 Q700A_13 1.0
    @2019 Q700A_14 1.0
    @2020 Q700A_15 1.0
    @2021 Q700A_16 1.0
    @2022 Q700A_17 1.0
    @2023 Q700A_18 1.0
    @2024 Q700A_19 1.0
    @2025 Q700A_20 1.0
    @2026 Q700A_21 1.0
    @2027 Q700A_22 1.0
    @2028 Q700A_23 1.0
    @2029 Q700A_24 1.0
    @2030 Q700A_25 1.0
    @2031 Q700B_01 1.0
    @2032 Q700B_02 1.0
    @2033 Q700B_03 1.0
    @2034 Q700B_04 1.0
    @2035 Q700B_05 1.0
    @2036 Q700B_06 1.0
    @2037 Q700B_07 1.0
    @2038 Q700B_08 1.0
    @2039 Q700B_09 1.0
    @2040 Q700B_10 1.0
    @2041 Q700B_11 1.0
    @2042 Q700B_12 1.0
    @2043 Q700B_13 1.0
    @2044 Q700B_14 1.0
    @2045 Q700B_15 1.0
    @2046 Q700B_16 1.0
    @2047 Q700B_17 1.0
    @2048 Q700B_18 1.0
    @2049 Q700B_19 1.0
    @2050 Q700B_20 1.0
    @2051 Q700B_21 1.0
    @2052 Q700B_22 1.0
    @2053 Q700B_23 1.0
    @2054 Q700B_24 1.0
    @2055 Q700B_25 1.0
    @2056 Q710_01  1.0
    @2057 Q710_02  1.0
    @2058 Q710_03  1.0
    @2059 Q710_04  1.0
    @2060 Q710_05  1.0
    @2061 Q710_06  1.0
    @2062 Q710_07  1.0
    @2063 Q710_08  1.0
    @2064 Q710_09  1.0
    @2065 Q710_10  1.0
    @2066 Q710_11  1.0
    @2067 Q710_12  1.0
    @2068 Q710_13  1.0
    @2069 Q711     1.0
    @2070 Q720     1.0
    @2071 Q801     1.0
    @2072 Q802A_1  1.0
    @2073 Q802A_2  1.0
    @2074 Q802A_3  1.0
    @2075 Q802A_4  1.0
    @2076 Q802A_5  1.0
    @2077 Q802A_6  1.0
    @2078 Q802A_7  1.0
    @2079 Q802B_1  1.0
    @2080 Q802B_2  1.0
    @2081 Q802B_3  1.0
    @2082 Q802B_4  1.0
    @2083 Q802B_5  1.0
    @2084 Q802B_6  1.0
    @2085 Q802B_7  1.0
    @2086 Q802C_1  1.0
    @2087 Q802C_2  1.0
    @2088 Q802C_3  1.0
    @2089 Q802C_4  1.0
    @2090 Q802C_5  1.0
    @2091 Q802C_6  1.0
    @2092 Q802C_7  1.0
    @2093 Q829     1.0
    @2094 Q830     1.0
    @2095 Q831A_1  1.0
    @2096 Q831A_2  1.0
    @2097 Q831B_1  1.0
    @2098 Q831B_2  1.0
    @2099 Q831C_1  1.0
    @2100 Q831C_2  1.0
    @2101 Q832     1.0
    @2102 Q833     1.0
    @2103 Q834     1.0
    @2104 Q835     1.0
    @2105 Q836     1.0
    @2106 Q837A_1  1.0
    @2107 Q837A_2  1.0
    @2108 Q837A_3  1.0
    @2109 Q837B_1  1.0
    @2110 Q837B_2  1.0
    @2111 Q837B_3  1.0
    @2112 Q838     1.0
    @2113 Q839A_1  1.0
    @2114 Q839A_2  1.0
    @2115 Q839A_3  1.0
    @2116 Q839A_4  1.0
    @2117 Q839B_1  1.0
    @2118 Q839B_2  1.0
    @2119 Q839B_3  1.0
    @2120 Q839B_4  1.0
    @2121 Q840A_1  1.0
    @2122 Q840A_2  1.0
    @2123 Q840A_3  1.0
    @2124 Q840A_4  1.0
    @2125 Q840A_5  1.0
    @2126 Q840A_6  1.0
    @2127 Q840A_7  1.0
    @2128 Q840A_8  1.0
    @2129 Q840B_1  1.0
    @2130 Q840B_2  1.0
    @2131 Q840B_3  1.0
    @2132 Q840B_4  1.0
    @2133 Q840B_5  1.0
    @2134 Q840B_6  1.0
    @2135 Q840B_7  1.0
    @2136 Q840B_8  1.0
    @2137 Q840C_1  1.0
    @2138 Q840C_2  1.0
    @2139 Q840C_3  1.0
    @2140 Q840C_4  1.0
    @2141 Q840C_5  1.0
    @2142 Q840C_6  1.0
    @2143 Q840C_7  1.0
    @2144 Q840C_8  1.0
    @2145 Q848     1.0
    @2146 Q849A_1  1.0
    @2147 Q849A_2  1.0
    @2148 Q849A_3  1.0
    @2149 Q849A_4  1.0
    @2150 Q849B_1  1.0
    @2151 Q849B_2  1.0
    @2152 Q849B_3  1.0
    @2153 Q849B_4  1.0
    @2154 Q850     1.0
    @2155 Q851A_1  1.0
    @2156 Q851A_2  1.0
    @2157 Q851A_3  1.0
    @2158 Q851A_4  1.0
    @2159 Q851B_1  1.0
    @2160 Q851B_2  1.0
    @2161 Q851B_3  1.0
    @2162 Q851B_4  1.0
    @2163 Q852     1.0
    @2164 Q853A_1  1.0
    @2165 Q853A_2  1.0
    @2166 Q853A_3  1.0
    @2167 Q853B_1  1.0
    @2168 Q853B_2  1.0
    @2169 Q853B_3  1.0
    @2170 Q854     1.0
    @2171 Q855     1.0
    @2172 Q856     1.0
    @2173 Q857     1.0
    @2174 Q858A_1  1.0
    @2175 Q858A_2  1.0
    @2176 Q858A_3  1.0
    @2177 Q858A_4  1.0
    @2178 Q858A_5  1.0
    @2179 Q858B_1  1.0
    @2180 Q858B_2  1.0
    @2181 Q858B_3  1.0
    @2182 Q858B_4  1.0
    @2183 Q858B_5  1.0
    @2184 Q858C_1  1.0
    @2185 Q858C_2  1.0
    @2186 Q858C_3  1.0
    @2187 Q858C_4  1.0
    @2188 Q858C_5  1.0
    @2189 Q859     1.0
    @2190 Q860A_1  1.0
    @2191 Q860A_2  1.0
    @2192 Q860B_1  1.0
    @2193 Q860B_2  1.0
    @2194 Q861     1.0
    @2195 Q862A_1  1.0
    @2196 Q862A_2  1.0
    @2197 Q862A_3  1.0
    @2198 Q862A_4  1.0
    @2199 Q862A_5  1.0
    @2200 Q862A_6  1.0
    @2201 Q862A_7  1.0
    @2202 Q862A_8  1.0
    @2203 Q862B_1  1.0
    @2204 Q862B_2  1.0
    @2205 Q862B_3  1.0
    @2206 Q862B_4  1.0
    @2207 Q862B_5  1.0
    @2208 Q862B_6  1.0
    @2209 Q862B_7  1.0
    @2210 Q862B_8  1.0
    @2211 Q862C_1  1.0
    @2212 Q862C_2  1.0
    @2213 Q862C_3  1.0
    @2214 Q862C_4  1.0
    @2215 Q862C_5  1.0
    @2216 Q862C_6  1.0
    @2217 Q862C_7  1.0
    @2218 Q862C_8  1.0
    @2219 Q863     1.0
    @2220 Q864     1.0
    @2221 Q865     1.0
    @2222 Q866     1.0
    @2223 Q867     1.0
    @2224 Q868     1.0
    @2225 Q869     1.0
    @2226 Q870     1.0
    @2227 Q871     1.0
    @2228 Q872     1.0
    @2229 Q873     1.0
    @2230 Q880     1.0
    @2231 Q881A_1  1.0
    @2232 Q881A_2  1.0
    @2233 Q881A_3  1.0
    @2234 Q881A_4  1.0
    @2235 Q881A_5  1.0
    @2236 Q881B_1  1.0
    @2237 Q881B_2  1.0
    @2238 Q881B_3  1.0
    @2239 Q881B_4  1.0
    @2240 Q881B_5  1.0
    @2241 Q881C_1  1.0
    @2242 Q881C_2  1.0
    @2243 Q881C_3  1.0
    @2244 Q881C_4  1.0
    @2245 Q881C_5  1.0
    @2246 Q901_01  1.0
    @2247 Q901_02  1.0
    @2248 Q901_03  1.0
    @2249 Q901_04  1.0
    @2250 Q901_05  1.0
    @2251 Q901_06  1.0
    @2252 Q901_07  1.0
    @2253 Q901_08  1.0
    @2254 Q901_09  1.0
    @2255 Q901_10  1.0
    @2256 Q901_11  1.0
    @2257 Q901_12  1.0
    @2258 Q901_13  1.0
    @2259 Q901_14  1.0
    @2260 Q901_15  1.0
    @2261 Q901_16  1.0
    @2262 Q901_17  1.0
    @2263 Q901_18  1.0
    @2264 Q901_19  1.0
    @2265 Q901_20  1.0
    @2266 Q901_21  1.0
    @2267 Q901_22  1.0
    @2268 Q901_23  1.0
    @2269 Q901_24  1.0
    @2270 Q901_25  1.0
    @2271 Q901_26  1.0
    @2272 Q901_27  1.0
    @2273 Q901_28  1.0
    @2274 Q901_29  1.0
    @2275 Q901_30  1.0
    @2276 Q901_31  1.0
    @2277 Q901_32  1.0
    @2278 Q901_33  1.0
    @2279 Q901_34  1.0
    @2280 Q901_35  1.0
    @2281 Q901_36  1.0
    @2282 Q901_37  1.0
    @2283 Q901_38  1.0
    @2284 Q902_1   1.0
    @2285 Q902_2   1.0
    @2286 Q902_3   1.0
    @2287 Q902_4   1.0
    @2288 Q902_5   1.0
    @2289 Q903_01  1.0
    @2290 Q903_02  1.0
    @2291 Q903_03  1.0
    @2292 Q903_04  1.0
    @2293 Q903_05  1.0
    @2294 Q903_06  1.0
    @2295 Q903_07  1.0
    @2296 Q903_08  1.0
    @2297 Q903_09  1.0
    @2298 Q903_10  1.0
    @2299 Q903_11  1.0
    @2300 Q903_12  1.0
    @2301 Q903_13  1.0
    @2302 Q903_14  1.0
    @2303 Q903_15  1.0
    @2304 Q903_16  1.0
    @2305 Q903_17  1.0
    @2306 Q903_18  1.0
    @2307 Q903_19  1.0
    @2308 Q903_20  1.0
    @2309 Q903_21  1.0
    @2310 Q903_22  1.0
    @2311 Q903_23  1.0
    @2312 Q903_24  1.0
    @2313 Q903_25  1.0
    @2314 Q903_26  1.0
    @2315 Q903_27  1.0
    @2316 Q903_28  1.0
    @2317 Q903_29  1.0
    @2318 Q903_30  1.0
    @2319 Q903_31  1.0
    @2320 Q903_32  1.0
    @2321 Q903_33  1.0
    @2322 Q903_34  1.0
    @2323 Q903_35  1.0
    @2324 Q903_36  1.0
    @2325 Q903_37  1.0
    @2326 Q903_38  1.0
    @2327 Q903_39  1.0
    @2328 Q903_40  1.0
    @2329 Q903_41  1.0
    @2330 Q903_42  1.0
    @2331 Q903_43  1.0
    @2332 Q903_44  1.0
    @2333 Q903_45  1.0
    @2334 Q903_46  1.0
    @2335 Q903_47  1.0
    @2336 Q904_01  1.0
    @2337 Q904_02  1.0
    @2338 Q904_03  1.0
    @2339 Q904_04  1.0
    @2340 Q904_05  1.0
    @2341 Q904_06  1.0
    @2342 Q904_07  1.0
    @2343 Q904_08  1.0
    @2344 Q904_09  1.0
    @2345 Q904_10  1.0
    @2346 Q904_11  1.0
    @2347 Q906_01  1.0
    @2348 Q906_02  1.0
    @2349 Q906_03  1.0
    @2350 Q906_04  1.0
    @2351 Q906_05  1.0
    @2352 Q906_06  1.0
    @2353 Q906_07  1.0
    @2354 Q906_08  1.0
    @2355 Q906_09  1.0
    @2356 Q906_10  1.0
    @2357 Q906_11  1.0
    @2358 Q906_12  1.0
    @2359 Q906_13  1.0
    @2360 Q906_14  1.0
    @2361 Q906_15  1.0
    @2362 Q906_16  1.0
    @2363 Q906_17  1.0
    @2364 Q906_18  1.0
    @2365 Q906_19  1.0
    @2366 Q906_20  1.0
    @2367 Q906_21  1.0
    @2368 Q906_22  1.0
    @2369 Q906_23  1.0
    @2370 Q906_24  1.0
    @2371 Q906_25  1.0
    @2372 Q906_26  1.0
    @2373 Q907_1   1.0
    @2374 Q907_2   1.0
    @2375 Q907_3   1.0
    @2376 Q907_4   1.0
    @2377 Q907_5   1.0
    @2378 Q907_6   1.0
    @2379 Q908_01  1.0
    @2380 Q908_02  1.0
    @2381 Q908_03  1.0
    @2382 Q908_04  1.0
    @2383 Q908_05  1.0
    @2384 Q908_06  1.0
    @2385 Q908_07  1.0
    @2386 Q908_08  1.0
    @2387 Q908_09  1.0
    @2388 Q908_10  1.0
    @2389 Q908_11  1.0
    @2390 Q908_12  1.0
    @2391 Q908_13  1.0
    @2392 Q908_14  1.0
    @2393 Q908_15  1.0
    @2394 Q908_16  1.0
    @2395 Q908_17  1.0
    @2396 Q909_1   1.0
    @2397 Q909_2   1.0
    @2398 Q909_3   1.0
    @2399 Q909_4   1.0
    @2400 Q909_5   1.0
    @2401 Q910     1.0
    @2402 Q911     1.0
    @2403 Q912_1   1.0
    @2404 Q912_2   1.0
    @2405 Q912_3   1.0
    @2406 Q912_4   1.0
    @2407 Q912_5   1.0
    @2408 Q912_6   1.0
    @2409 Q912_7   1.0
    @2410 Q912_8   1.0
    @2411 Q921_01  1.0
    @2412 Q921_02  1.0
    @2413 Q921_03  1.0
    @2414 Q921_04  1.0
    @2415 Q921_05  1.0
    @2416 Q921_06  1.0
    @2417 Q921_07  1.0
    @2418 Q921_08  1.0
    @2419 Q921_09  1.0
    @2420 Q921_10  1.0
    @2421 Q921_11  1.0
    @2422 Q922_1   1.0
    @2423 Q922_2   1.0
    @2424 Q922_3   1.0
    @2425 Q922_4   1.0
    @2426 Q922_5   1.0
    @2427 Q923     1.0
    @2428 Q924     1.0
    @2429 Q931_01  1.0
    @2430 Q931_02  1.0
    @2431 Q931_03  1.0
    @2432 Q931_04  1.0
    @2433 Q931_05  1.0
    @2434 Q931_06  1.0
    @2435 Q931_07  1.0
    @2436 Q931_08  1.0
    @2437 Q931_09  1.0
    @2438 Q931_10  1.0
    @2439 Q1001A_1 2.0
    @2441 Q1001A_2 2.0
    @2443 Q1001A_3 2.0
    @2445 Q1001A_4 2.0
    @2447 Q1001A_5 2.0
    @2449 Q1001A_6 2.0
    @2451 Q1001B_1 2.0
    @2453 Q1001B_2 2.0
    @2455 Q1001B_3 2.0
    @2457 Q1001B_4 2.0
    @2459 Q1001B_5 2.0
    @2461 Q1001B_6 2.0
    @2463 Q1002    1.0
    @2464 Q1003    1.0
    @2465 Q1004    1.0
    @2466 Q1005    1.0
    @2467 Q1006_1  1.0
    @2468 Q1006_2  1.0
    @2469 Q1006_3  1.0
    @2470 Q1007    1.0
    @2471 Q1008    1.0
    @2472 Q1009    1.0
    @2473 Q1010    1.0
    @2474 Q1011    1.0
    @2475 Q1012_1  1.0
    @2476 Q1012_2  1.0
    @2477 Q1012_3  1.0
    @2478 Q1012_4  1.0
    @2479 Q1012_5  1.0
    @2480 Q1012_6  1.0
    @2481 Q1013    1.0
    @2482 Q1014    1.0
    @2483 Q1015    1.0
    @2484 Q1016A   2.0
    @2486 Q1016B   2.0
    @2488 Q1017    1.0
    @2489 Q1018    1.0
    @2490 Q1019    1.0
    @2491 Q1020    1.0
    @2492 Q1021    1.0
    @2493 Q1022A   2.0
    @2495 Q1022B   2.0
    @2497 Q1023    1.0
    @2498 Q1024    1.0
    @2499 Q1025    1.0
    @2500 Q1026    1.0
    @2501 Q1027    1.0
    @2502 Q1050    2.0
    @2504 Q1051_01 1.0
    @2505 Q1051_02 1.0
    @2506 Q1051_03 1.0
    @2507 Q1051_04 1.0
    @2508 Q1051_05 1.0
    @2509 Q1051_06 1.0
    @2510 Q1051_07 1.0
    @2511 Q1051_08 1.0
    @2512 Q1051_09 1.0
    @2513 Q1051_10 1.0
    @2514 Q1051_11 1.0
    @2515 Q1051_12 1.0
    @2516 Q1051_13 1.0
    @2517 Q1052    1.0
    @2518 Q1101A   2.0
    @2520 Q1101B   2.0
    @2522 Q1102    1.0
    @2523 Q1103    1.0
    @2524 Q1103A   1.0
    @2525 Q1103B   1.0
    @2526 Q1104A_1 1.0
    @2527 Q1104A_2 1.0
    @2528 Q1104A_3 1.0
    @2529 Q1104A_4 1.0
    @2530 Q1104A_5 1.0
    @2531 Q1104A_6 1.0
    @2532 Q1104B_1 1.0
    @2533 Q1104B_2 1.0
    @2534 Q1104B_3 1.0
    @2535 Q1104B_4 1.0
    @2536 Q1104B_5 1.0
    @2537 Q1104B_6 1.0
    @2538 Q1201A   2.0
    @2540 Q1201B   2.0
    @2542 Q1202_01 1.0
    @2543 Q1202_02 1.0
    @2544 Q1202_03 1.0
    @2545 Q1202_04 1.0
    @2546 Q1202_05 1.0
    @2547 Q1202_06 1.0
    @2548 Q1202_07 1.0
    @2549 Q1202_08 1.0
    @2550 Q1202_09 1.0
    @2551 Q1202_10 1.0
    @2552 Q1202_11 1.0
    @2553 Q1202_12 1.0
    @2554 Q1203    1.0
    @2555 Q1203A   1.0
    @2556 Q1204    1.0
    @2557 Q1205    1.0
    @2558 Q1206    1.0
    @2559 Q1207    1.0
    @2560 Q1208    1.0
    @2561 Q1209_1  1.0
    @2562 Q1209_2  1.0
    @2563 Q1209_3  1.0
    @2564 Q1209_4  1.0
    @2565 Q1209_5  1.0
    @2566 Q1209_6  1.0
    @2567 Q1209_7  1.0
    @2568 Q1209_8  1.0
    @2569 Q1209_9  1.0
    @2570 Q1210A_01 1.0
    @2571 Q1210A_02 1.0
    @2572 Q1210A_03 1.0
    @2573 Q1210A_04 1.0
    @2574 Q1210A_05 1.0
    @2575 Q1210A_06 1.0
    @2576 Q1210A_07 1.0
    @2577 Q1210A_08 1.0
    @2578 Q1210A_09 1.0
    @2579 Q1210A_10 1.0
    @2580 Q1210A_11 1.0
    @2581 Q1210B_01 1.0
    @2582 Q1210B_02 1.0
    @2583 Q1210B_03 1.0
    @2584 Q1210B_04 1.0
    @2585 Q1210B_05 1.0
    @2586 Q1210B_06 1.0
    @2587 Q1210B_07 1.0
    @2588 Q1210B_08 1.0
    @2589 Q1210B_09 1.0
    @2590 Q1210B_10 1.0
    @2591 Q1210B_11 1.0
    @2592 Q1211_1  1.0
    @2593 Q1211_2  1.0
    @2594 Q1211_3  1.0
    @2595 Q1211_4  1.0
    @2596 Q1211_5  1.0
    @2597 Q1212    1.0
    @2598 Q1213    1.0
    @2599 Q1250    2.0
    @2601 Q1251_01 1.0
    @2602 Q1251_02 1.0
    @2603 Q1251_03 1.0
    @2604 Q1251_04 1.0
    @2605 Q1251_05 1.0
    @2606 Q1251_06 1.0
    @2607 Q1251_07 1.0
    @2608 Q1251_08 1.0
    @2609 Q1251_09 1.0
    @2610 Q1251_10 1.0
    @2611 Q1251_11 1.0
    @2612 Q1251_12 1.0
    @2613 Q1251_13 1.0
    @2614 Q1252    1.0
    @2615 Q1254    1.0
    @2616 Q1255    1.0
    @2617 Q1256    1.0
    @2618 Q1257    1.0
    @2619 Q1258    1.0
    @2620 Q1259    1.0
    @2621 Q1260    1.0
    @2622 Q1262    1.0
    @2623 Q1263    1.0
    @2624 Q1264    1.0
    @2625 Q1265    1.0
    @2626 Q1266    1.0
    @2627 Q1267    1.0
    @2628 Q1268    1.0
    @2629 Q1301    2.0
    @2631 Q1302_01 1.0
    @2632 Q1302_02 1.0
    @2633 Q1302_03 1.0
    @2634 Q1302_04 1.0
    @2635 Q1302_05 1.0
    @2636 Q1302_06 1.0
    @2637 Q1302_07 1.0
    @2638 Q1302_08 1.0
    @2639 Q1302_09 1.0
    @2640 Q1302_10 1.0
    @2641 Q1302_11 1.0
    @2642 Q1302_12 1.0
    @2643 Q1302_13 1.0
    @2644 Q1302_14 1.0
    @2645 Q1302_15 1.0
    @2646 Q1303    1.0
    @2647 Q1304    1.0
    @2648 Q1305    1.0
    @2649 Q1306    1.0
    @2650 Q1307    1.0
    @2651 Q1308    1.0
    @2652 Q1309    1.0
    @2653 Q1310_1  1.0
    @2654 Q1310_2  1.0
    @2655 Q1310_3  1.0
    @2656 Q1311    1.0
    @2657 Q1314A_1 1.0
    @2658 Q1314A_2 1.0
    @2659 Q1314A_3 1.0
    @2660 Q1314A_4 1.0
    @2661 Q1314A_5 1.0
    @2662 Q1314A_6 1.0
    @2663 Q1314A_7 1.0
    @2664 Q1314A_8 1.0
    @2665 Q1314A_9 1.0
    @2666 Q1314B_1 1.0
    @2667 Q1314B_2 1.0
    @2668 Q1314B_3 1.0
    @2669 Q1314B_4 1.0
    @2670 Q1314B_5 1.0
    @2671 Q1314B_6 1.0
    @2672 Q1314B_7 1.0
    @2673 Q1314B_8 1.0
    @2674 Q1314B_9 1.0
    @2675 Q1316_1  1.0
    @2676 Q1316_2  1.0
    @2677 Q1316_3  1.0
    @2678 Q1316_4  1.0
    @2679 Q1316_5  1.0
    @2680 Q1318_01 1.0
    @2681 Q1318_02 1.0
    @2682 Q1318_03 1.0
    @2683 Q1318_04 1.0
    @2684 Q1318_05 1.0
    @2685 Q1318_06 1.0
    @2686 Q1318_07 1.0
    @2687 Q1318_08 1.0
    @2688 Q1318_09 1.0
    @2689 Q1318_10 1.0
    @2690 Q1318_11 1.0
    @2691 Q1318_12 1.0
    @2692 Q1320_01 1.0
    @2693 Q1320_02 1.0
    @2694 Q1320_03 1.0
    @2695 Q1320_04 1.0
    @2696 Q1320_05 1.0
    @2697 Q1320_06 1.0
    @2698 Q1320_07 1.0
    @2699 Q1320_08 1.0
    @2700 Q1320_09 1.0
    @2701 Q1320_10 1.0
    @2702 Q1320_11 1.0
    @2703 Q1320_12 1.0
    @2704 Q1321    1.0
    @2705 Q1322    $6.
    @2711 Q1350    2.0
    @2713 Q1351_01 1.0
    @2714 Q1351_02 1.0
    @2715 Q1351_03 1.0
    @2716 Q1351_04 1.0
    @2717 Q1351_05 1.0
    @2718 Q1351_06 1.0
    @2719 Q1351_07 1.0
    @2720 Q1351_08 1.0
    @2721 Q1351_09 1.0
    @2722 Q1351_10 1.0
    @2723 Q1351_11 1.0
    @2724 Q1351_12 1.0
    @2725 Q1351_13 1.0
    @2726 Q1352    1.0
    @2727 Q1353B   2.0
    @2729 Q1353CA_01 1.0
    @2730 Q1353CA_02 1.0
    @2731 Q1353CA_03 1.0
    @2732 Q1353CA_04 1.0
    @2733 Q1353CA_05 1.0
    @2734 Q1353CA_06 1.0
    @2735 Q1353CA_07 1.0
    @2736 Q1353CA_08 1.0
    @2737 Q1353CA_09 1.0
    @2738 Q1353CA_10 1.0
    @2739 Q1353CA_11 1.0
    @2740 Q1353CA_12 1.0
    @2741 Q1353CA_13 1.0
    @2742 Q1353CA_14 1.0
    @2743 Q1353CA_15 1.0
    @2744 Q1353CA_16 1.0
    @2745 Q1353CA_17 1.0
    @2746 Q1353CA_18 1.0
    @2747 Q1353CA_19 1.0
    @2748 Q1353CA_20 1.0
    @2749 Q1353CA_21 1.0
    @2750 Q1353CA_22 1.0
    @2751 Q1353CA_23 1.0
    @2752 Q1353CA_24 1.0
    @2753 Q1353CA_25 1.0
    @2754 Q1353CA_26 1.0
    @2755 Q1353CA_27 1.0
    @2756 Q1353CA_28 1.0
    @2757 Q1353CA_29 1.0
    @2758 Q1353CA_30 1.0
    @2759 Q1353CA_31 1.0
    @2760 Q1353CA_32 1.0
    @2761 Q1353CA_33 1.0
    @2762 Q1353CA_34 1.0
    @2763 Q1353CA_35 1.0
    @2764 Q1353CA_36 1.0
    @2765 Q1353CA_37 1.0
    @2766 Q1353CA_38 1.0
    @2767 Q1353CA_39 1.0
    @2768 Q1353CA_40 1.0
    @2769 Q1353CA_41 1.0
    @2770 Q1353CA_42 1.0
    @2771 Q1353CA_43 1.0
    @2772 Q1353CA_44 1.0
    @2773 Q1353CB_01 1.0
    @2774 Q1353CB_02 1.0
    @2775 Q1353CB_03 1.0
    @2776 Q1353CB_04 1.0
    @2777 Q1353CB_05 1.0
    @2778 Q1353CB_06 1.0
    @2779 Q1353CB_07 1.0
    @2780 Q1353CB_08 1.0
    @2781 Q1353CB_09 1.0
    @2782 Q1353CB_10 1.0
    @2783 Q1353CB_11 1.0
    @2784 Q1353CB_12 1.0
    @2785 Q1353CB_13 1.0
    @2786 Q1353CB_14 1.0
    @2787 Q1353CB_15 1.0
    @2788 Q1353CB_16 1.0
    @2789 Q1353CB_17 1.0
    @2790 Q1353CB_18 1.0
    @2791 Q1353CB_19 1.0
    @2792 Q1353CB_20 1.0
    @2793 Q1353CB_21 1.0
    @2794 Q1353CB_22 1.0
    @2795 Q1353CB_23 1.0
    @2796 Q1353CB_24 1.0
    @2797 Q1353CB_25 1.0
    @2798 Q1353CB_26 1.0
    @2799 Q1353CB_27 1.0
    @2800 Q1353CB_28 1.0
    @2801 Q1353CB_29 1.0
    @2802 Q1353CB_30 1.0
    @2803 Q1353CB_31 1.0
    @2804 Q1353CB_32 1.0
    @2805 Q1353CB_33 1.0
    @2806 Q1353CB_34 1.0
    @2807 Q1353CB_35 1.0
    @2808 Q1353CB_36 1.0
    @2809 Q1353CB_37 1.0
    @2810 Q1353CB_38 1.0
    @2811 Q1353CB_39 1.0
    @2812 Q1353CB_40 1.0
    @2813 Q1353CB_41 1.0
    @2814 Q1353CB_42 1.0
    @2815 Q1353CB_43 1.0
    @2816 Q1353CB_44 1.0
    @2817 Q1353DA_1 1.0
    @2818 Q1353DA_2 1.0
    @2819 Q1353DA_3 1.0
    @2820 Q1353DA_4 1.0
    @2821 Q1353DA_5 1.0
    @2822 Q1353DB_1 1.0
    @2823 Q1353DB_2 1.0
    @2824 Q1353DB_3 1.0
    @2825 Q1353DB_4 1.0
    @2826 Q1353DB_5 1.0
    @2827 Q1353E_1 1.0
    @2828 Q1353E_2 1.0
    @2829 Q1353E_3 1.0
    @2830 Q1353E_4 1.0
    @2831 Q1353G   2.0
    @2833 Q1353HA_01 1.0
    @2834 Q1353HA_02 1.0
    @2835 Q1353HA_03 1.0
    @2836 Q1353HA_04 1.0
    @2837 Q1353HA_05 1.0
    @2838 Q1353HA_06 1.0
    @2839 Q1353HA_07 1.0
    @2840 Q1353HA_08 1.0
    @2841 Q1353HA_09 1.0
    @2842 Q1353HA_10 1.0
    @2843 Q1353HA_11 1.0
    @2844 Q1353HA_12 1.0
    @2845 Q1353HA_13 1.0
    @2846 Q1353HA_14 1.0
    @2847 Q1353HA_15 1.0
    @2848 Q1353HA_16 1.0
    @2849 Q1353HA_17 1.0
    @2850 Q1353HA_18 1.0
    @2851 Q1353HA_19 1.0
    @2852 Q1353HA_20 1.0
    @2853 Q1353HA_21 1.0
    @2854 Q1353HA_22 1.0
    @2855 Q1353HA_23 1.0
    @2856 Q1353HA_24 1.0
    @2857 Q1353HA_25 1.0
    @2858 Q1353HA_26 1.0
    @2859 Q1353HA_27 1.0
    @2860 Q1353HA_28 1.0
    @2861 Q1353HA_29 1.0
    @2862 Q1353HA_30 1.0
    @2863 Q1353HA_31 1.0
    @2864 Q1353HA_32 1.0
    @2865 Q1353HA_33 1.0
    @2866 Q1353HA_34 1.0
    @2867 Q1353HA_35 1.0
    @2868 Q1353HA_36 1.0
    @2869 Q1353HA_37 1.0
    @2870 Q1353HA_38 1.0
    @2871 Q1353HA_39 1.0
    @2872 Q1353HA_40 1.0
    @2873 Q1353HA_41 1.0
    @2874 Q1353HA_42 1.0
    @2875 Q1353HA_43 1.0
    @2876 Q1353HA_44 1.0
    @2877 Q1353HB_01 1.0
    @2878 Q1353HB_02 1.0
    @2879 Q1353HB_03 1.0
    @2880 Q1353HB_04 1.0
    @2881 Q1353HB_05 1.0
    @2882 Q1353HB_06 1.0
    @2883 Q1353HB_07 1.0
    @2884 Q1353HB_08 1.0
    @2885 Q1353HB_09 1.0
    @2886 Q1353HB_10 1.0
    @2887 Q1353HB_11 1.0
    @2888 Q1353HB_12 1.0
    @2889 Q1353HB_13 1.0
    @2890 Q1353HB_14 1.0
    @2891 Q1353HB_15 1.0
    @2892 Q1353HB_16 1.0
    @2893 Q1353HB_17 1.0
    @2894 Q1353HB_18 1.0
    @2895 Q1353HB_19 1.0
    @2896 Q1353HB_20 1.0
    @2897 Q1353HB_21 1.0
    @2898 Q1353HB_22 1.0
    @2899 Q1353HB_23 1.0
    @2900 Q1353HB_24 1.0
    @2901 Q1353HB_25 1.0
    @2902 Q1353HB_26 1.0
    @2903 Q1353HB_27 1.0
    @2904 Q1353HB_28 1.0
    @2905 Q1353HB_29 1.0
    @2906 Q1353HB_30 1.0
    @2907 Q1353HB_31 1.0
    @2908 Q1353HB_32 1.0
    @2909 Q1353HB_33 1.0
    @2910 Q1353HB_34 1.0
    @2911 Q1353HB_35 1.0
    @2912 Q1353HB_36 1.0
    @2913 Q1353HB_37 1.0
    @2914 Q1353HB_38 1.0
    @2915 Q1353HB_39 1.0
    @2916 Q1353HB_40 1.0
    @2917 Q1353HB_41 1.0
    @2918 Q1353HB_42 1.0
    @2919 Q1353HB_43 1.0
    @2920 Q1353HB_44 1.0
    @2921 Q1353IA_01 1.0
    @2922 Q1353IA_02 1.0
    @2923 Q1353IA_03 1.0
    @2924 Q1353IA_04 1.0
    @2925 Q1353IA_05 1.0
    @2926 Q1353IA_06 1.0
    @2927 Q1353IA_07 1.0
    @2928 Q1353IA_08 1.0
    @2929 Q1353IA_09 1.0
    @2930 Q1353IA_10 1.0
    @2931 Q1353IA_11 1.0
    @2932 Q1353IA_12 1.0
    @2933 Q1353IA_13 1.0
    @2934 Q1353IA_14 1.0
    @2935 Q1353IA_15 1.0
    @2936 Q1353IA_16 1.0
    @2937 Q1353IA_17 1.0
    @2938 Q1353IB_01 1.0
    @2939 Q1353IB_02 1.0
    @2940 Q1353IB_03 1.0
    @2941 Q1353IB_04 1.0
    @2942 Q1353IB_05 1.0
    @2943 Q1353IB_06 1.0
    @2944 Q1353IB_07 1.0
    @2945 Q1353IB_08 1.0
    @2946 Q1353IB_09 1.0
    @2947 Q1353IB_10 1.0
    @2948 Q1353IB_11 1.0
    @2949 Q1353IB_12 1.0
    @2950 Q1353IB_13 1.0
    @2951 Q1353IB_14 1.0
    @2952 Q1353IB_15 1.0
    @2953 Q1353IB_16 1.0
    @2954 Q1353IB_17 1.0
    @2955 Q1353J_1 1.0
    @2956 Q1353J_2 1.0
    @2957 Q1353J_3 1.0
    @2958 Q1353J_4 1.0
    @2959 Q1353J_5 1.0
    @2960 Q1353J_6 1.0
    @2961 Q1353J_7 1.0
    @2962 Q1353J_8 1.0
    @2963 Q1353J_9 1.0
    @2964 Q1401    2.0
    @2966 Q1402_1  1.0
    @2967 Q1402_2  1.0
    @2968 Q1402_3  1.0
    @2969 Q1402_4  1.0
    @2970 Q1402_5  1.0
    @2971 Q1402_6  1.0
    @2972 Q1406_1  1.0
    @2973 Q1406_2  1.0
    @2974 Q1406_3  1.0
    @2975 Q1406_4  1.0
    @2976 Q1406_5  1.0
    @2977 Q1406_6  1.0
    @2978 Q1408    1.0
    @2979 Q1409    1.0
    @2980 Q1410    1.0
    @2981 Q1411    1.0
    @2982 Q1412    1.0
    @2983 Q1415    1.0
    @2984 Q1416    1.0
    @2985 Q1417    1.0
    @2986 Q1418    1.0
    @2987 Q1419    1.0
    @2988 Q1420_01 1.0
    @2989 Q1420_02 1.0
    @2990 Q1420_03 1.0
    @2991 Q1420_04 1.0
    @2992 Q1420_05 1.0
    @2993 Q1420_06 1.0
    @2994 Q1420_07 1.0
    @2995 Q1420_08 1.0
    @2996 Q1420_09 1.0
    @2997 Q1420_10 1.0
    @2998 Q1420_11 1.0
    @2999 Q1420_12 1.0
    @3000 Q1421A_1 1.0
    @3001 Q1421A_2 1.0
    @3002 Q1421A_3 1.0
    @3003 Q1421A_4 1.0
    @3004 Q1421A_5 1.0
    @3005 Q1421A_6 1.0
    @3006 Q1421A_7 1.0
    @3007 Q1421A_8 1.0
    @3008 Q1421B_1 1.0
    @3009 Q1421B_2 1.0
    @3010 Q1421B_3 1.0
    @3011 Q1421B_4 1.0
    @3012 Q1421B_5 1.0
    @3013 Q1421B_6 1.0
    @3014 Q1421B_7 1.0
    @3015 Q1421B_8 1.0
    @3016 Q1422_1  1.0
    @3017 Q1422_2  1.0
    @3018 Q1422_3  1.0
    @3019 Q1422_4  1.0
    @3020 Q1422_5  1.0
    @3021 Q1422_6  1.0
    @3022 Q1422_7  1.0
    @3023 Q1450    2.0
    @3025 Q1451_01 1.0
    @3026 Q1451_02 1.0
    @3027 Q1451_03 1.0
    @3028 Q1451_04 1.0
    @3029 Q1451_05 1.0
    @3030 Q1451_06 1.0
    @3031 Q1451_07 1.0
    @3032 Q1451_08 1.0
    @3033 Q1451_09 1.0
    @3034 Q1451_10 1.0
    @3035 Q1451_11 1.0
    @3036 Q1451_12 1.0
    @3037 Q1451_13 1.0
    @3038 Q1452    1.0
    @3039 Q1454    1.0
    @3040 Q1455    1.0
    @3041 Q1456    1.0
    @3042 Q1457    1.0
    @3043 Q1458    1.0
    @3044 Q1459    1.0
    @3045 Q1460    1.0
    @3046 Q1461    1.0
    @3047 Q1462    1.0
    @3048 Q1601    1.0
    @3049 Q1602    1.0
    @3050 Q1603    1.0
    @3051 Q1604A_1 1.0
    @3052 Q1604A_2 1.0
    @3053 Q1604A_3 1.0
    @3054 Q1604A_4 1.0
    @3055 Q1604A_5 1.0
    @3056 Q1604A_6 1.0
    @3057 Q1604A_7 1.0
    @3058 Q1604A_8 1.0
    @3059 Q1604B_1 1.0
    @3060 Q1604B_2 1.0
    @3061 Q1604B_3 1.0
    @3062 Q1604B_4 1.0
    @3063 Q1604B_5 1.0
    @3064 Q1604B_6 1.0
    @3065 Q1604B_7 1.0
    @3066 Q1604B_8 1.0
    @3067 Q1605    1.0
    @3068 Q1606    1.0
    @3069 Q1607    1.0
    @3070 Q1608    1.0
    @3071 Q1609    1.0
    @3072 Q1610    1.0
    @3073 Q1611    1.0
    @3074 Q1612    1.0
    @3075 Q1613    1.0
    @3076 Q1614    1.0
    @3077 Q1615    3.0
    @3080 Q1616    3.0
    @3083 Q1617    1.0
    @3084 Q1618    1.0
    @3085 Q1619    2.0
    @3087 Q1621    2.0
    @3089 Q1622A_01 1.0
    @3090 Q1622A_02 1.0
    @3091 Q1622A_03 1.0
    @3092 Q1622A_04 1.0
    @3093 Q1622A_05 1.0
    @3094 Q1622A_06 1.0
    @3095 Q1622A_07 1.0
    @3096 Q1622A_08 1.0
    @3097 Q1622A_09 1.0
    @3098 Q1622A_10 1.0
    @3099 Q1622A_11 1.0
    @3100 Q1622A_12 1.0
    @3101 Q1622A_13 1.0
    @3102 Q1622A_14 1.0
    @3103 Q1622A_15 1.0
    @3104 Q1622B_01 1.0
    @3105 Q1622B_02 1.0
    @3106 Q1622B_03 1.0
    @3107 Q1622B_04 1.0
    @3108 Q1622B_05 1.0
    @3109 Q1622B_06 1.0
    @3110 Q1622B_07 1.0
    @3111 Q1622B_08 1.0
    @3112 Q1622B_09 1.0
    @3113 Q1622B_10 1.0
    @3114 Q1622B_11 1.0
    @3115 Q1622B_12 1.0
    @3116 Q1622B_13 1.0
    @3117 Q1622B_14 1.0
    @3118 Q1622B_15 1.0
    @3119 Q1623_01 1.0
    @3120 Q1623_02 1.0
    @3121 Q1623_03 1.0
    @3122 Q1623_04 1.0
    @3123 Q1623_05 1.0
    @3124 Q1623_06 1.0
    @3125 Q1623_07 1.0
    @3126 Q1623_08 1.0
    @3127 Q1623_09 1.0
    @3128 Q1623_10 1.0
    @3129 Q1623_11 1.0
    @3130 Q1623_12 1.0
    @3131 Q1623_13 1.0
    @3132 Q1623A   1.0
    @3133 Q1624_01 1.0
    @3134 Q1624_02 1.0
    @3135 Q1624_03 1.0
    @3136 Q1624_04 1.0
    @3137 Q1624_05 1.0
    @3138 Q1624_06 1.0
    @3139 Q1624_07 1.0
    @3140 Q1624_08 1.0
    @3141 Q1624_09 1.0
    @3142 Q1624_10 1.0
    @3143 Q1624_11 1.0
    @3144 Q1624_12 1.0
    @3145 Q1624_13 1.0
    @3146 Q1624_14 1.0
    @3147 Q1624_15 1.0
    @3148 Q1624_16 1.0
    @3149 Q1625_01 1.0
    @3150 Q1625_02 1.0
    @3151 Q1625_03 1.0
    @3152 Q1625_04 1.0
    @3153 Q1625_05 1.0
    @3154 Q1625_06 1.0
    @3155 Q1625_07 1.0
    @3156 Q1625_08 1.0
    @3157 Q1625_09 1.0
    @3158 Q1625_10 1.0
    @3159 Q1650    2.0
    @3161 Q1651_01 1.0
    @3162 Q1651_02 1.0
    @3163 Q1651_03 1.0
    @3164 Q1651_04 1.0
    @3165 Q1651_05 1.0
    @3166 Q1651_06 1.0
    @3167 Q1651_07 1.0
    @3168 Q1651_08 1.0
    @3169 Q1651_09 1.0
    @3170 Q1651_10 1.0
    @3171 Q1651_11 1.0
    @3172 Q1651_12 1.0
    @3173 Q1651_13 1.0
    @3174 Q1652    1.0
    @3175 Q1901    2.0
    @3177 Q1902    1.0
    @3178 Q1903    1.0
    @3179 Q1904    1.0
    @3180 Q1905    1.0
    @3181 Q1906    1.0
    @3182 Q1907    1.0
    @3183 Q1908    1.0
    @3184 Q1909    1.0
    @3185 Q1911    1.0
    @3186 Q1912    1.0
    @3187 Q1913    1.0
    @3188 Q1914    1.0
    @3189 Q1915    1.0
    @3190 Q1916    1.0
    @3191 Q1917    1.0
    @3192 Q1918    1.0
    @3193 Q1919    1.0
    @3194 Q1920    1.0
    @3195 Q1922    1.0
    @3196 Q1923    1.0
    @3197 Q1950    2.0
    @3199 Q1951_01 1.0
    @3200 Q1951_02 1.0
    @3201 Q1951_03 1.0
    @3202 Q1951_04 1.0
    @3203 Q1951_05 1.0
    @3204 Q1951_06 1.0
    @3205 Q1951_07 1.0
    @3206 Q1951_08 1.0
    @3207 Q1951_09 1.0
    @3208 Q1951_10 1.0
    @3209 Q1951_11 1.0
    @3210 Q1951_12 1.0
    @3211 Q1951_13 1.0
    @3212 Q1952    1.0
    @3213 Q2301    1.0
    @3214 Q2302    1.0
    @3215 Q2303    1.0
    @3216 Q2304    1.0
    @3217 Q2305    1.0
    @3218 Q2310    1.0
    @3219 Q2311    1.0
    @3220 Q2312    1.0
    @3221 Q2313    1.0
    @3222 Q2314    1.0
    @3223 Q2315    1.0
    @3224 Q2316    1.0
    @3225 Q2317    1.0
    @3226 Q2318    1.0
    @3227 Q2319    1.0
    @3228 Q2320    1.0
    @3229 Q2321    1.0
    @3230 Q2322    1.0
    @3231 Q2323    1.0
    @3232 Q2324    1.0
    @3233 Q2325    1.0
    @3234 Q2326    1.0
    @3235 Q2327    1.0
    @3236 Q2328    1.0
    @3237 Q2329    1.0
    @3238 Q2330    1.0
    @3239 Q2331A_01 1.0
    @3240 Q2331A_02 1.0
    @3241 Q2331A_03 1.0
    @3242 Q2331A_04 1.0
    @3243 Q2331A_05 1.0
    @3244 Q2331A_06 1.0
    @3245 Q2331A_07 1.0
    @3246 Q2331A_08 1.0
    @3247 Q2331A_09 1.0
    @3248 Q2331A_10 1.0
    @3249 Q2331A_11 1.0
    @3250 Q2331A_12 1.0
    @3251 Q2331A_13 1.0
    @3252 Q2331A_14 1.0
    @3253 Q2331A_15 1.0
    @3254 Q2331A_16 1.0
    @3255 Q2331A_17 1.0
    @3256 Q2331A_18 1.0
    @3257 Q2331A_19 1.0
    @3258 Q2331A_20 1.0
    @3259 Q2331A_21 1.0
    @3260 Q2331B_01 1.0
    @3261 Q2331B_02 1.0
    @3262 Q2331B_03 1.0
    @3263 Q2331B_04 1.0
    @3264 Q2331B_05 1.0
    @3265 Q2331B_06 1.0
    @3266 Q2331B_07 1.0
    @3267 Q2331B_08 1.0
    @3268 Q2331B_09 1.0
    @3269 Q2331B_10 1.0
    @3270 Q2331B_11 1.0
    @3271 Q2331B_12 1.0
    @3272 Q2331B_13 1.0
    @3273 Q2331B_14 1.0
    @3274 Q2331B_15 1.0
    @3275 Q2331B_16 1.0
    @3276 Q2331B_17 1.0
    @3277 Q2331B_18 1.0
    @3278 Q2331B_19 1.0
    @3279 Q2331B_20 1.0
    @3280 Q2331B_21 1.0
    @3281 Q2350    2.0
    @3283 Q2351_01 1.0
    @3284 Q2351_02 1.0
    @3285 Q2351_03 1.0
    @3286 Q2351_04 1.0
    @3287 Q2351_05 1.0
    @3288 Q2351_06 1.0
    @3289 Q2351_07 1.0
    @3290 Q2351_08 1.0
    @3291 Q2351_09 1.0
    @3292 Q2351_10 1.0
    @3293 Q2351_11 1.0
    @3294 Q2351_12 1.0
    @3295 Q2351_13 1.0
    @3296 Q2352    1.0
    @3297 Q2501    1.0
    @3298 Q2502    1.0
    @3299 Q2503    1.0
    @3300 Q2504    1.0
    @3301 Q2505    1.0
    @3302 Q2506    1.0
    @3303 Q2507    1.0
    @3304 Q2510A_01 1.0
    @3305 Q2510A_02 1.0
    @3306 Q2510A_03 1.0
    @3307 Q2510A_04 1.0
    @3308 Q2510A_05 1.0
    @3309 Q2510A_06 1.0
    @3310 Q2510A_07 1.0
    @3311 Q2510A_08 1.0
    @3312 Q2510A_09 1.0
    @3313 Q2510A_10 1.0
    @3314 Q2510A_11 1.0
    @3315 Q2510A_12 1.0
    @3316 Q2510A_13 1.0
    @3317 Q2510A_14 1.0
    @3318 Q2510A_15 1.0
    @3319 Q2510A_16 1.0
    @3320 Q2510A_17 1.0
    @3321 Q2510A_18 1.0
    @3322 Q2510A_19 1.0
    @3323 Q2510A_20 1.0
    @3324 Q2510B_01 1.0
    @3325 Q2510B_02 1.0
    @3326 Q2510B_03 1.0
    @3327 Q2510B_04 1.0
    @3328 Q2510B_05 1.0
    @3329 Q2510B_06 1.0
    @3330 Q2510B_07 1.0
    @3331 Q2510B_08 1.0
    @3332 Q2510B_09 1.0
    @3333 Q2510B_10 1.0
    @3334 Q2510B_11 1.0
    @3335 Q2510B_12 1.0
    @3336 Q2510B_13 1.0
    @3337 Q2510B_14 1.0
    @3338 Q2510B_15 1.0
    @3339 Q2510B_16 1.0
    @3340 Q2510B_17 1.0
    @3341 Q2510B_18 1.0
    @3342 Q2510B_19 1.0
    @3343 Q2510B_20 1.0
    @3344 Q2601_1  1.0
    @3345 Q2601_2  1.0
    @3346 Q2601_3  1.0
    @3347 Q2601_4  1.0
    @3348 Q2601_5  1.0
    @3349 Q2701    $5.
    @3354 Q2702    1.0
    @3355 Q2710    1.0
    @3356 Q2711    1.0
    @3357 Q2712_1  1.0
    @3358 Q2712_2  1.0
    @3359 Q2712_3  1.0
    @3360 Q2712_4  1.0
    @3361 Q2712_5  1.0
    @3362 Q2713    1.0
    @3363 Q2714A_1 1.0
    @3364 Q2714A_2 1.0
    @3365 Q2714A_3 1.0
    @3366 Q2714A_4 1.0
    @3367 Q2714A_5 1.0
    @3368 Q2714B_1 1.0
    @3369 Q2714B_2 1.0
    @3370 Q2714B_3 1.0
    @3371 Q2714B_4 1.0
    @3372 Q2714B_5 1.0
    @3373 Q2720    1.0
    @3374 Q2721    1.0
    @3375 Q2722    1.0
    @3376 Q2723    1.0
    @3377 Q2724    1.0
    @3378 Q2725    1.0
    @3379 Q2725A_1 1.0
    @3380 Q2725A_2 1.0
    @3381 Q3000_01 1.0
    @3382 Q3000_02 1.0
    @3383 Q3000_03 1.0
    @3384 Q3000_04 1.0
    @3385 Q3000_05 1.0
    @3386 Q3000_06 1.0
    @3387 Q3000_07 1.0
    @3388 Q3000_08 1.0
    @3389 Q3000_09 1.0
    @3390 Q3000_10 1.0
    @3391 Q3000_11 1.0
    @3392 Q3000_12 1.0
    @3393 ENDHH    2.0
    @3395 ENDMM    2.0
    ;
    if TOTFP    = 999 then TOTFP = .;
    if TOTSC    = 999 then TOTSC = .;
Run;
