libname user 'C:\SN 2018 SPA\EXPORT_2018\EXPORTEDFILES';
Proc format;
  value F00001_
     1 = "Dakar"
     2 = "Diourbel"
     3 = "Fatick"
     4 = "Kaffrine"
     5 = "Kaokack"
     6 = "Kedougou"
     7 = "Kolda"
     8 = "Louga"
     9 = "Matam"
    10 = "Saint Louis"
    11 = "Sediou"
    12 = "Tambacounda"
    13 = "Thies"
    14 = "Ziguinchor"
     ;
  value F00002_
     1 = "Hospital"
     2 = "Health centre"
     3 = "Health post"
     4 = "Case de santé (health hut)"
     ;
  value F00003_
     1 = "Government/public"
     2 = "NGO/ Private not for profit"
     3 = "Private for profit"
     ;
  value F00004_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00005_
     1 = "IPD only"
     2 = "OPD"
     ;
  value F00006_
     1 = "Facility begin interview/Complete"
     2 = "Facility Respondent not available"
     3 = "Postponed"
     4 = "Facility Refused"
     5 = "Partially complete"
     6 = "Other (non-existent)"
     ;
  value F00007_
     1 = "Facility Complete"
     2 = "Facility Respondent not available"
     3 = "Postponed"
     4 = "Facility Refused"
     5 = "Closed / not yet operational"
     6 = "Other (non-existent)"
     ;
  value F00008_
     1 = "Yes"
     2 = "No"
     ;
  value F00009_
     1 = "Yes"
     2 = "No"
     ;
  value F00010_
     1 = "Yes"
     2 = "No"
     ;
  value F00011_
     1 = "Yes"
     2 = "No"
     ;
  value F00012_
   998 = "Don't know"
     ;
  value F00013_
     1 = "Yes"
     2 = "No"
     ;
  value F00014_
     1 = "Only in this facility"
     2 = "Both in facility and outside"
     3 = "Only at an outside facility"
     ;
  value F00015_
     1 = "Yes"
     2 = "Facility stocks no medicines"
     ;
  value F00016_
     1 = "Stored in FP service site"
     2 = "Stored with other medicines"
     3 = "FP commodities not stocked"
     ;
  value F00017_
     1 = "Stored in TB service site"
     2 = "Stored with other medicines"
     3 = "TB medicines not stocked"
     ;
  value F00018_
     1 = "Stored in ARV service site"
     2 = "Stored with other medicines"
     3 = "ARV medicines not stocked"
     ;
  value F00019_
     0 = "No"
     1 = "Yes"
     ;
  value F00020_
     0 = "no"
     1 = "yes"
     ;
  value F00021_
     0 = "No"
     1 = "Yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00030_
     0 = "No"
     1 = "Yes"
     ;
  value F00031_
     0 = "No"
     1 = "Yes"
     ;
  value F00032_
     0 = "No"
     1 = "Yes"
     ;
  value F00033_
     0 = "No"
     1 = "Yes"
     ;
  value F00034_
     0 = "No"
     1 = "Yes"
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
     0 = "No"
     1 = "Yes"
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
     0 = "no"
     1 = "yes"
     ;
  value F00043_
     0 = "no"
     1 = "yes"
     ;
  value F00044_
     0 = "no"
     1 = "yes"
     ;
  value F00045_
     0 = "no"
     1 = "yes"
     ;
  value F00046_
     0 = "no"
     1 = "yes"
     ;
  value F00047_
     0 = "no"
     1 = "yes"
     ;
  value F00048_
     0 = "no"
     1 = "yes"
     ;
  value F00049_
     0 = "no"
     1 = "yes"
     ;
  value F00050_
     0 = "Delivery"
     1 = "Facility"
     ;
  value F00051_
     0 = "no"
     1 = "yes"
     ;
  value F00052_
     0 = "no"
     1 = "yes"
     ;
  value F00053_
     0 = "no"
     1 = "yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00085_
     0 = "No"
     1 = "Yes"
     ;
  value F00086_
     0 = "No"
     1 = "Yes"
     ;
  value F00087_
     0 = "No"
     1 = "Yes"
     ;
  value F00088_
     0 = "No"
     1 = "Yes"
     ;
  value F00089_
     0 = "No"
     1 = "Yes"
     ;
  value F00090_
     0 = "No"
     1 = "Yes"
     ;
  value F00091_
     0 = "No"
     1 = "Yes"
     ;
  value F00092_
     0 = "No"
     1 = "Yes"
     ;
  value F00093_
     0 = "No"
     1 = "Yes"
     ;
  value F00094_
     1 = "Yes, 24-Hour Staff"
     2 = "No, 24-Hour Staff"
     ;
  value F00095_
     1 = "Yes"
     2 = "Duty schedule not maintained"
     ;
  value F00096_
     1 = "Schedule observed"
     2 = "Schedule reported not seen"
     ;
  value F00097_
     1 = "Yes"
     2 = "No"
     ;
  value F00098_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00099_
     1 = "Yes"
     2 = "No"
     ;
  value F00100_
     1 = "Yes"
     2 = "No"
     ;
  value F00101_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00102_
     1 = "Yes"
     2 = "No"
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
     1 = "Yes"
     2 = "No"
     ;
  value F00111_
     0 = "No water source"
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
     ;
  value F00112_
     1 = "Onsite"
     2 = "Within 500 m of facility"
     3 = "Beyond 500 m of facility"
     ;
  value F00113_
     1 = "Yes"
     2 = "No"
     ;
  value $F00114_
'A    ' = "Fetching water"
'B    ' = "Call firefighters"
'C    ' = "Appeal to authority"
'D    ' = "Wait until the water returns"
'Y    ' = "Others"
     ;
  value F00115_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00116_
     1 = "Always available"
     2 = "Sometimes interrupted"
     8 = "Don't know"
     ;
  value F00117_
     1 = "Yes"
     2 = "No other source"
     ;
  value $F00118_
'A  ' = "Fuel operated generator"
'B  ' = "Battery operated generator"
'C  ' = "Solar system"
     ;
  value F00119_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00120_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00121_
     1 = "Yes"
     2 = "No"
     ;
  value F00122_
     1 = "Within the past 6 months"
     2 = "More than 6 months ago"
     ;
  value F00123_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00124_
     1 = "Yes"
     2 = "No"
     ;
  value F00125_
     1 = "Fixed fee covering all services"
     2 = "Charge fee for separate items"
     ;
  value F00126_
     1 = "Yes"
     2 = "No"
     ;
  value F00127_
     1 = "Yes"
     2 = "No posted fees"
     ;
  value F00128_
     1 = "Observed, All fees posted"
     2 = "Observed, Some but not all fees posted"
     ;
  value $F00129_
'A    ' = "Fee exempted/ discounted/ no payment expected"
'B    ' = "Fee exempted/ discounted/ payment expected later"
'C    ' = "Service not provided/ asked to return when able to pay"
'D    ' = "Accept payment in-kind"
'X    ' = "Other"
     ;
  value $F00130_
'A         ' = "Ministry of Health"
'B         ' = "Other public ministries"
'C         ' = "Medical schemes (insurance)"
'D         ' = "Social security fund"
'E         ' = "Reimbursement by employer"
'F         ' = "Government contribution to private"
'G         ' = "Donor agencies/ NGOs"
'H         ' = "Faith-based"
'I         ' = "Community programs"
'X         ' = "Other     "
     ;
  value F00131_
     1 = "YES"
     2 = "NO"
     3 = "DON'T KNOW"
     ;
  value F00132_
     1 = "Yes"
     2 = "No"
     ;
  value F00133_
     1 = "Monthly or more often"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less often than every 6 months"
     8 = "Don't know"
     ;
  value F00134_
     1 = "Yes"
     2 = "No records maintained"
     ;
  value F00135_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value $F00136_
'A          ' = "RHIS data quality"
'B          ' = "RHIS reporting"
'C          ' = "Timeliness of RHIS reporting"
'D          ' = "Quality of services"
'E          ' = "Client utilization"
'F          ' = "Disease data"
'G          ' = "Employment conditions"
'H          ' = "Finances or budget"
'X          ' = "Other      "
'Y          ' = "None of the above"
'Z          ' = "No record available"
     ;
  value F00137_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00138_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00139_
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
     ;
  value F00140_
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
     ;
  value F00141_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00142_
     1 = "Monthly or less"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less freqent than every 6 months"
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
  value F00145_
     1 = "Yes"
     2 = "No"
     ;
  value $F00146_
'A          ' = "Suggestion box"
'B          ' = "Client survey form"
'C          ' = "Client interview form"
'D          ' = "Official meeting with community leaders"
'E          ' = "Informal discussion with clients/ community"
'F          ' = "Email      "
'G          ' = "Facility's website"
'H          ' = "Letters from clients/ community"
'I          ' = "Direct complaint to agents / providers"
'X          ' = "Other      "
'Z          ' = "Don't know "
     ;
  value F00147_
     1 = "Yes"
     2 = "No procedure"
     8 = "Don't know"
     ;
  value F00148_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00149_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00150_
     1 = "Oui"
     2 = "No records not maintained"
     ;
  value F00151_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00152_
     1 = "Yes, always"
     2 = "Yes, sometimes"
     3 = "No"
     ;
  value F00153_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00154_
     1 = "Yes, always"
     2 = "Yes, sometimes"
     3 = "No"
     ;
  value F00155_
     1 = "YES"
     2 = "NO"
     ;
  value F00156_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00157_
     1 = "Yes"
     2 = "No"
     ;
  value F00158_
     1 = "Yes"
     2 = "No"
     ;
  value F00159_
     1 = "Monthly or more often"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less often than every 6 months"
     ;
  value F00160_
     1 = "Record observed"
     2 = "Reported, not seen"
     ;
  value F00161_
     1 = "Yes"
     2 = "No dedicated person"
     ;
  value F00162_
     1 = "Data manager/HMIS person"
     2 = "Facility in-charge"
     3 = "Other service provider"
     ;
  value F00163_
  9998 = "Don't know"
     ;
  value F00164_
  9998 = "Don't know"
     ;
  value F00165_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not used"
     ;
  value F00166_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00167_
     1 = "Used"
     2 = "Not used"
     ;
  value F00168_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00169_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00170_
     1 = "Bar"
     8 = "Don't know"
     ;
  value F00171_
   666 = "Automatic"
   995 = "Not used"
   998 = "Don't know"
     ;
  value F00172_
   666 = "Automatic"
   995 = "Not used"
   998 = "Don't know"
     ;
  value F00173_
     1 = "Chlorine"
     2 = "Formaldehyde"
     3 = "Gluteraldehyde"
    98 = "Don't know"
     ;
  value F00174_
     1 = "Yes"
     2 = "No"
     ;
  value F00175_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00176_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not used"
     ;
  value F00177_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00178_
     1 = "Used"
     2 = "Not used"
     ;
  value F00179_
   666 = "Automatic"
   995 = "Not used"
   998 = "Don't know"
     ;
  value F00180_
     1 = "Alcohol"
     2 = "Betadine"
     3 = "Chlorine de javel"
    98 = "Don't know"
     ;
  value F00181_
     2 = "Incinerator, 2-chamber industrial"
     3 = "Incinerator, 1-chamber drum/ brick"
     4 = "Open burning, Flat gound no protection"
     5 = "Open burning, pit or protected"
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
     2 = "Incinerator, 2-chamber industrial"
     3 = "Incinerator, 1-chamber drum/ brick"
     4 = "Open burning, Flat gound no protection"
     5 = "Open burning, pit or protected"
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
  value F00183_
     1 = "No waste visible"
     2 = "Waste visible, but protected area"
     3 = "Waste visible, not protected area"
     8 = "Waste site not inspected"
     ;
  value F00184_
     1 = "No waste visible"
     2 = "Waste visible, but protected"
     3 = "Waste visible, not protected"
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
  value F00191_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00192_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00193_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     9 = "Missing"
     ;
  value F00194_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00195_
     1 = "Yes"
     2 = "No protected client waiting area"
     ;
  value F00196_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00197_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
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
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00204_
     1 = "Yes"
     2 = "No"
     ;
  value F00205_
     1 = "Yes"
     2 = "No"
     ;
  value F00206_
     1 = "Yes"
     2 = "No"
     ;
  value F00207_
     1 = "Observed, at least 1 valid"
     2 = "Observed , none valid"
     3 = "Reported available, not seen"
     ;
  value F00208_
     1 = "Yes"
     2 = "No"
     ;
  value F00209_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00210_
     1 = "Yes"
     2 = "No"
     ;
  value F00211_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00212_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00213_
     1 = "Yes"
     2 = "No"
     ;
  value F00214_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00215_
     1 = "Yes"
     2 = "No"
     ;
  value F00216_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00217_
     1 = "Yes"
     2 = "No"
     ;
  value F00218_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00219_
     1 = "Yes"
     2 = "No"
     ;
  value $F00220_
'A   ' = "Proficiency panel"
'B   ' = "External inspection/ observation of technique"
'C   ' = "Blood sent outside for retesting"
'X   ' = "Other"
     ;
  value F00221_
     1 = "Yes"
     2 = "No"
     ;
  value F00222_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00223_
    95 = "Not available"
     ;
  value F00224_
     1 = "Yes"
     2 = "No"
     ;
  value $F00225_
'A    ' = "ELISA/EIA"
'B    ' = "Western blot"
'C    ' = "PCR for EID"
'D    ' = "Rapid testing"
'X    ' = "Other"
     ;
  value F00226_
     1 = "Yes"
     2 = "No"
     ;
  value F00227_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00228_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
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
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00232_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00233_
     1 = "Yes"
     2 = "No"
     ;
  value F00234_
     1 = "Yes"
     2 = "No"
     ;
  value F00235_
     1 = "Observed"
     2 = "Reported not seen"
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
     1 = "Yes"
     2 = "No"
     ;
  value F00239_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Normally available not today"
     ;
  value F00240_
     1 = "Yes"
     2 = "No"
     ;
  value F00241_
     1 = "Yes"
     2 = "No"
     ;
  value F00242_
     1 = "Yes"
     2 = "No"
     ;
  value F00243_
     1 = "Yes"
     2 = "No"
     ;
  value F00244_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00245_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00246_
     1 = "Yes"
     2 = "No"
     ;
  value F00247_
     1 = "Yes"
     2 = "No"
     ;
  value F00248_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00249_
     1 = "Brand Malaria ASPF"
     8 = "Other - specify"
     ;
  value F00250_
     1 = "Yes"
     2 = "No"
     ;
  value F00251_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00252_
     1 = "Yes"
     2 = "No"
     ;
  value F00253_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Normally available not today"
     ;
  value F00254_
     1 = "Yes"
     2 = "No"
     ;
  value F00255_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00256_
     1 = "Yes"
     2 = "No"
     ;
  value F00257_
     1 = "Yes"
     2 = "No"
     ;
  value F00258_
     1 = "Yes"
     2 = "No"
     ;
  value F00259_
     1 = "Yes"
     2 = "No"
     ;
  value F00260_
     1 = "Yes"
     2 = "No"
     ;
  value F00261_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00262_
     1 = "Yes"
     2 = "No"
     ;
  value F00263_
     1 = "Yes"
     2 = "No"
     ;
  value F00264_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00265_
     1 = "Yes"
     2 = "No"
     ;
  value F00266_
     1 = "Yes"
     2 = "No"
     ;
  value F00267_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00268_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00269_
     1 = "Yes"
     2 = "No"
     ;
  value F00270_
     1 = "Yes"
     2 = "No"
     ;
  value F00271_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00272_
     1 = "Yes"
     2 = "No"
     ;
  value F00273_
     1 = "Yes"
     2 = "No"
     ;
  value F00274_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00275_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00276_
     1 = "Yes"
     2 = "No"
     ;
  value F00277_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00278_
     1 = "Yes"
     2 = "No"
     ;
  value F00279_
     1 = "Observe"
     2 = "Reported not seen"
     ;
  value F00280_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00281_
     1 = "Yes"
     2 = "No"
     ;
  value F00282_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00283_
     1 = "Yes"
     2 = "No"
     ;
  value F00284_
     1 = "Internal QC only"
     2 = "External QC only"
     3 = "Both, internal and external QC"
     4 = "Send slide for re-reading"
     6 = "Other"
     ;
  value F00285_
     1 = "Yes"
     2 = "No"
     ;
  value F00286_
     1 = "Records for IQC only"
     2 = "Records for EQC only"
     3 = "Records for both QC procedures"
     ;
  value F00287_
     1 = "Yes"
     2 = "No"
     ;
  value F00288_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Normally available, not today"
     ;
  value F00289_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00290_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00291_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00292_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00293_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00294_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     6 = "yes rupture de stock"
     7 = "non"
     8 = "don't know"
     ;
  value F00295_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00296_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00297_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00298_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00299_
     1 = "Yes"
     2 = "No"
     ;
  value F00300_
     1 = "Yes, all medicines"
     2 = "Yes, only some medicines"
     3 = "No"
     ;
  value F00301_
     1 = "Computer system updated daily"
     2 = "Ledger/ stock card updated daily"
     3 = "Computer system not updated daily, but there is daily record of distributed medicines"
     4 = "Ledger/ stock card not updated daily, but there is a daily record of distributed medicines"
     6 = "Other"
     ;
  value F00302_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     ;
  value F00303_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00304_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00305_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00306_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00307_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00308_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00309_
     1 = "Yes"
     2 = "No"
     ;
  value F00310_
     1 = "Yes, all commodities"
     2 = "Not all commodities"
     3 = "No"
     ;
  value F00311_
     1 = "Computer system updated daily"
     2 = "Ledger/ stock card updated daily"
     3 = "Computer system not updated daily, but there is daily record of distributed commodities"
     4 = "Ledger/ stock card not updated daily, but there is a daily record of distributed commodities"
     6 = "Other"
     ;
  value F00312_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00313_
     1 = "Yes"
     2 = "No"
     ;
  value F00314_
     1 = "Yes, all medicines"
     2 = "Yes, only some medicines"
     3 = "No"
     ;
  value F00315_
     1 = "Computer system updated daily"
     2 = "Ledger/ stock card updated daily"
     3 = "Computer system not updated daily, but there is daily record of distributed medicines"
     4 = "Ledger/ stock card not updated daily, but there is a daily record of distributed medicines"
     6 = "Other"
     ;
  value F00316_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00317_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00318_
     1 = "Observed at least 1 valid"
     2 = "Observed, but none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00319_
     1 = "Yes"
     2 = "No"
     ;
  value F00320_
     1 = "Yes, all medicines"
     2 = "Yes, only some medicines"
     3 = "No"
     ;
  value F00321_
     1 = "Computer system updated daily"
     2 = "Ledger/ stock card updated daily"
     3 = "Computer system not updated daily, but there is daily record of distributed ARVs"
     4 = "Ledger/ stock card not updated daily, but there is a daily record of distributed ARVs"
     6 = "Other"
     ;
  value F00322_
     1 = "Yes"
     2 = "No"
     ;
  value F00323_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00324_
     1 = "Yes"
     2 = "No"
     ;
  value F00325_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00326_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00327_
     1 = "Routinely store vaccines"
     2 = "Stores no vaccines"
     ;
  value F00328_
     1 = "Refrigerator observed"
     2 = "Refrigerator not observed"
     ;
  value F00329_
     1 = "IG 50/SIBIR"
     2 = "TCW 3000"
     3 = "TCW 2000/SOLAIRE"
     4 = "RCW 40 SDD (SOLAR DIRECT DRIVER)"
     5 = "DOMESTIC REFRIGERATOR"
     6 = "OTHERS"
     ;
  value F00330_
     1 = "Yes"
     2 = "No"
     ;
  value F00331_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00332_
     1 = "Yes completed"
     2 = "No, not completed"
     ;
  value F00333_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00334_
     1 = "Between +2 and +8 degrees"
     2 = "Above +8 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functional"
     5 = "Vaccine refregerator not seen"
     ;
  value F00335_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value F00336_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00337_
     1 = "One set"
     2 = "Two or more sets"
     3 = "None"
     ;
  value F00338_
     1 = "DOMETIC"
     2 = "BLOW KINGS"
     3 = "GEOSTYLE"
     4 = "DOMESTIC REFRIGERATOR"
     5 = "OTHERS"
     ;
  value F00339_
     1 = "One set"
     2 = "Two or more sets"
     3 = "No ice packs, use purchased ice"
     4 = "No ice packs"
     ;
  value F00340_
    11 = "GENERAL INFORMATION  [Q710]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00341_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00342_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00343_
     1 = "Yes"
     2 = "No"
     ;
  value F00344_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00345_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00346_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00347_
     1 = "Yes"
     2 = "No"
     ;
  value F00348_
     1 = "Yes"
     2 = "No"
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
     2 = "Reported not seen"
     ;
  value F00353_
     1 = "Yes"
     2 = "No"
     ;
  value F00354_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not routinely conducted"
     8 = "Don't know"
     ;
  value F00355_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00356_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00357_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00358_
     1 = "Yes"
     2 = "No"
     ;
  value F00359_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00360_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00361_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00362_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00363_
     1 = "Provide (stock the commodity)"
     2 = "Prescribe/ counsel/ refer"
     3 = "No"
     ;
  value F00364_
     1 = "Yes"
     2 = "No"
     ;
  value F00365_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00366_
     1 = "Yes"
     2 = "No"
     ;
  value F00367_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00368_
     1 = "Yes"
     2 = "No"
     ;
  value F00369_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00370_
     1 = "Yes"
     2 = "No"
     ;
  value F00371_
     1 = "Yes"
     2 = "No"
     ;
  value F00372_
     1 = "Yes"
     2 = "No"
     ;
  value F00373_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00374_
     1 = "Yes"
     2 = "No"
     ;
  value F00375_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not routinely done"
     8 = "Don't know"
     ;
  value F00376_
     1 = "Routinely diagnose & treat STIs"
     2 = "Refer within facility"
     3 = "Refer outside facility"
     4 = "No diagnosis, treatment or referral"
     ;
  value F00377_
     1 = "Yes"
     2 = "No"
     ;
  value F00378_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00379_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00380_
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
     ;
  value F00381_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00382_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00383_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00384_
     1 = "FP service site"
     2 = "Central location in facility"
     3 = "Both locations"
     4 = "No equipment processed in facility"
     ;
  value $F00385_
'A     ' = "Autoclave"
'B     ' = "Dry heat sterilization"
'C     ' = "Soak in chlorine solution"
'D     ' = "Boil or steam"
'E     ' = "Wash with soap and water"
'F     ' = "Popinelle"
     ;
  value F00386_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00387_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00388_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00389_
     1 = "Yes"
     2 = "No"
     ;
  value F00390_
     1 = "Yes"
     2 = "Not all ANC days"
     ;
  value F00391_
     0 = "Less often than once/week"
     ;
  value F00392_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Available elsewhere in facility"
     5 = "Never available"
     ;
  value F00393_
     1 = "Yes"
     2 = "No"
     ;
  value F00394_
     1 = "Routinely diagnose and treat"
     2 = "Refer within facility"
     3 = "Rever outside facility"
     4 = "No diagnosis, treatment or referral"
     ;
  value F00395_
     1 = "Yes"
     2 = "No"
     ;
  value F00396_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00397_
     1 = "Yes"
     2 = "No"
     ;
  value F00398_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00399_
     1 = "Yes"
     2 = "No"
     ;
  value F00400_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00401_
     1 = "Yes"
     2 = "No"
     ;
  value F00402_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00403_
     1 = "Yes"
     2 = "No"
     ;
  value F00404_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00405_
     1 = "Yes"
     2 = "No"
     ;
  value F00406_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Activity not routinely done"
     8 = "Don't know"
     ;
  value F00407_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00408_
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
     ;
  value F00409_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00410_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00411_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00412_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00413_
     1 = "Yes"
     2 = "No"
     ;
  value F00414_
     1 = "Yes,  ANC service site"
     2 = "No, different location"
     ;
  value F00415_
     1 = "Yes"
     2 = "No"
     ;
  value F00416_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00417_
     1 = "Yes"
     2 = "No"
     ;
  value F00418_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00419_
     1 = "Yes"
     2 = "No"
     ;
  value F00420_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00421_
     1 = "Yes"
     2 = "No"
     ;
  value F00422_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00423_
     1 = "Yes"
     2 = "No"
     ;
  value F00424_
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00425_
     1 = "Yes"
     2 = "No"
     ;
  value F00426_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00427_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00428_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00429_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00430_
     1 = "Yes"
     2 = "No"
     ;
  value F00431_
     1 = "Yes"
     2 = "No"
     ;
  value F00432_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00433_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00434_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00435_
     1 = "Yes"
     2 = "No"
     ;
  value F00436_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00437_
     1 = "Yes"
     2 = "No"
     ;
  value F00438_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00439_
     1 = "Yes"
     2 = "No"
     ;
  value F00440_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00441_
     1 = "Yes"
     2 = "No"
     ;
  value F00442_
     1 = "Yes, separate room"
     2 = "No, integrated"
     ;
  value F00443_
     1 = "Yes"
     2 = "No use of partograph"
     ;
  value F00444_
     1 = "Routinely"
     2 = "Selectively"
     ;
  value F00445_
   998 = "Don't know"
     ;
  value F00446_
   998 = "Don't know"
     ;
  value F00447_
     1 = "Yes"
     2 = "No, does not participate"
     ;
  value F00448_
     1 = "For mothers only"
     2 = "For newborns only"
     3 = "For both mothers and newborns"
     ;
  value F00449_
    53 = "Only when case occurs"
    98 = "Don't know"
     ;
  value F00450_
    53 = "Only when case occurs"
    95 = "Always with maternal reviews"
    98 = "Don't know"
     ;
  value F00451_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00452_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00453_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Pas disponible"
     8 = "Don't know"
     ;
  value F00454_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00455_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00456_
     1 = "Yes"
     2 = "No"
     ;
  value F00457_
     1 = "Yes"
     2 = "No"
     ;
  value F00458_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00459_
     1 = "Yes"
     2 = "No"
     ;
  value F00460_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00461_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00462_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00463_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00464_
     1 = "Yes"
     2 = "No"
     ;
  value F00465_
     1 = "Yes"
     2 = "No"
     ;
  value F00466_
     1 = "Always"
     2 = "Only sometimes"
     ;
  value F00467_
     1 = "Yes"
     2 = "No"
     ;
  value F00468_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00469_
     1 = "Marque Malaria/Type Ag PF"
     2 = "Other"
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
     1 = "Yes"
     2 = "No"
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
     1 = "Yes"
     2 = "No"
     ;
  value F00478_
     1 = "Syndromic approach only"
     2 = "Etiologic (lab) only"
     3 = "Both syndromic and etiologic"
     ;
  value F00479_
     1 = "Yes"
     2 = "No"
     ;
  value F00480_
     1 = "Yes"
     2 = "No"
     ;
  value F00481_
     1 = "Routinely reffered or offered service"
     2 = "Referred only if client suspected to be HIV infected"
     ;
  value F00482_
     1 = "Yes"
     2 = "No"
     ;
  value F00483_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00484_
     1 = "Yes"
     2 = "No"
     ;
  value F00485_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00486_
     1 = "Yes"
     2 = "No"
     ;
  value F00487_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00488_
     1 = "Yes"
     2 = "No partner notification"
     ;
  value F00489_
     1 = "Always active"
     2 = "Sometimes active"
     3 = "Only passive"
     ;
  value F00490_
     1 = "Yes"
     2 = "No"
     ;
  value F00491_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00492_
     1 = "Observed"
     2 = "Reported not seen"
     3 = "Not available"
     8 = "Don't know"
     ;
  value F00493_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00494_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00495_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00496_
     1 = "Yes"
     2 = "No"
     ;
  value F00497_
     1 = "Sputum smear only"
     2 = "X-Ray only"
     3 = "Either sputum or X-Ray"
     4 = "Both sputum and X-Ray"
     5 = "Clinical symptoms only"
     6 = "Test Moleculaire Genexpert"
     ;
  value F00498_
     1 = "Yes"
     2 = "No"
     ;
  value F00499_
     1 = "Yes"
     2 = "No"
     ;
  value F00500_
     1 = "Yes"
     2 = "Register not kept"
     ;
  value F00501_
     1 = "Register seen (paper)"
     2 = "Register seen (electronic)"
     3 = "Register reported, not seen"
     ;
  value F00502_
     1 = "YES"
     2 = "NO"
     ;
  value F00503_
     1 = "M2"
     2 = "M5"
     3 = "FIN DE TRAITEMENT"
     4 = "M2,M5,FIN DE TRAITEMENT"
     ;
  value F00504_
     1 = "Yes"
     2 = "No"
     ;
  value F00505_
     1 = "Direct observe 2m, FU 4m"
     2 = "Direct observe 6m"
     3 = "Follow up clients only after first 2m direct observation elsewhere"
     4 = "Diagnose and treat while inpatient, discharge elsewhere for F-up"
     5 = "Provide full treatment, with no routine direct observation phase"
     6 = "Diagnose, prescribe/ provide medicines only, no F/up"
     7 = "Diagnose only, no treatment or prescription of medicine"
     ;
  value F00506_
     1 = "TDO SANITAIRE"
     2 = "TDO COMMUNAUTAIRE"
     3 = "TDO FAMILIAL"
     ;
  value F00507_
     1 = "Yes"
     2 = "No system"
     ;
  value F00508_
     1 = "System or register observed"
     2 = "System or register reported, not seen"
     ;
  value F00509_
     1 = "Yes"
     2 = "No"
     ;
  value F00510_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00511_
     1 = "Yes"
     2 = "No"
     ;
  value F00512_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00513_
     1 = "Yes"
     2 = "No"
     ;
  value F00514_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00515_
     1 = "Yes"
     2 = "No"
     ;
  value F00516_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00517_
     1 = "Yes"
     2 = "No"
     ;
  value F00518_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00519_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00520_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00521_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00522_
     1 = "Yes"
     2 = "No"
     ;
  value F00523_
     1 = "Yes"
     2 = "No"
     ;
  value F00524_
     1 = "Oui"
     2 = "Non"
     ;
  value F00525_
     1 = "Yes"
     2 = "No"
     ;
  value $F00526_
'A     ' = "HIV rapid test this service site"
'B     ' = "Blood drawn here, sent to lab"
'C     ' = "Client sent to other site in facility"
'D     ' = "Client sent to lab in facility"
'E     ' = "Client sent to external site"
'F     ' = "Blood drawn here, sent to external site"
     ;
  value F00527_
     1 = "Observed, at least 1 valid"
     2 = "Observed, none valid"
     3 = "Reported available, not seen"
     ;
  value F00528_
     1 = "Yes"
     2 = "No individual client chart/record"
     ;
  value F00529_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00530_
     1 = "Yes"
     2 = "No"
     ;
  value F00531_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00532_
     1 = "Yes"
     2 = "No"
     ;
  value F00533_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00534_
     1 = "Yes"
     2 = "No"
     ;
  value F00535_
     1 = "Yes"
     2 = "No"
     ;
  value F00536_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00537_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00538_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00539_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00540_
     1 = "Yes"
     2 = "No"
     ;
  value F00541_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00542_
     1 = "Yes"
     2 = "No agreement"
     ;
  value F00543_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00544_
     1 = "Yes"
     2 = "No"
     ;
  value F00545_
     1 = "Yes, service monitoring in the structure"
     2 = "Yes, service monitoring in the structure and in the community"
     3 = "No"
     ;
  value F00546_
     1 = "Yes"
     2 = "No"
     ;
  value F00547_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00548_
     1 = "Yes"
     2 = "No"
     ;
  value F00549_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00550_
     1 = "Routinely"
     2 = "Selectively"
     3 = "Never"
     8 = "Don't know"
     ;
  value F00551_
     1 = "Routinely"
     2 = "Selectively"
     3 = "Never"
     8 = "Don't know"
     ;
  value F00552_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00553_
     1 = "Yes"
     2 = "No system"
     ;
  value F00554_
     1 = "System or register observed"
     2 = "System or register reported not seen"
     ;
  value F00555_
     1 = "Yes"
     2 = "No"
     ;
  value F00556_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00557_
     1 = "Yes"
     2 = "No"
     ;
  value F00558_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00559_
     1 = "Yes"
     2 = "No"
     ;
  value F00560_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00561_
     1 = "Yes, diagnosis only"
     2 = "Yes, diagnosis and management"
     3 = "Yes, management only"
     4 = "No"
     ;
  value F00562_
     1 = "Yes"
     2 = "No"
     ;
  value F00563_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00564_
     1 = "Yes"
     2 = "No"
     ;
  value F00565_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00566_
     1 = "Yes, diagnosis only"
     2 = "Yes, diagnosis and management"
     3 = "Yes, management only"
     4 = "No"
     ;
  value F00567_
     1 = "Yes"
     2 = "No"
     ;
  value F00568_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00569_
     1 = "Yes"
     2 = "No"
     ;
  value F00570_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00571_
     1 = "Yes, diagnosis only"
     2 = "Yes, diagnosis and management"
     3 = "Yes, management only"
     4 = "No"
     ;
  value F00572_
     1 = "Yes"
     2 = "No"
     ;
  value F00573_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00574_
     1 = "Yes"
     2 = "No"
     ;
  value F00575_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00576_
     1 = "General information section [Q700]"
     2 = "Not previously seen"
     ;
  value F00577_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00578_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00579_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    23 = "MINOR SURGERY  [Q2450]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00580_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00581_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00582_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00583_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00584_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value F00585_
     1 = "Yes"
     2 = "No"
     ;
  value F00586_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00587_
    11 = "GENERAL INFORMATION  [Q710]"
    12 = "CHILD VACCINATION  [Q1050]"
    13 = "CHILD CURATIVE CARE  [Q1250]"
    14 = "FAMILY PLANNING  [Q1350]"
    15 = "ANTENATAL CARE  [Q1450]"
    16 = "PMTCT  [Q1550]"
    17 = "DELIVERY  [Q1650]"
    18 = "STI SERVICES  [Q1850]"
    19 = "TUBERCULOSIS  [Q1950]"
    21 = "HIV TESTING  [Q2050]"
    22 = "NCD  [Q2350]"
    31 = "NOT PREVIOUSLY SEEN"
     ;
  value F00588_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00589_
     1 = "Private room"
     2 = "Other room with auditory/visual privacy"
     3 = "Visual privacy only"
     4 = "No privacy"
     ;
  value F00590_
     1 = "Yes"
     2 = "No"
     ;
  value F00591_
     1 = "Yes"
     2 = "24-hr duty schedule not maintained"
     ;
  value F00592_
     1 = "Schedule observed"
     2 = "Schedule reported, not seen"
     ;
  value F00593_
     1 = "Yes"
     2 = "No"
     ;
  value F00594_
     1 = "Yes"
     2 = "24-hr duty schedule not maintained"
     ;
  value F00595_
     1 = "Schedule observed"
     2 = "Schedule reported, not seen"
     ;
  value F00596_
     1 = "Yes"
     2 = "No"
     ;
  value F00597_
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available"
     ;
  value F00598_
     1 = "Yes"
     2 = "No"
     8 = "Don't know"
     ;
  value F00599_
     1 = "Observed, at least 1 valid"
     2 = "Observed available , none valid"
     3 = "Reported available, not seen"
     4 = "Not available today/Don't know"
     5 = "Never available"
     ;
  value $F00600_
'A   ' = "National Blood Bank"
'B   ' = "Regional Blood Bank"
'C   ' = "Relatives donating directly"
'X   ' = "Other sources"
     ;
  value F00601_
     1 = "Yes"
     2 = "No"
     ;
  value F00602_
     1 = "Yes"
     2 = "No"
     ;
  value F00603_
     1 = "Only in this facility"
     2 = "Only at an external facility"
     3 = "Both internally and externally"
     ;
  value F00604_
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     4 = "No"
     ;
  value F00605_
     1 = "Yes"
     2 = "No"
     ;
  value F00606_
     1 = "Yes"
     2 = "No"
     ;
  value F00607_
     1 = "Yes"
     2 = "No"
     ;
  value F00608_
     1 = "Yes"
     2 = "No"
     ;
  value F00609_
     1 = "Yes"
     2 = "No"
     ;
  value F00610_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00611_
     1 = "Between +2 and +6 degrees"
     2 = "Above +6 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functional"
     ;
  value F00612_
     1 = "Yes"
     2 = "No"
     ;
  value F00613_
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F00614_
     1 = "Yes"
     2 = "No"
     ;
Data user.SNFC8HFLSP;
  attrib FACIL    label="Facility identification";
  attrib REC_TYPE length=$1 label="Record Type (length 1)";
  attrib REGION   format=F00001_. label="Region/Province";
  attrib FACTYPE  format=F00002_. label="Facility type";
  attrib MGA      format=F00003_. label="Managing authority (ownership)";
  attrib FTYPE    format=F00004_. label="Urban/rural";
  attrib IPD_OPD  format=F00005_. label="Inpatient only";
  attrib VISITLN_1 label="Visit Line number(1)";
  attrib VISITLN_2 label="Visit Line number(2)";
  attrib VISITLN_3 label="Visit Line number(3)";
  attrib VISITLN_4 label="Visit Line number(4)";
  attrib QVDAY_1  label="Day of visit(1)";
  attrib QVDAY_2  label="Day of visit(2)";
  attrib QVDAY_3  label="Day of visit(3)";
  attrib QVDAY_4  label="Day of visit(4)";
  attrib QVMONTH_1 label="Month of visit(1)";
  attrib QVMONTH_2 label="Month of visit(2)";
  attrib QVMONTH_3 label="Month of visit(3)";
  attrib QVMONTH_4 label="Month of visit(4)";
  attrib QVYEAR_1 label="Year of visit(1)";
  attrib QVYEAR_2 label="Year of visit(2)";
  attrib QVYEAR_3 label="Year of visit(3)";
  attrib QVYEAR_4 label="Year of visit(4)";
  attrib QVRESULT_1 format=F00006_. label="Result of visit(1)";
  attrib QVRESULT_2 format=F00006_. label="Result of visit(2)";
  attrib QVRESULT_3 format=F00006_. label="Result of visit(3)";
  attrib QVRESULT_4 format=F00006_. label="Result of visit(4)";
  attrib NUMVISIT label="No. of visits";
  attrib DAY      label="Day of interview";
  attrib MONTH    label="Month of interview";
  attrib YEAR     label="Year of interview";
  attrib INTVNO   label="Interviewer number";
  attrib RESULT   format=F00007_. label="Result of visit";
  attrib Q100     format=F00008_. label="Permission to interview";
  attrib Q101H    label="Start hour";
  attrib Q101M    label="Start minute";
  attrib Q102_01  format=F00009_. label="Services table(1)";
  attrib Q102_02  format=F00009_. label="Services table(2)";
  attrib Q102_03  format=F00009_. label="Services table(3)";
  attrib Q102_04  format=F00009_. label="Services table(4)";
  attrib Q102_05  format=F00009_. label="Services table(5)";
  attrib Q102_06  format=F00009_. label="Services table(6)";
  attrib Q102_07  format=F00009_. label="Services table(7)";
  attrib Q102_08  format=F00009_. label="Services table(8)";
  attrib Q102_09  format=F00009_. label="Services table(9)";
  attrib Q102_10  format=F00009_. label="Services table(10)";
  attrib Q102_11  format=F00009_. label="Services table(11)";
  attrib Q102_12  format=F00009_. label="Services table(12)";
  attrib Q102_13  format=F00009_. label="Services table(13)";
  attrib Q102_14  format=F00009_. label="Services table(14)";
  attrib Q102_15  format=F00009_. label="Services table(15)";
  attrib Q102_16  format=F00009_. label="Services table(16)";
  attrib Q102_17  format=F00009_. label="Services table(17)";
  attrib Q102_18  format=F00009_. label="Services table(18)";
  attrib Q102_19  format=F00009_. label="Services table(19)";
  attrib Q102_20  format=F00009_. label="Services table(20)";
  attrib Q110     format=F00010_. label="Routine in-patient care";
  attrib Q111     format=F00011_. label="Overnight observation";
  attrib Q112     format=F00012_. label="Number of overnight/in-patient beds";
  attrib Q200     format=F00013_. label="Processing equipment in facility";
  attrib Q201     format=F00014_. label="Location for processing equipment";
  attrib Q210     format=F00015_. label="Store meds (ARVs, vaccines, contraceptives)";
  attrib Q212     format=F00016_. label="Storage of contraceptive commodities";
  attrib Q214     format=F00017_. label="Storage of anti-TB medicines";
  attrib Q216     format=F00018_. label="Storage of antiretroviral (ARV) medicines";
  attrib PROVDERS label="Total provider interviews";
  attrib PROVLNF  label="Total lines for provider list";
  attrib ANCF     label="Total ANC observation";
  attrib FPF      label="Total FP observation";
  attrib SCF      label="Total Sick child observation";
  attrib STIF     label="Total STI observation";
  attrib FACWT    label="Sample weight";
  attrib STEAMF   format=F00019_. label="Steam facility";
  attrib STEAMC   format=F00020_. label="Steam capacity";
  attrib STEAMA   format=F00021_. label="Steam A";
  attrib STEAMB   format=F00022_. label="Steam B";
  attrib STEAMT   format=F00023_. label="Steam timer";
  attrib AUTOF    format=F00024_. label="Autoclave facility";
  attrib AUTOC    format=F00025_. label="Autoclave capacity";
  attrib AUTOWA   format=F00026_. label="AUTOClave wrapped A";
  attrib AUTOWB   format=F00027_. label="AUTOClave wrapped B";
  attrib AUTOUA   format=F00028_. label="AUTOClave unwrapped A";
  attrib AUTOUB   format=F00029_. label="AUTOClave unwrapped B";
  attrib AUTOT    format=F00030_. label="AUTOClave timer";
  attrib PRESSE   format=F00031_. label="EXCellent pressure";
  attrib PRESSG   format=F00032_. label="Good pressure";
  attrib TEMPE    format=F00033_. label="EXCellent TEMPErature";
  attrib TEMPG    format=F00034_. label="Good TEMPErature";
  attrib DHSFAC   format=F00035_. label="Dry heat facility";
  attrib DHSCAP   format=F00036_. label="Dry heat capacity";
  attrib DHSA     format=F00037_. label="Dry heat A";
  attrib DHSB     format=F00038_. label="Dry heat B";
  attrib DHSC     format=F00039_. label="Dry heat C";
  attrib DHST     format=F00040_. label="Dry heat timer";
  attrib HLDFAC   format=F00041_. label="HLD facility";
  attrib HLDCAP   label="HLD capacity";
  attrib HLDA     format=F00042_. label="HLD A";
  attrib HLDB     format=F00043_. label="HLD B";
  attrib HLDC     format=F00044_. label="HLD C";
  attrib HLDT     format=F00045_. label="HLD timer";
  attrib CHEMF    format=F00046_. label="Chemical facilty";
  attrib CHEMA    format=F00047_. label="Chemical A";
  attrib CHEMB    format=F00048_. label="Chemical B";
  attrib CHEMT    format=F00049_. label="Chemical timer";
  attrib SRC      format=F00050_. label="Source for information";
  attrib ANCT     label="Number of ANC clients listed";
  attrib FPT      label="Number of FP clients listed";
  attrib SCT      label="Number of SC listed";
  attrib ISTT     label="Number of IST listed";
  attrib ANYCAP   format=F00051_. label="Any sterilization/HLD capacity";
  attrib ANYKNOWL format=F00052_. label="Any sterilization/HLD knowledge";
  attrib ANYTIMER format=F00053_. label="Any sterilization/HLD timer";
  attrib FACEPI   format=F00054_. label="Facility offers EPI";
  attrib MODTMPFP format=F00055_. label="Facility offers any modern temporary fp";
  attrib STERFP   format=F00056_. label="Facility offers sterilization";
  attrib PAFP     format=F00057_. label="Facility offers periodic abstinence";
  attrib FACPMTCT format=F00058_. label="Facility offers PMTCT";
  attrib FACART   format=F00059_. label="Facility offers ART";
  attrib FACCSS   format=F00060_. label="Facility offers HIV care and support services";
  attrib FACMICROSC format=F00061_. label="Facility has functional microscope";
  attrib FACSLIDES format=F00062_. label="Facility has microscope slides";
  attrib FACTUBES format=F00063_. label="Facility has test tubes";
  attrib FACINCUB format=F00064_. label="Facility has functional incubator";
  attrib FACSHAKER format=F00065_. label="Facility has rotater/shaker";
  attrib FACHIVRDT format=F00066_. label="Facility offers HIV RDT";
  attrib FACHIVLAB format=F00067_. label="Facility offers HIV lab test";
  attrib FACHIVINTRNL format=F00068_. label="Facility offers HIV test internally";
  attrib FACHIVEXTRNL format=F00069_. label="Facility offers HIV test external source";
  attrib FACHIVTST format=F00070_. label="Facility offers HIV test (lab or RDT)";
  attrib FACHIVDXTX format=F00071_. label="Facility offers HIV dx/tx";
  attrib FACDBS   format=F00072_. label="Facility offers dried blood spot testing";
  attrib FACTBRDT format=F00073_. label="Facility offers TB RDT";
  attrib FACTBMICRO format=F00074_. label="Facility offers TB microscopy";
  attrib FACTBCULT format=F00075_. label="Facility offers TB culture";
  attrib FACTBXRAY format=F00076_. label="Facility offers TB xray";
  attrib FACTBINTRNL format=F00077_. label="Facility offers TB test internally";
  attrib FACTBEXTRNL format=F00078_. label="Facility offers TB test external source";
  attrib FACTBTST format=F00079_. label="Facility offers any TB test (lab or RDT)";
  attrib FACTBDXTX format=F00080_. label="Facility offers TB dx/tx";
  attrib FACMALRDT format=F00081_. label="Facility offers malaria RDT";
  attrib FACMALLAB format=F00082_. label="Facility offers malaria lab test";
  attrib FACMALTST format=F00083_. label="Facility offers any malaria test (lab or RDT)";
  attrib FACMALDXTX format=F00084_. label="Facility offers malaria dx/tx";
  attrib SHARPWASTE format=F00085_. label="Facility has good sharps waste disposal";
  attrib MEDWASTE format=F00086_. label="Facility has good medical waste disposal";
  attrib SHARPSTORE format=F00087_. label="Facility has good sharps waste storage";
  attrib MEDSTORE format=F00088_. label="Facility has good medical waste storage";
  attrib ELIGPROV label="Number of eligible providers (codes 1-9, result 1)";
  attrib ELIGTRAIN label="Number of providers with any training last 2 years";
  attrib ELIGSUPERV label="Number of providers supervised last 6 months";
  attrib ELIGBOTH label="Number of providers with supervision & any training last 2 years";
  attrib PROVIMCI label="Number of providers with IMCI training";
  attrib PROVGM   label="Number of providers with growth management training";
  attrib PROVEPI  label="Number of providers with EPI training";
  attrib HEMOGLOB format=F00089_. label="Test: Hemoglobin";
  attrib CD4      format=F00090_. label="Test: CD4";
  attrib BLOODGLUC format=F00091_. label="Test: Blood glucose";
  attrib SYPHSEROL format=F00092_. label="Test: Syph serology";
  attrib STOOLMICRO format=F00093_. label="Test: Stool microscopy";
  attrib Q300     format=F00094_. label="24-hour staff available or on call";
  attrib Q301     format=F00095_. label="Duty schedule or call list are maintained";
  attrib Q302     format=F00096_. label="Duty schedule or call list observed";
  attrib Q310     format=F00097_. label="Has landline telephone";
  attrib Q311     format=F00098_. label="Landline telephone observed";
  attrib Q312     format=F00099_. label="Landline telephone functioning";
  attrib Q313     format=F00100_. label="Has cellphone";
  attrib Q314     format=F00101_. label="Cellphone observed";
  attrib Q315     format=F00102_. label="Cellphone functioning";
  attrib Q316     format=F00103_. label="Has shortwave radio";
  attrib Q317     format=F00104_. label="Shortwave radio observed";
  attrib Q318     format=F00105_. label="Shortwave radio functioning";
  attrib Q319     format=F00106_. label="Has computer";
  attrib Q320     format=F00107_. label="Computer observed";
  attrib Q321     format=F00108_. label="Computer functioning";
  attrib Q322     format=F00109_. label="Access to email/internet";
  attrib Q323     format=F00110_. label="Access to internet at least 2 hours on days of client services";
  attrib Q330     format=F00111_. label="Most commonly used source of water";
  attrib Q331     format=F00112_. label="Water onsite";
  attrib Q332     format=F00113_. label="Routine time of year for water shortage";
  attrib Q333     length=$5 format=$F00114_. label="Dispositions a prendre en cas de pÃ©nurie d'eau?";
  attrib Q340     format=F00115_. label="Facility connected to central electrical grid";
  attrib Q341     format=F00116_. label="Availability of electricity";
  attrib Q342     format=F00117_. label="Other sources of electricity such as generator or solar system";
  attrib Q343     length=$3 format=$F00118_. label="Type of other source of electricity";
  attrib Q345     format=F00119_. label="Functional generator";
  attrib Q346     format=F00120_. label="Fuel (or charged battery) available";
  attrib Q350     format=F00121_. label="External supervision";
  attrib Q351     format=F00122_. label="Last supervisory visit to facility";
  attrib Q352_1   format=F00123_. label="Activities during supervisory visit(1)";
  attrib Q352_2   format=F00123_. label="Activities during supervisory visit(2)";
  attrib Q352_3   format=F00123_. label="Activities during supervisory visit(3)";
  attrib Q360     format=F00124_. label="Routine user fees";
  attrib Q361     format=F00125_. label="Fixed or separate fees";
  attrib Q362_01  format=F00126_. label="Fee schedule(1)";
  attrib Q362_02  format=F00126_. label="Fee schedule(2)";
  attrib Q362_03  format=F00126_. label="Fee schedule(3)";
  attrib Q362_04  format=F00126_. label="Fee schedule(4)";
  attrib Q362_05  format=F00126_. label="Fee schedule(5)";
  attrib Q362_06  format=F00126_. label="Fee schedule(6)";
  attrib Q362_07  format=F00126_. label="Fee schedule(7)";
  attrib Q362_08  format=F00126_. label="Fee schedule(8)";
  attrib Q362_09  format=F00126_. label="Fee schedule(9)";
  attrib Q362_10  format=F00126_. label="Fee schedule(10)";
  attrib Q362_11  format=F00126_. label="Fee schedule(11)";
  attrib Q362_12  format=F00126_. label="Fee schedule(12)";
  attrib Q362_13  format=F00126_. label="Fee schedule(13)";
  attrib Q362_14  format=F00126_. label="Fee schedule(14)";
  attrib Q362_15  format=F00126_. label="Fee schedule(15)";
  attrib Q362_16  format=F00126_. label="Fee schedule(16)";
  attrib Q362_17  format=F00126_. label="Fee schedule(17)";
  attrib Q363     format=F00127_. label="Fees posted";
  attrib Q364     format=F00128_. label="Fees posted observed";
  attrib Q365     length=$5 format=$F00129_. label="Procedure if client cannot pay fee";
  attrib Q370     length=$10 format=$F00130_. label="Source of revenue";
  attrib Q371_1   format=F00131_. label="Mise en place de la politique de gratuite(1)";
  attrib Q371_2   format=F00131_. label="Mise en place de la politique de gratuite(2)";
  attrib Q371_3   format=F00131_. label="Mise en place de la politique de gratuite(3)";
  attrib Q400A_01 label="Number of assigned, employed or seconded staff(1)";
  attrib Q400A_02 label="Number of assigned, employed or seconded staff(2)";
  attrib Q400A_03 label="Number of assigned, employed or seconded staff(3)";
  attrib Q400A_04 label="Number of assigned, employed or seconded staff(4)";
  attrib Q400A_05 label="Number of assigned, employed or seconded staff(5)";
  attrib Q400A_06 label="Number of assigned, employed or seconded staff(6)";
  attrib Q400A_07 label="Number of assigned, employed or seconded staff(7)";
  attrib Q400A_08 label="Number of assigned, employed or seconded staff(8)";
  attrib Q400A_09 label="Number of assigned, employed or seconded staff(9)";
  attrib Q400A_10 label="Number of assigned, employed or seconded staff(10)";
  attrib Q400A_11 label="Number of assigned, employed or seconded staff(11)";
  attrib Q400A_12 label="Number of assigned, employed or seconded staff(12)";
  attrib Q400A_13 label="Number of assigned, employed or seconded staff(13)";
  attrib Q400A_14 label="Number of assigned, employed or seconded staff(14)";
  attrib Q400A_15 label="Number of assigned, employed or seconded staff(15)";
  attrib Q400A_16 label="Number of assigned, employed or seconded staff(16)";
  attrib Q400A_17 label="Number of assigned, employed or seconded staff(17)";
  attrib Q400A_18 label="Number of assigned, employed or seconded staff(18)";
  attrib Q400A_19 label="Number of assigned, employed or seconded staff(19)";
  attrib Q400A_20 label="Number of assigned, employed or seconded staff(20)";
  attrib Q400A_21 label="Number of assigned, employed or seconded staff(21)";
  attrib Q400A_22 label="Number of assigned, employed or seconded staff(22)";
  attrib Q400A_23 label="Number of assigned, employed or seconded staff(23)";
  attrib Q400A_24 label="Number of assigned, employed or seconded staff(24)";
  attrib Q400B_01 label="Number of part time(1)";
  attrib Q400B_02 label="Number of part time(2)";
  attrib Q400B_03 label="Number of part time(3)";
  attrib Q400B_04 label="Number of part time(4)";
  attrib Q400B_05 label="Number of part time(5)";
  attrib Q400B_06 label="Number of part time(6)";
  attrib Q400B_07 label="Number of part time(7)";
  attrib Q400B_08 label="Number of part time(8)";
  attrib Q400B_09 label="Number of part time(9)";
  attrib Q400B_10 label="Number of part time(10)";
  attrib Q400B_11 label="Number of part time(11)";
  attrib Q400B_12 label="Number of part time(12)";
  attrib Q400B_13 label="Number of part time(13)";
  attrib Q400B_14 label="Number of part time(14)";
  attrib Q400B_15 label="Number of part time(15)";
  attrib Q400B_16 label="Number of part time(16)";
  attrib Q400B_17 label="Number of part time(17)";
  attrib Q400B_18 label="Number of part time(18)";
  attrib Q400B_19 label="Number of part time(19)";
  attrib Q400B_20 label="Number of part time(20)";
  attrib Q400B_21 label="Number of part time(21)";
  attrib Q400B_22 label="Number of part time(22)";
  attrib Q400B_23 label="Number of part time(23)";
  attrib Q400B_24 label="Number of part time(24)";
  attrib Q400AT   label="Total staff assigned, employed or seconded staff";
  attrib Q400BT   label="Total part time";
  attrib Q410     format=F00132_. label="Routine management meetings";
  attrib Q411     format=F00133_. label="Frequency of routine management meetings";
  attrib Q412     format=F00134_. label="Official records of management meetings";
  attrib Q413     format=F00135_. label="Official records of management meetings - observed";
  attrib Q414     length=$11 format=$F00136_. label="Items listed in official records";
  attrib Q415     format=F00137_. label="Facility made decisions based on discussion at last meeting";
  attrib Q416     format=F00138_. label="Facility taken follow-up actions based on meeting";
  attrib Q416A    format=F00139_. label="Does the structure regularly participate in the six-month meetings of the Management Committee?";
  attrib Q416B    format=F00140_. label="Can I see the minutes of the most recent meeting?";
  attrib Q417     format=F00141_. label="Routine meetings of management & community members";
  attrib Q418     format=F00142_. label="Frequency of management/ community meetings";
  attrib Q419     format=F00143_. label="Official records of management/ community meetings";
  attrib Q420     format=F00144_. label="Official records of management/ community meetings - observed";
  attrib Q430     format=F00145_. label="System to determine client opinions";
  attrib Q431     length=$11 format=$F00146_. label="Methods uses to elicit client opinions";
  attrib Q432     format=F00147_. label="Procedure to review client opinions";
  attrib Q433     format=F00148_. label="Procedure to review client opinions - observed";
  attrib Q440     format=F00149_. label="Routine QA activities";
  attrib Q441     format=F00150_. label="Official record of QA activities in past year";
  attrib Q442     format=F00151_. label="Official record of QA activities in past year - observed";
  attrib Q450     format=F00152_. label="Functional ambulance at facility";
  attrib Q451     format=F00153_. label="Functional ambulance at facility - observed";
  attrib Q452     format=F00154_. label="Ambulance available elsewhere";
  attrib Q452A    format=F00155_. label="Est ce que les patients payent pour avoir access a l'ambulance?";
  attrib Q453     format=F00156_. label="Fuel available";
  attrib Q460     format=F00157_. label="System to compile health services data";
  attrib Q461     format=F00158_. label="Facility regularly compiles information";
  attrib Q462     format=F00159_. label="Frequency of compilation";
  attrib Q463     format=F00160_. label="Copy of most recent report seen";
  attrib Q464     format=F00161_. label="Designated person for health statistics";
  attrib Q465     format=F00162_. label="Designated person for health statistics";
  attrib Q471     format=F00163_. label="Number of live discharges last full-month";
  attrib Q472     format=F00164_. label="Number of outpatient client visits last full month";
  attrib C400A_1  label="Number of assigned, employed or seconded staff(1)";
  attrib C400A_2  label="Number of assigned, employed or seconded staff(2)";
  attrib C400A_3  label="Number of assigned, employed or seconded staff(3)";
  attrib C400A_4  label="Number of assigned, employed or seconded staff(4)";
  attrib C400A_5  label="Number of assigned, employed or seconded staff(5)";
  attrib C400A_6  label="Number of assigned, employed or seconded staff(6)";
  attrib C400B_1  label="Number of part time(1)";
  attrib C400B_2  label="Number of part time(2)";
  attrib C400B_3  label="Number of part time(3)";
  attrib C400B_4  label="Number of part time(4)";
  attrib C400B_5  label="Number of part time(5)";
  attrib C400B_6  label="Number of part time(6)";
  attrib C400AT   label="Total staff assigned, employed or seconded staff";
  attrib C400BT   label="Total part time";
  attrib Q501A_01 format=F00165_. label="Use and availability(1)";
  attrib Q501A_02 format=F00165_. label="Use and availability(2)";
  attrib Q501A_03 format=F00165_. label="Use and availability(3)";
  attrib Q501A_04 format=F00165_. label="Use and availability(4)";
  attrib Q501A_05 format=F00165_. label="Use and availability(5)";
  attrib Q501A_06 format=F00165_. label="Use and availability(6)";
  attrib Q501A_07 format=F00165_. label="Use and availability(7)";
  attrib Q501A_08 format=F00165_. label="Use and availability(8)";
  attrib Q501A_09 format=F00165_. label="Use and availability(9)";
  attrib Q501A_10 format=F00165_. label="Use and availability(10)";
  attrib Q501B_01 format=F00166_. label="Equipment functioning(1)";
  attrib Q501B_02 format=F00166_. label="Equipment functioning(2)";
  attrib Q501B_03 format=F00166_. label="Equipment functioning(3)";
  attrib Q501B_04 format=F00166_. label="Equipment functioning(4)";
  attrib Q501B_05 format=F00166_. label="Equipment functioning(5)";
  attrib Q501B_06 format=F00166_. label="Equipment functioning(6)";
  attrib Q501B_07 format=F00166_. label="Equipment functioning(7)";
  attrib Q501B_08 format=F00166_. label="Equipment functioning(8)";
  attrib Q501B_09 format=F00166_. label="Equipment functioning(9)";
  attrib Q501B_10 format=F00166_. label="Equipment functioning(10)";
  attrib Q502A_1  format=F00167_. label="Method used(1)";
  attrib Q502A_2  format=F00167_. label="Method used(2)";
  attrib Q502A_3  format=F00167_. label="Method used(3)";
  attrib Q502A_4  format=F00167_. label="Method used(4)";
  attrib Q502A_5  format=F00167_. label="Method used(5)";
  attrib Q502B_1  format=F00168_. label="Temperature(1)";
  attrib Q502B_2  format=F00168_. label="Temperature(2)";
  attrib Q502B_3  format=F00168_. label="Temperature(3)";
  attrib Q502B_4  format=F00168_. label="Temperature(4)";
  attrib Q502B_5  format=F00168_. label="Temperature(5)";
  attrib Q502C_1  format=F00169_. label="Pressure(1)";
  attrib Q502C_2  format=F00169_. label="Pressure(2)";
  attrib Q502C_3  format=F00169_. label="Pressure(3)";
  attrib Q502C_4  format=F00169_. label="Pressure(4)";
  attrib Q502C_5  format=F00169_. label="Pressure(5)";
  attrib Q502D_1  format=F00170_. label="Units of pressure(1)";
  attrib Q502D_2  format=F00170_. label="Units of pressure(2)";
  attrib Q502D_3  format=F00170_. label="Units of pressure(3)";
  attrib Q502D_4  format=F00170_. label="Units of pressure(4)";
  attrib Q502D_5  format=F00170_. label="Units of pressure(5)";
  attrib Q502E_1  format=F00171_. label="Time not wrapped(1)";
  attrib Q502E_2  format=F00171_. label="Time not wrapped(2)";
  attrib Q502E_3  format=F00171_. label="Time not wrapped(3)";
  attrib Q502E_4  format=F00171_. label="Time not wrapped(4)";
  attrib Q502E_5  format=F00171_. label="Time not wrapped(5)";
  attrib Q502F_1  format=F00172_. label="Time wrapped(1)";
  attrib Q502F_2  format=F00172_. label="Time wrapped(2)";
  attrib Q502F_3  format=F00172_. label="Time wrapped(3)";
  attrib Q502F_4  format=F00172_. label="Time wrapped(4)";
  attrib Q502F_5  format=F00172_. label="Time wrapped(5)";
  attrib Q502G_1  format=F00173_. label="Chemical used(1)";
  attrib Q502G_2  format=F00173_. label="Chemical used(2)";
  attrib Q502G_3  format=F00173_. label="Chemical used(3)";
  attrib Q502G_4  format=F00173_. label="Chemical used(4)";
  attrib Q502G_5  format=F00173_. label="Chemical used(5)";
  attrib Q503     format=F00174_. label="Guidelines for sterilization";
  attrib Q504     format=F00175_. label="Guidelines for sterilization - observed";
  attrib C501A_1  format=F00176_. label="Use and availability(1)";
  attrib C501A_2  format=F00176_. label="Use and availability(2)";
  attrib C501A_3  format=F00176_. label="Use and availability(3)";
  attrib C501B_1  format=F00177_. label="Equipment functioning(1)";
  attrib C501B_2  format=F00177_. label="Equipment functioning(2)";
  attrib C501B_3  format=F00177_. label="Equipment functioning(3)";
  attrib C502A_1  format=F00178_. label="Method used(1)";
  attrib C502A_2  format=F00178_. label="Method used(2)";
  attrib C502E_1  format=F00179_. label="Time not wrapped(1)";
  attrib C502E_2  format=F00179_. label="Time not wrapped(2)";
  attrib C502G_1  format=F00180_. label="Chemical used(1)";
  attrib C502G_2  format=F00180_. label="Chemical used(2)";
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
  attrib Q700A_01 format=F00191_. label="Availability of basic supplies(1)";
  attrib Q700A_02 format=F00191_. label="Availability of basic supplies(2)";
  attrib Q700A_03 format=F00191_. label="Availability of basic supplies(3)";
  attrib Q700A_04 format=F00191_. label="Availability of basic supplies(4)";
  attrib Q700A_05 format=F00191_. label="Availability of basic supplies(5)";
  attrib Q700A_06 format=F00191_. label="Availability of basic supplies(6)";
  attrib Q700A_07 format=F00191_. label="Availability of basic supplies(7)";
  attrib Q700A_08 format=F00191_. label="Availability of basic supplies(8)";
  attrib Q700A_09 format=F00191_. label="Availability of basic supplies(9)";
  attrib Q700A_10 format=F00191_. label="Availability of basic supplies(10)";
  attrib Q700A_11 format=F00191_. label="Availability of basic supplies(11)";
  attrib Q700A_12 format=F00191_. label="Availability of basic supplies(12)";
  attrib Q700A_13 format=F00191_. label="Availability of basic supplies(13)";
  attrib Q700A_14 format=F00191_. label="Availability of basic supplies(14)";
  attrib Q700A_15 format=F00191_. label="Availability of basic supplies(15)";
  attrib Q700A_16 format=F00191_. label="Availability of basic supplies(16)";
  attrib Q700A_17 format=F00191_. label="Availability of basic supplies(17)";
  attrib Q700A_18 format=F00191_. label="Availability of basic supplies(18)";
  attrib Q700A_19 format=F00191_. label="Availability of basic supplies(19)";
  attrib Q700A_20 format=F00191_. label="Availability of basic supplies(20)";
  attrib Q700A_21 format=F00191_. label="Availability of basic supplies(21)";
  attrib Q700A_22 format=F00191_. label="Availability of basic supplies(22)";
  attrib Q700A_23 format=F00191_. label="Availability of basic supplies(23)";
  attrib Q700A_24 format=F00191_. label="Availability of basic supplies(24)";
  attrib Q700A_25 format=F00191_. label="Availability of basic supplies(25)";
  attrib Q700B_01 format=F00192_. label="Basic supplies functioning(1)";
  attrib Q700B_02 format=F00192_. label="Basic supplies functioning(2)";
  attrib Q700B_03 format=F00192_. label="Basic supplies functioning(3)";
  attrib Q700B_04 format=F00192_. label="Basic supplies functioning(4)";
  attrib Q700B_05 format=F00192_. label="Basic supplies functioning(5)";
  attrib Q700B_06 format=F00192_. label="Basic supplies functioning(6)";
  attrib Q700B_07 format=F00192_. label="Basic supplies functioning(7)";
  attrib Q700B_08 format=F00192_. label="Basic supplies functioning(8)";
  attrib Q700B_09 format=F00192_. label="Basic supplies functioning(9)";
  attrib Q700B_10 format=F00192_. label="Basic supplies functioning(10)";
  attrib Q700B_11 format=F00192_. label="Basic supplies functioning(11)";
  attrib Q700B_12 format=F00192_. label="Basic supplies functioning(12)";
  attrib Q700B_13 format=F00192_. label="Basic supplies functioning(13)";
  attrib Q700B_14 format=F00192_. label="Basic supplies functioning(14)";
  attrib Q700B_15 format=F00192_. label="Basic supplies functioning(15)";
  attrib Q700B_16 format=F00192_. label="Basic supplies functioning(16)";
  attrib Q700B_17 format=F00192_. label="Basic supplies functioning(17)";
  attrib Q700B_18 format=F00192_. label="Basic supplies functioning(18)";
  attrib Q700B_19 format=F00192_. label="Basic supplies functioning(19)";
  attrib Q700B_20 format=F00192_. label="Basic supplies functioning(20)";
  attrib Q700B_21 format=F00192_. label="Basic supplies functioning(21)";
  attrib Q700B_22 format=F00192_. label="Basic supplies functioning(22)";
  attrib Q700B_23 format=F00192_. label="Basic supplies functioning(23)";
  attrib Q700B_24 format=F00192_. label="Basic supplies functioning(24)";
  attrib Q700B_25 format=F00192_. label="Basic supplies functioning(25)";
  attrib Q710_01  format=F00193_. label="Precaution and condition of client examination room(1)";
  attrib Q710_02  format=F00193_. label="Precaution and condition of client examination room(2)";
  attrib Q710_03  format=F00193_. label="Precaution and condition of client examination room(3)";
  attrib Q710_04  format=F00193_. label="Precaution and condition of client examination room(4)";
  attrib Q710_05  format=F00193_. label="Precaution and condition of client examination room(5)";
  attrib Q710_06  format=F00193_. label="Precaution and condition of client examination room(6)";
  attrib Q710_07  format=F00193_. label="Precaution and condition of client examination room(7)";
  attrib Q710_08  format=F00193_. label="Precaution and condition of client examination room(8)";
  attrib Q710_09  format=F00193_. label="Precaution and condition of client examination room(9)";
  attrib Q710_10  format=F00193_. label="Precaution and condition of client examination room(10)";
  attrib Q710_11  format=F00193_. label="Precaution and condition of client examination room(11)";
  attrib Q710_12  format=F00193_. label="Precaution and condition of client examination room(12)";
  attrib Q710_13  format=F00193_. label="Precaution and condition of client examination room(13)";
  attrib Q711     format=F00194_. label="Setting of examination room";
  attrib Q720     format=F00195_. label="Protected client waiting area";
  attrib C700A_01 format=F00196_. label="Availability of basic supplies(1)";
  attrib C700A_02 format=F00196_. label="Availability of basic supplies(2)";
  attrib C700A_03 format=F00196_. label="Availability of basic supplies(3)";
  attrib C700A_04 format=F00196_. label="Availability of basic supplies(4)";
  attrib C700A_05 format=F00196_. label="Availability of basic supplies(5)";
  attrib C700A_06 format=F00196_. label="Availability of basic supplies(6)";
  attrib C700A_07 format=F00196_. label="Availability of basic supplies(7)";
  attrib C700A_08 format=F00196_. label="Availability of basic supplies(8)";
  attrib C700A_09 format=F00196_. label="Availability of basic supplies(9)";
  attrib C700A_10 format=F00196_. label="Availability of basic supplies(10)";
  attrib C700A_11 format=F00196_. label="Availability of basic supplies(11)";
  attrib C700A_12 format=F00196_. label="Availability of basic supplies(12)";
  attrib C700A_13 format=F00196_. label="Availability of basic supplies(13)";
  attrib C700A_14 format=F00196_. label="Availability of basic supplies(14)";
  attrib C700A_15 format=F00196_. label="Availability of basic supplies(15)";
  attrib C700A_16 format=F00196_. label="Availability of basic supplies(16)";
  attrib C700A_17 format=F00196_. label="Availability of basic supplies(17)";
  attrib C700A_18 format=F00196_. label="Availability of basic supplies(18)";
  attrib C700A_19 format=F00196_. label="Availability of basic supplies(19)";
  attrib C700A_20 format=F00196_. label="Availability of basic supplies(20)";
  attrib C700A_21 format=F00196_. label="Availability of basic supplies(21)";
  attrib C700A_22 format=F00196_. label="Availability of basic supplies(22)";
  attrib C700A_23 format=F00196_. label="Availability of basic supplies(23)";
  attrib C700A_24 format=F00196_. label="Availability of basic supplies(24)";
  attrib C700A_25 format=F00196_. label="Availability of basic supplies(25)";
  attrib C700A_26 format=F00196_. label="Availability of basic supplies(26)";
  attrib C700A_27 format=F00196_. label="Availability of basic supplies(27)";
  attrib C700A_28 format=F00196_. label="Availability of basic supplies(28)";
  attrib C700A_29 format=F00196_. label="Availability of basic supplies(29)";
  attrib C700A_30 format=F00196_. label="Availability of basic supplies(30)";
  attrib C700A_31 format=F00196_. label="Availability of basic supplies(31)";
  attrib C700A_32 format=F00196_. label="Availability of basic supplies(32)";
  attrib C700A_33 format=F00196_. label="Availability of basic supplies(33)";
  attrib C700A_34 format=F00196_. label="Availability of basic supplies(34)";
  attrib C700B_01 format=F00197_. label="Basic supplies functioning(1)";
  attrib C700B_02 format=F00197_. label="Basic supplies functioning(2)";
  attrib C700B_03 format=F00197_. label="Basic supplies functioning(3)";
  attrib C700B_04 format=F00197_. label="Basic supplies functioning(4)";
  attrib C700B_05 format=F00197_. label="Basic supplies functioning(5)";
  attrib C700B_06 format=F00197_. label="Basic supplies functioning(6)";
  attrib C700B_07 format=F00197_. label="Basic supplies functioning(7)";
  attrib C700B_08 format=F00197_. label="Basic supplies functioning(8)";
  attrib C700B_09 format=F00197_. label="Basic supplies functioning(9)";
  attrib C700B_10 format=F00197_. label="Basic supplies functioning(10)";
  attrib C700B_11 format=F00197_. label="Basic supplies functioning(11)";
  attrib C700B_12 format=F00197_. label="Basic supplies functioning(12)";
  attrib C700B_13 format=F00197_. label="Basic supplies functioning(13)";
  attrib C700B_14 format=F00197_. label="Basic supplies functioning(14)";
  attrib C700B_15 format=F00197_. label="Basic supplies functioning(15)";
  attrib C700B_16 format=F00197_. label="Basic supplies functioning(16)";
  attrib C700B_17 format=F00197_. label="Basic supplies functioning(17)";
  attrib C700B_18 format=F00197_. label="Basic supplies functioning(18)";
  attrib C700B_19 format=F00197_. label="Basic supplies functioning(19)";
  attrib C700B_20 format=F00197_. label="Basic supplies functioning(20)";
  attrib C700B_21 format=F00197_. label="Basic supplies functioning(21)";
  attrib C700B_22 format=F00197_. label="Basic supplies functioning(22)";
  attrib C700B_23 format=F00197_. label="Basic supplies functioning(23)";
  attrib C700B_24 format=F00197_. label="Basic supplies functioning(24)";
  attrib C700B_25 format=F00197_. label="Basic supplies functioning(25)";
  attrib C700B_26 format=F00197_. label="Basic supplies functioning(26)";
  attrib C700B_27 format=F00197_. label="Basic supplies functioning(27)";
  attrib C700B_28 format=F00197_. label="Basic supplies functioning(28)";
  attrib C700B_29 format=F00197_. label="Basic supplies functioning(29)";
  attrib C700B_30 format=F00197_. label="Basic supplies functioning(30)";
  attrib C700B_31 format=F00197_. label="Basic supplies functioning(31)";
  attrib C700B_32 format=F00197_. label="Basic supplies functioning(32)";
  attrib C700B_33 format=F00197_. label="Basic supplies functioning(33)";
  attrib C700B_34 format=F00197_. label="Basic supplies functioning(34)";
  attrib Q801     format=F00198_. label="Anemia testing";
  attrib Q802A_1  format=F00199_. label="Anemia equipment used(1)";
  attrib Q802A_2  format=F00199_. label="Anemia equipment used(2)";
  attrib Q802A_3  format=F00199_. label="Anemia equipment used(3)";
  attrib Q802A_4  format=F00199_. label="Anemia equipment used(4)";
  attrib Q802A_5  format=F00199_. label="Anemia equipment used(5)";
  attrib Q802A_6  format=F00199_. label="Anemia equipment used(6)";
  attrib Q802A_7  format=F00199_. label="Anemia equipment used(7)";
  attrib Q802A_8  format=F00199_. label="Anemia equipment used(8)";
  attrib Q802A_9  format=F00199_. label="Anemia equipment used(9)";
  attrib Q802B_1  format=F00200_. label="Observed equipment(1)";
  attrib Q802B_2  format=F00200_. label="Observed equipment(2)";
  attrib Q802B_3  format=F00200_. label="Observed equipment(3)";
  attrib Q802B_4  format=F00200_. label="Observed equipment(4)";
  attrib Q802B_5  format=F00200_. label="Observed equipment(5)";
  attrib Q802B_6  format=F00200_. label="Observed equipment(6)";
  attrib Q802B_7  format=F00200_. label="Observed equipment(7)";
  attrib Q802B_8  format=F00200_. label="Observed equipment(8)";
  attrib Q802B_9  format=F00200_. label="Observed equipment(9)";
  attrib Q802C_1  format=F00201_. label="Functioning(1)";
  attrib Q802C_2  format=F00201_. label="Functioning(2)";
  attrib Q802C_3  format=F00201_. label="Functioning(3)";
  attrib Q802C_4  format=F00201_. label="Functioning(4)";
  attrib Q802C_5  format=F00201_. label="Functioning(5)";
  attrib Q802C_6  format=F00201_. label="Functioning(6)";
  attrib Q802C_7  format=F00201_. label="Functioning(7)";
  attrib Q802C_8  format=F00201_. label="Functioning(8)";
  attrib Q802C_9  format=F00201_. label="Functioning(9)";
  attrib Q803     format=F00202_. label="CD4 equipment used";
  attrib Q804     format=F00203_. label="Observed CD4 equipment";
  attrib Q805     format=F00204_. label="Functioning CD4 equipment";
  attrib Q806     format=F00205_. label="Conduct any HIV tests including HIV RDT";
  attrib Q807     format=F00206_. label="HIV rapid test kit available";
  attrib Q808     format=F00207_. label="Observe HIV rapid test kit available";
  attrib Q809     format=F00208_. label="Use filter paper card to collect DBS";
  attrib Q810     format=F00209_. label="Observe DBS filter paper card";
  attrib Q811A_1  format=F00210_. label="HIV test equipment used(1)";
  attrib Q811A_2  format=F00210_. label="HIV test equipment used(2)";
  attrib Q811A_3  format=F00210_. label="HIV test equipment used(3)";
  attrib Q811A_4  format=F00210_. label="HIV test equipment used(4)";
  attrib Q811A_5  format=F00210_. label="HIV test equipment used(5)";
  attrib Q811A_6  format=F00210_. label="HIV test equipment used(6)";
  attrib Q811B_1  format=F00211_. label="HIV test equipment observed(1)";
  attrib Q811B_2  format=F00211_. label="HIV test equipment observed(2)";
  attrib Q811B_3  format=F00211_. label="HIV test equipment observed(3)";
  attrib Q811B_4  format=F00211_. label="HIV test equipment observed(4)";
  attrib Q811B_5  format=F00211_. label="HIV test equipment observed(5)";
  attrib Q811B_6  format=F00211_. label="HIV test equipment observed(6)";
  attrib Q811C_1  format=F00212_. label="HIV test equipment functioning(1)";
  attrib Q811C_2  format=F00212_. label="HIV test equipment functioning(2)";
  attrib Q811C_3  format=F00212_. label="HIV test equipment functioning(3)";
  attrib Q811C_4  format=F00212_. label="HIV test equipment functioning(4)";
  attrib Q811C_5  format=F00212_. label="HIV test equipment functioning(5)";
  attrib Q811C_6  format=F00212_. label="HIV test equipment functioning(6)";
  attrib Q812     format=F00213_. label="Written guidelines for HIV testing";
  attrib Q813     format=F00214_. label="Written guidelines for HIV testing - Observed";
  attrib Q814     format=F00215_. label="Written guidelines on confidentiality for HIV testing";
  attrib Q815     format=F00216_. label="Written guidelines on confidentiality for HIV testing - Observed";
  attrib Q816     format=F00217_. label="Other guidelines for HIV services";
  attrib Q817     format=F00218_. label="Other guidelines for HIV services - Observed";
  attrib Q818     format=F00219_. label="External QC for HIV testing";
  attrib Q819     length=$4 format=$F00220_. label="System of external QC for HIV";
  attrib Q820     format=F00221_. label="Record of external QC results";
  attrib Q821     format=F00222_. label="Observed record of external QC results";
  attrib Q822     format=F00223_. label="Most recent error rate for external QC";
  attrib Q823     format=F00224_. label="Send blood outside for HIV dx testing";
  attrib Q824     length=$5 format=$F00225_. label="HIV tests conducted outside facility";
  attrib Q825     format=F00226_. label="Record of outside test results";
  attrib Q826     format=F00227_. label="Record of outside test results - Observed";
  attrib Q827_01  format=F00228_. label="Precaution and condition in client examination room(1)";
  attrib Q827_02  format=F00228_. label="Precaution and condition in client examination room(2)";
  attrib Q827_03  format=F00228_. label="Precaution and condition in client examination room(3)";
  attrib Q827_04  format=F00228_. label="Precaution and condition in client examination room(4)";
  attrib Q827_05  format=F00228_. label="Precaution and condition in client examination room(5)";
  attrib Q827_06  format=F00228_. label="Precaution and condition in client examination room(6)";
  attrib Q827_07  format=F00228_. label="Precaution and condition in client examination room(7)";
  attrib Q827_08  format=F00228_. label="Precaution and condition in client examination room(8)";
  attrib Q827_09  format=F00228_. label="Precaution and condition in client examination room(9)";
  attrib Q827_10  format=F00228_. label="Precaution and condition in client examination room(10)";
  attrib Q827_11  format=F00228_. label="Precaution and condition in client examination room(11)";
  attrib Q827_12  format=F00228_. label="Precaution and condition in client examination room(12)";
  attrib Q827_13  format=F00228_. label="Precaution and condition in client examination room(13)";
  attrib Q830     format=F00229_. label="Blood glucose tests";
  attrib Q831A_1  format=F00230_. label="Glucose equipment used(1)";
  attrib Q831A_2  format=F00230_. label="Glucose equipment used(2)";
  attrib Q831B_1  format=F00231_. label="Observed equipment(1)";
  attrib Q831B_2  format=F00231_. label="Observed equipment(2)";
  attrib Q831C_1  format=F00232_. label="Functioning(1)";
  attrib Q831C_2  format=F00232_. label="Functioning(2)";
  attrib Q832     format=F00233_. label="Lever or renal function tests";
  attrib Q833     format=F00234_. label="Blood chemistry analyzer";
  attrib Q834     format=F00235_. label="Blood chemistry analyzer - Observed";
  attrib Q835     format=F00236_. label="Blood chemistry analyzer - Functioning";
  attrib Q836     format=F00237_. label="Urine chemistry testing/ urine pregnancy tests";
  attrib Q837A_1  format=F00238_. label="Dipsticks used(1)";
  attrib Q837A_2  format=F00238_. label="Dipsticks used(2)";
  attrib Q837A_3  format=F00238_. label="Dipsticks used(3)";
  attrib Q837B_1  format=F00239_. label="Dipsticks observed(1)";
  attrib Q837B_2  format=F00239_. label="Dipsticks observed(2)";
  attrib Q837B_3  format=F00239_. label="Dipsticks observed(3)";
  attrib Q838     format=F00240_. label="Send blood/ urine outside";
  attrib Q839A_1  format=F00241_. label="Tests outside(1)";
  attrib Q839A_2  format=F00241_. label="Tests outside(2)";
  attrib Q839A_3  format=F00241_. label="Tests outside(3)";
  attrib Q839A_4  format=F00241_. label="Tests outside(4)";
  attrib Q839B_1  format=F00242_. label="Record of outside test results(1)";
  attrib Q839B_2  format=F00242_. label="Record of outside test results(2)";
  attrib Q839B_3  format=F00242_. label="Record of outside test results(3)";
  attrib Q839B_4  format=F00242_. label="Record of outside test results(4)";
  attrib Q840A_1  format=F00243_. label="Parasite equipment used(1)";
  attrib Q840A_2  format=F00243_. label="Parasite equipment used(2)";
  attrib Q840A_3  format=F00243_. label="Parasite equipment used(3)";
  attrib Q840A_4  format=F00243_. label="Parasite equipment used(4)";
  attrib Q840A_5  format=F00243_. label="Parasite equipment used(5)";
  attrib Q840A_6  format=F00243_. label="Parasite equipment used(6)";
  attrib Q840A_7  format=F00243_. label="Parasite equipment used(7)";
  attrib Q840A_8  format=F00243_. label="Parasite equipment used(8)";
  attrib Q840B_1  format=F00244_. label="Parasite equipment observed(1)";
  attrib Q840B_2  format=F00244_. label="Parasite equipment observed(2)";
  attrib Q840B_3  format=F00244_. label="Parasite equipment observed(3)";
  attrib Q840B_4  format=F00244_. label="Parasite equipment observed(4)";
  attrib Q840B_5  format=F00244_. label="Parasite equipment observed(5)";
  attrib Q840B_6  format=F00244_. label="Parasite equipment observed(6)";
  attrib Q840B_7  format=F00244_. label="Parasite equipment observed(7)";
  attrib Q840B_8  format=F00244_. label="Parasite equipment observed(8)";
  attrib Q840C_1  format=F00245_. label="Functioning(1)";
  attrib Q840C_2  format=F00245_. label="Functioning(2)";
  attrib Q840C_3  format=F00245_. label="Functioning(3)";
  attrib Q840C_4  format=F00245_. label="Functioning(4)";
  attrib Q840C_5  format=F00245_. label="Functioning(5)";
  attrib Q840C_6  format=F00245_. label="Functioning(6)";
  attrib Q840C_7  format=F00245_. label="Functioning(7)";
  attrib Q840C_8  format=F00245_. label="Functioning(8)";
  attrib Q841     format=F00246_. label="Malaria testing";
  attrib Q842     format=F00247_. label="Malaria rapid testing kit";
  attrib Q843     format=F00248_. label="Observe Malaria rapid testing kit";
  attrib Q844     format=F00249_. label="Brand/type of malaria RDT kit";
  attrib Q845     format=F00250_. label="Training manual/poster/other job aid for RDT kit";
  attrib Q846     format=F00251_. label="Observed training manual/poster/other job aid for RDT kit";
  attrib Q847A_1  format=F00252_. label="Malaria test used(1)";
  attrib Q847A_2  format=F00252_. label="Malaria test used(2)";
  attrib Q847A_3  format=F00252_. label="Malaria test used(3)";
  attrib Q847A_4  format=F00252_. label="Malaria test used(4)";
  attrib Q847B_1  format=F00253_. label="Malaria equipment available(1)";
  attrib Q847B_2  format=F00253_. label="Malaria equipment available(2)";
  attrib Q847B_3  format=F00253_. label="Malaria equipment available(3)";
  attrib Q847B_4  format=F00253_. label="Malaria equipment available(4)";
  attrib Q848     format=F00254_. label="Gram staining";
  attrib Q849_1   format=F00255_. label="Stains available(1)";
  attrib Q849_2   format=F00255_. label="Stains available(2)";
  attrib Q849_3   format=F00255_. label="Stains available(3)";
  attrib Q849_4   format=F00255_. label="Stains available(4)";
  attrib Q850     format=F00256_. label="Specimens sent outside for staining or malaria testing";
  attrib Q851A_1  format=F00257_. label="Tests sent outside(1)";
  attrib Q851A_2  format=F00257_. label="Tests sent outside(2)";
  attrib Q851A_3  format=F00257_. label="Tests sent outside(3)";
  attrib Q851A_4  format=F00257_. label="Tests sent outside(4)";
  attrib Q851B_1  format=F00258_. label="Record of results(1)";
  attrib Q851B_2  format=F00258_. label="Record of results(2)";
  attrib Q851B_3  format=F00258_. label="Record of results(3)";
  attrib Q851B_4  format=F00258_. label="Record of results(4)";
  attrib Q852     format=F00259_. label="Stool microscopy";
  attrib Q853A_1  format=F00260_. label="Equipment used(1)";
  attrib Q853A_2  format=F00260_. label="Equipment used(2)";
  attrib Q853A_3  format=F00260_. label="Equipment used(3)";
  attrib Q853B_1  format=F00261_. label="Equipment available(1)";
  attrib Q853B_2  format=F00261_. label="Equipment available(2)";
  attrib Q853B_3  format=F00261_. label="Equipment available(3)";
  attrib Q854     format=F00262_. label="Syphilis tests";
  attrib Q855     format=F00263_. label="Syphilis rapid diagnostic test (RDT) kit";
  attrib Q856     format=F00264_. label="Observe Syphilis rapid test kit observed";
  attrib Q857     format=F00265_. label="Conduct any other syphilis testing";
  attrib Q858A_1  format=F00266_. label="Test conducted(1)";
  attrib Q858A_2  format=F00266_. label="Test conducted(2)";
  attrib Q858A_3  format=F00266_. label="Test conducted(3)";
  attrib Q858A_4  format=F00266_. label="Test conducted(4)";
  attrib Q858A_5  format=F00266_. label="Test conducted(5)";
  attrib Q858B_1  format=F00267_. label="Items for test available(1)";
  attrib Q858B_2  format=F00267_. label="Items for test available(2)";
  attrib Q858B_3  format=F00267_. label="Items for test available(3)";
  attrib Q858B_4  format=F00267_. label="Items for test available(4)";
  attrib Q858B_5  format=F00267_. label="Items for test available(5)";
  attrib Q858C_1  format=F00268_. label="Functioning(1)";
  attrib Q858C_2  format=F00268_. label="Functioning(2)";
  attrib Q858C_3  format=F00268_. label="Functioning(3)";
  attrib Q858C_4  format=F00268_. label="Functioning(4)";
  attrib Q858C_5  format=F00268_. label="Functioning(5)";
  attrib Q859     format=F00269_. label="Chlamydia test";
  attrib Q860A_1  format=F00270_. label="Chlamydia test conducted(1)";
  attrib Q860A_2  format=F00270_. label="Chlamydia test conducted(2)";
  attrib Q860B_1  format=F00271_. label="Items for test available(1)";
  attrib Q860B_2  format=F00271_. label="Items for test available(2)";
  attrib Q861     format=F00272_. label="TB tests";
  attrib Q862A_1  format=F00273_. label="TB test used(1)";
  attrib Q862A_2  format=F00273_. label="TB test used(2)";
  attrib Q862A_3  format=F00273_. label="TB test used(3)";
  attrib Q862A_4  format=F00273_. label="TB test used(4)";
  attrib Q862A_5  format=F00273_. label="TB test used(5)";
  attrib Q862A_6  format=F00273_. label="TB test used(6)";
  attrib Q862A_7  format=F00273_. label="TB test used(7)";
  attrib Q862B_1  format=F00274_. label="TB equipment available(1)";
  attrib Q862B_2  format=F00274_. label="TB equipment available(2)";
  attrib Q862B_3  format=F00274_. label="TB equipment available(3)";
  attrib Q862B_4  format=F00274_. label="TB equipment available(4)";
  attrib Q862B_5  format=F00274_. label="TB equipment available(5)";
  attrib Q862B_6  format=F00274_. label="TB equipment available(6)";
  attrib Q862B_7  format=F00274_. label="TB equipment available(7)";
  attrib Q862C_1  format=F00275_. label="Functioning(1)";
  attrib Q862C_2  format=F00275_. label="Functioning(2)";
  attrib Q862C_3  format=F00275_. label="Functioning(3)";
  attrib Q862C_4  format=F00275_. label="Functioning(4)";
  attrib Q862C_5  format=F00275_. label="Functioning(5)";
  attrib Q862C_6  format=F00275_. label="Functioning(6)";
  attrib Q862C_7  format=F00275_. label="Functioning(7)";
  attrib Q863     format=F00276_. label="Use TB diagnostic test";
  attrib Q864     format=F00277_. label="Observe TB rapid diagnostic test (RDT) kit";
  attrib Q865     format=F00278_. label="Maintain any sputum containers";
  attrib Q866     format=F00279_. label="Observe sample sputum container";
  attrib Q867     format=F00280_. label="Sputum sent outside for TB testing";
  attrib Q868     format=F00281_. label="Record of outside sputum tests";
  attrib Q869     format=F00282_. label="Record of outside sputum tests - observed";
  attrib Q870     format=F00283_. label="System for internal/ external QC for TB sputum smears";
  attrib Q871     format=F00284_. label="Type of quality control practice";
  attrib Q872     format=F00285_. label="Record of results from QC procedures for TB sputum";
  attrib Q873     format=F00286_. label="Record maintained for IQC/EQC/Both";
  attrib Q880     format=F00287_. label="Perform diagnostic X-rays";
  attrib Q881A_1  format=F00288_. label="Equipment available(1)";
  attrib Q881A_2  format=F00288_. label="Equipment available(2)";
  attrib Q881A_3  format=F00288_. label="Equipment available(3)";
  attrib Q881A_4  format=F00288_. label="Equipment available(4)";
  attrib Q881B_1  format=F00289_. label="Equipment working order(1)";
  attrib Q881B_2  format=F00289_. label="Equipment working order(2)";
  attrib Q881B_3  format=F00289_. label="Equipment working order(3)";
  attrib Q881B_4  format=F00289_. label="Equipment working order(4)";
  attrib Q901_01  format=F00290_. label="Antibiotics drugs(1)";
  attrib Q901_02  format=F00290_. label="Antibiotics drugs(2)";
  attrib Q901_03  format=F00290_. label="Antibiotics drugs(3)";
  attrib Q901_04  format=F00290_. label="Antibiotics drugs(4)";
  attrib Q901_05  format=F00290_. label="Antibiotics drugs(5)";
  attrib Q901_06  format=F00290_. label="Antibiotics drugs(6)";
  attrib Q901_07  format=F00290_. label="Antibiotics drugs(7)";
  attrib Q901_08  format=F00290_. label="Antibiotics drugs(8)";
  attrib Q901_09  format=F00290_. label="Antibiotics drugs(9)";
  attrib Q901_10  format=F00290_. label="Antibiotics drugs(10)";
  attrib Q901_11  format=F00290_. label="Antibiotics drugs(11)";
  attrib Q901_12  format=F00290_. label="Antibiotics drugs(12)";
  attrib Q901_13  format=F00290_. label="Antibiotics drugs(13)";
  attrib Q901_14  format=F00290_. label="Antibiotics drugs(14)";
  attrib Q901_15  format=F00290_. label="Antibiotics drugs(15)";
  attrib Q901_16  format=F00290_. label="Antibiotics drugs(16)";
  attrib Q901_17  format=F00290_. label="Antibiotics drugs(17)";
  attrib Q901_18  format=F00290_. label="Antibiotics drugs(18)";
  attrib Q901_19  format=F00290_. label="Antibiotics drugs(19)";
  attrib Q901_20  format=F00290_. label="Antibiotics drugs(20)";
  attrib Q901_21  format=F00290_. label="Antibiotics drugs(21)";
  attrib Q901_22  format=F00290_. label="Antibiotics drugs(22)";
  attrib Q901_23  format=F00290_. label="Antibiotics drugs(23)";
  attrib Q901_24  format=F00290_. label="Antibiotics drugs(24)";
  attrib Q902_1   format=F00291_. label="Worm infestation drugs(1)";
  attrib Q902_2   format=F00291_. label="Worm infestation drugs(2)";
  attrib Q903_01  format=F00292_. label="Drugs for Non communicable diseases(1)";
  attrib Q903_02  format=F00292_. label="Drugs for Non communicable diseases(2)";
  attrib Q903_03  format=F00292_. label="Drugs for Non communicable diseases(3)";
  attrib Q903_04  format=F00292_. label="Drugs for Non communicable diseases(4)";
  attrib Q903_05  format=F00292_. label="Drugs for Non communicable diseases(5)";
  attrib Q903_06  format=F00292_. label="Drugs for Non communicable diseases(6)";
  attrib Q903_07  format=F00292_. label="Drugs for Non communicable diseases(7)";
  attrib Q903_08  format=F00292_. label="Drugs for Non communicable diseases(8)";
  attrib Q903_09  format=F00292_. label="Drugs for Non communicable diseases(9)";
  attrib Q903_10  format=F00292_. label="Drugs for Non communicable diseases(10)";
  attrib Q903_11  format=F00292_. label="Drugs for Non communicable diseases(11)";
  attrib Q903_12  format=F00292_. label="Drugs for Non communicable diseases(12)";
  attrib Q903_13  format=F00292_. label="Drugs for Non communicable diseases(13)";
  attrib Q903_14  format=F00292_. label="Drugs for Non communicable diseases(14)";
  attrib Q903_15  format=F00292_. label="Drugs for Non communicable diseases(15)";
  attrib Q903_16  format=F00292_. label="Drugs for Non communicable diseases(16)";
  attrib Q903_17  format=F00292_. label="Drugs for Non communicable diseases(17)";
  attrib Q903_18  format=F00292_. label="Drugs for Non communicable diseases(18)";
  attrib Q903_19  format=F00292_. label="Drugs for Non communicable diseases(19)";
  attrib Q903_20  format=F00292_. label="Drugs for Non communicable diseases(20)";
  attrib Q903_21  format=F00292_. label="Drugs for Non communicable diseases(21)";
  attrib Q903_22  format=F00292_. label="Drugs for Non communicable diseases(22)";
  attrib Q903_23  format=F00292_. label="Drugs for Non communicable diseases(23)";
  attrib Q903_24  format=F00292_. label="Drugs for Non communicable diseases(24)";
  attrib Q903_25  format=F00292_. label="Drugs for Non communicable diseases(25)";
  attrib Q903_26  format=F00292_. label="Drugs for Non communicable diseases(26)";
  attrib Q904_1   format=F00293_. label="Anti-fungal medicines(1)";
  attrib Q904_2   format=F00293_. label="Anti-fungal medicines(2)";
  attrib Q904_3   format=F00293_. label="Anti-fungal medicines(3)";
  attrib Q904_4   format=F00293_. label="Anti-fungal medicines(4)";
  attrib Q904_5   format=F00293_. label="Anti-fungal medicines(5)";
  attrib Q905_01  format=F00294_. label="Anti-malarial medicines(1)";
  attrib Q905_02  format=F00294_. label="Anti-malarial medicines(2)";
  attrib Q905_03  format=F00294_. label="Anti-malarial medicines(3)";
  attrib Q905_04  format=F00294_. label="Anti-malarial medicines(4)";
  attrib Q905_05  format=F00294_. label="Anti-malarial medicines(5)";
  attrib Q905_06  format=F00294_. label="Anti-malarial medicines(6)";
  attrib Q905_07  format=F00294_. label="Anti-malarial medicines(7)";
  attrib Q905_08  format=F00294_. label="Anti-malarial medicines(8)";
  attrib Q905_09  format=F00294_. label="Anti-malarial medicines(9)";
  attrib Q905_10  format=F00294_. label="Anti-malarial medicines(10)";
  attrib Q906_01  format=F00295_. label="Maternal and child health drugs(1)";
  attrib Q906_02  format=F00295_. label="Maternal and child health drugs(2)";
  attrib Q906_03  format=F00295_. label="Maternal and child health drugs(3)";
  attrib Q906_04  format=F00295_. label="Maternal and child health drugs(4)";
  attrib Q906_05  format=F00295_. label="Maternal and child health drugs(5)";
  attrib Q906_06  format=F00295_. label="Maternal and child health drugs(6)";
  attrib Q906_07  format=F00295_. label="Maternal and child health drugs(7)";
  attrib Q906_08  format=F00295_. label="Maternal and child health drugs(8)";
  attrib Q906_09  format=F00295_. label="Maternal and child health drugs(9)";
  attrib Q906_10  format=F00295_. label="Maternal and child health drugs(10)";
  attrib Q906_11  format=F00295_. label="Maternal and child health drugs(11)";
  attrib Q906C_01 format=F00296_. label="rupture de stock dans les 7 derniers jours(1)";
  attrib Q906C_02 format=F00296_. label="rupture de stock dans les 7 derniers jours(2)";
  attrib Q906C_03 format=F00296_. label="rupture de stock dans les 7 derniers jours(3)";
  attrib Q906C_04 format=F00296_. label="rupture de stock dans les 7 derniers jours(4)";
  attrib Q906C_05 format=F00296_. label="rupture de stock dans les 7 derniers jours(5)";
  attrib Q906C_06 format=F00296_. label="rupture de stock dans les 7 derniers jours(6)";
  attrib Q906C_07 format=F00296_. label="rupture de stock dans les 7 derniers jours(7)";
  attrib Q906C_08 format=F00296_. label="rupture de stock dans les 7 derniers jours(8)";
  attrib Q906C_09 format=F00296_. label="rupture de stock dans les 7 derniers jours(9)";
  attrib Q906C_10 format=F00296_. label="rupture de stock dans les 7 derniers jours(10)";
  attrib Q906C_11 format=F00296_. label="rupture de stock dans les 7 derniers jours(11)";
  attrib Q907_1   format=F00297_. label="Intravenous fluids(1)";
  attrib Q907_2   format=F00297_. label="Intravenous fluids(2)";
  attrib Q907_3   format=F00297_. label="Intravenous fluids(3)";
  attrib Q908_1   format=F00298_. label="Fever reducing and pain medicines(1)";
  attrib Q908_2   format=F00298_. label="Fever reducing and pain medicines(2)";
  attrib Q908_3   format=F00298_. label="Fever reducing and pain medicines(3)";
  attrib Q909_1   format=F00299_. label="Storage condition: Antibiotics & general medicines(1)";
  attrib Q909_2   format=F00299_. label="Storage condition: Antibiotics & general medicines(2)";
  attrib Q909_3   format=F00299_. label="Storage condition: Antibiotics & general medicines(3)";
  attrib Q909_4   format=F00299_. label="Storage condition: Antibiotics & general medicines(4)";
  attrib Q909_5   format=F00299_. label="Storage condition: Antibiotics & general medicines(5)";
  attrib Q910     format=F00300_. label="Medicines organized (FIFO) - Antibiotics & general";
  attrib Q911     format=F00301_. label="System used to monitor amount of meds received, issued, present each day  - Antibiotics & general";
  attrib Q912_1   format=F00302_. label="Supply items(1)";
  attrib Q912_2   format=F00302_. label="Supply items(2)";
  attrib Q912_3   format=F00302_. label="Supply items(3)";
  attrib Q912_4   format=F00302_. label="Supply items(4)";
  attrib Q912_5   format=F00302_. label="Supply items(5)";
  attrib Q912_6   format=F00302_. label="Supply items(6)";
  attrib Q912_7   format=F00302_. label="Supply items(7)";
  attrib Q912_8   format=F00302_. label="Supply items(8)";
  attrib C901_1   format=F00303_. label="Antibiotics drugs(1)";
  attrib C901_2   format=F00303_. label="Antibiotics drugs(2)";
  attrib C901_3   format=F00303_. label="Antibiotics drugs(3)";
  attrib C902_1   format=F00304_. label="Worm infestation drugs(1)";
  attrib C902_2   format=F00304_. label="Worm infestation drugs(2)";
  attrib C905_1   format=F00305_. label="Anti-malarial medicines(1)";
  attrib C905_2   format=F00305_. label="Anti-malarial medicines(2)";
  attrib C905_3   format=F00305_. label="Anti-malarial medicines(3)";
  attrib C905_4   format=F00305_. label="Anti-malarial medicines(4)";
  attrib C906_01  format=F00306_. label="Maternal and child health drugs(1)";
  attrib C906_02  format=F00306_. label="Maternal and child health drugs(2)";
  attrib C906_03  format=F00306_. label="Maternal and child health drugs(3)";
  attrib C906_04  format=F00306_. label="Maternal and child health drugs(4)";
  attrib C906_05  format=F00306_. label="Maternal and child health drugs(5)";
  attrib C906_06  format=F00306_. label="Maternal and child health drugs(6)";
  attrib C906_07  format=F00306_. label="Maternal and child health drugs(7)";
  attrib C906_08  format=F00306_. label="Maternal and child health drugs(8)";
  attrib C906_09  format=F00306_. label="Maternal and child health drugs(9)";
  attrib C906_10  format=F00306_. label="Maternal and child health drugs(10)";
  attrib C906_11  format=F00306_. label="Maternal and child health drugs(11)";
  attrib C906_12  format=F00306_. label="Maternal and child health drugs(12)";
  attrib C906_13  format=F00306_. label="Maternal and child health drugs(13)";
  attrib C906_14  format=F00306_. label="Maternal and child health drugs(14)";
  attrib C906_15  format=F00306_. label="Maternal and child health drugs(15)";
  attrib C906_16  format=F00306_. label="Maternal and child health drugs(16)";
  attrib C908_1   format=F00307_. label="Fever reducing and pain medicines(1)";
  attrib C908_2   format=F00307_. label="Fever reducing and pain medicines(2)";
  attrib C908_3   format=F00307_. label="Fever reducing and pain medicines(3)";
  attrib Q921_01  format=F00308_. label="Contraceptives(1)";
  attrib Q921_02  format=F00308_. label="Contraceptives(2)";
  attrib Q921_03  format=F00308_. label="Contraceptives(3)";
  attrib Q921_04  format=F00308_. label="Contraceptives(4)";
  attrib Q921_05  format=F00308_. label="Contraceptives(5)";
  attrib Q921_06  format=F00308_. label="Contraceptives(6)";
  attrib Q921_07  format=F00308_. label="Contraceptives(7)";
  attrib Q921_08  format=F00308_. label="Contraceptives(8)";
  attrib Q921_09  format=F00308_. label="Contraceptives(9)";
  attrib Q921_10  format=F00308_. label="Contraceptives(10)";
  attrib Q922_1   format=F00309_. label="Storage conditions of contraceptives(1)";
  attrib Q922_2   format=F00309_. label="Storage conditions of contraceptives(2)";
  attrib Q922_3   format=F00309_. label="Storage conditions of contraceptives(3)";
  attrib Q922_4   format=F00309_. label="Storage conditions of contraceptives(4)";
  attrib Q922_5   format=F00309_. label="Storage conditions of contraceptives(5)";
  attrib Q923     format=F00310_. label="Medicines organized (FIFO) - conraceptive";
  attrib Q924     format=F00311_. label="System used to monitor amount of meds received, issued, present each day - contraceptive";
  attrib Q931_01  format=F00312_. label="Anti TB drugs(1)";
  attrib Q931_02  format=F00312_. label="Anti TB drugs(2)";
  attrib Q931_03  format=F00312_. label="Anti TB drugs(3)";
  attrib Q931_04  format=F00312_. label="Anti TB drugs(4)";
  attrib Q931_05  format=F00312_. label="Anti TB drugs(5)";
  attrib Q931_06  format=F00312_. label="Anti TB drugs(6)";
  attrib Q931_07  format=F00312_. label="Anti TB drugs(7)";
  attrib Q931_08  format=F00312_. label="Anti TB drugs(8)";
  attrib Q931_09  format=F00312_. label="Anti TB drugs(9)";
  attrib Q931_10  format=F00312_. label="Anti TB drugs(10)";
  attrib Q932_1   format=F00313_. label="Storage condition: Anti TB medicines(1)";
  attrib Q932_2   format=F00313_. label="Storage condition: Anti TB medicines(2)";
  attrib Q932_3   format=F00313_. label="Storage condition: Anti TB medicines(3)";
  attrib Q932_4   format=F00313_. label="Storage condition: Anti TB medicines(4)";
  attrib Q932_5   format=F00313_. label="Storage condition: Anti TB medicines(5)";
  attrib Q933     format=F00314_. label="Medicines organized (FIFO) - Anti TB";
  attrib Q934     format=F00315_. label="System used to monitor amount of meds received, issued, present each day - Anti TB";
  attrib Q941_1   format=F00316_. label="NRTIs (first line ARV's for adults)(1)";
  attrib Q941_2   format=F00316_. label="NRTIs (first line ARV's for adults)(2)";
  attrib Q941_3   format=F00316_. label="NRTIs (first line ARV's for adults)(3)";
  attrib Q941_4   format=F00316_. label="NRTIs (first line ARV's for adults)(4)";
  attrib Q941_5   format=F00316_. label="NRTIs (first line ARV's for adults)(5)";
  attrib Q942_1   format=F00317_. label="NNRTIs (first line ARV's for children)(1)";
  attrib Q942_2   format=F00317_. label="NNRTIs (first line ARV's for children)(2)";
  attrib Q942_3   format=F00317_. label="NNRTIs (first line ARV's for children)(3)";
  attrib Q942_4   format=F00317_. label="NNRTIs (first line ARV's for children)(4)";
  attrib Q942_5   format=F00317_. label="NNRTIs (first line ARV's for children)(5)";
  attrib Q942_6   format=F00317_. label="NNRTIs (first line ARV's for children)(6)";
  attrib Q942_7   format=F00317_. label="NNRTIs (first line ARV's for children)(7)";
  attrib Q943_1   format=F00318_. label="Second line ARV's for adults(1)";
  attrib Q943_2   format=F00318_. label="Second line ARV's for adults(2)";
  attrib Q943_3   format=F00318_. label="Second line ARV's for adults(3)";
  attrib Q943_4   format=F00318_. label="Second line ARV's for adults(4)";
  attrib Q943_5   format=F00318_. label="Second line ARV's for adults(5)";
  attrib Q945_1   format=F00319_. label="Storage conditions of ARVs(1)";
  attrib Q945_2   format=F00319_. label="Storage conditions of ARVs(2)";
  attrib Q945_3   format=F00319_. label="Storage conditions of ARVs(3)";
  attrib Q945_4   format=F00319_. label="Storage conditions of ARVs(4)";
  attrib Q945_5   format=F00319_. label="Storage conditions of ARVs(5)";
  attrib Q946     format=F00320_. label="Medicines organized (FIFO) - ARVs";
  attrib Q947     format=F00321_. label="System used to monitor amount of meds received, issued, present each day - ARVs";
  attrib Q1001A_1 label="Days/month immunization is available at facility(1)";
  attrib Q1001A_2 label="Days/month immunization is available at facility(2)";
  attrib Q1001A_3 label="Days/month immunization is available at facility(3)";
  attrib Q1001A_4 label="Days/month immunization is available at facility(4)";
  attrib Q1001A_5 label="Days/month immunization is available at facility(5)";
  attrib Q1001A_6 label="Days/month immunization is available at facility(6)";
  attrib Q1001A_7 label="Days/month immunization is available at facility(7)";
  attrib Q1001A_8 label="Days/month immunization is available at facility(8)";
  attrib Q1001A_9 label="Days/month immunization is available at facility(9)";
  attrib Q1001B_1 label="Days/month immunization is available through outreach(1)";
  attrib Q1001B_2 label="Days/month immunization is available through outreach(2)";
  attrib Q1001B_3 label="Days/month immunization is available through outreach(3)";
  attrib Q1001B_4 label="Days/month immunization is available through outreach(4)";
  attrib Q1001B_5 label="Days/month immunization is available through outreach(5)";
  attrib Q1001B_6 label="Days/month immunization is available through outreach(6)";
  attrib Q1001B_7 label="Days/month immunization is available through outreach(7)";
  attrib Q1001B_8 label="Days/month immunization is available through outreach(8)";
  attrib Q1001B_9 label="Days/month immunization is available through outreach(9)";
  attrib Q1002    format=F00322_. label="National guidelines available";
  attrib Q1003    format=F00323_. label="Observe national guidelines available";
  attrib Q1004    format=F00324_. label="Other guidelines";
  attrib Q1005    format=F00325_. label="Observe other guidelines";
  attrib Q1006_1  format=F00326_. label="Items for immunization available(1)";
  attrib Q1006_2  format=F00326_. label="Items for immunization available(2)";
  attrib Q1006_3  format=F00326_. label="Items for immunization available(3)";
  attrib Q1007    format=F00327_. label="Routinely store vaccines";
  attrib Q1008    format=F00328_. label="Refrigerator available - observec";
  attrib Q1008A   format=F00329_. label="Type of refrigerator";
  attrib Q1009    format=F00330_. label="Cold-chain monitoring system";
  attrib Q1010    format=F00331_. label="Observe cold-chain monitoring system";
  attrib Q1011    format=F00332_. label="Temp record completed past 30 days";
  attrib Q1012_1  format=F00333_. label="Vaccines available(1)";
  attrib Q1012_2  format=F00333_. label="Vaccines available(2)";
  attrib Q1012_3  format=F00333_. label="Vaccines available(3)";
  attrib Q1012_4  format=F00333_. label="Vaccines available(4)";
  attrib Q1012_5  format=F00333_. label="Vaccines available(5)";
  attrib Q1012_6  format=F00333_. label="Vaccines available(6)";
  attrib Q1012_7  format=F00333_. label="Vaccines available(7)";
  attrib Q1012_8  format=F00333_. label="Vaccines available(8)";
  attrib Q1012_9  format=F00333_. label="Vaccines available(9)";
  attrib Q1013    format=F00334_. label="Temperature of refrigerator for vaccines";
  attrib Q1013A   format=F00335_. label="Maintenance register of refrigerator maintained";
  attrib Q1013B   format=F00336_. label="Maintenance register of refrigerator observerd";
  attrib Q1014    format=F00337_. label="Vaccine carriers";
  attrib Q1014A   format=F00338_. label="Quel type de glaciÃ¨re avez-vous?";
  attrib Q1015    format=F00339_. label="Ice packs for vaccine carriers";
  attrib Q1050    format=F00340_. label="Data is already recorded for already assessed room/area";
  attrib Q1051_01 format=F00341_. label="Precaution and condition in client examination room(1)";
  attrib Q1051_02 format=F00341_. label="Precaution and condition in client examination room(2)";
  attrib Q1051_03 format=F00341_. label="Precaution and condition in client examination room(3)";
  attrib Q1051_04 format=F00341_. label="Precaution and condition in client examination room(4)";
  attrib Q1051_05 format=F00341_. label="Precaution and condition in client examination room(5)";
  attrib Q1051_06 format=F00341_. label="Precaution and condition in client examination room(6)";
  attrib Q1051_07 format=F00341_. label="Precaution and condition in client examination room(7)";
  attrib Q1051_08 format=F00341_. label="Precaution and condition in client examination room(8)";
  attrib Q1051_09 format=F00341_. label="Precaution and condition in client examination room(9)";
  attrib Q1051_10 format=F00341_. label="Precaution and condition in client examination room(10)";
  attrib Q1051_11 format=F00341_. label="Precaution and condition in client examination room(11)";
  attrib Q1051_12 format=F00341_. label="Precaution and condition in client examination room(12)";
  attrib Q1051_13 format=F00341_. label="Precaution and condition in client examination room(13)";
  attrib Q1052    format=F00342_. label="Setting of examination room";
  attrib Q1101A   label="Days/month child growth monitoring is available at facility";
  attrib Q1101B   label="Days/month immunization is available through outreach";
  attrib Q1102    format=F00343_. label="Guidelines for growth monitoring available";
  attrib Q1103    format=F00344_. label="Guidelines for growth monitoring observed";
  attrib Q1104A_1 format=F00345_. label="Items available(1)";
  attrib Q1104A_2 format=F00345_. label="Items available(2)";
  attrib Q1104A_3 format=F00345_. label="Items available(3)";
  attrib Q1104A_4 format=F00345_. label="Items available(4)";
  attrib Q1104A_5 format=F00345_. label="Items available(5)";
  attrib Q1104A_6 format=F00345_. label="Items available(6)";
  attrib Q1104B_1 format=F00346_. label="Items functioning(1)";
  attrib Q1104B_2 format=F00346_. label="Items functioning(2)";
  attrib Q1104B_3 format=F00346_. label="Items functioning(3)";
  attrib Q1104B_4 format=F00346_. label="Items functioning(4)";
  attrib Q1104B_5 format=F00346_. label="Items functioning(5)";
  attrib Q1104B_6 format=F00346_. label="Items functioning(6)";
  attrib Q1201A   label="Days/month child curative care is available at facility";
  attrib Q1201B   label="Days/month child curative care is available through outreach";
  attrib Q1202_1  format=F00347_. label="Services provided at facility(1)";
  attrib Q1202_2  format=F00347_. label="Services provided at facility(2)";
  attrib Q1202_3  format=F00347_. label="Services provided at facility(3)";
  attrib Q1202_4  format=F00347_. label="Services provided at facility(4)";
  attrib Q1202_5  format=F00347_. label="Services provided at facility(5)";
  attrib Q1203    format=F00348_. label="Follow IMCI guidelines";
  attrib Q1204    format=F00349_. label="IMCI guidelines available";
  attrib Q1205    format=F00350_. label="IMCI guidelines observed";
  attrib Q1206    format=F00351_. label="Other guidelines available";
  attrib Q1207    format=F00352_. label="Other guidelines observed";
  attrib Q1208    format=F00353_. label="Routine sick child observations before consult";
  attrib Q1209_1  format=F00354_. label="Routine activities(1)";
  attrib Q1209_2  format=F00354_. label="Routine activities(2)";
  attrib Q1209_3  format=F00354_. label="Routine activities(3)";
  attrib Q1209_4  format=F00354_. label="Routine activities(4)";
  attrib Q1209_5  format=F00354_. label="Routine activities(5)";
  attrib Q1209_6  format=F00354_. label="Routine activities(6)";
  attrib Q1209_7  format=F00354_. label="Routine activities(7)";
  attrib Q1210A_01 format=F00355_. label="Items available(1)";
  attrib Q1210A_02 format=F00355_. label="Items available(2)";
  attrib Q1210A_03 format=F00355_. label="Items available(3)";
  attrib Q1210A_04 format=F00355_. label="Items available(4)";
  attrib Q1210A_05 format=F00355_. label="Items available(5)";
  attrib Q1210A_06 format=F00355_. label="Items available(6)";
  attrib Q1210A_07 format=F00355_. label="Items available(7)";
  attrib Q1210A_08 format=F00355_. label="Items available(8)";
  attrib Q1210A_09 format=F00355_. label="Items available(9)";
  attrib Q1210A_10 format=F00355_. label="Items available(10)";
  attrib Q1210A_11 format=F00355_. label="Items available(11)";
  attrib Q1210A_12 format=F00355_. label="Items available(12)";
  attrib Q1210B_01 format=F00356_. label="Items functioning(1)";
  attrib Q1210B_02 format=F00356_. label="Items functioning(2)";
  attrib Q1210B_03 format=F00356_. label="Items functioning(3)";
  attrib Q1210B_04 format=F00356_. label="Items functioning(4)";
  attrib Q1210B_05 format=F00356_. label="Items functioning(5)";
  attrib Q1210B_06 format=F00356_. label="Items functioning(6)";
  attrib Q1210B_07 format=F00356_. label="Items functioning(7)";
  attrib Q1210B_08 format=F00356_. label="Items functioning(8)";
  attrib Q1210B_09 format=F00356_. label="Items functioning(9)";
  attrib Q1210B_10 format=F00356_. label="Items functioning(10)";
  attrib Q1210B_11 format=F00356_. label="Items functioning(11)";
  attrib Q1210B_12 format=F00356_. label="Items functioning(12)";
  attrib Q1211_1  format=F00357_. label="Visual aids for sick child service(1)";
  attrib Q1211_2  format=F00357_. label="Visual aids for sick child service(2)";
  attrib Q1211_3  format=F00357_. label="Visual aids for sick child service(3)";
  attrib Q1212    format=F00358_. label="Individual health cards";
  attrib Q1213    format=F00359_. label="Observe Individual health cards";
  attrib Q1250    format=F00360_. label="Data is already recorded for already assessed room/area";
  attrib Q1251_01 format=F00361_. label="Precaution and condition in client examination room(1)";
  attrib Q1251_02 format=F00361_. label="Precaution and condition in client examination room(2)";
  attrib Q1251_03 format=F00361_. label="Precaution and condition in client examination room(3)";
  attrib Q1251_04 format=F00361_. label="Precaution and condition in client examination room(4)";
  attrib Q1251_05 format=F00361_. label="Precaution and condition in client examination room(5)";
  attrib Q1251_06 format=F00361_. label="Precaution and condition in client examination room(6)";
  attrib Q1251_07 format=F00361_. label="Precaution and condition in client examination room(7)";
  attrib Q1251_08 format=F00361_. label="Precaution and condition in client examination room(8)";
  attrib Q1251_09 format=F00361_. label="Precaution and condition in client examination room(9)";
  attrib Q1251_10 format=F00361_. label="Precaution and condition in client examination room(10)";
  attrib Q1251_11 format=F00361_. label="Precaution and condition in client examination room(11)";
  attrib Q1251_12 format=F00361_. label="Precaution and condition in client examination room(12)";
  attrib Q1251_13 format=F00361_. label="Precaution and condition in client examination room(13)";
  attrib Q1252    format=F00362_. label="Setting of examination room";
  attrib Q1301    label="Number of days family planning services offered at facility";
  attrib Q1302_01 format=F00363_. label="Family planning methods provided/counseled(1)";
  attrib Q1302_02 format=F00363_. label="Family planning methods provided/counseled(2)";
  attrib Q1302_03 format=F00363_. label="Family planning methods provided/counseled(3)";
  attrib Q1302_04 format=F00363_. label="Family planning methods provided/counseled(4)";
  attrib Q1302_05 format=F00363_. label="Family planning methods provided/counseled(5)";
  attrib Q1302_06 format=F00363_. label="Family planning methods provided/counseled(6)";
  attrib Q1302_07 format=F00363_. label="Family planning methods provided/counseled(7)";
  attrib Q1302_08 format=F00363_. label="Family planning methods provided/counseled(8)";
  attrib Q1302_09 format=F00363_. label="Family planning methods provided/counseled(9)";
  attrib Q1302_10 format=F00363_. label="Family planning methods provided/counseled(10)";
  attrib Q1302_11 format=F00363_. label="Family planning methods provided/counseled(11)";
  attrib Q1302_12 format=F00363_. label="Family planning methods provided/counseled(12)";
  attrib Q1302_13 format=F00363_. label="Family planning methods provided/counseled(13)";
  attrib Q1302_14 format=F00363_. label="Family planning methods provided/counseled(14)";
  attrib Q1303    format=F00364_. label="National FP guidelines";
  attrib Q1304    format=F00365_. label="Observe national FP guidelines";
  attrib Q1305    format=F00366_. label="Other guidelines";
  attrib Q1306    format=F00367_. label="Observe other guidelines";
  attrib Q1307    format=F00368_. label="Individual records/cards for clients";
  attrib Q1308    format=F00369_. label="Observe Individual records/cards for clients";
  attrib Q1308A   format=F00370_. label="Client medical cards kept in the facility";
  attrib Q1308B   format=F00371_. label="Empty individual medical card seen";
  attrib Q1308C   format=F00372_. label="Filing system to store client files by status";
  attrib Q1308D   format=F00373_. label="Filing system for client files seen";
  attrib Q1309    format=F00374_. label="Routine actions for clients";
  attrib Q1310_1  format=F00375_. label="Type of routine actions(1)";
  attrib Q1310_2  format=F00375_. label="Type of routine actions(2)";
  attrib Q1310_3  format=F00375_. label="Type of routine actions(3)";
  attrib Q1311    format=F00376_. label="FP providers routeinly dx/tx STIs";
  attrib Q1312    format=F00377_. label="FP providers conduct HIV testing";
  attrib Q1313    format=F00378_. label="Presence of HIV rapid testing kit";
  attrib Q1314A_01 format=F00379_. label="FP equipment available(1)";
  attrib Q1314A_02 format=F00379_. label="FP equipment available(2)";
  attrib Q1314A_03 format=F00379_. label="FP equipment available(3)";
  attrib Q1314A_04 format=F00379_. label="FP equipment available(4)";
  attrib Q1314A_05 format=F00379_. label="FP equipment available(5)";
  attrib Q1314A_06 format=F00379_. label="FP equipment available(6)";
  attrib Q1314A_07 format=F00379_. label="FP equipment available(7)";
  attrib Q1314A_08 format=F00379_. label="FP equipment available(8)";
  attrib Q1314A_09 format=F00379_. label="FP equipment available(9)";
  attrib Q1314A_10 format=F00379_. label="FP equipment available(10)";
  attrib Q1314B_01 format=F00380_. label="FP equipment functioning(1)";
  attrib Q1314B_02 format=F00380_. label="FP equipment functioning(2)";
  attrib Q1314B_03 format=F00380_. label="FP equipment functioning(3)";
  attrib Q1314B_04 format=F00380_. label="FP equipment functioning(4)";
  attrib Q1314B_05 format=F00380_. label="FP equipment functioning(5)";
  attrib Q1314B_06 format=F00380_. label="FP equipment functioning(6)";
  attrib Q1314B_07 format=F00380_. label="FP equipment functioning(7)";
  attrib Q1314B_08 format=F00380_. label="FP equipment functioning(8)";
  attrib Q1314B_09 format=F00380_. label="FP equipment functioning(9)";
  attrib Q1314B_10 format=F00380_. label="FP equipment functioning(10)";
  attrib Q1316_1  format=F00381_. label="IUD/implant equipment(1)";
  attrib Q1316_2  format=F00381_. label="IUD/implant equipment(2)";
  attrib Q1316_3  format=F00381_. label="IUD/implant equipment(3)";
  attrib Q1316_4  format=F00381_. label="IUD/implant equipment(4)";
  attrib Q1318_1  format=F00382_. label="IUD equipment(1)";
  attrib Q1318_2  format=F00382_. label="IUD equipment(2)";
  attrib Q1318_3  format=F00382_. label="IUD equipment(3)";
  attrib Q1318_4  format=F00382_. label="IUD equipment(4)";
  attrib Q1318_5  format=F00382_. label="IUD equipment(5)";
  attrib Q1318_6  format=F00382_. label="IUD equipment(6)";
  attrib Q1320_1  format=F00383_. label="Implant equipment(1)";
  attrib Q1320_2  format=F00383_. label="Implant equipment(2)";
  attrib Q1320_3  format=F00383_. label="Implant equipment(3)";
  attrib Q1320_4  format=F00383_. label="Implant equipment(4)";
  attrib Q1320_5  format=F00383_. label="Implant equipment(5)";
  attrib Q1320_6  format=F00383_. label="Implant equipment(6)";
  attrib Q1321    format=F00384_. label="Equipment placed in the FP services processed for re-use";
  attrib Q1322    length=$6 format=$F00385_. label="Final processing method used for FP equipment";
  attrib Q1350    format=F00386_. label="Data is already recorded for already assessed room/area";
  attrib Q1351_01 format=F00387_. label="Precaution and condition in client examination room(1)";
  attrib Q1351_02 format=F00387_. label="Precaution and condition in client examination room(2)";
  attrib Q1351_03 format=F00387_. label="Precaution and condition in client examination room(3)";
  attrib Q1351_04 format=F00387_. label="Precaution and condition in client examination room(4)";
  attrib Q1351_05 format=F00387_. label="Precaution and condition in client examination room(5)";
  attrib Q1351_06 format=F00387_. label="Precaution and condition in client examination room(6)";
  attrib Q1351_07 format=F00387_. label="Precaution and condition in client examination room(7)";
  attrib Q1351_08 format=F00387_. label="Precaution and condition in client examination room(8)";
  attrib Q1351_09 format=F00387_. label="Precaution and condition in client examination room(9)";
  attrib Q1351_10 format=F00387_. label="Precaution and condition in client examination room(10)";
  attrib Q1351_11 format=F00387_. label="Precaution and condition in client examination room(11)";
  attrib Q1351_12 format=F00387_. label="Precaution and condition in client examination room(12)";
  attrib Q1351_13 format=F00387_. label="Precaution and condition in client examination room(13)";
  attrib Q1352    format=F00388_. label="Setting of FP service room";
  attrib Q1401    label="Number of days ANC available in facility";
  attrib Q1402_1  format=F00389_. label="Services offered(1)";
  attrib Q1402_2  format=F00389_. label="Services offered(2)";
  attrib Q1402_3  format=F00389_. label="Services offered(3)";
  attrib Q1402_4  format=F00389_. label="Services offered(4)";
  attrib Q1404    format=F00390_. label="TT vaccine available all days";
  attrib Q1405    format=F00391_. label="Days/week TT available";
  attrib Q1406_1  format=F00392_. label="Tests offered(1)";
  attrib Q1406_2  format=F00392_. label="Tests offered(2)";
  attrib Q1406_3  format=F00392_. label="Tests offered(3)";
  attrib Q1406_4  format=F00392_. label="Tests offered(4)";
  attrib Q1406_5  format=F00392_. label="Tests offered(5)";
  attrib Q1407_1  format=F00393_. label="Counseling services offered(1)";
  attrib Q1407_2  format=F00393_. label="Counseling services offered(2)";
  attrib Q1407_3  format=F00393_. label="Counseling services offered(3)";
  attrib Q1407_4  format=F00393_. label="Counseling services offered(4)";
  attrib Q1407_5  format=F00393_. label="Counseling services offered(5)";
  attrib Q1407_6  format=F00393_. label="Counseling services offered(6)";
  attrib Q1407_7  format=F00393_. label="Counseling services offered(7)";
  attrib Q1407_8  format=F00393_. label="Counseling services offered(8)";
  attrib Q1407_9  format=F00393_. label="Counseling services offered(9)";
  attrib Q1408    format=F00394_. label="ANC providers routinely dx,tx STIs";
  attrib Q1409    format=F00395_. label="National ANC guidelines";
  attrib Q1410    format=F00396_. label="National ANC guidelines observed";
  attrib Q1411    format=F00397_. label="Other ANC guidelines";
  attrib Q1412    format=F00398_. label="Other ANC guidelines observed";
  attrib Q1413    format=F00399_. label="IPT guidelines";
  attrib Q1414    format=F00400_. label="IPT guidelines observed";
  attrib Q1415    format=F00401_. label="Visual aids for client education";
  attrib Q1416    format=F00402_. label="Visual aids for client education observed";
  attrib Q1417    format=F00403_. label="Individual cards maintained";
  attrib Q1418    format=F00404_. label="Individual cards maintained observed";
  attrib Q1419    format=F00405_. label="System of routine actions for clients";
  attrib Q1420_1  format=F00406_. label="Specific routine actions(1)";
  attrib Q1420_2  format=F00406_. label="Specific routine actions(2)";
  attrib Q1420_3  format=F00406_. label="Specific routine actions(3)";
  attrib Q1420_4  format=F00406_. label="Specific routine actions(4)";
  attrib Q1420_5  format=F00406_. label="Specific routine actions(5)";
  attrib Q1420_6  format=F00406_. label="Specific routine actions(6)";
  attrib Q1421A_1 format=F00407_. label="ANC equipment available(1)";
  attrib Q1421A_2 format=F00407_. label="ANC equipment available(2)";
  attrib Q1421A_3 format=F00407_. label="ANC equipment available(3)";
  attrib Q1421A_4 format=F00407_. label="ANC equipment available(4)";
  attrib Q1421A_5 format=F00407_. label="ANC equipment available(5)";
  attrib Q1421A_6 format=F00407_. label="ANC equipment available(6)";
  attrib Q1421A_7 format=F00407_. label="ANC equipment available(7)";
  attrib Q1421B_1 format=F00408_. label="ANC equipment functioning(1)";
  attrib Q1421B_2 format=F00408_. label="ANC equipment functioning(2)";
  attrib Q1421B_3 format=F00408_. label="ANC equipment functioning(3)";
  attrib Q1421B_4 format=F00408_. label="ANC equipment functioning(4)";
  attrib Q1421B_5 format=F00408_. label="ANC equipment functioning(5)";
  attrib Q1421B_6 format=F00408_. label="ANC equipment functioning(6)";
  attrib Q1421B_7 format=F00408_. label="ANC equipment functioning(7)";
  attrib Q1422_1  format=F00409_. label="Medicine available today(1)";
  attrib Q1422_2  format=F00409_. label="Medicine available today(2)";
  attrib Q1422_3  format=F00409_. label="Medicine available today(3)";
  attrib Q1422_4  format=F00409_. label="Medicine available today(4)";
  attrib Q1422_5  format=F00409_. label="Medicine available today(5)";
  attrib Q1450    format=F00410_. label="Data is already recorded for already assessed room/area";
  attrib Q1451_01 format=F00411_. label="Precaution and condition in client examination room(1)";
  attrib Q1451_02 format=F00411_. label="Precaution and condition in client examination room(2)";
  attrib Q1451_03 format=F00411_. label="Precaution and condition in client examination room(3)";
  attrib Q1451_04 format=F00411_. label="Precaution and condition in client examination room(4)";
  attrib Q1451_05 format=F00411_. label="Precaution and condition in client examination room(5)";
  attrib Q1451_06 format=F00411_. label="Precaution and condition in client examination room(6)";
  attrib Q1451_07 format=F00411_. label="Precaution and condition in client examination room(7)";
  attrib Q1451_08 format=F00411_. label="Precaution and condition in client examination room(8)";
  attrib Q1451_09 format=F00411_. label="Precaution and condition in client examination room(9)";
  attrib Q1451_10 format=F00411_. label="Precaution and condition in client examination room(10)";
  attrib Q1451_11 format=F00411_. label="Precaution and condition in client examination room(11)";
  attrib Q1451_12 format=F00411_. label="Precaution and condition in client examination room(12)";
  attrib Q1451_13 format=F00411_. label="Precaution and condition in client examination room(13)";
  attrib Q1452    format=F00412_. label="Setting of ANC service";
  attrib Q1501_1  format=F00413_. label="PMTCT services offered(1)";
  attrib Q1501_2  format=F00413_. label="PMTCT services offered(2)";
  attrib Q1501_3  format=F00413_. label="PMTCT services offered(3)";
  attrib Q1501_4  format=F00413_. label="PMTCT services offered(4)";
  attrib Q1501_5  format=F00413_. label="PMTCT services offered(5)";
  attrib Q1501_6  format=F00413_. label="PMTCT services offered(6)";
  attrib Q1501_7  format=F00413_. label="PMTCT services offered(7)";
  attrib Q1501_8  format=F00413_. label="PMTCT services offered(8)";
  attrib Q1503    format=F00414_. label="PMTCT site same location as ANC service";
  attrib Q1504    format=F00415_. label="HIV rapid test kit available";
  attrib Q1505    format=F00416_. label="Observation of HIV rapid test kit available";
  attrib Q1507    format=F00417_. label="Providers use filter paper to collect DBS";
  attrib Q1508    format=F00418_. label="Observation of filter paper to collect DBS";
  attrib Q1509    format=F00419_. label="National guidelines for PMTCT";
  attrib Q1510    format=F00420_. label="National guidelines for PMTCT observed";
  attrib Q1511    format=F00421_. label="Other guidelines for PMTCT";
  attrib Q1512    format=F00422_. label="Other guidelines for PMTCT observed";
  attrib Q1513    format=F00423_. label="Guidelines for infant/ young child feeding (IYCF) counseling";
  attrib Q1514    format=F00424_. label="Guidelines for infant/ young child feeding (IYCF) counseling";
  attrib Q1515    format=F00425_. label="Stock any ARVs for PMTCT in service area";
  attrib Q1516_01 format=F00426_. label="ARVs available at service site(1)";
  attrib Q1516_02 format=F00426_. label="ARVs available at service site(2)";
  attrib Q1516_03 format=F00426_. label="ARVs available at service site(3)";
  attrib Q1516_04 format=F00426_. label="ARVs available at service site(4)";
  attrib Q1516_05 format=F00426_. label="ARVs available at service site(5)";
  attrib Q1516_06 format=F00426_. label="ARVs available at service site(6)";
  attrib Q1516_07 format=F00426_. label="ARVs available at service site(7)";
  attrib Q1516_08 format=F00426_. label="ARVs available at service site(8)";
  attrib Q1516_09 format=F00426_. label="ARVs available at service site(9)";
  attrib Q1516_10 format=F00426_. label="ARVs available at service site(10)";
  attrib Q1516_11 format=F00426_. label="ARVs available at service site(11)";
  attrib Q1516_12 format=F00426_. label="ARVs available at service site(12)";
  attrib Q1550    format=F00427_. label="Data is already recorded for already assessed room/area";
  attrib Q1551_01 format=F00428_. label="Precaution and condition in client examination room(1)";
  attrib Q1551_02 format=F00428_. label="Precaution and condition in client examination room(2)";
  attrib Q1551_03 format=F00428_. label="Precaution and condition in client examination room(3)";
  attrib Q1551_04 format=F00428_. label="Precaution and condition in client examination room(4)";
  attrib Q1551_05 format=F00428_. label="Precaution and condition in client examination room(5)";
  attrib Q1551_06 format=F00428_. label="Precaution and condition in client examination room(6)";
  attrib Q1551_07 format=F00428_. label="Precaution and condition in client examination room(7)";
  attrib Q1551_08 format=F00428_. label="Precaution and condition in client examination room(8)";
  attrib Q1551_09 format=F00428_. label="Precaution and condition in client examination room(9)";
  attrib Q1551_10 format=F00428_. label="Precaution and condition in client examination room(10)";
  attrib Q1551_11 format=F00428_. label="Precaution and condition in client examination room(11)";
  attrib Q1551_12 format=F00428_. label="Precaution and condition in client examination room(12)";
  attrib Q1551_13 format=F00428_. label="Precaution and condition in client examination room(13)";
  attrib Q1552    format=F00429_. label="Setting of PMTCT service site";
  attrib Q1601    format=F00430_. label="24-hour staff in facility or on-call";
  attrib Q1602    format=F00431_. label="Duty schedule/call list for 24-hour staff";
  attrib Q1603    format=F00432_. label="Duty schedule/call list for 24-hour staff observed";
  attrib Q1604A_1 format=F00433_. label="Interventions ever carried out in facilty(1)";
  attrib Q1604A_2 format=F00433_. label="Interventions ever carried out in facilty(2)";
  attrib Q1604A_3 format=F00433_. label="Interventions ever carried out in facilty(3)";
  attrib Q1604A_4 format=F00433_. label="Interventions ever carried out in facilty(4)";
  attrib Q1604A_5 format=F00433_. label="Interventions ever carried out in facilty(5)";
  attrib Q1604A_6 format=F00433_. label="Interventions ever carried out in facilty(6)";
  attrib Q1604A_7 format=F00433_. label="Interventions ever carried out in facilty(7)";
  attrib Q1604A_8 format=F00433_. label="Interventions ever carried out in facilty(8)";
  attrib Q1604B_1 format=F00434_. label="Interventions carried out in past 3 months(1)";
  attrib Q1604B_2 format=F00434_. label="Interventions carried out in past 3 months(2)";
  attrib Q1604B_3 format=F00434_. label="Interventions carried out in past 3 months(3)";
  attrib Q1604B_4 format=F00434_. label="Interventions carried out in past 3 months(4)";
  attrib Q1604B_5 format=F00434_. label="Interventions carried out in past 3 months(5)";
  attrib Q1604B_6 format=F00434_. label="Interventions carried out in past 3 months(6)";
  attrib Q1604B_7 format=F00434_. label="Interventions carried out in past 3 months(7)";
  attrib Q1604B_8 format=F00434_. label="Interventions carried out in past 3 months(8)";
  attrib Q1605    format=F00435_. label="National gujidelines for IMPAC";
  attrib Q1606    format=F00436_. label="National gujidelines for IMPAC observed";
  attrib Q1607    format=F00437_. label="National guidelines for CEmOC";
  attrib Q1608    format=F00438_. label="National guidelines for CEmOC observed";
  attrib Q1609    format=F00439_. label="Guidelines for management of pre-term labor";
  attrib Q1610    format=F00440_. label="Guidelines for management of pre-term labor observed";
  attrib Q1611    format=F00441_. label="Practice Kangaroo Mother Care in low-weight babies";
  attrib Q1612    format=F00442_. label="Separate room or space for Kangaroo Mother Care of intergrated in main postnatal ward";
  attrib Q1613    format=F00443_. label="Use partographs to monitor labor/delivery";
  attrib Q1614    format=F00444_. label="Routinely or selectively use partograph";
  attrib Q1615    format=F00445_. label="Number of dedicated maternity beds";
  attrib Q1616    format=F00446_. label="Number of dedicated delivery beds";
  attrib Q1617    format=F00447_. label="Regular reviews of maternal or newborn deaths or 'near misses'";
  attrib Q1618    format=F00448_. label="Reviews of mother, newborn or both";
  attrib Q1619    format=F00449_. label="Frequency of maternal deaths/near-misses reviews";
  attrib Q1621    format=F00450_. label="Frequency of newborn deaths/near misses reviews";
  attrib Q1622A_01 format=F00451_. label="Delivery area equipment available(1)";
  attrib Q1622A_02 format=F00451_. label="Delivery area equipment available(2)";
  attrib Q1622A_03 format=F00451_. label="Delivery area equipment available(3)";
  attrib Q1622A_04 format=F00451_. label="Delivery area equipment available(4)";
  attrib Q1622A_05 format=F00451_. label="Delivery area equipment available(5)";
  attrib Q1622A_06 format=F00451_. label="Delivery area equipment available(6)";
  attrib Q1622A_07 format=F00451_. label="Delivery area equipment available(7)";
  attrib Q1622A_08 format=F00451_. label="Delivery area equipment available(8)";
  attrib Q1622A_09 format=F00451_. label="Delivery area equipment available(9)";
  attrib Q1622A_10 format=F00451_. label="Delivery area equipment available(10)";
  attrib Q1622A_11 format=F00451_. label="Delivery area equipment available(11)";
  attrib Q1622A_12 format=F00451_. label="Delivery area equipment available(12)";
  attrib Q1622A_13 format=F00451_. label="Delivery area equipment available(13)";
  attrib Q1622A_14 format=F00451_. label="Delivery area equipment available(14)";
  attrib Q1622B_01 format=F00452_. label="Equipment functioning(1)";
  attrib Q1622B_02 format=F00452_. label="Equipment functioning(2)";
  attrib Q1622B_03 format=F00452_. label="Equipment functioning(3)";
  attrib Q1622B_04 format=F00452_. label="Equipment functioning(4)";
  attrib Q1622B_05 format=F00452_. label="Equipment functioning(5)";
  attrib Q1622B_06 format=F00452_. label="Equipment functioning(6)";
  attrib Q1622B_07 format=F00452_. label="Equipment functioning(7)";
  attrib Q1622B_08 format=F00452_. label="Equipment functioning(8)";
  attrib Q1622B_09 format=F00452_. label="Equipment functioning(9)";
  attrib Q1622B_10 format=F00452_. label="Equipment functioning(10)";
  attrib Q1622B_11 format=F00452_. label="Equipment functioning(11)";
  attrib Q1622B_12 format=F00452_. label="Equipment functioning(12)";
  attrib Q1622B_13 format=F00452_. label="Equipment functioning(13)";
  attrib Q1622B_14 format=F00452_. label="Equipment functioning(14)";
  attrib Q1623_01 format=F00453_. label="Other delivery items available(1)";
  attrib Q1623_02 format=F00453_. label="Other delivery items available(2)";
  attrib Q1623_03 format=F00453_. label="Other delivery items available(3)";
  attrib Q1623_04 format=F00453_. label="Other delivery items available(4)";
  attrib Q1623_05 format=F00453_. label="Other delivery items available(5)";
  attrib Q1623_06 format=F00453_. label="Other delivery items available(6)";
  attrib Q1623_07 format=F00453_. label="Other delivery items available(7)";
  attrib Q1623_08 format=F00453_. label="Other delivery items available(8)";
  attrib Q1623_09 format=F00453_. label="Other delivery items available(9)";
  attrib Q1623_10 format=F00453_. label="Other delivery items available(10)";
  attrib Q1623_11 format=F00453_. label="Other delivery items available(11)";
  attrib Q1623_12 format=F00453_. label="Other delivery items available(12)";
  attrib Q1624_01 format=F00454_. label="Routine newborn activities(1)";
  attrib Q1624_02 format=F00454_. label="Routine newborn activities(2)";
  attrib Q1624_03 format=F00454_. label="Routine newborn activities(3)";
  attrib Q1624_04 format=F00454_. label="Routine newborn activities(4)";
  attrib Q1624_05 format=F00454_. label="Routine newborn activities(5)";
  attrib Q1624_06 format=F00454_. label="Routine newborn activities(6)";
  attrib Q1624_07 format=F00454_. label="Routine newborn activities(7)";
  attrib Q1624_08 format=F00454_. label="Routine newborn activities(8)";
  attrib Q1624_09 format=F00454_. label="Routine newborn activities(9)";
  attrib Q1624_10 format=F00454_. label="Routine newborn activities(10)";
  attrib Q1624_11 format=F00454_. label="Routine newborn activities(11)";
  attrib Q1624_12 format=F00454_. label="Routine newborn activities(12)";
  attrib Q1624_13 format=F00454_. label="Routine newborn activities(13)";
  attrib Q1624_14 format=F00454_. label="Routine newborn activities(14)";
  attrib Q1624_15 format=F00454_. label="Routine newborn activities(15)";
  attrib Q1625_1  format=F00455_. label="Medicines in delivery area(1)";
  attrib Q1625_2  format=F00455_. label="Medicines in delivery area(2)";
  attrib Q1625_3  format=F00455_. label="Medicines in delivery area(3)";
  attrib Q1625_4  format=F00455_. label="Medicines in delivery area(4)";
  attrib Q1625_5  format=F00455_. label="Medicines in delivery area(5)";
  attrib Q1625_6  format=F00455_. label="Medicines in delivery area(6)";
  attrib Q1625_7  format=F00455_. label="Medicines in delivery area(7)";
  attrib Q1625_8  format=F00455_. label="Medicines in delivery area(8)";
  attrib Q1625_9  format=F00455_. label="Medicines in delivery area(9)";
  attrib Q1626    format=F00456_. label="Provide any PMTCT service for women come in to deliver";
  attrib Q1627    format=F00457_. label="Providers of delivery services conduct HIV testing from service site";
  attrib Q1628    format=F00458_. label="Observation of HIV rapid test kit available";
  attrib Q1629    format=F00459_. label="Stock any ARVs for PMTCT in service area";
  attrib Q1630_01 format=F00460_. label="ARVs medicines available for PMTCT at service site(1)";
  attrib Q1630_02 format=F00460_. label="ARVs medicines available for PMTCT at service site(2)";
  attrib Q1630_03 format=F00460_. label="ARVs medicines available for PMTCT at service site(3)";
  attrib Q1630_04 format=F00460_. label="ARVs medicines available for PMTCT at service site(4)";
  attrib Q1630_05 format=F00460_. label="ARVs medicines available for PMTCT at service site(5)";
  attrib Q1630_06 format=F00460_. label="ARVs medicines available for PMTCT at service site(6)";
  attrib Q1630_07 format=F00460_. label="ARVs medicines available for PMTCT at service site(7)";
  attrib Q1630_08 format=F00460_. label="ARVs medicines available for PMTCT at service site(8)";
  attrib Q1630_09 format=F00460_. label="ARVs medicines available for PMTCT at service site(9)";
  attrib Q1630_10 format=F00460_. label="ARVs medicines available for PMTCT at service site(10)";
  attrib Q1630_11 format=F00460_. label="ARVs medicines available for PMTCT at service site(11)";
  attrib Q1630_12 format=F00460_. label="ARVs medicines available for PMTCT at service site(12)";
  attrib Q1650    format=F00461_. label="Data is already recorded for already assessed room/area";
  attrib Q1651_01 format=F00462_. label="Precaution and condition in client examination room(1)";
  attrib Q1651_02 format=F00462_. label="Precaution and condition in client examination room(2)";
  attrib Q1651_03 format=F00462_. label="Precaution and condition in client examination room(3)";
  attrib Q1651_04 format=F00462_. label="Precaution and condition in client examination room(4)";
  attrib Q1651_05 format=F00462_. label="Precaution and condition in client examination room(5)";
  attrib Q1651_06 format=F00462_. label="Precaution and condition in client examination room(6)";
  attrib Q1651_07 format=F00462_. label="Precaution and condition in client examination room(7)";
  attrib Q1651_08 format=F00462_. label="Precaution and condition in client examination room(8)";
  attrib Q1651_09 format=F00462_. label="Precaution and condition in client examination room(9)";
  attrib Q1651_10 format=F00462_. label="Precaution and condition in client examination room(10)";
  attrib Q1651_11 format=F00462_. label="Precaution and condition in client examination room(11)";
  attrib Q1651_12 format=F00462_. label="Precaution and condition in client examination room(12)";
  attrib Q1651_13 format=F00462_. label="Precaution and condition in client examination room(13)";
  attrib Q1652    format=F00463_. label="Setting of delivery service room";
  attrib Q1701    label="Days/month malaria services available";
  attrib Q1702    format=F00464_. label="Providers diagnose malaria";
  attrib Q1703    format=F00465_. label="Providers use blood tests to verify diagnosis of malaria";
  attrib Q1704    format=F00466_. label="Use blood tests to verify diagnosis for alway or sometimes";
  attrib Q1705    format=F00467_. label="Have malaria rapid dx test";
  attrib Q1706    format=F00468_. label="Observation of malaria rapid dx test kit available";
  attrib Q1707    format=F00469_. label="Brand/type of malaria RDT kit";
  attrib Q1708    format=F00470_. label="Training manual/poster/other job aid";
  attrib Q1709    format=F00471_. label="Training manual/poster/other job aid observed";
  attrib Q1710    format=F00472_. label="Providers prescribe tx for malaria";
  attrib Q1711    format=F00473_. label="National guidelines for dx & tx of malaria";
  attrib Q1712    format=F00474_. label="National guidelines for dx & tx of malaria observed";
  attrib Q1713    format=F00475_. label="Other guidelines for dx/tx of malaria";
  attrib Q1714    format=F00476_. label="Other guidelines for dx/tx of malaria observed";
  attrib Q1801    label="Days/month STI services available";
  attrib Q1802    format=F00477_. label="Providers diagnose STIs";
  attrib Q1803    format=F00478_. label="Method of STI dx";
  attrib Q1804    format=F00479_. label="Providers prescribe tx for STIs";
  attrib Q1806    format=F00480_. label="STI clients routinely referered for HIV counseling/testing";
  attrib Q1807    format=F00481_. label="Referered routinely or only if client suspected";
  attrib Q1808    format=F00482_. label="STI services provide HIV testing";
  attrib Q1809    format=F00483_. label="Observation of HIV rapid dx test kit available";
  attrib Q1810    format=F00484_. label="National guidelines for dx/tx of STIs";
  attrib Q1811    format=F00485_. label="National guidelines for dx/tx of STIs observed";
  attrib Q1812    format=F00486_. label="Any other guidelines for dx/tx of STIs";
  attrib Q1813    format=F00487_. label="Any other guidelines for dx/tx of STIs observed";
  attrib Q1814    format=F00488_. label="Facility normally performs partner notification";
  attrib Q1815    format=F00489_. label="Notification always active, sometime active or passive";
  attrib Q1816    format=F00490_. label="Individual client health records used";
  attrib Q1817    format=F00491_. label="Individual client health records observed";
  attrib Q1818_01 format=F00492_. label="Visual aids and client infomation for home(1)";
  attrib Q1818_02 format=F00492_. label="Visual aids and client infomation for home(2)";
  attrib Q1818_03 format=F00492_. label="Visual aids and client infomation for home(3)";
  attrib Q1818_04 format=F00492_. label="Visual aids and client infomation for home(4)";
  attrib Q1818_05 format=F00492_. label="Visual aids and client infomation for home(5)";
  attrib Q1818_06 format=F00492_. label="Visual aids and client infomation for home(6)";
  attrib Q1818_07 format=F00492_. label="Visual aids and client infomation for home(7)";
  attrib Q1818_08 format=F00492_. label="Visual aids and client infomation for home(8)";
  attrib Q1818_09 format=F00492_. label="Visual aids and client infomation for home(9)";
  attrib Q1818_10 format=F00492_. label="Visual aids and client infomation for home(10)";
  attrib Q1818_11 format=F00492_. label="Visual aids and client infomation for home(11)";
  attrib Q1818_12 format=F00492_. label="Visual aids and client infomation for home(12)";
  attrib Q1818_13 format=F00492_. label="Visual aids and client infomation for home(13)";
  attrib Q1818_14 format=F00492_. label="Visual aids and client infomation for home(14)";
  attrib Q1850    format=F00493_. label="Data is already recorded for already assessed room/area";
  attrib Q1851_01 format=F00494_. label="Precaution and condition in client examination room(1)";
  attrib Q1851_02 format=F00494_. label="Precaution and condition in client examination room(2)";
  attrib Q1851_03 format=F00494_. label="Precaution and condition in client examination room(3)";
  attrib Q1851_04 format=F00494_. label="Precaution and condition in client examination room(4)";
  attrib Q1851_05 format=F00494_. label="Precaution and condition in client examination room(5)";
  attrib Q1851_06 format=F00494_. label="Precaution and condition in client examination room(6)";
  attrib Q1851_07 format=F00494_. label="Precaution and condition in client examination room(7)";
  attrib Q1851_08 format=F00494_. label="Precaution and condition in client examination room(8)";
  attrib Q1851_09 format=F00494_. label="Precaution and condition in client examination room(9)";
  attrib Q1851_10 format=F00494_. label="Precaution and condition in client examination room(10)";
  attrib Q1851_11 format=F00494_. label="Precaution and condition in client examination room(11)";
  attrib Q1851_12 format=F00494_. label="Precaution and condition in client examination room(12)";
  attrib Q1851_13 format=F00494_. label="Precaution and condition in client examination room(13)";
  attrib Q1852    format=F00495_. label="Setting of STI service area/room";
  attrib Q1901    label="Days/ month TB services offered";
  attrib Q1902    format=F00496_. label="Providers dx TB";
  attrib Q1903    format=F00497_. label="Most common method of TB dx";
  attrib Q1904    format=F00498_. label="Providers refer outside facility for TB dx";
  attrib Q1905    format=F00499_. label="Facility has agreement with referral facility for test results to be returned";
  attrib Q1906    format=F00500_. label="Register of referred clients for TB dx";
  attrib Q1907    format=F00501_. label="Register of referred clients for TB dx observed";
  attrib Q1907A   format=F00502_. label="Est-ce que la structure fait les controles microscopiques des TB a frottis positifs";
  attrib Q1907B   format=F00503_. label="Quelle est la frequence des controles microscopiques que cette  structure fait";
  attrib Q1908    format=F00504_. label="Providers prescribe tx for TB or manage TB clients";
  attrib Q1909    format=F00505_. label="Treatment strategy";
  attrib Q1909A   format=F00506_. label="A quel niveau se fait le traitement directement observe?";
  attrib Q1911    format=F00507_. label="System for testing TB clients for HIV";
  attrib Q1912    format=F00508_. label="System for testing TB clients for HIV observed";
  attrib Q1913    format=F00509_. label="HIV rapid test kit available";
  attrib Q1914    format=F00510_. label="Observe HIV rapid test kit available";
  attrib Q1915    format=F00511_. label="National guidelines for TB dx/tx";
  attrib Q1916    format=F00512_. label="National guidelines for TB dx/tx observed";
  attrib Q1917    format=F00513_. label="Guidelines for management of HIV and TB co-infection";
  attrib Q1918    format=F00514_. label="Guidelines for management of HIV and TB co-infection observed";
  attrib Q1919    format=F00515_. label="Guidelines related to MDR-TB tx";
  attrib Q1920    format=F00516_. label="Guidelines related to MDR-TB tx observed";
  attrib Q1922    format=F00517_. label="Maintain any sputum containers";
  attrib Q1923    format=F00518_. label="Maintain any sputum containers observed";
  attrib Q1950    format=F00519_. label="Data is already recorded for already assessed room/area";
  attrib Q1951_01 format=F00520_. label="Precaution and condition in client examination room(1)";
  attrib Q1951_02 format=F00520_. label="Precaution and condition in client examination room(2)";
  attrib Q1951_03 format=F00520_. label="Precaution and condition in client examination room(3)";
  attrib Q1951_04 format=F00520_. label="Precaution and condition in client examination room(4)";
  attrib Q1951_05 format=F00520_. label="Precaution and condition in client examination room(5)";
  attrib Q1951_06 format=F00520_. label="Precaution and condition in client examination room(6)";
  attrib Q1951_07 format=F00520_. label="Precaution and condition in client examination room(7)";
  attrib Q1951_08 format=F00520_. label="Precaution and condition in client examination room(8)";
  attrib Q1951_09 format=F00520_. label="Precaution and condition in client examination room(9)";
  attrib Q1951_10 format=F00520_. label="Precaution and condition in client examination room(10)";
  attrib Q1951_11 format=F00520_. label="Precaution and condition in client examination room(11)";
  attrib Q1951_12 format=F00520_. label="Precaution and condition in client examination room(12)";
  attrib Q1951_13 format=F00520_. label="Precaution and condition in client examination room(13)";
  attrib Q1952    format=F00521_. label="Setting of examination room";
  attrib C1901    format=F00522_. label="Chronic coughing patients referred";
  attrib C1902    format=F00523_. label="Referred patients are subject to feed back";
  attrib C1903    format=F00524_. label="Directly observed treatment of TB";
  attrib C1904    format=F00525_. label="The TB information kept in the case de santé";
  attrib Q2001    label="Days/ month HIV counseling/ testing services";
  attrib Q2002    length=$6 format=$F00526_. label="Procedure for HIV test";
  attrib Q2004    format=F00527_. label="HIV rapid test available at site";
  attrib Q2005    format=F00528_. label="Individual client card/chart";
  attrib Q2006    format=F00529_. label="Individual client card/chart observed";
  attrib Q2007    format=F00530_. label="National HIV counseling and testing guidelines";
  attrib Q2008    format=F00531_. label="National HIV counseling and testing guidelines observed";
  attrib Q2009    format=F00532_. label="Other HIV testing guideliens";
  attrib Q2010    format=F00533_. label="Other HIV testing guideliens observed";
  attrib Q2011    format=F00534_. label="Staff have access to HIV PEP";
  attrib Q2012    format=F00535_. label="Written protocols for post-exposure prophylaxis (PEP)";
  attrib Q2013    format=F00536_. label="Written protocols for post-exposure prophylaxis (PEP) observed";
  attrib Q2050    format=F00537_. label="Data is already recorded for already assessed room/area";
  attrib Q2051_01 format=F00538_. label="Precaution and condition in client examination room(1)";
  attrib Q2051_02 format=F00538_. label="Precaution and condition in client examination room(2)";
  attrib Q2051_03 format=F00538_. label="Precaution and condition in client examination room(3)";
  attrib Q2051_04 format=F00538_. label="Precaution and condition in client examination room(4)";
  attrib Q2051_05 format=F00538_. label="Precaution and condition in client examination room(5)";
  attrib Q2051_06 format=F00538_. label="Precaution and condition in client examination room(6)";
  attrib Q2051_07 format=F00538_. label="Precaution and condition in client examination room(7)";
  attrib Q2051_08 format=F00538_. label="Precaution and condition in client examination room(8)";
  attrib Q2051_09 format=F00538_. label="Precaution and condition in client examination room(9)";
  attrib Q2051_10 format=F00538_. label="Precaution and condition in client examination room(10)";
  attrib Q2051_11 format=F00538_. label="Precaution and condition in client examination room(11)";
  attrib Q2051_12 format=F00538_. label="Precaution and condition in client examination room(12)";
  attrib Q2051_13 format=F00538_. label="Precaution and condition in client examination room(13)";
  attrib Q2052    format=F00539_. label="Setting of HIV testing/counseling room";
  attrib Q2053    format=F00540_. label="Condoms available in service site";
  attrib Q2054    format=F00541_. label="Condoms available in service site observed";
  attrib Q2056    format=F00542_. label="Facility has agreement with referral site for test results return";
  attrib Q2057    format=F00543_. label="Facility has agreement with referral site for test results return observed";
  attrib Q2101    format=F00544_. label="Prescribe ART";
  attrib Q2102    format=F00545_. label="Provide tx follow-up services for ART";
  attrib Q2104    format=F00546_. label="National ART guidelines";
  attrib Q2105    format=F00547_. label="National ART guidelines observed";
  attrib Q2106    format=F00548_. label="Other ART guideines";
  attrib Q2107    format=F00549_. label="Other ART guideines observed";
  attrib Q2108_01 format=F00550_. label="Tests for baseline before starting ART(1)";
  attrib Q2108_02 format=F00550_. label="Tests for baseline before starting ART(2)";
  attrib Q2108_03 format=F00550_. label="Tests for baseline before starting ART(3)";
  attrib Q2108_04 format=F00550_. label="Tests for baseline before starting ART(4)";
  attrib Q2108_05 format=F00550_. label="Tests for baseline before starting ART(5)";
  attrib Q2108_06 format=F00550_. label="Tests for baseline before starting ART(6)";
  attrib Q2108_07 format=F00550_. label="Tests for baseline before starting ART(7)";
  attrib Q2108_08 format=F00550_. label="Tests for baseline before starting ART(8)";
  attrib Q2108_09 format=F00550_. label="Tests for baseline before starting ART(9)";
  attrib Q2108_10 format=F00550_. label="Tests for baseline before starting ART(10)";
  attrib Q2108_11 format=F00550_. label="Tests for baseline before starting ART(11)";
  attrib Q2108_12 format=F00550_. label="Tests for baseline before starting ART(12)";
  attrib Q2109_01 format=F00551_. label="Follow-up tests while ART in progress(1)";
  attrib Q2109_02 format=F00551_. label="Follow-up tests while ART in progress(2)";
  attrib Q2109_03 format=F00551_. label="Follow-up tests while ART in progress(3)";
  attrib Q2109_04 format=F00551_. label="Follow-up tests while ART in progress(4)";
  attrib Q2109_05 format=F00551_. label="Follow-up tests while ART in progress(5)";
  attrib Q2109_06 format=F00551_. label="Follow-up tests while ART in progress(6)";
  attrib Q2109_07 format=F00551_. label="Follow-up tests while ART in progress(7)";
  attrib Q2109_08 format=F00551_. label="Follow-up tests while ART in progress(8)";
  attrib Q2109_09 format=F00551_. label="Follow-up tests while ART in progress(9)";
  attrib Q2109_10 format=F00551_. label="Follow-up tests while ART in progress(10)";
  attrib Q2109_11 format=F00551_. label="Follow-up tests while ART in progress(11)";
  attrib Q2109_12 format=F00551_. label="Follow-up tests while ART in progress(12)";
  attrib Q2201_01 format=F00552_. label="HIV care services(1)";
  attrib Q2201_02 format=F00552_. label="HIV care services(2)";
  attrib Q2201_03 format=F00552_. label="HIV care services(3)";
  attrib Q2201_04 format=F00552_. label="HIV care services(4)";
  attrib Q2201_05 format=F00552_. label="HIV care services(5)";
  attrib Q2201_06 format=F00552_. label="HIV care services(6)";
  attrib Q2201_07 format=F00552_. label="HIV care services(7)";
  attrib Q2201_08 format=F00552_. label="HIV care services(8)";
  attrib Q2201_09 format=F00552_. label="HIV care services(9)";
  attrib Q2201_10 format=F00552_. label="HIV care services(10)";
  attrib Q2201_11 format=F00552_. label="HIV care services(11)";
  attrib Q2201_12 format=F00552_. label="HIV care services(12)";
  attrib Q2202    format=F00553_. label="Test HIV+ clients for TB";
  attrib Q2203    format=F00554_. label="Test HIV+ clients for TB observed";
  attrib Q2204    format=F00555_. label="National guidelines for clinical management of HIV";
  attrib Q2205    format=F00556_. label="National guidelines for clinical management of HIV observed";
  attrib Q2206    format=F00557_. label="Guidelines for palliative care";
  attrib Q2207    format=F00558_. label="Guidelines for palliative care observed";
  attrib Q2208    format=F00559_. label="Condoms available for clients";
  attrib Q2209    format=F00560_. label="Condoms available for clients observed";
  attrib Q2301    format=F00561_. label="Providers dx or manage diabetes";
  attrib Q2302    format=F00562_. label="National guidelines for diabetes";
  attrib Q2303    format=F00563_. label="National guidelines for diabetes observed";
  attrib Q2304    format=F00564_. label="Other guidelines for diabetes";
  attrib Q2305    format=F00565_. label="Other guidelines for diabetes observed";
  attrib Q2310    format=F00566_. label="Providers dx or manage cardiovascular disease";
  attrib Q2311    format=F00567_. label="National guidelines for cardiovascular disease";
  attrib Q2312    format=F00568_. label="National guidelines for cardiovascular disease observed";
  attrib Q2313    format=F00569_. label="Other guidelines for cardiovascular disease";
  attrib Q2314    format=F00570_. label="Other guidelines for cardiovascular disease observed";
  attrib Q2320    format=F00571_. label="Providers dx or manage chronic respiratory diseases";
  attrib Q2321    format=F00572_. label="National guidelines for chronic respiratory diseases";
  attrib Q2322    format=F00573_. label="National guidelines for chronic respiratory diseases observed";
  attrib Q2323    format=F00574_. label="Other guidelines for chronic respiratory diseases";
  attrib Q2324    format=F00575_. label="Other guidelines for chronic respiratory diseasesobserved";
  attrib Q2330    format=F00576_. label="Asses the room/area for basic supplies & equipment (recorded in Q700)";
  attrib Q2331A_01 format=F00577_. label="Availability of basic supplies(1)";
  attrib Q2331A_02 format=F00577_. label="Availability of basic supplies(2)";
  attrib Q2331A_03 format=F00577_. label="Availability of basic supplies(3)";
  attrib Q2331A_04 format=F00577_. label="Availability of basic supplies(4)";
  attrib Q2331A_05 format=F00577_. label="Availability of basic supplies(5)";
  attrib Q2331A_06 format=F00577_. label="Availability of basic supplies(6)";
  attrib Q2331A_07 format=F00577_. label="Availability of basic supplies(7)";
  attrib Q2331A_08 format=F00577_. label="Availability of basic supplies(8)";
  attrib Q2331A_09 format=F00577_. label="Availability of basic supplies(9)";
  attrib Q2331A_10 format=F00577_. label="Availability of basic supplies(10)";
  attrib Q2331A_11 format=F00577_. label="Availability of basic supplies(11)";
  attrib Q2331A_12 format=F00577_. label="Availability of basic supplies(12)";
  attrib Q2331A_13 format=F00577_. label="Availability of basic supplies(13)";
  attrib Q2331A_14 format=F00577_. label="Availability of basic supplies(14)";
  attrib Q2331A_15 format=F00577_. label="Availability of basic supplies(15)";
  attrib Q2331A_16 format=F00577_. label="Availability of basic supplies(16)";
  attrib Q2331A_17 format=F00577_. label="Availability of basic supplies(17)";
  attrib Q2331A_18 format=F00577_. label="Availability of basic supplies(18)";
  attrib Q2331A_19 format=F00577_. label="Availability of basic supplies(19)";
  attrib Q2331A_20 format=F00577_. label="Availability of basic supplies(20)";
  attrib Q2331A_21 format=F00577_. label="Availability of basic supplies(21)";
  attrib Q2331B_01 format=F00578_. label="Basic supplies functioning(1)";
  attrib Q2331B_02 format=F00578_. label="Basic supplies functioning(2)";
  attrib Q2331B_03 format=F00578_. label="Basic supplies functioning(3)";
  attrib Q2331B_04 format=F00578_. label="Basic supplies functioning(4)";
  attrib Q2331B_05 format=F00578_. label="Basic supplies functioning(5)";
  attrib Q2331B_06 format=F00578_. label="Basic supplies functioning(6)";
  attrib Q2331B_07 format=F00578_. label="Basic supplies functioning(7)";
  attrib Q2331B_08 format=F00578_. label="Basic supplies functioning(8)";
  attrib Q2331B_09 format=F00578_. label="Basic supplies functioning(9)";
  attrib Q2331B_10 format=F00578_. label="Basic supplies functioning(10)";
  attrib Q2331B_11 format=F00578_. label="Basic supplies functioning(11)";
  attrib Q2331B_12 format=F00578_. label="Basic supplies functioning(12)";
  attrib Q2331B_13 format=F00578_. label="Basic supplies functioning(13)";
  attrib Q2331B_14 format=F00578_. label="Basic supplies functioning(14)";
  attrib Q2331B_15 format=F00578_. label="Basic supplies functioning(15)";
  attrib Q2331B_16 format=F00578_. label="Basic supplies functioning(16)";
  attrib Q2331B_17 format=F00578_. label="Basic supplies functioning(17)";
  attrib Q2331B_18 format=F00578_. label="Basic supplies functioning(18)";
  attrib Q2331B_19 format=F00578_. label="Basic supplies functioning(19)";
  attrib Q2331B_20 format=F00578_. label="Basic supplies functioning(20)";
  attrib Q2331B_21 format=F00578_. label="Basic supplies functioning(21)";
  attrib Q2350    format=F00579_. label="Data is already recorded for already assessed room/area";
  attrib Q2351_01 format=F00580_. label="Precaution and condition in client examination room(1)";
  attrib Q2351_02 format=F00580_. label="Precaution and condition in client examination room(2)";
  attrib Q2351_03 format=F00580_. label="Precaution and condition in client examination room(3)";
  attrib Q2351_04 format=F00580_. label="Precaution and condition in client examination room(4)";
  attrib Q2351_05 format=F00580_. label="Precaution and condition in client examination room(5)";
  attrib Q2351_06 format=F00580_. label="Precaution and condition in client examination room(6)";
  attrib Q2351_07 format=F00580_. label="Precaution and condition in client examination room(7)";
  attrib Q2351_08 format=F00580_. label="Precaution and condition in client examination room(8)";
  attrib Q2351_09 format=F00580_. label="Precaution and condition in client examination room(9)";
  attrib Q2351_10 format=F00580_. label="Precaution and condition in client examination room(10)";
  attrib Q2351_11 format=F00580_. label="Precaution and condition in client examination room(11)";
  attrib Q2351_12 format=F00580_. label="Precaution and condition in client examination room(12)";
  attrib Q2351_13 format=F00580_. label="Precaution and condition in client examination room(13)";
  attrib Q2352    format=F00581_. label="Setting of examination room";
  attrib Q2401A_1 format=F00582_. label="Surgery equipment available(1)";
  attrib Q2401A_2 format=F00582_. label="Surgery equipment available(2)";
  attrib Q2401A_3 format=F00582_. label="Surgery equipment available(3)";
  attrib Q2401A_4 format=F00582_. label="Surgery equipment available(4)";
  attrib Q2401A_5 format=F00582_. label="Surgery equipment available(5)";
  attrib Q2401A_6 format=F00582_. label="Surgery equipment available(6)";
  attrib Q2401B_1 format=F00583_. label="Equipment functioning(1)";
  attrib Q2401B_2 format=F00583_. label="Equipment functioning(2)";
  attrib Q2401B_3 format=F00583_. label="Equipment functioning(3)";
  attrib Q2401B_4 format=F00583_. label="Equipment functioning(4)";
  attrib Q2401B_5 format=F00583_. label="Equipment functioning(5)";
  attrib Q2401B_6 format=F00583_. label="Equipment functioning(6)";
  attrib Q2402_1  format=F00584_. label="Surgery materials/ medicines(1)";
  attrib Q2402_2  format=F00584_. label="Surgery materials/ medicines(2)";
  attrib Q2402_3  format=F00584_. label="Surgery materials/ medicines(3)";
  attrib Q2402_4  format=F00584_. label="Surgery materials/ medicines(4)";
  attrib Q2402_5  format=F00584_. label="Surgery materials/ medicines(5)";
  attrib Q2403    format=F00585_. label="Guidelines for IMEESC";
  attrib Q2404    format=F00586_. label="Guidelines for IMEESC observed";
  attrib Q2450    format=F00587_. label="Data is already recorded for already assessed room/area";
  attrib Q2451_01 format=F00588_. label="Precaution and condition in client examination room(1)";
  attrib Q2451_02 format=F00588_. label="Precaution and condition in client examination room(2)";
  attrib Q2451_03 format=F00588_. label="Precaution and condition in client examination room(3)";
  attrib Q2451_04 format=F00588_. label="Precaution and condition in client examination room(4)";
  attrib Q2451_05 format=F00588_. label="Precaution and condition in client examination room(5)";
  attrib Q2451_06 format=F00588_. label="Precaution and condition in client examination room(6)";
  attrib Q2451_07 format=F00588_. label="Precaution and condition in client examination room(7)";
  attrib Q2451_08 format=F00588_. label="Precaution and condition in client examination room(8)";
  attrib Q2451_09 format=F00588_. label="Precaution and condition in client examination room(9)";
  attrib Q2451_10 format=F00588_. label="Precaution and condition in client examination room(10)";
  attrib Q2451_11 format=F00588_. label="Precaution and condition in client examination room(11)";
  attrib Q2451_12 format=F00588_. label="Precaution and condition in client examination room(12)";
  attrib Q2451_13 format=F00588_. label="Precaution and condition in client examination room(13)";
  attrib Q2452    format=F00589_. label="Setting of examination room";
  attrib Q2501    format=F00590_. label="C-section provider present or on-call 24 hours";
  attrib Q2502    format=F00591_. label="Duty schedule or call list for 24-hr for C-section";
  attrib Q2503    format=F00592_. label="Duty schedule or call list for 24-hr for C-section observed";
  attrib Q2504    format=F00593_. label="Anesthetist present or on-call 24 hours";
  attrib Q2505    format=F00594_. label="Duty schedule or call list for 24-hr for Anesthetist";
  attrib Q2506    format=F00595_. label="Duty schedule or call list for 24-hr for Anesthetist observed";
  attrib Q2507    format=F00596_. label="Has CS been performed during past 3 months";
  attrib Q2510A_01 format=F00597_. label="Anesthesia equipment available(1)";
  attrib Q2510A_02 format=F00597_. label="Anesthesia equipment available(2)";
  attrib Q2510A_03 format=F00597_. label="Anesthesia equipment available(3)";
  attrib Q2510A_04 format=F00597_. label="Anesthesia equipment available(4)";
  attrib Q2510A_05 format=F00597_. label="Anesthesia equipment available(5)";
  attrib Q2510A_06 format=F00597_. label="Anesthesia equipment available(6)";
  attrib Q2510A_07 format=F00597_. label="Anesthesia equipment available(7)";
  attrib Q2510A_08 format=F00597_. label="Anesthesia equipment available(8)";
  attrib Q2510A_09 format=F00597_. label="Anesthesia equipment available(9)";
  attrib Q2510A_10 format=F00597_. label="Anesthesia equipment available(10)";
  attrib Q2510B_01 format=F00598_. label="Anesthesia equipment functioning(1)";
  attrib Q2510B_02 format=F00598_. label="Anesthesia equipment functioning(2)";
  attrib Q2510B_03 format=F00598_. label="Anesthesia equipment functioning(3)";
  attrib Q2510B_04 format=F00598_. label="Anesthesia equipment functioning(4)";
  attrib Q2510B_05 format=F00598_. label="Anesthesia equipment functioning(5)";
  attrib Q2510B_06 format=F00598_. label="Anesthesia equipment functioning(6)";
  attrib Q2510B_07 format=F00598_. label="Anesthesia equipment functioning(7)";
  attrib Q2510B_08 format=F00598_. label="Anesthesia equipment functioning(8)";
  attrib Q2510B_09 format=F00598_. label="Anesthesia equipment functioning(9)";
  attrib Q2510B_10 format=F00598_. label="Anesthesia equipment functioning(10)";
  attrib Q2601_1  format=F00599_. label="Reagents available in facility(1)";
  attrib Q2601_2  format=F00599_. label="Reagents available in facility(2)";
  attrib Q2601_3  format=F00599_. label="Reagents available in facility(3)";
  attrib Q2601_4  format=F00599_. label="Reagents available in facility(4)";
  attrib Q2601_5  format=F00599_. label="Reagents available in facility(5)";
  attrib Q2701    length=$4 format=$F00600_. label="Source of blood for transfusion";
  attrib Q2702    format=F00601_. label="Blood transfusion an obstetric context during past 3 months";
  attrib Q2710    format=F00602_. label="Blood screening";
  attrib Q2711    format=F00603_. label="Blood screening location";
  attrib Q2712_1  format=F00604_. label="Screening for specific diseases(1)";
  attrib Q2712_2  format=F00604_. label="Screening for specific diseases(2)";
  attrib Q2712_3  format=F00604_. label="Screening for specific diseases(3)";
  attrib Q2712_4  format=F00604_. label="Screening for specific diseases(4)";
  attrib Q2713    format=F00605_. label="Send blood outside for screening";
  attrib Q2714A_1 format=F00606_. label="Blood sent out for specific diseases(1)";
  attrib Q2714A_2 format=F00606_. label="Blood sent out for specific diseases(2)";
  attrib Q2714A_3 format=F00606_. label="Blood sent out for specific diseases(3)";
  attrib Q2714A_4 format=F00606_. label="Blood sent out for specific diseases(4)";
  attrib Q2714B_1 format=F00607_. label="Record of outside test for specific diseases(1)";
  attrib Q2714B_2 format=F00607_. label="Record of outside test for specific diseases(2)";
  attrib Q2714B_3 format=F00607_. label="Record of outside test for specific diseases(3)";
  attrib Q2714B_4 format=F00607_. label="Record of outside test for specific diseases(4)";
  attrib Q2720    format=F00608_. label="Facility has run out of blood in past 3 months";
  attrib Q2721    format=F00609_. label="Refrigerator for storing blood";
  attrib Q2722    format=F00610_. label="Refrigerator for storing blood observed";
  attrib Q2723    format=F00611_. label="Temperature of refrigerator for blood";
  attrib Q2724    format=F00612_. label="Guidelines on appropriate use of blood & safe transfusion practices";
  attrib Q2725    format=F00613_. label="Guidelines on appropriate use of blood & safe transfusion practices observed";
  attrib Q3000_1  format=F00614_. label="Facility cleanliness table(1)";
  attrib Q3000_2  format=F00614_. label="Facility cleanliness table(2)";
  attrib Q3000_3  format=F00614_. label="Facility cleanliness table(3)";
  attrib Q3000_4  format=F00614_. label="Facility cleanliness table(4)";
  attrib Q3000_5  format=F00614_. label="Facility cleanliness table(5)";
  attrib Q3000_6  format=F00614_. label="Facility cleanliness table(6)";
  attrib Q3000_7  format=F00614_. label="Facility cleanliness table(7)";
  attrib Q3000_8  format=F00614_. label="Facility cleanliness table(8)";
  attrib ENDHH    label="Ending hour";
  attrib ENDMM    label="Ending minute";
infile 'C:\SN 2018 SPA\EXPORT_2018\EXPORTEDFILES\SNFC8HFLSP.DAT' LRECL=2153 MISSOVER ;
    input
    @1    FACIL    5.0
    @6    REC_TYPE $1.
    @7    REGION   2.0
    @9    FACTYPE  2.0
    @11   MGA      1.0
    @12   FTYPE    1.0
    @13   IPD_OPD  1.0
    @14   VISITLN_1 1.0
    @15   VISITLN_2 1.0
    @16   VISITLN_3 1.0
    @17   VISITLN_4 1.0
    @18   QVDAY_1  2.0
    @20   QVDAY_2  2.0
    @22   QVDAY_3  2.0
    @24   QVDAY_4  2.0
    @26   QVMONTH_1 2.0
    @28   QVMONTH_2 2.0
    @30   QVMONTH_3 2.0
    @32   QVMONTH_4 2.0
    @34   QVYEAR_1 4.0
    @38   QVYEAR_2 4.0
    @42   QVYEAR_3 4.0
    @46   QVYEAR_4 4.0
    @50   QVRESULT_1 1.0
    @51   QVRESULT_2 1.0
    @52   QVRESULT_3 1.0
    @53   QVRESULT_4 1.0
    @54   NUMVISIT 1.0
    @55   DAY      2.0
    @57   MONTH    2.0
    @59   YEAR     4.0
    @63   INTVNO   2.0
    @65   RESULT   1.0
    @66   Q100     1.0
    @67   Q101H    2.0
    @69   Q101M    2.0
    @71   Q102_01  1.0
    @72   Q102_02  1.0
    @73   Q102_03  1.0
    @74   Q102_04  1.0
    @75   Q102_05  1.0
    @76   Q102_06  1.0
    @77   Q102_07  1.0
    @78   Q102_08  1.0
    @79   Q102_09  1.0
    @80   Q102_10  1.0
    @81   Q102_11  1.0
    @82   Q102_12  1.0
    @83   Q102_13  1.0
    @84   Q102_14  1.0
    @85   Q102_15  1.0
    @86   Q102_16  1.0
    @87   Q102_17  1.0
    @88   Q102_18  1.0
    @89   Q102_19  1.0
    @90   Q102_20  1.0
    @91   Q110     1.0
    @92   Q111     1.0
    @93   Q112     3.0
    @96   Q200     1.0
    @97   Q201     1.0
    @98   Q210     1.0
    @99   Q212     1.0
    @100  Q214     1.0
    @101  Q216     1.0
    @102  PROVDERS 2.0
    @104  PROVLNF  2.0
    @106  ANCF     3.0
    @109  FPF      3.0
    @112  SCF      3.0
    @115  STIF     3.0
    @118  FACWT    8.0
    @126  STEAMF   1.0
    @127  STEAMC   1.0
    @128  STEAMA   1.0
    @129  STEAMB   1.0
    @130  STEAMT   1.0
    @131  AUTOF    1.0
    @132  AUTOC    1.0
    @133  AUTOWA   1.0
    @134  AUTOWB   1.0
    @135  AUTOUA   1.0
    @136  AUTOUB   1.0
    @137  AUTOT    1.0
    @138  PRESSE   1.0
    @139  PRESSG   1.0
    @140  TEMPE    1.0
    @141  TEMPG    1.0
    @142  DHSFAC   1.0
    @143  DHSCAP   1.0
    @144  DHSA     1.0
    @145  DHSB     1.0
    @146  DHSC     1.0
    @147  DHST     1.0
    @148  HLDFAC   1.0
    @149  HLDCAP   1.0
    @150  HLDA     1.0
    @151  HLDB     1.0
    @152  HLDC     1.0
    @153  HLDT     1.0
    @154  CHEMF    1.0
    @155  CHEMA    1.0
    @156  CHEMB    1.0
    @157  CHEMT    1.0
    @158  SRC      1.0
    @159  ANCT     3.0
    @162  FPT      3.0
    @165  SCT      3.0
    @168  ISTT     3.0
    @171  ANYCAP   1.0
    @172  ANYKNOWL 1.0
    @173  ANYTIMER 1.0
    @174  FACEPI   1.0
    @175  MODTMPFP 1.0
    @176  STERFP   1.0
    @177  PAFP     1.0
    @178  FACPMTCT 1.0
    @179  FACART   1.0
    @180  FACCSS   1.0
    @181  FACMICROSC 1.0
    @182  FACSLIDES 1.0
    @183  FACTUBES 1.0
    @184  FACINCUB 1.0
    @185  FACSHAKER 1.0
    @186  FACHIVRDT 1.0
    @187  FACHIVLAB 1.0
    @188  FACHIVINTRNL 1.0
    @189  FACHIVEXTRNL 1.0
    @190  FACHIVTST 1.0
    @191  FACHIVDXTX 1.0
    @192  FACDBS   1.0
    @193  FACTBRDT 1.0
    @194  FACTBMICRO 1.0
    @195  FACTBCULT 1.0
    @196  FACTBXRAY 1.0
    @197  FACTBINTRNL 1.0
    @198  FACTBEXTRNL 1.0
    @199  FACTBTST 1.0
    @200  FACTBDXTX 1.0
    @201  FACMALRDT 1.0
    @202  FACMALLAB 1.0
    @203  FACMALTST 1.0
    @204  FACMALDXTX 1.0
    @205  SHARPWASTE 1.0
    @206  MEDWASTE 1.0
    @207  SHARPSTORE 1.0
    @208  MEDSTORE 1.0
    @209  ELIGPROV 2.0
    @211  ELIGTRAIN 2.0
    @213  ELIGSUPERV 2.0
    @215  ELIGBOTH 2.0
    @217  PROVIMCI 2.0
    @219  PROVGM   2.0
    @221  PROVEPI  2.0
    @223  HEMOGLOB 1.0
    @224  CD4      1.0
    @225  BLOODGLUC 1.0
    @226  SYPHSEROL 1.0
    @227  STOOLMICRO 1.0
    @228  Q300     1.0
    @229  Q301     1.0
    @230  Q302     1.0
    @231  Q310     1.0
    @232  Q311     1.0
    @233  Q312     1.0
    @234  Q313     1.0
    @235  Q314     1.0
    @236  Q315     1.0
    @237  Q316     1.0
    @238  Q317     1.0
    @239  Q318     1.0
    @240  Q319     1.0
    @241  Q320     1.0
    @242  Q321     1.0
    @243  Q322     1.0
    @244  Q323     1.0
    @245  Q330     2.0
    @247  Q331     1.0
    @248  Q332     1.0
    @249  Q333     $5.
    @254  Q340     1.0
    @255  Q341     1.0
    @256  Q342     1.0
    @257  Q343     $3.
    @260  Q345     1.0
    @261  Q346     1.0
    @262  Q350     1.0
    @263  Q351     1.0
    @264  Q352_1   1.0
    @265  Q352_2   1.0
    @266  Q352_3   1.0
    @267  Q360     1.0
    @268  Q361     1.0
    @269  Q362_01  1.0
    @270  Q362_02  1.0
    @271  Q362_03  1.0
    @272  Q362_04  1.0
    @273  Q362_05  1.0
    @274  Q362_06  1.0
    @275  Q362_07  1.0
    @276  Q362_08  1.0
    @277  Q362_09  1.0
    @278  Q362_10  1.0
    @279  Q362_11  1.0
    @280  Q362_12  1.0
    @281  Q362_13  1.0
    @282  Q362_14  1.0
    @283  Q362_15  1.0
    @284  Q362_16  1.0
    @285  Q362_17  1.0
    @286  Q363     1.0
    @287  Q364     1.0
    @288  Q365     $5.
    @293  Q370     $10.
    @303  Q371_1   1.0
    @304  Q371_2   1.0
    @305  Q371_3   1.0
    @306  Q400A_01 3.0
    @309  Q400A_02 3.0
    @312  Q400A_03 3.0
    @315  Q400A_04 3.0
    @318  Q400A_05 3.0
    @321  Q400A_06 3.0
    @324  Q400A_07 3.0
    @327  Q400A_08 3.0
    @330  Q400A_09 3.0
    @333  Q400A_10 3.0
    @336  Q400A_11 3.0
    @339  Q400A_12 3.0
    @342  Q400A_13 3.0
    @345  Q400A_14 3.0
    @348  Q400A_15 3.0
    @351  Q400A_16 3.0
    @354  Q400A_17 3.0
    @357  Q400A_18 3.0
    @360  Q400A_19 3.0
    @363  Q400A_20 3.0
    @366  Q400A_21 3.0
    @369  Q400A_22 3.0
    @372  Q400A_23 3.0
    @375  Q400A_24 3.0
    @378  Q400B_01 3.0
    @381  Q400B_02 3.0
    @384  Q400B_03 3.0
    @387  Q400B_04 3.0
    @390  Q400B_05 3.0
    @393  Q400B_06 3.0
    @396  Q400B_07 3.0
    @399  Q400B_08 3.0
    @402  Q400B_09 3.0
    @405  Q400B_10 3.0
    @408  Q400B_11 3.0
    @411  Q400B_12 3.0
    @414  Q400B_13 3.0
    @417  Q400B_14 3.0
    @420  Q400B_15 3.0
    @423  Q400B_16 3.0
    @426  Q400B_17 3.0
    @429  Q400B_18 3.0
    @432  Q400B_19 3.0
    @435  Q400B_20 3.0
    @438  Q400B_21 3.0
    @441  Q400B_22 3.0
    @444  Q400B_23 3.0
    @447  Q400B_24 3.0
    @450  Q400AT   4.0
    @454  Q400BT   3.0
    @457  Q410     1.0
    @458  Q411     1.0
    @459  Q412     1.0
    @460  Q413     1.0
    @461  Q414     $11.
    @472  Q415     1.0
    @473  Q416     1.0
    @474  Q416A    1.0
    @475  Q416B    1.0
    @476  Q417     1.0
    @477  Q418     1.0
    @478  Q419     1.0
    @479  Q420     1.0
    @480  Q430     1.0
    @481  Q431     $11.
    @492  Q432     1.0
    @493  Q433     1.0
    @494  Q440     1.0
    @495  Q441     1.0
    @496  Q442     1.0
    @497  Q450     1.0
    @498  Q451     1.0
    @499  Q452     1.0
    @500  Q452A    1.0
    @501  Q453     1.0
    @502  Q460     1.0
    @503  Q461     1.0
    @504  Q462     1.0
    @505  Q463     1.0
    @506  Q464     1.0
    @507  Q465     1.0
    @508  Q471     4.0
    @512  Q472     4.0
    @516  C400A_1  3.0
    @519  C400A_2  3.0
    @522  C400A_3  3.0
    @525  C400A_4  3.0
    @528  C400A_5  3.0
    @531  C400A_6  3.0
    @534  C400B_1  3.0
    @537  C400B_2  3.0
    @540  C400B_3  3.0
    @543  C400B_4  3.0
    @546  C400B_5  3.0
    @549  C400B_6  3.0
    @552  C400AT   4.0
    @556  C400BT   3.0
    @559  Q501A_01 1.0
    @560  Q501A_02 1.0
    @561  Q501A_03 1.0
    @562  Q501A_04 1.0
    @563  Q501A_05 1.0
    @564  Q501A_06 1.0
    @565  Q501A_07 1.0
    @566  Q501A_08 1.0
    @567  Q501A_09 1.0
    @568  Q501A_10 1.0
    @569  Q501B_01 1.0
    @570  Q501B_02 1.0
    @571  Q501B_03 1.0
    @572  Q501B_04 1.0
    @573  Q501B_05 1.0
    @574  Q501B_06 1.0
    @575  Q501B_07 1.0
    @576  Q501B_08 1.0
    @577  Q501B_09 1.0
    @578  Q501B_10 1.0
    @579  Q502A_1  1.0
    @580  Q502A_2  1.0
    @581  Q502A_3  1.0
    @582  Q502A_4  1.0
    @583  Q502A_5  1.0
    @584  Q502B_1  3.0
    @587  Q502B_2  3.0
    @590  Q502B_3  3.0
    @593  Q502B_4  3.0
    @596  Q502B_5  3.0
    @599  Q502C_1  3.0
    @602  Q502C_2  3.0
    @605  Q502C_3  3.0
    @608  Q502C_4  3.0
    @611  Q502C_5  3.0
    @614  Q502D_1  1.0
    @615  Q502D_2  1.0
    @616  Q502D_3  1.0
    @617  Q502D_4  1.0
    @618  Q502D_5  1.0
    @619  Q502E_1  3.0
    @622  Q502E_2  3.0
    @625  Q502E_3  3.0
    @628  Q502E_4  3.0
    @631  Q502E_5  3.0
    @634  Q502F_1  3.0
    @637  Q502F_2  3.0
    @640  Q502F_3  3.0
    @643  Q502F_4  3.0
    @646  Q502F_5  3.0
    @649  Q502G_1  2.0
    @651  Q502G_2  2.0
    @653  Q502G_3  2.0
    @655  Q502G_4  2.0
    @657  Q502G_5  2.0
    @659  Q503     1.0
    @660  Q504     1.0
    @661  C501A_1  1.0
    @662  C501A_2  1.0
    @663  C501A_3  1.0
    @664  C501B_1  1.0
    @665  C501B_2  1.0
    @666  C501B_3  1.0
    @667  C502A_1  1.0
    @668  C502A_2  1.0
    @669  C502E_1  3.0
    @672  C502E_2  3.0
    @675  C502G_1  2.0
    @677  C502G_2  2.0
    @679  Q600     2.0
    @681  Q601     2.0
    @683  Q603     1.0
    @684  Q605     1.0
    @685  Q607     1.0
    @686  Q608     1.0
    @687  Q609     1.0
    @688  Q610     1.0
    @689  Q611     1.0
    @690  Q620     2.0
    @692  Q700A_01 1.0
    @693  Q700A_02 1.0
    @694  Q700A_03 1.0
    @695  Q700A_04 1.0
    @696  Q700A_05 1.0
    @697  Q700A_06 1.0
    @698  Q700A_07 1.0
    @699  Q700A_08 1.0
    @700  Q700A_09 1.0
    @701  Q700A_10 1.0
    @702  Q700A_11 1.0
    @703  Q700A_12 1.0
    @704  Q700A_13 1.0
    @705  Q700A_14 1.0
    @706  Q700A_15 1.0
    @707  Q700A_16 1.0
    @708  Q700A_17 1.0
    @709  Q700A_18 1.0
    @710  Q700A_19 1.0
    @711  Q700A_20 1.0
    @712  Q700A_21 1.0
    @713  Q700A_22 1.0
    @714  Q700A_23 1.0
    @715  Q700A_24 1.0
    @716  Q700A_25 1.0
    @717  Q700B_01 1.0
    @718  Q700B_02 1.0
    @719  Q700B_03 1.0
    @720  Q700B_04 1.0
    @721  Q700B_05 1.0
    @722  Q700B_06 1.0
    @723  Q700B_07 1.0
    @724  Q700B_08 1.0
    @725  Q700B_09 1.0
    @726  Q700B_10 1.0
    @727  Q700B_11 1.0
    @728  Q700B_12 1.0
    @729  Q700B_13 1.0
    @730  Q700B_14 1.0
    @731  Q700B_15 1.0
    @732  Q700B_16 1.0
    @733  Q700B_17 1.0
    @734  Q700B_18 1.0
    @735  Q700B_19 1.0
    @736  Q700B_20 1.0
    @737  Q700B_21 1.0
    @738  Q700B_22 1.0
    @739  Q700B_23 1.0
    @740  Q700B_24 1.0
    @741  Q700B_25 1.0
    @742  Q710_01  1.0
    @743  Q710_02  1.0
    @744  Q710_03  1.0
    @745  Q710_04  1.0
    @746  Q710_05  1.0
    @747  Q710_06  1.0
    @748  Q710_07  1.0
    @749  Q710_08  1.0
    @750  Q710_09  1.0
    @751  Q710_10  1.0
    @752  Q710_11  1.0
    @753  Q710_12  1.0
    @754  Q710_13  1.0
    @755  Q711     1.0
    @756  Q720     1.0
    @757  C700A_01 1.0
    @758  C700A_02 1.0
    @759  C700A_03 1.0
    @760  C700A_04 1.0
    @761  C700A_05 1.0
    @762  C700A_06 1.0
    @763  C700A_07 1.0
    @764  C700A_08 1.0
    @765  C700A_09 1.0
    @766  C700A_10 1.0
    @767  C700A_11 1.0
    @768  C700A_12 1.0
    @769  C700A_13 1.0
    @770  C700A_14 1.0
    @771  C700A_15 1.0
    @772  C700A_16 1.0
    @773  C700A_17 1.0
    @774  C700A_18 1.0
    @775  C700A_19 1.0
    @776  C700A_20 1.0
    @777  C700A_21 1.0
    @778  C700A_22 1.0
    @779  C700A_23 1.0
    @780  C700A_24 1.0
    @781  C700A_25 1.0
    @782  C700A_26 1.0
    @783  C700A_27 1.0
    @784  C700A_28 1.0
    @785  C700A_29 1.0
    @786  C700A_30 1.0
    @787  C700A_31 1.0
    @788  C700A_32 1.0
    @789  C700A_33 1.0
    @790  C700A_34 1.0
    @791  C700B_01 1.0
    @792  C700B_02 1.0
    @793  C700B_03 1.0
    @794  C700B_04 1.0
    @795  C700B_05 1.0
    @796  C700B_06 1.0
    @797  C700B_07 1.0
    @798  C700B_08 1.0
    @799  C700B_09 1.0
    @800  C700B_10 1.0
    @801  C700B_11 1.0
    @802  C700B_12 1.0
    @803  C700B_13 1.0
    @804  C700B_14 1.0
    @805  C700B_15 1.0
    @806  C700B_16 1.0
    @807  C700B_17 1.0
    @808  C700B_18 1.0
    @809  C700B_19 1.0
    @810  C700B_20 1.0
    @811  C700B_21 1.0
    @812  C700B_22 1.0
    @813  C700B_23 1.0
    @814  C700B_24 1.0
    @815  C700B_25 1.0
    @816  C700B_26 1.0
    @817  C700B_27 1.0
    @818  C700B_28 1.0
    @819  C700B_29 1.0
    @820  C700B_30 1.0
    @821  C700B_31 1.0
    @822  C700B_32 1.0
    @823  C700B_33 1.0
    @824  C700B_34 1.0
    @825  Q801     1.0
    @826  Q802A_1  1.0
    @827  Q802A_2  1.0
    @828  Q802A_3  1.0
    @829  Q802A_4  1.0
    @830  Q802A_5  1.0
    @831  Q802A_6  1.0
    @832  Q802A_7  1.0
    @833  Q802A_8  1.0
    @834  Q802A_9  1.0
    @835  Q802B_1  1.0
    @836  Q802B_2  1.0
    @837  Q802B_3  1.0
    @838  Q802B_4  1.0
    @839  Q802B_5  1.0
    @840  Q802B_6  1.0
    @841  Q802B_7  1.0
    @842  Q802B_8  1.0
    @843  Q802B_9  1.0
    @844  Q802C_1  1.0
    @845  Q802C_2  1.0
    @846  Q802C_3  1.0
    @847  Q802C_4  1.0
    @848  Q802C_5  1.0
    @849  Q802C_6  1.0
    @850  Q802C_7  1.0
    @851  Q802C_8  1.0
    @852  Q802C_9  1.0
    @853  Q803     1.0
    @854  Q804     1.0
    @855  Q805     1.0
    @856  Q806     1.0
    @857  Q807     1.0
    @858  Q808     1.0
    @859  Q809     1.0
    @860  Q810     1.0
    @861  Q811A_1  1.0
    @862  Q811A_2  1.0
    @863  Q811A_3  1.0
    @864  Q811A_4  1.0
    @865  Q811A_5  1.0
    @866  Q811A_6  1.0
    @867  Q811B_1  1.0
    @868  Q811B_2  1.0
    @869  Q811B_3  1.0
    @870  Q811B_4  1.0
    @871  Q811B_5  1.0
    @872  Q811B_6  1.0
    @873  Q811C_1  1.0
    @874  Q811C_2  1.0
    @875  Q811C_3  1.0
    @876  Q811C_4  1.0
    @877  Q811C_5  1.0
    @878  Q811C_6  1.0
    @879  Q812     1.0
    @880  Q813     1.0
    @881  Q814     1.0
    @882  Q815     1.0
    @883  Q816     1.0
    @884  Q817     1.0
    @885  Q818     1.0
    @886  Q819     $4.
    @890  Q820     1.0
    @891  Q821     1.0
    @892  Q822     2.0
    @894  Q823     1.0
    @895  Q824     $5.
    @900  Q825     1.0
    @901  Q826     1.0
    @902  Q827_01  1.0
    @903  Q827_02  1.0
    @904  Q827_03  1.0
    @905  Q827_04  1.0
    @906  Q827_05  1.0
    @907  Q827_06  1.0
    @908  Q827_07  1.0
    @909  Q827_08  1.0
    @910  Q827_09  1.0
    @911  Q827_10  1.0
    @912  Q827_11  1.0
    @913  Q827_12  1.0
    @914  Q827_13  1.0
    @915  Q830     1.0
    @916  Q831A_1  1.0
    @917  Q831A_2  1.0
    @918  Q831B_1  1.0
    @919  Q831B_2  1.0
    @920  Q831C_1  1.0
    @921  Q831C_2  1.0
    @922  Q832     1.0
    @923  Q833     1.0
    @924  Q834     1.0
    @925  Q835     1.0
    @926  Q836     1.0
    @927  Q837A_1  1.0
    @928  Q837A_2  1.0
    @929  Q837A_3  1.0
    @930  Q837B_1  1.0
    @931  Q837B_2  1.0
    @932  Q837B_3  1.0
    @933  Q838     1.0
    @934  Q839A_1  1.0
    @935  Q839A_2  1.0
    @936  Q839A_3  1.0
    @937  Q839A_4  1.0
    @938  Q839B_1  1.0
    @939  Q839B_2  1.0
    @940  Q839B_3  1.0
    @941  Q839B_4  1.0
    @942  Q840A_1  1.0
    @943  Q840A_2  1.0
    @944  Q840A_3  1.0
    @945  Q840A_4  1.0
    @946  Q840A_5  1.0
    @947  Q840A_6  1.0
    @948  Q840A_7  1.0
    @949  Q840A_8  1.0
    @950  Q840B_1  1.0
    @951  Q840B_2  1.0
    @952  Q840B_3  1.0
    @953  Q840B_4  1.0
    @954  Q840B_5  1.0
    @955  Q840B_6  1.0
    @956  Q840B_7  1.0
    @957  Q840B_8  1.0
    @958  Q840C_1  1.0
    @959  Q840C_2  1.0
    @960  Q840C_3  1.0
    @961  Q840C_4  1.0
    @962  Q840C_5  1.0
    @963  Q840C_6  1.0
    @964  Q840C_7  1.0
    @965  Q840C_8  1.0
    @966  Q841     1.0
    @967  Q842     1.0
    @968  Q843     1.0
    @969  Q844     1.0
    @970  Q845     1.0
    @971  Q846     1.0
    @972  Q847A_1  1.0
    @973  Q847A_2  1.0
    @974  Q847A_3  1.0
    @975  Q847A_4  1.0
    @976  Q847B_1  1.0
    @977  Q847B_2  1.0
    @978  Q847B_3  1.0
    @979  Q847B_4  1.0
    @980  Q848     1.0
    @981  Q849_1   1.0
    @982  Q849_2   1.0
    @983  Q849_3   1.0
    @984  Q849_4   1.0
    @985  Q850     1.0
    @986  Q851A_1  1.0
    @987  Q851A_2  1.0
    @988  Q851A_3  1.0
    @989  Q851A_4  1.0
    @990  Q851B_1  1.0
    @991  Q851B_2  1.0
    @992  Q851B_3  1.0
    @993  Q851B_4  1.0
    @994  Q852     1.0
    @995  Q853A_1  1.0
    @996  Q853A_2  1.0
    @997  Q853A_3  1.0
    @998  Q853B_1  1.0
    @999  Q853B_2  1.0
    @1000 Q853B_3  1.0
    @1001 Q854     1.0
    @1002 Q855     1.0
    @1003 Q856     1.0
    @1004 Q857     1.0
    @1005 Q858A_1  1.0
    @1006 Q858A_2  1.0
    @1007 Q858A_3  1.0
    @1008 Q858A_4  1.0
    @1009 Q858A_5  1.0
    @1010 Q858B_1  1.0
    @1011 Q858B_2  1.0
    @1012 Q858B_3  1.0
    @1013 Q858B_4  1.0
    @1014 Q858B_5  1.0
    @1015 Q858C_1  1.0
    @1016 Q858C_2  1.0
    @1017 Q858C_3  1.0
    @1018 Q858C_4  1.0
    @1019 Q858C_5  1.0
    @1020 Q859     1.0
    @1021 Q860A_1  1.0
    @1022 Q860A_2  1.0
    @1023 Q860B_1  1.0
    @1024 Q860B_2  1.0
    @1025 Q861     1.0
    @1026 Q862A_1  1.0
    @1027 Q862A_2  1.0
    @1028 Q862A_3  1.0
    @1029 Q862A_4  1.0
    @1030 Q862A_5  1.0
    @1031 Q862A_6  1.0
    @1032 Q862A_7  1.0
    @1033 Q862B_1  1.0
    @1034 Q862B_2  1.0
    @1035 Q862B_3  1.0
    @1036 Q862B_4  1.0
    @1037 Q862B_5  1.0
    @1038 Q862B_6  1.0
    @1039 Q862B_7  1.0
    @1040 Q862C_1  1.0
    @1041 Q862C_2  1.0
    @1042 Q862C_3  1.0
    @1043 Q862C_4  1.0
    @1044 Q862C_5  1.0
    @1045 Q862C_6  1.0
    @1046 Q862C_7  1.0
    @1047 Q863     1.0
    @1048 Q864     1.0
    @1049 Q865     1.0
    @1050 Q866     1.0
    @1051 Q867     1.0
    @1052 Q868     1.0
    @1053 Q869     1.0
    @1054 Q870     1.0
    @1055 Q871     1.0
    @1056 Q872     1.0
    @1057 Q873     1.0
    @1058 Q880     1.0
    @1059 Q881A_1  1.0
    @1060 Q881A_2  1.0
    @1061 Q881A_3  1.0
    @1062 Q881A_4  1.0
    @1063 Q881B_1  1.0
    @1064 Q881B_2  1.0
    @1065 Q881B_3  1.0
    @1066 Q881B_4  1.0
    @1067 Q901_01  1.0
    @1068 Q901_02  1.0
    @1069 Q901_03  1.0
    @1070 Q901_04  1.0
    @1071 Q901_05  1.0
    @1072 Q901_06  1.0
    @1073 Q901_07  1.0
    @1074 Q901_08  1.0
    @1075 Q901_09  1.0
    @1076 Q901_10  1.0
    @1077 Q901_11  1.0
    @1078 Q901_12  1.0
    @1079 Q901_13  1.0
    @1080 Q901_14  1.0
    @1081 Q901_15  1.0
    @1082 Q901_16  1.0
    @1083 Q901_17  1.0
    @1084 Q901_18  1.0
    @1085 Q901_19  1.0
    @1086 Q901_20  1.0
    @1087 Q901_21  1.0
    @1088 Q901_22  1.0
    @1089 Q901_23  1.0
    @1090 Q901_24  1.0
    @1091 Q902_1   1.0
    @1092 Q902_2   1.0
    @1093 Q903_01  1.0
    @1094 Q903_02  1.0
    @1095 Q903_03  1.0
    @1096 Q903_04  1.0
    @1097 Q903_05  1.0
    @1098 Q903_06  1.0
    @1099 Q903_07  1.0
    @1100 Q903_08  1.0
    @1101 Q903_09  1.0
    @1102 Q903_10  1.0
    @1103 Q903_11  1.0
    @1104 Q903_12  1.0
    @1105 Q903_13  1.0
    @1106 Q903_14  1.0
    @1107 Q903_15  1.0
    @1108 Q903_16  1.0
    @1109 Q903_17  1.0
    @1110 Q903_18  1.0
    @1111 Q903_19  1.0
    @1112 Q903_20  1.0
    @1113 Q903_21  1.0
    @1114 Q903_22  1.0
    @1115 Q903_23  1.0
    @1116 Q903_24  1.0
    @1117 Q903_25  1.0
    @1118 Q903_26  1.0
    @1119 Q904_1   1.0
    @1120 Q904_2   1.0
    @1121 Q904_3   1.0
    @1122 Q904_4   1.0
    @1123 Q904_5   1.0
    @1124 Q905_01  1.0
    @1125 Q905_02  1.0
    @1126 Q905_03  1.0
    @1127 Q905_04  1.0
    @1128 Q905_05  1.0
    @1129 Q905_06  1.0
    @1130 Q905_07  1.0
    @1131 Q905_08  1.0
    @1132 Q905_09  1.0
    @1133 Q905_10  1.0
    @1134 Q906_01  1.0
    @1135 Q906_02  1.0
    @1136 Q906_03  1.0
    @1137 Q906_04  1.0
    @1138 Q906_05  1.0
    @1139 Q906_06  1.0
    @1140 Q906_07  1.0
    @1141 Q906_08  1.0
    @1142 Q906_09  1.0
    @1143 Q906_10  1.0
    @1144 Q906_11  1.0
    @1145 Q906C_01 1.0
    @1146 Q906C_02 1.0
    @1147 Q906C_03 1.0
    @1148 Q906C_04 1.0
    @1149 Q906C_05 1.0
    @1150 Q906C_06 1.0
    @1151 Q906C_07 1.0
    @1152 Q906C_08 1.0
    @1153 Q906C_09 1.0
    @1154 Q906C_10 1.0
    @1155 Q906C_11 1.0
    @1156 Q907_1   1.0
    @1157 Q907_2   1.0
    @1158 Q907_3   1.0
    @1159 Q908_1   1.0
    @1160 Q908_2   1.0
    @1161 Q908_3   1.0
    @1162 Q909_1   1.0
    @1163 Q909_2   1.0
    @1164 Q909_3   1.0
    @1165 Q909_4   1.0
    @1166 Q909_5   1.0
    @1167 Q910     1.0
    @1168 Q911     1.0
    @1169 Q912_1   1.0
    @1170 Q912_2   1.0
    @1171 Q912_3   1.0
    @1172 Q912_4   1.0
    @1173 Q912_5   1.0
    @1174 Q912_6   1.0
    @1175 Q912_7   1.0
    @1176 Q912_8   1.0
    @1177 C901_1   1.0
    @1178 C901_2   1.0
    @1179 C901_3   1.0
    @1180 C902_1   1.0
    @1181 C902_2   1.0
    @1182 C905_1   1.0
    @1183 C905_2   1.0
    @1184 C905_3   1.0
    @1185 C905_4   1.0
    @1186 C906_01  1.0
    @1187 C906_02  1.0
    @1188 C906_03  1.0
    @1189 C906_04  1.0
    @1190 C906_05  1.0
    @1191 C906_06  1.0
    @1192 C906_07  1.0
    @1193 C906_08  1.0
    @1194 C906_09  1.0
    @1195 C906_10  1.0
    @1196 C906_11  1.0
    @1197 C906_12  1.0
    @1198 C906_13  1.0
    @1199 C906_14  1.0
    @1200 C906_15  1.0
    @1201 C906_16  1.0
    @1202 C908_1   1.0
    @1203 C908_2   1.0
    @1204 C908_3   1.0
    @1205 Q921_01  1.0
    @1206 Q921_02  1.0
    @1207 Q921_03  1.0
    @1208 Q921_04  1.0
    @1209 Q921_05  1.0
    @1210 Q921_06  1.0
    @1211 Q921_07  1.0
    @1212 Q921_08  1.0
    @1213 Q921_09  1.0
    @1214 Q921_10  1.0
    @1215 Q922_1   1.0
    @1216 Q922_2   1.0
    @1217 Q922_3   1.0
    @1218 Q922_4   1.0
    @1219 Q922_5   1.0
    @1220 Q923     1.0
    @1221 Q924     1.0
    @1222 Q931_01  1.0
    @1223 Q931_02  1.0
    @1224 Q931_03  1.0
    @1225 Q931_04  1.0
    @1226 Q931_05  1.0
    @1227 Q931_06  1.0
    @1228 Q931_07  1.0
    @1229 Q931_08  1.0
    @1230 Q931_09  1.0
    @1231 Q931_10  1.0
    @1232 Q932_1   1.0
    @1233 Q932_2   1.0
    @1234 Q932_3   1.0
    @1235 Q932_4   1.0
    @1236 Q932_5   1.0
    @1237 Q933     1.0
    @1238 Q934     1.0
    @1239 Q941_1   1.0
    @1240 Q941_2   1.0
    @1241 Q941_3   1.0
    @1242 Q941_4   1.0
    @1243 Q941_5   1.0
    @1244 Q942_1   1.0
    @1245 Q942_2   1.0
    @1246 Q942_3   1.0
    @1247 Q942_4   1.0
    @1248 Q942_5   1.0
    @1249 Q942_6   1.0
    @1250 Q942_7   1.0
    @1251 Q943_1   1.0
    @1252 Q943_2   1.0
    @1253 Q943_3   1.0
    @1254 Q943_4   1.0
    @1255 Q943_5   1.0
    @1256 Q945_1   1.0
    @1257 Q945_2   1.0
    @1258 Q945_3   1.0
    @1259 Q945_4   1.0
    @1260 Q945_5   1.0
    @1261 Q946     1.0
    @1262 Q947     1.0
    @1263 Q1001A_1 2.0
    @1265 Q1001A_2 2.0
    @1267 Q1001A_3 2.0
    @1269 Q1001A_4 2.0
    @1271 Q1001A_5 2.0
    @1273 Q1001A_6 2.0
    @1275 Q1001A_7 2.0
    @1277 Q1001A_8 2.0
    @1279 Q1001A_9 2.0
    @1281 Q1001B_1 2.0
    @1283 Q1001B_2 2.0
    @1285 Q1001B_3 2.0
    @1287 Q1001B_4 2.0
    @1289 Q1001B_5 2.0
    @1291 Q1001B_6 2.0
    @1293 Q1001B_7 2.0
    @1295 Q1001B_8 2.0
    @1297 Q1001B_9 2.0
    @1299 Q1002    1.0
    @1300 Q1003    1.0
    @1301 Q1004    1.0
    @1302 Q1005    1.0
    @1303 Q1006_1  1.0
    @1304 Q1006_2  1.0
    @1305 Q1006_3  1.0
    @1306 Q1007    1.0
    @1307 Q1008    1.0
    @1308 Q1008A   1.0
    @1309 Q1009    1.0
    @1310 Q1010    1.0
    @1311 Q1011    1.0
    @1312 Q1012_1  1.0
    @1313 Q1012_2  1.0
    @1314 Q1012_3  1.0
    @1315 Q1012_4  1.0
    @1316 Q1012_5  1.0
    @1317 Q1012_6  1.0
    @1318 Q1012_7  1.0
    @1319 Q1012_8  1.0
    @1320 Q1012_9  1.0
    @1321 Q1013    1.0
    @1322 Q1013A   1.0
    @1323 Q1013B   1.0
    @1324 Q1014    1.0
    @1325 Q1014A   1.0
    @1326 Q1015    1.0
    @1327 Q1050    2.0
    @1329 Q1051_01 1.0
    @1330 Q1051_02 1.0
    @1331 Q1051_03 1.0
    @1332 Q1051_04 1.0
    @1333 Q1051_05 1.0
    @1334 Q1051_06 1.0
    @1335 Q1051_07 1.0
    @1336 Q1051_08 1.0
    @1337 Q1051_09 1.0
    @1338 Q1051_10 1.0
    @1339 Q1051_11 1.0
    @1340 Q1051_12 1.0
    @1341 Q1051_13 1.0
    @1342 Q1052    1.0
    @1343 Q1101A   2.0
    @1345 Q1101B   2.0
    @1347 Q1102    1.0
    @1348 Q1103    1.0
    @1349 Q1104A_1 1.0
    @1350 Q1104A_2 1.0
    @1351 Q1104A_3 1.0
    @1352 Q1104A_4 1.0
    @1353 Q1104A_5 1.0
    @1354 Q1104A_6 1.0
    @1355 Q1104B_1 1.0
    @1356 Q1104B_2 1.0
    @1357 Q1104B_3 1.0
    @1358 Q1104B_4 1.0
    @1359 Q1104B_5 1.0
    @1360 Q1104B_6 1.0
    @1361 Q1201A   2.0
    @1363 Q1201B   2.0
    @1365 Q1202_1  1.0
    @1366 Q1202_2  1.0
    @1367 Q1202_3  1.0
    @1368 Q1202_4  1.0
    @1369 Q1202_5  1.0
    @1370 Q1203    1.0
    @1371 Q1204    1.0
    @1372 Q1205    1.0
    @1373 Q1206    1.0
    @1374 Q1207    1.0
    @1375 Q1208    1.0
    @1376 Q1209_1  1.0
    @1377 Q1209_2  1.0
    @1378 Q1209_3  1.0
    @1379 Q1209_4  1.0
    @1380 Q1209_5  1.0
    @1381 Q1209_6  1.0
    @1382 Q1209_7  1.0
    @1383 Q1210A_01 1.0
    @1384 Q1210A_02 1.0
    @1385 Q1210A_03 1.0
    @1386 Q1210A_04 1.0
    @1387 Q1210A_05 1.0
    @1388 Q1210A_06 1.0
    @1389 Q1210A_07 1.0
    @1390 Q1210A_08 1.0
    @1391 Q1210A_09 1.0
    @1392 Q1210A_10 1.0
    @1393 Q1210A_11 1.0
    @1394 Q1210A_12 1.0
    @1395 Q1210B_01 1.0
    @1396 Q1210B_02 1.0
    @1397 Q1210B_03 1.0
    @1398 Q1210B_04 1.0
    @1399 Q1210B_05 1.0
    @1400 Q1210B_06 1.0
    @1401 Q1210B_07 1.0
    @1402 Q1210B_08 1.0
    @1403 Q1210B_09 1.0
    @1404 Q1210B_10 1.0
    @1405 Q1210B_11 1.0
    @1406 Q1210B_12 1.0
    @1407 Q1211_1  1.0
    @1408 Q1211_2  1.0
    @1409 Q1211_3  1.0
    @1410 Q1212    1.0
    @1411 Q1213    1.0
    @1412 Q1250    2.0
    @1414 Q1251_01 1.0
    @1415 Q1251_02 1.0
    @1416 Q1251_03 1.0
    @1417 Q1251_04 1.0
    @1418 Q1251_05 1.0
    @1419 Q1251_06 1.0
    @1420 Q1251_07 1.0
    @1421 Q1251_08 1.0
    @1422 Q1251_09 1.0
    @1423 Q1251_10 1.0
    @1424 Q1251_11 1.0
    @1425 Q1251_12 1.0
    @1426 Q1251_13 1.0
    @1427 Q1252    1.0
    @1428 Q1301    2.0
    @1430 Q1302_01 1.0
    @1431 Q1302_02 1.0
    @1432 Q1302_03 1.0
    @1433 Q1302_04 1.0
    @1434 Q1302_05 1.0
    @1435 Q1302_06 1.0
    @1436 Q1302_07 1.0
    @1437 Q1302_08 1.0
    @1438 Q1302_09 1.0
    @1439 Q1302_10 1.0
    @1440 Q1302_11 1.0
    @1441 Q1302_12 1.0
    @1442 Q1302_13 1.0
    @1443 Q1302_14 1.0
    @1444 Q1303    1.0
    @1445 Q1304    1.0
    @1446 Q1305    1.0
    @1447 Q1306    1.0
    @1448 Q1307    1.0
    @1449 Q1308    1.0
    @1450 Q1308A   1.0
    @1451 Q1308B   1.0
    @1452 Q1308C   1.0
    @1453 Q1308D   1.0
    @1454 Q1309    1.0
    @1455 Q1310_1  1.0
    @1456 Q1310_2  1.0
    @1457 Q1310_3  1.0
    @1458 Q1311    1.0
    @1459 Q1312    1.0
    @1460 Q1313    1.0
    @1461 Q1314A_01 1.0
    @1462 Q1314A_02 1.0
    @1463 Q1314A_03 1.0
    @1464 Q1314A_04 1.0
    @1465 Q1314A_05 1.0
    @1466 Q1314A_06 1.0
    @1467 Q1314A_07 1.0
    @1468 Q1314A_08 1.0
    @1469 Q1314A_09 1.0
    @1470 Q1314A_10 1.0
    @1471 Q1314B_01 1.0
    @1472 Q1314B_02 1.0
    @1473 Q1314B_03 1.0
    @1474 Q1314B_04 1.0
    @1475 Q1314B_05 1.0
    @1476 Q1314B_06 1.0
    @1477 Q1314B_07 1.0
    @1478 Q1314B_08 1.0
    @1479 Q1314B_09 1.0
    @1480 Q1314B_10 1.0
    @1481 Q1316_1  1.0
    @1482 Q1316_2  1.0
    @1483 Q1316_3  1.0
    @1484 Q1316_4  1.0
    @1485 Q1318_1  1.0
    @1486 Q1318_2  1.0
    @1487 Q1318_3  1.0
    @1488 Q1318_4  1.0
    @1489 Q1318_5  1.0
    @1490 Q1318_6  1.0
    @1491 Q1320_1  1.0
    @1492 Q1320_2  1.0
    @1493 Q1320_3  1.0
    @1494 Q1320_4  1.0
    @1495 Q1320_5  1.0
    @1496 Q1320_6  1.0
    @1497 Q1321    1.0
    @1498 Q1322    $6.
    @1504 Q1350    2.0
    @1506 Q1351_01 1.0
    @1507 Q1351_02 1.0
    @1508 Q1351_03 1.0
    @1509 Q1351_04 1.0
    @1510 Q1351_05 1.0
    @1511 Q1351_06 1.0
    @1512 Q1351_07 1.0
    @1513 Q1351_08 1.0
    @1514 Q1351_09 1.0
    @1515 Q1351_10 1.0
    @1516 Q1351_11 1.0
    @1517 Q1351_12 1.0
    @1518 Q1351_13 1.0
    @1519 Q1352    1.0
    @1520 Q1401    2.0
    @1522 Q1402_1  1.0
    @1523 Q1402_2  1.0
    @1524 Q1402_3  1.0
    @1525 Q1402_4  1.0
    @1526 Q1404    1.0
    @1527 Q1405    1.0
    @1528 Q1406_1  1.0
    @1529 Q1406_2  1.0
    @1530 Q1406_3  1.0
    @1531 Q1406_4  1.0
    @1532 Q1406_5  1.0
    @1533 Q1407_1  1.0
    @1534 Q1407_2  1.0
    @1535 Q1407_3  1.0
    @1536 Q1407_4  1.0
    @1537 Q1407_5  1.0
    @1538 Q1407_6  1.0
    @1539 Q1407_7  1.0
    @1540 Q1407_8  1.0
    @1541 Q1407_9  1.0
    @1542 Q1408    1.0
    @1543 Q1409    1.0
    @1544 Q1410    1.0
    @1545 Q1411    1.0
    @1546 Q1412    1.0
    @1547 Q1413    1.0
    @1548 Q1414    1.0
    @1549 Q1415    1.0
    @1550 Q1416    1.0
    @1551 Q1417    1.0
    @1552 Q1418    1.0
    @1553 Q1419    1.0
    @1554 Q1420_1  1.0
    @1555 Q1420_2  1.0
    @1556 Q1420_3  1.0
    @1557 Q1420_4  1.0
    @1558 Q1420_5  1.0
    @1559 Q1420_6  1.0
    @1560 Q1421A_1 1.0
    @1561 Q1421A_2 1.0
    @1562 Q1421A_3 1.0
    @1563 Q1421A_4 1.0
    @1564 Q1421A_5 1.0
    @1565 Q1421A_6 1.0
    @1566 Q1421A_7 1.0
    @1567 Q1421B_1 1.0
    @1568 Q1421B_2 1.0
    @1569 Q1421B_3 1.0
    @1570 Q1421B_4 1.0
    @1571 Q1421B_5 1.0
    @1572 Q1421B_6 1.0
    @1573 Q1421B_7 1.0
    @1574 Q1422_1  1.0
    @1575 Q1422_2  1.0
    @1576 Q1422_3  1.0
    @1577 Q1422_4  1.0
    @1578 Q1422_5  1.0
    @1579 Q1450    2.0
    @1581 Q1451_01 1.0
    @1582 Q1451_02 1.0
    @1583 Q1451_03 1.0
    @1584 Q1451_04 1.0
    @1585 Q1451_05 1.0
    @1586 Q1451_06 1.0
    @1587 Q1451_07 1.0
    @1588 Q1451_08 1.0
    @1589 Q1451_09 1.0
    @1590 Q1451_10 1.0
    @1591 Q1451_11 1.0
    @1592 Q1451_12 1.0
    @1593 Q1451_13 1.0
    @1594 Q1452    1.0
    @1595 Q1501_1  1.0
    @1596 Q1501_2  1.0
    @1597 Q1501_3  1.0
    @1598 Q1501_4  1.0
    @1599 Q1501_5  1.0
    @1600 Q1501_6  1.0
    @1601 Q1501_7  1.0
    @1602 Q1501_8  1.0
    @1603 Q1503    1.0
    @1604 Q1504    1.0
    @1605 Q1505    1.0
    @1606 Q1507    1.0
    @1607 Q1508    1.0
    @1608 Q1509    1.0
    @1609 Q1510    1.0
    @1610 Q1511    1.0
    @1611 Q1512    1.0
    @1612 Q1513    1.0
    @1613 Q1514    1.0
    @1614 Q1515    1.0
    @1615 Q1516_01 1.0
    @1616 Q1516_02 1.0
    @1617 Q1516_03 1.0
    @1618 Q1516_04 1.0
    @1619 Q1516_05 1.0
    @1620 Q1516_06 1.0
    @1621 Q1516_07 1.0
    @1622 Q1516_08 1.0
    @1623 Q1516_09 1.0
    @1624 Q1516_10 1.0
    @1625 Q1516_11 1.0
    @1626 Q1516_12 1.0
    @1627 Q1550    2.0
    @1629 Q1551_01 1.0
    @1630 Q1551_02 1.0
    @1631 Q1551_03 1.0
    @1632 Q1551_04 1.0
    @1633 Q1551_05 1.0
    @1634 Q1551_06 1.0
    @1635 Q1551_07 1.0
    @1636 Q1551_08 1.0
    @1637 Q1551_09 1.0
    @1638 Q1551_10 1.0
    @1639 Q1551_11 1.0
    @1640 Q1551_12 1.0
    @1641 Q1551_13 1.0
    @1642 Q1552    1.0
    @1643 Q1601    1.0
    @1644 Q1602    1.0
    @1645 Q1603    1.0
    @1646 Q1604A_1 1.0
    @1647 Q1604A_2 1.0
    @1648 Q1604A_3 1.0
    @1649 Q1604A_4 1.0
    @1650 Q1604A_5 1.0
    @1651 Q1604A_6 1.0
    @1652 Q1604A_7 1.0
    @1653 Q1604A_8 1.0
    @1654 Q1604B_1 1.0
    @1655 Q1604B_2 1.0
    @1656 Q1604B_3 1.0
    @1657 Q1604B_4 1.0
    @1658 Q1604B_5 1.0
    @1659 Q1604B_6 1.0
    @1660 Q1604B_7 1.0
    @1661 Q1604B_8 1.0
    @1662 Q1605    1.0
    @1663 Q1606    1.0
    @1664 Q1607    1.0
    @1665 Q1608    1.0
    @1666 Q1609    1.0
    @1667 Q1610    1.0
    @1668 Q1611    1.0
    @1669 Q1612    1.0
    @1670 Q1613    1.0
    @1671 Q1614    1.0
    @1672 Q1615    3.0
    @1675 Q1616    3.0
    @1678 Q1617    1.0
    @1679 Q1618    1.0
    @1680 Q1619    2.0
    @1682 Q1621    2.0
    @1684 Q1622A_01 1.0
    @1685 Q1622A_02 1.0
    @1686 Q1622A_03 1.0
    @1687 Q1622A_04 1.0
    @1688 Q1622A_05 1.0
    @1689 Q1622A_06 1.0
    @1690 Q1622A_07 1.0
    @1691 Q1622A_08 1.0
    @1692 Q1622A_09 1.0
    @1693 Q1622A_10 1.0
    @1694 Q1622A_11 1.0
    @1695 Q1622A_12 1.0
    @1696 Q1622A_13 1.0
    @1697 Q1622A_14 1.0
    @1698 Q1622B_01 1.0
    @1699 Q1622B_02 1.0
    @1700 Q1622B_03 1.0
    @1701 Q1622B_04 1.0
    @1702 Q1622B_05 1.0
    @1703 Q1622B_06 1.0
    @1704 Q1622B_07 1.0
    @1705 Q1622B_08 1.0
    @1706 Q1622B_09 1.0
    @1707 Q1622B_10 1.0
    @1708 Q1622B_11 1.0
    @1709 Q1622B_12 1.0
    @1710 Q1622B_13 1.0
    @1711 Q1622B_14 1.0
    @1712 Q1623_01 1.0
    @1713 Q1623_02 1.0
    @1714 Q1623_03 1.0
    @1715 Q1623_04 1.0
    @1716 Q1623_05 1.0
    @1717 Q1623_06 1.0
    @1718 Q1623_07 1.0
    @1719 Q1623_08 1.0
    @1720 Q1623_09 1.0
    @1721 Q1623_10 1.0
    @1722 Q1623_11 1.0
    @1723 Q1623_12 1.0
    @1724 Q1624_01 1.0
    @1725 Q1624_02 1.0
    @1726 Q1624_03 1.0
    @1727 Q1624_04 1.0
    @1728 Q1624_05 1.0
    @1729 Q1624_06 1.0
    @1730 Q1624_07 1.0
    @1731 Q1624_08 1.0
    @1732 Q1624_09 1.0
    @1733 Q1624_10 1.0
    @1734 Q1624_11 1.0
    @1735 Q1624_12 1.0
    @1736 Q1624_13 1.0
    @1737 Q1624_14 1.0
    @1738 Q1624_15 1.0
    @1739 Q1625_1  1.0
    @1740 Q1625_2  1.0
    @1741 Q1625_3  1.0
    @1742 Q1625_4  1.0
    @1743 Q1625_5  1.0
    @1744 Q1625_6  1.0
    @1745 Q1625_7  1.0
    @1746 Q1625_8  1.0
    @1747 Q1625_9  1.0
    @1748 Q1626    1.0
    @1749 Q1627    1.0
    @1750 Q1628    1.0
    @1751 Q1629    1.0
    @1752 Q1630_01 1.0
    @1753 Q1630_02 1.0
    @1754 Q1630_03 1.0
    @1755 Q1630_04 1.0
    @1756 Q1630_05 1.0
    @1757 Q1630_06 1.0
    @1758 Q1630_07 1.0
    @1759 Q1630_08 1.0
    @1760 Q1630_09 1.0
    @1761 Q1630_10 1.0
    @1762 Q1630_11 1.0
    @1763 Q1630_12 1.0
    @1764 Q1650    2.0
    @1766 Q1651_01 1.0
    @1767 Q1651_02 1.0
    @1768 Q1651_03 1.0
    @1769 Q1651_04 1.0
    @1770 Q1651_05 1.0
    @1771 Q1651_06 1.0
    @1772 Q1651_07 1.0
    @1773 Q1651_08 1.0
    @1774 Q1651_09 1.0
    @1775 Q1651_10 1.0
    @1776 Q1651_11 1.0
    @1777 Q1651_12 1.0
    @1778 Q1651_13 1.0
    @1779 Q1652    1.0
    @1780 Q1701    2.0
    @1782 Q1702    1.0
    @1783 Q1703    1.0
    @1784 Q1704    1.0
    @1785 Q1705    1.0
    @1786 Q1706    1.0
    @1787 Q1707    1.0
    @1788 Q1708    1.0
    @1789 Q1709    1.0
    @1790 Q1710    1.0
    @1791 Q1711    1.0
    @1792 Q1712    1.0
    @1793 Q1713    1.0
    @1794 Q1714    1.0
    @1795 Q1801    2.0
    @1797 Q1802    1.0
    @1798 Q1803    1.0
    @1799 Q1804    1.0
    @1800 Q1806    1.0
    @1801 Q1807    1.0
    @1802 Q1808    1.0
    @1803 Q1809    1.0
    @1804 Q1810    1.0
    @1805 Q1811    1.0
    @1806 Q1812    1.0
    @1807 Q1813    1.0
    @1808 Q1814    1.0
    @1809 Q1815    1.0
    @1810 Q1816    1.0
    @1811 Q1817    1.0
    @1812 Q1818_01 1.0
    @1813 Q1818_02 1.0
    @1814 Q1818_03 1.0
    @1815 Q1818_04 1.0
    @1816 Q1818_05 1.0
    @1817 Q1818_06 1.0
    @1818 Q1818_07 1.0
    @1819 Q1818_08 1.0
    @1820 Q1818_09 1.0
    @1821 Q1818_10 1.0
    @1822 Q1818_11 1.0
    @1823 Q1818_12 1.0
    @1824 Q1818_13 1.0
    @1825 Q1818_14 1.0
    @1826 Q1850    2.0
    @1828 Q1851_01 1.0
    @1829 Q1851_02 1.0
    @1830 Q1851_03 1.0
    @1831 Q1851_04 1.0
    @1832 Q1851_05 1.0
    @1833 Q1851_06 1.0
    @1834 Q1851_07 1.0
    @1835 Q1851_08 1.0
    @1836 Q1851_09 1.0
    @1837 Q1851_10 1.0
    @1838 Q1851_11 1.0
    @1839 Q1851_12 1.0
    @1840 Q1851_13 1.0
    @1841 Q1852    1.0
    @1842 Q1901    2.0
    @1844 Q1902    1.0
    @1845 Q1903    1.0
    @1846 Q1904    1.0
    @1847 Q1905    1.0
    @1848 Q1906    1.0
    @1849 Q1907    1.0
    @1850 Q1907A   1.0
    @1851 Q1907B   1.0
    @1852 Q1908    1.0
    @1853 Q1909    1.0
    @1854 Q1909A   1.0
    @1855 Q1911    1.0
    @1856 Q1912    1.0
    @1857 Q1913    1.0
    @1858 Q1914    1.0
    @1859 Q1915    1.0
    @1860 Q1916    1.0
    @1861 Q1917    1.0
    @1862 Q1918    1.0
    @1863 Q1919    1.0
    @1864 Q1920    1.0
    @1865 Q1922    1.0
    @1866 Q1923    1.0
    @1867 Q1950    2.0
    @1869 Q1951_01 1.0
    @1870 Q1951_02 1.0
    @1871 Q1951_03 1.0
    @1872 Q1951_04 1.0
    @1873 Q1951_05 1.0
    @1874 Q1951_06 1.0
    @1875 Q1951_07 1.0
    @1876 Q1951_08 1.0
    @1877 Q1951_09 1.0
    @1878 Q1951_10 1.0
    @1879 Q1951_11 1.0
    @1880 Q1951_12 1.0
    @1881 Q1951_13 1.0
    @1882 Q1952    1.0
    @1883 C1901    1.0
    @1884 C1902    1.0
    @1885 C1903    1.0
    @1886 C1904    1.0
    @1887 Q2001    2.0
    @1889 Q2002    $6.
    @1895 Q2004    1.0
    @1896 Q2005    1.0
    @1897 Q2006    1.0
    @1898 Q2007    1.0
    @1899 Q2008    1.0
    @1900 Q2009    1.0
    @1901 Q2010    1.0
    @1902 Q2011    1.0
    @1903 Q2012    1.0
    @1904 Q2013    1.0
    @1905 Q2050    2.0
    @1907 Q2051_01 1.0
    @1908 Q2051_02 1.0
    @1909 Q2051_03 1.0
    @1910 Q2051_04 1.0
    @1911 Q2051_05 1.0
    @1912 Q2051_06 1.0
    @1913 Q2051_07 1.0
    @1914 Q2051_08 1.0
    @1915 Q2051_09 1.0
    @1916 Q2051_10 1.0
    @1917 Q2051_11 1.0
    @1918 Q2051_12 1.0
    @1919 Q2051_13 1.0
    @1920 Q2052    1.0
    @1921 Q2053    1.0
    @1922 Q2054    1.0
    @1923 Q2056    1.0
    @1924 Q2057    1.0
    @1925 Q2101    1.0
    @1926 Q2102    1.0
    @1927 Q2104    1.0
    @1928 Q2105    1.0
    @1929 Q2106    1.0
    @1930 Q2107    1.0
    @1931 Q2108_01 1.0
    @1932 Q2108_02 1.0
    @1933 Q2108_03 1.0
    @1934 Q2108_04 1.0
    @1935 Q2108_05 1.0
    @1936 Q2108_06 1.0
    @1937 Q2108_07 1.0
    @1938 Q2108_08 1.0
    @1939 Q2108_09 1.0
    @1940 Q2108_10 1.0
    @1941 Q2108_11 1.0
    @1942 Q2108_12 1.0
    @1943 Q2109_01 1.0
    @1944 Q2109_02 1.0
    @1945 Q2109_03 1.0
    @1946 Q2109_04 1.0
    @1947 Q2109_05 1.0
    @1948 Q2109_06 1.0
    @1949 Q2109_07 1.0
    @1950 Q2109_08 1.0
    @1951 Q2109_09 1.0
    @1952 Q2109_10 1.0
    @1953 Q2109_11 1.0
    @1954 Q2109_12 1.0
    @1955 Q2201_01 1.0
    @1956 Q2201_02 1.0
    @1957 Q2201_03 1.0
    @1958 Q2201_04 1.0
    @1959 Q2201_05 1.0
    @1960 Q2201_06 1.0
    @1961 Q2201_07 1.0
    @1962 Q2201_08 1.0
    @1963 Q2201_09 1.0
    @1964 Q2201_10 1.0
    @1965 Q2201_11 1.0
    @1966 Q2201_12 1.0
    @1967 Q2202    1.0
    @1968 Q2203    1.0
    @1969 Q2204    1.0
    @1970 Q2205    1.0
    @1971 Q2206    1.0
    @1972 Q2207    1.0
    @1973 Q2208    1.0
    @1974 Q2209    1.0
    @1975 Q2301    1.0
    @1976 Q2302    1.0
    @1977 Q2303    1.0
    @1978 Q2304    1.0
    @1979 Q2305    1.0
    @1980 Q2310    1.0
    @1981 Q2311    1.0
    @1982 Q2312    1.0
    @1983 Q2313    1.0
    @1984 Q2314    1.0
    @1985 Q2320    1.0
    @1986 Q2321    1.0
    @1987 Q2322    1.0
    @1988 Q2323    1.0
    @1989 Q2324    1.0
    @1990 Q2330    1.0
    @1991 Q2331A_01 1.0
    @1992 Q2331A_02 1.0
    @1993 Q2331A_03 1.0
    @1994 Q2331A_04 1.0
    @1995 Q2331A_05 1.0
    @1996 Q2331A_06 1.0
    @1997 Q2331A_07 1.0
    @1998 Q2331A_08 1.0
    @1999 Q2331A_09 1.0
    @2000 Q2331A_10 1.0
    @2001 Q2331A_11 1.0
    @2002 Q2331A_12 1.0
    @2003 Q2331A_13 1.0
    @2004 Q2331A_14 1.0
    @2005 Q2331A_15 1.0
    @2006 Q2331A_16 1.0
    @2007 Q2331A_17 1.0
    @2008 Q2331A_18 1.0
    @2009 Q2331A_19 1.0
    @2010 Q2331A_20 1.0
    @2011 Q2331A_21 1.0
    @2012 Q2331B_01 1.0
    @2013 Q2331B_02 1.0
    @2014 Q2331B_03 1.0
    @2015 Q2331B_04 1.0
    @2016 Q2331B_05 1.0
    @2017 Q2331B_06 1.0
    @2018 Q2331B_07 1.0
    @2019 Q2331B_08 1.0
    @2020 Q2331B_09 1.0
    @2021 Q2331B_10 1.0
    @2022 Q2331B_11 1.0
    @2023 Q2331B_12 1.0
    @2024 Q2331B_13 1.0
    @2025 Q2331B_14 1.0
    @2026 Q2331B_15 1.0
    @2027 Q2331B_16 1.0
    @2028 Q2331B_17 1.0
    @2029 Q2331B_18 1.0
    @2030 Q2331B_19 1.0
    @2031 Q2331B_20 1.0
    @2032 Q2331B_21 1.0
    @2033 Q2350    2.0
    @2035 Q2351_01 1.0
    @2036 Q2351_02 1.0
    @2037 Q2351_03 1.0
    @2038 Q2351_04 1.0
    @2039 Q2351_05 1.0
    @2040 Q2351_06 1.0
    @2041 Q2351_07 1.0
    @2042 Q2351_08 1.0
    @2043 Q2351_09 1.0
    @2044 Q2351_10 1.0
    @2045 Q2351_11 1.0
    @2046 Q2351_12 1.0
    @2047 Q2351_13 1.0
    @2048 Q2352    1.0
    @2049 Q2401A_1 1.0
    @2050 Q2401A_2 1.0
    @2051 Q2401A_3 1.0
    @2052 Q2401A_4 1.0
    @2053 Q2401A_5 1.0
    @2054 Q2401A_6 1.0
    @2055 Q2401B_1 1.0
    @2056 Q2401B_2 1.0
    @2057 Q2401B_3 1.0
    @2058 Q2401B_4 1.0
    @2059 Q2401B_5 1.0
    @2060 Q2401B_6 1.0
    @2061 Q2402_1  1.0
    @2062 Q2402_2  1.0
    @2063 Q2402_3  1.0
    @2064 Q2402_4  1.0
    @2065 Q2402_5  1.0
    @2066 Q2403    1.0
    @2067 Q2404    1.0
    @2068 Q2450    2.0
    @2070 Q2451_01 1.0
    @2071 Q2451_02 1.0
    @2072 Q2451_03 1.0
    @2073 Q2451_04 1.0
    @2074 Q2451_05 1.0
    @2075 Q2451_06 1.0
    @2076 Q2451_07 1.0
    @2077 Q2451_08 1.0
    @2078 Q2451_09 1.0
    @2079 Q2451_10 1.0
    @2080 Q2451_11 1.0
    @2081 Q2451_12 1.0
    @2082 Q2451_13 1.0
    @2083 Q2452    1.0
    @2084 Q2501    1.0
    @2085 Q2502    1.0
    @2086 Q2503    1.0
    @2087 Q2504    1.0
    @2088 Q2505    1.0
    @2089 Q2506    1.0
    @2090 Q2507    1.0
    @2091 Q2510A_01 1.0
    @2092 Q2510A_02 1.0
    @2093 Q2510A_03 1.0
    @2094 Q2510A_04 1.0
    @2095 Q2510A_05 1.0
    @2096 Q2510A_06 1.0
    @2097 Q2510A_07 1.0
    @2098 Q2510A_08 1.0
    @2099 Q2510A_09 1.0
    @2100 Q2510A_10 1.0
    @2101 Q2510B_01 1.0
    @2102 Q2510B_02 1.0
    @2103 Q2510B_03 1.0
    @2104 Q2510B_04 1.0
    @2105 Q2510B_05 1.0
    @2106 Q2510B_06 1.0
    @2107 Q2510B_07 1.0
    @2108 Q2510B_08 1.0
    @2109 Q2510B_09 1.0
    @2110 Q2510B_10 1.0
    @2111 Q2601_1  1.0
    @2112 Q2601_2  1.0
    @2113 Q2601_3  1.0
    @2114 Q2601_4  1.0
    @2115 Q2601_5  1.0
    @2116 Q2701    $4.
    @2120 Q2702    1.0
    @2121 Q2710    1.0
    @2122 Q2711    1.0
    @2123 Q2712_1  1.0
    @2124 Q2712_2  1.0
    @2125 Q2712_3  1.0
    @2126 Q2712_4  1.0
    @2127 Q2713    1.0
    @2128 Q2714A_1 1.0
    @2129 Q2714A_2 1.0
    @2130 Q2714A_3 1.0
    @2131 Q2714A_4 1.0
    @2132 Q2714B_1 1.0
    @2133 Q2714B_2 1.0
    @2134 Q2714B_3 1.0
    @2135 Q2714B_4 1.0
    @2136 Q2720    1.0
    @2137 Q2721    1.0
    @2138 Q2722    1.0
    @2139 Q2723    1.0
    @2140 Q2724    1.0
    @2141 Q2725    1.0
    @2142 Q3000_1  1.0
    @2143 Q3000_2  1.0
    @2144 Q3000_3  1.0
    @2145 Q3000_4  1.0
    @2146 Q3000_5  1.0
    @2147 Q3000_6  1.0
    @2148 Q3000_7  1.0
    @2149 Q3000_8  1.0
    @2150 ENDHH    2.0
    @2152 ENDMM    2.0
    ;
    if ANCF     = 999 then ANCF = .;
    if FPF      = 999 then FPF = .;
    if SCF      = 999 then SCF = .;
    if STIF     = 999 then STIF = .;
    if ANCT     = 999 then ANCT = .;
    if FPT      = 999 then FPT = .;
    if SCT      = 999 then SCT = .;
    if Q1251_01 = 9 then Q1251_01 = .;
    if Q1251_02 = 9 then Q1251_02 = .;
    if Q1251_03 = 9 then Q1251_03 = .;
    if Q1251_04 = 9 then Q1251_04 = .;
    if Q1251_05 = 9 then Q1251_05 = .;
    if Q1251_06 = 9 then Q1251_06 = .;
    if Q1251_07 = 9 then Q1251_07 = .;
    if Q1251_08 = 9 then Q1251_08 = .;
    if Q1251_09 = 9 then Q1251_09 = .;
    if Q1251_10 = 9 then Q1251_10 = .;
    if Q1251_11 = 9 then Q1251_11 = .;
    if Q1251_12 = 9 then Q1251_12 = .;
    if Q1251_13 = 9 then Q1251_13 = .;
Run;
