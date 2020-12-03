libname user 'C:\SNSPA_YEAR1\EXPORT\JEANNE\EXPORT';
Proc format;
  value F00001_
     1 = "Dakar"
     2 = "Diourbel"
     3 = "Fatick"
     4 = "Kaffrine"
     5 = "Kaokack"
     6 = "Kédougou"
     7 = "Kolda"
     8 = "Louga"
     9 = "Matam"
    10 = "Saint Louis"
    11 = "Sediou"
    12 = "Tambacounda"
    13 = "Thiès"
    14 = "Ziguinchor"
     ;
  value F00002_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00003_
     1 = "Hospital"
     2 = "Health centre"
     3 = "Clinic"
     4 = "Case de santé"
     ;
  value F00004_
     1 = "Government/public"
     2 = "NGO/ Private not for profit"
     3 = "Private for profit"
     4 = "Mission/ faith-based"
     ;
  value F00005_
     1 = "Both MCH and HIV SPAs (multiple units and services)"
     2 = "MCH SPA only"
     3 = "HIV SPA only"
     4 = "Both MCH and HIV SPAs (single units and services in inventory)"
     5 = "New SPA qre with renamed variables, no multiples in facility level"
     ;
  value F00006_
     1 = "Facility completed"
     2 = "Facility respondent not available"
     3 = "Partially completed"
     4 = "Facility refused"
     5 = "Facility closed/ not yet functional"
     6 = "Other"
     ;
  value F00007_
     1 = "Outpatient"
     2 = "Facility has only Inpatient"
     ;
  value F00008_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00009_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00010_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00011_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00012_
     0 = "No service"
     1 = "Yes,both male and female"
     2 = "Yes, female only"
     3 = "Yes, male only"
     7 = "Not returned from field"
     ;
  value F00013_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00014_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00015_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00016_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00017_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00018_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00019_
     0 = "Contraceptives not stocked"
     1 = "Stored in contraceptive service site"
     2 = "Stored with other medicines"
     ;
  value F00020_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00021_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00022_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00023_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00024_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00025_
     0 = "TB medicines not stocked"
     1 = "Stored in TB service site"
     2 = "Stored with other medicines"
     ;
  value F00026_
     0 = "ARV medicines not stocked"
     1 = "Stored in ART service site"
     2 = "Stored with other medicines"
     3 = "Stored in PMTCT service site"
     4 = "Stored in BOTH (ART and PMTCT) service sites"
     ;
  value F00027_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00028_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00029_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00030_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00031_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00032_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00033_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00034_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00035_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00036_
     1 = "January"
     2 = "February"
     3 = "March"
     4 = "April"
     5 = "May"
     6 = "June"
     7 = "July"
     8 = "August"
     9 = "September"
    10 = "October"
    11 = "November"
    12 = "December"
     ;
  value F00037_
     0 = "Case de Sante - subset of SPA questionnaire"
     1 = "Full SPA for facilities"
     ;
  value F00038_
     0 = "No 24 hour staff"
     1 = "Yes, 24 hours, duty schedule observed"
     2 = "Yes, 24 hours, duty schedule reported, not seen"
     3 = "Yes, 24 hours, duty schedule not maintained"
     ;
  value F00039_
    96 = "Other"
     ;
  value F00040_
  9998 = "Don't know"
     ;
  value F00041_
  9998 = "Don't know"
     ;
  value F00042_
  9998 = "Don't know"
     ;
  value F00043_
  9998 = "Don't know"
     ;
  value F00044_
  9998 = "Don't know"
     ;
  value F00045_
     0 = "Has none"
     1 = "Monthly or more often"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less often than every 6 months/irregularly"
     8 = "Don't know frequency"
     ;
  value F00046_
     0 = "No record maintained"
     1 = "Record observed"
     2 = "Reported, not seen"
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
     8 = "Don't know"
     ;
  value F00058_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00059_
     0 = "Has none"
     1 = "Yes, Monthly or more often"
     2 = "Yes, Every 2-3 months"
     3 = "Yes, Every 4-6 months"
     4 = "Yes, Less often than every 6 months/irregularly"
     6 = "Yes, don't know frequency"
     8 = "Don't know if any such meetings"
     ;
  value F00060_
     0 = "No record maintained"
     1 = "Record observed"
     2 = "Reported, not seen"
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
     0 = "No procedure"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00073_
     0 = "No record maintained"
     1 = "Record observed"
     2 = "Reported, not seen"
     ;
  value F00074_
     0 = "None"
     1 = "Yes, record observed"
     3 = "Yes, record reported, not seen"
     4 = "Yes, no records maintained"
     8 = "Don't know"
     ;
  value F00075_
     0 = "Never supervised from outside facility"
     1 = "Within past 6 months"
     2 = "More than 6 months ago"
     ;
  value F00076_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00077_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00078_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00079_
     0 = "Not connected"
     1 = "Connected, always available"
     2 = "Connected, sometimes interrupted"
     3 = "Connected, DK if interrupted in last 7 days"
     8 = "Don't know if connected"
     ;
  value F00080_
     0 = "No backup generator"
     5 = "Reported functional with fuel"
     6 = "Reported functional, no fuel, DK fuel"
     7 = "Reported not functional"
     8 = "Reported, don't know if functional"
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
     0 = "No water source"
    13 = "Piped into facility"
    14 = "Piped onto facility grounds"
    15 = "Public tap/standpipe"
    20 = "Protected well"
    21 = "Unprotected well"
    22 = "Tubewell/borehole undefined"
    23 = "Protected spring"
    24 = "Unprotected spring"
    30 = "River/lake/pond"
    31 = "Rain water"
    32 = "Bottled water"
    41 = "Cart/small tank/drum"
    42 = "Tanker truck"
    96 = "Other"
    98 = "Don't know"
     ;
  value F00085_
     0 = "No, beyond 500 m of facility"
     1 = "Yes, onsite"
     2 = "Yes, within 500 m of facility"
     ;
  value F00086_
     0 = "No"
     1 = "Yes"
     ;
  value F00087_
     0 = "No landline"
     1 = "Functioning observed landline"
     2 = "Non-functioning observed landline"
     3 = "Functioning reported landline"
     4 = "Non-functioning reported landline"
     ;
  value F00088_
     0 = "No cell phone"
     1 = "Functioning observed cellphone"
     2 = "Non-functioning observed cell phone"
     3 = "Functioning reported cell phone"
     4 = "Non-functioning reported cellphone"
     5 = "Functioning landline observed or reported"
     ;
  value F00089_
     0 = "No shortwave radio"
     1 = "Functioning observed shortwave radio"
     2 = "Non-functioning observed shortwave radio"
     3 = "Functioning reported shortwave radio"
     4 = "Non-functioning reported shortwave radio"
     5 = "Some functioning cell or landline phone observed or reported"
     ;
  value F00090_
     0 = "No functioning landline, cell phone or shortwave at facility"
     1 = "Observed functioning communication"
     3 = "Reported functioning communication"
     ;
  value F00091_
     0 = "No computer"
     1 = "Yes, functioning"
     2 = "Yes, but not functioning"
     ;
  value F00092_
     0 = "No computer"
     1 = "Yes, observed"
     2 = "Yes, reported, not seen"
     ;
  value F00093_
     0 = "No"
     1 = "Yes at least 2 hours on client service days"
     2 = "Yes, less than 2 hours on client service days"
     ;
  value F00094_
     0 = "No, no designated person"
     1 = "Yes, data manager/HMIS person"
     2 = "Yes, facility in-charge"
     3 = "Yes, other service provider"
     ;
  value F00095_
     0 = "No"
     1 = "Yes"
     ;
  value F00096_
     0 = "Never"
     1 = "Monthly or more often"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less often than every 6 months"
     ;
  value F00097_
     0 = "No reports compiled"
     1 = "Yes, observed"
     2 = "Yes, reported not seen"
     ;
  value F00098_
 99998 = "Don't know"
     ;
  value F00099_
 99998 = "Don't know"
     ;
  value F00100_
     0 = "No"
     1 = "Yes"
     2 = "Overnight observation - no routine inpatient"
     ;
  value F00101_
     0 = "No overnight observation"
   990 = "990 or more"
  9998 = "Don't know number of beds"
     ;
  value F00102_
     0 = "No"
     1 = "Yes, fixed fee covering all services"
     2 = "Yes, separate fees for separate items"
     ;
  value F00103_
     0 = "No"
     1 = "Yes"
     ;
  value F00104_
     0 = "No"
     1 = "Yes"
     ;
  value F00105_
     0 = "No"
     1 = "Yes"
     ;
  value F00106_
     0 = "No"
     1 = "Yes"
     ;
  value F00107_
     0 = "No"
     1 = "Yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00120_
     0 = "No fees posted"
     1 = "Yes, observed all fees"
     2 = "Yes, observed some fees"
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
     8 = "Don't know"
     ;
  value F00127_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00128_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00129_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00130_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00131_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00132_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00133_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00134_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00135_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00136_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00137_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00138_
     0 = "No ambulance, no access to other vehicle"
     4 = "Ambulance observed with fuel"
     5 = "Ambulance reported with  fuel"
     6 = "Ambulance obsd without/DK fuel"
     7 = "Ambulance reptd without/DK fuel"
     8 = "No ambulance, reptd access other vehicle"
     ;
  value F00139_
     0 = "No"
     1 = "Yes, protected waiting area"
     ;
  value F00140_
     0 = "No functioning facility"
     1 = "Yes"
     ;
  value F00141_
     0 = "No functioning facility"
    11 = "Flush to piped sewer system"
    12 = "Flush to septic tank"
    13 = "Flush to pit latrine"
    14 = "Flush to somewhere else"
    15 = "Flush, don't know where"
    21 = "Ventilated improved pit latrine"
    22 = "Pit latrine with slab"
    23 = "Pit latrine without slab/ open pit"
    31 = "Composting toilet"
    41 = "Bucket toilet"
    51 = "Hanging toilet/ hanging latrine"
     ;
  value F00142_
     0 = "Cleanliness: none of 8"
     1 = "Cleanliness: 1 of 8"
     2 = "Cleanliness: 2 of 8"
     3 = "Cleanliness: 3 of 8"
     4 = "Cleanliness: 4 of 8"
     5 = "Cleanliness: 5 of 8"
     6 = "Cleanliness: 6 of 8"
     7 = "Cleanliness: 7 of 8"
     8 = "Cleanliness: all possible"
     ;
  value F00143_
     0 = "No"
     1 = "Yes"
     ;
  value F00144_
     0 = "No"
     1 = "Yes"
     ;
  value F00145_
     0 = "No"
     1 = "Yes"
     ;
  value F00146_
     0 = "No"
     1 = "Yes"
     ;
  value F00147_
     0 = "No"
     1 = "Yes"
     ;
  value F00148_
     0 = "No"
     1 = "Yes"
     ;
  value F00149_
     0 = "No"
     1 = "Yes"
     ;
  value F00150_
     0 = "No"
     1 = "Yes"
     ;
  value F00151_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00152_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00153_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00154_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00155_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00156_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00157_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00158_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00159_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00160_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00161_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00162_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00163_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00164_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00165_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00166_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00167_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00168_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00169_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00170_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00171_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00172_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00173_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00174_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00175_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00176_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00177_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00178_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00179_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00180_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00181_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00182_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00183_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00184_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00185_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00186_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00187_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00188_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00189_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00190_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00191_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00192_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00193_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00194_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00195_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00196_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00197_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00198_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00199_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00200_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00201_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00202_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00203_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00204_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00205_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00206_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00207_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00208_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00209_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00210_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00211_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00212_
     0 = "Process no equipment"
     1 = "Process inside facility only"
     2 = "Process both in and out of facility"
     3 = "Process outside facility only"
     ;
  value F00213_
     0 = "Do not sterilize"
     1 = "Sterilize, see table for method used"
     2 = "Process outside of facility"
     ;
  value F00214_
     1 = "Dry heat sterilization"
     2 = "Autoclave"
     3 = "Boil or steam"
     4 = "Boil"
     5 = "Steam without pressure"
     6 = "Chemical HLD or sterilization"
     7 = "Decontamination"
     8 = "Other"
     ;
  value F00215_
     0 = "No"
     1 = "Yes"
     ;
  value F00216_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00217_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00218_
     1 = "Kg/sq cm"
     2 = "ATM pressure"
     3 = "Kilopascal"
     4 = "Millimeter HG"
     8 = "DK"
     ;
  value F00219_
   666 = "Automatic"
   995 = "Never used"
   998 = "Don't know"
     ;
  value F00220_
   666 = "Automatic"
   995 = "Never used"
   998 = "Don't know"
     ;
  value F00221_
     2 = "Chlorine (8/9 %)"
     3 = "Cidex"
     4 = "Betadine"
     5 = "Alcohol"
    10 = "Gluteraldehyde"
    13 = "Formaldehyde"
    98 = "Don't know"
     ;
  value F00222_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00223_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00224_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00225_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00226_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00227_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00228_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00229_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00230_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00231_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00232_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00233_
     2 = "Burn incinerator: 2-chamber industrial"
     3 = "Burn incinerator: 1-chamber drum/brick"
     4 = "Open burn: Flat ground-no protection"
     5 = "Open burn: Pit or protected Ground"
     6 = "Dump: flat ground - no protection"
     7 = "Dump: covered pit/pit latrine"
     8 = "Dump: open pit/no protection"
     9 = "Dump: protected ground/pit"
    10 = "Offsite: stored in covered container"
    11 = "Offsite: Stored in other protected environment"
    12 = "Offsite: Stored unprotected"
    95 = "Never have sharps waste"
    96 = "Other"
     ;
  value F00234_
     0 = "No waste visible"
     1 = "Waste visible, protected"
     2 = "Waste visible, unprotected"
     8 = "Waste site not inspected"
     ;
  value F00235_
     2 = "Burn incinerator: 2-chamber industrial"
     3 = "Burn incinerator: 1-chamber drum/brick"
     4 = "Open burn: Flat ground-no protection"
     5 = "Open burn: Pit or protected Ground"
     6 = "Dump: flat ground - no protection"
     7 = "Dump: covered pit/pit latrine"
     8 = "Dump: open pit/no protection"
     9 = "Dump: protected ground/pit"
    10 = "Offsite: stored in covered container"
    11 = "Offsite: Stored in other protected environment"
    12 = "Offsite: Stored unprotected"
    95 = "Never have infectious waste"
    96 = "Other"
     ;
  value F00236_
     0 = "No waste visible"
     1 = "Waste visible, protected"
     2 = "Waste visible, unprotected"
     8 = "Waste site not inspected"
     ;
  value F00237_
     1 = "Observed functioning with fuel"
     2 = "Observed functioning, no or DK fuel"
     3 = "Observed, not or DK if functioning"
     4 = "Reported, functioning with fuel"
     5 = "Reported, function, no or DK fuel"
     6 = "Reported,  not or DK if functioning"
     ;
  value F00238_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported, not seen"
     ;
  value F00239_
     0 = "No"
     1 = "Yes"
     ;
  value F00240_
     0 = "No"
     1 = "Yes"
     ;
  value F00241_
     0 = "No"
     1 = "Yes"
     ;
  value F00242_
     0 = "No"
     1 = "Yes"
     ;
  value F00243_
     0 = "No"
     1 = "Yes"
     ;
  value F00244_
     0 = "No"
     1 = "Yes"
     ;
  value F00245_
     0 = "No"
     1 = "Yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00270_
     0 = "No"
     1 = "Yes"
     ;
  value F00271_
     0 = "no"
     1 = "yes"
     ;
  value F00272_
     0 = "no"
     1 = "yes"
     ;
  value F00273_
     0 = "no"
     1 = "yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00298_
     0 = "No"
     1 = "Yes"
     ;
  value F00299_
     0 = "No"
     1 = "Yes"
     ;
  value F00300_
     0 = "No"
     1 = "Yes"
     ;
  value F00301_
     0 = "No"
     1 = "Yes"
     ;
  value F00302_
     0 = "No"
     1 = "Yes"
     ;
  value F00303_
     0 = "No"
     1 = "Yes"
     ;
  value F00304_
     0 = "No"
     1 = "Yes"
     ;
  value F00305_
     0 = "No"
     1 = "Yes"
     ;
  value F00306_
     0 = "No"
     1 = "Yes"
     ;
  value F00307_
     0 = "No"
     1 = "Yes"
     ;
  value F00308_
     0 = "No"
     1 = "Yes"
     ;
  value F00309_
     0 = "No"
     1 = "Yes"
     ;
  value F00310_
     0 = "No"
     1 = "Yes"
     ;
  value F00311_
     0 = "No"
     1 = "Yes"
     ;
  value F00312_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F00313_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00314_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00315_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00316_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00317_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00318_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00319_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00320_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00321_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00322_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00323_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00324_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00325_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00326_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00327_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00328_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00329_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00330_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00331_
     0 = "No"
     1 = "Yes"
     ;
  value F00332_
     0 = "No"
     1 = "Yes"
     ;
  value F00333_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00334_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00335_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00336_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00337_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00338_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00339_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00340_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00341_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F00342_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00343_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00344_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00345_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00346_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00347_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00348_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00349_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00350_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00351_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00352_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00353_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00354_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00355_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00356_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00357_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00358_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00359_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00360_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00361_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00362_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00363_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00364_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00365_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00366_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00367_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00368_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00369_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00370_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00371_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00372_
     0 = "No"
     1 = "Yes"
     ;
  value F00373_
     0 = "No"
     1 = "Yes"
     ;
  value F00374_
     0 = "No"
     1 = "Yes"
     ;
  value F00375_
     0 = "No"
     1 = "Yes"
     ;
  value F00376_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00377_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00378_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00379_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00380_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00381_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00382_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00383_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00384_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00385_
     0 = "No"
     1 = "Yes, routinely stores vaccines"
     ;
  value F00386_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00387_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00388_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00389_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00390_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00391_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00392_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00393_
     0 = "No"
     1 = "Yes"
     ;
  value F00394_
     0 = "None observed"
     1 = "Yes, observed"
     ;
  value F00395_
     1 = "Between +2 and +8 degrees"
     2 = "Above +8 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functional"
     5 = "Vaccine refrigerator not seen"
     ;
  value F00396_
     0 = "No"
     1 = "Yes, observed and completed twice daily for past 30 days"
     2 = "Yes, observed, missing whether completed twice daily in past 30 days"
     3 = "Yes, observed, not completed twice daily for past 30 days"
     4 = "Yes, reported"
     ;
  value F00397_
     0 = "None"
     1 = "One"
     2 = "Two or more"
     ;
  value F00398_
     0 = "Have none"
     1 = "Yes, one set"
     2 = "Yes, 2 or more sets"
     3 = "No, use purchased ice"
     ;
  value F00399_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00400_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00401_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00402_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00403_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00404_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00405_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00406_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00407_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00408_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00409_
     0 = "No"
     3 = "Prescribe, counsel, refer"
     ;
  value F00410_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00411_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00412_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00413_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00414_
     0 = "No"
     1 = "Yes"
     ;
  value F00415_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00416_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00417_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00418_
     0 = "No treatment"
     1 = "Routinely diagnose and treat STIs"
     2 = "Refers to other provider/location/outside facility"
     ;
  value F00419_
     0 = "No diagnosis/ treatment/ referral"
     1 = "Routinely diagnose and treat STIs"
     2 = "Diagnose but refer elsewhere for treatment"
     3 = "Refer elsewhere in facility for diagnosis and treatment"
     4 = "Refer outside facility for diagnosis and treatment"
     ;
  value F00420_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00421_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00422_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00423_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00424_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00425_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00426_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00427_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00428_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00429_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F00430_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00431_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00432_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00433_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00434_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00435_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00436_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00437_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00438_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00439_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00440_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00441_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00442_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00443_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00444_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00445_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00446_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00447_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00448_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00449_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00450_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00451_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00452_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00453_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00454_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00455_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00456_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00457_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00458_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00459_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00460_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00461_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00462_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00463_
     0 = "No FP equipment processed in facility"
     1 = "FP service site"
     2 = "Central location in facility"
     3 = "Both FP and central locations"
     ;
  value F00464_
     0 = "No"
     1 = "Yes"
     ;
  value F00465_
     0 = "No"
     1 = "Yes"
     ;
  value F00466_
     0 = "No"
     1 = "Yes"
     ;
  value F00467_
     0 = "No"
     1 = "Yes"
     ;
  value F00468_
     0 = "No"
     1 = "Yes"
     ;
  value F00469_
     0 = "No"
     1 = "Yes"
     ;
  value F00470_
     0 = "No HIV RTD done in FP service area"
     1 = "HIV RTD kit observed & valid"
     3 = "HIV RTD kit observed, none valid"
     4 = "HIV RTD kit reported available"
     5 = "HIV RTD kit not available today"
     6 = "HIV RTD kit used, missing valid"
     ;
  value F00471_
     0 = "No"
     1 = "Yes in family planning service"
     2 = "Yes in pharmacy"
     ;
  value F00472_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F00473_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
     ;
  value F00474_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00475_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00476_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00477_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00478_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00479_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00480_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00481_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00482_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00483_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00484_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00485_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F00486_
     0 = "No"
     1 = "Yes"
     ;
  value F00487_
     0 = "No"
     1 = "Yes"
     ;
  value F00488_
     0 = "No"
     1 = "Yes"
     ;
  value F00489_
     0 = "No"
     1 = "Yes"
     ;
  value F00490_
     0 = "No"
     1 = "Yes"
     ;
  value F00491_
     0 = "No"
     1 = "Yes"
     ;
  value F00492_
     0 = "No card"
     1 = "Yes, observed"
     2 = "Reported, not seen"
     ;
  value F00493_
     0 = "No file"
     1 = "Yes, observed"
     2 = "Reported, not seen"
     ;
  value F00494_
     0 = "No"
     1 = "Yes"
     ;
  value F00495_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00496_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00497_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00498_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00499_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00500_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00501_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00502_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00503_
     0 = "No"
     1 = "Yes"
     ;
  value F00504_
     0 = "No"
     1 = "Yes"
     ;
  value F00505_
     0 = "No"
     1 = "Yes"
     ;
  value F00506_
     0 = "No"
     1 = "Yes"
     ;
  value F00507_
     0 = "No"
     1 = "Yes"
     ;
  value F00508_
     0 = "No"
     1 = "Yes"
     ;
  value F00509_
     0 = "No"
     1 = "Yes"
     ;
  value F00510_
     0 = "No"
     1 = "Yes"
     ;
  value F00511_
     0 = "No"
     1 = "Yes"
     ;
  value F00512_
     0 = "No  (See V410A)"
     1 = "Routinely diagnose and treats"
     2 = "Refer other provider/service/facility"
     ;
  value F00513_
     0 = "No diagnosis, treatment, referral"
     1 = "Routinely diagnose and treat in service area"
     2 = "Diagnose, but refer elsewhere for treatment"
     3 = "Refer elsewhere in facility for diagnosis and treatment"
     4 = "Refer outside facility for diagnosis and treatment"
     ;
  value F00514_
     0 = "Not all days or never offered"
     1 = "Yes"
     ;
  value F00515_
     0 = "Less often than 1 day week"
     ;
  value F00516_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00517_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F00518_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00519_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00520_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00521_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00522_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00523_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00524_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00525_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00526_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00527_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00528_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00529_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00530_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00531_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00532_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00533_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00534_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00535_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00536_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00537_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00538_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00539_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00540_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00541_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00542_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00543_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00544_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00545_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00546_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00547_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00548_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00549_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00550_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00551_
     0 = "No"
     1 = "Yes"
     ;
  value F00552_
     0 = "No"
     1 = "Yes"
     ;
  value F00553_
     0 = "No"
     1 = "Yes"
     ;
  value F00554_
     0 = "No"
     1 = "Yes"
     ;
  value F00555_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00556_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00557_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00558_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00559_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00560_
   998 = "Don't know"
     ;
  value F00561_
     0 = "No"
     1 = "Yes"
     ;
  value F00562_
     0 = "No schedule"
     1 = "Schedule observed"
     3 = "Schedule reported, not seen"
     ;
  value F00563_
     0 = "No"
     1 = "Yes"
     ;
  value F00564_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00565_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00566_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00567_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00568_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00569_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00570_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00571_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00572_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00573_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00574_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00575_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00576_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00577_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F00578_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00579_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00580_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00581_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00582_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00583_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00584_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00585_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00586_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00587_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00588_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00589_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00590_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00591_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00592_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00593_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00594_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00595_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00596_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00597_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00598_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00599_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00600_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00601_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00602_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00603_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00604_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00605_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00606_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00607_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00608_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00609_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00610_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00611_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00612_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00613_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00614_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00615_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00616_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00617_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00618_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00619_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00620_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00621_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00622_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00623_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00624_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00625_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00626_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00627_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00628_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00629_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00630_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00631_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00632_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00633_
     0 = "Does not review"
     1 = "Reviews both"
     2 = "Reviews maternal only"
     3 = "Reviews newborns only"
     ;
  value F00634_
    53 = "When case occurs"
    98 = "Don't know"
     ;
  value F00635_
    53 = "When case occurs"
    95 = "Always with Maternal Reviews"
    98 = "Don't know"
     ;
  value F00636_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00637_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00638_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00639_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00640_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00641_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00642_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00643_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00644_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00645_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00646_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00647_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00648_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00649_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00650_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00651_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00652_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00653_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00654_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00655_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00656_
     0 = "No"
     1 = "Yes, observed schedule"
     3 = "Yes, reported"
     5 = "24-hour schedule not  maintained"
     ;
  value F00657_
     0 = "No"
     1 = "Yes, observed schedule"
     3 = "Yes, reported"
     5 = "24-hour schedule not  maintained"
     ;
  value F00658_
     0 = "Not used"
     1 = "Yes, in separate room"
     2 = "Yes, integrated"
     ;
  value F00659_
     0 = "Not used"
     1 = "Used routinely"
     2 = "Used selectively"
     ;
  value F00660_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00661_
     0 = "No"
     1 = "Yes  (See V1280+)"
     ;
  value F00662_
     0 = "Neither diagnosed nor treated"
     1 = "Diagnosed and treated"
     2 = "Diagnosed only"
     3 = "Treated only"
     ;
  value F00663_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00664_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00665_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00666_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00667_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00668_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00669_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00670_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00671_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00672_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00673_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00674_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00675_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00676_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00677_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00678_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00679_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00680_
     0 = "No"
     1 = "Yes, routinely"
     2 = "Only if suspected HIV"
     ;
  value F00681_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F00682_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00683_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00684_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00685_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00686_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00687_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00688_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00689_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00690_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00691_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00692_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00693_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00694_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00695_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00696_
     0 = "No"
     1 = "Yes"
     ;
  value F00697_
     0 = "No"
     1 = "Yes"
     ;
  value F00698_
     0 = "No"
     1 = "Yes"
     ;
  value F00699_
     0 = "No"
     1 = "Yes"
     ;
  value F00700_
     0 = "No"
     1 = "Yes, always or sometimes active"
     2 = "Yes, only passive"
     8 = "Don't know"
     ;
  value F00701_
     0 = "No"
     1 = "Yes, always active"
     2 = "Yes, sometimes active"
     3 = "Yes, only passive"
     8 = "Don't know"
     ;
  value F00702_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00703_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00704_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00705_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00706_
     0 = "No"
     1 = "Yes"
     ;
  value F00707_
     0 = "No"
     1 = "Yes"
     ;
  value F00708_
     0 = "No"
     1 = "Yes"
     ;
  value F00709_
     0 = "No"
     1 = "Yes"
     ;
  value F00710_
     0 = "No"
     1 = "Yes, no external QA"
     ;
  value F00711_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00712_
    95 = "No error rate provided"
    98 = "Don't know"
     ;
  value F00713_
     0 = "No"
     1 = "Yes"
     ;
  value F00714_
     0 = "No"
     1 = "Yes"
     ;
  value F00715_
     0 = "No"
     1 = "Yes"
     ;
  value F00716_
     0 = "No"
     1 = "Yes"
     ;
  value F00717_
     0 = "No"
     1 = "Yes"
     ;
  value F00718_
     0 = "No"
     1 = "Yes"
     ;
  value F00719_
     0 = "No"
     1 = "Yes"
     ;
  value F00720_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00721_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00722_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00723_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00724_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00725_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00726_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00727_
     0 = "No, send blood out"
     1 = "Yes, send no blood out for screening"
     ;
  value F00728_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00729_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00730_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00731_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00732_
     0 = "No, send blood/urine out"
     1 = "Yes, send no blood/urine out"
     ;
  value F00733_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00734_
     0 = "No record"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00735_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00736_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00737_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00738_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00739_
     0 = "No, send blood/urine out"
     1 = "Yes, send no blood/urine out"
     ;
  value F00740_
     0 = "No"
     1 = "Yes"
     ;
  value F00741_
     0 = "No"
     1 = "Yes"
     ;
  value F00742_
     0 = "No"
     1 = "Yes"
     ;
  value F00743_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00744_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00745_
     0 = "No"
     1 = "Yes"
     ;
  value F00746_
     0 = "No"
     1 = "Yes"
     ;
  value F00747_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00748_
     0 = "No"
     1 = "Yes"
     ;
  value F00749_
     0 = "No"
     1 = "Yes"
     ;
  value F00750_
     0 = "No"
     1 = "Yes"
     ;
  value F00751_
     0 = "No"
     1 = "Yes"
     ;
  value F00752_
     0 = "No"
     1 = "Yes"
     ;
  value F00753_
     0 = "No"
     1 = "Yes"
     ;
  value F00754_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00755_
     0 = "No"
     1 = "Yes"
     ;
  value F00756_
     0 = "No"
     1 = "Yes"
     ;
  value F00757_
     0 = "No"
     1 = "Yes"
     ;
  value F00758_
     0 = "No"
     1 = "Yes"
     ;
  value F00759_
     0 = "No screening tests done"
     1 = "Yes, screened in facility"
     2 = "Blood screened outside facility"
     3 = "Yes, blood screened both inside and outside"
     ;
  value F00760_
     0 = "No"
     1 = "Yes"
     ;
  value F00761_
     0 = "No"
     1 = "Yes"
     ;
  value F00762_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00763_
     0 = "No"
     1 = "Yes, no HIV tests (lab, RDT, DBS)"
     ;
  value F00764_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00765_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00766_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00767_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00768_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00769_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00770_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00771_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00772_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00773_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00774_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00775_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00776_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00777_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00778_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00779_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00780_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00781_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00782_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00783_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00784_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00785_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00786_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00787_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00788_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00789_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00790_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00791_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00792_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00793_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00794_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00795_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00796_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00797_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00798_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00799_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00800_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00801_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00802_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00803_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00804_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00805_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00806_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00807_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00808_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00809_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00810_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00811_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00812_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00813_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00814_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00815_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00816_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00817_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00818_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00819_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00820_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00821_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00822_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00823_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00824_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00825_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00826_
     0 = "Not used"
     1 = "Equipment used and observed valid"
     3 = "Equipment used & observed, none valid"
     4 = "Equipment used reported, not seen"
     5 = "Equipment used, not available today"
     6 = "Equipment used, missing if items available"
     ;
  value F00827_
     0 = "Not used"
     1 = "Equipment used and observed valid"
     3 = "Equipment used & observed, none valid"
     4 = "Equipment used reported, not seen"
     5 = "Equipment used, not available today"
     6 = "Equipment used, missing if items available"
     ;
  value F00828_
     0 = "Not used"
     1 = "Equipment used and observed valid"
     3 = "Equipment used & observed, none valid"
     4 = "Equipment used reported, not seen"
     5 = "Equipment used, not available today"
     6 = "Equipment used, missing if items available"
     ;
  value F00829_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00830_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00831_
     0 = "Not used"
     1 = "Yes, See V840G1"
     ;
  value F00832_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00833_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00834_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00835_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00836_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00837_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00838_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00839_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00840_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00841_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00842_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00843_
     0 = "Not used"
     1 = "Test used"
     ;
  value F00844_
     0 = "Not used"
     1 = "Yes, see V840P"
     ;
  value F00845_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00846_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00847_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00848_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00849_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00850_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00851_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00852_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00853_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00854_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00855_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00856_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00857_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00858_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F00859_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F00860_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F00861_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F00862_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F00863_
     0 = "No"
     1 = "Yes"
     ;
  value F00864_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00865_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00866_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00867_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00868_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00869_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00870_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00871_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00872_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00873_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00874_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00875_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00876_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00877_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00878_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00879_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00880_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00881_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00882_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00883_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00884_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00885_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00886_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00887_
     0 = "No"
     1 = "Yes"
     ;
  value F00888_
     0 = "No"
     1 = "Yes"
     ;
  value F00889_
     0 = "No"
     1 = "Yes"
     ;
  value F00890_
     0 = "No"
     1 = "Yes"
     ;
  value F00891_
     0 = "No"
     1 = "Yes"
     ;
  value F00892_
     0 = "No"
     1 = "Yes"
     ;
  value F00893_
     0 = "No"
     1 = "Yes"
     ;
  value F00894_
     0 = "No"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00895_
     0 = "No"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "None available today"
     ;
  value F00896_
     0 = "No system"
     1 = "Internal QC only"
     2 = "External QC only"
     3 = "Both internal and external QC"
     4 = "Send slide for re-reading"
     6 = "Other"
     7 = "Type of system missing"
     ;
  value F00897_
     0 = "No records"
     1 = "Records for IQC only"
     2 = "Records for EQC only"
     3 = "Records for both internal and external QC"
     4 = "Type of records missing"
     ;
  value F00898_
     0 = "No"
     1 = "Yes"
     ;
  value F00899_
     0 = "No"
     1 = "Yes"
     ;
  value F00900_
     0 = "No"
     1 = "Yes"
     ;
  value F00901_
     0 = "No"
     1 = "Yes"
     ;
  value F00902_
     0 = "No"
     1 = "Yes"
     ;
  value F00903_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00904_
     1 = "Between +2 and +6 degrees"
     2 = "Above + 6 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functioning"
     ;
  value F00905_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00906_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F00907_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
     ;
  value F00908_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00909_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00910_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00911_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00912_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00913_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00914_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00915_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00916_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00917_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00918_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00919_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00920_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00921_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00922_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00923_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00924_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00925_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00926_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00927_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00928_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00929_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00930_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00931_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00932_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00933_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00934_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00935_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00936_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00937_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00938_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00939_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00940_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00941_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00942_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00943_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00944_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00945_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00946_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00947_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00948_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00949_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00950_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00951_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00952_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00953_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00954_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00955_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00956_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00957_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00958_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00959_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00960_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00961_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00962_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00963_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00964_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00965_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00966_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00967_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00968_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00969_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00970_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00971_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00972_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00973_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00974_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00975_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00976_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00977_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00978_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00979_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00980_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00981_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00982_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00983_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00984_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00985_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00986_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00987_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00988_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00989_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00990_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00991_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00992_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00993_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00994_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00995_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00996_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00997_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00998_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00999_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01000_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01001_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01002_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01003_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01004_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01005_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01006_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01007_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01008_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01009_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01010_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01011_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01012_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01013_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01014_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01015_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01016_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01017_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01018_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01019_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01020_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01021_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01022_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01023_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01024_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01025_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01026_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01027_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01028_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01029_
     0 = "No"
     1 = "Yes"
     ;
  value F01030_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F01031_
     0 = "No"
     1 = "Yes"
     ;
  value F01032_
     0 = "No"
     1 = "Yes"
     ;
  value F01033_
     0 = "No"
     1 = "Yes"
     ;
  value F01034_
     0 = "No"
     1 = "Yes"
     ;
  value F01035_
     0 = "No"
     1 = "Yes"
     ;
  value F01036_
     0 = "No"
     1 = "Yes, type of client not defined"
     ;
  value F01037_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01038_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01039_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01040_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01041_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01042_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01043_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01044_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01045_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01046_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F01047_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
     ;
  value F01048_
     0 = "No"
     1 = "Yes"
     ;
  value F01049_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F01050_
     0 = "No"
     1 = "Yes"
     ;
  value F01051_
     0 = "No"
     1 = "Yes"
     ;
  value F01052_
     0 = "No"
     1 = "Yes"
     ;
  value F01053_
     0 = "No"
     1 = "Yes"
     ;
  value F01054_
     0 = "No"
     1 = "Yes"
     ;
  value F01055_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01056_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01057_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01058_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01059_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01060_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01061_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01062_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01063_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01064_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01065_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01066_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01067_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01068_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01069_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01070_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01071_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01072_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01073_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01074_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01075_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01076_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01077_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01078_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01079_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01080_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01081_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01082_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01083_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01084_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01085_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01086_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01087_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01088_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01089_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01090_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01091_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01092_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01093_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01094_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01095_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F01096_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
     ;
  value F01097_
     0 = "No"
     1 = "Yes"
     ;
  value F01098_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F01099_
     0 = "No"
     1 = "Yes"
     ;
  value F01100_
     0 = "No"
     1 = "Yes"
     ;
  value F01101_
     0 = "No"
     1 = "Yes"
     ;
  value F01102_
     0 = "No"
     1 = "Yes"
     ;
  value F01103_
     0 = "No"
     1 = "Yes"
     ;
  value F01104_
     0 = "Neither diagnose nor prescribe tx"
     1 = "Diagnose and prescribe tx"
     2 = "Diagnose only"
     3 = "Prescribe tx only"
     ;
  value F01105_
     0 = "No"
     1 = "Always"
     2 = "Only sometimes"
     ;
  value F01106_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01107_
     0 = "No"
     1 = "Yes"
     ;
  value F01108_
     0 = "No"
     1 = "Yes"
     ;
  value F01109_
     0 = "No"
     1 = "Yes"
     ;
  value F01110_
     0 = "No"
     1 = "Yes"
     ;
  value F01111_
     0 = "No"
     1 = "Yes"
     ;
  value F01112_
     0 = "No"
     1 = "Yes"
     ;
  value F01113_
     0 = "No"
     1 = "Yes"
     ;
  value F01114_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01115_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01116_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01117_
     1 = "Diagnose, refer and treat"
     2 = "Diagnose and refer"
     3 = "Diagnose and treat"
     4 = "Refer and treat"
     5 = "Diagnose only"
     6 = "Refer only"
     7 = "Treat only"
     ;
  value F01118_
     0 = "No TB diagnosis services"
     1 = "Sputum smear only"
     2 = "X-ray only"
     3 = "Either sputum or x-ray"
     4 = "Both sputum and x-ray"
     5 = "Clinical symptoms only"
     6 = "Refer to outside facility"
     ;
  value F01119_
     0 = "No"
     1 = "Yes"
     ;
  value F01120_
     0 = "No record"
     1 = "Yes, observed paper or electronic"
     2 = "Yes, reported, not seen"
     ;
  value F01121_
     1 = "Paper"
     2 = "Computer"
     ;
  value F01122_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01123_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01124_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01125_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01126_
     0 = "No treatment or prescription"
     1 = "Direct observe 2M intensive, FU 4M continuation"
     2 = "Direct observe 6M intensive"
     3 = "Follow up clients only after first 2M intensive direct observation elsewhere"
     4 = "Diagnose and treat while inpatient. Discharge to other clinic/unit for F/up"
     5 = "Provide full tx with no routine direct obs phase"
     6 = "Dx, prescribe/provide meds, no F/U"
     7 = "Dx only, no tx or prescription of medications"
     ;
  value F01127_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
    13 = "No TB dx/tx, no IC data"
     ;
  value F01128_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01129_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01130_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01131_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01132_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01133_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01134_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01135_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01136_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01137_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01138_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01139_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01140_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01141_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01142_
     0 = "No system"
     1 = "System or register observed"
     2 = "System or register reported, not seen"
     ;
  value F01143_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01144_
     0 = "Not available"
     1 = "Observed"
     3 = "Reported, not seen"
     4 = "Not available today"
     5 = "Have sputum container, missing if observed"
     ;
  value F01145_
     0 = "No TB meds in facility"
     1 = "Stored in service site"
     2 = "Stored in pharmacy"
     ;
  value F01146_
     0 = "No"
     1 = "Yes"
     ;
  value F01147_
     0 = "No"
     1 = "Yes"
     ;
  value F01148_
     0 = "No"
     1 = "Yes"
     ;
  value F01149_
     0 = "No"
     1 = "Yes"
     ;
  value F01150_
     1 = "Separate PMTCT services only"
     2 = "Separate PMTCT within ANC only"
     3 = "Separate PMTCT and PMTCT in delivery services"
     6 = "Separate PMTCT within ANC and delivery services"
     7 = "PMTCT only in delivery services"
     ;
  value F01151_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01152_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01153_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01154_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01155_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01156_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01157_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01158_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01159_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01160_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01161_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01162_
     0 = "No"
     1 = "Yes, no guidelines"
     ;
  value F01163_
     0 = "No delivery services"
     1 = "Yes, delivery services with PMTCT"
     3 = "Delivery services without PMTCT services"
     ;
  value F01164_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01165_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01166_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01167_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01168_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01169_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01170_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01171_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01172_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01173_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01174_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01175_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01176_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01177_
     0 = "No"
     1 = "Yes, none available"
     ;
  value F01178_
     0 = "Not available in PMTCT site"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01179_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01180_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F01181_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01182_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01183_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01184_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01185_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01186_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01187_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01188_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01189_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01190_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01191_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01192_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01193_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01194_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01195_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01196_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01197_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01198_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01199_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01200_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01201_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01202_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01203_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01204_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01205_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01206_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01207_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01208_
     0 = "No"
     1 = "Yes"
     ;
  value F01209_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01210_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F01211_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01212_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01213_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01214_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01215_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01216_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01217_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01218_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01219_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01220_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01221_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01222_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01223_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01224_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01225_
     0 = "No"
     1 = "Yes"
     ;
  value F01226_
     0 = "No"
     1 = "Yes"
     ;
  value F01227_
     0 = "No"
     1 = "Yes"
     ;
  value F01228_
     0 = "No"
     1 = "Yes"
     ;
  value F01229_
     0 = "No"
     1 = "Yes"
     ;
  value F01230_
     0 = "No"
     1 = "Yes"
     ;
  value F01231_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
    13 = "No blood drawn, no IC data"
     ;
  value F01232_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01233_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01234_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01235_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01236_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01237_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01238_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01239_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01240_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01241_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01242_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01243_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01244_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01245_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01246_
     0 = "No guidelines or protocols"
     1 = "Yes"
     ;
  value F01247_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01248_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01249_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "National protocols observed"
     ;
  value F01250_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01251_
     0 = "No HIV RTD done in HIV testing area"
     1 = "HIV RTD kit observed & valid"
     3 = "HIV RTD kit observed, none valid"
     4 = "HIV RTD kit reported available"
     5 = "HIV RTD kit not available today"
     ;
  value F01252_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01253_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01254_
     0 = "No agreement"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Verbal agreement only"
     ;
  value F01255_
     0 = "No ART services"
     1 = "Yes, both prescribe and provide follow-up"
     2 = "Yes, prescribe ART"
     3 = "Yes, provide follow-up services"
     4 = "Outreach site for providers from other facilities (no further data)"
     ;
  value F01256_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01257_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01258_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01259_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01260_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01261_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01262_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01263_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01264_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01265_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01266_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01267_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01268_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01269_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01270_
     0 = "No ART meds in facility"
     1 = "Stored in ART service site"
     2 = "Stored in pharmacy"
     3 = "Stored in PMTCT service area"
     4 = "Stored in ART and PMTCT service areas"
     ;
  value F01271_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01272_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01273_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01274_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01275_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01276_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01277_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01278_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01279_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01280_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01281_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01282_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01283_
     0 = "No"
     1 = "Yes, service monitoring in the structure"
     2 = "Yes, service monitoring in the structure and in the community"
     ;
  value F01284_
     0 = "Never provide these services or refer clients with HIV/AIDS for services"
     1 = "Yes, facility provides"
     ;
  value F01285_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01286_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01287_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01288_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01289_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01290_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01291_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01292_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01293_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01294_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01295_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01296_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01297_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01298_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01299_
     0 = "No system"
     1 = "System/ register observed"
     2 = "System/register reported, not seen"
     ;
  value F01300_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01301_
     0 = "No"
     1 = "Yes"
     ;
  value F01302_
     0 = "No"
     1 = "Diagnose and treat"
     2 = "Diagnose only"
     3 = "Treat only"
     ;
  value F01303_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01304_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01305_
     0 = "No"
     1 = "Yes"
     ;
  value F01306_
     0 = "No"
     1 = "Diagnose and treat"
     2 = "Diagnose only"
     3 = "Treat only"
     ;
  value F01307_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01308_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01309_
     0 = "No"
     1 = "Yes"
     ;
  value F01310_
     0 = "No"
     1 = "Diagnose and treat"
     2 = "Diagnose only"
     3 = "Treat only"
     ;
  value F01311_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01312_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01313_
     1 = "Collected in General information"
     2 = "Collected in N-C diseases service site"
     ;
  value F01314_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01315_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01316_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01317_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01318_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01319_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01320_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01321_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01322_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01323_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01324_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01325_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01326_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01327_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01328_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01329_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01330_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01331_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01332_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01333_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01334_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01335_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F01336_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01337_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01338_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01339_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01340_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01341_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01342_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01343_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01344_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01345_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01346_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01347_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01348_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01349_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01350_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01351_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01352_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01353_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01354_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01355_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01356_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01357_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01358_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01359_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01360_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01361_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01362_
     1 = "General information"
     2 = "Child vaccination"
     3 = "Sick child"
     4 = "Family planning"
     5 = "ANC"
     6 = "PMTCT"
     7 = "Delivery"
     8 = "STI services"
     9 = "TB"
    10 = "HIV testing"
    11 = "Non-communicable diseases"
    12 = "Minor surgery"
     ;
  value F01363_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01364_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01365_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01366_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01367_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01368_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01369_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01370_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01371_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01372_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01373_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01374_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01375_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01376_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
Data user.SNFC6IFLSR;
  attrib INV_ID   label="INVENTORY ID";
  attrib V000     length=$3 label="Survey phase";
  attrib V001     format=F00001_. label="Region (Country-specific)";
  attrib V002     label="District (Country-specific)";
  attrib V003     format=F00002_. label="Urban/rural";
  attrib V004     label="Facility number";
  attrib V005     label="Sample weight";
  attrib V007     format=F00003_. label="Facility type (Country-specific)";
  attrib V008     format=F00004_. label="Managing authority (Country-specific)";
  attrib V009     label="Interviewer";
  attrib V010     format=F00005_. label="Type of SPA";
  attrib V010A    format=F00006_. label="Result of survey";
  attrib V010B    format=F00007_. label="Outpatient or only inpatient";
  attrib V010C    label="Number of visits";
  attrib V012B    format=F00008_. label="Section 10: Child Immunization services present and completed - See V200s";
  attrib V012C    format=F00009_. label="Section 12: Sick children services present and completed - See V200s";
  attrib V012D    format=F00010_. label="Section 11: Growth monitoring services present and completed - See V200s";
  attrib V013     format=F00011_. label="Section 13: Family planning services present and completed See V300s";
  attrib V013S    format=F00012_. label="Section 13: Sterilizations performed See V300s";
  attrib V014A    format=F00013_. label="Section 14: Antenatal services present and completed - See V400s";
  attrib V015A    format=F00014_. label="Section 16: Normal delivery and newborn care present and completed - See V500s";
  attrib V015C    format=F00015_. label="Section 25: C-section delivery services present and completed - See V500s";
  attrib V016     format=F00016_. label="Section 18: STI Services present and completed - See V600s";
  attrib V018     format=F00017_. label="Section 10: Immunization Logistics Data present and completed - See LV200s";
  attrib V019     format=F00018_. label="Section 9.2 Contraceptive Logistics Data present and completed - See LV300s";
  attrib V019A    format=F00019_. label="Storage of contraceptives";
  attrib V033     format=F00020_. label="Section 4: HMIS data present and completed See V130-V135s";
  attrib V034     format=F00021_. label="Section 8: Laboratory data present and completed - See V800s";
  attrib V034A    format=F00022_. label="Section 26: Blood typing services present and completed -  - See V860s";
  attrib V034B    format=F00023_. label="Section 27: Blood transfusion data present and completed -See V834s, V838s, V861s, V896-899";
  attrib V035     format=F00024_. label="Section 9: Pharmacy data for drugs/ supplies present present and completed - See V900s";
  attrib V035A    format=F00025_. label="Storage of anti-TB medications";
  attrib V035B    format=F00026_. label="Storage of anti-retroviral medications";
  attrib V042     format=F00027_. label="Section 20: HIV testing data present and completed - See V1300s";
  attrib V043     format=F00028_. label="Section 19: TB data present and completed - See V1100s";
  attrib V044     format=F00029_. label="Section 21: HIV treatment: ART data present and completed - See V1400s";
  attrib V045     format=F00030_. label="Section 15: PMTCT of HIV infection services present and completed - See V1200s";
  attrib V045A    format=F00031_. label="Section 15: PMTCT of HIV infection services integrated with delivery services present and completed - See V1280s";
  attrib V046     format=F00032_. label="Section 22: HIV care and support data present and completed - See V1500s";
  attrib V047     format=F00033_. label="Section 17: Malaria present and completed - See V1000s";
  attrib V048     format=F00034_. label="Section 23: Non-communicable diseases data present and completed - See V1600s";
  attrib V049     format=F00035_. label="Section 24: Minor surgical services data present and completed - See V1700s";
  attrib V052     label="Number of providers on Provider Listing";
  attrib V058A    label="Number of Provider interviews attempted";
  attrib V058     label="Number of Provider interviews who provide some service completed";
  attrib V071     label="Number of ANC clients completed observation and exit";
  attrib V071A    label="Number of ANC clients completed observation";
  attrib V071C    label="Number of ANC clients attempted interview";
  attrib V072     label="Number of Sick child clients completed observation and exit";
  attrib V072A    label="Number of Sick child clients completed observation";
  attrib V072C    label="Number of Sick child clients attempted interview";
  attrib V073     label="Number of Family Planning clients completed observation and exit";
  attrib V073A    label="Number of Family Planning clients completed observation";
  attrib V073C    label="Number of Family Planning clients attempted interview";
  attrib V076     label="Total ANC clients at facility";
  attrib V077     label="Total Sick child clients at facility";
  attrib V078     label="Total Family planning clients at facility";
  attrib V081     format=F00036_. label="Month of survey";
  attrib V082     label="Year of survey";
  attrib V083     label="Day of survey";
  attrib V095A    label="Starting time of facility interview";
  attrib V095B    label="Ending time of facility interview";
  attrib SFSPA    format=F00037_. label="CS - Full SPA or subset for Case de Sante";
  attrib V101A    format=F00038_. label="Presence of trained health provider at facility 24 hours with duty schedule, present or on-call";
  attrib V102N    label="Number of staffing occurrences used for V102";
  attrib V102A_01 format=F00039_. label="Type of staff (survey specific)(1)";
  attrib V102A_02 format=F00039_. label="Type of staff (survey specific)(2)";
  attrib V102A_03 format=F00039_. label="Type of staff (survey specific)(3)";
  attrib V102A_04 format=F00039_. label="Type of staff (survey specific)(4)";
  attrib V102A_05 format=F00039_. label="Type of staff (survey specific)(5)";
  attrib V102A_06 format=F00039_. label="Type of staff (survey specific)(6)";
  attrib V102A_07 format=F00039_. label="Type of staff (survey specific)(7)";
  attrib V102A_08 format=F00039_. label="Type of staff (survey specific)(8)";
  attrib V102A_09 format=F00039_. label="Type of staff (survey specific)(9)";
  attrib V102A_10 format=F00039_. label="Type of staff (survey specific)(10)";
  attrib V102A_11 format=F00039_. label="Type of staff (survey specific)(11)";
  attrib V102A_12 format=F00039_. label="Type of staff (survey specific)(12)";
  attrib V102A_13 format=F00039_. label="Type of staff (survey specific)(13)";
  attrib V102A_14 format=F00039_. label="Type of staff (survey specific)(14)";
  attrib V102A_15 format=F00039_. label="Type of staff (survey specific)(15)";
  attrib V102A_16 format=F00039_. label="Type of staff (survey specific)(16)";
  attrib V102A_17 format=F00039_. label="Type of staff (survey specific)(17)";
  attrib V102A_18 format=F00039_. label="Type of staff (survey specific)(18)";
  attrib V102A_19 format=F00039_. label="Type of staff (survey specific)(19)";
  attrib V102A_20 format=F00039_. label="Type of staff (survey specific)(20)";
  attrib V102A_21 format=F00039_. label="Type of staff (survey specific)(21)";
  attrib V102A_22 format=F00039_. label="Type of staff (survey specific)(22)";
  attrib V102A_23 format=F00039_. label="Type of staff (survey specific)(23)";
  attrib V102A_24 format=F00039_. label="Type of staff (survey specific)(24)";
  attrib V102A_25 format=F00039_. label="Type of staff (survey specific)(25)";
  attrib V102A_26 format=F00039_. label="Type of staff (survey specific)(26)";
  attrib V102A_27 format=F00039_. label="Type of staff (survey specific)(27)";
  attrib V102D_01 format=F00040_. label="Staff assigned, employed or seconded(1)";
  attrib V102D_02 format=F00040_. label="Staff assigned, employed or seconded(2)";
  attrib V102D_03 format=F00040_. label="Staff assigned, employed or seconded(3)";
  attrib V102D_04 format=F00040_. label="Staff assigned, employed or seconded(4)";
  attrib V102D_05 format=F00040_. label="Staff assigned, employed or seconded(5)";
  attrib V102D_06 format=F00040_. label="Staff assigned, employed or seconded(6)";
  attrib V102D_07 format=F00040_. label="Staff assigned, employed or seconded(7)";
  attrib V102D_08 format=F00040_. label="Staff assigned, employed or seconded(8)";
  attrib V102D_09 format=F00040_. label="Staff assigned, employed or seconded(9)";
  attrib V102D_10 format=F00040_. label="Staff assigned, employed or seconded(10)";
  attrib V102D_11 format=F00040_. label="Staff assigned, employed or seconded(11)";
  attrib V102D_12 format=F00040_. label="Staff assigned, employed or seconded(12)";
  attrib V102D_13 format=F00040_. label="Staff assigned, employed or seconded(13)";
  attrib V102D_14 format=F00040_. label="Staff assigned, employed or seconded(14)";
  attrib V102D_15 format=F00040_. label="Staff assigned, employed or seconded(15)";
  attrib V102D_16 format=F00040_. label="Staff assigned, employed or seconded(16)";
  attrib V102D_17 format=F00040_. label="Staff assigned, employed or seconded(17)";
  attrib V102D_18 format=F00040_. label="Staff assigned, employed or seconded(18)";
  attrib V102D_19 format=F00040_. label="Staff assigned, employed or seconded(19)";
  attrib V102D_20 format=F00040_. label="Staff assigned, employed or seconded(20)";
  attrib V102D_21 format=F00040_. label="Staff assigned, employed or seconded(21)";
  attrib V102D_22 format=F00040_. label="Staff assigned, employed or seconded(22)";
  attrib V102D_23 format=F00040_. label="Staff assigned, employed or seconded(23)";
  attrib V102D_24 format=F00040_. label="Staff assigned, employed or seconded(24)";
  attrib V102D_25 format=F00040_. label="Staff assigned, employed or seconded(25)";
  attrib V102D_26 format=F00040_. label="Staff assigned, employed or seconded(26)";
  attrib V102D_27 format=F00040_. label="Staff assigned, employed or seconded(27)";
  attrib V102E_01 format=F00041_. label="Staff part-time(1)";
  attrib V102E_02 format=F00041_. label="Staff part-time(2)";
  attrib V102E_03 format=F00041_. label="Staff part-time(3)";
  attrib V102E_04 format=F00041_. label="Staff part-time(4)";
  attrib V102E_05 format=F00041_. label="Staff part-time(5)";
  attrib V102E_06 format=F00041_. label="Staff part-time(6)";
  attrib V102E_07 format=F00041_. label="Staff part-time(7)";
  attrib V102E_08 format=F00041_. label="Staff part-time(8)";
  attrib V102E_09 format=F00041_. label="Staff part-time(9)";
  attrib V102E_10 format=F00041_. label="Staff part-time(10)";
  attrib V102E_11 format=F00041_. label="Staff part-time(11)";
  attrib V102E_12 format=F00041_. label="Staff part-time(12)";
  attrib V102E_13 format=F00041_. label="Staff part-time(13)";
  attrib V102E_14 format=F00041_. label="Staff part-time(14)";
  attrib V102E_15 format=F00041_. label="Staff part-time(15)";
  attrib V102E_16 format=F00041_. label="Staff part-time(16)";
  attrib V102E_17 format=F00041_. label="Staff part-time(17)";
  attrib V102E_18 format=F00041_. label="Staff part-time(18)";
  attrib V102E_19 format=F00041_. label="Staff part-time(19)";
  attrib V102E_20 format=F00041_. label="Staff part-time(20)";
  attrib V102E_21 format=F00041_. label="Staff part-time(21)";
  attrib V102E_22 format=F00041_. label="Staff part-time(22)";
  attrib V102E_23 format=F00041_. label="Staff part-time(23)";
  attrib V102E_24 format=F00041_. label="Staff part-time(24)";
  attrib V102E_25 format=F00041_. label="Staff part-time(25)";
  attrib V102E_26 format=F00041_. label="Staff part-time(26)";
  attrib V102E_27 format=F00041_. label="Staff part-time(27)";
  attrib V102DT   format=F00042_. label="Total staff assigned, employed or seconded";
  attrib V102ET   format=F00043_. label="Total staff part-time";
  attrib V102T    format=F00044_. label="Total staff assigned, employed or seconded (copy of V102D)";
  attrib V105     format=F00045_. label="Frequency of routine management/administrative meetings";
  attrib V106     format=F00046_. label="Record of management meetings";
  attrib V106A    format=F00047_. label="Record most recent meet:RHIS data quality";
  attrib V106B    format=F00048_. label="Record most recent meet:RHIS reporting";
  attrib V106C    format=F00049_. label="Record most recent meet:Timeliness of RHIS reporting";
  attrib V106D    format=F00050_. label="Record most recent meet:Quality of services";
  attrib V106E    format=F00051_. label="Record most recent meet:Client utilization";
  attrib V106F    format=F00052_. label="Record most recent meet:Disease data";
  attrib V106G    format=F00053_. label="Record most recent meet:Employment conditions";
  attrib V106H    format=F00054_. label="Record most recent meet:Finances or budget";
  attrib V106X    format=F00055_. label="Record most recent meet:Other";
  attrib V106Y    format=F00056_. label="Record most recent meet:None of above topics";
  attrib V106AA   format=F00057_. label="Any decision made based on last meeting discussions";
  attrib V106BB   format=F00058_. label="Any follow-up actions based on last meeting discussions";
  attrib V107     format=F00059_. label="Frequency of routine staff/community meetings";
  attrib V108     format=F00060_. label="Record of staff/community meetings";
  attrib V109A    format=F00061_. label="Client opinion: Suggestion box";
  attrib V109B    format=F00062_. label="Client opinion:Client survey form";
  attrib V109C    format=F00063_. label="Client opinion:Client interview form";
  attrib V109D    format=F00064_. label="Client opinion:Official meetings with community leaders";
  attrib V109E    format=F00065_. label="Client opinion:Informal discussion with client/community";
  attrib V109F    format=F00066_. label="Client opinion:Email";
  attrib V109G    format=F00067_. label="Client opinion:Facility website";
  attrib V109H    format=F00068_. label="Client opinion:Letters from clients/ community";
  attrib V109X    format=F00069_. label="Client opinion:Other";
  attrib V109Y    format=F00070_. label="Client opinion:No feedback";
  attrib V109Z    format=F00071_. label="Client opinion:Don't know";
  attrib V110A    format=F00072_. label="Procedure for review/reporting of client opinion";
  attrib V110     format=F00073_. label="Record of procedure for reviewing client opinions";
  attrib V112     format=F00074_. label="Quality of care system";
  attrib V115     format=F00075_. label="Last outside supervisory visit";
  attrib V116G    format=F00076_. label="Supervisor action: Use a checklist to assess quality of available health services data";
  attrib V116H    format=F00077_. label="Supervisor action: Discuss performance of the facility based on available health services data";
  attrib V116I    format=F00078_. label="Supervisor action:Help facility make decisions based on available health services data";
  attrib V120A    format=F00079_. label="Facility connected to central supply electricity grid";
  attrib V120     format=F00080_. label="Functional backup generator with fuel (or charged battery)";
  attrib V121A    format=F00081_. label="Source other than central grid:Fuel-operated generator";
  attrib V121B    format=F00082_. label="Source other than central grid: Battery-operated generator";
  attrib V121C    format=F00083_. label="Source other than central grid: Solar system";
  attrib V123     format=F00084_. label="Most common source of water";
  attrib V124     format=F00085_. label="Water onsite or within 500 m of facility";
  attrib V126     format=F00086_. label="Routine time of year for water shortage";
  attrib V127A    format=F00087_. label="Functioning landline phone in facility";
  attrib V127B    format=F00088_. label="Functioning cell phone in facility";
  attrib V127C    format=F00089_. label="Functioning short-wave radio in facility";
  attrib V127     format=F00090_. label="Any functioning communication system observed or reported during all service times";
  attrib V128     format=F00091_. label="Facility has a functioning computer";
  attrib V128A    format=F00092_. label="Computer observed";
  attrib V129     format=F00093_. label="Facility has access to email";
  attrib V130     format=F00094_. label="Designated person responsible for health services data";
  attrib V131     format=F00095_. label="System in place to regularly collect health services data";
  attrib V132     format=F00096_. label="Frequency of reports containing health services data";
  attrib V133     format=F00097_. label="Most recent report observed";
  attrib V134     format=F00098_. label="Total outpatient visits for last completed calendar month";
  attrib V135     format=F00099_. label="Total inpatient live discharges for last completed calendar month";
  attrib V142     format=F00100_. label="Routinely provides inpatient care";
  attrib V143     format=F00101_. label="Number of beds for overnight observation/inpatient care";
  attrib V144     format=F00102_. label="Any client fees";
  attrib V144A    format=F00103_. label="Client fee:Client health card";
  attrib V144B    format=F00104_. label="Client fee:Consultation";
  attrib V144E    format=F00105_. label="Client fee:Medications";
  attrib V144F    format=F00106_. label="Client fee:Any tests";
  attrib V144G    format=F00107_. label="Client fee:Registration";
  attrib V144J    format=F00108_. label="Client fee:Vaccines";
  attrib V144K    format=F00109_. label="Client fee:Contraceptive commodities";
  attrib V144L    format=F00110_. label="Client fee:Normal deliveries";
  attrib V144M    format=F00111_. label="Client fee:Syringes and needles";
  attrib V144N    format=F00112_. label="Client fee:Cesarean section";
  attrib V144O    format=F00113_. label="Client fee:HIV diagnostic test";
  attrib V144P    format=F00114_. label="Client fee:Malaria rapid diagnostic test";
  attrib V144Q    format=F00115_. label="Client fee:Malaria microscopy";
  attrib V144R    format=F00116_. label="Client fee:Other lab tests";
  attrib V144S    format=F00117_. label="Client fee:ARVs for treatment";
  attrib V144T    format=F00118_. label="Client fee:ARVs for PMTCT";
  attrib V144U    format=F00119_. label="Client fee:Minor surgical procedures";
  attrib V145     format=F00120_. label="Client fees posted within easy sight";
  attrib V145A    format=F00121_. label="Client unable to pay:Fee exempted/ discounted/ no payment expected";
  attrib V145B    format=F00122_. label="Client unable to pay:Fee exempted/ discounted/ payment expected later";
  attrib V145C    format=F00123_. label="Client unable to pay:Service not provided/ asked to come back when able to pay";
  attrib V145D    format=F00124_. label="Client unable to pay:Accept payment in-kind";
  attrib V145X    format=F00125_. label="Client unable to pay:Other";
  attrib V146A    format=F00126_. label="Outside funding:CS";
  attrib V146B    format=F00127_. label="Outside funding:CS";
  attrib V146C    format=F00128_. label="Outside funding:CS";
  attrib V146D    format=F00129_. label="Outside funding:CS";
  attrib V146E    format=F00130_. label="Outside funding:CS";
  attrib V146F    format=F00131_. label="Outside funding:CS";
  attrib V146G    format=F00132_. label="Outside funding:CS";
  attrib V146H    format=F00133_. label="Outside funding:CS";
  attrib V146I    format=F00134_. label="Outside funding:CS";
  attrib V146X    format=F00135_. label="Outside funding:Other";
  attrib V146Y    format=F00136_. label="Outside funding:None";
  attrib V146Z    format=F00137_. label="Outside funding:Don't know";
  attrib V150     format=F00138_. label="Functional ambulance with fuel";
  attrib V152     format=F00139_. label="Waiting area for clients";
  attrib V153     format=F00140_. label="Functioning latrine for clients (no/yes)";
  attrib V153A    format=F00141_. label="Functioning latrine for clients";
  attrib V154     format=F00142_. label="General cleanliness: Score out of 8 possible credits";
  attrib V154A    format=F00143_. label="General cleanliness: Floor swept, no obvious dirt/waste (Y)";
  attrib V154B    format=F00144_. label="General cleanliness: Counters/tables/chairs wiped clean (Y)";
  attrib V154C    format=F00145_. label="General cleanliness: Needles/sharps outside of sharps box (N)";
  attrib V154D    format=F00146_. label="General cleanliness: Sharps box overflowing/torn/pierced (N)";
  attrib V154E    format=F00147_. label="General cleanliness: Bandages/ infectious waste lying uncovered (N)";
  attrib V154F    format=F00148_. label="General cleanliness: Walls with significant damage (N)";
  attrib V154G    format=F00149_. label="General cleanliness: Doors with significant damage (N)";
  attrib V154H    format=F00150_. label="General cleanliness: Ceiling with water stains or damage (N)";
  attrib V165A    format=F00151_. label="General OPD area: Measuring tape (for circumference)";
  attrib V165B    format=F00152_. label="General OPD area: Spacers for inhalers";
  attrib V165C    format=F00153_. label="General OPD area: IV infusion kits - adult";
  attrib V165D    format=F00154_. label="General OPD area: IV infusion kits - pediatric";
  attrib V165E    format=F00155_. label="CS*SANTE - General OPD area: #25";
  attrib V165F    format=F00156_. label="CS*SANTE - General OPD area: #26";
  attrib V165G    format=F00157_. label="CS*SANTE - General OPD area: #27";
  attrib V165H    format=F00158_. label="CS*SANTE - General OPD area: #28";
  attrib V165I    format=F00159_. label="CS*SANTE - General OPD area: #29";
  attrib V165J    format=F00160_. label="CS*SANTE - General OPD area: #30";
  attrib V165K    format=F00161_. label="CS*SANTE - General OPD area: #32";
  attrib V166A    format=F00162_. label="General OPD area: Adult weighing scale";
  attrib V166B    format=F00163_. label="General OPD area:Child weighing scale (250 g graduation)";
  attrib V166C    format=F00164_. label="General OPD area: Infant weighting scale (100 g graduation)";
  attrib V166D    format=F00165_. label="General OPD area: Stadiometer (or height rod)";
  attrib V166E    format=F00166_. label="General OPD area: Thermometer";
  attrib V166F    format=F00167_. label="General OPD area: Stethoscope";
  attrib V166G    format=F00168_. label="General OPD area: Digital BP apparatus";
  attrib V166H    format=F00169_. label="General OPD area: Manual BP apparatus";
  attrib V166I    format=F00170_. label="General OPD area: Light source (flashlight OK)";
  attrib V166J    format=F00171_. label="General OPD area: Self-inflating bag and mask (adult)";
  attrib V166K    format=F00172_. label="General OPD area: Self-inflating bag and mask (pediatric)";
  attrib V166L    format=F00173_. label="General OPD area: Micronebulizer";
  attrib V166M    format=F00174_. label="General OPD area: Peak flow meters";
  attrib V166N    format=F00175_. label="General OPD area: Pulse oximeter";
  attrib V166O    format=F00176_. label="General OPD area: Oxygen concentrators";
  attrib V166P    format=F00177_. label="General OPD area: Filled oxygen cylinder";
  attrib V166Q    format=F00178_. label="General OPD area: Oxygen distribution system";
  attrib V166XA   format=F00179_. label="CS*SANTE - General OPD area: #9";
  attrib V166XB   format=F00180_. label="CS*SANTE - General OPD area: #10";
  attrib V166XC   format=F00181_. label="CS*SANTE - General OPD area: #11";
  attrib V166XD   format=F00182_. label="CS*SANTE - General OPD area: #12";
  attrib V166XE   format=F00183_. label="CS*SANTE - General OPD area: #13";
  attrib V166XF   format=F00184_. label="CS*SANTE - General OPD area: #14";
  attrib V166XG   format=F00185_. label="CS*SANTE - General OPD area: #15";
  attrib V166XH   format=F00186_. label="CS*SANTE - General OPD area: #16";
  attrib V166XI   format=F00187_. label="CS*SANTE - General OPD area: #17";
  attrib V166XJ   format=F00188_. label="CS*SANTE - General OPD area: #18";
  attrib V166XK   format=F00189_. label="CS*SANTE - General OPD area: #19";
  attrib V166XL   format=F00190_. label="CS*SANTE - General OPD area: #20";
  attrib V166XM   format=F00191_. label="CS*SANTE - General OPD area: #21";
  attrib V166XN   format=F00192_. label="CS*SANTE - General OPD area: #22";
  attrib V166XO   format=F00193_. label="CS*SANTE - General OPD area: #23";
  attrib V166XP   format=F00194_. label="CS*SANTE - General OPD area: #24";
  attrib V166XQ   format=F00195_. label="CS*SANTE - General OPD area: #31";
  attrib V166XR   format=F00196_. label="CS*SANTE - General OPD area: #33";
  attrib V166XS   format=F00197_. label="CS*SANTE - General OPD area: #34";
  attrib V167     format=F00198_. label="General OPD area:Privacy for observed room";
  attrib V168A1   format=F00199_. label="General OPD area:Running water";
  attrib V168C    format=F00200_. label="General OPD area:Soap";
  attrib V168F    format=F00201_. label="General OPD area:Sharps container";
  attrib V168G    format=F00202_. label="General OPD area:Clean/sterile latex gloves";
  attrib V168J    format=F00203_. label="General OPD area:Disinfectant";
  attrib V168L    format=F00204_. label="General OPD area:Autodestruct syringes with needles or single-use disposable syringes with needles";
  attrib V168O    format=F00205_. label="General OPD area:Pedal Bin waste receptacle with lid and plastic liner";
  attrib V168P    format=F00206_. label="General OPD area:Other waste receptacle (not pedal bin)";
  attrib V168Q    format=F00207_. label="General OPD area:Alcohol-based hand rub";
  attrib V168R    format=F00208_. label="General OPD area:Medical masks";
  attrib V168S    format=F00209_. label="General OPD area:Gowns";
  attrib V168T    format=F00210_. label="General OPD area:Eye protection";
  attrib V168U    format=F00211_. label="General OPD area:Guidelines for standard precautions";
  attrib V173A    format=F00212_. label="Process equipment in facility or out";
  attrib V173     format=F00213_. label="Sterilize equipment(surgical)";
  attrib V175A_1  format=F00214_. label="Sterilization method(1)";
  attrib V175A_2  format=F00214_. label="Sterilization method(2)";
  attrib V175A_3  format=F00214_. label="Sterilization method(3)";
  attrib V175A_4  format=F00214_. label="Sterilization method(4)";
  attrib V175A_5  format=F00214_. label="Sterilization method(5)";
  attrib V175A_6  format=F00214_. label="Sterilization method(6)";
  attrib V175A_7  format=F00214_. label="Sterilization method(7)";
  attrib V175A_8  format=F00214_. label="Sterilization method(8)";
  attrib V175B_1  format=F00215_. label="Method used(1)";
  attrib V175B_2  format=F00215_. label="Method used(2)";
  attrib V175B_3  format=F00215_. label="Method used(3)";
  attrib V175B_4  format=F00215_. label="Method used(4)";
  attrib V175B_5  format=F00215_. label="Method used(5)";
  attrib V175B_6  format=F00215_. label="Method used(6)";
  attrib V175B_7  format=F00215_. label="Method used(7)";
  attrib V175B_8  format=F00215_. label="Method used(8)";
  attrib V175C_1  format=F00216_. label="Temperature used(1)";
  attrib V175C_2  format=F00216_. label="Temperature used(2)";
  attrib V175C_3  format=F00216_. label="Temperature used(3)";
  attrib V175C_4  format=F00216_. label="Temperature used(4)";
  attrib V175C_5  format=F00216_. label="Temperature used(5)";
  attrib V175C_6  format=F00216_. label="Temperature used(6)";
  attrib V175C_7  format=F00216_. label="Temperature used(7)";
  attrib V175C_8  format=F00216_. label="Temperature used(8)";
  attrib V175D_1  format=F00217_. label="Pressure used(1)";
  attrib V175D_2  format=F00217_. label="Pressure used(2)";
  attrib V175D_3  format=F00217_. label="Pressure used(3)";
  attrib V175D_4  format=F00217_. label="Pressure used(4)";
  attrib V175D_5  format=F00217_. label="Pressure used(5)";
  attrib V175D_6  format=F00217_. label="Pressure used(6)";
  attrib V175D_7  format=F00217_. label="Pressure used(7)";
  attrib V175D_8  format=F00217_. label="Pressure used(8)";
  attrib V175E_1  format=F00218_. label="Units of pressure used(1)";
  attrib V175E_2  format=F00218_. label="Units of pressure used(2)";
  attrib V175E_3  format=F00218_. label="Units of pressure used(3)";
  attrib V175E_4  format=F00218_. label="Units of pressure used(4)";
  attrib V175E_5  format=F00218_. label="Units of pressure used(5)";
  attrib V175E_6  format=F00218_. label="Units of pressure used(6)";
  attrib V175E_7  format=F00218_. label="Units of pressure used(7)";
  attrib V175E_8  format=F00218_. label="Units of pressure used(8)";
  attrib V175F_1  format=F00219_. label="Minutes unwrapped(1)";
  attrib V175F_2  format=F00219_. label="Minutes unwrapped(2)";
  attrib V175F_3  format=F00219_. label="Minutes unwrapped(3)";
  attrib V175F_4  format=F00219_. label="Minutes unwrapped(4)";
  attrib V175F_5  format=F00219_. label="Minutes unwrapped(5)";
  attrib V175F_6  format=F00219_. label="Minutes unwrapped(6)";
  attrib V175F_7  format=F00219_. label="Minutes unwrapped(7)";
  attrib V175F_8  format=F00219_. label="Minutes unwrapped(8)";
  attrib V175G_1  format=F00220_. label="Minutes wrapped(1)";
  attrib V175G_2  format=F00220_. label="Minutes wrapped(2)";
  attrib V175G_3  format=F00220_. label="Minutes wrapped(3)";
  attrib V175G_4  format=F00220_. label="Minutes wrapped(4)";
  attrib V175G_5  format=F00220_. label="Minutes wrapped(5)";
  attrib V175G_6  format=F00220_. label="Minutes wrapped(6)";
  attrib V175G_7  format=F00220_. label="Minutes wrapped(7)";
  attrib V175G_8  format=F00220_. label="Minutes wrapped(8)";
  attrib V175H_1  format=F00221_. label="Chemical solution(1)";
  attrib V175H_2  format=F00221_. label="Chemical solution(2)";
  attrib V175H_3  format=F00221_. label="Chemical solution(3)";
  attrib V175H_4  format=F00221_. label="Chemical solution(4)";
  attrib V175H_5  format=F00221_. label="Chemical solution(5)";
  attrib V175H_6  format=F00221_. label="Chemical solution(6)";
  attrib V175H_7  format=F00221_. label="Chemical solution(7)";
  attrib V175H_8  format=F00221_. label="Chemical solution(8)";
  attrib V176A    format=F00222_. label="Non-electric pot with cover(steam/boil):Available";
  attrib V176B    format=F00223_. label="TST indicator strips available";
  attrib V176C    format=F00224_. label="Chemicals for chemical HLD";
  attrib V177A    format=F00225_. label="Electric dry heat sterilizer:Available/functioning";
  attrib V177B    format=F00226_. label="Electric autoclave:Available/functioning";
  attrib V177C    format=F00227_. label="Non-electric autoclave:Available/functioning";
  attrib V177D    format=F00228_. label="Electric pot with cover(steam/boil):Available/functioning";
  attrib V177E    format=F00229_. label="Heat source for non-electric equipment:Available/functioning";
  attrib V177F    format=F00230_. label="Automatic timer:Available/functioning";
  attrib V177G    format=F00231_. label="CS - Sterilization by chaleur seche (poupinel): Available/functioning";
  attrib V178B    format=F00232_. label="Written guidelines for disinfection/sterilization available";
  attrib V184A    format=F00233_. label="Waste disposal of sharps (later version of codes)";
  attrib V185     format=F00234_. label="Sharps waste visible or protected";
  attrib V186A    format=F00235_. label="Waste disposal of contaminated/hazardous waste (later version of codes)";
  attrib V187     format=F00236_. label="Contaminated/hazardous waste visible or protected";
  attrib V189     format=F00237_. label="Incinerator functional and with fuel";
  attrib V190     format=F00238_. label="Guidelines on health care waste management in main outpatient service area";
  attrib V192FC   format=F00239_. label="DHS Main uses dry heat sterilization";
  attrib V192CP   format=F00240_. label="DHS Main has functioning dry heat sterilizer observed";
  attrib V192A    format=F00241_. label="DHS Main:temperature automatic or 170-250 & time autom or 60-120 minutes";
  attrib V192B    format=F00242_. label="DHS Main: temp autom or 170-250 & time 121-400 minutes";
  attrib V192C    format=F00243_. label="DHS Main: temp 160-169 & time 120-400 minutes";
  attrib V192T    format=F00244_. label="DHS Main has observed timer or time is automatic";
  attrib V193FC   format=F00245_. label="Autoclave Main uses autoclave";
  attrib V193CP   format=F00246_. label="Autoclave Main has observed func. electric autoclave or functioning non-electric autoclave with functioning heat source observed";
  attrib V193WA   format=F00247_. label="Autoclave Main: wrapped time is automatic or 30-60 minutes or 15 min & press 30 & temp 132";
  attrib V193WB   format=F00248_. label="Autoclave Main: wrapped time is 61-600 minutes";
  attrib V193UA   format=F00249_. label="Autoclave Main: unwrapped time is automatic or 20-40 minutes";
  attrib V193UB   format=F00250_. label="Autoclave Main: unwrapped time is 41-600 minutes";
  attrib V193T    format=F00251_. label="Autoclave Main: has observed timer or wrapped/unwrapped time is automatic";
  attrib V193PE   format=F00252_. label="Autoclave Main:pressure automatic or PPSQI is 13-17 or ATM pressure is 1 or 2 or kilopas is 106 or MMHG is 776";
  attrib V193PG   format=F00253_. label="Autoclave Main pressure automatic or PPSQI is 18-60 or ATM is 3-8 or Kilop is 107-213 or MMHG is 777-995 or (kilop or mmhg & n is 1,2,13-17";
  attrib V193TE   format=F00254_. label="Autoclave Main temp excellent (automatic or 121-132)";
  attrib V193TG   format=F00255_. label="Autoclave Main temp good (133-900)";
  attrib V194FC   format=F00256_. label="Boil/steam Main uses boil/steam";
  attrib V194CP   format=F00257_. label="Boil/steam Main has observed pot with functioning heat source observed";
  attrib V194A    format=F00258_. label="Boil/steam Main unwrapped time is automatic or 20-40 minutes";
  attrib V194B    format=F00259_. label="Boil/steam Main unwrapped time is 41-600 minutes";
  attrib V194T    format=F00260_. label="Boil/steam Main has observed timer or unwrapped time is automatic";
  attrib V195FC   format=F00261_. label="HLD Main uses HLD";
  attrib V195CP   format=F00262_. label="HLD Main has JIK or Cidex";
  attrib V195A    format=F00263_. label="HLD Main unwrapped time is 20-900";
  attrib V195B    format=F00264_. label="HLD Main has JIK or Cidex";
  attrib V195C    format=F00265_. label="HLD Main has JIK and unwrapped time is 480-994 minutes";
  attrib V195T    format=F00266_. label="HLD Main has observed timer or unwrapped time is automatic";
  attrib VT101    format=F00267_. label="Facility has good sharps waste disposal";
  attrib VT102    format=F00268_. label="Facility has good medical waste disposal";
  attrib VT103    format=F00269_. label="Facility has good sharps waste storage";
  attrib VT104    format=F00270_. label="Facility has good medical waste storage";
  attrib VT105    format=F00271_. label="Any sterilization/HLD capacity";
  attrib VT106    format=F00272_. label="Any sterilization/HLD knowledge";
  attrib VT107    format=F00273_. label="Any sterilization/HLD timer";
  attrib VT201    format=F00274_. label="Facility offers EPI";
  attrib VT202    format=F00275_. label="Facility offers EPI + BCG + Pneumococcal + Rotavirus";
  attrib VT301    format=F00276_. label="Facility offers any modern temporary family planning";
  attrib VT302    format=F00277_. label="Facility offers sterilization";
  attrib VT303    format=F00278_. label="Facility offers Periodic Abstinence/temporary family planning";
  attrib VT801    format=F00279_. label="Facility has functional microscope";
  attrib VT802    format=F00280_. label="Facility has microscope slides";
  attrib VT803    format=F00281_. label="Facility has test tubes";
  attrib VT804    format=F00282_. label="Facility has functional incubator";
  attrib VT805    format=F00283_. label="Facility has functional incubator for HIV testing";
  attrib VT806    format=F00284_. label="Facility has rotator/shaker";
  attrib VT807    format=F00285_. label="Facility offers HIV RDT";
  attrib VT808    format=F00286_. label="Facility offers HIV lab test";
  attrib VT809    format=F00287_. label="Facility offers HIV test internally";
  attrib VT810    format=F00288_. label="Facility offers HIV test external source";
  attrib VT811    format=F00289_. label="Facility offers HIV test (internal or external)";
  attrib VT812    format=F00290_. label="Facility offers HIV dx/tx";
  attrib VT813    format=F00291_. label="Facility offers dried blood spot testing";
  attrib VT814    format=F00292_. label="Facility offers TB RDT";
  attrib VT815    format=F00293_. label="Facility offers TB microscopy";
  attrib VT816    format=F00294_. label="Facility offers TB culture";
  attrib VT817    format=F00295_. label="Facility offers TB xray";
  attrib VT818    format=F00296_. label="Facility offers TB test internally";
  attrib VT819    format=F00297_. label="Facility offers TB test external source";
  attrib VT820    format=F00298_. label="Facility offers any TB test (internal or external)";
  attrib VT821    format=F00299_. label="Facility offers TB dx/tx";
  attrib VT822    format=F00300_. label="Facility offers malaria RDT";
  attrib VT823    format=F00301_. label="Facility offers malaria lab test";
  attrib VT824    format=F00302_. label="Facility offers any malaria test (lab or RDT)";
  attrib VT825    format=F00303_. label="Facility offers malaria dx/tx";
  attrib VT826    format=F00304_. label="Test: Hemoglobin";
  attrib VT827    format=F00305_. label="Test: CD4";
  attrib VT828    format=F00306_. label="Test: Blood glucose";
  attrib VT829    format=F00307_. label="Test: syphilis serology";
  attrib VT830    format=F00308_. label="Test: Stool microscopy";
  attrib VT1201   format=F00309_. label="Facility offers PMTCT";
  attrib VT1401   format=F00310_. label="Facility offers ART";
  attrib VT1501   format=F00311_. label="Facility offers HIV care and support services";
  attrib V200B    label="Days/mo at facility:BCG immunization";
  attrib V200C    label="Days/mo at facility:Sick child consult";
  attrib V200D    label="Days/mo at facility:Growth monitoring";
  attrib V200E    label="Days/mo at facility: Routine Pentavalent";
  attrib V200G    label="Days/mo at facility: Routine Polio";
  attrib V200H    label="Days/mo at facility: Routine Measles";
  attrib V200I    label="Days/mo at facility: Yellow Fever";
  attrib V200J    label="Days/mo at facility: CS";
  attrib V201B    label="Days/mo outreach:BCG immunization";
  attrib V201C    label="Days/mo outreach:Sick child consult";
  attrib V201D    label="Days/mo outreach:Growth monitoring";
  attrib V201E    label="Days/mo outreach: Routine Pentavalent";
  attrib V201G    label="Days/mo outreach: Routine Polio";
  attrib V201H    label="Days/mo outreach: Routine Measles";
  attrib V201I    label="Days/mo outreach: Yellow Fever";
  attrib V201J    label="Days/mo outreach: CS";
  attrib V230SRC  format=F00312_. label="Service where IC information collected for child vaccination";
  attrib V230     format=F00313_. label="Privacy for child vaccination";
  attrib V231A1   format=F00314_. label="Immun admin:Any running water";
  attrib V231C    format=F00315_. label="Immun admin:Soap";
  attrib V231F    format=F00316_. label="Immun admin:Sharps box";
  attrib V231G    format=F00317_. label="Immun admin:Latex gloves";
  attrib V231J    format=F00318_. label="Immun admin:Disinfectant";
  attrib V231L    format=F00319_. label="Immun admin: Auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V231O    format=F00320_. label="Immun admin:Waste receptacle  with lid and plastic liner (pedal bin)";
  attrib V231P    format=F00321_. label="Immun admin:Other waste receptacle";
  attrib V231Q    format=F00322_. label="Immun admin:Alcohol-based hand rub";
  attrib V231R    format=F00323_. label="Immun admin:Medical masks";
  attrib V231S    format=F00324_. label="Immun admin:Gowns";
  attrib V231T    format=F00325_. label="Immun admin:Eye protection";
  attrib V231U    format=F00326_. label="Immun admin:Guidelines for standard precautions";
  attrib V231XA   format=F00327_. label="Immun admin:Blank immun (child) cards";
  attrib V231XB   format=F00328_. label="Immun admin:Register or tally sheets";
  attrib V231XC   format=F00329_. label="Immun admin:Summary forms";
  attrib V231XD   format=F00330_. label="Immun admin:CS";
  attrib V242     format=F00331_. label="Providers in facility follow IMCI guidelines for children under 5 years";
  attrib V246     format=F00332_. label="Any routine actions for sick children";
  attrib V247A    format=F00333_. label="Routine:Weigh child";
  attrib V247B    format=F00334_. label="Routine:Plot weight on graph";
  attrib V247C    format=F00335_. label="Routine:Take child's temperature";
  attrib V247D    format=F00336_. label="Routine:Assess immunization status";
  attrib V247E    format=F00337_. label="Routine:Group health education";
  attrib V247F    format=F00338_. label="Routine:Administer paracetamol/sponge for fever";
  attrib V247G    format=F00339_. label="Routine:Triage sick children";
  attrib V263     format=F00340_. label="Individual records/charts for sick child consults";
  attrib V264SRC  format=F00341_. label="Service where IC information collected for sick child exam";
  attrib V264     format=F00342_. label="Privacy for sick child exam";
  attrib V264A1   format=F00343_. label="Sick child exam:Any running water";
  attrib V264C    format=F00344_. label="Sick child exam:Soap";
  attrib V264F    format=F00345_. label="Sick child exam:Sharps container";
  attrib V264G    format=F00346_. label="Sick child exam:Clean/sterile latex gloves";
  attrib V264J    format=F00347_. label="Sick child exam:Disinfectant";
  attrib V264L    format=F00348_. label="Sick child exam:Auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V264O    format=F00349_. label="Sick child exam:Waste receptacle  with lid and plastic liner (pedal bin)";
  attrib V264P    format=F00350_. label="Sick child exam:Other waste receptacle";
  attrib V264Q    format=F00351_. label="Sick child exam:Alcohol-based hand rub";
  attrib V264R    format=F00352_. label="Sick child exam:Medical masks";
  attrib V264S    format=F00353_. label="Sick child exam:Gowns";
  attrib V264T    format=F00354_. label="Sick child exam:Eye protection";
  attrib V264U    format=F00355_. label="Sick child exam:Guidelines for standard precautions";
  attrib V264XE   format=F00356_. label="Sick child exam:Calibrated 0.5 or 1 liter jar or pitcher for ORS";
  attrib V264XF   format=F00357_. label="Sick child exam:Cup and spoon";
  attrib V264XG   format=F00358_. label="Sick child exam:ORS packets";
  attrib V264XH   format=F00359_. label="Sick child exam:Staff has watch with second hand";
  attrib V264XI   format=F00360_. label="Sick child exam:At least 3 buckets for cleaning used cups";
  attrib V264XJ   format=F00361_. label="Sick child exam:Examination table/bed";
  attrib V265B    format=F00362_. label="Sick child exam other equip:Infant scale";
  attrib V265C    format=F00363_. label="Sick child exam other equip:Child scale";
  attrib V265D    format=F00364_. label="Sick child exam other equip:Thermometer";
  attrib V265E    format=F00365_. label="Sick child exam other equip:Timer or facility provided watch/clock with second hand";
  attrib V265H    format=F00366_. label="Sick child exam other equip:Stethoscope";
  attrib V266A    format=F00367_. label="Sick child consult:IMCI guidelines";
  attrib V266B    format=F00368_. label="Sick child consult:IMCI chart/booklet";
  attrib V266D    format=F00369_. label="Sick child consult:IMCI mother's cards";
  attrib V266E    format=F00370_. label="Sick child consult:Other visual aids for teaching caretakers";
  attrib V266X    format=F00371_. label="Sick child consult:Other guidelines for diagnosis and management of childhood illnesses";
  attrib V267A    format=F00372_. label="Child health services provided at facility:dx/tx child malnutrition";
  attrib V267B    format=F00373_. label="Child health services provided at facility:provide vitamin A supplementation";
  attrib V267C    format=F00374_. label="Child health services provided at facility:provide iron supplementation";
  attrib V267D    format=F00375_. label="Child health services provided at facility:provide zinc supplementation";
  attrib V271     format=F00376_. label="Growth monitoring: guidelines available";
  attrib V271A    format=F00377_. label="Growth monitoring: CS Guidelines";
  attrib V272A    format=F00378_. label="Growth monitoring equip: Infant scale";
  attrib V272B    format=F00379_. label="Growth monitoring equip: Child scale";
  attrib V272C    format=F00380_. label="Growth monitoring equip: Height or length board";
  attrib V273A    format=F00381_. label="Growth monitoring equip: Tape for measuring circumference";
  attrib V273B    format=F00382_. label="Growth monitoring equip: Growth charts";
  attrib V280A    format=F00383_. label="National guidelines for child vaccinations available";
  attrib V280X    format=F00384_. label="Other guidelines for child vaccinations available";
  attrib LV200    format=F00385_. label="Facility stores vaccines in service area";
  attrib LV203A   format=F00386_. label="Avail/valid:Tetanus toxoid in pharmacy";
  attrib LV203B   format=F00387_. label="Avail/valid:BCG and dilutant in service area";
  attrib LV203C   format=F00388_. label="Avail/valid:Oral polio in service area";
  attrib LV203D   format=F00389_. label="Avail/valid:Pentavalent/ DPT+Hib+HepB in service area";
  attrib LV203E   format=F00390_. label="Avail/valid:Measles vaccine & dilutent in service area";
  attrib LV203F   format=F00391_. label="Avail/valid:Hepatitis vaccine in service area";
  attrib LV203G   format=F00392_. label="Avail/valid:CS - Yellow fever vaccine in service area";
  attrib LV206B   format=F00393_. label="Any expired vaccines observed in service area or pharmacy";
  attrib LV221    format=F00394_. label="Vaccine storage in service area: refrigerator";
  attrib LV222A   format=F00395_. label="Temperature of vaccine storage in service area";
  attrib LV224    format=F00396_. label="Temperature monitoring chart present in service area and complete";
  attrib LV225    format=F00397_. label="Number of vaccine carriers in service area";
  attrib LV226    format=F00398_. label="Ice packs for vaccine carriers in service area";
  attrib SF1008A  label="CS - Type of refrigerator for vaccines";
  attrib SF1013B  label="CS - Maintenance sheet for refrigerator seen";
  attrib V301A    label="Days/month FP services offered";
  attrib V303A    format=F00399_. label="Offer:Combined oral pill";
  attrib V303B    format=F00400_. label="Offer:Progestin-only pill";
  attrib V303D    format=F00401_. label="Offer:Injectable combined (1 monthly)";
  attrib V303E    format=F00402_. label="Offer:Injectable Progestin only (2-3 monthly)";
  attrib V303G    format=F00403_. label="Offer:Implant";
  attrib V303H    format=F00404_. label="Offer:Implant (CS)";
  attrib V303I    format=F00405_. label="Offer:Male condom";
  attrib V303J    format=F00406_. label="Offer:Female condom";
  attrib V303K    format=F00407_. label="Offer:IUD";
  attrib V303L    format=F00408_. label="Offer:Emergency contraceptive pill";
  attrib V303O    format=F00409_. label="Offer:Counseling on natural methods";
  attrib V303Q    format=F00410_. label="Offer:Male sterilization";
  attrib V303R    format=F00411_. label="Offer:Female sterilization";
  attrib V303S    format=F00412_. label="Offer:Cycle Beads for SDM";
  attrib V303X    format=F00413_. label="Offer:Other (spermicide/ diaphragm)";
  attrib V306     format=F00414_. label="Routine action for FP before consultation";
  attrib V307A    format=F00415_. label="Routine:Weigh client";
  attrib V307B    format=F00416_. label="Routine:Take blood pressure";
  attrib V307C    format=F00417_. label="Routine:Group health sessions";
  attrib V308     format=F00418_. label="Provider routinely treats STIs";
  attrib V308A    format=F00419_. label="Provider routinely treats STIs (new codes)";
  attrib V324     format=F00420_. label="Individual records for FP clients";
  attrib V325A    format=F00421_. label="Supplies:Samples of FP methods";
  attrib V325D    format=F00422_. label="Supplies:Model for showing male condom use";
  attrib V325F    format=F00423_. label="Supplies:FP flip charts, leaflets";
  attrib V325G    format=F00424_. label="Supplies:Pelvic  model to demonstrate IUCD";
  attrib V325H    format=F00425_. label="Supplies:Examination bed or couch";
  attrib V325I    format=F00426_. label="CS - Supplies: Mannequin to demonstrate implant";
  attrib V327A    format=F00427_. label="Guidelines:National family planning guidelines";
  attrib V327X    format=F00428_. label="Guidelines:Other family planning guidelines";
  attrib V330SRC  format=F00429_. label="Source for infection control for family planning exam";
  attrib V330     format=F00430_. label="Privacy for family planning exam";
  attrib V331A1   format=F00431_. label="Family planning exam:Any running water";
  attrib V331C    format=F00432_. label="Family planning exam:Soap";
  attrib V331F    format=F00433_. label="Family planning exam:Sharps container";
  attrib V331G    format=F00434_. label="Family planning exam:Clean/sterile latex gloves";
  attrib V331J    format=F00435_. label="Family planning exam:Disinfectant";
  attrib V331L    format=F00436_. label="Family planning exam:Auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V331O    format=F00437_. label="Family planning exam:Waste receptacle  with lid and plastic liner (pedal bin)";
  attrib V331P    format=F00438_. label="Family planning exam:Other waste receptacle";
  attrib V331Q    format=F00439_. label="Family planning exam:Alcohol-based hand rub";
  attrib V331R    format=F00440_. label="Family planning exam:Medical masks";
  attrib V331S    format=F00441_. label="Family planning exam:Gowns";
  attrib V331T    format=F00442_. label="Family planning exam:Eye protection";
  attrib V331U    format=F00443_. label="Family planning exam:Guidelines for standard precautions";
  attrib V333B    format=F00444_. label="Other equip:Manual blood pressure apparatus";
  attrib V333C    format=F00445_. label="Other equip:Stethoscope";
  attrib V333D    format=F00446_. label="Other equip:Digital blood pressure apparatus";
  attrib V333E    format=F00447_. label="Other equip:Examination light (flashlight OK)";
  attrib V334A    format=F00448_. label="IUD/Implant:Sterile gloves";
  attrib V334B    format=F00449_. label="IUD/Implant:Antiseptic solution";
  attrib V334C    format=F00450_. label="IUD/Implant:Sponge-holding forceps";
  attrib V334D    format=F00451_. label="IUD/Implant:Gauze pad/cotton wool";
  attrib V335A    format=F00452_. label="IUD:Vaginal speculum small";
  attrib V335B    format=F00453_. label="IUD:Vaginal speculum:medium";
  attrib V335C    format=F00454_. label="IUD:Vaginal speculum large";
  attrib V335E    format=F00455_. label="IUD:Tenacula";
  attrib V335F    format=F00456_. label="IUD:Uterine sound";
  attrib V336A    format=F00457_. label="Implant:Local anesthetic";
  attrib V336B    format=F00458_. label="Implant:Sterile /syringe/needle";
  attrib V336C    format=F00459_. label="Implant:cannula/trochar for insertion";
  attrib V336D    format=F00460_. label="Implant:Scalpel with blade";
  attrib V336F    format=F00461_. label="Implant:Sealed implant pack";
  attrib V336G    format=F00462_. label="Implant:Minor surgery kit (artery forceps)";
  attrib V342     format=F00463_. label="Place where equipment (speculum/forceps) processed for reuse";
  attrib V343A    format=F00464_. label="Autoclave used for final FP equipment processing";
  attrib V343B    format=F00465_. label="Dry heat sterilization used for final FP equipment processing";
  attrib V343C    format=F00466_. label="Chlorine solution soak used for final FP equipment processing";
  attrib V343D    format=F00467_. label="Boil or steam used for final FP equipment processing";
  attrib V343E    format=F00468_. label="Soap and water wash used for final FP equipment processing";
  attrib V343F    format=F00469_. label="Soak in other chemical solution for final FP equipment processing";
  attrib V344     format=F00470_. label="HIV RTD kit available in FP service area";
  attrib LV300    format=F00471_. label="Stores contraceptives";
  attrib LV302A   format=F00472_. label="Contraceptive monitoring system - computer or ledge/stock cards";
  attrib LV302B   format=F00473_. label="Contraceptive monitoring system: Stock monitoring method of update";
  attrib LV303A   format=F00474_. label="Avail/valid:Combined oral pill";
  attrib LV303B   format=F00475_. label="Avail/valid:Progesterone only pill";
  attrib LV303D   format=F00476_. label="Avail/valid: (1 mo) Injectable combined";
  attrib LV303E   format=F00477_. label="Avail/valid:(2-3mo) Injectable progestin only (Depo)";
  attrib LV303G   format=F00478_. label="Avail/valid:Implant";
  attrib LV303H   format=F00479_. label="Avail/valid:CS implant";
  attrib LV303I   format=F00480_. label="Avail/valid:Male condom";
  attrib LV303J   format=F00481_. label="Avail/valid:Female condom";
  attrib LV303K   format=F00482_. label="Avail/valid:IUD";
  attrib LV303L   format=F00483_. label="Avail/valid:Emergency contraceptive pill";
  attrib LV303O   format=F00484_. label="Avail/valid:Cycle beads for SDM";
  attrib LV306A   format=F00485_. label="Contraceptives organized by expiry date";
  attrib LV306B   format=F00486_. label="Any expired observed";
  attrib LV320A   format=F00487_. label="Contraceptives stored in area protected from sunlight";
  attrib LV320C   format=F00488_. label="Storage room in area free of rodent/pest evidence";
  attrib LV320D   format=F00489_. label="Contraceptives stored in FP area off floor";
  attrib LV320E   format=F00490_. label="Contraceptives stored in area protected from water";
  attrib LV320F   format=F00491_. label="Contraceptives stored in area well ventilated";
  attrib SF1308B  format=F00492_. label="Empty client card available";
  attrib SF1308D  format=F00493_. label="Filing system for client files seen";
  attrib V401A    label="Days/month for antenatal care";
  attrib V406     format=F00494_. label="Routine actions for ANC clients before consult";
  attrib V407A    format=F00495_. label="Routine:Weigh client";
  attrib V407B    format=F00496_. label="Routine:Take BP";
  attrib V407C    format=F00497_. label="Routine:Urine test for protein";
  attrib V407D    format=F00498_. label="Routine:Blood test anemia";
  attrib V407E    format=F00499_. label="Routine:Group Health education";
  attrib V407F    format=F00500_. label="Routine:Malaria RDT";
  attrib V407G    format=F00501_. label="Routine:HIV testing and counseling for pregnant women";
  attrib V407H    format=F00502_. label="Routine:Measure client's height";
  attrib V409B    format=F00503_. label="Routine tx: Counseling about family planning";
  attrib V409C    format=F00504_. label="Routine tx: Counseling about HIV/AIDS";
  attrib V409E    format=F00505_. label="Routine tx: Counseling about preparations for delivery";
  attrib V409F    format=F00506_. label="Routine tx: Counseling on 4 recommended ANC visits";
  attrib V409G    format=F00507_. label="Routine tx: Counseling about ITNs to prevent mosquito bites and malaria";
  attrib V409H    format=F00508_. label="Routine tx: Counseling about breastfeeding";
  attrib V409I    format=F00509_. label="Routine tx: Counseling about newborn care";
  attrib V409J    format=F00510_. label="Routine tx: Counseling on postnatal care visits";
  attrib V409K    format=F00511_. label="CS - Routine tx: Counseling on danger signs for pregnancy and newborns";
  attrib V410     format=F00512_. label="Routinely treat STI or refer";
  attrib V410A    format=F00513_. label="Routinely treat STI or refer (new codes)";
  attrib V411A    format=F00514_. label="Tetanus offered all days ANC offered";
  attrib V411B    format=F00515_. label="Days/week tetanus toxoid offered";
  attrib V424     format=F00516_. label="Individual records for ANC clients";
  attrib V430SRC  format=F00517_. label="Service where IC information collected";
  attrib V430     format=F00518_. label="Privacy for antenatal exam";
  attrib V431A1   format=F00519_. label="ANC Exam:Any running water";
  attrib V431C    format=F00520_. label="ANC Exam:Soap";
  attrib V431F    format=F00521_. label="ANC Exam:Sharps container";
  attrib V431G    format=F00522_. label="ANC Exam:Clean/sterile latex gloves";
  attrib V431J    format=F00523_. label="ANC Exam:Disinfectant";
  attrib V431L    format=F00524_. label="ANC Exam:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V431O    format=F00525_. label="ANC Exam:Pedal bin waste receptacle with lid and plastic liner";
  attrib V431P    format=F00526_. label="ANC Exam:Other waste receptacle";
  attrib V431Q    format=F00527_. label="ANC Exam:Alcohol-based hand rub";
  attrib V431R    format=F00528_. label="ANC exam:Medical masks";
  attrib V431S    format=F00529_. label="ANC exam:Gowns";
  attrib V431T    format=F00530_. label="ANC exam:Eye protection";
  attrib V431U    format=F00531_. label="ANC exam:Guidelines for standard precautions";
  attrib V432A    format=F00532_. label="ANC medications available:SP/Fansidar";
  attrib V432B    format=F00533_. label="ANC medications available:Iron tablets";
  attrib V432C    format=F00534_. label="ANC medications available:Folic acid tablets";
  attrib V432D    format=F00535_. label="ANC medications available:Combined iron and folic acid tablets";
  attrib V432E    format=F00536_. label="ANC medications available:Tetanus Toxoid vaccine";
  attrib V432F    format=F00537_. label="ANC medications available:ITN mosquito bed net";
  attrib V433B    format=F00538_. label="ANC exam:Manual blood pressure apparatus";
  attrib V433C    format=F00539_. label="ANC exam:Stethoscope";
  attrib V433D    format=F00540_. label="ANC exam:Fetal stethoscope";
  attrib V433F    format=F00541_. label="ANC exam:Infant scale";
  attrib V433G    format=F00542_. label="ANC exam:Adult weighing scale";
  attrib V433H    format=F00543_. label="ANC exam:Digital BP apparatus";
  attrib V433I    format=F00544_. label="ANC exam:Examination light (flashlight OK)";
  attrib V434A    format=F00545_. label="Protocols:National guidelines for ANC";
  attrib V434B    format=F00546_. label="Protocols:IPT guidelines";
  attrib V434X    format=F00547_. label="Protocols:Other guidelines for ANC";
  attrib V435A    format=F00548_. label="ANC equipment: Examination bed or couch";
  attrib V435B    format=F00549_. label="ANC equipment: Tape measure for fundal height";
  attrib V460     format=F00550_. label="Visual aids for client education related to pregnancy or ANC care";
  attrib V461A    format=F00551_. label="Provision as part of routine ANC: Iron supplementation";
  attrib V461B    format=F00552_. label="Provision as part of routine ANC: Folic acid supplementation";
  attrib V461C    format=F00553_. label="Provision as part of routine ANC: Intermittent Preventive treatment (IPT) for malaria";
  attrib V461D    format=F00554_. label="Provision as part of routine ANC: Tetanus toxoid vaccination";
  attrib V462A    format=F00555_. label="Test as part of ANC: HIV RDT";
  attrib V462B    format=F00556_. label="Test as part of ANC: Urine protein test";
  attrib V462C    format=F00557_. label="Test as part of ANC: Urine glucose test";
  attrib V462D    format=F00558_. label="Test as part of ANC: Any rapid test for hemoglobin";
  attrib V462E    format=F00559_. label="Test as part of ANC: Syphilis RDT";
  attrib V501     format=F00560_. label="Number of maternity beds";
  attrib V501A    label="Number of delivery beds/couches";
  attrib V502     format=F00561_. label="Twenty-four hour coverage";
  attrib V503     format=F00562_. label="Schedule of 24-hour staffing";
  attrib V506C    format=F00563_. label="Routine:Kangaroo mother care";
  attrib V507A    format=F00564_. label="Newborn rtn:Suction with catheter";
  attrib V507B    format=F00565_. label="Newborn rtn:Weigh newborn";
  attrib V507C    format=F00566_. label="Newborn rtn:Give full bath within minutes/few hours after birth";
  attrib V507D    format=F00567_. label="Newborn rtn:Give prelacteal liquids";
  attrib V507E    format=F00568_. label="Newborn rtn:Give OPV prior to discharge";
  attrib V507F    format=F00569_. label="Newborn rtn:Give BCG prior to discharge";
  attrib V507G    format=F00570_. label="Newborn rtn:Suction newborn with bulb";
  attrib V507H    format=F00571_. label="Newborn rtn:Delivery to abdomen";
  attrib V507I    format=F00572_. label="Newborn rtn:Dry and wrap newborns to keep warm";
  attrib V507J    format=F00573_. label="Newborn rtn:Initiate breastfeeding in first hour";
  attrib V507K    format=F00574_. label="Newborn rtn:Complete exam of newborn before discharge";
  attrib V507L    format=F00575_. label="Newborn rtn:Administer Vitamin K to newborn";
  attrib V507M    format=F00576_. label="Newborn rtn:Apply tetracycline eye ointment to both eyes";
  attrib V530SRC  format=F00577_. label="Source for infection control for delivery room";
  attrib V530     format=F00578_. label="Privacy for delivery room";
  attrib V531A1   format=F00579_. label="Delivery:Running water";
  attrib V531C    format=F00580_. label="Delivery:Soap";
  attrib V531F    format=F00581_. label="Delivery:Sharps container";
  attrib V531G    format=F00582_. label="Delivery:Disposable latex gloves";
  attrib V531J    format=F00583_. label="Delivery:Disinfectant";
  attrib V531L    format=F00584_. label="Delivery:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V531O    format=F00585_. label="Delivery:Pedal bin waste receptacle with lid with plastic liner";
  attrib V531P    format=F00586_. label="Delivery:Other waste receptacle";
  attrib V531Q    format=F00587_. label="Delivery:Alcohol-based hand rub";
  attrib V531R    format=F00588_. label="Delivery:Medical masks";
  attrib V531S    format=F00589_. label="Delivery:Gowns";
  attrib V531T    format=F00590_. label="Delivery:Eye protection";
  attrib V531U    format=F00591_. label="Delivery:Guidelines for standard precautions";
  attrib V531XD   format=F00592_. label="Delivery:Suture material with needle";
  attrib V531XE   format=F00593_. label="Delivery:Sterile scissors or blade";
  attrib V531XF   format=F00594_. label="Delivery:Needle holder";
  attrib V531XG   format=F00595_. label="Delivery:Sterile gloves";
  attrib V531XH   format=F00596_. label="Delivery:Delivery pack";
  attrib V531XI   format=F00597_. label="Delivery:Swab holder";
  attrib V531XJ   format=F00598_. label="Delivery:Forceps large";
  attrib V531XK   format=F00599_. label="Delivery:Forceps medium";
  attrib V531XL   format=F00600_. label="Delivery:Cord clamp";
  attrib V531XM   format=F00601_. label="Delivery:Speculum";
  attrib V531XN   format=F00602_. label="Delivery:Episiotomy scissors";
  attrib V531XO   format=F00603_. label="Delivery:Delivery bed";
  attrib V533B    format=F00604_. label="Other equip:Examination light (flashlight OK)";
  attrib V533C    format=F00605_. label="Other equip:Manual BP apparatus";
  attrib V533D    format=F00606_. label="Other equip:Stethoscope";
  attrib V533E    format=F00607_. label="Other equip:Digital BP apparatus";
  attrib V533F    format=F00608_. label="Other equip:Foetal stethoscope";
  attrib V533G    format=F00609_. label="Other equip:Manual vacuum extractor";
  attrib V533H    format=F00610_. label="Other equip:Vacuum aspirator or D&C kit";
  attrib V533I    format=F00611_. label="Other equip:Thermometer";
  attrib V533J    format=F00612_. label="Other equip:Thermometer for low-body temperature";
  attrib V534A    format=F00613_. label="Medications:IV solution (plasma expanders) with infusion set";
  attrib V534C    format=F00614_. label="Medications:Injectable uterotonic (oxytocin)";
  attrib V534D    format=F00615_. label="Medications:Injectable diazepam";
  attrib V534E    format=F00616_. label="Medications:Injectable magnesium sulfate";
  attrib V534G    format=F00617_. label="Medications:Injectable antibiotic (ceftriaxone)";
  attrib V534I    format=F00618_. label="Medications:Antibiotic eye drops/ointment";
  attrib V534M    format=F00619_. label="Medications:Skin disinfectant (other than chlorhexidine)";
  attrib V534N    format=F00620_. label="Medications:4% chlorhexidine solution (umbilical cord cleansing)";
  attrib V534O    format=F00621_. label="Medications:Hydrazaline";
  attrib V536A    format=F00622_. label="Newborn:Infant resuscitation bag/mask or tube/mask)";
  attrib V536B    format=F00623_. label="Newborn:Incubator";
  attrib V536C    format=F00624_. label="Newborn:Other source of heat for premature infant";
  attrib V536D    format=F00625_. label="Newborn:Infant scale";
  attrib V536E    format=F00626_. label="Newborn:Suction bulb for mucus extraction";
  attrib V536F    format=F00627_. label="Newborn:Suction apparatus with catheter";
  attrib V537A    format=F00628_. label="Blank partograph";
  attrib V537B    format=F00629_. label="Guidelines: (IMPAC) Guidelines for Integrated Management of Pregnancy and Childbirth";
  attrib V537C    format=F00630_. label="Guidelines: (CEmOC) National guidelines for comprehensive emergency obstetric care";
  attrib V537D    format=F00631_. label="Guidelines:Guidelines for management of pre-term labor";
  attrib V537E    format=F00632_. label="Guidelines: (BEmOC) National guidelines for basic emergency obstetric care";
  attrib V545     format=F00633_. label="Review maternal/newborn deaths/near-misses";
  attrib V546A    format=F00634_. label="Frequency (every .. weeks):Maternal death/near-miss reviews";
  attrib V546B    format=F00635_. label="Frequency (every .. weeks):Neonatal death/near-miss reviews";
  attrib V554A    format=F00636_. label="Ever or past 3 months:Used parental oxytocic drugs";
  attrib V554B    format=F00637_. label="Ever or past 3 months:Used parental anti-convulsants for hypertension";
  attrib V554C    format=F00638_. label="Ever or past 3 months:Used manual placental removal";
  attrib V554D    format=F00639_. label="Ever or past 3 months:Used Parenteral antibiotics";
  attrib V554E    format=F00640_. label="Ever or past 3 months:Assisted vaginal delivery";
  attrib V554F    format=F00641_. label="Ever or past 3 months:Removal of retained products after delivery";
  attrib V554G    format=F00642_. label="Ever or past 3 months:Neonatal resuscitation";
  attrib V554H    format=F00643_. label="Ever or past 3 months:Corticosteroids for pre-term labor";
  attrib V554I    format=F00644_. label="Ever or past 3 months:Blood transfusion";
  attrib V554J    format=F00645_. label="Ever or past 3 months:Caesarean sections";
  attrib V557C    format=F00646_. label="C-Sect:Anesthesia giving set";
  attrib V557E    format=F00647_. label="C-Sect:Tubings and connectors for endotracheal tube";
  attrib V557F    format=F00648_. label="C-Sect:Oropharyngeal airway (adult)";
  attrib V557G    format=F00649_. label="C-Sect:Oropharyngeal airway (pediatric)";
  attrib V557H    format=F00650_. label="C-Sect:Magills forceps - adult";
  attrib V557I    format=F00651_. label="C-Sect:Magills forceps - pediatric";
  attrib V557J    format=F00652_. label="C-Sect:Endotracheal tube cuffed sizes 3.0-5.0";
  attrib V557K    format=F00653_. label="C-Sect:Endotracheal tube cuffed sizes 5.5-9.0";
  attrib V557L    format=F00654_. label="C-Sect:Intubating stylet";
  attrib V557M    format=F00655_. label="C-Sect:Spinal needle";
  attrib V558     format=F00656_. label="C-section provider present/on-call 24 hrs";
  attrib V559     format=F00657_. label="Anesthetist present/on call 24 hrs";
  attrib V567     format=F00658_. label="Method for Kangaroo Mother Care for low birth weight babies";
  attrib V568     format=F00659_. label="Use of partographs";
  attrib V569     format=F00660_. label="HIV RDT available in service site";
  attrib V570     format=F00661_. label="PMTCT services provided at delivery site";
  attrib V600A    format=F00662_. label="STIs diagnosed/ treated in this service";
  attrib V601A    label="Number days/month offered";
  attrib V624     format=F00663_. label="Individual records for STI clients";
  attrib V625A    format=F00664_. label="Visual aid: About STIs";
  attrib V625B    format=F00665_. label="Visual aid: About HIV/AIDS";
  attrib V625C    format=F00666_. label="Visual aid: Male condom model for demonstration";
  attrib V625D    format=F00667_. label="Visual aid: Posters on STIs";
  attrib V625E    format=F00668_. label="Visual aid: Posters on HIV/AIDS";
  attrib V625F    format=F00669_. label="Visual aid: Female condom model for demonstration";
  attrib V625G    format=F00670_. label="Visual aid: Visual aid for cervical cancer";
  attrib V626A    format=F00671_. label="Client take home: About STIs";
  attrib V626B    format=F00672_. label="Client take home: About HIV/AIDS";
  attrib V626C    format=F00673_. label="Client take home: Male condoms for client";
  attrib V626D    format=F00674_. label="Client take home: Female condoms for client";
  attrib V626E    format=F00675_. label="Client take home: About cervical cancer";
  attrib V626F    format=F00676_. label="Client take home: Male condom IEC materials";
  attrib V626G    format=F00677_. label="Client take home: Female condom IEC materials";
  attrib V627A    format=F00678_. label="Standards/protocols:National guidelines for diagnosis and treatment of STIs";
  attrib V627X    format=F00679_. label="Standards/protocols:Other guidelines for the diagnosis and treatment of STIs";
  attrib V628     format=F00680_. label="All STI clients are referred for HIV test";
  attrib V630SRC  format=F00681_. label="Source for infection control for STI service";
  attrib V630     format=F00682_. label="Privacy for STI exam";
  attrib V631A1   format=F00683_. label="STI Exam:Any (other) running water";
  attrib V631C    format=F00684_. label="STI Exam:Soap";
  attrib V631F    format=F00685_. label="STI Exam:Sharps container";
  attrib V631G    format=F00686_. label="STI Exam:Clean/sterile latex gloves";
  attrib V631J    format=F00687_. label="STI Exam:Disinfectant";
  attrib V631L    format=F00688_. label="STI Exam:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V631O    format=F00689_. label="STI Exam:Pedal bin waste receptacle with lid (pedal bin) and plastic liner";
  attrib V631P    format=F00690_. label="STI Exam:Other waste receptacle";
  attrib V631Q    format=F00691_. label="STI Exam:Alcohol-based hand rub";
  attrib V631R    format=F00692_. label="STI Exam:Medical masks";
  attrib V631S    format=F00693_. label="STI Exam:Gowns";
  attrib V631T    format=F00694_. label="STI Exam:Eye protection";
  attrib V631U    format=F00695_. label="STI Exam:Guidelines for standard precautions";
  attrib V640A    format=F00696_. label="Method of dx of STIs: Syndromic approach only";
  attrib V640B    format=F00697_. label="Method of dx of STIs: Etiologic only";
  attrib V640C    format=F00698_. label="Method of dx of STIs:Clinical judgment";
  attrib V640D    format=F00699_. label="Method of dx of STIs:Both syndromic and etiologic";
  attrib V642     format=F00700_. label="Performs partner notification/follow-up";
  attrib V642A    format=F00701_. label="Performs partner notification/follow-up";
  attrib V650     format=F00702_. label="HIV RDT available in service site";
  attrib V805A    format=F00703_. label="HIV Guideline present:How to conduct HIV test";
  attrib V805B    format=F00704_. label="HIV Guideline present:Confidentiality and disclosure of HIV test results";
  attrib V805X    format=F00705_. label="HIV Guideline present:Other relevant HIV guidelines";
  attrib V810A    format=F00706_. label="External QA:Proficiency panel";
  attrib V810B    format=F00707_. label="External QA:External inspection/observation of technique";
  attrib V810C    format=F00708_. label="External QA:Send blood for retesting";
  attrib V810X    format=F00709_. label="External QA:Other method";
  attrib V810Y    format=F00710_. label="External QA:No external QA";
  attrib V813     format=F00711_. label="Record of results from external QA";
  attrib V815     format=F00712_. label="Most recent error rate recorded by external QA";
  attrib V820A    format=F00713_. label="Send blood outside for HIV testing:ELISA";
  attrib V820B    format=F00714_. label="Send blood outside for HIV testing:Western Blot";
  attrib V820C    format=F00715_. label="Send blood outside for HIV testing:PCR for EID";
  attrib V820D    format=F00716_. label="Send blood outside for HIV testing:HIV RDT";
  attrib V820E    format=F00717_. label="Send blood outside for HIV testing:CS";
  attrib V820X    format=F00718_. label="Send blood outside for HIV testing:Other";
  attrib V820Y    format=F00719_. label="Send blood outside for HIV testing:No outside testing";
  attrib V821     format=F00720_. label="Record of result of outside HIV testing";
  attrib V821E    format=F00721_. label="Record of result of outside CS Other HIV specifc testing";
  attrib V834A    format=F00722_. label="Send blood out for screening and results:Syphilis";
  attrib V834B    format=F00723_. label="Send blood out for screening and results:Hepatitis B";
  attrib V834C    format=F00724_. label="Send blood out for screening and results:Hepatitis C";
  attrib V834D    format=F00725_. label="Send blood out for screening and results:HIV";
  attrib V834E    format=F00726_. label="Send blood out for screening and results:Malaria";
  attrib V834Y    format=F00727_. label="Send blood out for screening and results:Send no blood out for screening";
  attrib V835A    format=F00728_. label="Send blood/urine out for: Blood chemistries screening and results";
  attrib V835B    format=F00729_. label="Send blood/urine out for:Liver function test screening and results";
  attrib V835C    format=F00730_. label="Send blood/urine out for Urinalysis screening and results";
  attrib V835D    format=F00731_. label="Send blood/urine out for Pregnancy test";
  attrib V835Y    format=F00732_. label="Send blood/urine out for:Send no blood or urine out for screening/testing";
  attrib V836     format=F00733_. label="Send sputum outside for testing/ results returned";
  attrib V837     format=F00734_. label="Register for TB test results";
  attrib V838A    format=F00735_. label="Send blood out for:Gram stain/ results returned";
  attrib V838B    format=F00736_. label="Send blood out for:Indian ink stain/ results returned";
  attrib V838C    format=F00737_. label="Send blood out for:Malaria/ results returned";
  attrib V838D    format=F00738_. label="Send blood out for:Specimen for culture/ results returned";
  attrib V838Y    format=F00739_. label="Send blood out for:Send no blood out for gram staining, India ink staining, malaria, culture";
  attrib V839     format=F00740_. label="Blood transfusions performed";
  attrib V840A    format=F00741_. label="Any HIV tests:HIV including RDT/DBS";
  attrib V840A1   format=F00742_. label="Any HIV LAB tests (See V843)";
  attrib V840A2   format=F00743_. label="Lab tests:HIV RDT";
  attrib V840A3   format=F00744_. label="Lab tests:HIV DBS";
  attrib V840D    format=F00745_. label="Lab tests:Hematology (see V845, V846)";
  attrib V840E    format=F00746_. label="Lab tests:Syphilis (see V847, V848) or RDT";
  attrib V840E1   format=F00747_. label="Lab tests:Syphilis RDT";
  attrib V840E2   format=F00748_. label="Lab tests:Syphilis (see V847, V848) excluding RDT";
  attrib V840F    format=F00749_. label="Lab tests:Any LFT, Blood Glucose, Urine (see V849, V850,V851)";
  attrib V840F1   format=F00750_. label="Lab tests: Blood Glucose (see V849)";
  attrib V840F2   format=F00751_. label="Lab tests:Liver/renal function tests (see V850)";
  attrib V840F3   format=F00752_. label="Lab tests:Urine chemistries/ urine pregnancy test (see V851)";
  attrib V840G    format=F00753_. label="Lab tests:Malaria microscopy or rapid test (see V852)";
  attrib V840G1   format=F00754_. label="Lab tests:Malaria Rapid test";
  attrib V840I    format=F00755_. label="Lab tests:Gram stain (see V855)";
  attrib V840J    format=F00756_. label="Lab tests:Chlamydia (see V856)";
  attrib V840K    format=F00757_. label="Lab tests:TB (see V859)";
  attrib V840L    format=F00758_. label="Lab tests: Blood typing (see V860)";
  attrib V840M    format=F00759_. label="Lab tests: Blood screening in facility or sent out (see V861)";
  attrib V840N    format=F00760_. label="Lab tests: Stool microscopy (see V857)";
  attrib V840O    format=F00761_. label="Lab tests: CD4 tests (see V891, V892)";
  attrib V840P    format=F00762_. label="Lab tests:TB RDT";
  attrib V840Y    format=F00763_. label="Lab tests for HIV:No HIV tests";
  attrib V841A1   format=F00764_. label="Equipment used & available:Light Microscope+";
  attrib V841A2   format=F00765_. label="Equipment used & available:Electron Microscope+";
  attrib V841B    format=F00766_. label="Equipment used & available:Refrigerator+";
  attrib V841C    format=F00767_. label="Equipment used & available:Incubator+";
  attrib V841D    format=F00768_. label="Equipment used & available:Test tubes";
  attrib V841E    format=F00769_. label="Equipment used & available:Glass slides and covers";
  attrib V841F    format=F00770_. label="Equipment used & available:Culture medium";
  attrib V841G    format=F00771_. label="Equipment used & available:Centrifuge for CSF microbiology+";
  attrib V842A1   format=F00772_. label="Working order:Light Microscope";
  attrib V842A2   format=F00773_. label="Working order:Electron Microscope";
  attrib V842B    format=F00774_. label="Working order:Refrigerator";
  attrib V842C    format=F00775_. label="Working order:Incubator";
  attrib V842G    format=F00776_. label="Working order:Centrifuge for CSF microbiology";
  attrib V843A    format=F00777_. label="Test conducted & items available:ELISA/EIA scanner/ reader+";
  attrib V843B    format=F00778_. label="Test conducted & equipment available:CD4 Flow cytometer analyzer (FACS count machine)+";
  attrib V843C    format=F00779_. label="Test conducted & items available:Dynabeads with vortex mixer+";
  attrib V843E    format=F00780_. label="Test items available:Western Blot test assay";
  attrib V843F    format=F00781_. label="Test conducted & items available:PCR for viral load+";
  attrib V843G    format=F00782_. label="Test conducted & items available:PCR for DNA-EID+";
  attrib V843H    format=F00783_. label="Test conducted & items available:Washer for ELISA scanner/reader+";
  attrib V843I    format=F00784_. label="Test conducted & items available:Assay for ELISA scanner/reader+";
  attrib V843J    format=F00785_. label="Test conducted & items available:Incubator";
  attrib V843K    format=F00786_. label="Test conducted & equipment available:CD4 Fluorescent cartridge analyzer/ PIMA analyzer+";
  attrib V843L    format=F00787_. label="Test items available:CD4 Reagent kits for flow cytometer analyzer";
  attrib V843M    format=F00788_. label="Test items available:CD4 Cartridges for fluorescent cartridge analyzer";
  attrib V843N    format=F00789_. label="Test items available:CD4 Rapid CD4 test strips";
  attrib V844A    format=F00790_. label="Working order:ELISA/EIA scanner/ reader";
  attrib V844B    format=F00791_. label="Working order:CD4 Flow cytometer analyzer (FACS count machine)";
  attrib V844C    format=F00792_. label="Working order:Dynabeads with vortex mixer";
  attrib V844F    format=F00793_. label="Working order:PCR for viral load";
  attrib V844G    format=F00794_. label="Working order:PCR for DNA-EID";
  attrib V844H    format=F00795_. label="Working order:Washer for ELISA scanner/reader";
  attrib V844I    format=F00796_. label="Unexpired:Assay for ELISA scanner/reader";
  attrib V844J    format=F00797_. label="Working order:Incubator";
  attrib V844K    format=F00798_. label="Working order:CD4 Cartridges for fluorescent cartridge analyzer";
  attrib V844N    format=F00799_. label="Unexpired: CD4 Rapid test strips";
  attrib V845A    format=F00800_. label="Test conducted and items available:Hematology analyzer (total lymphocyte/FBC/platelet count)+";
  attrib V845B    format=F00801_. label="Test conducted and items available:HemoCue+";
  attrib V845C    format=F00802_. label="Test conducted and items available:Colorimeter/ hemoglobinometer+";
  attrib V845D    format=F00803_. label="Test items available:Colorimeter Drabkin solution";
  attrib V845G    format=F00804_. label="Test items available:Hemoglobin litmus paper";
  attrib V845H    format=F00805_. label="Test items available:Hemocue microcuvette with valid expiration date";
  attrib V845I    format=F00806_. label="Test items available:Colorimeter Pipette (for measuring blood volume)";
  attrib V845J    format=F00807_. label="CS - Test conducted and items available: Test of Emmel";
  attrib V845K    format=F00808_. label="CS - Test conducted and items available: Electrophoreses equipment";
  attrib V846A    format=F00809_. label="Working order:Hematology analyzer/Coulter";
  attrib V846B    format=F00810_. label="Working order:Hemoglobinometer/hemocue";
  attrib V846C    format=F00811_. label="Working order:Colorimeter/spectroscope";
  attrib V846J    format=F00812_. label="CS - Working order: Test of Emmel";
  attrib V846K    format=F00813_. label="CS - Working order:Electrophoreses equipment";
  attrib V847A    format=F00814_. label="Test conducted and items available:Syphilis VDRL";
  attrib V847B    format=F00815_. label="Test conducted and items available:Syphilis Rotator/shaker+";
  attrib V847C    format=F00816_. label="Test conducted and items available:Syphilis RPR";
  attrib V847D    format=F00817_. label="Test conducted and items available:Syphilis PCR for STIs (CTN)";
  attrib V847E    format=F00818_. label="Test conducted and items available:Syphilis (TPHA) Treponema Pallidum Hemaglutination Assay";
  attrib V848B    format=F00819_. label="Working order:Syphilis Rotator/shaker";
  attrib V849A    format=F00820_. label="Test conducted & equipment available:Blood chem analyzer (serum creatinine, glucose, LFT)+";
  attrib V849B    format=F00821_. label="Test conducted & equipment available: Glucometer+";
  attrib V849B1   format=F00822_. label="Test conducted & equipment available: Glucometer test strips+";
  attrib V850A    format=F00823_. label="Working order:Blood chem analyzer (serum creatinine, glucose, LFT)";
  attrib V850B    format=F00824_. label="Working order:Glucometer";
  attrib V850B1   format=F00825_. label="Unexpired:Glucometer test strips";
  attrib V851     format=F00826_. label="Test items available: Pregnancy test";
  attrib V851A    format=F00827_. label="Test items available:Urine protein dip sticks";
  attrib V851B    format=F00828_. label="Test items available:Urine glucose dip sticks";
  attrib V852A    format=F00829_. label="Test items available:Malaria Giemsa stain";
  attrib V852B    format=F00830_. label="Test items available:Malaria Field stain";
  attrib V852C    format=F00831_. label="Test items available:Malaria RDT (See V840G1)";
  attrib V852D    format=F00832_. label="Test items available:Malaria Acridine Orange (AO microscope & Acridine orange stain)";
  attrib V852E    format=F00833_. label="CS - Test items available: May Grumwald stain";
  attrib V855A    format=F00834_. label="Test items available:Crystal violet gram/Gentian violet stain";
  attrib V855B    format=F00835_. label="Test items available:Lugol's iodine gram stain/ solution";
  attrib V855C    format=F00836_. label="Test items available:Acetone/ Acetone-alcohol gram stain";
  attrib V855D    format=F00837_. label="Test items available:Neutral red/carbol fuchsin/other counter stain";
  attrib V856A    format=F00838_. label="Test items available:Chlamydia - Giemsa stain";
  attrib V856B    format=F00839_. label="Test items available:Chlamydia -  PCR";
  attrib V857B    format=F00840_. label="Test items available:Stool microscopy formal saline (for concentration method)";
  attrib V857C    format=F00841_. label="Test items available:Stool microscopy Lugol's iodine/Lugol's solution";
  attrib V857D    format=F00842_. label="Test items available:Stool microscopy normal saline (for direct microscopy)";
  attrib V859A    format=F00843_. label="Test items available:TB AFB Ziehl-Neelson test";
  attrib V859B    format=F00844_. label="Test items available:TB RDT conducted (see V840P)";
  attrib V859C    format=F00845_. label="Test items available:TB culture/growth medium (ex: MGIT 960)";
  attrib V859D    format=F00846_. label="Test items available:TB Carbol-Fuchsin";
  attrib V859E    format=F00847_. label="Test items available:TB 20-25% sulphuric acid or acid alcohol";
  attrib V859F    format=F00848_. label="Test items available:TB Methylene Blue";
  attrib V859G    format=F00849_. label="Test conducted & equipment available:TB Fluorescence Microscope+";
  attrib V859GG   format=F00850_. label="Working order: TB Fluorence microscope";
  attrib V859H    format=F00851_. label="Test items available:TB Biosafety hood/ cabinet";
  attrib V859I    format=F00852_. label="Test items available:TB Auramine stain for Fluorescence Microscope";
  attrib V860A    format=F00853_. label="Test items available:Blood typing Anti-A reagent (valid expiration date)";
  attrib V860B    format=F00854_. label="Test items available:Blood typing Anti-B reagent (valid expiration date)";
  attrib V860C    format=F00855_. label="Test items available:Blood typing Anti-A,B reagent (valid expiration date)";
  attrib V860D    format=F00856_. label="Test items available:Blood typing Anti-D reagent (valid expiration date)";
  attrib V860F    format=F00857_. label="Test items available:Blood typing Coomb's reagent (valid expiration date)";
  attrib V861A    format=F00858_. label="Blood for transfusion ever screened for:Syphilis";
  attrib V861B    format=F00859_. label="Blood for transfusion ever screened for:Hepatitis B";
  attrib V861C    format=F00860_. label="Blood for transfusion ever screened for:Hepatitis C";
  attrib V861D    format=F00861_. label="Blood for transfusion ever screened for:HIV";
  attrib V861E    format=F00862_. label="Blood for transfusion ever screened for:Malaria";
  attrib V863     format=F00863_. label="Performs diagnostic X-rays, ultrasound, computerized tomography";
  attrib V864A    format=F00864_. label="Equipment available:X-ray machine+";
  attrib V864B    format=F00865_. label="Equipment available:Unexpired film for X-rays+";
  attrib V864C    format=F00866_. label="Equipment available:Ultrasound equipment+";
  attrib V864D    format=F00867_. label="Equipment available:CT scan+";
  attrib V864F    format=F00868_. label="Equipment available:Digital X-ray not requiring film+";
  attrib V865A    format=F00869_. label="Working order:X-ray machine";
  attrib V865B    format=F00870_. label="CS - Working order:X-ray film";
  attrib V865C    format=F00871_. label="Working order:Ultrasound";
  attrib V865D    format=F00872_. label="Working order:CT scan";
  attrib V865F    format=F00873_. label="Working order:Digital X-ray not requiring film";
  attrib V866A1   format=F00874_. label="Lab: Any running water";
  attrib V866C    format=F00875_. label="Lab:Soap";
  attrib V866F    format=F00876_. label="Lab:Sharps container";
  attrib V866G    format=F00877_. label="Lab:Clean/sterile latex gloves";
  attrib V866J    format=F00878_. label="Lab:Disinfectant";
  attrib V866L    format=F00879_. label="Lab:Autodestruct syringes";
  attrib V866O    format=F00880_. label="Lab:Pedal bin waste receptacle with lid and plastic liner";
  attrib V866P    format=F00881_. label="Lab:Other waste receptacle";
  attrib V866Q    format=F00882_. label="Lab:Alcohol-based hand rub";
  attrib V866R    format=F00883_. label="Lab:Medical masks";
  attrib V866S    format=F00884_. label="Lab:Gowns";
  attrib V866T    format=F00885_. label="Lab:Eye protection";
  attrib V866U    format=F00886_. label="Lab:Guidelines for standard precautions";
  attrib V870A    format=F00887_. label="Malaria RDT kit brand A Paracheck";
  attrib V870B    format=F00888_. label="Malaria RDT kit brand B Care Start";
  attrib V870C    format=F00889_. label="Malaria RDT kit brand C First Response";
  attrib V870D    format=F00890_. label="Malaria RDT kit brand D SD Bioline";
  attrib V870E    format=F00891_. label="Malaria RDT kit brand E L'Ag Fp";
  attrib V870F    format=F00892_. label="Malaria RDT kit brand F";
  attrib V870X    format=F00893_. label="Malaria RDT kit brand X (OTHER)";
  attrib V871     format=F00894_. label="Training manual/job aid for using Malaria RDT";
  attrib V872     format=F00895_. label="Availability of sputum container";
  attrib V873A    format=F00896_. label="QC (internal or external):Type followed by facility";
  attrib V873B    format=F00897_. label="QC (internal or external):Records of results from QC procedures";
  attrib V874A    format=F00898_. label="Source of blood that is transfused: National Blood Bank";
  attrib V874B    format=F00899_. label="Source of blood that is transfused: Regional Blood Bank";
  attrib V874C    format=F00900_. label="Source of blood that is transfused: Relatives donating directly";
  attrib V874X    format=F00901_. label="Source of blood that is transfused: Other";
  attrib V875     format=F00902_. label="Facility has run out of blood for more than a day in past 3 months";
  attrib V876A    format=F00903_. label="Refrigerator available for blood storage in service area";
  attrib V876B    format=F00904_. label="Temperature in the blood storage refrigerator or freezer";
  attrib V877     format=F00905_. label="Guidelines on appropriate use of blood and safe transfusion practices";
  attrib V902A    format=F00906_. label="General, anti-malarial, IV solutions stock monitoring system - computer or ledge/stock cards";
  attrib V902B    format=F00907_. label="General medications, antimalarials, and supplies: Stock monitoring method of update";
  attrib V903_02  format=F00908_. label="General med:Aspirin capsules/ tablets";
  attrib V903_05  format=F00909_. label="General med:Albendazole";
  attrib V903_06  format=F00910_. label="General med:Amoxicillin tablet";
  attrib V903_08  format=F00911_. label="General med:Amoxicillin/Clavulinate (Augmentin)";
  attrib V903_10  format=F00912_. label="General med:Ampicillin injection";
  attrib V903_13  format=F00913_. label="General med:Azithromycin tablets";
  attrib V903_14  format=F00914_. label="General med:Azithromycin syrup/suspension";
  attrib V903_15  format=F00915_. label="General med:Amoxicillin syrup/suspension";
  attrib V903_16  format=F00916_. label="General med:Amitriptyline (Depression)";
  attrib V903_17  format=F00917_. label="General med:Amlodipine tablets (CCB for hypertension)";
  attrib V903_18  format=F00918_. label="General med:Atenolol (Beta-blocker)";
  attrib V903_19  format=F00919_. label="General med:A.C.E inhibitor Other than Enalapril";
  attrib V903_20  format=F00920_. label="General med:CS - Aureomycine 3% tube";
  attrib V903_21  format=F00921_. label="General med:CS - Alcool a90";
  attrib V903_22  format=F00922_. label="General med:CS - Albustix";
  attrib V904_03  format=F00923_. label="General med:Beclomethasone inhaler";
  attrib V904_04  format=F00924_. label="General med:Betamethasone injection";
  attrib V904_05  format=F00925_. label="General med:Benzathine Benzelpenicillin powder for injection (repeated in V916_01)";
  attrib V904_06  format=F00926_. label="General med:CS - Betadine gynecologic";
  attrib V904_07  format=F00927_. label="General med:CS - Bande de gaze";
  attrib V904_08  format=F00928_. label="General med:CS - Boite de sparadrap 5X10";
  attrib V905_03  format=F00929_. label="General med:Ceftriaxone injection";
  attrib V905_08  format=F00930_. label="General med:Ciprofloxacin";
  attrib V905_14  format=F00931_. label="General med:Co-trimoxazole tabs";
  attrib V905_18  format=F00932_. label="General med:Co-trimoxazole suspension";
  attrib V905_19  format=F00933_. label="General med:Cefixime tabs";
  attrib V905_20  format=F00934_. label="General med:Captopril";
  attrib V905_21  format=F00935_. label="General med:Calcium gluconate injection";
  attrib V905_22  format=F00936_. label="General med:CS - Cotol";
  attrib V905_23  format=F00937_. label="General med:CS - Coton hydrophile";
  attrib V906_03  format=F00938_. label="General med:Dexamethasone injection";
  attrib V906_07  format=F00939_. label="General med:Diazepam injection";
  attrib V906_08  format=F00940_. label="General med:Diclofenac tablets  (strong pain medicine)";
  attrib V906_11  format=F00941_. label="General med:Doxyclycline";
  attrib V906_12  format=F00942_. label="General med:CS - D";
  attrib V907_03  format=F00943_. label="General med:Erythromycin tabs";
  attrib V907_04  format=F00944_. label="General med:Erythromycin suspension";
  attrib V907_05  format=F00945_. label="General med:Enalapril capsule (A.C.E inhibitor)";
  attrib V907_06  format=F00946_. label="General med:Epinephrine injection";
  attrib V907_07  format=F00947_. label="General med:Eye ointment for newborn antibiotic other than tetracycline";
  attrib V907_08  format=F00948_. label="General med:CS - E";
  attrib V908_02  format=F00949_. label="General med:Fluconazole";
  attrib V908_03  format=F00950_. label="General med:Folic acid tablets";
  attrib V908_04  format=F00951_. label="General med:Furosemide (diuretic)";
  attrib V908_05  format=F00952_. label="General med:CS - F";
  attrib V909_02  format=F00953_. label="General med:Gentamycin injection";
  attrib V909_04  format=F00954_. label="General med:Glibneclamide (type 2 diabetes)";
  attrib V909_05  format=F00955_. label="General med:Glucose injectable solution";
  attrib V909_06  format=F00956_. label="General med:CS - G";
  attrib V910_03  format=F00957_. label="General med:Iron tablets";
  attrib V910_04  format=F00958_. label="General med:Iron + Folic acid tablets";
  attrib V910_06  format=F00959_. label="General med:Insulin injections (Lente) (diabetes)";
  attrib V910_07  format=F00960_. label="General med:Isosorbide Dinitrate";
  attrib V910_08  format=F00961_. label="General med:CS - I";
  attrib V911_02  format=F00962_. label="General med:CS - K";
  attrib V913_01  format=F00963_. label="General med:Magnesium sulphate injection";
  attrib V913_02  format=F00964_. label="General med:Mebendazole";
  attrib V913_04  format=F00965_. label="General med:Metrodonizole IV";
  attrib V913_05  format=F00966_. label="General med:Metrodonizole tablets";
  attrib V913_06  format=F00967_. label="General med:Miconazole (vaginal pessaries)";
  attrib V913_07  format=F00968_. label="General med:Miconazole cream";
  attrib V913_10  format=F00969_. label="General med:Metformin tablets (diabetes)";
  attrib V913_11  format=F00970_. label="General med:Misoprostol tablets";
  attrib V913_12  format=F00971_. label="General med:Metrodonizole injection";
  attrib V913_13  format=F00972_. label="General med:CS - Mercurochrome";
  attrib V914_05  format=F00973_. label="General med:Nystatin (oral suspension)";
  attrib V914_06  format=F00974_. label="General med:Nystatin (vaginal pessaries/cream)";
  attrib V914_07  format=F00975_. label="General med:Nifedipine tablets";
  attrib V914_08  format=F00976_. label="General med:CS - N";
  attrib V915_01  format=F00977_. label="General med:Oral rehydration salts (ORS) sachets";
  attrib V915_02  format=F00978_. label="General med:Oxytocin or other injectable uterotonic";
  attrib V915_03  format=F00979_. label="General med:Omeprazole";
  attrib V915_04  format=F00980_. label="General med:CS - O";
  attrib V916_01  format=F00981_. label="General med:Penicillin Benzathine Benzyl for injection (repeated in V904_05)";
  attrib V916_03  format=F00982_. label="General med:Penicillin injection";
  attrib V916_06  format=F00983_. label="General med:Prednisolone";
  attrib V916_07  format=F00984_. label="General med:Paracetamol tablets";
  attrib V916_08  format=F00985_. label="General med:Paracetamol syrup";
  attrib V916_09  format=F00986_. label="General med:CS - P";
  attrib V917_04  format=F00987_. label="General med:Salbutamol inhaler";
  attrib V917_05  format=F00988_. label="General med:Simvastatin (high cholesterol)";
  attrib V917_06  format=F00989_. label="General med:CS - S";
  attrib V918_01  format=F00990_. label="General med:Tetracycline caps";
  attrib V918_02  format=F00991_. label="General med:Tetracycline eye ointment";
  attrib V918_04  format=F00992_. label="General med:Thiazide diuretic";
  attrib V918_05  format=F00993_. label="General med:Tetanus Toxoid vaccine";
  attrib V918_06  format=F00994_. label="General med:CS - T";
  attrib V919_03  format=F00995_. label="General med:Vitamin A capsules";
  attrib V919_04  format=F00996_. label="CS - General med:Vitamin A capsules 200,000 ui";
  attrib V919_05  format=F00997_. label="CS - General med:Vitamin A capsules 100,000 ui";
  attrib V919_06  format=F00998_. label="CS - General med:Vitamin C";
  attrib V920_02  format=F00999_. label="General med:Zinc tablets";
  attrib V921_01  format=F01000_. label="General med:Heparin injection";
  attrib V921_02  format=F01001_. label="General med:Hydrocortisone";
  attrib V921_03  format=F01002_. label="General med:CS - H";
  attrib V922_03  format=F01003_. label="Antimalarial:Fansidar (Sulfadoxine+Pyrimethamine (SP)";
  attrib V922_04  format=F01004_. label="Antimalarial:Quinine tablets";
  attrib V922_05  format=F01005_. label="Antimalarial:Quinine injection";
  attrib V922_11  format=F01006_. label="Antimalarial:Injectable artesunate";
  attrib V922_12  format=F01007_. label="Antimalarial:Artesunate suppositories";
  attrib V922_13  format=F01008_. label="Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 20 mg/120 mg";
  attrib V922_14  format=F01009_. label="Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 40 mg/240 mg";
  attrib V922_15  format=F01010_. label="Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 60 mg/360 mg";
  attrib V922_16  format=F01011_. label="Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 80 mg/480 mg";
  attrib V922_17  format=F01012_. label="Antimalarial:Other anti-malarial medicine";
  attrib V922_18  format=F01013_. label="Antimalarial:CS";
  attrib V923_01  format=F01014_. label="Anti-TB:Ethambutol tabs";
  attrib V923_02  format=F01015_. label="Anti-TB:Isoniazid tabs";
  attrib V923_03  format=F01016_. label="Anti-TB:Pyrazinamide";
  attrib V923_04  format=F01017_. label="Anti-TB:Rifampicin";
  attrib V923_05  format=F01018_. label="Anti-TB:Streptomycin injectable";
  attrib V923_06  format=F01019_. label="Anti-TB:Isoniazid + Rifampicin";
  attrib V923_07  format=F01020_. label="Anti-TB:Isoniazid + Rifampicin Pyrazinamide (RHZ) (3FDC)";
  attrib V923_08  format=F01021_. label="Anti-TB:Isoniazid + Ethambutol (EH) (2FDC)";
  attrib V923_09  format=F01022_. label="Anti-TB:Isoniazid+Rifampicin+Pyrazinamide+Ethambutol (4FDC)";
  attrib V923_11  format=F01023_. label="Anti-TB:Isoniazid+Rifampicin+Ethambutol (RHE) (3FDC)";
  attrib V923_12  format=F01024_. label="Anti-TB:CS";
  attrib V924_01  format=F01025_. label="IV solution: Normal saline/ Sodium chloride injectable solution";
  attrib V924_02  format=F01026_. label="IV solution: 5% dextrose-normal saline";
  attrib V924_03  format=F01027_. label="IV solution: Ringers Lactate";
  attrib V924_05  format=F01028_. label="IV solution: CS";
  attrib V926     format=F01029_. label="Expired medications observed: General, anti-malarial, IV solutions";
  attrib V927     format=F01030_. label="Medications organized by date of expiration: General, anti-malarial, IV solutions";
  attrib V928A    format=F01031_. label="Medication stored protected from sun: General, anti-malarial, IV solutions";
  attrib V928B1   format=F01032_. label="Medication stored off floor: General, anti-malarial, IV solutions";
  attrib V928B2   format=F01033_. label="Medication protected from water: General, anti-malarial, IV solutions";
  attrib V928C    format=F01034_. label="Medication room clean of evidence of rodents/pests: General, anti-malarial, IV solutions";
  attrib V928D    format=F01035_. label="Medication room well ventilated: General, anti-malarial, IV solutions";
  attrib V936     format=F01036_. label="Stock antiretroviral medications";
  attrib V946_06  format=F01037_. label="Other supply: Infusion set for IV solution";
  attrib V946_07  format=F01038_. label="Other supply: cannula for administering IV fluids (24G)";
  attrib V946_09  format=F01039_. label="Other supply: Latex gloves";
  attrib V946_12  format=F01040_. label="Other supply: Disinfecting solution";
  attrib V946_13  format=F01041_. label="Other supply: Hand washing soap";
  attrib V946_16  format=F01042_. label="Other supply: Single-use disposable syringes with needles or auto-disable syringes with needles";
  attrib V946_17  format=F01043_. label="Other supply: Alcohol-based hand rub";
  attrib V946_18  format=F01044_. label="Other supply: Insecticide treated mosquito nets (ITNs)";
  attrib V946_19  format=F01045_. label="Other supply: CS";
  attrib V950A    format=F01046_. label="TB medications: Stock monitoring system - computer or ledge/stock cards";
  attrib V950B    format=F01047_. label="TB medications: Stock monitoring system updated daily";
  attrib V951     format=F01048_. label="Expired medications observed: TB meds";
  attrib V952     format=F01049_. label="Medications organized by date of expiration: TB meds";
  attrib V953A    format=F01050_. label="Medication stored protected from sun: TB meds";
  attrib V953B1   format=F01051_. label="Medication stored off floor: TB meds";
  attrib V953B2   format=F01052_. label="Medication protected from water: TB meds";
  attrib V953C    format=F01053_. label="Medication room clean of evidence of rodents/pests: TB meds";
  attrib V953D    format=F01054_. label="Medication room well ventilated: TB meds";
  attrib V955_01  format=F01055_. label="ARV NRTIs: Zidovudine (ZDV,AZT) tablets";
  attrib V955_02  format=F01056_. label="ARV NRTIs: Zidovudine (ZDV,AZT) syrup";
  attrib V955_03  format=F01057_. label="ARV NRTIs: Abacavir (ABC) tablets";
  attrib V955_04  format=F01058_. label="ARV NRTIs: Didanosine (ddI) tablets";
  attrib V955_05  format=F01059_. label="ARV NRTIs: Lamivudine (3TC) tablets";
  attrib V955_06  format=F01060_. label="ARV NRTIs: Lamivudine (3TC) syrup";
  attrib V955_07  format=F01061_. label="ARV NRTIs: Stavudine 30 (D4T)";
  attrib V955_08  format=F01062_. label="ARV NRTIs: Stavudine syrup";
  attrib V955_09  format=F01063_. label="ARV NRTIs: Tenofovir Disoproxil Fumarate (TDF)";
  attrib V955_10  format=F01064_. label="ARV NRTIs: Emtricitabine (FTC)";
  attrib V955_11  format=F01065_. label="ARV NRTIs: CS";
  attrib V956_01  format=F01066_. label="ARV NNRTIs: Nevirapine (NVP) tablets";
  attrib V956_02  format=F01067_. label="ARV NNRTIs: Nevirapine (NVP) syrup";
  attrib V956_03  format=F01068_. label="ARV NNRTIs: Efavirenz (EFV) tablets";
  attrib V956_04  format=F01069_. label="ARV NNRTIs: Efavirenz (EFV) syrup";
  attrib V956_05  format=F01070_. label="ARV NNRTIs: Delavirdine (DLV)";
  attrib V956_06  format=F01071_. label="ARV NNRTIs: CS";
  attrib V957_01  format=F01072_. label="ARV Protease inhib: Lopinavir (LPV)";
  attrib V957_02  format=F01073_. label="ARV Protease inhib: Indinavir (IDV)";
  attrib V957_03  format=F01074_. label="ARV Protease inhib: Nelfinavir (NFV)";
  attrib V957_04  format=F01075_. label="ARV Protease inhib: Saquinavir (SQV)";
  attrib V957_05  format=F01076_. label="ARV Protease inhib: Ritonavir (RTV)";
  attrib V957_06  format=F01077_. label="ARV Protease inhib: Atazanavir (ATV)";
  attrib V957_07  format=F01078_. label="ARV Protease inhib: Fosamprenavier (FPV)";
  attrib V957_08  format=F01079_. label="ARV Protease inhib: Tipranavir (TPV)";
  attrib V957_09  format=F01080_. label="ARV Protease inhib: Darunavir (DRV)";
  attrib V957_10  format=F01081_. label="ARV Protease inhib: CS- Lopinavir+Ritonavir (LPV+RTV)";
  attrib V957_11  format=F01082_. label="ARV Protease inhib: CS - Atazanavir + Ritonavir (ATV+RTV)";
  attrib V957_12  format=F01083_. label="ARV Protease inhib: CS - Darunavir _ Ritonavir (DRV+RTV)";
  attrib V958_01  format=F01084_. label="ARV Fusion inhib/comb ARV: Enfuvirdite (T-20)";
  attrib V958_02  format=F01085_. label="ARV Fusion inhib/comb ARV: Stavudine+Lamivudine (D4T+3TC)";
  attrib V958_03  format=F01086_. label="ARV Fusion inhib/comb ARV: Stavudine+Lamivudine+Nevirapine (D4T+3TC+NVP)";
  attrib V958_04  format=F01087_. label="ARV Fusion inhib/comb ARV: Zidovudine +Lamivudine (AZT+3TC)";
  attrib V958_05  format=F01088_. label="ARV Fusion inhib/comb ARV: Zidovudine +Lamivudine +Abacavir (AZT+3TC+ABC)";
  attrib V958_06  format=F01089_. label="ARV Fusion inhib/comb ARV: Zidovudine +Lamivudine +Nevirapine (AZT+3TC+NVP)";
  attrib V958_07  format=F01090_. label="ARV Fusion inhib/comb ARV: Tenofovir+Emtricitabine (TDF+FTC)";
  attrib V958_08  format=F01091_. label="ARV Fusion inhib/comb ARV: Tenofovir+Lamivudine (TDF+3TC)";
  attrib V958_09  format=F01092_. label="ARV Fusion inhib/comb ARV: Tenofovir+Lamivudine+Efavirenz (TDF+3TC+EFV)";
  attrib V958_10  format=F01093_. label="ARV Fusion inhib/comb ARV: Tenofovir+Emtricitabine+Efavirenz (TDF+FTC+EFV)";
  attrib V958_11  format=F01094_. label="ARV Fusion inhib/comb ARV: CS";
  attrib V960A    format=F01095_. label="ARVs: Stock monitoring system - computer or ledge/stock cards";
  attrib V960B    format=F01096_. label="ARVs: Stock monitoring system updated daily";
  attrib V961     format=F01097_. label="Expired medications observed: ARVs";
  attrib V962     format=F01098_. label="Medications organized by date of expiration: ARVs";
  attrib V963A    format=F01099_. label="Medication stored protected from sun: ARVs";
  attrib V963B1   format=F01100_. label="Medication stored off floor: ARVs";
  attrib V963B2   format=F01101_. label="Medication protected from water: ARVs";
  attrib V963C    format=F01102_. label="Medication room clean of evidence of rodents/pests: ARVs";
  attrib V963D    format=F01103_. label="Medication room well ventilated: ARVs";
  attrib V1001    label="Malaria: Days a month service offered";
  attrib V1002    format=F01104_. label="Malaria: Providers in facility diagnose and/or prescribe treatment";
  attrib V1003    format=F01105_. label="Malaria: Use blood tests to diagnose";
  attrib V1004    format=F01106_. label="Malaria: Use malaria RDT kit to diagnose";
  attrib V1005A   format=F01107_. label="Malaria: Malaria RDT brand A Paracheck";
  attrib V1005B   format=F01108_. label="Malaria: Malaria RDT brand B Care Start";
  attrib V1005C   format=F01109_. label="Malaria: Malaria RDT brand C First Response";
  attrib V1005D   format=F01110_. label="Malaria: Malaria RDT brand D SD Bioline";
  attrib V1005E   format=F01111_. label="Malaria: Malaria RDT brand E";
  attrib V1005F   format=F01112_. label="Malaria: Malaria RDT brand F";
  attrib V1005X   format=F01113_. label="Malaria: Malaria RDT other brand";
  attrib V1006    format=F01114_. label="Malaria: Training manual, poster, job aid for Malaria RDT use";
  attrib V1007A   format=F01115_. label="Malaria protocol: National Guidelines for diagnosis and treatment of malaria";
  attrib V1007X   format=F01116_. label="Malaria protocol:Other guidelines for diagnosis and treatment of malaria";
  attrib V1100A   format=F01117_. label="TB service dx/tx/refer";
  attrib V1106    format=F01118_. label="Method to diagnose TB";
  attrib V1107A   format=F01119_. label="Agreement with referral site for TB";
  attrib V1107B   format=F01120_. label="Record of clients referred for TB";
  attrib V1107C   format=F01121_. label="Referral register on paper or computer";
  attrib V1109A   format=F01122_. label="Guidelines reported:National guidelines for diagnosis and treatment of TB";
  attrib V1109B   format=F01123_. label="Guidelines reported:Guidelines for the management of HIV and TB co-infection";
  attrib V1109C   format=F01124_. label="Guidelines reported:Guidelines related to MDR-TB treatment";
  attrib V1109D   format=F01125_. label="Guidelines reported:Guidelines for TB infection control";
  attrib V1115    format=F01126_. label="Treatment for TB";
  attrib V1138SRC format=F01127_. label="Service where IC information collected";
  attrib V1138    format=F01128_. label="Privacy for TB room";
  attrib V1139A1  format=F01129_. label="TB exam:Any running water";
  attrib V1139C   format=F01130_. label="TB exam:Soap";
  attrib V1139F   format=F01131_. label="TB exam:Sharps container";
  attrib V1139G   format=F01132_. label="TB exam:Clean/sterile latex gloves";
  attrib V1139J   format=F01133_. label="TB exam:Disinfectant";
  attrib V1139L   format=F01134_. label="TB exam:Autodestruct syringes";
  attrib V1139O   format=F01135_. label="TB exam:Pedal Bin waste receptacle with plastic liner";
  attrib V1139P   format=F01136_. label="TB exam:Other waste receptacle (not pedal bin)";
  attrib V1139Q   format=F01137_. label="TB exam:Alcohol-based hand rub";
  attrib V1139R   format=F01138_. label="TB exam:Medical masks";
  attrib V1139S   format=F01139_. label="TB exam:Gowns";
  attrib V1139T   format=F01140_. label="TB exam:Eye protection";
  attrib V1139U   format=F01141_. label="TB exam:Guidelines for standard precautions";
  attrib V1140    label="Days/month TB services available";
  attrib V1141    format=F01142_. label="System for testing TB clients for HIV infection";
  attrib V1142    format=F01143_. label="HIV RDT kit available in this service";
  attrib V1143    format=F01144_. label="Sputum containers at this service site for collecting sputum specimen";
  attrib V1144    format=F01145_. label="TB meds stored in service area or pharmacy";
  attrib SF1901   format=F01146_. label="CS*SANTE - Chronic TB patients are referred - Case de santé";
  attrib SF1902   format=F01147_. label="CS*SANTE - Feedback is given for referred patients - Case de santé";
  attrib SF1903   format=F01148_. label="CS*SANTE - DOTS used for TB - Case de santé";
  attrib SF1904   format=F01149_. label="CS*SANTE - Information for TB cases retained - Case de santé";
  attrib V1206    format=F01150_. label="Method for provision of PMTCT";
  attrib V1208A1  format=F01151_. label="Service offered:HIV counseling and testing services to pregnant women";
  attrib V1208A2  format=F01152_. label="Service offered:HIV testing services to infants born to HIV+ women";
  attrib V1208F   format=F01153_. label="Service offered:Infant feeding counseling for HIV+ women";
  attrib V1208G1  format=F01154_. label="Service offered:Nutrition counseling for HIV+ pregnant women and their infants";
  attrib V1208H   format=F01155_. label="Service offered:Family planning counseling to HIV+ pregnant women";
  attrib V1208L   format=F01156_. label="Service offered:ARV prophylaxis for HIV+ pregnant women";
  attrib V1208M   format=F01157_. label="Service offered:ARV prophylaxis for newborns of HIV+ women";
  attrib V1208S   format=F01158_. label="Service offered:PMTCT services with delivery services";
  attrib V1215A   format=F01159_. label="Observed guideline:National guidelines for PMTCT";
  attrib V1215B   format=F01160_. label="Observed guideline:Guidelines for infant and young child feeding counseling";
  attrib V1215X   format=F01161_. label="Observed guideline:Other guidelines for PMTCT";
  attrib V1215Y   format=F01162_. label="Observed guideline:No guidelines";
  attrib V1260    format=F01163_. label="Delivery services in facility";
  attrib V1273A   format=F01164_. label="ARV stocked in PMTCT unit:Nevirapine (NVP)";
  attrib V1273B   format=F01165_. label="ARV stocked in PMTCT unit:Zidovudine (AZT)";
  attrib V1273C   format=F01166_. label="ARV stocked in PMTCT unit:Zidovudine (AZT) and Lamivudine (3TC)";
  attrib V1273D   format=F01167_. label="ARV stocked in PMTCT unit:Lamivudine (3TC)";
  attrib V1273E   format=F01168_. label="ARV stocked in PMTCT unit:Lopinavir (LPV/r)";
  attrib V1273F   format=F01169_. label="ARV stocked in PMTCT unit:Abacavir (ABC)";
  attrib V1273G   format=F01170_. label="ARV stocked in PMTCT unit:Efavirenz (EFV)";
  attrib V1273H   format=F01171_. label="ARV stocked in PMTCT unit:Tenafovir disoproxil fulmarate (TDF)";
  attrib V1273I   format=F01172_. label="ARV stocked in PMTCT unit:Emtricitabine (ETC)";
  attrib V1273J   format=F01173_. label="ARV stocked in PMTCT unit:Nevirapine (NVP) syrup";
  attrib V1273K   format=F01174_. label="ARV stocked in PMTCT unit:Zidovudine (AZT) syrup";
  attrib V1273L   format=F01175_. label="ARV stocked in PMTCT unit:Lamivudine (3TC)+Efavirenz (EFV)+Tenafovir (TDF)";
  attrib V1273M   format=F01176_. label="ARV stocked in PMTCT unit:CS";
  attrib V1273Y   format=F01177_. label="ARV stocked in PMTCT unit:No ARV available from this clinic";
  attrib V1274    format=F01178_. label="HIV RDT available in service site";
  attrib V1275    format=F01179_. label="Dried blood spot filter paper (DBS) available in service site";
  attrib V1276SRC format=F01180_. label="Service where IC information collected";
  attrib V1276    format=F01181_. label="Privacy for PMTCT service site";
  attrib V1277A1  format=F01182_. label="PMTCT service site:Any running water";
  attrib V1277C   format=F01183_. label="PMTCT service site:Soap";
  attrib V1277F   format=F01184_. label="PMTCT service site:Sharps container";
  attrib V1277G   format=F01185_. label="PMTCT service site:Clean/sterile latex gloves";
  attrib V1277J   format=F01186_. label="PMTCT service site:Disinfectant";
  attrib V1277L   format=F01187_. label="PMTCT service site:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V1277O   format=F01188_. label="PMTCT service site:Pedal bin waste receptacle with lid and plastic liner";
  attrib V1277P   format=F01189_. label="PMTCT service site:Other waste receptacle";
  attrib V1277Q   format=F01190_. label="PMTCT service site:Alcohol-based hand rub";
  attrib V1277R   format=F01191_. label="PMTCT service site:Medical masks";
  attrib V1277S   format=F01192_. label="PMTCT service site:Gowns";
  attrib V1277T   format=F01193_. label="PMTCT service site:Eye protection";
  attrib V1277U   format=F01194_. label="PMTCT service site:Guidelines for standard precautions";
  attrib V1278A   format=F01195_. label="ARV stocked in delivery site for PMTCT:Nevirapine (NVP)";
  attrib V1278B   format=F01196_. label="ARV stocked in delivery site for PMTCT:Zidovudine (AZT)";
  attrib V1278C   format=F01197_. label="ARV stocked in delivery site for PMTCT:Zidovudine (AZT) and Lamivudine (3TC)";
  attrib V1278D   format=F01198_. label="ARV stocked in delivery site for PMTCT:Lamivudine (3TC)";
  attrib V1278E   format=F01199_. label="ARV stocked in delivery site for PMTCT:Lopinavir (LPV/r)";
  attrib V1278F   format=F01200_. label="ARV stocked in delivery site for PMTCT:Abacavir (ABC)";
  attrib V1278G   format=F01201_. label="ARV stocked in delivery site for PMTCT:Efavirenz (EFV)";
  attrib V1278H   format=F01202_. label="ARV stocked in delivery site for PMTCT:Tenafovir disoproxil fulmarate (TDF)";
  attrib V1278I   format=F01203_. label="ARV stocked in delivery site for PMTCT:Emtricitabine (ETC)";
  attrib V1278J   format=F01204_. label="ARV stocked in delivery site for PMTCT:Nevirapine (NVP) syrup";
  attrib V1278K   format=F01205_. label="ARV stocked in delivery site for PMTCT:Zidovudine (AZT) syrup";
  attrib V1278L   format=F01206_. label="ARV stocked in delivery site for PMTCT:Lamivudine(3TC) + Efavirenz(EFV) + Tenafovir (TDF)";
  attrib V1278M   format=F01207_. label="ARV stocked in delivery site for PMTCT:CS";
  attrib V1278Y   format=F01208_. label="ARV stocked in delivery site for PMTCT:No ARV available from this clinic";
  attrib V1279    format=F01209_. label="HIV RDT available in delivery service site for PMTCT";
  attrib V1280SRC format=F01210_. label="Service where IC information collected in delivery service site for PMTCT";
  attrib V1280    format=F01211_. label="Privacy for PMTCT service in delivery site";
  attrib V1281A1  format=F01212_. label="Delivery service site for PMTCT:Any running water";
  attrib V1281C   format=F01213_. label="Delivery service site for PMTCT:Soap";
  attrib V1281F   format=F01214_. label="Delivery service site for PMTCT:Sharps container";
  attrib V1281G   format=F01215_. label="Delivery service site for PMTCT:Clean/sterile latex gloves";
  attrib V1281J   format=F01216_. label="Delivery service site for PMTCT:Disinfectant";
  attrib V1281L   format=F01217_. label="Delivery service site for PMTCT:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V1281O   format=F01218_. label="Delivery service site for PMTCT:Pedal bin waste receptacle with lid and plastic liner";
  attrib V1281P   format=F01219_. label="Delivery service site for PMTCT:Other waste receptacle";
  attrib V1281Q   format=F01220_. label="Delivery service site for PMTCT:Alcohol-based hand rub";
  attrib V1281R   format=F01221_. label="Delivery service site for PMTCT:Medical masks";
  attrib V1281S   format=F01222_. label="Delivery service site for PMTCT:Gowns";
  attrib V1281T   format=F01223_. label="Delivery service site for PMTCT:Eye protection";
  attrib V1281U   format=F01224_. label="Delivery service site for PMTCT:Guidelines for standard precautions";
  attrib V1307A   label="Days/month HIV testing services available";
  attrib V1327D   format=F01225_. label="Procedure for HIV test:Client referred to other unit in this facility";
  attrib V1327E   format=F01226_. label="Procedure for HIV test:Blood drawn by this unit staff, sent to lab";
  attrib V1327G   format=F01227_. label="Procedure for HIV test:Client sent to lab in this facility";
  attrib V1327H   format=F01228_. label="Procedure for HIV test:Rapid test done in clinic/unit";
  attrib V1327I   format=F01229_. label="Procedure for HIV test:Client sent outside facility";
  attrib V1327J   format=F01230_. label="Procedure for HIV test:Blood drawn in unit and sent outside facility for testing";
  attrib V1328SRC format=F01231_. label="Service where IC information collected";
  attrib V1328    format=F01232_. label="Privacy for blood draw room";
  attrib V1329A1  format=F01233_. label="Blood draw room:Any running water";
  attrib V1329C   format=F01234_. label="Blood draw room:Soap";
  attrib V1329F   format=F01235_. label="Blood draw room:Sharps container";
  attrib V1329G   format=F01236_. label="Blood draw room:Clean/sterile latex gloves";
  attrib V1329J   format=F01237_. label="Blood draw room:Disinfectant";
  attrib V1329L   format=F01238_. label="Blood draw room:Autodestruct syringes";
  attrib V1329O   format=F01239_. label="Blood draw room:Pedal Bin waste receptacle with plastic liner";
  attrib V1329P   format=F01240_. label="Blood draw room:Other waste receptacle (not pedal bin)";
  attrib V1329Q   format=F01241_. label="Blood draw room:Alcohol-based hand rub";
  attrib V1329R   format=F01242_. label="Blood draw room:Medical masks";
  attrib V1329S   format=F01243_. label="Blood draw room:Gowns";
  attrib V1329T   format=F01244_. label="Blood draw room:Eye protection";
  attrib V1329U   format=F01245_. label="Blood draw room:Guidelines for standard precautions";
  attrib V1331    format=F01246_. label="Guidelines in unit";
  attrib V1332A   format=F01247_. label="National protocol available:National HIV counseling and testing guidelines";
  attrib V1332B   format=F01248_. label="National protocol available:Written protocols for PEP";
  attrib V1332X   format=F01249_. label="National protocol available:Other guidelines on HIV testing";
  attrib V1337    format=F01250_. label="Individual client chart";
  attrib V1355    format=F01251_. label="Sample HIV RDT kit";
  attrib V1356    format=F01252_. label="Staff in this facility have access to HIV PEP";
  attrib V1357    format=F01253_. label="Have condoms available in service site for HIV counsel/testing clients";
  attrib V1358    format=F01254_. label="Have agreement with referral site for return of results of outside HIV tests";
  attrib V1400A   format=F01255_. label="Type of ART services available";
  attrib V1415A   format=F01256_. label="Pre-ART baseline:Hemoglobin/hematocrit";
  attrib V1415B   format=F01257_. label="Pre-ART baseline:Full blood count";
  attrib V1415C   format=F01258_. label="Pre-ART baseline:Pregnancy test for women";
  attrib V1415D   format=F01259_. label="Pre-ART baseline:Renal function tests (including serum creatinine and U&E)";
  attrib V1415E   format=F01260_. label="Pre-ART baseline:Urinalysis";
  attrib V1415F   format=F01261_. label="Pre-ART baseline:Liver function tests";
  attrib V1415G   format=F01262_. label="Pre-ART baseline:TB sputum test";
  attrib V1415H   format=F01263_. label="Pre-ART baseline:Chest X-ray";
  attrib V1415I   format=F01264_. label="Pre-ART baseline:Hepatitis B";
  attrib V1415J   format=F01265_. label="Pre-ART baseline:CD4 T-cell count";
  attrib V1415K   format=F01266_. label="Pre-ART baseline:HIV RNA viral load";
  attrib V1415X   format=F01267_. label="Pre-ART baseline:Other routine tests";
  attrib V1421A   format=F01268_. label="Guideline available:National ART guidelines";
  attrib V1421X   format=F01269_. label="Guideline available:Other ART guidelines";
  attrib V1451    format=F01270_. label="ART meds stored in service area or pharmacy";
  attrib V1452A   format=F01271_. label="Monitor ART:Hemoglobin/hematocrit";
  attrib V1452B   format=F01272_. label="Monitor ART:Full blood count";
  attrib V1452C   format=F01273_. label="Monitor ART:Pregnancy test for women";
  attrib V1452D   format=F01274_. label="Monitor ART:Renal function tests (including serum creatinine and U&E)";
  attrib V1452E   format=F01275_. label="Monitor ART:Urinalysis";
  attrib V1452F   format=F01276_. label="Monitor ART:Liver function tests";
  attrib V1452G   format=F01277_. label="Monitor ART:TB sputum test";
  attrib V1452H   format=F01278_. label="Monitor ART:Chest X-ray";
  attrib V1452I   format=F01279_. label="Monitor ART:Hepatitis B";
  attrib V1452J   format=F01280_. label="Monitor ART:CD4 T-cell count";
  attrib V1452K   format=F01281_. label="Monitor ART:HIV RNA viral load";
  attrib V1452X   format=F01282_. label="Monitor ART::Other routine tests";
  attrib SF2102   format=F01283_. label="CS - Type of follow-up treatment for HIV+ clients";
  attrib V1515    format=F01284_. label="Provision of clinical care or support services for HIV/AIDS clients";
  attrib V1516A   format=F01285_. label="Curative HIV care:Prescribe treatment for opportunistic infections/symptoms";
  attrib V1516B   format=F01286_. label="Curative HIV care:Provide systemic IV treatment of specific fungal infections";
  attrib V1516C   format=F01287_. label="Curative HIV care:Provide treatment for Kaposi's sarcoma";
  attrib V1516D   format=F01288_. label="Curative HIV care:Provide/prescribe palliative care";
  attrib V1516E   format=F01289_. label="Curative HIV care:Provide nutritional rehabilitation";
  attrib V1516F   format=F01290_. label="Curative HIV care:Prescribe/provide fortified protein supplementation (FPS)";
  attrib V1516J   format=F01291_. label="Curative HIV care:Provide pediatric care for HIV+ children";
  attrib V1518B   format=F01292_. label="Preventive services for HIV+ clients:INH (preventive tx for TB)";
  attrib V1518C   format=F01293_. label="Preventive services for HIV+ clients:Primary preventive care (ex: CPT)";
  attrib V1518D   format=F01294_. label="Preventive services for HIV+ clients:Provide/prescribe micronutrient supplementation";
  attrib V1518E   format=F01295_. label="Preventive services for HIV+ clients:General family planning counseling/services for health reasons related to HIV/AIDS";
  attrib V1518F   format=F01296_. label="Preventive services for HIV+ clients:Provide condoms to prevent further transmission of HIV/AIDS";
  attrib V1521A   format=F01297_. label="National protocol available:National guidelines for clinical management of HIV/AIDS";
  attrib V1521B   format=F01298_. label="National protocol available:Guidelines for palliative care of HIV/AIDS+ clients";
  attrib V1560    format=F01299_. label="HIV care and support: TB routine screening and testing HIV+ clients";
  attrib V1561    format=F01300_. label="HIV care and support: Condoms available at site for clients receiving services";
  attrib V1601    format=F01301_. label="N-C diseases:Diagnose and/or manage diabetes";
  attrib V1601A   format=F01302_. label="N-C diseases:Diagnose and/or manage diabetes (specific)";
  attrib V1602A   format=F01303_. label="N-C diseases:National guidelines for the diagnosis/management of diabetes";
  attrib V1602X   format=F01304_. label="N-C diseases:Other guidelines for the diagnosis/management of diabetes";
  attrib V1603    format=F01305_. label="N-C diseases:Diagnose and/or manage cardio-vascular diseases";
  attrib V1603A   format=F01306_. label="N-C diseases:Diagnose and/or manage cardio-vascular diseases (specific)";
  attrib V1604A   format=F01307_. label="N-C diseases:National guidelines for the diagnosis/management of cardio-vascular diseases";
  attrib V1604X   format=F01308_. label="N-C diseases:Other guidelines for the diagnosis/management of cardio-vascular diseases";
  attrib V1605    format=F01309_. label="N-C diseases:Diagnose and/or manage chronic respiratory diseases (COPD)";
  attrib V1605A   format=F01310_. label="N-C diseases:Diagnose and/or manage chronic respiratory diseases (specific)";
  attrib V1606A   format=F01311_. label="N-C diseases:National guidelines for the diagnosis/management of chronic respiratory diseases (COPD)";
  attrib V1606X   format=F01312_. label="N-C diseases:Other guidelines for the diagnosis/management of chronic respiratory diseases (COPD)";
  attrib V1607SRC format=F01313_. label="N-C diseases:Source for basic supplies/equipment";
  attrib V1607A   format=F01314_. label="N-C diseases: Measuring tape (for circumference)";
  attrib V1607B   format=F01315_. label="N-C diseases: Spacers for inhalers";
  attrib V1607C   format=F01316_. label="N-C diseases: IV infusion kits - adult";
  attrib V1607D   format=F01317_. label="N-C diseases: IV infusion kits - pediatric";
  attrib V1608A   format=F01318_. label="N-C diseases: Adult weighing scale";
  attrib V1608B   format=F01319_. label="N-C diseases:Child weighing scale (250 g graduation)";
  attrib V1608C   format=F01320_. label="N-C diseases: Infant weighting scale (100 g graduation)";
  attrib V1608D   format=F01321_. label="N-C diseases: Stadiometer (or height rod)";
  attrib V1608E   format=F01322_. label="N-C diseases: Thermometer";
  attrib V1608F   format=F01323_. label="N-C diseases: Stethoscope";
  attrib V1608G   format=F01324_. label="N-C diseases: Digital BP apparatus";
  attrib V1608H   format=F01325_. label="N-C diseases: Manual BP apparatus";
  attrib V1608I   format=F01326_. label="N-C diseases: Light source (flashlight OK)";
  attrib V1608J   format=F01327_. label="N-C diseases: Self-inflating bag and mask (adult)";
  attrib V1608K   format=F01328_. label="N-C diseases: Self-inflating bag and mask (pediatric)";
  attrib V1608L   format=F01329_. label="N-C diseases: Micronebulizer";
  attrib V1608M   format=F01330_. label="N-C diseases: Peak flow meters";
  attrib V1608N   format=F01331_. label="N-C diseases: Pulse oximeter";
  attrib V1608O   format=F01332_. label="N-C diseases: Oxygen concentrators";
  attrib V1608P   format=F01333_. label="N-C diseases: Filled oxygen cylinder";
  attrib V1608Q   format=F01334_. label="N-C diseases: Oxygen distribution system";
  attrib V1609SRC format=F01335_. label="Service where IC information collected for non-communicable diseases";
  attrib V1609    format=F01336_. label="Non-communicable diseases:Privacy for observed room";
  attrib V1610A1  format=F01337_. label="Non-communicable diseases: Running water";
  attrib V1610C   format=F01338_. label="Non-communicable diseases:Soap";
  attrib V1610F   format=F01339_. label="Non-communicable diseases:Sharps container";
  attrib V1610G   format=F01340_. label="Non-communicable diseases:Clean/sterile latex gloves";
  attrib V1610J   format=F01341_. label="Non-communicable diseases:Disinfectant";
  attrib V1610L   format=F01342_. label="Non-communicable diseases:Autodestruct syringes with needles or single-use disposable syringes with needles";
  attrib V1610O   format=F01343_. label="Non-communicable diseases:Pedal Bin waste receptacle with lid and plastic liner";
  attrib V1610P   format=F01344_. label="Non-communicable diseases:Other waste receptacle (not pedal bin)";
  attrib V1610Q   format=F01345_. label="Non-communicable diseases:Alcohol-based hand rub";
  attrib V1610R   format=F01346_. label="Non-communicable diseases:Medical masks";
  attrib V1610S   format=F01347_. label="Non-communicable diseases:Gowns";
  attrib V1610T   format=F01348_. label="Non-communicable diseases:Eye protection";
  attrib V1610U   format=F01349_. label="Non-communicable diseases:Guidelines for standard precautions";
  attrib V1701    format=F01350_. label="Guidelines for Integrated management of emergency and essential surgical care (IMEESC)";
  attrib V1702A   format=F01351_. label="Minor surgical: Needle holder";
  attrib V1702B   format=F01352_. label="Minor surgical: scalpel handle with blade";
  attrib V1702C   format=F01353_. label="Minor surgical: Retractor";
  attrib V1702D   format=F01354_. label="Minor surgical: Surgical scissors";
  attrib V1702E   format=F01355_. label="Minor surgical: Nasogastric tube (10-17g)";
  attrib V1702F   format=F01356_. label="Minor surgical: Tourniquet";
  attrib V1703A   format=F01357_. label="Minor surgical materials: Absorbable suture material";
  attrib V1703B   format=F01358_. label="Minor surgical materials: Non-absorbable suture material";
  attrib V1703C   format=F01359_. label="Minor surgical materials: Skin disinfectant";
  attrib V1703D   format=F01360_. label="Minor surgical materials: Lidocaine/ lignocaine injection";
  attrib V1703E   format=F01361_. label="Minor surgical materials: Ketamine injection";
  attrib V1704SRC format=F01362_. label="Source for infection control for minor surgery service";
  attrib V1704    format=F01363_. label="Privacy for minor surgery service";
  attrib V1705A1  format=F01364_. label="Minor surgery:Running water";
  attrib V1705C   format=F01365_. label="Minor surgery:Soap";
  attrib V1705F   format=F01366_. label="Minor surgery:Sharps container";
  attrib V1705G   format=F01367_. label="Minor surgery:Disposable latex gloves";
  attrib V1705J   format=F01368_. label="Minor surgery:Disinfectant";
  attrib V1705L   format=F01369_. label="Minor surgery:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V1705O   format=F01370_. label="Minor surgery:Pedal bin waste receptacle with lid with plastic liner";
  attrib V1705P   format=F01371_. label="Minor surgery:Other waste receptacle";
  attrib V1705Q   format=F01372_. label="Minor surgery:Alcohol-based hand rub";
  attrib V1705R   format=F01373_. label="Minor surgery:Medical masks";
  attrib V1705S   format=F01374_. label="Minor surgery:Gowns";
  attrib V1705T   format=F01375_. label="Minor surgery:Eye protection";
  attrib V1705U   format=F01376_. label="Minor surgery:Guidelines for standard precautions";
  attrib V2000A   label="Number of eligible providers";
  attrib V2000B   label="Number of providers with any training last 2 years";
  attrib V2000C   label="Number of providers supervised last 6 months";
  attrib V2000D   label="Number of providers with supervision & any training last 2 years";
  attrib V2000E   label="Number of providers with IMCI training";
  attrib V2000F   label="Number of providers with growth management training";
  attrib V2000G   label="Number of providers with EPI training";
  attrib V2001A   label="Child health: n of providers in service";
  attrib V2001B   label="Child health: n of providers in service with any IMCI, GM or EPI training last 2 years";
  attrib V2001C   label="Child health: n of providers in service and supervised";
  attrib V2001D   label="Child health: n of providers in service with supervision & training last 2 years";
  attrib V2002A   label="FP: n of providers in service";
  attrib V2002B   label="FP: n of providers in service with training last 2 years";
  attrib V2002C   label="FP: n of providers in service and supervised";
  attrib V2002D   label="FP: n of providers in service with supervision & training last 2 years";
  attrib V2003A   label="ANC: n of providers in service";
  attrib V2003B   label="ANC: n of providers in service with training last 2 years";
  attrib V2003C   label="ANC: n of providers in service and supervised";
  attrib V2003D   label="ANC: n of providers in service with supervision & training last 2 years";
  attrib V2004A   label="Delivery: n of providers in service";
  attrib V2004B   label="Delivery: n of providers in service with training last 2 years";
  attrib V2004C   label="Delivery: n of providers in service and supervised";
  attrib V2004D   label="Delivery: n of providers in service with supervision & training last 2 years";
  attrib V2005A   label="CSS: n of providers in service";
  attrib V2005B   label="CSS: n of providers in service with training last 2 years";
  attrib V2005C   label="CSS: n of providers in service and supervised";
  attrib V2005D   label="CSS: n of providers in service with supervision & training last 2 years";
  attrib V2006A   label="ART: n of providers in service";
  attrib V2006B   label="ART: n of providers in service with training last 2 years";
  attrib V2006C   label="ART: n of providers in service and supervised";
  attrib V2006D   label="ART: n of providers in service with supervision & training last 2 years";
  attrib V2007A   label="STI: n of providers in service";
  attrib V2007B   label="STI: n of providers in service with training last 2 years";
  attrib V2007C   label="STI: n of providers in service and supervised";
  attrib V2007D   label="STI: n of providers in service with supervision & training last 2 years";
  attrib V2008A   label="Diabetes: n of providers in service";
  attrib V2008B   label="Diabetes: n of providers in service with training last 2 years";
  attrib V2008C   label="Diabetes: n of providers in service and supervised";
  attrib V2008D   label="Diabetes: n of providers in service with supervision & training last 2 years";
  attrib V2009A   label="Cardio: n of providers in service";
  attrib V2009B   label="Cardio: n of providers in service with training last 2 years";
  attrib V2009C   label="Cardio: n of providers in service and supervised";
  attrib V2009D   label="Cardio: n of providers in service with supervision & training last 2 years";
  attrib V2010A   label="Respiratory: n of providers in service";
  attrib V2010B   label="Respiratory: n of providers in service with training last 2 years";
  attrib V2010C   label="Respiratory: n of providers in service and supervised";
  attrib V2010D   label="Respiratory: n of providers in service with supervision & training last 2 years";
  attrib V2011A   label="TB: n of providers in service";
  attrib V2011B   label="TB: n of providers in service with training last 2 years";
  attrib V2011C   label="TB: n of providers in service and supervised";
  attrib V2011D   label="TB: n of providers in service with supervision & training last 2 years";
  attrib V2012A   label="Malaria: n of providers in service";
  attrib V2012B   label="Malaria: n of providers in service with training last 2 years";
  attrib V2012C   label="Malaria: n of providers in service and supervised";
  attrib V2012D   label="Malaria: n of providers in service with supervision & training last 2 years";
infile 'C:\SNSPA_YEAR1\EXPORT\JEANNE\EXPORT\SNFC6IFLSR.DAT' LRECL=2054 MISSOVER ;
    input
    @1    INV_ID   5.0
    @6    V000     $3.
    @9    V001     2.0
    @11   V002     3.0
    @14   V003     1.0
    @15   V004     5.0
    @20   V005     8.0
    @28   V007     2.0
    @30   V008     2.0
    @32   V009     3.0
    @35   V010     1.0
    @36   V010A    1.0
    @37   V010B    1.0
    @38   V010C    1.0
    @39   V012B    1.0
    @40   V012C    1.0
    @41   V012D    1.0
    @42   V013     1.0
    @43   V013S    1.0
    @44   V014A    1.0
    @45   V015A    1.0
    @46   V015C    1.0
    @47   V016     1.0
    @48   V018     1.0
    @49   V019     1.0
    @50   V019A    1.0
    @51   V033     1.0
    @52   V034     1.0
    @53   V034A    1.0
    @54   V034B    1.0
    @55   V035     1.0
    @56   V035A    1.0
    @57   V035B    1.0
    @58   V042     1.0
    @59   V043     1.0
    @60   V044     1.0
    @61   V045     1.0
    @62   V045A    1.0
    @63   V046     1.0
    @64   V047     1.0
    @65   V048     1.0
    @66   V049     1.0
    @67   V052     2.0
    @69   V058A    2.0
    @71   V058     2.0
    @73   V071     2.0
    @75   V071A    2.0
    @77   V071C    2.0
    @79   V072     2.0
    @81   V072A    2.0
    @83   V072C    2.0
    @85   V073     2.0
    @87   V073A    2.0
    @89   V073C    2.0
    @91   V076     3.0
    @94   V077     3.0
    @97   V078     3.0
    @100  V081     2.0
    @102  V082     4.0
    @106  V083     2.0
    @108  V095A    4.0
    @112  V095B    4.0
    @116  SFSPA    1.0
    @117  V101A    1.0
    @118  V102N    2.0
    @120  V102A_01 2.0
    @122  V102A_02 2.0
    @124  V102A_03 2.0
    @126  V102A_04 2.0
    @128  V102A_05 2.0
    @130  V102A_06 2.0
    @132  V102A_07 2.0
    @134  V102A_08 2.0
    @136  V102A_09 2.0
    @138  V102A_10 2.0
    @140  V102A_11 2.0
    @142  V102A_12 2.0
    @144  V102A_13 2.0
    @146  V102A_14 2.0
    @148  V102A_15 2.0
    @150  V102A_16 2.0
    @152  V102A_17 2.0
    @154  V102A_18 2.0
    @156  V102A_19 2.0
    @158  V102A_20 2.0
    @160  V102A_21 2.0
    @162  V102A_22 2.0
    @164  V102A_23 2.0
    @166  V102A_24 2.0
    @168  V102A_25 2.0
    @170  V102A_26 2.0
    @172  V102A_27 2.0
    @174  V102D_01 4.0
    @178  V102D_02 4.0
    @182  V102D_03 4.0
    @186  V102D_04 4.0
    @190  V102D_05 4.0
    @194  V102D_06 4.0
    @198  V102D_07 4.0
    @202  V102D_08 4.0
    @206  V102D_09 4.0
    @210  V102D_10 4.0
    @214  V102D_11 4.0
    @218  V102D_12 4.0
    @222  V102D_13 4.0
    @226  V102D_14 4.0
    @230  V102D_15 4.0
    @234  V102D_16 4.0
    @238  V102D_17 4.0
    @242  V102D_18 4.0
    @246  V102D_19 4.0
    @250  V102D_20 4.0
    @254  V102D_21 4.0
    @258  V102D_22 4.0
    @262  V102D_23 4.0
    @266  V102D_24 4.0
    @270  V102D_25 4.0
    @274  V102D_26 4.0
    @278  V102D_27 4.0
    @282  V102E_01 4.0
    @286  V102E_02 4.0
    @290  V102E_03 4.0
    @294  V102E_04 4.0
    @298  V102E_05 4.0
    @302  V102E_06 4.0
    @306  V102E_07 4.0
    @310  V102E_08 4.0
    @314  V102E_09 4.0
    @318  V102E_10 4.0
    @322  V102E_11 4.0
    @326  V102E_12 4.0
    @330  V102E_13 4.0
    @334  V102E_14 4.0
    @338  V102E_15 4.0
    @342  V102E_16 4.0
    @346  V102E_17 4.0
    @350  V102E_18 4.0
    @354  V102E_19 4.0
    @358  V102E_20 4.0
    @362  V102E_21 4.0
    @366  V102E_22 4.0
    @370  V102E_23 4.0
    @374  V102E_24 4.0
    @378  V102E_25 4.0
    @382  V102E_26 4.0
    @386  V102E_27 4.0
    @390  V102DT   4.0
    @394  V102ET   4.0
    @398  V102T    4.0
    @402  V105     1.0
    @403  V106     1.0
    @404  V106A    1.0
    @405  V106B    1.0
    @406  V106C    1.0
    @407  V106D    1.0
    @408  V106E    1.0
    @409  V106F    1.0
    @410  V106G    1.0
    @411  V106H    1.0
    @412  V106X    1.0
    @413  V106Y    1.0
    @414  V106AA   1.0
    @415  V106BB   1.0
    @416  V107     1.0
    @417  V108     1.0
    @418  V109A    1.0
    @419  V109B    1.0
    @420  V109C    1.0
    @421  V109D    1.0
    @422  V109E    1.0
    @423  V109F    1.0
    @424  V109G    1.0
    @425  V109H    1.0
    @426  V109X    1.0
    @427  V109Y    1.0
    @428  V109Z    1.0
    @429  V110A    1.0
    @430  V110     1.0
    @431  V112     1.0
    @432  V115     1.0
    @433  V116G    1.0
    @434  V116H    1.0
    @435  V116I    1.0
    @436  V120A    1.0
    @437  V120     1.0
    @438  V121A    1.0
    @439  V121B    1.0
    @440  V121C    1.0
    @441  V123     2.0
    @443  V124     1.0
    @444  V126     1.0
    @445  V127A    1.0
    @446  V127B    1.0
    @447  V127C    1.0
    @448  V127     1.0
    @449  V128     1.0
    @450  V128A    1.0
    @451  V129     1.0
    @452  V130     1.0
    @453  V131     1.0
    @454  V132     1.0
    @455  V133     1.0
    @456  V134     5.0
    @461  V135     5.0
    @466  V142     1.0
    @467  V143     4.0
    @471  V144     1.0
    @472  V144A    1.0
    @473  V144B    1.0
    @474  V144E    1.0
    @475  V144F    1.0
    @476  V144G    1.0
    @477  V144J    1.0
    @478  V144K    1.0
    @479  V144L    1.0
    @480  V144M    1.0
    @481  V144N    1.0
    @482  V144O    1.0
    @483  V144P    1.0
    @484  V144Q    1.0
    @485  V144R    1.0
    @486  V144S    1.0
    @487  V144T    1.0
    @488  V144U    1.0
    @489  V145     1.0
    @490  V145A    1.0
    @491  V145B    1.0
    @492  V145C    1.0
    @493  V145D    1.0
    @494  V145X    1.0
    @495  V146A    1.0
    @496  V146B    1.0
    @497  V146C    1.0
    @498  V146D    1.0
    @499  V146E    1.0
    @500  V146F    1.0
    @501  V146G    1.0
    @502  V146H    1.0
    @503  V146I    1.0
    @504  V146X    1.0
    @505  V146Y    1.0
    @506  V146Z    1.0
    @507  V150     1.0
    @508  V152     1.0
    @509  V153     1.0
    @510  V153A    2.0
    @512  V154     2.0
    @514  V154A    1.0
    @515  V154B    1.0
    @516  V154C    1.0
    @517  V154D    1.0
    @518  V154E    1.0
    @519  V154F    1.0
    @520  V154G    1.0
    @521  V154H    1.0
    @522  V165A    1.0
    @523  V165B    1.0
    @524  V165C    1.0
    @525  V165D    1.0
    @526  V165E    1.0
    @527  V165F    1.0
    @528  V165G    1.0
    @529  V165H    1.0
    @530  V165I    1.0
    @531  V165J    1.0
    @532  V165K    1.0
    @533  V166A    1.0
    @534  V166B    1.0
    @535  V166C    1.0
    @536  V166D    1.0
    @537  V166E    1.0
    @538  V166F    1.0
    @539  V166G    1.0
    @540  V166H    1.0
    @541  V166I    1.0
    @542  V166J    1.0
    @543  V166K    1.0
    @544  V166L    1.0
    @545  V166M    1.0
    @546  V166N    1.0
    @547  V166O    1.0
    @548  V166P    1.0
    @549  V166Q    1.0
    @550  V166XA   1.0
    @551  V166XB   1.0
    @552  V166XC   1.0
    @553  V166XD   1.0
    @554  V166XE   1.0
    @555  V166XF   1.0
    @556  V166XG   1.0
    @557  V166XH   1.0
    @558  V166XI   1.0
    @559  V166XJ   1.0
    @560  V166XK   1.0
    @561  V166XL   1.0
    @562  V166XM   1.0
    @563  V166XN   1.0
    @564  V166XO   1.0
    @565  V166XP   1.0
    @566  V166XQ   1.0
    @567  V166XR   1.0
    @568  V166XS   1.0
    @569  V167     1.0
    @570  V168A1   1.0
    @571  V168C    1.0
    @572  V168F    1.0
    @573  V168G    1.0
    @574  V168J    1.0
    @575  V168L    1.0
    @576  V168O    1.0
    @577  V168P    1.0
    @578  V168Q    1.0
    @579  V168R    1.0
    @580  V168S    1.0
    @581  V168T    1.0
    @582  V168U    1.0
    @583  V173A    1.0
    @584  V173     1.0
    @585  V175A_1  1.0
    @586  V175A_2  1.0
    @587  V175A_3  1.0
    @588  V175A_4  1.0
    @589  V175A_5  1.0
    @590  V175A_6  1.0
    @591  V175A_7  1.0
    @592  V175A_8  1.0
    @593  V175B_1  1.0
    @594  V175B_2  1.0
    @595  V175B_3  1.0
    @596  V175B_4  1.0
    @597  V175B_5  1.0
    @598  V175B_6  1.0
    @599  V175B_7  1.0
    @600  V175B_8  1.0
    @601  V175C_1  3.0
    @604  V175C_2  3.0
    @607  V175C_3  3.0
    @610  V175C_4  3.0
    @613  V175C_5  3.0
    @616  V175C_6  3.0
    @619  V175C_7  3.0
    @622  V175C_8  3.0
    @625  V175D_1  3.0
    @628  V175D_2  3.0
    @631  V175D_3  3.0
    @634  V175D_4  3.0
    @637  V175D_5  3.0
    @640  V175D_6  3.0
    @643  V175D_7  3.0
    @646  V175D_8  3.0
    @649  V175E_1  1.0
    @650  V175E_2  1.0
    @651  V175E_3  1.0
    @652  V175E_4  1.0
    @653  V175E_5  1.0
    @654  V175E_6  1.0
    @655  V175E_7  1.0
    @656  V175E_8  1.0
    @657  V175F_1  3.0
    @660  V175F_2  3.0
    @663  V175F_3  3.0
    @666  V175F_4  3.0
    @669  V175F_5  3.0
    @672  V175F_6  3.0
    @675  V175F_7  3.0
    @678  V175F_8  3.0
    @681  V175G_1  3.0
    @684  V175G_2  3.0
    @687  V175G_3  3.0
    @690  V175G_4  3.0
    @693  V175G_5  3.0
    @696  V175G_6  3.0
    @699  V175G_7  3.0
    @702  V175G_8  3.0
    @705  V175H_1  2.0
    @707  V175H_2  2.0
    @709  V175H_3  2.0
    @711  V175H_4  2.0
    @713  V175H_5  2.0
    @715  V175H_6  2.0
    @717  V175H_7  2.0
    @719  V175H_8  2.0
    @721  V176A    1.0
    @722  V176B    1.0
    @723  V176C    1.0
    @724  V177A    1.0
    @725  V177B    1.0
    @726  V177C    1.0
    @727  V177D    1.0
    @728  V177E    1.0
    @729  V177F    1.0
    @730  V177G    1.0
    @731  V178B    1.0
    @732  V184A    2.0
    @734  V185     1.0
    @735  V186A    2.0
    @737  V187     1.0
    @738  V189     1.0
    @739  V190     1.0
    @740  V192FC   1.0
    @741  V192CP   1.0
    @742  V192A    1.0
    @743  V192B    1.0
    @744  V192C    1.0
    @745  V192T    1.0
    @746  V193FC   1.0
    @747  V193CP   1.0
    @748  V193WA   1.0
    @749  V193WB   1.0
    @750  V193UA   1.0
    @751  V193UB   1.0
    @752  V193T    1.0
    @753  V193PE   1.0
    @754  V193PG   1.0
    @755  V193TE   1.0
    @756  V193TG   1.0
    @757  V194FC   1.0
    @758  V194CP   1.0
    @759  V194A    1.0
    @760  V194B    1.0
    @761  V194T    1.0
    @762  V195FC   1.0
    @763  V195CP   1.0
    @764  V195A    1.0
    @765  V195B    1.0
    @766  V195C    1.0
    @767  V195T    1.0
    @768  VT101    1.0
    @769  VT102    1.0
    @770  VT103    1.0
    @771  VT104    1.0
    @772  VT105    1.0
    @773  VT106    1.0
    @774  VT107    1.0
    @775  VT201    1.0
    @776  VT202    1.0
    @777  VT301    1.0
    @778  VT302    1.0
    @779  VT303    1.0
    @780  VT801    1.0
    @781  VT802    1.0
    @782  VT803    1.0
    @783  VT804    1.0
    @784  VT805    1.0
    @785  VT806    1.0
    @786  VT807    1.0
    @787  VT808    1.0
    @788  VT809    1.0
    @789  VT810    1.0
    @790  VT811    1.0
    @791  VT812    1.0
    @792  VT813    1.0
    @793  VT814    1.0
    @794  VT815    1.0
    @795  VT816    1.0
    @796  VT817    1.0
    @797  VT818    1.0
    @798  VT819    1.0
    @799  VT820    1.0
    @800  VT821    1.0
    @801  VT822    1.0
    @802  VT823    1.0
    @803  VT824    1.0
    @804  VT825    1.0
    @805  VT826    1.0
    @806  VT827    1.0
    @807  VT828    1.0
    @808  VT829    1.0
    @809  VT830    1.0
    @810  VT1201   1.0
    @811  VT1401   1.0
    @812  VT1501   1.0
    @813  V200B    2.0
    @815  V200C    2.0
    @817  V200D    2.0
    @819  V200E    2.0
    @821  V200G    2.0
    @823  V200H    2.0
    @825  V200I    2.0
    @827  V200J    2.0
    @829  V201B    2.0
    @831  V201C    2.0
    @833  V201D    2.0
    @835  V201E    2.0
    @837  V201G    2.0
    @839  V201H    2.0
    @841  V201I    2.0
    @843  V201J    2.0
    @845  V230SRC  2.0
    @847  V230     1.0
    @848  V231A1   1.0
    @849  V231C    1.0
    @850  V231F    1.0
    @851  V231G    1.0
    @852  V231J    1.0
    @853  V231L    1.0
    @854  V231O    1.0
    @855  V231P    1.0
    @856  V231Q    1.0
    @857  V231R    1.0
    @858  V231S    1.0
    @859  V231T    1.0
    @860  V231U    1.0
    @861  V231XA   1.0
    @862  V231XB   1.0
    @863  V231XC   1.0
    @864  V231XD   1.0
    @865  V242     1.0
    @866  V246     1.0
    @867  V247A    1.0
    @868  V247B    1.0
    @869  V247C    1.0
    @870  V247D    1.0
    @871  V247E    1.0
    @872  V247F    1.0
    @873  V247G    1.0
    @874  V263     1.0
    @875  V264SRC  2.0
    @877  V264     1.0
    @878  V264A1   1.0
    @879  V264C    1.0
    @880  V264F    1.0
    @881  V264G    1.0
    @882  V264J    1.0
    @883  V264L    1.0
    @884  V264O    1.0
    @885  V264P    1.0
    @886  V264Q    1.0
    @887  V264R    1.0
    @888  V264S    1.0
    @889  V264T    1.0
    @890  V264U    1.0
    @891  V264XE   1.0
    @892  V264XF   1.0
    @893  V264XG   1.0
    @894  V264XH   1.0
    @895  V264XI   1.0
    @896  V264XJ   1.0
    @897  V265B    1.0
    @898  V265C    1.0
    @899  V265D    1.0
    @900  V265E    1.0
    @901  V265H    1.0
    @902  V266A    1.0
    @903  V266B    1.0
    @904  V266D    1.0
    @905  V266E    1.0
    @906  V266X    1.0
    @907  V267A    1.0
    @908  V267B    1.0
    @909  V267C    1.0
    @910  V267D    1.0
    @911  V271     1.0
    @912  V271A    1.0
    @913  V272A    1.0
    @914  V272B    1.0
    @915  V272C    1.0
    @916  V273A    1.0
    @917  V273B    1.0
    @918  V280A    1.0
    @919  V280X    1.0
    @920  LV200    1.0
    @921  LV203A   1.0
    @922  LV203B   1.0
    @923  LV203C   1.0
    @924  LV203D   1.0
    @925  LV203E   1.0
    @926  LV203F   1.0
    @927  LV203G   1.0
    @928  LV206B   1.0
    @929  LV221    1.0
    @930  LV222A   1.0
    @931  LV224    1.0
    @932  LV225    1.0
    @933  LV226    1.0
    @934  SF1008A  1.0
    @935  SF1013B  1.0
    @936  V301A    2.0
    @938  V303A    1.0
    @939  V303B    1.0
    @940  V303D    1.0
    @941  V303E    1.0
    @942  V303G    1.0
    @943  V303H    1.0
    @944  V303I    1.0
    @945  V303J    1.0
    @946  V303K    1.0
    @947  V303L    1.0
    @948  V303O    1.0
    @949  V303Q    1.0
    @950  V303R    1.0
    @951  V303S    1.0
    @952  V303X    1.0
    @953  V306     1.0
    @954  V307A    1.0
    @955  V307B    1.0
    @956  V307C    1.0
    @957  V308     1.0
    @958  V308A    1.0
    @959  V324     1.0
    @960  V325A    1.0
    @961  V325D    1.0
    @962  V325F    1.0
    @963  V325G    1.0
    @964  V325H    1.0
    @965  V325I    1.0
    @966  V327A    1.0
    @967  V327X    1.0
    @968  V330SRC  2.0
    @970  V330     1.0
    @971  V331A1   1.0
    @972  V331C    1.0
    @973  V331F    1.0
    @974  V331G    1.0
    @975  V331J    1.0
    @976  V331L    1.0
    @977  V331O    1.0
    @978  V331P    1.0
    @979  V331Q    1.0
    @980  V331R    1.0
    @981  V331S    1.0
    @982  V331T    1.0
    @983  V331U    1.0
    @984  V333B    1.0
    @985  V333C    1.0
    @986  V333D    1.0
    @987  V333E    1.0
    @988  V334A    1.0
    @989  V334B    1.0
    @990  V334C    1.0
    @991  V334D    1.0
    @992  V335A    1.0
    @993  V335B    1.0
    @994  V335C    1.0
    @995  V335E    1.0
    @996  V335F    1.0
    @997  V336A    1.0
    @998  V336B    1.0
    @999  V336C    1.0
    @1000 V336D    1.0
    @1001 V336F    1.0
    @1002 V336G    1.0
    @1003 V342     1.0
    @1004 V343A    1.0
    @1005 V343B    1.0
    @1006 V343C    1.0
    @1007 V343D    1.0
    @1008 V343E    1.0
    @1009 V343F    1.0
    @1010 V344     1.0
    @1011 LV300    1.0
    @1012 LV302A   1.0
    @1013 LV302B   1.0
    @1014 LV303A   1.0
    @1015 LV303B   1.0
    @1016 LV303D   1.0
    @1017 LV303E   1.0
    @1018 LV303G   1.0
    @1019 LV303H   1.0
    @1020 LV303I   1.0
    @1021 LV303J   1.0
    @1022 LV303K   1.0
    @1023 LV303L   1.0
    @1024 LV303O   1.0
    @1025 LV306A   1.0
    @1026 LV306B   1.0
    @1027 LV320A   1.0
    @1028 LV320C   1.0
    @1029 LV320D   1.0
    @1030 LV320E   1.0
    @1031 LV320F   1.0
    @1032 SF1308B  1.0
    @1033 SF1308D  1.0
    @1034 V401A    2.0
    @1036 V406     1.0
    @1037 V407A    1.0
    @1038 V407B    1.0
    @1039 V407C    1.0
    @1040 V407D    1.0
    @1041 V407E    1.0
    @1042 V407F    1.0
    @1043 V407G    1.0
    @1044 V407H    1.0
    @1045 V409B    1.0
    @1046 V409C    1.0
    @1047 V409E    1.0
    @1048 V409F    1.0
    @1049 V409G    1.0
    @1050 V409H    1.0
    @1051 V409I    1.0
    @1052 V409J    1.0
    @1053 V409K    1.0
    @1054 V410     1.0
    @1055 V410A    1.0
    @1056 V411A    1.0
    @1057 V411B    1.0
    @1058 V424     1.0
    @1059 V430SRC  2.0
    @1061 V430     1.0
    @1062 V431A1   1.0
    @1063 V431C    1.0
    @1064 V431F    1.0
    @1065 V431G    1.0
    @1066 V431J    1.0
    @1067 V431L    1.0
    @1068 V431O    1.0
    @1069 V431P    1.0
    @1070 V431Q    1.0
    @1071 V431R    1.0
    @1072 V431S    1.0
    @1073 V431T    1.0
    @1074 V431U    1.0
    @1075 V432A    1.0
    @1076 V432B    1.0
    @1077 V432C    1.0
    @1078 V432D    1.0
    @1079 V432E    1.0
    @1080 V432F    1.0
    @1081 V433B    1.0
    @1082 V433C    1.0
    @1083 V433D    1.0
    @1084 V433F    1.0
    @1085 V433G    1.0
    @1086 V433H    1.0
    @1087 V433I    1.0
    @1088 V434A    1.0
    @1089 V434B    1.0
    @1090 V434X    1.0
    @1091 V435A    1.0
    @1092 V435B    1.0
    @1093 V460     1.0
    @1094 V461A    1.0
    @1095 V461B    1.0
    @1096 V461C    1.0
    @1097 V461D    1.0
    @1098 V462A    1.0
    @1099 V462B    1.0
    @1100 V462C    1.0
    @1101 V462D    1.0
    @1102 V462E    1.0
    @1103 V501     3.0
    @1106 V501A    3.0
    @1109 V502     1.0
    @1110 V503     1.0
    @1111 V506C    1.0
    @1112 V507A    1.0
    @1113 V507B    1.0
    @1114 V507C    1.0
    @1115 V507D    1.0
    @1116 V507E    1.0
    @1117 V507F    1.0
    @1118 V507G    1.0
    @1119 V507H    1.0
    @1120 V507I    1.0
    @1121 V507J    1.0
    @1122 V507K    1.0
    @1123 V507L    1.0
    @1124 V507M    1.0
    @1125 V530SRC  2.0
    @1127 V530     1.0
    @1128 V531A1   1.0
    @1129 V531C    1.0
    @1130 V531F    1.0
    @1131 V531G    1.0
    @1132 V531J    1.0
    @1133 V531L    1.0
    @1134 V531O    1.0
    @1135 V531P    1.0
    @1136 V531Q    1.0
    @1137 V531R    1.0
    @1138 V531S    1.0
    @1139 V531T    1.0
    @1140 V531U    1.0
    @1141 V531XD   1.0
    @1142 V531XE   1.0
    @1143 V531XF   1.0
    @1144 V531XG   1.0
    @1145 V531XH   1.0
    @1146 V531XI   1.0
    @1147 V531XJ   1.0
    @1148 V531XK   1.0
    @1149 V531XL   1.0
    @1150 V531XM   1.0
    @1151 V531XN   1.0
    @1152 V531XO   1.0
    @1153 V533B    1.0
    @1154 V533C    1.0
    @1155 V533D    1.0
    @1156 V533E    1.0
    @1157 V533F    1.0
    @1158 V533G    1.0
    @1159 V533H    1.0
    @1160 V533I    1.0
    @1161 V533J    1.0
    @1162 V534A    1.0
    @1163 V534C    1.0
    @1164 V534D    1.0
    @1165 V534E    1.0
    @1166 V534G    1.0
    @1167 V534I    1.0
    @1168 V534M    1.0
    @1169 V534N    1.0
    @1170 V534O    1.0
    @1171 V536A    1.0
    @1172 V536B    1.0
    @1173 V536C    1.0
    @1174 V536D    1.0
    @1175 V536E    1.0
    @1176 V536F    1.0
    @1177 V537A    1.0
    @1178 V537B    1.0
    @1179 V537C    1.0
    @1180 V537D    1.0
    @1181 V537E    1.0
    @1182 V545     1.0
    @1183 V546A    2.0
    @1185 V546B    2.0
    @1187 V554A    1.0
    @1188 V554B    1.0
    @1189 V554C    1.0
    @1190 V554D    1.0
    @1191 V554E    1.0
    @1192 V554F    1.0
    @1193 V554G    1.0
    @1194 V554H    1.0
    @1195 V554I    1.0
    @1196 V554J    1.0
    @1197 V557C    1.0
    @1198 V557E    1.0
    @1199 V557F    1.0
    @1200 V557G    1.0
    @1201 V557H    1.0
    @1202 V557I    1.0
    @1203 V557J    1.0
    @1204 V557K    1.0
    @1205 V557L    1.0
    @1206 V557M    1.0
    @1207 V558     1.0
    @1208 V559     1.0
    @1209 V567     1.0
    @1210 V568     1.0
    @1211 V569     1.0
    @1212 V570     1.0
    @1213 V600A    1.0
    @1214 V601A    2.0
    @1216 V624     1.0
    @1217 V625A    1.0
    @1218 V625B    1.0
    @1219 V625C    1.0
    @1220 V625D    1.0
    @1221 V625E    1.0
    @1222 V625F    1.0
    @1223 V625G    1.0
    @1224 V626A    1.0
    @1225 V626B    1.0
    @1226 V626C    1.0
    @1227 V626D    1.0
    @1228 V626E    1.0
    @1229 V626F    1.0
    @1230 V626G    1.0
    @1231 V627A    1.0
    @1232 V627X    1.0
    @1233 V628     1.0
    @1234 V630SRC  2.0
    @1236 V630     1.0
    @1237 V631A1   1.0
    @1238 V631C    1.0
    @1239 V631F    1.0
    @1240 V631G    1.0
    @1241 V631J    1.0
    @1242 V631L    1.0
    @1243 V631O    1.0
    @1244 V631P    1.0
    @1245 V631Q    1.0
    @1246 V631R    1.0
    @1247 V631S    1.0
    @1248 V631T    1.0
    @1249 V631U    1.0
    @1250 V640A    1.0
    @1251 V640B    1.0
    @1252 V640C    1.0
    @1253 V640D    1.0
    @1254 V642     1.0
    @1255 V642A    1.0
    @1256 V650     1.0
    @1257 V805A    1.0
    @1258 V805B    1.0
    @1259 V805X    1.0
    @1260 V810A    1.0
    @1261 V810B    1.0
    @1262 V810C    1.0
    @1263 V810X    1.0
    @1264 V810Y    1.0
    @1265 V813     1.0
    @1266 V815     2.0
    @1268 V820A    1.0
    @1269 V820B    1.0
    @1270 V820C    1.0
    @1271 V820D    1.0
    @1272 V820E    1.0
    @1273 V820X    1.0
    @1274 V820Y    1.0
    @1275 V821     1.0
    @1276 V821E    1.0
    @1277 V834A    1.0
    @1278 V834B    1.0
    @1279 V834C    1.0
    @1280 V834D    1.0
    @1281 V834E    1.0
    @1282 V834Y    1.0
    @1283 V835A    1.0
    @1284 V835B    1.0
    @1285 V835C    1.0
    @1286 V835D    1.0
    @1287 V835Y    1.0
    @1288 V836     1.0
    @1289 V837     1.0
    @1290 V838A    1.0
    @1291 V838B    1.0
    @1292 V838C    1.0
    @1293 V838D    1.0
    @1294 V838Y    1.0
    @1295 V839     1.0
    @1296 V840A    1.0
    @1297 V840A1   1.0
    @1298 V840A2   1.0
    @1299 V840A3   1.0
    @1300 V840D    1.0
    @1301 V840E    1.0
    @1302 V840E1   1.0
    @1303 V840E2   1.0
    @1304 V840F    1.0
    @1305 V840F1   1.0
    @1306 V840F2   1.0
    @1307 V840F3   1.0
    @1308 V840G    1.0
    @1309 V840G1   1.0
    @1310 V840I    1.0
    @1311 V840J    1.0
    @1312 V840K    1.0
    @1313 V840L    1.0
    @1314 V840M    1.0
    @1315 V840N    1.0
    @1316 V840O    1.0
    @1317 V840P    1.0
    @1318 V840Y    1.0
    @1319 V841A1   1.0
    @1320 V841A2   1.0
    @1321 V841B    1.0
    @1322 V841C    1.0
    @1323 V841D    1.0
    @1324 V841E    1.0
    @1325 V841F    1.0
    @1326 V841G    1.0
    @1327 V842A1   1.0
    @1328 V842A2   1.0
    @1329 V842B    1.0
    @1330 V842C    1.0
    @1331 V842G    1.0
    @1332 V843A    1.0
    @1333 V843B    1.0
    @1334 V843C    1.0
    @1335 V843E    1.0
    @1336 V843F    1.0
    @1337 V843G    1.0
    @1338 V843H    1.0
    @1339 V843I    1.0
    @1340 V843J    1.0
    @1341 V843K    1.0
    @1342 V843L    1.0
    @1343 V843M    1.0
    @1344 V843N    1.0
    @1345 V844A    1.0
    @1346 V844B    1.0
    @1347 V844C    1.0
    @1348 V844F    1.0
    @1349 V844G    1.0
    @1350 V844H    1.0
    @1351 V844I    1.0
    @1352 V844J    1.0
    @1353 V844K    1.0
    @1354 V844N    1.0
    @1355 V845A    1.0
    @1356 V845B    1.0
    @1357 V845C    1.0
    @1358 V845D    1.0
    @1359 V845G    1.0
    @1360 V845H    1.0
    @1361 V845I    1.0
    @1362 V845J    1.0
    @1363 V845K    1.0
    @1364 V846A    1.0
    @1365 V846B    1.0
    @1366 V846C    1.0
    @1367 V846J    1.0
    @1368 V846K    1.0
    @1369 V847A    1.0
    @1370 V847B    1.0
    @1371 V847C    1.0
    @1372 V847D    1.0
    @1373 V847E    1.0
    @1374 V848B    1.0
    @1375 V849A    1.0
    @1376 V849B    1.0
    @1377 V849B1   1.0
    @1378 V850A    1.0
    @1379 V850B    1.0
    @1380 V850B1   1.0
    @1381 V851     1.0
    @1382 V851A    1.0
    @1383 V851B    1.0
    @1384 V852A    1.0
    @1385 V852B    1.0
    @1386 V852C    1.0
    @1387 V852D    1.0
    @1388 V852E    1.0
    @1389 V855A    1.0
    @1390 V855B    1.0
    @1391 V855C    1.0
    @1392 V855D    1.0
    @1393 V856A    1.0
    @1394 V856B    1.0
    @1395 V857B    1.0
    @1396 V857C    1.0
    @1397 V857D    1.0
    @1398 V859A    1.0
    @1399 V859B    1.0
    @1400 V859C    1.0
    @1401 V859D    1.0
    @1402 V859E    1.0
    @1403 V859F    1.0
    @1404 V859G    1.0
    @1405 V859GG   1.0
    @1406 V859H    1.0
    @1407 V859I    1.0
    @1408 V860A    1.0
    @1409 V860B    1.0
    @1410 V860C    1.0
    @1411 V860D    1.0
    @1412 V860F    1.0
    @1413 V861A    1.0
    @1414 V861B    1.0
    @1415 V861C    1.0
    @1416 V861D    1.0
    @1417 V861E    1.0
    @1418 V863     1.0
    @1419 V864A    1.0
    @1420 V864B    1.0
    @1421 V864C    1.0
    @1422 V864D    1.0
    @1423 V864F    1.0
    @1424 V865A    1.0
    @1425 V865B    1.0
    @1426 V865C    1.0
    @1427 V865D    1.0
    @1428 V865F    1.0
    @1429 V866A1   1.0
    @1430 V866C    1.0
    @1431 V866F    1.0
    @1432 V866G    1.0
    @1433 V866J    1.0
    @1434 V866L    1.0
    @1435 V866O    1.0
    @1436 V866P    1.0
    @1437 V866Q    1.0
    @1438 V866R    1.0
    @1439 V866S    1.0
    @1440 V866T    1.0
    @1441 V866U    1.0
    @1442 V870A    1.0
    @1443 V870B    1.0
    @1444 V870C    1.0
    @1445 V870D    1.0
    @1446 V870E    1.0
    @1447 V870F    1.0
    @1448 V870X    1.0
    @1449 V871     1.0
    @1450 V872     1.0
    @1451 V873A    1.0
    @1452 V873B    1.0
    @1453 V874A    1.0
    @1454 V874B    1.0
    @1455 V874C    1.0
    @1456 V874X    1.0
    @1457 V875     1.0
    @1458 V876A    1.0
    @1459 V876B    1.0
    @1460 V877     1.0
    @1461 V902A    1.0
    @1462 V902B    1.0
    @1463 V903_02  1.0
    @1464 V903_05  1.0
    @1465 V903_06  1.0
    @1466 V903_08  1.0
    @1467 V903_10  1.0
    @1468 V903_13  1.0
    @1469 V903_14  1.0
    @1470 V903_15  1.0
    @1471 V903_16  1.0
    @1472 V903_17  1.0
    @1473 V903_18  1.0
    @1474 V903_19  1.0
    @1475 V903_20  1.0
    @1476 V903_21  1.0
    @1477 V903_22  1.0
    @1478 V904_03  1.0
    @1479 V904_04  1.0
    @1480 V904_05  1.0
    @1481 V904_06  1.0
    @1482 V904_07  1.0
    @1483 V904_08  1.0
    @1484 V905_03  1.0
    @1485 V905_08  1.0
    @1486 V905_14  1.0
    @1487 V905_18  1.0
    @1488 V905_19  1.0
    @1489 V905_20  1.0
    @1490 V905_21  1.0
    @1491 V905_22  1.0
    @1492 V905_23  1.0
    @1493 V906_03  1.0
    @1494 V906_07  1.0
    @1495 V906_08  1.0
    @1496 V906_11  1.0
    @1497 V906_12  1.0
    @1498 V907_03  1.0
    @1499 V907_04  1.0
    @1500 V907_05  1.0
    @1501 V907_06  1.0
    @1502 V907_07  1.0
    @1503 V907_08  1.0
    @1504 V908_02  1.0
    @1505 V908_03  1.0
    @1506 V908_04  1.0
    @1507 V908_05  1.0
    @1508 V909_02  1.0
    @1509 V909_04  1.0
    @1510 V909_05  1.0
    @1511 V909_06  1.0
    @1512 V910_03  1.0
    @1513 V910_04  1.0
    @1514 V910_06  1.0
    @1515 V910_07  1.0
    @1516 V910_08  1.0
    @1517 V911_02  1.0
    @1518 V913_01  1.0
    @1519 V913_02  1.0
    @1520 V913_04  1.0
    @1521 V913_05  1.0
    @1522 V913_06  1.0
    @1523 V913_07  1.0
    @1524 V913_10  1.0
    @1525 V913_11  1.0
    @1526 V913_12  1.0
    @1527 V913_13  1.0
    @1528 V914_05  1.0
    @1529 V914_06  1.0
    @1530 V914_07  1.0
    @1531 V914_08  1.0
    @1532 V915_01  1.0
    @1533 V915_02  1.0
    @1534 V915_03  1.0
    @1535 V915_04  1.0
    @1536 V916_01  1.0
    @1537 V916_03  1.0
    @1538 V916_06  1.0
    @1539 V916_07  1.0
    @1540 V916_08  1.0
    @1541 V916_09  1.0
    @1542 V917_04  1.0
    @1543 V917_05  1.0
    @1544 V917_06  1.0
    @1545 V918_01  1.0
    @1546 V918_02  1.0
    @1547 V918_04  1.0
    @1548 V918_05  1.0
    @1549 V918_06  1.0
    @1550 V919_03  1.0
    @1551 V919_04  1.0
    @1552 V919_05  1.0
    @1553 V919_06  1.0
    @1554 V920_02  1.0
    @1555 V921_01  1.0
    @1556 V921_02  1.0
    @1557 V921_03  1.0
    @1558 V922_03  1.0
    @1559 V922_04  1.0
    @1560 V922_05  1.0
    @1561 V922_11  1.0
    @1562 V922_12  1.0
    @1563 V922_13  1.0
    @1564 V922_14  1.0
    @1565 V922_15  1.0
    @1566 V922_16  1.0
    @1567 V922_17  1.0
    @1568 V922_18  1.0
    @1569 V923_01  1.0
    @1570 V923_02  1.0
    @1571 V923_03  1.0
    @1572 V923_04  1.0
    @1573 V923_05  1.0
    @1574 V923_06  1.0
    @1575 V923_07  1.0
    @1576 V923_08  1.0
    @1577 V923_09  1.0
    @1578 V923_11  1.0
    @1579 V923_12  1.0
    @1580 V924_01  1.0
    @1581 V924_02  1.0
    @1582 V924_03  1.0
    @1583 V924_05  1.0
    @1584 V926     1.0
    @1585 V927     1.0
    @1586 V928A    1.0
    @1587 V928B1   1.0
    @1588 V928B2   1.0
    @1589 V928C    1.0
    @1590 V928D    1.0
    @1591 V936     1.0
    @1592 V946_06  1.0
    @1593 V946_07  1.0
    @1594 V946_09  1.0
    @1595 V946_12  1.0
    @1596 V946_13  1.0
    @1597 V946_16  1.0
    @1598 V946_17  1.0
    @1599 V946_18  1.0
    @1600 V946_19  1.0
    @1601 V950A    1.0
    @1602 V950B    1.0
    @1603 V951     1.0
    @1604 V952     1.0
    @1605 V953A    1.0
    @1606 V953B1   1.0
    @1607 V953B2   1.0
    @1608 V953C    1.0
    @1609 V953D    1.0
    @1610 V955_01  1.0
    @1611 V955_02  1.0
    @1612 V955_03  1.0
    @1613 V955_04  1.0
    @1614 V955_05  1.0
    @1615 V955_06  1.0
    @1616 V955_07  1.0
    @1617 V955_08  1.0
    @1618 V955_09  1.0
    @1619 V955_10  1.0
    @1620 V955_11  1.0
    @1621 V956_01  1.0
    @1622 V956_02  1.0
    @1623 V956_03  1.0
    @1624 V956_04  1.0
    @1625 V956_05  1.0
    @1626 V956_06  1.0
    @1627 V957_01  1.0
    @1628 V957_02  1.0
    @1629 V957_03  1.0
    @1630 V957_04  1.0
    @1631 V957_05  1.0
    @1632 V957_06  1.0
    @1633 V957_07  1.0
    @1634 V957_08  1.0
    @1635 V957_09  1.0
    @1636 V957_10  1.0
    @1637 V957_11  1.0
    @1638 V957_12  1.0
    @1639 V958_01  1.0
    @1640 V958_02  1.0
    @1641 V958_03  1.0
    @1642 V958_04  1.0
    @1643 V958_05  1.0
    @1644 V958_06  1.0
    @1645 V958_07  1.0
    @1646 V958_08  1.0
    @1647 V958_09  1.0
    @1648 V958_10  1.0
    @1649 V958_11  1.0
    @1650 V960A    1.0
    @1651 V960B    1.0
    @1652 V961     1.0
    @1653 V962     1.0
    @1654 V963A    1.0
    @1655 V963B1   1.0
    @1656 V963B2   1.0
    @1657 V963C    1.0
    @1658 V963D    1.0
    @1659 V1001    2.0
    @1661 V1002    1.0
    @1662 V1003    1.0
    @1663 V1004    1.0
    @1664 V1005A   1.0
    @1665 V1005B   1.0
    @1666 V1005C   1.0
    @1667 V1005D   1.0
    @1668 V1005E   1.0
    @1669 V1005F   1.0
    @1670 V1005X   1.0
    @1671 V1006    1.0
    @1672 V1007A   1.0
    @1673 V1007X   1.0
    @1674 V1100A   1.0
    @1675 V1106    1.0
    @1676 V1107A   1.0
    @1677 V1107B   1.0
    @1678 V1107C   1.0
    @1679 V1109A   1.0
    @1680 V1109B   1.0
    @1681 V1109C   1.0
    @1682 V1109D   1.0
    @1683 V1115    1.0
    @1684 V1138SRC 2.0
    @1686 V1138    1.0
    @1687 V1139A1  1.0
    @1688 V1139C   1.0
    @1689 V1139F   1.0
    @1690 V1139G   1.0
    @1691 V1139J   1.0
    @1692 V1139L   1.0
    @1693 V1139O   1.0
    @1694 V1139P   1.0
    @1695 V1139Q   1.0
    @1696 V1139R   1.0
    @1697 V1139S   1.0
    @1698 V1139T   1.0
    @1699 V1139U   1.0
    @1700 V1140    2.0
    @1702 V1141    1.0
    @1703 V1142    1.0
    @1704 V1143    1.0
    @1705 V1144    1.0
    @1706 SF1901   1.0
    @1707 SF1902   1.0
    @1708 SF1903   1.0
    @1709 SF1904   1.0
    @1710 V1206    2.0
    @1712 V1208A1  1.0
    @1713 V1208A2  1.0
    @1714 V1208F   1.0
    @1715 V1208G1  1.0
    @1716 V1208H   1.0
    @1717 V1208L   1.0
    @1718 V1208M   1.0
    @1719 V1208S   1.0
    @1720 V1215A   1.0
    @1721 V1215B   1.0
    @1722 V1215X   1.0
    @1723 V1215Y   1.0
    @1724 V1260    1.0
    @1725 V1273A   1.0
    @1726 V1273B   1.0
    @1727 V1273C   1.0
    @1728 V1273D   1.0
    @1729 V1273E   1.0
    @1730 V1273F   1.0
    @1731 V1273G   1.0
    @1732 V1273H   1.0
    @1733 V1273I   1.0
    @1734 V1273J   1.0
    @1735 V1273K   1.0
    @1736 V1273L   1.0
    @1737 V1273M   1.0
    @1738 V1273Y   1.0
    @1739 V1274    1.0
    @1740 V1275    1.0
    @1741 V1276SRC 2.0
    @1743 V1276    1.0
    @1744 V1277A1  1.0
    @1745 V1277C   1.0
    @1746 V1277F   1.0
    @1747 V1277G   1.0
    @1748 V1277J   1.0
    @1749 V1277L   1.0
    @1750 V1277O   1.0
    @1751 V1277P   1.0
    @1752 V1277Q   1.0
    @1753 V1277R   1.0
    @1754 V1277S   1.0
    @1755 V1277T   1.0
    @1756 V1277U   1.0
    @1757 V1278A   1.0
    @1758 V1278B   1.0
    @1759 V1278C   1.0
    @1760 V1278D   1.0
    @1761 V1278E   1.0
    @1762 V1278F   1.0
    @1763 V1278G   1.0
    @1764 V1278H   1.0
    @1765 V1278I   1.0
    @1766 V1278J   1.0
    @1767 V1278K   1.0
    @1768 V1278L   1.0
    @1769 V1278M   1.0
    @1770 V1278Y   1.0
    @1771 V1279    1.0
    @1772 V1280SRC 2.0
    @1774 V1280    1.0
    @1775 V1281A1  1.0
    @1776 V1281C   1.0
    @1777 V1281F   1.0
    @1778 V1281G   1.0
    @1779 V1281J   1.0
    @1780 V1281L   1.0
    @1781 V1281O   1.0
    @1782 V1281P   1.0
    @1783 V1281Q   1.0
    @1784 V1281R   1.0
    @1785 V1281S   1.0
    @1786 V1281T   1.0
    @1787 V1281U   1.0
    @1788 V1307A   2.0
    @1790 V1327D   1.0
    @1791 V1327E   1.0
    @1792 V1327G   1.0
    @1793 V1327H   1.0
    @1794 V1327I   1.0
    @1795 V1327J   1.0
    @1796 V1328SRC 2.0
    @1798 V1328    1.0
    @1799 V1329A1  1.0
    @1800 V1329C   1.0
    @1801 V1329F   1.0
    @1802 V1329G   1.0
    @1803 V1329J   1.0
    @1804 V1329L   1.0
    @1805 V1329O   1.0
    @1806 V1329P   1.0
    @1807 V1329Q   1.0
    @1808 V1329R   1.0
    @1809 V1329S   1.0
    @1810 V1329T   1.0
    @1811 V1329U   1.0
    @1812 V1331    1.0
    @1813 V1332A   1.0
    @1814 V1332B   1.0
    @1815 V1332X   1.0
    @1816 V1337    1.0
    @1817 V1355    1.0
    @1818 V1356    1.0
    @1819 V1357    1.0
    @1820 V1358    1.0
    @1821 V1400A   1.0
    @1822 V1415A   1.0
    @1823 V1415B   1.0
    @1824 V1415C   1.0
    @1825 V1415D   1.0
    @1826 V1415E   1.0
    @1827 V1415F   1.0
    @1828 V1415G   1.0
    @1829 V1415H   1.0
    @1830 V1415I   1.0
    @1831 V1415J   1.0
    @1832 V1415K   1.0
    @1833 V1415X   1.0
    @1834 V1421A   1.0
    @1835 V1421X   1.0
    @1836 V1451    1.0
    @1837 V1452A   1.0
    @1838 V1452B   1.0
    @1839 V1452C   1.0
    @1840 V1452D   1.0
    @1841 V1452E   1.0
    @1842 V1452F   1.0
    @1843 V1452G   1.0
    @1844 V1452H   1.0
    @1845 V1452I   1.0
    @1846 V1452J   1.0
    @1847 V1452K   1.0
    @1848 V1452X   1.0
    @1849 SF2102   1.0
    @1850 V1515    1.0
    @1851 V1516A   1.0
    @1852 V1516B   1.0
    @1853 V1516C   1.0
    @1854 V1516D   1.0
    @1855 V1516E   1.0
    @1856 V1516F   1.0
    @1857 V1516J   1.0
    @1858 V1518B   1.0
    @1859 V1518C   1.0
    @1860 V1518D   1.0
    @1861 V1518E   1.0
    @1862 V1518F   1.0
    @1863 V1521A   1.0
    @1864 V1521B   1.0
    @1865 V1560    1.0
    @1866 V1561    1.0
    @1867 V1601    1.0
    @1868 V1601A   1.0
    @1869 V1602A   1.0
    @1870 V1602X   1.0
    @1871 V1603    1.0
    @1872 V1603A   1.0
    @1873 V1604A   1.0
    @1874 V1604X   1.0
    @1875 V1605    1.0
    @1876 V1605A   1.0
    @1877 V1606A   1.0
    @1878 V1606X   1.0
    @1879 V1607SRC 1.0
    @1880 V1607A   1.0
    @1881 V1607B   1.0
    @1882 V1607C   1.0
    @1883 V1607D   1.0
    @1884 V1608A   1.0
    @1885 V1608B   1.0
    @1886 V1608C   1.0
    @1887 V1608D   1.0
    @1888 V1608E   1.0
    @1889 V1608F   1.0
    @1890 V1608G   1.0
    @1891 V1608H   1.0
    @1892 V1608I   1.0
    @1893 V1608J   1.0
    @1894 V1608K   1.0
    @1895 V1608L   1.0
    @1896 V1608M   1.0
    @1897 V1608N   1.0
    @1898 V1608O   1.0
    @1899 V1608P   1.0
    @1900 V1608Q   1.0
    @1901 V1609SRC 2.0
    @1903 V1609    1.0
    @1904 V1610A1  1.0
    @1905 V1610C   1.0
    @1906 V1610F   1.0
    @1907 V1610G   1.0
    @1908 V1610J   1.0
    @1909 V1610L   1.0
    @1910 V1610O   1.0
    @1911 V1610P   1.0
    @1912 V1610Q   1.0
    @1913 V1610R   1.0
    @1914 V1610S   1.0
    @1915 V1610T   1.0
    @1916 V1610U   1.0
    @1917 V1701    1.0
    @1918 V1702A   1.0
    @1919 V1702B   1.0
    @1920 V1702C   1.0
    @1921 V1702D   1.0
    @1922 V1702E   1.0
    @1923 V1702F   1.0
    @1924 V1703A   1.0
    @1925 V1703B   1.0
    @1926 V1703C   1.0
    @1927 V1703D   1.0
    @1928 V1703E   1.0
    @1929 V1704SRC 2.0
    @1931 V1704    1.0
    @1932 V1705A1  1.0
    @1933 V1705C   1.0
    @1934 V1705F   1.0
    @1935 V1705G   1.0
    @1936 V1705J   1.0
    @1937 V1705L   1.0
    @1938 V1705O   1.0
    @1939 V1705P   1.0
    @1940 V1705Q   1.0
    @1941 V1705R   1.0
    @1942 V1705S   1.0
    @1943 V1705T   1.0
    @1944 V1705U   1.0
    @1945 V2000A   2.0
    @1947 V2000B   2.0
    @1949 V2000C   2.0
    @1951 V2000D   2.0
    @1953 V2000E   2.0
    @1955 V2000F   2.0
    @1957 V2000G   2.0
    @1959 V2001A   2.0
    @1961 V2001B   2.0
    @1963 V2001C   2.0
    @1965 V2001D   2.0
    @1967 V2002A   2.0
    @1969 V2002B   2.0
    @1971 V2002C   2.0
    @1973 V2002D   2.0
    @1975 V2003A   2.0
    @1977 V2003B   2.0
    @1979 V2003C   2.0
    @1981 V2003D   2.0
    @1983 V2004A   2.0
    @1985 V2004B   2.0
    @1987 V2004C   2.0
    @1989 V2004D   2.0
    @1991 V2005A   2.0
    @1993 V2005B   2.0
    @1995 V2005C   2.0
    @1997 V2005D   2.0
    @1999 V2006A   2.0
    @2001 V2006B   2.0
    @2003 V2006C   2.0
    @2005 V2006D   2.0
    @2007 V2007A   2.0
    @2009 V2007B   2.0
    @2011 V2007C   2.0
    @2013 V2007D   2.0
    @2015 V2008A   2.0
    @2017 V2008B   2.0
    @2019 V2008C   2.0
    @2021 V2008D   2.0
    @2023 V2009A   2.0
    @2025 V2009B   2.0
    @2027 V2009C   2.0
    @2029 V2009D   2.0
    @2031 V2010A   2.0
    @2033 V2010B   2.0
    @2035 V2010C   2.0
    @2037 V2010D   2.0
    @2039 V2011A   2.0
    @2041 V2011B   2.0
    @2043 V2011C   2.0
    @2045 V2011D   2.0
    @2047 V2012A   2.0
    @2049 V2012B   2.0
    @2051 V2012C   2.0
    @2053 V2012D   2.0
    ;
    if V003     =      9 then V003 = .;
    if V077     =    999 then V077 = .;
    if V078     =    999 then V078 = .;
    if V101A    =      9 then V101A = .;
    if V102D_01 =   9999 then V102D_01 = .;
    if V102D_02 =   9999 then V102D_02 = .;
    if V102D_03 =   9999 then V102D_03 = .;
    if V102D_04 =   9999 then V102D_04 = .;
    if V102D_05 =   9999 then V102D_05 = .;
    if V102D_06 =   9999 then V102D_06 = .;
    if V102D_07 =   9999 then V102D_07 = .;
    if V102D_08 =   9999 then V102D_08 = .;
    if V102D_09 =   9999 then V102D_09 = .;
    if V102D_10 =   9999 then V102D_10 = .;
    if V102D_11 =   9999 then V102D_11 = .;
    if V102D_12 =   9999 then V102D_12 = .;
    if V102D_13 =   9999 then V102D_13 = .;
    if V102D_14 =   9999 then V102D_14 = .;
    if V102D_15 =   9999 then V102D_15 = .;
    if V102D_16 =   9999 then V102D_16 = .;
    if V102D_17 =   9999 then V102D_17 = .;
    if V102D_18 =   9999 then V102D_18 = .;
    if V102D_19 =   9999 then V102D_19 = .;
    if V102D_20 =   9999 then V102D_20 = .;
    if V102D_21 =   9999 then V102D_21 = .;
    if V102D_22 =   9999 then V102D_22 = .;
    if V102D_23 =   9999 then V102D_23 = .;
    if V102D_24 =   9999 then V102D_24 = .;
    if V102D_25 =   9999 then V102D_25 = .;
    if V102D_26 =   9999 then V102D_26 = .;
    if V102D_27 =   9999 then V102D_27 = .;
    if V102E_01 =   9999 then V102E_01 = .;
    if V102E_02 =   9999 then V102E_02 = .;
    if V102E_03 =   9999 then V102E_03 = .;
    if V102E_04 =   9999 then V102E_04 = .;
    if V102E_05 =   9999 then V102E_05 = .;
    if V102E_06 =   9999 then V102E_06 = .;
    if V102E_07 =   9999 then V102E_07 = .;
    if V102E_08 =   9999 then V102E_08 = .;
    if V102E_09 =   9999 then V102E_09 = .;
    if V102E_10 =   9999 then V102E_10 = .;
    if V102E_11 =   9999 then V102E_11 = .;
    if V102E_12 =   9999 then V102E_12 = .;
    if V102E_13 =   9999 then V102E_13 = .;
    if V102E_14 =   9999 then V102E_14 = .;
    if V102E_15 =   9999 then V102E_15 = .;
    if V102E_16 =   9999 then V102E_16 = .;
    if V102E_17 =   9999 then V102E_17 = .;
    if V102E_18 =   9999 then V102E_18 = .;
    if V102E_19 =   9999 then V102E_19 = .;
    if V102E_20 =   9999 then V102E_20 = .;
    if V102E_21 =   9999 then V102E_21 = .;
    if V102E_22 =   9999 then V102E_22 = .;
    if V102E_23 =   9999 then V102E_23 = .;
    if V102E_24 =   9999 then V102E_24 = .;
    if V102E_25 =   9999 then V102E_25 = .;
    if V102E_26 =   9999 then V102E_26 = .;
    if V102E_27 =   9999 then V102E_27 = .;
    if V102DT   =   9999 then V102DT = .;
    if V102ET   =   9999 then V102ET = .;
    if V102T    =   9999 then V102T = .;
    if V105     =      9 then V105 = .;
    if V106     =      9 then V106 = .;
    if V106A    =      9 then V106A = .;
    if V106B    =      9 then V106B = .;
    if V106C    =      9 then V106C = .;
    if V106D    =      9 then V106D = .;
    if V106E    =      9 then V106E = .;
    if V106F    =      9 then V106F = .;
    if V106G    =      9 then V106G = .;
    if V106H    =      9 then V106H = .;
    if V106X    =      9 then V106X = .;
    if V106Y    =      9 then V106Y = .;
    if V106AA   =      9 then V106AA = .;
    if V106BB   =      9 then V106BB = .;
    if V107     =      9 then V107 = .;
    if V108     =      9 then V108 = .;
    if V109A    =      9 then V109A = .;
    if V109B    =      9 then V109B = .;
    if V109C    =      9 then V109C = .;
    if V109D    =      9 then V109D = .;
    if V109E    =      9 then V109E = .;
    if V109F    =      9 then V109F = .;
    if V109G    =      9 then V109G = .;
    if V109H    =      9 then V109H = .;
    if V109X    =      9 then V109X = .;
    if V109Y    =      9 then V109Y = .;
    if V109Z    =      9 then V109Z = .;
    if V110A    =      9 then V110A = .;
    if V110     =      9 then V110 = .;
    if V112     =      9 then V112 = .;
    if V115     =      9 then V115 = .;
    if V116G    =      9 then V116G = .;
    if V116H    =      9 then V116H = .;
    if V116I    =      9 then V116I = .;
    if V120A    =      9 then V120A = .;
    if V120     =      9 then V120 = .;
    if V121A    =      9 then V121A = .;
    if V121B    =      9 then V121B = .;
    if V121C    =      9 then V121C = .;
    if V123     =     99 then V123 = .;
    if V124     =      9 then V124 = .;
    if V126     =      9 then V126 = .;
    if V127A    =      9 then V127A = .;
    if V127B    =      9 then V127B = .;
    if V127     =      9 then V127 = .;
    if V128     =      9 then V128 = .;
    if V128A    =      9 then V128A = .;
    if V129     =      9 then V129 = .;
    if V130     =      9 then V130 = .;
    if V131     =      9 then V131 = .;
    if V132     =      9 then V132 = .;
    if V133     =      9 then V133 = .;
    if V134     =  99999 then V134 = .;
    if V135     =  99999 then V135 = .;
    if V142     =      9 then V142 = .;
    if V143     =   9999 then V143 = .;
    if V144     =      9 then V144 = .;
    if V144A    =      9 then V144A = .;
    if V144B    =      9 then V144B = .;
    if V144E    =      9 then V144E = .;
    if V144F    =      9 then V144F = .;
    if V144G    =      9 then V144G = .;
    if V144J    =      9 then V144J = .;
    if V144K    =      9 then V144K = .;
    if V144L    =      9 then V144L = .;
    if V144M    =      9 then V144M = .;
    if V144N    =      9 then V144N = .;
    if V144O    =      9 then V144O = .;
    if V144P    =      9 then V144P = .;
    if V144Q    =      9 then V144Q = .;
    if V144R    =      9 then V144R = .;
    if V144S    =      9 then V144S = .;
    if V144T    =      9 then V144T = .;
    if V144U    =      9 then V144U = .;
    if V145     =      9 then V145 = .;
    if V145A    =      9 then V145A = .;
    if V145B    =      9 then V145B = .;
    if V145C    =      9 then V145C = .;
    if V145D    =      9 then V145D = .;
    if V145X    =      9 then V145X = .;
    if V146A    =      9 then V146A = .;
    if V146B    =      9 then V146B = .;
    if V146C    =      9 then V146C = .;
    if V146D    =      9 then V146D = .;
    if V146E    =      9 then V146E = .;
    if V146F    =      9 then V146F = .;
    if V146G    =      9 then V146G = .;
    if V146H    =      9 then V146H = .;
    if V146I    =      9 then V146I = .;
    if V146X    =      9 then V146X = .;
    if V146Y    =      9 then V146Y = .;
    if V146Z    =      9 then V146Z = .;
    if V150     =      9 then V150 = .;
    if V152     =      9 then V152 = .;
    if V153     =      9 then V153 = .;
    if V153A    =     99 then V153A = .;
    if V165A    =      9 then V165A = .;
    if V165B    =      9 then V165B = .;
    if V165C    =      9 then V165C = .;
    if V165D    =      9 then V165D = .;
    if V165E    =      9 then V165E = .;
    if V165F    =      9 then V165F = .;
    if V165G    =      9 then V165G = .;
    if V165H    =      9 then V165H = .;
    if V165I    =      9 then V165I = .;
    if V165J    =      9 then V165J = .;
    if V165K    =      9 then V165K = .;
    if V166A    =      9 then V166A = .;
    if V166B    =      9 then V166B = .;
    if V166C    =      9 then V166C = .;
    if V166D    =      9 then V166D = .;
    if V166E    =      9 then V166E = .;
    if V166F    =      9 then V166F = .;
    if V166G    =      9 then V166G = .;
    if V166H    =      9 then V166H = .;
    if V166I    =      9 then V166I = .;
    if V166J    =      9 then V166J = .;
    if V166K    =      9 then V166K = .;
    if V166L    =      9 then V166L = .;
    if V166M    =      9 then V166M = .;
    if V166N    =      9 then V166N = .;
    if V166O    =      9 then V166O = .;
    if V166P    =      9 then V166P = .;
    if V166Q    =      9 then V166Q = .;
    if V166XA   =      9 then V166XA = .;
    if V166XB   =      9 then V166XB = .;
    if V166XC   =      9 then V166XC = .;
    if V166XD   =      9 then V166XD = .;
    if V166XE   =      9 then V166XE = .;
    if V166XF   =      9 then V166XF = .;
    if V166XG   =      9 then V166XG = .;
    if V166XH   =      9 then V166XH = .;
    if V166XI   =      9 then V166XI = .;
    if V166XJ   =      9 then V166XJ = .;
    if V166XK   =      9 then V166XK = .;
    if V166XL   =      9 then V166XL = .;
    if V166XM   =      9 then V166XM = .;
    if V166XN   =      9 then V166XN = .;
    if V166XO   =      9 then V166XO = .;
    if V166XP   =      9 then V166XP = .;
    if V166XQ   =      9 then V166XQ = .;
    if V166XR   =      9 then V166XR = .;
    if V166XS   =      9 then V166XS = .;
    if V167     =      9 then V167 = .;
    if V168A1   =      9 then V168A1 = .;
    if V168C    =      9 then V168C = .;
    if V168F    =      9 then V168F = .;
    if V168G    =      9 then V168G = .;
    if V168J    =      9 then V168J = .;
    if V168L    =      9 then V168L = .;
    if V168O    =      9 then V168O = .;
    if V168P    =      9 then V168P = .;
    if V168Q    =      9 then V168Q = .;
    if V168R    =      9 then V168R = .;
    if V168S    =      9 then V168S = .;
    if V168T    =      9 then V168T = .;
    if V168U    =      9 then V168U = .;
    if V173A    =      9 then V173A = .;
    if V173     =      9 then V173 = .;
    if V175B_1  =      9 then V175B_1 = .;
    if V175B_2  =      9 then V175B_2 = .;
    if V175B_3  =      9 then V175B_3 = .;
    if V175B_4  =      9 then V175B_4 = .;
    if V175B_5  =      9 then V175B_5 = .;
    if V175B_6  =      9 then V175B_6 = .;
    if V175B_7  =      9 then V175B_7 = .;
    if V175B_8  =      9 then V175B_8 = .;
    if V175C_1  =    999 then V175C_1 = .;
    if V175C_2  =    999 then V175C_2 = .;
    if V175C_3  =    999 then V175C_3 = .;
    if V175C_4  =    999 then V175C_4 = .;
    if V175C_5  =    999 then V175C_5 = .;
    if V175C_6  =    999 then V175C_6 = .;
    if V175C_7  =    999 then V175C_7 = .;
    if V175C_8  =    999 then V175C_8 = .;
    if V175D_1  =    999 then V175D_1 = .;
    if V175D_2  =    999 then V175D_2 = .;
    if V175D_3  =    999 then V175D_3 = .;
    if V175D_4  =    999 then V175D_4 = .;
    if V175D_5  =    999 then V175D_5 = .;
    if V175D_6  =    999 then V175D_6 = .;
    if V175D_7  =    999 then V175D_7 = .;
    if V175D_8  =    999 then V175D_8 = .;
    if V175E_1  =      9 then V175E_1 = .;
    if V175E_2  =      9 then V175E_2 = .;
    if V175E_3  =      9 then V175E_3 = .;
    if V175E_4  =      9 then V175E_4 = .;
    if V175E_5  =      9 then V175E_5 = .;
    if V175E_6  =      9 then V175E_6 = .;
    if V175E_7  =      9 then V175E_7 = .;
    if V175E_8  =      9 then V175E_8 = .;
    if V175F_1  =    999 then V175F_1 = .;
    if V175F_2  =    999 then V175F_2 = .;
    if V175F_3  =    999 then V175F_3 = .;
    if V175F_4  =    999 then V175F_4 = .;
    if V175F_5  =    999 then V175F_5 = .;
    if V175F_6  =    999 then V175F_6 = .;
    if V175F_7  =    999 then V175F_7 = .;
    if V175F_8  =    999 then V175F_8 = .;
    if V175G_1  =    999 then V175G_1 = .;
    if V175G_2  =    999 then V175G_2 = .;
    if V175G_3  =    999 then V175G_3 = .;
    if V175G_4  =    999 then V175G_4 = .;
    if V175G_5  =    999 then V175G_5 = .;
    if V175G_6  =    999 then V175G_6 = .;
    if V175G_7  =    999 then V175G_7 = .;
    if V175G_8  =    999 then V175G_8 = .;
    if V175H_1  =     99 then V175H_1 = .;
    if V175H_2  =     99 then V175H_2 = .;
    if V175H_3  =     99 then V175H_3 = .;
    if V175H_4  =     99 then V175H_4 = .;
    if V175H_5  =     99 then V175H_5 = .;
    if V175H_6  =     99 then V175H_6 = .;
    if V175H_7  =     99 then V175H_7 = .;
    if V175H_8  =     99 then V175H_8 = .;
    if V176A    =      9 then V176A = .;
    if V176B    =      9 then V176B = .;
    if V176C    =      9 then V176C = .;
    if V177A    =      9 then V177A = .;
    if V177B    =      9 then V177B = .;
    if V177C    =      9 then V177C = .;
    if V177D    =      9 then V177D = .;
    if V177E    =      9 then V177E = .;
    if V177F    =      9 then V177F = .;
    if V177G    =      9 then V177G = .;
    if V178B    =      9 then V178B = .;
    if V184A    =     99 then V184A = .;
    if V185     =      9 then V185 = .;
    if V186A    =     99 then V186A = .;
    if V187     =      9 then V187 = .;
    if V189     =      9 then V189 = .;
    if V190     =      9 then V190 = .;
    if V195C    =      9 then V195C = .;
    if V200B    =     99 then V200B = .;
    if V200C    =     99 then V200C = .;
    if V200D    =     99 then V200D = .;
    if V200E    =     99 then V200E = .;
    if V200G    =     99 then V200G = .;
    if V200H    =     99 then V200H = .;
    if V200I    =     99 then V200I = .;
    if V200J    =     99 then V200J = .;
    if V201B    =     99 then V201B = .;
    if V201C    =     99 then V201C = .;
    if V201D    =     99 then V201D = .;
    if V201E    =     99 then V201E = .;
    if V201G    =     99 then V201G = .;
    if V201H    =     99 then V201H = .;
    if V201I    =     99 then V201I = .;
    if V201J    =     99 then V201J = .;
    if V230     =      9 then V230 = .;
    if V231A1   =      9 then V231A1 = .;
    if V231C    =      9 then V231C = .;
    if V231F    =      9 then V231F = .;
    if V231G    =      9 then V231G = .;
    if V231J    =      9 then V231J = .;
    if V231L    =      9 then V231L = .;
    if V231O    =      9 then V231O = .;
    if V231P    =      9 then V231P = .;
    if V231Q    =      9 then V231Q = .;
    if V231R    =      9 then V231R = .;
    if V231S    =      9 then V231S = .;
    if V231T    =      9 then V231T = .;
    if V231U    =      9 then V231U = .;
    if V231XA   =      9 then V231XA = .;
    if V231XB   =      9 then V231XB = .;
    if V231XC   =      9 then V231XC = .;
    if V231XD   =      9 then V231XD = .;
    if V242     =      9 then V242 = .;
    if V246     =      9 then V246 = .;
    if V247A    =      9 then V247A = .;
    if V247B    =      9 then V247B = .;
    if V247C    =      9 then V247C = .;
    if V247D    =      9 then V247D = .;
    if V247E    =      9 then V247E = .;
    if V247F    =      9 then V247F = .;
    if V247G    =      9 then V247G = .;
    if V263     =      9 then V263 = .;
    if V264     =      9 then V264 = .;
    if V264A1   =      9 then V264A1 = .;
    if V264C    =      9 then V264C = .;
    if V264F    =      9 then V264F = .;
    if V264G    =      9 then V264G = .;
    if V264J    =      9 then V264J = .;
    if V264L    =      9 then V264L = .;
    if V264O    =      9 then V264O = .;
    if V264P    =      9 then V264P = .;
    if V264Q    =      9 then V264Q = .;
    if V264R    =      9 then V264R = .;
    if V264S    =      9 then V264S = .;
    if V264T    =      9 then V264T = .;
    if V264U    =      9 then V264U = .;
    if V264XE   =      9 then V264XE = .;
    if V264XF   =      9 then V264XF = .;
    if V264XG   =      9 then V264XG = .;
    if V264XH   =      9 then V264XH = .;
    if V264XI   =      9 then V264XI = .;
    if V264XJ   =      9 then V264XJ = .;
    if V265B    =      9 then V265B = .;
    if V265C    =      9 then V265C = .;
    if V265D    =      9 then V265D = .;
    if V265E    =      9 then V265E = .;
    if V265H    =      9 then V265H = .;
    if V266A    =      9 then V266A = .;
    if V266B    =      9 then V266B = .;
    if V266D    =      9 then V266D = .;
    if V266E    =      9 then V266E = .;
    if V266X    =      9 then V266X = .;
    if V271     =      9 then V271 = .;
    if V271A    =      9 then V271A = .;
    if V272A    =      9 then V272A = .;
    if V272B    =      9 then V272B = .;
    if V272C    =      9 then V272C = .;
    if V273A    =      9 then V273A = .;
    if V273B    =      9 then V273B = .;
    if V280A    =      9 then V280A = .;
    if V280X    =      9 then V280X = .;
    if LV200    =      9 then LV200 = .;
    if LV203A   =      9 then LV203A = .;
    if LV203B   =      9 then LV203B = .;
    if LV203C   =      9 then LV203C = .;
    if LV203D   =      9 then LV203D = .;
    if LV203E   =      9 then LV203E = .;
    if LV203F   =      9 then LV203F = .;
    if LV203G   =      9 then LV203G = .;
    if LV206B   =      9 then LV206B = .;
    if LV221    =      9 then LV221 = .;
    if LV222A   =      9 then LV222A = .;
    if LV224    =      9 then LV224 = .;
    if LV225    =      9 then LV225 = .;
    if LV226    =      9 then LV226 = .;
    if V301A    =     99 then V301A = .;
    if V303A    =      9 then V303A = .;
    if V303B    =      9 then V303B = .;
    if V303D    =      9 then V303D = .;
    if V303E    =      9 then V303E = .;
    if V303G    =      9 then V303G = .;
    if V303H    =      9 then V303H = .;
    if V303I    =      9 then V303I = .;
    if V303J    =      9 then V303J = .;
    if V303K    =      9 then V303K = .;
    if V303L    =      9 then V303L = .;
    if V303O    =      9 then V303O = .;
    if V303Q    =      9 then V303Q = .;
    if V303R    =      9 then V303R = .;
    if V303S    =      9 then V303S = .;
    if V303X    =      9 then V303X = .;
    if V306     =      9 then V306 = .;
    if V307A    =      9 then V307A = .;
    if V307B    =      9 then V307B = .;
    if V307C    =      9 then V307C = .;
    if V308     =      9 then V308 = .;
    if V308A    =      9 then V308A = .;
    if V324     =      9 then V324 = .;
    if V325A    =      9 then V325A = .;
    if V325D    =      9 then V325D = .;
    if V325F    =      9 then V325F = .;
    if V325G    =      9 then V325G = .;
    if V325H    =      9 then V325H = .;
    if V325I    =      9 then V325I = .;
    if V327A    =      9 then V327A = .;
    if V327X    =      9 then V327X = .;
    if V330     =      9 then V330 = .;
    if V331A1   =      9 then V331A1 = .;
    if V331C    =      9 then V331C = .;
    if V331F    =      9 then V331F = .;
    if V331G    =      9 then V331G = .;
    if V331J    =      9 then V331J = .;
    if V331L    =      9 then V331L = .;
    if V331O    =      9 then V331O = .;
    if V331P    =      9 then V331P = .;
    if V331Q    =      9 then V331Q = .;
    if V331R    =      9 then V331R = .;
    if V331S    =      9 then V331S = .;
    if V331T    =      9 then V331T = .;
    if V331U    =      9 then V331U = .;
    if V333B    =      9 then V333B = .;
    if V333C    =      9 then V333C = .;
    if V333D    =      9 then V333D = .;
    if V333E    =      9 then V333E = .;
    if V334A    =      9 then V334A = .;
    if V334B    =      9 then V334B = .;
    if V334C    =      9 then V334C = .;
    if V334D    =      9 then V334D = .;
    if V335A    =      9 then V335A = .;
    if V335B    =      9 then V335B = .;
    if V335C    =      9 then V335C = .;
    if V335E    =      9 then V335E = .;
    if V335F    =      9 then V335F = .;
    if V336A    =      9 then V336A = .;
    if V336B    =      9 then V336B = .;
    if V336C    =      9 then V336C = .;
    if V336D    =      9 then V336D = .;
    if V336F    =      9 then V336F = .;
    if V336G    =      9 then V336G = .;
    if V342     =      9 then V342 = .;
    if V343A    =      9 then V343A = .;
    if V343B    =      9 then V343B = .;
    if V343C    =      9 then V343C = .;
    if V343D    =      9 then V343D = .;
    if V343E    =      9 then V343E = .;
    if V343F    =      9 then V343F = .;
    if V344     =      9 then V344 = .;
    if LV302A   =      9 then LV302A = .;
    if LV302B   =      9 then LV302B = .;
    if LV303A   =      9 then LV303A = .;
    if LV303B   =      9 then LV303B = .;
    if LV303D   =      9 then LV303D = .;
    if LV303E   =      9 then LV303E = .;
    if LV303G   =      9 then LV303G = .;
    if LV303H   =      9 then LV303H = .;
    if LV303I   =      9 then LV303I = .;
    if LV303J   =      9 then LV303J = .;
    if LV303K   =      9 then LV303K = .;
    if LV303L   =      9 then LV303L = .;
    if LV303O   =      9 then LV303O = .;
    if LV306A   =      9 then LV306A = .;
    if LV306B   =      9 then LV306B = .;
    if LV320A   =      9 then LV320A = .;
    if LV320C   =      9 then LV320C = .;
    if LV320D   =      9 then LV320D = .;
    if LV320E   =      9 then LV320E = .;
    if LV320F   =      9 then LV320F = .;
    if V401A    =     99 then V401A = .;
    if V406     =      9 then V406 = .;
    if V407A    =      9 then V407A = .;
    if V407B    =      9 then V407B = .;
    if V407C    =      9 then V407C = .;
    if V407D    =      9 then V407D = .;
    if V407E    =      9 then V407E = .;
    if V407F    =      9 then V407F = .;
    if V407G    =      9 then V407G = .;
    if V407H    =      9 then V407H = .;
    if V409B    =      9 then V409B = .;
    if V409C    =      9 then V409C = .;
    if V409E    =      9 then V409E = .;
    if V409F    =      9 then V409F = .;
    if V409G    =      9 then V409G = .;
    if V409H    =      9 then V409H = .;
    if V409I    =      9 then V409I = .;
    if V409J    =      9 then V409J = .;
    if V409K    =      9 then V409K = .;
    if V410     =      9 then V410 = .;
    if V410A    =      9 then V410A = .;
    if V411A    =      9 then V411A = .;
    if V411B    =      9 then V411B = .;
    if V424     =      9 then V424 = .;
    if V430     =      9 then V430 = .;
    if V431A1   =      9 then V431A1 = .;
    if V431C    =      9 then V431C = .;
    if V431F    =      9 then V431F = .;
    if V431G    =      9 then V431G = .;
    if V431J    =      9 then V431J = .;
    if V431L    =      9 then V431L = .;
    if V431O    =      9 then V431O = .;
    if V431P    =      9 then V431P = .;
    if V431Q    =      9 then V431Q = .;
    if V431R    =      9 then V431R = .;
    if V431S    =      9 then V431S = .;
    if V431T    =      9 then V431T = .;
    if V431U    =      9 then V431U = .;
    if V432A    =      9 then V432A = .;
    if V432B    =      9 then V432B = .;
    if V432C    =      9 then V432C = .;
    if V432D    =      9 then V432D = .;
    if V432E    =      9 then V432E = .;
    if V432F    =      9 then V432F = .;
    if V433B    =      9 then V433B = .;
    if V433C    =      9 then V433C = .;
    if V433D    =      9 then V433D = .;
    if V433F    =      9 then V433F = .;
    if V433G    =      9 then V433G = .;
    if V433H    =      9 then V433H = .;
    if V433I    =      9 then V433I = .;
    if V434A    =      9 then V434A = .;
    if V434B    =      9 then V434B = .;
    if V434X    =      9 then V434X = .;
    if V435A    =      9 then V435A = .;
    if V435B    =      9 then V435B = .;
    if V460     =      9 then V460 = .;
    if V461A    =      9 then V461A = .;
    if V461B    =      9 then V461B = .;
    if V461C    =      9 then V461C = .;
    if V461D    =      9 then V461D = .;
    if V462A    =      9 then V462A = .;
    if V462B    =      9 then V462B = .;
    if V462C    =      9 then V462C = .;
    if V462D    =      9 then V462D = .;
    if V462E    =      9 then V462E = .;
    if V501     =    999 then V501 = .;
    if V501A    =    999 then V501A = .;
    if V502     =      9 then V502 = .;
    if V503     =      9 then V503 = .;
    if V506C    =      9 then V506C = .;
    if V507A    =      9 then V507A = .;
    if V507B    =      9 then V507B = .;
    if V507C    =      9 then V507C = .;
    if V507D    =      9 then V507D = .;
    if V507E    =      9 then V507E = .;
    if V507F    =      9 then V507F = .;
    if V507G    =      9 then V507G = .;
    if V507H    =      9 then V507H = .;
    if V507I    =      9 then V507I = .;
    if V507J    =      9 then V507J = .;
    if V507K    =      9 then V507K = .;
    if V507L    =      9 then V507L = .;
    if V507M    =      9 then V507M = .;
    if V530     =      9 then V530 = .;
    if V531A1   =      9 then V531A1 = .;
    if V531C    =      9 then V531C = .;
    if V531F    =      9 then V531F = .;
    if V531G    =      9 then V531G = .;
    if V531J    =      9 then V531J = .;
    if V531L    =      9 then V531L = .;
    if V531O    =      9 then V531O = .;
    if V531P    =      9 then V531P = .;
    if V531Q    =      9 then V531Q = .;
    if V531R    =      9 then V531R = .;
    if V531S    =      9 then V531S = .;
    if V531T    =      9 then V531T = .;
    if V531U    =      9 then V531U = .;
    if V531XD   =      9 then V531XD = .;
    if V531XE   =      9 then V531XE = .;
    if V531XF   =      9 then V531XF = .;
    if V531XG   =      9 then V531XG = .;
    if V531XH   =      9 then V531XH = .;
    if V531XI   =      9 then V531XI = .;
    if V531XJ   =      9 then V531XJ = .;
    if V531XK   =      9 then V531XK = .;
    if V531XL   =      9 then V531XL = .;
    if V531XM   =      9 then V531XM = .;
    if V531XN   =      9 then V531XN = .;
    if V531XO   =      9 then V531XO = .;
    if V533B    =      9 then V533B = .;
    if V533C    =      9 then V533C = .;
    if V533D    =      9 then V533D = .;
    if V533E    =      9 then V533E = .;
    if V533F    =      9 then V533F = .;
    if V533G    =      9 then V533G = .;
    if V533H    =      9 then V533H = .;
    if V533I    =      9 then V533I = .;
    if V533J    =      9 then V533J = .;
    if V534A    =      9 then V534A = .;
    if V534C    =      9 then V534C = .;
    if V534D    =      9 then V534D = .;
    if V534E    =      9 then V534E = .;
    if V534G    =      9 then V534G = .;
    if V534I    =      9 then V534I = .;
    if V534M    =      9 then V534M = .;
    if V534N    =      9 then V534N = .;
    if V534O    =      9 then V534O = .;
    if V536A    =      9 then V536A = .;
    if V536B    =      9 then V536B = .;
    if V536C    =      9 then V536C = .;
    if V536D    =      9 then V536D = .;
    if V536E    =      9 then V536E = .;
    if V536F    =      9 then V536F = .;
    if V537A    =      9 then V537A = .;
    if V537B    =      9 then V537B = .;
    if V537C    =      9 then V537C = .;
    if V537D    =      9 then V537D = .;
    if V537E    =      9 then V537E = .;
    if V545     =      9 then V545 = .;
    if V546A    =     99 then V546A = .;
    if V546B    =     99 then V546B = .;
    if V554A    =      9 then V554A = .;
    if V554B    =      9 then V554B = .;
    if V554C    =      9 then V554C = .;
    if V554D    =      9 then V554D = .;
    if V554E    =      9 then V554E = .;
    if V554F    =      9 then V554F = .;
    if V554G    =      9 then V554G = .;
    if V554H    =      9 then V554H = .;
    if V554I    =      9 then V554I = .;
    if V554J    =      9 then V554J = .;
    if V557C    =      9 then V557C = .;
    if V557E    =      9 then V557E = .;
    if V557F    =      9 then V557F = .;
    if V557G    =      9 then V557G = .;
    if V557H    =      9 then V557H = .;
    if V557I    =      9 then V557I = .;
    if V557J    =      9 then V557J = .;
    if V557K    =      9 then V557K = .;
    if V557L    =      9 then V557L = .;
    if V557M    =      9 then V557M = .;
    if V558     =      9 then V558 = .;
    if V559     =      9 then V559 = .;
    if V567     =      9 then V567 = .;
    if V568     =      9 then V568 = .;
    if V569     =      9 then V569 = .;
    if V570     =      9 then V570 = .;
    if V600A    =      9 then V600A = .;
    if V601A    =     99 then V601A = .;
    if V624     =      9 then V624 = .;
    if V625A    =      9 then V625A = .;
    if V625B    =      9 then V625B = .;
    if V625C    =      9 then V625C = .;
    if V625D    =      9 then V625D = .;
    if V625E    =      9 then V625E = .;
    if V625F    =      9 then V625F = .;
    if V625G    =      9 then V625G = .;
    if V626A    =      9 then V626A = .;
    if V626B    =      9 then V626B = .;
    if V626C    =      9 then V626C = .;
    if V626D    =      9 then V626D = .;
    if V626E    =      9 then V626E = .;
    if V626F    =      9 then V626F = .;
    if V626G    =      9 then V626G = .;
    if V627A    =      9 then V627A = .;
    if V627X    =      9 then V627X = .;
    if V628     =      9 then V628 = .;
    if V630     =      9 then V630 = .;
    if V631A1   =      9 then V631A1 = .;
    if V631C    =      9 then V631C = .;
    if V631F    =      9 then V631F = .;
    if V631G    =      9 then V631G = .;
    if V631J    =      9 then V631J = .;
    if V631L    =      9 then V631L = .;
    if V631O    =      9 then V631O = .;
    if V631P    =      9 then V631P = .;
    if V631Q    =      9 then V631Q = .;
    if V631R    =      9 then V631R = .;
    if V631S    =      9 then V631S = .;
    if V631T    =      9 then V631T = .;
    if V631U    =      9 then V631U = .;
    if V640A    =      9 then V640A = .;
    if V640B    =      9 then V640B = .;
    if V640C    =      9 then V640C = .;
    if V640D    =      9 then V640D = .;
    if V642     =      9 then V642 = .;
    if V642A    =      9 then V642A = .;
    if V650     =      9 then V650 = .;
    if V805A    =      9 then V805A = .;
    if V805B    =      9 then V805B = .;
    if V805X    =      9 then V805X = .;
    if V810A    =      9 then V810A = .;
    if V810B    =      9 then V810B = .;
    if V810C    =      9 then V810C = .;
    if V810X    =      9 then V810X = .;
    if V810Y    =      9 then V810Y = .;
    if V813     =      9 then V813 = .;
    if V815     =     99 then V815 = .;
    if V820A    =      9 then V820A = .;
    if V820B    =      9 then V820B = .;
    if V820C    =      9 then V820C = .;
    if V820D    =      9 then V820D = .;
    if V820E    =      9 then V820E = .;
    if V820X    =      9 then V820X = .;
    if V820Y    =      9 then V820Y = .;
    if V821     =      9 then V821 = .;
    if V821E    =      9 then V821E = .;
    if V834A    =      9 then V834A = .;
    if V834B    =      9 then V834B = .;
    if V834C    =      9 then V834C = .;
    if V834D    =      9 then V834D = .;
    if V834E    =      9 then V834E = .;
    if V834Y    =      9 then V834Y = .;
    if V835A    =      9 then V835A = .;
    if V835B    =      9 then V835B = .;
    if V835C    =      9 then V835C = .;
    if V835D    =      9 then V835D = .;
    if V835Y    =      9 then V835Y = .;
    if V836     =      9 then V836 = .;
    if V837     =      9 then V837 = .;
    if V838A    =      9 then V838A = .;
    if V838B    =      9 then V838B = .;
    if V838C    =      9 then V838C = .;
    if V838D    =      9 then V838D = .;
    if V838Y    =      9 then V838Y = .;
    if V839     =      9 then V839 = .;
    if V840A    =      9 then V840A = .;
    if V840A1   =      9 then V840A1 = .;
    if V840A2   =      9 then V840A2 = .;
    if V840A3   =      9 then V840A3 = .;
    if V840D    =      9 then V840D = .;
    if V840E    =      9 then V840E = .;
    if V840E1   =      9 then V840E1 = .;
    if V840E2   =      9 then V840E2 = .;
    if V840F    =      9 then V840F = .;
    if V840F1   =      9 then V840F1 = .;
    if V840F2   =      9 then V840F2 = .;
    if V840F3   =      9 then V840F3 = .;
    if V840G    =      9 then V840G = .;
    if V840G1   =      9 then V840G1 = .;
    if V840I    =      9 then V840I = .;
    if V840J    =      9 then V840J = .;
    if V840K    =      9 then V840K = .;
    if V840L    =      9 then V840L = .;
    if V840M    =      9 then V840M = .;
    if V840N    =      9 then V840N = .;
    if V840O    =      9 then V840O = .;
    if V840P    =      9 then V840P = .;
    if V840Y    =      9 then V840Y = .;
    if V841A1   =      9 then V841A1 = .;
    if V841A2   =      9 then V841A2 = .;
    if V841B    =      9 then V841B = .;
    if V841C    =      9 then V841C = .;
    if V841D    =      9 then V841D = .;
    if V841E    =      9 then V841E = .;
    if V841F    =      9 then V841F = .;
    if V841G    =      9 then V841G = .;
    if V842A1   =      9 then V842A1 = .;
    if V842A2   =      9 then V842A2 = .;
    if V842B    =      9 then V842B = .;
    if V842C    =      9 then V842C = .;
    if V842G    =      9 then V842G = .;
    if V843A    =      9 then V843A = .;
    if V843B    =      9 then V843B = .;
    if V843C    =      9 then V843C = .;
    if V843E    =      9 then V843E = .;
    if V843F    =      9 then V843F = .;
    if V843G    =      9 then V843G = .;
    if V843H    =      9 then V843H = .;
    if V843I    =      9 then V843I = .;
    if V843J    =      9 then V843J = .;
    if V843K    =      9 then V843K = .;
    if V843L    =      9 then V843L = .;
    if V843M    =      9 then V843M = .;
    if V843N    =      9 then V843N = .;
    if V844A    =      9 then V844A = .;
    if V844B    =      9 then V844B = .;
    if V844C    =      9 then V844C = .;
    if V844F    =      9 then V844F = .;
    if V844G    =      9 then V844G = .;
    if V844H    =      9 then V844H = .;
    if V844I    =      9 then V844I = .;
    if V844J    =      9 then V844J = .;
    if V844K    =      9 then V844K = .;
    if V844N    =      9 then V844N = .;
    if V845A    =      9 then V845A = .;
    if V845B    =      9 then V845B = .;
    if V845C    =      9 then V845C = .;
    if V845D    =      9 then V845D = .;
    if V845G    =      9 then V845G = .;
    if V845H    =      9 then V845H = .;
    if V845I    =      9 then V845I = .;
    if V845J    =      9 then V845J = .;
    if V845K    =      9 then V845K = .;
    if V846A    =      9 then V846A = .;
    if V846B    =      9 then V846B = .;
    if V846C    =      9 then V846C = .;
    if V846J    =      9 then V846J = .;
    if V846K    =      9 then V846K = .;
    if V847A    =      9 then V847A = .;
    if V847B    =      9 then V847B = .;
    if V847C    =      9 then V847C = .;
    if V847D    =      9 then V847D = .;
    if V847E    =      9 then V847E = .;
    if V848B    =      9 then V848B = .;
    if V849A    =      9 then V849A = .;
    if V849B    =      9 then V849B = .;
    if V849B1   =      9 then V849B1 = .;
    if V850A    =      9 then V850A = .;
    if V850B    =      9 then V850B = .;
    if V850B1   =      9 then V850B1 = .;
    if V851     =      9 then V851 = .;
    if V851A    =      9 then V851A = .;
    if V851B    =      9 then V851B = .;
    if V852A    =      9 then V852A = .;
    if V852B    =      9 then V852B = .;
    if V852C    =      9 then V852C = .;
    if V852D    =      9 then V852D = .;
    if V852E    =      9 then V852E = .;
    if V855A    =      9 then V855A = .;
    if V855B    =      9 then V855B = .;
    if V855C    =      9 then V855C = .;
    if V855D    =      9 then V855D = .;
    if V856A    =      9 then V856A = .;
    if V856B    =      9 then V856B = .;
    if V857B    =      9 then V857B = .;
    if V857C    =      9 then V857C = .;
    if V857D    =      9 then V857D = .;
    if V859A    =      9 then V859A = .;
    if V859B    =      9 then V859B = .;
    if V859C    =      9 then V859C = .;
    if V859D    =      9 then V859D = .;
    if V859E    =      9 then V859E = .;
    if V859F    =      9 then V859F = .;
    if V859G    =      9 then V859G = .;
    if V859GG   =      9 then V859GG = .;
    if V859H    =      9 then V859H = .;
    if V859I    =      9 then V859I = .;
    if V860A    =      9 then V860A = .;
    if V860B    =      9 then V860B = .;
    if V860C    =      9 then V860C = .;
    if V860D    =      9 then V860D = .;
    if V860F    =      9 then V860F = .;
    if V861A    =      9 then V861A = .;
    if V861B    =      9 then V861B = .;
    if V861C    =      9 then V861C = .;
    if V861D    =      9 then V861D = .;
    if V861E    =      9 then V861E = .;
    if V863     =      9 then V863 = .;
    if V864A    =      9 then V864A = .;
    if V864B    =      9 then V864B = .;
    if V864C    =      9 then V864C = .;
    if V864D    =      9 then V864D = .;
    if V864F    =      9 then V864F = .;
    if V865A    =      9 then V865A = .;
    if V865B    =      9 then V865B = .;
    if V865C    =      9 then V865C = .;
    if V865D    =      9 then V865D = .;
    if V865F    =      9 then V865F = .;
    if V866A1   =      9 then V866A1 = .;
    if V866C    =      9 then V866C = .;
    if V866F    =      9 then V866F = .;
    if V866G    =      9 then V866G = .;
    if V866J    =      9 then V866J = .;
    if V866L    =      9 then V866L = .;
    if V866O    =      9 then V866O = .;
    if V866P    =      9 then V866P = .;
    if V866Q    =      9 then V866Q = .;
    if V866R    =      9 then V866R = .;
    if V866S    =      9 then V866S = .;
    if V866T    =      9 then V866T = .;
    if V866U    =      9 then V866U = .;
    if V870A    =      9 then V870A = .;
    if V870B    =      9 then V870B = .;
    if V870C    =      9 then V870C = .;
    if V870D    =      9 then V870D = .;
    if V870E    =      9 then V870E = .;
    if V870F    =      9 then V870F = .;
    if V870X    =      9 then V870X = .;
    if V871     =      9 then V871 = .;
    if V872     =      9 then V872 = .;
    if V873A    =      9 then V873A = .;
    if V873B    =      9 then V873B = .;
    if V874A    =      9 then V874A = .;
    if V874B    =      9 then V874B = .;
    if V874C    =      9 then V874C = .;
    if V874X    =      9 then V874X = .;
    if V875     =      9 then V875 = .;
    if V876A    =      9 then V876A = .;
    if V876B    =      9 then V876B = .;
    if V877     =      9 then V877 = .;
    if V902A    =      9 then V902A = .;
    if V902B    =      9 then V902B = .;
    if V903_02  =      9 then V903_02 = .;
    if V903_05  =      9 then V903_05 = .;
    if V903_06  =      9 then V903_06 = .;
    if V903_08  =      9 then V903_08 = .;
    if V903_10  =      9 then V903_10 = .;
    if V903_13  =      9 then V903_13 = .;
    if V903_14  =      9 then V903_14 = .;
    if V903_15  =      9 then V903_15 = .;
    if V903_16  =      9 then V903_16 = .;
    if V903_17  =      9 then V903_17 = .;
    if V903_18  =      9 then V903_18 = .;
    if V903_19  =      9 then V903_19 = .;
    if V903_20  =      9 then V903_20 = .;
    if V903_21  =      9 then V903_21 = .;
    if V903_22  =      9 then V903_22 = .;
    if V904_03  =      9 then V904_03 = .;
    if V904_04  =      9 then V904_04 = .;
    if V904_05  =      9 then V904_05 = .;
    if V904_06  =      9 then V904_06 = .;
    if V904_07  =      9 then V904_07 = .;
    if V904_08  =      9 then V904_08 = .;
    if V905_03  =      9 then V905_03 = .;
    if V905_08  =      9 then V905_08 = .;
    if V905_14  =      9 then V905_14 = .;
    if V905_18  =      9 then V905_18 = .;
    if V905_19  =      9 then V905_19 = .;
    if V905_20  =      9 then V905_20 = .;
    if V905_21  =      9 then V905_21 = .;
    if V905_22  =      9 then V905_22 = .;
    if V905_23  =      9 then V905_23 = .;
    if V906_03  =      9 then V906_03 = .;
    if V906_07  =      9 then V906_07 = .;
    if V906_08  =      9 then V906_08 = .;
    if V906_11  =      9 then V906_11 = .;
    if V906_12  =      9 then V906_12 = .;
    if V907_03  =      9 then V907_03 = .;
    if V907_04  =      9 then V907_04 = .;
    if V907_05  =      9 then V907_05 = .;
    if V907_06  =      9 then V907_06 = .;
    if V907_07  =      9 then V907_07 = .;
    if V907_08  =      9 then V907_08 = .;
    if V908_02  =      9 then V908_02 = .;
    if V908_03  =      9 then V908_03 = .;
    if V908_04  =      9 then V908_04 = .;
    if V908_05  =      9 then V908_05 = .;
    if V909_02  =      9 then V909_02 = .;
    if V909_04  =      9 then V909_04 = .;
    if V909_05  =      9 then V909_05 = .;
    if V909_06  =      9 then V909_06 = .;
    if V910_03  =      9 then V910_03 = .;
    if V910_04  =      9 then V910_04 = .;
    if V910_06  =      9 then V910_06 = .;
    if V910_07  =      9 then V910_07 = .;
    if V910_08  =      9 then V910_08 = .;
    if V911_02  =      9 then V911_02 = .;
    if V913_01  =      9 then V913_01 = .;
    if V913_02  =      9 then V913_02 = .;
    if V913_04  =      9 then V913_04 = .;
    if V913_05  =      9 then V913_05 = .;
    if V913_06  =      9 then V913_06 = .;
    if V913_07  =      9 then V913_07 = .;
    if V913_10  =      9 then V913_10 = .;
    if V913_11  =      9 then V913_11 = .;
    if V913_12  =      9 then V913_12 = .;
    if V913_13  =      9 then V913_13 = .;
    if V914_05  =      9 then V914_05 = .;
    if V914_06  =      9 then V914_06 = .;
    if V914_07  =      9 then V914_07 = .;
    if V914_08  =      9 then V914_08 = .;
    if V915_01  =      9 then V915_01 = .;
    if V915_02  =      9 then V915_02 = .;
    if V915_03  =      9 then V915_03 = .;
    if V915_04  =      9 then V915_04 = .;
    if V916_01  =      9 then V916_01 = .;
    if V916_03  =      9 then V916_03 = .;
    if V916_06  =      9 then V916_06 = .;
    if V916_07  =      9 then V916_07 = .;
    if V916_08  =      9 then V916_08 = .;
    if V916_09  =      9 then V916_09 = .;
    if V917_04  =      9 then V917_04 = .;
    if V917_05  =      9 then V917_05 = .;
    if V917_06  =      9 then V917_06 = .;
    if V918_01  =      9 then V918_01 = .;
    if V918_02  =      9 then V918_02 = .;
    if V918_04  =      9 then V918_04 = .;
    if V918_05  =      9 then V918_05 = .;
    if V918_06  =      9 then V918_06 = .;
    if V919_03  =      9 then V919_03 = .;
    if V919_04  =      9 then V919_04 = .;
    if V919_05  =      9 then V919_05 = .;
    if V919_06  =      9 then V919_06 = .;
    if V920_02  =      9 then V920_02 = .;
    if V921_01  =      9 then V921_01 = .;
    if V921_02  =      9 then V921_02 = .;
    if V921_03  =      9 then V921_03 = .;
    if V922_03  =      9 then V922_03 = .;
    if V922_04  =      9 then V922_04 = .;
    if V922_05  =      9 then V922_05 = .;
    if V922_11  =      9 then V922_11 = .;
    if V922_12  =      9 then V922_12 = .;
    if V922_13  =      9 then V922_13 = .;
    if V922_14  =      9 then V922_14 = .;
    if V922_15  =      9 then V922_15 = .;
    if V922_16  =      9 then V922_16 = .;
    if V922_17  =      9 then V922_17 = .;
    if V922_18  =      9 then V922_18 = .;
    if V923_01  =      9 then V923_01 = .;
    if V923_02  =      9 then V923_02 = .;
    if V923_03  =      9 then V923_03 = .;
    if V923_04  =      9 then V923_04 = .;
    if V923_05  =      9 then V923_05 = .;
    if V923_06  =      9 then V923_06 = .;
    if V923_07  =      9 then V923_07 = .;
    if V923_08  =      9 then V923_08 = .;
    if V923_09  =      9 then V923_09 = .;
    if V923_11  =      9 then V923_11 = .;
    if V923_12  =      9 then V923_12 = .;
    if V924_01  =      9 then V924_01 = .;
    if V924_02  =      9 then V924_02 = .;
    if V924_03  =      9 then V924_03 = .;
    if V924_05  =      9 then V924_05 = .;
    if V926     =      9 then V926 = .;
    if V927     =      9 then V927 = .;
    if V928A    =      9 then V928A = .;
    if V928B1   =      9 then V928B1 = .;
    if V928B2   =      9 then V928B2 = .;
    if V928C    =      9 then V928C = .;
    if V928D    =      9 then V928D = .;
    if V936     =      9 then V936 = .;
    if V946_06  =      9 then V946_06 = .;
    if V946_07  =      9 then V946_07 = .;
    if V946_09  =      9 then V946_09 = .;
    if V946_12  =      9 then V946_12 = .;
    if V946_13  =      9 then V946_13 = .;
    if V946_16  =      9 then V946_16 = .;
    if V946_17  =      9 then V946_17 = .;
    if V946_18  =      9 then V946_18 = .;
    if V946_19  =      9 then V946_19 = .;
    if V950A    =      9 then V950A = .;
    if V950B    =      9 then V950B = .;
    if V951     =      9 then V951 = .;
    if V952     =      9 then V952 = .;
    if V953A    =      9 then V953A = .;
    if V953B1   =      9 then V953B1 = .;
    if V953B2   =      9 then V953B2 = .;
    if V953C    =      9 then V953C = .;
    if V953D    =      9 then V953D = .;
    if V955_01  =      9 then V955_01 = .;
    if V955_02  =      9 then V955_02 = .;
    if V955_03  =      9 then V955_03 = .;
    if V955_04  =      9 then V955_04 = .;
    if V955_05  =      9 then V955_05 = .;
    if V955_06  =      9 then V955_06 = .;
    if V955_07  =      9 then V955_07 = .;
    if V955_08  =      9 then V955_08 = .;
    if V955_09  =      9 then V955_09 = .;
    if V955_10  =      9 then V955_10 = .;
    if V955_11  =      9 then V955_11 = .;
    if V956_01  =      9 then V956_01 = .;
    if V956_02  =      9 then V956_02 = .;
    if V956_03  =      9 then V956_03 = .;
    if V956_04  =      9 then V956_04 = .;
    if V956_05  =      9 then V956_05 = .;
    if V956_06  =      9 then V956_06 = .;
    if V957_01  =      9 then V957_01 = .;
    if V957_02  =      9 then V957_02 = .;
    if V957_03  =      9 then V957_03 = .;
    if V957_04  =      9 then V957_04 = .;
    if V957_05  =      9 then V957_05 = .;
    if V957_06  =      9 then V957_06 = .;
    if V957_07  =      9 then V957_07 = .;
    if V957_08  =      9 then V957_08 = .;
    if V957_09  =      9 then V957_09 = .;
    if V957_10  =      9 then V957_10 = .;
    if V957_11  =      9 then V957_11 = .;
    if V957_12  =      9 then V957_12 = .;
    if V958_01  =      9 then V958_01 = .;
    if V958_02  =      9 then V958_02 = .;
    if V958_03  =      9 then V958_03 = .;
    if V958_04  =      9 then V958_04 = .;
    if V958_05  =      9 then V958_05 = .;
    if V958_06  =      9 then V958_06 = .;
    if V958_07  =      9 then V958_07 = .;
    if V958_08  =      9 then V958_08 = .;
    if V958_09  =      9 then V958_09 = .;
    if V958_10  =      9 then V958_10 = .;
    if V958_11  =      9 then V958_11 = .;
    if V960A    =      9 then V960A = .;
    if V960B    =      9 then V960B = .;
    if V961     =      9 then V961 = .;
    if V962     =      9 then V962 = .;
    if V963A    =      9 then V963A = .;
    if V963B1   =      9 then V963B1 = .;
    if V963B2   =      9 then V963B2 = .;
    if V963C    =      9 then V963C = .;
    if V963D    =      9 then V963D = .;
    if V1001    =     99 then V1001 = .;
    if V1002    =      9 then V1002 = .;
    if V1003    =      9 then V1003 = .;
    if V1004    =      9 then V1004 = .;
    if V1005A   =      9 then V1005A = .;
    if V1005B   =      9 then V1005B = .;
    if V1005C   =      9 then V1005C = .;
    if V1005D   =      9 then V1005D = .;
    if V1005E   =      9 then V1005E = .;
    if V1005F   =      9 then V1005F = .;
    if V1005X   =      9 then V1005X = .;
    if V1006    =      9 then V1006 = .;
    if V1007A   =      9 then V1007A = .;
    if V1007X   =      9 then V1007X = .;
    if V1100A   =      9 then V1100A = .;
    if V1106    =      9 then V1106 = .;
    if V1107A   =      9 then V1107A = .;
    if V1107B   =      9 then V1107B = .;
    if V1107C   =      9 then V1107C = .;
    if V1109A   =      9 then V1109A = .;
    if V1109B   =      9 then V1109B = .;
    if V1109C   =      9 then V1109C = .;
    if V1109D   =      9 then V1109D = .;
    if V1115    =      9 then V1115 = .;
    if V1138    =      9 then V1138 = .;
    if V1139A1  =      9 then V1139A1 = .;
    if V1139C   =      9 then V1139C = .;
    if V1139F   =      9 then V1139F = .;
    if V1139G   =      9 then V1139G = .;
    if V1139J   =      9 then V1139J = .;
    if V1139L   =      9 then V1139L = .;
    if V1139O   =      9 then V1139O = .;
    if V1139P   =      9 then V1139P = .;
    if V1139Q   =      9 then V1139Q = .;
    if V1139R   =      9 then V1139R = .;
    if V1139S   =      9 then V1139S = .;
    if V1139T   =      9 then V1139T = .;
    if V1139U   =      9 then V1139U = .;
    if V1140    =     99 then V1140 = .;
    if V1141    =      9 then V1141 = .;
    if V1142    =      9 then V1142 = .;
    if V1143    =      9 then V1143 = .;
    if V1144    =      9 then V1144 = .;
    if V1206    =     99 then V1206 = .;
    if V1208A1  =      9 then V1208A1 = .;
    if V1208A2  =      9 then V1208A2 = .;
    if V1208F   =      9 then V1208F = .;
    if V1208G1  =      9 then V1208G1 = .;
    if V1208H   =      9 then V1208H = .;
    if V1208L   =      9 then V1208L = .;
    if V1208M   =      9 then V1208M = .;
    if V1208S   =      9 then V1208S = .;
    if V1215A   =      9 then V1215A = .;
    if V1215B   =      9 then V1215B = .;
    if V1215X   =      9 then V1215X = .;
    if V1215Y   =      9 then V1215Y = .;
    if V1260    =      9 then V1260 = .;
    if V1273A   =      9 then V1273A = .;
    if V1273B   =      9 then V1273B = .;
    if V1273C   =      9 then V1273C = .;
    if V1273D   =      9 then V1273D = .;
    if V1273E   =      9 then V1273E = .;
    if V1273F   =      9 then V1273F = .;
    if V1273G   =      9 then V1273G = .;
    if V1273H   =      9 then V1273H = .;
    if V1273I   =      9 then V1273I = .;
    if V1273J   =      9 then V1273J = .;
    if V1273K   =      9 then V1273K = .;
    if V1273L   =      9 then V1273L = .;
    if V1273M   =      9 then V1273M = .;
    if V1273Y   =      9 then V1273Y = .;
    if V1274    =      9 then V1274 = .;
    if V1275    =      9 then V1275 = .;
    if V1276    =      9 then V1276 = .;
    if V1277A1  =      9 then V1277A1 = .;
    if V1277C   =      9 then V1277C = .;
    if V1277F   =      9 then V1277F = .;
    if V1277G   =      9 then V1277G = .;
    if V1277J   =      9 then V1277J = .;
    if V1277L   =      9 then V1277L = .;
    if V1277O   =      9 then V1277O = .;
    if V1277P   =      9 then V1277P = .;
    if V1277Q   =      9 then V1277Q = .;
    if V1277R   =      9 then V1277R = .;
    if V1277S   =      9 then V1277S = .;
    if V1277T   =      9 then V1277T = .;
    if V1277U   =      9 then V1277U = .;
    if V1278A   =      9 then V1278A = .;
    if V1278B   =      9 then V1278B = .;
    if V1278C   =      9 then V1278C = .;
    if V1278D   =      9 then V1278D = .;
    if V1278E   =      9 then V1278E = .;
    if V1278F   =      9 then V1278F = .;
    if V1278G   =      9 then V1278G = .;
    if V1278H   =      9 then V1278H = .;
    if V1278I   =      9 then V1278I = .;
    if V1278J   =      9 then V1278J = .;
    if V1278K   =      9 then V1278K = .;
    if V1278L   =      9 then V1278L = .;
    if V1278M   =      9 then V1278M = .;
    if V1278Y   =      9 then V1278Y = .;
    if V1279    =      9 then V1279 = .;
    if V1280    =      9 then V1280 = .;
    if V1281A1  =      9 then V1281A1 = .;
    if V1281C   =      9 then V1281C = .;
    if V1281F   =      9 then V1281F = .;
    if V1281G   =      9 then V1281G = .;
    if V1281J   =      9 then V1281J = .;
    if V1281L   =      9 then V1281L = .;
    if V1281O   =      9 then V1281O = .;
    if V1281P   =      9 then V1281P = .;
    if V1281Q   =      9 then V1281Q = .;
    if V1281R   =      9 then V1281R = .;
    if V1281S   =      9 then V1281S = .;
    if V1281T   =      9 then V1281T = .;
    if V1281U   =      9 then V1281U = .;
    if V1307A   =     99 then V1307A = .;
    if V1327D   =      9 then V1327D = .;
    if V1327E   =      9 then V1327E = .;
    if V1327G   =      9 then V1327G = .;
    if V1327H   =      9 then V1327H = .;
    if V1327I   =      9 then V1327I = .;
    if V1327J   =      9 then V1327J = .;
    if V1328    =      9 then V1328 = .;
    if V1329A1  =      9 then V1329A1 = .;
    if V1329C   =      9 then V1329C = .;
    if V1329F   =      9 then V1329F = .;
    if V1329G   =      9 then V1329G = .;
    if V1329J   =      9 then V1329J = .;
    if V1329L   =      9 then V1329L = .;
    if V1329O   =      9 then V1329O = .;
    if V1329P   =      9 then V1329P = .;
    if V1329Q   =      9 then V1329Q = .;
    if V1329R   =      9 then V1329R = .;
    if V1329S   =      9 then V1329S = .;
    if V1329T   =      9 then V1329T = .;
    if V1329U   =      9 then V1329U = .;
    if V1331    =      9 then V1331 = .;
    if V1332A   =      9 then V1332A = .;
    if V1332B   =      9 then V1332B = .;
    if V1332X   =      9 then V1332X = .;
    if V1337    =      9 then V1337 = .;
    if V1355    =      9 then V1355 = .;
    if V1356    =      9 then V1356 = .;
    if V1357    =      9 then V1357 = .;
    if V1358    =      9 then V1358 = .;
    if V1415A   =      9 then V1415A = .;
    if V1415B   =      9 then V1415B = .;
    if V1415C   =      9 then V1415C = .;
    if V1415D   =      9 then V1415D = .;
    if V1415E   =      9 then V1415E = .;
    if V1415F   =      9 then V1415F = .;
    if V1415G   =      9 then V1415G = .;
    if V1415H   =      9 then V1415H = .;
    if V1415I   =      9 then V1415I = .;
    if V1415J   =      9 then V1415J = .;
    if V1415K   =      9 then V1415K = .;
    if V1415X   =      9 then V1415X = .;
    if V1421A   =      9 then V1421A = .;
    if V1421X   =      9 then V1421X = .;
    if V1451    =      9 then V1451 = .;
    if V1452A   =      9 then V1452A = .;
    if V1452B   =      9 then V1452B = .;
    if V1452C   =      9 then V1452C = .;
    if V1452D   =      9 then V1452D = .;
    if V1452E   =      9 then V1452E = .;
    if V1452F   =      9 then V1452F = .;
    if V1452G   =      9 then V1452G = .;
    if V1452H   =      9 then V1452H = .;
    if V1452I   =      9 then V1452I = .;
    if V1452J   =      9 then V1452J = .;
    if V1452K   =      9 then V1452K = .;
    if V1452X   =      9 then V1452X = .;
    if V1515    =      9 then V1515 = .;
    if V1516A   =      9 then V1516A = .;
    if V1516B   =      9 then V1516B = .;
    if V1516C   =      9 then V1516C = .;
    if V1516D   =      9 then V1516D = .;
    if V1516E   =      9 then V1516E = .;
    if V1516F   =      9 then V1516F = .;
    if V1516J   =      9 then V1516J = .;
    if V1518B   =      9 then V1518B = .;
    if V1518C   =      9 then V1518C = .;
    if V1518D   =      9 then V1518D = .;
    if V1518E   =      9 then V1518E = .;
    if V1518F   =      9 then V1518F = .;
    if V1521A   =      9 then V1521A = .;
    if V1521B   =      9 then V1521B = .;
    if V1560    =      9 then V1560 = .;
    if V1561    =      9 then V1561 = .;
    if V1601    =      9 then V1601 = .;
    if V1601A   =      9 then V1601A = .;
    if V1602A   =      9 then V1602A = .;
    if V1602X   =      9 then V1602X = .;
    if V1603    =      9 then V1603 = .;
    if V1603A   =      9 then V1603A = .;
    if V1604A   =      9 then V1604A = .;
    if V1604X   =      9 then V1604X = .;
    if V1605    =      9 then V1605 = .;
    if V1605A   =      9 then V1605A = .;
    if V1606A   =      9 then V1606A = .;
    if V1606X   =      9 then V1606X = .;
    if V1607A   =      9 then V1607A = .;
    if V1607B   =      9 then V1607B = .;
    if V1607C   =      9 then V1607C = .;
    if V1607D   =      9 then V1607D = .;
    if V1608A   =      9 then V1608A = .;
    if V1608B   =      9 then V1608B = .;
    if V1608C   =      9 then V1608C = .;
    if V1608D   =      9 then V1608D = .;
    if V1608E   =      9 then V1608E = .;
    if V1608F   =      9 then V1608F = .;
    if V1608G   =      9 then V1608G = .;
    if V1608H   =      9 then V1608H = .;
    if V1608I   =      9 then V1608I = .;
    if V1608J   =      9 then V1608J = .;
    if V1608K   =      9 then V1608K = .;
    if V1608L   =      9 then V1608L = .;
    if V1608M   =      9 then V1608M = .;
    if V1608N   =      9 then V1608N = .;
    if V1608O   =      9 then V1608O = .;
    if V1608P   =      9 then V1608P = .;
    if V1608Q   =      9 then V1608Q = .;
    if V1609    =      9 then V1609 = .;
    if V1610A1  =      9 then V1610A1 = .;
    if V1610C   =      9 then V1610C = .;
    if V1610F   =      9 then V1610F = .;
    if V1610G   =      9 then V1610G = .;
    if V1610J   =      9 then V1610J = .;
    if V1610L   =      9 then V1610L = .;
    if V1610O   =      9 then V1610O = .;
    if V1610P   =      9 then V1610P = .;
    if V1610Q   =      9 then V1610Q = .;
    if V1610R   =      9 then V1610R = .;
    if V1610S   =      9 then V1610S = .;
    if V1610T   =      9 then V1610T = .;
    if V1610U   =      9 then V1610U = .;
    if V1701    =      9 then V1701 = .;
    if V1702A   =      9 then V1702A = .;
    if V1702B   =      9 then V1702B = .;
    if V1702C   =      9 then V1702C = .;
    if V1702D   =      9 then V1702D = .;
    if V1702E   =      9 then V1702E = .;
    if V1702F   =      9 then V1702F = .;
    if V1703A   =      9 then V1703A = .;
    if V1703B   =      9 then V1703B = .;
    if V1703C   =      9 then V1703C = .;
    if V1703D   =      9 then V1703D = .;
    if V1703E   =      9 then V1703E = .;
    if V1704    =      9 then V1704 = .;
    if V1705A1  =      9 then V1705A1 = .;
    if V1705C   =      9 then V1705C = .;
    if V1705F   =      9 then V1705F = .;
    if V1705G   =      9 then V1705G = .;
    if V1705J   =      9 then V1705J = .;
    if V1705L   =      9 then V1705L = .;
    if V1705O   =      9 then V1705O = .;
    if V1705P   =      9 then V1705P = .;
    if V1705Q   =      9 then V1705Q = .;
    if V1705R   =      9 then V1705R = .;
    if V1705S   =      9 then V1705S = .;
    if V1705T   =      9 then V1705T = .;
    if V1705U   =      9 then V1705U = .;
Run;
