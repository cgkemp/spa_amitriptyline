libname user 'D:\SPADHS-FINALIZED\NPHFS_2015\RECODE\EXPORT\EXPORTEDFILES';
Proc format;
  value F00001_
     1 = "Eastern mountain"
     2 = "Central mountain"
     3 = "Western mountain"
     4 = "Eastern hill"
     5 = "Central hill"
     6 = "Western hill"
     7 = "Mid-western hill"
     8 = "Far-western hill"
     9 = "Eastern terai"
    10 = "Central terai"
    11 = "Western terai"
    12 = "Mid-western terai"
    13 = "Far-western terai"
     ;
  value F00002_
     1 = "Taplejung"
     2 = "Panchthar"
     3 = "Ilam"
     4 = "Jhapa"
     5 = "Morang"
     6 = "Sunsari"
     7 = "Dhankuta"
     8 = "Terhathum"
     9 = "Sankhuwasabha"
    10 = "Bhojpur"
    11 = "Solukhumbu"
    12 = "Okhaldhunga"
    13 = "Khotang"
    14 = "Udayapur"
    15 = "Saptari"
    16 = "Siraha"
    17 = "Dhanusa"
    18 = "Mahotari"
    19 = "Sarlahi"
    20 = "Sindhuli"
    21 = "Ramechhap"
    22 = "Dolakha"
    23 = "Sindhupalchowk"
    24 = "Kavre"
    25 = "Lalitpur"
    26 = "Bhaktapur"
    27 = "Kathmandu"
    28 = "Nuwakot"
    29 = "Rasuwa"
    30 = "Dhading"
    31 = "Makwanpur"
    32 = "Rautahat"
    33 = "Bara"
    34 = "Parsa"
    35 = "Chitwan"
    36 = "Gorkha"
    37 = "Lamjung"
    38 = "Tanahu"
    39 = "Syangja"
    40 = "Kaski"
    41 = "Manang"
    42 = "Mustang"
    43 = "Myagdi"
    44 = "Parbat"
    45 = "Baglung"
    46 = "Gulmi"
    47 = "Palpa"
    48 = "Nawalparasi"
    49 = "Rupandehi"
    50 = "Kapilbastu"
    51 = "Arghakhanchi"
    52 = "Pyuthan"
    53 = "Rolpa"
    54 = "Rukum"
    55 = "Salyan"
    56 = "Dang"
    57 = "Banke"
    58 = "Bardia"
    59 = "Surkhet"
    60 = "Dailekh"
    61 = "Jajarkot"
    62 = "Dolpa"
    63 = "Jumla"
    64 = "Kalikot"
    65 = "Mugu"
    66 = "Humla"
    67 = "Bajura"
    68 = "Bajhang"
    69 = "Achham"
    70 = "Doti"
    71 = "Kailali"
    72 = "Kanchanpur"
    73 = "Dadeldhura"
    74 = "Baitadi"
    75 = "Darchula"
     ;
  value F00003_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00004_
     1 = "Central Govt. Hospital"
     2 = "Regional Govt. Hospital"
     3 = "Sub-regional Govt. Hospital"
     4 = "Zonal Govt. Hospital"
     5 = "District Govt. Hospital"
     6 = "Other Hospital (Not state-owned)"
     7 = "Primary Health Care Center (PHCC)"
     8 = "Health Post (HP)"
     9 = "Sub-health post (SHP)"
    10 = "Urban Health Centre (UHC)"
    11 = "HTC (stand alone)"
    12 = "Other Public Hospital"
    13 = "Central Level Govt. Hospital"
    14 = "District Level Govt. Hospital"
     ;
  value F00005_
     1 = "Government/Public"
     2 = "NGO/Private not-for-profit"
     3 = "Private for profit"
     4 = "Mission/ Faith-based"
     ;
  value F00006_
     1 = "Both MCH and HIV SPAs (multiple units and services)"
     2 = "MCH SPA only"
     3 = "HIV SPA only"
     4 = "Both MCH and HIV SPAs (single units and services in inventory)"
     5 = "New SPA qre with renamed variables, no multiples in facility level"
     ;
  value F00007_
     1 = "Facility completed"
     2 = "Facility respondent not available"
     3 = "Partially completed"
     4 = "Facility refused"
     5 = "Facility closed/ not yet functional"
     6 = "Other"
     ;
  value F00008_
     1 = "Outpatient"
     2 = "Facility has only Inpatient"
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
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00013_
     0 = "No service"
     1 = "Yes,both male and female"
     2 = "Yes, female only"
     3 = "Yes, male only"
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
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00020_
     0 = "Contraceptives not stocked"
     1 = "Stored in contraceptive service site"
     2 = "Stored with other medicines"
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
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00026_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
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
     0 = "TB medicines not stocked"
     1 = "Stored in TB service site"
     2 = "Stored with other medicines"
     ;
  value F00030_
     0 = "ARV medicines not stocked"
     1 = "Stored in ART service site"
     2 = "Stored with other medicines"
     3 = "Stored in PMTCT service site"
     4 = "Stored in BOTH (ART and PMTCT) service sites"
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
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00037_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00038_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00039_
     0 = "No service"
     1 = "Yes, completed"
     7 = "Not returned from field"
     ;
  value F00040_
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
  value F00041_
     1 = "Province 1"
     2 = "Province 2"
     3 = "Province 3"
     4 = "Province 4"
     5 = "Province 5"
     6 = "Province 6"
     7 = "Province 7"
     ;
  value F00042_
     1 = "Eastern"
     2 = "Central"
     3 = "Western"
     4 = "Mid-western"
     5 = "Far-western"
     ;
  value F00043_
     1 = "CEmONC"
     2 = "BEmONC"
     3 = "Birthing center"
     8 = "None of the above"
     ;
  value F00044_
     0 = "No 24 hour staff"
     1 = "Yes, 24 hours, duty schedule observed"
     2 = "Yes, 24 hours, duty schedule reported, not seen"
     3 = "Yes, 24 hours, duty schedule not maintained"
     ;
  value F00045_
     1 = "01 GENERALIST [NON-SPECIALIST] (01)"
     2 = "02 GYNECOLOGIST / OBSTETRICIAN (02)"
     3 = "03 ANESTHESIOLOGIST (03)"
     4 = "04 PATHOLOGIST (04)"
     5 = "05 GENERAL SURGEON (05)"
     6 = "06 PEDIATRICIAN (06)"
     7 = "07 MEDICAL OFFICER (MBBS, BDS) (08)"
     8 = "08 OTHER SPECIALISTS MEDICAL DOCTORS (07)"
    15 = "09 NURSE/AUXILLARY NURSE MIDWIFE (ANM) (10)"
    16 = "10 ANESTHETIC ASSISTANT (09)"
    25 = "11 LAB TECHNOLOGIST/OFFICER/ LAB TECH/LAB ASSISTANT (11)"
    26 = "12 PHARMACIST (13)"
    27 = "13 RADIOGRAPHER/DARK ROOM ASSISTANT (14)"
    35 = "14 COUNSELOR WITH CLINICAL QUALIFICATION (STAND-ALONE HTC ONLY) (16)"
    36 = "15 COUNSELOR WITHOUT CLINICAL QUALIFICATION (STAND-ALONE HTC ONLY) (17)"
    40 = "16 HEALTH ASSISTANT (HA) / AHW / SAHW / PUBLIC HEALTH INSPECTOR (12)"
    41 = "17 PHYSIOTHERAPIST/PHYSIOTHERAPY ASSISTANT (15)"
    42 = "18 OTHER CLINICAL STAFF NOT LISTED ABOVE (E.G., DIETICIAN) (18)"
    50 = "19 NON-CLINICAL STAFF/NO TECHNICAL QUALIFICATION (19)"
     ;
  value F00046_
  9998 = "Don't know"
     ;
  value F00047_
  9998 = "Don't know"
     ;
  value F00048_
  9998 = "Don't know"
     ;
  value F00049_
  9998 = "Don't know"
     ;
  value F00050_
  9998 = "Don't know"
     ;
  value F00051_
  9998 = "Don't know"
     ;
  value F00052_
  9998 = "Don't know"
     ;
  value F00053_
  9998 = "Don't know"
     ;
  value F00054_
  9998 = "Don't know"
     ;
  value F00055_
  9998 = "Don't know"
     ;
  value F00056_
  9998 = "Don't know"
     ;
  value F00057_
  9998 = "Don't know"
     ;
  value F00058_
  9998 = "Don't know"
     ;
  value F00059_
  9998 = "Don't know"
     ;
  value F00060_
  9998 = "Don't know"
     ;
  value F00061_
  9998 = "Don't know"
     ;
  value F00062_
  9998 = "Don't know"
     ;
  value F00063_
     0 = "Has none"
     1 = "Monthly or more often"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less often than every 6 months/irregularly"
     8 = "Don't know frequency"
     ;
  value F00064_
     0 = "No record maintained"
     1 = "Record observed"
     2 = "Reported, not seen"
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
     8 = "Don't know"
     ;
  value F00076_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00077_
     0 = "Has none"
     1 = "Yes, Monthly or more often"
     2 = "Yes, Every 2-3 months"
     3 = "Yes, Every 4-6 months"
     4 = "Yes, Less often than every 6 months/irregularly"
     6 = "Yes, don't know frequency"
     8 = "Don't know if any such meetings"
     ;
  value F00078_
     0 = "No record maintained"
     1 = "Record observed"
     2 = "Reported, not seen"
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
     0 = "No procedure"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00091_
     0 = "No record maintained"
     1 = "Record observed"
     2 = "Reported, not seen"
     ;
  value F00092_
     0 = "None"
     1 = "Yes, record observed"
     3 = "Yes, record reported, not seen"
     4 = "Yes, no records maintained"
     8 = "Don't know"
     ;
  value F00093_
     0 = "Never supervised from outside facility"
     1 = "Within past 4 months"
     2 = "More than 4 months ago"
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
     0 = "Not connected"
     1 = "Connected, always available"
     2 = "Connected, sometimes interrupted"
     3 = "Connected, DK if interrupted in last 7 days"
     8 = "Don't know if connected"
     ;
  value F00098_
     0 = "No backup generator"
     5 = "Reported functional with fuel"
     6 = "Reported functional, no fuel, DK fuel"
     7 = "Reported not functional"
     8 = "Reported, don't know if functional"
     ;
  value F00099_
     0 = "No"
     1 = "Yes"
     ;
  value F00100_
     0 = "No"
     1 = "Yes"
     ;
  value F00101_
     0 = "No"
     1 = "Yes"
     ;
  value F00102_
     0 = "No"
     1 = "Yes"
     ;
  value F00103_
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
  value F00104_
     0 = "No, beyond 500 m of facility"
     1 = "Yes, onsite"
     2 = "Yes, within 500 m of facility"
     ;
  value F00105_
     0 = "No"
     1 = "Yes"
     ;
  value F00106_
     0 = "No landline"
     1 = "Functioning observed landline"
     2 = "Non-functioning observed landline"
     3 = "Functioning reported landline"
     4 = "Non-functioning reported landline"
     ;
  value F00107_
     0 = "No cell phone"
     1 = "Functioning observed cellphone"
     2 = "Non-functioning observed cell phone"
     3 = "Functioning reported cell phone"
     4 = "Non-functioning reported cellphone"
     5 = "Functioning landline observed or reported"
     ;
  value F00108_
     0 = "No shortwave radio"
     1 = "Functioning observed shortwave radio"
     2 = "Non-functioning observed shortwave radio"
     3 = "Functioning reported shortwave radio"
     4 = "Non-functioning reported shortwave radio"
     5 = "Some functioning cell or landline phone observed or reported"
     ;
  value F00109_
     0 = "No functioning landline, cell phone or shortwave at facility"
     1 = "Observed functioning communication"
     3 = "Reported functioning communication"
     ;
  value F00110_
     0 = "No computer"
     1 = "Yes, functioning"
     2 = "Yes, but not functioning"
     ;
  value F00111_
     0 = "No computer"
     1 = "Yes, observed"
     2 = "Yes, reported, not seen"
     ;
  value F00112_
     0 = "No"
     1 = "Yes at least 2 hours on client service days"
     2 = "Yes, less than 2 hours on client service days"
     ;
  value F00113_
     0 = "No, no designated person"
     1 = "Yes, data manager/HMIS person"
     2 = "Yes, facility in-charge"
     3 = "Yes, other service provider"
     ;
  value F00114_
     0 = "No"
     1 = "Yes with training"
     2 = "Yes without training"
     8 = "Yes, don't know about training"
     ;
  value F00115_
     0 = "No"
     1 = "Yes"
     ;
  value F00116_
     0 = "Never"
     1 = "Monthly or more often"
     2 = "Every 2-3 months"
     3 = "Every 4-6 months"
     4 = "Less often than every 6 months"
     ;
  value F00117_
     0 = "No reports compiled"
     1 = "Yes, observed"
     2 = "Yes, reported not seen"
     ;
  value F00118_
999998 = "Don't know"
     ;
  value F00119_
999998 = "Don't know"
     ;
  value F00120_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00121_
     1 = "Weekly"
     2 = "Monthly"
     3 = "Every two months"
     4 = "Once in four months"
     6 = "Others"
     ;
  value F00122_
     0 = "No"
     1 = "Yes, clearly readable"
     2 = "Yes, but not clearly readable"
     ;
  value F00123_
     1 = "Outside building - visible place"
     2 = "Outside building - not visible place"
     3 = "Inside building - visible place"
     4 = "Inside building - not visible place"
     ;
  value F00124_
     0 = "No"
     1 = "Yes"
     ;
  value F00125_
    98 = "Don't know"
     ;
  value F00126_
    98 = "Don't know"
     ;
  value F00127_
    98 = "Don't know"
     ;
  value F00128_
    98 = "Don't know"
     ;
  value F00129_
    98 = "Don't know"
     ;
  value F00130_
    98 = "Don't know"
     ;
  value F00131_
    98 = "Don't know"
     ;
  value F00132_
    98 = "Don't know"
     ;
  value F00133_
    98 = "Don't know"
     ;
  value F00134_
    98 = "Don't know"
     ;
  value F00135_
    98 = "Don't know"
     ;
  value F00136_
    98 = "Don't know"
     ;
  value F00137_
     0 = "No"
     1 = "Guidelines observed"
     2 = "Reported, not seen"
     ;
  value F00138_
     0 = "No"
     1 = "Action plan observed"
     2 = "Reported, not seen"
     ;
  value F00139_
     0 = "No"
     1 = "Yes"
     2 = "Overnight observation - no routine inpatient"
     ;
  value F00140_
     0 = "No overnight observation"
   990 = "990 or more"
  9998 = "Don't know number of beds"
     ;
  value F00141_
     0 = "No"
     1 = "Yes, fixed fee covering all services"
     2 = "Yes, separate fees for separate items"
     ;
  value F00142_
     0 = "No"
     1 = "Yes"
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
     8 = "DK/no response/NA"
     ;
  value F00148_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00149_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00150_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00151_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00152_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00153_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00154_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00155_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00156_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00157_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00158_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00159_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00160_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00161_
     0 = "No"
     1 = "Yes"
     8 = "DK/no response/NA"
     ;
  value F00162_
     0 = "No fees posted"
     1 = "Yes, observed all fees"
     2 = "Yes, observed some fees"
     ;
  value F00163_
     0 = "No"
     1 = "Yes"
     ;
  value F00164_
     0 = "No"
     1 = "Yes"
     ;
  value F00165_
     0 = "No"
     1 = "Yes"
     ;
  value F00166_
     0 = "No"
     1 = "Yes"
     ;
  value F00167_
     0 = "No"
     1 = "Yes"
     ;
  value F00168_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00169_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00170_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00171_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00172_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00173_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00174_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00175_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00176_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00177_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00178_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00179_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00180_
     0 = "No ambulance, no driver, no access to other vehicle"
     4 = "Ambulance observed with fuel"
     5 = "Ambulance reported with  fuel"
     6 = "Ambulance obsd without/DK fuel"
     7 = "Ambulance reptd without/DK fuel"
     8 = "No ambulance, reptd access other vehicle"
     ;
  value F00181_
     0 = "No"
     1 = "Yes, protected waiting area"
     ;
  value F00182_
     0 = "No functioning facility"
     1 = "Yes"
     ;
  value F00183_
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
  value F00184_
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
  value F00185_
     0 = "No"
     1 = "Yes"
     ;
  value F00186_
     0 = "No"
     1 = "Yes"
     ;
  value F00187_
     0 = "No"
     1 = "Yes"
     ;
  value F00188_
     0 = "No"
     1 = "Yes"
     ;
  value F00189_
     0 = "No"
     1 = "Yes"
     ;
  value F00190_
     0 = "No"
     1 = "Yes"
     ;
  value F00191_
     0 = "No"
     1 = "Yes"
     ;
  value F00192_
     0 = "No"
     1 = "Yes"
     ;
  value F00193_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00194_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00195_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00196_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
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
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00199_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00200_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00201_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00202_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00203_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00204_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00205_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00206_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00207_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00208_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00209_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00210_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00211_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00212_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00213_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00214_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00215_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00216_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00217_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00218_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00219_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00220_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00221_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00222_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00223_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00224_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00225_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00226_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00227_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00228_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00229_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00230_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00231_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00232_
999999998 = "Don't know"
     ;
  value F00233_
999999998 = "Don't know"
     ;
  value F00234_
999999998 = "Don't know"
     ;
  value F00235_
999999998 = "Don't know"
     ;
  value F00236_
999999998 = "Don't know"
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
     1 = "Stretcher"
     2 = "Doko"
     3 = "Rickshaw/Bicycle"
     4 = "Auto mobile"
     5 = "Hand cart/wheel barrow"
     6 = "Animal driven cart/ Tanga"
     7 = "Hired ambulance"
    96 = "Other"
     0 = "None"
     ;
  value F00244_
     1 = "Yes, used HMIS 9.3"
     2 = "Yes, uses HMIS 9.4"
     3 = "Yes, uses HMIS 9.5"
     4 = "No, uses a seperate form"
     5 = "Do not report to HMIS"
     ;
  value F00245_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00246_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00247_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00248_
     1 = "Yes, updated fully"
     2 = "Yes, updated partially"
     3 = "Yes, not updated at all"
     4 = "Not available"
     5 = "Not used"
     ;
  value F00249_
999998 = "Don't know"
     ;
  value F00250_
999998 = "Don't know"
     ;
  value F00251_
999998 = "Don't know"
     ;
  value F00252_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00253_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00254_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00255_
     0 = "Not available"
     1 = "LHMIS person"
     2 = "Facility In-charge"
     3 = "Other service provider"
     ;
  value F00256_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00257_
  9998 = "Don't know"
     ;
  value F00258_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00259_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00260_
     0 = "Not available"
     8 = "Don't know"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00261_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00262_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00263_
     0 = "Process no equipment"
     1 = "Process inside facility only"
     2 = "Process both in and out of facility"
     3 = "Process outside facility only"
     ;
  value F00264_
     0 = "Do not sterilize"
     1 = "Sterilize, see table for method used"
     2 = "Process outside of facility"
     ;
  value F00265_
     1 = "Dry heat sterilization"
     2 = "Autoclave"
     3 = "Boil or steam"
     4 = "Boil"
     5 = "Steam without pressure"
     6 = "Chemical HLD or sterilization"
     7 = "Decontamination"
     8 = "Other"
     ;
  value F00266_
     0 = "No"
     1 = "Yes"
     ;
  value F00267_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00268_
   666 = "Automatic"
   998 = "Don't know"
     ;
  value F00269_
     1 = "Kg/sq cm"
     2 = "ATM pressure"
     3 = "Kilopascal (LB/N2)"
     4 = "Millimeter HG"
     5 = "LB/SQ IN"
     8 = "DK"
     ;
  value F00270_
   666 = "Automatic"
   995 = "Never used"
   998 = "Don't know"
     ;
  value F00271_
   666 = "Automatic"
   995 = "Never used"
   998 = "Don't know"
     ;
  value F00272_
     2 = "Chlorine (8/9 %)"
     3 = "Cidex"
     4 = "Betadine"
     5 = "Alcohol"
    10 = "Gluteraldehyde"
    13 = "Formaldehyde"
    98 = "Don't know"
     ;
  value F00273_
     0 = "No"
     1 = "Yes"
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
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00280_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00281_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00282_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00283_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00284_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00285_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00286_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00287_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00288_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00289_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported"
     ;
  value F00290_
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
    13 = "Burn and dump"
    95 = "Never have sharps waste"
    96 = "Other"
     ;
  value F00291_
     0 = "No waste visible"
     1 = "Waste visible, protected"
     2 = "Waste visible, unprotected"
     8 = "Waste site not inspected"
     ;
  value F00292_
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
    13 = "Burn and dump"
    95 = "Never have infectious waste"
    96 = "Other"
     ;
  value F00293_
     0 = "No waste visible"
     1 = "Waste visible, protected"
     2 = "Waste visible, unprotected"
     8 = "Waste site not inspected"
     ;
  value F00294_
     1 = "Observed functioning with fuel"
     2 = "Observed functioning, no or DK fuel"
     3 = "Observed, not or DK if functioning"
     4 = "Reported, functioning with fuel"
     5 = "Reported, function, no or DK fuel"
     6 = "Reported,  not or DK if functioning"
     ;
  value F00295_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported, not seen"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00313_
     0 = "No"
     1 = "Yes"
     ;
  value F00314_
     0 = "No"
     1 = "Yes"
     ;
  value F00315_
     0 = "No"
     1 = "Yes"
     ;
  value F00316_
     0 = "No"
     1 = "Yes"
     ;
  value F00317_
     0 = "No"
     1 = "Yes"
     ;
  value F00318_
     0 = "No"
     1 = "Yes"
     ;
  value F00319_
     0 = "No"
     1 = "Yes"
     ;
  value F00320_
     0 = "No"
     1 = "Yes"
     ;
  value F00321_
     0 = "No"
     1 = "Yes"
     ;
  value F00322_
     0 = "No"
     1 = "Yes"
     ;
  value F00323_
     0 = "No"
     1 = "Yes"
     ;
  value F00324_
     0 = "No"
     1 = "Yes"
     ;
  value F00325_
     0 = "No"
     1 = "Yes"
     ;
  value F00326_
     0 = "No"
     1 = "Yes"
     ;
  value F00327_
     0 = "No"
     1 = "Yes"
     ;
  value F00328_
     0 = "no"
     1 = "yes"
     ;
  value F00329_
     0 = "no"
     1 = "yes"
     ;
  value F00330_
     0 = "no"
     1 = "yes"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00334_
     0 = "No"
     1 = "Yes"
     ;
  value F00335_
     0 = "No"
     1 = "Yes"
     ;
  value F00336_
     0 = "No"
     1 = "Yes"
     ;
  value F00337_
     0 = "No"
     1 = "Yes"
     ;
  value F00338_
     0 = "No"
     1 = "Yes"
     ;
  value F00339_
     0 = "No"
     1 = "Yes"
     ;
  value F00340_
     0 = "No"
     1 = "Yes"
     ;
  value F00341_
     0 = "No"
     1 = "Yes"
     ;
  value F00342_
     0 = "No"
     1 = "Yes"
     ;
  value F00343_
     0 = "No"
     1 = "Yes"
     ;
  value F00344_
     0 = "No"
     1 = "Yes"
     ;
  value F00345_
     0 = "No"
     1 = "Yes"
     ;
  value F00346_
     0 = "No"
     1 = "Yes"
     ;
  value F00347_
     0 = "No"
     1 = "Yes"
     ;
  value F00348_
     0 = "No"
     1 = "Yes"
     ;
  value F00349_
     0 = "No"
     1 = "Yes"
     ;
  value F00350_
     0 = "No"
     1 = "Yes"
     ;
  value F00351_
     0 = "No"
     1 = "Yes"
     ;
  value F00352_
     0 = "No"
     1 = "Yes"
     ;
  value F00353_
     0 = "No"
     1 = "Yes"
     ;
  value F00354_
     0 = "No"
     1 = "Yes"
     ;
  value F00355_
     0 = "No"
     1 = "Yes"
     ;
  value F00356_
     0 = "No"
     1 = "Yes"
     ;
  value F00357_
     0 = "No"
     1 = "Yes"
     ;
  value F00358_
     0 = "No"
     1 = "Yes"
     ;
  value F00359_
     0 = "No"
     1 = "Yes"
     ;
  value F00360_
     0 = "No"
     1 = "Yes"
     ;
  value F00361_
     0 = "No"
     1 = "Yes"
     ;
  value F00362_
     0 = "No"
     1 = "Yes"
     ;
  value F00363_
     0 = "No"
     1 = "Yes"
     ;
  value F00364_
     0 = "No"
     1 = "Yes"
     ;
  value F00365_
     0 = "No"
     1 = "Yes"
     ;
  value F00366_
     0 = "No"
     1 = "Yes"
     ;
  value F00367_
     0 = "No"
     1 = "Yes"
     ;
  value F00368_
     0 = "No"
     1 = "Yes"
     ;
  value F00369_
     0 = "No"
     1 = "Yes"
     ;
  value F00370_
     1 = "Return to its source"
     2 = "Burning pit"
     3 = "Incinerator"
     4 = "Burning chamber with chimney"
     5 = "Dump"
     6 = "Remove offsite"
     ;
  value F00371_
     0 = "No"
     1 = "Yes"
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
  value F00375_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
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
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00379_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00380_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
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
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00386_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00387_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00388_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00389_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00390_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00391_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00392_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00393_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00394_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00395_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00396_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00397_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00398_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00399_
     0 = "No"
     1 = "Yes"
     ;
  value F00400_
     0 = "No"
     1 = "Yes"
     ;
  value F00401_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00402_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00403_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00404_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00405_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00406_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00407_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00408_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00409_
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
  value F00410_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00411_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00412_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00413_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00414_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00415_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00416_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00417_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00418_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00419_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00420_
     0 = "Not available"
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
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00430_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00431_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00432_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00433_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00434_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00435_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00436_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00437_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00438_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
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
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00445_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00446_
     0 = "No"
     1 = "Yes"
     ;
  value F00447_
     0 = "No"
     1 = "Yes"
     ;
  value F00448_
     0 = "No"
     1 = "Yes"
     ;
  value F00449_
     0 = "No"
     1 = "Yes"
     ;
  value F00450_
     0 = "No"
     1 = "Yes"
     ;
  value F00451_
     0 = "No"
     1 = "Yes"
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
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00455_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00456_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00457_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00458_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
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
     0 = "No"
     1 = "Yes, routinely stores vaccines"
     3 = "Receive all vaccines from higher center and stores for short time"
     ;
  value F00464_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00465_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00466_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00467_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00468_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00469_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00470_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00471_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00472_
     0 = "No"
     1 = "Yes"
     ;
  value F00473_
     0 = "None observed"
     1 = "Yes, observed"
     ;
  value F00474_
     1 = "Between +2 and +8 degrees"
     2 = "Above +8 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functional"
     5 = "Vaccine refrigerator not seen"
     6 = "No thermometer"
     ;
  value F00475_
     0 = "No"
     1 = "Yes, observed and completed twice daily for past 30 days"
     2 = "Yes, observed, missing whether completed twice daily in past 30 days"
     3 = "Yes, observed, not completed twice daily for past 30 days"
     4 = "Yes, reported"
     ;
  value F00476_
     0 = "None"
     1 = "One"
     2 = "Two or more"
     ;
  value F00477_
     0 = "Have none"
     1 = "Yes, one set"
     2 = "Yes, 2 or more sets"
     3 = "No, use purchased ice"
     ;
  value F00478_
     1 = "Icepack condition mentained"
     2 = "Not maintained"
     ;
  value F00479_
     0 = "Not/Never available"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available today/ No vaccination day"
     ;
  value F00480_
     0 = "No"
     1 = "Yes"
     ;
  value F00481_
     0 = "No"
     1 = "Yes"
     ;
  value F00482_
     0 = "Not/Never available"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available today"
     ;
  value F00483_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00484_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00485_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00486_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00487_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00488_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00489_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00490_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00491_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00492_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00493_
     0 = "No"
     3 = "Prescribe, counsel, refer"
     ;
  value F00494_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00495_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00496_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00497_
     0 = "No"
     1 = "Provide"
     3 = "Prescribe, counsel, refer"
     ;
  value F00498_
     0 = "No"
     1 = "Yes"
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
     0 = "No treatment"
     1 = "Routinely diagnose and treat STIs"
     2 = "Refers to other provider/location/outside facility"
     ;
  value F00503_
     0 = "No diagnosis/ treatment/ referral"
     1 = "Routinely diagnose and treat STIs"
     2 = "Diagnose but refer elsewhere for treatment"
     3 = "Refer elsewhere in facility for diagnosis and treatment"
     4 = "Refer outside facility for diagnosis and treatment"
     ;
  value F00504_
     0 = "No/not available"
     1 = "Yes, at least one observed"
     2 = "Yes, at least one reported"
     ;
  value F00505_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00506_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00507_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00508_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00509_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00510_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00511_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00512_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00513_
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
  value F00514_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00515_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00516_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00517_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00518_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
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
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00531_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00532_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00533_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00534_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00535_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00536_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00537_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00538_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00539_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00540_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00541_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00542_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00543_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00544_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
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
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00549_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00550_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00551_
     0 = "No FP equipment processed in facility"
     1 = "FP service site"
     2 = "Central location in facility"
     3 = "Both FP and central locations"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00556_
     0 = "No"
     1 = "Yes"
     ;
  value F00557_
     0 = "No"
     1 = "Yes"
     ;
  value F00558_
     0 = "No HIV RTD done in FP service area"
     1 = "HIV RTD kit observed & valid"
     3 = "HIV RTD kit observed, none valid"
     4 = "HIV RTD kit reported available"
     5 = "HIV RTD kit not available today"
     6 = "HIV RTD kit used, missing valid"
     ;
  value F00559_
     0 = "No"
     1 = "Yes in family planning service"
     2 = "Yes in pharmacy"
     ;
  value F00560_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F00561_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
     ;
  value F00562_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00563_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00564_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00565_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00566_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00567_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00568_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00569_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00570_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00571_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00572_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
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
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00578_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00579_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00580_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F00581_
     0 = "No"
     1 = "Yes"
     ;
  value F00582_
     0 = "No"
     1 = "Yes"
     ;
  value F00583_
     0 = "No"
     1 = "Yes"
     ;
  value F00584_
     0 = "No"
     1 = "Yes"
     ;
  value F00585_
     0 = "No"
     1 = "Yes"
     ;
  value F00586_
     0 = "No"
     1 = "Yes"
     ;
  value F00587_
     1 = "Within prior 4 full weeks"
     2 = "Between 4-12 weeks"
     3 = "More than 12 weeks ago"
     4 = "No routine supply system"
     8 = "Don't know"
     ;
  value F00588_
     1 = "Determines own need and more"
     2 = "Need determined elsewhere"
     3 = "Both (differ by commodity)"
     8 = "Don't know"
     ;
  value F00589_
     1 = "Order to maintain fixed stock"
     2 = "Order same amount"
     3 = "Order based on the utilization"
     6 = "Other"
     8 = "Don't know"
     ;
  value F00590_
     1 = "Less than 2 weeks"
     2 = "Between 2 weeks and 1 month"
     3 = "Between 1 and 2 months"
     4 = "Between 2 and 4 months"
     5 = "Between 4 and 6 months"
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
     ;
  value F00593_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00594_
     0 = "No"
     1 = "Yes"
     ;
  value F00595_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00596_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00597_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00598_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00599_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00600_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00601_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00602_
     0 = "Not routinely conducted"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00603_
     0 = "No"
     1 = "Yes"
     ;
  value F00604_
     0 = "No"
     1 = "Yes"
     ;
  value F00605_
     0 = "No"
     1 = "Yes"
     ;
  value F00606_
     0 = "No"
     1 = "Yes"
     ;
  value F00607_
     0 = "No"
     1 = "Yes"
     ;
  value F00608_
     0 = "No"
     1 = "Yes"
     ;
  value F00609_
     0 = "No"
     1 = "Yes"
     ;
  value F00610_
     0 = "No"
     1 = "Yes"
     ;
  value F00611_
     0 = "No  (See V410A)"
     1 = "Routinely diagnose and treats"
     2 = "Refer other provider/service/facility"
     ;
  value F00612_
     0 = "No diagnosis, treatment, referral"
     1 = "Routinely diagnose and treat in service area"
     2 = "Diagnose, but refer elsewhere for treatment"
     3 = "Refer elsewhere in facility for diagnosis and treatment"
     4 = "Refer outside facility for diagnosis and treatment"
     ;
  value F00613_
     0 = "Not all days or never offered"
     1 = "Yes"
     ;
  value F00614_
     0 = "Less often than 1 day month"
     ;
  value F00615_
     0 = "No/not available"
     1 = "Yes, at least one observed"
     2 = "Yes, at least one reported"
     ;
  value F00616_
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
  value F00617_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00618_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00619_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00620_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00621_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00622_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00623_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00624_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00625_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00626_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00627_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00628_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
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
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00634_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00635_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00636_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00637_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00638_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00639_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00640_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00641_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00642_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00643_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00644_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00645_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
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
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00651_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00652_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00653_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00654_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00655_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00656_
     0 = "No"
     1 = "Yes"
     ;
  value F00657_
     0 = "No"
     1 = "Yes"
     ;
  value F00658_
     0 = "No"
     1 = "Yes"
     ;
  value F00659_
     0 = "No"
     1 = "Yes"
     ;
  value F00660_
     0 = "No"
     1 = "Yes"
     ;
  value F00661_
     0 = "No"
     1 = "Yes"
     ;
  value F00662_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00663_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00664_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00665_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00666_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00667_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00668_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00669_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     6 = "Available elsewhere in facility"
     ;
  value F00670_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00671_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00672_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00673_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00674_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00675_
     0 = "No diagnosis, treatment, referral"
     1 = "Routinely diagnose and treat in service area"
     2 = "Diagnose, but refer elsewhere for treatment"
     3 = "Refer elsewhere in facility for diagnosis and treatment"
     4 = "Refer outside facility for diagnosis and treatment"
     ;
  value F00676_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00677_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00678_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00679_
   998 = "Don't know"
     ;
  value F00680_
     0 = "No"
     1 = "Yes"
     ;
  value F00681_
     0 = "No schedule"
     1 = "Schedule observed"
     3 = "Schedule reported, not seen"
     ;
  value F00682_
     0 = "No"
     1 = "Yes"
     ;
  value F00683_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00684_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00685_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00686_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00687_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00688_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00689_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00690_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00691_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00692_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00693_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00694_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00695_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00696_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00697_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00698_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00699_
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
  value F00700_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00701_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00702_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00703_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00704_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00705_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00706_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00707_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00708_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00709_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00710_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00711_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00712_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00713_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00714_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00715_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00716_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00717_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00718_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00719_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00720_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00721_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00722_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00723_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00724_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00725_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00726_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00727_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00728_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00729_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00730_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00731_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00732_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00733_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00734_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00735_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00736_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00737_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00738_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00739_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00740_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00741_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00742_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
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
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00746_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00747_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00748_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00749_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00750_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00751_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00752_
     0 = "Not available"
     1 = "Observed/functioning"
     2 = "Observed/missing functioning"
     3 = "Observed/not/DK functioning"
     4 = "Reported/functioning"
     5 = "Reported/missing functioning"
     6 = "Reported/not/DK functioning"
     ;
  value F00753_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00754_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00755_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00756_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00757_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00758_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00759_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00760_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00761_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00762_
     0 = "Does not review"
     1 = "Reviews both"
     2 = "Reviews maternal only"
     3 = "Reviews newborns only"
     ;
  value F00763_
    53 = "When case occurs"
    98 = "Don't know"
     ;
  value F00764_
    53 = "When case occurs"
    95 = "Always with Maternal Reviews"
    98 = "Don't know"
     ;
  value F00765_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00766_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00767_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00768_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00769_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00770_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00771_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00772_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00773_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00774_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00775_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00776_
     0 = "Never provided"
     1 = "Provided in past 3 months"
     2 = "Ever provided, not in past 3 months"
     3 = "Ever provided, DK/missing if past 3 months"
     8 = "Don't know if ever provided"
     ;
  value F00777_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00778_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00779_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00780_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00781_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00782_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00783_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00784_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00785_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00786_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F00787_
     0 = "No"
     1 = "Yes, observed schedule"
     3 = "Yes, reported"
     5 = "24-hour schedule not  maintained"
     ;
  value F00788_
     0 = "No"
     1 = "Yes, observed schedule"
     3 = "Yes, reported"
     5 = "24-hour schedule not  maintained"
     ;
  value F00789_
     0 = "Not used"
     1 = "Yes, in separate room"
     2 = "Yes, integrated"
     ;
  value F00790_
     0 = "Not used"
     1 = "Used routinely"
     2 = "Used selectively"
     ;
  value F00791_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     8 = "Don't know/never available"
     ;
  value F00792_
     0 = "No"
     1 = "Yes  (See V1280+)"
     ;
  value F00793_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00794_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00795_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00796_
     1 = "Within 72 hours"
     2 = "After 72 hours"
     3 = "Varies from case to case"
     8 = "Don't know"
     ;
  value F00797_
     0 = "No system"
     1 = "Less than 2 weeks"
     2 = "Between 2 weeks and 1 month"
     3 = "Between 1 and 2 months"
     4 = "Between 2 and 4 months"
     5 = "Between 4 and 6 months"
     ;
  value F00798_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00799_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00800_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00801_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00802_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00803_
     0 = "Neither diagnosed nor treated"
     1 = "Diagnosed and treated"
     2 = "Diagnosed only"
     3 = "Treated only"
     ;
  value F00804_
     0 = "No"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00805_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00806_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00807_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00808_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00809_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00810_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00811_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00812_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00813_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00814_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00815_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00816_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00817_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00818_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     8 = "Don't know"
     ;
  value F00819_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00820_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00821_
     0 = "No"
     1 = "Yes, routinely"
     2 = "Only if suspected HIV"
     ;
  value F00822_
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
  value F00823_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F00824_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00825_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00826_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00827_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00828_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00829_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00830_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00831_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00832_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00833_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00834_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00835_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00836_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00837_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00838_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F00839_
     0 = "No"
     1 = "Yes"
     ;
  value F00840_
     0 = "No"
     1 = "Yes"
     ;
  value F00841_
     0 = "No"
     1 = "Yes"
     ;
  value F00842_
     0 = "No"
     1 = "Yes"
     ;
  value F00843_
     0 = "No"
     1 = "Yes"
     ;
  value F00844_
     0 = "No"
     1 = "Yes"
     ;
  value F00845_
     0 = "No"
     1 = "Yes, always or sometimes active"
     2 = "Yes, only passive"
     8 = "Don't know"
     ;
  value F00846_
     0 = "No"
     1 = "Yes, always active"
     2 = "Yes, sometimes active"
     3 = "Yes, only passive"
     8 = "Don't know"
     ;
  value F00847_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     8 = "Don't know/never available"
     ;
  value F00848_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00849_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00850_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00851_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00852_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00853_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00854_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00855_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00856_
     0 = "No"
     1 = "Yes"
     ;
  value F00857_
     0 = "No"
     1 = "Yes"
     ;
  value F00858_
     0 = "No"
     1 = "Yes"
     ;
  value F00859_
     0 = "No"
     1 = "Yes"
     ;
  value F00860_
     0 = "No"
     1 = "Yes, no external QA"
     ;
  value F00861_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00862_
    95 = "No error rate provided"
    98 = "Don't know"
     ;
  value F00863_
     0 = "No"
     1 = "Yes"
     ;
  value F00864_
     0 = "No"
     1 = "Yes"
     ;
  value F00865_
     0 = "No"
     1 = "Yes"
     ;
  value F00866_
     0 = "No"
     1 = "Yes"
     ;
  value F00867_
     0 = "No"
     1 = "Yes"
     ;
  value F00868_
     0 = "No"
     1 = "Yes"
     ;
  value F00869_
     0 = "No"
     1 = "Yes"
     ;
  value F00870_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00871_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F00872_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00873_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00874_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00875_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00876_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00877_
     0 = "No, send blood out"
     1 = "Yes, send no blood out for screening"
     ;
  value F00878_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00879_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00880_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00881_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00882_
     0 = "No, send blood/urine out"
     1 = "Yes, send no blood/urine out"
     ;
  value F00883_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00884_
     0 = "No record"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F00885_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00886_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00887_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00888_
     0 = "No"
     1 = "Yes, record of test results observed"
     2 = "Yes, record of test results missing"
     3 = "Yes, no record of test results observed"
     ;
  value F00889_
     0 = "No, send blood/urine out"
     1 = "Yes, send no blood/urine out"
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
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     8 = "Don't know/never available"
     ;
  value F00894_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00895_
     0 = "No"
     1 = "Yes"
     ;
  value F00896_
     0 = "No"
     1 = "Yes"
     ;
  value F00897_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
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
     0 = "No"
     1 = "Yes"
     ;
  value F00904_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00905_
     0 = "No"
     1 = "Yes"
     ;
  value F00906_
     0 = "No"
     1 = "Yes"
     ;
  value F00907_
     0 = "No"
     1 = "Yes"
     ;
  value F00908_
     0 = "No"
     1 = "Yes"
     ;
  value F00909_
     0 = "No screening tests done"
     1 = "Yes, screened in facility"
     2 = "Blood screened outside facility"
     3 = "Yes, blood screened both inside and outside"
     ;
  value F00910_
     0 = "No"
     1 = "Yes"
     ;
  value F00911_
     0 = "No"
     1 = "Yes"
     ;
  value F00912_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F00913_
     0 = "No"
     1 = "Yes, no HIV tests (lab, RDT, DBS)"
     ;
  value F00914_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00915_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00916_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00917_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00918_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00919_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00920_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00921_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00922_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00923_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00924_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00925_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00926_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00927_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00928_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00929_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00930_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00931_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00932_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00933_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00934_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00935_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00936_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00937_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00938_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00939_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00940_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00941_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00942_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00943_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00944_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00945_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00946_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00947_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00948_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00949_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00950_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00951_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00952_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00953_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00954_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00955_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00956_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00957_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00958_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00959_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00960_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00961_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00962_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00963_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00964_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00965_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00966_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00967_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00968_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00969_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00970_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00971_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00972_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00973_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00974_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00975_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00976_
     0 = "Not used"
     1 = "Equipment used and observed valid"
     3 = "Equipment used & observed, none valid"
     4 = "Equipment used reported, not seen"
     5 = "Equipment used, not available today"
     6 = "Equipment used, missing if items available"
     ;
  value F00977_
     0 = "Not used"
     1 = "Equipment used and observed valid"
     3 = "Equipment used & observed, none valid"
     4 = "Equipment used reported, not seen"
     5 = "Equipment used, not available today"
     6 = "Equipment used, missing if items available"
     ;
  value F00978_
     0 = "Not used"
     1 = "Equipment used and observed valid"
     3 = "Equipment used & observed, none valid"
     4 = "Equipment used reported, not seen"
     5 = "Equipment used, not available today"
     6 = "Equipment used, missing if items available"
     ;
  value F00979_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00980_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00981_
     0 = "Not used"
     1 = "Yes, See V840G1"
     ;
  value F00982_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00983_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00984_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00985_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00986_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00987_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00988_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00989_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00990_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00991_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00992_
     0 = "Not used"
     1 = "Test used"
     ;
  value F00993_
     0 = "Not used"
     1 = "Yes, see V840P"
     ;
  value F00994_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00995_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00996_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00997_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00998_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F00999_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01000_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F01001_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
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
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F01008_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F01009_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F01010_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F01011_
     0 = "Never"
     1 = "Always"
     2 = "Sometimes"
     3 = "Rarely"
     ;
  value F01012_
     0 = "No"
     1 = "Yes"
     ;
  value F01013_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F01014_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F01015_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F01016_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F01017_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F01018_
     0 = "Not used"
     1 = "Equipment used and observed"
     2 = "Equipment used reported, not seen"
     3 = "Equipment used, not available today"
     4 = "Equipment used, missing if items available"
     ;
  value F01019_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01020_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01021_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01022_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01023_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01024_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01025_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01026_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01027_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01028_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01029_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01030_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01031_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01032_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01033_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01034_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01035_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01036_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01037_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01038_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01039_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01040_
     0 = "No"
     1 = "Yes"
     ;
  value F01041_
     0 = "No"
     1 = "Yes"
     ;
  value F01042_
     0 = "No"
     1 = "Yes"
     ;
  value F01043_
     0 = "No"
     1 = "Yes"
     ;
  value F01044_
     0 = "No"
     1 = "Yes"
     ;
  value F01045_
     0 = "No"
     1 = "Yes"
     ;
  value F01046_
     0 = "No"
     1 = "Yes"
     ;
  value F01047_
     0 = "No"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01048_
     0 = "No"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "None available today"
     ;
  value F01049_
     0 = "No system"
     1 = "Internal QC only"
     2 = "External QC only"
     3 = "Both internal and external QC"
     4 = "Send slide for re-reading"
     6 = "Other"
     7 = "Type of system missing"
     ;
  value F01050_
     0 = "No records"
     1 = "Records for IQC only"
     2 = "Records for EQC only"
     3 = "Records for both internal and external QC"
     4 = "Type of records missing"
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
     0 = "No"
     1 = "Yes"
     ;
  value F01056_
     0 = "No"
     1 = "Yes"
     ;
  value F01057_
     0 = "No"
     1 = "Yes"
     ;
  value F01058_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01059_
     1 = "Between +2 and +6 degrees"
     2 = "Above + 6 degrees"
     3 = "Below +2 degrees"
     4 = "Thermometer not functioning"
     5 = "Thermometer not available"
     ;
  value F01060_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01061_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01062_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F01063_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F01064_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported not seen"
     ;
  value F01065_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01066_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01067_
     1 = "Mobile"
     2 = "Static"
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
     1 = "External quality assurance(EQUAS)"
     2 = "Early infant diagnosis(EID)"
     3 = "Both"
     8 = "Don't know"
     ;
  value F01074_
     0 = "No"
     1 = "Yes"
     ;
  value F01075_
     0 = "Not/Never available"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Not available today"
     ;
  value F01076_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01077_
     0 = "No"
     1 = "Yes"
     ;
  value F01078_
     0 = "No"
     1 = "Yes"
     ;
  value F01079_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01080_
     1 = "MOBILE"
     2 = "STATIC"
     ;
  value F01081_
     0 = "No record"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01082_
     0 = "No"
     1 = "Yes"
     ;
  value F01083_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01084_
     0 = "No"
     1 = "Yes"
     ;
  value F01085_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01086_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F01087_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
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
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01096_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01097_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01098_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01099_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01100_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01101_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01102_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01103_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01104_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01105_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01106_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01107_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01108_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01109_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01110_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01111_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01112_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01113_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01114_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01115_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01116_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01117_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01118_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01119_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01120_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01121_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01122_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01123_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01124_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01125_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01126_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01127_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01128_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01129_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01130_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01131_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01132_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01133_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01134_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01135_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01136_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01137_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01138_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01139_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01140_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01141_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01142_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01143_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01144_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01145_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01146_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01147_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01148_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01149_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01150_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01151_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01152_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01153_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01154_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01155_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01156_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01157_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01158_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01159_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01160_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01161_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01162_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01163_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
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
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01178_
     0 = "Never available"
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
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01181_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01182_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01183_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01184_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01185_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01186_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01187_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01188_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01189_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01190_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01191_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01192_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01193_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01194_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
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
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01209_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01210_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01211_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01212_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01213_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01214_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01215_
     0 = "No"
     1 = "Yes"
     ;
  value F01216_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F01217_
     0 = "No"
     1 = "Yes"
     ;
  value F01218_
     0 = "No"
     1 = "Yes"
     ;
  value F01219_
     0 = "No"
     1 = "Yes"
     ;
  value F01220_
     0 = "No"
     1 = "Yes"
     ;
  value F01221_
     0 = "No"
     1 = "Yes"
     ;
  value F01222_
     0 = "No"
     1 = "Yes, type of client not defined"
     ;
  value F01223_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01224_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01225_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01226_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01227_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01228_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01229_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01230_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01231_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01232_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F01233_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
     ;
  value F01234_
     0 = "No"
     1 = "Yes"
     ;
  value F01235_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F01236_
     0 = "No"
     1 = "Yes"
     ;
  value F01237_
     0 = "No"
     1 = "Yes"
     ;
  value F01238_
     0 = "No"
     1 = "Yes"
     ;
  value F01239_
     0 = "No"
     1 = "Yes"
     ;
  value F01240_
     0 = "No"
     1 = "Yes"
     ;
  value F01241_
     0 = "No"
     1 = "Yes"
     ;
  value F01242_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01243_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01244_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01245_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01246_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01247_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01248_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01249_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01250_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01251_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01252_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01253_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01254_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01255_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01256_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01257_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01258_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01259_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01260_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01261_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01262_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01263_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01264_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01265_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01266_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01267_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01268_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01269_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01270_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01271_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01272_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01273_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01274_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01275_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01276_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01277_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01278_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01279_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01280_
     1 = "Computer system"
     2 = "Ledger/stock card system"
     6 = "Other system"
     ;
  value F01281_
     0 = "No"
     2 = "Observed, updated daily"
     3 = "Observed, daily register of distributed items observed"
     6 = "Other system"
     ;
  value F01282_
     0 = "No"
     1 = "Yes"
     ;
  value F01283_
     0 = "No"
     1 = "Yes, all"
     2 = "Yes, only some"
     ;
  value F01284_
     0 = "No"
     1 = "Yes"
     ;
  value F01285_
     0 = "No"
     1 = "Yes"
     ;
  value F01286_
     0 = "No"
     1 = "Yes"
     ;
  value F01287_
     0 = "No"
     1 = "Yes"
     ;
  value F01288_
     0 = "No"
     1 = "Yes"
     ;
  value F01289_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01290_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01291_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01292_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01293_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01294_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01295_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01296_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01297_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01298_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01299_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01300_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01301_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01302_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01303_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01304_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01305_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01306_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01307_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01308_
     0 = "Never available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01309_
     0 = "Neither diagnose nor prescribe tx"
     1 = "Diagnose and prescribe tx"
     2 = "Diagnose only"
     3 = "Prescribe tx only"
     ;
  value F01310_
     0 = "No"
     1 = "Always"
     2 = "Only sometimes"
     ;
  value F01311_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01312_
     0 = "No"
     1 = "Yes"
     ;
  value F01313_
     0 = "No"
     1 = "Yes"
     ;
  value F01314_
     0 = "No"
     1 = "Yes"
     ;
  value F01315_
     0 = "No"
     1 = "Yes"
     ;
  value F01316_
     0 = "No"
     1 = "Yes"
     ;
  value F01317_
     0 = "No"
     1 = "Yes"
     ;
  value F01318_
     0 = "No"
     1 = "Yes"
     ;
  value F01319_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01320_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01321_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01322_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01323_
     1 = "Diagnose, refer and treat"
     2 = "Diagnose and refer"
     3 = "Diagnose and treat"
     4 = "Refer and treat"
     5 = "Diagnose only"
     6 = "Refer only"
     7 = "Treat only"
     ;
  value F01324_
     0 = "No TB diagnosis services"
     1 = "Sputum smear only"
     2 = "X-ray only"
     3 = "Either sputum or x-ray"
     4 = "Both sputum and x-ray"
     5 = "Clinical symptoms only"
     6 = "Refer to outside facility"
     7 = "GENE XPERT"
     8 = "ALL 3: SPUTUM + XRAY + GENE XPERT"
     ;
  value F01325_
     0 = "No"
     1 = "Yes"
     ;
  value F01326_
     0 = "No record"
     1 = "Yes, observed paper and/or electronic"
     2 = "Yes, reported, not seen"
     ;
  value F01327_
     1 = "Paper"
     2 = "Computer"
     4 = "Both paper and computer"
     ;
  value F01328_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01329_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01330_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01331_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01332_
     0 = "No"
     1 = "2M intensive phase, 4M continuation phase"
     2 = "3M intensive phase, 4M continuation phase"
     6 = "Other"
     ;
  value F01333_
     0 = "No treatment or prescription"
     1 = "Follow up clients only after first 2M intensive direct observation elsewhere"
     2 = "Diagnose and treat while inpatient. Discharge to other clinic/unit for F/up"
     3 = "Provide full tx with no routine direct obs phase"
     4 = "Dx, prescribe/provide meds, no F/U"
     5 = "Dx only, no tx or prescription of medications"
     7 = "Provide full treatment, with routine direct observation phase"
     6 = "Other"
     ;
  value F01334_
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
  value F01335_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01336_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01337_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01338_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01339_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01340_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01341_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01342_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01343_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01344_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01345_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01346_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01347_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01348_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01349_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01350_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01351_
     0 = "No system"
     1 = "System or register observed"
     2 = "System or register reported, not seen"
     ;
  value F01352_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     8 = "Don't know/never available"
     ;
  value F01353_
     0 = "Not available"
     1 = "Observed"
     3 = "Reported, not seen"
     4 = "Not available today"
     5 = "Have sputum container, missing if observed"
     ;
  value F01354_
     0 = "No TB meds in facility"
     1 = "Stored in service site"
     2 = "Stored in pharmacy"
     ;
  value F01355_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01356_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
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
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01361_
     1 = "Separate PMTCT services only"
     2 = "Separate PMTCT within ANC only"
     3 = "Separate PMTCT and PMTCT in delivery services"
     6 = "Separate PMTCT within ANC and delivery services"
     7 = "PMTCT only in delivery services"
     8 = "No HIV counseling/test, other PMTCT services"
     ;
  value F01362_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01363_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01364_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01365_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01366_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01367_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01368_
     0 = "No service or referral"
     1 = "Service offered in this facility"
     ;
  value F01369_
     0 = "No service or referral"
     1 = "Service offered in this facility"
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
     0 = "No"
     1 = "Yes, no guidelines"
     ;
  value F01374_
     0 = "No delivery services"
     1 = "Yes, delivery services with PMTCT"
     3 = "Delivery services without PMTCT services"
     ;
  value F01375_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01376_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01377_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01378_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01379_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01380_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01381_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01382_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01383_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01384_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01385_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01386_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01387_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01388_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01389_
     0 = "No"
     1 = "Yes, none available"
     ;
  value F01390_
     0 = "Not available in PMTCT site"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     8 = "Don't know/never available"
     ;
  value F01391_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01392_
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
  value F01393_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01394_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01395_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01396_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01397_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01398_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01399_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01400_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01401_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01402_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01403_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01404_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01405_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01406_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01407_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01408_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01409_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01410_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01411_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01412_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01413_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01414_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01415_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01416_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01417_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01418_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01419_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01420_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01421_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01422_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01423_
     0 = "No"
     1 = "Yes"
     ;
  value F01424_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     8 = "Don't know/never available"
     ;
  value F01425_
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
  value F01426_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01427_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01428_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01429_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01430_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01431_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01432_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01433_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01434_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01435_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01436_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01437_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01438_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01439_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01440_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01441_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01442_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01443_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01444_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01445_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01446_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01447_
     0 = "No"
     1 = "Yes"
     ;
  value F01448_
     0 = "No"
     1 = "Yes"
     ;
  value F01449_
     0 = "No"
     1 = "Yes"
     ;
  value F01450_
     0 = "No"
     1 = "Yes"
     ;
  value F01451_
     0 = "No"
     1 = "Yes"
     ;
  value F01452_
     0 = "No"
     1 = "Yes"
     ;
  value F01453_
     0 = "No"
     1 = "Yes"
     ;
  value F01454_
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
  value F01455_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01456_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01457_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01458_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01459_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01460_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01461_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01462_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01463_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01464_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01465_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01466_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01467_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01468_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01469_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01470_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01471_
     0 = "No guidelines or protocols"
     1 = "Yes"
     ;
  value F01472_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01473_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01474_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "National protocols observed"
     ;
  value F01475_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01476_
     0 = "No HIV RDT done in HIV testing area"
     2 = "HIV RDT kit observed & valid"
     3 = "HIV RDT kit observed, none valid"
     4 = "HIV RDT kit reported available"
     5 = "HIV RDT kit not available today"
     8 = "Don't know/never available"
     ;
  value F01477_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     8 = "Don't know"
     ;
  value F01478_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01479_
     0 = "No agreement"
     1 = "Observed"
     2 = "Reported, not seen"
     3 = "Verbal agreement only"
     ;
  value F01480_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01481_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01482_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01483_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01484_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01485_
     0 = "No ART services"
     1 = "Yes, both prescribe and provide follow-up"
     2 = "Yes, prescribe ART"
     3 = "Yes, provide follow-up services"
     4 = "Outreach site for providers from other facilities (no further data)"
     ;
  value F01486_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01487_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01488_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01489_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01490_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01491_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01492_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01493_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01494_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01495_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01496_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01497_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01498_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01499_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01500_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01501_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01502_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01503_
     0 = "No ART meds in facility"
     1 = "Stored in ART service site"
     2 = "Stored in pharmacy"
     3 = "Stored in PMTCT service area"
     4 = "Stored in ART and PMTCT service areas"
     ;
  value F01504_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01505_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01506_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01507_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01508_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01509_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01510_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01511_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01512_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01513_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01514_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01515_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01516_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01517_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01518_
     0 = "Never"
     1 = "Routinely"
     2 = "Selectively"
     8 = "Don't know"
     ;
  value F01519_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01520_
     0 = "Never provide these services or refer clients with HIV/AIDS for services"
     1 = "Yes, facility provides"
     ;
  value F01521_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01522_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01523_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01524_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01525_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01526_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01527_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01528_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01529_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01530_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01531_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01532_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01533_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported available, not seen"
     ;
  value F01534_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01535_
     0 = "No system"
     1 = "System/ register observed"
     2 = "System/register reported, not seen"
     ;
  value F01536_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01537_
     0 = "Never offer service"
     1 = "Facility provides service"
     8 = "Don't know"
     ;
  value F01538_
     0 = "No"
     1 = "Yes"
     ;
  value F01539_
     0 = "No"
     1 = "Diagnose and treat"
     2 = "Diagnose only"
     3 = "Treat only"
     ;
  value F01540_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01541_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01542_
     0 = "No"
     1 = "Yes"
     ;
  value F01543_
     0 = "No"
     1 = "Diagnose and treat"
     2 = "Diagnose only"
     3 = "Treat only"
     ;
  value F01544_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01545_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01546_
     0 = "No"
     1 = "Yes"
     ;
  value F01547_
     0 = "No"
     1 = "Diagnose and treat"
     2 = "Diagnose only"
     3 = "Treat only"
     ;
  value F01548_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01549_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, available, not seen"
     ;
  value F01550_
     1 = "Collected in General information"
     2 = "Collected in N-C diseases service site"
     ;
  value F01551_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01552_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01553_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01554_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01555_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01556_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01557_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01558_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01559_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01560_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01561_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01562_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01563_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01564_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01565_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01566_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01567_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01568_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01569_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01570_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01571_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01572_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01573_
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
  value F01574_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01575_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01576_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01577_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01578_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01579_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01580_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01581_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01582_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01583_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01584_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01585_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01586_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01587_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01588_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01589_
     0 = "Not available"
     1 = "Yes, observed"
     2 = "Yes, reported"
     ;
  value F01590_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01591_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01592_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01593_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01594_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01595_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01596_
     0 = "Not available"
     1 = "Observed, functioning"
     2 = "Observed, missing functioning"
     3 = "Observed, not/DK if functioning"
     4 = "Reported functioning"
     5 = "Reported, missing functioning"
     6 = "Reported, not/DK  if functioning"
     ;
  value F01597_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01598_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01599_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01600_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01601_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01602_
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
  value F01603_
     0 = "No privacy"
     1 = "Private room, visual and auditory privacy"
     2 = "Other room, visual and auditory privacy"
     4 = "Visual privacy only"
     ;
  value F01604_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01605_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01606_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01607_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01608_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01609_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01610_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01611_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01612_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01613_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01614_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01615_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01616_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01617_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01618_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01619_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01620_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01621_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01622_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01623_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01624_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01625_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
  value F01626_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01627_
     0 = "Not available"
     1 = "Observed"
     2 = "Reported, not seen"
     ;
  value F01628_
     0 = "Never available"
     2 = "Yes, observed, at least 1 valid"
     3 = "Yes, observed, none valid"
     4 = "Yes, reported available, not seen"
     5 = "Not available today"
     ;
Data user.NPFC71FLSR;
  attrib INV_ID   label="INVENTORY ID";
  attrib V000     length=$3 label="Survey phase";
  attrib V001     format=F00001_. label="Sub Region (Country-specific)";
  attrib V002     format=F00002_. label="District (Country-specific)";
  attrib V003     format=F00003_. label="NA - Urban/rural";
  attrib V004     label="Facility number";
  attrib V005     label="Sample weight";
  attrib V007     format=F00004_. label="Facility type (Country-specific)";
  attrib V008     format=F00005_. label="Managing authority (Country-specific)";
  attrib V009     label="Interviewer";
  attrib V010     format=F00006_. label="Type of SPA";
  attrib V010A    format=F00007_. label="Result of survey";
  attrib V010B    format=F00008_. label="NA - Outpatient or only inpatient";
  attrib V010C    label="Number of visits";
  attrib V012B    format=F00009_. label="Section 10: Child Immunization services present and completed - See V200s";
  attrib V012C    format=F00010_. label="Section 12: Sick children services present and completed - See V200s";
  attrib V012D    format=F00011_. label="Section 11: Growth monitoring services present and completed - See V200s";
  attrib V013     format=F00012_. label="Section 13: Family planning services present and completed See V300s";
  attrib V013S    format=F00013_. label="Section 13: Sterilizations performed See V300s";
  attrib V014A    format=F00014_. label="Section 14: Antenatal services present and completed - See V400s";
  attrib V015A    format=F00015_. label="Section 16: Normal delivery and newborn care present and completed - See V500s";
  attrib V015C    format=F00016_. label="Section 25: C-section delivery services present and completed - See V500s";
  attrib V016     format=F00017_. label="Section 18: STI Services present and completed - See V600s";
  attrib V018     format=F00018_. label="Section 10: Immunization Logistics Data present and completed - See LV200s";
  attrib V019     format=F00019_. label="Section 9.2 Contraceptive Logistics Data present and completed - See LV300s";
  attrib V019A    format=F00020_. label="Storage of contraceptives";
  attrib V033     format=F00021_. label="Section 4: HMIS data present and completed See V130-V135s";
  attrib V034     format=F00022_. label="Section 8: Laboratory data present and completed - See V800s";
  attrib V034A    format=F00023_. label="Section 26: Blood typing services present and completed -  - See V860s";
  attrib V034B    format=F00024_. label="Section 27: Blood transfusion data present and completed -See V834s, V838s, V861s, V896-899";
  attrib V034C    format=F00025_. label="Section 17A: Diagnosis or treatment of Kalaazar/ Leishmaniasis - CS";
  attrib V034D    format=F00026_. label="Section 17B: Management of snake bite - CS";
  attrib V034E    format=F00027_. label="Section 17B: Management of dog bite/ Rabies - CS";
  attrib V035     format=F00028_. label="Section 9: Pharmacy data for drugs/ supplies present present and completed - See V900s";
  attrib V035A    format=F00029_. label="Storage of anti-TB medications";
  attrib V035B    format=F00030_. label="Storage of anti-retroviral medications";
  attrib V042     format=F00031_. label="Section 20: HIV testing data present and completed - See V1300s";
  attrib V043     format=F00032_. label="Section 19: TB data present and completed - See V1100s";
  attrib V044     format=F00033_. label="Section 21: HIV treatment: ART data present and completed - See V1400s";
  attrib V045     format=F00034_. label="Section 15: PMTCT of HIV infection services present and completed - See V1200s";
  attrib V045A    format=F00035_. label="Section 15: PMTCT of HIV infection services integrated with delivery services present and completed - See V1280s";
  attrib V046     format=F00036_. label="Section 22: HIV care and support data present and completed - See V1500s";
  attrib V047     format=F00037_. label="Section 17: Malaria present and completed - See V1000s";
  attrib V048     format=F00038_. label="Section 23: Non-communicable diseases data present and completed - See V1600s";
  attrib V049     format=F00039_. label="Section 24: Minor surgical services data present and completed - See V1700s";
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
  attrib V074     label="Number of Postpartum clients completed exit - CS";
  attrib V073A    label="Number of Family Planning clients completed observation";
  attrib V073C    label="Number of Family Planning clients attempted interview";
  attrib V074C    label="Number of Postpartum clients attempted interview - CS";
  attrib V076     label="Total ANC clients at facility";
  attrib V077     label="Total Sick child clients at facility";
  attrib V078     label="Total Family planning clients at facility";
  attrib V079     label="Total Postpartum clients at facility - CS";
  attrib V081     format=F00040_. label="Month of survey";
  attrib V082     label="Year of survey";
  attrib V083     label="Day of survey";
  attrib V095A    label="Starting time of facility interview";
  attrib V095B    label="Ending time of facility interview";
  attrib PROVINCE format=F00041_. label="Province (Country specific)";
  attrib DEVREG   format=F00042_. label="Development region";
  attrib SF101A   format=F00043_. label="Site type - CS";
  attrib V101A    format=F00044_. label="Presence of trained health provider at facility 24 hours with duty schedule, present or on-call";
  attrib V102N    label="Number of staffing occurrences used for V102";
  attrib V102A_01 format=F00045_. label="Type of staff (survey specific)(1)";
  attrib V102A_02 format=F00045_. label="Type of staff (survey specific)(2)";
  attrib V102A_03 format=F00045_. label="Type of staff (survey specific)(3)";
  attrib V102A_04 format=F00045_. label="Type of staff (survey specific)(4)";
  attrib V102A_05 format=F00045_. label="Type of staff (survey specific)(5)";
  attrib V102A_06 format=F00045_. label="Type of staff (survey specific)(6)";
  attrib V102A_07 format=F00045_. label="Type of staff (survey specific)(7)";
  attrib V102A_08 format=F00045_. label="Type of staff (survey specific)(8)";
  attrib V102A_09 format=F00045_. label="Type of staff (survey specific)(9)";
  attrib V102A_10 format=F00045_. label="Type of staff (survey specific)(10)";
  attrib V102A_11 format=F00045_. label="Type of staff (survey specific)(11)";
  attrib V102A_12 format=F00045_. label="Type of staff (survey specific)(12)";
  attrib V102A_13 format=F00045_. label="Type of staff (survey specific)(13)";
  attrib V102A_14 format=F00045_. label="Type of staff (survey specific)(14)";
  attrib V102A_15 format=F00045_. label="Type of staff (survey specific)(15)";
  attrib V102A_16 format=F00045_. label="Type of staff (survey specific)(16)";
  attrib V102A_17 format=F00045_. label="Type of staff (survey specific)(17)";
  attrib V102A_18 format=F00045_. label="Type of staff (survey specific)(18)";
  attrib V102A_19 format=F00045_. label="Type of staff (survey specific)(19)";
  attrib V102D_01 format=F00046_. label="Staff assigned, employed or seconded(1)";
  attrib V102D_02 format=F00046_. label="Staff assigned, employed or seconded(2)";
  attrib V102D_03 format=F00046_. label="Staff assigned, employed or seconded(3)";
  attrib V102D_04 format=F00046_. label="Staff assigned, employed or seconded(4)";
  attrib V102D_05 format=F00046_. label="Staff assigned, employed or seconded(5)";
  attrib V102D_06 format=F00046_. label="Staff assigned, employed or seconded(6)";
  attrib V102D_07 format=F00046_. label="Staff assigned, employed or seconded(7)";
  attrib V102D_08 format=F00046_. label="Staff assigned, employed or seconded(8)";
  attrib V102D_09 format=F00046_. label="Staff assigned, employed or seconded(9)";
  attrib V102D_10 format=F00046_. label="Staff assigned, employed or seconded(10)";
  attrib V102D_11 format=F00046_. label="Staff assigned, employed or seconded(11)";
  attrib V102D_12 format=F00046_. label="Staff assigned, employed or seconded(12)";
  attrib V102D_13 format=F00046_. label="Staff assigned, employed or seconded(13)";
  attrib V102D_14 format=F00046_. label="Staff assigned, employed or seconded(14)";
  attrib V102D_15 format=F00046_. label="Staff assigned, employed or seconded(15)";
  attrib V102D_16 format=F00046_. label="Staff assigned, employed or seconded(16)";
  attrib V102D_17 format=F00046_. label="Staff assigned, employed or seconded(17)";
  attrib V102D_18 format=F00046_. label="Staff assigned, employed or seconded(18)";
  attrib V102D_19 format=F00046_. label="Staff assigned, employed or seconded(19)";
  attrib V102E_01 format=F00047_. label="NA - Staff part-time(1)";
  attrib V102E_02 format=F00047_. label="NA - Staff part-time(2)";
  attrib V102E_03 format=F00047_. label="NA - Staff part-time(3)";
  attrib V102E_04 format=F00047_. label="NA - Staff part-time(4)";
  attrib V102E_05 format=F00047_. label="NA - Staff part-time(5)";
  attrib V102E_06 format=F00047_. label="NA - Staff part-time(6)";
  attrib V102E_07 format=F00047_. label="NA - Staff part-time(7)";
  attrib V102E_08 format=F00047_. label="NA - Staff part-time(8)";
  attrib V102E_09 format=F00047_. label="NA - Staff part-time(9)";
  attrib V102E_10 format=F00047_. label="NA - Staff part-time(10)";
  attrib V102E_11 format=F00047_. label="NA - Staff part-time(11)";
  attrib V102E_12 format=F00047_. label="NA - Staff part-time(12)";
  attrib V102E_13 format=F00047_. label="NA - Staff part-time(13)";
  attrib V102E_14 format=F00047_. label="NA - Staff part-time(14)";
  attrib V102E_15 format=F00047_. label="NA - Staff part-time(15)";
  attrib V102E_16 format=F00047_. label="NA - Staff part-time(16)";
  attrib V102E_17 format=F00047_. label="NA - Staff part-time(17)";
  attrib V102E_18 format=F00047_. label="NA - Staff part-time(18)";
  attrib V102E_19 format=F00047_. label="NA - Staff part-time(19)";
  attrib V102F_01 format=F00048_. label="Sanctioned posts - MOHP - CS(1)";
  attrib V102F_02 format=F00048_. label="Sanctioned posts - MOHP - CS(2)";
  attrib V102F_03 format=F00048_. label="Sanctioned posts - MOHP - CS(3)";
  attrib V102F_04 format=F00048_. label="Sanctioned posts - MOHP - CS(4)";
  attrib V102F_05 format=F00048_. label="Sanctioned posts - MOHP - CS(5)";
  attrib V102F_06 format=F00048_. label="Sanctioned posts - MOHP - CS(6)";
  attrib V102F_07 format=F00048_. label="Sanctioned posts - MOHP - CS(7)";
  attrib V102F_08 format=F00048_. label="Sanctioned posts - MOHP - CS(8)";
  attrib V102F_09 format=F00048_. label="Sanctioned posts - MOHP - CS(9)";
  attrib V102F_10 format=F00048_. label="Sanctioned posts - MOHP - CS(10)";
  attrib V102F_11 format=F00048_. label="Sanctioned posts - MOHP - CS(11)";
  attrib V102F_12 format=F00048_. label="Sanctioned posts - MOHP - CS(12)";
  attrib V102F_13 format=F00048_. label="Sanctioned posts - MOHP - CS(13)";
  attrib V102F_14 format=F00048_. label="Sanctioned posts - MOHP - CS(14)";
  attrib V102F_15 format=F00048_. label="Sanctioned posts - MOHP - CS(15)";
  attrib V102F_16 format=F00048_. label="Sanctioned posts - MOHP - CS(16)";
  attrib V102F_17 format=F00048_. label="Sanctioned posts - MOHP - CS(17)";
  attrib V102F_18 format=F00048_. label="Sanctioned posts - MOHP - CS(18)";
  attrib V102F_19 format=F00048_. label="Sanctioned posts - MOHP - CS(19)";
  attrib V102G_01 format=F00049_. label="Sanctioned posts - Local govt.  - CS(1)";
  attrib V102G_02 format=F00049_. label="Sanctioned posts - Local govt.  - CS(2)";
  attrib V102G_03 format=F00049_. label="Sanctioned posts - Local govt.  - CS(3)";
  attrib V102G_04 format=F00049_. label="Sanctioned posts - Local govt.  - CS(4)";
  attrib V102G_05 format=F00049_. label="Sanctioned posts - Local govt.  - CS(5)";
  attrib V102G_06 format=F00049_. label="Sanctioned posts - Local govt.  - CS(6)";
  attrib V102G_07 format=F00049_. label="Sanctioned posts - Local govt.  - CS(7)";
  attrib V102G_08 format=F00049_. label="Sanctioned posts - Local govt.  - CS(8)";
  attrib V102G_09 format=F00049_. label="Sanctioned posts - Local govt.  - CS(9)";
  attrib V102G_10 format=F00049_. label="Sanctioned posts - Local govt.  - CS(10)";
  attrib V102G_11 format=F00049_. label="Sanctioned posts - Local govt.  - CS(11)";
  attrib V102G_12 format=F00049_. label="Sanctioned posts - Local govt.  - CS(12)";
  attrib V102G_13 format=F00049_. label="Sanctioned posts - Local govt.  - CS(13)";
  attrib V102G_14 format=F00049_. label="Sanctioned posts - Local govt.  - CS(14)";
  attrib V102G_15 format=F00049_. label="Sanctioned posts - Local govt.  - CS(15)";
  attrib V102G_16 format=F00049_. label="Sanctioned posts - Local govt.  - CS(16)";
  attrib V102G_17 format=F00049_. label="Sanctioned posts - Local govt.  - CS(17)";
  attrib V102G_18 format=F00049_. label="Sanctioned posts - Local govt.  - CS(18)";
  attrib V102G_19 format=F00049_. label="Sanctioned posts - Local govt.  - CS(19)";
  attrib V102H_01 format=F00050_. label="Filled posts by - MOHP  - CS(1)";
  attrib V102H_02 format=F00050_. label="Filled posts by - MOHP  - CS(2)";
  attrib V102H_03 format=F00050_. label="Filled posts by - MOHP  - CS(3)";
  attrib V102H_04 format=F00050_. label="Filled posts by - MOHP  - CS(4)";
  attrib V102H_05 format=F00050_. label="Filled posts by - MOHP  - CS(5)";
  attrib V102H_06 format=F00050_. label="Filled posts by - MOHP  - CS(6)";
  attrib V102H_07 format=F00050_. label="Filled posts by - MOHP  - CS(7)";
  attrib V102H_08 format=F00050_. label="Filled posts by - MOHP  - CS(8)";
  attrib V102H_09 format=F00050_. label="Filled posts by - MOHP  - CS(9)";
  attrib V102H_10 format=F00050_. label="Filled posts by - MOHP  - CS(10)";
  attrib V102H_11 format=F00050_. label="Filled posts by - MOHP  - CS(11)";
  attrib V102H_12 format=F00050_. label="Filled posts by - MOHP  - CS(12)";
  attrib V102H_13 format=F00050_. label="Filled posts by - MOHP  - CS(13)";
  attrib V102H_14 format=F00050_. label="Filled posts by - MOHP  - CS(14)";
  attrib V102H_15 format=F00050_. label="Filled posts by - MOHP  - CS(15)";
  attrib V102H_16 format=F00050_. label="Filled posts by - MOHP  - CS(16)";
  attrib V102H_17 format=F00050_. label="Filled posts by - MOHP  - CS(17)";
  attrib V102H_18 format=F00050_. label="Filled posts by - MOHP  - CS(18)";
  attrib V102H_19 format=F00050_. label="Filled posts by - MOHP  - CS(19)";
  attrib V102I_01 format=F00051_. label="Filled posts by - Local govt.  - CS(1)";
  attrib V102I_02 format=F00051_. label="Filled posts by - Local govt.  - CS(2)";
  attrib V102I_03 format=F00051_. label="Filled posts by - Local govt.  - CS(3)";
  attrib V102I_04 format=F00051_. label="Filled posts by - Local govt.  - CS(4)";
  attrib V102I_05 format=F00051_. label="Filled posts by - Local govt.  - CS(5)";
  attrib V102I_06 format=F00051_. label="Filled posts by - Local govt.  - CS(6)";
  attrib V102I_07 format=F00051_. label="Filled posts by - Local govt.  - CS(7)";
  attrib V102I_08 format=F00051_. label="Filled posts by - Local govt.  - CS(8)";
  attrib V102I_09 format=F00051_. label="Filled posts by - Local govt.  - CS(9)";
  attrib V102I_10 format=F00051_. label="Filled posts by - Local govt.  - CS(10)";
  attrib V102I_11 format=F00051_. label="Filled posts by - Local govt.  - CS(11)";
  attrib V102I_12 format=F00051_. label="Filled posts by - Local govt.  - CS(12)";
  attrib V102I_13 format=F00051_. label="Filled posts by - Local govt.  - CS(13)";
  attrib V102I_14 format=F00051_. label="Filled posts by - Local govt.  - CS(14)";
  attrib V102I_15 format=F00051_. label="Filled posts by - Local govt.  - CS(15)";
  attrib V102I_16 format=F00051_. label="Filled posts by - Local govt.  - CS(16)";
  attrib V102I_17 format=F00051_. label="Filled posts by - Local govt.  - CS(17)";
  attrib V102I_18 format=F00051_. label="Filled posts by - Local govt.  - CS(18)";
  attrib V102I_19 format=F00051_. label="Filled posts by - Local govt.  - CS(19)";
  attrib V102J_01 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(1)";
  attrib V102J_02 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(2)";
  attrib V102J_03 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(3)";
  attrib V102J_04 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(4)";
  attrib V102J_05 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(5)";
  attrib V102J_06 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(6)";
  attrib V102J_07 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(7)";
  attrib V102J_08 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(8)";
  attrib V102J_09 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(9)";
  attrib V102J_10 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(10)";
  attrib V102J_11 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(11)";
  attrib V102J_12 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(12)";
  attrib V102J_13 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(13)";
  attrib V102J_14 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(14)";
  attrib V102J_15 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(15)";
  attrib V102J_16 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(16)";
  attrib V102J_17 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(17)";
  attrib V102J_18 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(18)";
  attrib V102J_19 format=F00052_. label="Filled posts by - Contracted or deputation  - CS(19)";
  attrib V102K_01 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(1)";
  attrib V102K_02 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(2)";
  attrib V102K_03 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(3)";
  attrib V102K_04 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(4)";
  attrib V102K_05 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(5)";
  attrib V102K_06 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(6)";
  attrib V102K_07 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(7)";
  attrib V102K_08 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(8)";
  attrib V102K_09 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(9)";
  attrib V102K_10 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(10)";
  attrib V102K_11 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(11)";
  attrib V102K_12 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(12)";
  attrib V102K_13 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(13)";
  attrib V102K_14 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(14)";
  attrib V102K_15 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(15)";
  attrib V102K_16 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(16)";
  attrib V102K_17 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(17)";
  attrib V102K_18 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(18)";
  attrib V102K_19 format=F00053_. label="Filled posts by - Employed directly by facility  - CS(19)";
  attrib V102DT   format=F00054_. label="Total staff assigned, employed or seconded";
  attrib V102ET   format=F00055_. label="NA - Total staff part-time";
  attrib V102T    format=F00056_. label="Total staff assigned, employed or seconded (copy of V102D)  - CS";
  attrib V102FT   format=F00057_. label="Total Sanctioned posts - MOHP  - CS";
  attrib V102GT   format=F00058_. label="Total Sanctioned posts - Local govt.  - CS";
  attrib V102HT   format=F00059_. label="Total Filled posts by - MOHP  - CS";
  attrib V102IT   format=F00060_. label="Total Filled posts by - Local govt.  - CS";
  attrib V102JT   format=F00061_. label="Total Filled posts by - Contracted or deputation  - CS";
  attrib V102KT   format=F00062_. label="Total Filled posts by - Employed directly by facility  - CS";
  attrib V105     format=F00063_. label="Frequency of routine management/administrative meetings";
  attrib V106     format=F00064_. label="Record of management meetings";
  attrib V106A    format=F00065_. label="Record most recent meet:HMIS data quality";
  attrib V106B    format=F00066_. label="Record most recent meet:HMIS reporting";
  attrib V106C    format=F00067_. label="Record most recent meet:Timeliness of HMIS reporting";
  attrib V106D    format=F00068_. label="Record most recent meet:Quality of services";
  attrib V106E    format=F00069_. label="Record most recent meet:Client utilization";
  attrib V106F    format=F00070_. label="Record most recent meet:Disease data";
  attrib V106G    format=F00071_. label="Record most recent meet:Employment conditions";
  attrib V106H    format=F00072_. label="Record most recent meet:Finances or budget";
  attrib V106X    format=F00073_. label="Record most recent meet:Other";
  attrib V106Y    format=F00074_. label="Record most recent meet:None of above topics";
  attrib V106AA   format=F00075_. label="Any decision made based on last meeting discussions";
  attrib V106BB   format=F00076_. label="Any follow-up actions based on last meeting discussions";
  attrib V107     format=F00077_. label="Frequency of routine staff/community meetings";
  attrib V108     format=F00078_. label="Record of staff/community meetings";
  attrib V109A    format=F00079_. label="Client opinion: Suggestion box";
  attrib V109B    format=F00080_. label="Client opinion:Client survey form";
  attrib V109C    format=F00081_. label="Client opinion:Client interview form";
  attrib V109D    format=F00082_. label="Client opinion:Official meetings with community leaders";
  attrib V109E    format=F00083_. label="Client opinion:Informal discussion with client/community";
  attrib V109F    format=F00084_. label="Client opinion:Email";
  attrib V109G    format=F00085_. label="Client opinion:Facility website";
  attrib V109H    format=F00086_. label="Client opinion:Letters from clients/ community";
  attrib V109X    format=F00087_. label="Client opinion:Other";
  attrib V109Y    format=F00088_. label="Client opinion:No feedback";
  attrib V109Z    format=F00089_. label="Client opinion:Don't know";
  attrib V110A    format=F00090_. label="Procedure for review/reporting of client opinion";
  attrib V110     format=F00091_. label="Record of procedure for reviewing client opinions";
  attrib V112     format=F00092_. label="Quality of care system";
  attrib V115     format=F00093_. label="Last outside supervisory visit";
  attrib V116G    format=F00094_. label="Supervisor action: Use a checklist to assess quality of available health services data";
  attrib V116H    format=F00095_. label="Supervisor action: Discuss performance of the facility based on available health services data";
  attrib V116I    format=F00096_. label="Supervisor action:Help facility make decisions based on available health services data";
  attrib V120A    format=F00097_. label="Facility connected to central supply electricity grid";
  attrib V120     format=F00098_. label="Functional backup generator with fuel (or charged battery)";
  attrib V121A    format=F00099_. label="Source other than central grid:Fuel-operated generator";
  attrib V121B    format=F00100_. label="Source other than central grid: Battery-operated generator";
  attrib V121C    format=F00101_. label="Source other than central grid: Solar system";
  attrib V121D    format=F00102_. label="Source other than central grid: Invertor - CS";
  attrib V123     format=F00103_. label="Most common source of water";
  attrib V124     format=F00104_. label="Water onsite or within 500 m of facility";
  attrib V126     format=F00105_. label="Routine time of year for water shortage";
  attrib V127A    format=F00106_. label="Functioning landline phone in facility";
  attrib V127B    format=F00107_. label="Functioning cell phone in facility";
  attrib V127C    format=F00108_. label="NA - Functioning short-wave radio in facility";
  attrib V127     format=F00109_. label="Any functioning communication system observed or reported during all service times";
  attrib V128     format=F00110_. label="Facility has a functioning computer";
  attrib V128A    format=F00111_. label="Computer observed";
  attrib V129     format=F00112_. label="Facility has access to email";
  attrib V130     format=F00113_. label="NA - Designated person responsible for health services data";
  attrib SF464    format=F00114_. label="Designated responsible person with formal training received for health statistics - CS";
  attrib V131     format=F00115_. label="System in place to regularly collect health services data";
  attrib V132     format=F00116_. label="Frequency of reports containing health services data";
  attrib V133     format=F00117_. label="Most recent report observed";
  attrib V134     format=F00118_. label="Total outpatient visits for last completed calendar month";
  attrib V135     format=F00119_. label="Total inpatient live discharges for last completed calendar month";
  attrib SF346B   format=F00120_. label="Functional invertor";
  attrib SF351A   format=F00121_. label="Frequency of receiving supervision in past 4 months";
  attrib SF420A   format=F00122_. label="Have citizen charter";
  attrib SF420B   format=F00123_. label="Location of the citizen charter";
  attrib SF420D   format=F00124_. label="Has management committee";
  attrib SF420E   label="Year of committee formed (year in AD)";
  attrib SF420FA1 format=F00125_. label="Total members";
  attrib SF420FB1 format=F00126_. label="Total Male members";
  attrib SF420FC1 format=F00127_. label="Total Female members";
  attrib SF420FA2 format=F00128_. label="Dalit total members";
  attrib SF420FB2 format=F00129_. label="Dalit Male members";
  attrib SF420FC2 format=F00130_. label="Dalit Female members";
  attrib SF420FA3 format=F00131_. label="Janajati total members";
  attrib SF420FB3 format=F00132_. label="Janajati Male members";
  attrib SF420FC3 format=F00133_. label="Janajati Female members";
  attrib SF420FA4 format=F00134_. label="Other total members";
  attrib SF420FB4 format=F00135_. label="Other Male members";
  attrib SF420FC4 format=F00136_. label="Other Female members";
  attrib SF442AB  format=F00137_. label="Quality assurance guidelines";
  attrib SF442CD  format=F00138_. label="Quality assurance action plan";
  attrib V142     format=F00139_. label="Routinely provides inpatient care";
  attrib V143     format=F00140_. label="Number of beds for overnight observation/inpatient care";
  attrib V144     format=F00141_. label="Any client fees";
  attrib V144A    format=F00142_. label="Client fee:Client health card";
  attrib V144B    format=F00143_. label="Client fee:Consultation";
  attrib V144E    format=F00144_. label="Client fee:Medications";
  attrib V144F    format=F00145_. label="Client fee:Any tests";
  attrib V144G    format=F00146_. label="NA - Client fee:Registration";
  attrib V144J    format=F00147_. label="Client fee:Vaccines";
  attrib V144K    format=F00148_. label="Client fee:Contraceptive commodities";
  attrib V144L    format=F00149_. label="Client fee:Normal deliveries";
  attrib V144M    format=F00150_. label="Client fee:Syringes and needles";
  attrib V144N    format=F00151_. label="Client fee:Cesarean section";
  attrib V144O    format=F00152_. label="Client fee:HIV diagnostic test";
  attrib V144P    format=F00153_. label="Client fee:Malaria rapid diagnostic test";
  attrib V144Q    format=F00154_. label="Client fee:Malaria microscopy";
  attrib V144R    format=F00155_. label="Client fee:Other lab tests";
  attrib V144S    format=F00156_. label="Client fee:ARVs for treatment";
  attrib V144T    format=F00157_. label="Client fee:ARVs for PMTCT";
  attrib V144U    format=F00158_. label="Client fee:Minor surgical procedures";
  attrib V144V    format=F00159_. label="Client fee:Hemoglobin test - CS";
  attrib V144W    format=F00160_. label="Client fee:Chest x-ray - CS";
  attrib V144X    format=F00161_. label="Client fee:General bed charge for inpatient - CS";
  attrib V145     format=F00162_. label="Client fees posted within easy sight";
  attrib V145A    format=F00163_. label="Client unable to pay:Fee exempted/ discounted/ no payment expected";
  attrib V145B    format=F00164_. label="Client unable to pay:Fee exempted/ discounted/ payment expected later";
  attrib V145C    format=F00165_. label="Client unable to pay:Service not provided/ asked to come back when able to pay";
  attrib V145D    format=F00166_. label="Client unable to pay:Accept payment in-kind";
  attrib V145X    format=F00167_. label="Client unable to pay:Other";
  attrib V146A    format=F00168_. label="Outside funding:Ministry of health";
  attrib V146B    format=F00169_. label="Outside funding:Ministry of federal affairs and local development";
  attrib V146C    format=F00170_. label="Outside funding:Service charge";
  attrib V146D    format=F00171_. label="Outside funding:Training colleges (nursing and medical)";
  attrib V146E    format=F00172_. label="NA - Outside funding:CS";
  attrib V146F    format=F00173_. label="NA - Outside funding:CS";
  attrib V146G    format=F00174_. label="NA - Outside funding:CS";
  attrib V146H    format=F00175_. label="NA - Outside funding:CS";
  attrib V146I    format=F00176_. label="NA - Outside funding:CS";
  attrib V146X    format=F00177_. label="Outside funding:Other";
  attrib V146Y    format=F00178_. label="NA - Outside funding:None";
  attrib V146Z    format=F00179_. label="NA - Outside funding:Don't know";
  attrib V150     format=F00180_. label="Functional ambulance with fuel";
  attrib V152     format=F00181_. label="Waiting area for clients";
  attrib V153     format=F00182_. label="Functioning latrine for clients (no/yes)";
  attrib V153A    format=F00183_. label="Functioning latrine for clients";
  attrib V154     format=F00184_. label="General cleanliness: Score out of 8 possible credits";
  attrib V154A    format=F00185_. label="General cleanliness: Floor swept, no obvious dirt/waste (Y)";
  attrib V154B    format=F00186_. label="General cleanliness: Counters/tables/chairs wiped clean (Y)";
  attrib V154C    format=F00187_. label="General cleanliness: Needles/sharps outside of sharps box (N)";
  attrib V154D    format=F00188_. label="General cleanliness: Sharps box overflowing/torn/pierced (N)";
  attrib V154E    format=F00189_. label="General cleanliness: Bandages/ infectious waste lying uncovered (N)";
  attrib V154F    format=F00190_. label="General cleanliness: Walls with significant damage (N)";
  attrib V154G    format=F00191_. label="General cleanliness: Doors with significant damage (N)";
  attrib V154H    format=F00192_. label="General cleanliness: Ceiling with water stains or damage (N)";
  attrib V165A    format=F00193_. label="General OPD area: Measuring tape (for circumference)";
  attrib V165B    format=F00194_. label="General OPD area: Spacers for inhalers";
  attrib V165C    format=F00195_. label="General OPD area: IV infusion kits - adult";
  attrib V165D    format=F00196_. label="General OPD area: IV infusion kits - pediatric";
  attrib V166A    format=F00197_. label="General OPD area: Adult weighing scale";
  attrib V166B    format=F00198_. label="General OPD area:Child weighing scale (250 g graduation)";
  attrib V166C    format=F00199_. label="General OPD area: Infant weighting scale (100 g graduation)";
  attrib V166D    format=F00200_. label="General OPD area: Stadiometer (or height rod)";
  attrib V166E    format=F00201_. label="General OPD area: Thermometer";
  attrib V166F    format=F00202_. label="General OPD area: Stethoscope";
  attrib V166G    format=F00203_. label="General OPD area: Digital BP apparatus";
  attrib V166H    format=F00204_. label="General OPD area: Manual BP apparatus";
  attrib V166I    format=F00205_. label="General OPD area: Light source (flashlight OK)";
  attrib V166J    format=F00206_. label="General OPD area: Self-inflating bag and mask (adult)";
  attrib V166K    format=F00207_. label="General OPD area: Self-inflating bag and mask (pediatric)";
  attrib V166L    format=F00208_. label="General OPD area: Micronebulizer";
  attrib V166M    format=F00209_. label="General OPD area: Peak flow meters";
  attrib V166N    format=F00210_. label="General OPD area: Pulse oximeter";
  attrib V166O    format=F00211_. label="General OPD area: Oxygen concentrators";
  attrib V166P    format=F00212_. label="General OPD area: Filled oxygen cylinder";
  attrib V166Q    format=F00213_. label="General OPD area: Oxygen distribution system";
  attrib V166R    format=F00214_. label="General OPD area: Wheel chair - CS";
  attrib SV165A   format=F00215_. label="General OPD area: Measuring tape  - CS (Functioning column is CS)";
  attrib V167     format=F00216_. label="General OPD area:Privacy for observed room";
  attrib V168A1   format=F00217_. label="General OPD area:Running water";
  attrib V168C    format=F00218_. label="General OPD area:Soap";
  attrib V168F    format=F00219_. label="General OPD area:Sharps container";
  attrib V168G    format=F00220_. label="General OPD area:Clean/sterile latex gloves";
  attrib V168J    format=F00221_. label="General OPD area:Disinfectant";
  attrib V168L    format=F00222_. label="General OPD area:Autodestruct syringes with needles or single-use disposable syringes with needles";
  attrib V168O    format=F00223_. label="General OPD area:Pedal Bin waste receptacle with lid and plastic liner";
  attrib V168P    format=F00224_. label="General OPD area:Other waste receptacle (not pedal bin)";
  attrib V168Q    format=F00225_. label="General OPD area:Alcohol-based hand rub";
  attrib V168R    format=F00226_. label="General OPD area:Medical masks";
  attrib V168S    format=F00227_. label="General OPD area:Gowns";
  attrib V168T    format=F00228_. label="General OPD area:Eye protection";
  attrib V168U    format=F00229_. label="General OPD area:Guidelines for standard precautions";
  attrib V168V    format=F00230_. label="General OPD area:Needle destroyer - CS";
  attrib V168W    format=F00231_. label="General OPD area:Methylated spirit and glycirine 70:30 - CS";
  attrib V146AB   format=F00232_. label="Outside funding in NRs.:Ministry of health in NRs. - CS";
  attrib V146BB   format=F00233_. label="Outside funding in NRs.:Ministry of federal affairs and local development in NRs. - CS";
  attrib V146CB   format=F00234_. label="Outside funding in NRs.:Service charge in NRs. - CS";
  attrib V146DB   format=F00235_. label="Outside funding in NRs.:Training colleges (nursing and medical)in NRs. - CS";
  attrib V146XB   format=F00236_. label="Outside funding in NRs.:Other in NRs. - CS";
  attrib SF370DA1 format=F00237_. label="Financial audit - FY 2068/69";
  attrib SF370DB1 format=F00238_. label="Financial audit - FY 2069/70";
  attrib SF370DC1 format=F00239_. label="Financial audit - FY 2070/71";
  attrib SF370DA2 format=F00240_. label="Social audit - FY 2068/69";
  attrib SF370DB2 format=F00241_. label="Social audit - FY 2069/70";
  attrib SF370DC2 format=F00242_. label="Social audit - FY 2070/71";
  attrib SF453A   format=F00243_. label="Mode of other emergency transport";
  attrib SF462A   format=F00244_. label="Type of HMIS form used";
  attrib SF465BC  format=F00245_. label="HMIS recording and reporting tool kit 2070";
  attrib SF465DE  format=F00246_. label="HMIS user manual 2070";
  attrib SF465FG  format=F00247_. label="HMIS indicator booklet 2070";
  attrib SF465H   format=F00248_. label="Updated monthly monitoring sheet of last 3 months";
  attrib SF471    format=F00249_. label="Number of admissions (discharges)in last fiscal year 2013-14 (adult and children) - CS";
  attrib SF471A   format=F00250_. label="Number of inpatient days in last fiscal year 2013-14 (adult and children) - CS";
  attrib SF472    format=F00251_. label="Number of outpatient client visits in last fiscal year 2013-14 (adult and children) - CS";
  attrib SF472EF  format=F00252_. label="Displayed updated key health service data";
  attrib SF480A   format=F00253_. label="Have system to manage health LMIS data";
  attrib SF480BC  format=F00254_. label="Regular complie any reports containing LMIS data";
  attrib SF480DE  format=F00255_. label="Have designated person for LMIS";
  attrib SF480F   format=F00256_. label="Designated person formally trained on LMIS";
  attrib SF480G   format=F00257_. label="Year of person trained";
  attrib SF480HI  format=F00258_. label="Have storage mgmt guideline (FLEX)";
  attrib SF480JK  format=F00259_. label="Have health logistic pull system manual";
  attrib SF700CD  format=F00260_. label="Equipment audit report";
  attrib SF720AB  format=F00261_. label="Disaster preparedness contingency plan/manual";
  attrib SF720C   format=F00262_. label="conduct Drill Down excercise";
  attrib V173A    format=F00263_. label="Process equipment in facility or out";
  attrib V173     format=F00264_. label="Sterilize equipment(surgical)";
  attrib V175A_1  format=F00265_. label="Sterilization method(1)";
  attrib V175A_2  format=F00265_. label="Sterilization method(2)";
  attrib V175A_3  format=F00265_. label="Sterilization method(3)";
  attrib V175A_4  format=F00265_. label="Sterilization method(4)";
  attrib V175A_5  format=F00265_. label="Sterilization method(5)";
  attrib V175A_6  format=F00265_. label="Sterilization method(6)";
  attrib V175A_7  format=F00265_. label="Sterilization method(7)";
  attrib V175A_8  format=F00265_. label="Sterilization method(8)";
  attrib V175B_1  format=F00266_. label="Method used(1)";
  attrib V175B_2  format=F00266_. label="Method used(2)";
  attrib V175B_3  format=F00266_. label="Method used(3)";
  attrib V175B_4  format=F00266_. label="Method used(4)";
  attrib V175B_5  format=F00266_. label="Method used(5)";
  attrib V175B_6  format=F00266_. label="Method used(6)";
  attrib V175B_7  format=F00266_. label="Method used(7)";
  attrib V175B_8  format=F00266_. label="Method used(8)";
  attrib V175C_1  format=F00267_. label="Temperature used(1)";
  attrib V175C_2  format=F00267_. label="Temperature used(2)";
  attrib V175C_3  format=F00267_. label="Temperature used(3)";
  attrib V175C_4  format=F00267_. label="Temperature used(4)";
  attrib V175C_5  format=F00267_. label="Temperature used(5)";
  attrib V175C_6  format=F00267_. label="Temperature used(6)";
  attrib V175C_7  format=F00267_. label="Temperature used(7)";
  attrib V175C_8  format=F00267_. label="Temperature used(8)";
  attrib V175D_1  format=F00268_. label="Pressure used(1)";
  attrib V175D_2  format=F00268_. label="Pressure used(2)";
  attrib V175D_3  format=F00268_. label="Pressure used(3)";
  attrib V175D_4  format=F00268_. label="Pressure used(4)";
  attrib V175D_5  format=F00268_. label="Pressure used(5)";
  attrib V175D_6  format=F00268_. label="Pressure used(6)";
  attrib V175D_7  format=F00268_. label="Pressure used(7)";
  attrib V175D_8  format=F00268_. label="Pressure used(8)";
  attrib V175E_1  format=F00269_. label="Units of pressure used(1)";
  attrib V175E_2  format=F00269_. label="Units of pressure used(2)";
  attrib V175E_3  format=F00269_. label="Units of pressure used(3)";
  attrib V175E_4  format=F00269_. label="Units of pressure used(4)";
  attrib V175E_5  format=F00269_. label="Units of pressure used(5)";
  attrib V175E_6  format=F00269_. label="Units of pressure used(6)";
  attrib V175E_7  format=F00269_. label="Units of pressure used(7)";
  attrib V175E_8  format=F00269_. label="Units of pressure used(8)";
  attrib V175F_1  format=F00270_. label="Minutes unwrapped(1)";
  attrib V175F_2  format=F00270_. label="Minutes unwrapped(2)";
  attrib V175F_3  format=F00270_. label="Minutes unwrapped(3)";
  attrib V175F_4  format=F00270_. label="Minutes unwrapped(4)";
  attrib V175F_5  format=F00270_. label="Minutes unwrapped(5)";
  attrib V175F_6  format=F00270_. label="Minutes unwrapped(6)";
  attrib V175F_7  format=F00270_. label="Minutes unwrapped(7)";
  attrib V175F_8  format=F00270_. label="Minutes unwrapped(8)";
  attrib V175G_1  format=F00271_. label="Minutes wrapped(1)";
  attrib V175G_2  format=F00271_. label="Minutes wrapped(2)";
  attrib V175G_3  format=F00271_. label="Minutes wrapped(3)";
  attrib V175G_4  format=F00271_. label="Minutes wrapped(4)";
  attrib V175G_5  format=F00271_. label="Minutes wrapped(5)";
  attrib V175G_6  format=F00271_. label="Minutes wrapped(6)";
  attrib V175G_7  format=F00271_. label="Minutes wrapped(7)";
  attrib V175G_8  format=F00271_. label="Minutes wrapped(8)";
  attrib V175H_1  format=F00272_. label="NA - Chemical solution(1)";
  attrib V175H_2  format=F00272_. label="NA - Chemical solution(2)";
  attrib V175H_3  format=F00272_. label="NA - Chemical solution(3)";
  attrib V175H_4  format=F00272_. label="NA - Chemical solution(4)";
  attrib V175H_5  format=F00272_. label="NA - Chemical solution(5)";
  attrib V175H_6  format=F00272_. label="NA - Chemical solution(6)";
  attrib V175H_7  format=F00272_. label="NA - Chemical solution(7)";
  attrib V175H_8  format=F00272_. label="NA - Chemical solution(8)";
  attrib V175HA_1 format=F00273_. label="Chemical solution : Alcohol - CS(1)";
  attrib V175HA_2 format=F00273_. label="Chemical solution : Alcohol - CS(2)";
  attrib V175HA_3 format=F00273_. label="Chemical solution : Alcohol - CS(3)";
  attrib V175HA_4 format=F00273_. label="Chemical solution : Alcohol - CS(4)";
  attrib V175HA_5 format=F00273_. label="Chemical solution : Alcohol - CS(5)";
  attrib V175HA_6 format=F00273_. label="Chemical solution : Alcohol - CS(6)";
  attrib V175HA_7 format=F00273_. label="Chemical solution : Alcohol - CS(7)";
  attrib V175HA_8 format=F00273_. label="Chemical solution : Alcohol - CS(8)";
  attrib V175HB_1 format=F00274_. label="Chemical solution : Betadine - CS(1)";
  attrib V175HB_2 format=F00274_. label="Chemical solution : Betadine - CS(2)";
  attrib V175HB_3 format=F00274_. label="Chemical solution : Betadine - CS(3)";
  attrib V175HB_4 format=F00274_. label="Chemical solution : Betadine - CS(4)";
  attrib V175HB_5 format=F00274_. label="Chemical solution : Betadine - CS(5)";
  attrib V175HB_6 format=F00274_. label="Chemical solution : Betadine - CS(6)";
  attrib V175HB_7 format=F00274_. label="Chemical solution : Betadine - CS(7)";
  attrib V175HB_8 format=F00274_. label="Chemical solution : Betadine - CS(8)";
  attrib V175HC_1 format=F00275_. label="Chemical solution : Chlorine - CS(1)";
  attrib V175HC_2 format=F00275_. label="Chemical solution : Chlorine - CS(2)";
  attrib V175HC_3 format=F00275_. label="Chemical solution : Chlorine - CS(3)";
  attrib V175HC_4 format=F00275_. label="Chemical solution : Chlorine - CS(4)";
  attrib V175HC_5 format=F00275_. label="Chemical solution : Chlorine - CS(5)";
  attrib V175HC_6 format=F00275_. label="Chemical solution : Chlorine - CS(6)";
  attrib V175HC_7 format=F00275_. label="Chemical solution : Chlorine - CS(7)";
  attrib V175HC_8 format=F00275_. label="Chemical solution : Chlorine - CS(8)";
  attrib V175HD_1 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(1)";
  attrib V175HD_2 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(2)";
  attrib V175HD_3 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(3)";
  attrib V175HD_4 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(4)";
  attrib V175HD_5 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(5)";
  attrib V175HD_6 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(6)";
  attrib V175HD_7 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(7)";
  attrib V175HD_8 format=F00276_. label="Chemical solution : Cidex/Gluteraldehyde - CS(8)";
  attrib V175HE_1 format=F00277_. label="Chemical solution : Formaldehyde - CS(1)";
  attrib V175HE_2 format=F00277_. label="Chemical solution : Formaldehyde - CS(2)";
  attrib V175HE_3 format=F00277_. label="Chemical solution : Formaldehyde - CS(3)";
  attrib V175HE_4 format=F00277_. label="Chemical solution : Formaldehyde - CS(4)";
  attrib V175HE_5 format=F00277_. label="Chemical solution : Formaldehyde - CS(5)";
  attrib V175HE_6 format=F00277_. label="Chemical solution : Formaldehyde - CS(6)";
  attrib V175HE_7 format=F00277_. label="Chemical solution : Formaldehyde - CS(7)";
  attrib V175HE_8 format=F00277_. label="Chemical solution : Formaldehyde - CS(8)";
  attrib V175HZ_1 format=F00278_. label="Chemical solution : Don't know - CS(1)";
  attrib V175HZ_2 format=F00278_. label="Chemical solution : Don't know - CS(2)";
  attrib V175HZ_3 format=F00278_. label="Chemical solution : Don't know - CS(3)";
  attrib V175HZ_4 format=F00278_. label="Chemical solution : Don't know - CS(4)";
  attrib V175HZ_5 format=F00278_. label="Chemical solution : Don't know - CS(5)";
  attrib V175HZ_6 format=F00278_. label="Chemical solution : Don't know - CS(6)";
  attrib V175HZ_7 format=F00278_. label="Chemical solution : Don't know - CS(7)";
  attrib V175HZ_8 format=F00278_. label="Chemical solution : Don't know - CS(8)";
  attrib V176A    format=F00279_. label="Non-electric pot with cover(steam/boil):Available";
  attrib V176B    format=F00280_. label="TST indicator strips available";
  attrib V176C    format=F00281_. label="Chemicals for chemical HLD";
  attrib V177A    format=F00282_. label="Electric dry heat sterilizer:Available/functioning";
  attrib V177B    format=F00283_. label="Electric autoclave:Available/functioning";
  attrib V177C    format=F00284_. label="Non-electric autoclave:Available/functioning";
  attrib V177D    format=F00285_. label="Electric pot with cover(steam/boil):Available/functioning";
  attrib V177E    format=F00286_. label="Heat source for non-electric equipment:Available/functioning";
  attrib V177F    format=F00287_. label="Automatic timer:Available/functioning";
  attrib V177G    format=F00288_. label="Non-electric pot with cover(steam/boil):Available/functioning- CS";
  attrib V178B    format=F00289_. label="Written guidelines for disinfection/sterilization available";
  attrib V184A    format=F00290_. label="Waste disposal of sharps (later version of codes)";
  attrib V185     format=F00291_. label="Sharps waste visible or protected";
  attrib V186A    format=F00292_. label="Waste disposal of contaminated/hazardous waste (later version of codes)";
  attrib V187     format=F00293_. label="Contaminated/hazardous waste visible or protected";
  attrib V189     format=F00294_. label="Incinerator functional and with fuel";
  attrib V190     format=F00295_. label="Guidelines on health care waste management in main outpatient service area";
  attrib V192FC   format=F00296_. label="DHS Main uses dry heat sterilization";
  attrib V192CP   format=F00297_. label="DHS Main has functioning dry heat sterilizer observed";
  attrib V192A    format=F00298_. label="DHS Main:temperature automatic or 170-250 & time autom or 60-120 minutes";
  attrib V192B    format=F00299_. label="DHS Main: temp autom or 170-250 & time 121-400 minutes";
  attrib V192C    format=F00300_. label="DHS Main: temp 160-169 & time 120-400 minutes";
  attrib V192T    format=F00301_. label="DHS Main has observed timer or time is automatic";
  attrib V193FC   format=F00302_. label="Autoclave Main uses autoclave";
  attrib V193CP   format=F00303_. label="Autoclave Main has observed func. electric autoclave or functioning non-electric autoclave with functioning heat source observed";
  attrib V193WA   format=F00304_. label="Autoclave Main: wrapped time is automatic or 30-60 minutes or 15 min & press 30 & temp 132";
  attrib V193WB   format=F00305_. label="Autoclave Main: wrapped time is 61-600 minutes";
  attrib V193UA   format=F00306_. label="Autoclave Main: unwrapped time is automatic or 20-40 minutes";
  attrib V193UB   format=F00307_. label="Autoclave Main: unwrapped time is 41-600 minutes";
  attrib V193T    format=F00308_. label="Autoclave Main: has observed timer or wrapped/unwrapped time is automatic";
  attrib V193PE   format=F00309_. label="Autoclave Main:pressure automatic or PPSQI is 13-17 or ATM pressure is 1 or 2 or kilopas is 106 or MMHG is 776";
  attrib V193PG   format=F00310_. label="Autoclave Main pressure automatic or PPSQI is 18-60 or ATM is 3-8 or Kilop is 107-213 or MMHG is 777-995 or (kilop or mmhg & n is 1,2,13-17";
  attrib V193TE   format=F00311_. label="Autoclave Main temp excellent (automatic or 121-132)";
  attrib V193TG   format=F00312_. label="Autoclave Main temp good (133-900)";
  attrib V194FC   format=F00313_. label="Boil/steam Main uses boil/steam";
  attrib V194CP   format=F00314_. label="Boil/steam Main has observed pot with functioning heat source observed";
  attrib V194A    format=F00315_. label="Boil/steam Main unwrapped time is automatic or 20-40 minutes";
  attrib V194B    format=F00316_. label="Boil/steam Main unwrapped time is 41-600 minutes";
  attrib V194T    format=F00317_. label="Boil/steam Main has observed timer or unwrapped time is automatic";
  attrib V195FC   format=F00318_. label="HLD Main uses HLD";
  attrib V195CP   format=F00319_. label="HLD Main has JIK or Cidex";
  attrib V195A    format=F00320_. label="HLD Main unwrapped time is 20-900";
  attrib V195B    format=F00321_. label="HLD Main has JIK or Cidex";
  attrib V195C    format=F00322_. label="HLD Main has JIK and unwrapped time is 480-994 minutes";
  attrib V195T    format=F00323_. label="HLD Main has observed timer or unwrapped time is automatic";
  attrib VT101    format=F00324_. label="Facility has good sharps waste disposal";
  attrib VT102    format=F00325_. label="Facility has good medical waste disposal";
  attrib VT103    format=F00326_. label="Facility has good sharps waste storage";
  attrib VT104    format=F00327_. label="Facility has good medical waste storage";
  attrib VT105    format=F00328_. label="Any sterilization/HLD capacity";
  attrib VT106    format=F00329_. label="Any sterilization/HLD knowledge";
  attrib VT107    format=F00330_. label="Any sterilization/HLD timer";
  attrib VT201    format=F00331_. label="Facility offers EPI";
  attrib VT202    format=F00332_. label="NA - Facility offers EPI + BCG + Pneumococcal + Rotavirus";
  attrib VT301    format=F00333_. label="Facility offers any modern temporary family planning";
  attrib VT302    format=F00334_. label="Facility offers sterilization";
  attrib VT303    format=F00335_. label="Facility offers Periodic Abstinence/temporary family planning";
  attrib VT801    format=F00336_. label="Facility has functional microscope";
  attrib VT802    format=F00337_. label="Facility has microscope slides";
  attrib VT803    format=F00338_. label="Facility has test tubes";
  attrib VT804    format=F00339_. label="Facility has functional incubator";
  attrib VT805    format=F00340_. label="Facility has functional incubator for HIV testing";
  attrib VT806    format=F00341_. label="Facility has rotator/shaker";
  attrib VT807    format=F00342_. label="Facility offers HIV RDT";
  attrib VT808    format=F00343_. label="Facility offers HIV lab test";
  attrib VT809    format=F00344_. label="Facility offers HIV test internally";
  attrib VT810    format=F00345_. label="Facility offers HIV test external source";
  attrib VT811    format=F00346_. label="Facility offers HIV test (internal or external)";
  attrib VT812    format=F00347_. label="Facility offers HIV dx/tx";
  attrib VT813    format=F00348_. label="Facility offers dried blood spot testing";
  attrib VT814    format=F00349_. label="Facility offers TB RDT";
  attrib VT815    format=F00350_. label="Facility offers TB microscopy";
  attrib VT816    format=F00351_. label="Facility offers TB culture";
  attrib VT817    format=F00352_. label="Facility offers TB xray";
  attrib VT818    format=F00353_. label="Facility offers TB test internally";
  attrib VT819    format=F00354_. label="Facility offers TB test external source";
  attrib VT820    format=F00355_. label="Facility offers any TB test (internal or external)";
  attrib VT821    format=F00356_. label="Facility offers TB dx/tx";
  attrib VT822    format=F00357_. label="Facility offers malaria RDT";
  attrib VT823    format=F00358_. label="Facility offers malaria lab test";
  attrib VT824    format=F00359_. label="Facility offers any malaria test (lab or RDT)";
  attrib VT825    format=F00360_. label="Facility offers malaria dx/tx";
  attrib VT826    format=F00361_. label="Test: Hemoglobin";
  attrib VT827    format=F00362_. label="Test: CD4";
  attrib VT828    format=F00363_. label="Test: Blood glucose";
  attrib VT829    format=F00364_. label="Test: syphilis serology";
  attrib VT830    format=F00365_. label="Test: Stool microscopy";
  attrib VT1201   format=F00366_. label="Facility offers PMTCT";
  attrib VT1401   format=F00367_. label="Facility offers ART";
  attrib VT1501   format=F00368_. label="Facility offers HIV care and support services";
  attrib SF600A   format=F00369_. label="Segragate the waste at time of collection - CS";
  attrib SF601A   format=F00370_. label="Way of expired drugs disposed";
  attrib SF603B   format=F00371_. label="Sharps waste desinfected prior to collection for off-site disposal";
  attrib SF605B   format=F00372_. label="Medical waste desinfected prior to collection for off-site disposal";
  attrib SF620A   format=F00373_. label="Toilet (latrine) disable friendly";
  attrib V200B    label="Days/mo at facility:BCG immunization";
  attrib V200C    label="Days/mo at facility:Sick child consult";
  attrib V200D    label="Days/mo at facility:Growth monitoring";
  attrib V200E    label="Days/mo at facility: Routine Pentavalent";
  attrib V200G    label="Days/mo at facility: Routine Polio";
  attrib V200H    label="Days/mo at facility: Routine Measles";
  attrib V200I    label="Days/mo at facility: Pneumococcal vaccination (PCV) - CS";
  attrib V200J    label="Days/mo at facility: Japanese encephalitis (JE) - CS";
  attrib V201B    label="Days/mo outreach:BCG immunization";
  attrib V201C    label="Days/mo outreach:Sick child consult";
  attrib V201D    label="Days/mo outreach:Growth monitoring";
  attrib V201E    label="Days/mo outreach: Routine Pentavalent";
  attrib V201G    label="Days/mo outreach: Routine Polio";
  attrib V201H    label="Days/mo outreach: Routine Measles";
  attrib V201I    label="Days/mo outreach: Pneumococcal vaccination (PCV) - CS";
  attrib V201J    label="Days/mo outreach: Japanese encephalitis (JE)";
  attrib V230SRC  format=F00374_. label="Service where IC information collected for child vaccination";
  attrib V230     format=F00375_. label="Privacy for child vaccination";
  attrib V231A1   format=F00376_. label="Immun admin:Any running water";
  attrib V231C    format=F00377_. label="Immun admin:Soap";
  attrib V231F    format=F00378_. label="Immun admin:Sharps box";
  attrib V231G    format=F00379_. label="Immun admin:Latex gloves";
  attrib V231J    format=F00380_. label="Immun admin:Disinfectant";
  attrib V231L    format=F00381_. label="Immun admin: Auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V231O    format=F00382_. label="Immun admin:Waste receptacle  with lid and plastic liner (pedal bin)";
  attrib V231P    format=F00383_. label="Immun admin:Other waste receptacle";
  attrib V231Q    format=F00384_. label="Immun admin:Alcohol-based hand rub";
  attrib V231R    format=F00385_. label="Immun admin:Medical masks";
  attrib V231S    format=F00386_. label="Immun admin:Gowns";
  attrib V231T    format=F00387_. label="Immun admin:Eye protection";
  attrib V231U    format=F00388_. label="Immun admin:Guidelines for standard precautions";
  attrib V231V    format=F00389_. label="Immun admin:Needle destroyer - CS";
  attrib V231W    format=F00390_. label="Immun admin:Methylated spirit and glycirin 70:30 - CS";
  attrib V231XA   format=F00391_. label="Immun admin:Blank immun (child) cards";
  attrib V231XB   format=F00392_. label="Immun admin:Register or tally sheets";
  attrib V231XC   format=F00393_. label="NA - Immun admin:Summary forms";
  attrib V231XD   format=F00394_. label="Immun admin:FCHV report form (HMIS 9.1) - CS";
  attrib V231XE   format=F00395_. label="Immun admin:Immunization and outreach clinic programme report (HMIS 9.2) - CS";
  attrib V231XF   format=F00396_. label="Immun admin:Monthly progress report (HMIS 9.3) - CS";
  attrib V231XG   format=F00397_. label="Immun admin:Hospital monthly progress report (HMIS 9.4) - CS";
  attrib V231XH   format=F00398_. label="Immun admin:Immunization register - CS";
  attrib V242     format=F00399_. label="Providers in facility follow IMCI guidelines for children under 5 years";
  attrib V246     format=F00400_. label="Any routine actions for sick children";
  attrib V247A    format=F00401_. label="Routine:Weigh child";
  attrib V247B    format=F00402_. label="Routine:Plot weight on graph";
  attrib V247C    format=F00403_. label="Routine:Take child's temperature";
  attrib V247D    format=F00404_. label="Routine:Assess immunization status";
  attrib V247E    format=F00405_. label="Routine:Group health education";
  attrib V247F    format=F00406_. label="Routine:Administer paracetamol/sponge for fever";
  attrib V247G    format=F00407_. label="Routine:Triage sick children";
  attrib V263     format=F00408_. label="Individual records/charts for sick child consults";
  attrib V264SRC  format=F00409_. label="Service where IC information collected for sick child exam";
  attrib V264     format=F00410_. label="Privacy for sick child exam";
  attrib V264A1   format=F00411_. label="Sick child exam:Any running water";
  attrib V264C    format=F00412_. label="Sick child exam:Soap";
  attrib V264F    format=F00413_. label="Sick child exam:Sharps container";
  attrib V264G    format=F00414_. label="Sick child exam:Clean/sterile latex gloves";
  attrib V264J    format=F00415_. label="Sick child exam:Disinfectant";
  attrib V264L    format=F00416_. label="Sick child exam:Auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V264O    format=F00417_. label="Sick child exam:Waste receptacle  with lid and plastic liner (pedal bin)";
  attrib V264P    format=F00418_. label="Sick child exam:Other waste receptacle";
  attrib V264Q    format=F00419_. label="Sick child exam:Alcohol-based hand rub";
  attrib V264R    format=F00420_. label="Sick child exam:Medical masks";
  attrib V264S    format=F00421_. label="Sick child exam:Gowns";
  attrib V264T    format=F00422_. label="Sick child exam:Eye protection";
  attrib V264U    format=F00423_. label="Sick child exam:Guidelines for standard precautions";
  attrib V264V    format=F00424_. label="Sick child exam:Needle destroyer - CS";
  attrib V264W    format=F00425_. label="Sick child exam:Methylated spirit and glycirin 70:30 - CS";
  attrib V264XE   format=F00426_. label="Sick child exam:Calibrated 0.5 or 1 liter jar or pitcher for ORS";
  attrib V264XF   format=F00427_. label="Sick child exam:Cup and spoon";
  attrib V264XG   format=F00428_. label="Sick child exam:ORS packets";
  attrib V264XH   format=F00429_. label="Sick child exam:Staff has watch with second hand";
  attrib V264XI   format=F00430_. label="Sick child exam:At least 1 buckets for cleaning used cups";
  attrib V264XJ   format=F00431_. label="Sick child exam:Examination table/bed";
  attrib V265B    format=F00432_. label="Sick child exam other equip:Infant scale";
  attrib V265C    format=F00433_. label="Sick child exam other equip:Child scale";
  attrib V265D    format=F00434_. label="Sick child exam other equip:Thermometer";
  attrib V265E    format=F00435_. label="Sick child exam other equip:Timer or facility provided watch/clock with second hand";
  attrib V265F    format=F00436_. label="Sick child exam other equip:Staff has watch with seconds or other device - CS";
  attrib V265G    format=F00437_. label="Sick child exam other equip:Examination bed or table - CS";
  attrib V265H    format=F00438_. label="Sick child exam other equip:Stethoscope";
  attrib V266A    format=F00439_. label="Sick child consult:IMCI guidelines";
  attrib V266B    format=F00440_. label="Sick child consult:IMCI chart/booklet";
  attrib V266D    format=F00441_. label="Sick child consult:IMCI mother's cards";
  attrib V266E    format=F00442_. label="Sick child consult:Other visual aids for teaching caretakers";
  attrib V266F    format=F00443_. label="Sick child consult:IEC materials on Infant and Young Child Feeding (IYCF)";
  attrib V266G    format=F00444_. label="Sick child consult:IEC materials on IMCI?";
  attrib V266X    format=F00445_. label="Sick child consult:Other guidelines for diagnosis and management of childhood illnesses";
  attrib V267A    format=F00446_. label="Child health services provided at facility:dx/tx child malnutrition";
  attrib V267B    format=F00447_. label="Child health services provided at facility:provide vitamin A supplementation";
  attrib V267C    format=F00448_. label="NA - Child health services provided at facility:provide iron supplementation";
  attrib V267D    format=F00449_. label="Child health services provided at facility:provide zinc supplementation";
  attrib V267E    format=F00450_. label="Child health services provided at facility:provide deworming to school children";
  attrib V267F    format=F00451_. label="Child health services provided at facility:provide balvita, micronutrient powder (MNP)";
  attrib V271     format=F00452_. label="Growth monitoring: guidelines available";
  attrib V271A    format=F00453_. label="NA - Growth monitoring: CS Guidelines";
  attrib V272A    format=F00454_. label="Growth monitoring equip: Infant scale";
  attrib V272B    format=F00455_. label="Growth monitoring equip: Child scale";
  attrib V272C    format=F00456_. label="Growth monitoring equip: Height or length board";
  attrib V272D    format=F00457_. label="Growth monitoring equip: Tape for measuring head circumference - CS";
  attrib V272F    format=F00458_. label="Growth monitoring equip: Tape for mid-upper arm circumference - CS";
  attrib V273A    format=F00459_. label="Growth monitoring equip: Tape for measuring head circumference";
  attrib V273B    format=F00460_. label="Growth monitoring equip: Growth charts";
  attrib V280A    format=F00461_. label="National guidelines for child vaccinations available";
  attrib V280X    format=F00462_. label="Other guidelines for child vaccinations available";
  attrib LV200    format=F00463_. label="Facility stores vaccines in service area";
  attrib LV203A   format=F00464_. label="Avail/valid:Tetanus toxoid in pharmacy";
  attrib LV203B   format=F00465_. label="Avail/valid:BCG and dilutant in service area";
  attrib LV203C   format=F00466_. label="Avail/valid:Oral polio in service area";
  attrib LV203D   format=F00467_. label="Avail/valid:Pentavalent/ DPT+Hib+HepB in service area";
  attrib LV203E   format=F00468_. label="Avail/valid:Measles vaccine & dilutent in service area";
  attrib LV203F   format=F00469_. label="NA - Avail/valid:Hepatitis vaccine in service area";
  attrib LV203G   format=F00470_. label="Avail/valid: Pneumococcal conjugate Vaccine (PCV 13)- CS";
  attrib LV203H   format=F00471_. label="Avail/valid: Japanese encephalistis vaccine (JE)- CS";
  attrib LV206B   format=F00472_. label="Any expired vaccines observed in service area or pharmacy";
  attrib LV221    format=F00473_. label="Vaccine storage in service area: refrigerator";
  attrib LV222A   format=F00474_. label="Temperature of vaccine storage in service area";
  attrib LV224    format=F00475_. label="Temperature monitoring chart present in service area and complete";
  attrib LV225    format=F00476_. label="Number of vaccine carriers in service area";
  attrib LV226    format=F00477_. label="Ice packs for vaccine carriers in service area";
  attrib SF1015A  format=F00478_. label="Icepack condition observed";
  attrib SF1015BC format=F00479_. label="Vaccine building system";
  attrib SF1015D  format=F00480_. label="Follow multi dose-vial policy (MDVP)";
  attrib SF1015E  format=F00481_. label="Follow vaccine vial monitoring (VVM)";
  attrib SF1015FG format=F00482_. label="Use of adverse effect following immunization (AEFI)";
  attrib V301A    label="Days/month FP services offered";
  attrib V303A    format=F00483_. label="Offer:Combined oral pill";
  attrib V303B    format=F00484_. label="Offer:Progestin-only pill";
  attrib V303D    format=F00485_. label="NA - Offer:Injectable combined (1 monthly)";
  attrib V303E    format=F00486_. label="Offer:Injectable Progestin only (2-3 monthly)";
  attrib V303G    format=F00487_. label="Offer:Implant";
  attrib V303H    format=F00488_. label="NA - Offer:Implant (CS)";
  attrib V303I    format=F00489_. label="Offer:Male condom";
  attrib V303J    format=F00490_. label="NA - Offer:Female condom";
  attrib V303K    format=F00491_. label="Offer:IUD";
  attrib V303L    format=F00492_. label="Offer:Emergency contraceptive pill";
  attrib V303O    format=F00493_. label="Offer:Counseling on natural methods";
  attrib V303Q    format=F00494_. label="Offer:Male sterilization";
  attrib V303R    format=F00495_. label="Offer:Female sterilization";
  attrib V303S    format=F00496_. label="NA - Offer:Cycle Beads for SDM";
  attrib V303X    format=F00497_. label="NA - Offer:Other (spermicide/ diaphragm)";
  attrib V306     format=F00498_. label="Routine action for FP before consultation";
  attrib V307A    format=F00499_. label="Routine:Weigh client";
  attrib V307B    format=F00500_. label="Routine:Take blood pressure";
  attrib V307C    format=F00501_. label="Routine:Group health sessions";
  attrib V308     format=F00502_. label="Provider routinely treats STIs";
  attrib V308A    format=F00503_. label="Provider routinely treats STIs (new codes)";
  attrib V324     format=F00504_. label="Individual records for FP clients";
  attrib V325A    format=F00505_. label="NA - Supplies:Samples of FP methods";
  attrib V325D    format=F00506_. label="Supplies:Model for showing male condom use";
  attrib V325F    format=F00507_. label="Supplies:FP flip charts, leaflets";
  attrib V325G    format=F00508_. label="Supplies:Pelvic  model to demonstrate IUCD";
  attrib V325H    format=F00509_. label="Supplies:Examination bed or couch";
  attrib V325I    format=F00510_. label="Supplies:FP kit for counselling kit - CS";
  attrib V327A    format=F00511_. label="Guidelines:National family planning guidelines";
  attrib V327X    format=F00512_. label="Guidelines:Other family planning guidelines";
  attrib V330SRC  format=F00513_. label="Source for infection control for family planning exam";
  attrib V330     format=F00514_. label="Privacy for family planning exam";
  attrib V331A1   format=F00515_. label="Family planning exam:Any running water";
  attrib V331C    format=F00516_. label="Family planning exam:Soap";
  attrib V331F    format=F00517_. label="Family planning exam:Sharps container";
  attrib V331G    format=F00518_. label="Family planning exam:Clean/sterile latex gloves";
  attrib V331J    format=F00519_. label="Family planning exam:Disinfectant";
  attrib V331L    format=F00520_. label="Family planning exam:Auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V331O    format=F00521_. label="Family planning exam:Waste receptacle  with lid and plastic liner (pedal bin)";
  attrib V331P    format=F00522_. label="Family planning exam:Other waste receptacle";
  attrib V331Q    format=F00523_. label="Family planning exam:Alcohol-based hand rub";
  attrib V331R    format=F00524_. label="Family planning exam:Medical masks";
  attrib V331S    format=F00525_. label="Family planning exam:Gowns";
  attrib V331T    format=F00526_. label="Family planning exam:Eye protection";
  attrib V331U    format=F00527_. label="Family planning exam:Guidelines for standard precautions";
  attrib V331V    format=F00528_. label="Family planning:Needle destroyer - CS";
  attrib V331W    format=F00529_. label="Family planning:Methylated spirit and glycirin 70:30 - CS";
  attrib V333B    format=F00530_. label="Other equip:Manual blood pressure apparatus";
  attrib V333C    format=F00531_. label="Other equip:Stethoscope";
  attrib V333D    format=F00532_. label="Other equip:Digital blood pressure apparatus";
  attrib V333E    format=F00533_. label="Other equip:Examination light (flashlight OK)";
  attrib V333F    format=F00534_. label="Other equip:Examination bed or table - CS";
  attrib V333G    format=F00535_. label="Other equip:Goose lamp - CS";
  attrib V334A    format=F00536_. label="IUD/Implant:Sterile gloves";
  attrib V334B    format=F00537_. label="IUD/Implant:Antiseptic solution";
  attrib V334C    format=F00538_. label="IUD/Implant:Sponge-holding forceps";
  attrib V334D    format=F00539_. label="IUD/Implant:Gauze pad/cotton wool";
  attrib V335A    format=F00540_. label="IUD:Vaginal speculum small";
  attrib V335B    format=F00541_. label="IUD:Vaginal speculum:medium";
  attrib V335C    format=F00542_. label="IUD:Vaginal speculum large";
  attrib V335E    format=F00543_. label="IUD:Tenacula";
  attrib V335F    format=F00544_. label="IUD:Uterine sound";
  attrib V336A    format=F00545_. label="Implant:Local anesthetic";
  attrib V336B    format=F00546_. label="Implant:Sterile /syringe/needle";
  attrib V336C    format=F00547_. label="Implant:cannula/trochar for insertion";
  attrib V336D    format=F00548_. label="Implant:Scalpel with blade";
  attrib V336F    format=F00549_. label="Implant:Sealed implant pack";
  attrib V336G    format=F00550_. label="Implant:Minor surgery kit (artery forceps)";
  attrib V342     format=F00551_. label="Place where equipment (speculum/forceps) processed for reuse";
  attrib V343A    format=F00552_. label="Autoclave used for final FP equipment processing";
  attrib V343B    format=F00553_. label="Dry heat sterilization used for final FP equipment processing";
  attrib V343C    format=F00554_. label="Chlorine solution soak used for final FP equipment processing";
  attrib V343D    format=F00555_. label="Boil or steam used for final FP equipment processing";
  attrib V343E    format=F00556_. label="Soap and water wash used for final FP equipment processing";
  attrib V343F    format=F00557_. label="Soak in other chemical solution for final FP equipment processing";
  attrib V344     format=F00558_. label="NA - HIV RTD kit available in FP service area";
  attrib LV300    format=F00559_. label="Stores contraceptives";
  attrib LV302A   format=F00560_. label="Contraceptive monitoring system - computer or ledge/stock cards";
  attrib LV302B   format=F00561_. label="Contraceptive monitoring system: Stock monitoring method of update";
  attrib LV303A   format=F00562_. label="Avail/valid:Combined oral pill";
  attrib LV303B   format=F00563_. label="Avail/valid:Progesterone only pill";
  attrib LV303D   format=F00564_. label="NA - Avail/valid: (1 mo) Injectable combined";
  attrib LV303E   format=F00565_. label="Avail/valid:(2-3mo) Injectable progestin only (Depo)";
  attrib LV303G   format=F00566_. label="Avail/valid:Implant";
  attrib LV303H   format=F00567_. label="NA - Avail/valid:CS implant";
  attrib LV303I   format=F00568_. label="Avail/valid:Male condom";
  attrib LV303J   format=F00569_. label="NA - Avail/valid:Female condom";
  attrib LV303K   format=F00570_. label="Avail/valid:IUD";
  attrib LV303L   format=F00571_. label="Avail/valid:Emergency contraceptive pill";
  attrib LV303O   format=F00572_. label="NA - Avail/valid:Cycle beads for SDM";
  attrib SLV303A  format=F00573_. label="Out of stock:Combined oral contraceptive pills - CS";
  attrib SLV303B  format=F00574_. label="Out of stock:Progestin only contraceptive pills - CS";
  attrib SLV303E  format=F00575_. label="Out of stock:Progestin-only injectable contraceptives (DEPO)- CS";
  attrib SLV303G  format=F00576_. label="Out of stock:Implant (Zedel)- CS";
  attrib SLV303I  format=F00577_. label="Out of stock:Male condoms - CS";
  attrib SLV303K  format=F00578_. label="Out of stock:Intrauterine contraceptive device (Copper-T)- CS";
  attrib SLV303L  format=F00579_. label="Out of stock:Emergency contraceptives pills - CS";
  attrib LV306A   format=F00580_. label="Contraceptives organized by expiry date";
  attrib LV306B   format=F00581_. label="Any expired observed";
  attrib LV320A   format=F00582_. label="Contraceptives stored in area protected from sunlight";
  attrib LV320C   format=F00583_. label="Storage room in area free of rodent/pest evidence";
  attrib LV320D   format=F00584_. label="Contraceptives stored in FP area off floor";
  attrib LV320E   format=F00585_. label="Contraceptives stored in area protected from water";
  attrib LV320F   format=F00586_. label="Contraceptives stored in area well ventilated";
  attrib SF924A   format=F00587_. label="Time received last routine supply of contraceptives";
  attrib SF924B   format=F00588_. label="Way of determine the qty of contraceptive";
  attrib SF924C   format=F00589_. label="Best system use to determine qty to order";
  attrib SF924D   format=F00590_. label="Time takes bt ordering and receiving";
  attrib SF1308_01 format=F00591_. label="Face sheet (HMIS 3.5)";
  attrib SF1308_02 format=F00592_. label="Health service card (HMIS 1.2)";
  attrib SF1308_03 format=F00593_. label="FP register (HMIS 3.2, 3.3, 3.4)";
  attrib V401A    label="Days/month for antenatal care";
  attrib V406     format=F00594_. label="Routine actions for ANC clients before consult";
  attrib V407A    format=F00595_. label="Routine:Weigh client";
  attrib V407B    format=F00596_. label="Routine:Take BP";
  attrib V407C    format=F00597_. label="Routine:Urine test for protein";
  attrib V407D    format=F00598_. label="Routine:Blood test anemia";
  attrib V407E    format=F00599_. label="Routine:Group Health education";
  attrib V407F    format=F00600_. label="Routine:Malaria RDT";
  attrib V407G    format=F00601_. label="Routine:HIV testing and counseling for pregnant women";
  attrib V407H    format=F00602_. label="Routine:Measure client's height";
  attrib V409B    format=F00603_. label="Routine tx: Counseling about family planning";
  attrib V409C    format=F00604_. label="Routine tx: Counseling about HIV/AIDS";
  attrib V409E    format=F00605_. label="Routine tx: Counseling about preparations for delivery";
  attrib V409F    format=F00606_. label="Routine tx: Counseling on 4 recommended ANC visits";
  attrib V409G    format=F00607_. label="Routine tx: Counseling about ITNs to prevent mosquito bites and malaria";
  attrib V409H    format=F00608_. label="Routine tx: Counseling about breastfeeding";
  attrib V409I    format=F00609_. label="Routine tx: Counseling about newborn care";
  attrib V409J    format=F00610_. label="Routine tx: Counseling on postnatal care visits";
  attrib V410     format=F00611_. label="Routinely treat STI or refer";
  attrib V410A    format=F00612_. label="Routinely treat STI or refer (new codes)";
  attrib V411A    format=F00613_. label="Tetanus offered all days ANC offered";
  attrib V411B    format=F00614_. label="Days/MONTH tetanus toxoid offered";
  attrib V424     format=F00615_. label="Individual records for ANC clients";
  attrib V430SRC  format=F00616_. label="Service where IC information collected";
  attrib V430     format=F00617_. label="Privacy for antenatal exam";
  attrib V431A1   format=F00618_. label="ANC Exam:Any running water";
  attrib V431C    format=F00619_. label="ANC Exam:Soap";
  attrib V431F    format=F00620_. label="ANC Exam:Sharps container";
  attrib V431G    format=F00621_. label="ANC Exam:Clean/sterile latex gloves";
  attrib V431J    format=F00622_. label="ANC Exam:Disinfectant";
  attrib V431L    format=F00623_. label="ANC Exam:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V431O    format=F00624_. label="ANC Exam:Pedal bin waste receptacle with lid and plastic liner";
  attrib V431P    format=F00625_. label="ANC Exam:Other waste receptacle";
  attrib V431Q    format=F00626_. label="ANC Exam:Alcohol-based hand rub";
  attrib V431R    format=F00627_. label="ANC exam:Medical masks";
  attrib V431S    format=F00628_. label="ANC exam:Gowns";
  attrib V431T    format=F00629_. label="ANC exam:Eye protection";
  attrib V431U    format=F00630_. label="ANC exam:Guidelines for standard precautions";
  attrib V431V    format=F00631_. label="ANC exam:Needle destroyer - CS";
  attrib V431W    format=F00632_. label="ANC exam:Methylated spirit and glycirine 70:30 - CS";
  attrib V432A    format=F00633_. label="NA - ANC medications available:SP/Fansidar";
  attrib V432B    format=F00634_. label="ANC medications available:Iron tablets";
  attrib V432C    format=F00635_. label="ANC medications available:Folic acid tablets";
  attrib V432D    format=F00636_. label="ANC medications available:Combined iron and folic acid tablets";
  attrib V432E    format=F00637_. label="ANC medications available:Tetanus Toxoid vaccine";
  attrib V432F    format=F00638_. label="ANC medications available:ITN mosquito bed net";
  attrib V432G    format=F00639_. label="ANC medications available:Albendazole - CS";
  attrib V433B    format=F00640_. label="ANC exam:Manual blood pressure apparatus";
  attrib V433C    format=F00641_. label="ANC exam:Stethoscope";
  attrib V433D    format=F00642_. label="ANC exam:Fetal stethoscope";
  attrib V433F    format=F00643_. label="NA - ANC exam:Infant scale";
  attrib V433G    format=F00644_. label="ANC exam:Adult weighing scale";
  attrib V433H    format=F00645_. label="ANC exam:Digital BP apparatus";
  attrib V433I    format=F00646_. label="ANC exam:Examination light (flashlight OK)";
  attrib V433J    format=F00647_. label="ANC exam:Examination bed/table - CS";
  attrib V433K    format=F00648_. label="ANC exam:Measuring tape for fundal height - CS";
  attrib V433L    format=F00649_. label="ANC exam:Thermometer - CS";
  attrib V434A    format=F00650_. label="Protocols:National guidelines for ANC";
  attrib V434B    format=F00651_. label="NA - Protocols:IPT guidelines";
  attrib V434X    format=F00652_. label="Protocols:Other guidelines for ANC";
  attrib V435A    format=F00653_. label="ANC equipment: Examination bed or couch";
  attrib V435B    format=F00654_. label="ANC equipment: Tape measure for fundal height";
  attrib V460     format=F00655_. label="Visual aids for client education related to pregnancy or ANC care";
  attrib V461A    format=F00656_. label="Provision as part of routine ANC: Iron supplementation";
  attrib V461B    format=F00657_. label="Provision as part of routine ANC: Folic acid supplementation";
  attrib V461C    format=F00658_. label="NA - Provision as part of routine ANC: Intermittent Preventive treatment (IPT) for malaria";
  attrib V461D    format=F00659_. label="Provision as part of routine ANC: Tetanus toxoid vaccination";
  attrib V461E    format=F00660_. label="Provision as part of routine ANC: Albendazole - CS";
  attrib V461F    format=F00661_. label="Provision as part of routine ANC: Misoprostol - CS";
  attrib V462A    format=F00662_. label="NA - Test as part of ANC: HIV RDT";
  attrib V462B    format=F00663_. label="Test as part of ANC: Urine protein test";
  attrib V462C    format=F00664_. label="Test as part of ANC: Urine glucose test";
  attrib V462D    format=F00665_. label="Test as part of ANC: Any rapid test for hemoglobin";
  attrib V462E    format=F00666_. label="Test as part of ANC: Syphilis RDT";
  attrib V462F    format=F00667_. label="Test as part of ANC: Blood glucose test - CS";
  attrib V462G    format=F00668_. label="Test as part of ANC: Blood grouping - CS";
  attrib V462H    format=F00669_. label="Test as part of ANC: Urine pregnancy test - CS";
  attrib SF1401A  label="Number of days ANC outreach clinic conducted";
  attrib SF1403A  label="Number of days Td vaccination services available";
  attrib SF1406A1 format=F00670_. label="RDT kit available at ANC: Determine";
  attrib SF1406A2 format=F00671_. label="RDT kit available at ANC: Unigold";
  attrib SF1406A3 format=F00672_. label="RDT kit available at ANC: Statpack";
  attrib SF1406A4 format=F00673_. label="RDT kit available at ANC: Tridot";
  attrib SF1406A5 format=F00674_. label="RDT kit available at ANC: OTHER";
  attrib SF1408A  format=F00675_. label="ANC providers routinely dx,tx HIV";
  attrib SF1418_1 format=F00676_. label="Client records: Maternal and new health card (HMIS 3.5)";
  attrib SF1418_2 format=F00677_. label="Client records: Maternal and new born health register (HMIS 3.6)";
  attrib SF1418_3 format=F00678_. label="Client records: Any other client's health card";
  attrib V501     format=F00679_. label="Number of maternity beds";
  attrib V501A    label="Number of delivery beds/couches";
  attrib V502     format=F00680_. label="Twenty-four hour coverage";
  attrib V503     format=F00681_. label="Schedule of 24-hour staffing";
  attrib V506C    format=F00682_. label="Routine:Kangaroo mother care";
  attrib V507A    format=F00683_. label="Newborn rtn:Suction with catheter";
  attrib V507B    format=F00684_. label="Newborn rtn:Weigh newborn";
  attrib V507C    format=F00685_. label="NA - Newborn rtn:Give full bath within minutes/few hours after birth";
  attrib V507D    format=F00686_. label="NA - Newborn rtn:Give prelacteal liquids";
  attrib V507E    format=F00687_. label="NA - Newborn rtn:Give OPV prior to discharge";
  attrib V507F    format=F00688_. label="Newborn rtn:Give BCG prior to discharge";
  attrib V507G    format=F00689_. label="Newborn rtn:Suction newborn with bulb";
  attrib V507H    format=F00690_. label="Newborn rtn:Delivery to abdomen (skin to skin)";
  attrib V507I    format=F00691_. label="Newborn rtn:Dry and wrap newborns to keep warm";
  attrib V507J    format=F00692_. label="Newborn rtn:Initiate breastfeeding in first hour";
  attrib V507K    format=F00693_. label="Newborn rtn:Complete exam of newborn before discharge";
  attrib V507L    format=F00694_. label="Newborn rtn:Administer Vitamin K to newborn";
  attrib V507M    format=F00695_. label="Newborn rtn:Apply tetracycline eye ointment to both eyes";
  attrib V507N    format=F00696_. label="Newborn rtn:Apply Chlorexidine ointment to umbilical stump - CS";
  attrib V507O    format=F00697_. label="Newborn rtn:Delay full bath after 24hours of birth - CS";
  attrib V507P    format=F00698_. label="Newborn rtn:Administer Vitamin K to preterm babies CS";
  attrib V530SRC  format=F00699_. label="Source for infection control for delivery room";
  attrib V530     format=F00700_. label="Privacy for delivery room";
  attrib V531A1   format=F00701_. label="Delivery:Running water";
  attrib V531C    format=F00702_. label="Delivery:Soap";
  attrib V531F    format=F00703_. label="Delivery:Sharps container";
  attrib V531G    format=F00704_. label="Delivery:Disposable latex gloves";
  attrib V531J    format=F00705_. label="Delivery:Disinfectant";
  attrib V531L    format=F00706_. label="Delivery:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V531O    format=F00707_. label="Delivery:Pedal bin waste receptacle with lid with plastic liner";
  attrib V531P    format=F00708_. label="Delivery:Other waste receptacle";
  attrib V531Q    format=F00709_. label="Delivery:Alcohol-based hand rub";
  attrib V531R    format=F00710_. label="Delivery:Medical masks";
  attrib V531S    format=F00711_. label="Delivery:Gowns";
  attrib V531T    format=F00712_. label="Delivery:Eye protection";
  attrib V531U    format=F00713_. label="Delivery:Guidelines for standard precautions";
  attrib V531V    format=F00714_. label="Delivery:Needle destroyer - CS";
  attrib V531W    format=F00715_. label="Delivery:Methylated spirit and glycirine 70:30 - CS";
  attrib V531XD   format=F00716_. label="Delivery:Suture material with needle";
  attrib V531XE   format=F00717_. label="Delivery:Sterile scissors or blade";
  attrib V531XF   format=F00718_. label="Delivery:Needle holder";
  attrib V531XG   format=F00719_. label="NA - Delivery:Sterile gloves";
  attrib V531XH   format=F00720_. label="Delivery:Delivery pack";
  attrib V531XI   format=F00721_. label="Delivery:Swab holder";
  attrib V531XJ   format=F00722_. label="Delivery:Forceps large";
  attrib V531XK   format=F00723_. label="Delivery:Forceps medium";
  attrib V531XL   format=F00724_. label="Delivery:Cord clamp";
  attrib V531XM   format=F00725_. label="Delivery:Speculum";
  attrib V531XN   format=F00726_. label="Delivery:Episiotomy scissors";
  attrib V531XO   format=F00727_. label="Delivery:Delivery bed";
  attrib V531XP   format=F00728_. label="Delivery:Wrapper (4 pieces) - CS";
  attrib V531XQ   format=F00729_. label="Delivery:NYANO JHOLA (Warm bag) - CS";
  attrib V533B    format=F00730_. label="Other equip:Examination light (flashlight OK)";
  attrib V533C    format=F00731_. label="Other equip:Manual BP apparatus";
  attrib V533D    format=F00732_. label="Other equip:Stethoscope";
  attrib V533E    format=F00733_. label="Other equip:Digital BP apparatus";
  attrib V533F    format=F00734_. label="Other equip:Foetal stethoscope";
  attrib V533G    format=F00735_. label="Other equip:Manual vacuum extractor";
  attrib V533H    format=F00736_. label="Other equip:Vacuum aspirator or D&C kit";
  attrib V533I    format=F00737_. label="Other equip:Thermometer";
  attrib V533J    format=F00738_. label="NA - Other equip:Thermometer for low-body temperature";
  attrib V534A    format=F00739_. label="Medications:IV solution (plasma expanders) with infusion set";
  attrib V534C    format=F00740_. label="Medications:Injectable uterotonic (oxytocin)";
  attrib V534D    format=F00741_. label="Medications:Injectable diazepam";
  attrib V534E    format=F00742_. label="Medications:Injectable magnesium sulfate";
  attrib V534G    format=F00743_. label="Medications:Injectable antibiotic (ceftriaxone)";
  attrib V534I    format=F00744_. label="Medications:Antibiotic eye drops/ointment";
  attrib V534M    format=F00745_. label="Medications:Skin disinfectant (other than chlorhexidine)";
  attrib V534N    format=F00746_. label="Medications:4% chlorhexidine solution (umbilical cord cleansing)";
  attrib V534O    format=F00747_. label="Medications:Hydrazaline";
  attrib V534P    format=F00748_. label="Medications:Nifedipine - CS";
  attrib V534Q    format=F00749_. label="Medications:Mother's milk substitute - CS";
  attrib V534R    format=F00750_. label="Medications:Calcium gluconate - CS";
  attrib V536A    format=F00751_. label="Newborn:Infant resuscitation bag/mask or tube/mask)";
  attrib V536B    format=F00752_. label="Newborn:Incubator";
  attrib V536C    format=F00753_. label="Newborn:Other source of heat for premature infant";
  attrib V536D    format=F00754_. label="Newborn:Infant scale";
  attrib V536E    format=F00755_. label="Newborn:Suction bulb for mucus extraction";
  attrib V536F    format=F00756_. label="Newborn:Suction apparatus with catheter";
  attrib V537A    format=F00757_. label="Blank partograph";
  attrib V537B    format=F00758_. label="NA - Guidelines: (IMPAC) Guidelines for Integrated Management of Pregnancy and Childbirth";
  attrib V537C    format=F00759_. label="NA - Guidelines: (CEmOC) National guidelines for comprehensive emergency obstetric care";
  attrib V537D    format=F00760_. label="NA - Guidelines:Guidelines for management of pre-term labor";
  attrib V537E    format=F00761_. label="NA - Guidelines: (BEmOC) National guidelines for basic emergency obstetric care";
  attrib V545     format=F00762_. label="Review maternal/newborn deaths/near-misses";
  attrib V546A    format=F00763_. label="Frequency (every .. weeks):Maternal death/near-miss reviews";
  attrib V546B    format=F00764_. label="Frequency (every .. weeks):Neonatal death/near-miss reviews";
  attrib V554A    format=F00765_. label="Ever or past 3 months:Used parental oxytocic drugs";
  attrib V554B    format=F00766_. label="Ever or past 3 months:Used parental anti-convulsants for hypertension";
  attrib V554C    format=F00767_. label="Ever or past 3 months:Used manual placental removal";
  attrib V554D    format=F00768_. label="Ever or past 3 months:Used Parenteral antibiotics";
  attrib V554E    format=F00769_. label="Ever or past 3 months:Assisted vaginal delivery";
  attrib V554F    format=F00770_. label="Ever or past 3 months:Removal of retained products after delivery";
  attrib V554G    format=F00771_. label="Ever or past 3 months:Neonatal resuscitation";
  attrib V554H    format=F00772_. label="Ever or past 3 months:Corticosteroids for pre-term labor";
  attrib V554I    format=F00773_. label="Ever or past 3 months:Blood transfusion";
  attrib V554J    format=F00774_. label="Ever or past 3 months:Caesarean sections";
  attrib V554K    format=F00775_. label="Ever or past 3 months:Comprehensive abortion care (CAC) by MVA";
  attrib V554L    format=F00776_. label="Ever or past 3 months:Medical abortion";
  attrib V557C    format=F00777_. label="C-Sect:Anesthesia giving set";
  attrib V557E    format=F00778_. label="C-Sect:Tubings and connectors for endotracheal tube";
  attrib V557F    format=F00779_. label="C-Sect:Oropharyngeal airway (adult)";
  attrib V557G    format=F00780_. label="C-Sect:Oropharyngeal airway (pediatric)";
  attrib V557H    format=F00781_. label="C-Sect:Magills forceps - adult";
  attrib V557I    format=F00782_. label="C-Sect:Magills forceps - pediatric";
  attrib V557J    format=F00783_. label="C-Sect:Endotracheal tube cuffed sizes 3.0-5.0";
  attrib V557K    format=F00784_. label="C-Sect:Endotracheal tube cuffed sizes 5.5-9.0";
  attrib V557L    format=F00785_. label="C-Sect:Intubating stylet";
  attrib V557M    format=F00786_. label="C-Sect:Spinal needle";
  attrib V558     format=F00787_. label="C-section provider present/on-call 24 hrs";
  attrib V559     format=F00788_. label="Anesthetist present/on call 24 hrs";
  attrib V567     format=F00789_. label="Method for Kangaroo Mother Care for low birth weight babies";
  attrib V568     format=F00790_. label="Use of partographs";
  attrib V569     format=F00791_. label="HIV RDT available in service site";
  attrib V570     format=F00792_. label="PMTCT services provided at delivery site";
  attrib SF1605   format=F00793_. label="Guidelines: National medical standard vol III (NMH Vol III)- CS";
  attrib SF1607   format=F00794_. label="Guidelines: RH clinical guidelines - CS";
  attrib SF1617A  format=F00795_. label="Maternal or new born death form";
  attrib SF1619A  format=F00796_. label="Time elapses before amaternal deaths reviews";
  attrib SF1625AB format=F00797_. label="Time taken bt ordering and receiving EOC drugs";
  attrib SF16281  format=F00798_. label="RDT kit available at PMTCT during delivery: Determine";
  attrib SF16282  format=F00799_. label="RDT kit available at PMTCT during delivery: Unigold";
  attrib SF16283  format=F00800_. label="RDT kit available at PMTCT during delivery: Statpack";
  attrib SF16284  format=F00801_. label="RDT kit available at PMTCT during delivery: Tridot";
  attrib SF16285  format=F00802_. label="RDT kit available at PMTCT during delivery: OTHER";
  attrib V600A    format=F00803_. label="STIs diagnosed/ treated in this service";
  attrib V601A    label="Number days/month offered";
  attrib V624     format=F00804_. label="Individual records for STI clients";
  attrib V625A    format=F00805_. label="Visual aid: About STIs";
  attrib V625B    format=F00806_. label="Visual aid: About HIV/AIDS";
  attrib V625C    format=F00807_. label="Visual aid: Male condom model for demonstration";
  attrib V625D    format=F00808_. label="Visual aid: Posters on STIs";
  attrib V625E    format=F00809_. label="Visual aid: Posters on HIV/AIDS";
  attrib V625F    format=F00810_. label="NA - Visual aid: Female condom model for demonstration";
  attrib V625G    format=F00811_. label="Visual aid: Visual aid for cervical cancer";
  attrib V626A    format=F00812_. label="Client take home: About STIs";
  attrib V626B    format=F00813_. label="Client take home: About HIV/AIDS";
  attrib V626C    format=F00814_. label="Client take home: Male condoms for client";
  attrib V626D    format=F00815_. label="NA - Client take home: Female condoms for client";
  attrib V626E    format=F00816_. label="Client take home: About cervical cancer";
  attrib V626F    format=F00817_. label="Client take home: Male condom IEC materials";
  attrib V626G    format=F00818_. label="NA - Client take home: Female condom IEC materials";
  attrib V627A    format=F00819_. label="Standards/protocols:National guidelines for diagnosis and treatment of STIs";
  attrib V627X    format=F00820_. label="NA - Standards/protocols:Other guidelines for the diagnosis and treatment of STIs";
  attrib V628     format=F00821_. label="All STI clients are referred for HIV test";
  attrib V630SRC  format=F00822_. label="Source for infection control for STI service";
  attrib V630     format=F00823_. label="Privacy for STI exam";
  attrib V631A1   format=F00824_. label="STI Exam:Any (other) running water";
  attrib V631C    format=F00825_. label="STI Exam:Soap";
  attrib V631F    format=F00826_. label="STI Exam:Sharps container";
  attrib V631G    format=F00827_. label="STI Exam:Clean/sterile latex gloves";
  attrib V631J    format=F00828_. label="STI Exam:Disinfectant";
  attrib V631L    format=F00829_. label="STI Exam:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V631O    format=F00830_. label="STI Exam:Pedal bin waste receptacle with lid (pedal bin) and plastic liner";
  attrib V631P    format=F00831_. label="STI Exam:Other waste receptacle";
  attrib V631Q    format=F00832_. label="STI Exam:Alcohol-based hand rub";
  attrib V631R    format=F00833_. label="STI Exam:Medical masks";
  attrib V631S    format=F00834_. label="STI Exam:Gowns";
  attrib V631T    format=F00835_. label="STI Exam:Eye protection";
  attrib V631U    format=F00836_. label="STI Exam:Guidelines for standard precautions";
  attrib V631V    format=F00837_. label="STI Exam:Needle destroyer - CS";
  attrib V631W    format=F00838_. label="STI Exam:Methylated spirit and glycirine 70:30 - CS";
  attrib V640A    format=F00839_. label="Method of dx of STIs: Syndromic approach only";
  attrib V640B    format=F00840_. label="Method of dx of STIs: Etiologic only";
  attrib V640C    format=F00841_. label="NA - Method of dx of STIs:Clinical judgment";
  attrib V640D    format=F00842_. label="Method of dx of STIs:Both syndromic and etiologic";
  attrib V640E    format=F00843_. label="Method of dx of STIs:Clinical diagnosis only - CS";
  attrib V640F    format=F00844_. label="Method of dx of STIs:Both clinical diagnosis and etiologic - CS";
  attrib V642     format=F00845_. label="Performs partner notification/follow-up";
  attrib V642A    format=F00846_. label="Performs partner notification/follow-up";
  attrib V650     format=F00847_. label="HIV RDT available in service site";
  attrib SF18091  format=F00848_. label="RDT kit available at STI: Determine";
  attrib SF18092  format=F00849_. label="RDT kit available at STI: Unigold";
  attrib SF18093  format=F00850_. label="RDT kit available at STI: Statpack";
  attrib SF18094  format=F00851_. label="RDT kit available at STI: Tridot";
  attrib SF18095  format=F00852_. label="RDT kit available at STI: OTHER";
  attrib V805A    format=F00853_. label="HIV Guideline present:How to conduct HIV test";
  attrib V805B    format=F00854_. label="HIV Guideline present:Confidentiality and disclosure of HIV test results";
  attrib V805X    format=F00855_. label="HIV Guideline present:Other relevant HIV guidelines";
  attrib V810A    format=F00856_. label="External QA:Proficiency panel";
  attrib V810B    format=F00857_. label="External QA:External inspection/observation of technique";
  attrib V810C    format=F00858_. label="External QA:Send blood for retesting";
  attrib V810X    format=F00859_. label="External QA:Other method";
  attrib V810Y    format=F00860_. label="External QA:No external QA";
  attrib V813     format=F00861_. label="Record of results from external QA";
  attrib V815     format=F00862_. label="Most recent error rate recorded by external QA";
  attrib V820A    format=F00863_. label="Send blood outside for HIV testing:ELISA";
  attrib V820B    format=F00864_. label="Send blood outside for HIV testing:Western Blot";
  attrib V820C    format=F00865_. label="Send blood outside for HIV testing:PCR for EID";
  attrib V820D    format=F00866_. label="Send blood outside for HIV testing:HIV RDT";
  attrib V820E    format=F00867_. label="NA - Send blood outside for HIV testing:CS";
  attrib V820X    format=F00868_. label="Send blood outside for HIV testing:Other";
  attrib V820Y    format=F00869_. label="Send blood outside for HIV testing:No outside testing";
  attrib V821     format=F00870_. label="Record of result of outside HIV testing";
  attrib V821E    format=F00871_. label="NA - Record of result of outside CS Other HIV specifc testing";
  attrib V834A    format=F00872_. label="Send blood out for screening and results:Syphilis";
  attrib V834B    format=F00873_. label="Send blood out for screening and results:Hepatitis B";
  attrib V834C    format=F00874_. label="Send blood out for screening and results:Hepatitis C";
  attrib V834D    format=F00875_. label="Send blood out for screening and results:HIV";
  attrib V834E    format=F00876_. label="NA - Send blood out for screening and results:Malaria";
  attrib V834Y    format=F00877_. label="Send blood out for screening and results:Send no blood out for screening";
  attrib V835A    format=F00878_. label="Send blood/urine out for: Blood chemistries screening and results";
  attrib V835B    format=F00879_. label="Send blood/urine out for:Liver function test screening and results";
  attrib V835C    format=F00880_. label="Send blood/urine out for Urinalysis screening and results";
  attrib V835D    format=F00881_. label="Send blood/urine out for Pregnancy test";
  attrib V835Y    format=F00882_. label="Send blood/urine out for:Send no blood or urine out for screening/testing";
  attrib V836     format=F00883_. label="Send sputum outside for testing/ results returned";
  attrib V837     format=F00884_. label="Register for TB test results";
  attrib V838A    format=F00885_. label="Send blood out for:Gram stain/ results returned";
  attrib V838B    format=F00886_. label="Send blood out for:Indian ink stain/ results returned";
  attrib V838C    format=F00887_. label="Send blood out for:Malaria/ results returned";
  attrib V838D    format=F00888_. label="Send blood out for:Specimen for culture/ results returned";
  attrib V838Y    format=F00889_. label="Send blood out for:Send no blood out for gram staining, India ink staining, malaria, culture";
  attrib V839     format=F00890_. label="Blood transfusions performed";
  attrib V840A    format=F00891_. label="Any HIV tests:HIV including RDT/DBS";
  attrib V840A1   format=F00892_. label="Any HIV LAB tests (See V843)";
  attrib V840A2   format=F00893_. label="Lab tests:HIV RDT";
  attrib V840A3   format=F00894_. label="Lab tests:HIV DBS";
  attrib V840D    format=F00895_. label="Lab tests:Hematology (see V845, V846)";
  attrib V840E    format=F00896_. label="Lab tests:Syphilis (see V847, V848) or RDT";
  attrib V840E1   format=F00897_. label="Lab tests:Syphilis RDT";
  attrib V840E2   format=F00898_. label="Lab tests:Syphilis (see V847, V848) excluding RDT";
  attrib V840F    format=F00899_. label="Lab tests:Any LFT, Blood Glucose, Urine (see V849, V850,V851)";
  attrib V840F1   format=F00900_. label="Lab tests: Blood Glucose (see V849)";
  attrib V840F2   format=F00901_. label="Lab tests:Liver/renal function tests (see V850)";
  attrib V840F3   format=F00902_. label="Lab tests:Urine chemistries/ urine pregnancy test (see V851)";
  attrib V840G    format=F00903_. label="Lab tests:Malaria microscopy or rapid test (see V852)";
  attrib V840G1   format=F00904_. label="Lab tests:Malaria Rapid test";
  attrib V840I    format=F00905_. label="Lab tests:Gram stain (see V855)";
  attrib V840J    format=F00906_. label="Lab tests:Chlamydia (see V856)";
  attrib V840K    format=F00907_. label="Lab tests:TB (see V859)";
  attrib V840L    format=F00908_. label="Lab tests: Blood typing (see V860)";
  attrib V840M    format=F00909_. label="Lab tests: Blood screening in facility or sent out (see V861)";
  attrib V840N    format=F00910_. label="Lab tests: Stool microscopy (see V857)";
  attrib V840O    format=F00911_. label="Lab tests: CD4 tests (see V891, V892)";
  attrib V840P    format=F00912_. label="Lab tests:TB RDT";
  attrib V840Y    format=F00913_. label="Lab tests for HIV:No HIV tests";
  attrib V841A1   format=F00914_. label="Equipment used & available:Light Microscope+";
  attrib V841A2   format=F00915_. label="Equipment used & available:Electron Microscope+";
  attrib V841B    format=F00916_. label="Equipment used & available:Refrigerator+";
  attrib V841C    format=F00917_. label="Equipment used & available:Incubator+";
  attrib V841D    format=F00918_. label="Equipment used & available:Test tubes";
  attrib V841E    format=F00919_. label="Equipment used & available:Glass slides and covers";
  attrib V841F    format=F00920_. label="Equipment used & available:Culture medium";
  attrib V841G    format=F00921_. label="Equipment used & available:Centrifuge for CSF microbiology+";
  attrib V842A1   format=F00922_. label="Working order:Light Microscope";
  attrib V842A2   format=F00923_. label="Working order:Electron Microscope";
  attrib V842B    format=F00924_. label="Working order:Refrigerator";
  attrib V842C    format=F00925_. label="Working order:Incubator";
  attrib V842G    format=F00926_. label="Working order:Centrifuge for CSF microbiology";
  attrib V843A    format=F00927_. label="Test conducted & items available:ELISA/CLIA scanner/ reader+";
  attrib V843B    format=F00928_. label="Test conducted & equipment available:CD4 Flow cytometer analyzer (FACS count machine)+";
  attrib V843C    format=F00929_. label="Test conducted & items available:Dynabeads with vortex mixer+";
  attrib V843E    format=F00930_. label="Test items available:Western Blot test assay";
  attrib V843F    format=F00931_. label="Test conducted & items available:PCR for viral load+";
  attrib V843G    format=F00932_. label="Test conducted & items available:PCR for DNA-EID+";
  attrib V843H    format=F00933_. label="Test conducted & items available:Washer for ELISA scanner/reader+";
  attrib V843I    format=F00934_. label="Test conducted & items available:Assay for ELISA scanner/reader+";
  attrib V843J    format=F00935_. label="Test conducted & items available:Incubator";
  attrib V843K    format=F00936_. label="Test conducted & equipment available:CD4 Fluorescent cartridge analyzer/ PIMA analyzer+";
  attrib V843L    format=F00937_. label="Test items available:CD4 Reagent kits for flow cytometer analyzer";
  attrib V843M    format=F00938_. label="Test items available:CD4 Cartridges for fluorescent cartridge analyzer";
  attrib V843N    format=F00939_. label="Test items available:CD4 Rapid CD4 test strips";
  attrib V844A    format=F00940_. label="Working order:ELISA/EIA scanner/ reader";
  attrib V844B    format=F00941_. label="Working order:CD4 Flow cytometer analyzer (FACS count machine)";
  attrib V844C    format=F00942_. label="Working order:Dynabeads with vortex mixer";
  attrib V844F    format=F00943_. label="Working order:PCR for viral load";
  attrib V844G    format=F00944_. label="Working order:PCR for DNA-EID";
  attrib V844H    format=F00945_. label="Working order:Washer for ELISA scanner/reader";
  attrib V844I    format=F00946_. label="Unexpired:Assay for ELISA scanner/reader";
  attrib V844J    format=F00947_. label="Working order:Incubator";
  attrib V844K    format=F00948_. label="Working order:CD4 Cartridges for fluorescent cartridge analyzer";
  attrib V844N    format=F00949_. label="Unexpired: CD4 Rapid test strips";
  attrib V845A    format=F00950_. label="Test conducted and items available:Hematology analyzer (total lymphocyte/FBC/platelet count)+";
  attrib V845B    format=F00951_. label="Test conducted and items available:HemoCue+";
  attrib V845C    format=F00952_. label="Test conducted and items available:Colorimeter/ hemoglobinometer+";
  attrib V845D    format=F00953_. label="Test items available:Colorimeter Drabkin solution";
  attrib V845G    format=F00954_. label="NA - Test items available:Hemoglobin litmus paper";
  attrib V845H    format=F00955_. label="Test items available:Hemocue microcuvette with valid expiration date";
  attrib V845I    format=F00956_. label="Test items available:Colorimeter Pipette (for measuring blood volume)";
  attrib V845J    format=F00957_. label="Test items available:Sahli's hemoglobinometer - CS";
  attrib V845K    format=F00958_. label="Test items available:Other - CS";
  attrib V846A    format=F00959_. label="Working order:Hematology analyzer/Coulter";
  attrib V846B    format=F00960_. label="Working order:Hemoglobinometer/hemocue";
  attrib V846C    format=F00961_. label="Working order:Colorimeter/ hemoglobinometer+";
  attrib V846D    format=F00962_. label="NA - Working order:Colorimeter/spectroscope";
  attrib V846H    format=F00963_. label="Working order:Microcuvette - CS";
  attrib V847A    format=F00964_. label="Test conducted and items available:Syphilis VDRL";
  attrib V847B    format=F00965_. label="Test conducted and items available:Syphilis Rotator/shaker+";
  attrib V847C    format=F00966_. label="Test conducted and items available:Syphilis RPR";
  attrib V847D    format=F00967_. label="Test conducted and items available:Syphilis PCR for STIs (CTN)";
  attrib V847E    format=F00968_. label="Test conducted and items available:Syphilis (TPHA) Treponema Pallidum Hemaglutination Assay";
  attrib V848B    format=F00969_. label="Working order:Syphilis Rotator/shaker";
  attrib V849A    format=F00970_. label="Test conducted & equipment available:Blood chem analyzer (serum creatinine, glucose, LFT)+";
  attrib V849B    format=F00971_. label="Test conducted & equipment available: Glucometer+";
  attrib V849B1   format=F00972_. label="Test conducted & equipment available: Glucometer test strips+";
  attrib V850A    format=F00973_. label="Working order:Blood chem analyzer (serum creatinine, glucose, LFT)";
  attrib V850B    format=F00974_. label="Working order:Glucometer";
  attrib V850B1   format=F00975_. label="Unexpired:Glucometer test strips";
  attrib V851     format=F00976_. label="Test items available: Pregnancy test";
  attrib V851A    format=F00977_. label="Test items available:Urine protein dip sticks";
  attrib V851B    format=F00978_. label="Test items available:Urine glucose dip sticks";
  attrib V852A    format=F00979_. label="Test items available:Malaria Giemsa stain";
  attrib V852B    format=F00980_. label="Test items available:Malaria Field stain";
  attrib V852C    format=F00981_. label="Test items available:Malaria RDT (See V840G1)";
  attrib V852D    format=F00982_. label="Test items available:Malaria Acridine Orange (AO microscope & Acridine orange stain)";
  attrib V855A    format=F00983_. label="Test items available:Crystal violet gram/Gentian violet stain";
  attrib V855B    format=F00984_. label="Test items available:Lugol's iodine gram stain/ solution";
  attrib V855C    format=F00985_. label="Test items available:Acetone/ Acetone-alcohol gram stain";
  attrib V855D    format=F00986_. label="Test items available:Neutral red/carbol fuchsin/other counter stain";
  attrib V856A    format=F00987_. label="Test items available:Chlamydia - Giemsa stain";
  attrib V856B    format=F00988_. label="Test items available:Chlamydia -  PCR";
  attrib V857B    format=F00989_. label="Test items available:Stool microscopy formal saline (for concentration method)";
  attrib V857C    format=F00990_. label="Test items available:Stool microscopy Lugol's iodine/Lugol's solution";
  attrib V857D    format=F00991_. label="Test items available:Stool microscopy normal saline (for direct microscopy)";
  attrib V859A    format=F00992_. label="Test items available:TB AFB Ziehl-Neelson test";
  attrib V859B    format=F00993_. label="Test items available:TB RDT conducted (see V840P)";
  attrib V859C    format=F00994_. label="Test items available:TB culture/growth medium (ex: MGIT 960)";
  attrib V859D    format=F00995_. label="Test items available:TB Carbol-Fuchsin";
  attrib V859E    format=F00996_. label="Test items available:TB 20-25% sulphuric acid or acid alcohol";
  attrib V859F    format=F00997_. label="Test items available:TB Methylene Blue";
  attrib V859G    format=F00998_. label="Test conducted & equipment available:TB Fluorescence Microscope+";
  attrib V859GG   format=F00999_. label="Working order: TB Fluorence microscope";
  attrib V859H    format=F01000_. label="Test items available:TB Biosafety hood/ cabinet";
  attrib V859I    format=F01001_. label="Test items available:TB Auramine stain for Fluorescence Microscope";
  attrib V860A    format=F01002_. label="Test items available:Blood typing Anti-A reagent (valid expiration date)";
  attrib V860B    format=F01003_. label="Test items available:Blood typing Anti-B reagent (valid expiration date)";
  attrib V860C    format=F01004_. label="Test items available:Blood typing Anti-A,B reagent (valid expiration date)";
  attrib V860D    format=F01005_. label="Test items available:Blood typing Anti-D reagent (valid expiration date)";
  attrib V860F    format=F01006_. label="Test items available:Blood typing Coomb's reagent (valid expiration date)";
  attrib V861A    format=F01007_. label="Blood for transfusion ever screened for:Syphilis";
  attrib V861B    format=F01008_. label="Blood for transfusion ever screened for:Hepatitis B";
  attrib V861C    format=F01009_. label="Blood for transfusion ever screened for:Hepatitis C";
  attrib V861D    format=F01010_. label="Blood for transfusion ever screened for:HIV";
  attrib V861E    format=F01011_. label="NA - Blood for transfusion ever screened for:Malaria";
  attrib V863     format=F01012_. label="Performs diagnostic X-rays, ultrasound, computerized tomography";
  attrib V864A    format=F01013_. label="Equipment available:X-ray machine+";
  attrib V864B    format=F01014_. label="Equipment available:Unexposed film for X-rays+";
  attrib V864C    format=F01015_. label="Equipment available:Ultrasound equipment+";
  attrib V864D    format=F01016_. label="Equipment available:CT scan+";
  attrib V864F    format=F01017_. label="Equipment available:Digital X-ray not requiring film+";
  attrib V864E    format=F01018_. label="Equipment available:MRI - CS";
  attrib V865A    format=F01019_. label="Working order:X-ray machine";
  attrib V865B    format=F01020_. label="Working order:Unexposed film for X-rays";
  attrib V865C    format=F01021_. label="Working order:Ultrasound";
  attrib V865D    format=F01022_. label="Working order:CT scan";
  attrib V865F    format=F01023_. label="Working order:Digital X-ray not requiring film";
  attrib V865E    format=F01024_. label="Working order:MRI - CS";
  attrib V866A1   format=F01025_. label="Lab: Any running water";
  attrib V866C    format=F01026_. label="Lab:Soap";
  attrib V866F    format=F01027_. label="Lab:Sharps container";
  attrib V866G    format=F01028_. label="Lab:Clean/sterile latex gloves";
  attrib V866J    format=F01029_. label="Lab:Disinfectant";
  attrib V866L    format=F01030_. label="Lab:Autodestruct syringes";
  attrib V866O    format=F01031_. label="Lab:Pedal bin waste receptacle with lid and plastic liner";
  attrib V866P    format=F01032_. label="Lab:Other waste receptacle";
  attrib V866Q    format=F01033_. label="Lab:Alcohol-based hand rub";
  attrib V866R    format=F01034_. label="Lab:Medical masks";
  attrib V866S    format=F01035_. label="Lab:Gowns";
  attrib V866T    format=F01036_. label="Lab:Eye protection";
  attrib V866U    format=F01037_. label="Lab:Guidelines for standard precautions";
  attrib V866V    format=F01038_. label="Lab:Needle destroyer - CS";
  attrib V866W    format=F01039_. label="Lab:Methylated spirit and glycirin 70:30 - CS";
  attrib V870A    format=F01040_. label="NA - Malaria RDT kit brand A Paracheck";
  attrib V870B    format=F01041_. label="NA - Malaria RDT kit brand B Care Start";
  attrib V870C    format=F01042_. label="NA - Malaria RDT kit brand C First Response";
  attrib V870D    format=F01043_. label="NA - Malaria RDT kit brand D SD Bioline";
  attrib V870E    format=F01044_. label="NA - Malaria RDT kit brand E";
  attrib V870F    format=F01045_. label="NA - Malaria RDT kit brand F";
  attrib V870X    format=F01046_. label="NA - Malaria RDT kit brand X (OTHER)";
  attrib V871     format=F01047_. label="Training manual/job aid for using Malaria RDT";
  attrib V872     format=F01048_. label="Availability of sputum container";
  attrib V873A    format=F01049_. label="QC (internal or external):Type followed by facility";
  attrib V873B    format=F01050_. label="QC (internal or external):Records of results from QC procedures";
  attrib V874A    format=F01051_. label="Source of blood that is transfused: National Blood Bank";
  attrib V874B    format=F01052_. label="Source of blood that is transfused: Regional Blood Bank";
  attrib V874C    format=F01053_. label="NA - Source of blood that is transfused: Relatives donating directly";
  attrib V874D    format=F01054_. label="Source of blood that is transfused: District Blood Bank - CS";
  attrib V874E    format=F01055_. label="Source of blood that is transfused: Hospital Blood Bank - CS";
  attrib V874X    format=F01056_. label="Source of blood that is transfused: Other";
  attrib V875     format=F01057_. label="Facility has run out of blood for more than a day in past 3 months";
  attrib V876A    format=F01058_. label="Refrigerator available for blood storage in service area";
  attrib V876B    format=F01059_. label="Temperature in the blood storage refrigerator or freezer";
  attrib V877     format=F01060_. label="Guidelines on appropriate use of blood and safe transfusion practices";
  attrib SF802D   format=F01061_. label="CBC test on site";
  attrib SF802EB1 format=F01062_. label="CBC test item available: Hemocytometer (Glass slide)";
  attrib SF802EB2 format=F01063_. label="CBC test item available: Cover glass for Hemocytometer";
  attrib SF802EB3 format=F01064_. label="CBC test item available: Microscope for Hemocytometer";
  attrib SF802EC1 format=F01065_. label="Working order: Hemocytometer (Glass slide)";
  attrib SF802EC3 format=F01066_. label="Working order: Microscope for Hemocytometer";
  attrib SF803A   format=F01067_. label="Type of D4 testing service available";
  attrib SF8081   format=F01068_. label="RDT kit available: Determine";
  attrib SF8082   format=F01069_. label="RDT kit available: Unigold";
  attrib SF8083   format=F01070_. label="RDT kit available: Statpack";
  attrib SF8084   format=F01071_. label="RDT kit available: Tridot";
  attrib SF8085   format=F01072_. label="RDT kit available: OTHER";
  attrib SF809A   format=F01073_. label="Purpose of DBS card/paper is used";
  attrib SF835A   format=F01074_. label="Bilirubin test";
  attrib SF835BC  format=F01075_. label="Bilirubino meter/Colorimeter";
  attrib SF835D   format=F01076_. label="Bilirubino meter/Colorimeter is working order";
  attrib SF840X   format=F01077_. label="Test equipment for Parasite";
  attrib SF847C   format=F01078_. label="Test for diagnosis of Kalaazar/Leishmaniasis";
  attrib SF847DE  format=F01079_. label="Lab tests:RDT for Kalaazar";
  attrib SF863A   format=F01080_. label="Type of TB Gene expert service available";
  attrib SF873AB  format=F01081_. label="Sputum microscopy manual available";
  attrib SF874A   format=F01082_. label="Any tests for Hepatitis B";
  attrib SF874BC  format=F01083_. label="Use DT for detection of Hep B suface antigen";
  attrib SF874D   format=F01084_. label="Any test for Hepatitis C";
  attrib SF874EF  format=F01085_. label="Use test kit to diagnose Hep C";
  attrib V902A    format=F01086_. label="General, anti-malarial, IV solutions stock monitoring system - computer or ledge/stock cards";
  attrib V902B    format=F01087_. label="General medications, antimalarials, and supplies: Stock monitoring method of update";
  attrib V903_02  format=F01088_. label="General med:Aspirin capsules/ tablets";
  attrib V903_05  format=F01089_. label="General med:Albendazole";
  attrib V903_06  format=F01090_. label="General med:Amoxicillin tablet";
  attrib V903_08  format=F01091_. label="General med:Amoxicillin/Clavulinate (Augmentin)";
  attrib V903_10  format=F01092_. label="General med:Ampicillin injection";
  attrib V903_13  format=F01093_. label="General med:Azithromycin tablets";
  attrib V903_14  format=F01094_. label="General med:Azithromycin syrup/suspension";
  attrib V903_15  format=F01095_. label="General med:Amoxicillin syrup/suspension";
  attrib V903_16  format=F01096_. label="General med:Amitriptyline (Depression)";
  attrib V903_17  format=F01097_. label="General med:Amlodipine tablets (CCB for hypertension)";
  attrib V903_18  format=F01098_. label="General med:Atenolol (Beta-blocker)";
  attrib V903_19  format=F01099_. label="NA - General med:A.C.E inhibitor Other than Enalapril";
  attrib V903_20  format=F01100_. label="General med:CHLORAMPHENICOL (Caps/Applicap)CS - A";
  attrib V904_03  format=F01101_. label="General med:Beclomethasone inhaler";
  attrib V904_04  format=F01102_. label="General med:Betamethasone injection";
  attrib V904_05  format=F01103_. label="NA - General med:Benzathine Benzelpenicillin powder for injection (repeated in V916_01)";
  attrib V904_06  format=F01104_. label="General med:CLOXACILLIN (Tabs/Caps)CS - B";
  attrib V905_03  format=F01105_. label="General med:Ceftriaxone injection";
  attrib V905_08  format=F01106_. label="NA - General med:Ciprofloxacin";
  attrib V905_14  format=F01107_. label="General med:Co-trimoxazole tabs";
  attrib V905_18  format=F01108_. label="General med:Co-trimoxazole suspension";
  attrib V905_19  format=F01109_. label="General med:Cefixime tabs";
  attrib V905_20  format=F01110_. label="General med:Captopril";
  attrib V905_21  format=F01111_. label="General med:Calcium gluconate injection";
  attrib V905_22  format=F01112_. label="General med:NEOMYCIN OINTMENT  CS - C";
  attrib V906_03  format=F01113_. label="General med:Dexamethasone injection";
  attrib V906_07  format=F01114_. label="General med:Diazepam injection";
  attrib V906_08  format=F01115_. label="General med:Diclofenac tablets  (strong pain medicine)";
  attrib V906_11  format=F01116_. label="General med:Doxyclycline";
  attrib V906_12  format=F01117_. label="General med:CIPROFLOXACIN INFUSION  CS - D";
  attrib V907_03  format=F01118_. label="General med:Erythromycin tabs";
  attrib V907_04  format=F01119_. label="General med:Erythromycin suspension";
  attrib V907_05  format=F01120_. label="NA - General med:Enalapril capsule (A.C.E inhibitor)";
  attrib V907_06  format=F01121_. label="General med:Epinephrine injection";
  attrib V907_07  format=F01122_. label="NA - General med:Eye ointment for newborn antibiotic other than tetracycline";
  attrib V907_08  format=F01123_. label="General med:CIPROFLOXACIN EAR DROP  CS - E";
  attrib V908_02  format=F01124_. label="General med:Fluconazole";
  attrib V908_03  format=F01125_. label="General med:Folic acid tablets";
  attrib V908_04  format=F01126_. label="General med:Furosemide (diuretic)";
  attrib V908_05  format=F01127_. label="General med:CIPROFLOXACIN EYE DROP CS - F";
  attrib V909_02  format=F01128_. label="General med:Gentamycin injection";
  attrib V909_04  format=F01129_. label="General med:Glibneclamide (type 2 diabetes)";
  attrib V909_05  format=F01130_. label="General med:Glucose injectable solution";
  attrib V909_06  format=F01131_. label="General med:METOCHLOPRAMIDE TABLETS/INJECTION CS - G";
  attrib V910_08  format=F01132_. label="General med:CHLORPHENIRAMINE TABLETS - CS H";
  attrib V911_02  format=F01133_. label="General med:PHENIRAMINE INJECTION CS I";
  attrib V913_13  format=F01134_. label="General med:CETRIZINE (TABS/SUSPENSION)CS J";
  attrib V914_08  format=F01135_. label="General med:ALUMINIUM HYDROXIDE + MAGNESIUM HYDROXIDE TABLETS (ANTACID)- CS K";
  attrib V915_04  format=F01136_. label="General med:HYOSCINE BUTYLBROMIDE (TABS/INJECTION)CS L";
  attrib V916_09  format=F01137_. label="General med:PHENOBARBITONE TABLETS CS M";
  attrib V917_06  format=F01138_. label="General med:PROMETHAZINE HYDROCHLORIDE TABLETS CS N";
  attrib V918_06  format=F01139_. label="General med:ALPRAZOLAM TABLETS CS O";
  attrib V921_04  format=F01140_. label="General med:CHLORPROMAZINE TABLETS CS P";
  attrib V921_05  format=F01141_. label="General med:DIGOXIN TABLETS - CS";
  attrib V921_06  format=F01142_. label="General med:ALLOPURINOL TABLETS  - CS";
  attrib V921_07  format=F01143_. label="General med:CARBAMAZEPINE TABLETS  - CS";
  attrib V921_08  format=F01144_. label="General med:OXYMETAZOLINE NASAL DROPS  - CS";
  attrib V921_09  format=F01145_. label="General med:ACETAZOLAMIDE TABLETS  - CS";
  attrib V921_10  format=F01146_. label="General med:LEVOTHYROXIN TABLETS  - CS";
  attrib V921_11  format=F01147_. label="General med:AMINOPHYLLINE TABLETS  - CS";
  attrib V910_03  format=F01148_. label="General med:Iron tablets";
  attrib V910_04  format=F01149_. label="General med:Iron + Folic acid tablets";
  attrib V910_06  format=F01150_. label="General med:Insulin injections (Lente) (diabetes)";
  attrib V910_07  format=F01151_. label="General med:Isosorbide Dinitrate";
  attrib V913_01  format=F01152_. label="General med:Magnesium sulphate injection";
  attrib V913_02  format=F01153_. label="General med:Mebendazole";
  attrib V913_04  format=F01154_. label="NA - General med:Metrodonizole IV";
  attrib V913_05  format=F01155_. label="General med:Metrodonizole tablets";
  attrib V913_06  format=F01156_. label="General med:Miconazole (vaginal pessaries)";
  attrib V913_07  format=F01157_. label="General med:Miconazole cream";
  attrib V913_10  format=F01158_. label="General med:Metformin tablets (diabetes)";
  attrib V913_11  format=F01159_. label="General med:Misoprostol tablets";
  attrib V913_12  format=F01160_. label="General med:Metrodonizole injection";
  attrib V914_05  format=F01161_. label="General med:Nystatin (oral suspension)";
  attrib V914_06  format=F01162_. label="General med:Nystatin (vaginal pessaries/cream)";
  attrib V908_06  format=F01163_. label="General med:COTRIMAZOLE SKIN CREAM - CS";
  attrib V908_07  format=F01164_. label="General med:COTRIMAZOLE PESSARY TAB - CS";
  attrib V914_07  format=F01165_. label="NA - General med:Nifedipine tablets";
  attrib V915_01  format=F01166_. label="General med:Oral rehydration salts (ORS) sachets";
  attrib V915_02  format=F01167_. label="General med:Oxytocin or other injectable uterotonic";
  attrib V915_03  format=F01168_. label="General med:Omeprazole";
  attrib V916_01  format=F01169_. label="NA - General med:Penicillin Benzathine Benzyl for injection (repeated in V904_05)";
  attrib V916_03  format=F01170_. label="General med:Penicillin injection";
  attrib V916_06  format=F01171_. label="General med:Prednisolone";
  attrib V916_07  format=F01172_. label="General med:Paracetamol tablets";
  attrib V916_08  format=F01173_. label="General med:Paracetamol syrup";
  attrib V906_09  format=F01174_. label="General med:IBUPROFEN TABLETS";
  attrib V906_10  format=F01175_. label="General med:INDOMETHACIN TABLETS";
  attrib V917_04  format=F01176_. label="General med:Salbutamol inhaler";
  attrib V917_05  format=F01177_. label="General med:Simvastatin (high cholesterol)";
  attrib V918_01  format=F01178_. label="General med:Tetracycline caps";
  attrib V918_02  format=F01179_. label="General med:Tetracycline eye ointment";
  attrib V918_04  format=F01180_. label="General med:Thiazide diuretic";
  attrib V918_05  format=F01181_. label="General med:Tetanus Toxoid vaccine";
  attrib V919_03  format=F01182_. label="General med:Vitamin A capsules";
  attrib V920_02  format=F01183_. label="General med:Zinc tablets";
  attrib V920_03  format=F01184_. label="General med:INJECTION VITAMIN K - CS";
  attrib V920_04  format=F01185_. label="General med:MEDICAL ABORTION COMBI-PACK (MIFEPRISTONE 200mg + MISOPROSTOL 800 microgram vaginal tablets)- CS";
  attrib V921_01  format=F01186_. label="General med:Heparin injection";
  attrib V921_02  format=F01187_. label="General med:Hydrocortisone";
  attrib V922_03  format=F01188_. label="Antimalarial:Fansidar (Sulfadoxine+Pyrimethamine (SP)";
  attrib V922_04  format=F01189_. label="Antimalarial:Quinine tablets";
  attrib V922_05  format=F01190_. label="Antimalarial:Quinine injection";
  attrib V922_11  format=F01191_. label="Antimalarial:Injectable artesunate";
  attrib V922_12  format=F01192_. label="NA - Antimalarial:Artesunate suppositories";
  attrib V922_13  format=F01193_. label="Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 6 tablet/pack";
  attrib V922_14  format=F01194_. label="NA - Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 12 tablet/pack";
  attrib V922_15  format=F01195_. label="NA - Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 18 tablet/pack";
  attrib V922_16  format=F01196_. label="NA - Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 24 tablet/pack";
  attrib V922_17  format=F01197_. label="Antimalarial:Other anti-malarial medicine";
  attrib V922_18  format=F01198_. label="Antimalarial:Chloroquine tablets - CS";
  attrib V922_19  format=F01199_. label="Antimalarial:Primaquine tablets - CS";
  attrib V923_01  format=F01200_. label="Anti-TB:Ethambutol tabs";
  attrib V923_02  format=F01201_. label="Anti-TB:Isoniazid tabs";
  attrib V923_03  format=F01202_. label="Anti-TB:Pyrazinamide";
  attrib V923_04  format=F01203_. label="Anti-TB:Rifampicin";
  attrib V923_05  format=F01204_. label="Anti-TB:Streptomycin injectable";
  attrib V923_06  format=F01205_. label="Anti-TB:Isoniazid + Rifampicin";
  attrib V923_07  format=F01206_. label="Anti-TB:Isoniazid + Rifampicin Pyrazinamide (RHZ) (3FDC)";
  attrib V923_08  format=F01207_. label="Anti-TB:Isoniazid + Ethambutol (EH) (2FDC)";
  attrib V923_09  format=F01208_. label="Anti-TB:Isoniazid+Rifampicin+Pyrazinamide+Ethambutol (4FDC)";
  attrib V923_11  format=F01209_. label="Anti-TB:Isoniazid+Rifampicin+Ethambutol (RHE) (3FDC)";
  attrib V923_12  format=F01210_. label="NA - Anti-TB:CS";
  attrib V924_01  format=F01211_. label="IV solution: Normal saline/ Sodium chloride injectable solution";
  attrib V924_02  format=F01212_. label="IV solution: 5% dextrose-normal saline";
  attrib V924_03  format=F01213_. label="IV solution: Ringers Lactate";
  attrib V924_05  format=F01214_. label="NA - IV solution: CS";
  attrib V926     format=F01215_. label="Expired medications observed: General, anti-malarial, IV solutions";
  attrib V927     format=F01216_. label="Medications organized by date of expiration: General, anti-malarial, IV solutions";
  attrib V928A    format=F01217_. label="Medication stored protected from sun: General, anti-malarial, IV solutions";
  attrib V928B1   format=F01218_. label="Medication stored off floor: General, anti-malarial, IV solutions";
  attrib V928B2   format=F01219_. label="Medication protected from water: General, anti-malarial, IV solutions";
  attrib V928C    format=F01220_. label="Medication room clean of evidence of rodents/pests: General, anti-malarial, IV solutions";
  attrib V928D    format=F01221_. label="Medication room well ventilated: General, anti-malarial, IV solutions";
  attrib V936     format=F01222_. label="Stock antiretroviral medications";
  attrib V946_06  format=F01223_. label="Other supply: Infusion set for IV solution";
  attrib V946_07  format=F01224_. label="Other supply: cannula for administering IV fluids (24G)";
  attrib V946_09  format=F01225_. label="Other supply: Latex gloves";
  attrib V946_12  format=F01226_. label="Other supply: Disinfecting solution";
  attrib V946_13  format=F01227_. label="Other supply: Hand washing soap";
  attrib V946_16  format=F01228_. label="Other supply: Single-use disposable syringes with needles or auto-disable syringes with needles";
  attrib V946_17  format=F01229_. label="Other supply: Alcohol-based hand rub";
  attrib V946_18  format=F01230_. label="Other supply: Insecticide treated mosquito nets (ITNs)";
  attrib V946_19  format=F01231_. label="NA - Other supply: CS";
  attrib V950A    format=F01232_. label="TB medications: Stock monitoring system - computer or ledge/stock cards";
  attrib V950B    format=F01233_. label="TB medications: Stock monitoring system updated daily";
  attrib V951     format=F01234_. label="Expired medications observed: TB meds";
  attrib V952     format=F01235_. label="Medications organized by date of expiration: TB meds";
  attrib V953A    format=F01236_. label="Medication stored protected from sun: TB meds";
  attrib V953B1   format=F01237_. label="Medication stored off floor: TB meds";
  attrib V953B2   format=F01238_. label="Medication protected from water: TB meds";
  attrib V953C    format=F01239_. label="Medication room clean of evidence of rodents/pests: TB meds";
  attrib V953D    format=F01240_. label="Medication room well ventilated: TB meds";
  attrib V953E    format=F01241_. label="Medicines protected from moisture/humidity: TB meds - CS";
  attrib V955_01  format=F01242_. label="ARV NRTIs: Zidovudine (ZDV,AZT) tablets";
  attrib V955_02  format=F01243_. label="ARV NRTIs: Zidovudine (ZDV,AZT) syrup";
  attrib V955_03  format=F01244_. label="ARV NRTIs: Abacavir (ABC) tablets";
  attrib V955_04  format=F01245_. label="ARV NRTIs: Didanosine (ddI) tablets";
  attrib V955_05  format=F01246_. label="ARV NRTIs: Lamivudine (3TC) tablets";
  attrib V955_06  format=F01247_. label="ARV NRTIs: Lamivudine (3TC) syrup";
  attrib V955_07  format=F01248_. label="ARV NRTIs: Stavudine 30 (D4T)";
  attrib V955_08  format=F01249_. label="ARV NRTIs: Stavudine syrup";
  attrib V955_09  format=F01250_. label="ARV NRTIs: Tenofovir Disoproxil Fumarate (TDF)";
  attrib V955_10  format=F01251_. label="NA - ARV NRTIs: Emtricitabine (FTC)";
  attrib V955_11  format=F01252_. label="NA - ARV NRTIs: CS";
  attrib V956_01  format=F01253_. label="ARV NNRTIs: Nevirapine (NVP) tablets";
  attrib V956_02  format=F01254_. label="ARV NNRTIs: Nevirapine (NVP) syrup";
  attrib V956_03  format=F01255_. label="ARV NNRTIs: Efavirenz (EFV) tablets";
  attrib V956_04  format=F01256_. label="ARV NNRTIs: Efavirenz (EFV) syrup";
  attrib V956_05  format=F01257_. label="NA - ARV NNRTIs: Delavirdine (DLV)";
  attrib V956_06  format=F01258_. label="NA - ARV NNRTIs: CS";
  attrib V957_01  format=F01259_. label="NA - ARV Protease inhib: Lopinavir (LPV)";
  attrib V957_02  format=F01260_. label="NA - ARV Protease inhib: Indinavir (IDV))";
  attrib V957_03  format=F01261_. label="NA - ARV Protease inhib: Nelfinavir (NFV))";
  attrib V957_04  format=F01262_. label="NA - ARV Protease inhib: Saquinavir (SQV))";
  attrib V957_05  format=F01263_. label="NA - ARV Protease inhib: Ritonavir (RTV))";
  attrib V957_06  format=F01264_. label="NA - ARV Protease inhib: Atazanavir (ATV))";
  attrib V957_07  format=F01265_. label="NA - ARV Protease inhib: Fosamprenavier (FPV))";
  attrib V957_08  format=F01266_. label="NA - ARV Protease inhib: Tipranavir (TPV))";
  attrib V957_09  format=F01267_. label="NA - ARV Protease inhib: Darunavir (DRV))";
  attrib V957_10  format=F01268_. label="ARV Protease inhib: LOPINAVIR (LPV) + RITONAVIR (RTV) - CS";
  attrib V958_01  format=F01269_. label="NA - ARV Fusion inhib/comb ARV: Enfuvirdite (T-20)";
  attrib V958_02  format=F01270_. label="ARV Fusion inhib/comb ARV: Stavudine+Lamivudine (D4T+3TC)";
  attrib V958_03  format=F01271_. label="ARV Fusion inhib/comb ARV: Stavudine+Lamivudine+Nevirapine (D4T+3TC+NVP)";
  attrib V958_04  format=F01272_. label="ARV Fusion inhib/comb ARV: Zidovudine +Lamivudine (AZT+3TC)";
  attrib V958_05  format=F01273_. label="ARV Fusion inhib/comb ARV: Zidovudine +Lamivudine +Abacavir (AZT+3TC+ABC)";
  attrib V958_06  format=F01274_. label="ARV Fusion inhib/comb ARV: Zidovudine +Lamivudine +Nevirapine (AZT+3TC+NVP)";
  attrib V958_07  format=F01275_. label="NA - ARV Fusion inhib/comb ARV: Tenofovir+Emtricitabine (TDF+FTC";
  attrib V958_08  format=F01276_. label="ARV Fusion inhib/comb ARV: Tenofovir+Lamivudine (TDF+3TC)";
  attrib V958_09  format=F01277_. label="ARV Fusion inhib/comb ARV: Tenofovir+Lamivudine+Efavirenz (TDF+3TC+EFV)";
  attrib V958_10  format=F01278_. label="NA - ARV Fusion inhib/comb ARV: Tenofovir+Emtricitabine+Efavirenz (TDF+FTC+EFV";
  attrib V958_11  format=F01279_. label="NA - ARV Fusion inhib/comb ARV: CS";
  attrib V960A    format=F01280_. label="ARVs: Stock monitoring system - computer or ledge/stock cards";
  attrib V960B    format=F01281_. label="ARVs: Stock monitoring system updated daily";
  attrib V961     format=F01282_. label="Expired medications observed: ARVs";
  attrib V962     format=F01283_. label="Medications organized by date of expiration: ARVs";
  attrib V963A    format=F01284_. label="Medication stored protected from sun: ARVs";
  attrib V963B1   format=F01285_. label="Medication stored off floor: ARVs";
  attrib V963B2   format=F01286_. label="Medication protected from water: ARVs";
  attrib V963C    format=F01287_. label="Medication room clean of evidence of rodents/pests: ARVs";
  attrib V963D    format=F01288_. label="Medication room well ventilated: ARVs";
  attrib SF905A_01 format=F01289_. label="Kalaazar:TAB MILTEFOSINE";
  attrib SF905A_02 format=F01290_. label="Kalaazar:INJ. AMPHOTERICIN B";
  attrib SF905A_03 format=F01291_. label="Kalaazar:INJ. LIPOSOMAL AMPHOTERICIN B";
  attrib SF905A_04 format=F01292_. label="Kalaazar:COMBINATION (MILTEFOSINE + PARAMYCIN)";
  attrib SF908A_01 format=F01293_. label="Other med:ACYCLOVIR TABS/OINTMENT (ANTIVIRAL)";
  attrib SF908A_02 format=F01294_. label="Other med:TINIDAZOLE TABLETS (ANTI-PROTOZOAL)";
  attrib SF908A_03 format=F01295_. label="Other med:VITAMIN B COMPLEX";
  attrib SF908A_04 format=F01296_. label="Other med:CALAMINE LOTION";
  attrib SF908A_05 format=F01297_. label="Other med:GAMMA BENZENE HEXACHLORIDE LOTION";
  attrib SF908A_06 format=F01298_. label="Other med:BENZOIC ACID + SALICYLIC ACID OINTMENT";
  attrib SF908A_07 format=F01299_. label="Other med:SILVER SULPHADIAZINE CREAM";
  attrib SF908A_08 format=F01300_. label="Other med:GENTIAN VIOLET SOLUTION (2%)";
  attrib SF908A_09 format=F01301_. label="Other med:POVIDONE IODINE";
  attrib SF908A_10 format=F01302_. label="Other med:CLOVE OIL";
  attrib SF908A_11 format=F01303_. label="Other med:ATROPINE INJECTION";
  attrib SF908A_12 format=F01304_. label="Other med:PALIDOXIME SODIUM";
  attrib SF908A_13 format=F01305_. label="Other med:ACTIVATED CHARCOAL";
  attrib SF908A_14 format=F01306_. label="Other med:LIGNOCAINE INJECTION";
  attrib SF912A_01 format=F01307_. label="Other supply: Refrigerator for storage of drugs";
  attrib SF912A_02 format=F01308_. label="Other supply: Thermometer to monitor room temperature where drugs are stored";
  attrib V1001    label="Malaria: Days a month service offered";
  attrib V1002    format=F01309_. label="Malaria: Providers in facility diagnose and/or prescribe treatment";
  attrib V1003    format=F01310_. label="Malaria: Use blood tests to diagnose";
  attrib V1004    format=F01311_. label="Malaria: Use malaria RDT kit to diagnose";
  attrib V1005A   format=F01312_. label="NA - Malaria: Malaria RDT brand A Paracheck";
  attrib V1005B   format=F01313_. label="NA - Malaria: Malaria RDT brand B Care Start";
  attrib V1005C   format=F01314_. label="NA - Malaria: Malaria RDT brand C First Response";
  attrib V1005D   format=F01315_. label="NA - Malaria: Malaria RDT brand D SD Bioline";
  attrib V1005E   format=F01316_. label="NA - Malaria: Malaria RDT brand E";
  attrib V1005F   format=F01317_. label="NA - Malaria: Malaria RDT brand F";
  attrib V1005X   format=F01318_. label="NA - Malaria: Malaria RDT other brand";
  attrib V1006    format=F01319_. label="Malaria: Training manual, poster, job aid for Malaria RDT use";
  attrib V1007A   format=F01320_. label="Malaria protocol: National Guidelines for diagnosis and treatment of malaria";
  attrib V1007X   format=F01321_. label="NA - Malaria protocol:Other guidelines for diagnosis and treatment of malaria";
  attrib SF1712AB format=F01322_. label="Malaria protocol:National clinical protocal for malaria";
  attrib V1100A   format=F01323_. label="TB service dx/tx/refer";
  attrib V1106    format=F01324_. label="Method to diagnose TB";
  attrib V1107A   format=F01325_. label="Agreement with referral site for TB";
  attrib V1107B   format=F01326_. label="Record of clients referred for TB";
  attrib V1107C   format=F01327_. label="Referral register on paper or computer";
  attrib V1109A   format=F01328_. label="Guidelines reported:National guidelines for diagnosis and treatment of TB";
  attrib V1109B   format=F01329_. label="Guidelines reported:Guidelines for the management of HIV and TB co-infection";
  attrib V1109C   format=F01330_. label="Guidelines reported:Guidelines related to MDR-TB treatment";
  attrib V1109D   format=F01331_. label="NA - Guidelines reported:Guidelines for TB infection control";
  attrib SV1100A  format=F01332_. label="TB Treatment regimen - CS";
  attrib V1115    format=F01333_. label="Treatment approach for TB";
  attrib V1138SRC format=F01334_. label="Service where IC information collected";
  attrib V1138    format=F01335_. label="Privacy for TB room";
  attrib V1139A1  format=F01336_. label="TB exam:Any running water";
  attrib V1139C   format=F01337_. label="TB exam:Soap";
  attrib V1139F   format=F01338_. label="TB exam:Sharps container";
  attrib V1139G   format=F01339_. label="TB exam:Clean/sterile latex gloves";
  attrib V1139J   format=F01340_. label="TB exam:Disinfectant";
  attrib V1139L   format=F01341_. label="TB exam:Autodestruct syringes";
  attrib V1139O   format=F01342_. label="TB exam:Pedal Bin waste receptacle with plastic liner";
  attrib V1139P   format=F01343_. label="TB exam:Other waste receptacle (not pedal bin)";
  attrib V1139Q   format=F01344_. label="TB exam:Alcohol-based hand rub";
  attrib V1139R   format=F01345_. label="TB exam:Medical masks";
  attrib V1139S   format=F01346_. label="TB exam:Gowns";
  attrib V1139T   format=F01347_. label="TB exam:Eye protection";
  attrib V1139U   format=F01348_. label="TB exam:Guidelines for standard precautions";
  attrib V1139V   format=F01349_. label="TB exam:Needle destroyer - CS";
  attrib V1139W   format=F01350_. label="TB exam:Methylated spirit and glycirine 70:30 - CS";
  attrib V1140    label="Days/month TB services available";
  attrib V1141    format=F01351_. label="System for testing TB clients for HIV infection";
  attrib V1142    format=F01352_. label="HIV RDT kit available in this service";
  attrib V1143    format=F01353_. label="Sputum containers at this service site for collecting sputum specimen";
  attrib V1144    format=F01354_. label="TB meds stored in service area or pharmacy";
  attrib SF19141  format=F01355_. label="RDT kit available at TB: Determine";
  attrib SF19142  format=F01356_. label="RDT kit available at TB: Unigold";
  attrib SF19143  format=F01357_. label="RDT kit available at TB: Statpack";
  attrib SF19144  format=F01358_. label="RDT kit available at TB: Tridot";
  attrib SF19145  format=F01359_. label="RDT kit available at TB: OTHER";
  attrib SF1916AB format=F01360_. label="PAL guideline available";
  attrib V1206    format=F01361_. label="Method for provision of PMTCT";
  attrib V1208A1  format=F01362_. label="Service offered:HIV counseling and testing services to pregnant women";
  attrib V1208A2  format=F01363_. label="Service offered:HIV testing services to infants born to HIV+ women";
  attrib V1208F   format=F01364_. label="Service offered:Infant feeding counseling for HIV+ women";
  attrib V1208G1  format=F01365_. label="Service offered:Nutrition counseling for HIV+ pregnant women and their infants";
  attrib V1208H   format=F01366_. label="Service offered:Family planning counseling to HIV+ pregnant women";
  attrib V1208L   format=F01367_. label="Service offered:ARV prophylaxis for HIV+ pregnant women";
  attrib V1208M   format=F01368_. label="Service offered:ARV prophylaxis for newborns of HIV+ women";
  attrib V1208S   format=F01369_. label="Service offered:PMTCT services with delivery services";
  attrib V1215A   format=F01370_. label="Observed guideline:National guidelines for PMTCT";
  attrib V1215B   format=F01371_. label="NA - Observed guideline:Guidelines for infant and young child feeding counseling";
  attrib V1215X   format=F01372_. label="NA - Observed guideline:Other guidelines for PMTCT";
  attrib V1215Y   format=F01373_. label="NA - Observed guideline:No guidelines";
  attrib V1260    format=F01374_. label="Delivery services in facility";
  attrib V1273A   format=F01375_. label="ARV stocked in PMTCT unit:Nevirapine (NVP)";
  attrib V1273B   format=F01376_. label="ARV stocked in PMTCT unit:Zidovudine (AZT)";
  attrib V1273C   format=F01377_. label="ARV stocked in PMTCT unit:Zidovudine (AZT) and Lamivudine (3TC)";
  attrib V1273D   format=F01378_. label="ARV stocked in PMTCT unit:Lamivudine (3TC)";
  attrib V1273E   format=F01379_. label="ARV stocked in PMTCT unit:Lopinavir (LPV/r)";
  attrib V1273F   format=F01380_. label="ARV stocked in PMTCT unit:Abacavir (ABC)";
  attrib V1273G   format=F01381_. label="ARV stocked in PMTCT unit:Efavirenz (EFV)";
  attrib V1273H   format=F01382_. label="ARV stocked in PMTCT unit:Tenafovir disoproxil fulmarate (TDF)";
  attrib V1273I   format=F01383_. label="NA - ARV stocked in PMTCT unit:Emtricitabine (ETC)";
  attrib V1273J   format=F01384_. label="ARV stocked in PMTCT unit:Nevirapine (NVP) syrup";
  attrib V1273K   format=F01385_. label="ARV stocked in PMTCT unit:Zidovudine (AZT) syrup";
  attrib V1273L   format=F01386_. label="ARV stocked in PMTCT unit:Lamivudine (3TC)+Efavirenz (EFV)+Tenafovir (TDF)";
  attrib V1273M   format=F01387_. label="ARV stocked in PMTCT unit:ZIDOVUDINE (AZT, ZDV)+3TC+NVP - CS";
  attrib V1273N   format=F01388_. label="ARV stocked in PMTCT unit:3TC+EFV - CS";
  attrib V1273Y   format=F01389_. label="ARV stocked in PMTCT unit:No ARV available from this clinic";
  attrib V1274    format=F01390_. label="HIV RDT available in service site";
  attrib V1275    format=F01391_. label="Dried blood spot filter paper (DBS) available in service site";
  attrib V1276SRC format=F01392_. label="Service where IC information collected";
  attrib V1276    format=F01393_. label="Privacy for PMTCT service site";
  attrib V1277A1  format=F01394_. label="PMTCT service site:Any running water";
  attrib V1277C   format=F01395_. label="PMTCT service site:Soap";
  attrib V1277F   format=F01396_. label="PMTCT service site:Sharps container";
  attrib V1277G   format=F01397_. label="PMTCT service site:Clean/sterile latex gloves";
  attrib V1277J   format=F01398_. label="PMTCT service site:Disinfectant";
  attrib V1277L   format=F01399_. label="PMTCT service site:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V1277O   format=F01400_. label="PMTCT service site:Pedal bin waste receptacle with lid and plastic liner";
  attrib V1277P   format=F01401_. label="PMTCT service site:Other waste receptacle";
  attrib V1277Q   format=F01402_. label="PMTCT service site:Alcohol-based hand rub";
  attrib V1277R   format=F01403_. label="PMTCT service site:Medical masks";
  attrib V1277S   format=F01404_. label="PMTCT service site:Gowns";
  attrib V1277T   format=F01405_. label="PMTCT service site:Eye protection";
  attrib V1277U   format=F01406_. label="PMTCT service site:Guidelines for standard precautions";
  attrib V1277V   format=F01407_. label="PMTCT service site:Needle destroyer - CS";
  attrib V1277W   format=F01408_. label="PMTCT service site:Methylated spirit and glycirine 70:30 - CS";
  attrib V1278A   format=F01409_. label="ARV stocked in delivery site for PMTCT:Nevirapine (NVP)";
  attrib V1278B   format=F01410_. label="ARV stocked in delivery site for PMTCT:Zidovudine (AZT)";
  attrib V1278C   format=F01411_. label="ARV stocked in delivery site for PMTCT:Zidovudine (AZT) and Lamivudine (3TC)";
  attrib V1278D   format=F01412_. label="ARV stocked in delivery site for PMTCT:Lamivudine (3TC)";
  attrib V1278E   format=F01413_. label="ARV stocked in delivery site for PMTCT:Lopinavir (LPV/r)";
  attrib V1278F   format=F01414_. label="ARV stocked in delivery site for PMTCT:Abacavir (ABC)";
  attrib V1278G   format=F01415_. label="ARV stocked in delivery site for PMTCT:Efavirenz (EFV)";
  attrib V1278H   format=F01416_. label="ARV stocked in delivery site for PMTCT:Tenafovir disoproxil fulmarate (TDF)";
  attrib V1278I   format=F01417_. label="NA - ARV stocked in delivery site for PMTCT:Emtricitabine (ETC)";
  attrib V1278J   format=F01418_. label="ARV stocked in delivery site for PMTCT:Nevirapine (NVP) syrup";
  attrib V1278K   format=F01419_. label="ARV stocked in delivery site for PMTCT:Zidovudine (AZT) syrup";
  attrib V1278L   format=F01420_. label="ARV stocked in delivery site for PMTCT:Lamivudine(3TC) + Efavirenz(EFV) + Tenafovir (TDF)";
  attrib V1278M   format=F01421_. label="ARV stocked in delivery site for PMTCT:Zidovudine (AZT, ZDV) + 3TC + NVP - CS";
  attrib V1278N   format=F01422_. label="ARV stocked in delivery site for PMTCT:3TC + EFV - CS";
  attrib V1278Y   format=F01423_. label="ARV stocked in delivery site for PMTCT:No ARV available from this clinic";
  attrib V1279    format=F01424_. label="HIV RDT available in delivery service site for PMTCT";
  attrib V1280SRC format=F01425_. label="Service where IC information collected in delivery service site for PMTCT";
  attrib V1280    format=F01426_. label="Privacy for PMTCT service in delivery site";
  attrib V1281A1  format=F01427_. label="Delivery service site for PMTCT:Any running water";
  attrib V1281C   format=F01428_. label="Delivery service site for PMTCT:Soap";
  attrib V1281F   format=F01429_. label="Delivery service site for PMTCT:Sharps container";
  attrib V1281G   format=F01430_. label="Delivery service site for PMTCT:Clean/sterile latex gloves";
  attrib V1281J   format=F01431_. label="Delivery service site for PMTCT:Disinfectant";
  attrib V1281L   format=F01432_. label="Delivery service site for PMTCT:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V1281O   format=F01433_. label="Delivery service site for PMTCT:Pedal bin waste receptacle with lid and plastic liner";
  attrib V1281P   format=F01434_. label="Delivery service site for PMTCT:Other waste receptacle";
  attrib V1281Q   format=F01435_. label="Delivery service site for PMTCT:Alcohol-based hand rub";
  attrib V1281R   format=F01436_. label="Delivery service site for PMTCT:Medical masks";
  attrib V1281S   format=F01437_. label="Delivery service site for PMTCT:Gowns";
  attrib V1281T   format=F01438_. label="Delivery service site for PMTCT:Eye protection";
  attrib V1281U   format=F01439_. label="Delivery service site for PMTCT:Guidelines for standard precautions";
  attrib V1281V   format=F01440_. label="Delivery service site for PMTCT:Needle destroyer - CS";
  attrib V1281W   format=F01441_. label="Delivery service site for PMTCT:Methylated spirit and glycirine 70:30 - CS";
  attrib SF1505A1 format=F01442_. label="RDT kit available at PMTCT: Determine";
  attrib SF1505A2 format=F01443_. label="RDT kit available at PMTCT: Unigold";
  attrib SF1505A3 format=F01444_. label="RDT kit available at PMTCT: Statpack";
  attrib SF1505A4 format=F01445_. label="RDT kit available at PMTCT: Tridot";
  attrib SF1505A5 format=F01446_. label="RDT kit available at PMTCT: OTHER";
  attrib SF1508A  format=F01447_. label="DBS sample for external quality assurance";
  attrib V1307A   label="Days/month HIV testing services available";
  attrib V1327D   format=F01448_. label="Procedure for HIV test:Client referred to other unit in this facility";
  attrib V1327E   format=F01449_. label="Procedure for HIV test:Blood drawn by this unit staff, sent to lab";
  attrib V1327G   format=F01450_. label="Procedure for HIV test:Client sent to lab in this facility";
  attrib V1327H   format=F01451_. label="Procedure for HIV test:Rapid test done in clinic/unit";
  attrib V1327I   format=F01452_. label="Procedure for HIV test:Client sent outside facility";
  attrib V1327J   format=F01453_. label="Procedure for HIV test:Blood drawn in unit and sent outside facility for testing";
  attrib V1328SRC format=F01454_. label="Service where IC information collected";
  attrib V1328    format=F01455_. label="Privacy for blood draw room";
  attrib V1329A1  format=F01456_. label="Blood draw room:Any running water";
  attrib V1329C   format=F01457_. label="Blood draw room:Soap";
  attrib V1329F   format=F01458_. label="Blood draw room:Sharps container";
  attrib V1329G   format=F01459_. label="Blood draw room:Clean/sterile latex gloves";
  attrib V1329J   format=F01460_. label="Blood draw room:Disinfectant";
  attrib V1329L   format=F01461_. label="Blood draw room:Autodestruct syringes";
  attrib V1329O   format=F01462_. label="Blood draw room:Pedal Bin waste receptacle with plastic liner";
  attrib V1329P   format=F01463_. label="Blood draw room:Other waste receptacle (not pedal bin)";
  attrib V1329Q   format=F01464_. label="Blood draw room:Alcohol-based hand rub";
  attrib V1329R   format=F01465_. label="Blood draw room:Medical masks";
  attrib V1329S   format=F01466_. label="Blood draw room:Gowns";
  attrib V1329T   format=F01467_. label="Blood draw room:Eye protection";
  attrib V1329U   format=F01468_. label="Blood draw room:Guidelines for standard precautions";
  attrib V1329V   format=F01469_. label="Blood draw room:Needle destroyer - CS";
  attrib V1329W   format=F01470_. label="Blood draw room:Methylated spirit and glycirine 70:30 - CS";
  attrib V1331    format=F01471_. label="Guidelines in unit";
  attrib V1332A   format=F01472_. label="National protocol available:National HIV counseling and testing guidelines";
  attrib V1332B   format=F01473_. label="National protocol available:Written protocols for PEP";
  attrib V1332X   format=F01474_. label="NA - National protocol available:Other guidelines on HIV testing";
  attrib V1337    format=F01475_. label="Individual client chart";
  attrib V1355    format=F01476_. label="Sample HIV RDT kit";
  attrib V1356    format=F01477_. label="Staff in this facility have access to HIV PEP";
  attrib V1357    format=F01478_. label="Have condoms available in service site for HIV counsel/testing clients";
  attrib V1358    format=F01479_. label="NA - Have agreement with referral site for return of results of outside HIV tests";
  attrib SF20041  format=F01480_. label="RDT kit available at HIV testing: Determine";
  attrib SF20042  format=F01481_. label="RDT kit available at HIV testing: Unigold";
  attrib SF20043  format=F01482_. label="RDT kit available at HIV testing: Statpack";
  attrib SF20044  format=F01483_. label="RDT kit available at HIV testing: Tridot";
  attrib SF20045  format=F01484_. label="RDT kit available at HIV testing: OTHER";
  attrib V1400A   format=F01485_. label="Type of ART services available";
  attrib V1415A   format=F01486_. label="Pre-ART baseline:Hemoglobin/hematocrit";
  attrib V1415B   format=F01487_. label="Pre-ART baseline:Full blood count";
  attrib V1415C   format=F01488_. label="Pre-ART baseline:Pregnancy test for women";
  attrib V1415D   format=F01489_. label="Pre-ART baseline:Renal function tests (including serum creatinine and U&E)";
  attrib V1415E   format=F01490_. label="Pre-ART baseline:Urinalysis";
  attrib V1415F   format=F01491_. label="Pre-ART baseline:Liver function tests";
  attrib V1415G   format=F01492_. label="Pre-ART baseline:TB sputum test";
  attrib V1415H   format=F01493_. label="Pre-ART baseline:Chest X-ray";
  attrib V1415I   format=F01494_. label="Pre-ART baseline:Hepatitis B";
  attrib V1415J   format=F01495_. label="Pre-ART baseline:CD4 T-cell count";
  attrib V1415K   format=F01496_. label="Pre-ART baseline:HIV RNA viral load";
  attrib V1415L   format=F01497_. label="Pre-ART baseline:Blood sugar level - CS";
  attrib V1415M   format=F01498_. label="Pre-ART baseline:Cervical pap smear - CS";
  attrib V1415N   format=F01499_. label="Pre-ART baseline:Hepatitis C - CS";
  attrib V1415X   format=F01500_. label="Pre-ART baseline:Other routine tests";
  attrib V1421A   format=F01501_. label="Guideline available:National ART guidelines";
  attrib V1421X   format=F01502_. label="NA - Guideline available:Other ART guidelines";
  attrib V1451    format=F01503_. label="ART meds stored in service area or pharmacy";
  attrib V1452A   format=F01504_. label="Monitor ART:Hemoglobin/hematocrit";
  attrib V1452B   format=F01505_. label="Monitor ART:Full blood count";
  attrib V1452C   format=F01506_. label="Monitor ART:Pregnancy test for women";
  attrib V1452D   format=F01507_. label="Monitor ART:Renal function tests (including serum creatinine and U&E)";
  attrib V1452E   format=F01508_. label="Monitor ART:Urinalysis";
  attrib V1452F   format=F01509_. label="Monitor ART:Liver function tests";
  attrib V1452G   format=F01510_. label="Monitor ART:TB sputum test";
  attrib V1452H   format=F01511_. label="Monitor ART:Chest X-ray";
  attrib V1452I   format=F01512_. label="Monitor ART:Hepatitis B";
  attrib V1452J   format=F01513_. label="Monitor ART:CD4 T-cell count";
  attrib V1452K   format=F01514_. label="Monitor ART:HIV RNA viral load";
  attrib V1452L   format=F01515_. label="Monitor ART:Blood sugar level - CS";
  attrib V1452M   format=F01516_. label="Monitor ART:Cervical pap smear - CS";
  attrib V1452N   format=F01517_. label="Monitor ART:Hepatitis C - CS";
  attrib V1452X   format=F01518_. label="Monitor ART::Other routine tests";
  attrib SF2105AB format=F01519_. label="Guideline available:National guidelines for HIV and AIDS in children";
  attrib V1515    format=F01520_. label="Provision of clinical care or support services for HIV/AIDS clients";
  attrib V1516A   format=F01521_. label="Curative HIV care:Prescribe treatment for opportunistic infections/symptoms";
  attrib V1516B   format=F01522_. label="NA - Curative HIV care:Provide systemic IV treatment of specific fungal infections";
  attrib V1516C   format=F01523_. label="NA - Curative HIV care:Provide treatment for Kaposi's sarcoma";
  attrib V1516D   format=F01524_. label="Curative HIV care:Provide/prescribe palliative care";
  attrib V1516E   format=F01525_. label="Curative HIV care:Provide nutritional rehabilitation";
  attrib V1516F   format=F01526_. label="Curative HIV care:Prescribe/provide fortified protein supplementation (FPS)";
  attrib V1516J   format=F01527_. label="Curative HIV care:Provide pediatric care for HIV+ children";
  attrib V1518B   format=F01528_. label="Preventive services for HIV+ clients:INH (preventive tx for TB)";
  attrib V1518C   format=F01529_. label="Preventive services for HIV+ clients:Primary preventive care (ex: CPT)";
  attrib V1518D   format=F01530_. label="Preventive services for HIV+ clients:Provide/prescribe micronutrient supplementation";
  attrib V1518E   format=F01531_. label="Preventive services for HIV+ clients:General family planning counseling/services for health reasons related to HIV/AIDS";
  attrib V1518F   format=F01532_. label="Preventive services for HIV+ clients:Provide condoms to prevent further transmission of HIV/AIDS";
  attrib V1521A   format=F01533_. label="National protocol available:National guidelines for clinical management of HIV/AIDS";
  attrib V1521B   format=F01534_. label="National protocol available:Guidelines for palliative care of HIV/AIDS+ clients";
  attrib V1560    format=F01535_. label="HIV care and support: TB routine screening and testing HIV+ clients";
  attrib V1561    format=F01536_. label="HIV care and support: Condoms available at site for clients receiving services";
  attrib SF2205A  format=F01537_. label="Provide community care center (CCC) service";
  attrib V1601    format=F01538_. label="N-C diseases:Diagnose and/or manage diabetes";
  attrib V1601A   format=F01539_. label="N-C diseases:Diagnose and/or manage diabetes (specific)";
  attrib V1602A   format=F01540_. label="NA - N-C diseases:National guidelines for the diagnosis/management of diabetes";
  attrib V1602X   format=F01541_. label="N-C diseases:Other guidelines for the diagnosis/management of diabetes";
  attrib V1603    format=F01542_. label="N-C diseases:Diagnose and/or manage cardio-vascular diseases";
  attrib V1603A   format=F01543_. label="N-C diseases:Diagnose and/or manage cardio-vascular diseases (specific)";
  attrib V1604A   format=F01544_. label="NA - N-C diseases:National guidelines for the diagnosis/management of cardio-vascular diseases";
  attrib V1604X   format=F01545_. label="N-C diseases:Other guidelines for the diagnosis/management of cardio-vascular diseases";
  attrib V1605    format=F01546_. label="N-C diseases:Diagnose and/or manage chronic respiratory diseases (COPD)";
  attrib V1605A   format=F01547_. label="N-C diseases:Diagnose and/or manage chronic respiratory diseases (specific)";
  attrib V1606A   format=F01548_. label="NA - N-C diseases:National guidelines for the diagnosis/management of chronic respiratory diseases (COPD)";
  attrib V1606X   format=F01549_. label="N-C diseases:Other guidelines for the diagnosis/management of chronic respiratory diseases (COPD)";
  attrib V1607SRC format=F01550_. label="N-C diseases:Source for basic supplies/equipment";
  attrib V1607A   format=F01551_. label="N-C diseases: Measuring tape (for circumference)";
  attrib V1607B   format=F01552_. label="N-C diseases: Spacers for inhalers";
  attrib V1607C   format=F01553_. label="N-C diseases: IV infusion kits - adult";
  attrib V1607D   format=F01554_. label="N-C diseases: IV infusion kits - pediatric";
  attrib V1608A   format=F01555_. label="N-C diseases: Adult weighing scale";
  attrib V1608B   format=F01556_. label="N-C diseases:Child weighing scale (250 g graduation)";
  attrib V1608C   format=F01557_. label="N-C diseases: Infant weighting scale (100 g graduation)";
  attrib V1608D   format=F01558_. label="N-C diseases: Stadiometer (or height rod)";
  attrib V1608E   format=F01559_. label="N-C diseases: Thermometer";
  attrib V1608F   format=F01560_. label="N-C diseases: Stethoscope";
  attrib V1608G   format=F01561_. label="N-C diseases: Digital BP apparatus";
  attrib V1608H   format=F01562_. label="N-C diseases: Manual BP apparatus";
  attrib V1608I   format=F01563_. label="N-C diseases: Light source (flashlight OK)";
  attrib V1608J   format=F01564_. label="N-C diseases: Self-inflating bag and mask (adult)";
  attrib V1608K   format=F01565_. label="N-C diseases: Self-inflating bag and mask (pediatric)";
  attrib V1608L   format=F01566_. label="N-C diseases: Micronebulizer";
  attrib V1608M   format=F01567_. label="N-C diseases: Peak flow meters";
  attrib V1608N   format=F01568_. label="N-C diseases: Pulse oximeter";
  attrib V1608O   format=F01569_. label="N-C diseases: Oxygen concentrators";
  attrib V1608P   format=F01570_. label="N-C diseases: Filled oxygen cylinder";
  attrib V1608Q   format=F01571_. label="N-C diseases: Oxygen distribution system";
  attrib V1608R   format=F01572_. label="N-C diseases: Measuring tape (for circumference) - CS";
  attrib V1609SRC format=F01573_. label="Service where IC information collected for non-communicable diseases";
  attrib V1609    format=F01574_. label="Non-communicable diseases:Privacy for observed room";
  attrib V1610A1  format=F01575_. label="Non-communicable diseases: Running water";
  attrib V1610C   format=F01576_. label="Non-communicable diseases:Soap";
  attrib V1610F   format=F01577_. label="Non-communicable diseases:Sharps container";
  attrib V1610G   format=F01578_. label="Non-communicable diseases:Clean/sterile latex gloves";
  attrib V1610J   format=F01579_. label="Non-communicable diseases:Disinfectant";
  attrib V1610L   format=F01580_. label="Non-communicable diseases:Autodestruct syringes with needles or single-use disposable syringes with needles";
  attrib V1610O   format=F01581_. label="Non-communicable diseases:Pedal Bin waste receptacle with lid and plastic liner";
  attrib V1610P   format=F01582_. label="Non-communicable diseases:Other waste receptacle (not pedal bin)";
  attrib V1610Q   format=F01583_. label="Non-communicable diseases:Alcohol-based hand rub";
  attrib V1610R   format=F01584_. label="Non-communicable diseases:Medical masks";
  attrib V1610S   format=F01585_. label="Non-communicable diseases:Gowns";
  attrib V1610T   format=F01586_. label="Non-communicable diseases:Eye protection";
  attrib V1610U   format=F01587_. label="Non-communicable diseases:Guidelines for standard precautions";
  attrib V1610V   format=F01588_. label="Non-communicable diseases:Needle destroyer - CS";
  attrib V1610W   format=F01589_. label="Non-communicable diseases:Methylated spirit and glycirine 70:30 - CS";
  attrib V1701    format=F01590_. label="Guidelines for Integrated management of emergency and essential surgical care (IMEESC)";
  attrib V1702A   format=F01591_. label="Minor surgical: Needle holder";
  attrib V1702B   format=F01592_. label="Minor surgical: scalpel handle with blade";
  attrib V1702C   format=F01593_. label="Minor surgical: Retractor";
  attrib V1702D   format=F01594_. label="Minor surgical: Surgical scissors";
  attrib V1702E   format=F01595_. label="Minor surgical: Nasogastric tube (10-17g)";
  attrib V1702F   format=F01596_. label="Minor surgical: Tourniquet";
  attrib V1703A   format=F01597_. label="Minor surgical materials: Absorbable suture material";
  attrib V1703B   format=F01598_. label="Minor surgical materials: Non-absorbable suture material";
  attrib V1703C   format=F01599_. label="Minor surgical materials: Skin disinfectant";
  attrib V1703D   format=F01600_. label="Minor surgical materials: Lidocaine/ lignocaine injection";
  attrib V1703E   format=F01601_. label="Minor surgical materials: Ketamine injection";
  attrib V1704SRC format=F01602_. label="Source for infection control for minor surgery service";
  attrib V1704    format=F01603_. label="Privacy for minor surgery service";
  attrib V1705A1  format=F01604_. label="Minor surgery:Running water";
  attrib V1705C   format=F01605_. label="Minor surgery:Soap";
  attrib V1705F   format=F01606_. label="Minor surgery:Sharps container";
  attrib V1705G   format=F01607_. label="Minor surgery:Disposable latex gloves";
  attrib V1705J   format=F01608_. label="Minor surgery:Disinfectant";
  attrib V1705L   format=F01609_. label="Minor surgery:auto-disable syringes with needles/single-use standard disposable syringes with needles";
  attrib V1705O   format=F01610_. label="Minor surgery:Pedal bin waste receptacle with lid with plastic liner";
  attrib V1705P   format=F01611_. label="Minor surgery:Other waste receptacle";
  attrib V1705Q   format=F01612_. label="Minor surgery:Alcohol-based hand rub";
  attrib V1705R   format=F01613_. label="Minor surgery:Medical masks";
  attrib V1705S   format=F01614_. label="Minor surgery:Gowns";
  attrib V1705T   format=F01615_. label="Minor surgery:Eye protection";
  attrib V1705U   format=F01616_. label="Minor surgery:Guidelines for standard precautions";
  attrib V1705V   format=F01617_. label="Minor surgery:Needle destroyer - CS";
  attrib V1705W   format=F01618_. label="Minor surgery:Methylated spirit and glycirine 70:30 - CS";
  attrib SF1720B  format=F01619_. label="Kalazaar:Provide diagnosis using RDT (RK-39) kit";
  attrib SF1720BC format=F01620_. label="Kalaazar:  use RDT (RK-39) kit";
  attrib SF1720D  format=F01621_. label="Kalazaar:Provide treatment";
  attrib SF1720EF format=F01622_. label="Kalaazar: National protocal for diagnosis and treatment";
  attrib SF1730B  format=F01623_. label="Snake bite: Provide first aid mgmt";
  attrib SF1730BC format=F01624_. label="Snake bite: National protocal for mgmt of snake bite";
  attrib SF1730BD format=F01625_. label="Snake bite: ASV available";
  attrib SF1730E  label="Snake bite: Distance of nearest referral center for sanke bite in KM";
  attrib SF1740B  format=F01626_. label="Dog bite: Provide first aid mgmt";
  attrib SF1740BC format=F01627_. label="Dog bite: National protocal for mgmt of dog bite";
  attrib SF1740BD format=F01628_. label="Dog bite: ARV available";
  attrib SF1740E  label="Dog bite: Distance of nearest referral center for dog bite in KM";
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
infile 'D:\SPADHS-FINALIZED\NPHFS_2015\RECODE\EXPORT\EXPORTEDFILES\NPFC71FLSR.DAT' LRECL=2876 MISSOVER ;
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
    @55   V034C    1.0
    @56   V034D    1.0
    @57   V034E    1.0
    @58   V035     1.0
    @59   V035A    1.0
    @60   V035B    1.0
    @61   V042     1.0
    @62   V043     1.0
    @63   V044     1.0
    @64   V045     1.0
    @65   V045A    1.0
    @66   V046     1.0
    @67   V047     1.0
    @68   V048     1.0
    @69   V049     1.0
    @70   V052     2.0
    @72   V058A    2.0
    @74   V058     2.0
    @76   V071     2.0
    @78   V071A    2.0
    @80   V071C    2.0
    @82   V072     2.0
    @84   V072A    2.0
    @86   V072C    2.0
    @88   V073     2.0
    @90   V074     2.0
    @92   V073A    2.0
    @94   V073C    2.0
    @96   V074C    2.0
    @98   V076     3.0
    @101  V077     3.0
    @104  V078     3.0
    @107  V079     3.0
    @110  V081     2.0
    @112  V082     4.0
    @116  V083     2.0
    @118  V095A    4.0
    @122  V095B    4.0
    @126  PROVINCE 1.0
    @127  DEVREG   1.0
    @128  SF101A   1.0
    @129  V101A    1.0
    @130  V102N    2.0
    @132  V102A_01 2.0
    @134  V102A_02 2.0
    @136  V102A_03 2.0
    @138  V102A_04 2.0
    @140  V102A_05 2.0
    @142  V102A_06 2.0
    @144  V102A_07 2.0
    @146  V102A_08 2.0
    @148  V102A_09 2.0
    @150  V102A_10 2.0
    @152  V102A_11 2.0
    @154  V102A_12 2.0
    @156  V102A_13 2.0
    @158  V102A_14 2.0
    @160  V102A_15 2.0
    @162  V102A_16 2.0
    @164  V102A_17 2.0
    @166  V102A_18 2.0
    @168  V102A_19 2.0
    @170  V102D_01 4.0
    @174  V102D_02 4.0
    @178  V102D_03 4.0
    @182  V102D_04 4.0
    @186  V102D_05 4.0
    @190  V102D_06 4.0
    @194  V102D_07 4.0
    @198  V102D_08 4.0
    @202  V102D_09 4.0
    @206  V102D_10 4.0
    @210  V102D_11 4.0
    @214  V102D_12 4.0
    @218  V102D_13 4.0
    @222  V102D_14 4.0
    @226  V102D_15 4.0
    @230  V102D_16 4.0
    @234  V102D_17 4.0
    @238  V102D_18 4.0
    @242  V102D_19 4.0
    @246  V102E_01 4.0
    @250  V102E_02 4.0
    @254  V102E_03 4.0
    @258  V102E_04 4.0
    @262  V102E_05 4.0
    @266  V102E_06 4.0
    @270  V102E_07 4.0
    @274  V102E_08 4.0
    @278  V102E_09 4.0
    @282  V102E_10 4.0
    @286  V102E_11 4.0
    @290  V102E_12 4.0
    @294  V102E_13 4.0
    @298  V102E_14 4.0
    @302  V102E_15 4.0
    @306  V102E_16 4.0
    @310  V102E_17 4.0
    @314  V102E_18 4.0
    @318  V102E_19 4.0
    @322  V102F_01 4.0
    @326  V102F_02 4.0
    @330  V102F_03 4.0
    @334  V102F_04 4.0
    @338  V102F_05 4.0
    @342  V102F_06 4.0
    @346  V102F_07 4.0
    @350  V102F_08 4.0
    @354  V102F_09 4.0
    @358  V102F_10 4.0
    @362  V102F_11 4.0
    @366  V102F_12 4.0
    @370  V102F_13 4.0
    @374  V102F_14 4.0
    @378  V102F_15 4.0
    @382  V102F_16 4.0
    @386  V102F_17 4.0
    @390  V102F_18 4.0
    @394  V102F_19 4.0
    @398  V102G_01 4.0
    @402  V102G_02 4.0
    @406  V102G_03 4.0
    @410  V102G_04 4.0
    @414  V102G_05 4.0
    @418  V102G_06 4.0
    @422  V102G_07 4.0
    @426  V102G_08 4.0
    @430  V102G_09 4.0
    @434  V102G_10 4.0
    @438  V102G_11 4.0
    @442  V102G_12 4.0
    @446  V102G_13 4.0
    @450  V102G_14 4.0
    @454  V102G_15 4.0
    @458  V102G_16 4.0
    @462  V102G_17 4.0
    @466  V102G_18 4.0
    @470  V102G_19 4.0
    @474  V102H_01 4.0
    @478  V102H_02 4.0
    @482  V102H_03 4.0
    @486  V102H_04 4.0
    @490  V102H_05 4.0
    @494  V102H_06 4.0
    @498  V102H_07 4.0
    @502  V102H_08 4.0
    @506  V102H_09 4.0
    @510  V102H_10 4.0
    @514  V102H_11 4.0
    @518  V102H_12 4.0
    @522  V102H_13 4.0
    @526  V102H_14 4.0
    @530  V102H_15 4.0
    @534  V102H_16 4.0
    @538  V102H_17 4.0
    @542  V102H_18 4.0
    @546  V102H_19 4.0
    @550  V102I_01 4.0
    @554  V102I_02 4.0
    @558  V102I_03 4.0
    @562  V102I_04 4.0
    @566  V102I_05 4.0
    @570  V102I_06 4.0
    @574  V102I_07 4.0
    @578  V102I_08 4.0
    @582  V102I_09 4.0
    @586  V102I_10 4.0
    @590  V102I_11 4.0
    @594  V102I_12 4.0
    @598  V102I_13 4.0
    @602  V102I_14 4.0
    @606  V102I_15 4.0
    @610  V102I_16 4.0
    @614  V102I_17 4.0
    @618  V102I_18 4.0
    @622  V102I_19 4.0
    @626  V102J_01 4.0
    @630  V102J_02 4.0
    @634  V102J_03 4.0
    @638  V102J_04 4.0
    @642  V102J_05 4.0
    @646  V102J_06 4.0
    @650  V102J_07 4.0
    @654  V102J_08 4.0
    @658  V102J_09 4.0
    @662  V102J_10 4.0
    @666  V102J_11 4.0
    @670  V102J_12 4.0
    @674  V102J_13 4.0
    @678  V102J_14 4.0
    @682  V102J_15 4.0
    @686  V102J_16 4.0
    @690  V102J_17 4.0
    @694  V102J_18 4.0
    @698  V102J_19 4.0
    @702  V102K_01 4.0
    @706  V102K_02 4.0
    @710  V102K_03 4.0
    @714  V102K_04 4.0
    @718  V102K_05 4.0
    @722  V102K_06 4.0
    @726  V102K_07 4.0
    @730  V102K_08 4.0
    @734  V102K_09 4.0
    @738  V102K_10 4.0
    @742  V102K_11 4.0
    @746  V102K_12 4.0
    @750  V102K_13 4.0
    @754  V102K_14 4.0
    @758  V102K_15 4.0
    @762  V102K_16 4.0
    @766  V102K_17 4.0
    @770  V102K_18 4.0
    @774  V102K_19 4.0
    @778  V102DT   4.0
    @782  V102ET   4.0
    @786  V102T    4.0
    @790  V102FT   4.0
    @794  V102GT   4.0
    @798  V102HT   4.0
    @802  V102IT   4.0
    @806  V102JT   4.0
    @810  V102KT   4.0
    @814  V105     1.0
    @815  V106     1.0
    @816  V106A    1.0
    @817  V106B    1.0
    @818  V106C    1.0
    @819  V106D    1.0
    @820  V106E    1.0
    @821  V106F    1.0
    @822  V106G    1.0
    @823  V106H    1.0
    @824  V106X    1.0
    @825  V106Y    1.0
    @826  V106AA   1.0
    @827  V106BB   1.0
    @828  V107     1.0
    @829  V108     1.0
    @830  V109A    1.0
    @831  V109B    1.0
    @832  V109C    1.0
    @833  V109D    1.0
    @834  V109E    1.0
    @835  V109F    1.0
    @836  V109G    1.0
    @837  V109H    1.0
    @838  V109X    1.0
    @839  V109Y    1.0
    @840  V109Z    1.0
    @841  V110A    1.0
    @842  V110     1.0
    @843  V112     1.0
    @844  V115     1.0
    @845  V116G    1.0
    @846  V116H    1.0
    @847  V116I    1.0
    @848  V120A    1.0
    @849  V120     1.0
    @850  V121A    1.0
    @851  V121B    1.0
    @852  V121C    1.0
    @853  V121D    1.0
    @854  V123     2.0
    @856  V124     1.0
    @857  V126     1.0
    @858  V127A    1.0
    @859  V127B    1.0
    @860  V127C    1.0
    @861  V127     1.0
    @862  V128     1.0
    @863  V128A    1.0
    @864  V129     1.0
    @865  V130     1.0
    @866  SF464    1.0
    @867  V131     1.0
    @868  V132     1.0
    @869  V133     1.0
    @870  V134     6.0
    @876  V135     6.0
    @882  SF346B   1.0
    @883  SF351A   1.0
    @884  SF420A   1.0
    @885  SF420B   1.0
    @886  SF420D   1.0
    @887  SF420E   4.0
    @891  SF420FA1 2.0
    @893  SF420FB1 2.0
    @895  SF420FC1 2.0
    @897  SF420FA2 2.0
    @899  SF420FB2 2.0
    @901  SF420FC2 2.0
    @903  SF420FA3 2.0
    @905  SF420FB3 2.0
    @907  SF420FC3 2.0
    @909  SF420FA4 2.0
    @911  SF420FB4 2.0
    @913  SF420FC4 2.0
    @915  SF442AB  1.0
    @916  SF442CD  1.0
    @917  V142     1.0
    @918  V143     4.0
    @922  V144     1.0
    @923  V144A    1.0
    @924  V144B    1.0
    @925  V144E    1.0
    @926  V144F    1.0
    @927  V144G    1.0
    @928  V144J    1.0
    @929  V144K    1.0
    @930  V144L    1.0
    @931  V144M    1.0
    @932  V144N    1.0
    @933  V144O    1.0
    @934  V144P    1.0
    @935  V144Q    1.0
    @936  V144R    1.0
    @937  V144S    1.0
    @938  V144T    1.0
    @939  V144U    1.0
    @940  V144V    1.0
    @941  V144W    1.0
    @942  V144X    1.0
    @943  V145     1.0
    @944  V145A    1.0
    @945  V145B    1.0
    @946  V145C    1.0
    @947  V145D    1.0
    @948  V145X    1.0
    @949  V146A    1.0
    @950  V146B    1.0
    @951  V146C    1.0
    @952  V146D    1.0
    @953  V146E    1.0
    @954  V146F    1.0
    @955  V146G    1.0
    @956  V146H    1.0
    @957  V146I    1.0
    @958  V146X    1.0
    @959  V146Y    1.0
    @960  V146Z    1.0
    @961  V150     1.0
    @962  V152     1.0
    @963  V153     1.0
    @964  V153A    2.0
    @966  V154     2.0
    @968  V154A    1.0
    @969  V154B    1.0
    @970  V154C    1.0
    @971  V154D    1.0
    @972  V154E    1.0
    @973  V154F    1.0
    @974  V154G    1.0
    @975  V154H    1.0
    @976  V165A    1.0
    @977  V165B    1.0
    @978  V165C    1.0
    @979  V165D    1.0
    @980  V166A    1.0
    @981  V166B    1.0
    @982  V166C    1.0
    @983  V166D    1.0
    @984  V166E    1.0
    @985  V166F    1.0
    @986  V166G    1.0
    @987  V166H    1.0
    @988  V166I    1.0
    @989  V166J    1.0
    @990  V166K    1.0
    @991  V166L    1.0
    @992  V166M    1.0
    @993  V166N    1.0
    @994  V166O    1.0
    @995  V166P    1.0
    @996  V166Q    1.0
    @997  V166R    1.0
    @998  SV165A   1.0
    @999  V167     1.0
    @1000 V168A1   1.0
    @1001 V168C    1.0
    @1002 V168F    1.0
    @1003 V168G    1.0
    @1004 V168J    1.0
    @1005 V168L    1.0
    @1006 V168O    1.0
    @1007 V168P    1.0
    @1008 V168Q    1.0
    @1009 V168R    1.0
    @1010 V168S    1.0
    @1011 V168T    1.0
    @1012 V168U    1.0
    @1013 V168V    1.0
    @1014 V168W    1.0
    @1015 V146AB   9.0
    @1024 V146BB   9.0
    @1033 V146CB   9.0
    @1042 V146DB   9.0
    @1051 V146XB   9.0
    @1060 SF370DA1 1.0
    @1061 SF370DB1 1.0
    @1062 SF370DC1 1.0
    @1063 SF370DA2 1.0
    @1064 SF370DB2 1.0
    @1065 SF370DC2 1.0
    @1066 SF453A   2.0
    @1068 SF462A   1.0
    @1069 SF465BC  1.0
    @1070 SF465DE  1.0
    @1071 SF465FG  1.0
    @1072 SF465H   1.0
    @1073 SF471    6.0
    @1079 SF471A   6.0
    @1085 SF472    6.0
    @1091 SF472EF  1.0
    @1092 SF480A   1.0
    @1093 SF480BC  1.0
    @1094 SF480DE  1.0
    @1095 SF480F   1.0
    @1096 SF480G   4.0
    @1100 SF480HI  1.0
    @1101 SF480JK  1.0
    @1102 SF700CD  1.0
    @1103 SF720AB  1.0
    @1104 SF720C   1.0
    @1105 V173A    1.0
    @1106 V173     1.0
    @1107 V175A_1  1.0
    @1108 V175A_2  1.0
    @1109 V175A_3  1.0
    @1110 V175A_4  1.0
    @1111 V175A_5  1.0
    @1112 V175A_6  1.0
    @1113 V175A_7  1.0
    @1114 V175A_8  1.0
    @1115 V175B_1  1.0
    @1116 V175B_2  1.0
    @1117 V175B_3  1.0
    @1118 V175B_4  1.0
    @1119 V175B_5  1.0
    @1120 V175B_6  1.0
    @1121 V175B_7  1.0
    @1122 V175B_8  1.0
    @1123 V175C_1  3.0
    @1126 V175C_2  3.0
    @1129 V175C_3  3.0
    @1132 V175C_4  3.0
    @1135 V175C_5  3.0
    @1138 V175C_6  3.0
    @1141 V175C_7  3.0
    @1144 V175C_8  3.0
    @1147 V175D_1  3.0
    @1150 V175D_2  3.0
    @1153 V175D_3  3.0
    @1156 V175D_4  3.0
    @1159 V175D_5  3.0
    @1162 V175D_6  3.0
    @1165 V175D_7  3.0
    @1168 V175D_8  3.0
    @1171 V175E_1  1.0
    @1172 V175E_2  1.0
    @1173 V175E_3  1.0
    @1174 V175E_4  1.0
    @1175 V175E_5  1.0
    @1176 V175E_6  1.0
    @1177 V175E_7  1.0
    @1178 V175E_8  1.0
    @1179 V175F_1  3.0
    @1182 V175F_2  3.0
    @1185 V175F_3  3.0
    @1188 V175F_4  3.0
    @1191 V175F_5  3.0
    @1194 V175F_6  3.0
    @1197 V175F_7  3.0
    @1200 V175F_8  3.0
    @1203 V175G_1  3.0
    @1206 V175G_2  3.0
    @1209 V175G_3  3.0
    @1212 V175G_4  3.0
    @1215 V175G_5  3.0
    @1218 V175G_6  3.0
    @1221 V175G_7  3.0
    @1224 V175G_8  3.0
    @1227 V175H_1  2.0
    @1229 V175H_2  2.0
    @1231 V175H_3  2.0
    @1233 V175H_4  2.0
    @1235 V175H_5  2.0
    @1237 V175H_6  2.0
    @1239 V175H_7  2.0
    @1241 V175H_8  2.0
    @1243 V175HA_1 2.0
    @1245 V175HA_2 2.0
    @1247 V175HA_3 2.0
    @1249 V175HA_4 2.0
    @1251 V175HA_5 2.0
    @1253 V175HA_6 2.0
    @1255 V175HA_7 2.0
    @1257 V175HA_8 2.0
    @1259 V175HB_1 2.0
    @1261 V175HB_2 2.0
    @1263 V175HB_3 2.0
    @1265 V175HB_4 2.0
    @1267 V175HB_5 2.0
    @1269 V175HB_6 2.0
    @1271 V175HB_7 2.0
    @1273 V175HB_8 2.0
    @1275 V175HC_1 2.0
    @1277 V175HC_2 2.0
    @1279 V175HC_3 2.0
    @1281 V175HC_4 2.0
    @1283 V175HC_5 2.0
    @1285 V175HC_6 2.0
    @1287 V175HC_7 2.0
    @1289 V175HC_8 2.0
    @1291 V175HD_1 2.0
    @1293 V175HD_2 2.0
    @1295 V175HD_3 2.0
    @1297 V175HD_4 2.0
    @1299 V175HD_5 2.0
    @1301 V175HD_6 2.0
    @1303 V175HD_7 2.0
    @1305 V175HD_8 2.0
    @1307 V175HE_1 2.0
    @1309 V175HE_2 2.0
    @1311 V175HE_3 2.0
    @1313 V175HE_4 2.0
    @1315 V175HE_5 2.0
    @1317 V175HE_6 2.0
    @1319 V175HE_7 2.0
    @1321 V175HE_8 2.0
    @1323 V175HZ_1 2.0
    @1325 V175HZ_2 2.0
    @1327 V175HZ_3 2.0
    @1329 V175HZ_4 2.0
    @1331 V175HZ_5 2.0
    @1333 V175HZ_6 2.0
    @1335 V175HZ_7 2.0
    @1337 V175HZ_8 2.0
    @1339 V176A    1.0
    @1340 V176B    1.0
    @1341 V176C    1.0
    @1342 V177A    1.0
    @1343 V177B    1.0
    @1344 V177C    1.0
    @1345 V177D    1.0
    @1346 V177E    1.0
    @1347 V177F    1.0
    @1348 V177G    1.0
    @1349 V178B    1.0
    @1350 V184A    2.0
    @1352 V185     1.0
    @1353 V186A    2.0
    @1355 V187     1.0
    @1356 V189     1.0
    @1357 V190     1.0
    @1358 V192FC   1.0
    @1359 V192CP   1.0
    @1360 V192A    1.0
    @1361 V192B    1.0
    @1362 V192C    1.0
    @1363 V192T    1.0
    @1364 V193FC   1.0
    @1365 V193CP   1.0
    @1366 V193WA   1.0
    @1367 V193WB   1.0
    @1368 V193UA   1.0
    @1369 V193UB   1.0
    @1370 V193T    1.0
    @1371 V193PE   1.0
    @1372 V193PG   1.0
    @1373 V193TE   1.0
    @1374 V193TG   1.0
    @1375 V194FC   1.0
    @1376 V194CP   1.0
    @1377 V194A    1.0
    @1378 V194B    1.0
    @1379 V194T    1.0
    @1380 V195FC   1.0
    @1381 V195CP   1.0
    @1382 V195A    1.0
    @1383 V195B    1.0
    @1384 V195C    1.0
    @1385 V195T    1.0
    @1386 VT101    1.0
    @1387 VT102    1.0
    @1388 VT103    1.0
    @1389 VT104    1.0
    @1390 VT105    1.0
    @1391 VT106    1.0
    @1392 VT107    1.0
    @1393 VT201    1.0
    @1394 VT202    1.0
    @1395 VT301    1.0
    @1396 VT302    1.0
    @1397 VT303    1.0
    @1398 VT801    1.0
    @1399 VT802    1.0
    @1400 VT803    1.0
    @1401 VT804    1.0
    @1402 VT805    1.0
    @1403 VT806    1.0
    @1404 VT807    1.0
    @1405 VT808    1.0
    @1406 VT809    1.0
    @1407 VT810    1.0
    @1408 VT811    1.0
    @1409 VT812    1.0
    @1410 VT813    1.0
    @1411 VT814    1.0
    @1412 VT815    1.0
    @1413 VT816    1.0
    @1414 VT817    1.0
    @1415 VT818    1.0
    @1416 VT819    1.0
    @1417 VT820    1.0
    @1418 VT821    1.0
    @1419 VT822    1.0
    @1420 VT823    1.0
    @1421 VT824    1.0
    @1422 VT825    1.0
    @1423 VT826    1.0
    @1424 VT827    1.0
    @1425 VT828    1.0
    @1426 VT829    1.0
    @1427 VT830    1.0
    @1428 VT1201   1.0
    @1429 VT1401   1.0
    @1430 VT1501   1.0
    @1431 SF600A   1.0
    @1432 SF601A   1.0
    @1433 SF603B   1.0
    @1434 SF605B   1.0
    @1435 SF620A   1.0
    @1436 V200B    2.0
    @1438 V200C    2.0
    @1440 V200D    2.0
    @1442 V200E    2.0
    @1444 V200G    2.0
    @1446 V200H    2.0
    @1448 V200I    2.0
    @1450 V200J    2.0
    @1452 V201B    2.0
    @1454 V201C    2.0
    @1456 V201D    2.0
    @1458 V201E    2.0
    @1460 V201G    2.0
    @1462 V201H    2.0
    @1464 V201I    2.0
    @1466 V201J    2.0
    @1468 V230SRC  2.0
    @1470 V230     1.0
    @1471 V231A1   1.0
    @1472 V231C    1.0
    @1473 V231F    1.0
    @1474 V231G    1.0
    @1475 V231J    1.0
    @1476 V231L    1.0
    @1477 V231O    1.0
    @1478 V231P    1.0
    @1479 V231Q    1.0
    @1480 V231R    1.0
    @1481 V231S    1.0
    @1482 V231T    1.0
    @1483 V231U    1.0
    @1484 V231V    1.0
    @1485 V231W    1.0
    @1486 V231XA   1.0
    @1487 V231XB   1.0
    @1488 V231XC   1.0
    @1489 V231XD   1.0
    @1490 V231XE   1.0
    @1491 V231XF   1.0
    @1492 V231XG   1.0
    @1493 V231XH   1.0
    @1494 V242     1.0
    @1495 V246     1.0
    @1496 V247A    1.0
    @1497 V247B    1.0
    @1498 V247C    1.0
    @1499 V247D    1.0
    @1500 V247E    1.0
    @1501 V247F    1.0
    @1502 V247G    1.0
    @1503 V263     1.0
    @1504 V264SRC  2.0
    @1506 V264     1.0
    @1507 V264A1   1.0
    @1508 V264C    1.0
    @1509 V264F    1.0
    @1510 V264G    1.0
    @1511 V264J    1.0
    @1512 V264L    1.0
    @1513 V264O    1.0
    @1514 V264P    1.0
    @1515 V264Q    1.0
    @1516 V264R    1.0
    @1517 V264S    1.0
    @1518 V264T    1.0
    @1519 V264U    1.0
    @1520 V264V    1.0
    @1521 V264W    1.0
    @1522 V264XE   1.0
    @1523 V264XF   1.0
    @1524 V264XG   1.0
    @1525 V264XH   1.0
    @1526 V264XI   1.0
    @1527 V264XJ   1.0
    @1528 V265B    1.0
    @1529 V265C    1.0
    @1530 V265D    1.0
    @1531 V265E    1.0
    @1532 V265F    1.0
    @1533 V265G    1.0
    @1534 V265H    1.0
    @1535 V266A    1.0
    @1536 V266B    1.0
    @1537 V266D    1.0
    @1538 V266E    1.0
    @1539 V266F    1.0
    @1540 V266G    1.0
    @1541 V266X    1.0
    @1542 V267A    1.0
    @1543 V267B    1.0
    @1544 V267C    1.0
    @1545 V267D    1.0
    @1546 V267E    1.0
    @1547 V267F    1.0
    @1548 V271     1.0
    @1549 V271A    1.0
    @1550 V272A    1.0
    @1551 V272B    1.0
    @1552 V272C    1.0
    @1553 V272D    1.0
    @1554 V272F    1.0
    @1555 V273A    1.0
    @1556 V273B    1.0
    @1557 V280A    1.0
    @1558 V280X    1.0
    @1559 LV200    1.0
    @1560 LV203A   1.0
    @1561 LV203B   1.0
    @1562 LV203C   1.0
    @1563 LV203D   1.0
    @1564 LV203E   1.0
    @1565 LV203F   1.0
    @1566 LV203G   1.0
    @1567 LV203H   1.0
    @1568 LV206B   1.0
    @1569 LV221    1.0
    @1570 LV222A   1.0
    @1571 LV224    1.0
    @1572 LV225    1.0
    @1573 LV226    1.0
    @1574 SF1015A  1.0
    @1575 SF1015BC 1.0
    @1576 SF1015D  1.0
    @1577 SF1015E  1.0
    @1578 SF1015FG 1.0
    @1579 V301A    2.0
    @1581 V303A    1.0
    @1582 V303B    1.0
    @1583 V303D    1.0
    @1584 V303E    1.0
    @1585 V303G    1.0
    @1586 V303H    1.0
    @1587 V303I    1.0
    @1588 V303J    1.0
    @1589 V303K    1.0
    @1590 V303L    1.0
    @1591 V303O    1.0
    @1592 V303Q    1.0
    @1593 V303R    1.0
    @1594 V303S    1.0
    @1595 V303X    1.0
    @1596 V306     1.0
    @1597 V307A    1.0
    @1598 V307B    1.0
    @1599 V307C    1.0
    @1600 V308     1.0
    @1601 V308A    1.0
    @1602 V324     1.0
    @1603 V325A    1.0
    @1604 V325D    1.0
    @1605 V325F    1.0
    @1606 V325G    1.0
    @1607 V325H    1.0
    @1608 V325I    1.0
    @1609 V327A    1.0
    @1610 V327X    1.0
    @1611 V330SRC  2.0
    @1613 V330     1.0
    @1614 V331A1   1.0
    @1615 V331C    1.0
    @1616 V331F    1.0
    @1617 V331G    1.0
    @1618 V331J    1.0
    @1619 V331L    1.0
    @1620 V331O    1.0
    @1621 V331P    1.0
    @1622 V331Q    1.0
    @1623 V331R    1.0
    @1624 V331S    1.0
    @1625 V331T    1.0
    @1626 V331U    1.0
    @1627 V331V    1.0
    @1628 V331W    1.0
    @1629 V333B    1.0
    @1630 V333C    1.0
    @1631 V333D    1.0
    @1632 V333E    1.0
    @1633 V333F    1.0
    @1634 V333G    1.0
    @1635 V334A    1.0
    @1636 V334B    1.0
    @1637 V334C    1.0
    @1638 V334D    1.0
    @1639 V335A    1.0
    @1640 V335B    1.0
    @1641 V335C    1.0
    @1642 V335E    1.0
    @1643 V335F    1.0
    @1644 V336A    1.0
    @1645 V336B    1.0
    @1646 V336C    1.0
    @1647 V336D    1.0
    @1648 V336F    1.0
    @1649 V336G    1.0
    @1650 V342     1.0
    @1651 V343A    1.0
    @1652 V343B    1.0
    @1653 V343C    1.0
    @1654 V343D    1.0
    @1655 V343E    1.0
    @1656 V343F    1.0
    @1657 V344     1.0
    @1658 LV300    1.0
    @1659 LV302A   1.0
    @1660 LV302B   1.0
    @1661 LV303A   1.0
    @1662 LV303B   1.0
    @1663 LV303D   1.0
    @1664 LV303E   1.0
    @1665 LV303G   1.0
    @1666 LV303H   1.0
    @1667 LV303I   1.0
    @1668 LV303J   1.0
    @1669 LV303K   1.0
    @1670 LV303L   1.0
    @1671 LV303O   1.0
    @1672 SLV303A  1.0
    @1673 SLV303B  1.0
    @1674 SLV303E  1.0
    @1675 SLV303G  1.0
    @1676 SLV303I  1.0
    @1677 SLV303K  1.0
    @1678 SLV303L  1.0
    @1679 LV306A   1.0
    @1680 LV306B   1.0
    @1681 LV320A   1.0
    @1682 LV320C   1.0
    @1683 LV320D   1.0
    @1684 LV320E   1.0
    @1685 LV320F   1.0
    @1686 SF924A   1.0
    @1687 SF924B   1.0
    @1688 SF924C   1.0
    @1689 SF924D   1.0
    @1690 SF1308_01 1.0
    @1691 SF1308_02 1.0
    @1692 SF1308_03 1.0
    @1693 V401A    2.0
    @1695 V406     1.0
    @1696 V407A    1.0
    @1697 V407B    1.0
    @1698 V407C    1.0
    @1699 V407D    1.0
    @1700 V407E    1.0
    @1701 V407F    1.0
    @1702 V407G    1.0
    @1703 V407H    1.0
    @1704 V409B    1.0
    @1705 V409C    1.0
    @1706 V409E    1.0
    @1707 V409F    1.0
    @1708 V409G    1.0
    @1709 V409H    1.0
    @1710 V409I    1.0
    @1711 V409J    1.0
    @1712 V410     1.0
    @1713 V410A    1.0
    @1714 V411A    1.0
    @1715 V411B    2.0
    @1717 V424     1.0
    @1718 V430SRC  2.0
    @1720 V430     1.0
    @1721 V431A1   1.0
    @1722 V431C    1.0
    @1723 V431F    1.0
    @1724 V431G    1.0
    @1725 V431J    1.0
    @1726 V431L    1.0
    @1727 V431O    1.0
    @1728 V431P    1.0
    @1729 V431Q    1.0
    @1730 V431R    1.0
    @1731 V431S    1.0
    @1732 V431T    1.0
    @1733 V431U    1.0
    @1734 V431V    1.0
    @1735 V431W    1.0
    @1736 V432A    1.0
    @1737 V432B    1.0
    @1738 V432C    1.0
    @1739 V432D    1.0
    @1740 V432E    1.0
    @1741 V432F    1.0
    @1742 V432G    1.0
    @1743 V433B    1.0
    @1744 V433C    1.0
    @1745 V433D    1.0
    @1746 V433F    1.0
    @1747 V433G    1.0
    @1748 V433H    1.0
    @1749 V433I    1.0
    @1750 V433J    1.0
    @1751 V433K    1.0
    @1752 V433L    1.0
    @1753 V434A    1.0
    @1754 V434B    1.0
    @1755 V434X    1.0
    @1756 V435A    1.0
    @1757 V435B    1.0
    @1758 V460     1.0
    @1759 V461A    1.0
    @1760 V461B    1.0
    @1761 V461C    1.0
    @1762 V461D    1.0
    @1763 V461E    1.0
    @1764 V461F    1.0
    @1765 V462A    1.0
    @1766 V462B    1.0
    @1767 V462C    1.0
    @1768 V462D    1.0
    @1769 V462E    1.0
    @1770 V462F    1.0
    @1771 V462G    1.0
    @1772 V462H    1.0
    @1773 SF1401A  2.0
    @1775 SF1403A  2.0
    @1777 SF1406A1 1.0
    @1778 SF1406A2 1.0
    @1779 SF1406A3 1.0
    @1780 SF1406A4 1.0
    @1781 SF1406A5 1.0
    @1782 SF1408A  1.0
    @1783 SF1418_1 1.0
    @1784 SF1418_2 1.0
    @1785 SF1418_3 1.0
    @1786 V501     3.0
    @1789 V501A    3.0
    @1792 V502     1.0
    @1793 V503     1.0
    @1794 V506C    1.0
    @1795 V507A    1.0
    @1796 V507B    1.0
    @1797 V507C    1.0
    @1798 V507D    1.0
    @1799 V507E    1.0
    @1800 V507F    1.0
    @1801 V507G    1.0
    @1802 V507H    1.0
    @1803 V507I    1.0
    @1804 V507J    1.0
    @1805 V507K    1.0
    @1806 V507L    1.0
    @1807 V507M    1.0
    @1808 V507N    1.0
    @1809 V507O    1.0
    @1810 V507P    1.0
    @1811 V530SRC  2.0
    @1813 V530     1.0
    @1814 V531A1   1.0
    @1815 V531C    1.0
    @1816 V531F    1.0
    @1817 V531G    1.0
    @1818 V531J    1.0
    @1819 V531L    1.0
    @1820 V531O    1.0
    @1821 V531P    1.0
    @1822 V531Q    1.0
    @1823 V531R    1.0
    @1824 V531S    1.0
    @1825 V531T    1.0
    @1826 V531U    1.0
    @1827 V531V    1.0
    @1828 V531W    1.0
    @1829 V531XD   1.0
    @1830 V531XE   1.0
    @1831 V531XF   1.0
    @1832 V531XG   1.0
    @1833 V531XH   1.0
    @1834 V531XI   1.0
    @1835 V531XJ   1.0
    @1836 V531XK   1.0
    @1837 V531XL   1.0
    @1838 V531XM   1.0
    @1839 V531XN   1.0
    @1840 V531XO   1.0
    @1841 V531XP   1.0
    @1842 V531XQ   1.0
    @1843 V533B    1.0
    @1844 V533C    1.0
    @1845 V533D    1.0
    @1846 V533E    1.0
    @1847 V533F    1.0
    @1848 V533G    1.0
    @1849 V533H    1.0
    @1850 V533I    1.0
    @1851 V533J    1.0
    @1852 V534A    1.0
    @1853 V534C    1.0
    @1854 V534D    1.0
    @1855 V534E    1.0
    @1856 V534G    1.0
    @1857 V534I    1.0
    @1858 V534M    1.0
    @1859 V534N    1.0
    @1860 V534O    1.0
    @1861 V534P    1.0
    @1862 V534Q    1.0
    @1863 V534R    1.0
    @1864 V536A    1.0
    @1865 V536B    1.0
    @1866 V536C    1.0
    @1867 V536D    1.0
    @1868 V536E    1.0
    @1869 V536F    1.0
    @1870 V537A    1.0
    @1871 V537B    1.0
    @1872 V537C    1.0
    @1873 V537D    1.0
    @1874 V537E    1.0
    @1875 V545     1.0
    @1876 V546A    2.0
    @1878 V546B    2.0
    @1880 V554A    1.0
    @1881 V554B    1.0
    @1882 V554C    1.0
    @1883 V554D    1.0
    @1884 V554E    1.0
    @1885 V554F    1.0
    @1886 V554G    1.0
    @1887 V554H    1.0
    @1888 V554I    1.0
    @1889 V554J    1.0
    @1890 V554K    1.0
    @1891 V554L    1.0
    @1892 V557C    1.0
    @1893 V557E    1.0
    @1894 V557F    1.0
    @1895 V557G    1.0
    @1896 V557H    1.0
    @1897 V557I    1.0
    @1898 V557J    1.0
    @1899 V557K    1.0
    @1900 V557L    1.0
    @1901 V557M    1.0
    @1902 V558     1.0
    @1903 V559     1.0
    @1904 V567     1.0
    @1905 V568     1.0
    @1906 V569     1.0
    @1907 V570     1.0
    @1908 SF1605   1.0
    @1909 SF1607   1.0
    @1910 SF1617A  1.0
    @1911 SF1619A  1.0
    @1912 SF1625AB 1.0
    @1913 SF16281  1.0
    @1914 SF16282  1.0
    @1915 SF16283  1.0
    @1916 SF16284  1.0
    @1917 SF16285  1.0
    @1918 V600A    1.0
    @1919 V601A    2.0
    @1921 V624     1.0
    @1922 V625A    1.0
    @1923 V625B    1.0
    @1924 V625C    1.0
    @1925 V625D    1.0
    @1926 V625E    1.0
    @1927 V625F    1.0
    @1928 V625G    1.0
    @1929 V626A    1.0
    @1930 V626B    1.0
    @1931 V626C    1.0
    @1932 V626D    1.0
    @1933 V626E    1.0
    @1934 V626F    1.0
    @1935 V626G    1.0
    @1936 V627A    1.0
    @1937 V627X    1.0
    @1938 V628     1.0
    @1939 V630SRC  2.0
    @1941 V630     1.0
    @1942 V631A1   1.0
    @1943 V631C    1.0
    @1944 V631F    1.0
    @1945 V631G    1.0
    @1946 V631J    1.0
    @1947 V631L    1.0
    @1948 V631O    1.0
    @1949 V631P    1.0
    @1950 V631Q    1.0
    @1951 V631R    1.0
    @1952 V631S    1.0
    @1953 V631T    1.0
    @1954 V631U    1.0
    @1955 V631V    1.0
    @1956 V631W    1.0
    @1957 V640A    1.0
    @1958 V640B    1.0
    @1959 V640C    1.0
    @1960 V640D    1.0
    @1961 V640E    1.0
    @1962 V640F    1.0
    @1963 V642     1.0
    @1964 V642A    1.0
    @1965 V650     1.0
    @1966 SF18091  1.0
    @1967 SF18092  1.0
    @1968 SF18093  1.0
    @1969 SF18094  1.0
    @1970 SF18095  1.0
    @1971 V805A    1.0
    @1972 V805B    1.0
    @1973 V805X    1.0
    @1974 V810A    1.0
    @1975 V810B    1.0
    @1976 V810C    1.0
    @1977 V810X    1.0
    @1978 V810Y    1.0
    @1979 V813     1.0
    @1980 V815     2.0
    @1982 V820A    1.0
    @1983 V820B    1.0
    @1984 V820C    1.0
    @1985 V820D    1.0
    @1986 V820E    1.0
    @1987 V820X    1.0
    @1988 V820Y    1.0
    @1989 V821     1.0
    @1990 V821E    1.0
    @1991 V834A    1.0
    @1992 V834B    1.0
    @1993 V834C    1.0
    @1994 V834D    1.0
    @1995 V834E    1.0
    @1996 V834Y    1.0
    @1997 V835A    1.0
    @1998 V835B    1.0
    @1999 V835C    1.0
    @2000 V835D    1.0
    @2001 V835Y    1.0
    @2002 V836     1.0
    @2003 V837     1.0
    @2004 V838A    1.0
    @2005 V838B    1.0
    @2006 V838C    1.0
    @2007 V838D    1.0
    @2008 V838Y    1.0
    @2009 V839     1.0
    @2010 V840A    1.0
    @2011 V840A1   1.0
    @2012 V840A2   1.0
    @2013 V840A3   1.0
    @2014 V840D    1.0
    @2015 V840E    1.0
    @2016 V840E1   1.0
    @2017 V840E2   1.0
    @2018 V840F    1.0
    @2019 V840F1   1.0
    @2020 V840F2   1.0
    @2021 V840F3   1.0
    @2022 V840G    1.0
    @2023 V840G1   1.0
    @2024 V840I    1.0
    @2025 V840J    1.0
    @2026 V840K    1.0
    @2027 V840L    1.0
    @2028 V840M    1.0
    @2029 V840N    1.0
    @2030 V840O    1.0
    @2031 V840P    1.0
    @2032 V840Y    1.0
    @2033 V841A1   1.0
    @2034 V841A2   1.0
    @2035 V841B    1.0
    @2036 V841C    1.0
    @2037 V841D    1.0
    @2038 V841E    1.0
    @2039 V841F    1.0
    @2040 V841G    1.0
    @2041 V842A1   1.0
    @2042 V842A2   1.0
    @2043 V842B    1.0
    @2044 V842C    1.0
    @2045 V842G    1.0
    @2046 V843A    1.0
    @2047 V843B    1.0
    @2048 V843C    1.0
    @2049 V843E    1.0
    @2050 V843F    1.0
    @2051 V843G    1.0
    @2052 V843H    1.0
    @2053 V843I    1.0
    @2054 V843J    1.0
    @2055 V843K    1.0
    @2056 V843L    1.0
    @2057 V843M    1.0
    @2058 V843N    1.0
    @2059 V844A    1.0
    @2060 V844B    1.0
    @2061 V844C    1.0
    @2062 V844F    1.0
    @2063 V844G    1.0
    @2064 V844H    1.0
    @2065 V844I    1.0
    @2066 V844J    1.0
    @2067 V844K    1.0
    @2068 V844N    1.0
    @2069 V845A    1.0
    @2070 V845B    1.0
    @2071 V845C    1.0
    @2072 V845D    1.0
    @2073 V845G    1.0
    @2074 V845H    1.0
    @2075 V845I    1.0
    @2076 V845J    1.0
    @2077 V845K    1.0
    @2078 V846A    1.0
    @2079 V846B    1.0
    @2080 V846C    1.0
    @2081 V846D    1.0
    @2082 V846H    1.0
    @2083 V847A    1.0
    @2084 V847B    1.0
    @2085 V847C    1.0
    @2086 V847D    1.0
    @2087 V847E    1.0
    @2088 V848B    1.0
    @2089 V849A    1.0
    @2090 V849B    1.0
    @2091 V849B1   1.0
    @2092 V850A    1.0
    @2093 V850B    1.0
    @2094 V850B1   1.0
    @2095 V851     1.0
    @2096 V851A    1.0
    @2097 V851B    1.0
    @2098 V852A    1.0
    @2099 V852B    1.0
    @2100 V852C    1.0
    @2101 V852D    1.0
    @2102 V855A    1.0
    @2103 V855B    1.0
    @2104 V855C    1.0
    @2105 V855D    1.0
    @2106 V856A    1.0
    @2107 V856B    1.0
    @2108 V857B    1.0
    @2109 V857C    1.0
    @2110 V857D    1.0
    @2111 V859A    1.0
    @2112 V859B    1.0
    @2113 V859C    1.0
    @2114 V859D    1.0
    @2115 V859E    1.0
    @2116 V859F    1.0
    @2117 V859G    1.0
    @2118 V859GG   1.0
    @2119 V859H    1.0
    @2120 V859I    1.0
    @2121 V860A    1.0
    @2122 V860B    1.0
    @2123 V860C    1.0
    @2124 V860D    1.0
    @2125 V860F    1.0
    @2126 V861A    1.0
    @2127 V861B    1.0
    @2128 V861C    1.0
    @2129 V861D    1.0
    @2130 V861E    1.0
    @2131 V863     1.0
    @2132 V864A    1.0
    @2133 V864B    1.0
    @2134 V864C    1.0
    @2135 V864D    1.0
    @2136 V864F    1.0
    @2137 V864E    1.0
    @2138 V865A    1.0
    @2139 V865B    1.0
    @2140 V865C    1.0
    @2141 V865D    1.0
    @2142 V865F    1.0
    @2143 V865E    1.0
    @2144 V866A1   1.0
    @2145 V866C    1.0
    @2146 V866F    1.0
    @2147 V866G    1.0
    @2148 V866J    1.0
    @2149 V866L    1.0
    @2150 V866O    1.0
    @2151 V866P    1.0
    @2152 V866Q    1.0
    @2153 V866R    1.0
    @2154 V866S    1.0
    @2155 V866T    1.0
    @2156 V866U    1.0
    @2157 V866V    1.0
    @2158 V866W    1.0
    @2159 V870A    1.0
    @2160 V870B    1.0
    @2161 V870C    1.0
    @2162 V870D    1.0
    @2163 V870E    1.0
    @2164 V870F    1.0
    @2165 V870X    1.0
    @2166 V871     1.0
    @2167 V872     1.0
    @2168 V873A    1.0
    @2169 V873B    1.0
    @2170 V874A    1.0
    @2171 V874B    1.0
    @2172 V874C    1.0
    @2173 V874D    1.0
    @2174 V874E    1.0
    @2175 V874X    1.0
    @2176 V875     1.0
    @2177 V876A    1.0
    @2178 V876B    1.0
    @2179 V877     1.0
    @2180 SF802D   1.0
    @2181 SF802EB1 1.0
    @2182 SF802EB2 1.0
    @2183 SF802EB3 1.0
    @2184 SF802EC1 1.0
    @2185 SF802EC3 1.0
    @2186 SF803A   1.0
    @2187 SF8081   1.0
    @2188 SF8082   1.0
    @2189 SF8083   1.0
    @2190 SF8084   1.0
    @2191 SF8085   1.0
    @2192 SF809A   1.0
    @2193 SF835A   1.0
    @2194 SF835BC  1.0
    @2195 SF835D   1.0
    @2196 SF840X   1.0
    @2197 SF847C   1.0
    @2198 SF847DE  1.0
    @2199 SF863A   1.0
    @2200 SF873AB  1.0
    @2201 SF874A   1.0
    @2202 SF874BC  1.0
    @2203 SF874D   1.0
    @2204 SF874EF  1.0
    @2205 V902A    1.0
    @2206 V902B    1.0
    @2207 V903_02  1.0
    @2208 V903_05  1.0
    @2209 V903_06  1.0
    @2210 V903_08  1.0
    @2211 V903_10  1.0
    @2212 V903_13  1.0
    @2213 V903_14  1.0
    @2214 V903_15  1.0
    @2215 V903_16  1.0
    @2216 V903_17  1.0
    @2217 V903_18  1.0
    @2218 V903_19  1.0
    @2219 V903_20  1.0
    @2220 V904_03  1.0
    @2221 V904_04  1.0
    @2222 V904_05  1.0
    @2223 V904_06  1.0
    @2224 V905_03  1.0
    @2225 V905_08  1.0
    @2226 V905_14  1.0
    @2227 V905_18  1.0
    @2228 V905_19  1.0
    @2229 V905_20  1.0
    @2230 V905_21  1.0
    @2231 V905_22  1.0
    @2232 V906_03  1.0
    @2233 V906_07  1.0
    @2234 V906_08  1.0
    @2235 V906_11  1.0
    @2236 V906_12  1.0
    @2237 V907_03  1.0
    @2238 V907_04  1.0
    @2239 V907_05  1.0
    @2240 V907_06  1.0
    @2241 V907_07  1.0
    @2242 V907_08  1.0
    @2243 V908_02  1.0
    @2244 V908_03  1.0
    @2245 V908_04  1.0
    @2246 V908_05  1.0
    @2247 V909_02  1.0
    @2248 V909_04  1.0
    @2249 V909_05  1.0
    @2250 V909_06  1.0
    @2251 V910_08  1.0
    @2252 V911_02  1.0
    @2253 V913_13  1.0
    @2254 V914_08  1.0
    @2255 V915_04  1.0
    @2256 V916_09  1.0
    @2257 V917_06  1.0
    @2258 V918_06  1.0
    @2259 V921_04  1.0
    @2260 V921_05  1.0
    @2261 V921_06  1.0
    @2262 V921_07  1.0
    @2263 V921_08  1.0
    @2264 V921_09  1.0
    @2265 V921_10  1.0
    @2266 V921_11  1.0
    @2267 V910_03  1.0
    @2268 V910_04  1.0
    @2269 V910_06  1.0
    @2270 V910_07  1.0
    @2271 V913_01  1.0
    @2272 V913_02  1.0
    @2273 V913_04  1.0
    @2274 V913_05  1.0
    @2275 V913_06  1.0
    @2276 V913_07  1.0
    @2277 V913_10  1.0
    @2278 V913_11  1.0
    @2279 V913_12  1.0
    @2280 V914_05  1.0
    @2281 V914_06  1.0
    @2282 V908_06  1.0
    @2283 V908_07  1.0
    @2284 V914_07  1.0
    @2285 V915_01  1.0
    @2286 V915_02  1.0
    @2287 V915_03  1.0
    @2288 V916_01  1.0
    @2289 V916_03  1.0
    @2290 V916_06  1.0
    @2291 V916_07  1.0
    @2292 V916_08  1.0
    @2293 V906_09  1.0
    @2294 V906_10  1.0
    @2295 V917_04  1.0
    @2296 V917_05  1.0
    @2297 V918_01  1.0
    @2298 V918_02  1.0
    @2299 V918_04  1.0
    @2300 V918_05  1.0
    @2301 V919_03  1.0
    @2302 V920_02  1.0
    @2303 V920_03  1.0
    @2304 V920_04  1.0
    @2305 V921_01  1.0
    @2306 V921_02  1.0
    @2307 V922_03  1.0
    @2308 V922_04  1.0
    @2309 V922_05  1.0
    @2310 V922_11  1.0
    @2311 V922_12  1.0
    @2312 V922_13  1.0
    @2313 V922_14  1.0
    @2314 V922_15  1.0
    @2315 V922_16  1.0
    @2316 V922_17  1.0
    @2317 V922_18  1.0
    @2318 V922_19  1.0
    @2319 V923_01  1.0
    @2320 V923_02  1.0
    @2321 V923_03  1.0
    @2322 V923_04  1.0
    @2323 V923_05  1.0
    @2324 V923_06  1.0
    @2325 V923_07  1.0
    @2326 V923_08  1.0
    @2327 V923_09  1.0
    @2328 V923_11  1.0
    @2329 V923_12  1.0
    @2330 V924_01  1.0
    @2331 V924_02  1.0
    @2332 V924_03  1.0
    @2333 V924_05  1.0
    @2334 V926     1.0
    @2335 V927     1.0
    @2336 V928A    1.0
    @2337 V928B1   1.0
    @2338 V928B2   1.0
    @2339 V928C    1.0
    @2340 V928D    1.0
    @2341 V936     1.0
    @2342 V946_06  1.0
    @2343 V946_07  1.0
    @2344 V946_09  1.0
    @2345 V946_12  1.0
    @2346 V946_13  1.0
    @2347 V946_16  1.0
    @2348 V946_17  1.0
    @2349 V946_18  1.0
    @2350 V946_19  1.0
    @2351 V950A    1.0
    @2352 V950B    1.0
    @2353 V951     1.0
    @2354 V952     1.0
    @2355 V953A    1.0
    @2356 V953B1   1.0
    @2357 V953B2   1.0
    @2358 V953C    1.0
    @2359 V953D    1.0
    @2360 V953E    1.0
    @2361 V955_01  1.0
    @2362 V955_02  1.0
    @2363 V955_03  1.0
    @2364 V955_04  1.0
    @2365 V955_05  1.0
    @2366 V955_06  1.0
    @2367 V955_07  1.0
    @2368 V955_08  1.0
    @2369 V955_09  1.0
    @2370 V955_10  1.0
    @2371 V955_11  1.0
    @2372 V956_01  1.0
    @2373 V956_02  1.0
    @2374 V956_03  1.0
    @2375 V956_04  1.0
    @2376 V956_05  1.0
    @2377 V956_06  1.0
    @2378 V957_01  1.0
    @2379 V957_02  1.0
    @2380 V957_03  1.0
    @2381 V957_04  1.0
    @2382 V957_05  1.0
    @2383 V957_06  1.0
    @2384 V957_07  1.0
    @2385 V957_08  1.0
    @2386 V957_09  1.0
    @2387 V957_10  1.0
    @2388 V958_01  1.0
    @2389 V958_02  1.0
    @2390 V958_03  1.0
    @2391 V958_04  1.0
    @2392 V958_05  1.0
    @2393 V958_06  1.0
    @2394 V958_07  1.0
    @2395 V958_08  1.0
    @2396 V958_09  1.0
    @2397 V958_10  1.0
    @2398 V958_11  1.0
    @2399 V960A    1.0
    @2400 V960B    1.0
    @2401 V961     1.0
    @2402 V962     1.0
    @2403 V963A    1.0
    @2404 V963B1   1.0
    @2405 V963B2   1.0
    @2406 V963C    1.0
    @2407 V963D    1.0
    @2408 SF905A_01 1.0
    @2409 SF905A_02 1.0
    @2410 SF905A_03 1.0
    @2411 SF905A_04 1.0
    @2412 SF908A_01 1.0
    @2413 SF908A_02 1.0
    @2414 SF908A_03 1.0
    @2415 SF908A_04 1.0
    @2416 SF908A_05 1.0
    @2417 SF908A_06 1.0
    @2418 SF908A_07 1.0
    @2419 SF908A_08 1.0
    @2420 SF908A_09 1.0
    @2421 SF908A_10 1.0
    @2422 SF908A_11 1.0
    @2423 SF908A_12 1.0
    @2424 SF908A_13 1.0
    @2425 SF908A_14 1.0
    @2426 SF912A_01 1.0
    @2427 SF912A_02 1.0
    @2428 V1001    2.0
    @2430 V1002    1.0
    @2431 V1003    1.0
    @2432 V1004    1.0
    @2433 V1005A   1.0
    @2434 V1005B   1.0
    @2435 V1005C   1.0
    @2436 V1005D   1.0
    @2437 V1005E   1.0
    @2438 V1005F   1.0
    @2439 V1005X   1.0
    @2440 V1006    1.0
    @2441 V1007A   1.0
    @2442 V1007X   1.0
    @2443 SF1712AB 1.0
    @2444 V1100A   1.0
    @2445 V1106    1.0
    @2446 V1107A   1.0
    @2447 V1107B   1.0
    @2448 V1107C   1.0
    @2449 V1109A   1.0
    @2450 V1109B   1.0
    @2451 V1109C   1.0
    @2452 V1109D   1.0
    @2453 SV1100A  1.0
    @2454 V1115    1.0
    @2455 V1138SRC 2.0
    @2457 V1138    1.0
    @2458 V1139A1  1.0
    @2459 V1139C   1.0
    @2460 V1139F   1.0
    @2461 V1139G   1.0
    @2462 V1139J   1.0
    @2463 V1139L   1.0
    @2464 V1139O   1.0
    @2465 V1139P   1.0
    @2466 V1139Q   1.0
    @2467 V1139R   1.0
    @2468 V1139S   1.0
    @2469 V1139T   1.0
    @2470 V1139U   1.0
    @2471 V1139V   1.0
    @2472 V1139W   1.0
    @2473 V1140    2.0
    @2475 V1141    1.0
    @2476 V1142    1.0
    @2477 V1143    1.0
    @2478 V1144    1.0
    @2479 SF19141  1.0
    @2480 SF19142  1.0
    @2481 SF19143  1.0
    @2482 SF19144  1.0
    @2483 SF19145  1.0
    @2484 SF1916AB 1.0
    @2485 V1206    2.0
    @2487 V1208A1  1.0
    @2488 V1208A2  1.0
    @2489 V1208F   1.0
    @2490 V1208G1  1.0
    @2491 V1208H   1.0
    @2492 V1208L   1.0
    @2493 V1208M   1.0
    @2494 V1208S   1.0
    @2495 V1215A   1.0
    @2496 V1215B   1.0
    @2497 V1215X   1.0
    @2498 V1215Y   1.0
    @2499 V1260    1.0
    @2500 V1273A   1.0
    @2501 V1273B   1.0
    @2502 V1273C   1.0
    @2503 V1273D   1.0
    @2504 V1273E   1.0
    @2505 V1273F   1.0
    @2506 V1273G   1.0
    @2507 V1273H   1.0
    @2508 V1273I   1.0
    @2509 V1273J   1.0
    @2510 V1273K   1.0
    @2511 V1273L   1.0
    @2512 V1273M   1.0
    @2513 V1273N   1.0
    @2514 V1273Y   1.0
    @2515 V1274    1.0
    @2516 V1275    1.0
    @2517 V1276SRC 2.0
    @2519 V1276    1.0
    @2520 V1277A1  1.0
    @2521 V1277C   1.0
    @2522 V1277F   1.0
    @2523 V1277G   1.0
    @2524 V1277J   1.0
    @2525 V1277L   1.0
    @2526 V1277O   1.0
    @2527 V1277P   1.0
    @2528 V1277Q   1.0
    @2529 V1277R   1.0
    @2530 V1277S   1.0
    @2531 V1277T   1.0
    @2532 V1277U   1.0
    @2533 V1277V   1.0
    @2534 V1277W   1.0
    @2535 V1278A   1.0
    @2536 V1278B   1.0
    @2537 V1278C   1.0
    @2538 V1278D   1.0
    @2539 V1278E   1.0
    @2540 V1278F   1.0
    @2541 V1278G   1.0
    @2542 V1278H   1.0
    @2543 V1278I   1.0
    @2544 V1278J   1.0
    @2545 V1278K   1.0
    @2546 V1278L   1.0
    @2547 V1278M   1.0
    @2548 V1278N   1.0
    @2549 V1278Y   1.0
    @2550 V1279    1.0
    @2551 V1280SRC 2.0
    @2553 V1280    1.0
    @2554 V1281A1  1.0
    @2555 V1281C   1.0
    @2556 V1281F   1.0
    @2557 V1281G   1.0
    @2558 V1281J   1.0
    @2559 V1281L   1.0
    @2560 V1281O   1.0
    @2561 V1281P   1.0
    @2562 V1281Q   1.0
    @2563 V1281R   1.0
    @2564 V1281S   1.0
    @2565 V1281T   1.0
    @2566 V1281U   1.0
    @2567 V1281V   1.0
    @2568 V1281W   1.0
    @2569 SF1505A1 1.0
    @2570 SF1505A2 1.0
    @2571 SF1505A3 1.0
    @2572 SF1505A4 1.0
    @2573 SF1505A5 1.0
    @2574 SF1508A  1.0
    @2575 V1307A   2.0
    @2577 V1327D   1.0
    @2578 V1327E   1.0
    @2579 V1327G   1.0
    @2580 V1327H   1.0
    @2581 V1327I   1.0
    @2582 V1327J   1.0
    @2583 V1328SRC 2.0
    @2585 V1328    1.0
    @2586 V1329A1  1.0
    @2587 V1329C   1.0
    @2588 V1329F   1.0
    @2589 V1329G   1.0
    @2590 V1329J   1.0
    @2591 V1329L   1.0
    @2592 V1329O   1.0
    @2593 V1329P   1.0
    @2594 V1329Q   1.0
    @2595 V1329R   1.0
    @2596 V1329S   1.0
    @2597 V1329T   1.0
    @2598 V1329U   1.0
    @2599 V1329V   1.0
    @2600 V1329W   1.0
    @2601 V1331    1.0
    @2602 V1332A   1.0
    @2603 V1332B   1.0
    @2604 V1332X   1.0
    @2605 V1337    1.0
    @2606 V1355    1.0
    @2607 V1356    1.0
    @2608 V1357    1.0
    @2609 V1358    1.0
    @2610 SF20041  1.0
    @2611 SF20042  1.0
    @2612 SF20043  1.0
    @2613 SF20044  1.0
    @2614 SF20045  1.0
    @2615 V1400A   1.0
    @2616 V1415A   1.0
    @2617 V1415B   1.0
    @2618 V1415C   1.0
    @2619 V1415D   1.0
    @2620 V1415E   1.0
    @2621 V1415F   1.0
    @2622 V1415G   1.0
    @2623 V1415H   1.0
    @2624 V1415I   1.0
    @2625 V1415J   1.0
    @2626 V1415K   1.0
    @2627 V1415L   1.0
    @2628 V1415M   1.0
    @2629 V1415N   1.0
    @2630 V1415X   1.0
    @2631 V1421A   1.0
    @2632 V1421X   1.0
    @2633 V1451    1.0
    @2634 V1452A   1.0
    @2635 V1452B   1.0
    @2636 V1452C   1.0
    @2637 V1452D   1.0
    @2638 V1452E   1.0
    @2639 V1452F   1.0
    @2640 V1452G   1.0
    @2641 V1452H   1.0
    @2642 V1452I   1.0
    @2643 V1452J   1.0
    @2644 V1452K   1.0
    @2645 V1452L   1.0
    @2646 V1452M   1.0
    @2647 V1452N   1.0
    @2648 V1452X   1.0
    @2649 SF2105AB 1.0
    @2650 V1515    1.0
    @2651 V1516A   1.0
    @2652 V1516B   1.0
    @2653 V1516C   1.0
    @2654 V1516D   1.0
    @2655 V1516E   1.0
    @2656 V1516F   1.0
    @2657 V1516J   1.0
    @2658 V1518B   1.0
    @2659 V1518C   1.0
    @2660 V1518D   1.0
    @2661 V1518E   1.0
    @2662 V1518F   1.0
    @2663 V1521A   1.0
    @2664 V1521B   1.0
    @2665 V1560    1.0
    @2666 V1561    1.0
    @2667 SF2205A  1.0
    @2668 V1601    1.0
    @2669 V1601A   1.0
    @2670 V1602A   1.0
    @2671 V1602X   1.0
    @2672 V1603    1.0
    @2673 V1603A   1.0
    @2674 V1604A   1.0
    @2675 V1604X   1.0
    @2676 V1605    1.0
    @2677 V1605A   1.0
    @2678 V1606A   1.0
    @2679 V1606X   1.0
    @2680 V1607SRC 1.0
    @2681 V1607A   1.0
    @2682 V1607B   1.0
    @2683 V1607C   1.0
    @2684 V1607D   1.0
    @2685 V1608A   1.0
    @2686 V1608B   1.0
    @2687 V1608C   1.0
    @2688 V1608D   1.0
    @2689 V1608E   1.0
    @2690 V1608F   1.0
    @2691 V1608G   1.0
    @2692 V1608H   1.0
    @2693 V1608I   1.0
    @2694 V1608J   1.0
    @2695 V1608K   1.0
    @2696 V1608L   1.0
    @2697 V1608M   1.0
    @2698 V1608N   1.0
    @2699 V1608O   1.0
    @2700 V1608P   1.0
    @2701 V1608Q   1.0
    @2702 V1608R   1.0
    @2703 V1609SRC 2.0
    @2705 V1609    1.0
    @2706 V1610A1  1.0
    @2707 V1610C   1.0
    @2708 V1610F   1.0
    @2709 V1610G   1.0
    @2710 V1610J   1.0
    @2711 V1610L   1.0
    @2712 V1610O   1.0
    @2713 V1610P   1.0
    @2714 V1610Q   1.0
    @2715 V1610R   1.0
    @2716 V1610S   1.0
    @2717 V1610T   1.0
    @2718 V1610U   1.0
    @2719 V1610V   1.0
    @2720 V1610W   1.0
    @2721 V1701    1.0
    @2722 V1702A   1.0
    @2723 V1702B   1.0
    @2724 V1702C   1.0
    @2725 V1702D   1.0
    @2726 V1702E   1.0
    @2727 V1702F   1.0
    @2728 V1703A   1.0
    @2729 V1703B   1.0
    @2730 V1703C   1.0
    @2731 V1703D   1.0
    @2732 V1703E   1.0
    @2733 V1704SRC 2.0
    @2735 V1704    1.0
    @2736 V1705A1  1.0
    @2737 V1705C   1.0
    @2738 V1705F   1.0
    @2739 V1705G   1.0
    @2740 V1705J   1.0
    @2741 V1705L   1.0
    @2742 V1705O   1.0
    @2743 V1705P   1.0
    @2744 V1705Q   1.0
    @2745 V1705R   1.0
    @2746 V1705S   1.0
    @2747 V1705T   1.0
    @2748 V1705U   1.0
    @2749 V1705V   1.0
    @2750 V1705W   1.0
    @2751 SF1720B  1.0
    @2752 SF1720BC 1.0
    @2753 SF1720D  1.0
    @2754 SF1720EF 1.0
    @2755 SF1730B  1.0
    @2756 SF1730BC 1.0
    @2757 SF1730BD 1.0
    @2758 SF1730E  3.0
    @2761 SF1740B  1.0
    @2762 SF1740BC 1.0
    @2763 SF1740BD 1.0
    @2764 SF1740E  3.0
    @2767 V2000A   2.0
    @2769 V2000B   2.0
    @2771 V2000C   2.0
    @2773 V2000D   2.0
    @2775 V2000E   2.0
    @2777 V2000F   2.0
    @2779 V2000G   2.0
    @2781 V2001A   2.0
    @2783 V2001B   2.0
    @2785 V2001C   2.0
    @2787 V2001D   2.0
    @2789 V2002A   2.0
    @2791 V2002B   2.0
    @2793 V2002C   2.0
    @2795 V2002D   2.0
    @2797 V2003A   2.0
    @2799 V2003B   2.0
    @2801 V2003C   2.0
    @2803 V2003D   2.0
    @2805 V2004A   2.0
    @2807 V2004B   2.0
    @2809 V2004C   2.0
    @2811 V2004D   2.0
    @2813 V2005A   2.0
    @2815 V2005B   2.0
    @2817 V2005C   2.0
    @2819 V2005D   2.0
    @2821 V2006A   2.0
    @2823 V2006B   2.0
    @2825 V2006C   2.0
    @2827 V2006D   2.0
    @2829 V2007A   2.0
    @2831 V2007B   2.0
    @2833 V2007C   2.0
    @2835 V2007D   2.0
    @2837 V2008A   2.0
    @2839 V2008B   2.0
    @2841 V2008C   2.0
    @2843 V2008D   2.0
    @2845 V2009A   2.0
    @2847 V2009B   2.0
    @2849 V2009C   2.0
    @2851 V2009D   2.0
    @2853 V2010A   2.0
    @2855 V2010B   2.0
    @2857 V2010C   2.0
    @2859 V2010D   2.0
    @2861 V2011A   2.0
    @2863 V2011B   2.0
    @2865 V2011C   2.0
    @2867 V2011D   2.0
    @2869 V2012A   2.0
    @2871 V2012B   2.0
    @2873 V2012C   2.0
    @2875 V2012D   2.0
    ;
    if V003     = 9 then V003 = .;
    if V077     = 999 then V077 = .;
    if V078     = 999 then V078 = .;
    if V079     = 999 then V079 = .;
    if V101A    = 9 then V101A = .;
    if V102D_01 = 9999 then V102D_01 = .;
    if V102D_02 = 9999 then V102D_02 = .;
    if V102D_03 = 9999 then V102D_03 = .;
    if V102D_04 = 9999 then V102D_04 = .;
    if V102D_05 = 9999 then V102D_05 = .;
    if V102D_06 = 9999 then V102D_06 = .;
    if V102D_07 = 9999 then V102D_07 = .;
    if V102D_08 = 9999 then V102D_08 = .;
    if V102D_09 = 9999 then V102D_09 = .;
    if V102D_10 = 9999 then V102D_10 = .;
    if V102D_11 = 9999 then V102D_11 = .;
    if V102D_12 = 9999 then V102D_12 = .;
    if V102D_13 = 9999 then V102D_13 = .;
    if V102D_14 = 9999 then V102D_14 = .;
    if V102D_15 = 9999 then V102D_15 = .;
    if V102D_16 = 9999 then V102D_16 = .;
    if V102D_17 = 9999 then V102D_17 = .;
    if V102D_18 = 9999 then V102D_18 = .;
    if V102D_19 = 9999 then V102D_19 = .;
    if V102E_01 = 9999 then V102E_01 = .;
    if V102E_02 = 9999 then V102E_02 = .;
    if V102E_03 = 9999 then V102E_03 = .;
    if V102E_04 = 9999 then V102E_04 = .;
    if V102E_05 = 9999 then V102E_05 = .;
    if V102E_06 = 9999 then V102E_06 = .;
    if V102E_07 = 9999 then V102E_07 = .;
    if V102E_08 = 9999 then V102E_08 = .;
    if V102E_09 = 9999 then V102E_09 = .;
    if V102E_10 = 9999 then V102E_10 = .;
    if V102E_11 = 9999 then V102E_11 = .;
    if V102E_12 = 9999 then V102E_12 = .;
    if V102E_13 = 9999 then V102E_13 = .;
    if V102E_14 = 9999 then V102E_14 = .;
    if V102E_15 = 9999 then V102E_15 = .;
    if V102E_16 = 9999 then V102E_16 = .;
    if V102E_17 = 9999 then V102E_17 = .;
    if V102E_18 = 9999 then V102E_18 = .;
    if V102E_19 = 9999 then V102E_19 = .;
    if V102F_01 = 9999 then V102F_01 = .;
    if V102F_02 = 9999 then V102F_02 = .;
    if V102F_03 = 9999 then V102F_03 = .;
    if V102F_04 = 9999 then V102F_04 = .;
    if V102F_05 = 9999 then V102F_05 = .;
    if V102F_06 = 9999 then V102F_06 = .;
    if V102F_07 = 9999 then V102F_07 = .;
    if V102F_08 = 9999 then V102F_08 = .;
    if V102F_09 = 9999 then V102F_09 = .;
    if V102F_10 = 9999 then V102F_10 = .;
    if V102F_11 = 9999 then V102F_11 = .;
    if V102F_12 = 9999 then V102F_12 = .;
    if V102F_13 = 9999 then V102F_13 = .;
    if V102F_14 = 9999 then V102F_14 = .;
    if V102F_15 = 9999 then V102F_15 = .;
    if V102F_16 = 9999 then V102F_16 = .;
    if V102F_17 = 9999 then V102F_17 = .;
    if V102F_18 = 9999 then V102F_18 = .;
    if V102F_19 = 9999 then V102F_19 = .;
    if V102G_01 = 9999 then V102G_01 = .;
    if V102G_02 = 9999 then V102G_02 = .;
    if V102G_03 = 9999 then V102G_03 = .;
    if V102G_04 = 9999 then V102G_04 = .;
    if V102G_05 = 9999 then V102G_05 = .;
    if V102G_06 = 9999 then V102G_06 = .;
    if V102G_07 = 9999 then V102G_07 = .;
    if V102G_08 = 9999 then V102G_08 = .;
    if V102G_09 = 9999 then V102G_09 = .;
    if V102G_10 = 9999 then V102G_10 = .;
    if V102G_11 = 9999 then V102G_11 = .;
    if V102G_12 = 9999 then V102G_12 = .;
    if V102G_13 = 9999 then V102G_13 = .;
    if V102G_14 = 9999 then V102G_14 = .;
    if V102G_15 = 9999 then V102G_15 = .;
    if V102G_16 = 9999 then V102G_16 = .;
    if V102G_17 = 9999 then V102G_17 = .;
    if V102G_18 = 9999 then V102G_18 = .;
    if V102G_19 = 9999 then V102G_19 = .;
    if V102H_01 = 9999 then V102H_01 = .;
    if V102H_02 = 9999 then V102H_02 = .;
    if V102H_03 = 9999 then V102H_03 = .;
    if V102H_04 = 9999 then V102H_04 = .;
    if V102H_05 = 9999 then V102H_05 = .;
    if V102H_06 = 9999 then V102H_06 = .;
    if V102H_07 = 9999 then V102H_07 = .;
    if V102H_08 = 9999 then V102H_08 = .;
    if V102H_09 = 9999 then V102H_09 = .;
    if V102H_10 = 9999 then V102H_10 = .;
    if V102H_11 = 9999 then V102H_11 = .;
    if V102H_12 = 9999 then V102H_12 = .;
    if V102H_13 = 9999 then V102H_13 = .;
    if V102H_14 = 9999 then V102H_14 = .;
    if V102H_15 = 9999 then V102H_15 = .;
    if V102H_16 = 9999 then V102H_16 = .;
    if V102H_17 = 9999 then V102H_17 = .;
    if V102H_18 = 9999 then V102H_18 = .;
    if V102H_19 = 9999 then V102H_19 = .;
    if V102I_01 = 9999 then V102I_01 = .;
    if V102I_02 = 9999 then V102I_02 = .;
    if V102I_03 = 9999 then V102I_03 = .;
    if V102I_04 = 9999 then V102I_04 = .;
    if V102I_05 = 9999 then V102I_05 = .;
    if V102I_06 = 9999 then V102I_06 = .;
    if V102I_07 = 9999 then V102I_07 = .;
    if V102I_08 = 9999 then V102I_08 = .;
    if V102I_09 = 9999 then V102I_09 = .;
    if V102I_10 = 9999 then V102I_10 = .;
    if V102I_11 = 9999 then V102I_11 = .;
    if V102I_12 = 9999 then V102I_12 = .;
    if V102I_13 = 9999 then V102I_13 = .;
    if V102I_14 = 9999 then V102I_14 = .;
    if V102I_15 = 9999 then V102I_15 = .;
    if V102I_16 = 9999 then V102I_16 = .;
    if V102I_17 = 9999 then V102I_17 = .;
    if V102I_18 = 9999 then V102I_18 = .;
    if V102I_19 = 9999 then V102I_19 = .;
    if V102J_01 = 9999 then V102J_01 = .;
    if V102J_02 = 9999 then V102J_02 = .;
    if V102J_03 = 9999 then V102J_03 = .;
    if V102J_04 = 9999 then V102J_04 = .;
    if V102J_05 = 9999 then V102J_05 = .;
    if V102J_06 = 9999 then V102J_06 = .;
    if V102J_07 = 9999 then V102J_07 = .;
    if V102J_08 = 9999 then V102J_08 = .;
    if V102J_09 = 9999 then V102J_09 = .;
    if V102J_10 = 9999 then V102J_10 = .;
    if V102J_11 = 9999 then V102J_11 = .;
    if V102J_12 = 9999 then V102J_12 = .;
    if V102J_13 = 9999 then V102J_13 = .;
    if V102J_14 = 9999 then V102J_14 = .;
    if V102J_15 = 9999 then V102J_15 = .;
    if V102J_16 = 9999 then V102J_16 = .;
    if V102J_17 = 9999 then V102J_17 = .;
    if V102J_18 = 9999 then V102J_18 = .;
    if V102J_19 = 9999 then V102J_19 = .;
    if V102K_01 = 9999 then V102K_01 = .;
    if V102K_02 = 9999 then V102K_02 = .;
    if V102K_03 = 9999 then V102K_03 = .;
    if V102K_04 = 9999 then V102K_04 = .;
    if V102K_05 = 9999 then V102K_05 = .;
    if V102K_06 = 9999 then V102K_06 = .;
    if V102K_07 = 9999 then V102K_07 = .;
    if V102K_08 = 9999 then V102K_08 = .;
    if V102K_09 = 9999 then V102K_09 = .;
    if V102K_10 = 9999 then V102K_10 = .;
    if V102K_11 = 9999 then V102K_11 = .;
    if V102K_12 = 9999 then V102K_12 = .;
    if V102K_13 = 9999 then V102K_13 = .;
    if V102K_14 = 9999 then V102K_14 = .;
    if V102K_15 = 9999 then V102K_15 = .;
    if V102K_16 = 9999 then V102K_16 = .;
    if V102K_17 = 9999 then V102K_17 = .;
    if V102K_18 = 9999 then V102K_18 = .;
    if V102K_19 = 9999 then V102K_19 = .;
    if V102DT   = 9999 then V102DT = .;
    if V102ET   = 9999 then V102ET = .;
    if V102T    = 9999 then V102T = .;
    if V102FT   = 9999 then V102FT = .;
    if V102GT   = 9999 then V102GT = .;
    if V102HT   = 9999 then V102HT = .;
    if V102IT   = 9999 then V102IT = .;
    if V102JT   = 9999 then V102JT = .;
    if V102KT   = 9999 then V102KT = .;
    if V105     = 9 then V105 = .;
    if V106     = 9 then V106 = .;
    if V106A    = 9 then V106A = .;
    if V106B    = 9 then V106B = .;
    if V106C    = 9 then V106C = .;
    if V106D    = 9 then V106D = .;
    if V106E    = 9 then V106E = .;
    if V106F    = 9 then V106F = .;
    if V106G    = 9 then V106G = .;
    if V106H    = 9 then V106H = .;
    if V106X    = 9 then V106X = .;
    if V106Y    = 9 then V106Y = .;
    if V106AA   = 9 then V106AA = .;
    if V106BB   = 9 then V106BB = .;
    if V107     = 9 then V107 = .;
    if V108     = 9 then V108 = .;
    if V109A    = 9 then V109A = .;
    if V109B    = 9 then V109B = .;
    if V109C    = 9 then V109C = .;
    if V109D    = 9 then V109D = .;
    if V109E    = 9 then V109E = .;
    if V109F    = 9 then V109F = .;
    if V109G    = 9 then V109G = .;
    if V109H    = 9 then V109H = .;
    if V109X    = 9 then V109X = .;
    if V109Y    = 9 then V109Y = .;
    if V109Z    = 9 then V109Z = .;
    if V110A    = 9 then V110A = .;
    if V110     = 9 then V110 = .;
    if V112     = 9 then V112 = .;
    if V115     = 9 then V115 = .;
    if V116G    = 9 then V116G = .;
    if V116H    = 9 then V116H = .;
    if V116I    = 9 then V116I = .;
    if V120A    = 9 then V120A = .;
    if V120     = 9 then V120 = .;
    if V121A    = 9 then V121A = .;
    if V121B    = 9 then V121B = .;
    if V121C    = 9 then V121C = .;
    if V121D    = 9 then V121D = .;
    if V123     = 99 then V123 = .;
    if V124     = 9 then V124 = .;
    if V126     = 9 then V126 = .;
    if V127A    = 9 then V127A = .;
    if V127B    = 9 then V127B = .;
    if V127     = 9 then V127 = .;
    if V128     = 9 then V128 = .;
    if V128A    = 9 then V128A = .;
    if V129     = 9 then V129 = .;
    if V130     = 9 then V130 = .;
    if SF464    = 9 then SF464 = .;
    if V131     = 9 then V131 = .;
    if V132     = 9 then V132 = .;
    if V133     = 9 then V133 = .;
    if V134     = 999999 then V134 = .;
    if V135     = 999999 then V135 = .;
    if SF346B   = 9 then SF346B = .;
    if SF420D   = 9 then SF420D = .;
    if SF442AB  = 9 then SF442AB = .;
    if SF442CD  = 9 then SF442CD = .;
    if V142     = 9 then V142 = .;
    if V143     = 9999 then V143 = .;
    if V144     = 9 then V144 = .;
    if V144A    = 9 then V144A = .;
    if V144B    = 9 then V144B = .;
    if V144E    = 9 then V144E = .;
    if V144F    = 9 then V144F = .;
    if V144G    = 9 then V144G = .;
    if V144J    = 9 then V144J = .;
    if V144K    = 9 then V144K = .;
    if V144L    = 9 then V144L = .;
    if V144M    = 9 then V144M = .;
    if V144N    = 9 then V144N = .;
    if V144O    = 9 then V144O = .;
    if V144P    = 9 then V144P = .;
    if V144Q    = 9 then V144Q = .;
    if V144R    = 9 then V144R = .;
    if V144S    = 9 then V144S = .;
    if V144T    = 9 then V144T = .;
    if V144U    = 9 then V144U = .;
    if V144V    = 9 then V144V = .;
    if V144W    = 9 then V144W = .;
    if V144X    = 9 then V144X = .;
    if V145     = 9 then V145 = .;
    if V145A    = 9 then V145A = .;
    if V145B    = 9 then V145B = .;
    if V145C    = 9 then V145C = .;
    if V145D    = 9 then V145D = .;
    if V145X    = 9 then V145X = .;
    if V146A    = 9 then V146A = .;
    if V146B    = 9 then V146B = .;
    if V146C    = 9 then V146C = .;
    if V146D    = 9 then V146D = .;
    if V146E    = 9 then V146E = .;
    if V146F    = 9 then V146F = .;
    if V146G    = 9 then V146G = .;
    if V146H    = 9 then V146H = .;
    if V146I    = 9 then V146I = .;
    if V146X    = 9 then V146X = .;
    if V146Y    = 9 then V146Y = .;
    if V146Z    = 9 then V146Z = .;
    if V150     = 9 then V150 = .;
    if V152     = 9 then V152 = .;
    if V153     = 9 then V153 = .;
    if V153A    = 99 then V153A = .;
    if V165A    = 9 then V165A = .;
    if V165B    = 9 then V165B = .;
    if V165C    = 9 then V165C = .;
    if V165D    = 9 then V165D = .;
    if V166A    = 9 then V166A = .;
    if V166B    = 9 then V166B = .;
    if V166C    = 9 then V166C = .;
    if V166D    = 9 then V166D = .;
    if V166E    = 9 then V166E = .;
    if V166F    = 9 then V166F = .;
    if V166G    = 9 then V166G = .;
    if V166H    = 9 then V166H = .;
    if V166I    = 9 then V166I = .;
    if V166J    = 9 then V166J = .;
    if V166K    = 9 then V166K = .;
    if V166L    = 9 then V166L = .;
    if V166M    = 9 then V166M = .;
    if V166N    = 9 then V166N = .;
    if V166O    = 9 then V166O = .;
    if V166P    = 9 then V166P = .;
    if V166Q    = 9 then V166Q = .;
    if V166R    = 9 then V166R = .;
    if SV165A   = 9 then SV165A = .;
    if V167     = 9 then V167 = .;
    if V168A1   = 9 then V168A1 = .;
    if V168C    = 9 then V168C = .;
    if V168F    = 9 then V168F = .;
    if V168G    = 9 then V168G = .;
    if V168J    = 9 then V168J = .;
    if V168L    = 9 then V168L = .;
    if V168O    = 9 then V168O = .;
    if V168P    = 9 then V168P = .;
    if V168Q    = 9 then V168Q = .;
    if V168R    = 9 then V168R = .;
    if V168S    = 9 then V168S = .;
    if V168T    = 9 then V168T = .;
    if V168U    = 9 then V168U = .;
    if V168V    = 9 then V168V = .;
    if V168W    = 9 then V168W = .;
    if SF370DA1 = 9 then SF370DA1 = .;
    if SF370DB1 = 9 then SF370DB1 = .;
    if SF370DC1 = 9 then SF370DC1 = .;
    if SF370DA2 = 9 then SF370DA2 = .;
    if SF370DB2 = 9 then SF370DB2 = .;
    if SF370DC2 = 9 then SF370DC2 = .;
    if SF465BC  = 9 then SF465BC = .;
    if SF465DE  = 9 then SF465DE = .;
    if SF465FG  = 9 then SF465FG = .;
    if SF465H   = 9 then SF465H = .;
    if SF472EF  = 9 then SF472EF = .;
    if SF480A   = 9 then SF480A = .;
    if SF480BC  = 9 then SF480BC = .;
    if SF480DE  = 9 then SF480DE = .;
    if SF480F   = 9 then SF480F = .;
    if SF480HI  = 9 then SF480HI = .;
    if SF480JK  = 9 then SF480JK = .;
    if SF700CD  = 9 then SF700CD = .;
    if SF720AB  = 9 then SF720AB = .;
    if SF720C   = 9 then SF720C = .;
    if V173A    = 9 then V173A = .;
    if V173     = 9 then V173 = .;
    if V175B_1  = 9 then V175B_1 = .;
    if V175B_2  = 9 then V175B_2 = .;
    if V175B_3  = 9 then V175B_3 = .;
    if V175B_4  = 9 then V175B_4 = .;
    if V175B_5  = 9 then V175B_5 = .;
    if V175B_6  = 9 then V175B_6 = .;
    if V175B_7  = 9 then V175B_7 = .;
    if V175B_8  = 9 then V175B_8 = .;
    if V175C_1  = 999 then V175C_1 = .;
    if V175C_2  = 999 then V175C_2 = .;
    if V175C_3  = 999 then V175C_3 = .;
    if V175C_4  = 999 then V175C_4 = .;
    if V175C_5  = 999 then V175C_5 = .;
    if V175C_6  = 999 then V175C_6 = .;
    if V175C_7  = 999 then V175C_7 = .;
    if V175C_8  = 999 then V175C_8 = .;
    if V175D_1  = 999 then V175D_1 = .;
    if V175D_2  = 999 then V175D_2 = .;
    if V175D_3  = 999 then V175D_3 = .;
    if V175D_4  = 999 then V175D_4 = .;
    if V175D_5  = 999 then V175D_5 = .;
    if V175D_6  = 999 then V175D_6 = .;
    if V175D_7  = 999 then V175D_7 = .;
    if V175D_8  = 999 then V175D_8 = .;
    if V175E_1  = 9 then V175E_1 = .;
    if V175E_2  = 9 then V175E_2 = .;
    if V175E_3  = 9 then V175E_3 = .;
    if V175E_4  = 9 then V175E_4 = .;
    if V175E_5  = 9 then V175E_5 = .;
    if V175E_6  = 9 then V175E_6 = .;
    if V175E_7  = 9 then V175E_7 = .;
    if V175E_8  = 9 then V175E_8 = .;
    if V175F_1  = 999 then V175F_1 = .;
    if V175F_2  = 999 then V175F_2 = .;
    if V175F_3  = 999 then V175F_3 = .;
    if V175F_4  = 999 then V175F_4 = .;
    if V175F_5  = 999 then V175F_5 = .;
    if V175F_6  = 999 then V175F_6 = .;
    if V175F_7  = 999 then V175F_7 = .;
    if V175F_8  = 999 then V175F_8 = .;
    if V175G_1  = 999 then V175G_1 = .;
    if V175G_2  = 999 then V175G_2 = .;
    if V175G_3  = 999 then V175G_3 = .;
    if V175G_4  = 999 then V175G_4 = .;
    if V175G_5  = 999 then V175G_5 = .;
    if V175G_6  = 999 then V175G_6 = .;
    if V175G_7  = 999 then V175G_7 = .;
    if V175G_8  = 999 then V175G_8 = .;
    if V175H_1  = 99 then V175H_1 = .;
    if V175H_2  = 99 then V175H_2 = .;
    if V175H_3  = 99 then V175H_3 = .;
    if V175H_4  = 99 then V175H_4 = .;
    if V175H_5  = 99 then V175H_5 = .;
    if V175H_6  = 99 then V175H_6 = .;
    if V175H_7  = 99 then V175H_7 = .;
    if V175H_8  = 99 then V175H_8 = .;
    if V176A    = 9 then V176A = .;
    if V176B    = 9 then V176B = .;
    if V176C    = 9 then V176C = .;
    if V177A    = 9 then V177A = .;
    if V177B    = 9 then V177B = .;
    if V177C    = 9 then V177C = .;
    if V177D    = 9 then V177D = .;
    if V177E    = 9 then V177E = .;
    if V177F    = 9 then V177F = .;
    if V177G    = 9 then V177G = .;
    if V178B    = 9 then V178B = .;
    if V184A    = 99 then V184A = .;
    if V185     = 9 then V185 = .;
    if V186A    = 99 then V186A = .;
    if V187     = 9 then V187 = .;
    if V189     = 9 then V189 = .;
    if V190     = 9 then V190 = .;
    if V195C    = 9 then V195C = .;
    if V200B    = 99 then V200B = .;
    if V200C    = 99 then V200C = .;
    if V200D    = 99 then V200D = .;
    if V200E    = 99 then V200E = .;
    if V200G    = 99 then V200G = .;
    if V200H    = 99 then V200H = .;
    if V200I    = 99 then V200I = .;
    if V200J    = 99 then V200J = .;
    if V201B    = 99 then V201B = .;
    if V201C    = 99 then V201C = .;
    if V201D    = 99 then V201D = .;
    if V201E    = 99 then V201E = .;
    if V201G    = 99 then V201G = .;
    if V201H    = 99 then V201H = .;
    if V201I    = 99 then V201I = .;
    if V201J    = 99 then V201J = .;
    if V230     = 9 then V230 = .;
    if V231A1   = 9 then V231A1 = .;
    if V231C    = 9 then V231C = .;
    if V231F    = 9 then V231F = .;
    if V231G    = 9 then V231G = .;
    if V231J    = 9 then V231J = .;
    if V231L    = 9 then V231L = .;
    if V231O    = 9 then V231O = .;
    if V231P    = 9 then V231P = .;
    if V231Q    = 9 then V231Q = .;
    if V231R    = 9 then V231R = .;
    if V231S    = 9 then V231S = .;
    if V231T    = 9 then V231T = .;
    if V231U    = 9 then V231U = .;
    if V231V    = 9 then V231V = .;
    if V231W    = 9 then V231W = .;
    if V231XA   = 9 then V231XA = .;
    if V231XB   = 9 then V231XB = .;
    if V231XC   = 9 then V231XC = .;
    if V231XD   = 9 then V231XD = .;
    if V231XE   = 9 then V231XE = .;
    if V231XF   = 9 then V231XF = .;
    if V231XG   = 9 then V231XG = .;
    if V231XH   = 9 then V231XH = .;
    if V242     = 9 then V242 = .;
    if V246     = 9 then V246 = .;
    if V247A    = 9 then V247A = .;
    if V247B    = 9 then V247B = .;
    if V247C    = 9 then V247C = .;
    if V247D    = 9 then V247D = .;
    if V247E    = 9 then V247E = .;
    if V247F    = 9 then V247F = .;
    if V247G    = 9 then V247G = .;
    if V263     = 9 then V263 = .;
    if V264     = 9 then V264 = .;
    if V264A1   = 9 then V264A1 = .;
    if V264C    = 9 then V264C = .;
    if V264F    = 9 then V264F = .;
    if V264G    = 9 then V264G = .;
    if V264J    = 9 then V264J = .;
    if V264L    = 9 then V264L = .;
    if V264O    = 9 then V264O = .;
    if V264P    = 9 then V264P = .;
    if V264Q    = 9 then V264Q = .;
    if V264R    = 9 then V264R = .;
    if V264S    = 9 then V264S = .;
    if V264T    = 9 then V264T = .;
    if V264U    = 9 then V264U = .;
    if V264V    = 9 then V264V = .;
    if V264W    = 9 then V264W = .;
    if V264XE   = 9 then V264XE = .;
    if V264XF   = 9 then V264XF = .;
    if V264XG   = 9 then V264XG = .;
    if V264XH   = 9 then V264XH = .;
    if V264XI   = 9 then V264XI = .;
    if V264XJ   = 9 then V264XJ = .;
    if V265B    = 9 then V265B = .;
    if V265C    = 9 then V265C = .;
    if V265D    = 9 then V265D = .;
    if V265E    = 9 then V265E = .;
    if V265F    = 9 then V265F = .;
    if V265G    = 9 then V265G = .;
    if V265H    = 9 then V265H = .;
    if V266A    = 9 then V266A = .;
    if V266B    = 9 then V266B = .;
    if V266D    = 9 then V266D = .;
    if V266E    = 9 then V266E = .;
    if V266F    = 9 then V266F = .;
    if V266G    = 9 then V266G = .;
    if V266X    = 9 then V266X = .;
    if V271     = 9 then V271 = .;
    if V271A    = 9 then V271A = .;
    if V272A    = 9 then V272A = .;
    if V272B    = 9 then V272B = .;
    if V272C    = 9 then V272C = .;
    if V272D    = 9 then V272D = .;
    if V272F    = 9 then V272F = .;
    if V273A    = 9 then V273A = .;
    if V273B    = 9 then V273B = .;
    if V280A    = 9 then V280A = .;
    if V280X    = 9 then V280X = .;
    if LV200    = 9 then LV200 = .;
    if LV203A   = 9 then LV203A = .;
    if LV203B   = 9 then LV203B = .;
    if LV203C   = 9 then LV203C = .;
    if LV203D   = 9 then LV203D = .;
    if LV203E   = 9 then LV203E = .;
    if LV203F   = 9 then LV203F = .;
    if LV203G   = 9 then LV203G = .;
    if LV203H   = 9 then LV203H = .;
    if LV206B   = 9 then LV206B = .;
    if LV221    = 9 then LV221 = .;
    if LV222A   = 9 then LV222A = .;
    if LV224    = 9 then LV224 = .;
    if LV225    = 9 then LV225 = .;
    if LV226    = 9 then LV226 = .;
    if SF1015BC = 9 then SF1015BC = .;
    if SF1015D  = 9 then SF1015D = .;
    if SF1015E  = 9 then SF1015E = .;
    if SF1015FG = 9 then SF1015FG = .;
    if V301A    = 99 then V301A = .;
    if V303A    = 9 then V303A = .;
    if V303B    = 9 then V303B = .;
    if V303D    = 9 then V303D = .;
    if V303E    = 9 then V303E = .;
    if V303G    = 9 then V303G = .;
    if V303H    = 9 then V303H = .;
    if V303I    = 9 then V303I = .;
    if V303J    = 9 then V303J = .;
    if V303K    = 9 then V303K = .;
    if V303L    = 9 then V303L = .;
    if V303O    = 9 then V303O = .;
    if V303Q    = 9 then V303Q = .;
    if V303R    = 9 then V303R = .;
    if V303S    = 9 then V303S = .;
    if V303X    = 9 then V303X = .;
    if V306     = 9 then V306 = .;
    if V307A    = 9 then V307A = .;
    if V307B    = 9 then V307B = .;
    if V307C    = 9 then V307C = .;
    if V308     = 9 then V308 = .;
    if V308A    = 9 then V308A = .;
    if V324     = 9 then V324 = .;
    if V325A    = 9 then V325A = .;
    if V325D    = 9 then V325D = .;
    if V325F    = 9 then V325F = .;
    if V325G    = 9 then V325G = .;
    if V325H    = 9 then V325H = .;
    if V325I    = 9 then V325I = .;
    if V327A    = 9 then V327A = .;
    if V327X    = 9 then V327X = .;
    if V330     = 9 then V330 = .;
    if V331A1   = 9 then V331A1 = .;
    if V331C    = 9 then V331C = .;
    if V331F    = 9 then V331F = .;
    if V331G    = 9 then V331G = .;
    if V331J    = 9 then V331J = .;
    if V331L    = 9 then V331L = .;
    if V331O    = 9 then V331O = .;
    if V331P    = 9 then V331P = .;
    if V331Q    = 9 then V331Q = .;
    if V331R    = 9 then V331R = .;
    if V331S    = 9 then V331S = .;
    if V331T    = 9 then V331T = .;
    if V331U    = 9 then V331U = .;
    if V331V    = 9 then V331V = .;
    if V331W    = 9 then V331W = .;
    if V333B    = 9 then V333B = .;
    if V333C    = 9 then V333C = .;
    if V333D    = 9 then V333D = .;
    if V333E    = 9 then V333E = .;
    if V333F    = 9 then V333F = .;
    if V333G    = 9 then V333G = .;
    if V334A    = 9 then V334A = .;
    if V334B    = 9 then V334B = .;
    if V334C    = 9 then V334C = .;
    if V334D    = 9 then V334D = .;
    if V335A    = 9 then V335A = .;
    if V335B    = 9 then V335B = .;
    if V335C    = 9 then V335C = .;
    if V335E    = 9 then V335E = .;
    if V335F    = 9 then V335F = .;
    if V336A    = 9 then V336A = .;
    if V336B    = 9 then V336B = .;
    if V336C    = 9 then V336C = .;
    if V336D    = 9 then V336D = .;
    if V336F    = 9 then V336F = .;
    if V336G    = 9 then V336G = .;
    if V342     = 9 then V342 = .;
    if V343A    = 9 then V343A = .;
    if V343B    = 9 then V343B = .;
    if V343C    = 9 then V343C = .;
    if V343D    = 9 then V343D = .;
    if V343E    = 9 then V343E = .;
    if V343F    = 9 then V343F = .;
    if V344     = 9 then V344 = .;
    if LV302A   = 9 then LV302A = .;
    if LV302B   = 9 then LV302B = .;
    if LV303A   = 9 then LV303A = .;
    if LV303B   = 9 then LV303B = .;
    if LV303D   = 9 then LV303D = .;
    if LV303E   = 9 then LV303E = .;
    if LV303G   = 9 then LV303G = .;
    if LV303H   = 9 then LV303H = .;
    if LV303I   = 9 then LV303I = .;
    if LV303J   = 9 then LV303J = .;
    if LV303K   = 9 then LV303K = .;
    if LV303L   = 9 then LV303L = .;
    if LV303O   = 9 then LV303O = .;
    if SLV303A  = 9 then SLV303A = .;
    if SLV303B  = 9 then SLV303B = .;
    if SLV303E  = 9 then SLV303E = .;
    if SLV303G  = 9 then SLV303G = .;
    if SLV303I  = 9 then SLV303I = .;
    if SLV303K  = 9 then SLV303K = .;
    if SLV303L  = 9 then SLV303L = .;
    if LV306A   = 9 then LV306A = .;
    if LV306B   = 9 then LV306B = .;
    if LV320A   = 9 then LV320A = .;
    if LV320C   = 9 then LV320C = .;
    if LV320D   = 9 then LV320D = .;
    if LV320E   = 9 then LV320E = .;
    if LV320F   = 9 then LV320F = .;
    if SF1308_01 = 9 then SF1308_01 = .;
    if SF1308_02 = 9 then SF1308_02 = .;
    if SF1308_03 = 9 then SF1308_03 = .;
    if V401A    = 99 then V401A = .;
    if V406     = 9 then V406 = .;
    if V407A    = 9 then V407A = .;
    if V407B    = 9 then V407B = .;
    if V407C    = 9 then V407C = .;
    if V407D    = 9 then V407D = .;
    if V407E    = 9 then V407E = .;
    if V407F    = 9 then V407F = .;
    if V407G    = 9 then V407G = .;
    if V407H    = 9 then V407H = .;
    if V409B    = 9 then V409B = .;
    if V409C    = 9 then V409C = .;
    if V409E    = 9 then V409E = .;
    if V409F    = 9 then V409F = .;
    if V409G    = 9 then V409G = .;
    if V409H    = 9 then V409H = .;
    if V409I    = 9 then V409I = .;
    if V409J    = 9 then V409J = .;
    if V410     = 9 then V410 = .;
    if V410A    = 9 then V410A = .;
    if V411A    = 9 then V411A = .;
    if V411B    = 99 then V411B = .;
    if V424     = 9 then V424 = .;
    if V430     = 9 then V430 = .;
    if V431A1   = 9 then V431A1 = .;
    if V431C    = 9 then V431C = .;
    if V431F    = 9 then V431F = .;
    if V431G    = 9 then V431G = .;
    if V431J    = 9 then V431J = .;
    if V431L    = 9 then V431L = .;
    if V431O    = 9 then V431O = .;
    if V431P    = 9 then V431P = .;
    if V431Q    = 9 then V431Q = .;
    if V431R    = 9 then V431R = .;
    if V431S    = 9 then V431S = .;
    if V431T    = 9 then V431T = .;
    if V431U    = 9 then V431U = .;
    if V431V    = 9 then V431V = .;
    if V431W    = 9 then V431W = .;
    if V432A    = 9 then V432A = .;
    if V432B    = 9 then V432B = .;
    if V432C    = 9 then V432C = .;
    if V432D    = 9 then V432D = .;
    if V432E    = 9 then V432E = .;
    if V432F    = 9 then V432F = .;
    if V432G    = 9 then V432G = .;
    if V433B    = 9 then V433B = .;
    if V433C    = 9 then V433C = .;
    if V433D    = 9 then V433D = .;
    if V433F    = 9 then V433F = .;
    if V433G    = 9 then V433G = .;
    if V433H    = 9 then V433H = .;
    if V433I    = 9 then V433I = .;
    if V433J    = 9 then V433J = .;
    if V433K    = 9 then V433K = .;
    if V433L    = 9 then V433L = .;
    if V434A    = 9 then V434A = .;
    if V434B    = 9 then V434B = .;
    if V434X    = 9 then V434X = .;
    if V435A    = 9 then V435A = .;
    if V435B    = 9 then V435B = .;
    if V460     = 9 then V460 = .;
    if V461A    = 9 then V461A = .;
    if V461B    = 9 then V461B = .;
    if V461C    = 9 then V461C = .;
    if V461D    = 9 then V461D = .;
    if V461E    = 9 then V461E = .;
    if V461F    = 9 then V461F = .;
    if V462A    = 9 then V462A = .;
    if V462B    = 9 then V462B = .;
    if V462C    = 9 then V462C = .;
    if V462D    = 9 then V462D = .;
    if V462E    = 9 then V462E = .;
    if V462F    = 9 then V462F = .;
    if V462G    = 9 then V462G = .;
    if V462H    = 9 then V462H = .;
    if SF1406A1 = 9 then SF1406A1 = .;
    if SF1406A2 = 9 then SF1406A2 = .;
    if SF1406A3 = 9 then SF1406A3 = .;
    if SF1406A4 = 9 then SF1406A4 = .;
    if SF1406A5 = 9 then SF1406A5 = .;
    if SF1408A  = 9 then SF1408A = .;
    if SF1418_1 = 9 then SF1418_1 = .;
    if SF1418_2 = 9 then SF1418_2 = .;
    if SF1418_3 = 9 then SF1418_3 = .;
    if V501     = 999 then V501 = .;
    if V501A    = 999 then V501A = .;
    if V502     = 9 then V502 = .;
    if V503     = 9 then V503 = .;
    if V506C    = 9 then V506C = .;
    if V507A    = 9 then V507A = .;
    if V507B    = 9 then V507B = .;
    if V507C    = 9 then V507C = .;
    if V507D    = 9 then V507D = .;
    if V507E    = 9 then V507E = .;
    if V507F    = 9 then V507F = .;
    if V507G    = 9 then V507G = .;
    if V507H    = 9 then V507H = .;
    if V507I    = 9 then V507I = .;
    if V507J    = 9 then V507J = .;
    if V507K    = 9 then V507K = .;
    if V507L    = 9 then V507L = .;
    if V507M    = 9 then V507M = .;
    if V507N    = 9 then V507N = .;
    if V507O    = 9 then V507O = .;
    if V507P    = 9 then V507P = .;
    if V530     = 9 then V530 = .;
    if V531A1   = 9 then V531A1 = .;
    if V531C    = 9 then V531C = .;
    if V531F    = 9 then V531F = .;
    if V531G    = 9 then V531G = .;
    if V531J    = 9 then V531J = .;
    if V531L    = 9 then V531L = .;
    if V531O    = 9 then V531O = .;
    if V531P    = 9 then V531P = .;
    if V531Q    = 9 then V531Q = .;
    if V531R    = 9 then V531R = .;
    if V531S    = 9 then V531S = .;
    if V531T    = 9 then V531T = .;
    if V531U    = 9 then V531U = .;
    if V531V    = 9 then V531V = .;
    if V531W    = 9 then V531W = .;
    if V531XD   = 9 then V531XD = .;
    if V531XE   = 9 then V531XE = .;
    if V531XF   = 9 then V531XF = .;
    if V531XG   = 9 then V531XG = .;
    if V531XH   = 9 then V531XH = .;
    if V531XI   = 9 then V531XI = .;
    if V531XJ   = 9 then V531XJ = .;
    if V531XK   = 9 then V531XK = .;
    if V531XL   = 9 then V531XL = .;
    if V531XM   = 9 then V531XM = .;
    if V531XN   = 9 then V531XN = .;
    if V531XO   = 9 then V531XO = .;
    if V531XP   = 9 then V531XP = .;
    if V531XQ   = 9 then V531XQ = .;
    if V533B    = 9 then V533B = .;
    if V533C    = 9 then V533C = .;
    if V533D    = 9 then V533D = .;
    if V533E    = 9 then V533E = .;
    if V533F    = 9 then V533F = .;
    if V533G    = 9 then V533G = .;
    if V533H    = 9 then V533H = .;
    if V533I    = 9 then V533I = .;
    if V533J    = 9 then V533J = .;
    if V534A    = 9 then V534A = .;
    if V534C    = 9 then V534C = .;
    if V534D    = 9 then V534D = .;
    if V534E    = 9 then V534E = .;
    if V534G    = 9 then V534G = .;
    if V534I    = 9 then V534I = .;
    if V534M    = 9 then V534M = .;
    if V534N    = 9 then V534N = .;
    if V534O    = 9 then V534O = .;
    if V534P    = 9 then V534P = .;
    if V534Q    = 9 then V534Q = .;
    if V534R    = 9 then V534R = .;
    if V536A    = 9 then V536A = .;
    if V536B    = 9 then V536B = .;
    if V536C    = 9 then V536C = .;
    if V536D    = 9 then V536D = .;
    if V536E    = 9 then V536E = .;
    if V536F    = 9 then V536F = .;
    if V537A    = 9 then V537A = .;
    if V537B    = 9 then V537B = .;
    if V537C    = 9 then V537C = .;
    if V537D    = 9 then V537D = .;
    if V537E    = 9 then V537E = .;
    if V545     = 9 then V545 = .;
    if V546A    = 99 then V546A = .;
    if V546B    = 99 then V546B = .;
    if V554A    = 9 then V554A = .;
    if V554B    = 9 then V554B = .;
    if V554C    = 9 then V554C = .;
    if V554D    = 9 then V554D = .;
    if V554E    = 9 then V554E = .;
    if V554F    = 9 then V554F = .;
    if V554G    = 9 then V554G = .;
    if V554H    = 9 then V554H = .;
    if V554I    = 9 then V554I = .;
    if V554J    = 9 then V554J = .;
    if V554K    = 9 then V554K = .;
    if V554L    = 9 then V554L = .;
    if V557C    = 9 then V557C = .;
    if V557E    = 9 then V557E = .;
    if V557F    = 9 then V557F = .;
    if V557G    = 9 then V557G = .;
    if V557H    = 9 then V557H = .;
    if V557I    = 9 then V557I = .;
    if V557J    = 9 then V557J = .;
    if V557K    = 9 then V557K = .;
    if V557L    = 9 then V557L = .;
    if V557M    = 9 then V557M = .;
    if V558     = 9 then V558 = .;
    if V559     = 9 then V559 = .;
    if V567     = 9 then V567 = .;
    if V568     = 9 then V568 = .;
    if V569     = 9 then V569 = .;
    if V570     = 9 then V570 = .;
    if SF1605   = 9 then SF1605 = .;
    if SF1607   = 9 then SF1607 = .;
    if SF1617A  = 9 then SF1617A = .;
    if SF16281  = 9 then SF16281 = .;
    if SF16282  = 9 then SF16282 = .;
    if SF16283  = 9 then SF16283 = .;
    if SF16284  = 9 then SF16284 = .;
    if SF16285  = 9 then SF16285 = .;
    if V600A    = 9 then V600A = .;
    if V601A    = 99 then V601A = .;
    if V624     = 9 then V624 = .;
    if V625A    = 9 then V625A = .;
    if V625B    = 9 then V625B = .;
    if V625C    = 9 then V625C = .;
    if V625D    = 9 then V625D = .;
    if V625E    = 9 then V625E = .;
    if V625F    = 9 then V625F = .;
    if V625G    = 9 then V625G = .;
    if V626A    = 9 then V626A = .;
    if V626B    = 9 then V626B = .;
    if V626C    = 9 then V626C = .;
    if V626D    = 9 then V626D = .;
    if V626E    = 9 then V626E = .;
    if V626F    = 9 then V626F = .;
    if V626G    = 9 then V626G = .;
    if V627A    = 9 then V627A = .;
    if V627X    = 9 then V627X = .;
    if V628     = 9 then V628 = .;
    if V630     = 9 then V630 = .;
    if V631A1   = 9 then V631A1 = .;
    if V631C    = 9 then V631C = .;
    if V631F    = 9 then V631F = .;
    if V631G    = 9 then V631G = .;
    if V631J    = 9 then V631J = .;
    if V631L    = 9 then V631L = .;
    if V631O    = 9 then V631O = .;
    if V631P    = 9 then V631P = .;
    if V631Q    = 9 then V631Q = .;
    if V631R    = 9 then V631R = .;
    if V631S    = 9 then V631S = .;
    if V631T    = 9 then V631T = .;
    if V631U    = 9 then V631U = .;
    if V631V    = 9 then V631V = .;
    if V631W    = 9 then V631W = .;
    if V640A    = 9 then V640A = .;
    if V640B    = 9 then V640B = .;
    if V640C    = 9 then V640C = .;
    if V640D    = 9 then V640D = .;
    if V640E    = 9 then V640E = .;
    if V640F    = 9 then V640F = .;
    if V642     = 9 then V642 = .;
    if V642A    = 9 then V642A = .;
    if V650     = 9 then V650 = .;
    if SF18091  = 9 then SF18091 = .;
    if SF18092  = 9 then SF18092 = .;
    if SF18093  = 9 then SF18093 = .;
    if SF18094  = 9 then SF18094 = .;
    if SF18095  = 9 then SF18095 = .;
    if V805A    = 9 then V805A = .;
    if V805B    = 9 then V805B = .;
    if V805X    = 9 then V805X = .;
    if V810A    = 9 then V810A = .;
    if V810B    = 9 then V810B = .;
    if V810C    = 9 then V810C = .;
    if V810X    = 9 then V810X = .;
    if V810Y    = 9 then V810Y = .;
    if V813     = 9 then V813 = .;
    if V815     = 99 then V815 = .;
    if V820A    = 9 then V820A = .;
    if V820B    = 9 then V820B = .;
    if V820C    = 9 then V820C = .;
    if V820D    = 9 then V820D = .;
    if V820E    = 9 then V820E = .;
    if V820X    = 9 then V820X = .;
    if V820Y    = 9 then V820Y = .;
    if V821     = 9 then V821 = .;
    if V821E    = 9 then V821E = .;
    if V834A    = 9 then V834A = .;
    if V834B    = 9 then V834B = .;
    if V834C    = 9 then V834C = .;
    if V834D    = 9 then V834D = .;
    if V834E    = 9 then V834E = .;
    if V834Y    = 9 then V834Y = .;
    if V835A    = 9 then V835A = .;
    if V835B    = 9 then V835B = .;
    if V835C    = 9 then V835C = .;
    if V835D    = 9 then V835D = .;
    if V835Y    = 9 then V835Y = .;
    if V836     = 9 then V836 = .;
    if V837     = 9 then V837 = .;
    if V838A    = 9 then V838A = .;
    if V838B    = 9 then V838B = .;
    if V838C    = 9 then V838C = .;
    if V838D    = 9 then V838D = .;
    if V838Y    = 9 then V838Y = .;
    if V839     = 9 then V839 = .;
    if V840A    = 9 then V840A = .;
    if V840A1   = 9 then V840A1 = .;
    if V840A2   = 9 then V840A2 = .;
    if V840A3   = 9 then V840A3 = .;
    if V840D    = 9 then V840D = .;
    if V840E    = 9 then V840E = .;
    if V840E1   = 9 then V840E1 = .;
    if V840E2   = 9 then V840E2 = .;
    if V840F    = 9 then V840F = .;
    if V840F1   = 9 then V840F1 = .;
    if V840F2   = 9 then V840F2 = .;
    if V840F3   = 9 then V840F3 = .;
    if V840G    = 9 then V840G = .;
    if V840G1   = 9 then V840G1 = .;
    if V840I    = 9 then V840I = .;
    if V840J    = 9 then V840J = .;
    if V840K    = 9 then V840K = .;
    if V840L    = 9 then V840L = .;
    if V840M    = 9 then V840M = .;
    if V840N    = 9 then V840N = .;
    if V840O    = 9 then V840O = .;
    if V840P    = 9 then V840P = .;
    if V840Y    = 9 then V840Y = .;
    if V841A1   = 9 then V841A1 = .;
    if V841A2   = 9 then V841A2 = .;
    if V841B    = 9 then V841B = .;
    if V841C    = 9 then V841C = .;
    if V841D    = 9 then V841D = .;
    if V841E    = 9 then V841E = .;
    if V841F    = 9 then V841F = .;
    if V841G    = 9 then V841G = .;
    if V842A1   = 9 then V842A1 = .;
    if V842A2   = 9 then V842A2 = .;
    if V842B    = 9 then V842B = .;
    if V842C    = 9 then V842C = .;
    if V842G    = 9 then V842G = .;
    if V843A    = 9 then V843A = .;
    if V843B    = 9 then V843B = .;
    if V843C    = 9 then V843C = .;
    if V843E    = 9 then V843E = .;
    if V843F    = 9 then V843F = .;
    if V843G    = 9 then V843G = .;
    if V843H    = 9 then V843H = .;
    if V843I    = 9 then V843I = .;
    if V843J    = 9 then V843J = .;
    if V843K    = 9 then V843K = .;
    if V843L    = 9 then V843L = .;
    if V843M    = 9 then V843M = .;
    if V843N    = 9 then V843N = .;
    if V844A    = 9 then V844A = .;
    if V844B    = 9 then V844B = .;
    if V844C    = 9 then V844C = .;
    if V844F    = 9 then V844F = .;
    if V844G    = 9 then V844G = .;
    if V844H    = 9 then V844H = .;
    if V844I    = 9 then V844I = .;
    if V844J    = 9 then V844J = .;
    if V844K    = 9 then V844K = .;
    if V844N    = 9 then V844N = .;
    if V845A    = 9 then V845A = .;
    if V845B    = 9 then V845B = .;
    if V845C    = 9 then V845C = .;
    if V845D    = 9 then V845D = .;
    if V845G    = 9 then V845G = .;
    if V845H    = 9 then V845H = .;
    if V845I    = 9 then V845I = .;
    if V845J    = 9 then V845J = .;
    if V845K    = 9 then V845K = .;
    if V846A    = 9 then V846A = .;
    if V846B    = 9 then V846B = .;
    if V846C    = 9 then V846C = .;
    if V846D    = 9 then V846D = .;
    if V846H    = 9 then V846H = .;
    if V847A    = 9 then V847A = .;
    if V847B    = 9 then V847B = .;
    if V847C    = 9 then V847C = .;
    if V847D    = 9 then V847D = .;
    if V847E    = 9 then V847E = .;
    if V848B    = 9 then V848B = .;
    if V849A    = 9 then V849A = .;
    if V849B    = 9 then V849B = .;
    if V849B1   = 9 then V849B1 = .;
    if V850A    = 9 then V850A = .;
    if V850B    = 9 then V850B = .;
    if V850B1   = 9 then V850B1 = .;
    if V851     = 9 then V851 = .;
    if V851A    = 9 then V851A = .;
    if V851B    = 9 then V851B = .;
    if V852A    = 9 then V852A = .;
    if V852B    = 9 then V852B = .;
    if V852C    = 9 then V852C = .;
    if V852D    = 9 then V852D = .;
    if V855A    = 9 then V855A = .;
    if V855B    = 9 then V855B = .;
    if V855C    = 9 then V855C = .;
    if V855D    = 9 then V855D = .;
    if V856A    = 9 then V856A = .;
    if V856B    = 9 then V856B = .;
    if V857B    = 9 then V857B = .;
    if V857C    = 9 then V857C = .;
    if V857D    = 9 then V857D = .;
    if V859A    = 9 then V859A = .;
    if V859B    = 9 then V859B = .;
    if V859C    = 9 then V859C = .;
    if V859D    = 9 then V859D = .;
    if V859E    = 9 then V859E = .;
    if V859F    = 9 then V859F = .;
    if V859G    = 9 then V859G = .;
    if V859GG   = 9 then V859GG = .;
    if V859H    = 9 then V859H = .;
    if V859I    = 9 then V859I = .;
    if V860A    = 9 then V860A = .;
    if V860B    = 9 then V860B = .;
    if V860C    = 9 then V860C = .;
    if V860D    = 9 then V860D = .;
    if V860F    = 9 then V860F = .;
    if V861A    = 9 then V861A = .;
    if V861B    = 9 then V861B = .;
    if V861C    = 9 then V861C = .;
    if V861D    = 9 then V861D = .;
    if V861E    = 9 then V861E = .;
    if V863     = 9 then V863 = .;
    if V864A    = 9 then V864A = .;
    if V864B    = 9 then V864B = .;
    if V864C    = 9 then V864C = .;
    if V864D    = 9 then V864D = .;
    if V864F    = 9 then V864F = .;
    if V864E    = 9 then V864E = .;
    if V865A    = 9 then V865A = .;
    if V865B    = 9 then V865B = .;
    if V865C    = 9 then V865C = .;
    if V865D    = 9 then V865D = .;
    if V865F    = 9 then V865F = .;
    if V865E    = 9 then V865E = .;
    if V866A1   = 9 then V866A1 = .;
    if V866C    = 9 then V866C = .;
    if V866F    = 9 then V866F = .;
    if V866G    = 9 then V866G = .;
    if V866J    = 9 then V866J = .;
    if V866L    = 9 then V866L = .;
    if V866O    = 9 then V866O = .;
    if V866P    = 9 then V866P = .;
    if V866Q    = 9 then V866Q = .;
    if V866R    = 9 then V866R = .;
    if V866S    = 9 then V866S = .;
    if V866T    = 9 then V866T = .;
    if V866U    = 9 then V866U = .;
    if V866V    = 9 then V866V = .;
    if V866W    = 9 then V866W = .;
    if V870A    = 9 then V870A = .;
    if V870B    = 9 then V870B = .;
    if V870C    = 9 then V870C = .;
    if V870D    = 9 then V870D = .;
    if V870E    = 9 then V870E = .;
    if V870F    = 9 then V870F = .;
    if V870X    = 9 then V870X = .;
    if V871     = 9 then V871 = .;
    if V872     = 9 then V872 = .;
    if V873A    = 9 then V873A = .;
    if V873B    = 9 then V873B = .;
    if V874A    = 9 then V874A = .;
    if V874B    = 9 then V874B = .;
    if V874C    = 9 then V874C = .;
    if V874D    = 9 then V874D = .;
    if V874E    = 9 then V874E = .;
    if V874X    = 9 then V874X = .;
    if V875     = 9 then V875 = .;
    if V876A    = 9 then V876A = .;
    if V876B    = 9 then V876B = .;
    if V877     = 9 then V877 = .;
    if SF802D   = 9 then SF802D = .;
    if SF802EB1 = 9 then SF802EB1 = .;
    if SF802EB2 = 9 then SF802EB2 = .;
    if SF802EB3 = 9 then SF802EB3 = .;
    if SF802EC1 = 9 then SF802EC1 = .;
    if SF802EC3 = 9 then SF802EC3 = .;
    if SF803A   = 9 then SF803A = .;
    if SF8081   = 9 then SF8081 = .;
    if SF8082   = 9 then SF8082 = .;
    if SF8083   = 9 then SF8083 = .;
    if SF8084   = 9 then SF8084 = .;
    if SF8085   = 9 then SF8085 = .;
    if SF835A   = 9 then SF835A = .;
    if SF835BC  = 9 then SF835BC = .;
    if SF835D   = 9 then SF835D = .;
    if SF840X   = 9 then SF840X = .;
    if SF847C   = 9 then SF847C = .;
    if SF847DE  = 9 then SF847DE = .;
    if SF873AB  = 9 then SF873AB = .;
    if SF874A   = 9 then SF874A = .;
    if SF874BC  = 9 then SF874BC = .;
    if SF874D   = 9 then SF874D = .;
    if SF874EF  = 9 then SF874EF = .;
    if V902A    = 9 then V902A = .;
    if V902B    = 9 then V902B = .;
    if V903_02  = 9 then V903_02 = .;
    if V903_05  = 9 then V903_05 = .;
    if V903_06  = 9 then V903_06 = .;
    if V903_08  = 9 then V903_08 = .;
    if V903_10  = 9 then V903_10 = .;
    if V903_13  = 9 then V903_13 = .;
    if V903_14  = 9 then V903_14 = .;
    if V903_15  = 9 then V903_15 = .;
    if V903_16  = 9 then V903_16 = .;
    if V903_17  = 9 then V903_17 = .;
    if V903_18  = 9 then V903_18 = .;
    if V903_19  = 9 then V903_19 = .;
    if V903_20  = 9 then V903_20 = .;
    if V904_03  = 9 then V904_03 = .;
    if V904_04  = 9 then V904_04 = .;
    if V904_05  = 9 then V904_05 = .;
    if V904_06  = 9 then V904_06 = .;
    if V905_03  = 9 then V905_03 = .;
    if V905_08  = 9 then V905_08 = .;
    if V905_14  = 9 then V905_14 = .;
    if V905_18  = 9 then V905_18 = .;
    if V905_19  = 9 then V905_19 = .;
    if V905_20  = 9 then V905_20 = .;
    if V905_21  = 9 then V905_21 = .;
    if V905_22  = 9 then V905_22 = .;
    if V906_03  = 9 then V906_03 = .;
    if V906_07  = 9 then V906_07 = .;
    if V906_08  = 9 then V906_08 = .;
    if V906_11  = 9 then V906_11 = .;
    if V906_12  = 9 then V906_12 = .;
    if V907_03  = 9 then V907_03 = .;
    if V907_04  = 9 then V907_04 = .;
    if V907_05  = 9 then V907_05 = .;
    if V907_06  = 9 then V907_06 = .;
    if V907_07  = 9 then V907_07 = .;
    if V907_08  = 9 then V907_08 = .;
    if V908_02  = 9 then V908_02 = .;
    if V908_03  = 9 then V908_03 = .;
    if V908_04  = 9 then V908_04 = .;
    if V908_05  = 9 then V908_05 = .;
    if V909_02  = 9 then V909_02 = .;
    if V909_04  = 9 then V909_04 = .;
    if V909_05  = 9 then V909_05 = .;
    if V909_06  = 9 then V909_06 = .;
    if V910_08  = 9 then V910_08 = .;
    if V911_02  = 9 then V911_02 = .;
    if V913_13  = 9 then V913_13 = .;
    if V914_08  = 9 then V914_08 = .;
    if V915_04  = 9 then V915_04 = .;
    if V916_09  = 9 then V916_09 = .;
    if V917_06  = 9 then V917_06 = .;
    if V918_06  = 9 then V918_06 = .;
    if V921_04  = 9 then V921_04 = .;
    if V921_05  = 9 then V921_05 = .;
    if V921_06  = 9 then V921_06 = .;
    if V921_07  = 9 then V921_07 = .;
    if V921_08  = 9 then V921_08 = .;
    if V921_09  = 9 then V921_09 = .;
    if V921_10  = 9 then V921_10 = .;
    if V921_11  = 9 then V921_11 = .;
    if V910_03  = 9 then V910_03 = .;
    if V910_04  = 9 then V910_04 = .;
    if V910_06  = 9 then V910_06 = .;
    if V910_07  = 9 then V910_07 = .;
    if V913_01  = 9 then V913_01 = .;
    if V913_02  = 9 then V913_02 = .;
    if V913_04  = 9 then V913_04 = .;
    if V913_05  = 9 then V913_05 = .;
    if V913_06  = 9 then V913_06 = .;
    if V913_07  = 9 then V913_07 = .;
    if V913_10  = 9 then V913_10 = .;
    if V913_11  = 9 then V913_11 = .;
    if V913_12  = 9 then V913_12 = .;
    if V914_05  = 9 then V914_05 = .;
    if V914_06  = 9 then V914_06 = .;
    if V908_06  = 9 then V908_06 = .;
    if V908_07  = 9 then V908_07 = .;
    if V914_07  = 9 then V914_07 = .;
    if V915_01  = 9 then V915_01 = .;
    if V915_02  = 9 then V915_02 = .;
    if V915_03  = 9 then V915_03 = .;
    if V916_01  = 9 then V916_01 = .;
    if V916_03  = 9 then V916_03 = .;
    if V916_06  = 9 then V916_06 = .;
    if V916_07  = 9 then V916_07 = .;
    if V916_08  = 9 then V916_08 = .;
    if V906_09  = 9 then V906_09 = .;
    if V906_10  = 9 then V906_10 = .;
    if V917_04  = 9 then V917_04 = .;
    if V917_05  = 9 then V917_05 = .;
    if V918_01  = 9 then V918_01 = .;
    if V918_02  = 9 then V918_02 = .;
    if V918_04  = 9 then V918_04 = .;
    if V918_05  = 9 then V918_05 = .;
    if V919_03  = 9 then V919_03 = .;
    if V920_02  = 9 then V920_02 = .;
    if V920_03  = 9 then V920_03 = .;
    if V920_04  = 9 then V920_04 = .;
    if V921_01  = 9 then V921_01 = .;
    if V921_02  = 9 then V921_02 = .;
    if V922_03  = 9 then V922_03 = .;
    if V922_04  = 9 then V922_04 = .;
    if V922_05  = 9 then V922_05 = .;
    if V922_11  = 9 then V922_11 = .;
    if V922_12  = 9 then V922_12 = .;
    if V922_13  = 9 then V922_13 = .;
    if V922_14  = 9 then V922_14 = .;
    if V922_15  = 9 then V922_15 = .;
    if V922_16  = 9 then V922_16 = .;
    if V922_17  = 9 then V922_17 = .;
    if V922_18  = 9 then V922_18 = .;
    if V922_19  = 9 then V922_19 = .;
    if V923_01  = 9 then V923_01 = .;
    if V923_02  = 9 then V923_02 = .;
    if V923_03  = 9 then V923_03 = .;
    if V923_04  = 9 then V923_04 = .;
    if V923_05  = 9 then V923_05 = .;
    if V923_06  = 9 then V923_06 = .;
    if V923_07  = 9 then V923_07 = .;
    if V923_08  = 9 then V923_08 = .;
    if V923_09  = 9 then V923_09 = .;
    if V923_11  = 9 then V923_11 = .;
    if V923_12  = 9 then V923_12 = .;
    if V924_01  = 9 then V924_01 = .;
    if V924_02  = 9 then V924_02 = .;
    if V924_03  = 9 then V924_03 = .;
    if V924_05  = 9 then V924_05 = .;
    if V926     = 9 then V926 = .;
    if V927     = 9 then V927 = .;
    if V928A    = 9 then V928A = .;
    if V928B1   = 9 then V928B1 = .;
    if V928B2   = 9 then V928B2 = .;
    if V928C    = 9 then V928C = .;
    if V928D    = 9 then V928D = .;
    if V936     = 9 then V936 = .;
    if V946_06  = 9 then V946_06 = .;
    if V946_07  = 9 then V946_07 = .;
    if V946_09  = 9 then V946_09 = .;
    if V946_12  = 9 then V946_12 = .;
    if V946_13  = 9 then V946_13 = .;
    if V946_16  = 9 then V946_16 = .;
    if V946_17  = 9 then V946_17 = .;
    if V946_18  = 9 then V946_18 = .;
    if V946_19  = 9 then V946_19 = .;
    if V950A    = 9 then V950A = .;
    if V950B    = 9 then V950B = .;
    if V951     = 9 then V951 = .;
    if V952     = 9 then V952 = .;
    if V953A    = 9 then V953A = .;
    if V953B1   = 9 then V953B1 = .;
    if V953B2   = 9 then V953B2 = .;
    if V953C    = 9 then V953C = .;
    if V953D    = 9 then V953D = .;
    if V953E    = 9 then V953E = .;
    if V955_01  = 9 then V955_01 = .;
    if V955_02  = 9 then V955_02 = .;
    if V955_03  = 9 then V955_03 = .;
    if V955_04  = 9 then V955_04 = .;
    if V955_05  = 9 then V955_05 = .;
    if V955_06  = 9 then V955_06 = .;
    if V955_07  = 9 then V955_07 = .;
    if V955_08  = 9 then V955_08 = .;
    if V955_09  = 9 then V955_09 = .;
    if V955_10  = 9 then V955_10 = .;
    if V955_11  = 9 then V955_11 = .;
    if V956_01  = 9 then V956_01 = .;
    if V956_02  = 9 then V956_02 = .;
    if V956_03  = 9 then V956_03 = .;
    if V956_04  = 9 then V956_04 = .;
    if V956_05  = 9 then V956_05 = .;
    if V956_06  = 9 then V956_06 = .;
    if V957_01  = 9 then V957_01 = .;
    if V957_02  = 9 then V957_02 = .;
    if V957_03  = 9 then V957_03 = .;
    if V957_04  = 9 then V957_04 = .;
    if V957_05  = 9 then V957_05 = .;
    if V957_06  = 9 then V957_06 = .;
    if V957_07  = 9 then V957_07 = .;
    if V957_08  = 9 then V957_08 = .;
    if V957_09  = 9 then V957_09 = .;
    if V957_10  = 9 then V957_10 = .;
    if V958_01  = 9 then V958_01 = .;
    if V958_02  = 9 then V958_02 = .;
    if V958_03  = 9 then V958_03 = .;
    if V958_04  = 9 then V958_04 = .;
    if V958_05  = 9 then V958_05 = .;
    if V958_06  = 9 then V958_06 = .;
    if V958_07  = 9 then V958_07 = .;
    if V958_08  = 9 then V958_08 = .;
    if V958_09  = 9 then V958_09 = .;
    if V958_10  = 9 then V958_10 = .;
    if V958_11  = 9 then V958_11 = .;
    if V960A    = 9 then V960A = .;
    if V960B    = 9 then V960B = .;
    if V961     = 9 then V961 = .;
    if V962     = 9 then V962 = .;
    if V963A    = 9 then V963A = .;
    if V963B1   = 9 then V963B1 = .;
    if V963B2   = 9 then V963B2 = .;
    if V963C    = 9 then V963C = .;
    if V963D    = 9 then V963D = .;
    if SF905A_01 = 9 then SF905A_01 = .;
    if SF905A_02 = 9 then SF905A_02 = .;
    if SF905A_03 = 9 then SF905A_03 = .;
    if SF905A_04 = 9 then SF905A_04 = .;
    if SF908A_01 = 9 then SF908A_01 = .;
    if SF908A_02 = 9 then SF908A_02 = .;
    if SF908A_03 = 9 then SF908A_03 = .;
    if SF908A_04 = 9 then SF908A_04 = .;
    if SF908A_05 = 9 then SF908A_05 = .;
    if SF908A_06 = 9 then SF908A_06 = .;
    if SF908A_07 = 9 then SF908A_07 = .;
    if SF908A_08 = 9 then SF908A_08 = .;
    if SF908A_09 = 9 then SF908A_09 = .;
    if SF908A_10 = 9 then SF908A_10 = .;
    if SF908A_11 = 9 then SF908A_11 = .;
    if SF908A_12 = 9 then SF908A_12 = .;
    if SF908A_13 = 9 then SF908A_13 = .;
    if SF908A_14 = 9 then SF908A_14 = .;
    if SF912A_01 = 9 then SF912A_01 = .;
    if SF912A_02 = 9 then SF912A_02 = .;
    if V1001    = 99 then V1001 = .;
    if V1002    = 9 then V1002 = .;
    if V1003    = 9 then V1003 = .;
    if V1004    = 9 then V1004 = .;
    if V1005A   = 9 then V1005A = .;
    if V1005B   = 9 then V1005B = .;
    if V1005C   = 9 then V1005C = .;
    if V1005D   = 9 then V1005D = .;
    if V1005E   = 9 then V1005E = .;
    if V1005F   = 9 then V1005F = .;
    if V1005X   = 9 then V1005X = .;
    if V1006    = 9 then V1006 = .;
    if V1007A   = 9 then V1007A = .;
    if V1007X   = 9 then V1007X = .;
    if SF1712AB = 9 then SF1712AB = .;
    if V1100A   = 9 then V1100A = .;
    if V1106    = 9 then V1106 = .;
    if V1107A   = 9 then V1107A = .;
    if V1107B   = 9 then V1107B = .;
    if V1107C   = 9 then V1107C = .;
    if V1109A   = 9 then V1109A = .;
    if V1109B   = 9 then V1109B = .;
    if V1109C   = 9 then V1109C = .;
    if V1109D   = 9 then V1109D = .;
    if SV1100A  = 9 then SV1100A = .;
    if V1115    = 9 then V1115 = .;
    if V1138    = 9 then V1138 = .;
    if V1139A1  = 9 then V1139A1 = .;
    if V1139C   = 9 then V1139C = .;
    if V1139F   = 9 then V1139F = .;
    if V1139G   = 9 then V1139G = .;
    if V1139J   = 9 then V1139J = .;
    if V1139L   = 9 then V1139L = .;
    if V1139O   = 9 then V1139O = .;
    if V1139P   = 9 then V1139P = .;
    if V1139Q   = 9 then V1139Q = .;
    if V1139R   = 9 then V1139R = .;
    if V1139S   = 9 then V1139S = .;
    if V1139T   = 9 then V1139T = .;
    if V1139U   = 9 then V1139U = .;
    if V1139V   = 9 then V1139V = .;
    if V1139W   = 9 then V1139W = .;
    if V1140    = 99 then V1140 = .;
    if V1141    = 9 then V1141 = .;
    if V1142    = 9 then V1142 = .;
    if V1143    = 9 then V1143 = .;
    if V1144    = 9 then V1144 = .;
    if SF19141  = 9 then SF19141 = .;
    if SF19142  = 9 then SF19142 = .;
    if SF19143  = 9 then SF19143 = .;
    if SF19144  = 9 then SF19144 = .;
    if SF19145  = 9 then SF19145 = .;
    if SF1916AB = 9 then SF1916AB = .;
    if V1206    = 99 then V1206 = .;
    if V1208A1  = 9 then V1208A1 = .;
    if V1208A2  = 9 then V1208A2 = .;
    if V1208F   = 9 then V1208F = .;
    if V1208G1  = 9 then V1208G1 = .;
    if V1208H   = 9 then V1208H = .;
    if V1208L   = 9 then V1208L = .;
    if V1208M   = 9 then V1208M = .;
    if V1208S   = 9 then V1208S = .;
    if V1215A   = 9 then V1215A = .;
    if V1215B   = 9 then V1215B = .;
    if V1215X   = 9 then V1215X = .;
    if V1215Y   = 9 then V1215Y = .;
    if V1260    = 9 then V1260 = .;
    if V1273A   = 9 then V1273A = .;
    if V1273B   = 9 then V1273B = .;
    if V1273C   = 9 then V1273C = .;
    if V1273D   = 9 then V1273D = .;
    if V1273E   = 9 then V1273E = .;
    if V1273F   = 9 then V1273F = .;
    if V1273G   = 9 then V1273G = .;
    if V1273H   = 9 then V1273H = .;
    if V1273I   = 9 then V1273I = .;
    if V1273J   = 9 then V1273J = .;
    if V1273K   = 9 then V1273K = .;
    if V1273L   = 9 then V1273L = .;
    if V1273M   = 9 then V1273M = .;
    if V1273N   = 9 then V1273N = .;
    if V1273Y   = 9 then V1273Y = .;
    if V1274    = 9 then V1274 = .;
    if V1275    = 9 then V1275 = .;
    if V1276    = 9 then V1276 = .;
    if V1277A1  = 9 then V1277A1 = .;
    if V1277C   = 9 then V1277C = .;
    if V1277F   = 9 then V1277F = .;
    if V1277G   = 9 then V1277G = .;
    if V1277J   = 9 then V1277J = .;
    if V1277L   = 9 then V1277L = .;
    if V1277O   = 9 then V1277O = .;
    if V1277P   = 9 then V1277P = .;
    if V1277Q   = 9 then V1277Q = .;
    if V1277R   = 9 then V1277R = .;
    if V1277S   = 9 then V1277S = .;
    if V1277T   = 9 then V1277T = .;
    if V1277U   = 9 then V1277U = .;
    if V1277V   = 9 then V1277V = .;
    if V1277W   = 9 then V1277W = .;
    if V1278A   = 9 then V1278A = .;
    if V1278B   = 9 then V1278B = .;
    if V1278C   = 9 then V1278C = .;
    if V1278D   = 9 then V1278D = .;
    if V1278E   = 9 then V1278E = .;
    if V1278F   = 9 then V1278F = .;
    if V1278G   = 9 then V1278G = .;
    if V1278H   = 9 then V1278H = .;
    if V1278I   = 9 then V1278I = .;
    if V1278J   = 9 then V1278J = .;
    if V1278K   = 9 then V1278K = .;
    if V1278L   = 9 then V1278L = .;
    if V1278M   = 9 then V1278M = .;
    if V1278N   = 9 then V1278N = .;
    if V1278Y   = 9 then V1278Y = .;
    if V1279    = 9 then V1279 = .;
    if V1280    = 9 then V1280 = .;
    if V1281A1  = 9 then V1281A1 = .;
    if V1281C   = 9 then V1281C = .;
    if V1281F   = 9 then V1281F = .;
    if V1281G   = 9 then V1281G = .;
    if V1281J   = 9 then V1281J = .;
    if V1281L   = 9 then V1281L = .;
    if V1281O   = 9 then V1281O = .;
    if V1281P   = 9 then V1281P = .;
    if V1281Q   = 9 then V1281Q = .;
    if V1281R   = 9 then V1281R = .;
    if V1281S   = 9 then V1281S = .;
    if V1281T   = 9 then V1281T = .;
    if V1281U   = 9 then V1281U = .;
    if V1281V   = 9 then V1281V = .;
    if V1281W   = 9 then V1281W = .;
    if SF1505A1 = 9 then SF1505A1 = .;
    if SF1505A2 = 9 then SF1505A2 = .;
    if SF1505A3 = 9 then SF1505A3 = .;
    if SF1505A4 = 9 then SF1505A4 = .;
    if SF1505A5 = 9 then SF1505A5 = .;
    if SF1508A  = 9 then SF1508A = .;
    if V1307A   = 99 then V1307A = .;
    if V1327D   = 9 then V1327D = .;
    if V1327E   = 9 then V1327E = .;
    if V1327G   = 9 then V1327G = .;
    if V1327H   = 9 then V1327H = .;
    if V1327I   = 9 then V1327I = .;
    if V1327J   = 9 then V1327J = .;
    if V1328    = 9 then V1328 = .;
    if V1329A1  = 9 then V1329A1 = .;
    if V1329C   = 9 then V1329C = .;
    if V1329F   = 9 then V1329F = .;
    if V1329G   = 9 then V1329G = .;
    if V1329J   = 9 then V1329J = .;
    if V1329L   = 9 then V1329L = .;
    if V1329O   = 9 then V1329O = .;
    if V1329P   = 9 then V1329P = .;
    if V1329Q   = 9 then V1329Q = .;
    if V1329R   = 9 then V1329R = .;
    if V1329S   = 9 then V1329S = .;
    if V1329T   = 9 then V1329T = .;
    if V1329U   = 9 then V1329U = .;
    if V1329V   = 9 then V1329V = .;
    if V1329W   = 9 then V1329W = .;
    if V1331    = 9 then V1331 = .;
    if V1332A   = 9 then V1332A = .;
    if V1332B   = 9 then V1332B = .;
    if V1332X   = 9 then V1332X = .;
    if V1337    = 9 then V1337 = .;
    if V1355    = 9 then V1355 = .;
    if V1356    = 9 then V1356 = .;
    if V1357    = 9 then V1357 = .;
    if V1358    = 9 then V1358 = .;
    if SF20041  = 9 then SF20041 = .;
    if SF20042  = 9 then SF20042 = .;
    if SF20043  = 9 then SF20043 = .;
    if SF20044  = 9 then SF20044 = .;
    if SF20045  = 9 then SF20045 = .;
    if V1415A   = 9 then V1415A = .;
    if V1415B   = 9 then V1415B = .;
    if V1415C   = 9 then V1415C = .;
    if V1415D   = 9 then V1415D = .;
    if V1415E   = 9 then V1415E = .;
    if V1415F   = 9 then V1415F = .;
    if V1415G   = 9 then V1415G = .;
    if V1415H   = 9 then V1415H = .;
    if V1415I   = 9 then V1415I = .;
    if V1415J   = 9 then V1415J = .;
    if V1415K   = 9 then V1415K = .;
    if V1415L   = 9 then V1415L = .;
    if V1415M   = 9 then V1415M = .;
    if V1415N   = 9 then V1415N = .;
    if V1415X   = 9 then V1415X = .;
    if V1421A   = 9 then V1421A = .;
    if V1421X   = 9 then V1421X = .;
    if V1451    = 9 then V1451 = .;
    if V1452A   = 9 then V1452A = .;
    if V1452B   = 9 then V1452B = .;
    if V1452C   = 9 then V1452C = .;
    if V1452D   = 9 then V1452D = .;
    if V1452E   = 9 then V1452E = .;
    if V1452F   = 9 then V1452F = .;
    if V1452G   = 9 then V1452G = .;
    if V1452H   = 9 then V1452H = .;
    if V1452I   = 9 then V1452I = .;
    if V1452J   = 9 then V1452J = .;
    if V1452K   = 9 then V1452K = .;
    if V1452L   = 9 then V1452L = .;
    if V1452M   = 9 then V1452M = .;
    if V1452N   = 9 then V1452N = .;
    if V1452X   = 9 then V1452X = .;
    if SF2105AB = 9 then SF2105AB = .;
    if V1515    = 9 then V1515 = .;
    if V1516A   = 9 then V1516A = .;
    if V1516B   = 9 then V1516B = .;
    if V1516C   = 9 then V1516C = .;
    if V1516D   = 9 then V1516D = .;
    if V1516E   = 9 then V1516E = .;
    if V1516F   = 9 then V1516F = .;
    if V1516J   = 9 then V1516J = .;
    if V1518B   = 9 then V1518B = .;
    if V1518C   = 9 then V1518C = .;
    if V1518D   = 9 then V1518D = .;
    if V1518E   = 9 then V1518E = .;
    if V1518F   = 9 then V1518F = .;
    if V1521A   = 9 then V1521A = .;
    if V1521B   = 9 then V1521B = .;
    if V1560    = 9 then V1560 = .;
    if V1561    = 9 then V1561 = .;
    if SF2205A  = 9 then SF2205A = .;
    if V1601    = 9 then V1601 = .;
    if V1601A   = 9 then V1601A = .;
    if V1602A   = 9 then V1602A = .;
    if V1602X   = 9 then V1602X = .;
    if V1603    = 9 then V1603 = .;
    if V1603A   = 9 then V1603A = .;
    if V1604A   = 9 then V1604A = .;
    if V1604X   = 9 then V1604X = .;
    if V1605    = 9 then V1605 = .;
    if V1605A   = 9 then V1605A = .;
    if V1606A   = 9 then V1606A = .;
    if V1606X   = 9 then V1606X = .;
    if V1607A   = 9 then V1607A = .;
    if V1607B   = 9 then V1607B = .;
    if V1607C   = 9 then V1607C = .;
    if V1607D   = 9 then V1607D = .;
    if V1608A   = 9 then V1608A = .;
    if V1608B   = 9 then V1608B = .;
    if V1608C   = 9 then V1608C = .;
    if V1608D   = 9 then V1608D = .;
    if V1608E   = 9 then V1608E = .;
    if V1608F   = 9 then V1608F = .;
    if V1608G   = 9 then V1608G = .;
    if V1608H   = 9 then V1608H = .;
    if V1608I   = 9 then V1608I = .;
    if V1608J   = 9 then V1608J = .;
    if V1608K   = 9 then V1608K = .;
    if V1608L   = 9 then V1608L = .;
    if V1608M   = 9 then V1608M = .;
    if V1608N   = 9 then V1608N = .;
    if V1608O   = 9 then V1608O = .;
    if V1608P   = 9 then V1608P = .;
    if V1608Q   = 9 then V1608Q = .;
    if V1608R   = 9 then V1608R = .;
    if V1609    = 9 then V1609 = .;
    if V1610A1  = 9 then V1610A1 = .;
    if V1610C   = 9 then V1610C = .;
    if V1610F   = 9 then V1610F = .;
    if V1610G   = 9 then V1610G = .;
    if V1610J   = 9 then V1610J = .;
    if V1610L   = 9 then V1610L = .;
    if V1610O   = 9 then V1610O = .;
    if V1610P   = 9 then V1610P = .;
    if V1610Q   = 9 then V1610Q = .;
    if V1610R   = 9 then V1610R = .;
    if V1610S   = 9 then V1610S = .;
    if V1610T   = 9 then V1610T = .;
    if V1610U   = 9 then V1610U = .;
    if V1610V   = 9 then V1610V = .;
    if V1610W   = 9 then V1610W = .;
    if V1701    = 9 then V1701 = .;
    if V1702A   = 9 then V1702A = .;
    if V1702B   = 9 then V1702B = .;
    if V1702C   = 9 then V1702C = .;
    if V1702D   = 9 then V1702D = .;
    if V1702E   = 9 then V1702E = .;
    if V1702F   = 9 then V1702F = .;
    if V1703A   = 9 then V1703A = .;
    if V1703B   = 9 then V1703B = .;
    if V1703C   = 9 then V1703C = .;
    if V1703D   = 9 then V1703D = .;
    if V1703E   = 9 then V1703E = .;
    if V1704    = 9 then V1704 = .;
    if V1705A1  = 9 then V1705A1 = .;
    if V1705C   = 9 then V1705C = .;
    if V1705F   = 9 then V1705F = .;
    if V1705G   = 9 then V1705G = .;
    if V1705J   = 9 then V1705J = .;
    if V1705L   = 9 then V1705L = .;
    if V1705O   = 9 then V1705O = .;
    if V1705P   = 9 then V1705P = .;
    if V1705Q   = 9 then V1705Q = .;
    if V1705R   = 9 then V1705R = .;
    if V1705S   = 9 then V1705S = .;
    if V1705T   = 9 then V1705T = .;
    if V1705U   = 9 then V1705U = .;
    if V1705V   = 9 then V1705V = .;
    if V1705W   = 9 then V1705W = .;
    if SF1720B  = 9 then SF1720B = .;
    if SF1720BC = 9 then SF1720BC = .;
    if SF1720D  = 9 then SF1720D = .;
    if SF1720EF = 9 then SF1720EF = .;
    if SF1730B  = 9 then SF1730B = .;
    if SF1730BC = 9 then SF1730BC = .;
    if SF1730BD = 9 then SF1730BD = .;
    if SF1740B  = 9 then SF1740B = .;
    if SF1740BC = 9 then SF1740BC = .;
    if SF1740BD = 9 then SF1740BD = .;
Run;
