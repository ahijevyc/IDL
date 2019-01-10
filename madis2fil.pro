; copied to yellowstone Dec 2 2014.  Because MMM IDL version doesn't have irregular contour function yet.

function ok, x
  return, x lt 3e18 and x ne -9999.
end
pro madis2fil
  basedir = '/glade/work/ahijevyc/mpex/surface/'
  infile = basedir + '20130519_2000'
  hh = strmid(file_basename(infile),9,2)
  savfile = basedir + hh+'.sav'
  if file_test(savfile) then restore, savfile else begin
    ;*************************************************************
    ; IDL script for reading NetCDF file:
    ;*************************************************************
    close, /all
    
    ncid = NCDF_OPEN(infile)            ; Open The NetCDF file
    NCDF_VARGET, ncid,  0, nStaticIds      ; Read in variable 'nStaticIds'
    NCDF_VARGET, ncid,  1, staticIds      ; Read in variable 'staticIds'
    NCDF_VARGET, ncid,  2, lastRecord      ; Read in variable 'lastRecord'
    
    NCDF_VARGET, ncid,  3, invTime      ; Read in variable 'invTime'
    
    NCDF_VARGET, ncid,  4, prevRecord      ; Read in variable 'prevRecord'
    
    NCDF_VARGET, ncid,  5, inventory      ; Read in variable 'inventory'
    
    NCDF_VARGET, ncid,  6, globalInventory      ; Read in variable 'globalInventory'
    
    NCDF_VARGET, ncid,  7, firstOverflow      ; Read in variable 'firstOverflow'
    
    NCDF_VARGET, ncid,  8, isOverflow      ; Read in variable 'isOverflow'
    
    NCDF_VARGET, ncid,  9, firstInBin      ; Read in variable 'firstInBin'
    
    NCDF_VARGET, ncid,  10, lastInBin      ; Read in variable 'lastInBin'
    
    NCDF_VARGET, ncid,  11, secondsStage1_2      ; Read in variable 'secondsStage1_2'
    
    NCDF_VARGET, ncid,  12, secondsStage3      ; Read in variable 'secondsStage3'
    
    NCDF_VARGET, ncid,  13, providerId      ; Read in variable 'providerId'
    
    NCDF_VARGET, ncid,  14, stationId      ; Read in variable 'stationId'
    
    NCDF_VARGET, ncid,  15, handbook5Id      ; Read in variable 'handbook5Id'
    
    NCDF_VARGET, ncid,  16, stationName      ; Read in variable 'stationName'
    
    NCDF_VARGET, ncid,  17, homeWFO      ; Read in variable 'homeWFO'
    
    NCDF_VARGET, ncid,  18, numericWMOid      ; Read in variable 'numericWMOid'
    
    NCDF_VARGET, ncid,  19, stationType      ; Read in variable 'stationType'
    
    NCDF_VARGET, ncid,  20, dataProvider      ; Read in variable 'dataProvider'
    
    NCDF_VARGET, ncid,  21, filterSetNum      ; Read in variable 'filterSetNum'
    
    NCDF_VARGET, ncid,  22, QCT      ; Read in variable 'QCT'
    
    NCDF_VARGET, ncid,  23, ICT      ; Read in variable 'ICT'
    
    NCDF_VARGET, ncid,  24, latitude      ; Read in variable 'latitude'
    
    NCDF_VARGET, ncid,  25, longitude      ; Read in variable 'longitude'
    
    NCDF_VARGET, ncid,  26, elevation      ; Read in variable 'elevation'
    
    NCDF_VARGET, ncid,  27, observationTime      ; Read in variable 'observationTime'
    
    NCDF_VARGET, ncid,  28, reportTime      ; Read in variable 'reportTime'
    
    NCDF_VARGET, ncid,  29, receivedTime      ; Read in variable 'receivedTime'
    
    NCDF_VARGET, ncid,  30, temperature      ; Read in variable 'temperature'
    
    NCDF_VARGET, ncid,  31, tempChangeTime      ; Read in variable 'tempChangeTime'
    
    NCDF_VARGET, ncid,  32, temperatureDD      ; Read in variable 'temperatureDD'
    temperatureDD = string(temperatureDD)
    
    NCDF_VARGET, ncid,  33, temperatureQCA      ; Read in variable 'temperatureQCA'
    
    NCDF_VARGET, ncid,  34, temperatureQCR      ; Read in variable 'temperatureQCR'
    
    NCDF_VARGET, ncid,  35, temperatureQCD      ; Read in variable 'temperatureQCD'
    
    NCDF_VARGET, ncid,  36, temperatureICA      ; Read in variable 'temperatureICA'
    
    NCDF_VARGET, ncid,  37, temperatureICR      ; Read in variable 'temperatureICR'
    
    NCDF_VARGET, ncid,  38, dewpoint      ; Read in variable 'dewpoint'
    
    NCDF_VARGET, ncid,  39, dewpointDD      ; Read in variable 'dewpointDD'
    
    NCDF_VARGET, ncid,  40, dewpointQCA      ; Read in variable 'dewpointQCA'
    
    NCDF_VARGET, ncid,  41, dewpointQCR      ; Read in variable 'dewpointQCR'
    
    NCDF_VARGET, ncid,  42, dewpointQCD      ; Read in variable 'dewpointQCD'
    
    NCDF_VARGET, ncid,  43, dewpointICA      ; Read in variable 'dewpointICA'
    
    NCDF_VARGET, ncid,  44, dewpointICR      ; Read in variable 'dewpointICR'
    
    NCDF_VARGET, ncid,  45, relHumidity      ; Read in variable 'relHumidity'
    
    NCDF_VARGET, ncid,  46, rhChangeTime      ; Read in variable 'rhChangeTime'
    
    NCDF_VARGET, ncid,  47, relHumidityDD      ; Read in variable 'relHumidityDD'
    
    NCDF_VARGET, ncid,  48, relHumidityQCA      ; Read in variable 'relHumidityQCA'
    
    NCDF_VARGET, ncid,  49, relHumidityQCR      ; Read in variable 'relHumidityQCR'
    
    NCDF_VARGET, ncid,  50, relHumidityQCD      ; Read in variable 'relHumidityQCD'
    
    NCDF_VARGET, ncid,  51, stationPressure      ; Read in variable 'stationPressure'
    
    NCDF_VARGET, ncid,  52, stationPressChangeTime      ; Read in variable 'stationPressChangeTime'
    
    NCDF_VARGET, ncid,  53, stationPressureDD      ; Read in variable 'stationPressureDD'
    
    NCDF_VARGET, ncid,  54, stationPressureQCA      ; Read in variable 'stationPressureQCA'
    
    NCDF_VARGET, ncid,  55, stationPressureQCR      ; Read in variable 'stationPressureQCR'
    
    NCDF_VARGET, ncid,  56, stationPressureQCD      ; Read in variable 'stationPressureQCD'
    
    NCDF_VARGET, ncid,  57, stationPressureICA      ; Read in variable 'stationPressureICA'
    
    NCDF_VARGET, ncid,  58, stationPressureICR      ; Read in variable 'stationPressureICR'
    
    NCDF_VARGET, ncid,  59, seaLevelPressure      ; Read in variable 'seaLevelPressure'
    
    NCDF_VARGET, ncid,  60, seaLevelPressureDD      ; Read in variable 'seaLevelPressureDD'
    
    NCDF_VARGET, ncid,  61, seaLevelPressureQCA      ; Read in variable 'seaLevelPressureQCA'
    
    NCDF_VARGET, ncid,  62, seaLevelPressureQCR      ; Read in variable 'seaLevelPressureQCR'
    
    NCDF_VARGET, ncid,  63, seaLevelPressureQCD      ; Read in variable 'seaLevelPressureQCD'
    
    NCDF_VARGET, ncid,  64, seaLevelPressureICA      ; Read in variable 'seaLevelPressureICA'
    
    NCDF_VARGET, ncid,  65, seaLevelPressureICR      ; Read in variable 'seaLevelPressureICR'
    
    NCDF_VARGET, ncid,  66, pressChangeChar      ; Read in variable 'pressChangeChar'
    
    NCDF_VARGET, ncid,  67, pressChange3Hour      ; Read in variable 'pressChange3Hour'
    
    NCDF_VARGET, ncid,  68, pressChange3HourDD      ; Read in variable 'pressChange3HourDD'
    
    NCDF_VARGET, ncid,  69, pressChange3HourQCA      ; Read in variable 'pressChange3HourQCA'
    
    NCDF_VARGET, ncid,  70, pressChange3HourQCR      ; Read in variable 'pressChange3HourQCR'
    
    NCDF_VARGET, ncid,  71, pressChange3HourQCD      ; Read in variable 'pressChange3HourQCD'
    
    NCDF_VARGET, ncid,  72, pressChange3HourICA      ; Read in variable 'pressChange3HourICA'
    
    NCDF_VARGET, ncid,  73, pressChange3HourICR      ; Read in variable 'pressChange3HourICR'
    
    NCDF_VARGET, ncid,  74, altimeter      ; Read in variable 'altimeter'
    
    NCDF_VARGET, ncid,  75, altimeterDD      ; Read in variable 'altimeterDD'
    
    NCDF_VARGET, ncid,  76, altimeterQCA      ; Read in variable 'altimeterQCA'
    
    NCDF_VARGET, ncid,  77, altimeterQCR      ; Read in variable 'altimeterQCR'
    
    NCDF_VARGET, ncid,  78, altimeterQCD      ; Read in variable 'altimeterQCD'
    
    NCDF_VARGET, ncid,  79, windDir      ; Read in variable 'windDir'
    
    NCDF_VARGET, ncid,  80, windDirChangeTime      ; Read in variable 'windDirChangeTime'
    
    NCDF_VARGET, ncid,  81, windDirDD      ; Read in variable 'windDirDD'
    
    NCDF_VARGET, ncid,  82, windDirQCA      ; Read in variable 'windDirQCA'
    
    NCDF_VARGET, ncid,  83, windDirQCR      ; Read in variable 'windDirQCR'
    
    NCDF_VARGET, ncid,  84, windDirQCD      ; Read in variable 'windDirQCD'
    
    NCDF_VARGET, ncid,  85, windDirICA      ; Read in variable 'windDirICA'
    
    NCDF_VARGET, ncid,  86, windDirICR      ; Read in variable 'windDirICR'
    
    NCDF_VARGET, ncid,  87, windSpeed      ; Read in variable 'windSpeed'
    
    NCDF_VARGET, ncid,  88, windSpeedChangeTime      ; Read in variable 'windSpeedChangeTime'
    
    NCDF_VARGET, ncid,  89, windSpeedDD      ; Read in variable 'windSpeedDD'
    
    NCDF_VARGET, ncid,  90, windSpeedQCA      ; Read in variable 'windSpeedQCA'
    
    NCDF_VARGET, ncid,  91, windSpeedQCR      ; Read in variable 'windSpeedQCR'
    
    NCDF_VARGET, ncid,  92, windSpeedQCD      ; Read in variable 'windSpeedQCD'
    
    NCDF_VARGET, ncid,  93, windSpeedICA      ; Read in variable 'windSpeedICA'
    
    NCDF_VARGET, ncid,  94, windSpeedICR      ; Read in variable 'windSpeedICR'
    
    NCDF_VARGET, ncid,  95, windGust      ; Read in variable 'windGust'
    
    NCDF_VARGET, ncid,  96, windGustChangeTime      ; Read in variable 'windGustChangeTime'
    
    NCDF_VARGET, ncid,  97, windGustDD      ; Read in variable 'windGustDD'
    
    NCDF_VARGET, ncid,  98, windGustQCA      ; Read in variable 'windGustQCA'
    
    NCDF_VARGET, ncid,  99, windGustQCR      ; Read in variable 'windGustQCR'
    
    NCDF_VARGET, ncid,  100, windGustQCD      ; Read in variable 'windGustQCD'
    
    NCDF_VARGET, ncid,  101, windDirMax      ; Read in variable 'windDirMax'
    
    NCDF_VARGET, ncid,  102, windDirMaxDD      ; Read in variable 'windDirMaxDD'
    
    NCDF_VARGET, ncid,  103, windDirMaxQCA      ; Read in variable 'windDirMaxQCA'
    
    NCDF_VARGET, ncid,  104, windDirMaxQCR      ; Read in variable 'windDirMaxQCR'
    
    NCDF_VARGET, ncid,  105, windDirMaxQCD      ; Read in variable 'windDirMaxQCD'
    
    NCDF_VARGET, ncid,  106, visibility      ; Read in variable 'visibility'
    
    NCDF_VARGET, ncid,  107, visibilityDD      ; Read in variable 'visibilityDD'
    
    NCDF_VARGET, ncid,  108, visibilityQCA      ; Read in variable 'visibilityQCA'
    
    NCDF_VARGET, ncid,  109, visibilityQCR      ; Read in variable 'visibilityQCR'
    
    NCDF_VARGET, ncid,  110, visibilityQCD      ; Read in variable 'visibilityQCD'
    
    NCDF_VARGET, ncid,  111, visibilityICA      ; Read in variable 'visibilityICA'
    
    NCDF_VARGET, ncid,  112, visibilityICR      ; Read in variable 'visibilityICR'
    
    NCDF_VARGET, ncid,  113, rawPrecip      ; Read in variable 'rawPrecip'
    
    NCDF_VARGET, ncid,  114, precipAccum      ; Read in variable 'precipAccum'
    
    NCDF_VARGET, ncid,  115, precipAccumDD      ; Read in variable 'precipAccumDD'
    
    NCDF_VARGET, ncid,  116, precipAccumQCA      ; Read in variable 'precipAccumQCA'
    
    NCDF_VARGET, ncid,  117, precipAccumQCR      ; Read in variable 'precipAccumQCR'
    
    NCDF_VARGET, ncid,  118, precipAccumQCD      ; Read in variable 'precipAccumQCD'
    
    NCDF_VARGET, ncid,  119, precipAccumICA      ; Read in variable 'precipAccumICA'
    
    NCDF_VARGET, ncid,  120, precipAccumICR      ; Read in variable 'precipAccumICR'
    
    NCDF_VARGET, ncid,  121, precipRate      ; Read in variable 'precipRate'
    
    NCDF_VARGET, ncid,  122, precipRateDD      ; Read in variable 'precipRateDD'
    
    NCDF_VARGET, ncid,  123, precipRateQCA      ; Read in variable 'precipRateQCA'
    
    NCDF_VARGET, ncid,  124, precipRateQCR      ; Read in variable 'precipRateQCR'
    
    NCDF_VARGET, ncid,  125, precipRateQCD      ; Read in variable 'precipRateQCD'
    
    NCDF_VARGET, ncid,  126, precipType      ; Read in variable 'precipType'
    
    NCDF_VARGET, ncid,  127, precipIntensity      ; Read in variable 'precipIntensity'
    
    NCDF_VARGET, ncid,  128, timeSinceLastPcp      ; Read in variable 'timeSinceLastPcp'
    
    NCDF_VARGET, ncid,  129, solarRadiation      ; Read in variable 'solarRadiation'
    
    NCDF_VARGET, ncid,  130, solarRadChangeTime      ; Read in variable 'solarRadChangeTime'
    
    NCDF_VARGET, ncid,  131, seaSurfaceTemp      ; Read in variable 'seaSurfaceTemp'
    
    NCDF_VARGET, ncid,  132, seaSurfaceTempDD      ; Read in variable 'seaSurfaceTempDD'
    
    NCDF_VARGET, ncid,  133, seaSurfaceTempQCA      ; Read in variable 'seaSurfaceTempQCA'
    
    NCDF_VARGET, ncid,  134, seaSurfaceTempQCR      ; Read in variable 'seaSurfaceTempQCR'
    
    NCDF_VARGET, ncid,  135, seaSurfaceTempQCD      ; Read in variable 'seaSurfaceTempQCD'
    
    NCDF_VARGET, ncid,  136, seaSurfaceTempICA      ; Read in variable 'seaSurfaceTempICA'
    
    NCDF_VARGET, ncid,  137, seaSurfaceTempICR      ; Read in variable 'seaSurfaceTempICR'
    
    NCDF_VARGET, ncid,  138, rawMessage      ; Read in variable 'rawMessage'
    
    NCDF_VARGET, ncid,  139, totalColumnPWV      ; Read in variable 'totalColumnPWV'
    
    NCDF_VARGET, ncid,  140, totalSignalDelay      ; Read in variable 'totalSignalDelay'
    
    NCDF_VARGET, ncid,  141, drySignalDelay      ; Read in variable 'drySignalDelay'
    
    NCDF_VARGET, ncid,  142, wetSignalDelay      ; Read in variable 'wetSignalDelay'
    
    NCDF_VARGET, ncid,  143, meanWeightedTemperature      ; Read in variable 'meanWeightedTemperature'
    
    NCDF_VARGET, ncid,  144, formalError      ; Read in variable 'formalError'
    
    NCDF_VARGET, ncid,  145, capPi      ; Read in variable 'capPi'
    
    NCDF_VARGET, ncid,  146, roadTemperature1      ; Read in variable 'roadTemperature1'
    
    NCDF_VARGET, ncid,  147, roadTemperature2      ; Read in variable 'roadTemperature2'
    
    NCDF_VARGET, ncid,  148, roadTemperature3      ; Read in variable 'roadTemperature3'
    
    NCDF_VARGET, ncid,  149, roadTemperature4      ; Read in variable 'roadTemperature4'
    
    NCDF_VARGET, ncid,  150, roadLiquidFreezeTemp1      ; Read in variable 'roadLiquidFreezeTemp1'
    
    NCDF_VARGET, ncid,  151, roadLiquidFreezeTemp2      ; Read in variable 'roadLiquidFreezeTemp2'
    
    NCDF_VARGET, ncid,  152, roadLiquidFreezeTemp3      ; Read in variable 'roadLiquidFreezeTemp3'
    
    NCDF_VARGET, ncid,  153, roadLiquidFreezeTemp4      ; Read in variable 'roadLiquidFreezeTemp4'
    
    NCDF_VARGET, ncid,  154, roadLiquidChemFactor1      ; Read in variable 'roadLiquidChemFactor1'
    
    NCDF_VARGET, ncid,  155, roadLiquidChemFactor2      ; Read in variable 'roadLiquidChemFactor2'
    
    NCDF_VARGET, ncid,  156, roadLiquidChemFactor3      ; Read in variable 'roadLiquidChemFactor3'
    
    NCDF_VARGET, ncid,  157, roadLiquidChemFactor4      ; Read in variable 'roadLiquidChemFactor4'
    
    NCDF_VARGET, ncid,  158, roadLiquidChemPercent1      ; Read in variable 'roadLiquidChemPercent1'
    
    NCDF_VARGET, ncid,  159, roadLiquidChemPercent2      ; Read in variable 'roadLiquidChemPercent2'
    
    NCDF_VARGET, ncid,  160, roadLiquidChemPercent3      ; Read in variable 'roadLiquidChemPercent3'
    
    NCDF_VARGET, ncid,  161, roadLiquidChemPercent4      ; Read in variable 'roadLiquidChemPercent4'
    
    NCDF_VARGET, ncid,  162, roadLiquidIcePercent1      ; Read in variable 'roadLiquidIcePercent1'
    
    NCDF_VARGET, ncid,  163, roadLiquidIcePercent2      ; Read in variable 'roadLiquidIcePercent2'
    
    NCDF_VARGET, ncid,  164, roadLiquidIcePercent3      ; Read in variable 'roadLiquidIcePercent3'
    
    NCDF_VARGET, ncid,  165, roadLiquidIcePercent4      ; Read in variable 'roadLiquidIcePercent4'
    
    NCDF_VARGET, ncid,  166, roadLiquidDepth1      ; Read in variable 'roadLiquidDepth1'
    
    NCDF_VARGET, ncid,  167, roadLiquidDepth2      ; Read in variable 'roadLiquidDepth2'
    
    NCDF_VARGET, ncid,  168, roadLiquidDepth3      ; Read in variable 'roadLiquidDepth3'
    
    NCDF_VARGET, ncid,  169, roadLiquidDepth4      ; Read in variable 'roadLiquidDepth4'
    
    NCDF_VARGET, ncid,  170, roadState1      ; Read in variable 'roadState1'
    
    NCDF_VARGET, ncid,  171, roadState2      ; Read in variable 'roadState2'
    
    NCDF_VARGET, ncid,  172, roadState3      ; Read in variable 'roadState3'
    
    NCDF_VARGET, ncid,  173, roadState4      ; Read in variable 'roadState4'
    
    NCDF_VARGET, ncid,  174, roadSubsurfaceTemp1      ; Read in variable 'roadSubsurfaceTemp1'
    
    NCDF_VARGET, ncid,  175, roadSubsurfaceTemp2      ; Read in variable 'roadSubsurfaceTemp2'
    
    NCDF_VARGET, ncid,  176, roadSubsurfaceTemp3      ; Read in variable 'roadSubsurfaceTemp3'
    
    NCDF_VARGET, ncid,  177, roadSubsurfaceTemp4      ; Read in variable 'roadSubsurfaceTemp4'
    
    NCDF_VARGET, ncid,  178, fuelTemperature      ; Read in variable 'fuelTemperature'
    
    NCDF_VARGET, ncid,  179, fuelTempChangeTime      ; Read in variable 'fuelTempChangeTime'
    
    NCDF_VARGET, ncid,  180, fuelMoisture      ; Read in variable 'fuelMoisture'
    
    NCDF_VARGET, ncid,  181, fuelMoistChangeTime      ; Read in variable 'fuelMoistChangeTime'
    
    NCDF_VARGET, ncid,  182, soilTemperature      ; Read in variable 'soilTemperature'
    
    NCDF_VARGET, ncid,  183, soilTemperatureDD      ; Read in variable 'soilTemperatureDD'
    
    NCDF_VARGET, ncid,  184, soilTemperatureQCA      ; Read in variable 'soilTemperatureQCA'
    
    NCDF_VARGET, ncid,  185, soilTemperatureQCR      ; Read in variable 'soilTemperatureQCR'
    
    NCDF_VARGET, ncid,  186, soilTemperatureQCD      ; Read in variable 'soilTemperatureQCD'
    
    NCDF_VARGET, ncid,  187, soilMoisture      ; Read in variable 'soilMoisture'
    
    NCDF_VARGET, ncid,  188, soilMoisturePercent      ; Read in variable 'soilMoisturePercent'
    
    NCDF_VARGET, ncid,  189, soilMoisturePercentDD      ; Read in variable 'soilMoisturePercentDD'
    
    NCDF_VARGET, ncid,  190, soilMoisturePercentQCA      ; Read in variable 'soilMoisturePercentQCA'
    
    NCDF_VARGET, ncid,  191, soilMoisturePercentQCR      ; Read in variable 'soilMoisturePercentQCR'
    
    NCDF_VARGET, ncid,  192, soilMoisturePercentQCD      ; Read in variable 'soilMoisturePercentQCD'
    
    NCDF_VARGET, ncid,  193, waterLevel      ; Read in variable 'waterLevel'
    
    NCDF_VARGET, ncid,  194, mobileLat      ; Read in variable 'mobileLat'
    
    NCDF_VARGET, ncid,  195, mobileLon      ; Read in variable 'mobileLon'
    
    NCDF_VARGET, ncid,  196, mobileElev      ; Read in variable 'mobileElev'
    
    NCDF_VARGET, ncid,  197, presWx      ; Read in variable 'presWx'
    
    NCDF_VARGET, ncid,  198, numSkyCoverObs      ; Read in variable 'numSkyCoverObs'
    
    NCDF_VARGET, ncid,  199, skyCvr      ; Read in variable 'skyCvr'
    
    NCDF_VARGET, ncid,  200, skyCovLayerBase      ; Read in variable 'skyCovLayerBase'
    
    NCDF_VARGET, ncid,  201, test1      ; Read in variable 'test1'
    
    NCDF_VARGET, ncid,  202, numPST      ; Read in variable 'numPST'
    
    NCDF_VARGET, ncid,  203, code1PST      ; Read in variable 'code1PST'
    
    NCDF_VARGET, ncid,  204, code2PST      ; Read in variable 'code2PST'
    
    NCDF_VARGET, ncid,  205, code3PST      ; Read in variable 'code3PST'
    
    NCDF_VARGET, ncid,  206, code4PST      ; Read in variable 'code4PST'
    
    NCDF_VARGET, ncid,  207, namePST      ; Read in variable 'namePST'
    
    NCDF_VARGET, ncid,  208, typePST      ; Read in variable 'typePST'
    
    NCDF_CLOSE, ncid      ; Close the NetCDF file
    save, filename=savfile
  endelse
  if hh eq '18' then min_list = [56]
  if hh eq '19' then min_list = [40]
  if hh eq '20' then min_list = [34]
  windSpeed = windSpeed * 1.94384
  
  latmin = 33.4
  latmax = 37.4
  ;lonmin = -100.6
  lonmin = -99.7
  ;lonmax = -95.5
  lonmax = -96.4
  d = 0.3
  
  parms = ['PRES','TMPC','DWPC','RELH','DRCT','SKNT','MIXR']
  data = fltarr(n_elements(parms), n_elements(invTime))
  data[0,*] = stationPressure / 100.
  data[1,*] = temperature - !CONST.T0
  data[2,*] = dewpoint - !CONST.T0
  data[3,*] = relHumidity
  data[4,*] = windDir
  data[5,*] = windSpeed
  
  mixr = replicate(!VALUES.D_NAN, n_elements(Temperature))
  igood = where(ok(stationPressure) and ok(dewpoint), ng)
  if ng gt 0 then mixr[igood] = mixr_sat(dewpoint[igood], stationPressure[igood]/100.)
  igood = where(ok(temperature) and ok(relHumidity) and ok(stationPressure), ng)
  if ng gt 0 then mixr[igood] = relHumidity[igood]/100. * mixr_sat(temperature[igood], stationPressure[igood]/100.)
  if ng gt 0 then dewpoint[igood] = rh2tdew(temperature[igood], relHumidity[igood])
  
  yymmddhhmm = string(observationTime/3600L/24L + julday(1,1,1970), format='(c(CYI4.4,CMOI2.2,CDI2.2,"/",CHI2.2,CMI2.2))')
  
  jultime = julday(1,1,1970,0,0,0) + observationTime/24d/3600d; fill in hour argument to julday (12 is default)
  dt = 1d/24/3600 * 60
  
  
  for iminutes=0,n_elements(min_list)-1 do begin
    minutes = min_list[iminutes]
    ; assumes seconds past 1-1-1970 00:00:00Z
    mintime = julday(5,19,2013,hh,minutes,00) - dt
    maxtime = julday(5,19,2013,hh,minutes,00) + dt
    
    il = where(latitude ge latmin and latitude lt latmax $
      and longitude ge lonmin and longitude lt lonmax $
      and jultime ge mintime and jultime le maxtime $
      and (stationPressureQCA and stationPressureQCR eq 0) $
      and ok(stationPressure) $
      and (ok(dewpointDD) or ok(relHumidity)) $
      , n, /null)

    title=file_basename(infile) + $
      string(mintime,maxtime,format='("!C",C(CHI2.2,":",CMI2.2,":",CSI2.2),"-",c(CHI2.2,":",CMI2.2,":",CSI2.2)," UTC")')
    KTLX_lat=35.3331
    KTLX_lon=-97.2775
    m = map('Orthographic',center_longitude=KTLX_lon, center_latitude=0, limit=[latmin+d,lonmin+d,latmax-d,lonmax-d], $
      margin=0.15, title=title, layout=[1,1,1+iminutes])
    m1 = mapcontinents(/usa)
    grid=m.mapgrid
    grid.font_name='Arial'
    grid.font_size=14
    grid.thick=0.3
    grid.label_position=0
    grid.color = 'blue'
    grid.grid_longitude = 1
    grid.grid_latitude = 1
    
    if n gt 2 then begin
      ;mixr = text(longitude[il], latitude[il], string(mixr[il],format='(F4.1)'), /data, target=m)
      font_size=10
      m.window.refresh, /disable
      T = text(longitude[il], latitude[il], string(round(temperature[il]-!CONST.T0),format='(I2)'), /data, target=m, color='red',alignment=1,font_size=font_size)
      Td = text(longitude[il], latitude[il], string(round(dewpoint[il]-!CONST.T0),format='(I2)'), /data, color='green', target=m, alignment=1,vertical_alignment=1,font_size=font_size)
      ;wnd = text(longitude[il], latitude[il], string(round(windSpeed[il]), format='(I0)')+","+string(windDir[il],format='(I0)'), /data, target=m, vertical_alignment=0.5,font_size=font_size)
      for i=0,n-1 do begin
        pts = plot([longitude[il[i]]], [latitude[il[i]]], sym_object=windbarb(windSpeed[il[i]],windDir[il[i]]), overplot=m, sym_size=0.75, sym_color='grey')
      endfor
      ;p = contour(mixr[il], longitude[il], latitude[il], overplot=m,grid_units='deg', $
      ;  c_label_show=1, c_value=[8,12], c_thick=2, color='dark blue', font_size=16)
    endif
    m.window.refresh
    m.window.save, infile + '_' + hh + ":" + string(minutes, format='(I2.2)') + '.eps'

  endfor
  openw, lun, './t.fil', /get_lun
  printf, lun, 'PARM = ', strjoin(parms, ';')
  printf, lun
  printf, lun, 'STN', 'YYMMDD/HHMM', parms, format='(A7,A15,99A10)'
  for i=0,n-1 do printf, lun, string(stationID[*,il[i]]), yymmddhhmm[il[i]], data[*,il[i]], mixr[il[i]], format='(A7,A15,'+strtrim(n_elements(parms),2)+'1F10.2)'
  close, lun
  free_lun, lun
end