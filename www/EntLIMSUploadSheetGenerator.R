library(data.table)

debugList1 = list()
debugList2 = list()
debugList3 = list()
debugList4 = list()

target_mg_mL_normal <- list(1, 0.25, 0.05, 0.01)
target_mg_mL_hemip <- list(2, 0.5, 0.1, 0.02)
target_mg_mL_lygus <- list(4, 1, 0.2, 0.04)
#This needs to be updated to reflect the new concentrations

uploadSheetHeader <- list(
  "Plate Number",
  "Well",
  "Sample Name",
  "Amount",
  "Unit",
  "Conc",
  "C_Unit",
  "Sample Description",
  "Created By",
  "Created On",
  "Method Code",
  "Sample Treatment",
  "Solvent Description",
  "Enterprise ID",
  "Enterprise System",
  "Aliquot ID"
)

create_upload_sheet = function(plates, proteinNames, numNormalPlates, numHemipPlates, numLygusPlates)
{
 
  worklistColumnNames <- names(plates[[1]])

  blankString <- c("","")
  emptyVol <- c(0,0)
  wellName <- c(paste("A", "5", sep = ""),paste("A", "6", sep = ""))
  
  controlWells <- data.frame(
                    sourcePlate = blankString,
                    sourceWell = blankString,
                    proteinVol = emptyVol,
                    bufferVol = emptyVol,
                    destinationPlate = blankString,
                    wellName <- wellName,
                    sourcePlateID = blankString,
                    destinationPlateID = blankString,
                    libID = blankString,
                    constructName = c("UTC", "Control"),
                    srcID = blankString,
                    groupID = blankString,
                    sourceType = blankString,
                    rep = blankString,
                    conc = blankString,
                    template = blankString)

  names(controlWells) <- worklistColumnNames

  numPlates <- list(numNormalPlates, numHemipPlates, numLygusPlates)

  for(x in 1:3) #1 = normal plates, 2 = hemip, 3 = lygus
  {

    for(i in 1:numPlates[[x]])
    {
      plateNum <- paste("Plate", i, sep = "")
      controlWells[1, 'Destination Plate'] <- plateNum
      controlWells[2,'Destination Plate'] <- plateNum
      plates[[x]] <- as.data.frame(rbind(plates[[x]], controlWells))
    }
  } 
  
  limsUploadSheet <- list()
  for (x in 1:3)
  {
    numberOfExperiments <- nrow(plates[[x]])
    numberOfTreatedSamples <- numberOfExperiments - (2 * numPlates[[x]])
    
    currentDate <- as.Date(Sys.Date(), format = "%m/%d/%Y")
    if(x == 1)
      {volume = 1.2
      target_mg_mL_for_insect = target_mg_mL_normal
    }else if (x == 2)
      { volume = 0.6
      target_mg_mL_for_insect = target_mg_mL_hemip
    
    }else 
      {volume = 0.3
      target_mg_mL_for_insect = target_mg_mL_lygus
    }
    
    amount <- rep(volume, numberOfExperiments)
    units <- rep("mL", numberOfExperiments)
    conc <- rep(target_mg_mL_for_insect, length = numberOfTreatedSamples)
    controlConc <- rep(0, numberOfExperiments-numberOfTreatedSamples)
    conc <- c(conc, controlConc)
    cUnit <-rep("mg/mL", numberOfExperiments)
    description <- rep("", numberOfExperiments)
    createdBy <- rep("", numberOfExperiments)
    createdOn <- rep(currentDate, numberOfExperiments)
    methodCode <- rep(0, numberOfExperiments)
    sampleTreatment <- rep("", numberOfExperiments)
    solvDescription <- rep("", numberOfExperiments)
    enterpriseID <- rep("", numberOfExperiments)
    enterpriseSys <- rep("", numberOfExperiments)
    aliquotID <- rep("", numberOfExperiments)
    
    tempdF <- data.frame(
      a = list(plates[[x]]['Destination Plate']),
      b = list(plates[[x]]['Destination Well']),
      c = list(plates[[x]]['Observed_Construct_Name']),
      d = amount,
      e = units,
      f = unlist(conc),
      g = cUnit,
      h = description,
      i = createdBy,
      j = createdOn, 
      k = methodCode,
      l = sampleTreatment,
      m = solvDescription,
      n = enterpriseID,
      o = enterpriseSys,
      p = aliquotID
      
    )
    names(tempdF) <- uploadSheetHeader
    limsUploadSheet[[x]] <- tempdF  
  }
  return(limsUploadSheet)
}