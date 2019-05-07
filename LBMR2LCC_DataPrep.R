# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "LBMR2LCC_DataPrep",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5", LBMR2LCC_DataPrep = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LBMR2LCC_DataPrep.Rmd"),
  reqdPkgs = list("caret", "data.table", "dplyr", "LandR", "magrittr", "raster", "rlang", "tibble", "xgboost", "parallel"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(
      objectName = "DEM", 
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1WHZnpWokgjraR7tGz0mIJEr9klLGUzB0",
      desc = "Elevation data from AdaptWest."
    ),
    expectsInput(
      objectName = "kNN_AgeMap", 
      objectClass = "RasterLayer",
      sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
      desc = "Stand age from the maps of Canada's forest attributes (2001)."
    ),
    expectsInput(
      objectName = "kNN_Biomass", 
      objectClass = "RasterLayer",
      sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar",
      desc = "Biomass per species from the maps of Canada's forest attributes (2001)."
    ),
    expectsInput(
      objectName = "kNN_SpeciesCoverPc", 
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Species cover from the maps of Canada's forest attributes (2001) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "vegMap",
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1ziUPnFZMamA5Yi6Hhex9aZKerXLpVxvz",
      desc = "Land Cover Map of Canada 2005 (LCC05)."
    ),
    expectsInput(
      objectName = "rasterToMatch", 
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1NIjFbkckG3sewkTqPGaBGQDQLboPQ0wc",
      desc = "a template raster describing the studyArea"
    ),
    expectsInput(
      objectName = "studyArea", 
      objectClass = "SpatialPolygonsDataFrame",
      sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
      desc = "a template polygon describing the studyArea"
    ),
    expectsInput(
      objectName = "VRUG", 
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=15Kcs83EyHnc-7vVbrg48srFrlD91WDtp",
      desc = "Ruggedness within BCR6."
    )
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(
      objectName = "LCC",
      objectClass = "RasterStack",
      desc = "Land cover class as predicted by the classifier within BCR6 as contained in the Northwest Territories."
    )
  )
))

## event types
#   - type `init` is required for initialization

doEvent.LBMR2LCC_DataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)
      sim <- MapLBMR2LCC(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LBMR2LCC_DataPrep", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LBMR2LCC_DataPrep", "save")
      
      sim <- scheduleEvent(sim, P(sim)$.runInitialTime + 1, "LBMR2LCC_DataPrep", "MapLBMR2LCC")
    },
    MapLBMR2LCC = {
      sim <- MapLBMR2LCC(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "LBMR2LCC_DataPrep", "MapLBMR2LCC")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "LBMR2LCC_DataPrep", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "LBMR2LCC_DataPrep", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "LBMR2LCC_DataPrep", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "LBMR2LCC_DataPrep", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  traini <- function()
  {
    notNA <- !is.na(sim[["rasterToMatch"]][])
    
    # sp2keep <- c("Abie_Bal", "Betu_Pap", "Lari_Lar", "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Pinu_Con", "Popu_Bal", "Popu_Tre")
      sp2keep <- as.character(unique(sim$cohortData$speciesCode))
    cart_data <- bind_cols(
      setNames(
        as_tibble(
          sim[["kNN_SpeciesCoverPc"]][notNA] / 100 * sim[["kNN_Biomass"]][notNA]
        ),
        sp2keep
      ),
      tibble(
        lcc = sim[["vegMap"]][notNA],
        age = sim[["kNN_AgeMap"]][notNA],
        elev = sim[["DEM"]][notNA],
        vrug = sim[["VRUG"]][notNA]
      )
    ) %>%
      # Remove pixels with NA
      dplyr::filter_all(all_vars(!is.na(.))) %>%
      
      # Filter out pixels with classes that do not burn or disturbed recently
      dplyr::filter(!lcc %in% c(33:39)) %>%
      
      # Reclassify LCC to our classes
      mutate(
        lcc = as.factor(
          case_when(
            lcc == 7 ~ 0, # conifer_medium_density
            lcc %in% c(16:18, 21:32) ~ 1, # herbs_shurb
            lcc == 13 ~ 2, # mixedwood_conifer_dom
            lcc == 20 ~ 3, # open_conifer
            lcc %in% c(1, 6, 8:10) ~ 4, # other_conifer
            lcc %in% c(2:5, 11:12, 14:15) ~ 5, # other_treed
            lcc == 19 ~ 6 # wetlands
          )
        )
      )
    
    set.seed(1)

    
    sample_frac2 <- function(tbl, size = 1, max_size = 1e5, replace = FALSE, weight = NULL)
    {
      size <- enquo(size)
      max_size <- enquo(max_size)
      weight <- enquo(weight)
      
      dplyr::slice(
        tbl, 
        sample.int(
          n(), 
          min(!!max_size, round(n() * dplyr:::check_frac(!!size, replace = replace))),
          replace = replace, 
          prob = !!weight
        )
      )
    }
    
    train_set <- cart_data %>% group_by(lcc) %>% sample_frac2(.7) %>% ungroup
    
    rm(cart_data)
    
    dtrain <- xgb.DMatrix(as.matrix(dplyr::select(train_set, -lcc)), label = as.matrix(dplyr::select(train_set, lcc)))
    
    nlevels <- nlevels(dplyr::select(train_set, lcc)[["lcc"]])
    
    rm(train_set)
    
    param <- list(
      colsample_bytree = 1,
      eta = .1, 
      eval_metric = "mlogloss",
      gamma = 0, 
      max_depth = 5,
      min_child_weight = 1,
      num_class = nlevels,
      objective = "multi:softmax",
      silent = 1,
      subsample = 0.7,
      nthread = parallel::detectCores()/2 # 
    )
    
    xgb.train(params = param, 
      data = dtrain,
      nrounds = 1000
    )  
  }

  mod[["trainedClassifier"]] <- Cache(traini, userTags = c("function:train",
                                                          "objectName:trainerClassifier"), cacheId = "afee1eb014fac309") # Work around to guarantee it will load correctly
 
   # ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

MapLBMR2LCC <- function(sim)
{

  notNA <- !is.na(sim[["rasterToMatch"]][])
  
  # sp2keep <- c(
  #   "Abie_Bal", "Betu_Pap", "Lari_Lar",
  #   "Pice_Gla", "Pice_Mar", "Pinu_Ban", 
  #   "Pinu_Con", "Popu_Bal", "Popu_Tre"
  # )
  sp2keep <- as.character(unique(sim$cohortData$speciesCode))
  
  spTable <- data.table(
    speciesCode = sp2keep
  )
  age <- rasterizeReduced(
    # sim$cohortData[, .(biomass = max(age)), by = "pixelGroup"],
      sim$cohortData[, .(age = max(age)), by = "pixelGroup"], # Tried to fix it... not sure it is correct ~TM
    sim$pixelGroupMap,
    "age",
    "pixelGroup"
  )

  # dt <- dcast(
  #   # Sum biomass by pixelGroup and species code          # Biomass data for all sp  # 0: sp is absent  # g/m2 to t/ha
  #   sim[["cohortData"]][, .(B = sum(B)), by = c("pixelGroup", "speciesCode")][spTable, on = "speciesCode"][is.na(B), B := 0][, B := B * 10],
  #   B + pixelGroup ~ speciesCode
  # )
  
  dt <- dcast(
    # Sum biomass by pixelGroup and species code          # Biomass data for all sp  # 0: sp is absent  # g/m2 to t/ha
    sim[["cohortData"]][, .(B = sum(B)), by = c("pixelGroup", "speciesCode")][spTable, on = "speciesCode"][is.na(B), B := 0][, B := B * 10],
    pixelGroup ~ speciesCode
  )
  
    newdata <- bind_cols(tibble(
      age = age[notNA],
      elev = sim[["DEM"]][notNA],
      vrug = sim[["VRUG"]][notNA]
    ),
    setNames(as_tibble(stack(
      lapply(
        X = sp2keep,
        FUN = function(sp) {
          rasterizeReduced(
            reduced = dt,
            fullRaster = sim$pixelGroupMap,
            newRasterCols = sp,
            mapcode = "pixelGroup"
          )
        }
      )
    )[notNA]), nm = sp2keep))

 # newdata <- bind_cols(
 #   tibble(
 #     age = age[notNA],
 #     elev = sim[["DEM"]][notNA],
 #     vrug = sim[["VRUG"]][notNA]
 #   ),
 #   setNames(
 #     as_tibble(
 #       stack(
 #         lapply(
 #           sp2keep,
 #           function(sp, dt)
 #           {
 #             rasterizeReduced(dt, sim$pixelGroupMap, sp, "pixelGroup")
 #           }
 #         ),
 #         dt = dcast(
 #           # Sum biomass by pixelGroup and species code          # Biomass data for all sp  # 0: sp is absent  # g/m2 to t/ha
 #           sim[["cohortData"]][, .(B = sum(B)), by = c("pixelGroup", "speciesCode")][spTable, on = "speciesCode"][is.na(B), B := 0][, B := B * 10],
 #           B ~ speciesCode
 #         )
 #       )[notNA]
 #     ),
 #     nm = sp2keep
 #   )
 # )

  LCC <- sim[["LCC2005"]]
  # LCC[px_id][age < 15] <- 34 # LCC's recently burned class => fireSense's disturbed class
  LCC[notNA][age[notNA] < 21] <- 34 # There are no pixels with age < 20 in the age map
  

  # pred <- predict(mod[["trainedClassifier"]], newdata = newdata) # Doesn't work ~TM
# Error in xgb.DMatrix(newdata, missing = missing) : 
  # xgb.DMatrix does not support construction from list

  # Workaround due to hardcoding species in a previous module...
  newdataNames <- names(newdata)
  modelNames <- mod[["trainedClassifier"]][["feature_names"]]
  df <- setdiff(modelNames, newdataNames)
  newdata[[df]] <- NA
  toMatch <- match(mod[["trainedClassifier"]]$feature_names, names(newdata))
  newdata <- as.matrix(newdata)
  newdata <- newdata[, toMatch]

  # OBS.: Reading from cacheId here doesn't work!!!
  # pred <- Cache(predict, mod[["trainedClassifier"]], newdata = as.matrix(newdata), 
  #               userTags = c("trial:Trial2", 
  #                            "module:LBMR2LCC_DataPrep", 
  #                            "function:predict"), cacheId = "e663a93b428e8fe1") # Remove cache id for future runs / other areas (here is just to speed up the process)
  
  pred <- Cache(mean, cacheId = "e663a93b428e8fe1")
  
  # pred <- Cache(predict, mod[["trainedClassifier"]], newdata = as.matrix(newdata),
  #               userTags = c("objectName:trainedClassifier", 
  #                            "module:LBMR2LCC_DataPrep", 
  #                            "mainFun:prediction"), cacheId = "41b44e42338d53e0")
  #                           
    
  lccCode <- c(7, 16, 13, 20, 1, 2, 19) # ???? What is this and where does it come from?!

  for (i in 0:6)
  {
    # LCC[px_id][pred == i] <- lccCode[i]
    LCC[notNA][pred == i] <- lccCode[i + 1] # lcccCode[i]
  }
  
  sim[["LCC"]] <- setNames(
    Cache(
      raster::stack,
      lapply(
        c(1:32, 34:35),
        function(x) LCC == x
      )
    ),
    nm = paste0("cl", c(1:32, 34:35))
  )
  
  invisible(sim)
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim))
  {
    sim[["rasterToMatch"]] <- Cache(
      targetFile = "BCR6_NWT-2.tif",
      prepInputs, 
      url = "https://drive.google.com/open?id=1NIjFbkckG3sewkTqPGaBGQDQLboPQ0wc",
      destinationPath = tempdir()
    )
  }
  
  
  if (!suppliedElsewhere(object = "studyArea", sim = sim))
  {
    sim[["studyArea"]] <- Cache(
      prepInputs, 
      url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
      destinationPath = tempdir()
    )
  }
  
  if (!suppliedElsewhere(object = "DEM", sim = sim))
  {
    sim[["DEM"]] <- Cache(
      prepInputs,
      targetFile = "nadem100laz_BCR6_NWT.tif",
      url = "https://drive.google.com/open?id=1SKnXVqUD10_VdemQaPaz9MrWiNZzK7VY",
      destinationPath = dataPath(sim),
      rasterToMatch = sim[["rasterToMatch"]],
      maskWithRTM = TRUE,
      studyArea = sim[["studyArea"]],
      filename2 = NULL,
      overwrite = TRUE,
      method = "bilinear",
      omitArgs = c("destinationPath", "maskWithRTM")
    )
  }
  
  if (!suppliedElsewhere(object = "kNN_Biomass", sim = sim))
  {
    sim[["kNN_Biomass"]] <- Cache(
      prepInputs,
      "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar",
      targetFile = "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif",
      destinationPath = dataPath(sim),
      rasterToMatch = sim[["rasterToMatch"]],
      maskWithRTM = TRUE,
      studyArea = sim[["studyArea"]],
      filename2 = NULL,
      overwrite = TRUE,
      method = "bilinear",
      omitArgs = c("destinationPath", "maskWithRTM")
    )
  }
  
  if (!suppliedElsewhere(object = "kNN_AgeMap", sim = sim))
  {
    sim[["kNN_AgeMap"]] <- Cache(
      prepInputs,
      "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
      targetFile = "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif",
      destinationPath = dataPath(sim),
      rasterToMatch = sim[["rasterToMatch"]],
      maskWithRTM = TRUE,
      studyArea = sim[["studyArea"]],
      filename2 = NULL,
      overwrite = TRUE,
      method = "bilinear",
      omitArgs = c("destinationPath", "maskWithRTM")
    )
  }
  
  if (!suppliedElsewhere(object = "kNN_SpeciesCoverPc", sim = sim))
  {
    sp2keep <- c("Abie_Bal", "Betu_Pap", "Lari_Lar", "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Pinu_Con", "Popu_Bal", "Popu_Tre")
    
    sim[["kNN_SpeciesCoverPc"]] <- Cache(
      postProcess,
      prepSpeciesLayers_KNN(
        destinationPath = dataPath(sim),
        outputPath = outputPath(sim),
        sppEquiv = LandR::sppEquivalencies_CA[KNN %in% sp2keep],
        sppEquivCol = "LandR",
        rasterToMatch = sim[["rasterToMatch"]],
        studyArea = sim[["studyArea"]]),
      method = "bilinear",
      datatype = "INT2U",
      destinationPath = tempdir(),
      rasterToMatch = sim[["rasterToMatch"]],
      maskWithRTM = TRUE,
      studyArea = sim[["studyArea"]],
      filename2 = NULL,
      overwrite = TRUE
    )
  }
  
  if (!suppliedElsewhere(object = "vegMap", sim = sim))
  {
    sim[["vegMap"]] <- Cache(
      prepInputs, 
      targetFile = "LCC2005_V1_4a.tif",
      url = "https://drive.google.com/open?id=1ziUPnFZMamA5Yi6Hhex9aZKerXLpVxvz", 
      destinationPath = dataPath(sim),
      rasterToMatch = sim[["rasterToMatch"]],
      maskWithRTM = TRUE,
      studyArea = sim[["studyArea"]],
      filename2 = NULL,
      overwrite = TRUE,
      method = "ngb",
      omitArgs = c("destinationPath", "maskWithRTM")
    )
  }
  
  if (!suppliedElsewhere(object = "VRUG", sim = sim))
  {
    sim[["VRUG"]] <- Cache(
      prepInputs,
      targetFile = "vrug_bcr6.tif",
      url = "https://drive.google.com/open?id=15Kcs83EyHnc-7vVbrg48srFrlD91WDtp", 
      destinationPath = dataPath(sim),
      rasterToMatch = sim[["rasterToMatch"]],
      maskWithRTM = TRUE,
      studyArea = sim[["studyArea"]],
      filename2 = NULL,
      overwrite = TRUE,
      datatype = "FLT4S",
      method = "bilinear",
      omitArgs = c("destinationPath", "maskWithRTM")
    )
  }
  
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
