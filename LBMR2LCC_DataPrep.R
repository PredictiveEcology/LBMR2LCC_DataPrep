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
  reqdPkgs = list("caret", "dplyr", "LandR", "magrittr", "raster", "rlang", "tibble", "xgboost"),
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
      objectName = "BCR6_NWT_RT", 
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1NIjFbkckG3sewkTqPGaBGQDQLboPQ0wc",
      desc = "Template raster of BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "DEM_BCR6_NWT", 
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/file/d/1SKnXVqUD10_VdemQaPaz9MrWiNZzK7VY/view",
      desc = "Elevation data from AdaptWest within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "kNN_AgeMap_BCR6_NWT", 
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Stand age from the maps of Canada's forest attributes (2001) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "kNN_Biomass_BCR6_NWT", 
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Biomass per species from the maps of Canada's forest attributes (2001) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "kNN_SpeciesCoverPc_BCR6_NWT", 
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Species cover from the maps of Canada's forest attributes (2001) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "LCC05_BCR6_NWT", 
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN",
      desc = "Land Cover Map of Canada 2005 (LCC05) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "VRUG_BCR6_NWT", 
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Ruggedness within BCR6 as contained in the Northwest Territories."
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

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LBMR2LCC_DataPrep", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LBMR2LCC_DataPrep", "save")
      
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LBMR2LCC_DataPrep", "MapLBMR2LCC")
    },
    MapLBMR2LCC = {
      sim <- MapLBMR2LCC(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "LBMR2LCC_DataPrep", "MapLBMR2LCC")
    }
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
  
  train <- function()
  {
    notNA <- !is.na(sim[["BCR6_NWT_RT"]][])
    
    sp2keep <- c("Abie_Bal", "Betu_Pap", "Lari_Lar", "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Pinu_Con", "Popu_Bal", "Popu_Tre")
    
    cart_data <- bind_cols(
      setNames(
        as_tibble(
          sim[["kNN_SpeciesCoverPc_BCR6_NWT"]][notNA] / 100 * sim[["kNN_Biomass_BCR6_NWT"]][notNA]
        ),
        sp2keep
      ),
      tibble(
        lcc = sim[["LCC05_BCR6_NWT"]][notNA],
        age = sim[["kNN_AgeMap_BCR6_NWT"]][notNA],
        elev = sim[["DEM_BCR6_NWT"]][notNA],
        vrug = sim[["VRUG_BCR6_NWT"]][notNA]
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
    
    rm(train_set)
    
    param <- list(
      colsample_bytree = 1,
      eta = .1, 
      eval_metric = "mlogloss",
      gamma = 0, 
      max_depth = 5,
      min_child_weight = 1,
      # nthread = 32, # Use all cores by default
      num_class = nlevels(dplyr::select(train_set, lcc)[["lcc"]]),
      objective = "multi:softmax",
      silent = 1,
      subsample = 0.7
    )
    
    xgb.train(
      params = param, 
      data = dtrain,
      nrounds = 1000
    )  
  }
  
  mod[["trainedClassifier"]] <- Cache(train)

  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

MapLBMR2LCC <- function(sim)
{
  notNA <- !is.na(sim[["BCR6_NWT_RT"]][])
  
  sp2keep <- c(
    "Abie_Bal", "Betu_Pap", "Lari_Lar",
    "Pice_Gla", "Pice_Mar", "Pinu_Ban", 
    "Pinu_Con", "Popu_Bal", "Popu_Tre"
  )
  
  spTable <- data.table(
    speciesCode = sp2keep
  )

  age <- rasterizeReduced(sim$cohortData[, .(biomass = max(age)), by = "pixelGroup"], sim$pixelGroupMap, "age", "pixelGroup")
  
  newdata <- bind_cols(
    tibble(
      age = age[notNA],
      elev = sim[["DEM_BCR6_NWT"]][notNA],
      vrug = sim[["VRUG_BCR6_NWT"]][notNA]
    ),
    setNames(
      as_tibble(
        stack(
          lapply(
            sp2keep,
            function(sp, dt)
            {
              rasterizeReduced(dt, sim$pixelGroupMap, sp, "pixelGroup")
            }
          ),
          dt = dcast(
                                 # Sum biomass by pixelGroup and species code          # Biomass data for all sp  # 0: sp is absent  # g/m2 to t/ha
            sim[["cohortData"]][, .(B = sum(B)), by = c("pixelGroup", "speciesCode")][spTable, on = "speciesCode"][is.na(B), B := 0][, B := B * 10],
            B ~ speciesCode
          )
        )[notNA]
      ),
      nm = sp2keep
    )
  )
    
  LCC <- sim[["BCR6_NWT_RT"]]
  LCC[px_id][age < 15] <- 34
  
  pred <- predict(mod[["trainedClassifier"]], newdata = newdata)  
  lccCode <- c(7, 16, 13, 20, 1, 2, 19)
  
  for (i in 0:6)
  {
    LCC[px_id][pred == i] <- lccCode[i]
  }
  
  sim[["LCC"]] <- setNames(
    raster::stack(
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
  
  if (!suppliedElsewhere(object = "BCR6_NWT_RT", sim = sim))
  {
    sim[["BCR6_NWT_RT"]] <- Cache(
      prepInputs,
      targetFile = "BCR6_NWT-2.tif",
      url = "https://drive.google.com/open?id=1NIjFbkckG3sewkTqPGaBGQDQLboPQ0wc",
      destinationPath = tempdir()
    )
  }
  
  if (!suppliedElsewhere(object = "kNN_Biomass_BCR6_NWT", sim = sim))
  {
    sim[["kNN_Biomass_BCR6_NWT"]] <- Cache(
      prepInputs,
      "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar",
      targetFile = "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif",
      rasterToMatch = sim[["BCR6_NWT_RT"]],
      maskWithRTM = TRUE,
      filename2 = "kNN_Biomass_BCR6_NWT.tif",
      destinationPath = tempdir()
    )
  }

  if (!suppliedElsewhere(object = "DEM_BCR6_NWT", sim = sim))
  {
    sim[["DEM_BCR6_NWT"]] <- Cache(
      prepInputs,
      targetFile = "nadem100laz_BCR6_NWT.tif",
      url = "https://drive.google.com/file/d/1SKnXVqUD10_VdemQaPaz9MrWiNZzK7VY/view",
      destinationPath = tempdir()
    )
  }
  
  if (!suppliedElsewhere(object = "kNN_AgeMap_BCR6_NWT", sim = sim))
  {
    sim[["kNN_AgeMap_BCR6_NWT"]] <- Cache(
      prepInputs,
      "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
      targetFile = "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif",
      rasterToMatch = sim[["BCR6_NWT_RT"]],
      maskWithRTM = TRUE,
      filename2 = "kNN_AgeMap_BCR6_NWT.tif",
      destinationPath = tempdir()
    )
  }
  
  if (!suppliedElsewhere(object = "kNN_SpeciesCoverPc_BCR6_NWT", sim = sim))
  {
    sp2keep <- c("Abie_Bal", "Betu_Pap", "Lari_Lar", "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Pinu_Con", "Popu_Bal", "Popu_Tre")
    
    sim[["kNN_SpeciesCoverPc_BCR6_NWT"]] <- Cache(
      postProcess,
      prepSpeciesLayers_KNN(
        destinationPath = inputPath(sim),
        outputPath = outputPath(sim),
        sppEquiv = LandR::sppEquivalencies_CA[KNN %in% sp2keep],
        sppEquivCol = "LandR",
        rasterToMatch = sim[["BCR6_NWT_RT"]],
        studyArea = NULL
      ),
      rasterToMatch = sim[["BCR6_NWT_RT"]],
      maskWithRTM = TRUE,
      method = "bilinear",
      datatype = "INT2U",
      filename2 = "kNN_SpeciesCover.tif"
    )
  }
  
  if (!suppliedElsewhere(object = "LCC05_BCR6_NWT", sim = sim))
  {
    sim[["LCC05_BCR6_NWT"]] <- Cache(
      prepInputs, 
      targetFile = "LCC2005_V1_4a_BCR6_NWT.tif",
      url = "https://drive.google.com/file/d/1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN/view", 
      destinationPath = tempdir()
    )
  }
  
  if (!suppliedElsewhere(object = "VRUG_BCR6_NWT", sim = sim))
  {
    sim[["VRUG_BCR6_NWT"]] <- Cache(
      postProcess,
      x = prepInputs(
        targetFile = "vrug_bcr6.tif",
        url = "https://drive.google.com/open?id=15Kcs83EyHnc-7vVbrg48srFrlD91WDtp", 
        destinationPath = tempdir()
      ),
      rasterToMatch = sim[["BCR6_NWT_RT"]],
      method = "bilinear",
      maskWithRTM = TRUE,
      filename2 = file.path(tempdir(), "VRUG_BCR6_NWT.tif"),
      datatype = "FLT4S"
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
