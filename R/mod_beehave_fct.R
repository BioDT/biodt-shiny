
#' Title
#'
#' @return list, list containing used constant values
init_const <- function(){
  const <- list(
    ABANDON_POLLEN_PATCH_PROB_PER_S = 0.00002, # probability of experienced (but not necessarily active) pollen foragers to abandon their current pollen patch (1/s)
    ENERGY_HONEY_per_g = 12.78,  # energy content (in kJ/g) of 1g honey (also used for fondant or syrup)
    MAX_HONEY_STORE_kg = 50,  # maximum amount honey that can be stored (defined as weight)
    MAX_BROODCELLS = 2000099,  # maximum brood space (i.e. not limiting; alternative value: e.g. 20000) in cells
    MAX_EGG_LAYING = 1600,  # maximum egg laying rate per day (eggs / day)
    QueenAgeing = FALSE,  # if true: egg laying rate decreases with queen age (following BEEPOP) and the queen is replaced every year
    MAX_km_PER_DAY = 7299,  # maximum total distance a forager can fly on a single day (default value not limiting), if limiting: e.g. 72 km (= 2 * 36 km: Lindauer 1948, referring to Gontarski)
    AllowReinfestation = FALSE,
    POLLEN_STORE_INIT = 100, # initial pollen store (g)
    FORAGER_NURSING_CONTRIBUTION = 0.2,  # contribution of foragers on brood care (i.e. foragers are 80% less efficient than inhive bees)
    SQUADRON_SIZE = 100, # number of foragers in the super-individuals "foragerSquadron" (bees) (in original code 100)
    CRITICAL_COLONY_SIZE_WINTER = 4000, # threshold colony size for winter survival on julian day 365 (Martin (2001): 4000 adult workers during winter (from Free & Spencer-Booth 1958, Harbo 1983)) (bees)
    MORTALITY_EGGS = 0.03, # daily mortality rate of worker eggs (Schmickl & Crailsheim (2007): 0.03) (mortality/d)
    HATCHING_AGE = 3, # age at which worker larvae hatch from eggs (Winston (1987) p.50) (d)
    MORTALITY_DRONE_EGGS = 0.064, # daily mortality rate of drone eggs (Fukuda, Ohtani (1977): 100 eggs, 82 unsealed brood, 60 sealed brood and 56 adults) (mortality/d)
    DRONE_HATCHING_AGE = 3, # age at which drone larvae hatch from eggs (Jay (1963), Hrassnig & Crailsheim (2005)) (d)
    EMERGING_AGE = 21, # age at which adult workers emerge from pupae (Winston (1987) p.50) (d)
    MAX_BROOD_NURSE_RATIO = 3, # maximum amount of brood, nurse bees can care for, with nurse bees = totalIHbees + totalForagers * FORAGER_NURSING_CONTRIBUTION (Free & Racey (1968): 3; Becher et al. (2010): 2.65)
    DRONE_EGGS_PROPORTION = 0.04, # proportion of drone eggs (Wilkinson & Smith (2002): 0.04 (from Allen 1963, 1965))
    SEASON_START = 1, # first day of foraging season, January 1, (julian day)
    SEASON_STOP = 365, # last day of foraging season (December 31) (julian day)
    DRONE_PUPATION_AGE = 10, # age of pupation for drone (Winston (1987)) (d)
    DRONE_EMERGING_AGE = 24, # age at which adult drones emerge from pupae (Winston (1987)) (d)
    PRE_SWARMING_PERIOD = 3, # defines period during which colony prepares for swarming (Schmickl & Crailsheim (2007): 3d, Winston (1987) p. 184: "until the week before swarming") (d)
    N_INITIAL_MITES_HEALTHY = 0, # initial number of healthy mites (mites)
    N_INITIAL_MITES_INFECTED = 0, # initial number of infected mites (mites)
    POST_SWARMING_PERIOD = 0, # defines period after swarming until egglaying etc. normalises (d)
    AFF_BASE = 21, # default value of Aff (deGrandi-Hoffman et al. (1989): 21d) (d)
    CROPVOLUME = 50, # volume of a forager's crop, is completely filled at flower patch (Winston (1987), Nuñez (1966, 1970), Schmid-Hempel et al. (1985)) (μl)
    DRONE_EGGLAYING_START = 115, # beginning of drone production, (April 25 (Allen 1963: late April ..late August)), (d)
    DRONE_EGGLAYING_STOP = 240, # end of drone production, (August 28 (Allen 1963: late April ..late August)), (d)
    MORTALITY_LARVAE = 0.01, # daily mortality rate of worker larvae (Schmickl & Crailsheim (2007): 0.01) (mortality/d)
    PUPATION_AGE = 9, # age of worker larvae pupation (Winston (1987) p.50) (d)
    MORTALITY_DRONE_LARVAE = 0.044, # daily mortality rate of drone larvae (Fukuda, Ohtani (1977): 100 eggs, 82 unsealed brood, 60 sealed brood and 56 adults) (mortality/d)
    MORTALITY_PUPAE = 0.001, # daily mortality rate of worker pupae (Schmickl & Crailsheim (2007): 0.001) (mortality/d)
    MORTALITY_DRONE_PUPAE = 0.005, # daily mortality rate of drone pupae (Fukuda, Ohtani (1977): 100 eggs, 82 unsealed brood, 60 sealed brood and 56 adults) (mortality/d)
    MIN_AFF = 7, # minimum possible value age of first foraging (Aff) (Winston (1987) p.92 (minimum AFF between 3 and 20 days, maximum AFF between 27 and 65 days)) (d)
    MAX_AFF = 50, # maximum possible value age of first foraging (Aff) (Winston (1987) p.92 (minimum AFF between 3 and 20 days, maximum AFF between 27 and 65 days)) (d)
    MORTALITY_INHIVE = 0.004, # daily mortality rate of healthy in-hive bees and foragers (derived from Martin (2001), Fig. 2b (non-infected, winter)) (mortality/d)
    MORTALITY_INHIVE_INFECTED_AS_PUPA = 0.012, # (virus depending, 0.012 (DWV); 1 (APV)), daily mortality rate of in-hive bees and foragers, infected as pupae (derived from Martin (2001), Fig. 2b (infected, winter)) (mortality/d)
    MORTALITY_DRONES = 0.05, # daily mortality rate of healthy adult drones (Fukuda Ohati (1977): Fig. 3, "summer", mean lifespan: 14d) (mortality/d)
    DRONE_LIFESPAN = 37, # maximum lifespan of an adult drone (Fukuda, Ohtani (1977): 14d in summer; 32-42d in autumn, 37 = average autumn life span) (d)
    MAX_TOTAL_KM = 800, # Neukirch (1982): 838km max. flight performance in a foragers life (set to 800km in the model, as mortality acts only at end of time step), maximum total distance a forager can fly during lifetime (km)
    STEPWIDTH = 50, # to scale size of coloured bars in the colony histogram (GUI, "world") of worker brood and adult workers
    STEPWIDTHdrones = 5, # to scale size of coloured bars in the colony histogram (GUI, "world") of drone brood and adult drones
    FLIGHT_VELOCITY = 6.5, # derived from Seeley 1994, mean velocity during foraging flight see also Ribbands p127: 12.5-14.9mph (= 5.58-6.66m/s), flight speed of a forager (m/s)
    MAX_PROPORTION_POLLEN_FORAGERS = 0.8, # Lindauer (1952): 0.8, maximum proportion of pollen foragers
    LIFESPAN = 290, # Sakagami, Fukuda (1968): max. 290d, maximum lifespan of a worker bee (d)
    MIN_IDEAL_POLLEN_STORE = 250, # minimum amount of pollen foragers are trying to store (g)
    PROTEIN_STORE_NURSES_d = 7, # Crailsheim (1990): 7d, if no pollen is present and the brood to nurses ratio is at its maximum, then the protein stores of the nurse bees lasts for PROTEIN_STORE_NURSES_d days; if less brood is present, then the protein stores last proportionally longer (d)
    TIME_NECTAR_GATHERING = 1200, # Winston (1987) p. 172: average: 30-80min; Note: modelled handling time can increase with depletion of the patch, time to fill crop with nectar if nectar quantity is not yet reduced in the flower patch (s)
    TIME_POLLEN_GATHERING = 600, # Winston (1987) p. 172: 10 min; Note: modelled handling time can increase with depletion of the patch, time to collect a pollen load if pollen quantity is not yet reduced in the flower patch (s)
    TIME_UNLOADING = 116, # Seeley (1994): 116s, time to unload nectar in the colony (s)
    TIME_UNLOADING_POLLEN = 210, # Ribbands (1953) p.131: 3.5 minutes (from Park 1922,1928b), time to unload pollen in the colony (s)
    N_INITIAL_BEES = 3000, # initial colony size (all foragers) (bees)
    INPUT_FILE = "Input2_405.txt", # contains data on food availability etc. of all flower patches for 365d
    WEIGHT_WORKER_g = 0.1, # Schmickl & Crailsheim (2007): 0.1g, Martin (1998): 1kg adults = 9000 bees; Calis et al. (1999): 0.125g; higher weight => less mites!, weight of a worker bee (g)
    FIND_DANCED_PATCH_PROB = 0.5, # ca. average of reported values: Seeley 1983: 0.21; Judd 1995: 0.25; references in Biesmeijer, deVries 2001: 0.95 (Oettingen-Spielberg 1949), 0.73 (Lindauer 1952), probability for a recruit to find the advertised patch
    FLIGHTCOSTS_PER_m = 0.000006, # Goller, Esch (1990): 0.000006531 kJ/m; Schmid-Hempl et al. (1985): 0.0334W, energy consumption on flight per meter (kJ/m)
    FORAGING_STOP_PROB = 0.3, # probability per foraging round that an active forager switches to "resting"
    MAX_DANCE_CIRCUITS = 117, # Seeley & Towne (1992): 117, maximum circuits a bee can perform per dance (dance circuits)
    POLLEN_DANCE_FOLLOWERS = 2, # number of dance followers of a successful pollen forager (bees)
    POLLENLOAD = 0.015, # Schmickl & Crailsheim (2007): 0.015g (from Seeley 1995), amount of pollen collected during a single, successful pollen foraging trip, equals two pollen pellets (g)
    MORTALITY_FOR_PER_SEC = 0.00001, # Visscher & Dukas (1997): 0.036 per hour foraging, mortality rate of foragers per second foraging (mortality/s)
    ENERGY_SUCROSE = 0.00582, # wikipedia.org: sucrose: 342.3g/mol; 17kJ/g => 0.005819kJ/μmol, sucrose energy per molar concentration (kJ/μmol)
    N_GENERIC_PLOTS = 8, # of "generic" plots shown on the interface
    MITE_FALL_DRONECELL = 0.2, # Martin (1998): 20%, probability that a mite emerging from a drone cell will fall from the comb and die (1/emergence)
    MITE_FALL_WORKERCELL = 0.3, # Martin (1998): 30%, probability that a mite emerging from a worker cell will fall from the comb and die (1/emergence)
    MITE_MORTALITY_BROODPERIOD = 0.006, # Martin (1998): 0.006; Fries et al. (1994) (Tab. 6): 0.006, mite mortality rate per day during brood period (mortality/d)
    MITE_MORTALITY_WINTER = 0.002, # Martin (1998): 0.002; Fries et al. (1994) (Tab. 6): 0.004, mite mortality rate per day if no brood is present (mortality/d)
    DISTANCE_G = 500, # e.g. Steffan-Dewenter & Kuhn (2003): foraging distance ca. 60 - 10000m, mean: 1500m, distance of the "green" patch to the colony (m)
    DISTANCE_R = 1500, # e.g. Steffan-Dewenter & Kuhn (2003): foraging distance ca. 60 - 10000m, mean: 1500m, distance of the "red" patch to the colony (m)
    SHIFT_R = 30, # shifts the seasonal food flow of the "red" flower patch to earlier (positive) or later (negative) in the year
    SHIFT_G = -40, # shifts the seasonal food flow of the "green" flower patch to earlier (positive) or later (negative) in the year
    QUANTITY_R_l = 20, # amount of nectar available at "red" patch (l)
    QUANTITY_G_l = 20, # amount of nectar available at "green" patch (l)
    POLLEN_R_kg = 1, # amount of pollen available at "red" patch (kg)
    POLLEN_G_kg = 1, # amount of pollen available at "green" patch (kg)
    CONC_G = 1.5, # e.g. Seeley (1986) Fig. 2: sugar concentration of collected nectar: 0.5-2.5 mol/l, sucrose concentration in nectar of "green" patch (mol/l)
    CONC_R = 1.5, # e.g. Seeley (1986) Fig. 2: sugar concentration of collected nectar: 0.5-2.5 mol/l, sucrose concentration in nectar of "red" patch (mol/l)
    DETECT_PROB_G = 0.2, # probability, a searching foragerSquadron finds the "green" patch
    DETECT_PROB_R = 0.2, # probability, a searching foragerSquadron finds the "red" patch
    DANCE_SLOPE = 1.16, # Seeley (1994): max. 1.16 (Tab. 2), to calculate # circuits a bee dances for a patch, depending o the patch quality (energetic efficiency) (dance circuits)
    DANCE_INTERCEPT = 0 # (Seeley (1994): min. 0.6 (Tab.2)), to calculate # circuits a bee dances for a patch, depending o the patch quality (energetic efficiency) (dance circuits)
  )

  return(const)
}
