#-------------------------------------------------------------------------------------
#' Fix the human_eco flag
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.human_eco = function(toxval.db,reset=T){
  printCurrentFunction(toxval.db)

  cat("  set all to 'not classified'\n")
  if(reset) runQuery("update toxval set human_eco='not classified'",toxval.db)

  n0 = runQuery("select count(*) from toxval",toxval.db)[1,1]

  ################################################################################################
  cat("  set genetox records to 'genetox'\n")
  query <- "update toxval set human_eco='genetox' where human_eco='not classified' and risk_assessment_class='genotoxicity'"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 1:",n0,n1,"\n")

  ################################################################################################
  cat("  set mammalian records to 'human health'\n")
  query <- "update toxval set human_eco='human health' where human_eco='not classified' and species_id in
  (select species_id from species where species_supercategory='mammals')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 2:",n0,n1,"\n")

  ################################################################################################
  cat("  set appropriate records to 'eco'\n")
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and species_id in
  (select species_id from species where species_supercategory in
  ('algae, moss, fungi',
  'amphibians',
  'birds',
  'crustaceans',
  'diatom',
  'fish',
  'flowers, trees, shrubs, ferns',
  'insects/spiders',
  'invertebrates',
  'microorganisms',
  'molluscs',
  'reptiles',
  'worms',
  'bacteria',
  'soil','yeast')
  )"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 3:",n0,n1,"\n")

  ################################################################################################
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and species_original in
  ('aerobic',
  'aquatic toxicity experiments using',
  'domestic secondary effluent',
  'hydrolytic',
  'juvenile',
  'microcosm/mesocosm',
  'mixed culture',
  'municipal',
  'native biomass',
  'natural seawater',
  'river water',
  'secondary effluent')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 4:",n0,n1,"\n")

  ################################################################################################
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and toxval_subtype_original in (
  'Toxicity to aquatic algae and cyanobacteria',
  'Toxicity to microorganisms',
  'Toxicity to other above-ground organisms',
  'Toxicity to other aquatic organisms',
  'Toxicity to soil macroorganisms except arthropods',
  'Toxicity to soil microorganisms',
  'Toxicity to terrestrial arthropods',
  'Long-term toxicity to fish',
  'Short-term toxicity to aquatic invertebrates',
  'Short-term toxicity to fish',
  'Toxicity to microorganisms',
  'Toxicity to soil microorganisms',
  'Toxicity to terrestrial plants',
  'Toxicity to aquatic plants other than algae')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 5:",n0,n1,"\n")

  ################################################################################################
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%Aquatic%')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 6:",n0,n1,"\n")

  ################################################################################################
  cat("  set other ecotox records to 'eco'\n")
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and source='ECOTOX'"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 7:",n0,n1,"\n")

  ################################################################################################
  cat("  fix ECHA IUCLID fish studies\n")
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%ToxicityToFish%')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 8:",n0,n1,"\n")

  query <- "update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%ToxicityToAquaInv%')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 9:",n0,n1,"\n")

  query <- "update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%ToxToFish%')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 10:",n0,n1,"\n")

  query <- "update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%EcoTox%')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 11:",n0,n1,"\n")
  ################################################################################################
  runQuery("update toxval set human_eco='eco' where human_eco='not classified' and species_id in
           (select species_id from species
           where species_supercategory
           in (
           'algae, moss, fungi; standard test species',
           'amphibians; standard test species',
           'amphibians; standard test species; u.s. exotic/nuisance species',
           'birds; standard test species',
           'crustaceans; standard test species',
           'crustaceans; standard test species; u.s. exotic/nuisance species',
           'fish; standard test species',
           'fish; standard test species; u.s. exotic/nuisance species',
           'fish; standard test species; u.s. threatened and endangered species',
           'fish; u.s. exotic/nuisance species',
           'fish; u.s. threatened and endangered species',
           'flowers, trees, shrubs, ferns; standard test species',
           'flowers, trees, shrubs, ferns; u.s. exotic/nuisance species',
           'insects/spiders; standard test species'
           )
  )",toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 12:",n0,n1,"\n")

  ################################################################################################
  runQuery("update toxval set human_eco='human health' where human_eco='not classified' and species_id in
           (select species_id from species
           where species_supercategory
           in (
           'mammals; standard test species'
          )
  )",toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 13:",n0,n1,"\n")

  ################################################################################################
  runQuery("update toxval set human_eco='eco' where human_eco='not classified' and species_id in
           (select species_id from species
           where species_common
           in (
           'aquatic community organisms - sediment','aquatic community organisms - water'
           )
  )",toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 14:",n0,n1,"\n")

  ################################################################################################
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and source='ECHA IUCLID' and species_original='-' and toxval_type like 'EC%' and toxval_units='mg/L'"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 15:",n0,n1,"\n")

  ################################################################################################
  cat("  set 'no species' records to 'human health'\n")
  runQuery("update toxval set human_eco='human health' where human_eco='not classified' and species_id<0",toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 16:",n0,n1,"\n")
  ################################################################################################

  cat("  set IC50/ug/L, mg/L records to 'eco'\n")
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and toxval_type in ('ug/L','mg/L')"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 17:",n0,n1,"\n")
  ################################################################################################

  cat("  set remaining unclassified records to human health'\n")
  query <- "update toxval set human_eco='human health' where human_eco='not classified'"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 18:",n0,n1,"\n")
  ################################################################################################

  cat("  odd eco records'\n")
  query <- "update toxval set human_eco='eco' where risk_assessment_class='eco aquatic'"
  runQuery(query,toxval.db)
  n1 = runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  cat("step 18:",n0,n1,"\n")
  ################################################################################################
  count.nc <- runQuery("select count(*) from toxval where human_eco='not classified'",toxval.db)[1,1]
  count.hh <- runQuery("select count(*) from toxval where human_eco='human health'",toxval.db)[1,1]
  count.ec <- runQuery("select count(*) from toxval where human_eco='eco'",toxval.db)[1,1]
  count.gt <- runQuery("select count(*) from toxval where human_eco='genetox'",toxval.db)[1,1]

  cat("counts in human health, eco, genetox and not classified:",count.hh,count.ec,count.gt,count.nc,"\n")

  x <- runQuery("select a.species_id, a.species_common, a.species_scientific, a.species_supercategory,
                a.habitat, b.species_original, b.human_eco from species a, toxval b where a.species_id=b.species_id",toxval.db)
  x <- unique(x)
  file <- "./species/species_by_human_eco.xlsx"
  write.xlsx(x,file)
}
