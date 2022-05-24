#-------------------------------------------------------------------------------------
#' Fix the human_eco flag
#' @param toxval.db The version of toxval in which the data is altered.
#' @return The database will be altered
#' @export
#--------------------------------------------------------------------------------------
fix.human_eco.by.source <- function(toxval.db,source, reset=T){
  printCurrentFunction(paste(toxval.db,":", source))
  
  
  cat("  set all to 'not classified' based on source\n")
  if(reset) runQuery(paste0("update toxval set human_eco='not classified' where source like '",source,"'"),toxval.db)
  
  n0 = runQuery(paste0("select count(*) from toxval where source like '",source,"'"),toxval.db)[1,1]
  
  ################################################################################################
  cat("  set genetox records to 'genetox'\n")
  query <- paste0("update toxval set human_eco='genetox' where human_eco='not classified' and risk_assessment_class='genotoxicity' and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 1:",n0,n1,"\n")
  
  ################################################################################################
  cat("  set mammalian records to 'human health'\n")
  query <- paste0("update toxval set human_eco='human health' where human_eco='not classified' and species_id in
  (select species_id from species_ecotox where ecotox_group='mammals') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 2:",n0,n1,"\n")
  
  ################################################################################################
  cat("  set appropriate records to 'eco'\n")
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and species_id in
  (select species_id from species_ecotox where ecotox_group in
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
  ) and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 3:",n0,n1,"\n")
  
  ################################################################################################
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and species_original in
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
  'secondary effluent') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 4:",n0,n1,"\n")
  
  ################################################################################################
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and toxval_subtype_original in (
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
  'Toxicity to aquatic plants other than algae') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 5:",n0,n1,"\n")
  
  ################################################################################################
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%Aquatic%') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 6:",n0,n1,"\n")
  
  ################################################################################################
  cat("  set other ecotox records to 'eco'\n")
  query <- "update toxval set human_eco='eco' where human_eco='not classified' and source='ECOTOX'"
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 7:",n0,n1,"\n")
  
  ################################################################################################
  cat("  fix ECHA IUCLID fish studies\n")
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%ToxicityToFish%') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 8:",n0,n1,"\n")
  
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%ToxicityToAquaInv%') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 9:",n0,n1,"\n")
  
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%ToxToFish%') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 10:",n0,n1,"\n")
  
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and study_type_original like ('%EcoTox%') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 11:",n0,n1,"\n")
  ################################################################################################
  runQuery(paste0("update toxval set human_eco='eco' where human_eco='not classified' and species_id in
           (select species_id from species_ecotox
           where ecotox_group
           in (
           'algae, moss, fungistandard test species',
           'amphibiansstandard test species',
           'amphibiansstandard test speciesu.s. exotic/nuisance species',
           'birdsstandard test species',
           'crustaceansstandard test species',
           'crustaceansstandard test speciesu.s. exotic/nuisance species',
           'fishstandard test species',
           'fishstandard test speciesu.s. exotic/nuisance species',
           'fishstandard test speciesu.s. threatened and endangered species',
           'fishu.s. exotic/nuisance species',
           'fishu.s. threatened and endangered species',
           'flowers, trees, shrubs, fernsstandard test species',
           'flowers, trees, shrubs, fernsu.s. exotic/nuisance species',
           'insects/spidersstandard test species'
           )
  ) and source like '",source,"'"),toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 12:",n0,n1,"\n")
  
  ################################################################################################
  runQuery(paste0("update toxval set human_eco='human health' where human_eco='not classified' and species_id in
           (select species_id from species_ecotox
           where ecotox_group
           in (
           'mammalsstandard test species'
          )
  ) and source like '",source,"'"),toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 13:",n0,n1,"\n")
  
  ################################################################################################
  runQuery(paste0("update toxval set human_eco='eco' where human_eco='not classified' and species_id in
           (select species_id from species_ecotox
           where common_name
           in (
           'aquatic community organisms - sediment','aquatic community organisms - water'
           )
  ) and source like '",source,"'"),toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 14:",n0,n1,"\n")
  
  ################################################################################################
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and source='ECHA IUCLID' and species_original='-' and toxval_type like 'EC%' and toxval_units='mg/L' and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 15:",n0,n1,"\n")
  
  ################################################################################################
  cat("  set 'no species' records to 'human health'\n")
  runQuery(paste0("update toxval set human_eco='human health' where human_eco='not classified' and species_id<0 and source like '",source,"'") ,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 16:",n0,n1,"\n")
  ################################################################################################
  
  cat("  set IC50/ug/L, mg/L records to 'eco'\n")
  query <- paste0("update toxval set human_eco='eco' where human_eco='not classified' and toxval_type in ('ug/L','mg/L') and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 17:",n0,n1,"\n")
  ################################################################################################
  
  cat("  set remaining unclassified records to human health'\n")
  query <- paste0("update toxval set human_eco='human health' where human_eco='not classified' and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'") ,toxval.db)[1,1]
  cat("step 18:",n0,n1,"\n")
  ################################################################################################
  
  cat("  odd eco records'\n")
  query <- paste0("update toxval set human_eco='eco' where risk_assessment_class='eco aquatic' and source like '",source,"'")
  runQuery(query,toxval.db)
  n1 = runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  cat("step 18:",n0,n1,"\n")
  ################################################################################################
  count.nc <- runQuery(paste0("select count(*) from toxval where human_eco='not classified' and source like '",source,"'"),toxval.db)[1,1]
  count.hh <- runQuery(paste0("select count(*) from toxval where human_eco='human health'and source like '",source,"'"),toxval.db)[1,1]
  count.ec <- runQuery(paste0("select count(*) from toxval where human_eco='eco' and source like '",source,"'"),toxval.db)[1,1]
  count.gt <- runQuery(paste0("select count(*) from toxval where human_eco='genetox' and source like '",source,"'"),toxval.db)[1,1]
  
  cat("counts in human health, eco, genetox and not classified:",count.hh,count.ec,count.gt,count.nc,"\n")
  
  x <- runQuery(paste0("select a.species_id, a.common_name, a.latin_name, a.ecotox_group,
                a.habitat, b.species_original, b.human_eco from species_ecotox a, toxval b where a.species_id=b.species_id and source like '",source,"'"),toxval.db)
  x <- unique(x)
  # file <- paste0("./species/species_by_human_eco_",source,"_",Sys.Date(),".xlsx")
  # write.xlsx(x,file)
}
