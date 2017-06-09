library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(openxlsx)
library(lazyeval)

options(stringsAsFactors = F)
common_fields <- c("id", "fullname", "region", "council", "workPlace", "workPost", "adress")
options(stringsAsFactors = FALSE)
defined <- read.csv("file:///home/pavlo/scripts/nazk_new/defined.csv")

setwd("/home/pavlo/scripts/nazk_new")
#saving_defined_and_undefined
undefined <- read.csv("file:///home/pavlo/scripts/nazk_new/undefined.csv")
dwb <- createWorkbook("defined_undefined")
addWorksheet(dwb, "Визначені")
writeData(dwb, "Визначені", defined)
addWorksheet(dwb, "Декілька декларацій")
writeData(dwb, "Декілька декларацій", filter(undefined, guid != ""))
addWorksheet(dwb, "Немає декларації")
writeData(dwb, "Немає декларації", filter(undefined, guid == ""))
saveWorkbook(dwb, file = "undefined.xlsx", overwrite = TRUE)

#currencies rate
currencies <- list()
currencies$UAH <- 1
currencies$USD <- 27.190858
currencies$EUR <- 28.422604
currencies$RUB <- 0.45113
currencies$CHF <- 26.528471
currencies$PLN <- 6.439048
currencies$GBP <- 33.320755
currencies$CZK <- 1.051871
currencies$HUF <- 0.0916592
currencies$CAD <- 20.080969


#shoud we add factions/committee column?
add_faction <- FALSE
#shortlist_file <- "/home/pavlo/GitHub/output/mps_coms.csv"
shortlist_file <- "/home/pavlo/GitHub/output/mps_facts.csv"
if (add_faction) {
  mps_factions <- read.csv(shortlist_file)
}

sort_df <- function(x) {
  x[sort(names(x))]
}

#functiob to substract one data.frame from another
subdt <- function(t2, t1, common_fields) {
  for (cf in common_fields) {
    if (cf %in% names(t1)) {
      common <- intersect(c(t1[,cf]), t2[,cf])
      cr <- interp(~cf %in% common, cf = as.name(cf))
      sort_cf = interp(~cf, cf =  as.name(cf))
      t1 <- t1 %>%
        #filter_(paste0(cf," %in% common")) %>%
        filter_(.dots = cr) %>%
        arrange_(.dots = sort_cf)
      t2 <- t2 %>%
        filter_(.dots = cr) %>%
        arrange_(.dots = sort_cf)
    }
  }
 classes <- sapply(t1, class)
 ret <- t1[cf]
 for (col in 1:length(classes)) {
   if (classes[col] == "numeric" | classes[col] == "integer") {
     ret[names(t1)[col]] <- t2[, col] - t1[,col]
   } else {
     if (length(na.exclude(t2[, col]) ) > 0 & length(na.exclude(t1[, col])) > 0) {
       if (na.exclude(t2[, col]) == na.exclude(t1[,col])) {
         ret[names(t1)[col]] <- t2[, col]
       }
     }
   }
 }
 ret
}

#function to make medians and means correct
add_nulls <- function(t, defined, objects_to_add, fields) {
      t <- ungroup(t)
      all_guids_all_objects <- merge(defined, 
                                     data.frame(objectType = objects_to_add),
                                     all = TRUE)
      blank_objects <- merge(all_guids_all_objects, t, by.x = c("guid", "objectType"),
                             by.y = c("id", "objectType"), all.x = TRUE)
      blank_objects[is.na(blank_objects[fields[1]]), fields] <- 0
      blank_objects
}

fix_postcodes <- function(n) {
  s = as.character(n)
  if (!(is.na(s))) {
    if (nchar(s) < 5) {
      lack_symbols <- 5-nchar(s)
      paste0(paste0(rep("0",lack_symbols), collapse = ''),s, collapse = '')
    } else {
      s
    }
  } else {
    '0'
  }
}

group_alternative_fields <- function(t, alternative_fields) {
  for (i in 1:length(alternative_fields)) {
    fields <- alternative_fields[[i]]
    pivot_column <- rep(x = "", times = nrow(t))
    for (j in 1:length(fields)) {
      non_blank <- (!is.na(t[,fields[j]]))  & (t[,fields[j]] != '')
      pivot_column[non_blank] <- t[non_blank, fields[j]]
      t[,fields[j]] <- NULL
    }
    t[, names(alternative_fields[i])] <- pivot_column
  }
  t
}

merge_non_blank <- function(x,y, z = NULL) {
  if ((is.na(x)) | (x == '')) {
    ''
  } else {
    if (!(is.null(z))) {
      paste(x,y,z)
    } else {
      paste(x,y)
    }
    
  }
}

to_hryvnas <- function(amount, currency) {
  #print(currency)
  cur = as.character(currency)
  ret <- amount * currencies[cur][[1]]
  #print(ret)
  if (length(ret) == 0) {
    0
  } else {
    ret
  }
}

family_members <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_2.csv")
realty <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_3.csv")
realty$region <- NULL
realty$totalArea <- as.numeric(gsub(",",".", realty$totalArea))
unfinished <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_4.csv")
unfinished$region <- NULL
unfinished$totalArea <- as.numeric(gsub(",",".", unfinished$totalArea))
values <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_5.csv")
transport <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_6.csv")
stocks <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_7.csv")
corporate_rights <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_9.csv")
incomes <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_11.csv")
incomes$sizeIncome <- as.numeric(gsub(",",".", incomes$sizeIncome))
incomes$source_ua_fullname <- mapply(merge_non_blank, incomes$source_ua_lastname, incomes$source_ua_firstname, incomes$source_ua_middlename)
assets <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_12.csv")
assets$sizeAssets <- as.numeric(gsub(",",".", assets$sizeAssets))
assets$in_hryvnas <- mapply(to_hryvnas, assets$sizeAssets, assets$assetsCurrency)
obligations <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_13.csv")
obligations$sizeObligation <- as.numeric(gsub(",",".", obligations$sizeObligation))
obligations$in_hryvnas <- mapply(to_hryvnas, obligations$sizeObligation, obligations$currency)
obligations$person_ua <- mapply(merge_non_blank, obligations$emitent_ua_lastname, obligations$emitent_ua_firstname, obligations$emitent_ua_middlename)
company_name_group <- list(company = c("emitent_ua_company_name",  "emitent_eng_company_name"), company_code = c("emitent_eng_company_code", "emitent_ua_company_code"), person = c("emitent_eng_fullname", "person_ua"))
obligations <- group_alternative_fields(obligations, company_name_group)

get_council_data <- function(council, defined) {
  print(council)
  wb <- createWorkbook("decls")
  #family_members_sheet  
  family_members <- merge(family_members, defined, by.x = "id", by.y = "guid")
  family_members$family_member <- mapply(merge_non_blank, family_members$lastname, family_members$firstname, family_members$middlename)
  fm_fields <-  c(common_fields ,'object_id','family_member', 'subjectRelation')
  #names(family_members)[which(names(family_members) == "fullname")] <- "person_name"
  family_members <- family_members[, fm_fields]
  family_members$relation_name <- paste0(family_members$subjectRelation, ", ", family_members$family_member)
  fm_list <- list()
  for (i in 1:nrow(family_members)) {
    fm_list[as.character(family_members$object_id[i])] <- family_members$relation_name[i]
  }
  
  if (add_faction) {
    family_members <- merge(mps_factions, family_members, by.x = "person_name", by.y = "fullname", all.x = TRUE)
    
  }
  addWorksheet(wb, "Члени сім'ї")
  writeData(wb, "Члени сім'ї", family_members)
  #write.csv(family_members, "summary/family_members.csv", row.names = FALSE)
  
  
  #realty_sheet
  realty <- merge(realty, defined, by.x = "id", by.y = "guid")
  #names(realty)[which(names(realty) == "fullname")] <- "person_name"
  fields <- c(common_fields, "objectType")
  dots <- lapply(fields, as.symbol)
  realty_short <- realty %>%
    group_by_(.dots=dots) %>%
    summarise(number_of_objects = n(), sum_of_area = sum(totalArea), max_area = max(totalArea))
  if (add_faction) {
    realty_short <- merge(mps_factions, realty_short, by.x = "person_name", by.y = "fullname", all.x = TRUE)
    #realty_short[is.na(realty_short)] <- 0
    #[,c("number_of_objects", "sum_of_area", "max_area")] realty_short$fullname %in% defined$full_info
  }
  if (council == "Івано-Франківська міська рада") {
    print("stop")
  }
  objects_to_add <- c("Квартира", "Житловий будинок", "Садовий (дачний) будинок", "Кімната")
  living_areas <- c("Квартира", "Житловий будинок", "Садовий (дачний) будинок", "Кімната")
  usual_living_areas <- c("Квартира", "Житловий будинок")
  
  fields <- c("number_of_objects", "sum_of_area", "max_area")
  if (council == "Чернівецька обласна рада") {
    print("stop")
  }
  if (nrow(realty) > 0) {
    realty$declarer_or_family <- "декларант"
    realty$declarer_or_family[ifelse(is.na(realty$person), FALSE, realty$person != 1)] <- sapply(realty$person[ifelse(is.na(realty$person), FALSE, realty$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  realty_short <- add_nulls(realty_short, defined, objects_to_add, fields)
  homeless <- realty_short %>% 
    filter(!is.na(guid), objectType %in% living_areas) %>%
    group_by(guid, person_name) %>%
    summarise(living_objects = sum(number_of_objects)) %>%
    filter(living_objects == 0)
  hard_living_condition <- realty_short %>% 
    filter(!is.na(guid), objectType %in% usual_living_areas) %>%
    group_by(guid, person_name) %>%
    summarise(apartments_houses = sum(number_of_objects)) %>%
    filter(apartments_houses == 0, !person_name %in% homeless$person_name)
  addWorksheet(wb, "Нерухомість")
  writeData(wb, "Нерухомість", sort_df(realty))
  homeless_wb = createWorkbook()
  addWorksheet(homeless_wb, "Безхатченки")
  writeData(homeless_wb, "Безхатченки", homeless)
  addWorksheet(homeless_wb, "Дача або кімната")
  writeData(homeless_wb, "Дача або кімната", hard_living_condition)
  saveWorkbook(homeless_wb, file = paste0("summary_councils/безхатьки_", council, ".xlsx"), overwrite = TRUE)
  
  #write.csv(realty_short, "summary/realty.csv", row.names = FALSE)
  
  #unfinished_objects_sheet
  unfinished <- merge(unfinished, defined, by.x = "id", by.y = "guid")
  #names(unfinished)[which(names(unfinished) == "fullname")] <- "person_name"
  # fields <- c(common_fields, "objectType")
  # dots <- lapply(fields, as.symbol)
  # unfinished_short <- unfinished %>%
  #   group_by_(.dots=dots) %>%
  #   summarise(number_of_objects = n(), sum_of_area = sum(totalArea), max_area = max(totalArea))
  # if (add_faction) {
  #   unfinished_short <- merge(mps_factions, unfinished_short, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  #   unfinished_short[is.na(unfinished_short)] <- 0
  #   # & unfinished$fullname %in% defined$full_info
  # } 
  if (nrow(unfinished) > 0) {
    unfinished$declarer_or_family <- "декларант"
    unfinished$declarer_or_family[ifelse(is.na(unfinished$person), FALSE, unfinished$person != 1)] <- sapply(unfinished$person[ifelse(is.na(unfinished$person), FALSE, unfinished$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Незакінчене")
  writeData(wb, "Незакінчене", sort_df(unfinished))
  #write.csv(unfinished_short, "summary/unfinished.csv", row.names = FALSE)
  
  #valuable_items
  values <- merge(values, defined, by.x = "id", by.y = "guid")
  #names(values)[which(names(values) == "fullname")] <- "person_name"
  # fields <- c(common_fields, "objectType", "otherObjectType", "trademark", "propertyDescr")
  # dots <- lapply(fields, as.symbol)
  # values <- values %>%
  #   group_by_(.dots=dots) %>%
  #   summarise(number_of_objects = n())
  #write.csv(values, "summary/values.csv", row.names = FALSE)
  if (add_faction) {
    values <- merge(mps_factions, values, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  }
  if (nrow(values) > 0) {
    values$declarer_or_family <- "декларант"
    values$declarer_or_family[ifelse(is.na(values$person), FALSE, values$person != 1)] <- sapply(values$person[ifelse(is.na(values$person), FALSE, values$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Цінні предмети")
  writeData(wb, "Цінні предмети", sort_df(values))
  
  
  
  
  
  #transport
  transport <- merge(transport, defined, by.x = "id", by.y = "guid")
  #names(transport)[which(names(transport) == "fullname")] <- "person_name"
  # fields <- c(common_fields, "objectType", "brand", "model")
  # dots <- lapply(fields, as.symbol)
  # transport <- transport %>%
  #   group_by_(.dots=dots) %>%
  #   summarise(number_of_objects = n())
  #write.csv(transport, "summary/transport.csv", row.names = FALSE)
  if (add_faction) {
    transport <- merge(mps_factions, transport, by.x = "person_name", by.y = "fullname", all.x = TRUE)
    #transport[is.na(transport)] <- 0
    # & transport$fullname %in% defined$full_info
  }
  objects_to_add <- c("Автомобіль легковий")
  fields <- c("number_of_objects")
  if (nrow(transport) > 0) {
    transport$declarer_or_family <- "декларант"
    transport$declarer_or_family[ifelse(is.na(transport$person), FALSE, transport$person != 1)] <- sapply(transport$person[ifelse(is.na(transport$person), FALSE, transport$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  #transport <- add_nulls(transport, objects_to_add, fields)
  addWorksheet(wb, "Транспорт")
  writeData(wb, "Транспорт", sort_df(transport))
  
  #stocks
  stocks <- merge(stocks, defined, by.x = "id", by.y = "guid")
  #names(stocks)[which(names(stocks) == "fullname")] <- "person_name"
  stocks$cost <- as.numeric(gsub(",",".", stocks$cost))
  fields <- c(common_fields, "typeProperty", "otherObjectType", "emitent_type",  "company_name", "company_code", "amount", "cost")
  dots <- lapply(fields, as.symbol)
  company_name_group <- list(company_code = c("emitent_ua_company_code",  "emitent_eng_company_code"),
                             company_name = c("emitent_eng_company_name", "emitent_ua_company_name"))
  stocks_grouped <- group_alternative_fields(stocks, company_name_group)
  # stocks <- stocks_grouped %>%
  #   select_(.dots = dots)
  # if (add_faction) {
  #   stocks <- merge(mps_factions, stocks, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  # 
  # }
  if (nrow(stocks_grouped) > 0) {
    stocks_grouped$declarer_or_family <- "декларант"
    stocks_grouped$declarer_or_family[ifelse(is.na(stocks_grouped$person), FALSE, stocks_grouped$person != 1)] <- sapply(stocks_grouped$person[ifelse(is.na(stocks_grouped$person), FALSE, stocks_grouped$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Цінні папери")
  writeData(wb, "Цінні папери", sort_df(stocks_grouped))
  
  #corporate_rights
  corporate_rights <- merge(corporate_rights, defined, by.x = "id", by.y = "guid")
  #names(corporate_rights)[which(names(corporate_rights) == "fullname")] <- "person_name"
  # fields <- c(common_fields, "legalForm",  "name", "beneficial_owner_company_code", "address", "mail")
  # dots <- lapply(fields, as.symbol)
  # corporate_rights <- corporate_rights %>% 
  #   select_(.dots = dots)
  # if (add_faction) {
  #   corporate_rights <- merge(mps_factions, corporate_rights, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  # }
  if (nrow(corporate_rights) > 0) {
    corporate_rights$declarer_or_family <- "декларант"
    corporate_rights$declarer_or_family[ifelse(is.na(corporate_rights$person), FALSE, corporate_rights$person != 1)] <- sapply(corporate_rights$person[ifelse(is.na(corporate_rights$person), FALSE, corporate_rights$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Корпоративні права")
  writeData(wb, "Корпоративні права", sort_df(corporate_rights))
  
  
  
  #incomes
  incomes <- merge(incomes, defined, by.x = "id", by.y = "guid")
  company_name_group <- list(company = c("source_ua_company_name",  "source_eng_company_name"))
  incomes_grouped <- group_alternative_fields(incomes, company_name_group)
  person_group <- list(person_source = c("source_ua_fullname", "source_eng_fullname"))
  incomes_grouped <- group_alternative_fields(incomes_grouped, person_group)
  #names(incomes)[which(names(incomes) == "fullname")] <- "person_name"
  # fields <- c(common_fields)
  # dots <- lapply(fields, as.symbol)
  # incomes <- incomes_grouped %>%
  #   group_by_(.dots = dots) %>%
  #   summarise(incomes = sum(sizeIncome, na.rm = TRUE))
  # if (add_faction) {
  #   incomes <- merge(mps_factions, incomes, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  #   incomes$incomes[is.na(incomes$incomes) & incomes$person_name %in% defined$person_name] <- 0
  # }
  incomes_grouped$declarer_or_family <- "декларант"
  incomes_grouped$declarer_or_family[!is.na(incomes_grouped$person != 1)] <- sapply(incomes_grouped$person[!is.na(incomes_grouped$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  addWorksheet(wb, "Доходи")
  writeData(wb, "Доходи", sort_df(incomes_grouped))
  
  

  #assets
  assets <- merge(assets, defined, by.x = "id", by.y = "guid")
  #names(assets)[which(names(assets) == "fullname")] <- "person_name"
  
  fields <- c(common_fields)
  # dots <- lapply(fields, as.symbol)
  # assets <- assets %>%
  #   group_by_(.dots = dots) %>%
  #   summarise(assets = sum(in_hryvnas, na.rm = TRUE))
  if (add_faction) {
    assets <- merge(mps_factions, assets, by.x = "person_name", by.y = "fullname", all.x = TRUE)
    assets$assets[is.na(assets$assets) & assets$person_name %in% defined$person_name] <- 0
  }
  if (nrow(assets) > 0) {
    assets$declarer_or_family <- "декларант"
    assets$declarer_or_family[ifelse(is.na(assets$person), FALSE, assets$person != 1)] <- sapply(assets$person[ifelse(is.na(assets$person), FALSE, assets$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Заощадження")
  writeData(wb, "Заощадження", sort_df(assets))
  #write.csv(assets, "summary/assets.csv", row.names = FALSE)
  
  
  #obligation
  obligations <- merge(obligations, defined, by.x = "id", by.y = "guid")
  #names(obligations)[which(names(obligations) == "fullname")] <- "person_name"
  
  fields <- c(common_fields, "objectType", "otherObjectType", "sizeObligation", "currency", "in_hryvnas", "company", "company_code", "person")
  # dots <- lapply(fields, as.symbol)
  # obligations <- obligations %>%
  #   select_(.dots = dots)
  if (add_faction) {
    obligations <- merge(mps_factions, obligations, by.x = "person_name", by.y = "fullname", all.x = TRUE)
    obligations[is.na(obligations)] <- 0
    # & obligations$fullname %in% defined$full_info
  }
  #write.csv(obligations, "summary/obligations.csv", row.names = FALSE)
  if (nrow(obligations) > 0) {
    obligations$declarer_or_family <- "декларант"
    obligations$declarer_or_family[ifelse(is.na(obligations$person), FALSE, obligations$person != 1)] <- sapply(obligations$person[ifelse(is.na(obligations$person), FALSE, obligations$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Фінансові зобов'язання")
  writeData(wb, "Фінансові зобов'язання", sort_df(obligations))
  
  #expenses
  expenses <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_14.csv")
  expenses <- merge(expenses, defined, by.x = "id", by.y = "guid")
  #names(expenses)[which(names(expenses) == "fullname")] <- "person_name"
  expenses$costAmount <- as.numeric(gsub(",",".", expenses$costAmount))
  fields <- c(common_fields)
  dots <- lapply(fields, as.symbol)
  # expenses <- expenses %>%
  #   group_by_(.dots = dots) %>%
  #   summarise(expenses = sum(costAmount))
  if (add_faction) {
    expenses <- merge(mps_factions, expenses, by.x = "person_name", by.y = "fullname", all.x = TRUE)
    expenses[is.na(expenses)] <- 0
    #& expenses$fullname %in% defined$full_info
  }
  #write.csv(expenses, "summary/expenses.csv", row.names = FALSE)
  if (nrow(obligations) > 0) {
    expenses$declarer_or_family <- "декларант"
    expenses$declarer_or_family[ifelse(is.na(expenses$person), FALSE, expenses$person != 1)] <- sapply(expenses$person[ifelse(is.na(expenses$person), FALSE, expenses$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Витрати")
  writeData(wb, "Витрати", sort_df(expenses))
  
  
  #by_jobs
  by_jobs <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_15.csv")
  by_jobs <- merge(by_jobs, defined, by.x = "id", by.y = "guid")
  #names(by_jobs)[which(names(by_jobs) == "fullname")] <- "person_name"
  by_jobs$person_ua <- mapply(merge_non_blank, by_jobs$emitent_ua_lastname, by_jobs$emitent_ua_firstname, by_jobs$emitent_ua_middlename)
  company_name_group <- list(company = c("emitent_ua_company_name",  
                                         "emitent_eng_company_name"), 
                             company_code = c("emitent_eng_company_code", 
                                              "emitent_ua_company_code"), 
                             person = c("emitent_eng_fullname", "person_ua"))
  by_jobs <- group_alternative_fields(by_jobs, company_name_group)
  fields <- c(common_fields, "company", "company_code", "person", "description", "paid")
  # dots <- lapply(fields, as.symbol)
  # by_jobs <- by_jobs %>%
  #   select_(.dots = dots)
  if (add_faction) {
    by_jobs <- merge(mps_factions, by_jobs, by.x = "person_name", by.y = "fullname", all.x = TRUE)
    
  }
  if (nrow(by_jobs) > 0) {
    by_jobs$declarer_or_family <- "декларант"
    by_jobs$declarer_or_family[ifelse(is.na(by_jobs$person), FALSE, by_jobs$person != 1)] <- sapply(by_jobs$person[ifelse(is.na(by_jobs$person), FALSE, by_jobs$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  addWorksheet(wb, "Робота за сумісництвом")
  writeData(wb, "Робота за сумісництвом", sort_df(by_jobs))
  #write.csv(by_jobs, "summary/by_jobs.csv", row.names = FALSE)
  
  
  #gos
  organisations <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_16.csv")
  organisations <- merge(organisations, defined, by.x = "id", by.y = "guid")
  #names(organisations)[which(names(organisations) == "fullname")] <- "person_name"
  fields <- c(common_fields, "type", "objectType", "objectName", "unitType", "unitName")
  dots <- lapply(fields, as.symbol)
  # organisations <- organisations %>%
  #   select_(.dots = dots)
  if (add_faction) {
    organisations <- merge(mps_factions, organisations, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  }
  if (nrow(organisations) > 0) {
    organisations$declarer_or_family <- "декларант"
    organisations$declarer_or_family[ifelse(is.na(organisations$person), FALSE, organisations$person != 1)] <- sapply(organisations$person[ifelse(is.na(organisations$person), FALSE, organisations$person != 1)], function(x) fm_list[as.character(x)][[1]] )
  }
  
  addWorksheet(wb, "Організація")
  writeData(wb, "Організація", sort_df(organisations))
  #write.csv(organisations, "summary/organisations.csv", row.names = FALSE)
  saveWorkbook(wb, file = paste0("summary_councils/", council, ".xlsx"), overwrite = TRUE)
}

councils <- unique(defined$council) 
for (con in councils) {
  def <- filter(defined, council == con)
  get_council_data(con, def)
}



  
