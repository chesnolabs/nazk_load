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

#shoud we add factions/committee column?
add_faction <- FALSE
#shortlist_file <- "/home/pavlo/GitHub/output/mps_coms.csv"
shortlist_file <- "/home/pavlo/GitHub/output/mps_facts.csv"
if (add_faction) {
  mps_factions <- read.csv(shortlist_file)
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
add_nulls <- function(t, objects_to_add, fields) {
      t <- ungroup(t)
      for (j in 1:length(objects_to_add)) {
        nms <- filter(t, objectType == objects_to_add[j])
        nms <- defined[!(defined$person_name %in% nms$fullname),]
        names(nms)[which(names(nms) == "person_name")] <- "fullname"
        for (i in 1:nrow(nms)) {
          new_row <- t[1,]
          new_row <- data.frame(apply(new_row, c(1,2), function(x) {NA}))
          new_row[,names(new_row)[names(new_row) %in% names(nms)]] <- nms[i, names(new_row)[names(new_row) %in% names(nms)]]
          new_row$objectType <- objects_to_add[j]
          new_row[,fields] <- 0
         t <- rbind(t, new_row)
        }
        number_na <- which(t$fullname %in% nms$fullname & is.na( t[,fields[1]]))
        if (length(number_na) > 0) {
          t <- t[-number_na,]
        }
        
      }
      #t <- rbind( t[1:(i-1), ], t[(i+1):nrow(t),])
  if (add_faction) {
    t$faction[is.na(t$faction)] <- sapply(t$person_name[is.na(t$faction)], function(x) {mps_factions$faction[mps_factions$person_name == x][1]})
  }
  t
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



wb <- createWorkbook("decls")

#family_members_sheet  
family_members <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_2.csv")
family_members <- merge(family_members, defined, by.x = "id", by.y = "guid")
family_members$family_member <- mapply(merge_non_blank, family_members$lastname, family_members$firstname, family_members$middlename)
fm_fields <-  c(common_fields ,'family_member', 'subjectRelation')
#names(family_members)[which(names(family_members) == "fullname")] <- "person_name"
family_members <- family_members[, fm_fields]
if (add_faction) {
  family_members <- merge(mps_factions, family_members, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  
}
addWorksheet(wb, "Члени сім'ї")
writeData(wb, "Члени сім'ї", family_members)
#write.csv(family_members, "summary/family_members.csv", row.names = FALSE)


#realty_sheet
realty <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_3.csv")
realty$region <- NULL
realty$totalArea <- as.numeric(gsub(",",".", realty$totalArea))
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
objects_to_add <- c("Квартира", "Житловий будинок", "Садовий (дачний) будинок", "Земельна ділянка")
fields <- c("number_of_objects", "sum_of_area", "max_area")
realty_short <- add_nulls(realty_short, objects_to_add, fields)
addWorksheet(wb, "Нерухомість")
writeData(wb, "Нерухомість", realty_short)
#write.csv(realty_short, "summary/realty.csv", row.names = FALSE)

#unfinished_objects_sheet
unfinished <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_4.csv")
unfinished$region <- NULL
unfinished$totalArea <- as.numeric(gsub(",",".", unfinished$totalArea))
unfinished <- merge(unfinished, defined, by.x = "id", by.y = "guid")
#names(unfinished)[which(names(unfinished) == "fullname")] <- "person_name"
fields <- c(common_fields, "objectType")
dots <- lapply(fields, as.symbol)
unfinished_short <- unfinished %>%
  group_by_(.dots=dots) %>%
  summarise(number_of_objects = n(), sum_of_area = sum(totalArea), max_area = max(totalArea))
if (add_faction) {
  unfinished_short <- merge(mps_factions, unfinished_short, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  unfinished_short[is.na(unfinished_short)] <- 0
  # & unfinished$fullname %in% defined$full_info
} 
addWorksheet(wb, "Незакінчене")
writeData(wb, "Незакінчене", unfinished_short)
#write.csv(unfinished_short, "summary/unfinished.csv", row.names = FALSE)

#valuable_items
values <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_5.csv")
values <- merge(values, defined, by.x = "id", by.y = "guid")
#names(values)[which(names(values) == "fullname")] <- "person_name"
fields <- c(common_fields, "objectType", "otherObjectType", "trademark", "propertyDescr")
dots <- lapply(fields, as.symbol)
values <- values %>%
  group_by_(.dots=dots) %>%
  summarise(number_of_objects = n())
#write.csv(values, "summary/values.csv", row.names = FALSE)
if (add_faction) {
  values <- merge(mps_factions, values, by.x = "person_name", by.y = "fullname", all.x = TRUE)
}
addWorksheet(wb, "Цінні предмети")
writeData(wb, "Цінні предмети", values)





#transport
transport <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_6.csv")
transport <- merge(transport, defined, by.x = "id", by.y = "guid")
#names(transport)[which(names(transport) == "fullname")] <- "person_name"
fields <- c(common_fields, "objectType", "brand", "model")
dots <- lapply(fields, as.symbol)
transport <- transport %>%
  group_by_(.dots=dots) %>%
  summarise(number_of_objects = n())
#write.csv(transport, "summary/transport.csv", row.names = FALSE)
if (add_faction) {
  transport <- merge(mps_factions, transport, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  #transport[is.na(transport)] <- 0
  # & transport$fullname %in% defined$full_info
}
objects_to_add <- c("Автомобіль легковий")
fields <- c("number_of_objects")
transport <- add_nulls(transport, objects_to_add, fields)
addWorksheet(wb, "Транспорт")
writeData(wb, "Транспорт", transport)

#stocks
stocks <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_7.csv")
stocks <- merge(stocks, defined, by.x = "id", by.y = "guid")
#names(stocks)[which(names(stocks) == "fullname")] <- "person_name"
stocks$cost <- as.numeric(gsub(",",".", stocks$cost))
fields <- c(common_fields, "typeProperty", "otherObjectType", "emitent_type",  "company_name", "company_code", "amount", "cost")
dots <- lapply(fields, as.symbol)
company_name_group <- list(company_code = c("emitent_ua_company_code",  "emitent_eng_company_code"), 
                           company_name = c("emitent_eng_company_name", "emitent_ua_company_name"))
stocks_grouped <- group_alternative_fields(stocks, company_name_group)
stocks <- stocks_grouped %>% 
  select_(.dots = dots)
if (add_faction) {
  stocks <- merge(mps_factions, stocks, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  
}
#write.csv(stocks, "summary/stocks.csv", row.names = FALSE)
addWorksheet(wb, "Цінні папери")
writeData(wb, "Цінні папери", stocks)

#corporate_rights
corporate_rights <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_9.csv")
corporate_rights <- merge(corporate_rights, defined, by.x = "id", by.y = "guid")
#names(corporate_rights)[which(names(corporate_rights) == "fullname")] <- "person_name"
fields <- c(common_fields, "legalForm",  "name", "beneficial_owner_company_code", "address", "mail")
dots <- lapply(fields, as.symbol)
corporate_rights <- corporate_rights %>% 
  select_(.dots = dots)
#write.csv(corporate_rights, "summary/corporate_rights.csv", row.names = FALSE)
if (add_faction) {
  corporate_rights <- merge(mps_factions, corporate_rights, by.x = "person_name", by.y = "fullname", all.x = TRUE)
}
addWorksheet(wb, "Корпоративні права")
writeData(wb, "Корпоративні права", corporate_rights)



#incomes
incomes <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_11.csv")
incomes <- merge(incomes, defined, by.x = "id", by.y = "guid")
#names(incomes)[which(names(incomes) == "fullname")] <- "person_name"
incomes$sizeIncome <- as.numeric(gsub(",",".", incomes$sizeIncome))
incomes$source_ua_fullname <- mapply(merge_non_blank, incomes$source_ua_lastname, incomes$source_ua_firstname, incomes$source_ua_middlename)
company_name_group <- list(company = c("source_ua_company_name",  "source_eng_company_name"))
incomes_grouped <- group_alternative_fields(incomes, company_name_group)
person_group <- list(person_source = c("source_ua_fullname", "source_eng_fullname"))
incomes_grouped <- group_alternative_fields(incomes_grouped, person_group)
fields <- c(common_fields)
dots <- lapply(fields, as.symbol)
incomes <- incomes_grouped %>%
  group_by_(.dots = dots) %>%
  summarise(incomes = sum(sizeIncome, na.rm = TRUE))
#write.csv(incomes, "summary/incomes.csv", row.names = FALSE)
if (add_faction) {
  incomes <- merge(mps_factions, incomes, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  incomes$incomes[is.na(incomes$incomes) & incomes$person_name %in% defined$person_name] <- 0
}
addWorksheet(wb, "Доходи")
writeData(wb, "Доходи", incomes)

#checking whether declarers are bussinessmen
bussinessmen <- defined 
bussinessmen$has_bussiness <- bussinessmen$guid %in% corporate_rights$id |
  bussinessmen$guid %in% stocks$id |
  bussinessmen$guid %in% incomes_grouped$id[incomes_grouped$objectType == "Дохід від зайняття підприємницькою діяльністю" | incomes_grouped$objectType == "Дохід від відчуження цінних паперів та корпоративних прав"]
if (add_faction) {
  bussinessmen <- merge(mps_factions, bussinessmen, by.x = "person_name", by.y = "person_name", all.x = TRUE)
  bussinessmen[is.na(bussinessmen)] <- 0
  # & incomes$fullname %in% defined$full_info
}
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

#assets
assets <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_12.csv")
assets <- merge(assets, defined, by.x = "id", by.y = "guid")
#names(assets)[which(names(assets) == "fullname")] <- "person_name"
assets$sizeAssets <- as.numeric(gsub(",",".", assets$sizeAssets))
assets$in_hryvnas <- mapply(to_hryvnas, assets$sizeAssets, assets$assetsCurrency)
fields <- c(common_fields)
dots <- lapply(fields, as.symbol)
assets <- assets %>%
  group_by_(.dots = dots) %>%
  summarise(assets = sum(in_hryvnas, na.rm = TRUE))
if (add_faction) {
  assets <- merge(mps_factions, assets, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  assets$assets[is.na(assets$assets) & assets$person_name %in% defined$person_name] <- 0
}
addWorksheet(wb, "Заощадження")
writeData(wb, "Заощадження", assets)
#write.csv(assets, "summary/assets.csv", row.names = FALSE)


#obligation
obligations <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_13.csv")
obligations <- merge(obligations, defined, by.x = "id", by.y = "guid")
#names(obligations)[which(names(obligations) == "fullname")] <- "person_name"
obligations$sizeObligation <- as.numeric(gsub(",",".", obligations$sizeObligation))
obligations$in_hryvnas <- mapply(to_hryvnas, obligations$sizeObligation, obligations$currency)
obligations$person_ua <- mapply(merge_non_blank, obligations$emitent_ua_lastname, obligations$emitent_ua_firstname, obligations$emitent_ua_middlename)
company_name_group <- list(company = c("emitent_ua_company_name",  "emitent_eng_company_name"), company_code = c("emitent_eng_company_code", "emitent_ua_company_code"), person = c("emitent_eng_fullname", "person_ua"))
obligations <- group_alternative_fields(obligations, company_name_group)
fields <- c(common_fields, "objectType", "otherObjectType", "sizeObligation", "currency", "in_hryvnas", "company", "company_code", "person")
dots <- lapply(fields, as.symbol)
obligations <- obligations %>%
  select_(.dots = dots)
if (add_faction) {
  obligations <- merge(mps_factions, obligations, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  obligations[is.na(obligations)] <- 0
  # & obligations$fullname %in% defined$full_info
}
#write.csv(obligations, "summary/obligations.csv", row.names = FALSE)
addWorksheet(wb, "Фінансові зобов'язання")
writeData(wb, "Фінансові зобов'язання", obligations)

#expenses
expenses <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_14.csv")
expenses <- merge(expenses, defined, by.x = "id", by.y = "guid")
#names(expenses)[which(names(expenses) == "fullname")] <- "person_name"
expenses$costAmount <- as.numeric(gsub(",",".", expenses$costAmount))
fields <- c(common_fields)
dots <- lapply(fields, as.symbol)
expenses <- expenses %>%
  group_by_(.dots = dots) %>%
  summarise(expenses = sum(costAmount))
if (add_faction) {
  expenses <- merge(mps_factions, expenses, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  expenses[is.na(expenses)] <- 0
  #& expenses$fullname %in% defined$full_info
}
#write.csv(expenses, "summary/expenses.csv", row.names = FALSE)
addWorksheet(wb, "Витрати")
writeData(wb, "Витрати", expenses)


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
dots <- lapply(fields, as.symbol)
by_jobs <- by_jobs %>%
  select_(.dots = dots)
if (add_faction) {
  by_jobs <- merge(mps_factions, by_jobs, by.x = "person_name", by.y = "fullname", all.x = TRUE)
  
}
addWorksheet(wb, "Робота за сумісництвом")
writeData(wb, "Робота за сумісництвом", by_jobs)
#write.csv(by_jobs, "summary/by_jobs.csv", row.names = FALSE)


#gos
organisations <- read.csv("file:///home/pavlo/scripts/nazk_new/csvs/step_16.csv")
organisations <- merge(organisations, defined, by.x = "id", by.y = "guid")
#names(organisations)[which(names(organisations) == "fullname")] <- "person_name"
fields <- c(common_fields, "type", "objectType", "objectName", "unitType", "unitName")
dots <- lapply(fields, as.symbol)
organisations <- organisations %>%
  select_(.dots = dots)
if (add_faction) {
  organisations <- merge(mps_factions, organisations, by.x = "person_name", by.y = "fullname", all.x = TRUE)
}
addWorksheet(wb, "Організація")
writeData(wb, "Організація", organisations)
#write.csv(organisations, "summary/organisations.csv", row.names = FALSE)
saveWorkbook(wb, file = "summary/declarations_summary.xlsx", overwrite = TRUE)

#count tramps
year_children <- 18174
year_adults <- 17212
is_poor <- function(guid, min = TRUE, old = FALSE) {
  fm_amount <- sum(family_members$id == guid) + 1
  means <- sum(incomes$incomes[incomes$id == guid])
  if (!old) {
    means <- means + sum(assets$assets[assets$id == guid])
  }
  if (min) {
    ret <- year_adults + (fm_amount - 1) * year_children > means
  } else {
    ret <- year_adults  * (fm_amount - 1) > means
  }
  if (length(ret) == 0) {
    TRUE
  } else {
    ret
  }
  
}
beggars <- createWorkbook("poor")
poor_new <- defined
poor_new$poor_min <- sapply(poor_new$guid, is_poor)
poor_new$poor_max <- sapply(poor_new$guid, is_poor, FALSE)
addWorksheet(beggars, "Бідняки із заощадженнями")
writeData(beggars, "Бідняки із заощадженнями", filter(poor_new, poor_min))
poor_old <- defined
poor_old$poor_min <- sapply(poor_old$guid, is_poor,TRUE, TRUE)
poor_old$poor_max <- sapply(poor_old$guid, is_poor, FALSE, TRUE)
addWorksheet(beggars, "Бідняки без заощаджень")
writeData(beggars, "Бідняки без заощаджень", filter(poor_old, poor_min))

saveWorkbook(beggars, file = "beggars.xlsx", overwrite = TRUE)


  
