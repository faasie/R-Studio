get_plans <- function (company_id) {
  query <- paste("select distinct(e.symbol)
  from company_provider a, company b, providers c, provider_plans d, plans e
  where b.company_id = '", company_id, "'
  and e.enabled = TRUE
  and a.company_id = b.company_id
  and a.provider_id = c.provider_id
  and c.provider_id = d.provider_id
  and d.plan_id = e.plan_id", sep = "")
  
  plans <- dbGetQuery(clever, query)
  
  plans
}