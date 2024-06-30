# write
user_base <- tibble::tibble(
  user = c("admin", 'jens.klenke', 'minh.salzmann-hoang', 'thomas.kania'),
  password = sapply(c('Xu4rOfa*Re', 'Cax3x9?oCu', 'crig0FAg_*', 'tH2Nich!PH'), 
                    sodium::password_store),
  permissions = c("admin", "admin", "standard", "standard"),
  name = c("User One", "User Two", "User Three", "User Four"), 
  mail =c('jens.klenke@vwl.uni-due.de', 'jens.klenke@vwl.uni-due.de',
          'minh.salzmann-hoang@uni-due.de' ,'thomas.kania@uni-due.de')
)

#
save(user_base, 
     file = here::here("user_base.RData"))

