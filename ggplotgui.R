install.packages("ggplotgui")

# In order to install the most recent version of this package, you'll need to use the "devtools"-package
install.packages("devtools")
devtools::install_github("gertstulp/ggplotgui")

library("ggplotgui")

# You can call the function with and without passing a dataset
ggplot_shiny()
ggplot_shiny(mpg) # Passing ggplot's mpg dataset


