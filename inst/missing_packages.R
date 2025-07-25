library(tidyverse)

installed <- installed.packages()

installed_names <- rownames(installed)

deps <- lapply(rownames(installed), function(x){
  lib <- installed[x, "LibPath"]
  tmp <- tibble(desc::desc_get_deps(file.path(lib,x))) %>% 
    select(type, package) %>% 
    mutate(main = x)
  tmp
})

all_deps <- do.call(rbind, deps)

# depends
missing_depends <- all_deps %>% 
  filter(type == 'Depends') %>% 
  filter(!package %in% c(installed_names, "R")) %>% 
  distinct(package) %>% 
  pull() %>% 
  sort()

# imports
missing_imports <- all_deps %>% 
  filter(type == 'Imports') %>% 
  filter(!package %in% c(installed_names)) %>% 
  distinct(package) %>% 
  pull() %>% 
  sort()

# both
sort(unique(c(missing_depends, missing_imports)))


# suggests
missing_suggests <- all_deps %>% 
  filter(type == 'Suggests') %>% 
  filter(!package %in% c(installed_names)) %>% 
  distinct(package) %>% 
  pull() %>% 
  sort()

missing_enhances <- all_deps %>% 
  filter(type == 'Enhances') %>% 
  filter(!package %in% c(installed_names)) %>% 
  distinct(package) %>% 
  pull() %>% 
  sort()

# both
sort(unique(c(missing_suggests, missing_enhances)))

# individual
missing_depends
missing_imports
missing_suggests
missing_enhances

# all
sort(unique(c(missing_depends, missing_imports, missing_suggests, missing_enhances)))


# which packages have missing dependencies (specifically depends and imports)
all_deps %>% 
  filter(type == 'Depends') %>% 
  filter(package %in% missing_depends)

all_deps %>% 
  filter(type == 'Imports') %>% 
  filter(package %in% missing_imports) %>% 
  distinct(main) %>% 
  View()
