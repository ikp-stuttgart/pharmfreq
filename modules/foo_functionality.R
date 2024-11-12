foo_functionality <- function(x) {
  # CYP2D6 ------------------------------------------------------------------
  if (unique(x$gene) == "CYP2D6") {
    tmp_raw <- x %>%
      split(as.character(.$functional_status)) %>%
      map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
      janitor::clean_names()

    tmp_UM <- tmp_raw[grepl("increased|normal|decr", names(tmp_raw))]

    if (length(tmp_UM) != 3) {
      tmp_UM <- 0
    } else {
      tmp_UM <- tmp_UM$increased_function * tmp_UM$increased_function +
        (2 * tmp_UM$increased_function * tmp_UM$normal_function) +
        (2 * tmp_UM$increased_function * tmp_UM$decreased_function)
    }

    tmp_IM <- tmp_raw[grepl("no_fun|normal|decr", names(tmp_raw))]
    if (length(tmp_IM) != 3) {
      tmp_IM <- 0
    } else {
      tmp_IM <- tmp_IM$decreased_function * tmp_IM$decreased_function +
        2 * tmp_IM$decreased_function * tmp_IM$no_function +
        2 * tmp_IM$normal_function * tmp_IM$no_function
    }
    
    tmp_PM <- tmp_raw[grepl("no_fun", names(tmp_raw))]
    if (length(tmp_PM) != 1) {
      tmp_PM <- 0
    } else {
      tmp_PM <- tmp_PM$no_function * tmp_PM$no_function
    }

    tmp_NM <- 1 - sum(c(tmp_PM[[1]], tmp_IM[[1]], tmp_UM[[1]]))
    res <- tibble(
      phenotype = c("PM", "IM", "NM", "UM"),
      Frequency = c(tmp_PM[[1]], tmp_IM[[1]], tmp_NM, tmp_UM[[1]])
    ) %>%
      mutate(phenotype = fct_inorder(phenotype))
  }
# CYP3A5 ------------------------------------------------------------------
  if (unique(x$gene) == "CYP3A5") {

    tmp_NM <- x %>%
      filter(grepl("normal", functional_status)) %>%
      mutate(ff = as.character(functional_status)) %>%
      split(.$ff) %>%
      janitor::clean_names()

    if (length(tmp_NM) != 1) {
      tmp_NM <- 0
    } else {
      tmp_NM <- tmp_NM %>%
        map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
        janitor::clean_names()
      tmp_NM <- tmp_NM$normal_function * tmp_NM$normal_function
    }

    tmp_PM <- x %>%
      filter(grepl("no f", functional_status)) %>%
      mutate(ff = as.character(functional_status)) %>%
      split(.$ff) %>%
      janitor::clean_names()

    if (length(tmp_NM) != 1) {
      tmp_PM <- 0
    } else {
      tmp_PM <- tmp_PM %>%
        map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
        janitor::clean_names()
      tmp_PM <- tmp_PM$no_function * tmp_PM$no_function
    }

    tmp_IM <- x %>%
      filter(grepl("no f|norm", functional_status)) %>%
      mutate(ff = as.character(functional_status)) %>%
      split(.$ff) %>%
      janitor::clean_names()

    if (length(tmp_IM) != 2) {
      tmp_IM <- 0
    } else {
      tmp_IM <- tmp_IM %>%
        map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
        janitor::clean_names()
      tmp_IM <- 2 * tmp_IM$no_function * tmp_IM$normal_function
    }

    res <- tibble(
      phenotype = c("PM", "IM", "NM"),
      Frequency = c(tmp_PM[[1]], tmp_IM[[1]], tmp_NM[[1]])
    ) %>%
      mutate(phenotype = fct_inorder(phenotype))
  }
  # CYP2B6 ------------------------------------------------------------------
  if (unique(x$gene) == "CYP2B6") {
    tmp_raw <- x %>%
      split(as.character(.$functional_status)) %>%
      map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
      janitor::clean_names()

    tmp_UM <- tmp_raw[grepl("increased", names(tmp_raw))]
    if (length(tmp_UM) != 1) {
      tmp_UM <- 0
    } else {
      tmp_UM <- tmp_UM$increased_function * tmp_UM$increased_function
    }

    tmp_RM <- tmp_raw[grepl("increased|norm", names(tmp_raw))]
    if (length(tmp_RM) != 2) {
      tmp_RM <- 0
    } else {
      tmp_RM <- 2 * tmp_RM$increased_function * tmp_RM$normal_function
    }

    tmp_IM <- tmp_raw[grepl("increased|norm|no_|decr", names(tmp_raw))]
    if (length(tmp_IM) != 4) {
      tmp_IM <- 0
    } else {
      tmp_IM <- 2 * tmp_IM$normal_function * tmp_IM$decreased_function +
        2 * tmp_IM$normal_function * tmp_IM$no_function +
        2 * tmp_IM$increased_function * tmp_IM$decreased_function +
        2 * tmp_IM$increased_function * tmp_IM$no_function
    }

    tmp_PM <- tmp_raw[grepl("no_|decr", names(tmp_raw))]
    if (length(tmp_PM) != 2) {
      tmp_PM <- 0
    } else {
      tmp_PM <- tmp_PM$decreased_function * tmp_PM$decreased_function +
        tmp_PM$no_function * tmp_PM$no_function +
        2 * tmp_PM$decreased_function * tmp_PM$no_function
    }


    tmp_NM <- 1 - sum(c(tmp_PM[[1]], tmp_IM[[1]], tmp_RM[[1]], tmp_UM[[1]]))
    res <- tibble(
      phenotype = c("PM", "IM", "NM", "RM", "UM"),
      Frequency = c(tmp_PM[[1]], tmp_IM[[1]], tmp_NM, tmp_RM[[1]], tmp_UM[[1]])
    ) %>%
      mutate(phenotype = fct_inorder(phenotype))
  } 
  # CYP2C19 -----------------------------------------------------------------
  if (unique(x$gene) == "CYP2C19") {
    tmp_raw <- x %>%
      split(as.character(.$functional_status)) %>%
      map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
      janitor::clean_names()


    tmp_UM <- tmp_raw[grepl("increased", names(tmp_raw))]
    if (length(tmp_UM) != 1) {
      tmp_UM <- 0
    } else {
      tmp_UM <- tmp_UM$increased_function * tmp_UM$increased_function
    }

    tmp_RM <- tmp_raw[grepl("increased|normal", names(tmp_raw))]
    if (length(tmp_RM) != 2) {
      tmp_RM <- 0
    } else {
      tmp_RM <- 2 * tmp_RM$increased_function * tmp_RM$normal_function
    }

    tmp_IM <- tmp_raw[grepl("decreased|normal|no_|increased", names(tmp_raw))]
    if (length(tmp_IM) != 4) {
      tmp_IM <- 0
    } else {
      tmp_IM <- 2 * tmp_IM$normal_function * tmp_IM$decreased_function +
        2 * tmp_IM$normal_function * tmp_IM$no_function +
        2 * tmp_IM$increased_function * tmp_IM$decreased_function +
        2 * tmp_IM$increased_function * tmp_IM$no_function +
        tmp_IM$decreased_function * tmp_IM$decreased_function
    }

   tmp_PM <- tmp_raw[grepl("decreased|no_", names(tmp_raw))]
    if (length(tmp_PM) != 2) {
      tmp_PM <- 0
    } else {
      tmp_PM <- tmp_PM$no_function * tmp_PM$no_function +
        2 * tmp_PM$decreased_function * tmp_PM$no_function
    }

    tmp_NM <- 1 - sum(c(tmp_PM[[1]], tmp_IM[[1]], tmp_RM[[1]], tmp_UM[[1]]))

    res <- tibble(
      phenotype = c("PM", "IM", "NM", "RM", "UM"),
      Frequency = c(tmp_PM[[1]], tmp_IM[[1]], tmp_NM, tmp_RM[[1]], tmp_UM[[1]])
    ) %>%
      mutate(phenotype = fct_inorder(phenotype))
  } # cyp2c19
# CYP2C9 ------------------------------------------------------------------
  if (unique(x$gene) == "CYP2C9") {
    tmp_raw <- x %>%
      split(as.character(.$functional_status)) %>%
      map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
      janitor::clean_names()


    tmp_IM <- tmp_raw[grepl("normal|decreased|no_", names(tmp_raw))]
    if (length(tmp_IM) != 3) {
      tmp_IM <- 0
    } else {
      tmp_IM <- 2 * tmp_IM$normal_function * tmp_IM$decreased_function +
        2 * tmp_IM$normal_function * tmp_IM$no_function +
        tmp_IM$decreased_function * tmp_IM$decreased_function
    }

    tmp_PM <- tmp_raw[grepl("decreased|no_", names(tmp_raw))]
    if (length(tmp_PM) != 2) {
      tmp_PM <- 0
    } else {
      tmp_PM <- 2 * tmp_PM$decreased_function * tmp_PM$no_function +
        tmp_PM$no_function * tmp_PM$no_function
    }

    tmp_NM <- 1 - sum(c(tmp_PM[[1]], tmp_IM[[1]]))

    res <- tibble(
      phenotype = c("PM", "IM", "NM"),
      Frequency = c(tmp_PM[[1]], tmp_IM[[1]], tmp_NM)
    ) %>%
      mutate(phenotype = fct_inorder(phenotype))
  }
# UGT1A1 ------------------------------------------------------------------
  if (unique(x$gene) == "UGT1A1") {
    tmp_raw <- x %>%
      split(as.character(.$functional_status)) %>%
      map(summarise, sum_freq = sum(Weighted, na.rm = T)) %>%
      janitor::clean_names()


    tmp_PM <- tmp_raw[grepl("decreas", names(tmp_raw))]
    if (length(tmp_PM) != 1) {
      tmp_PM <- 0
    } else {
      tmp_PM <- tmp_PM$decreased_function * tmp_PM$decreased_function
    }

    tmp_IM <- tmp_raw[grepl("increased|decreased|normal", names(tmp_raw))]
    if (length(tmp_IM) != 3) {
      tmp_IM <- 0
    } else {
      tmp_IM <- 2 * tmp_IM$normal_function * tmp_IM$decreased_function +
        2 * tmp_IM$increased_function * tmp_IM$decreased_function
    }

    tmp_NM <- 1 - sum(c(tmp_PM[[1]], tmp_IM[[1]]))

    res <- tibble(
      phenotype = c("PM", "IM", "NM"),
      Frequency = c(tmp_PM[[1]], tmp_IM[[1]], tmp_NM)
    ) %>%
      mutate(phenotype = fct_inorder(phenotype))
  } 
  res
}
