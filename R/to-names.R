
fix_to_names <-  function(z) {
    rownames(z) <- 
      reduce2(
      c(
      "surgspec = Plastics"
      , "pgy_bin ="
      , "smoke = Yes"
      , "hypermed = Yes"
      , "tothlos"
      , "inout = "
      , "asaclas"
      , "wndclas"
      , "cdarrest.*"
      , "cdmi.*"
      , "cnscoma.*"
      , "cnscva.*"
      , "neurodef.*"
      , "othbleed.*"
      , "othsysep.*"
      , "urninf.*"
      , "oprenaf.*"
      , "dehis = No Complication (%)"
      , "orgspcssi = No Complication (%)"
      , "zzzzzzzzzzzzzzzz"
      , "oupneumo = "
      , "reintub = "
      , "orgspcssi = "
      , "dehis = "
      , "pulembol = "
      ), 
      c(
      "Surgical Specialty: Plastics"
      , "PGY:"
      , "Smoker"
      , "Prescribed Antihypertensives"
      , "Hospital Length of Stay"
      , "" #--
      , "ASA Class"
      , "Would Classification"
      , "No Cardiac Arrest (%)"
      , "No MI (%)"
      , "No Coma (%)"
      , "No CVA (%)"
      , "No Neurological Deficit (%)"
      , "No Bleeding (%)"
      , "No Sepsis (%)"
      , "No UTI (%)"
      , "No Renal Failure (%)"
      , "No Wound Disruption (%)"
      , "No Organ SSI"
      , ""
      , ""
      , ""
      , ""
      , ""
      , ""
      ), 
      function(a, b, d) gsub(b, d, a),
      .init = rownames(z)
      )

  z
}

