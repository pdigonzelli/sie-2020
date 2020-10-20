  FIELD Region AS CHARACTER FORMAT "x(30)" LABEL "Region"~
  FIELD Puerto AS CHARACTER FORMAT "x(25)" LABEL "Puerto"~
  FIELD id_region LIKE r_lugdes_region.id_region VALIDATE ~
  FIELD id_lugdes_region LIKE r_lugdes_region.id_lugdes VALIDATE 
