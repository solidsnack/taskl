
module Language.TaskL.Override where


data Override msg t          =  Override msg (OverrideCore t)


data OverrideCore t          =  Accept t
                             |  Select (Override t) (Override t)
                             |  Keep (Override t) (Override t)
                             |  Merge t (Override t)

