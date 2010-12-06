{-# LANGUAGE OverloadedStrings
  #-}


module System.TaskL.Bash.Codegen.IdemShell where

import Prelude hiding (concat)
import Data.List (sort, nub)

import Data.ByteString hiding (map)
import qualified Text.ShellEscape as Esc

import Data.ByteString.EncDec
import System.TaskL.IdemShell
import System.TaskL.IdemShell.Path
import System.TaskL.Bash.Program (cmd)
import qualified System.TaskL.Bash.Program as Program
import System.TaskL.Bash.Codegen.Utils


{-| Class of objects that may be translated to Bash programs. Both 'Command'
    and 'Test' can have code generated from them.
 -}
class CodeGen t where
  codeGen                   ::  t -> Program.Term

instance CodeGen Command where
  codeGen command            =  case command of
    CHOWN p o               ->  undefined
    CHMOD p m               ->  undefined
    RM p                    ->  undefined
    CP p' p                 ->  undefined
    LNs p' p                ->  undefined
    TOUCH p                 ->  undefined
    MKDIR p                 ->  undefined
    USERADD u attrs         ->  undefined
    USERDEL u               ->  undefined
    GROUPADD g attrs        ->  undefined
    GROUPDEL g              ->  undefined
    GPASSWDa g users        ->  undefined
    GPASSWDd g users        ->  undefined

instance CodeGen Test where
  codeGen test               =  case collapse test of
    LSo p o                 ->  cmd ["idem_LSo",     escEnc p,   chownStyle o]
    LSm p m                 ->  cmd ["idem_LSm",     escEnc p,   fullModeRE m]
    DASHe p                 ->  cmd ["idem_DASHe",   escEnc p]
    DASH_ node p            ->  cmd ["idem_DASH_",   nodeTest node, escEnc p]
    DIFFq p' p              ->  cmd ["idem_DIFFq",   escEnc p',  escEnc p]
    LSl p' p                ->  cmd ["idem_LSl",     escEnc p',  escEnc p]
    GETENTu u               ->  cmd ["idem_GETENTu", escEnc u]
    GETENTg g               ->  cmd ["idem_GETENTg", escEnc g]
    GROUPS u g              ->  cmd ["idem_GROUPS",  escEnc u,   escEnc g]
    Not t                   ->  Program.Bang (codeGen t)
    And t t'                ->  codeGen t `Program.And` codeGen t'
    Or t t'                 ->  codeGen t `Program.Or` codeGen t'
    TRUE                    ->  cmd ["true"]
    FALSE                   ->  cmd ["false"]


{-| Remove redundant negations.
 -}
collapse                    ::  Test -> Test
collapse (Not (Not test))    =  collapse test
collapse test                =  test


fullModeRE                  ::  Mode -> ByteString
fullModeRE (Mode ur uw ux us gr gw gx gs or ow ox ot) =
  (concat . map (flippedModeRE ur uw ux us gr gw gx gs or ow ox ot))
    [Nine0,Nine1,Nine2,Nine3,Nine4,Nine5,Nine6,Nine7,Nine8]
 where
  flippedModeRE ur  uw  ux  us  gr  gw  gx  gs  or  ow  ox  ot  idx =
         modeRE idx ur  uw  ux  us  gr  gw  gx  gs  or  ow  ox  ot


data Nine = Nine0 | Nine1 | Nine2 | Nine3 | Nine4 | Nine5 | Nine6
          | Nine7 | Nine8


modeRE :: Nine -> TriState -> TriState -> TriState -> TriState
               -> TriState -> TriState -> TriState -> TriState
               -> TriState -> TriState -> TriState -> TriState -> ByteString
modeRE    Nine0    On  _   _   _    _   _   _   _    _   _   _   _   =  "r"
modeRE    Nine0    Off _   _   _    _   _   _   _    _   _   _   _   =  "-"
modeRE    Nine0    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine1    _   On  _   _    _   _   _   _    _   _   _   _   =  "w"
modeRE    Nine1    _   Off _   _    _   _   _   _    _   _   _   _   =  "-"
modeRE    Nine1    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine2    _   _   On  On   _   _   _   _    _   _   _   _   =  "s"
modeRE    Nine2    _   _   On  Off  _   _   _   _    _   _   _   _   =  "x"
modeRE    Nine2    _   _   On  _    _   _   _   _    _   _   _   _   =  "[sx]"
modeRE    Nine2    _   _   Off On   _   _   _   _    _   _   _   _   =  "S"
modeRE    Nine2    _   _   Off Off  _   _   _   _    _   _   _   _   =  "-"
modeRE    Nine2    _   _   Off _    _   _   _   _    _   _   _   _   =  "[sS]"
modeRE    Nine2    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine3    _   _   _   _    On  _   _   _    _   _   _   _   =  "r"
modeRE    Nine3    _   _   _   _    Off _   _   _    _   _   _   _   =  "-"
modeRE    Nine3    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine4    _   _   _   _    _   On  _   _    _   _   _   _   =  "w"
modeRE    Nine4    _   _   _   _    _   Off _   _    _   _   _   _   =  "-"
modeRE    Nine4    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine5    _   _   _   _    _   _   On  On   _   _   _   _   =  "s"
modeRE    Nine5    _   _   _   _    _   _   On  Off  _   _   _   _   =  "x"
modeRE    Nine5    _   _   _   _    _   _   On  _    _   _   _   _   =  "[sx]"
modeRE    Nine5    _   _   _   _    _   _   Off On   _   _   _   _   =  "S"
modeRE    Nine5    _   _   _   _    _   _   Off Off  _   _   _   _   =  "-"
modeRE    Nine5    _   _   _   _    _   _   Off _    _   _   _   _   =  "[sS]"
modeRE    Nine5    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine6    _   _   _   _    _   _   _   _    On  _   _   _   =  "r"
modeRE    Nine6    _   _   _   _    _   _   _   _    Off _   _   _   =  "-"
modeRE    Nine6    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine7    _   _   _   _    _   _   _   _    _   On  _   _   =  "w"
modeRE    Nine7    _   _   _   _    _   _   _   _    _   Off _   _   =  "-"
modeRE    Nine7    _   _   _   _    _   _   _   _    _   _   _   _   =  "."
modeRE    Nine8    _   _   _   _    _   _   _   _    _   _   On  On  =  "t"
modeRE    Nine8    _   _   _   _    _   _   _   _    _   _   On  Off =  "x"
modeRE    Nine8    _   _   _   _    _   _   _   _    _   _   On  _   =  "[tx]"
modeRE    Nine8    _   _   _   _    _   _   _   _    _   _   Off On  =  "T"
modeRE    Nine8    _   _   _   _    _   _   _   _    _   _   Off Off =  "-"
modeRE    Nine8    _   _   _   _    _   _   _   _    _   _   Off _   =  "[tT]"
modeRE    Nine8    _   _   _   _    _   _   _   _    _   _   _   _   =  "."



