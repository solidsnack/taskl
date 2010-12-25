{-# LANGUAGE OverloadedStrings
           , ParallelListComp
           , PostfixOperators
  #-}


module Language.TaskL.Codegen.IdemShell where

import Prelude hiding (concat)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (sort, foldl')

import Data.ByteString.Char8 hiding (map, foldl', filter)
import qualified Text.ShellEscape as Esc

import Language.TaskL.EncDec
import Language.TaskL.IdemShell
import Language.TaskL.IdemShell.Path
import Language.TaskL.Bash.Program (cmd)
import qualified Language.TaskL.Bash.Program as Program
import Language.TaskL.Codegen.Utils


{-| Class of objects that may be translated to Bash programs. Both 'Command'
    and 'Test' can have code generated from them.
 -}
class CodeGen t where
  codeGen                   ::  t -> Program.Term

instance CodeGen Command where
  codeGen command            =  case command of
    CHOWN p o               ->  cmd "idem_CHOWN"    [destP  p,  chownStyle o]
    CHMOD p m               ->  cmd "idem_CHMOD"    [destP  p, modeSymbolic m]
    RM p                    ->  cmd "idem_RM"       [destP  p]
    CP p' p                 ->  cmd "idem_CP"       [destP  p', destP p]
    LNs p' p                ->  cmd "idem_LNs"      [destP  p', destP p]
    TOUCH p                 ->  cmd "idem_TOUCH"    [destP  p]
    MKDIR p                 ->  cmd "idem_MKDIR"    [destP  p]
    USERADD u attrs         ->  cmd "idem_USERADD"  [escEnc u]--,  attrs]
    USERDEL u               ->  cmd "idem_USERDEL"  [escEnc u]
    GROUPADD g attrs        ->  cmd "idem_GROUPADD" [escEnc g]--,  attrs]
    GROUPDEL g              ->  cmd "idem_GROUPDEL" [escEnc g]
    GPASSWDm g inU outU     ->  cmd "idem_GPASSWDm" ( escEnc g : f '+' inU
                                                              ++ f '-' outU )
     where
      f char                 =  map (cons char . escEnc) . Set.toList

instance CodeGen Test where
  codeGen test               =  case collapse test of
    LSo p o                 ->  cmd "idem_LSo"      [destP p,   chownStyle o]
    LSm p m                 ->  cmd "idem_LSm"      [destP p,   fullModeRE m]
    DASHe p                 ->  cmd "idem_DASHe"    [destP p]
    DASH_ node p            ->  cmd "idem_DASH_"    [nodeTest node, destP p]
    DIFFq p' p              ->  cmd "idem_DIFFq"    [destP p',  destP p]
    LSl p' p                ->  cmd "idem_LSl"      [destP p',  destP p]
    GETENTu u               ->  cmd "idem_GETENTu"  [escEnc u]
    GETENTg g               ->  cmd "idem_GETENTg"  [escEnc g]
    GROUPS u g              ->  cmd "idem_GROUPS"   [escEnc u,  escEnc g]
    Not t                   ->  Program.Bang (codeGen' t)
    And t t'                ->  codeGen' t `Program.And` codeGen' t'
    Or t t'                 ->  codeGen' t `Program.Or` codeGen' t'
    TRUE                    ->  cmd "true"          []
    FALSE                   ->  cmd "false"         []
   where
    codeGen' t | flat t      =  codeGen t
               | otherwise   =  Program.Group (codeGen t)


{-| Remove redundant negations.
 -}
collapse                    ::  Test -> Test
collapse (Not (Not test))    =  collapse test
collapse test                =  test

{-| Is this a flat, simple test or one that needs grouping?
 -}
flat                        ::  Test -> Bool
flat (And _ _)               =  False
flat (Or _ _)                =  False
flat _                       =  True


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


data Four                    =  Four0 | Four1 | Four2 | Four3

data Three                   =  Three0 | Three1 | Three2


modeSymbolic                ::  Mode -> ByteString
modeSymbolic (Mode ur uw ux us gr gw gx gs or ow ox ot) = concatModes $
  foldl' f (("",""),("",""),("",""))
           [ (st, (ugo, bit)) | st <- [ur,uw,ux,us,gr,gw,gx,gs,or,ow,ox,ot]
                              | ugo <- [Three0,Three1,Three2]
                              , bit <- [Four0,Four1,Four2,Four3]            ]
 where
  f strings (st, (ugo, bit)) =
    appendModeBit st ugo strings (modeSymbol ugo bit)
  appendModeBit  On  ugo ( (u', u_), (g', g_), (o', o_) ) c = case ugo of
    Three0 -> ( (u' `snoc` c, u_), (g', g_),          (o', o_) )
    Three1 -> ( (u', u_),          (g' `snoc` c, g_), (o', o_) )
    Three2 -> ( (u', u_),          (g', g_),          (o' `snoc` c, o_) )
  appendModeBit  Off ugo ( (u', u_), (g', g_), (o', o_) ) c = case ugo of
    Three0 -> ( (u', u_ `snoc` c), (g', g_),          (o', o_) )
    Three1 -> ( (u', u_),          (g', g_ `snoc` c), (o', o_) )
    Three2 -> ( (u', u_),          (g', g_),          (o', o_ `snoc` c) )
  appendModeBit  Indifferent _   strings _ = strings
  showMode ugoC pair         =  concat $ case pair of
    ("","")                 ->  []
    (p ,"")                 ->  [ugo', p]
    ("", n)                 ->  [ugo_ , n]
    (p , n)                 ->  [ugo', p, ",", ugo_, n]
   where
    (ugo', ugo_)             =  (ugoC `cons` "+", ugoC `cons` "-")
  concatModes (u,g,o) = (intercalate "," . filter (/= "")) [ showMode 'u' u
                                                           , showMode 'g' g
                                                           , showMode 'o' o ]


modeSymbol                  ::  Three -> Four -> Char
modeSymbol Three0 Four0      =  'r'
modeSymbol Three0 Four1      =  'w'
modeSymbol Three0 Four2      =  'x'
modeSymbol Three0 Four3      =  's'
modeSymbol Three1 Four0      =  'r'
modeSymbol Three1 Four1      =  'w'
modeSymbol Three1 Four2      =  'x'
modeSymbol Three1 Four3      =  's'
modeSymbol Three2 Four0      =  'r'
modeSymbol Three2 Four1      =  'w'
modeSymbol Three2 Four2      =  'x'
modeSymbol Three2 Four3      =  't'


destP path                   =  if (path /?)
                                  then  "\"$T\"" `append` escEnc path
                                  else  escEnc path

