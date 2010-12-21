{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
           , ParallelListComp
           , PostfixOperators
  #-}


module Language.TaskL.IdemShell where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Applicative
import Data.Monoid

import Data.ByteString.Char8

import Language.TaskL.IdemShell.PasswdDB
import Language.TaskL.IdemShell.Path
import Language.TaskL.Combination
import Language.TaskL.EncDec


data Command                 =  CHOWN Path Ownership
                             |  CHMOD Path Mode
                             |  RM Path
                             |  CP Path Path
                             |  LNs Path Path
                             |  TOUCH Path
                             |  MKDIR Path
                             |  USERADD UNick UserAttrs
                             |  USERDEL UNick
                             |  GROUPADD GNick GroupAttrs
                             |  GROUPDEL GNick
                             |  GPASSWDm GNick (Set UNick) (Set UNick)
deriving instance Eq Command
deriving instance Show Command
instance Combine Command where
  combine a b
    | a == b                 =  Combined a
    | conflictDirFile a b    =  Contradictory a b
    | conflictDirFile b a    =  Contradictory a b
    | otherwise              =  merge a b


data Test                    =  LSo Path Ownership
                             |  LSm Path Mode
                             |  DASHe Path
                             |  DASH_ NodeType Path
                             |  DIFFq Path Path
                             |  LSl Path Path
                             |  GETENTu UNick
                             |  GETENTg GNick
                             |  GROUPS UNick GNick
                             |  Not Test
                             |  And Test Test
                             |  Or Test Test
                             |  TRUE
                             |  FALSE
deriving instance Show Test
instance Eq Test where
  LSo p0 o0 == LSo p1 o1     =  p0 == p1 && o0 == o1
  LSm p0 m0 == LSm p1 m1     =  p0 == p1 && m0 == m1
  DASHe p0 == DASHe p1       =  p0 == p1
  DASH_ a x == DASH_ b y     =  a == b && x == y
  DIFFq a x == DIFFq b y     =  a == b && x == y
  LSl p0 p0' == LSl p1 p1'   =  p0 == p1 && p0' == p1'
  GETENTu u0 == GETENTu u1   =  u0 == u1
  GETENTg g0 == GETENTg g1   =  g0 == g1
  GROUPS a x == GROUPS b y   =  a == b && x == y
  Not t0 == Not t1           =  t0 == t1
  And TRUE t0 == t1          =  t0 == t1
  And t0 TRUE == t1          =  t0 == t1
  t0 == And TRUE t1          =  t0 == t1
  t0 == And t1 TRUE          =  t0 == t1
  And t0 t0' == And t1 t1'   =  t0 == t1 && t0' == t1'
  Or FALSE t0 == t1          =  t0 == t1
  Or t0 FALSE == t1          =  t0 == t1
  t0 == Or FALSE t1          =  t0 == t1
  t0 == Or t1 FALSE          =  t0 == t1
  Or t0 t0' == Or t1 t1'     =  t0 == t1 && t0' == t1'
  TRUE == TRUE               =  True
  FALSE == FALSE             =  True
  _ == _                     =  False
instance Monoid Test where
  mempty                     =  FALSE
  mappend FALSE x            =  x
  mappend x FALSE            =  x
  mappend x y                =  Or x y


data NodeType                =  File
                             |  Directory
                             |  Symlink
deriving instance Eq NodeType
deriving instance Show NodeType

nodeTest                    ::  NodeType -> ByteString
nodeTest File                =  "-f"
nodeTest Directory           =  "-d"
nodeTest Symlink             =  "-L"


data Ownership               =  Both User Group
                             |  OnlyUser User
                             |  OnlyGroup Group
deriving instance Eq Ownership
deriving instance Show Ownership
instance Combine Ownership where
  combine a b                =  case (a, b) of
    (Both u _   , OnlyUser u' ) | u == u'     ->  Combined a
                                | otherwise   ->  Contradictory a b
    (Both _ g   , OnlyGroup g') | g == g'     ->  Combined a
                                | otherwise   ->  Contradictory a b
    (OnlyUser u , Both u' _   ) | u == u'     ->  Combined b
                                | otherwise   ->  Contradictory a b
    (OnlyUser u , OnlyGroup g')               ->  Combined (Both u g')
    (OnlyGroup g, Both _ g'   ) | g == g'     ->  Combined b
                                | otherwise   ->  Contradictory a b
    (OnlyGroup g, OnlyUser u' )               ->  Combined (Both u' g)
    (_                        )               ->  if a == b
                                                    then Combined a
                                                    else Contradictory a b

-- |    This is actually wrong; final colon means something to @chown@.
chownStyle                  ::  Ownership -> ByteString
chownStyle (Both u g)        =  enc u `append` ":" `append` enc g
chownStyle (OnlyUser u)      =  enc u `append` ":"
chownStyle (OnlyGroup g)     =  ":" `append` enc g


data Mode                    =  Mode TriState -- ^ User read bit.
                                     TriState -- ^ User write bit.
                                     TriState -- ^ User execute bit.
                                     TriState -- ^ Set UID bit.
                                     TriState -- ^ Group read bit.
                                     TriState -- ^ Group write bit.
                                     TriState -- ^ Group execute bit.
                                     TriState -- ^ Set GID bit.
                                     TriState -- ^ Other read bit.
                                     TriState -- ^ Other write bit.
                                     TriState -- ^ Other execute bit.
                                     TriState -- ^ Sticky bit.
deriving instance Eq Mode
deriving instance Show Mode
instance Combine Mode where
  combine a@(Mode ur  uw  ux  us  gr  gw  gx  gs  or  ow  ox  ot )
          b@(Mode ur' uw' ux' us' gr' gw' gx' gs' or' ow' ox' ot') =
    case combinations of
      [ Combined ur_, Combined uw_, Combined ux_, Combined us_,
        Combined gr_, Combined gw_, Combined gx_, Combined gs_,
        Combined or_, Combined ow_, Combined ox_, Combined ot_ ]
         -> Combined (Mode ur_ uw_ ux_ us_ gr_ gw_ gx_ gs_ or_ ow_ ox_ ot_)
      _                     ->  Contradictory a b
   where
    combinations =
      [ combine x y | x <- [ur, uw, ux, us, gr, gw, gx, gs, or, ow, ox, ot ]
                    | y <- [ur',uw',ux',us',gr',gw',gx',gs',or',ow',ox',ot'] ]

data TriState                =  On | Indifferent | Off
deriving instance Eq TriState
deriving instance Show TriState
instance Combine TriState where
  combine On Off             =  Contradictory On Off
  combine On _               =  Combined On
  combine Off On             =  Contradictory Off On
  combine Off _              =  Combined Off
  combine _   _              =  Combined Indifferent


data UserAttrs               =  UserAttrs
deriving instance Eq UserAttrs
deriving instance Show UserAttrs

data GroupAttrs              =  GroupAttrs
deriving instance Eq GroupAttrs
deriving instance Show GroupAttrs


{-| Test commands that will assuredly exit 0 after the command is run.
 -}
essentialTest               ::  Command -> Test
essentialTest thing          =  case thing of
  CHOWN p o                 ->  LSo p o
  CHMOD p m                 ->  LSm p m
  RM p                      ->  Not (DASHe p)
  CP p' p                   ->  Not (DIFFq p' p)
  LNs p' p                  ->  DASH_ Symlink p `And` LSl p' p
  TOUCH p                   ->  DASH_ File p
  MKDIR p                   ->  DASH_ Directory p
  USERADD nick _            ->  GETENTu nick
  USERDEL nick              ->  (Not . GETENTu) nick
  GROUPADD nick _           ->  GETENTg nick
  GROUPDEL nick             ->  (Not . GETENTg) nick
  GPASSWDm gNick inU outU   ->  case terms of [ ] -> TRUE
                                              h:t -> List.foldl' And h t
   where
    terms                    =  smap (`GROUPS` gNick) inU
                            ++  smap (Not . (`GROUPS` gNick)) outU
    smap f                   =  List.map f . Set.toList


label                       ::  Command -> ByteString
label thing                  =  case thing of
  CHOWN p _                 ->  "fs/own:" `append` enc p
  CHMOD p _                 ->  "fs/mode:" `append` enc p
  RM p                      ->  "fs/node:" `append` enc p
  CP _ p                    ->  "fs/node:" `append` enc p
  LNs _ p                   ->  "fs/node:" `append` enc p
  TOUCH p                   ->  "fs/node:" `append` enc p
  MKDIR p                   ->  "fs/node:" `append` enc p
  USERADD nick _            ->  "pw/user:" `append` enc nick
  USERDEL nick              ->  "pw/user:" `append` enc nick
  GROUPADD nick _           ->  "pw/group:" `append` enc nick
  GROUPDEL nick             ->  "pw/group:" `append` enc nick
  GPASSWDm nick _ _         ->  "pw/members:" `append` enc nick


--  Use GADTs for this later.
merge                       ::  Command -> Command -> Combination Command
merge a@(CHOWN p0 o0) b      =  case b of
  CHOWN p1 o1 | p0 /= p1    ->  Separate a b
              | otherwise   ->  case combine o0 o1 of
                                  Combined o  ->  Combined (CHOWN p0 o)
                                  _           ->  Contradictory a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge a@(CHMOD p0 m0) b      =  case b of
  CHMOD p1 m1 | p0 /= p1    ->  Separate a b
              | otherwise   ->  case combine m0 m1 of
                                  Combined m  ->  Combined (CHMOD p0 m)
                                  _           ->  Contradictory a b
  RM _                      ->  merge b a
  LNs _ p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge a@(RM p0) b            =  case b of
  CHOWN p1 _                ->  if p0 </? p1 then Contradictory a b
                                             else Separate a b
  CHMOD p1 _                ->  if p0 </? p1 then Contradictory a b
                                             else Separate a b
  CP s1 p1                  ->  if p0 </? p1 || p0 </? s1
                                  then Contradictory a b
                                  else Separate a b
  LNs p' p1                 ->  if p0 </? p1 || p0 </? p'
                                  then  Contradictory a b
                                  else  Separate a b
  TOUCH p1                  ->  if p0 </? p1 then Contradictory a b
                                             else Separate a b
  MKDIR p1                  ->  if p0 </? p1 then Contradictory a b
                                             else Separate a b
  _                         ->  Separate a b
merge a@(CP _ p0) b          =  case b of
  CP _ p1                   ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge a@(LNs _ p0) b         =  case b of
  LNs _ p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge a@(TOUCH p0) b         =  case b of
  TOUCH p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  MKDIR p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge a@(MKDIR p0) b         =  case b of
  TOUCH _                   ->  merge b a
  MKDIR p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge a@(USERADD u0 _) b     =  case b of
  USERADD u1 _              ->  if u0 == u1 then Contradictory a b
                                            else Separate a b
  USERDEL u1                ->  if u0 == u1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge a@(USERDEL u0) b       =  case b of
  USERADD u1 _              ->  if u0 == u1 then Contradictory a b
                                         else Separate a b
  USERDEL u1                ->  if u0 == u1 then Contradictory a b
                                         else Separate a b
  _                         ->  Separate a b
merge a@(GROUPADD g0 _) b    =  case b of
  GROUPADD g1 _             ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  GROUPDEL g1               ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge a@(GROUPDEL g0) b      =  case b of
  GROUPADD g1 _             ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  GROUPDEL g1               ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  GPASSWDm g1 _ _           ->  if g0 == g1 then Contradictory a b
                                            else  Separate a b
  _                         ->  Separate a b
merge a@(GPASSWDm g0 x y) b  =  case b of
  GPASSWDm g1 x' y'         ->  if not (g0 == g1)
                                  then  Separate a b
                                  else  if not (nointer x y' && nointer y x')
                                          then  Contradictory a b
                                          else  Combined (GPASSWDm g0
                                                           (Set.union x x')
                                                           (Set.union y y'))
                                 where
                                  nointer = (Set.null .) . Set.intersection
  _                         ->  merge b a


impliedDirectories          ::  Command -> [Path]
impliedDirectories thing     =  case thing of
   CHOWN p _                ->  [(p -/)]
   CHMOD p _                ->  [(p -/)]
   CP p' p                  ->  [(p -/), (p' -/)]
   LNs p' p                 ->  [(p -/), (p' -/)]
   TOUCH p                  ->  [(p -/)]
   MKDIR p                  ->  [p]
   _                        ->  []

impliedFiles                ::  Command -> [Path]
impliedFiles thing           =  case thing of
   CP p' p                  ->  [p', p]
   TOUCH p                  ->  [p]
   _                        ->  []

conflictDirFile             ::  Command -> Command -> Bool
conflictDirFile a b = List.any (`List.any` impliedDirectories b) filesAbove
 where
  filesAbove                 =  (</?) <$> impliedFiles a

