{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeOperators
           , TypeFamilies
           , TupleSections
           , FlexibleInstances
           , FlexibleContexts
           , RecordWildCards
           , NoMonomorphismRestriction
           , UndecidableInstances #-}
module System.TaskL.Compiler where

import           Prelude hiding (catch, mapM, any, concatMap)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Control.Arrow
import           Control.Applicative
import           Control.Exception
import           Control.Monad hiding (mapM)
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Data.Char
import           Data.Either
import           Data.Foldable hiding (sequence_)
import           Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import           Data.Traversable
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid hiding (All)
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree
import qualified Text.Printf as Printf
import           System.Exit
import           System.IO

import qualified Data.Aeson as Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml
import qualified Text.Libyaml
import qualified Language.Bash as Bash

import           System.TaskL.Strings
import           System.TaskL.Task
import           System.TaskL.JSON


-- | The type of compiler phases, including, parsing, internal AST
--   transformations and the eventual compilation to Bash.
type a :- b = a -> Either Text b

-- | The type of compiler phases which perform IO, including loading files and
--   terminating the program.
type a :~ b = a -> IO b

-- | Provide a contextual error message prefix and transform a pure phase into
--   a side-effecting phase.
lift :: ByteString -> a :- b -> a :~ b
lift prefix f = either (err . (prefix <>) . Text.encodeUtf8) return . f

 ----------------------------- Compilation Steps ------------------------------

load :: [(ByteString, Handle)] :~ [Module]
load  = mapM loadYAML

merge :: [Module] :~ Module
merge modules = return Module{ from = froms, defs = map }
 where (froms, map) = foldl' f ("",mempty) modules
       f (s, map) Module{..} = (if s == "" then from
                                           else s <> "\n" <> from, map <> defs)

tasks :: [(Name, [ByteString])] -> Module :~ [Bash.Statement ()]
tasks requests Module{..} = do
  expanded <- lift (from <> ": ") (unify <=< mapM (down defs)) requests
  let (numberedForest, mapping) = index expanded
      reformatted = [ printf (Map.size mapping) <$> t | t <- numberedForest ]
      tasks = ann (statements (concatMap plan reformatted))
  return [Bash.Function "tasks" tasks, statements (arrays mapping)]
 where arrays :: Map (Name, [ByteString]) Int -> [Bash.Statement ()]
       arrays m = argv <$> Map.toAscList m
        where argv ((name, args), i) = Bash.Assign (Bash.Array v lits)
               where Just v = Bash.identifier ("argv" <> printf (Map.size m) i)
                     lits   = Bash.literal <$> (toStr name : args)
       plan :: Tree ByteString -> [Bash.Statement ()]
       plan (Node b forest) = cmd "enter"
                            : concatMap plan forest ++ [cmd "try", cmd "leave"]
        where cmd action = Bash.SimpleCommand action [Bash.literal b]
       printf w i = ByteString.pack (Printf.printf strFormat i)
        where strFormat = "%0" <> show (length $ show w) <> "d"

bodies :: [(Name, [ByteString])] -> Module :~ [Bash.Statement ()]
bodies requests Module{..} = lift' $ do
  needed   <- if missing /= mempty
              then Left $ Text.unwords ("Missing: ":(toStr <$> toList missing))
              else Right needed
  narrowed <- (defs !?) `mapM` Set.toList needed
  mapM (uncurry body) (List.sortBy (comparing fst) narrowed)
 where needed    = reachable requested defs
       requested = Set.fromList (fst <$> requests)
       missing   = requested `Set.difference` needed
       lift' m   = lift (from <> ": ") (const m) . const $ () -- Hmmmmmmm
       map !? k  = maybe (Left ("Missing: " <> toStr k)) (Right . (k,))
                         (Map.lookup k map)

bash :: [(Name, [ByteString])] -> Module :~ Bash.Annotated ()
bash  = undefined

 ------------------------------ Bash Generation -------------------------------

-- | Translate a named task to Bash.
body :: Name -> Task :- Bash.Statement ()
body name (Task vars Knot{..}) = do
  fname <- Bash.funcName (toStr name) !? ("Bad function name: " <> toStr name)
  return (Bash.Function fname (ann $ statements (initVars <> commands)))
 where initVars = set . cast <$> vars
       commands = cmd <$> argvs where Commands argvs = code
       (!?) :: Maybe t -> Text :- t
       (!?) m text = maybe (Left text) Right m
       cast (var, val) = (label2ident var, Bash.literal <$> val)
       -- This is fairly tricky. If the var is not set and there is no default,
       -- then it is an error; but if there is a default, we should use it and
       -- not call shift (which might result in script failure).
       set (var, val)          = maybe pop id (defaultTo <$> val)
        where pop              = local var (Bash.ReadVar "$1") --> shift
              defaultTo expr   = (Bash.IsSet "$1" &&> pop) ||> local var expr
              shift            = Bash.SimpleCommand "shift" []
              local ident expr = Bash.Local (Bash.Var ident expr)

cmd :: ([Either Label ByteString], [Arg]) -> Bash.Statement ()
cmd (command, args) = Bash.SimpleCommand (singleArgument command)
                                         [compoundArgument arg | arg <- args]

singleArgument :: [Either Label ByteString] -> Bash.Expression ()
singleArgument pieces = joined (expr <$> pieces)
 where expr (Left l)  = Bash.ReadVar (Bash.VarIdent (label2ident l))
       expr (Right b) = Bash.literal b
       joined [   ] = ""
       joined [ h ] = h
       joined (h:t) = Bash.Concat h (joined t)

compoundArgument :: Arg -> Bash.Expression ()
compoundArgument (Scalar simple) = singleArgument simple
compoundArgument Tail            = Bash.ARGVElements
compoundArgument All             = error "All is not implemented :("

label2ident :: Label -> Bash.Identifier
label2ident label = fromJust (Bash.identifier . ByteString.map f $ toStr label)
 where f c = if c == '-' then '_' else c

ann :: Bash.Statement () -> Bash.Annotated ()
ann stmt       = Bash.Annotated () stmt

(&&>),(||>),(-->):: Bash.Statement () -> Bash.Statement () -> Bash.Statement ()
stmt &&> stmt' = ann stmt `Bash.AndAnd`   ann stmt'
stmt ||> stmt' = ann stmt `Bash.OrOr`     ann stmt'
stmt --> stmt' = ann stmt `Bash.Sequence` ann stmt'

statements :: [Bash.Statement ()] -> Bash.Statement ()
statements [    ] = Bash.NoOp ""
statements [stmt] = stmt
statements (h:t)  = h --> statements t

 ---------------------- Work With Bindings & Request Lists --------------------

template :: Task -> [ByteString] :- Forest (Name, [ByteString])
template (Task vars Knot{..}) vals = do
  bound <- left insufficientE (binding vals vars)
  mapM (use bound `mapM`) deps
 where left f = either (Left . f) Right
       unboundE label = "Unbound variable: " <> toStr label
       insufficientE labels = Text.unwords
         ("Insufficient arguments; missing:" : (toStr <$> labels))
       use bound Use{..}  =  (task,) . mconcat
                         <$> left unboundE (mapM (bind bound) args)

down :: Map Name Task -> (Name, [ByteString]) :- Tree (Name, [ByteString])
down defs (task, args) = do
  body    <- maybe (left undef) Right (Map.lookup task defs)
  shallow <- either left return (template body args)
  deep    <- mapM (across (Map.delete task defs)) shallow
  return (Node (task, args) deep)
 where left :: Text -> Either Text t
       left  = Left . ((toStr task <> ": ") <>)
       undef = "No such task! (Or a cycle was found)."

across :: Map Name Task -> Tree (Name, [ByteString])
       :- Tree (Name, [ByteString])
across defs (Node (task, args) forest) = do
  Node _ forest' <- down defs (task, args)
  forest''       <- mapM (across (Map.delete task defs)) forest
  return (Node (task, args) (merge (forest' ++ forest'')))
 where merge things = [ Node a (merge b) | Node a b <- toForest merged ]
        where merged = Map.toList $ Map.fromListWith (<>) (unForest things)
              unForest f = [ (a, b)   | Node a b <- f ]
              toForest l = [ Node a b | (a, b)   <- l ]

-- | Bind vector of arguments to variables and return leftover variables (if
--   there are too few arguments) or arguments (if there are too few
--   variables).
binding :: [ByteString] -> [(Label, Maybe ByteString)]
        -> Either [Label] Binding
binding values variables = f [] values variables
 where f found (h:t) ((l, _)     :labels)    = f ((l, h):found) t  labels
       f found [   ] ((l, Just b):labels)    = f ((l, b):found) [] labels
       f _     [   ] labels@((_, Nothing):_) = Left (fst <$> labels)
       f found rest  [ ]                     = Right (Binding found rest)

bind :: Binding -> Arg -> Either Label [ByteString]
bind Binding{..} Tail        = Right rest
bind Binding{..} All         = Right ((snd <$> bound) <> rest)
bind Binding{..} (Scalar ss) = (:[]) . mconcat <$> mapM (either lk Right) ss
 where lk label = maybe (Left label) Right (List.lookup label bound)

data Binding = Binding { bound :: [(Label, ByteString)], rest :: [ByteString] }
 deriving (Eq, Ord, Show)

reachable :: Set Name -> Map Name Task -> Set Name
reachable requested available = search requested
 where onlyNames  = names <$> available
       subTasks k = maybe mempty id (Map.lookup k onlyNames)
       search :: Set Name -> Set Name
       search set = if next == set then set else search next
        where next = fold (set : (subTasks <$> toList set))

names :: Task -> Set Name
names (Task _ Knot{..}) = fold [ toSet tree | tree <- deps ]
 where toSet tree = Set.fromList $ Tree.flatten (task <$> tree)

 --------------------------------- Utilities ----------------------------------

-- | Functions for informational, diagnostic and warning messages published by
--   the compiler pipeline.
msg, out :: ByteString :~ ()
msg = echo stderr
out = echo stdout

-- | A way to signal an error in the compiler pipeline, terminating it and
--   providing a message.
err :: ByteString :~ any
err = (>> exitFailure) . echo stderr

echo :: Handle -> ByteString -> IO ()
echo h = ByteString.hPutStrLn h . fst . ByteString.spanEnd isSpace

 -------------------------------- Loading Files -------------------------------

openModule :: FilePath :~ (ByteString, Handle)
openModule path = (ByteString.pack path,) <$> openFile path ReadMode

loadYAML :: (ByteString, Handle) :~ Module
loadYAML (name, handle) = do
  parseResult <- yamlDecode =<< ByteString.hGetContents handle
  mapping     <- yamlProcessErrors parseResult
  case partitionEithers (partition <$> halfParse mapping) of
    ([      ], [  ]) -> return (Module name mempty)
    (warnings, [  ]) -> sequence_ warnings >> err' "No tasks were valid :("
    (warnings, defs) -> Module name (Map.fromList defs) <$ sequence_ warnings
 where
  err'       :: String -> IO any
  err'        = err . (<>) (name <> ": ") . ByteString.pack
  msg'       :: String -> IO ()
  msg'        = msg . (<>) (name <> ": ") . ByteString.pack
  halfParse o = ((Text.unpack &&& unStr) *** eitherJSON) <$> HashMap.toList o
  eitherJSON json = case Aeson.fromJSON json of
    Aeson.Error s   -> Left s
    Aeson.Success v -> Right v
  partition result = case result of
    ((k, Left  _), _)       -> Left $ msg' ("Bad task name: " <> k)
    ((k, Right _), Left s)  -> Left $ msg' ("Bad task body for: "<>k<>"\n "<>s)
    ((_, Right n), Right b) -> Right (n, b)
  yamlProcessErrors yamlProcessingResult = case yamlProcessingResult of
     Left exc                           -> err' (yamlErrorInfo exc)
     Right (Left s)                     -> err' ("YAML Error: " <> s)
     Right (Right (Data.Yaml.Object o)) -> return o
     Right (Right _)                    -> err' "YAML root should be a map."

yamlDecode :: ByteString -> IO (Either Data.Yaml.ParseException
                                       (Either String Aeson.Value))
yamlDecode  = Data.Yaml.decodeHelper . Text.Libyaml.decode

yamlErrorInfo :: Data.Yaml.ParseException -> String
yamlErrorInfo Data.Yaml.NonScalarKey =
  "Please use only scalar keys (numbers or strings)."
yamlErrorInfo (Data.Yaml.UnknownAlias alias) = "Missing alias: " <> alias
yamlErrorInfo (Data.Yaml.UnexpectedEvent a b) = unlines
  [ "Unexpected item in YAML event stream:", "  " <> show a,
    "while expecting:",                      "  " <> show b ]
yamlErrorInfo (Data.Yaml.InvalidYaml exc) = case exc of
  Nothing -> "The YAML was invalid in some way."
  Just (Text.Libyaml.YamlException s) -> "The YAML was invalid:\n  " <> s
  Just (Text.Libyaml.YamlParseException{..}) ->
    "The YAML was invalid (at " <> i yamlProblemMark <> "):\n  " <> yamlProblem
    where i Text.Libyaml.YamlMark{..} = show yamlLine <>":"<> show yamlColumn

 ------------------------------ Pretty Printing -------------------------------

renderModule :: (Aeson.ToJSON Module) => Module :~ ()
renderModule m@Module{..} = do when (from /= mempty) (out comment)
                               out (Data.Yaml.encode m)
 where comment = ByteString.unlines (("# " <>) <$> ByteString.lines from)

 --------- Graph utilities (many representations of graphs are used) ----------

-- | Ensure that edges in each tree are reflected in all trees.
unify :: (Ord t) => Forest t :- Forest t
unify forest | cyclic           = Left "Cycle detected."
             | Just vs <- roots = Right [ f <$> tree | tree <- Graph.dfs g vs ]
             | otherwise        = Left "Impossible error!"
 where (g, f, v) = graph . asMap $ concatMap adjacencies forest
       roots     = mapM v [ a | Node a _ <- forest ]
       cyclic    = any (/=[]) [ sub | Node _ sub <- Graph.scc g ]

-- | Transform a tree to an adjacency list representation of its edges.
--   Duplicate edges are retained.
adjacencies :: Tree t -> [(t, [t])]
adjacencies (Node t [ ]) = (t, []) : []
adjacencies (Node t sub) = (t, Tree.rootLabel <$> sub)
                         : concatMap adjacencies sub

-- | Merge an adjacency list represented as tuples to a representation backed
--   by maps of sets, eliminating duplicate edges.
asMap :: (Ord t) => [(t, [t])] -> Map t (Set t)
asMap  = (Set.fromList <$>) . Map.fromListWith (++)

-- | Convert from a 'Map' representation of adjacency lists to a list backed
--   representation.
unMap :: (Ord t) => Map t (Set t) -> [(t, [t])]
unMap  = Map.toAscList . (Set.toAscList <$>)

-- | Merge adjacency lists and produce a graph.
graph :: (Ord t) => Map t (Set t) -> (Graph, Vertex -> t, t -> Maybe Vertex)
graph map = (g, restore, v)
 where restore x = y where (y, _, _) = f x
       (g, f, v) = Graph.graphFromEdges
                   [ (main, main, deps) | (main, deps) <- unMap map ]

-- | Number all the unique nodes labels of a forest, replacing them with their
--   numbers and returning the mapping items to numbers.
index :: forall t. (Ord t) => Forest t -> (Forest Int, Map t Int)
index forest = State.runState (mapM (traverse number) forest) mempty
 where number :: t -> State (Map t Int) Int
       number t = maybe (insert t) return =<< State.gets (Map.lookup t)
       insert t = do fresh <- State.gets Map.size
                     State.modify (Map.insert t fresh)
                     return fresh

