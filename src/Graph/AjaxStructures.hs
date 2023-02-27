{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Graph.AjaxStructures where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word8)
import Debug.Trace
import GHC.Generics
import Graph.IntMap (Edge8 (..), EdgeAttribute (..), ExtractNodeType (..), Graph (..))

type UINode = Word32

type Node = Word32

-- A Graph consists of nodes and edges, graph drawing arranges it from left to right,
-- start to end. This layed out graph has a center (column number) over which the
-- search masks filters
type CGraph = Graph UINodeLabel [UIEdge]

type CGraphL = (Graph UINodeLabel [UIEdge], Map Node (Int, Int))

data UIGraph = UIGraph CGraph ExpandPath

data UINodeLabel = UINodeLabel
  { uinode :: SpecialNode,
    nestingFeatures :: Maybe LayerFeatures,
    verticalNumber :: Maybe Word32 -- we want to keep the order of vertically connected Nodes
    -- (e.g. in options), so we enumerate them during construction
    -- and examine them during graph drawing, when reducing crossings
  }
  deriving (Show, Generic)

type Channel = Int -- The nth type of a type node
-- This is path of function and type nodes with spaces that can be filled with options

type ExpandPath = [InoutNodes]

data InoutNodes = InoutNodes [UINode] [UINode]

data UIEdge = UIEdge
  { lineClass :: Text,
    strokeWidth :: Maybe Int,
    opacity :: Maybe Double,
    dashArray :: Maybe Text,
    lineType :: Maybe LineType,
    -- | commercial function, open source, is the function side effect generating (violet)
    lineColor :: Maybe Text,
    lineName :: Text,
    channelNrIn :: Maybe Channel, -- arg nr x of constructor node

    -- | Me, Colleague-Name to remeber past decisions, but only people you know,
    -- or company, group
    channelNrOut :: Channel, -- function output is connected with input nr x of type node
    edgeType :: EdgeType
  }
  deriving (Show, Generic, Eq, Ord)

vertBit :: Word8
vertBit = 0x1

virtBit :: Word8
virtBit = 0x2

type GraphMoveX = Int

type Column = (GraphMoveX, [UINode])

type Opacity = Double

data Option
  = NoOption Opacity
  | ExpansionOption -- option node that was the result of expanding a type
  | ExpansionStartType -- no filter arrow when input/ouput types are all the same
  | OnlyOption -- option node that was the result of searching
  -- (not connected to previous nodes)
  | Expandable Opacity -- show a plus sign
  | Expanded Opacity -- show a minus sign
  deriving (Show, Generic)

instance FromJSON Option

instance ToJSON Option

isOption :: Option -> Bool
isOption ExpansionOption = True
isOption ExpansionStartType = True
isOption OnlyOption = True
isOption _ = False

instance Eq UINodeLabel where
  node1 == node2 = uinode node1 == uinode node2

instance Enum UINodeLabel where
  toEnum n = UINodeLabel (DN (DummyNode n)) Nothing Nothing
  fromEnum (UINodeLabel _ _ _) = 0

instance ExtractNodeType UINodeLabel where
  extractNodeType (UINodeLabel sp _ _) = showAbbrev sp

data LayerFeatures = LayerFeatures
  { layer :: Int, -- Expanding a fuction generates a new layer
  -- (maybe make every new layer a little bit darker)
    border :: Maybe Border -- To set the right css values (border, boxshadow)
  }
  deriving (Show, Generic)

instance FromJSON LayerFeatures

instance ToJSON LayerFeatures

data Border
  = LeftBorder
  | RightBorder
  | TopBorder
  | BottomBorder
  | LeftTopBorder
  | RightTopBorder
  | LeftBottomBorder
  | RightBottomBorder
  deriving (Show, Generic)

instance FromJSON Border

instance ToJSON Border

isDummyLabel :: UINodeLabel -> Bool
isDummyLabel (UINodeLabel (DN _) _ _) = True
isDummyLabel _ = False

isFuncLabel :: UINodeLabel -> Bool
isFuncLabel (UINodeLabel (FuN _) _ _) = True
isFuncLabel _ = False

isCaseLabel :: UINodeLabel -> Bool
isCaseLabel (UINodeLabel (Case _) _ _) = True
isCaseLabel _ = False

isLitLabel :: UINodeLabel -> Bool
isLitLabel (UINodeLabel (LIT _) _ _) = True
isLitLabel _ = False

isTyLabel :: UINodeLabel -> Bool
isTyLabel (UINodeLabel (TN _) _ _) = True
isTyLabel _ = False

isArgLabel :: UINodeLabel -> Bool
isArgLabel (UINodeLabel (AN _) _ _) = True
isArgLabel _ = False

isMainArgLabel :: UINodeLabel -> Bool
isMainArgLabel (UINodeLabel (AN (ArgNode _ref _name _nr _isv isMainArg _sel)) _ _) = isMainArg
isMainArgLabel _ = False

isConnLabel :: UINodeLabel -> Bool
isConnLabel (UINodeLabel (CN _) _ _) = True
isConnLabel _ = False

data SpecialNode
  = FuN FunctionNode
  | Case CaseNode
  | LAM LambdaNode
  | LIT LiteralNode
  | TN [TypeNode]
  | DN DummyNode
  | CN ConnectionNode
  | AN ArgNode
  | MN MetaNodeRef
  | ST StateNode
  deriving (Eq, Ord, Show, Generic)

showAbbrev :: SpecialNode -> String
showAbbrev (FuN _) = "F"
showAbbrev (ST _) = "S"
showAbbrev (Case _) = "Case"
showAbbrev (LIT _) = "Lit"
showAbbrev (LAM _) = "Lam"
showAbbrev (TN _) = "Type"
showAbbrev (DN _) = "Dummy"
showAbbrev (CN _) = "Conn"
showAbbrev (AN _) = "Arg"
showAbbrev (MN _) = "Meta"

tn :: SpecialNode -> [TypeNode]
tn (TN typeNodes) = typeNodes
tn _ = [] -- error ("tn" ++ (show (nName n)))

data DummyNode = DummyNode {dsize :: Int} deriving (Eq, Ord, Show, Generic)

data ConnectionNode = ConnectionNode {size :: Int} deriving (Eq, Ord, Show, Generic)

data TypeNode = TypeNode
  { typeName :: Text,
    tyExample :: Text,
    tyExampleImage :: BasicSVG,
    typeMarked :: Bool, -- A type that was marked means:
    -- Optional functions at the in- or output are displayed
    typeCtrlMarked :: Bool, -- don't do anything yet
    typeSelected :: Bool, -- There is always one node selected, for navigation with the keyboard
    typeHidden :: Bool, --
    backendTypeNode :: Word32, -- Link to node of backend graph
    expandL :: Bool, -- When at the search not a function is clicked but a type, the type is single and
    -- we remember if it was input or output, to have a clue in which direction to expand
    editable :: Bool -- is it possible to enter example data like String, Int, Bool, ...
  }
  deriving (Eq, Ord, Show, Generic)

data FunctionNode = FunctionNode
  { functionUnique :: Text,
    functionName :: Text,
    functionType :: Text, -- maybe not displayed
    functionPackage :: Text,
    functionNameSpace :: Text,
    --    expanded       :: Bool, -- search for the definition of the function and display
    -- the graph
    fMarked :: Bool,
    fSelected :: Bool,
    explodeSelected :: Bool,
    backendFunctionNode :: (Maybe Word32, Maybe Word32), -- link to (function-node, bindr-node)
    -- of backend graph
    functionNodeRank :: Double,
    pathInfo :: PathInfo, -- when expanding a graph we want to give hints in the edges of
    -- to the options. Eg dotted or not.
    virtualFunction :: Bool, -- a function that is not selectable
    uniqueEdge :: Word32 -- in the backend a program is a graph that has unique edges
  }
  deriving (Eq, Ord, Generic, Show)

data CaseNode = CaseNode
  { casePackage :: Text,
    caseModule :: Text,
    caseName :: Text,
    caseArgs :: [(Text, Bool)],
    caseSelected :: Bool,
    caseDropdown ::
      ( Bool, -- display one case and a dropdown list to chose from or without dropdown
        Bool, -- is active
        (Int, [(UINode, Text)]) -- dropdown list
      )
  }
  deriving (Eq, Ord, Generic)

instance (Show CaseNode) where
  show (CaseNode _p _m n _a _s _d) = T.unpack n

data PathInfo
  = TypeMatch Double -- results in a dotted line connection
  | ApplyMatch Double -- function was applied in at least
  | JustConnected
  deriving (Eq, Ord, Generic, Show)

-- instance Show FunctionNode
--  where show (FunctionNode fu _ _ _ _ mar _ _ _ _ _ _) = show (fu,mar) ++ "\n"

data LambdaNode = LambdaNode
  { lambdaUnique :: Text,
    lambdaName :: Text,
    lamMarked :: Bool,
    lamSelected :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

data LiteralNode = LiteralNode
  { literalUnique :: Text,
    literalName :: Text,
    literalType :: Text, -- maybe not displayed
    lMarked :: Bool,
    lSelected :: Bool,
    backendLiteralNode :: Maybe Word32 -- link to node of backend graph
  }
  deriving (Eq, Ord, Show, Generic)

data ArgNode = ArgNode
  { refFunctionUnique :: Text,
    argName :: Text,
    argNr :: Int,
    isValueArg :: Bool,
    isMainFunctionArg :: Bool,
    argSelected :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

data MetaNodeRef = MetaNodeRef
  { metaNodeName :: Text,
    metaSelected :: Bool,
    metaWait :: Bool,
    metaMessage :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

data StateNode = StateNode
  { stateName :: Text,
    stateMarked :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

type AppliedInContext = Maybe UINode -> CGraph -> Bool

type FunctionToExecuteOnUI = Maybe UINode -> Int -> CGraph -> CGraph

--type Channel = Int -- The nth type of a type node

data EdgeType
  = NormalEdge
  | VerticalEdge -- When having options, they appear continuously in one column
  -- We mark this in the graph with vertical edges from the first
  -- option to the second and so on
  | VirtualHorEdge -- virtual edges are not displayed but used for layouting and
  -- naviagtion with the keyboard
  | SeparatingEdge -- to connect graph components that are separate
  deriving (Show, Generic, Eq, Ord)

instance EdgeAttribute [UIEdge] where -- Why can two nodes be connected with more than one edge?
-- To connect one function with several input types that are part of one type node
  fastEdgeAttr (e : _) = f e
    where
      f (UIEdge _ _ _ _ _ _ _ _ _ VerticalEdge) = vertBit
      f (UIEdge _ _ _ _ _ _ _ _ _ VirtualHorEdge) = virtBit
      f (UIEdge _ _ _ _ _ _ _ _ _ _) = 0
  fastEdgeAttr _ = 0
  edgeFromAttr =
    Map.fromList
      [ (vertBit, [UIEdge "" Nothing Nothing Nothing Nothing Nothing "" Nothing 0 VerticalEdge]),
        (virtBit, [UIEdge "" Nothing Nothing Nothing Nothing Nothing "" Nothing 0 VirtualHorEdge]),
        (0, [UIEdge "" Nothing Nothing Nothing Nothing Nothing "" Nothing 0 NormalEdge])
      ]
  show_e (Just [UIEdge "" Nothing Nothing Nothing Nothing Nothing "" Nothing 0 e]) = show e
  show_e _ = "no Edge"

  bases _ = [Edge8 0, Edge8 vertBit, Edge8 virtBit]

sepBit :: Word8
sepBit = 0x4

dummyEdge :: UIEdge
dummyEdge = UIEdge "" Nothing Nothing Nothing Nothing Nothing "" Nothing 0 NormalEdge

myFromJust :: Int -> Maybe a -> a
myFromJust i term
  | isJust term -- Debug.Trace.trace ("myFromJustTrue "++ show i)
    =
    fromJust term
  | otherwise =
    Debug.Trace.trace
      ("myFromJust " ++ show i)
      fromJust
      term

myhead :: Int -> [a] -> a
myhead i a
  | null a = error ("head: empty list " ++ (show i))
  | otherwise = head a

vhead :: (VU.Unbox a) => Int -> VU.Vector a -> a
vhead i a
  | VU.null a = error ("VU.head: empty list " ++ (show i))
  | otherwise = VU.head a

data LineType
  = Slanted Int -- a line consisiting of slanted sublines,
  -- (a lot of people did this, but no famous)
  | Curly -- contains a several subpaths
  deriving (Show, Generic, Eq, Ord)

type BasicSVG = [SVGEl]

type Stroke = String

data SVGEl
  = Circle Float Float Float Stroke -- circle_ [cx_ "5", cy_ "85", r_ "5", stroke "black"] []
  | Line Float Float Float Float Stroke
  deriving (Eq, Ord, Show, Generic)
