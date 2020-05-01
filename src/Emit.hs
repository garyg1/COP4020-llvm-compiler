{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Emit where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           Data.ByteString.Short           (ShortByteString, fromShort,
                                                  toShort)
import           Data.Function
import           Data.Int
import           Data.List
import qualified Data.Map                        as Map
import           Data.String
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.AddrSpace              as Addr
import qualified LLVM.AST.Attribute              as A
import qualified LLVM.AST.CallingConvention      as CC
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.InlineAssembly as IAS
import qualified LLVM.AST.Float                  as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global                 as AST
import qualified LLVM.AST.Global                 as G
import qualified LLVM.AST.Linkage                as L
import qualified LLVM.AST.Type                   as T
import qualified LLVM.Context                    as Ctx
import qualified LLVM.Module                     as M
import qualified Syntax                          as S

import qualified Data.ByteString                 as S (ByteString, unpack)
import qualified Data.ByteString.Char8           as C8 (pack)
import           Data.Char                       (chr)

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar (strToSBS var)
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar (strToSBS x) >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Int n) = return $ cons $ C.Int 32 n
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  fn' <- externf (strToSBS fn)
  call fn' largs
cgen x = error $ "Unimplemented operator" ++ show x

-- https://stackoverflow.com/questions/4702325/best-way-to-convert-between-char-and-word8
strToSBS :: String -> ShortByteString
strToSBS = toShort . C8.pack

sbsToStr :: ShortByteString -> String
sbsToStr = map (chr . fromEnum) . S.unpack . fromShort

bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = label }

addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  defs <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = defs ++ [d] }

entry :: Codegen AST.Name
entry = gets currentBlock

addBlock :: AST.Name -> Codegen AST.Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (qname)

setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

define ::  AST.Type -> ShortByteString -> [(AST.Type, AST.Name)] -> [G.BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  AST.GlobalDefinition $ G.functionDefaults {
    G.name        = AST.Name label
  , G.parameters  = ([G.Parameter ty nm [] | (ty, nm) <- argtys], False)
  , G.returnType  = retty
  , G.basicBlocks = body
  }

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [G.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> G.BasicBlock
makeBlock (l, (BlockState _ s t)) = G.BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing  = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: AST.Name
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState entryBlockName Map.empty [] 1 0 Map.empty []

codegenFrom :: [AST.Definition] -> CodegenState
codegenFrom defs' = emptyCodegen { defs = defs' }

execCodegen :: [AST.Definition] -> Codegen a -> CodegenState
execCodegen defs m = execState (runCodegen m) (codegenFrom defs)

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: AST.Instruction -> Codegen (AST.Operand)
instr ins = do
  n <- fresh
  let ref = (AST.UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref AST.:= ins) : i } )
  return $ local ref

terminator :: AST.Named AST.Terminator -> Codegen (AST.Named AST.Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

type SymbolTable = [(AST.Name, AST.Operand)]

type Names = Map.Map AST.Name Int

uniqueName :: AST.Name -> Names -> (AST.Name, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (AST.Name (strToSBS (nm'' ++ show ix)), Map.insert nm (ix+1) ns)
  where
    (AST.Name nm') = nm
    nm'' = sbsToStr nm'

data CodegenState = CodegenState
    { currentBlock :: AST.Name
    , blocks       :: Map.Map AST.Name BlockState
    , symtab       :: SymbolTable
    , blockCount   :: Int
    , count        :: Word
    , names        :: Names
    , defs         :: [AST.Definition]
    }
    deriving Show

data BlockState = BlockState
    { idx   :: Int
    , stack :: [AST.Named AST.Instruction]
    , term  :: Maybe (AST.Named AST.Terminator)
    }
    deriving Show


external ::  AST.Type -> ShortByteString -> [(AST.Type, AST.Name)] -> LLVM ()
external retty label argtys = addDefn $
  AST.GlobalDefinition $ G.functionDefaults {
    G.name        = AST.Name label
  , G.linkage     = L.External
  , G.parameters  = ([G.Parameter ty nm [] | (ty, nm) <- argtys], False)
  , G.returnType  = retty
  , G.basicBlocks = []
  }

toSig :: [S.Declaration] -> [(AST.Type, AST.Name)]
toSig = map (\(t, n) -> (convertType t, AST.Name (strToSBS n)))

double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

int :: AST.Type
int = AST.IntegerType 32

bool :: AST.Type
bool = AST.IntegerType 32

convertType :: S.Type -> AST.Type
convertType S.TypeBool  = int
convertType S.TypeFloat = double
convertType S.TypeInt   = int
convertType S.TypeVoid  = T.void

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function (type',name) args body) = do
  defs <- gets AST.moduleDefinitions
  define (convertType type') (strToSBS name) fnargs $ createBlocks $ (execCodegen defs $ do
    entry <- addBlock entryBlockName
    setBlock entry
    forM args $ \(t,n) -> do
      var <- alloca double
      store var (local (AST.Name (strToSBS n)))
      assign (strToSBS n) var
    cgen body >>= ret
    )
  where
    fnargs = toSig args

codegenTop (S.Extern (type',name) args) = do
  external (convertType type') (strToSBS name) fnargs
  where fnargs = toSig args

codegenTop _ = error "unknown"

-------------------------------------------------------------------------------

-- References
local ::  AST.Name -> AST.Operand
local = AST.LocalReference double

global ::  AST.Name -> C.Constant
global = C.GlobalReference double

getFn :: AST.Definition -> G.Global
getFn (AST.GlobalDefinition g) = g
getFn x = error $ show x

paramToType :: G.Parameter -> AST.Type
paramToType (G.Parameter t _ _) = t

{-
main: Function {linkage = External, visibility = Default, dllStorageClass = Nothing, callingConvention = C, returnAttributes = [], returnType = IntegerType {typeBits = 32}, name = Name "print", parameters = ([Parameter (FloatingPointType {floatingPointType = DoubleFP}) (Name "x") []],False), functionAttributes = [], section = Nothing, comdat = Nothing, alignment = 0, garbageCollectorName = Nothing, prefix = Nothing, basicBlocks = [], personalityFunction = Nothing, metadata = []}
-}
externf :: ShortByteString -> Codegen (Either a0 AST.Operand)
externf name = do
  fn <- (getdefn name)
  let fn' = getFn fn
  let rt = G.returnType fn'
  let (params, _) = G.parameters fn'
  return $ ((Right $ AST.ConstantOperand $ C.GlobalReference (T.PointerType (T.FunctionType rt (map paramToType params) False) (Addr.AddrSpace 0)) (AST.Name name)))

-- Arithmetic and Constants
fadd :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fadd a b = instr $ AST.FAdd AST.noFastMathFlags a b []

fsub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fsub a b = instr $ AST.FSub AST.noFastMathFlags a b []

fmul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fmul a b = instr $ AST.FMul AST.noFastMathFlags a b []

fdiv :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fdiv a b = instr $ AST.FDiv AST.noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
fcmp cond a b = instr $ AST.FCmp cond a b []

cons :: C.Constant -> AST.Operand
cons = AST.ConstantOperand

uitofp :: AST.Type -> AST.Operand -> Codegen AST.Operand
uitofp ty a = instr $ AST.UIToFP a ty []

toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: ShortByteString -> AST.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(AST.Name var, x)] ++ lcls }

getvar :: ShortByteString -> Codegen AST.Operand
getvar var = do
  syms <- gets symtab
  case lookup (AST.Name var) syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var


getdefn :: ShortByteString -> Codegen AST.Definition
getdefn name = do
  defs' <- gets defs
  case find (\(AST.GlobalDefinition f) -> AST.name f == AST.Name name) defs' of
    Just x  -> return x
    Nothing -> error $ "Definition not in scope: " ++ show name


-- Effects
call :: Either IAS.InlineAssembly AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = do
  instr $ AST.Call Nothing CC.C [] fn (toArgs args) [] []

alloca :: AST.Type -> Codegen AST.Operand
alloca ty = instr $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen AST.Operand
store ptr val = instr $ AST.Store False ptr val Nothing 0 []

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr $ AST.Load False ptr Nothing 0 []

-- Control Flow
br :: AST.Name -> Codegen (AST.Named AST.Terminator)
br val = terminator $ AST.Do $ AST.Br val []

cbr :: AST.Operand -> AST.Name -> AST.Name -> Codegen (AST.Named AST.Terminator)
cbr cond tr fl = terminator $ AST.Do $ AST.CondBr cond tr fl []

ret :: AST.Operand -> Codegen (AST.Named AST.Terminator)
ret val = terminator $ AST.Do $ AST.Ret (Just val) []

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = Ctx.withContext $ \context ->
  M.withModuleFromAST context newast $ \m -> do
    llstr <- M.moduleLLVMAssembly m
    putStrLn (bsToStr llstr)
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
