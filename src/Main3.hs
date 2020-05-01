{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Module as LLVMIR
import qualified LLVM.IRBuilder.Monad as LLVMIR
import Data.IORef
import LLVM.AST.Instruction
import Data.Int
import qualified Data.Map.Strict as Map
import Foreign.Ptr
import       qualified    LLVM.AST.Type as T
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Constant hiding (Add)
import LLVM.AST.Global
import LLVM.CodeGenOpt
import LLVM.CodeModel
import LLVM.Context
import LLVM.Internal.OrcJIT.CompileLayer
import qualified LLVM.AST.AddrSpace as A2
import LLVM.Module
import LLVM.OrcJIT
import LLVM.Relocation
import LLVM.Target
import Prelude hiding (mod)
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])

decl :: LLVMIR.MonadModuleBuilder m => m Operand
decl = LLVMIR.extern "foo" [T.i32] T.void

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference int

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

defMain :: Definition
defMain = GlobalDefinition functionDefaults
  { name = Name "main"
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [
            Do $ Call Nothing CC.C [] (
              (Right $ ConstantOperand $ C.GlobalReference (PointerType (FunctionType T.void [T.i32] False) (A2.AddrSpace 0)) (Name "foo"))
            ) [(ConstantOperand $ Int 32 10, [])] [] [],
          ConstantOperand (Int 32 42)))
        ]
        (Do $ Ret $ Just $ C.LocalReference $ Name "local")


module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd, defMain, myDecl]
  }
  where (_, [myDecl]) = LLVMIR.runModuleBuilder LLVMIR.emptyModuleBuilder decl


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm


main :: IO ()
main = toLLVM module_