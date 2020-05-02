# Haskell LLVM Compiler for C-like language

## Contact
```
Gary Gurlaskie
6761-4364
COP4020 Spring 2020, Project 5
Dr. Alin Dobra
```

## Usage
### Install
```
# run Ubuntu 16.04 using Docker (if you don't have it)
docker pull ubuntu:16.04
docker run -d -t -i -v $(pwd):/workspace ubuntu:16.04
docker exec -it <container> /bin/bash
cd workspace

# install deps and build project (takes 3-4 minutes on my Mac)
./install.sh
```
### Run Tests (System)
```
./runtests.sh
```

### Language
Here is an example program. As you can see, the language is "C-like" (in that it supports assignment, types, etc.).
```
extern int print_int(int x);
extern int print_float(float x);
extern float one_float();
extern int one();

def int k_int() (
    3
);

def float k_float() (
    3.0
);

def int add_int(int a, int b) (
    a = k_int():
    a + k_int()
);

def float add_float(float a, float b) (
    a + b + one_float() + k_float()
);

def int main() (
    print_int(add_int(1, 2)):
    print_float(add_float(1.0, 2.0))
);
```

which will produce
```
$ ./compile.sh smokeInt && ./bin/main
building
running
compiling to .o
compiling external libs
compiling to binary
6
7.000000
```

## Features Added
#### Features / Maintenance
features I added beyond the starter code:
- types (supports `int` and `float`)
- fixed `extern` calls (did not work in starter code)

other work to improve starter code:
- migrated to LLVM 9.0
- cleaned up import namespaces

#### Installation
A big chunk of work went into actually getting the starter code to run on my OS:
- brew does not vendor `llvm` below `llvm@6`, and the `brew install` LLVM@4 suggested actually builds LLVM from source (which failed on macOS 10.14 after 1-2 hours). So I ended up using Ubuntu 16.04 (via Docker).
- There were a number of issues with `extern` in the starter code (like [this one]( https://stackoverflow.com/questions/48846787/encodeexception-the-serialized-globalreference-has-type-pointertype)).
- Also, `stack` did not want to link C libraries (both `.so` and `.a`) and the guide suggests [compiling manually](http://www.stephendiehl.com/llvm/#external-functions), which is not a good idea.
- There were incompatability issues between `ShortByteString` and `String` in method signatures (even with `OverloadedStrings` enabled) which required essentially explicit casting.

I discovered near the end of the project that [someone else](https://github.com/sam46/Paskell) had already encountered all these problems, and that most of his approaches were nearly identical (such as `anonInstr`, `ShortByteString` issues, and `instr :: Type -> Instruction -> Codegen (Operand)`).


## Approach
### Types
To add types, there was some work in the parser and in the compiler. The parser work is very simple, so there's not much to say about it -- we just need to store types along with most names. In the compiler, the basic idea of the change is that every time a type-dependent instruction (`fadd`/`add`, `call`, etc.) is used, we need to somehow know the type of the values involved. There are a number of 

function call is made, we need to somehow know the type of the values. In order to make that data available, we needed to extend the `CodegenState` with additional fields **from the `LLVM` monad**. Here's how that looks:
```haskell
-- added `defs`
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

execCodegen :: [AST.Definition] -> Codegen a -> CodegenState
execCodegen defs m = execState (runCodegen m) (codegenFrom defs)

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function (rtype, name) args body) = do
  -- need to grab most recent global functions
  defs <- gets AST.moduleDefinitions
  define {- ... -} (execCodegen defs $ do
    -- ...
    )
  where
    fnargs = toSig args
```

Then, every time an function call was made, we grab the type information.

### Getting `extern` to work on LLVM 9
To fix `extern` for LLVM 9, we needed to make sure `extern` functions were called with the correct type reference. Otherwise LLVM 9 complains by saying:
```
EncodeException "The serialized GlobalReference has type IntegerType {typeBits = 32} but should have type PointerType {pointerReferent = FunctionType ... }
```
after some debugging, this entailed rewriting the `externf` function (that's supposed to prepare a call to an extern function). Currently it looked like
```haskell
externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double
```
and the problems with that are
1. it causes the "serialized GlobalReference" error above in LLVM 9
2. we need to use type information now that there's more than one type.

The rewrite made it look like:
```haskell
externf :: ShortByteString -> Codegen (Either a0 AST.Operand)
externf name = do
  fn <- (getdefn name)
  let fn' = getFn fn
  let rt = G.returnType fn'
  let (params, _) = G.parameters fn'
  return $ ((Right $ AST.ConstantOperand $ C.GlobalReference (T.PointerType (T.FunctionType rt (map paramToType params) False) (Addr.AddrSpace 0)) (AST.Name name)))
```

There were a number of other changes in order to implement the above features.

## Dependencies
- Ubuntu 16.04
- System-Level LLVM@9 installation
- see the `.cabal` file for list of all libraries used