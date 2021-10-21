module Binary

import JS

import Control.Monad.State
import Control.Monad.Reader

import Syntax
import Data.List1
import Data.List

export
Get : Type -> Type
Get = ReaderT UInt8Array (StateT Bits32 IO)

export
Put : Type -> Type
Put = ReaderT (Array Bits8) (StateT Bits32 IO)

loadBits8 : Get Bits8
loadBits8 = do
  i <- lift $ get <* modify (+ 1)
  arr <- ask
  mx <- readIO arr i
  pure $ assert_total $ case mx of Just x => x

saveBits8 : Bits8 -> Put ()
saveBits8 x = do
  i <- lift $ get <* modify (+ 1)
  arr <- ask
  writeIO arr i x

interface Binary a where
  load : Get a
  save : a -> Put ()

implementation Binary Bits8 where
  load = loadBits8
  save = saveBits8

implementation Binary Bool where
  load = pure $ !loadBits8 /= 0
  save b = saveBits8 $ if b then 1 else 0

implementation Binary Bits16 where
  load = do
    lo <- loadBits8
    hi <- loadBits8
    pure $ cast hi * 0x100 + cast lo

  save x = do
    let hi = x `div` 0x100
        lo = x `mod` 0x100
    save lo *> save hi

implementation Binary Double where
  load = cast . the Bits16 <$> load
  save = save . the Bits16 . cast

tag : Bits8 -> Put ()
tag = save

loadTag : Get Bits8
loadTag = load

saveList : (a -> Put ()) -> List a -> Put ()
saveList save' xs = save (the Bits8 $ cast $ length xs) *> traverse_ save' xs

loadList : Get a -> Get (List a)
loadList loadElem = do
  n <- load
  sequence (replicate (cast $ the Bits8 n) loadElem)

saveList1 : (a -> Put ()) -> List1 a -> Put ()
saveList1 save (x ::: xs) = save x *> saveList save xs

loadList1 : Get a -> Get (List1 a)
loadList1 load = (:::) <$> load <*> loadList load

implementation Binary Id where
  load = MkId <$> loadList1 load
  save (MkId xs) = saveList1 save xs

implementation Binary Var0 where
  load = loadTag >>= \tag => assert_total $ case tag of
    0x01 => RealVar <$> load
    0x02 => IntVar <$> load
    0x03 => StrVar <$> load

  save (RealVar id) = tag 0x01 *> save id
  save (IntVar id) = tag 0x02 *> save id
  save (StrVar id) = tag 0x03 *> save id

mutual
  implementation Binary Var where
    load = MkVar <$> load <*> loadList load
    save (MkVar v xs) = save v *> saveList save xs

  implementation Binary Expr where
    load = loadTag >>= \tag => case tag of
      0x41 => VarE <$> load
      0X22 => StrLitE <$> loadList load
      0x31 => NumLitE <$> load
      0xab => NegE <$> load
      _ => assert_total $ case opFromTag tag of
        Just op => Bin op <$> load <*> load
        Nothing => case funFromTag tag of
          Just f => FunE f <$> loadList1 load

      where
        opFromTag : Bits8 -> Maybe BinOp
        opFromTag 0xb2 = Just Eq
        opFromTag 0xf1 = Just NEq
        opFromTag 0xb3 = Just LT
        opFromTag 0xf2 = Just LE
        opFromTag 0xb1 = Just GT
        opFromTag 0xf3 = Just GE
        opFromTag 0xaa = Just Plus
        opFromTag 0xf4 = Just Minus
        opFromTag 0xac = Just Mul
        opFromTag 0xaf = Just And
        opFromTag 0xb0 = Just Or
        opFromTag _ = Nothing

        funFromTag : Bits8 -> Maybe Fun
        funFromTag 0xc2 = Just Peek
        funFromTag 0xb5 = Just IntFun
        funFromTag 0xbb = Just Rnd
        funFromTag 0xc8 = Just LeftStr
        funFromTag 0xc7 = Just Chr
        funFromTag 0xc5 = Just Val
        funFromTag 0xc6 = Just Asc
        funFromTag 0xa3 = Just Tab
        funFromTag _ = Nothing

    save expr = case expr of
      VarE v     => tag 0x41 *> save v
      StrLitE s  => tag 0x22 *> saveList save s
      NumLitE n  => tag 0x31 *> save n
      NegE x     => tag 0xab *> save x
      Bin op x y => tag (opToTag op) *> save x *> save y
      FunE f xs  => tag (funToTag f) *> saveList1 save xs
      where
        opToTag : BinOp -> Bits8
        opToTag Eq    = 0xb2
        opToTag NEq   = 0xf1
        opToTag LT    = 0xb3
        opToTag LE    = 0xf2
        opToTag GT    = 0xb1
        opToTag GE    = 0xf3
        opToTag Plus  = 0xaa
        opToTag Minus = 0xf4
        opToTag Mul   = 0xac
        opToTag And   = 0xaf
        opToTag Or    = 0xb0

        funToTag : Fun -> Bits8
        funToTag Peek    = 0xc2
        funToTag IntFun  = 0xb5
        funToTag Rnd     = 0xbb
        funToTag LeftStr = 0xc8
        funToTag Chr     = 0xc7
        funToTag Val     = 0xc5
        funToTag Asc     = 0xc6
        funToTag Tab     = 0xa3

implementation Binary Stmt where
  load = loadTag >>= \tag => assert_total $ case tag of
    0x80 => pure End
    0x81 => For <$> load <*> load <*> load <*> load
    0x82 => Next <$> load
    0x83 => Data <$> loadList1 load
    0x84 => InputH <$> load <*> loadList1 load
    0x87 => Read <$> load
    0x89 => Goto <$> load
    0x8a => pure Run
    0x8b => If <$> load <*> load
    0x8d => Gosub <$> load
    0x8e => pure Return
    0x8f => pure Rem
    0x97 => Poke <$> load <*> load
    0x98 => PrintH <$> load <*> loadList1 load
    0x99 => Print <$> loadList load <*> load
    0x9c => pure Clr
    0x9e => Sys <$> load
    0x9f => Open <$> load <*> load <*> load <*> load
    0xa0 => Close <$> load
    0xa1 => Get <$> load
    0xb2 => Assign <$> load <*> load
    0x01 => OnGoto <$> load <*> loadList1 load
    0x02 => OnGosub <$> load <*> loadList1 load

  save stmt = case stmt of
    End                  => tag 0x80
    For v start end step => tag 0x81 *> save v *> save start *> save end *> save step
    Next v               => tag 0x82 *> save v
    Data ns              => tag 0x83 *> saveList1 save ns
    InputH fd vs         => tag 0x84 *> save fd *> saveList1 save vs
    Read v               => tag 0x87 *> save v
    Goto lab             => tag 0x89 *> save lab
    Run                  => tag 0x8a
    If c s               => tag 0x8b *> save c *> save s
    Gosub lab            => tag 0x8d *> save lab
    Return               => tag 0x8e
    Rem                  => tag 0x8f
    Poke e1 e2           => tag 0x97 *> save e1 *> save e2
    PrintH fd es         => tag 0x98 *> save fd *> saveList1 save es
    Print es sameLine    => tag 0x99 *> saveList save es *> save sameLine
    Clr                  => tag 0x9c
    Sys n                => tag 0x9e *> save n
    Open fd x y z        => tag 0x9f *> save fd *> save x *> save y *> save z
    Close fd             => tag 0xa0 *> save fd
    Get v                => tag 0xa1 *> save v
    Assign v e           => tag 0xb2 *> save v *> save e
    OnGoto e labs        => tag 0x01 *> save e *> saveList1 save labs
    OnGosub e labs       => tag 0x02 *> save e *> saveList1 save labs
