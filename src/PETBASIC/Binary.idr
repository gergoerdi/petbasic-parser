module PETBASIC.Binary

import Data.Binary

import PETBASIC.Syntax
import Data.List1
import Data.List

export
implementation Binary Bool where
  get = pure $ !getBits8 /= 0
  put b = putBits8 $ if b then 1 else 0

export
implementation Binary Bits16 where
  get = do
    lo <- getBits8
    hi <- getBits8
    pure $ cast hi * 0x100 + cast lo

  put x = do
    let hi = the Bits8 $ cast $ x `div` 0x100
        lo = the Bits8 $ cast $ x `mod` 0x100
    put lo *> put hi

export
implementation Binary Double where
  get = cast . the Bits16 <$> get
  put = put . the Bits16 . cast

tag : MonadPut m => Bits8 -> m ()
tag = put

getTag : MonadGet m => m Bits8
getTag = get

export
putList : MonadPut m => (a -> m ()) -> List a -> m ()
putList put' xs = put (the Bits16 $ cast $ length xs) *> traverse_ put' xs

export
getList : MonadGet m => m a -> m (List a)
getList getElem = do
  n <- get
  sequence (replicate (cast $ the Bits16 n) getElem)

export
putList1 : MonadPut m => (a -> m ()) -> List1 a -> m ()
putList1 put (x ::: xs) = put x *> putList put xs

export
getList1 : MonadGet m => m a -> m (List1 a)
getList1 get = (:::) <$> get <*> getList get

implementation Binary Id where
  get = MkId <$> getList1 get
  put (MkId xs) = putList1 put xs

implementation Binary Var0 where
  get = getTag >>= \tag => assert_total $ case tag of
    0x01 => RealVar <$> get
    0x02 => IntVar <$> get
    0x03 => StrVar <$> get

  put (RealVar id) = tag 0x01 *> put id
  put (IntVar id) = tag 0x02 *> put id
  put (StrVar id) = tag 0x03 *> put id

mutual
  implementation Binary Var where
    get = MkVar <$> get <*> getList get
    put (MkVar v xs) = put v *> putList put xs

  implementation Binary Expr where
    get = getTag >>= \tag => case tag of
      0x41 => VarE <$> get
      0X22 => StrLitE <$> getList get
      0x31 => NumLitE <$> get
      0xab => NegE <$> get
      _ => assert_total $ case opFromTag tag of
        Just op => Bin op <$> get <*> get
        Nothing => case funFromTag tag of
          Just f => FunE f <$> getList1 get

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

    put expr = case expr of
      VarE v     => tag 0x41 *> put v
      StrLitE s  => tag 0x22 *> putList put s
      NumLitE n  => tag 0x31 *> put n
      NegE x     => tag 0xab *> put x
      Bin op x y => tag (opToTag op) *> put x *> put y
      FunE f xs  => tag (funToTag f) *> putList1 put xs
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

export
implementation Binary Stmt where
  get = getTag >>= \tag => assert_total $ case tag of
    0x80 => pure End
    0x81 => For <$> get <*> get <*> get <*> get
    0x82 => Next <$> get
    0x83 => Data <$> getList1 get
    0x84 => InputH <$> get <*> getList1 get
    0x87 => Read <$> get
    0x89 => Goto <$> get
    0x8a => pure Run
    0x8b => If <$> get <*> get
    0x8d => Gosub <$> get
    0x8e => pure Return
    0x8f => pure Rem
    0x97 => Poke <$> get <*> get
    0x98 => PrintH <$> get <*> getList1 get
    0x99 => Print <$> getList get <*> get
    0x9c => pure Clr
    0x9e => Sys <$> get
    0x9f => Open <$> get <*> get <*> get <*> get
    0xa0 => Close <$> get
    0xa1 => Get <$> get
    0xb2 => Assign <$> get <*> get
    0x01 => OnGoto <$> get <*> getList1 get
    0x02 => OnGosub <$> get <*> getList1 get

  put stmt = case stmt of
    End                  => tag 0x80
    For v start end step => tag 0x81 *> put v *> put start *> put end *> put step
    Next v               => tag 0x82 *> put v
    Data ns              => tag 0x83 *> putList1 put ns
    InputH fd vs         => tag 0x84 *> put fd *> putList1 put vs
    Read v               => tag 0x87 *> put v
    Goto lab             => tag 0x89 *> put lab
    Run                  => tag 0x8a
    If c s               => tag 0x8b *> put c *> put s
    Gosub lab            => tag 0x8d *> put lab
    Return               => tag 0x8e
    Rem                  => tag 0x8f
    Poke e1 e2           => tag 0x97 *> put e1 *> put e2
    PrintH fd es         => tag 0x98 *> put fd *> putList1 put es
    Print es sameLine    => tag 0x99 *> putList put es *> put sameLine
    Clr                  => tag 0x9c
    Sys n                => tag 0x9e *> put n
    Open fd x y z        => tag 0x9f *> put fd *> put x *> put y *> put z
    Close fd             => tag 0xa0 *> put fd
    Get v                => tag 0xa1 *> put v
    Assign v e           => tag 0xb2 *> put v *> put e
    OnGoto e labs        => tag 0x01 *> put e *> putList1 put labs
    OnGosub e labs       => tag 0x02 *> put e *> putList1 put labs
