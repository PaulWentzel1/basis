{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Finance.Derivatives where

-- | Direction of a position
data Direction = Long | Short
    deriving (Show, Eq)

-- | Possible types of vanilla options
data OptionType = Call | Put
    deriving (Show, Eq)

-- | Possible Option exercises. European: At maturity; Bermudan: Discreete Intervals; American: Any point between aquiring and maturity
data OptionExercise = European | Bermudan | American
    deriving (Show, Eq)

-- | Out of the money, At the money, At the forward, in the money
data Moneyness = OTM | ATM | ATF | ITM

newtype MktPx          = MktPx          { unMktPx :: Double }          deriving (Eq, Ord, Show)
newtype Spot           = Spot           { unSpot :: Double }           deriving (Eq, Ord, Show)
newtype Strike         = Strike         { unStrike :: Double }         deriving (Eq, Ord, Show)
newtype Volatility     = Volatility     { unVolatility :: Double }     deriving (Eq, Ord, Show)
newtype InterestRate   = InterestRate   { unInterestRate :: Double }   deriving (Eq, Ord, Show)
newtype CostOfCarry    = CostOfCarry    { unCostOfCarry :: Double }    deriving (Eq, Ord, Show)
newtype TimeToMaturity = TimeToMaturity { unTimeToMaturity :: Double } deriving (Eq, Ord, Show)

-- TODO refactor commodity future/forward functions into generalized Futures functions
newtype FVCommodity    = FVCommodity   { unFVCommodity :: Double }   deriving (Eq, Ord, Show)
newtype PVCommodity    = PVCommodity   { unPVCommodity :: Double }   deriving (Eq, Ord, Show)
newtype StorageCost    = StorageCost   { unStorageCost :: Double }   deriving (Eq, Ord, Show)
newtype InsuranceCost  = InsuranceCost { unInsuranceCost :: Double } deriving (Eq, Ord, Show)


data VanillaOption = VanillaOption
    { optType         :: OptionType
    , optExercise     :: OptionExercise
    , optDirection    :: Direction
    , optUnderlyingPx :: Spot
    , optStrike       :: Strike
    , optCostOfCarry  :: CostOfCarry
    , optInterestRate :: InterestRate
    , optMaturity     :: TimeToMaturity
    , optVolatility   :: Volatility
    , optPrice        :: MktPx -- Optional
    }

vanillaOptIntrVal :: VanillaOption -> Double
vanillaOptIntrVal opt = case optType opt of
    Call -> max 0 (spot - strike)
    Put  -> max 0 (spot - strike)
  where
    Spot spot = optUnderlyingPx opt
    Strike strike = optStrike opt


d1 :: Spot -> Strike -> CostOfCarry -> Volatility -> TimeToMaturity -> Double
d1 (Spot s) (Strike k) (CostOfCarry b) (Volatility sigma) (TimeToMaturity t) =
    (log (s /k) + (b + sigma**2 / 2) * t) / (sigma * sqrt t)

d2 :: Spot -> Strike -> CostOfCarry -> Volatility -> TimeToMaturity -> Double
d2 (Spot s) (Strike k) (CostOfCarry b) (Volatility sigma) (TimeToMaturity t) =
    d1 (Spot s) (Strike k) (CostOfCarry b) (Volatility sigma) (TimeToMaturity t) - sigma * sqrt t


calcFVCommodity :: PVCommodity -> TimeToMaturity -> InterestRate -> StorageCost -> InsuranceCost -> FVCommodity
calcFVCommodity (PVCommodity c) (TimeToMaturity t) (InterestRate r) (StorageCost s) (InsuranceCost i) =
    FVCommodity (c * (1 + r * t) + (s * t) + (i * t))

calcPVCommodity :: FVCommodity -> TimeToMaturity -> InterestRate -> StorageCost -> InsuranceCost -> PVCommodity
calcPVCommodity (FVCommodity f) (TimeToMaturity t) (InterestRate r) (StorageCost s) (InsuranceCost i) =
    PVCommodity ((f - (s + i) * t) / (1 + r * t))

convenienceYield :: Either FVCommodity PVCommodity -> MktPx -> TimeToMaturity -> InterestRate -> StorageCost -> InsuranceCost -> Double
convenienceYield (Left fv) (MktPx p) t r s i =                -- Left case, Future Value
    let PVCommodity pv = calcPVCommodity fv t r s i
    in p - pv
convenienceYield (Right (PVCommodity pv)) (MktPx p) _ _ _ _ = -- Right case, Present Value
    p - pv
