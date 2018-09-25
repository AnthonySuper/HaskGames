{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes
           , TypeFamilies
           , TypeInType
           , DeriveGeneric
           , TypeOperators
           , UndecidableInstances
           , FlexibleInstances
           , DefaultSignatures
           , TypeApplications #-}

module Reflex.Contrib.AutoForm where
    import Reflex.Dom
    import qualified Data.Text as T
    import GHC.Generics
    import Control.Lens
    import qualified Data.Map.Strict as Map
    import Control.Applicative (liftA2)
    import Text.Read (readMaybe)
    import Data.Maybe (maybe)
    import Data.Monoid ((<>))

    data Gender
        = Male
        | Female
        | Other T.Text
        deriving (Show, Read, Eq, Generic)

    data Status
        = Registered
        | EmailConfirmed
        | CreditCarded
        deriving (Show, Read, Eq, Generic, Enum, Bounded, Ord)

    data Person
        = Person
        { _personName :: T.Text
        , _personAge :: Integer
        , _personStatus :: Status
        } deriving (Show, Read, Generic)

   
    class Fieldable f where
        toField :: (MonadWidget t m)
                => T.Text -> T.Text -> m (Dynamic t f)
        --

    data FormConfig
        = FormConfig
        { _formConfigLabelMapper :: T.Text -> T.Text
        , _formConfigNameMapper :: T.Text -> T.Text
        , _formConfigNameCombiner :: T.Text -> T.Text -> T.Text
        , _formConfigLabelCombiner :: T.Text -> T.Text -> T.Text 
        } deriving (Generic)

    makeLenses ''FormConfig

    toForm :: forall t m f. (MonadWidget t m, Formable f)
           => T.Text -> T.Text -> m (Dynamic t f)
    toForm = toFormCfg (defaultFormCfg @f)

    class Formable f where
        toFormCfg :: (MonadWidget t m)
               => FormConfig -> T.Text -> T.Text -> m (Dynamic t f)
        default toFormCfg :: ( MonadWidget t m 
                          , Generic f
                          , GFormable (Rep f)) 
                       => FormConfig -> T.Text -> T.Text -> m (Dynamic t f)
        toFormCfg c a b = (fmap . fmap $ GHC.Generics.to) $ gToFormCfg c a b

        defaultFormCfg :: FormConfig
        defaultFormCfg = FormConfig id id mappend mappend 
        

    class GFormable f where
        gToFormCfg :: (MonadWidget t m)
                => FormConfig -> T.Text -> T.Text -> m (Dynamic t (f a))
        --

    instance (GFormable lhs, GFormable rhs) => GFormable ((:*:) lhs rhs) where
        gToFormCfg c n l = do
            a' <- gToFormCfg c n l 
            b' <- gToFormCfg c n l 
            return $ liftA2 (:*:) a' b'
    

    instance (Fieldable f) => GFormable (K1 i f) where
        gToFormCfg c a b = (fmap . fmap $ K1) (toField a b)

    instance (GFormable a) => GFormable (M1 D s a) where
        gToFormCfg c name label = (fmap . fmap $ M1) (gToFormCfg c name label)

    instance (GFormable a, Constructor c) => GFormable (M1 C c a) where
        gToFormCfg c name label = (fmap . fmap $ M1) (gToFormCfg c name' label)
            where
                name' = (c ^. formConfigNameCombiner) name constructorName
                constructorName :: T.Text
                constructorName = T.pack $ conName (undefined :: t c a p)

    instance (GFormable a, Selector s) => GFormable (M1 S s a) where
        gToFormCfg c name label = (fmap . fmap $ M1) (gToFormCfg c name' label')
            where
                name' = (c ^. formConfigNameCombiner) name selectorName
                label' = (c ^. formConfigLabelMapper) selectorName
                selectorName = T.pack $ selName (undefined :: t s a p)

    simpleInput input ats mapper name label = elClass "div" "pure-control-group" $ do
        let labelAts = ("for" =: name)
        let fieldAts = ("name" =: name) <> ats
        elAttr "label" labelAts $ text label
        e <- input $ def &
            attributes %~ (mappend $ pure fieldAts)
        return $ mapper <$> value e

    type family BoolAnd a b where
        BoolAnd 'True 'True = 'True
        BoolAnd _ _ = 'False

    type family IsAllNullary k where
        IsAllNullary (M1 D _ k) = IsAllNullary k
        IsAllNullary (C1 _ U1) = 'True
        IsAllNullary (a :+: b) = BoolAnd (IsAllNullary a) (IsAllNullary b)
        IsAllNullary _ = 'False

    instance Fieldable T.Text where
        toField = simpleInput textInput mempty id
    
    instance Fieldable Bool where
        toField = simpleInput (checkbox True) mempty id

    instance Fieldable Integer where 
        toField = simpleNumber
        
    instance Fieldable Int where 
        toField = simpleNumber

    instance {-# OVERLAPPABLE #-} ( Generic k
             , IsAllNullary (Rep k) ~ 'True
             , Bounded k
             , Enum k
             , Show k
             , Ord k  ) => Fieldable k where
        toField = simpleInput inp mempty id 
            where
                inp = dropdown minBound elms
                elms = pure $ Map.fromList (map toPair [minBound..maxBound])
                toPair x = (x, T.pack $ show x)


    simpleNumber :: forall t m a. (MonadWidget t m, Integral a, Read a) 
                 => T.Text -> T.Text -> m (Dynamic t a)
    simpleNumber = simpleInput textInput fields readM
        where
            fields 
                = "type" =: "number" <>
                "step" =: "1"
            readM :: T.Text -> a 
            readM f = maybe 0 id (readMaybe $ T.unpack f)

    
    
    statusField :: (MonadWidget t m)
               => m (Dynamic t Status)
    statusField = toField "status" "Status"

    instance Formable Person 