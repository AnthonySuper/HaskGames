{-# LANGUAGE OverloadedStrings
           , RecursiveDo
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies #-}
module Main where
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Control.Lens 
import Control.Monad.Fix

data Importance
  = High
  | Normal
  | Low
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data TodoItem
  = TodoItem { _todoItemTitle :: T.Text
             , _todoItemBody :: T.Text
             , _todoItemImportance :: Importance
             }
    deriving (Show, Read, Eq, Ord)

data ItemEvent
  = AddItem TodoItem
  | RemoveItem TodoItem

foldTodoItems :: ItemEvent -> [TodoItem] -> [TodoItem]
foldTodoItems evt items =
  case evt of
    AddItem i -> i : items
    RemoveItem i -> filter (/= i) items

makeLenses ''TodoItem

item = TodoItem "Who knows" "Not I" Low

enumValues :: (Show k, Bounded k, Ord k, Enum k) 
           => (Map.Map k T.Text)
enumValues = mapped
  where
    mapped = mconcat (map valToText vals)
    valToText x = x =: T.pack (show x)
    vals = [minBound..maxBound]

hideAttrs :: Bool -> Map.Map T.Text T.Text
hideAttrs True = "hidden" := "true"
hideAttrs False = "hidde" := "false"

elDynHidden :: T.Text -> Dynamic t Bool -> m a -> m a
elDynHidden = (. hideAttrs) . elDynAttr

main = mainWidget $ el "div" $ do
  rec otherDyn <- holdDyn item (updated editDyn)
      editDyn <- elDynHidden "div" editingDyn $ editItem evt otherDyn
      editingDyn <- elDynHidden "div" (not <$> editingDyn) $ dispItemWrapper otherDyn 
  return ()


dispItemWrapper :: forall t m.
                   ( DomBuilder t m
                   , Monad m
                   , Reflex t
                   , PostBuild t m
                   , MonadHold t m
                   , MonadFix m
                   )
                => Dynamic t TodoItem
                -> m (Dynamic t Bool)
dispItemWrapper i = mdo
  let mappedFunc x = if x then "editing" else "const"
      dynClass = mappedFunc <$> toggleDyn
  toggleDyn <- elDynClass "li" dynClass $ do
    buttonEvt <- button "Toggle Edit"
    dispItem i
    toggle False buttonEvt
  dynText dynClass
  return toggleDyn

editItem :: forall t q m.
            ( DomBuilder t m
            , Monad m
            , Reflex t
            , PostBuild t m
            , MonadHold t m
            , MonadFix m
            , DomBuilderSpace m ~ GhcjsDomSpace
            )
         =>
            Event t q -> Dynamic t TodoItem -> m (Dynamic t TodoItem)
editItem toggle item = elClass "div" "todo-item-edit" $ do
  let setValue :: (TodoItem -> a) -> Event t a
      setValue v = tag (v <$> current item) toggle
  let setTitleValue = setValue _todoItemTitle
      setBodyValue = setValue _todoItemBody
      setImportanceValue = setValue _todoItemImportance
  titleBox <- textInput $ def
    & textInputConfig_setValue .~ setTitleValue
    & textInputConfig_attributes .~ constDyn ("class" =: "edit" <> 
                                              "name" =: "title")
  descriptionBox <- textInput $ def
    & textInputConfig_setValue .~ setBodyValue
    & textInputConfig_attributes .~ constDyn ("class" =: "edit" <> 
                                              "name" =: "body")
  importanceBox <- dropdown Low (constDyn enumValues) $ def
    & dropdownConfig_setValue .~ setImportanceValue
    & dropdownConfig_attributes .~ constDyn ("class" =: "edit" <> 
                                             "name" =: "importance")
  let tbv = titleBox ^. textInput_value :: Dynamic t T.Text
      dbv = descriptionBox ^. textInput_value :: Dynamic t T.Text
      ibv = importanceBox ^. dropdown_value :: Dynamic t Importance
  return $ TodoItem <$> tbv
           <*> dbv
           <*> ibv

dispItem :: (DomBuilder t m, Monad m, Reflex t, PostBuild t m)
         => Dynamic t TodoItem
         -> m ()
dispItem item = elClass "div" "todo-item" $ do
  el "h2" $ dynText $ fmap (^. todoItemTitle) item
  el "div" $ dynText $ fmap (^. todoItemBody) item

