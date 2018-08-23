{-# LANGUAGE OverloadedStrings #-}
module Main where
import Reflex.Dom

main = mainWidget $ el "div" $ do
  t <- textInput def
  dynText $ _textInput_value t
  
