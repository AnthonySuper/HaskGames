{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}
module GameView.FillBlanks.ChatView where
    import Reflex.Dom
    import Reflex.Helpers
    import Reflex.InputWrapper
    import qualified Data.ByteString.Lazy as BS
    import qualified Data.Text as T
    import qualified Data.Map.Strict as Map
    import Game.FillBlanks.Event
    import Control.Lens
    import Data.Aeson (encode)
    import Data.Monoid ((<>))
    import GHCJS.DOM.Element (getScrollTop, getScrollHeight, setScrollTop)
    import Data.Functor (($>))

    data ChatMsg
        = ChatMsg 
        {   _chatMsgAuthor :: T.Text
        ,   _chatMsgBody :: T.Text
        } deriving (Show, Eq, Ord)

    makeLenses ''ChatMsg

    asChat :: Iso' (T.Text, T.Text) ChatMsg 
    asChat = iso (uncurry ChatMsg) (\x -> (x ^. chatMsgAuthor, x ^.chatMsgBody))

    toChatMessage :: Prism' ServerEvent ChatMsg
    toChatMessage = _ChatMessage . asChat

    chatView :: (MonadWidget t m)
             => Event t ServerEvent
             -> m (Event t [BS.ByteString])
    chatView evt = elClass "section" "section chat-view" $ mdo
        let newMsg = fmapMaybe (preview toChatMessage) evt 
        msgCount <- count newMsg
        let insertEvt = attachWith Map.insert (current msgCount) newMsg 
        refDyn <- foldDyn ($) mempty insertEvt
        (e, _) <- elAttr' "div" ("class" =: "chat-messages") $
            listWithKey refDyn chatItem
        let scrollPerf = scrollBottom (_element_raw e)
        performEvent (newMsg $> scrollPerf)
        input <- textInput $ def
            & textInputConfig_setValue .~ ("" <$ enterPressed)
            & textInputConfig_attributes .~ pure ("name" =: "chat-message")
        let enterPressed = ffilter (== 13) (input ^. textInput_keyup)
        return $ tagCurrent (pure . encode . SendChat <$> value input) enterPressed

    chatItem k itm = elClass "div" "chat-item" $ do
        elClass "span" "chat-user" $
            dynText $ (<> ": ") . view chatMsgAuthor <$> itm
        elClass "span" "chat-msg" $
            dynText $ view chatMsgBody <$> itm

    scrollBottom elm = do
        sh <- getScrollHeight elm
        setScrollTop elm sh
        