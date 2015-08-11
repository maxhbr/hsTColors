--------------------------------------------------------------------------------
-- |
-- Module      : System.HsTColors
-- Note : A minimal library which contains some small and simple functions to
-- make/print strings with terminal colors (and more).
--
-- Based on: https://github.com/schell/steeloverseer/blob/master/src/ANSIColors.hs
--
--------------------------------------------------------------------------------
module System.HsTColors
       ( HasANSICode (..)
       , ANSINone (..), ANSICode (..), ANSIColor (..), ANSIDecoration (..)
       , uncolor
       , uncolLength
       , isColored
--  String coloring
       , colorString
       , blackString, redString, greenString, yellowString, blueString, magentaString, cyanString, whiteString
       , boldString, backgroundString, underlineString, blinkString
--  Show colored things
       , colorShow
       , blackShow, redShow, greenShow, yellowShow, blueShow, magentaShow, cyanShow, whiteShow
       , boldShow, backgroundShow, underlineShow, blinkShow
--  Put colored strings
       , colorPutStrLn
       , blackPutStrLn, redPutStrLn, greenPutStrLn, yellowPutStrLn, bluePutStrLn, magentaPutStrLn, cyanPutStrLn, whitePutStrLn
       , boldPutStrLn, backgroundPutStrLn, underlinePutStrLn, blinkPutStrLn
       , colorPutStr
       , blackPutStr, redPutStr, greenPutStr, yellowPutStr, bluePutStr, magentaPutStr, cyanPutStr, whitePutStr
       , boldPutStr, backgroundPutStr, underlinePutStr, blinkPutStr
--  Print colored things
       , colorPrint
       , blackPrint, redPrint, greenPrint, yellowPrint, bluePrint, magentaPrint, cyanPrint, whitePrint
       , boldPrint, backgroundPrint, underlinePrint, blinkPrint
--  Trace colored strings
       , colorTrace
       , blackTrace, redTrace, greenTrace, yellowTrace, blueTrace, magentaTrace, cyanTrace, whiteTrace
       , boldTrace, backgroundTrace, underlineTrace, blinkTrace
--  IdTrace colored strings
       , colorIdTrace
       , blackIdTrace, redIdTrace, greenIdTrace, yellowIdTrace, blueIdTrace, magentaIdTrace, cyanIdTrace, whiteIdTrace
       , boldIdTrace, backgroundIdTrace, underlineIdTrace, blinkIdTrace
       ) where

import Debug.Trace (trace)
import Data.Maybe (fromJust)
import Data.Typeable (typeOf, cast, Typeable)

class (Show a) => HasANSICode a where
  toANSICode :: a -> String
  showANSICode :: a -> String
  showANSICode a = "\27[" ++ toANSICode a ++ "m"

--------------------------------------------------------------------------------
data ANSINone = ANSINone deriving (Ord,Eq)
instance HasANSICode ANSINone where
  toANSICode ANSINone = "0"
instance Show ANSINone where
  show = showANSICode

--------------------------------------------------------------------------------
data ANSICode = ANSICode String deriving (Ord,Eq)
instance HasANSICode ANSICode where
  toANSICode (ANSICode s) = s
instance Show ANSICode where
  show = showANSICode

--------------------------------------------------------------------------------
data ANSIColor = ANSIBlack
               | ANSIRed
               | ANSIGreen
               | ANSIYellow
               | ANSIBlue
               | ANSIMagenta
               | ANSICyan
               | ANSIWhite
               deriving (Ord,Eq)
instance HasANSICode ANSIColor where
  toANSICode ANSIBlack   = "30"
  toANSICode ANSIRed     = "31"
  toANSICode ANSIGreen   = "32"
  toANSICode ANSIYellow  = "33"
  toANSICode ANSIBlue    = "34"
  toANSICode ANSIMagenta = "35"
  toANSICode ANSICyan    = "36"
  toANSICode ANSIWhite   = "37"
instance Show ANSIColor where
  show = showANSICode

--------------------------------------------------------------------------------
data ANSIDecoration = ANSIBold
                    | ANSIBackground
                    | ANSIUnderline
                    | ANSIBlink
                    deriving (Ord,Eq)
instance HasANSICode ANSIDecoration where
  toANSICode ANSIBold       = "1"
  toANSICode ANSIBackground = "3"
  toANSICode ANSIUnderline  = "4"
  toANSICode ANSIBlink      = "5"
instance Show ANSIDecoration where
  show = showANSICode

--------------------------------------------------------------------------------
-- Basic functions
-- |The 'uncolor' function returns a String without the coloring
uncolor :: String -> String
uncolor ('\27':('[':(_:('m':ss))))     = uncolor ss
uncolor ('\27':('[':(_:(_:('m':ss))))) = uncolor ss
uncolor (s:ss)                         = s : uncolor ss
uncolor ""                             = ""

-- | the 'uncolLength' returns the real length of an strnig, ignoring the coloring
uncolLength :: String -> Int
uncolLength = length . uncolor

-- |The 'uncolor' function returns a String without the coloring
isColored :: String -> Bool
isColored ('\27':('[':(_:('m':_))))     = True
isColored ('\27':('[':(_:(_:('m':_))))) = True
isColored (_:ss)                        = isColored ss
isColored ""                            = False

--------------------------------------------------------------------------------
--  String coloring

colorString :: HasANSICode a => a -> String -> String
colorString c s = show c ++ s ++ show ANSINone

blackString, redString, greenString, yellowString, blueString, magentaString, cyanString, whiteString :: String -> String
blackString   = colorString ANSIBlack
redString     = colorString ANSIRed
greenString   = colorString ANSIGreen
yellowString  = colorString ANSIYellow
blueString    = colorString ANSIBlue
magentaString = colorString ANSIMagenta
cyanString    = colorString ANSICyan
whiteString   = colorString ANSIWhite
boldString, backgroundString, underlineString, blinkString :: String -> String
boldString       = colorString ANSIBold
backgroundString = colorString ANSIBackground
underlineString  = colorString ANSIUnderline
blinkString      = colorString ANSIBlink

--------------------------------------------------------------------------------
--  Show colored things
colorShow :: (HasANSICode a, Show b) => a -> b -> String
colorShow c s = colorString c (show s)

blackShow, redShow, greenShow, yellowShow, blueShow, magentaShow, cyanShow, whiteShow :: Show a => a -> String
blackShow   = colorShow ANSIBlack
redShow     = colorShow ANSIRed
greenShow   = colorShow ANSIGreen
yellowShow  = colorShow ANSIYellow
blueShow    = colorShow ANSIBlue
magentaShow = colorShow ANSIMagenta
cyanShow    = colorShow ANSICyan
whiteShow   = colorShow ANSIWhite
boldShow, backgroundShow, underlineShow, blinkShow :: Show a => a -> String
boldShow       = colorShow ANSIBold
backgroundShow = colorShow ANSIBackground
underlineShow  = colorShow ANSIUnderline
blinkShow      = colorShow ANSIBlink

--------------------------------------------------------------------------------
--  Put colored strings
colorPutStrLn :: HasANSICode a => a -> String -> IO ()
colorPutStrLn c = putStrLn . colorString c

blackPutStrLn, redPutStrLn, greenPutStrLn, yellowPutStrLn, bluePutStrLn, magentaPutStrLn, cyanPutStrLn, whitePutStrLn :: String -> IO ()
blackPutStrLn   = colorPutStrLn ANSIBlack
redPutStrLn     = colorPutStrLn ANSIRed
greenPutStrLn   = colorPutStrLn ANSIGreen
yellowPutStrLn  = colorPutStrLn ANSIYellow
bluePutStrLn    = colorPutStrLn ANSIBlue
magentaPutStrLn = colorPutStrLn ANSIMagenta
cyanPutStrLn    = colorPutStrLn ANSICyan
whitePutStrLn   = colorPutStrLn ANSIWhite
boldPutStrLn, backgroundPutStrLn, underlinePutStrLn, blinkPutStrLn :: String -> IO ()
boldPutStrLn       = colorPutStrLn ANSIBold
backgroundPutStrLn = colorPutStrLn ANSIBackground
underlinePutStrLn  = colorPutStrLn ANSIUnderline
blinkPutStrLn      = colorPutStrLn ANSIBlink

colorPutStr :: HasANSICode a => a -> String -> IO ()
colorPutStr c = putStr . colorString c

blackPutStr, redPutStr, greenPutStr, yellowPutStr, bluePutStr, magentaPutStr, cyanPutStr, whitePutStr :: String -> IO ()
blackPutStr   = colorPutStrLn ANSIBlack
redPutStr     = colorPutStr ANSIRed
greenPutStr   = colorPutStr ANSIGreen
yellowPutStr  = colorPutStr ANSIYellow
bluePutStr    = colorPutStr ANSIBlue
magentaPutStr = colorPutStr ANSIMagenta
cyanPutStr    = colorPutStr ANSICyan
whitePutStr   = colorPutStr ANSIWhite
boldPutStr, backgroundPutStr, underlinePutStr, blinkPutStr :: String -> IO ()
boldPutStr       = colorPutStr ANSIBold
backgroundPutStr = colorPutStr ANSIBackground
underlinePutStr  = colorPutStr ANSIUnderline
blinkPutStr      = colorPutStr ANSIBlink

--------------------------------------------------------------------------------
--  Print colored things
colorPrint :: (HasANSICode a, Show b) => a -> b -> IO ()
colorPrint c = colorPutStrLn c . show

blackPrint, redPrint, greenPrint, yellowPrint, bluePrint, magentaPrint, cyanPrint, whitePrint :: Show a => a -> IO ()
blackPrint   = colorPrint ANSIBlack
redPrint     = colorPrint ANSIRed
greenPrint   = colorPrint ANSIGreen
yellowPrint  = colorPrint ANSIYellow
bluePrint    = colorPrint ANSIBlue
magentaPrint = colorPrint ANSIMagenta
cyanPrint    = colorPrint ANSICyan
whitePrint   = colorPrint ANSIWhite
boldPrint, backgroundPrint, underlinePrint, blinkPrint :: Show a => a -> IO ()
boldPrint       = colorPrint ANSIBold
backgroundPrint = colorPrint ANSIBackground
underlinePrint  = colorPrint ANSIUnderline
blinkPrint      = colorPrint ANSIBlink

--------------------------------------------------------------------------------
--  Trace colored strings
colorTrace :: (HasANSICode a, Typeable b, Show b) => a -> b -> b' -> b'
colorTrace a x = trace (colorString a $ if typeOf x == typeOf ""
                                        then (fromJust $ cast x :: String)
                                        else show x)

blackTrace, redTrace, greenTrace, yellowTrace, blueTrace, magentaTrace, cyanTrace, whiteTrace :: (Typeable a, Show a) => a -> a' -> a'
blackTrace   = colorTrace ANSIBlack
redTrace     = colorTrace ANSIRed
greenTrace   = colorTrace ANSIGreen
yellowTrace  = colorTrace ANSIYellow
blueTrace    = colorTrace ANSIBlue
magentaTrace = colorTrace ANSIMagenta
cyanTrace    = colorTrace ANSICyan
whiteTrace   = colorTrace ANSIWhite
boldTrace, backgroundTrace, underlineTrace, blinkTrace :: (Typeable a, Show a) => a -> a' -> a'
boldTrace       = colorTrace ANSIBold
backgroundTrace = colorTrace ANSIBackground
underlineTrace  = colorTrace ANSIUnderline
blinkTrace      = colorTrace ANSIBlink

--------------------------------------------------------------------------------
--  IdTrace colored strings
colorIdTrace :: (HasANSICode a, Typeable b, Show b) => a -> b -> b
colorIdTrace a x = colorTrace a x x

blackIdTrace, redIdTrace, greenIdTrace, yellowIdTrace, blueIdTrace, magentaIdTrace, cyanIdTrace, whiteIdTrace :: (Typeable a, Show a) => a -> a
blackIdTrace   = colorIdTrace ANSIBlack
redIdTrace     = colorIdTrace ANSIRed
greenIdTrace   = colorIdTrace ANSIGreen
yellowIdTrace  = colorIdTrace ANSIYellow
blueIdTrace    = colorIdTrace ANSIBlue
magentaIdTrace = colorIdTrace ANSIMagenta
cyanIdTrace    = colorIdTrace ANSICyan
whiteIdTrace   = colorIdTrace ANSIWhite
boldIdTrace, backgroundIdTrace, underlineIdTrace, blinkIdTrace :: (Typeable a, Show a) => a -> a
boldIdTrace       = colorIdTrace ANSIBold
backgroundIdTrace = colorIdTrace ANSIBackground
underlineIdTrace  = colorIdTrace ANSIUnderline
blinkIdTrace      = colorIdTrace ANSIBlink
